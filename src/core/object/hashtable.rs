//! The design of the hashtable is based on the requirments we have. First is we
//! need it to support being both thread local and global. Second we need
//! iterate and mutate at the same time. Third we need to be able to clean up
//! the heap allocation when it is garbage collected.
use super::{CloneIn, Gc, IntoObject, ObjCell, Object, WithLifetime};
use crate::core::env::interned_symbols;
use crate::core::gc::{Block, GcHeap, GcState, Trace};
use crate::NewtypeMarkable;
use macro_attr_2018::macro_attr;
use newtype_derive_2018::{NewtypeDebug, NewtypeDeref, NewtypeDisplay};
use rune_core::hashmap::{HashSet, IndexMap};
use rune_macros::Trace;
use std::cell::RefCell;
use std::fmt::{self, Debug, Display, Write};
use std::ptr::NonNull;
use std::sync::Mutex;

pub(crate) type HashTable<'ob> = IndexMap<Object<'ob>, Object<'ob>>;

macro_attr! {
    #[derive(PartialEq, Eq, NewtypeDebug!, NewtypeDisplay!, NewtypeDeref!, NewtypeMarkable!, Trace)]
    pub(crate) struct LispHashTable(GcHeap<HashTableCore<'static>>);
}

impl LispHashTable {
    pub(in crate::core) unsafe fn new(table: HashTable, constant: bool) -> Self {
        Self(GcHeap::new(HashTableCore::new(table, constant), constant))
    }

    pub(in crate::core) fn forwarding_ptr(&self) -> Option<NonNull<u8>> {
        use crate::core::gc::AllocState as A;
        match self.0.allocation_state() {
            A::Forwarded(f) => Some(f),
            A::Global => panic!("global hashtable allocation found in local heap"),
            A::Unmoved => None,
        }
    }
}

pub(crate) struct HashTableCore<'ob>(HashTableType<'ob>);

// Hashtables are currently the only data structure that can be shared between
// threads. This is because it is used for caching in some functions in
// cl-generic.
enum HashTableType<'a> {
    Local(RefCell<HashTableInner<'a>>),
    Global(Mutex<HashTableInner<'a>>),
}

struct HashTableInner<'ob> {
    // The current index of a [`maphash`] iterator. This is needed because we
    // can't hold the hashtable across calls to elisp (it might mutate it).
    iter_idx: usize,
    inner: HashTable<'ob>,
}

impl<'a> HashTableCore<'a> {
    pub(in crate::core) unsafe fn new(table: HashTable, constant: bool) -> Self {
        let table = std::mem::transmute::<HashTable<'_>, HashTable<'a>>(table);
        let inner = HashTableInner { iter_idx: 0, inner: table };
        if constant {
            HashTableCore(HashTableType::Global(Mutex::new(inner)))
        } else {
            HashTableCore(HashTableType::Local(RefCell::new(inner)))
        }
    }

    fn with<F, T>(&self, mut f: F) -> T
    where
        F: FnMut(&mut HashTable<'a>) -> T,
    {
        match &self.0 {
            HashTableType::Local(table) => f(&mut table.borrow_mut().inner),
            HashTableType::Global(table) => f(&mut table.lock().unwrap().inner),
        }
    }

    pub(crate) fn len(&self) -> usize {
        self.with(|x| x.len())
    }

    pub(crate) fn get(&self, key: Object) -> Option<Object<'_>> {
        self.with(|x| x.get(&key).copied())
    }

    pub(crate) fn get_index(&self, index: usize) -> Option<(Object, Object)> {
        self.with(|x| x.get_index(index).map(|(k, v)| (*k, *v)))
    }

    pub(crate) fn get_index_of(&self, key: Object) -> Option<usize> {
        self.with(|x| x.get_index_of(&key))
    }

    pub(crate) fn insert(&self, key: Object, value: Object) {
        match &self.0 {
            HashTableType::Local(table) => {
                let key = unsafe { key.with_lifetime() };
                let value = unsafe { value.with_lifetime() };
                table.borrow_mut().inner.insert(key, value)
            }
            HashTableType::Global(table) => {
                let map = interned_symbols().lock().unwrap();
                let block = map.global_block();
                // Need to clone these objects in the global block since this
                // hashtable is globally shared
                let key = unsafe { key.clone_in(block).with_lifetime() };
                let value = unsafe { value.clone_in(block).with_lifetime() };
                table.lock().unwrap().inner.insert(key, value)
            }
        };
    }

    pub(crate) fn shift_remove(&self, key: Object) {
        let key = unsafe { key.with_lifetime() };
        self.with(|x| x.shift_remove(&key));
    }

    pub(crate) fn get_iter_index(&self) -> usize {
        match &self.0 {
            HashTableType::Local(table) => table.borrow().iter_idx,
            HashTableType::Global(table) => table.lock().unwrap().iter_idx,
        }
    }

    pub(crate) fn set_iter_index(&self, index: usize) {
        match &self.0 {
            HashTableType::Local(table) => table.borrow_mut().iter_idx = index,
            HashTableType::Global(table) => table.lock().unwrap().iter_idx = index,
        }
    }
}

impl Trace for HashTableCore<'_> {
    fn trace(&self, state: &mut GcState) {
        let HashTableType::Local(table) = &self.0 else {
            panic!("Global hash table should not be traced")
        };
        let table = &mut table.borrow_mut().inner;
        // ObjCell are updated in place when traced, so casting to ObjCell will
        // allow all the objects to be updated.
        let table = unsafe {
            std::mem::transmute::<&mut IndexMap<Object, Object>, &mut IndexMap<ObjCell, ObjCell>>(
                table,
            )
        };
        table.rehash_keys(|key, val| {
            key.trace(state);
            val.trace(state);
        });
    }
}

impl Eq for HashTableCore<'_> {}
impl PartialEq for HashTableCore<'_> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl Debug for HashTableCore<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.display_walk(f, &mut HashSet::default())
    }
}

impl Display for HashTableCore<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.display_walk(f, &mut HashSet::default())
    }
}

impl<'new> CloneIn<'new, &'new Self> for LispHashTable {
    fn clone_in<const C: bool>(&self, bk: &'new Block<C>) -> Gc<&'new Self> {
        let mut table = HashTable::default();
        self.with(|x| {
            for (key, value) in x {
                let new_key = key.clone_in(bk);
                let new_value = value.clone_in(bk);
                table.insert(new_key, new_value);
            }
        });
        table.into_obj(bk)
    }
}

impl HashTableCore<'_> {
    pub(super) fn display_walk(
        &self,
        f: &mut fmt::Formatter,
        seen: &mut HashSet<*const u8>,
    ) -> fmt::Result {
        let ptr = (self as *const Self).cast();
        if seen.contains(&ptr) {
            return write!(f, "#0");
        }
        seen.insert(ptr);

        write!(f, "#s(hash-table (")?;
        self.with(|x| {
            for (i, (k, v)) in x.iter().enumerate() {
                if i != 0 {
                    f.write_char(' ')?;
                }
                k.untag().display_walk(f, seen)?;
                f.write_char(' ')?;
                v.untag().display_walk(f, seen)?;
            }
            Ok(())
        })?;
        write!(f, "))")
    }
}
