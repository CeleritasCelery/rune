use super::{CloneIn, Gc, IntoObject, ObjCell, Object};
use crate::core::gc::{Block, GcHeap, Trace};
use crate::Markable;
use macro_attr_2018::macro_attr;
use newtype_derive_2018::{NewtypeDebug, NewtypeDeref, NewtypeDisplay};
use rune_core::hashmap::{HashSet, IndexMap};
use rune_macros::Trace;
use std::cell::{BorrowMutError, Ref, RefCell, RefMut};
use std::fmt::{self, Debug, Display, Write};

pub(crate) type HashTable<'ob> = IndexMap<Object<'ob>, Object<'ob>>;

pub(crate) struct HashTableView<'ob, T> {
    pub(crate) iter_next: usize,
    inner: IndexMap<Object<'ob>, T>,
}

#[derive(Eq)]
pub(crate) struct LispHashTableInner {
    inner: RefCell<HashTableView<'static, ObjCell>>,
}

macro_attr! {
    #[derive(PartialEq, Eq, NewtypeDebug!, NewtypeDisplay!, NewtypeDeref!, Markable!, Trace)]
    pub(crate) struct LispHashTable(GcHeap<LispHashTableInner>);
}

impl LispHashTable {
    pub(in crate::core) unsafe fn new<const C: bool>(table: HashTable, block: &Block<C>) -> Self {
        Self(GcHeap::new(LispHashTableInner::new(table), block))
    }

    pub(in crate::core) fn make_const(&mut self) {
        // TODO: once we have resolved
        // https://github.com/CeleritasCelery/rune/issues/58 Leak the borrow so
        // that is cannot be borrowed mutabley. In the mean time this will
        // introduce a race condtion when access from multiple threads.
        // std::mem::forget(self.inner.borrow());
    }
}

impl<'ob, T> std::ops::Deref for HashTableView<'ob, T> {
    type Target = IndexMap<Object<'ob>, T>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'ob, T> std::ops::DerefMut for HashTableView<'ob, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl PartialEq for LispHashTableInner {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}

impl<T: PartialEq> PartialEq for HashTableView<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}

impl<T: Eq> Eq for HashTableView<'_, T> {}

impl LispHashTableInner {
    // SAFETY: Since this type does not have an object lifetime, it is only safe
    // to create an owned version in context of the allocator.
    pub(in crate::core) unsafe fn new(vec: HashTable) -> Self {
        let cell = std::mem::transmute::<
            IndexMap<Object, Object>,
            IndexMap<Object<'static>, ObjCell>,
        >(vec);
        Self { inner: RefCell::new(HashTableView { iter_next: 0, inner: cell }) }
    }

    pub(crate) fn borrow<'a>(&'a self) -> Ref<'a, HashTableView<'a, ObjCell>> {
        unsafe {
            std::mem::transmute::<
                Ref<'a, HashTableView<'static, _>>,
                Ref<'a, HashTableView<'a, ObjCell>>,
            >(self.inner.borrow())
        }
    }

    pub(crate) fn try_borrow_mut(
        &self,
    ) -> Result<RefMut<'_, HashTableView<'_, Object<'_>>>, BorrowMutError> {
        unsafe {
            self.inner.try_borrow_mut().map(|x| {
                std::mem::transmute::<
                    RefMut<'_, HashTableView<'static, ObjCell>>,
                    RefMut<'_, HashTableView<'_, Object<'_>>>,
                >(x)
            })
        }
    }
}

impl<'new> CloneIn<'new, &'new Self> for LispHashTable {
    fn clone_in<const C: bool>(&self, bk: &'new Block<C>) -> Gc<&'new Self> {
        let mut table = HashTable::default();
        let borrow = self.borrow();
        for (key, value) in &borrow.inner {
            let new_key = key.clone_in(bk);
            let new_value = value.get().clone_in(bk);
            table.insert(new_key, new_value);
        }
        table.into_obj(bk)
    }
}

impl Trace for LispHashTableInner {
    fn trace(&self, stack: &mut Vec<super::RawObj>) {
        let table = self.borrow();
        for (k, v) in &table.inner {
            if k.is_markable() {
                stack.push(k.into_raw());
            }
            if v.get().is_markable() {
                stack.push(v.get().into_raw());
            }
        }
    }
}

impl Debug for LispHashTableInner {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.display_walk(f, &mut HashSet::default())
    }
}

impl Display for LispHashTableInner {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.display_walk(f, &mut HashSet::default())
    }
}

impl LispHashTableInner {
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
        for (i, (k, v)) in self.borrow().inner.iter().enumerate() {
            if i != 0 {
                f.write_char(' ')?;
            }
            k.untag().display_walk(f, seen)?;
            f.write_char(' ')?;
            v.get().untag().display_walk(f, seen)?;
        }
        write!(f, "))")
    }
}
