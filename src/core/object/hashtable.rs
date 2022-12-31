use std::cell::{BorrowMutError, Ref, RefCell, RefMut};
use std::fmt::{Debug, Display};

use std::collections::hash_map::Iter as HashIter;

use streaming_iterator::StreamingIterator;

use crate::core::gc::{Context, Root, Rt};
use crate::{
    core::gc::{GcManaged, GcMark, Trace},
    hashmap::HashMap,
};

use super::{CloneIn, Gc, GcObj, IntoObject, MutObjCell, ObjCell};

pub(crate) type HashTable<'ob> = HashMap<GcObj<'ob>, GcObj<'ob>>;
pub(crate) type HashTableView<'ob, T> = HashMap<GcObj<'ob>, T>;
#[derive(Debug)]
pub(crate) struct LispHashTable {
    gc: GcMark,
    is_const: bool,
    inner: RefCell<HashTableView<'static, ObjCell>>,
}

impl PartialEq for LispHashTable {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}

impl Eq for LispHashTable {}

impl LispHashTable {
    // SAFETY: Since this type does not have an object lifetime, it is only safe
    // to create an owned version in context of the allocator.
    pub(in crate::core) unsafe fn new(vec: HashTable) -> Self {
        let cell = std::mem::transmute::<HashTable<'_>, HashTableView<'static, ObjCell>>(vec);
        Self {
            gc: GcMark::default(),
            is_const: false,
            inner: RefCell::new(cell),
        }
    }

    pub(in crate::core) fn make_const(&mut self) {
        self.is_const = true;
        // Leak the borrow so that is cannot be borrowed mutabley
        std::mem::forget(self.inner.borrow());
    }

    pub(crate) fn borrow<'a>(&'a self) -> Ref<'a, HashTableView<'a, ObjCell>> {
        unsafe {
            std::mem::transmute::<
                Ref<'a, HashTableView<'static, _>>,
                Ref<'a, HashTableView<'a, ObjCell>>,
            >(self.inner.borrow())
        }
    }

    // This is a stop gap solution until we have a better model for the hashmap.
    // Right now when we are iterating over the map, we will be holding a
    // reference to it. But that means we can't mutate the values. So we have
    // this shared_mut method that will give us a MutObjCell. This lets us
    // modify existing keys, but not insert new ones.
    pub(crate) fn try_borrow_shared_mut(
        &self,
    ) -> Result<Ref<'_, HashTableView<'_, MutObjCell>>, anyhow::Error> {
        if self.is_const {
            Err(anyhow::anyhow!("Attempt to borrow immutable hashtable"))
        } else {
            unsafe {
                Ok(std::mem::transmute::<
                    Ref<'_, HashTableView<'static, ObjCell>>,
                    Ref<'_, HashTableView<'_, MutObjCell>>,
                >(self.inner.borrow()))
            }
        }
    }

    pub(crate) fn try_borrow_mut(&self) -> Result<RefMut<'_, HashTable<'_>>, BorrowMutError> {
        unsafe {
            self.inner.try_borrow_mut().map(|x| {
                std::mem::transmute::<
                    RefMut<'_, HashTableView<'static, ObjCell>>,
                    RefMut<'_, HashTableView<'_, GcObj<'_>>>,
                >(x)
            })
        }
    }

    pub(crate) fn iter<'rt, 'rs, 'a>(
        table: &'rt Rt<Gc<&'static LispHashTable>>,
        root: &'rt mut Root<'rs, 'rt, (GcObj<'static>, GcObj<'static>)>,
        cx: &'a Context,
    ) -> HashTableStreamIter<'rt, 'rs> {
        // SAFETY: Since we know table is rooted, it is safe to change the inner
        // lifetime to static and the outer one to match 'rt
        let ref_cell = unsafe {
            let cell = table.get(cx).borrow();
            std::mem::transmute::<
                Ref<'a, HashTableView<'a, ObjCell>>,
                Ref<'rt, HashTableView<'static, ObjCell>>,
            >(cell)
        };
        // SAFETY: We are creating two references to the hashtable. One is
        // cell::Ref that will make sure the borrow flag forbids mutable borrows
        // until we are done with iteration. The second is an iterator over the
        // hashtable. You can't normally move the Ref while it borrowed (by the
        // iterator), but we are hacking it here. I am not even sure if this is
        // sound, and HashTables need a better abtraction.
        let iter = unsafe {
            let hashtable: &HashTableView<'static, ObjCell> = &ref_cell;
            let hashtable: &'rt HashTableView<'static, ObjCell> = &*(hashtable as *const _);
            hashtable.iter()
        };
        HashTableStreamIter {
            iter,
            _rf: ref_cell,
            item: Some(root),
        }
    }
}

impl<'new> CloneIn<'new, &'new Self> for LispHashTable {
    fn clone_in<const C: bool>(&self, bk: &'new crate::core::gc::Block<C>) -> Gc<&'new Self> {
        let mut table = HashTable::default();
        let borrow = self.borrow();
        for (key, value) in borrow.iter() {
            let new_key = key.clone_in(bk);
            let new_value = value.get().clone_in(bk);
            table.insert(new_key, new_value);
        }
        table.into_obj(bk)
    }
}

impl Trace for LispHashTable {
    fn trace(&self, stack: &mut Vec<super::RawObj>) {
        let table = self.borrow();
        for (k, v) in &*table {
            if k.is_markable() {
                stack.push(k.into_raw());
            }
            if v.get().is_markable() {
                stack.push(v.get().into_raw());
            }
        }
        self.mark();
    }
}

impl GcManaged for LispHashTable {
    fn get_mark(&self) -> &GcMark {
        &self.gc
    }
}

pub(crate) struct HashTableStreamIter<'rt, 'rs> {
    _rf: Ref<'rt, HashTableView<'static, ObjCell>>,
    iter: HashIter<'rt, GcObj<'static>, ObjCell>,
    item: Option<&'rt mut Root<'rs, 'rt, (GcObj<'static>, GcObj<'static>)>>,
}

impl<'rt, 'rs> StreamingIterator for HashTableStreamIter<'rt, 'rs> {
    type Item = (Rt<GcObj<'static>>, Rt<GcObj<'static>>);

    fn advance(&mut self) {
        if let Some((k, v)) = self.iter.next() {
            let item = self
                .item
                .as_mut()
                .expect("item should never be None while iter is Some");
            unsafe {
                let tuple: &mut (Rt<GcObj>, Rt<GcObj>) = item.deref_mut_unchecked();
                tuple.0.set(*k);
                tuple.1.set(v.get());
            }
        } else {
            self.item = None;
        }
    }

    fn get(&self) -> Option<&Self::Item> {
        self.item.as_ref().map(|x| &****x)
    }
}

impl Display for LispHashTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}
