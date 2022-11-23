use std::cell::{BorrowMutError, Ref, RefCell, RefMut};
use std::fmt::{Debug, Display};
use std::ops::Deref;

use std::collections::hash_map::Iter as HashIter;

use streaming_iterator::StreamingIterator;

use crate::core::gc::{Context, Root, Rt};
use crate::{
    core::gc::{GcManaged, GcMark, Trace},
    hashmap::HashMap,
};

use super::{Gc, GcObj, WithLifetime};

pub(crate) type HashTable<'ob> = HashMap<GcObj<'ob>, GcObj<'ob>>;
#[derive(Debug)]
pub(crate) struct LispHashTable {
    gc: GcMark,
    inner: RefCell<HashTable<'static>>,
}

impl LispHashTable {
    // SAFETY: Since this type does not have an object lifetime, it is only safe
    // to create an owned version in context of the allocator.
    pub(in crate::core) unsafe fn new(vec: HashTable) -> Self {
        let cell = std::mem::transmute::<HashTable<'_>, HashTable<'static>>(vec);
        Self {
            gc: GcMark::default(),
            inner: RefCell::new(cell),
        }
    }

    pub(in crate::core) fn make_const(&self) {
        // Leak the borrow so that is cannot be borrowed mutabley
        std::mem::forget(self.inner.borrow());
    }

    pub(crate) fn borrow<'a>(&'a self) -> Ref<'a, HashTable<'a>> {
        unsafe {
            std::mem::transmute::<Ref<'a, HashTable<'static>>, Ref<'a, HashTable<'a>>>(
                self.inner.borrow(),
            )
        }
    }

    pub(crate) fn try_borrow_mut(&self) -> Result<RefMut<'_, HashTable<'_>>, BorrowMutError> {
        unsafe {
            self.inner.try_borrow_mut().map(|x| {
                std::mem::transmute::<RefMut<'_, HashTable<'static>>, RefMut<'_, HashTable<'_>>>(x)
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
            std::mem::transmute::<Ref<'a, HashTable<'a>>, Ref<'rt, HashTable<'static>>>(cell)
        };
        // SAFETY: We are creating two references to the hashtable. One is
        // cell::Ref that will make sure the borrow flag forbids mutable borrows
        // until we are done with iteration. The second is an iterator over the
        // hashtable. You can't normally move the Ref while it borrowed (by the
        // iterator), but we are hacking it here. I am not even sure if this is
        // sound, and HashTables need a better abtraction.
        let iter = unsafe {
            let hashtable: &HashTable<'static> = &ref_cell;
            let hashtable: &'rt HashTable<'static> = &*(hashtable as *const _);
            hashtable.iter()
        };
        HashTableStreamIter {
            iter,
            _rf: ref_cell,
            item: Some(root),
        }
    }
}

impl Trace for LispHashTable {
    fn trace(&self, stack: &mut Vec<super::RawObj>) {
        let table = self.borrow();
        for (k, v) in &*table {
            if k.is_markable() {
                stack.push(k.into_raw());
            }
            if v.is_markable() {
                stack.push(v.into_raw());
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

impl<'old, 'new> WithLifetime<'new> for &'old LispHashTable {
    type Out = &'new LispHashTable;

    unsafe fn with_lifetime(self) -> Self::Out {
        &*(self as *const LispHashTable)
    }
}

pub(crate) struct HashTableStreamIter<'rt, 'rs> {
    _rf: Ref<'rt, HashTable<'static>>,
    iter: HashIter<'rt, GcObj<'static>, GcObj<'static>>,
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
                tuple.1.set(*v);
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

#[derive(PartialEq)]
pub(crate) struct LispFloat {
    gc: GcMark,
    float: f64,
}

impl LispFloat {
    pub(in crate::core) fn new(float: f64) -> Self {
        Self {
            gc: GcMark::default(),
            float,
        }
    }
}

impl Deref for LispFloat {
    type Target = f64;

    fn deref(&self) -> &Self::Target {
        &self.float
    }
}

impl GcManaged for LispFloat {
    fn get_mark(&self) -> &GcMark {
        &self.gc
    }
}

impl Display for LispFloat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let float = self.float;
        if float.fract() == 0.0_f64 {
            write!(f, "{float:.1}")
        } else {
            write!(f, "{float}")
        }
    }
}

impl Debug for LispFloat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}
