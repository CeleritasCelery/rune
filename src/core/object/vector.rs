use std::cell::{BorrowMutError, Ref, RefCell, RefMut};

use crate::hashmap::HashMap;

use super::GcObj;

#[derive(Debug)]
pub(crate) struct LispVec {
    inner: RefCell<Vec<GcObj<'static>>>,
}

impl LispVec {
    pub(in crate::core) unsafe fn new(vec: Vec<GcObj>) -> Self {
        let cell = std::mem::transmute::<Vec<GcObj>, Vec<GcObj<'static>>>(vec);
        Self {
            inner: RefCell::new(cell),
        }
    }

    pub(in crate::core) fn make_const(&self) {
        // Leak the borrow so that is cannot be borrowed mutabley
        std::mem::forget(self.inner.borrow());
    }

    pub(crate) fn len(&self) -> usize {
        self.borrow().len()
    }

    pub(crate) fn borrow<'a>(&'a self) -> Ref<'a, Vec<GcObj<'a>>> {
        unsafe {
            std::mem::transmute::<Ref<'a, Vec<GcObj<'static>>>, Ref<'a, Vec<GcObj<'a>>>>(
                self.inner.borrow(),
            )
        }
    }

    pub(crate) fn try_borrow_mut(&self) -> Result<RefMut<'_, Vec<GcObj<'_>>>, BorrowMutError> {
        unsafe {
            self.inner.try_borrow_mut().map(|x| {
                std::mem::transmute::<RefMut<'_, Vec<GcObj<'static>>>, RefMut<'_, Vec<GcObj<'_>>>>(
                    x,
                )
            })
        }
    }
}

#[repr(transparent)]
pub(crate) struct RecordBuilder<'ob>(pub(crate) Vec<GcObj<'ob>>);

#[derive(Debug)]
#[repr(transparent)]
pub(crate) struct Record(LispVec);

impl std::ops::Deref for Record {
    type Target = LispVec;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub(crate) type HashTable<'ob> = HashMap<GcObj<'ob>, GcObj<'ob>>;
#[derive(Debug)]
pub(crate) struct LispHashTable {
    inner: RefCell<HashTable<'static>>,
}

impl LispHashTable {
    pub(in crate::core) unsafe fn new(vec: HashTable) -> Self {
        let cell = std::mem::transmute::<HashTable<'_>, HashTable<'static>>(vec);
        Self {
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
}
