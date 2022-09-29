use std::cell::RefCell;

use super::GcObj;

#[derive(Debug)]
pub(crate) struct LispVec<'ob> {
    inner: RefCell<Vec<GcObj<'ob>>>,
}

impl<'ob> LispVec<'ob> {
    pub(in crate::core) fn new(vec: Vec<GcObj<'ob>>) -> Self {
        Self {
            inner: RefCell::new(vec),
        }
    }

    pub(in crate::core) fn new_const(vec: Vec<GcObj<'ob>>) -> Self {
        let vec = Self::new(vec);
        // Leak the borrow so that is cannot be borrowed mutabley
        std::mem::forget(vec.inner.borrow());
        vec
    }

    pub(crate) fn len(&self) -> usize {
        self.borrow().len()
    }
}

impl<'ob> std::ops::Deref for LispVec<'ob> {
    type Target = RefCell<Vec<GcObj<'ob>>>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
