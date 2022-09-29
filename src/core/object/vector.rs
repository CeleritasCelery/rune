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

    pub(in crate::core) fn make_const(&self) {
        // Leak the borrow so that is cannot be borrowed mutabley
        std::mem::forget(self.inner.borrow());
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

#[derive(Debug)]
#[repr(transparent)]
pub(crate) struct Record<'ob>(pub(in crate::core::object) LispVec<'ob>);

impl<'ob> Record<'ob> {
    pub(crate) fn new(vec: Vec<GcObj<'ob>>) -> Self {
        Record(LispVec::new(vec))
    }
}

impl<'ob> std::ops::Deref for Record<'ob> {
    type Target = LispVec<'ob>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
