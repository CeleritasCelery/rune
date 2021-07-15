use crate::object::{GcObject, IntoObject, Object};
use std::cell::RefCell;

#[derive(Debug, PartialEq)]
pub struct Arena {
    objects: RefCell<Vec<GcObject>>,
}

impl<'ob> Arena {
    pub const fn new() -> Self {
        Arena {
            objects: RefCell::new(Vec::new()),
        }
    }

    pub fn alloc<T>(&self, obj: T) -> *const T {
        Box::into_raw(Box::new(obj))
    }

    pub fn register(&self, obj: Object<'ob>) {
        self.objects.borrow_mut().push(unsafe { obj.into_gc() });
    }

    pub fn add<Input, Output>(&'ob self, item: Input) -> Output
    where
        Input: IntoObject<'ob, Output>,
    {
        item.into_obj(self)
    }
}

impl Drop for Arena {
    fn drop(&mut self) {
        for obj in self.objects.get_mut() {
            unsafe {
                obj.drop();
            }
        }
    }
}

impl Clone for Arena {
    fn clone(&self) -> Self {
        let new = Arena::new();
        for old in self.objects.borrow().iter() {
            old.clone_in(&new);
        }
        new
    }
}
