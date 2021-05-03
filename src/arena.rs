use crate::lisp_object::{GcObject, IntoObject, RefObject, TaggedObject};
use std::cell::RefCell;

#[derive(Debug, PartialEq)]
pub struct Arena {
    objects: RefCell<Vec<GcObject>>,
}

impl<'obj> Arena {
    pub const fn new() -> Self {
        Arena {
            objects: RefCell::new(Vec::new()),
        }
    }

    pub fn insert<T, U, V>(&'obj self, obj: T) -> U
    where
        T: IntoObject<V>,
        U: RefObject<'obj>,
        V: TaggedObject + Into<U> + Copy,
    {
        let obj = obj.into_object(self);
        if obj.is_boxed() {
            self.objects.borrow_mut().push(obj.into_gc());
        }
        obj.into()
    }

    pub fn alloc<T>(&self, obj: T) -> *const T {
        Box::into_raw(Box::new(obj))
    }
}

impl std::ops::Drop for Arena {
    fn drop(&mut self) {
        for obj in self.objects.get_mut() {
            unsafe {
                obj.drop();
            }
        }
    }
}

impl std::clone::Clone for Arena {
    fn clone(&self) -> Self {
        let new = Arena::new();
        for old in self.objects.borrow().iter() {
            old.clone_in(&new);
        }
        new
    }
}
