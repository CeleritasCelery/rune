use crate::lisp_object::{Object, LispObj, IntoObject};
use std::cell::RefCell;

pub struct Arena {
    objects: RefCell<Vec<LispObj>>,
}

impl<'obj> Arena {
    pub fn insert<T>(&'obj self, obj: T) -> Object
    where
        T: IntoObject<'obj>,
    {
        let (obj, allocated) = obj.into_object(self);
        if allocated {
            self.objects.borrow_mut().push(obj.clone().inner());
        }
        obj
    }

    pub fn alloc<T>(&self, obj: T) -> *const T {
        Box::into_raw(Box::new(obj))
    }
}

impl std::ops::Drop for Arena {
    fn drop(&mut self) {
        for obj in self.objects.get_mut() {
            unsafe {
                obj.dealloc();
            }
        }
    }
}

impl Arena {
    pub const fn new() -> Self {
        Arena {
            objects: RefCell::new(Vec::new()),
        }
    }
}
