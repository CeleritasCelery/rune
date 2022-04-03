#![allow(dead_code)]

use super::{GcCell, RootSet};

// uninhabited types
enum Vtable {}
enum Data {}

#[repr(C)]
#[derive(Debug, Copy, Clone)]
struct DynObject {
    data: *const Data,
    vtable: *const Vtable,
}

pub(crate) trait Trace {
    fn mark(&self);
}

#[derive(Debug)]
pub(crate) struct GcRoot<'rt> {
    obj: DynObject,
    root_set: &'rt RootSet,
}

impl<'rt> Drop for GcRoot<'rt> {
    fn drop(&mut self) {
        self.root_set.root_structs.borrow_mut().pop();
    }
}

impl<'rt> GcRoot<'rt> {
    pub(crate) unsafe fn new(root_set: &'rt RootSet) -> Self {
        use std::ptr::null;
        Self {
            obj: DynObject {
                vtable: null(),
                data: null(),
            },
            root_set,
        }
    }

    pub(crate) fn set<'a, 'id, T: Trace>(
        &mut self,
        root: &'a mut GcCell<'id, T>,
    ) -> &'a GcCell<'id, T> {
        {
            let data = root.deref();
            self.obj.vtable = Self::extract_vtable(data);
            self.obj.data = (data as *const T).cast::<Data>();
            unsafe {
                // false positive
                #[allow(clippy::transmute_ptr_to_ptr)]
                self.root_set
                    .root_structs
                    .borrow_mut()
                    .push(std::mem::transmute::<&GcRoot<'rt>, &GcRoot<'static>>(self)
                        as *const GcRoot<'static>);
            }
        }
        root
    }

    fn extract_vtable<T: Trace>(data: &T) -> *const Vtable {
        let obj = data as &dyn Trace;
        unsafe { std::mem::transmute::<&dyn Trace, DynObject>(obj).vtable }
    }

    fn dyn_data(&self) -> &dyn Trace {
        unsafe { std::mem::transmute::<DynObject, &dyn Trace>(self.obj) }
    }
}

impl<T: Trace> Trace for Vec<T> {
    fn mark(&self) {
        for x in self {
            x.mark();
        }
    }
}

impl<T: Trace> Trace for Option<T> {
    fn mark(&self) {
        self.as_ref().map(Trace::mark);
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{arena::Arena, root_struct};

    struct Foo(u64);
    impl Trace for Foo {
        fn mark(&self) {
            assert!(self.0 == 7);
        }
    }

    #[test]
    fn test_gc_root() {
        let roots = &RootSet::default();
        let gc = &mut Arena::new(roots);
        let foo = Foo(7);
        assert_eq!(roots.root_structs.borrow().len(), 0);
        {
            root_struct!(_root, foo, gc);
            assert_eq!(roots.root_structs.borrow().len(), 1);
        }
        assert_eq!(roots.root_structs.borrow().len(), 0);
    }
}
