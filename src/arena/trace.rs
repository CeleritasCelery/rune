use std::fmt::Debug;

use super::{Root, RootSet};

pub(crate) trait Trace {
    fn mark(&self);
}

pub(crate) struct RootStruct<'rt> {
    obj: Option<*const dyn Trace>,
    root_set: &'rt RootSet,
}

impl<'rt> Debug for RootStruct<'rt> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("GcRoot")
            .field("root_set", &self.root_set)
            .finish()
    }
}

impl<'rt> Drop for RootStruct<'rt> {
    fn drop(&mut self) {
        self.root_set.root_structs.borrow_mut().pop();
    }
}

impl<'rt> RootStruct<'rt> {
    pub(crate) unsafe fn new(root_set: &'rt RootSet) -> Self {
        Self {
            obj: None,
            root_set,
        }
    }

    pub(crate) fn set<'a, 'id, T: Trace + 'static>(
        &mut self,
        root: &'a mut Root<'id, T>,
    ) -> &'a Root<'id, T> {
        assert!(self.obj.is_none(), "RootStruct should only be set once");
        let dyn_ptr = root.deref() as &dyn Trace as *const dyn Trace;
        self.obj = Some(dyn_ptr);
        self.root_set.root_structs.borrow_mut().push(dyn_ptr);
        unsafe { &*dyn_ptr.cast::<Root<'id, T>>() }
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
