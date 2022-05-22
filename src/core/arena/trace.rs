use std::fmt::Debug;

use super::super::object::RawObj;

use super::{Root, RootSet};

pub(crate) trait Trace {
    fn mark(&self, stack: &mut Vec<RawObj>);
}

pub(crate) struct RootStruct<'rt> {
    set: bool,
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
        assert!(self.set, "RootStruct was dropped while still not set");
        self.root_set.root_structs.borrow_mut().pop();
    }
}

impl<'rt> RootStruct<'rt> {
    pub(crate) unsafe fn new(root_set: &'rt RootSet) -> Self {
        Self {
            set: false,
            root_set,
        }
    }

    pub(crate) fn set<'a, 'id, T: Trace + 'static>(
        &mut self,
        root: &'a mut Root<'id, T>,
    ) -> &'a Root<'id, T> {
        assert!(!self.set, "RootStruct should only be set once");
        let dyn_ptr = root.deref() as &dyn Trace as *const dyn Trace;
        self.set = true;
        self.root_set.root_structs.borrow_mut().push(dyn_ptr);
        unsafe { &*dyn_ptr.cast::<Root<'id, T>>() }
    }
}

impl<T: Trace> Trace for Vec<T> {
    fn mark(&self, stack: &mut Vec<RawObj>) {
        for x in self {
            x.mark(stack);
        }
    }
}

impl<T: Trace> Trace for Option<T> {
    fn mark(&self, stack: &mut Vec<RawObj>) {
        if let Some(x) = self.as_ref() {
            x.mark(stack);
        }
    }
}

#[cfg(test)]
mod test {
    use super::super::super::arena::Arena;
    use super::*;
    use crate::root_struct;

    struct Foo(u64);
    impl Trace for Foo {
        fn mark(&self, _stack: &mut Vec<RawObj>) {
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
