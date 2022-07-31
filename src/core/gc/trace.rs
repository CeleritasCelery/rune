use super::super::object::RawObj;

pub(crate) trait Trace {
    fn mark(&self, stack: &mut Vec<RawObj>);
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
    use super::super::super::gc::{Context, RootSet};
    use super::*;
    use crate::root;

    struct Foo(u64);
    impl Trace for Foo {
        fn mark(&self, _stack: &mut Vec<RawObj>) {
            assert!(self.0 == 7);
        }
    }

    #[test]
    fn test_trace_root() {
        let roots = &RootSet::default();
        let gc = &mut Context::new(roots);
        let foo = Foo(7);
        assert_eq!(roots.roots.borrow().len(), 0);
        {
            root!(_root, foo, gc);
            assert_eq!(roots.roots.borrow().len(), 1);
        }
        assert_eq!(roots.roots.borrow().len(), 0);
    }
}
