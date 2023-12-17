use super::super::object::RawObj;
use rune_core::hashmap::{HashMap, HashSet};

pub(crate) trait Trace {
    fn trace(&self, stack: &mut Vec<RawObj>);
}

impl Trace for usize {
    fn trace(&self, _: &mut Vec<RawObj>) {}
}

impl Trace for u64 {
    fn trace(&self, _: &mut Vec<RawObj>) {}
}

impl Trace for i64 {
    fn trace(&self, _: &mut Vec<RawObj>) {}
}

impl<T: Trace> Trace for &T {
    fn trace(&self, stack: &mut Vec<RawObj>) {
        (*self).trace(stack);
    }
}

impl<T: Trace, U: Trace> Trace for (T, U) {
    fn trace(&self, stack: &mut Vec<RawObj>) {
        self.0.trace(stack);
        self.1.trace(stack);
    }
}

impl<T: Trace> Trace for [T] {
    fn trace(&self, stack: &mut Vec<RawObj>) {
        for x in self {
            x.trace(stack);
        }
    }
}

impl<T: Trace> Trace for Vec<T> {
    fn trace(&self, stack: &mut Vec<RawObj>) {
        for x in self {
            x.trace(stack);
        }
    }
}

impl<T: Trace> Trace for std::collections::VecDeque<T> {
    fn trace(&self, stack: &mut Vec<RawObj>) {
        for x in self {
            x.trace(stack);
        }
    }
}

impl<K: Trace, V: Trace> Trace for HashMap<K, V> {
    fn trace(&self, stack: &mut Vec<RawObj>) {
        for key in self.keys() {
            key.trace(stack);
        }
        for value in self.values() {
            value.trace(stack);
        }
    }
}

impl<T: Trace> Trace for HashSet<T> {
    fn trace(&self, stack: &mut Vec<RawObj>) {
        for x in self {
            x.trace(stack);
        }
    }
}

impl<T: Trace> Trace for Option<T> {
    fn trace(&self, stack: &mut Vec<RawObj>) {
        if let Some(x) = self.as_ref() {
            x.trace(stack);
        }
    }
}

#[cfg(test)]
mod test {
    use super::super::super::gc::{Context, RootSet};
    use super::*;
    use rune_core::macros::root;

    struct Foo(u64);
    impl Trace for Foo {
        fn trace(&self, _stack: &mut Vec<RawObj>) {
            assert!(self.0 == 7);
        }
    }

    #[test]
    fn test_trace_root() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        let foo = Foo(7);
        assert_eq!(roots.roots.borrow().len(), 0);
        {
            root!(_root, foo, cx);
            assert_eq!(roots.roots.borrow().len(), 1);
        }
        assert_eq!(roots.roots.borrow().len(), 0);
    }
}
