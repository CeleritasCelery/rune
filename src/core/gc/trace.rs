use std::cell::RefCell;

use super::super::object::RawObj;
use crate::core::object::{Gc, Object};
use rune_core::hashmap::{HashMap, HashSet};

/// A trait for owned types that can be traced by the garbage collector. This should be implemented
/// by any type that will hold references to GC managed objects.
pub(crate) trait Trace {
    fn trace(&self, state: &mut GcState);
}

/// A trait for types that are pointers to GC objects. This is a separate trait from `Trace` because
/// pointers will need to be updated during garbage collection (with the `GcMoveable` trait). Derive
/// `Trace` will not handle use `GcMoveable` to move the object to the new space, so the pointer
/// will not get updated. This trait is used by the `Slot` type to update pointers during garbage
/// collection.
pub(crate) trait TracePtr {
    fn trace_ptr(&self, state: &mut GcState);
}

pub(crate) struct GcState {
    stack: Vec<RawObj>,
    pub(in crate::core) to_space: bumpalo::Bump,
}

impl GcState {
    pub fn new() -> Self {
        GcState { stack: Vec::new(), to_space: bumpalo::Bump::new() }
    }

    pub fn push(&mut self, obj: Object) {
        self.stack.push(Gc::into_raw(obj));
    }

    pub fn trace_stack(&mut self) {
        while let Some(raw) = self.stack.pop() {
            let obj = unsafe { Object::from_raw(raw) };
            obj.trace_ptr(self);
        }
    }
}

impl<T: Trace> TracePtr for &T {
    fn trace_ptr(&self, state: &mut GcState) {
        (*self).trace(state);
    }
}

impl Trace for usize {
    fn trace(&self, _: &mut GcState) {}
}

impl Trace for u64 {
    fn trace(&self, _: &mut GcState) {}
}

impl Trace for i64 {
    fn trace(&self, _: &mut GcState) {}
}

impl<T: Trace, U: Trace> Trace for (T, U) {
    fn trace(&self, state: &mut GcState) {
        self.0.trace(state);
        self.1.trace(state);
    }
}

impl<T: Trace> Trace for [T] {
    fn trace(&self, state: &mut GcState) {
        for x in self {
            x.trace(state);
        }
    }
}

impl<T: Trace, const N: usize> Trace for [T; N] {
    fn trace(&self, state: &mut GcState) {
        for x in self {
            x.trace(state);
        }
    }
}

impl<T: Trace> Trace for Vec<T> {
    fn trace(&self, state: &mut GcState) {
        for x in self {
            x.trace(state);
        }
    }
}

impl<T: Trace> Trace for RefCell<T> {
    fn trace(&self, state: &mut GcState) {
        self.borrow().trace(state);
    }
}

impl<T: Trace> Trace for std::collections::VecDeque<T> {
    fn trace(&self, state: &mut GcState) {
        for x in self {
            x.trace(state);
        }
    }
}

impl<K: Trace, V: Trace> Trace for HashMap<K, V> {
    fn trace(&self, state: &mut GcState) {
        for key in self.keys() {
            key.trace(state);
        }
        for value in self.values() {
            value.trace(state);
        }
    }
}

impl<T: Trace> Trace for HashSet<T> {
    fn trace(&self, state: &mut GcState) {
        for x in self {
            x.trace(state);
        }
    }
}

impl<T: Trace> Trace for Option<T> {
    fn trace(&self, state: &mut GcState) {
        if let Some(x) = self.as_ref() {
            x.trace(state);
        }
    }
}

#[cfg(test)]
mod test {
    use super::super::super::gc::{Context, RootSet};
    use super::*;
    use rune_core::macros::root;

    #[derive(Default)]
    struct Foo(u64);
    impl Trace for Foo {
        fn trace(&self, _state: &mut GcState) {
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
            root!(_root, init(foo), cx);
            assert_eq!(roots.roots.borrow().len(), 1);
        }
        assert_eq!(roots.roots.borrow().len(), 0);
    }
}
