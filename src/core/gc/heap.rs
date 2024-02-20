use std::{
    cell::Cell,
    fmt::Display,
    ops::{Deref, DerefMut},
};

use crate::core::object::RawObj;

use super::{Block, Trace};

// align to 64-bit boundaries
#[repr(align(8))]
#[derive(Debug)]
struct GcHeader {
    marked: Cell<bool>,
}

unsafe impl Sync for GcHeader {}

/// A block of memory allocated on the heap that is managed by the garbage collector.
#[repr(C)]
#[derive(Debug)]
pub(crate) struct GcHeap<T: ?Sized> {
    header: GcHeader,
    data: T,
}

impl<T> GcHeap<T> {
    pub(in crate::core) fn new<const CONST: bool>(data: T, _: &Block<CONST>) -> Self {
        Self {
            // if the block is const, mark the object so it will not be traced
            header: GcHeader { marked: Cell::new(CONST) },
            data,
        }
    }

    /// Allocate a new object that is not managed by the garbage collector. This
    /// is used for "pure" storage of objects that are allocated at the start of
    /// the program and never freed. Only used for the Symbol map at the moment.
    pub(in crate::core) const fn new_pure(data: T) -> Self {
        Self { header: GcHeader { marked: Cell::new(true) }, data }
    }
}

pub(in crate::core) trait Markable {
    fn mark(&self);
    fn unmark(&self);
    fn is_marked(&self) -> bool;
}

#[macro_export]
macro_rules! Markable {
    (() $vis:vis struct $name:ident $($tail:tt)+) => {
        impl $crate::core::gc::Markable for $name {
            fn is_marked(&self) -> bool {
                self.0.is_marked()
            }

            fn mark(&self) {
                self.0.mark()
            }

            fn unmark(&self) {
                self.0.unmark()
            }
        }
    };
}

impl<T> Markable for GcHeap<T> {
    fn is_marked(&self) -> bool {
        self.header.marked.get()
    }

    fn mark(&self) {
        self.header.marked.set(true);
    }

    fn unmark(&self) {
        self.header.marked.set(false);
    }
}

impl<T: Trace> Trace for GcHeap<T> {
    fn trace(&self, stack: &mut Vec<RawObj>) {
        self.trace_mark(stack);
    }
}

impl<T: Trace> GcHeap<T> {
    pub(in crate::core) fn trace_mark(&self, stack: &mut Vec<RawObj>) {
        if !self.is_marked() {
            self.mark();
            self.data.trace(stack);
        }
    }
}

impl<T: PartialEq> PartialEq for GcHeap<T> {
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data
    }
}

impl<T: PartialEq> PartialEq<T> for GcHeap<T> {
    fn eq(&self, other: &T) -> bool {
        self.data == *other
    }
}

impl<T: Eq> Eq for GcHeap<T> {}

impl<T> Deref for GcHeap<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T> DerefMut for GcHeap<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

impl<T: Display> Display for GcHeap<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &self.data)
    }
}
