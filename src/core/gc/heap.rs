use std::{
    cell::Cell,
    fmt::Display,
    ops::{Deref, DerefMut},
};

use crate::core::object::RawObj;

use super::{Block, GcManaged, GcMark, Trace};

// align to 64-bit boundaries
#[repr(align(8))]
#[derive(Debug)]
struct GcHeader {
    marked: GcMark,
    #[allow(dead_code)]
    size: u32,
}

unsafe impl Sync for GcHeader {}

#[repr(C)]
#[derive(Debug)]
pub(crate) struct GcHeap<T: ?Sized> {
    header: GcHeader,
    data: T,
}

impl<T> GcHeap<T> {
    pub(in crate::core) fn new<const CONST: bool>(data: T, _: &Block<CONST>) -> Self {
        Self {
            header: GcHeader {
                // if the block is const, mark the object so it will not be traced
                marked: GcMark(Cell::new(CONST)),
                size: std::mem::size_of::<T>() as u32,
            },
            data,
        }
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

impl<T> GcManaged for GcHeap<T> {
    fn get_mark(&self) -> &GcMark {
        &self.header.marked
    }
}

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
