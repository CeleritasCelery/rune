use std::{
    fmt::Display,
    ops::{Deref, DerefMut},
};

use crate::core::object::RawObj;

use super::{GcManaged, GcMark, Trace};

// align to 64-bit boundaries
#[repr(align(8))]
#[derive(Debug)]
struct GcHeader {
    marked: GcMark,
    #[allow(dead_code)]
    size: u32,
}

#[repr(C)]
#[derive(Debug)]
pub(crate) struct GcHeap<T: ?Sized> {
    header: GcHeader,
    data: T,
}

impl<T> GcHeap<T> {
    pub(in crate::core) fn new(data: T) -> Self {
        Self {
            header: GcHeader { marked: GcMark::default(), size: std::mem::size_of::<T>() as u32 },
            data,
        }
    }
}

impl<T: Trace> GcHeap<T> {
    pub(in crate::core) fn trace_mark(&self, stack: &mut Vec<RawObj>) {
        self.mark();
        self.data.trace(stack);
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
