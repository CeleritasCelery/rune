use super::{GcState, Trace};
use std::{
    cell::Cell,
    fmt,
    mem::ManuallyDrop,
    ops::{Deref, DerefMut},
};

union GcHeader {
    header: ManuallyDrop<HeaderData>,
    fwd_ptr: *mut u8,
}

unsafe impl Send for GcHeader {}

impl GcHeader {
    const fn new(marked: bool) -> Self {
        GcHeader { header: ManuallyDrop::new(HeaderData::new(marked)) }
    }

    fn is_present(&self) -> bool {
        unsafe { self.header.is_present == HeaderData::PRESENT }
    }

    fn get_header(&self) -> Result<&HeaderData, *mut u8> {
        if self.is_present() {
            Ok(unsafe { &*self.header })
        } else {
            Err(unsafe { self.fwd_ptr })
        }
    }
}

impl fmt::Debug for GcHeader {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.get_header() {
            Ok(header) => write!(f, "GcHeader {{ header: {:?} }}", header),
            Err(ptr) => write!(f, "GcHeader {{ fwd_ptr: {:?} }}", ptr),
        }
    }
}

// Layout if very important here, we need to make sure that the is_present bit
// is the first bit in the struct. That way we can use it to check if it is a
// header data or a forwarding pointer. This works because an object is always
// aligned to 8 bytes, so the first 3 bits will be 0 if it is a forwarding
// pointer.
#[repr(C, align(8))]
#[derive(Debug)]
struct HeaderData {
    is_present: u8,
    marked: Cell<bool>,
}

impl HeaderData {
    const PRESENT: u8 = 1;
    const fn new(marked: bool) -> Self {
        Self { is_present: Self::PRESENT, marked: Cell::new(marked) }
    }
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
    pub(in crate::core) fn new(data: T, constant: bool) -> Self {
        Self {
            // if the block is const, mark the object so it will not be traced
            header: GcHeader::new(constant),
            data,
        }
    }

    /// Allocate a new object that is not managed by the garbage collector. This
    /// is used for "pure" storage of objects that are allocated at the start of
    /// the program and never freed. Only used for the Symbol map at the moment.
    pub(in crate::core) const fn new_pure(data: T) -> Self {
        Self { header: GcHeader::new(true), data }
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
        self.header.get_header().unwrap().marked.get()
    }

    fn mark(&self) {
        self.header.get_header().unwrap().marked.set(true);
    }

    fn unmark(&self) {
        self.header.get_header().unwrap().marked.set(false);
    }
}

impl<T: Trace> Trace for GcHeap<T> {
    fn trace(&self, state: &mut GcState) {
        // This is type is only one that contains mark bits, so it is where we
        // actually mark the object
        if !self.is_marked() {
            self.mark();
            self.data.trace(state);
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

impl<T: fmt::Display> fmt::Display for GcHeap<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.data)
    }
}
