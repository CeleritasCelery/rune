use super::{GcState, Trace};
use std::{
    alloc::Layout,
    cell::{Cell, UnsafeCell},
    fmt,
    mem::ManuallyDrop,
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

union GcHeader {
    header: ManuallyDrop<HeaderData>,
    fwd_ptr: NonNull<u8>,
}

unsafe impl Send for GcHeader {}

impl GcHeader {
    const fn new(marked: bool) -> Self {
        GcHeader { header: ManuallyDrop::new(HeaderData::new(marked)) }
    }

    fn is_present(&self) -> bool {
        unsafe { self.header.is_present == HeaderData::PRESENT }
    }

    fn get_header(&self) -> Result<&HeaderData, NonNull<u8>> {
        if self.is_present() {
            Ok(unsafe { &*self.header })
        } else {
            Err(unsafe { self.fwd_ptr })
        }
    }
}

impl fmt::Debug for GcHeader {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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

/// A block of memory allocated on the heap that is managed by the garbage collector.
#[repr(C)]
#[derive(Debug)]
pub(crate) struct GcHeap<T: ?Sized> {
    header: UnsafeCell<GcHeader>,
    data: T,
}

unsafe impl<T: Sync> Sync for GcHeap<T> {}

impl<T> GcHeap<T> {
    pub(in crate::core) const fn new(data: T, constant: bool) -> Self {
        Self {
            // if the block is const, mark the object so it will not be traced
            header: UnsafeCell::new(GcHeader::new(constant)),
            data,
        }
    }

    /// Allocate a new object that is not managed by the garbage collector. This
    /// is used for "pure" storage of objects that are allocated at the start of
    /// the program and never freed. Only used for the Symbol map at the moment.
    pub(in crate::core) const fn new_pure(data: T) -> Self {
        Self::new(data, true)
    }

    fn header(&self) -> &GcHeader {
        unsafe { &*self.header.get() }
    }

    pub(in crate::core) fn forward(&self, fwd_ptr: NonNull<u8>) {
        let header = unsafe { &mut *self.header.get() };
        header.fwd_ptr = fwd_ptr;
    }

    pub(in crate::core) fn allocation_state(&self) -> AllocState {
        match self.header().get_header() {
            Ok(header) => {
                if header.marked.get() {
                    AllocState::Global
                } else {
                    AllocState::Unmoved
                }
            }
            Err(fwd) => AllocState::Forwarded(fwd),
        }
    }

    fn is_marked(&self) -> bool {
        self.header().get_header().unwrap().marked.get()
    }
}

pub(in crate::core) enum AllocState {
    Forwarded(NonNull<u8>),
    Global,
    Unmoved,
}

pub(in crate::core) trait Markable {
    type Value;
    fn move_value(&self, _to_space: &bumpalo::Bump) -> Option<(Self::Value, bool)> {
        None
    }
}

impl<'a, T: Markable<Value = NonNull<T>>> Markable for &'a T {
    type Value = &'a T;

    fn move_value(&self, to_space: &bumpalo::Bump) -> Option<(Self::Value, bool)> {
        let val = (*self).move_value(to_space);
        val.map(|(ptr, moved)| (unsafe { ptr.as_ref() }, moved))
    }
}

#[macro_export]
macro_rules! derive_markable {
    ($name:ident) => {
        impl $crate::core::gc::Markable for $name {
            type Value = std::ptr::NonNull<Self>;

            fn move_value(&self, to_space: &bumpalo::Bump) -> Option<(Self::Value, bool)> {
                match self.0.move_value(to_space) {
                    Some((ptr, moved)) => Some((ptr.cast::<Self>(), moved)),
                    None => None,
                }
            }
        }
    };
}

impl<T> Markable for GcHeap<T> {
    type Value = NonNull<Self>;

    fn move_value(&self, to_space: &bumpalo::Bump) -> Option<(Self::Value, bool)> {
        use std::ptr;
        match self.header().get_header() {
            Ok(header) => {
                if header.marked.get() {
                    // The object is global and should not be moved
                    return None;
                }
                // move to to_space
                let layout = Layout::for_value(self);
                let to_ptr = to_space.alloc_layout(layout);
                unsafe {
                    let src = ptr::from_ref(self);
                    let dst = to_ptr.cast::<Self>().as_ptr();
                    ptr::copy_nonoverlapping(src, dst, 1);
                }
                // write forwarding pointer
                self.forward(to_ptr);
                // return new address
                Some((to_ptr.cast::<Self>(), true))
            }
            Err(fwd) => Some((fwd.cast::<Self>(), false)),
        }
    }
}

impl<T: Trace> Trace for GcHeap<T> {
    fn trace(&self, state: &mut GcState) {
        // This is type is only one that contains mark bits, so it is where we
        // actually mark the object
        if !self.is_marked() {
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
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", &self.data)
    }
}
