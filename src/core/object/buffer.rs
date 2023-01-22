use std::ptr::NonNull;


pub(crate) struct Buffer {
    ptr: NonNull<u8>,
    capacity: usize,
    gap: usize,
    len: usize,
}
