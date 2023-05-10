use super::{Gc, RawObj, TagType, WithLifetime};
use crate::core::gc::{GcManaged, GcMark, Trace};
use std::{fmt::Display, sync::Mutex};
use text_buffer::Buffer as TextBuffer;

#[derive(Debug)]
#[allow(dead_code)]
struct BufferData {
    name: String,
    file_name: String,
    text: TextBuffer,
}

#[derive(Debug)]
pub(crate) struct Buffer {
    text_buffer: Mutex<Option<BufferData>>,
}

impl PartialEq for Buffer {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl Eq for Buffer {}

impl Display for Buffer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let data = self.text_buffer.lock().unwrap();
        let name = match data.as_ref() {
            Some(buf) => &buf.name,
            None => "deleted buffer",
        };
        write!(f, "#<{name}>")
    }
}

impl Trace for Buffer {
    fn trace(&self, _v: &mut Vec<RawObj>) {
        todo!()
    }
}

impl GcManaged for Buffer {
    fn get_mark(&self) -> &GcMark {
        panic!("Buffer does not use GcMark")
    }
}

impl<'old, 'new> Buffer {
    pub(in crate::core) fn clone_in<const C: bool>(
        &'old self,
        _: &'new crate::core::gc::Block<C>,
    ) -> Gc<&'new Buffer> {
        unsafe { self.with_lifetime().tag() }
    }
}
