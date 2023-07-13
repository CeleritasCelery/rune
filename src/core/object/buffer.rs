use super::{Gc, GcObj, Object, RawObj, TagType, WithLifetime};
use crate::core::{
    error::{Type, TypeError},
    gc::{AllocObject, Block, GcManaged, GcMark, Trace},
};
use anyhow::{bail, Result};
use std::{
    fmt::Display,
    sync::{Mutex, MutexGuard},
};
use text_buffer::Buffer as TextBuffer;

#[derive(Debug)]
pub(crate) struct Buffer<'a> {
    data: MutexGuard<'a, Option<BufferData>>,
}

impl<'a> Buffer<'a> {
    fn new(data: MutexGuard<'a, Option<BufferData>>) -> Result<Self> {
        if data.is_none() {
            bail!("selecting deleted buffer");
        }
        Ok(Self { data })
    }

    fn get(&self) -> &BufferData {
        // buffer can never be none because we check it as part of `new`. Could
        // make this unchecked at some point.
        self.data.as_ref().unwrap()
    }

    fn get_mut(&mut self) -> &mut BufferData {
        // buffer can never be none because we check it as part of `new`. Could
        // make this unchecked at some point.
        self.data.as_mut().unwrap()
    }

    pub(crate) fn insert(&mut self, arg: GcObj) -> Result<()> {
        match arg.untag() {
            Object::Int(i) => {
                let Ok(u_32) = i.try_into() else { bail!("{i} is an invalid char") };
                let Some(chr) = char::from_u32(u_32) else { bail!("{i} is an Invalid char") };
                self.get_mut().text.insert_char(chr);
            }
            Object::String(s) => self.get_mut().text.insert(s.try_into()?),
            x => bail!(TypeError::new(Type::String, x)),
        }
        Ok(())
    }

    pub(crate) fn delete(&mut self, beg: usize, end: usize) {
        self.get_mut().text.delete_range(beg, end);
    }
}

impl<'old, 'new> WithLifetime<'new> for Buffer<'old> {
    type Out = Buffer<'new>;

    unsafe fn with_lifetime(self) -> Self::Out {
        std::mem::transmute(self)
    }
}

impl PartialEq<str> for Buffer<'_> {
    fn eq(&self, other: &str) -> bool {
        self.get().text == other
    }
}

#[derive(Debug)]
#[allow(dead_code)]
struct BufferData {
    name: String,
    text: TextBuffer,
}

#[derive(Debug)]
pub(crate) struct LispBuffer {
    text_buffer: Mutex<Option<BufferData>>,
}

impl LispBuffer {
    pub(crate) fn create(name: String, block: &Block<true>) -> &LispBuffer {
        let new = Self {
            text_buffer: Mutex::new(Some(BufferData {
                name,
                text: TextBuffer::new(),
            })),
        };
        let ptr = new.alloc_obj(block);
        unsafe { &*ptr }
    }

    pub(in crate::core) fn lock(&self) -> Result<Buffer<'_>> {
        let buffer = self.text_buffer.lock().unwrap();
        Buffer::new(buffer)
    }
}

impl PartialEq for LispBuffer {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl Eq for LispBuffer {}

impl Display for LispBuffer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let data = self.text_buffer.lock().unwrap();
        let name = match data.as_ref() {
            Some(buf) => &buf.name,
            None => "deleted buffer",
        };
        write!(f, "#<{name}>")
    }
}

impl Trace for LispBuffer {
    fn trace(&self, _v: &mut Vec<RawObj>) {
        // Implement once we hold gc data in the buffer
    }
}

impl GcManaged for LispBuffer {
    fn get_mark(&self) -> &GcMark {
        panic!("Buffer does not use GcMark")
    }
}

impl<'old, 'new> LispBuffer {
    pub(in crate::core) fn clone_in<const C: bool>(
        &'old self,
        _: &'new Block<C>,
    ) -> Gc<&'new LispBuffer> {
        unsafe { self.with_lifetime().tag() }
    }
}
