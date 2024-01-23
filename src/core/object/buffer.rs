use super::{Gc, GcObj, Object, RawObj, TagType, WithLifetime};
use crate::core::{
    error::{Type, TypeError},
    gc::{AllocObject, Block, Context, GcHeap, Trace},
};
use anyhow::{bail, Result};
use std::{
    fmt::Display,
    ops::{Deref, DerefMut},
    sync::{Mutex, MutexGuard},
};
use text_buffer::Buffer as TextBuffer;

/// A Handle to an open buffer. Only one thread can hold this at a time.
#[derive(Debug)]
pub(crate) struct OpenBuffer<'a> {
    data: MutexGuard<'a, Option<BufferData>>,
    back_ref: &'a LispBuffer,
}

impl<'a> OpenBuffer<'a> {
    fn get(&self) -> &BufferData {
        // buffer can never be none because we check it as part of `lock`.
        self.data.as_ref().unwrap()
    }

    fn get_mut(&mut self) -> &mut BufferData {
        // buffer can never be none because we check it as part of `lock`.
        self.data.as_mut().unwrap()
    }

    // TODO: we shouldn't leave it empty
    pub(crate) fn kill(&mut self) -> bool {
        let killed = self.data.is_some();
        *self.data = None;
        killed
    }

    pub(crate) fn lisp_buffer<'ob>(&self, cx: &'ob Context) -> &'ob LispBuffer {
        cx.bind(self.back_ref)
    }

    pub(crate) fn insert(&mut self, arg: GcObj) -> Result<()> {
        match arg.untag() {
            Object::Int(i) => {
                let Ok(u_32) = i.try_into() else { bail!("{i} is an invalid char") };
                let Some(chr) = char::from_u32(u_32) else { bail!("{i} is an Invalid char") };
                self.get_mut().text.insert_char(chr);
            }
            Object::String(s) => self.get_mut().text.insert(s),
            x => bail!(TypeError::new(Type::String, x)),
        }
        Ok(())
    }

    pub(crate) fn delete(&mut self, beg: usize, end: usize) {
        self.get_mut().text.delete_range(beg, end);
    }
}

impl<'old, 'new> WithLifetime<'new> for OpenBuffer<'old> {
    type Out = OpenBuffer<'new>;

    unsafe fn with_lifetime(self) -> Self::Out {
        std::mem::transmute(self)
    }
}

impl PartialEq<str> for OpenBuffer<'_> {
    fn eq(&self, other: &str) -> bool {
        self.get().text == other
    }
}

impl Deref for OpenBuffer<'_> {
    type Target = BufferData;

    fn deref(&self) -> &Self::Target {
        self.get()
    }
}

impl DerefMut for OpenBuffer<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.get_mut()
    }
}

/// The actual data of the buffer. Buffer local variables will be stored here
/// eventually.
#[derive(Debug)]
pub(crate) struct BufferData {
    pub(crate) name: String,
    pub(crate) text: TextBuffer,
}

/// A lisp handle to a buffer. This is a just a reference type and does not give
/// access to the contents until it is locked and a `OpenBuffer` is returned.
#[derive(Debug)]
pub(crate) struct LispBufferInner {
    text_buffer: Mutex<Option<BufferData>>,
}

pub(crate) type LispBuffer = GcHeap<LispBufferInner>;

impl LispBuffer {
    pub(crate) fn create(name: String, block: &Block<true>) -> &LispBuffer {
        let new = LispBufferInner {
            text_buffer: Mutex::new(Some(BufferData { name, text: TextBuffer::new() })),
        };
        let ptr = new.alloc_obj(block);
        unsafe { &*ptr }
    }

    pub(in crate::core) fn lock(&self) -> Result<OpenBuffer<'_>> {
        let guard = self.text_buffer.lock().unwrap();
        if guard.is_none() {
            bail!("selecting deleted buffer");
        }
        Ok(OpenBuffer { data: guard, back_ref: self })
    }
}

impl PartialEq for LispBufferInner {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl PartialEq<OpenBuffer<'_>> for LispBuffer {
    fn eq(&self, other: &OpenBuffer) -> bool {
        other.back_ref == &**self
    }
}

impl PartialEq<LispBuffer> for OpenBuffer<'_> {
    fn eq(&self, other: &LispBuffer) -> bool {
        self.back_ref == &**other
    }
}

impl Eq for LispBufferInner {}

impl Display for LispBufferInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let data = self.text_buffer.lock().unwrap();
        let name = match data.as_ref() {
            Some(buf) => &buf.name,
            None => "deleted buffer",
        };
        write!(f, "#<{name}>")
    }
}

impl Trace for LispBufferInner {
    fn trace(&self, _v: &mut Vec<RawObj>) {
        // Implement once we hold gc data in the buffer
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
