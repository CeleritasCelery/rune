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
#[allow(dead_code)]
pub(crate) struct Buffer {
    name: String,
    text: TextBuffer,
}

impl Buffer {
    pub(crate) fn insert(&mut self, arg: GcObj) -> Result<()> {
        match arg.untag() {
            Object::Int(i) => {
                let Ok(u_32) = i.try_into() else {bail!("{i} is an invalid char")};
                let Some(chr) = char::from_u32(u_32) else {bail!("{i} is an Invalid char")};
                self.text.insert_char(chr);
            }
            Object::String(s) => self.text.insert(s.try_into()?),
            x => bail!(TypeError::new(Type::String, x)),
        }
        Ok(())
    }
}

#[derive(Debug)]
pub(crate) struct LispBuffer {
    text_buffer: Mutex<Option<Buffer>>,
}

impl LispBuffer {
    pub(crate) fn create(name: String, block: &Block<true>) -> &LispBuffer {
        let new = Self {
            text_buffer: Mutex::new(Some(Buffer {
                name,
                text: TextBuffer::new(),
            })),
        };
        let ptr = new.alloc_obj(block);
        unsafe { &*ptr }
    }

    pub(crate) fn get(&self) -> MutexGuard<'_, Option<Buffer>> {
        self.text_buffer.lock().unwrap()
    }

    pub(crate) fn is_live(&self) -> bool {
        self.text_buffer.lock().unwrap().is_some()
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
        todo!()
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
