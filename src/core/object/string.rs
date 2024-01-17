use super::{CloneIn, IntoObject};
use crate::core::gc::{Block, GcManaged, GcMark};
use rune_macros::Trace;
use std::{
    fmt::{Debug, Display},
    ops::Deref,
};

#[derive(PartialEq, Eq, Trace)]
pub(crate) struct LispString {
    gc: GcMark,
    #[no_trace]
    string: String,
}

unsafe impl Sync for LispString {}

impl LispString {
    pub(crate) fn get_char_at(&self, idx: usize) -> Option<u32> {
        self.string.chars().nth(idx).map(|c| c.into())
    }

    pub(crate) fn len(&self) -> usize {
        self.string.chars().count()
    }

    pub(crate) unsafe fn from_string(string: String) -> Self {
        Self { gc: GcMark::default(), string }
    }
}

impl<'new> CloneIn<'new, &'new Self> for LispString {
    fn clone_in<const C: bool>(&self, bk: &'new Block<C>) -> super::Gc<&'new Self> {
        self.string.clone().into_obj(bk)
    }
}

impl GcManaged for LispString {
    fn get_mark(&self) -> &GcMark {
        &self.gc
    }
}

impl Deref for LispString {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &self.string
    }
}

impl AsRef<str> for LispString {
    fn as_ref(&self) -> &str {
        &self.string
    }
}

impl Display for LispString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", &**self)
    }
}

impl Debug for LispString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

impl<'a> From<&'a LispString> for &'a str {
    fn from(value: &'a LispString) -> Self {
        value
    }
}

impl<'a> From<&'a LispString> for &'a [u8] {
    fn from(value: &'a LispString) -> Self {
        value.as_bytes()
    }
}

#[derive(PartialEq, Eq, Trace)]
pub(crate) struct ByteString {
    gc: GcMark,
    #[no_trace]
    string: Vec<u8>,
}

unsafe impl Sync for ByteString {}

impl ByteString {
    pub(super) fn new(string: Vec<u8>) -> Self {
        Self { gc: GcMark::default(), string }
    }
}

impl GcManaged for ByteString {
    fn get_mark(&self) -> &GcMark {
        &self.gc
    }
}

impl Deref for ByteString {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        &self.string
    }
}

impl<'new> CloneIn<'new, &'new Self> for ByteString {
    fn clone_in<const C: bool>(&self, bk: &'new Block<C>) -> super::Gc<&'new Self> {
        self.string.to_vec().into_obj(bk)
    }
}

impl Display for ByteString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for byte in &self.string {
            if byte.is_ascii() {
                write!(f, "{}", *byte as char)?;
            } else {
                write!(f, "\\{:03o}", byte)?;
            }
        }
        Ok(())
    }
}

impl Debug for ByteString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}
