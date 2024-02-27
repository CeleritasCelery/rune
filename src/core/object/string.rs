use super::{CloneIn, IntoObject};
use crate::core::gc::{Block, GcHeap, GcState, Trace};
use crate::Markable;
use macro_attr_2018::macro_attr;
use newtype_derive_2018::*;
use rune_macros::Trace;
use std::fmt::{Debug, Display};

macro_attr! {
    #[derive(PartialEq, Eq, NewtypeDeref!, NewtypeDebug!, NewtypeDisplay!, Markable!, Trace)]
    pub(crate) struct LispString(GcHeap<String>);
}

impl LispString {
    pub(crate) fn new(string: String, constant: bool) -> Self {
        Self(GcHeap::new(string, constant))
    }
}

impl Trace for String {
    fn trace(&self, _: &mut GcState) {}
}

impl Trace for Vec<u8> {
    fn trace(&self, _: &mut GcState) {}
}

impl LispString {
    pub(crate) fn len(&self) -> usize {
        self.chars().count()
    }
}

impl<'new> CloneIn<'new, &'new Self> for LispString {
    fn clone_in<const C: bool>(&self, bk: &'new Block<C>) -> super::Gc<&'new Self> {
        (**self).clone().into_obj(bk)
    }
}

impl AsRef<str> for LispString {
    fn as_ref(&self) -> &str {
        self
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

macro_attr! {
    #[derive(PartialEq, Eq, NewtypeDeref!, Markable!, Trace)]
    pub(crate) struct ByteString(GcHeap<Vec<u8>>);
}

impl ByteString {
    pub(crate) fn new(string: Vec<u8>, constant: bool) -> Self {
        Self(GcHeap::new(string, constant))
    }
}

impl<'new> CloneIn<'new, &'new Self> for ByteString {
    fn clone_in<const C: bool>(&self, bk: &'new Block<C>) -> super::Gc<&'new Self> {
        (**self).to_vec().into_obj(bk)
    }
}

impl Display for ByteString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for byte in &**self {
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
