use super::{CloneIn, IntoObject};
use crate::core::gc::{Block, GcHeap, GcState, Trace};
use crate::NewtypeMarkable;
use bumpalo::collections::String as GcString;
use macro_attr_2018::macro_attr;
use newtype_derive_2018::*;
use rune_macros::Trace;
use std::cell::Cell;
use std::fmt::{Debug, Display};
use std::ops::Deref;

macro_attr! {
    #[derive(NewtypeMarkable!, Trace)]
    pub(crate) struct LispString(GcHeap<LispStringInner>);
}

struct LispStringInner(Cell<*mut str>);

impl LispStringInner {
    fn get_str(&self) -> &str {
        unsafe { &*self.0.get() }
    }
}

impl Trace for LispStringInner {
    fn trace(&self, state: &mut GcState) {
        let slice = self.get_str();
        let new = state.to_space.alloc_str(slice);
        self.0.set(new);
    }
}

impl Debug for LispString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(self.inner(), f)
    }
}

impl Display for LispString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self.inner(), f)
    }
}

impl PartialEq for LispString {
    fn eq(&self, other: &Self) -> bool {
        self.inner() == other.inner()
    }
}

impl Eq for LispString {}

impl PartialEq<str> for LispString {
    fn eq(&self, other: &str) -> bool {
        self.inner() == other
    }
}

impl Deref for LispString {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.inner()
    }
}

impl LispString {
    pub(in crate::core) unsafe fn new(string: *const str, constant: bool) -> Self {
        Self(GcHeap::new(LispStringInner(Cell::new(string as *mut str)), constant))
    }

    pub(crate) fn inner(&self) -> &str {
        self.0.get_str()
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
        GcString::from_str_in(self.inner(), &bk.objects).into_obj(bk)
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
    #[derive(PartialEq, Eq, NewtypeDeref!, NewtypeMarkable!, Trace)]
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
