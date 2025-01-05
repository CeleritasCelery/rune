use super::{CloneIn, IntoObject};
use crate::core::gc::{AllocState, Block, GcHeap, GcState, Markable, Trace};
use newtype_derive_2018::*;
use std::cell::Cell;
use std::fmt::{Debug, Display};
use std::ops::Deref;
use std::ptr::NonNull;

pub(crate) type GcString<'a> = bumpalo::collections::String<'a>;
pub(crate) struct LispString(GcHeap<LispStringInner>);

// This type needs to be this complex due to to string mutation.
//
// Case 1: The new char is the same utf8 size as the old one:
// We can update the string in place using the mutable pointer.
//
// Case 2: The new char is a different size:
// Need to allocate a new string and update the cell to point to that.
struct LispStringInner(Cell<*mut str>);

impl Markable for LispString {
    type Value = std::ptr::NonNull<LispString>;

    fn move_value(&self, to_space: &bumpalo::Bump) -> Option<(Self::Value, bool)> {
        match self.0.allocation_state() {
            AllocState::Forwarded(f) => Some((f.cast::<Self>(), false)),
            AllocState::Global => None,
            AllocState::Unmoved => {
                let ptr = {
                    let mut new = GcString::from_str_in(self, to_space);
                    let lisp_str = unsafe { LispString::new(new.as_mut_str(), false) };
                    std::mem::forget(new);
                    let alloc = to_space.alloc(lisp_str);
                    NonNull::from(alloc)
                };
                self.0.forward(ptr.cast::<u8>());
                Some((ptr, true))
            }
        }
    }
}

impl Trace for LispString {
    fn trace(&self, _state: &mut GcState) {}
}

impl Debug for LispString {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        Debug::fmt(self.inner(), f)
    }
}

impl Display for LispString {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut output = String::new();
        for c in self.inner().chars() {
            match c {
                '\\' => output.push_str("\\\\"),
                '"' => output.push_str("\\\""),
                c => output.push(c),
            }
        }
        Display::fmt(&output, f)
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
    pub(in crate::core) unsafe fn new(string: *mut str, constant: bool) -> Self {
        Self(GcHeap::new(LispStringInner(Cell::new(string)), constant))
    }

    pub(crate) fn inner(&self) -> &str {
        unsafe { &*self.0 .0.get() }
    }
}

impl LispString {
    pub(crate) fn len(&self) -> usize {
        self.chars().count()
    }

    pub(crate) fn clear(&self) {
        let inner_mut_str = unsafe { &mut *self.0 .0.get() };
        for byte in unsafe { inner_mut_str.as_bytes_mut().iter_mut() } {
            *byte = b'\0';
        }
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

pub(crate) struct ByteString(GcHeap<*mut [u8]>);
type ByteVec<'a> = bumpalo::collections::Vec<'a, u8>;

impl Deref for ByteString {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        self.inner()
    }
}

impl Markable for ByteString {
    type Value = std::ptr::NonNull<ByteString>;

    fn move_value(&self, to_space: &bumpalo::Bump) -> Option<(Self::Value, bool)> {
        match self.0.allocation_state() {
            AllocState::Forwarded(f) => Some((f.cast::<Self>(), false)),
            AllocState::Global => None,
            AllocState::Unmoved => {
                let ptr = {
                    let mut new = ByteVec::new_in(to_space);
                    new.extend_from_slice(self.inner());
                    let byte_string = ByteString::new(new.as_mut_slice(), false);
                    std::mem::forget(new);
                    let alloc = to_space.alloc(byte_string);
                    NonNull::from(alloc)
                };
                self.0.forward(ptr.cast::<u8>());
                Some((ptr, true))
            }
        }
    }
}

impl PartialEq for ByteString {
    fn eq(&self, other: &Self) -> bool {
        self.inner() == other.inner()
    }
}

impl Eq for ByteString {}

impl Trace for ByteString {
    fn trace(&self, _state: &mut GcState) {}
}

impl ByteString {
    pub(in crate::core) fn new(string: *mut [u8], constant: bool) -> Self {
        Self(GcHeap::new(string, constant))
    }

    pub(crate) fn inner(&self) -> &[u8] {
        unsafe { &**self.0 }
    }
}

impl<'new> CloneIn<'new, &'new Self> for ByteString {
    fn clone_in<const C: bool>(&self, bk: &'new Block<C>) -> super::Gc<&'new Self> {
        // TODO: Allocate in the non-gc heap here
        (**self).to_vec().into_obj(bk)
    }
}

impl Display for ByteString {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
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
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::core::gc::{Context, RootSet};
    use rune_core::macros::root;

    #[test]
    fn test_string_aliasing() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        let s1 = cx.add(String::from("hello"));
        let mut s2 = cx.string_with_capacity(5);
        s2.push_str("hello");
        let s2 = cx.add(s2);
        assert_eq!(s1, s2);
        root!(s1, cx);
        root!(s2, cx);
        cx.garbage_collect(true);
        assert_eq!(s1, s2);
    }

    #[test]
    fn test_byte_string_aliasing() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        let s1 = cx.add(vec![1u8, 2, 3]);
        let s2 = cx.add(vec![1u8, 2, 3]);
        let s2 = cx.add(s2);
        assert_eq!(s1, s2);
        root!(s1, cx);
        root!(s2, cx);
        cx.garbage_collect(true);
        assert_eq!(s1, s2);
    }
}
