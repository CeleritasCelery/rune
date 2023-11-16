use super::{CloneIn, IntoObject};
use crate::core::gc::{Block, GcManaged, GcMark};
use anyhow::Result;
use bstr::{BStr, BString, ByteSlice};
use fn_macros::Trace;
use std::{
    fmt::{Debug, Display},
    ops::Deref,
};

#[derive(PartialEq, Eq, Trace)]
pub(crate) struct LispString {
    gc: GcMark,
    #[no_trace]
    string: StrType,
}

unsafe impl Sync for LispString {}

#[derive(Debug, PartialEq, Eq)]
enum StrType {
    String(String),
    BString(BString),
}

impl LispString {
    pub(crate) fn get_char_at(&self, idx: usize) -> Option<char> {
        match &self.string {
            StrType::String(s) => s.chars().nth(idx),
            StrType::BString(s) => s.chars().nth(idx),
        }
    }

    pub(crate) fn len(&self) -> usize {
        match &self.string {
            StrType::String(s) => s.chars().count(),
            StrType::BString(s) => s.chars().count(),
        }
    }

    pub(crate) unsafe fn from_string(value: String) -> Self {
        Self { gc: GcMark::default(), string: StrType::String(value) }
    }

    pub(crate) unsafe fn from_bstring(value: Vec<u8>) -> Self {
        Self { gc: GcMark::default(), string: StrType::BString(BString::from(value)) }
    }

    pub(crate) fn is_valid_unicode(&self) -> bool {
        matches!(&self.string, StrType::String(_))
    }
}

impl<'new> CloneIn<'new, &'new Self> for LispString {
    fn clone_in<const C: bool>(&self, bk: &'new Block<C>) -> super::Gc<&'new Self> {
        match &self.string {
            StrType::String(s) => s.clone().into_obj(bk),
            StrType::BString(s) => s.as_bytes().to_vec().into_obj(bk),
        }
    }
}

impl GcManaged for LispString {
    fn get_mark(&self) -> &GcMark {
        &self.gc
    }
}

impl Deref for LispString {
    type Target = BStr;

    fn deref(&self) -> &Self::Target {
        match &self.string {
            StrType::String(s) => BStr::new(s),
            StrType::BString(s) => s.as_ref(),
        }
    }
}

impl Display for LispString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.string {
            StrType::String(s) => write!(f, "\"{s}\""),
            StrType::BString(s) => {
                let bytes: &[u8] = s.as_ref();
                write!(f, "\"{bytes:?}\"")
            }
        }
    }
}

impl Debug for LispString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

impl<'a> TryFrom<&'a LispString> for &'a str {
    type Error = anyhow::Error;

    fn try_from(value: &'a LispString) -> Result<Self, Self::Error> {
        match &value.string {
            StrType::String(s) => Ok(s),
            StrType::BString(s) => Ok(s.try_into()?),
        }
    }
}
