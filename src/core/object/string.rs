use crate::core::gc::{GcManaged, GcMark};
use anyhow::Result;
use bstr::{BStr, BString, ByteSlice};
use std::{
    fmt::{Debug, Display},
    ops::Deref,
};

#[derive(PartialEq)]
pub(crate) struct LispString {
    gc: GcMark,
    string: StrType,
}

#[derive(Debug, PartialEq)]
pub(crate) enum StrType {
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
}

impl From<String> for LispString {
    fn from(value: String) -> Self {
        Self {
            gc: GcMark::default(),
            string: StrType::String(value),
        }
    }
}

impl From<Vec<u8>> for LispString {
    fn from(value: Vec<u8>) -> Self {
        Self {
            gc: GcMark::default(),
            string: StrType::BString(BString::from(value)),
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
            StrType::BString(s) => write!(f, "\"{s}\""),
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

impl Clone for LispString {
    fn clone(&self) -> Self {
        match &self.string {
            StrType::String(s) => Self::from(s.clone()),
            StrType::BString(s) => Self::from(s.as_bytes().to_vec()),
        }
    }
}
