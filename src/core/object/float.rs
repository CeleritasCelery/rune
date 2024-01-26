use crate::core::gc::GcHeap;
use std::fmt::{Debug, Display};
use std::ops::Deref;

mod sealed {
    #[derive(PartialEq)]
    #[repr(transparent)]
    pub(crate) struct LispFloatInner(pub(in crate::core) f64);
}

pub(in crate::core) use sealed::LispFloatInner;

impl Eq for LispFloatInner {}

/// A wrapper type for floats to work around issues with Eq. Rust only allows
/// types to be used in match statements if they derive Eq. Even if you never
/// actually use that field in a match. So we need a float wrapper that
/// implements that trait.
pub(crate) type LispFloat = GcHeap<LispFloatInner>;

impl Deref for LispFloatInner {
    type Target = f64;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Display for LispFloatInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let float = self.0;
        if float.fract() == 0.0_f64 {
            write!(f, "{float:.1}")
        } else {
            write!(f, "{float}")
        }
    }
}

impl Debug for LispFloatInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}
