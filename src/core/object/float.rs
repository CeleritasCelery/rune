use super::{CloneIn, IntoObject};
use crate::core::gc::{Block, GcHeap, GcState, Trace};
use crate::derive_GcMoveable;
use rune_macros::Trace;
use std::fmt::{Debug, Display};

/// A wrapper type for floats to work around issues with Eq. Rust only allows
/// types to be used in match statements if they derive Eq. Even if you never
/// actually use that field in a match. So we need a float wrapper that
/// implements that trait.
#[derive(PartialEq, Trace)]
pub(crate) struct LispFloat(GcHeap<f64>);

derive_GcMoveable!(LispFloat);

impl std::ops::Deref for LispFloat {
    type Target = f64;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl LispFloat {
    pub fn new(float: f64, constant: bool) -> Self {
        LispFloat(GcHeap::new(float, constant))
    }
}

impl Trace for f64 {
    fn trace(&self, _: &mut GcState) {}
}

impl Eq for LispFloat {}

impl<'new> CloneIn<'new, &'new LispFloat> for LispFloat {
    fn clone_in<const C: bool>(&self, bk: &'new Block<C>) -> super::Gc<&'new Self> {
        (**self).into_obj(bk)
    }
}

impl Display for LispFloat {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let float = **self;
        if float.fract() == 0.0_f64 {
            write!(f, "{float:.1}")
        } else {
            write!(f, "{float}")
        }
    }
}

impl Debug for LispFloat {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{self}")
    }
}
