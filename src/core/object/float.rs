use super::{CloneIn, IntoObject, RawObj};
use crate::core::gc::{Block, GcHeap, Trace};
use crate::Markable;
use macro_attr_2018::macro_attr;
use newtype_derive_2018::*;
use rune_macros::Trace;
use std::fmt::{Debug, Display};

macro_attr! {
    /// A wrapper type for floats to work around issues with Eq. Rust only allows
    /// types to be used in match statements if they derive Eq. Even if you never
    /// actually use that field in a match. So we need a float wrapper that
    /// implements that trait.
    #[derive(PartialEq, NewtypeDeref!, Markable!, Trace)]
    pub(crate) struct LispFloat(GcHeap<f64>);
}

impl LispFloat {
    pub fn new<const C: bool>(float: f64, bk: &Block<C>) -> Self {
        LispFloat(GcHeap::new(float, bk))
    }
}

impl Trace for f64 {
    fn trace(&self, _: &mut Vec<RawObj>) {}
}

impl Eq for LispFloat {}

impl<'new> CloneIn<'new, &'new LispFloat> for LispFloat {
    fn clone_in<const C: bool>(&self, bk: &'new Block<C>) -> super::Gc<&'new Self> {
        (**self).into_obj(bk)
    }
}

impl Display for LispFloat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let float = **self;
        if float.fract() == 0.0_f64 {
            write!(f, "{float:.1}")
        } else {
            write!(f, "{float}")
        }
    }
}

impl Debug for LispFloat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}
