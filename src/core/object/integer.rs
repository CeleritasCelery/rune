use super::{CloneIn, IntoObject};
use crate::core::gc::{Block, GcHeap, GcState, Trace};
use crate::derive_GcMoveable;
use num_bigint::BigInt;
use rune_macros::Trace;
use std::fmt::{Debug, Display};

#[derive(PartialEq, Eq, Trace)]
pub (crate) struct LispInteger(GcHeap<FlexInt>);

/// A wrapper for integer type
/// The type is represented by primitive but can fallback to bigint on overflow
#[derive(PartialEq, Eq)]
pub (crate) enum FlexInt {
    /// Represents the small fixed size type
    Fixed(i64),
    /// Represents the big dyanmically sized type
    Big(BigInt),
}

derive_GcMoveable!(LispInteger);

impl Trace for FlexInt {
    fn trace(&self, _: &mut GcState) {}
}

impl<'new> CloneIn<'new, &'new FlexInt> for FlexInt {
    fn clone_in<const C: bool>(&self, bk: &'new Block<C>) -> super::Gc<&'new Self> {
        todo!()
    }
}