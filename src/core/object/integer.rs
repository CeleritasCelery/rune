use super::{CloneIn, IntoObject};
use crate::core::gc::{Block, GcHeap, GcState, Trace};
use crate::derive_GcMoveable;
use num_bigint::BigInt;
use rune_macros::Trace;
use std::fmt::{Display};

#[derive(PartialEq, Eq, Trace)]
pub(crate) struct LispInteger(GcHeap<FlexInt>);

/// A wrapper for integer type
/// The type is represented by primitive but can fallback to bigint on overflow
#[derive(PartialEq, Eq)]
pub(crate) enum FlexInt {
    /// Represents the small fixed size type
    Fixed(i64),
    /// Represents the big dyanmically sized type
    Big(BigInt),
}

derive_GcMoveable!(LispInteger);

impl LispInteger {
    pub fn new(flex_int: FlexInt, constant: bool) -> Self {
        LispInteger(GcHeap::new(flex_int, constant))
    }
}

impl Display for LispInteger {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // let flex_int = **self;
        write!(f, "")
    }
}

impl FlexInt {}

impl Trace for FlexInt {
    fn trace(&self, _: &mut GcState) {}
}

impl<'new> CloneIn<'new, &'new Self> for LispInteger {
    fn clone_in<const C: bool>(&self, bk: &'new Block<C>) -> super::Gc<&'new Self> {

        // super::Gc::<&'new Self> { self.0.clone_in(bk) }
    }
}

// impl<'new> CloneIn<'new, &'new Self> for FlexInt {
//     fn clone_in<const C: bool>(&self, bk: &'new Block<C>) -> super::Gc<&'new Self> {
//         self.clone_in(bk)
//     }
// }

macro_rules! impl_from_small_integer {
    ($($num_type:ty),+) => {
        $(
            impl From<$num_type> for FlexInt {
                fn from(value: $num_type) -> Self {
                    FlexInt::Fixed(value as i64)
                }
            }
        )+
    }
}

macro_rules! impl_from_large_integer {
    ($($num_type:ty),+) => {
        $(
            impl From<$num_type> for FlexInt {
                fn from(value: $num_type) -> Self {
                    FlexInt::Big(BigInt::from(value))
                }
            }
        )+
    }
}

// From implementations for types that fit into i64
impl_from_small_integer!(u8, u16, u32, i8, i16, i32, i64);

// From implementations for types that overflow i64
impl_from_large_integer!(u64, u128, usize, i128, isize);

impl From<BigInt> for FlexInt {
    fn from(value: BigInt) -> Self {
        FlexInt::Big(value)
    }
}
