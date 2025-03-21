use super::{CloneIn, IntoObject};
use crate::core::gc::{Block, GcHeap, GcState, Trace};
use crate::derive_GcMoveable;
use num_bigint::BigInt;
use std::fmt::Display;
use std::ops::Deref;

// #[derive(PartialEq, Eq, Trace)]
// pub(crate) struct LispInteger(GcHeap<FlexInt>);

#[derive(PartialEq, Eq)]
pub(crate) struct LispBigInt(GcHeap<BigInt>);

/// A wrapper for integer type
/// The type is represented by primitive but can fallback to bigint on overflow
#[derive(PartialEq, Eq)]
pub(crate) enum FlexInt {
    /// Represents the small fixed size type
    Fixed(i64),
    /// Represents the big dyanmically sized type
    Big(BigInt),
}

// derive_GcMoveable!(LispInteger);
derive_GcMoveable!(LispBigInt);

impl<'new> CloneIn<'new, &'new Self> for LispBigInt {
    fn clone_in<const C: bool>(&self, bk: &'new Block<C>) -> super::Gc<&'new Self> {
        let big_int= self.0.clone();
        big_int.into_obj(bk)
    }
}

// impl<'new> CloneIn<'new, &'new Self> for LispInteger {
//     fn clone_in<const C: bool>(&self, bk: &'new Block<C>) -> super::Gc<&'new Self> {
//         match &*self.0 {
//             FlexInt::Fixed(n) => FlexInt::Fixed(*n).into_obj(bk),
//             FlexInt::Big(big_int) => FlexInt::Big(big_int.clone()).into_obj(bk),
//         }
//     }
// }

impl LispBigInt {
    pub fn new(big_int: BigInt, constant: bool) -> Self {
        LispBigInt(GcHeap::new(big_int, constant))
    }
}

impl Display for LispBigInt {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Deref for LispBigInt {
    type Target = BigInt;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Trace for LispBigInt {
    fn trace(&self, _: &mut GcState) {}
}

// impl FlexInt {}


// impl<'new> CloneIn<'new, &'new Self> for LispInteger {
//     fn clone_in<const C: bool>(&self, bk: &'new Block<C>) -> super::Gc<&'new Self> {
//         match &*self.0 {
//             FlexInt::Fixed(n) => FlexInt::Fixed(*n).into_obj(bk),
//             FlexInt::Big(big_int) => FlexInt::Big(big_int.clone()).into_obj(bk),
//         }
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
