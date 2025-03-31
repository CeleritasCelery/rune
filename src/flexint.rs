use core::fmt;
use std::{
    mem,
    ops::{
        Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Not, Rem, RemAssign, Sub, SubAssign,
    },
};

use anyhow::anyhow;
use num_bigint::BigInt;
use num_integer::Integer;
use num_traits::{
    CheckedAdd, CheckedDiv, CheckedMul, CheckedSub, FromPrimitive, Num, One, ToPrimitive, Zero,
};

use crate::{
    arith::{MAX_FIXNUM, MIN_FIXNUM, NumberValue},
    core::object::ObjectType,
};

/// A type that can represent a flexible integer, either a `BigInt` or a `i64`.
/// Keeps the data within the smallest possible type whenver possible, and moving
/// to the Big when overflow happens.
#[derive(Debug, Clone)]
pub enum FlexInt {
    S(i64),    // Represents a small integer, fits in i64
    B(BigInt), // Represents a big integer, uses BigInt for larger values
}

impl fmt::Display for FlexInt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FlexInt::S(value) => write!(f, "{}", value),
            FlexInt::B(value) => write!(f, "{}", value),
        }
    }
}

impl FlexInt {
    // Helper function to create a FlexInt from a BigInt
    fn shrink(value: BigInt) -> Self {
        value
            .to_i64()
            .filter(|&n| (MIN_FIXNUM..=MAX_FIXNUM).contains(&n))
            .map_or_else(|| FlexInt::B(value), FlexInt::S)
    }
}

impl PartialEq for FlexInt {
    fn eq(&self, other: &Self) -> bool {
        match *self {
            FlexInt::S(a) => match *other {
                FlexInt::S(b) => PartialEq::eq(&a, &b),
                FlexInt::B(ref b) => PartialEq::eq(&BigInt::from(a), b),
            },
            FlexInt::B(ref a) => match *other {
                FlexInt::S(b) => PartialEq::eq(a, &BigInt::from(b)),
                FlexInt::B(ref b) => PartialEq::eq(a, b),
            },
        }
    }
}

impl Eq for FlexInt {}

impl Ord for FlexInt {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match *self {
            FlexInt::S(a) => match *other {
                FlexInt::S(b) => Ord::cmp(&a, &b),
                FlexInt::B(ref b) => Ord::cmp(&BigInt::from(a), b),
            },
            FlexInt::B(ref a) => match *other {
                FlexInt::S(b) => Ord::cmp(a, &BigInt::from(b)),
                FlexInt::B(ref b) => Ord::cmp(a, b),
            },
        }
    }
}

impl PartialOrd for FlexInt {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Neg for FlexInt {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            FlexInt::S(v) => FlexInt::S(Neg::neg(v)),
            FlexInt::B(v) => FlexInt::B(Neg::neg(v)),
        }
    }
}

impl Not for FlexInt {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            FlexInt::S(v) => FlexInt::S(Not::not(v)),
            FlexInt::B(v) => FlexInt::B(Not::not(v)),
        }
    }
}

impl Rem for FlexInt {
    type Output = Self;

    fn rem(self, other: Self) -> Self::Output {
        match self {
            FlexInt::S(a) => match other {
                FlexInt::S(b) => FlexInt::S(Rem::rem(a, b)),
                FlexInt::B(ref b) => FlexInt::shrink(Rem::rem(a, b)),
            },
            FlexInt::B(a) => match other {
                FlexInt::S(b) => FlexInt::shrink(Rem::rem(a, b)),
                FlexInt::B(b) => FlexInt::shrink(Rem::rem(a, b)),
            },
        }
    }
}

impl Rem for &FlexInt {
    type Output = FlexInt;

    fn rem(self, other: Self) -> FlexInt {
        match self {
            FlexInt::S(a) => match other {
                FlexInt::S(b) => FlexInt::S(Rem::rem(a, b)),
                FlexInt::B(b) => FlexInt::shrink(Rem::rem(a, b)),
            },
            FlexInt::B(a) => match other {
                FlexInt::S(b) => FlexInt::shrink(Rem::rem(a, b)),
                FlexInt::B(b) => FlexInt::shrink(Rem::rem(a, b)),
            },
        }
    }
}

impl<'a> Rem<&'a FlexInt> for FlexInt {
    type Output = FlexInt;

    fn rem(self, other: &'a FlexInt) -> FlexInt {
        match self {
            FlexInt::S(a) => match other {
                FlexInt::S(b) => FlexInt::S(Rem::rem(a, b)),
                FlexInt::B(b) => FlexInt::shrink(Rem::rem(a, b)),
            },
            FlexInt::B(a) => match other {
                FlexInt::S(b) => FlexInt::shrink(Rem::rem(a, BigInt::from(*b))),
                FlexInt::B(b) => FlexInt::shrink(Rem::rem(a, b)),
            },
        }
    }
}

impl RemAssign for FlexInt {
    fn rem_assign(&mut self, other: Self) {
        *self = match *self {
            FlexInt::S(mut a) => match other {
                FlexInt::S(b) => {
                    RemAssign::rem_assign(&mut a, b);
                    FlexInt::S(a)
                }
                FlexInt::B(b) => {
                    let mut a_ = BigInt::from(a);
                    RemAssign::rem_assign(&mut a_, &b);
                    FlexInt::shrink(a_)
                }
            },
            FlexInt::B(ref mut a) => {
                let mut a_ = mem::replace(a, BigInt::zero());
                match other {
                    FlexInt::S(b) => {
                        RemAssign::rem_assign(&mut a_, BigInt::from(b));
                        FlexInt::shrink(a_)
                    }
                    FlexInt::B(b) => {
                        RemAssign::rem_assign(&mut a_, b);
                        FlexInt::shrink(a_)
                    }
                }
            }
        }
    }
}

impl Add for FlexInt {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        match self {
            FlexInt::S(a) => match other {
                FlexInt::S(b) => CheckedAdd::checked_add(&a, &b).map_or_else(
                    || FlexInt::shrink(Add::add(BigInt::from(a), BigInt::from(b))),
                    FlexInt::S,
                ),
                FlexInt::B(b) => FlexInt::shrink(Add::add(BigInt::from(a), b)),
            },
            FlexInt::B(a) => match other {
                FlexInt::S(b) => FlexInt::shrink(Add::add(a, BigInt::from(b))),
                FlexInt::B(b) => FlexInt::shrink(Add::add(a, b)),
            },
        }
    }
}

impl Add for &FlexInt {
    type Output = FlexInt;

    fn add(self, other: Self) -> Self::Output {
        match *self {
            FlexInt::S(a) => match *other {
                FlexInt::S(b) => CheckedAdd::checked_add(&a, &b).map_or_else(
                    || FlexInt::shrink(Add::add(BigInt::from(a), BigInt::from(b))),
                    FlexInt::S,
                ),
                FlexInt::B(ref b) => FlexInt::shrink(Add::add(BigInt::from(a), b)),
            },
            FlexInt::B(ref a) => match *other {
                FlexInt::S(b) => FlexInt::shrink(Add::add(a, &BigInt::from(b))),
                FlexInt::B(ref b) => FlexInt::shrink(Add::add(a, b)),
            },
        }
    }
}

impl<'a> Add<&'a FlexInt> for FlexInt {
    type Output = FlexInt;
    fn add(self, other: &'a Self) -> Self::Output {
        match self {
            FlexInt::S(a) => match other {
                FlexInt::S(b) => CheckedAdd::checked_add(&a, b).map_or_else(
                    || FlexInt::shrink(Add::add(BigInt::from(a), BigInt::from(*b))),
                    FlexInt::S,
                ),
                FlexInt::B(b) => FlexInt::shrink(Add::add(BigInt::from(a), b)),
            },
            FlexInt::B(a) => match other {
                FlexInt::S(b) => FlexInt::shrink(Add::add(a, BigInt::from(*b))),
                FlexInt::B(b) => FlexInt::shrink(Add::add(a, b)),
            },
        }
    }
}

impl Div for FlexInt {
    type Output = Self;

    fn div(self, other: Self) -> Self::Output {
        match self {
            FlexInt::S(a) => match other {
                FlexInt::S(b) => CheckedDiv::checked_div(&a, &b).map_or_else(
                    || FlexInt::shrink(Div::div(BigInt::from(a), BigInt::from(b))),
                    FlexInt::S,
                ),
                FlexInt::B(b) => FlexInt::shrink(Div::div(BigInt::from(a), b)),
            },
            FlexInt::B(a) => match other {
                FlexInt::S(b) => FlexInt::shrink(Div::div(a, BigInt::from(b))),
                FlexInt::B(b) => FlexInt::shrink(Div::div(a, b)),
            },
        }
    }
}

impl Div for &FlexInt {
    type Output = FlexInt;

    fn div(self, other: Self) -> Self::Output {
        match *self {
            FlexInt::S(a) => match *other {
                FlexInt::S(b) => CheckedDiv::checked_div(&a, &b).map_or_else(
                    || FlexInt::shrink(Div::div(BigInt::from(a), BigInt::from(b))),
                    FlexInt::S,
                ),
                FlexInt::B(ref b) => FlexInt::shrink(Div::div(BigInt::from(a), b)),
            },
            FlexInt::B(ref a) => match *other {
                FlexInt::S(b) => FlexInt::shrink(Div::div(a, &BigInt::from(b))),
                FlexInt::B(ref b) => FlexInt::shrink(Div::div(a, b)),
            },
        }
    }
}

impl<'a> Div<&'a FlexInt> for FlexInt {
    type Output = FlexInt;

    fn div(self, other: &'a Self) -> Self::Output {
        match self {
            FlexInt::S(a) => match other {
                FlexInt::S(b) => CheckedDiv::checked_div(&a, b).map_or_else(
                    || FlexInt::shrink(Div::div(BigInt::from(a), BigInt::from(*b))),
                    FlexInt::S,
                ),
                FlexInt::B(b) => FlexInt::shrink(Div::div(BigInt::from(a), b)),
            },
            FlexInt::B(a) => match other {
                FlexInt::S(b) => FlexInt::shrink(Div::div(a, BigInt::from(*b))),
                FlexInt::B(b) => FlexInt::shrink(Div::div(a, b)),
            },
        }
    }
}

impl Mul for FlexInt {
    type Output = Self;

    fn mul(self, other: Self) -> Self::Output {
        match self {
            FlexInt::S(a) => match other {
                FlexInt::S(b) => CheckedMul::checked_mul(&a, &b).map_or_else(
                    || FlexInt::shrink(Mul::mul(BigInt::from(a), BigInt::from(b))),
                    FlexInt::S,
                ),
                FlexInt::B(b) => FlexInt::shrink(Mul::mul(BigInt::from(a), b)),
            },
            FlexInt::B(a) => match other {
                FlexInt::S(b) => FlexInt::shrink(Mul::mul(a, BigInt::from(b))),
                FlexInt::B(b) => FlexInt::shrink(Mul::mul(a, b)),
            },
        }
    }
}

impl Mul for &FlexInt {
    type Output = FlexInt;

    fn mul(self, other: Self) -> Self::Output {
        match *self {
            FlexInt::S(a) => match *other {
                FlexInt::S(b) => CheckedMul::checked_mul(&a, &b).map_or_else(
                    || FlexInt::shrink(Mul::mul(BigInt::from(a), BigInt::from(b))),
                    FlexInt::S,
                ),
                FlexInt::B(ref b) => FlexInt::shrink(Mul::mul(BigInt::from(a), b)),
            },
            FlexInt::B(ref a) => match *other {
                FlexInt::S(b) => FlexInt::shrink(Mul::mul(a, &BigInt::from(b))),
                FlexInt::B(ref b) => FlexInt::shrink(Mul::mul(a, b)),
            },
        }
    }
}

impl<'a> Mul<&'a FlexInt> for FlexInt {
    type Output = FlexInt;

    fn mul(self, other: &'a Self) -> Self::Output {
        match self {
            FlexInt::S(a) => match other {
                FlexInt::S(b) => CheckedMul::checked_mul(&a, b).map_or_else(
                    || FlexInt::shrink(Mul::mul(BigInt::from(a), BigInt::from(*b))),
                    FlexInt::S,
                ),
                FlexInt::B(b) => FlexInt::shrink(Mul::mul(BigInt::from(a), b)),
            },
            FlexInt::B(a) => match other {
                FlexInt::S(b) => FlexInt::shrink(Mul::mul(a, &BigInt::from(*b))),
                FlexInt::B(b) => FlexInt::shrink(Mul::mul(a, b)),
            },
        }
    }
}

impl Sub for FlexInt {
    type Output = Self;

    fn sub(self, other: Self) -> Self::Output {
        match self {
            FlexInt::S(a) => match other {
                FlexInt::S(b) => CheckedSub::checked_sub(&a, &b).map_or_else(
                    || FlexInt::shrink(Sub::sub(BigInt::from(a), BigInt::from(b))),
                    FlexInt::S,
                ),
                FlexInt::B(b) => FlexInt::shrink(Sub::sub(BigInt::from(a), b)),
            },
            FlexInt::B(a) => match other {
                FlexInt::S(b) => FlexInt::shrink(Sub::sub(a, BigInt::from(b))),
                FlexInt::B(b) => FlexInt::shrink(Sub::sub(a, b)),
            },
        }
    }
}

impl Sub for &FlexInt {
    type Output = FlexInt;

    fn sub(self, other: Self) -> Self::Output {
        match *self {
            FlexInt::S(a) => match *other {
                FlexInt::S(b) => CheckedSub::checked_sub(&a, &b).map_or_else(
                    || FlexInt::shrink(Sub::sub(BigInt::from(a), BigInt::from(b))),
                    FlexInt::S,
                ),
                FlexInt::B(ref b) => FlexInt::shrink(Sub::sub(BigInt::from(a), b)),
            },
            FlexInt::B(ref a) => match *other {
                FlexInt::S(b) => FlexInt::shrink(Sub::sub(a, &BigInt::from(b))),
                FlexInt::B(ref b) => FlexInt::shrink(Sub::sub(a, b)),
            },
        }
    }
}

impl<'a> Sub<&'a FlexInt> for FlexInt {
    type Output = FlexInt;

    fn sub(self, other: &'a Self) -> Self::Output {
        match self {
            FlexInt::S(a) => match other {
                FlexInt::S(b) => CheckedSub::checked_sub(&a, b).map_or_else(
                    || FlexInt::shrink(Sub::sub(BigInt::from(a), BigInt::from(*b))),
                    FlexInt::S,
                ),
                FlexInt::B(b) => FlexInt::shrink(Sub::sub(BigInt::from(a), b)),
            },
            FlexInt::B(a) => match other {
                FlexInt::S(b) => FlexInt::shrink(Sub::sub(a, BigInt::from(*b))),
                FlexInt::B(b) => FlexInt::shrink(Sub::sub(a, b)),
            },
        }
    }
}

impl AddAssign for FlexInt {
    fn add_assign(&mut self, other: Self) {
        *self = match *self {
            FlexInt::S(a) => match other {
                FlexInt::S(b) => CheckedAdd::checked_add(&a, &b).map_or_else(
                    || {
                        let mut a_ = BigInt::from(a);
                        AddAssign::add_assign(&mut a_, BigInt::from(b));
                        FlexInt::shrink(a_)
                    },
                    FlexInt::S,
                ),
                FlexInt::B(b) => {
                    let mut a_ = BigInt::from(a);
                    AddAssign::add_assign(&mut a_, b);
                    FlexInt::shrink(a_)
                }
            },
            FlexInt::B(ref mut a) => {
                let mut a_ = mem::replace(a, BigInt::zero());
                match other {
                    FlexInt::S(b) => {
                        AddAssign::add_assign(&mut a_, BigInt::from(b));
                        FlexInt::shrink(a_)
                    }
                    FlexInt::B(b) => {
                        AddAssign::add_assign(&mut a_, b);
                        FlexInt::shrink(a_)
                    }
                }
            }
        }
    }
}

impl<'a> AddAssign<&'a Self> for FlexInt {
    fn add_assign(&mut self, other: &'a Self) {
        *self = match *self {
            FlexInt::S(a) => match other {
                FlexInt::S(b) => CheckedAdd::checked_add(&a, b).map_or_else(
                    || {
                        let mut a_ = BigInt::from(a);
                        AddAssign::add_assign(&mut a_, BigInt::from(*b));
                        FlexInt::shrink(a_)
                    },
                    FlexInt::S,
                ),
                FlexInt::B(b) => {
                    let mut a_ = BigInt::from(a);
                    AddAssign::add_assign(&mut a_, b);
                    FlexInt::shrink(a_)
                }
            },
            FlexInt::B(ref mut a) => {
                let mut a_ = mem::replace(a, BigInt::zero());
                match other {
                    FlexInt::S(b) => {
                        AddAssign::add_assign(&mut a_, BigInt::from(*b));
                        FlexInt::shrink(a_)
                    }
                    FlexInt::B(b) => {
                        AddAssign::add_assign(&mut a_, b);
                        FlexInt::shrink(a_)
                    }
                }
            }
        }
    }
}

impl DivAssign for FlexInt {
    fn div_assign(&mut self, other: Self) {
        *self = match *self {
            FlexInt::S(a) => match other {
                FlexInt::S(b) => CheckedDiv::checked_div(&a, &b).map_or_else(
                    || {
                        let mut a_ = BigInt::from(a);
                        DivAssign::div_assign(&mut a_, BigInt::from(b));
                        FlexInt::shrink(a_)
                    },
                    FlexInt::S,
                ),
                FlexInt::B(b) => {
                    let mut a_ = BigInt::from(a);
                    DivAssign::div_assign(&mut a_, b);
                    FlexInt::shrink(a_)
                }
            },
            FlexInt::B(ref mut a) => {
                let mut a_ = mem::replace(a, BigInt::zero());
                match other {
                    FlexInt::S(b) => {
                        DivAssign::div_assign(&mut a_, BigInt::from(b));
                        FlexInt::shrink(a_)
                    }
                    FlexInt::B(b) => {
                        DivAssign::div_assign(&mut a_, b);
                        FlexInt::shrink(a_)
                    }
                }
            }
        }
    }
}

impl<'a> DivAssign<&'a Self> for FlexInt {
    fn div_assign(&mut self, other: &'a Self) {
        *self = match *self {
            FlexInt::S(a) => match other {
                FlexInt::S(b) => CheckedDiv::checked_div(&a, b).map_or_else(
                    || {
                        let mut a_ = BigInt::from(a);
                        DivAssign::div_assign(&mut a_, BigInt::from(*b));
                        FlexInt::shrink(a_)
                    },
                    FlexInt::S,
                ),
                FlexInt::B(b) => {
                    let mut a_ = BigInt::from(a);
                    DivAssign::div_assign(&mut a_, b);
                    FlexInt::shrink(a_)
                }
            },
            FlexInt::B(ref mut a) => {
                let mut a_ = mem::replace(a, BigInt::zero());
                match other {
                    FlexInt::S(b) => {
                        DivAssign::div_assign(&mut a_, BigInt::from(*b));
                        FlexInt::shrink(a_)
                    }
                    FlexInt::B(b) => {
                        DivAssign::div_assign(&mut a_, b);
                        FlexInt::shrink(a_)
                    }
                }
            }
        }
    }
}

impl MulAssign for FlexInt {
    fn mul_assign(&mut self, other: Self) {
        *self = match *self {
            FlexInt::S(a) => match other {
                FlexInt::S(b) => CheckedMul::checked_mul(&a, &b).map_or_else(
                    || {
                        let mut a_ = BigInt::from(a);
                        MulAssign::mul_assign(&mut a_, BigInt::from(b));
                        FlexInt::shrink(a_)
                    },
                    FlexInt::S,
                ),
                FlexInt::B(b) => {
                    let mut a_ = BigInt::from(a);
                    MulAssign::mul_assign(&mut a_, b);
                    FlexInt::shrink(a_)
                }
            },
            FlexInt::B(ref mut a) => {
                let mut a_ = mem::replace(a, BigInt::zero());
                match other {
                    FlexInt::S(b) => {
                        MulAssign::mul_assign(&mut a_, BigInt::from(b));
                        FlexInt::shrink(a_)
                    }
                    FlexInt::B(b) => {
                        MulAssign::mul_assign(&mut a_, b);
                        FlexInt::shrink(a_)
                    }
                }
            }
        }
    }
}

impl<'a> MulAssign<&'a Self> for FlexInt {
    fn mul_assign(&mut self, other: &'a Self) {
        *self = match *self {
            FlexInt::S(a) => match other {
                FlexInt::S(b) => CheckedMul::checked_mul(&a, b).map_or_else(
                    || {
                        let mut a_ = BigInt::from(a);
                        MulAssign::mul_assign(&mut a_, BigInt::from(*b));
                        FlexInt::shrink(a_)
                    },
                    FlexInt::S,
                ),
                FlexInt::B(b) => {
                    let mut a_ = BigInt::from(a);
                    MulAssign::mul_assign(&mut a_, b);
                    FlexInt::shrink(a_)
                }
            },
            FlexInt::B(ref mut a) => {
                let mut a_ = mem::replace(a, BigInt::zero());
                match other {
                    FlexInt::S(b) => {
                        MulAssign::mul_assign(&mut a_, BigInt::from(*b));
                        FlexInt::shrink(a_)
                    }
                    FlexInt::B(b) => {
                        MulAssign::mul_assign(&mut a_, b);
                        FlexInt::shrink(a_)
                    }
                }
            }
        }
    }
}

impl SubAssign for FlexInt {
    fn sub_assign(&mut self, other: Self) {
        *self = match *self {
            FlexInt::S(a) => match other {
                FlexInt::S(b) => CheckedSub::checked_sub(&a, &b).map_or_else(
                    || {
                        let mut a_ = BigInt::from(a);
                        SubAssign::sub_assign(&mut a_, BigInt::from(b));
                        FlexInt::shrink(a_)
                    },
                    FlexInt::S,
                ),
                FlexInt::B(b) => {
                    let mut a_ = BigInt::from(a);
                    SubAssign::sub_assign(&mut a_, b);
                    FlexInt::shrink(a_)
                }
            },
            FlexInt::B(ref mut a) => {
                let mut a_ = mem::replace(a, BigInt::zero());
                match other {
                    FlexInt::S(b) => {
                        SubAssign::sub_assign(&mut a_, BigInt::from(b));
                        FlexInt::shrink(a_)
                    }
                    FlexInt::B(b) => {
                        SubAssign::sub_assign(&mut a_, b);
                        FlexInt::shrink(a_)
                    }
                }
            }
        }
    }
}

impl<'a> SubAssign<&'a Self> for FlexInt {
    fn sub_assign(&mut self, other: &'a Self) {
        *self = match *self {
            FlexInt::S(a) => match other {
                FlexInt::S(b) => CheckedSub::checked_sub(&a, b).map_or_else(
                    || {
                        let mut a_ = BigInt::from(a);
                        SubAssign::sub_assign(&mut a_, BigInt::from(*b));
                        FlexInt::shrink(a_)
                    },
                    FlexInt::S,
                ),
                FlexInt::B(b) => {
                    let mut a_ = BigInt::from(a);
                    SubAssign::sub_assign(&mut a_, b);
                    FlexInt::shrink(a_)
                }
            },
            FlexInt::B(ref mut a) => {
                let mut a_ = mem::replace(a, BigInt::zero());
                match other {
                    FlexInt::S(b) => {
                        SubAssign::sub_assign(&mut a_, BigInt::from(*b));
                        FlexInt::shrink(a_)
                    }
                    FlexInt::B(b) => {
                        SubAssign::sub_assign(&mut a_, b);
                        FlexInt::shrink(a_)
                    }
                }
            }
        }
    }
}

impl CheckedAdd for FlexInt {
    fn checked_add(&self, other: &Self) -> Option<Self> {
        match self {
            FlexInt::S(a) => match other {
                FlexInt::S(b) => CheckedAdd::checked_add(a, b).map_or_else(
                    || {
                        CheckedAdd::checked_add(&BigInt::from(*a), &BigInt::from(*b))
                            .map(FlexInt::shrink)
                    },
                    |val| Some(FlexInt::S(val)),
                ),
                FlexInt::B(b) => CheckedAdd::checked_add(&BigInt::from(*a), b).map(FlexInt::shrink),
            },
            FlexInt::B(a) => match other {
                FlexInt::S(b) => CheckedAdd::checked_add(a, &BigInt::from(*b)).map(FlexInt::shrink),
                FlexInt::B(b) => CheckedAdd::checked_add(a, b).map(FlexInt::shrink),
            },
        }
    }
}

impl CheckedDiv for FlexInt {
    fn checked_div(&self, other: &Self) -> Option<Self> {
        match self {
            FlexInt::S(a) => match other {
                FlexInt::S(b) => CheckedDiv::checked_div(a, b).map_or_else(
                    || {
                        CheckedDiv::checked_div(&BigInt::from(*a), &BigInt::from(*b))
                            .map(FlexInt::shrink)
                    },
                    |val| Some(FlexInt::S(val)),
                ),
                FlexInt::B(b) => CheckedDiv::checked_div(&BigInt::from(*a), b).map(FlexInt::shrink),
            },
            FlexInt::B(a) => match other {
                FlexInt::S(b) => CheckedDiv::checked_div(a, &BigInt::from(*b)).map(FlexInt::shrink),
                FlexInt::B(b) => CheckedDiv::checked_div(a, b).map(FlexInt::shrink),
            },
        }
    }
}

impl CheckedMul for FlexInt {
    fn checked_mul(&self, other: &Self) -> Option<Self> {
        match self {
            FlexInt::S(a) => match other {
                FlexInt::S(b) => CheckedMul::checked_mul(a, b).map_or_else(
                    || {
                        CheckedMul::checked_mul(&BigInt::from(*a), &BigInt::from(*b))
                            .map(FlexInt::shrink)
                    },
                    |val| Some(FlexInt::S(val)),
                ),
                FlexInt::B(b) => CheckedMul::checked_mul(&BigInt::from(*a), b).map(FlexInt::shrink),
            },
            FlexInt::B(a) => match other {
                FlexInt::S(b) => CheckedMul::checked_mul(a, &BigInt::from(*b)).map(FlexInt::shrink),
                FlexInt::B(b) => CheckedMul::checked_mul(a, b).map(FlexInt::shrink),
            },
        }
    }
}

impl CheckedSub for FlexInt {
    fn checked_sub(&self, other: &Self) -> Option<Self> {
        match self {
            FlexInt::S(a) => match other {
                FlexInt::S(b) => CheckedSub::checked_sub(a, b).map_or_else(
                    || {
                        CheckedSub::checked_sub(&BigInt::from(*a), &BigInt::from(*b))
                            .map(FlexInt::shrink)
                    },
                    |val| Some(FlexInt::S(val)),
                ),
                FlexInt::B(b) => CheckedSub::checked_sub(&BigInt::from(*a), b).map(FlexInt::shrink),
            },
            FlexInt::B(a) => match other {
                FlexInt::S(b) => CheckedSub::checked_sub(a, &BigInt::from(*b)).map(FlexInt::shrink),
                FlexInt::B(b) => CheckedSub::checked_sub(a, b).map(FlexInt::shrink),
            },
        }
    }
}

impl From<i64> for FlexInt {
    fn from(value: i64) -> Self {
        FlexInt::S(value)
    }
}

impl From<BigInt> for FlexInt {
    fn from(value: BigInt) -> Self {
        FlexInt::shrink(value)
    }
}

impl Zero for FlexInt {
    #[inline]
    fn zero() -> Self {
        FlexInt::S(0)
    }

    #[inline]
    fn is_zero(&self) -> bool {
        *self == Self::zero()
    }
}

impl One for FlexInt {
    #[inline]
    fn one() -> Self {
        FlexInt::S(1)
    }
}

impl Num for FlexInt {
    type FromStrRadixErr = <BigInt as Num>::FromStrRadixErr;

    fn from_str_radix(s: &str, radix: u32) -> Result<Self, Self::FromStrRadixErr> {
        i64::from_str_radix(s, radix)
            .map(FlexInt::S)
            .or_else(|_| BigInt::from_str_radix(s, radix).map(FlexInt::shrink))
    }
}

impl Integer for FlexInt {
    fn div_floor(&self, other: &Self) -> Self {
        match *self {
            FlexInt::S(ref a) => match *other {
                FlexInt::S(ref b) => FlexInt::S(num_integer::Integer::div_floor(a, b)),
                FlexInt::B(ref b) => FlexInt::shrink(BigInt::div_floor(&BigInt::from(*a), b)),
            },
            FlexInt::B(ref a) => match *other {
                FlexInt::S(ref b) => FlexInt::shrink(BigInt::div_floor(a, &BigInt::from(*b))),
                FlexInt::B(ref b) => FlexInt::shrink(BigInt::div_floor(a, b)),
            },
        }
    }

    fn mod_floor(&self, other: &Self) -> Self {
        match *self {
            FlexInt::S(ref a) => match *other {
                FlexInt::S(ref b) => FlexInt::S(i64::mod_floor(a, b)),
                FlexInt::B(ref b) => FlexInt::shrink(BigInt::mod_floor(&BigInt::from(*a), b)),
            },
            FlexInt::B(ref a) => match *other {
                FlexInt::S(ref b) => FlexInt::shrink(BigInt::mod_floor(a, &BigInt::from(*b))),
                FlexInt::B(ref b) => FlexInt::shrink(BigInt::mod_floor(a, b)),
            },
        }
    }

    fn gcd(&self, other: &Self) -> Self {
        match *self {
            FlexInt::S(ref a) => match *other {
                FlexInt::S(ref b) => FlexInt::S(i64::gcd(a, b)),
                FlexInt::B(ref b) => FlexInt::shrink(BigInt::gcd(&BigInt::from(*a), b)),
            },
            FlexInt::B(ref a) => match *other {
                FlexInt::S(ref b) => FlexInt::shrink(BigInt::gcd(a, &BigInt::from(*b))),
                FlexInt::B(ref b) => FlexInt::shrink(BigInt::gcd(a, b)),
            },
        }
    }

    fn lcm(&self, other: &Self) -> Self {
        match *self {
            FlexInt::S(ref a) => match *other {
                FlexInt::S(ref b) => FlexInt::S(num_integer::Integer::div_floor(a, b)),
                FlexInt::B(ref b) => FlexInt::shrink(BigInt::div_floor(&BigInt::from(*a), b)),
            },
            FlexInt::B(ref a) => match *other {
                FlexInt::S(ref b) => FlexInt::shrink(BigInt::div_floor(a, &BigInt::from(*b))),
                FlexInt::B(ref b) => FlexInt::shrink(BigInt::div_floor(a, b)),
            },
        }
    }

    fn is_multiple_of(&self, other: &Self) -> bool {
        match *self {
            FlexInt::S(ref a) => match *other {
                FlexInt::S(ref b) => i64::is_multiple_of(a, b),
                FlexInt::B(ref b) => BigInt::is_multiple_of(&BigInt::from(*a), b),
            },
            FlexInt::B(ref a) => match *other {
                FlexInt::S(ref b) => BigInt::is_multiple_of(a, &BigInt::from(*b)),
                FlexInt::B(ref b) => BigInt::is_multiple_of(a, b),
            },
        }
    }

    fn is_even(&self) -> bool {
        match *self {
            FlexInt::S(ref a) => a.is_even(),
            FlexInt::B(ref b) => b.is_even(),
        }
    }

    fn is_odd(&self) -> bool {
        match *self {
            FlexInt::S(ref a) => a.is_even(),
            FlexInt::B(ref b) => b.is_even(),
        }
    }

    fn div_rem(&self, other: &Self) -> (Self, Self) {
        match *self {
            FlexInt::S(ref a) => match *other {
                FlexInt::S(ref b) => {
                    let (c, d) = i64::div_rem(a, b);
                    (FlexInt::S(c), FlexInt::S(d))
                }
                FlexInt::B(ref b) => {
                    let (c, d) = BigInt::div_rem(&BigInt::from(*a), b);
                    (FlexInt::shrink(c), FlexInt::shrink(d))
                }
            },
            FlexInt::B(ref a) => match *other {
                FlexInt::S(ref b) => {
                    let (c, d) = BigInt::div_rem(a, &BigInt::from(*b));
                    (FlexInt::shrink(c), FlexInt::shrink(d))
                }
                FlexInt::B(ref b) => {
                    let (c, d) = BigInt::div_rem(a, b);
                    (FlexInt::shrink(c), FlexInt::shrink(d))
                }
            },
        }
    }
}

impl ToPrimitive for FlexInt {
    fn to_i64(&self) -> Option<i64> {
        match self {
            FlexInt::S(val) => Some(*val),
            FlexInt::B(val) => val.to_i64(),
        }
    }

    fn to_u64(&self) -> Option<u64> {
        match self {
            FlexInt::S(val) => val.to_u64(),
            FlexInt::B(val) => val.to_u64(),
        }
    }
}

impl From<ObjectType<'_>> for FlexInt {
    fn from(obj: ObjectType) -> Self {
        match obj {
            ObjectType::Int(x) => FlexInt::S(x),
            ObjectType::BigInt(x) => FlexInt::B((**x).clone()),
            _ => unreachable!(),
        }
    }
}

impl From<FlexInt> for NumberValue {
    fn from(value: FlexInt) -> NumberValue {
        match value {
            FlexInt::S(x) => NumberValue::Int(x),
            FlexInt::B(x) => NumberValue::Big(x),
        }
    }
}

impl From<FlexInt> for BigInt {
    fn from(value: FlexInt) -> BigInt {
        match value {
            FlexInt::S(x) => BigInt::from(x),
            FlexInt::B(x) => x,
        }
    }
}

impl TryFrom<f64> for FlexInt {
    type Error = anyhow::Error;

    fn try_from(value: f64) -> anyhow::Result<Self> {
        if value.is_nan() || value.is_infinite() {
            return Err(anyhow!("Invalid float value"));
        }
        let out = match value.to_i64() {
            Some(i) => FlexInt::from(i),
            None => {
                return BigInt::from_f64(value)
                    .map(FlexInt::from)
                    .ok_or(anyhow!("Invalid float value"));
            }
        };
        Ok(out)
    }
}
