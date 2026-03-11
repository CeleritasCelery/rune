//! Arithmetic operators.
use crate::core::object::{Gc, IntoObject, Number, NumberType, ObjectType};
use float_cmp::ApproxEq;
use num_bigint::BigInt;
use num_traits::{FromPrimitive, ToPrimitive, Zero};
use rune_macros::defun;
use std::cmp::PartialEq;
use std::ops::{Add, Div, Mul, Neg, Rem, Sub};

pub(crate) const MAX_FIXNUM: i64 = i64::MAX >> 8;
pub(crate) const MIN_FIXNUM: i64 = i64::MIN >> 8;

/// Similar to the object type [NumberType], but contains a float instead of a
/// reference to a float. This makes it easier to construct and mutate.
#[derive(Debug, PartialEq, Clone)]
pub(crate) enum NumberValue {
    Int(i64),
    Float(f64),
    Big(BigInt),
}

impl Number<'_> {
    pub(crate) fn val(self) -> NumberValue {
        match self.untag() {
            NumberType::Int(x) => NumberValue::Int(x),
            NumberType::Float(x) => NumberValue::Float(**x),
            NumberType::Big(x) => NumberValue::Big((**x).clone()),
        }
    }
}

impl IntoObject for NumberValue {
    type Out<'ob> = ObjectType<'ob>;

    fn into_obj<const C: bool>(self, block: &crate::core::gc::Block<C>) -> Gc<Self::Out<'_>> {
        match self {
            NumberValue::Int(x) => x.into(),
            NumberValue::Float(x) => block.add(x),
            NumberValue::Big(x) => block.add(x),
        }
    }
}

impl NumberValue {
    pub fn coerce_integer(self) -> NumberValue {
        match self {
            NumberValue::Float(x) => {
                if x.is_finite() && x <= MAX_FIXNUM as f64 && x >= MIN_FIXNUM as f64 {
                    NumberValue::Int(x as i64)
                } else {
                    NumberValue::Big(BigInt::from_f64(x).unwrap_or_else(BigInt::zero))
                }
            }
            NumberValue::Big(x) => x
                .to_i64()
                .filter(|&n| (MIN_FIXNUM..=MAX_FIXNUM).contains(&n))
                .map(NumberValue::Int)
                .unwrap_or_else(|| NumberValue::Big(x)),
            other => other,
        }
    }
}

pub(crate) fn arith(
    cur: NumberValue,
    next: NumberValue,
    int_fn: fn(i64, i64) -> i64,
    float_fn: fn(f64, f64) -> f64,
    big_fn: fn(BigInt, BigInt) -> BigInt,
) -> NumberValue {
    use NumberValue as N;
    match (cur, next) {
        (N::Int(l), N::Int(r)) => N::Int(int_fn(l, r)),
        (N::Int(l), N::Float(r)) => N::Float(float_fn(l as f64, r)),
        (N::Float(l), N::Int(r)) => N::Float(float_fn(l, r as f64)),
        (N::Float(l), N::Float(r)) => N::Float(float_fn(l, r)),
        (N::Int(l), N::Big(r)) => N::Big(big_fn(l.into(), r)),
        (N::Big(l), N::Int(r)) => N::Big(big_fn(l, r.into())),
        (N::Big(l), N::Big(r)) => N::Big(big_fn(l, r)),
        (N::Float(l), N::Big(r)) => N::Float(float_fn(l, r.to_f64().unwrap())), // TODO: Should round to nearest float on error
        (N::Big(l), N::Float(r)) => N::Float(float_fn(l.to_f64().unwrap(), r)), // TODO: Should round to nearest float on error
    }
}

//////////////////////////
// Arithmetic operators //
//////////////////////////

impl Zero for NumberValue {
    fn zero() -> Self {
        NumberValue::Int(0)
    }
    fn is_zero(&self) -> bool {
        match self {
            NumberValue::Int(x) => *x == 0,
            NumberValue::Float(x) => *x == 0.0,
            NumberValue::Big(x) => x.is_zero(),
        }
    }
}

impl Neg for NumberValue {
    type Output = Self;
    fn neg(self) -> Self::Output {
        match self {
            NumberValue::Int(x) => NumberValue::Int(-x),
            NumberValue::Float(x) => NumberValue::Float(-x),
            NumberValue::Big(x) => NumberValue::Big(-x),
        }
    }
}

impl Add for NumberValue {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        arith(self, rhs, Add::add, Add::add, Add::add)
    }
}

impl Sub for NumberValue {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        arith(self, rhs, Sub::sub, Sub::sub, Sub::sub)
    }
}

impl Mul for NumberValue {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        arith(self, rhs, Mul::mul, Mul::mul, Mul::mul)
    }
}

impl Div for NumberValue {
    type Output = Self;
    fn div(self, rhs: Self) -> Self::Output {
        arith(self, rhs, Div::div, Div::div, Div::div)
    }
}

impl Rem for NumberValue {
    type Output = Self;
    fn rem(self, rhs: Self) -> Self::Output {
        arith(self, rhs, Rem::rem, Rem::rem, Rem::rem)
    }
}

impl PartialEq<i64> for Number<'_> {
    fn eq(&self, other: &i64) -> bool {
        match self.val() {
            NumberValue::Int(num) => num == *other,
            NumberValue::Float(num) => num == *other as f64,
            NumberValue::Big(num) => num == BigInt::from(*other),
        }
    }
}

impl PartialEq<f64> for Number<'_> {
    fn eq(&self, other: &f64) -> bool {
        match self.val() {
            NumberValue::Int(num) => num as f64 == *other,
            NumberValue::Float(num) => num.approx_eq(*other, (f64::EPSILON, 2)),
            NumberValue::Big(num) => {
                num.to_f64().is_some_and(|n| n.approx_eq(*other, (f64::EPSILON, 2)))
            } // TODO: Check behavior when conversion fails
        }
    }
}

impl PartialEq<BigInt> for Number<'_> {
    fn eq(&self, other: &BigInt) -> bool {
        match self.val() {
            NumberValue::Int(num) => BigInt::from(num) == *other,
            NumberValue::Float(num) => {
                other.to_f64().is_some_and(|n| n.approx_eq(num, (f64::EPSILON, 2)))
            } // TODO: Check
            NumberValue::Big(num) => num == *other,
        }
    }
}

impl PartialOrd for NumberValue {
    fn partial_cmp(&self, other: &NumberValue) -> Option<std::cmp::Ordering> {
        match self {
            NumberValue::Int(lhs) => match other {
                NumberValue::Int(rhs) => lhs.partial_cmp(rhs),
                NumberValue::Float(rhs) => (*lhs as f64).partial_cmp(rhs),
                NumberValue::Big(rhs) => BigInt::from(*lhs).partial_cmp(rhs),
            },
            NumberValue::Float(lhs) => match other {
                NumberValue::Int(rhs) => lhs.partial_cmp(&(*rhs as f64)),
                NumberValue::Float(rhs) => lhs.partial_cmp(rhs),
                NumberValue::Big(rhs) => {
                    lhs.partial_cmp(&rhs.to_f64().unwrap_or(f64::NAN)) // TODO: Handle conversion failure
                }
            },
            NumberValue::Big(lhs) => match other {
                NumberValue::Int(rhs) => lhs.partial_cmp(&BigInt::from(*rhs)),
                NumberValue::Float(rhs) => lhs.to_f64().and_then(|n| n.partial_cmp(rhs)),
                NumberValue::Big(rhs) => lhs.partial_cmp(rhs),
            },
        }
    }
}

#[defun(name = "+")]
pub(crate) fn add(vars: &[Number]) -> NumberValue {
    vars.iter().fold(NumberValue::Int(0), |acc, x| acc + x.val())
}

#[defun(name = "-")]
pub(crate) fn sub(number: Option<Number>, numbers: &[Number]) -> NumberValue {
    match number {
        Some(num) => {
            let num = num.val();
            if numbers.is_empty() {
                -num
            } else {
                numbers.iter().fold(num, |acc, x| acc - x.val())
            }
        }
        None => NumberValue::Int(0),
    }
}

#[defun(name = "*")]
pub(crate) fn mul(numbers: &[Number]) -> NumberValue {
    numbers.iter().fold(NumberValue::Int(1), |acc, x| acc * x.val())
}

#[defun(name = "/")]
pub(crate) fn div(number: Number, divisors: &[Number]) -> NumberValue {
    divisors.iter().fold(number.val(), |acc, x| acc / x.val())
}

#[defun(name = "1+")]
pub(crate) fn add_one(number: Number) -> NumberValue {
    number.val() + NumberValue::Int(1)
}

#[defun(name = "1-")]
pub(crate) fn sub_one(number: Number) -> NumberValue {
    number.val() - NumberValue::Int(1)
}

#[defun(name = "=")]
pub(crate) fn num_eq(number: Number, numbers: &[Number]) -> bool {
    match number.val() {
        NumberValue::Int(num) => numbers.iter().all(|&x| x == num),
        NumberValue::Float(num) => numbers.iter().all(|&x| x == num),
        NumberValue::Big(num) => numbers.iter().all(|&x| x == num),
    }
}

#[defun(name = "/=")]
pub(crate) fn num_ne(number: Number, numbers: &[Number]) -> bool {
    match number.val() {
        NumberValue::Int(num) => numbers.iter().all(|&x| x != num),
        NumberValue::Float(num) => numbers.iter().all(|&x| x != num),
        NumberValue::Big(num) => numbers.iter().all(|&x| x != num),
    }
}

fn cmp(number: Number, numbers: &[Number], cmp: fn(&NumberValue, &NumberValue) -> bool) -> bool {
    numbers
        .iter()
        .try_fold(number.val(), |acc, &x| cmp(&acc, &x.val()).then_some(NumberValue::Int(0)))
        .is_some()
}

#[defun(name = "<")]
pub(crate) fn less_than(number: Number, numbers: &[Number]) -> bool {
    cmp(number, numbers, NumberValue::lt)
}

#[defun(name = "<=")]
pub(crate) fn less_than_or_eq(number: Number, numbers: &[Number]) -> bool {
    cmp(number, numbers, NumberValue::le)
}

#[defun(name = ">")]
pub(crate) fn greater_than(number: Number, numbers: &[Number]) -> bool {
    cmp(number, numbers, NumberValue::gt)
}

#[defun(name = ">=")]
pub(crate) fn greater_than_or_eq(number: Number, numbers: &[Number]) -> bool {
    cmp(number, numbers, NumberValue::ge)
}

#[defun]
pub(crate) fn logior(ints_or_markers: &[Gc<i64>]) -> i64 {
    ints_or_markers.iter().fold(0, |acc, x| acc | x.untag())
}

#[defun]
fn logand(int_or_markers: &[Gc<i64>]) -> i64 {
    int_or_markers.iter().fold(-1, |accum, x| accum & x.untag())
}

#[defun(name = "mod")]
pub(crate) fn modulo(x: Number, y: Number) -> NumberValue {
    x.val() % y.val()
}

#[defun(name = "%")]
pub(crate) fn remainder(x: i64, y: i64) -> i64 {
    // TODO: Handle markers
    x % y
}

#[expect(clippy::trivially_copy_pass_by_ref)]
fn max_val(x: NumberValue, y: &Number) -> NumberValue {
    let y = y.val();
    if x > y { x } else { y }
}

#[expect(clippy::trivially_copy_pass_by_ref)]
fn min_val(x: NumberValue, y: &Number) -> NumberValue {
    let y = y.val();
    if x < y { x } else { y }
}

#[defun]
pub(crate) fn max(number_or_marker: Number, number_or_markers: &[Number]) -> NumberValue {
    number_or_markers.iter().fold(number_or_marker.val(), max_val)
}

#[defun]
pub(crate) fn min(number_or_marker: Number, number_or_markers: &[Number]) -> NumberValue {
    number_or_markers.iter().fold(number_or_marker.val(), min_val)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::core::gc::{Context, RootSet};

    #[test]
    fn test_add() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        assert_eq!(add(&[]), NumberValue::Int(0));
        assert_eq!(add(&[7.into(), 13.into()]), NumberValue::Int(20));
        assert_eq!(add(&[1.into(), cx.add_as(2.5)]), NumberValue::Float(3.5));
        assert_eq!(add(&[0.into(), (-1).into()]), NumberValue::Int(-1));
    }

    #[test]
    fn test_sub() {
        assert_eq!(sub(None, &[]), NumberValue::Int(0));
        assert_eq!(sub(Some(7.into()), &[]), NumberValue::Int(-7));
        assert_eq!(sub(Some(7.into()), &[13.into()]), NumberValue::Int(-6));
        assert_eq!(sub(Some(0.into()), &[(-1).into()]), NumberValue::Int(1));
    }

    #[test]
    fn test_mul() {
        assert_eq!(mul(&[]), NumberValue::Int(1));
        assert_eq!(mul(&[7.into(), 13.into()]), NumberValue::Int(91));
        assert_eq!(mul(&[(-1).into(), 1.into()]), NumberValue::Int(-1));
    }

    #[test]
    fn test_div() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);

        assert_eq!(div(cx.add_as(12.0), &[]), NumberValue::Float(12.0));
        assert_eq!(div(12.into(), &[5.into(), 2.into()]), NumberValue::Int(1));
    }

    #[test]
    fn test_eq() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        let int1 = 1.into();
        let float1 = cx.add_as(1.0);
        let float1_1 = cx.add_as(1.1);

        assert!(num_eq(int1, &[]));
        assert!(num_eq(int1, &[cx.add_as(1.0)]));
        assert!(num_eq(float1, &[1.into()]));
        assert!(!num_eq(float1, &[1.into(), 1.into(), float1_1]));
    }

    #[test]
    fn test_cmp() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        assert!(less_than(1.into(), &[]));
        assert!(less_than(1.into(), &[cx.add_as(1.1)]));
        assert!(!less_than(cx.add_as(1.0), &[1.into()]));
        assert!(less_than(cx.add_as(1.0), &[cx.add_as(1.1), 2.into(), cx.add_as(2.1)]));
    }

    #[test]
    fn test_max_min() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        assert_eq!(
            max(cx.add_as(1.0), &[cx.add_as(2.1), cx.add_as(1.1), cx.add_as(1.0)]),
            NumberValue::Float(2.1)
        );
        assert_eq!(
            min(cx.add_as(1.1), &[cx.add_as(1.0), cx.add_as(2.1), cx.add_as(1.0)]),
            NumberValue::Float(1.0)
        );
    }

    #[test]
    fn test_other() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        assert_eq!(logand(&[258.into_obj(cx), 255.into_obj(cx)]), 2);
    }
}
