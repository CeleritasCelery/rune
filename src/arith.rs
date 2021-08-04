use crate::object::NumberValue::{Float, Int};
use crate::object::{Number, NumberValue};
use fn_macros::defun;
use std::cmp::{PartialEq, PartialOrd};
use std::ops::{Add, Div, Mul, Neg, Sub};

fn arith(
    cur: NumberValue,
    next: NumberValue,
    int_fn: fn(i64, i64) -> i64,
    float_fn: fn(f64, f64) -> f64,
) -> NumberValue {
    match cur {
        Float(cur) => match next {
            Float(next) => Float(float_fn(cur, next)),
            Int(next) => Float(float_fn(cur, next as f64)),
        },
        Int(cur) => match next {
            Float(next) => Float(float_fn(cur as f64, next)),
            Int(next) => Int(int_fn(cur, next)),
        },
    }
}

//////////////////////////
// Arithmetic operators //
//////////////////////////

impl Neg for NumberValue {
    type Output = Self;
    fn neg(self) -> Self::Output {
        match self {
            Int(x) => Int(-x),
            Float(x) => Float(-x),
        }
    }
}

impl Add for NumberValue {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        arith(self, rhs, Add::add, Add::add)
    }
}

impl Sub for NumberValue {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        arith(self, rhs, Sub::sub, Sub::sub)
    }
}

impl Mul for NumberValue {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        arith(self, rhs, Mul::mul, Mul::mul)
    }
}

impl Div for NumberValue {
    type Output = Self;
    fn div(self, rhs: Self) -> Self::Output {
        arith(self, rhs, Div::div, Div::div)
    }
}

impl<'ob> PartialEq<i64> for Number<'ob> {
    fn eq(&self, other: &i64) -> bool {
        match self.val() {
            Int(num) => num == *other,
            Float(num) => num == *other as f64,
        }
    }
}

impl<'ob> PartialEq<f64> for Number<'ob> {
    fn eq(&self, other: &f64) -> bool {
        match self.val() {
            Int(num) => num as f64 == *other,
            Float(num) => (*other - num).abs() <= f64::EPSILON,
        }
    }
}

impl PartialOrd for NumberValue {
    fn partial_cmp(&self, other: &NumberValue) -> Option<std::cmp::Ordering> {
        match self {
            Int(lhs) => match other {
                Int(rhs) => lhs.partial_cmp(rhs),
                Float(rhs) => (*lhs as f64).partial_cmp(rhs),
            },
            Float(lhs) => match other {
                Int(rhs) => lhs.partial_cmp(&(*rhs as f64)),
                Float(rhs) => lhs.partial_cmp(rhs),
            },
        }
    }
}

#[defun(name = "+")]
pub(crate) fn add(vars: &[Number]) -> NumberValue {
    vars.iter().fold(Int(0), |acc, x| acc + x.val())
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
        None => Int(0),
    }
}

#[defun(name = "*")]
pub(crate) fn mul(numbers: &[Number]) -> NumberValue {
    numbers.iter().fold(Int(1), |acc, x| acc * x.val())
}

#[defun(name = "/")]
pub(crate) fn div(number: Number, divisors: &[Number]) -> NumberValue {
    divisors.iter().fold(number.val(), |acc, x| acc / x.val())
}

#[defun(name = "1+")]
pub(crate) fn plus_one(number: Number) -> NumberValue {
    number.val() + Int(1)
}

#[defun(name = "1-")]
pub(crate) fn minus_one(number: Number) -> NumberValue {
    number.val() - Int(1)
}

#[defun(name = "=")]
pub(crate) fn num_eq(number: Number, numbers: &[Number]) -> bool {
    match number.val() {
        Int(num) => numbers.iter().all(|&x| x == num),
        Float(num) => numbers.iter().all(|&x| x == num),
    }
}

#[defun(name = "/=")]
#[allow(clippy::float_cmp)]
pub(crate) fn num_ne(number: Number, numbers: &[Number]) -> bool {
    match number.val() {
        Int(num) => numbers.iter().all(|&x| x != num),
        Float(num) => numbers.iter().all(|&x| x != num),
    }
}

fn cmp(number: Number, numbers: &[Number], cmp: fn(&NumberValue, &NumberValue) -> bool) -> bool {
    numbers
        .iter()
        .try_fold(number.val(), |acc, &x| cmp(&acc, &x.val()).then(|| Int(0)))
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

defsubr!(
    add,
    sub,
    mul,
    div,
    plus_one,
    minus_one,
    num_eq,
    num_ne,
    less_than,
    less_than_or_eq,
    greater_than,
    greater_than_or_eq
);

#[cfg(test)]
mod test {
    use super::*;
    use crate::arena::Arena;

    #[test]
    #[allow(clippy::float_cmp)]
    fn test_add() {
        let arena = &Arena::new();
        let (int1, int7, int13, float2_5) = into_objects!(1, 7, 13, 2.5; arena);

        assert_eq!(add(&[]), Int(0));
        assert_eq!(add(&[int7, int13]), Int(20));
        assert_eq!(add(&[int1, float2_5]), Float(3.5));
    }

    #[test]
    fn test_sub() {
        let arena = &Arena::new();
        let int13 = into_objects!(13; arena);

        assert_eq!(sub(None, &[]), Int(0));
        assert_eq!(sub(Some(7.into()), &[]), Int(-7));
        assert_eq!(sub(Some(7.into()), &[int13]), Int(-6));
    }

    #[test]
    fn test_mul() {
        let arena = &Arena::new();
        let args = vec_into_object![7, 13; arena];

        assert_eq!(mul(&[]), Int(1));
        assert_eq!(mul(&args), Int(91));
    }

    #[test]
    #[allow(clippy::float_cmp)]
    fn test_div() {
        let arena = &Arena::new();
        let (int2, int5, float12) = into_objects!(2, 5, 12.0; arena);

        assert_eq!(div(float12, &[]), Float(12.0));
        assert_eq!(div(12.into(), &[int5, int2]), Int(1));
    }

    #[test]
    fn test_eq() {
        let arena = &Arena::new();
        let (int1, float1, float1_1) = into_objects![1, 1.0, 1.1; arena];

        assert!(num_eq(int1, &[]));
        assert!(num_eq(int1, &[float1]));
        assert!(num_eq(float1, &[int1]));
        assert!(!num_eq(float1, &[int1, int1, float1_1]));
    }

    #[test]
    fn test_cmp() {
        let arena = &Arena::new();
        let (int1, int2, float1, float1_1, float2_1) = into_objects![1, 2, 1.0, 1.1, 2.1; arena];

        assert!(less_than(int1, &[]));
        assert!(less_than(int1, &[float1_1]));
        assert!(!less_than(float1, &[int1]));
        assert!(less_than(float1, &[float1_1, int2, float2_1]));
    }
}
