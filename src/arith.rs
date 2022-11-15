use crate::core::object::{Gc, IntoObject, Number};
use float_cmp::ApproxEq;
use fn_macros::defun;
use std::cmp::{PartialEq, PartialOrd};
use std::ops::{Add, Div, Mul, Neg, Rem, Sub};

#[derive(Debug, PartialEq, Copy, Clone)]
pub(crate) enum NumberValue {
    Int(i64),
    Float(f64),
}

impl<'ob> Gc<Number<'ob>> {
    pub(crate) fn val(self) -> NumberValue {
        match self.get() {
            Number::Int(x) => NumberValue::Int(x),
            Number::Float(x) => NumberValue::Float(**x),
        }
    }
}

impl IntoObject for NumberValue {
    type Out<'ob> = Number<'ob>;

    fn into_obj<const C: bool>(self, block: &crate::core::gc::Block<C>) -> Gc<Self::Out<'_>> {
        match self {
            NumberValue::Int(x) => x.into(),
            NumberValue::Float(x) => block.add(x),
        }
    }

    unsafe fn from_obj_ptr<'ob>(_ptr: *const u8) -> Self::Out<'ob> {
        todo!()
    }
}

fn arith(
    cur: NumberValue,
    next: NumberValue,
    int_fn: fn(i64, i64) -> i64,
    float_fn: fn(f64, f64) -> f64,
) -> NumberValue {
    match cur {
        NumberValue::Float(cur) => match next {
            NumberValue::Float(next) => NumberValue::Float(float_fn(cur, next)),
            NumberValue::Int(next) => NumberValue::Float(float_fn(cur, next as f64)),
        },
        NumberValue::Int(cur) => match next {
            NumberValue::Float(next) => NumberValue::Float(float_fn(cur as f64, next)),
            NumberValue::Int(next) => NumberValue::Int(int_fn(cur, next)),
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
            NumberValue::Int(x) => NumberValue::Int(-x),
            NumberValue::Float(x) => NumberValue::Float(-x),
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

impl Rem for NumberValue {
    type Output = Self;
    fn rem(self, rhs: Self) -> Self::Output {
        arith(self, rhs, Rem::rem, Rem::rem)
    }
}

impl<'ob> PartialEq<i64> for Gc<Number<'ob>> {
    fn eq(&self, other: &i64) -> bool {
        match self.val() {
            NumberValue::Int(num) => num == *other,
            NumberValue::Float(num) => num == *other as f64,
        }
    }
}

impl<'ob> PartialEq<f64> for Gc<Number<'ob>> {
    fn eq(&self, other: &f64) -> bool {
        match self.val() {
            NumberValue::Int(num) => num as f64 == *other,
            NumberValue::Float(num) => num.approx_eq(*other, (f64::EPSILON, 2)),
        }
    }
}

impl PartialOrd for NumberValue {
    fn partial_cmp(&self, other: &NumberValue) -> Option<std::cmp::Ordering> {
        match self {
            NumberValue::Int(lhs) => match other {
                NumberValue::Int(rhs) => lhs.partial_cmp(rhs),
                NumberValue::Float(rhs) => (*lhs as f64).partial_cmp(rhs),
            },
            NumberValue::Float(lhs) => match other {
                NumberValue::Int(rhs) => lhs.partial_cmp(&(*rhs as f64)),
                NumberValue::Float(rhs) => lhs.partial_cmp(rhs),
            },
        }
    }
}

#[defun(name = "+")]
pub(crate) fn add(vars: &[Gc<Number>]) -> NumberValue {
    vars.iter()
        .fold(NumberValue::Int(0), |acc, x| acc + x.val())
}

#[defun(name = "-")]
pub(crate) fn sub(number: Option<Gc<Number>>, numbers: &[Gc<Number>]) -> NumberValue {
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
pub(crate) fn mul(numbers: &[Gc<Number>]) -> NumberValue {
    numbers
        .iter()
        .fold(NumberValue::Int(1), |acc, x| acc * x.val())
}

#[defun(name = "/")]
pub(crate) fn div(number: Gc<Number>, divisors: &[Gc<Number>]) -> NumberValue {
    divisors.iter().fold(number.val(), |acc, x| acc / x.val())
}

#[defun(name = "1+")]
pub(crate) fn add_one(number: Gc<Number>) -> NumberValue {
    number.val() + NumberValue::Int(1)
}

#[defun(name = "1-")]
pub(crate) fn sub_one(number: Gc<Number>) -> NumberValue {
    number.val() - NumberValue::Int(1)
}

#[defun(name = "=")]
pub(crate) fn num_eq(number: Gc<Number>, numbers: &[Gc<Number>]) -> bool {
    match number.val() {
        NumberValue::Int(num) => numbers.iter().all(|&x| x == num),
        NumberValue::Float(num) => numbers.iter().all(|&x| x == num),
    }
}

#[defun(name = "/=")]
#[allow(clippy::float_cmp)] // This is a bug in clippy, we are not comparing floats directly
pub(crate) fn num_ne(number: Gc<Number>, numbers: &[Gc<Number>]) -> bool {
    match number.val() {
        NumberValue::Int(num) => numbers.iter().all(|&x| x != num),
        NumberValue::Float(num) => numbers.iter().all(|&x| x != num),
    }
}

fn cmp(
    number: Gc<Number>,
    numbers: &[Gc<Number>],
    cmp: fn(&NumberValue, &NumberValue) -> bool,
) -> bool {
    numbers
        .iter()
        .try_fold(number.val(), |acc, &x| {
            cmp(&acc, &x.val()).then_some(NumberValue::Int(0))
        })
        .is_some()
}

#[defun(name = "<")]
pub(crate) fn less_than(number: Gc<Number>, numbers: &[Gc<Number>]) -> bool {
    cmp(number, numbers, NumberValue::lt)
}

#[defun(name = "<=")]
pub(crate) fn less_than_or_eq(number: Gc<Number>, numbers: &[Gc<Number>]) -> bool {
    cmp(number, numbers, NumberValue::le)
}

#[defun(name = ">")]
pub(crate) fn greater_than(number: Gc<Number>, numbers: &[Gc<Number>]) -> bool {
    cmp(number, numbers, NumberValue::gt)
}

#[defun(name = ">=")]
pub(crate) fn greater_than_or_eq(number: Gc<Number>, numbers: &[Gc<Number>]) -> bool {
    cmp(number, numbers, NumberValue::ge)
}

#[defun]
pub(crate) fn logior(ints_or_markers: &[Gc<i64>]) -> i64 {
    ints_or_markers.iter().fold(0, |acc, x| acc | x.get())
}

#[defun]
fn logand(int_or_markers: &[Gc<i64>]) -> i64 {
    int_or_markers.iter().fold(-1, |accum, x| accum & x.get())
}

#[defun(name = "mod")]
pub(crate) fn modulo(x: Gc<Number>, y: Gc<Number>) -> NumberValue {
    x.val() % y.val()
}

#[allow(clippy::trivially_copy_pass_by_ref)]
fn max_val(x: NumberValue, y: &Gc<Number>) -> NumberValue {
    let y = y.val();
    let ret = if x > y { x } else { y };
    ret
}

#[allow(clippy::trivially_copy_pass_by_ref)]
fn min_val(x: NumberValue, y: &Gc<Number>) -> NumberValue {
    let y = y.val();
    let ret = if x < y { x } else { y };
    ret
}

#[defun]
fn max(number_or_marker: Gc<Number>, number_or_markers: &[Gc<Number>]) -> NumberValue {
    number_or_markers
        .iter()
        .fold(number_or_marker.val(), max_val)
}

#[defun]
fn min(number_or_marker: Gc<Number>, number_or_markers: &[Gc<Number>]) -> NumberValue {
    number_or_markers
        .iter()
        .fold(number_or_marker.val(), min_val)
}

define_symbols!(
    FUNCS => {
        add,
        sub,
        mul,
        div,
        add_one,
        sub_one,
        num_eq,
        num_ne,
        less_than,
        less_than_or_eq,
        greater_than,
        greater_than_or_eq,
        logior,
        logand,
        modulo,
        min,
        max,
    }
);

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
        assert_eq!(add(&[1.into(), cx.add(2.5)]), NumberValue::Float(3.5));
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

        assert_eq!(div(cx.add(12.0), &[]), NumberValue::Float(12.0));
        assert_eq!(div(12.into(), &[5.into(), 2.into()]), NumberValue::Int(1));
    }

    #[test]
    fn test_eq() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        let int1 = 1.into();
        let float1: Gc<Number> = cx.add(1.0);
        let float1_1 = cx.add(1.1);

        assert!(num_eq(int1, &[]));
        assert!(num_eq(int1, &[cx.add(1.0)]));
        assert!(num_eq(float1, &[1.into()]));
        assert!(!num_eq(float1, &[1.into(), 1.into(), float1_1]));
    }

    #[test]
    fn test_cmp() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        assert!(less_than(1.into(), &[]));
        assert!(less_than(1.into(), &[cx.add(1.1)]));
        assert!(!less_than(cx.add(1.0), &[1.into()]));
        assert!(less_than(
            cx.add(1.0),
            &[cx.add(1.1), 2.into(), cx.add(2.1)]
        ));
    }

    #[test]
    fn test_max_min() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        assert_eq!(
            max(cx.add(1.0), &[cx.add(2.1), cx.add(1.1), cx.add(1.0)]),
            cx.add(2.1).val()
        );
        assert_eq!(
            min(cx.add(1.1), &[cx.add(1.0), cx.add(2.1), cx.add(1.0)]),
            cx.add(1.0).val()
        );
    }

    #[test]
    fn test_other() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        assert_eq!(logand(&[cx.add(258), cx.add(255)]), 2);
    }
}
