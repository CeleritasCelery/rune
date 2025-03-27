//! Operations on floats.
use std::ops::{AddAssign, BitAnd, Div, Rem, SubAssign};

use crate::{
    arith::NumberValue,
    core::{
        cons::Cons,
        gc::Context,
        object::{Number, NumberType, Object},
    },
};
use anyhow::Result;
use anyhow::anyhow;
use num_bigint::BigInt;
use num_integer::Integer;
use num_traits::{FromPrimitive, Signed, ToPrimitive, Zero};

use rune_macros::defun;

#[inline(always)]
fn coerce(arg: Number) -> f64 {
    match arg.untag() {
        NumberType::Int(i) => i as f64,
        NumberType::Float(f) => **f,
        NumberType::Big(b) => b.to_f64().unwrap(), // TODO: Handle big integers
    }
}

/* Return the integer exponent E such that D * FLT_RADIX**E (i.e.,
scalbn (D, E)) is an integer that has precision equal to D and is
representable as a double.

Return DBL_MANT_DIG - DBL_MIN_EXP (the maximum possible valid
scale) if D is zero or tiny.  Return one greater than that if
D is infinite, and two greater than that if D is a NaN.  */
fn double_integer_scale(d: f64) -> i64 {
    let exponent = frexp_f(d).1 - 1;

    if f64::MIN_EXP as i64 - 1 <= exponent && exponent < i64::MAX {
        f64::MANTISSA_DIGITS as i64 - 1 - exponent
    } else {
        let x = f64::MANTISSA_DIGITS as i64 - f64::MIN_EXP as i64;
        let c = if exponent == i64::MAX { 1 } else { 0 };
        if d.is_nan() { x + 2 } else { x + c }
    }
}

fn rescale_for_division(n: NumberValue, nscale: i64, dscale: i64) -> Result<BigInt> {
    let mut result = match n {
        NumberValue::Int(i) => BigInt::from(i),
        NumberValue::Float(f) => {
            if (f64::MANTISSA_DIGITS as i64 - f64::MIN_EXP as i64) < nscale {
                return Err(anyhow!("Overflow error"));
            }
            BigInt::from_f64(libm::scalbn(f, nscale as i32)).expect("Conversion error")
        }
        NumberValue::Big(b) => b,
    };

    if nscale < dscale {
        let power = (dscale - nscale) * f64::RADIX.ilog2() as i64;
        result *= BigInt::from(2).pow(power as u32);
    }

    Ok(result)
}

fn rounding_driver(
    n: NumberValue,
    d: Option<NumberValue>,
    double_round: fn(f64) -> f64,
    int_divide: fn(i64, i64) -> i64,
    bigum_divide: fn(BigInt, BigInt) -> BigInt,
) -> Result<NumberValue> {
    let d = match d {
        None => {
            return Ok(match n {
                NumberValue::Float(f) => NumberValue::Float(double_round(f)).coerce_integer(),
                other => other,
            });
        }
        Some(d) if d.is_zero() => {
            return Err(anyhow!("(arith-error)"));
        }
        Some(d) => d,
    };

    match (n, d) {
        (NumberValue::Int(n), NumberValue::Int(d)) => Ok(NumberValue::Int(int_divide(n, d))),
        (n, d) => {
            let dscale = match d {
                NumberValue::Float(f) => double_integer_scale(f),
                _ => 0,
            };

            let nscale = match n {
                NumberValue::Float(f) => double_integer_scale(f),
                _ => 0,
            };

            /* If the numerator is finite and the denominator infinite, the
            quotient is zero and there is no need to try the impossible task
            of rescaling the denominator.  */
            if dscale == f64::MANTISSA_DIGITS as i64 - f64::MIN_EXP as i64 + 1 && nscale < dscale {
                return Ok(NumberValue::Int(0));
            }

            let num = rescale_for_division(n, nscale, dscale)?;
            let denom = rescale_for_division(d, dscale, nscale)?;
            Ok(NumberValue::Big(bigum_divide(num, denom)).coerce_integer())
        }
    }
}

fn round2<T>(num: T, den: T) -> T
where
    T: Div<Output = T>
        + Rem<Output = T>
        + PartialOrd
        + num_traits::Zero
        + num_traits::Signed
        + BitAnd<Output = T>
        + AddAssign
        + SubAssign
        + Clone,
{
    // The C language's division operator gives us the remainder R
    // corresponding to truncated division, but we want the remainder R1
    // on the other side of 0 if R1 is closer to 0 than R is; because we
    // want to round to even, we also want R1 if R and R1 are the same
    // distance from 0 and if the truncated quotient is odd.
    let mut q: T = num.clone() / den.clone();
    let r: T = num.clone() % den.clone();
    let neg_d = den < T::zero();
    let neg_r = r < T::zero();
    let abs_r: T = r.abs();
    let abs_r1: T = den.abs() - abs_r.clone();

    let increment = if (q.clone() & T::one()) == T::one() { T::zero() } else { T::one() };
    if abs_r1 < abs_r + increment {
        if neg_d == neg_r {
            q += T::one();
        } else {
            q -= T::one();
        }
    }
    q
}

const LIMBS_LIMIT: usize = 2147483642;

fn checked_pow(base: BigInt, exp: u32) -> Result<BigInt> {
    // Check base size (number of limbs)
    let nbase = base.bits() as usize; // Number of bits used by the base
    let n = nbase * exp as usize; // Approximate number of limbs required for the result

    // Check overflow condition
    if n > LIMBS_LIMIT {
        return Err(anyhow!("Overflow error".to_string()));
    }
    Ok(base.pow(exp))
}

#[defun]
fn floor(num: Number, divisor: Option<Number>) -> Result<NumberValue> {
    rounding_driver(
        num.val(),
        divisor.map(|d| d.val()),
        |f| f.floor(),
        |n, d| num_integer::Integer::div_floor(&n, &d),
        |n, d| n.div_floor(&d),
    )
}

#[defun]
fn ceiling(num: Number, divisor: Option<Number>) -> Result<NumberValue> {
    rounding_driver(
        num.val(),
        divisor.map(|d| d.val()),
        |f| f.ceil(),
        |n, d| num_integer::Integer::div_ceil(&n, &d),
        |n, d| n.div_ceil(&d),
    )
}

#[defun]
fn round(num: Number, divisor: Option<Number>) -> Result<NumberValue> {
    rounding_driver(
        num.val(),
        divisor.map(|d| d.val()), //
        |f| f.round(),
        round2,
        round2,
    )
}

#[defun]
fn truncate(num: Number, divisor: Option<Number>) -> Result<NumberValue> {
    rounding_driver(
        num.val(),
        divisor.map(|d| d.val()), //
        |f| f,
        |n, d| n.div(&d),
        |n, d| n.div(&d),
    )
}

#[defun]
fn fceiling(num: f64) -> f64 {
    num.ceil()
}

#[defun]
fn ffloor(num: f64) -> f64 {
    num.floor()
}

#[defun]
fn fround(num: f64) -> f64 {
    num.round()
}

#[defun]
fn ftruncate(num: f64) -> f64 {
    num.trunc()
}

#[defun]
fn float(arg: Number) -> NumberValue {
    NumberValue::Float(coerce(arg))
}

#[defun]
fn asin(arg: Number) -> f64 {
    coerce(arg).asin()
}

#[defun]
fn acos(arg: Number) -> f64 {
    coerce(arg).acos()
}

#[defun]
fn atan(arg: Number, x: Option<f64>) -> f64 {
    if let Some(x) = x { coerce(arg).atan2(x) } else { coerce(arg).atan() }
}

#[defun]
fn cos(arg: Number) -> f64 {
    coerce(arg).cos()
}

#[defun]
fn sin(arg: Number) -> f64 {
    coerce(arg).sin()
}

#[defun]
fn tan(arg: Number) -> f64 {
    coerce(arg).tan()
}

#[defun]
fn isnan(arg: Number) -> bool {
    match arg.untag() {
        NumberType::Float(f) => f.is_nan(),
        _ => false,
    }
}

#[defun]
fn copysign(x: f64, y: f64) -> f64 {
    x.copysign(y)
}

#[defun]
fn exp(arg: Number) -> f64 {
    coerce(arg).exp()
}

#[defun]
fn expt(x: Number, y: Number) -> NumberValue {
    // If either is a float, we use the float version
    match (x.untag(), y.untag()) {
        (NumberType::Float(_), _) | (_, NumberType::Float(_)) => {
            return NumberValue::Float(coerce(x).powf(coerce(y)));
        }
        (_, _) => {}
    };

    // Otherwise, we use the integer version
    let bx = match x.untag() {
        NumberType::Int(x) => BigInt::from(x),
        NumberType::Big(b) => (*b).clone(),
        NumberType::Float(x) => {
            return NumberValue::Float(x.powf(coerce(y)));
        }
    };

    let by = match y.untag() {
        NumberType::Int(x) => u32::try_from(x).ok(),
        NumberType::Big(b) => b.to_u32(),
        NumberType::Float(y) => {
            return NumberValue::Float(coerce(x).powf(**y));
        }
    };

    if let Some(y) = by {
        let result = checked_pow(bx, y).unwrap();
        return NumberValue::Big(result).coerce_integer();
    }
    NumberValue::Float(coerce(x).powf(coerce(y)))
}

#[defun]
fn log(arg: Number, base: Option<f64>) -> f64 {
    if let Some(base) = base {
        coerce(arg).log(base)
    } else {
        coerce(arg).log(std::f64::consts::E)
    }
}

#[defun]
fn sqrt(arg: Number) -> f64 {
    coerce(arg).sqrt()
}

#[defun]
fn abs(arg: Number) -> NumberValue {
    match arg.untag() {
        NumberType::Int(i) => NumberValue::Int(i.abs()),
        NumberType::Float(f) => NumberValue::Float(f.abs()),
        NumberType::Big(b) => NumberValue::Big(b.abs()),
    }
}

#[defun]
fn ldexp(s: Number, e: i64) -> f64 {
    coerce(s) * 2f64.powi(e as i32)
}

#[defun]
fn logb(arg: Number) -> i64 {
    let l2 = coerce(arg).log2();
    // Round down to an integer
    l2.floor() as i64
}

#[defun]
fn frexp<'ob>(x: Number, cx: &'ob Context) -> Object<'ob> {
    let f = coerce(x);
    let (significand, exponent) = frexp_f(f);
    Cons::new(significand, exponent, cx).into()
}

fn frexp_f(f: f64) -> (f64, i64) {
    let (x, exp) = libm::frexp(f);
    (x, exp as i64)
}
