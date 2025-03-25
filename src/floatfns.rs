//! Operations on floats.
use core::num;
use std::ops::Div;

use crate::{
    arith::{self, NumberValue},
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
use num_traits::{FromPrimitive, ToPrimitive, Zero};

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
        result = result * BigInt::from(2).pow(power as u32);
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
                NumberValue::Float(f) => NumberValue::Float(double_round(f)).to_integer(),
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
            return Ok(NumberValue::Big(bigum_divide(num, denom)).to_integer());
        }
    }
}

#[defun]
fn floor(num: Number, divisor: Option<Number>) -> Result<NumberValue> {
    rounding_driver(
        num.val(),
        divisor.map(|d| d.val()),
        |f| f.floor(),
        |n, d| n.div_floor(&d),
        |n, d| n.div_floor(&d),
    )
}

#[defun]
fn ceiling(num: Number, divisor: Option<Number>) -> Result<NumberValue> {
    rounding_driver(
        num.val(),
        divisor.map(|d| d.val()),
        |f| f.ceil(),
        |n, d| n.div_ceil(&d),
        |n, d| n.div_ceil(&d),
    )
}

// #[defun]
// fn fceiling(arg: Number) -> f64 {
//     match arg.untag() {
//         NumberType::Int(i) => i as f64,
//         NumberType::Float(f) => f.ceil(),
//     }
// }

// #[defun]
// fn round(arg: Number) -> i64 {
//     match arg.untag() {
//         NumberType::Int(i) => i,
//         NumberType::Float(f) => f.round() as i64,
//     }
// }

#[defun]
fn truncate(num: Number, divisor: Option<Number>) -> Result<NumberValue> {
    rounding_driver(num.val(), divisor.map(|d| d.val()), |f| f, |n, d| n.div(&d), |n, d| n.div(&d))
}

// #[defun]
// fn float<'ob>(arg: Number<'ob>, cx: &'ob Context) -> Number<'ob> {
//     match arg.untag() {
//         NumberType::Int(i) => cx.add_as(i as f64),
//         NumberType::Float(_) => arg,
//     }
// }

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

// #[defun]
// fn isnan(arg: Number) -> bool {
//     match arg.untag() {
//         NumberType::Int(_) => false,
//         NumberType::Float(f) => f.is_nan(),
//     }
// }

#[defun]
fn copysign(x: Number, y: Number) -> f64 {
    coerce(x).copysign(coerce(y))
}

#[defun]
fn exp(arg: Number) -> f64 {
    coerce(arg).exp()
}

#[defun]
fn expt(x: Number, y: Number) -> NumberValue {
    // If either is a float, we use the float version
    match (x.untag(), y.untag()) {
        (NumberType::Int(x), NumberType::Int(y)) => NumberValue::Int(x.pow(y as u32)),
        _ => {
            let x = coerce(x);
            let y = coerce(y);
            NumberValue::Float(x.powf(y))
        }
    }
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

// #[defun]
// fn abs(arg: Number) -> NumberValue {
//     match arg.untag() {
//         NumberType::Int(i) => NumberValue::Int(i.abs()),
//         NumberType::Float(f) => NumberValue::Float(f.abs()),
//     }
// }

#[defun]
fn ldexp(s: Number, e: i64) -> f64 {
    // TODO: overflow check -> bail!?
    coerce(s) * 2f64.powi(e as i32)
}

#[defun]
fn logb(arg: Number) -> i64 {
    let l2 = coerce(arg).log2();
    // Round down to an integer
    l2.floor() as i64
}

// Rust does not have frexp, so we have to implement it ourselves
// Source: https://stackoverflow.com/questions/55690397/where-is-the-frexp-function-for-f32-in-rust
fn frexp_f(f: f64) -> (f64, i64) {
    if f.is_zero() || f.is_infinite() || f.is_nan() {
        return (f, 0);
    }

    let lg = f.abs().log2();
    let x = (lg - lg.floor() - 1.0).exp2();
    let exp = lg.floor() + 1.0;
    (f.signum() * x, exp as i64)
}

#[defun]
fn frexp<'ob>(x: Number, cx: &'ob Context) -> Object<'ob> {
    let f = coerce(x);
    let (significand, exponent) = frexp_f(f);
    Cons::new(significand, exponent, cx).into()
}

#[cfg(test)]
mod test {
    use crate::interpreter::assert_lisp;

    #[test]
    fn test_floor() {
        assert_lisp("(floor 1 -2)", "-1");
        // assert_lisp("(floor 0 -0.0)", "arith-error");
        assert_lisp(
            "(floor 5994075590485518098614452039918638881146941881234478715467383783598649398185401621312377889848862509948275153705727693548522812899079421120481350627531464566370422590572585528049408418896764645218625131504253704691271381208042307584.0 nil)",
            "5994075590485518098614452039918638881146941881234478715467383783598649398185401621312377889848862509948275153705727693548522812899079421120481350627531464566370422590572585528049408418896764645218625131504253704691271381208042307584",
        );
        assert_lisp(
            "(floor 5994075590485518098614452039918638881146941881234478715467383783598649398185401621312377889848862509948275153705727693548522812899079421120481350627531464566370422590572585528049408418896764645218625131504253704691271381208042307584.0 nil)",
            "5994075590485518098614452039918638881146941881234478715467383783598649398185401621312377889848862509948275153705727693548522812899079421120481350627531464566370422590572585528049408418896764645218625131504253704691271381208042307584",
        );
        assert_lisp(
            "(floor -44122902879431969491750680114212286152660105718239592448.0 3)",
            "-14707634293143989830583560038070762050886701906079864150",
        );
        assert_lisp("(floor -97651715312475760.0 nil)", "-97651715312475760");
        assert_lisp(
            "(floor -0.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000011247981962136344 -14869275859372792347960709425659708825137801195943253943014060542864699836857403927732362122441772425694766828016793823593666404006764240328006866934664274134551713379318796896704590266059959485411578573114354498316108551837963681383757577005549399861244474064025242092719152028270226046976.0)",
            "0",
        );
        assert_lisp(
            "(floor -0.0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000054036049998902035 nil)",
            "-1",
        );
    }

    #[test]
    fn test_truncate() {
        assert_lisp("(truncate 6176627708866652160.0 nil)", "6176627708866652160");
        assert_lisp("(truncate 212571529420423104.0 nil)", "212571529420423136");
    }
}
