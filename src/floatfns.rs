//! Operations on floats.
use crate::{
    arith::NumberValue,
    core::{
        cons::Cons,
        gc::Context,
        object::{Number, NumberType, Object},
    },
};

use rune_macros::defun;

#[inline(always)]
fn coerce(arg: Number) -> f64 {
    match arg.untag() {
        NumberType::Int(i) => i as f64,
        NumberType::Float(f) => **f,
    }
}

#[defun]
fn floor(arg: Number, divisor: Option<Number>) -> i64 {
    let num = match divisor {
        Some(div) => arg.val() / div.val(),
        None => arg.val(),
    };
    match num {
        NumberValue::Int(i) => i,
        NumberValue::Float(f) => f.floor() as i64,
    }
}

#[defun]
fn ceiling(arg: Number) -> i64 {
    match arg.untag() {
        NumberType::Int(i) => i,
        NumberType::Float(f) => f.ceil() as i64,
    }
}

#[defun]
fn fceiling(arg: Number) -> f64 {
    match arg.untag() {
        NumberType::Int(i) => i as f64,
        NumberType::Float(f) => f.ceil(),
    }
}

#[defun]
fn round(arg: Number) -> i64 {
    match arg.untag() {
        NumberType::Int(i) => i,
        NumberType::Float(f) => f.round() as i64,
    }
}

#[defun]
fn truncate(arg: Number) -> i64 {
    match arg.untag() {
        NumberType::Int(i) => i,
        NumberType::Float(f) => f.trunc() as i64,
    }
}

#[defun]
fn float<'ob>(arg: Number<'ob>, cx: &'ob Context) -> Number<'ob> {
    match arg.untag() {
        NumberType::Int(i) => cx.add_as(i as f64),
        NumberType::Float(_) => arg,
    }
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
    if let Some(x) = x {
        coerce(arg).atan2(x)
    } else {
        coerce(arg).atan()
    }
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
        NumberType::Int(_) => false,
        NumberType::Float(f) => f.is_nan(),
    }
}

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

#[defun]
fn abs(arg: Number) -> NumberValue {
    match arg.untag() {
        NumberType::Int(i) => NumberValue::Int(i.abs()),
        NumberType::Float(f) => NumberValue::Float(f.abs()),
    }
}

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
fn frexp_f(s: f64) -> (f64, i64) {
    if 0.0 == s {
        (s, 0)
    } else {
        let lg2 = s.abs().log2();
        let x = (lg2 - lg2.floor() - 1.0).exp2();
        let exp = lg2.floor() + 1.0;
        (s.signum() * x, exp as i64)
    }
}

#[defun]
fn frexp<'ob>(x: Number, cx: &'ob Context) -> Object<'ob> {
    let f = coerce(x);
    let (significand, exponent) = frexp_f(f);
    Cons::new(significand, exponent, cx).into()
}
