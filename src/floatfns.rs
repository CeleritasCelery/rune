//! Operations on floats.
use crate::{
    arith::NumberValue,
    core::{
        gc::Context,
        object::{Number, NumberType},
    },
};
use rune_macros::defun;

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
fn float<'ob>(arg: Number<'ob>, cx: &'ob Context) -> Number<'ob> {
    match arg.untag() {
        NumberType::Int(i) => cx.add_as(i as f64),
        NumberType::Float(_) => arg,
    }
}
