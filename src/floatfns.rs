use crate::{
    arith::NumberValue,
    core::object::{Gc, Number},
};
use fn_macros::defun;

#[defun]
fn floor(arg: Gc<Number>, divisor: Option<Gc<Number>>) -> i64 {
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
fn float(arg: Gc<Number>) -> f64 {
    match arg.get() {
        Number::Int(i) => i as f64,
        Number::Float(f) => *f,
    }
}

define_symbols!(FUNCS => {floor, float});
