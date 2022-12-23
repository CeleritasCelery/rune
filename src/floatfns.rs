use crate::{
    arith::NumberValue,
    core::{
        gc::Context,
        object::{Gc, Number},
    },
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
fn float<'ob>(arg: Gc<Number<'ob>>, cx: &'ob Context) -> Gc<Number<'ob>> {
    match arg.untag() {
        Number::Int(i) => cx.add_as(i as f64),
        Number::Float(_) => arg,
    }
}
