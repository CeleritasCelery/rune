#![allow(dead_code)]

use crate::lispobject::LispObj;

pub fn add(lhs: &LispObj, rhs: &LispObj) -> LispObj {
    let x = lhs.as_fixnum().unwrap_or_else(|| {panic!{"lhs is not a number"}});
    let y = rhs.as_fixnum().unwrap_or_else(|| {panic!{"rhs is not a number"}});
    LispObj::from_fixnum(x + y)
}

pub fn sub(lhs: &LispObj, rhs: &LispObj) -> LispObj {
    let x = lhs.as_fixnum().unwrap_or_else(|| {panic!{"lhs is not a number"}});
    let y = rhs.as_fixnum().unwrap_or_else(|| {panic!{"rhs is not a number"}});
    LispObj::from_fixnum(x - y)
}

pub fn mul(lhs: &LispObj, rhs: &LispObj) -> LispObj {
    let x = lhs.as_fixnum().unwrap_or_else(|| {panic!{"lhs is not a number"}});
    let y = rhs.as_fixnum().unwrap_or_else(|| {panic!{"rhs is not a number"}});
    LispObj::from_fixnum(x * y)
}

pub fn div(lhs: &LispObj, rhs: &LispObj) -> LispObj {
    let x = lhs.as_fixnum().unwrap_or_else(|| {panic!{"lhs is not a number"}});
    let y = rhs.as_fixnum().unwrap_or_else(|| {panic!{"rhs is not a number"}});
    LispObj::from_fixnum(x / y)
}

pub fn run() {
    let x = &LispObj::from(16);
    let y = &LispObj::from(4);
    let add = add(x, y);
    let sub = sub(x, y);
    let mul = mul(x, y);
    let div = div(x, y);

    println!("{} {} {} {}", add.as_int().unwrap(), sub.as_int().unwrap(), mul.as_int().unwrap(), div.as_int().unwrap());
}
