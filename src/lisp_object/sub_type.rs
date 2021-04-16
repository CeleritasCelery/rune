use crate::lisp_object::{Cons, LispFn, LispObj, SubrFn, Value};

pub struct Function(i64);

impl From<LispFn> for Function {
    fn from(x: LispFn) -> Self {
        Self(LispObj::from(x).into_raw())
    }
}

impl From<SubrFn> for Function {
    fn from(x: SubrFn) -> Self {
        Self(LispObj::from(x).into_raw())
    }
}

pub enum FunctionValue<'a> {
    LispFn(&'a LispFn),
    SubrFn(&'a SubrFn),
}

impl<'a> Function {
    pub fn val(self) -> FunctionValue<'a> {
        use std::mem::transmute;
        unsafe {
            match LispObj::from_bits(self.0).val() {
                Value::LispFn(x) => FunctionValue::LispFn(transmute(x)), // transmute to fix lifetimes
                Value::SubrFn(x) => FunctionValue::SubrFn(transmute(x)),
                _ => unreachable!("Function was invalid type"),
            }
        }
    }
}

pub struct Number(i64);

impl From<i64> for Number {
    fn from(x: i64) -> Self {
        Self(LispObj::from(x).into_raw())
    }
}

impl From<f64> for Number {
    fn from(x: f64) -> Self {
        Self(LispObj::from(x).into_raw())
    }
}

impl From<Number> for LispObj {
    fn from(x: Number) -> Self {
         LispObj::from_bits(x.0)
    }
}

pub enum NumberValue {
    Int(i64),
    Float(f64),
}

impl Number {
    pub fn val(&self) -> NumberValue {
        match LispObj::from_bits(self.0).val()  {
            Value::Int(x) => NumberValue::Int(x),
            Value::Float(x) => NumberValue::Float(x),
            _ => unreachable!("Number was invalid type"),
        }
    }
}

pub struct List(i64);

pub enum ListValue<'a> {
    Nil,
    Cons(&'a Cons<'a>),
}

impl List {
    pub fn val(&self) -> ListValue {
        match LispObj::from_bits(self.0).val()  {
            Value::Nil => ListValue::Nil,
            Value::Cons(x) => ListValue::Cons(unsafe { std::mem::transmute(x) }),
            _ => unreachable!("Number was invalid type"),
        }
    }
}

impl From<List> for LispObj {
    fn from(x: List) -> Self {
         LispObj::from_bits(x.0)
    }
}
