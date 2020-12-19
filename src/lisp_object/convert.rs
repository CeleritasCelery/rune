use crate::lisp_object::*;
use crate::error::{Error, Type};
use std::convert::TryFrom;
use std::mem::transmute;

pub fn get_type(obj: LispObj) -> Type {
    use Type::*;
    match obj.val() {
        Value::Symbol(_) => Symbol,
        Value::Float(_) => Float,
        Value::Void => Void,
        Value::String(_) => String,
        Value::Nil => Nil,
        Value::True => True,
        Value::Cons(_) => Cons,
        Value::Int(_) => Int,
        Value::LispFn(_) => Func,
        Value::SubrFn(_) => Func,
    }
}

fn expect_type(exp_type: Type, obj: LispObj) -> Error {
    Error::Type(exp_type, get_type(obj))
}

impl TryFrom<LispObj> for Function {
    type Error = Error;
    fn try_from(obj: LispObj) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::LispFn(_) | Value::SubrFn(_) => {
              Ok(unsafe {transmute(obj)})
            }
            _ => Err(expect_type(Type::Func, obj))
        }
    }
}

impl TryFrom<LispObj> for Number {
    type Error = Error;
    fn try_from(obj: LispObj) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::Int(_) | Value::Float(_) => {
              Ok(unsafe {transmute(obj)})
            }
            _ => Err(expect_type(Type::Number, obj))
        }
    }
}

impl TryFrom<LispObj> for Option<Number> {
    type Error = Error;
    fn try_from(obj: LispObj) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::Int(_) | Value::Float(_) => {
              Ok(Some(unsafe {transmute(obj)}))
            }
            Value::Nil => Ok(None),
            _ => Err(expect_type(Type::Number, obj))
        }
    }
}

impl TryFrom<LispObj> for List {
    type Error = Error;
    fn try_from(obj: LispObj) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::Cons(_) | Value::Nil => {
              Ok(unsafe {transmute(obj)})
            }
            _ => Err(expect_type(Type::List, obj))
        }
    }
}

impl TryFrom<LispObj> for Symbol {
    type Error = Error;
    fn try_from(obj: LispObj) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::Symbol(x) => Ok(x),
            _ => Err(expect_type(Type::Symbol, obj)),
        }
    }
}

impl TryFrom<LispObj> for Option<Symbol> {
    type Error = Error;
    fn try_from(obj: LispObj) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::Symbol(x) => Ok(Some(x)),
            Value::Nil => Ok(None),
            _ => Err(expect_type(Type::Symbol, obj)),
        }
    }
}


impl TryFrom<LispObj> for i64 {
    type Error = Error;
    fn try_from(obj: LispObj) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::Int(x) => Ok(x),
            _ => Err(expect_type(Type::Int, obj))
        }
    }
}

impl TryFrom<LispObj> for Option<i64> {
    type Error = Error;
    fn try_from(obj: LispObj) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::Int(x) => Ok(Some(x)),
            Value::Nil => Ok(None),
            _ => Err(expect_type(Type::Int, obj))
        }
    }
}

impl TryFrom<LispObj> for f64 {
    type Error = Error;
    fn try_from(obj: LispObj) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::Float(x) => Ok(x),
            _ => Err(expect_type(Type::Float, obj))
        }
    }
}

impl TryFrom<LispObj> for Option<f64> {
    type Error = Error;
    fn try_from(obj: LispObj) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::Float(x) => Ok(Some(x)),
            Value::Nil => Ok(None),
            _ => Err(expect_type(Type::Float, obj))
        }
    }
}

impl TryFrom<LispObj> for bool {
    type Error = Error;
    fn try_from(obj: LispObj) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::Nil => Ok(false),
            _ => Ok(true)
        }
    }
}

impl<'a> TryFrom<&'a LispObj> for &'a String {
    type Error = Error;
    fn try_from(obj: &'a LispObj) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::String(x) => Ok(x),
            _ => Err(expect_type(Type::String, *obj))
        }
    }
}

impl<'a> TryFrom<&'a LispObj> for Option<&'a String> {
    type Error = Error;
    fn try_from(obj: &'a LispObj) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::String(x) => Ok(Some(x)),
            Value::Nil => Ok(None),
            _ => Err(expect_type(Type::String, *obj))
        }
    }
}

impl<'a> TryFrom<&'a LispObj> for &'a Cons {
    type Error = Error;
    fn try_from(obj: &'a LispObj) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::Cons(x) => Ok(x),
            _ => Err(expect_type(Type::Int, *obj))
        }
    }
}

impl<'a> TryFrom<&'a LispObj> for Option<&'a Cons> {
    type Error = Error;
    fn try_from(obj: &'a LispObj) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::Cons(x) => Ok(Some(x)),
            Value::Nil => Ok(None),
            _ => Err(expect_type(Type::Int, *obj))
        }
    }
}

impl<'a> TryFrom<&'a LispObj> for &'a LispFn {
    type Error = Error;
    fn try_from(obj: &'a LispObj) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::LispFn(x) => Ok(x),
            _ => Err(expect_type(Type::Func, *obj))
        }
    }
}

impl<'a> TryFrom<&'a LispObj> for Option<&'a LispFn> {
    type Error = Error;
    fn try_from(obj: &'a LispObj) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::LispFn(x) => Ok(Some(x)),
            Value::Nil => Ok(None),
            _ => Err(expect_type(Type::Func, *obj))
        }
    }
}

impl<'a> TryFrom<&'a LispObj> for &'a SubrFn {
    type Error = Error;
    fn try_from(obj: &'a LispObj) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::SubrFn(x) => Ok(x),
            _ => Err(expect_type(Type::Func, *obj))
        }
    }
}

impl<'a> TryFrom<&'a LispObj> for Option<&'a SubrFn> {
    type Error = Error;
    fn try_from(obj: &'a LispObj) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::SubrFn(x) => Ok(Some(x)),
            Value::Nil => Ok(None),
            _ => Err(expect_type(Type::Func, *obj))
        }
    }
}

pub fn try_from_slice<T>(slice: &[LispObj]) ->
    Result<&[T], Error> where T: TryFrom<LispObj, Error = Error>
{
    debug_assert_eq!(size_of::<LispObj>(), size_of::<T>());
    for x in slice.iter() {
        let _: T = TryFrom::try_from(*x)?;
    }
    let ptr = slice.as_ptr() as *const T;
    let len = slice.len();
    Ok(unsafe {std::slice::from_raw_parts(ptr, len)})
}

impl From<i64> for LispObj {
    fn from(i: i64) -> Self {
        LispObj {bits: i << TAG_SIZE}
    }
}

impl From<f64> for LispObj {
    fn from (f: f64) -> Self {
        LispObj::from_tagged_ptr(f, Tag::Float)
    }
}

impl From<bool> for LispObj {
    fn from(b: bool) -> Self {
        LispObj::from_tag(if b {Tag::True} else {Tag::Nil})
    }
}

impl From<&str> for LispObj {
    fn from(s: &str) -> Self {
        LispObj::from_tagged_ptr(s.to_owned(), Tag::LongStr)
    }
}

impl From<String> for LispObj {
    fn from(s: String) -> Self {
        LispObj::from_tagged_ptr(s, Tag::LongStr)
    }
}

impl<T> From<Option<T>> for LispObj where T: Into<LispObj>  {
    fn from(t: Option<T>) -> Self {
        match t {
            Some(x) => x.into(),
            None => LispObj::nil(),
        }
    }
}


#[cfg(test)]
mod test {
    use super::*;
    use std::convert::TryInto;

    fn wrapper(args: &[LispObj]) -> Result<i64, Error> {
        Ok(inner(std::convert::TryFrom::try_from(args[0])?, std::convert::TryFrom::try_from(&args[1])?))
    }

    fn inner(arg0: Option<i64>, arg1: &Cons) -> i64 {
        let x: i64 = arg1.car.try_into().unwrap();
        arg0.unwrap() + x
    }

    #[test]
    fn test() {
        let obj0 = LispObj::from(5);
        let obj1 = LispObj::from(cons!(1, 2));
        let vec = vec![obj0, obj1];
        let res = wrapper(vec.as_slice());
        assert_eq!(6, res.unwrap());
    }
}
