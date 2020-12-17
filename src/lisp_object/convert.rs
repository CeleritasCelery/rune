use crate::lisp_object::*;
use crate::error::{Error, Type};
use std::convert::TryFrom;

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


impl TryFrom<LispObj> for Fixnum {
    type Error = Error;
    fn try_from(obj: LispObj) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::Int(x) => Ok(x),
            _ => Err(expect_type(Type::Int, obj))
        }
    }
}

impl TryFrom<LispObj> for Option<Fixnum> {
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

#[cfg(test)]
mod test {
    use super::*;
    use std::convert::TryInto;

    fn wrapper(args: &[LispObj]) -> Result<i64, Error> {
        Ok(inner(std::convert::TryFrom::try_from(args[0])?, std::convert::TryFrom::try_from(&args[1])?))
    }

    fn inner(arg0: Option<Fixnum>, arg1: &Cons) -> i64 {
        let x: Fixnum = arg1.car.try_into().unwrap();
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
