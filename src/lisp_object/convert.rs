use crate::error::{Error, Type};
use crate::lisp_object::*;
use std::convert::TryFrom;
use std::mem::transmute;

impl TryFrom<LispObj> for Function {
    type Error = Error;
    fn try_from(obj: LispObj) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::LispFn(_) | Value::SubrFn(_) => Ok(unsafe { transmute(obj) }),
            x => Err(Error::Type(Type::Func, x.get_type())),
        }
    }
}

impl TryFrom<LispObj> for Number {
    type Error = Error;
    fn try_from(obj: LispObj) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::Int(_) | Value::Float(_) => Ok(unsafe { transmute(obj) }),
            x => Err(Error::Type(Type::Number, x.get_type())),
        }
    }
}

impl TryFrom<LispObj> for Option<Number> {
    type Error = Error;
    fn try_from(obj: LispObj) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::Int(_) | Value::Float(_) => Ok(Some(unsafe { transmute(obj) })),
            Value::Nil => Ok(None),
            x => Err(Error::Type(Type::Number, x.get_type())),
        }
    }
}

impl TryFrom<LispObj> for List {
    type Error = Error;
    fn try_from(obj: LispObj) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::Cons(_) | Value::Nil => Ok(unsafe { transmute(obj) }),
            x => Err(Error::Type(Type::List, x.get_type())),
        }
    }
}

impl TryFrom<LispObj> for bool {
    type Error = Error;
    fn try_from(obj: LispObj) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::Nil => Ok(false),
            _ => Ok(true),
        }
    }
}

pub fn try_from_slice<T>(slice: &[LispObj]) -> Result<&[T], Error>
where
    T: TryFrom<LispObj, Error = Error>,
{
    debug_assert_eq!(size_of::<LispObj>(), size_of::<T>());
    for x in slice.iter() {
        let _: T = TryFrom::try_from(*x)?;
    }
    let ptr = slice.as_ptr() as *const T;
    let len = slice.len();
    Ok(unsafe { std::slice::from_raw_parts(ptr, len) })
}

type Int = i64;
define_unbox!(Int);

impl From<Int> for LispObj {
    fn from(i: Int) -> Self {
        LispObj {
            bits: i << TAG_SIZE,
        }
    }
}

type Float = f64;
define_unbox!(Float);

impl From<Float> for LispObj {
    fn from(f: Float) -> Self {
        LispObj::from_tagged_ptr(f, Tag::Float)
    }
}

impl From<bool> for LispObj {
    fn from(b: bool) -> Self {
        LispObj::from_tag(if b { Tag::True } else { Tag::Nil })
    }
}

impl From<&str> for LispObj {
    fn from(s: &str) -> Self {
        LispObj::from_tagged_ptr(s.to_owned(), Tag::LongStr)
    }
}

define_unbox_ref!(String);
impl From<String> for LispObj {
    fn from(s: String) -> Self {
        LispObj::from_tagged_ptr(s, Tag::LongStr)
    }
}

impl<T> From<Option<T>> for LispObj
where
    T: Into<LispObj>,
{
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

    fn wrapper(args: &[LispObj]) -> Result<Int, Error> {
        Ok(inner(
            std::convert::TryFrom::try_from(args[0])?,
            std::convert::TryFrom::try_from(&args[1])?,
        ))
    }

    fn inner(arg0: Option<Int>, arg1: &Cons) -> Int {
        let x: Int = arg1.car.try_into().unwrap();
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
