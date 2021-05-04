use crate::error::{Error, Type};
use crate::object::*;
use std::convert::TryFrom;
use std::convert::TryInto;
use std::mem::transmute;

impl<'obj> TryFrom<Object<'obj>> for Function<'obj> {
    type Error = Error;
    fn try_from(obj: Object) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::LispFn(_) | Value::SubrFn(_) => Ok(unsafe { transmute(obj) }),
            x => Err(Error::Type(Type::Func, x.get_type())),
        }
    }
}

impl<'obj> TryFrom<Object<'obj>> for Number<'obj> {
    type Error = Error;
    fn try_from(obj: Object) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::Int(_) | Value::Float(_) => Ok(unsafe { transmute(obj) }),
            x => Err(Error::Type(Type::Number, x.get_type())),
        }
    }
}

impl<'obj> TryFrom<Object<'obj>> for Option<Number<'obj>> {
    type Error = Error;
    fn try_from(obj: Object) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::Int(_) | Value::Float(_) => Ok(Some(unsafe { transmute(obj) })),
            Value::Nil => Ok(None),
            x => Err(Error::Type(Type::Number, x.get_type())),
        }
    }
}

impl<'obj> TryFrom<Object<'obj>> for NumberValue {
    type Error = Error;
    fn try_from(obj: Object<'obj>) -> Result<Self, Self::Error> {
        TryInto::<Number>::try_into(obj).map(|x| x.val())
    }
}

impl<'obj> TryFrom<Object<'obj>> for Option<NumberValue> {
    type Error = Error;
    fn try_from(obj: Object<'obj>) -> Result<Self, Self::Error> {
        TryInto::<Option<Number>>::try_into(obj).map(|x| x.map(|x| x.val()))
    }
}

impl<'obj> TryFrom<Object<'obj>> for bool {
    type Error = Error;
    fn try_from(obj: Object) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::Nil => Ok(false),
            _ => Ok(true),
        }
    }
}

pub fn try_from_slice<'obj, T>(slice: &[Object<'obj>]) -> Result<&'obj [T], Error>
where
    T: TryFrom<Object<'obj>, Error = Error>,
{
    debug_assert_eq!(size_of::<Object>(), size_of::<T>());
    for x in slice.iter() {
        let _: T = TryFrom::try_from(*x)?;
    }
    let ptr = slice.as_ptr() as *const T;
    let len = slice.len();
    Ok(unsafe { std::slice::from_raw_parts(ptr, len) })
}

type Int = i64;
define_unbox!(Int);

impl<'obj> From<i64> for Object<'obj> {
    fn from(x: i64) -> Self {
        let x: Number = x.into();
        x.into()
    }
}

impl<'obj> IntoObject<'obj, Object<'obj>> for i64 {
    fn into_obj(self, _arena: &'obj Arena) -> Object<'obj> {
        self.into()
    }
}

type Float = f64;
define_unbox!(Float);

impl<'obj> IntoObject<'obj, Object<'obj>> for f64 {
    fn into_obj(self, arena: &'obj Arena) -> Object<'obj> {
        let obj: Number = self.into_obj(arena);
        obj.into()
    }
}

impl<'obj> From<bool> for Object<'obj> {
    fn from(b: bool) -> Self {
        InnerObject::from_tag(if b { Tag::True } else { Tag::Nil }).into()
    }
}

impl<'obj> IntoObject<'obj, Object<'obj>> for bool {
    fn into_obj(self, _arena: &'obj Arena) -> Object<'obj> {
        self.into()
    }
}

impl<'obj> IntoObject<'obj, Object<'obj>> for &str {
    fn into_obj(self, arena: &'obj Arena) -> Object<'obj> {
        InnerObject::from_type(self.to_owned(), Tag::String, arena).into()
    }
}

define_unbox_ref!(String);

impl<'obj> IntoObject<'obj, Object<'obj>> for String {
    fn into_obj(self, arena: &'obj Arena) -> Object<'obj> {
        InnerObject::from_type(self, Tag::String, arena).into()
    }
}

impl<'obj, T> From<Option<T>> for Object<'obj>
where
    T: Into<Object<'obj>>,
{
    fn from(t: Option<T>) -> Self {
        match t {
            Some(x) => x.into(),
            None => Object::nil(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::convert::TryInto;

    fn wrapper(args: &[Object]) -> Result<Int, Error> {
        Ok(inner(
            std::convert::TryFrom::try_from(args[0])?,
            std::convert::TryFrom::try_from(args[1])?,
        ))
    }

    fn inner(arg0: Option<Int>, arg1: &Cons) -> Int {
        let x: Int = arg1.car().try_into().unwrap();
        arg0.unwrap() + x
    }

    #[test]
    fn test() {
        let arena = &Arena::new();
        let obj0 = 5.into_obj(arena);
        let obj1 = cons!(1, 2; arena).into_obj(arena);
        let vec = vec![obj0, obj1];
        let res = wrapper(vec.as_slice());
        assert_eq!(6, res.unwrap());
    }
}
