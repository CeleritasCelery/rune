use crate::error::{Error, Type};
use crate::lisp_object::*;
use std::convert::TryFrom;
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

impl IntoObject<IntObject> for i64 {
    fn into_object(self, _arena: &Arena) -> IntObject {
        IntObject(IntObject::new_tagged(self))
    }
}

type Float = f64;
define_unbox!(Float);

impl IntoObject<FloatObject> for f64 {
    fn into_object(self, arena: &Arena) -> FloatObject {
        let ptr = arena.alloc(self);
        FloatObject(FloatObject::new_tagged(ptr as i64))
    }
}

impl<'obj> From<bool> for Object<'obj> {
    fn from(b: bool) -> Self {
        Object::from_tag(if b { Tag::True } else { Tag::Nil })
    }
}

impl IntoObject<BoolObject> for bool {
    fn into_object(self, _arena: &Arena) -> BoolObject {
        if self {
            TrueObject(TrueObject::new_tagged(0)).into()
        } else {
            NilObject(NilObject::new_tagged(0)).into()
        }
    }
}

impl IntoObject<StringObject> for &str {
    fn into_object(self, arena: &Arena) -> StringObject {
        let ptr = arena.alloc(self.to_owned());
        StringObject(StringObject::new_tagged(ptr as i64))
    }
}

define_unbox_ref!(String);

impl IntoObject<StringObject> for String {
    fn into_object(self, arena: &Arena) -> StringObject {
        let ptr = arena.alloc(self);
        StringObject(StringObject::new_tagged(ptr as i64))
    }
}

impl<'obj> IntoObject<Object<'obj>> for Object<'obj> {
    fn into_object(self, _arena: &Arena) -> Object<'obj> {
        self
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
        let arena = Arena::new();
        let obj0 = arena.insert(5);
        let obj1 = arena.insert(cons!(1, 2; arena));
        let vec = vec![obj0, obj1];
        let res = wrapper(vec.as_slice());
        assert_eq!(6, res.unwrap());
    }
}
