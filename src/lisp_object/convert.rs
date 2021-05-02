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

impl<'obj> From<Int> for Object<'obj> {
    fn from(i: Int) -> Self {
        unsafe {
            Object::from_ptr(i as *const i64, Tag::Int)
        }
    }
}

impl<'obj> IntoObject<'obj> for i64 {
    fn into_object(self, _alloc: &Arena) -> (Object, bool) {
        unsafe {
            (Object::from_ptr(self as *const i64, Tag::Int), false)
        }
    }
}

impl IntoTagObject<IntObject> for i64 {
    fn into_object(self, _arena: &Arena) -> IntObject {
        IntObject(IntObject::new_tagged(self))
    }
}

type Float = f64;
define_unbox!(Float);

impl<'obj> From<f64> for Object<'obj> {
    fn from(f: f64) -> Self {
        Object::from_tagged_ptr(f, Tag::Float)
    }
}

impl<'obj> IntoObject<'obj> for f64 {
    fn into_object(self, arena: &Arena) -> (Object, bool) {
        Object::from_type(arena, self, Tag::Float)
    }
}

impl IntoTagObject<FloatObject> for f64 {
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

impl<'obj> IntoObject<'obj> for bool {
    fn into_object(self, _alloc: &Arena) -> (Object, bool) {
        (Object::from_tag(if self { Tag::True } else { Tag::Nil }), false)
    }
}

impl IntoTagObject<BoolObject> for bool {
    fn into_object(self, _arena: &Arena) -> BoolObject {
        if self {
            TrueObject(TrueObject::new_tagged(0)).into()
        } else {
            NilObject(NilObject::new_tagged(0)).into()
        }
    }
}

impl<'obj> From<&str> for Object<'obj> {
    fn from(s: &str) -> Self {
        Object::from_tagged_ptr(s.to_owned(), Tag::String)
    }
}

impl<'obj> IntoObject<'obj> for &str {
    fn into_object(self, arena: &Arena) -> (Object, bool) {
        Object::from_type(arena, self.to_owned(), Tag::String)
    }
}

impl IntoTagObject<StringObject> for &str {
    fn into_object(self, arena: &Arena) -> StringObject {
        let ptr = arena.alloc(self.to_owned());
        StringObject(StringObject::new_tagged(ptr as i64))
    }
}

define_unbox_ref!(String);
impl<'obj> From<String> for Object<'obj> {
    fn from(s: String) -> Self {
        Object::from_tagged_ptr(s, Tag::String)
    }
}

impl<'obj> IntoObject<'obj> for String {
    fn into_object(self, arena: &Arena) -> (Object, bool) {
        Object::from_type(arena, self, Tag::String)
    }
}

impl IntoTagObject<StringObject> for String {
    fn into_object(self, arena: &Arena) -> StringObject {
        let ptr = arena.alloc(self);
        StringObject(StringObject::new_tagged(ptr as i64))
    }
}

impl<'obj> IntoObject<'obj> for Object<'obj> {
    fn into_object(self, _arena: &'obj Arena) -> (Object<'obj>, bool) {
        (self, false)
    }
}

impl<'obj> IntoTagObject<Object<'obj>> for Object<'obj> {
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

impl<'obj, T: IntoObject<'obj>> IntoObject<'obj> for Option<T> {
    fn into_object(self, arena: &'obj Arena) -> (Object, bool) {
        match self {
            Some(x) => x.into_object(arena),
            None => (Object::nil(), false),
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
        let obj0 = LispObj::from(5);
        let obj1 = LispObj::from(cons!(1, 2; arena));
        let vec = vec![obj0, obj1];
        let res = wrapper(vec.as_slice());
        assert_eq!(6, res.unwrap());
    }
}
