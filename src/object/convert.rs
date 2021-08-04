use crate::arena::Arena;
use crate::cons::Cons;
use crate::error::{Error, Type};
use crate::object::{
    Function, InnerObject, IntoObject, List, LocalFunction, Number, Object, Tag, Value, NIL,
};
use crate::symbol::Symbol;
use std::convert::TryFrom;
use std::mem::transmute;

impl<'ob> TryFrom<Object<'ob>> for Function<'ob> {
    type Error = Error;
    fn try_from(obj: Object) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::LispFn(_) | Value::SubrFn(_) => {
                Ok(unsafe { transmute::<Object, Function>(obj) })
            }
            x => Err(Error::Type(Type::Func, x.get_type())),
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for LocalFunction<'ob> {
    type Error = Error;
    fn try_from(obj: Object) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::LispFn(_) | Value::SubrFn(_) | Value::Cons(_) => {
                Ok(unsafe { transmute::<Object, LocalFunction>(obj) })
            }
            x => Err(Error::Type(Type::Func, x.get_type())),
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for Number<'ob> {
    type Error = Error;
    fn try_from(obj: Object) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::Int(_) | Value::Float(_) => Ok(unsafe { transmute::<Object, Number>(obj) }),
            x => Err(Error::Type(Type::Number, x.get_type())),
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for Option<Number<'ob>> {
    type Error = Error;
    fn try_from(obj: Object) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::Int(_) | Value::Float(_) => {
                Ok(Some(unsafe { transmute::<Object, Number>(obj) }))
            }
            Value::Nil => Ok(None),
            x => Err(Error::Type(Type::Number, x.get_type())),
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for bool {
    type Error = Error;
    fn try_from(obj: Object) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::Nil => Ok(false),
            _ => Ok(true),
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for List<'ob> {
    type Error = Error;
    fn try_from(obj: Object) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::Cons(cons) => Ok(List::Cons(cons)),
            Value::Nil => Ok(List::Nil),
            x => Err(Error::Type(Type::List, x.get_type())),
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for Option<List<'ob>> {
    type Error = Error;
    fn try_from(obj: Object) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::Cons(cons) => Ok(Some(List::Cons(cons))),
            Value::Nil => Ok(None),
            x => Err(Error::Type(Type::List, x.get_type())),
        }
    }
}

// This is required because we have no specialization yet
pub(crate) fn try_from_slice<'borrow, 'ob>(
    slice: &'borrow [Object<'ob>],
) -> Result<&'borrow [Number<'ob>], Error> {
    for x in slice.iter() {
        let num: Number = TryFrom::try_from(*x)?;
        unsafe {
            // ensure they have the same bit representation
            debug_assert_eq!(transmute::<Number, i64>(num), transmute(*x));
        }
    }
    let ptr = slice.as_ptr().cast::<Number>();
    let len = slice.len();
    Ok(unsafe { std::slice::from_raw_parts(ptr, len) })
}

define_unbox!(Int, i64);

impl<'ob> From<i64> for Object<'ob> {
    fn from(x: i64) -> Self {
        let x: Number = x.into();
        x.into()
    }
}

impl<'ob> IntoObject<'ob, Object<'ob>> for i64 {
    fn into_obj(self, _arena: &'ob Arena) -> Object<'ob> {
        self.into()
    }
}

define_unbox!(Float, f64);

impl<'ob> IntoObject<'ob, Object<'ob>> for f64 {
    fn into_obj(self, arena: &'ob Arena) -> Object<'ob> {
        let obj: Number = self.into_obj(arena);
        obj.into()
    }
}

impl<'ob> From<bool> for Object<'ob> {
    fn from(b: bool) -> Self {
        InnerObject::from_tag(if b { Tag::True } else { Tag::Nil }).into()
    }
}

impl<'ob> IntoObject<'ob, Object<'ob>> for bool {
    fn into_obj(self, _arena: &'ob Arena) -> Object<'ob> {
        self.into()
    }
}

impl<'ob> IntoObject<'ob, Object<'ob>> for &str {
    fn into_obj(self, arena: &'ob Arena) -> Object<'ob> {
        InnerObject::from_type(self.to_owned(), Tag::String, arena).into()
    }
}

define_unbox!(String, &String);

impl<'ob> IntoObject<'ob, Object<'ob>> for String {
    fn into_obj(self, arena: &'ob Arena) -> Object<'ob> {
        InnerObject::from_type(self, Tag::String, arena).into()
    }
}
define_unbox!(Symbol, Symbol);

impl<'ob> From<Symbol> for Object<'ob> {
    fn from(s: Symbol) -> Self {
        let ptr = s.as_ptr();
        InnerObject::from_ptr(ptr as *mut u8, Tag::Symbol).into()
    }
}

impl<'ob> IntoObject<'ob, Object<'ob>> for Symbol {
    fn into_obj(self, _arena: &'ob Arena) -> Object<'ob> {
        self.into()
    }
}

impl<'ob> IntoObject<'ob, Object<'ob>> for Cons<'ob> {
    fn into_obj(self, arena: &'ob Arena) -> Object<'ob> {
        InnerObject::from_type(self, Tag::Cons, arena).into()
    }
}

impl<'ob, T> From<Option<T>> for Object<'ob>
where
    T: Into<Object<'ob>>,
{
    fn from(t: Option<T>) -> Self {
        match t {
            Some(x) => x.into(),
            None => NIL,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::convert::TryInto;

    fn wrapper(args: &[Object]) -> Result<i64, Error> {
        Ok(inner(
            std::convert::TryFrom::try_from(args[0])?,
            std::convert::TryFrom::try_from(args[1])?,
        ))
    }

    fn inner(arg0: Option<i64>, arg1: &Cons) -> i64 {
        let x: i64 = arg1.car().try_into().unwrap();
        arg0.unwrap() + x
    }

    #[test]
    fn test() {
        let arena = &Arena::new();
        let obj0 = 5.into_obj(arena);
        let obj1 = Cons::new(1.into(), 2.into()).into_obj(arena);
        let vec = vec![obj0, obj1];
        let res = wrapper(vec.as_slice());
        assert_eq!(6, res.unwrap());
    }
}
