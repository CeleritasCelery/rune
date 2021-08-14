use crate::arena::Arena;
use crate::cons::Cons;
use crate::error::{Error, Type};
use crate::object::{Callable, Function, IntoObject, List, Number, Object};
use crate::symbol::Symbol;
use std::convert::{TryFrom, TryInto};

use super::Data;

impl<'ob> TryFrom<Object<'ob>> for Function<'ob> {
    type Error = Error;
    fn try_from(obj: Object<'ob>) -> Result<Self, Self::Error> {
        match obj {
            Object::LispFn(x) => Ok(Function::LispFn(x)),
            Object::SubrFn(x) => Ok(Function::SubrFn(x)),
            x => Err(Error::Type(Type::Func, x.get_type())),
        }
    }
}

impl<'ob> TryFrom<&'ob Cons<'ob>> for Callable<'ob> {
    type Error = Error;
    fn try_from(cons: &'ob Cons<'ob>) -> Result<Self, Self::Error> {
        match cons.car() {
            Object::Symbol(sym) if (!sym).get_name() == "macro" => {
                if matches!(cons.cdr(), Object::LispFn(_)) {
                    Ok(Callable::Macro(Data::from_ref(cons)))
                } else {
                    Err(Error::Type(Type::Func, Type::Cons))
                }
            }
            _ => Err(Error::Type(Type::Func, Type::Cons)),
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for Callable<'ob> {
    type Error = Error;
    fn try_from(obj: Object<'ob>) -> Result<Self, Self::Error> {
        match obj {
            Object::LispFn(x) => Ok(Callable::LispFn(x)),
            Object::SubrFn(x) => Ok(Callable::SubrFn(x)),
            Object::Cons(cons) => (!cons).try_into(),
            x => Err(Error::Type(Type::Func, x.get_type())),
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for Number<'ob> {
    type Error = Error;
    fn try_from(obj: Object<'ob>) -> Result<Self, Self::Error> {
        match obj {
            Object::Int(x) => Ok(Number::Int(x)),
            Object::Float(x) => Ok(Number::Float(x)),
            x => Err(Error::Type(Type::Number, x.get_type())),
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for Option<Number<'ob>> {
    type Error = Error;
    fn try_from(obj: Object<'ob>) -> Result<Self, Self::Error> {
        match obj {
            Object::Int(x) => Ok(Some(Number::Int(x))),
            Object::Float(x) => Ok(Some(Number::Float(x))),
            Object::Nil => Ok(None),
            x => Err(Error::Type(Type::Number, x.get_type())),
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for bool {
    type Error = Error;
    fn try_from(obj: Object) -> Result<Self, Self::Error> {
        match obj {
            Object::Nil => Ok(false),
            _ => Ok(true),
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for List<'ob> {
    type Error = Error;
    fn try_from(obj: Object<'ob>) -> Result<Self, Self::Error> {
        match obj {
            Object::Cons(cons) => Ok(List::Cons(!cons)),
            Object::Nil => Ok(List::Nil),
            x => Err(Error::Type(Type::List, x.get_type())),
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for Option<List<'ob>> {
    type Error = Error;
    fn try_from(obj: Object<'ob>) -> Result<Self, Self::Error> {
        match obj {
            Object::Cons(cons) => Ok(Some(List::Cons(!cons))),
            Object::Nil => Ok(None),
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
            debug_assert_eq!(
                std::mem::transmute::<Number, i64>(num),
                std::mem::transmute(*x)
            );
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
        if b {
            Object::True
        } else {
            Object::Nil
        }
    }
}

impl<'ob> IntoObject<'ob, Object<'ob>> for bool {
    fn into_obj(self, _arena: &'ob Arena) -> Object<'ob> {
        self.into()
    }
}

impl<'ob> IntoObject<'ob, Object<'ob>> for &str {
    fn into_obj(self, arena: &'ob Arena) -> Object<'ob> {
        let rf = arena.alloc_string(self.to_owned());
        Object::String(Data::from_ref(rf))
    }
}

define_unbox!(String, &'ob String);

impl<'ob> IntoObject<'ob, Object<'ob>> for String {
    fn into_obj(self, arena: &'ob Arena) -> Object<'ob> {
        let rf = arena.alloc_string(self);
        Object::String(Data::from_ref(rf))
    }
}
define_unbox!(Symbol, Symbol);

impl<'ob> From<Symbol> for Object<'ob> {
    fn from(s: Symbol) -> Self {
        Object::Symbol(Data::from_symbol(s))
    }
}

impl<'ob> IntoObject<'ob, Object<'ob>> for Symbol {
    fn into_obj(self, _arena: &'ob Arena) -> Object<'ob> {
        self.into()
    }
}

impl<'ob> IntoObject<'ob, Object<'ob>> for Cons<'ob> {
    fn into_obj(self, arena: &'ob Arena) -> Object<'ob> {
        let rf = arena.alloc_cons(self);
        Object::Cons(Data::from_ref(rf))
    }
}

impl<'ob, T> From<Option<T>> for Object<'ob>
where
    T: Into<Object<'ob>>,
{
    fn from(t: Option<T>) -> Self {
        match t {
            Some(x) => x.into(),
            None => Object::Nil,
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
