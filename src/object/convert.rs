//! This module holds implementation of conversion functions. Much of
//! this code could be replaced with macros or specialized generics if
//! those are ever stabalized.

use std::cell::RefCell;

use crate::arena::Block;
use crate::cons::Cons;
use crate::error::{Error, Type};
use crate::object::{FuncCell, Function, IntoObject, List, Number, Object};
use crate::symbol::Symbol;
use anyhow::{anyhow, Context};

use super::{Bits, Callable, Data, IntOrMarker};

impl<'ob> TryFrom<Object<'ob>> for Function<'ob> {
    type Error = anyhow::Error;
    fn try_from(obj: Object<'ob>) -> Result<Self, Self::Error> {
        match obj {
            Object::LispFn(x) => Ok(Function::LispFn(x)),
            Object::SubrFn(x) => Ok(Function::SubrFn(x)),
            Object::Cons(x) => Ok(Function::Uncompiled(x)),
            Object::Symbol(sym) => match sym.resolve_func()? {
                Some(x) => Ok(x),
                None => Err(anyhow!("Void Function: {}", sym)),
            },
            x => Err(Error::from_object(Type::Func, x).into()),
        }
    }
}

impl<'ob> TryFrom<Callable<'ob>> for Function<'ob> {
    type Error = anyhow::Error;
    fn try_from(obj: Callable<'ob>) -> Result<Self, Self::Error> {
        match obj {
            Callable::LispFn(x) => Ok(Function::LispFn(x)),
            Callable::SubrFn(x) => Ok(Function::SubrFn(x)),
            Callable::Macro(_) => Err(anyhow!("Macros are invalid as functions")),
            Callable::Uncompiled(x) => Ok(Function::Uncompiled(x)),
        }
    }
}

impl<'ob> From<&'ob Cons<'ob>> for FuncCell<'ob> {
    fn from(cons: &'ob Cons<'ob>) -> Self {
        match cons.try_into() {
            Ok(x) => FuncCell::Macro(Data::from_ref(x)),
            Err(_) => FuncCell::Uncompiled(Data::from_ref(cons)),
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for FuncCell<'ob> {
    type Error = Error;
    fn try_from(obj: Object<'ob>) -> Result<Self, Self::Error> {
        match obj {
            Object::LispFn(x) => Ok(FuncCell::LispFn(x)),
            Object::SubrFn(x) => Ok(FuncCell::SubrFn(x)),
            Object::Symbol(x) => Ok(FuncCell::Symbol(x)),
            Object::Cons(cons) => Ok((!cons).into()),
            _ => Err(Error::from_object(Type::Func, obj)),
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for Number<'ob> {
    type Error = Error;
    fn try_from(obj: Object<'ob>) -> Result<Self, Self::Error> {
        match obj {
            Object::Int(x) => Ok(Number::Int(x)),
            Object::Float(x) => Ok(Number::Float(x)),
            _ => Err(Error::from_object(Type::Number, obj)),
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for Option<Number<'ob>> {
    type Error = Error;
    fn try_from(obj: Object<'ob>) -> Result<Self, Self::Error> {
        match obj {
            Object::Int(x) => Ok(Some(Number::Int(x))),
            Object::Float(x) => Ok(Some(Number::Float(x))),
            Object::Nil(_) => Ok(None),
            _ => Err(Error::from_object(Type::Number, obj)),
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for usize {
    type Error = anyhow::Error;
    fn try_from(obj: Object<'ob>) -> Result<Self, Self::Error> {
        match obj {
            Object::Int(x) => (!x)
                .try_into()
                .with_context(|| format!("Integer must be positive, but was {}", !x)),
            _ => Err(Error::from_object(Type::Int, obj).into()),
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for Option<usize> {
    type Error = anyhow::Error;
    fn try_from(obj: Object<'ob>) -> Result<Self, Self::Error> {
        match obj {
            Object::Int(x) => match (!x).try_into() {
                Ok(x) => Ok(Some(x)),
                Err(e) => {
                    Err(e).with_context(|| format!("Integer must be positive, but was {}", !x))
                }
            },
            Object::Nil(_) => Ok(None),
            _ => Err(Error::from_object(Type::Int, obj).into()),
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for IntOrMarker {
    type Error = Error;
    fn try_from(obj: Object<'ob>) -> Result<Self, Self::Error> {
        match obj {
            Object::Int(int) => Ok(IntOrMarker::new(int)),
            _ => Err(Error::from_object(Type::Int, obj)),
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for bool {
    type Error = Error;
    fn try_from(obj: Object) -> Result<Self, Self::Error> {
        match obj {
            Object::Nil(_) => Ok(false),
            _ => Ok(true),
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for Option<bool> {
    type Error = Error;
    fn try_from(obj: Object) -> Result<Self, Self::Error> {
        match obj {
            Object::Nil(_) => Ok(None),
            _ => Ok(Some(true)),
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for List<'ob> {
    type Error = Error;
    fn try_from(obj: Object<'ob>) -> Result<Self, Self::Error> {
        match obj {
            Object::Cons(cons) => Ok(List::Cons(cons)),
            Object::Nil(_) => Ok(List::Nil),
            _ => Err(Error::from_object(Type::List, obj)),
        }
    }
}

impl<'ob> From<&'ob Cons<'ob>> for List<'ob> {
    fn from(x: &'ob Cons<'ob>) -> Self {
        List::Cons(x.into())
    }
}

/// This function is required because we have no specialization yet.
/// Essentially this let's us convert one type to another "in place"
/// without the need to allocate a new slice. We ensure that the two
/// types have the exact same representation, so that no writes
/// actually need to be performed.
pub(crate) fn try_from_slice<'brw, 'ob, T>(
    slice: &'brw [Object<'ob>],
) -> Result<&'brw [T], anyhow::Error>
where
    T: TryFrom<Object<'ob>, Error = Error> + Bits + 'ob,
{
    assert_eq!(std::mem::size_of::<Object>(), std::mem::size_of::<T>());
    for x in slice.iter() {
        let new: T = TryFrom::try_from(*x)?;
        // ensure they have the same bit representation
        debug_assert_eq!(new.bits(), x.bits());
    }
    let ptr = slice.as_ptr().cast::<T>();
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
    fn into_obj<const C: bool>(self, _arena: &'ob Block<C>) -> Object<'ob> {
        self.into()
    }
}

define_unbox!(Float, &'ob f64);

impl<'ob> IntoObject<'ob, Object<'ob>> for f64 {
    fn into_obj<const C: bool>(self, arena: &'ob Block<C>) -> Object<'ob> {
        let obj: Number = self.into_obj(arena);
        obj.into()
    }
}

impl<'ob> From<bool> for Object<'ob> {
    fn from(b: bool) -> Self {
        if b {
            Object::TRUE
        } else {
            Object::NIL
        }
    }
}

impl<'ob> IntoObject<'ob, Object<'ob>> for bool {
    fn into_obj<const C: bool>(self, _: &'ob Block<C>) -> Object<'ob> {
        self.into()
    }
}

impl<'ob> IntoObject<'ob, Object<'ob>> for &str {
    fn into_obj<const C: bool>(self, block: &'ob Block<C>) -> Object<'ob> {
        let rf = block.alloc_string(self.to_owned());
        Object::String(Data::from_ref(rf))
    }
}

define_unbox!(String, &'ob String);
define_unbox!(String, &'ob str);

impl<'ob> IntoObject<'ob, Object<'ob>> for String {
    fn into_obj<const C: bool>(self, block: &'ob Block<C>) -> Object<'ob> {
        let rf = block.alloc_string(self);
        Object::String(Data::from_ref(rf))
    }
}

define_unbox!(Vec, &'ob RefCell<Vec<Object<'ob>>>);

impl<'ob> IntoObject<'ob, Object<'ob>> for Vec<Object<'ob>> {
    fn into_obj<const C: bool>(self, block: &'ob Block<C>) -> Object<'ob> {
        let rf = block.alloc_vec(self);
        Object::Vec(Data::from_ref(rf))
    }
}

define_unbox!(Symbol, Symbol);

impl<'ob> From<Symbol> for Object<'ob> {
    fn from(s: Symbol) -> Self {
        Object::Symbol(Data::from_ref(s))
    }
}

impl<'ob> IntoObject<'ob, Object<'ob>> for Symbol {
    fn into_obj<const C: bool>(self, _arena: &'ob Block<C>) -> Object<'ob> {
        self.into()
    }
}

impl<'ob> IntoObject<'ob, Object<'ob>> for Cons<'ob> {
    fn into_obj<const C: bool>(self, block: &'ob Block<C>) -> Object<'ob> {
        let rf = block.alloc_cons(self);
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
            None => Object::NIL,
        }
    }
}

#[cfg(test)]
mod test {
    use crate::arena::{Arena, RootSet};

    use super::*;

    fn wrapper(args: &[Object], arena: &Arena) -> Result<i64, Error> {
        Ok(inner(
            std::convert::TryFrom::try_from(args[0])?,
            std::convert::TryFrom::try_from(args[1])?,
            arena,
        ))
    }

    fn inner(arg0: Option<i64>, arg1: &Cons, arena: &Arena) -> i64 {
        let x: i64 = arg1.car(arena).try_into().unwrap();
        arg0.unwrap() + x
    }

    #[test]
    fn test() {
        let roots = &RootSet::default();
        let arena = &Arena::new(roots);
        let obj0 = 5.into_obj(arena);
        let obj1 = Cons::new(1.into(), 2.into()).into_obj(arena);
        let vec = vec![obj0, obj1];
        let res = wrapper(vec.as_slice(), arena);
        assert_eq!(6, res.unwrap());
    }
}
