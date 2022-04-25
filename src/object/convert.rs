//! This module holds implementation of conversion functions. Much of
//! this code could be replaced with macros or specialized generics if
//! those are ever stabalized.

use std::cell::RefCell;

use crate::arena::Arena;
use crate::cons::Cons;
use crate::error::{Error, Type};
use crate::object::Object;
use crate::symbol::{sym, Symbol};
use anyhow::Context;

use super::{CallableX, Gc, ObjectX};

impl<'ob> Cons {
    pub(crate) fn try_as_macro(&self, gc: &'ob Arena) -> anyhow::Result<Gc<CallableX<'ob>>> {
        let car = self.car(gc);
        match car.get() {
            ObjectX::Symbol(sym) if sym == &sym::MACRO => {
                let cdr = self.cdr(gc);
                let x = cdr.try_into()?;
                Ok(x)
            }
            x => Err(Error::from_object(Type::Symbol, x).into()),
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for usize {
    type Error = anyhow::Error;
    fn try_from(obj: Object<'ob>) -> Result<Self, Self::Error> {
        match obj.get() {
            ObjectX::Int(x) => (!x)
                .try_into()
                .with_context(|| format!("Integer must be positive, but was {}", !x)),
            x => Err(Error::from_object(Type::Int, x).into()),
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for Option<usize> {
    type Error = anyhow::Error;
    fn try_from(obj: Object<'ob>) -> Result<Self, Self::Error> {
        match obj.get() {
            ObjectX::Int(x) => match (!x).try_into() {
                Ok(x) => Ok(Some(x)),
                Err(e) => {
                    Err(e).with_context(|| format!("Integer must be positive, but was {}", !x))
                }
            },
            ObjectX::Nil => Ok(None),
            _ => Err(Error::from_object(Type::Int, obj).into()),
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for bool {
    type Error = Error;
    fn try_from(obj: Object) -> Result<Self, Self::Error> {
        match obj.get() {
            ObjectX::Nil => Ok(false),
            _ => Ok(true),
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for Option<bool> {
    type Error = Error;
    fn try_from(obj: Object) -> Result<Self, Self::Error> {
        match obj.get() {
            ObjectX::Nil => Ok(None),
            _ => Ok(Some(true)),
        }
    }
}

/// This function is required because we have no specialization yet.
/// Essentially this let's us convert one type to another "in place"
/// without the need to allocate a new slice. We ensure that the two
/// types have the exact same representation, so that no writes
/// actually need to be performed.
pub(crate) fn try_from_slice<'brw, 'ob, T>(
    slice: &'brw [Gc<ObjectX<'ob>>],
) -> Result<&'brw [Gc<T>], anyhow::Error>
where
    Gc<T>: TryFrom<Gc<ObjectX<'ob>>, Error = Error> + 'ob,
{
    for x in slice.iter() {
        let _new = Gc::<T>::try_from(*x)?;
    }
    let ptr = slice.as_ptr().cast::<Gc<T>>();
    let len = slice.len();
    Ok(unsafe { std::slice::from_raw_parts(ptr, len) })
}

define_unbox!(Int, i64);

define_unbox!(Float, &'ob f64);

impl<'ob> From<bool> for Object<'ob> {
    fn from(b: bool) -> Self {
        if b {
            Object::TRUE
        } else {
            Object::NIL
        }
    }
}

define_unbox!(String, &'ob String);
define_unbox!(String, &'ob str);

define_unbox!(Vec, &'ob RefCell<Vec<Object<'ob>>>);

define_unbox!(Symbol, Symbol);

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
        let obj0 = arena.add(5);
        // SAFETY: We don't call garbage collect so references are valid
        let obj1 = unsafe { arena.add(Cons::new(1.into(), 2.into())) };
        let vec = vec![obj0, obj1];
        let res = wrapper(vec.as_slice(), arena);
        assert_eq!(6, res.unwrap());
    }
}
