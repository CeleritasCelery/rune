//! This module holds implementation of conversion functions. Much of
//! this code could be replaced with macros or specialized generics if
//! those are ever stabalized.

use super::{
    super::error::{ArgError, Type, TypeError},
    ByteString, LispHashTable, LispString, LispVec, OptionalFlag, NIL, TRUE,
};
use super::{Gc, LispFloat, Object, ObjectType, Symbol};
use anyhow::Context;

impl<'ob> TryFrom<Object<'ob>> for &'ob str {
    type Error = anyhow::Error;
    fn try_from(obj: Object<'ob>) -> Result<Self, Self::Error> {
        match obj.untag() {
            ObjectType::String(x) => Ok(x),
            x => Err(TypeError::new(Type::String, x).into()),
        }
    }
}

impl TryFrom<Object<'_>> for f64 {
    type Error = anyhow::Error;
    fn try_from(obj: Object) -> Result<Self, Self::Error> {
        match obj.untag() {
            ObjectType::Int(x) => Ok(x as f64),
            ObjectType::Float(x) => Ok(**x),
            x => Err(TypeError::new(Type::Number, x).into()),
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for Option<&'ob str> {
    type Error = anyhow::Error;
    fn try_from(obj: Object<'ob>) -> Result<Self, Self::Error> {
        match obj.untag() {
            ObjectType::NIL => Ok(None),
            ObjectType::String(x) => Ok(Some(x)),
            x => Err(TypeError::new(Type::String, x).into()),
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for usize {
    type Error = anyhow::Error;
    fn try_from(obj: Object<'ob>) -> Result<Self, Self::Error> {
        match obj.untag() {
            ObjectType::Int(x) => {
                x.try_into().with_context(|| format!("Integer must be positive, but was {x}"))
            }
            x => Err(TypeError::new(Type::Int, x).into()),
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for u64 {
    type Error = anyhow::Error;
    fn try_from(obj: Object<'ob>) -> Result<Self, Self::Error> {
        match obj.untag() {
            ObjectType::Int(x) => {
                x.try_into().with_context(|| format!("Integer must be positive, but was {x}"))
            }
            x => Err(TypeError::new(Type::Int, x).into()),
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for Option<usize> {
    type Error = anyhow::Error;
    fn try_from(obj: Object<'ob>) -> Result<Self, Self::Error> {
        match obj.untag() {
            ObjectType::Int(x) => match x.try_into() {
                Ok(x) => Ok(Some(x)),
                Err(e) => Err(e).with_context(|| format!("Integer must be positive, but was {x}")),
            },
            ObjectType::NIL => Ok(None),
            _ => Err(TypeError::new(Type::Int, obj).into()),
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for bool {
    type Error = ArgError;
    fn try_from(obj: Object) -> Result<Self, Self::Error> {
        Ok(obj.is_nil())
    }
}

impl<'ob> TryFrom<Object<'ob>> for OptionalFlag {
    type Error = ArgError;
    fn try_from(obj: Object) -> Result<Self, Self::Error> {
        Ok(obj.is_nil().then_some(()))
    }
}

/// This function is required because we have no specialization yet.
/// Essentially this let's us convert one type to another "in place"
/// without the need to allocate a new slice. We ensure that the two
/// types have the exact same representation, so that no writes
/// actually need to be performed.
pub(crate) fn try_from_slice<'brw, 'ob, T, E>(
    slice: &'brw [Object<'ob>],
) -> Result<&'brw [Gc<T>], E>
where
    Gc<T>: TryFrom<Object<'ob>, Error = E> + 'ob,
{
    for x in slice {
        let _new = Gc::<T>::try_from(*x)?;
    }
    let ptr = slice.as_ptr().cast::<Gc<T>>();
    let len = slice.len();
    Ok(unsafe { std::slice::from_raw_parts(ptr, len) })
}

impl<'ob> From<bool> for Object<'ob> {
    fn from(b: bool) -> Self {
        if b {
            TRUE
        } else {
            NIL
        }
    }
}

define_unbox!(Int, i64);
define_unbox!(Float, &'ob LispFloat);
define_unbox!(HashTable, &'ob LispHashTable);
define_unbox!(String, &'ob LispString);
define_unbox!(ByteString, String, &'ob ByteString);
define_unbox!(Vec, &'ob LispVec);
define_unbox!(Symbol, Symbol<'ob>);

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
    use crate::core::cons::Cons;

    use super::super::super::gc::{Context, RootSet};

    use super::*;

    fn wrapper(args: &[Object]) -> Result<i64, TypeError> {
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
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        let obj0 = cx.add(5);
        // SAFETY: We don't call garbage collect so references are valid
        let obj1 = cx.add(Cons::new(1, 2, cx));
        let vec = vec![obj0, obj1];
        let res = wrapper(vec.as_slice());
        assert_eq!(6, res.unwrap());
    }
}
