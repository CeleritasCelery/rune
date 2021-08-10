use crate::arena::Arena;
use crate::cons::Cons;
use crate::error::{Error, Type};
use crate::object::{Data, IntoObject, LispFn, Object, SubrFn};
use std::convert::TryFrom;

#[derive(Copy, Clone, Debug)]
pub(crate) enum Function<'ob> {
    LispFn(Data<&'ob LispFn<'ob>>),
    SubrFn(Data<&'ob SubrFn>),
}

impl<'ob> From<Function<'ob>> for Object<'ob> {
    fn from(x: Function<'ob>) -> Self {
        match x {
            Function::LispFn(x) => Object::LispFn(x),
            Function::SubrFn(x) => Object::SubrFn(x),
        }
    }
}

impl<'ob> IntoObject<'ob, Function<'ob>> for LispFn<'ob> {
    fn into_obj(self, arena: &'ob Arena) -> Function<'ob> {
        let rf = arena.alloc_lisp_fn(self);
        Function::LispFn(Data::from_ref(rf))
    }
}

impl<'ob> IntoObject<'ob, Function<'ob>> for SubrFn {
    fn into_obj(self, arena: &'ob Arena) -> Function<'ob> {
        let rf = arena.alloc_subr_fn(self);
        Function::SubrFn(Data::from_ref(rf))
    }
}

impl<'old, 'new> Function<'old> {
    pub(crate) fn clone_in(self, arena: &'new Arena) -> Function<'new> {
        match self {
            Function::LispFn(x) => (*x).clone_in(arena).into_obj(arena),
            Function::SubrFn(x) => (*x).into_obj(arena),
        }
    }
}

pub(crate) enum FunctionValue<'ob> {
    LispFn(&'ob LispFn<'ob>),
    SubrFn(&'ob SubrFn),
}

impl<'ob> Function<'ob> {
    #[inline(always)]
    pub(crate) fn val(self) -> FunctionValue<'ob> {
        match self {
            Function::LispFn(x) => FunctionValue::LispFn(x.get()),
            Function::SubrFn(x) => FunctionValue::SubrFn(x.get()),
        }
    }

    #[cfg(test)]
    pub(crate) fn as_lisp_fn(self) -> Option<&'ob LispFn<'ob>> {
        match self {
            Function::LispFn(x) => Some(x.get()),
            Function::SubrFn(_) => None,
        }
    }

    #[cfg(test)]
    pub(crate) fn as_subr_fn(self) -> Option<&'ob SubrFn> {
        match self {
            Function::SubrFn(x) => Some(x.get()),
            Function::LispFn(_) => None,
        }
    }

    #[cfg(miri)]
    pub(crate) fn set_as_miri_root(self) {
        match self {
            Function::LispFn(x) => {
                let ptr: *const _ = &x;
                unsafe {
                    miri_static_root(ptr as _);
                }
            }
            Function::SubrFn(x) => {
                let ptr: *const _ = &x;
                unsafe {
                    miri_static_root(ptr as _);
                }
            }
        }
    }
}

#[cfg(miri)]
extern "Rust" {
    fn miri_static_root(ptr: *const u8);
}

#[derive(Copy, Clone, Debug)]
pub(crate) enum LocalFunction<'ob> {
    LispFn(Data<&'ob LispFn<'ob>>),
    SubrFn(Data<&'ob SubrFn>),
    Cons(Data<&'ob Cons<'ob>>),
}

pub(crate) enum LocalFunctionValue<'ob> {
    LispFn(&'ob LispFn<'ob>),
    SubrFn(&'ob SubrFn),
    Cons(&'ob Cons<'ob>),
}

impl<'ob> LocalFunction<'ob> {
    #[inline(always)]
    pub(crate) fn val(self) -> LocalFunctionValue<'ob> {
        match self {
            LocalFunction::LispFn(x) => LocalFunctionValue::LispFn(x.get()),
            LocalFunction::SubrFn(x) => LocalFunctionValue::SubrFn(x.get()),
            LocalFunction::Cons(x) => LocalFunctionValue::Cons(x.get()),
        }
    }
}

impl<'ob> IntoObject<'ob, LocalFunction<'ob>> for LispFn<'ob> {
    fn into_obj(self, arena: &'ob Arena) -> LocalFunction<'ob> {
        let rf = arena.alloc_lisp_fn(self);
        LocalFunction::LispFn(Data::from_ref(rf))
    }
}

impl<'ob> TryFrom<LocalFunction<'ob>> for Function<'ob> {
    type Error = Error;

    fn try_from(value: LocalFunction<'ob>) -> Result<Self, Self::Error> {
        match value {
            LocalFunction::LispFn(x) => Ok(Function::LispFn(x)),
            LocalFunction::SubrFn(x) => Ok(Function::SubrFn(x)),
            LocalFunction::Cons(_) => Err(Error::Type(Type::Func, Type::Cons)),
        }
    }
}

#[derive(Copy, Clone)]
pub(crate) enum Number<'ob> {
    Int(Data<i64>),
    Float(Data<&'ob f64>),
}

impl<'ob> From<i64> for Number<'ob> {
    fn from(x: i64) -> Self {
        Number::Int(Data::from_int(x))
    }
}

impl<'ob> From<Number<'ob>> for Object<'ob> {
    fn from(x: Number<'ob>) -> Self {
        match x {
            Number::Int(x) => Object::Int(x),
            Number::Float(x) => Object::Float(x),
        }
    }
}

impl<'ob> IntoObject<'ob, Number<'ob>> for i64 {
    fn into_obj(self, _arena: &'ob Arena) -> Number<'ob> {
        self.into()
    }
}

impl<'ob> IntoObject<'ob, Number<'ob>> for f64 {
    fn into_obj(self, arena: &'ob Arena) -> Number<'ob> {
        let rf = arena.alloc_f64(self);
        Number::Float(Data::from_ref(rf))
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub(crate) enum NumberValue {
    Int(i64),
    Float(f64),
}

impl<'ob> Number<'ob> {
    #[inline(always)]
    pub(crate) fn val(self) -> NumberValue {
        match self {
            Number::Int(x) => NumberValue::Int(x.get()),
            Number::Float(x) => NumberValue::Float(*x),
        }
    }
}

impl<'ob> IntoObject<'ob, Object<'ob>> for NumberValue {
    fn into_obj(self, arena: &'ob Arena) -> Object<'ob> {
        match self {
            NumberValue::Int(x) => x.into(),
            NumberValue::Float(x) => x.into_obj(arena),
        }
    }
}

#[derive(Copy, Clone)]
pub(crate) enum List<'o> {
    Nil,
    Cons(&'o Cons<'o>),
}

#[cfg(test)]
mod test {
    use super::*;
    use std::mem::size_of;

    #[test]
    fn sub_type_size() {
        assert_eq!(size_of::<Object>(), size_of::<Function>());
        assert_eq!(size_of::<Option<Object>>(), size_of::<Option<Function>>());
        assert_eq!(size_of::<Object>(), size_of::<Number>());
        assert_eq!(size_of::<Option<Object>>(), size_of::<Option<Number>>());
    }
}
