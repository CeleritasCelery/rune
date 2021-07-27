use crate::arena::Arena;
use crate::cons::Cons;
use crate::object::{InnerObject, IntoObject, LispFn, Object, SubrFn, Tag, Value};
use std::marker::PhantomData;

#[derive(Copy, Clone)]
pub(crate) struct Function<'ob> {
    data: InnerObject,
    marker: PhantomData<&'ob ()>,
}

impl<'ob> std::ops::Deref for Function<'ob> {
    type Target = Object<'ob>;

    fn deref(&self) -> &Self::Target {
        let ptr: *const Self = self;
        unsafe { &*ptr.cast::<Object>() }
    }
}

impl<'ob> From<Function<'ob>> for Object<'ob> {
    fn from(x: Function<'ob>) -> Self {
        x.data.into()
    }
}

impl<'ob> From<InnerObject> for Function<'ob> {
    fn from(data: InnerObject) -> Self {
        Self {
            data,
            marker: PhantomData,
        }
    }
}

impl<'ob> IntoObject<'ob, Function<'ob>> for LispFn<'ob> {
    fn into_obj(self, arena: &'ob Arena) -> Function<'ob> {
        InnerObject::from_type(self, Tag::LispFn, arena).into()
    }
}

impl<'ob> IntoObject<'ob, Function<'ob>> for SubrFn {
    fn into_obj(self, arena: &'ob Arena) -> Function<'ob> {
        InnerObject::from_type(self, Tag::SubrFn, arena).into()
    }
}

pub(crate) enum FunctionValue<'ob> {
    LispFn(&'ob LispFn<'ob>),
    SubrFn(&'ob SubrFn),
}

impl<'ob> Function<'ob> {
    #[inline(always)]
    pub(crate) fn val(self) -> FunctionValue<'ob> {
        match self.data.val() {
            Value::LispFn(x) => FunctionValue::LispFn(x),
            Value::SubrFn(x) => FunctionValue::SubrFn(x),
            _ => unreachable!("Function was invalid type"),
        }
    }

    #[cfg(test)]
    pub(crate) fn as_lisp_fn(self) -> Option<&'ob LispFn<'ob>> {
        match self.val() {
            FunctionValue::LispFn(x) => Some(x),
            FunctionValue::SubrFn(_) => None,
        }
    }

    #[cfg(test)]
    pub(crate) fn as_subr_fn(self) -> Option<&'ob SubrFn> {
        match self.val() {
            FunctionValue::SubrFn(x) => Some(x),
            FunctionValue::LispFn(_) => None,
        }
    }

    #[cfg(miri)]
    pub(crate) fn set_as_miri_root(self) {
        match self.val() {
            FunctionValue::LispFn(x) => {
                let ptr: *const _ = x;
                unsafe {
                    miri_static_root(ptr as _);
                }
            }
            FunctionValue::SubrFn(x) => {
                let ptr: *const _ = x;
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

#[derive(Copy, Clone)]
pub(crate) struct Number<'ob> {
    data: InnerObject,
    marker: PhantomData<&'ob ()>,
}

impl<'ob> From<InnerObject> for Number<'ob> {
    fn from(data: InnerObject) -> Self {
        Self {
            data,
            marker: PhantomData,
        }
    }
}

impl<'ob> From<i64> for Number<'ob> {
    fn from(x: i64) -> Self {
        InnerObject::from_tag_bits(x, Tag::Int).into()
    }
}

impl<'ob> From<Number<'ob>> for Object<'ob> {
    fn from(x: Number) -> Self {
        x.data.into()
    }
}

impl<'ob> IntoObject<'ob, Number<'ob>> for i64 {
    fn into_obj(self, _arena: &'ob Arena) -> Number<'ob> {
        self.into()
    }
}

impl<'ob> IntoObject<'ob, Number<'ob>> for f64 {
    fn into_obj(self, arena: &'ob Arena) -> Number<'ob> {
        InnerObject::from_type(self, Tag::Float, arena).into()
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
        match self.data.val() {
            Value::Int(x) => NumberValue::Int(x),
            Value::Float(x) => NumberValue::Float(x),
            _ => unreachable!("Number was invalid type"),
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
