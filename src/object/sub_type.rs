use crate::object::*;
use enum_as_inner::EnumAsInner;

pub struct Function<'a> {
    data: InnerObject,
    marker: PhantomData<&'a ()>,
}

impl<'obj> TagObject<'obj> for Function<'obj> {}

impl<'obj> From<Function<'obj>> for Object<'obj> {
    fn from(x: Function<'obj>) -> Self {
        x.data.into()
    }
}

impl<'obj> From<InnerObject> for Function<'obj> {
    fn from(data: InnerObject) -> Self {
        Self {
            data,
            marker: PhantomData,
        }
    }
}

impl<'obj> IntoObject<'obj, Function<'obj>> for LispFn {
    fn into_obj(self, arena: &'obj Arena) -> Function<'obj> {
        InnerObject::from_type(self, Tag::LispFn, arena).into()
    }
}

impl<'obj> IntoObject<'obj, Function<'obj>> for SubrFn {
    fn into_obj(self, arena: &'obj Arena) -> Function<'obj> {
        InnerObject::from_type(self, Tag::SubrFn, arena).into()
    }
}

#[derive(Debug, EnumAsInner)]
pub enum FunctionValue<'a> {
    LispFn(&'a LispFn),
    SubrFn(&'a SubrFn),
}

impl<'a> Function<'a> {
    #[inline(always)]
    pub fn val(self) -> FunctionValue<'a> {
        match self.data.val() {
            Value::LispFn(x) => FunctionValue::LispFn(x),
            Value::SubrFn(x) => FunctionValue::SubrFn(x),
            _ => unreachable!("Function was invalid type"),
        }
    }
}

#[derive(Copy, Clone)]
pub struct Number<'a> {
    data: InnerObject,
    marker: PhantomData<&'a ()>,
}

impl<'obj> TagObject<'obj> for Number<'obj> {}

impl<'obj> From<InnerObject> for Number<'obj> {
    fn from(data: InnerObject) -> Self {
        Self {
            data,
            marker: PhantomData,
        }
    }
}

impl<'obj> From<i64> for Number<'obj> {
    fn from(x: i64) -> Self {
        InnerObject::from_tag_bits(x, Tag::Int).into()
    }
}

impl<'obj> From<Number<'obj>> for Object<'obj> {
    fn from(x: Number) -> Self {
        x.data.into()
    }
}

impl<'obj> IntoObject<'obj, Number<'obj>> for i64 {
    fn into_obj(self, _arena: &'obj Arena) -> Number<'obj> {
        self.into()
    }
}

impl<'obj> IntoObject<'obj, Number<'obj>> for f64 {
    fn into_obj(self, arena: &'obj Arena) -> Number<'obj> {
        InnerObject::from_type(self, Tag::Float, arena).into()
    }
}

#[derive(Debug, PartialEq)]
pub enum NumberValue {
    Int(i64),
    Float(f64),
}

impl<'obj> Number<'obj> {
    #[inline(always)]
    pub fn val(self) -> NumberValue {
        match self.data.val() {
            Value::Int(x) => NumberValue::Int(x),
            Value::Float(x) => NumberValue::Float(x),
            _ => unreachable!("Number was invalid type"),
        }
    }
}

impl<'obj> IntoObject<'obj, Object<'obj>> for NumberValue {
    fn into_obj(self, arena: &'obj Arena) -> Object<'obj> {
        match self {
            NumberValue::Int(x) => x.into(),
            NumberValue::Float(x) => x.into_obj(arena),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::mem::size_of;

    #[test]
    fn sub_type_size() {
        assert_eq!(8, size_of::<Function>());
        assert_eq!(8, size_of::<Option<Function>>());
        assert_eq!(16, size_of::<FunctionValue>());
        assert_eq!(8, size_of::<Number>());
        assert_eq!(8, size_of::<Option<Number>>());
        assert_eq!(16, size_of::<NumberValue>());
    }
}
