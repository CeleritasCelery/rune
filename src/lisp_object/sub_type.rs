use crate::lisp_object::*;

pub struct Function<'a>{
    data: InnerObject,
    marker: PhantomData<&'a ()>,
}

impl<'a> Function<'a> {
    fn from_bits(bits: i64) -> Self {
        Self {
            data: InnerObject {bits},
            marker: PhantomData,
        }
    }
}

impl<'obj> From<Function<'obj>> for Object<'obj> {
    fn from(x: Function<'obj>) -> Self {
        Object::from_bits(x.data.into_raw())
    }
}

impl<'a> RefObject<'a> for Function<'a> {}

impl<'a> From<LispFnObject> for Function<'a> {
    fn from(x: LispFnObject) -> Self { Self::from_bits(x.0) }
}

impl<'a> From<SubrFnObject> for Function<'a> {
    fn from(x: SubrFnObject) -> Self { Self::from_bits(x.0) }
}

pub enum FunctionValue<'a> {
    LispFn(&'a LispFn),
    SubrFn(&'a SubrFn),
}

impl<'a> Function<'a> {
    pub fn val(self) -> FunctionValue<'a> {
        match self.data.val() {
            Value::LispFn(x) => FunctionValue::LispFn(x),
            Value::SubrFn(x) => FunctionValue::SubrFn(x),
            _ => unreachable!("Function was invalid type"),
        }
    }
}

pub struct Number<'a>{
    data: InnerObject,
    marker: PhantomData<&'a ()>,
}

impl<'a> Number<'a> {
    fn from_bits(bits: i64) -> Self {
        Self {
            data: InnerObject {bits},
            marker: PhantomData,
        }
    }
}

impl<'a> RefObject<'a> for Number<'a> {}

impl<'a> From<IntObject> for Number<'a> {
    fn from(x: IntObject) -> Self { Self::from_bits(x.0) }
}

impl<'a> From<FloatObject> for Number<'a> {
    fn from(x: FloatObject) -> Self { Self::from_bits(x.0) }
}

// TODO remove
impl<'obj> From<i64> for Number<'obj> {
    fn from(x: i64) -> Self {
        Self::from_bits(LispObj::from(x).into_raw())
    }
}

impl<'obj> From<f64> for Number<'obj> {
    fn from(x: f64) -> Self {
        Self::from_bits(LispObj::from(x).into_raw())
    }
}

impl<'obj> From<Number<'obj>> for Object<'obj> {
    fn from(x: Number) -> Self {
        Object::from_bits(x.data.into_raw())
    }
}

pub enum NumberValue {
    Int(i64),
    Float(f64),
}

impl<'obj> Number<'obj> {
    pub fn val(&self) -> NumberValue {
        match self.data.val()  {
            Value::Int(x) => NumberValue::Int(x),
            Value::Float(x) => NumberValue::Float(x),
            _ => unreachable!("Number was invalid type"),
        }
    }
}
