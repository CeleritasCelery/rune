use crate::arena::{Allocation, Block};
use crate::cons::Cons;
use crate::object::{Data, IntoObject, LispFn, Object, SubrFn};
use crate::symbol::Symbol;

use super::Bits;

#[repr(align(8))]
#[derive(Copy, Clone, Debug)]
pub(crate) enum Function<'ob> {
    LispFn(Data<&'ob Allocation<LispFn<'ob>>>),
    SubrFn(Data<&'static SubrFn>),
    Uncompiled(Data<&'ob Cons<'ob>>),
    Symbol(Data<Symbol>),
}

impl<'ob> From<Function<'ob>> for Object<'ob> {
    fn from(x: Function<'ob>) -> Self {
        match x {
            Function::LispFn(x) => Object::LispFn(x),
            Function::SubrFn(x) => Object::SubrFn(x),
            Function::Uncompiled(x) => Object::Cons(x),
            Function::Symbol(x) => Object::Symbol(x),
        }
    }
}

impl<'ob> IntoObject<'ob, Function<'ob>> for LispFn<'ob> {
    fn into_obj<const C: bool>(self, block: &'ob Block<C>) -> Function<'ob> {
        let rf = block.alloc_lisp_fn(self);
        Function::LispFn(Data::from_ref(rf))
    }
}

impl<'ob> IntoObject<'ob, Function<'ob>> for SubrFn {
    fn into_obj<const C: bool>(self, block: &'ob Block<C>) -> Function<'ob> {
        let rf = block.alloc_subr_fn(self);
        Function::SubrFn(Data::from_ref(rf))
    }
}

#[repr(align(8))]
#[derive(Debug, Copy, Clone)]
pub(crate) enum FuncCell<'ob> {
    LispFn(Data<&'ob Allocation<LispFn<'ob>>>),
    SubrFn(Data<&'static SubrFn>),
    Cons(Data<&'ob Cons<'ob>>),
    Symbol(Data<Symbol>),
}

#[cfg(miri)]
extern "Rust" {
    fn miri_static_root(ptr: *const u8);
}

#[cfg(miri)]
impl<'ob> FuncCell<'ob> {
    pub(crate) fn set_as_miri_root(self) {
        match self {
            FuncCell::LispFn(x) => {
                let ptr: *const _ = &x;
                unsafe {
                    miri_static_root(ptr as _);
                }
            }
            FuncCell::SubrFn(x) => {
                let ptr: *const _ = &x;
                unsafe {
                    miri_static_root(ptr as _);
                }
            }
            FuncCell::Cons(x) => {
                let ptr: *const _ = &x;
                unsafe {
                    miri_static_root(ptr as _);
                }
            }
            FuncCell::Symbol(_) => {}
        }
    }
}

impl<'ob> IntoObject<'ob, FuncCell<'ob>> for LispFn<'ob> {
    fn into_obj<const C: bool>(self, arena: &'ob Block<C>) -> FuncCell<'ob> {
        let x: Function = self.into_obj(arena);
        x.into()
    }
}

impl<'ob> IntoObject<'ob, FuncCell<'ob>> for SubrFn {
    fn into_obj<const C: bool>(self, arena: &'ob Block<C>) -> FuncCell<'ob> {
        let x: Function = self.into_obj(arena);
        x.into()
    }
}

impl<'ob> IntoObject<'ob, FuncCell<'ob>> for Cons<'ob> {
    fn into_obj<const C: bool>(self, block: &'ob Block<C>) -> FuncCell<'ob> {
        let rf = block.alloc_cons(self);
        FuncCell::Cons(Data::from_ref(rf))
    }
}

impl<'ob> From<Function<'ob>> for FuncCell<'ob> {
    fn from(x: Function<'ob>) -> Self {
        match x {
            Function::LispFn(x) => FuncCell::LispFn(x),
            Function::SubrFn(x) => FuncCell::SubrFn(x),
            Function::Uncompiled(x) => FuncCell::Cons(x),
            Function::Symbol(x) => FuncCell::Symbol(x),
        }
    }
}

impl<'ob> From<FuncCell<'ob>> for Object<'ob> {
    fn from(x: FuncCell<'ob>) -> Self {
        match x {
            FuncCell::LispFn(x) => Object::LispFn(x),
            FuncCell::SubrFn(x) => Object::SubrFn(x),
            FuncCell::Cons(x) => Object::Cons(x),
            FuncCell::Symbol(x) => Object::Symbol(x),
        }
    }
}

impl<'ob> IntoObject<'ob, Object<'ob>> for FuncCell<'ob> {
    fn into_obj<const C: bool>(self, _arena: &'ob Block<C>) -> Object<'ob> {
        self.into()
    }
}

impl<'ob> From<&'static SubrFn> for FuncCell<'ob> {
    fn from(x: &'static SubrFn) -> Self {
        FuncCell::SubrFn(Data::from_ref(x))
    }
}

impl<'ob> IntoObject<'ob, Object<'ob>> for Option<FuncCell<'ob>> {
    fn into_obj<const C: bool>(self, _arena: &'ob Block<C>) -> Object<'ob> {
        match self {
            Some(FuncCell::LispFn(x)) => Object::LispFn(x),
            Some(FuncCell::SubrFn(x)) => Object::SubrFn(x),
            Some(FuncCell::Cons(x)) => Object::Cons(x),
            Some(FuncCell::Symbol(x)) => Object::Symbol(x),
            None => Object::NIL,
        }
    }
}

impl<'a> FuncCell<'a> {
    pub(crate) fn clone_in<'ob, const C: bool>(self, bk: &'ob Block<C>) -> FuncCell<'ob> {
        match self {
            // TODO: once gc is implemented change this to not copy the lispfn
            FuncCell::LispFn(x) => x.clone_in(bk).into_obj(bk),
            FuncCell::SubrFn(x) => FuncCell::SubrFn(x),
            FuncCell::Cons(x) => x.clone_in(bk).into_obj(bk),
            FuncCell::Symbol(x) => FuncCell::Symbol(x),
        }
    }
}

#[repr(align(8))]
#[derive(Debug, Copy, Clone)]
pub(crate) enum Callable<'ob> {
    LispFn(Data<&'ob Allocation<LispFn<'ob>>>),
    SubrFn(Data<&'static SubrFn>),
    Cons(Data<&'ob Cons<'ob>>),
}

impl<'ob> From<Callable<'ob>> for Object<'ob> {
    fn from(x: Callable<'ob>) -> Self {
        match x {
            Callable::LispFn(x) => Object::LispFn(x),
            Callable::SubrFn(x) => Object::SubrFn(x),
            Callable::Cons(x) => Object::Cons(x),
        }
    }
}

#[repr(align(8))]
#[derive(Copy, Clone)]
pub(crate) enum Number<'ob> {
    Int(Data<i64>),
    Float(Data<&'ob Allocation<f64>>),
}

impl<'ob> Bits for Number<'ob> {
    fn bits(self) -> u64 {
        unsafe { std::mem::transmute::<Self, u64>(self) }
    }
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
    fn into_obj<const C: bool>(self, _arena: &'ob Block<C>) -> Number<'ob> {
        self.into()
    }
}

impl<'ob> IntoObject<'ob, Number<'ob>> for f64 {
    fn into_obj<const C: bool>(self, block: &'ob Block<C>) -> Number<'ob> {
        let rf = block.alloc_f64(self);
        Number::Float(Data::from_ref(rf))
    }
}

#[derive(Copy, Clone)]
pub(crate) struct IntOrMarker {
    _padding: u8,
    pub(crate) int: Data<i64>,
}

impl Bits for IntOrMarker {
    fn bits(self) -> u64 {
        unsafe { std::mem::transmute::<Self, u64>(self) }
    }
}

impl IntOrMarker {
    pub(crate) fn new(int: Data<i64>) -> Self {
        Self { int, _padding: 0 }
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
            Number::Int(x) => NumberValue::Int(!x),
            Number::Float(x) => NumberValue::Float(**x),
        }
    }
}

impl<'ob> IntoObject<'ob, Object<'ob>> for NumberValue {
    fn into_obj<const C: bool>(self, arena: &'ob Block<C>) -> Object<'ob> {
        match self {
            NumberValue::Int(x) => x.into(),
            NumberValue::Float(x) => x.into_obj(arena),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub(crate) enum List<'o> {
    Nil,
    Cons(Data<&'o Cons<'o>>),
}

impl<'ob> From<List<'ob>> for Object<'ob> {
    fn from(val: List<'ob>) -> Self {
        match val {
            List::Nil => Object::NIL,
            List::Cons(cons) => Object::Cons(cons),
        }
    }
}

impl<'ob> IntoObject<'ob, Object<'ob>> for List<'ob> {
    fn into_obj<const C: bool>(self, _arena: &'ob Block<C>) -> Object<'ob> {
        self.into()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::mem::{align_of, size_of};

    #[test]
    fn sub_type_size() {
        assert_eq!(size_of::<Object>(), size_of::<Function>());
        assert_eq!(size_of::<Object>(), size_of::<FuncCell>());
        assert_eq!(size_of::<Option<Object>>(), size_of::<Option<Function>>());
        assert_eq!(size_of::<Object>(), size_of::<Number>());
        assert_eq!(size_of::<Option<Object>>(), size_of::<Option<Number>>());
        assert_eq!(align_of::<Object>(), align_of::<Function>());
        assert_eq!(align_of::<Object>(), align_of::<FuncCell>());
        assert_eq!(align_of::<Option<Object>>(), align_of::<Option<Function>>());
        assert_eq!(align_of::<Object>(), align_of::<Number>());
    }
}
