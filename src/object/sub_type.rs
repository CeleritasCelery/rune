use crate::arena::Arena;
use crate::cons::Cons;
use crate::object::{Data, IntoObject, LispFn, Object, SubrFn};
use crate::symbol::Symbol;

use super::Bits;

#[repr(align(8))]
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
        Function::LispFn(Data::from_mut_ref(rf))
    }
}

impl<'ob> IntoObject<'ob, Function<'ob>> for SubrFn {
    fn into_obj(self, arena: &'ob Arena) -> Function<'ob> {
        let rf = arena.alloc_subr_fn(self);
        Function::SubrFn(Data::from_mut_ref(rf))
    }
}

impl<'ob> FuncCell<'ob> {
    #[cfg(miri)]
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
            FuncCell::Macro(x) => {
                let ptr: *const _ = &x;
                unsafe {
                    miri_static_root(ptr as _);
                }
            }
            FuncCell::Symbol(_) => {}
        }
    }
}

#[cfg(miri)]
extern "Rust" {
    fn miri_static_root(ptr: *const u8);
}

#[repr(align(8))]
#[derive(Debug, Copy, Clone)]
pub(crate) enum FuncCell<'ob> {
    LispFn(Data<&'ob LispFn<'ob>>),
    SubrFn(Data<&'ob SubrFn>),
    Macro(Data<&'ob Cons<'ob>>),
    Uncompiled(Data<&'ob Cons<'ob>>),
    Symbol(Data<Symbol>),
}

impl<'ob> IntoObject<'ob, FuncCell<'ob>> for LispFn<'ob> {
    fn into_obj(self, arena: &'ob Arena) -> FuncCell<'ob> {
        let x: Function = self.into_obj(arena);
        x.into()
    }
}

impl<'ob> IntoObject<'ob, FuncCell<'ob>> for SubrFn {
    fn into_obj(self, arena: &'ob Arena) -> FuncCell<'ob> {
        let x: Function = self.into_obj(arena);
        x.into()
    }
}

impl<'ob> From<Function<'ob>> for FuncCell<'ob> {
    fn from(x: Function<'ob>) -> Self {
        match x {
            Function::LispFn(x) => FuncCell::LispFn(x),
            Function::SubrFn(x) => FuncCell::SubrFn(x),
        }
    }
}

impl<'ob> From<FuncCell<'ob>> for Object<'ob> {
    fn from(x: FuncCell<'ob>) -> Self {
        match x {
            FuncCell::LispFn(x) => Object::LispFn(x),
            FuncCell::SubrFn(x) => Object::SubrFn(x),
            FuncCell::Macro(x) | FuncCell::Uncompiled(x) => Object::Cons(x),
            FuncCell::Symbol(x) => Object::Symbol(x),
        }
    }
}

impl<'ob> IntoObject<'ob, Object<'ob>> for FuncCell<'ob> {
    fn into_obj(self, _arena: &'ob Arena) -> Object<'ob> {
        self.into()
    }
}

impl<'ob> From<&'static SubrFn> for FuncCell<'ob> {
    fn from(x: &'static SubrFn) -> Self {
        FuncCell::SubrFn(Data::from_ref(x))
    }
}

impl<'ob> IntoObject<'ob, Object<'ob>> for Option<FuncCell<'ob>> {
    fn into_obj(self, _arena: &'ob Arena) -> Object<'ob> {
        match self {
            Some(FuncCell::LispFn(x)) => Object::LispFn(x),
            Some(FuncCell::SubrFn(x)) => Object::SubrFn(x),
            Some(FuncCell::Macro(x) | FuncCell::Uncompiled(x)) => Object::Cons(x),
            Some(FuncCell::Symbol(x)) => Object::Symbol(x),
            None => Object::NIL,
        }
    }
}

impl<'a> FuncCell<'a> {
    pub(crate) fn clone_in(self, arena: &Arena) -> Object {
        match self {
            FuncCell::LispFn(x) => x.clone_in(arena).into_obj(arena),
            FuncCell::SubrFn(x) => x.into_obj(arena),
            FuncCell::Macro(x) | FuncCell::Uncompiled(x) => x.clone_in(arena).into_obj(arena),
            FuncCell::Symbol(x) => (!x).into(),
        }
    }
}

#[repr(align(8))]
#[derive(Debug, Copy, Clone)]
pub(crate) enum Callable<'ob> {
    LispFn(Data<&'ob LispFn<'ob>>),
    SubrFn(Data<&'ob SubrFn>),
    Macro(Data<&'ob Cons<'ob>>),
    Uncompiled(Data<&'ob Cons<'ob>>),
}

impl<'ob> From<Callable<'ob>> for Object<'ob> {
    fn from(x: Callable<'ob>) -> Self {
        match x {
            Callable::LispFn(x) => Object::LispFn(x),
            Callable::SubrFn(x) => Object::SubrFn(x),
            Callable::Macro(x) | Callable::Uncompiled(x) => Object::Cons(x),
        }
    }
}

#[repr(align(8))]
#[derive(Copy, Clone)]
pub(crate) enum Number<'ob> {
    Int(Data<i64>),
    Float(Data<&'ob f64>),
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
    fn into_obj(self, _arena: &'ob Arena) -> Number<'ob> {
        self.into()
    }
}

impl<'ob> IntoObject<'ob, Number<'ob>> for f64 {
    fn into_obj(self, arena: &'ob Arena) -> Number<'ob> {
        let rf = arena.alloc_f64(self);
        Number::Float(Data::from_mut_ref(rf))
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
    fn into_obj(self, _arena: &'ob Arena) -> Object<'ob> {
        self.into()
    }
}

#[derive(Clone)]
pub(crate) struct ListIterData<'ob>(List<'ob>);

impl<'ob> Iterator for ListIterData<'ob> {
    type Item = anyhow::Result<Data<&'ob Cons<'ob>>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.0 {
            List::Nil => None,
            List::Cons(cons) => {
                self.0 = match cons.cdr() {
                    Object::Cons(next) => List::Cons(next),
                    Object::Nil(_) => List::Nil,
                    _ => return Some(Err(anyhow::anyhow!("Found non-nil cdr at end of list"))),
                };
                Some(Ok(cons))
            }
        }
    }
}

#[derive(Clone)]
pub(crate) struct ListIterMut<'ob>(ListIterData<'ob>);

impl<'ob> Iterator for ListIterMut<'ob> {
    type Item = anyhow::Result<&'ob mut Cons<'ob>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.0.next() {
            Some(Ok(x)) => match x.inner_mut() {
                Some(x) => Some(Ok(x)),
                None => Some(Err(anyhow::anyhow!("attempt to modify immutable list"))),
            },
            Some(Err(e)) => Some(Err(e)),
            None => None,
        }
    }
}

impl<'ob> IntoIterator for List<'ob> {
    type Item = anyhow::Result<Data<&'ob Cons<'ob>>>;
    type IntoIter = ListIterData<'ob>;

    fn into_iter(self) -> Self::IntoIter {
        ListIterData(self)
    }
}

impl<'ob> List<'ob> {
    pub(crate) fn conses_mut(self) -> ListIterMut<'ob> {
        ListIterMut(ListIterData(self))
    }

    pub(crate) fn conses(self) -> ListIterData<'ob> {
        ListIterData(self)
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
