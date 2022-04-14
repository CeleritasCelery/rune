//! The core object defintions.
//!
//! Objects are implemented as rust enum with at max a 56 bit payload. This
//! means that it will always be 64 bits. 32 bit systems are not supported.
//! Because of this it gives us more flexibility in the amount of information we
//! can encode in the object header. For example, we can have 255 variants of
//! objects before we need to box the object header. We are making the
//! assumption that pointers are no bigger then 56 bits and that they are word
//! aligned. All objects should be bound to a lifetime to ensure sound operation
//! of the vm.

mod convert;
mod data;
mod func;
mod sub_type;

pub(crate) use convert::*;
use data::Data;
pub(crate) use data::Inner;
pub(crate) use func::*;
pub(crate) use sub_type::*;

use crate::arena::{Allocation, Block, ConstrainLifetime};
use crate::cons::Cons;
use crate::symbol::Symbol;
use std::cell::RefCell;
use std::fmt;

#[repr(u8, align(8))]
#[derive(Copy, Clone, PartialEq)]
pub(crate) enum Object<'ob> {
    Int(Data<i64>),
    Float(Data<&'ob Allocation<f64>>),
    Symbol(Data<Symbol>),
    True(Data<()>),
    Nil(Data<()>),
    Cons(Data<&'ob Cons<'ob>>),
    Vec(Data<&'ob Allocation<RefCell<Vec<Object<'ob>>>>>),
    String(Data<&'ob Allocation<String>>),
    LispFn(Data<&'ob Allocation<LispFn<'ob>>>),
    SubrFn(Data<&'static SubrFn>),
}

impl<'ob> Object<'ob> {
    pub(crate) fn mark(self) {
        match self {
            Object::Float(x) => x.get_alloc().mark(),
            Object::Cons(x) => x.mark(),
            Object::Vec(vec) => {
                for x in vec.borrow().iter() {
                    x.mark();
                }
                vec.mark();
            }
            Object::String(x) => x.get_alloc().mark(),
            Object::LispFn(x) => x.get_alloc().mark(),
            Object::Int(_)
            | Object::Symbol(_)
            | Object::True(_)
            | Object::Nil(_)
            | Object::SubrFn(_) => {}
        }
    }
}

#[repr(transparent)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub(crate) struct RawObj(u64);

impl Default for RawObj {
    fn default() -> Self {
        Object::NIL.into()
    }
}

impl<'a> From<Object<'a>> for RawObj {
    fn from(x: Object<'a>) -> Self {
        unsafe { std::mem::transmute(x) }
    }
}

pub(crate) trait Bits {
    fn bits(self) -> u64;
}

impl<'ob> Bits for Object<'ob> {
    fn bits(self) -> u64 {
        unsafe { std::mem::transmute::<Self, u64>(self) }
    }
}

impl<'ob> PartialEq<&str> for Object<'ob> {
    fn eq(&self, other: &&str) -> bool {
        match self {
            Object::String(x) => ***x == *other,
            _ => false,
        }
    }
}

impl<'ob> PartialEq<Symbol> for Object<'ob> {
    fn eq(&self, other: &Symbol) -> bool {
        match self {
            Object::Symbol(x) => !*x == *other,
            _ => false,
        }
    }
}

impl<'ob> PartialEq<f64> for Object<'ob> {
    fn eq(&self, other: &f64) -> bool {
        use float_cmp::ApproxEq;
        match self {
            Object::Float(x) => x.approx_eq(*other, (f64::EPSILON, 2)),
            _ => false,
        }
    }
}

impl<'ob> PartialEq<i64> for Object<'ob> {
    fn eq(&self, other: &i64) -> bool {
        match self {
            Object::Int(x) => !*x == *other,
            _ => false,
        }
    }
}

pub(crate) trait IntoObject<'ob, T> {
    fn into_obj<const C: bool>(self, block: &'ob Block<C>) -> T;
}

impl<'old, 'new> IntoObject<'new, Object<'new>> for Object<'old> {
    fn into_obj<const C: bool>(self, block: &'new Block<C>) -> Object<'new> {
        self.constrain_lifetime(block)
    }
}

impl<'old, 'new> IntoObject<'new, Object<'new>> for &Object<'old> {
    fn into_obj<const C: bool>(self, gc: &'new Block<C>) -> Object<'new> {
        self.constrain_lifetime(gc)
    }
}

impl<'old, 'new> IntoObject<'new, Object<'new>> for Option<Object<'old>> {
    fn into_obj<const C: bool>(self, gc: &'new Block<C>) -> Object<'new> {
        match self {
            Some(x) => x.constrain_lifetime(gc),
            None => Object::NIL,
        }
    }
}

fn vec_clone_in<'old, 'new, const C: bool>(
    vec: &[Object<'old>],
    bk: &'new Block<C>,
) -> Vec<Object<'new>> {
    vec.iter().map(|x| x.clone_in(bk)).collect()
}

impl<'old, 'new> Object<'old> {
    /// Clone object in a new arena
    pub(crate) fn clone_in<const C: bool>(self, bk: &'new Block<C>) -> Object<'new> {
        // TODO: Handle pointers to the same object
        match self {
            Object::Int(x) => (!x).into(),
            Object::Cons(x) => x.clone_in(bk).into_obj(bk),
            Object::String(x) => (!x).clone().into_obj(bk),
            Object::Symbol(x) => Object::Symbol(x),
            Object::LispFn(x) => x.clone_in(bk).into_obj(bk),
            Object::SubrFn(x) => Object::SubrFn(x),
            Object::True(_) => Object::TRUE,
            Object::Nil(_) => Object::NIL,
            Object::Float(x) => x.into_obj(bk),
            Object::Vec(x) => vec_clone_in(&x.borrow(), bk).into_obj(bk),
        }
    }
}

impl<'ob> Object<'ob> {
    pub(crate) const TRUE: Object<'ob> = Object::True(data::UNUSED);
    pub(crate) const NIL: Object<'ob> = Object::Nil(data::UNUSED);

    /// Return the type of an object
    pub(crate) const fn get_type(self) -> crate::error::Type {
        use crate::error::Type;
        match self {
            Object::Symbol(_) => Type::Symbol,
            Object::Float(_) => Type::Float,
            Object::String(_) => Type::String,
            Object::Nil(_) => Type::Nil,
            Object::True(_) => Type::True,
            Object::Cons(_) => Type::Cons,
            Object::Vec(_) => Type::Vec,
            Object::Int(_) => Type::Int,
            Object::LispFn(_) | Object::SubrFn(_) => Type::Func,
        }
    }

    pub(crate) fn ptr_eq(self, other: Object) -> bool {
        use std::mem::transmute;
        match self {
            Object::Nil(_)
            | Object::True(_)
            | Object::Int(_)
            | Object::Float(_)
            | Object::Symbol(_)
            | Object::Cons(_)
            | Object::Vec(_)
            | Object::String(_)
            | Object::LispFn(_)
            | Object::SubrFn(_) => unsafe {
                transmute::<Self, i64>(self) == transmute::<Object, i64>(other)
            },
        }
    }

    pub(crate) unsafe fn from_raw(raw: RawObj) -> Self {
        std::mem::transmute(raw)
    }
}

impl<'ob> Default for Object<'ob> {
    fn default() -> Self {
        Object::NIL
    }
}

impl<'ob> Default for &Object<'ob> {
    fn default() -> Self {
        &Object::NIL
    }
}

impl<'ob> fmt::Display for Object<'ob> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Int(x) => write!(f, "{x}"),
            Object::Cons(x) => write!(f, "{x}"),
            Object::Vec(x) => write!(f, "{x:?}"),
            Object::String(x) => write!(f, "\"{x}\""),
            Object::Symbol(x) => write!(f, "{x}"),
            Object::LispFn(x) => write!(f, "(lambda {x:?})"),
            Object::SubrFn(x) => write!(f, "{x:?}"),
            Object::True(_) => write!(f, "t"),
            Object::Nil(_) => write!(f, "nil"),
            Object::Float(x) => {
                let x = ***x;
                if x.fract() == 0.0_f64 {
                    write!(f, "{x:.1}")
                } else {
                    write!(f, "{x}")
                }
            }
        }
    }
}

impl<'ob> fmt::Debug for Object<'ob> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Int(x) => write!(f, "{x}"),
            Object::Cons(x) => write!(f, "{x:?}"),
            Object::Vec(x) => write!(f, "{x:?}"),
            Object::String(string) => {
                write!(
                    f,
                    "\"{}\"",
                    string
                        .chars()
                        .map(|x| if x == '\n' { '<' } else { x })
                        .collect::<String>()
                )
            }
            Object::Symbol(x) => write!(f, "{x}"),
            Object::LispFn(x) => write!(f, "(lambda {x:?})"),
            Object::SubrFn(x) => write!(f, "{x:?}"),
            Object::True(_) => write!(f, "t"),
            Object::Nil(_) => write!(f, "nil"),
            Object::Float(x) => {
                let x = ***x;
                if x.fract() == 0.0_f64 {
                    write!(f, "{x:.1}")
                } else {
                    write!(f, "{x}")
                }
            }
        }
    }
}

#[allow(clippy::assertions_on_constants)]
mod const_assertions {
    use super::{Data, Object};
    use std::mem::{align_of, size_of};
    const _: () = assert!(isize::BITS == 64);
    const _: () = assert!(size_of::<isize>() == size_of::<Object>());
    const _: () = assert!(align_of::<isize>() == align_of::<Object>());
    const _: () = assert!(size_of::<Object>() == size_of::<Option<Object>>());
    const _: () =
        assert!(0x1800_i64 == unsafe { std::mem::transmute(Object::Int(Data::from_int(0x18))) });
}

#[cfg(test)]
mod test {

    use crate::arena::{Arena, RootSet};

    use super::*;

    #[test]
    fn integer() {
        let roots = &RootSet::default();
        let arena = &Arena::new(roots);
        {
            let int: Object = 3.into_obj(arena);
            assert!(matches!(int, Object::Int(_)));
            assert_eq!(int, Object::Int(Data::from_int(3)));
        }
        {
            let int: Object = 0.into_obj(arena);
            assert_eq!(int, Object::Int(Data::from_int(0)));
        }
    }

    #[test]
    #[allow(clippy::float_cmp)]
    fn float() {
        let roots = &RootSet::default();
        let arena = &Arena::new(roots);
        let x: Object = 1.3.into_obj(arena);
        assert!(matches!(x, Object::Float(_)));
        assert_eq!(x, 1.3);
    }

    #[test]
    fn string() {
        let roots = &RootSet::default();
        let arena = &Arena::new(roots);
        {
            let x: Object = "foo".into_obj(arena);
            assert!(matches!(x, Object::String(_)));
            assert_eq!(x, "foo");
        }
        {
            let x: Object = "bar".to_owned().into_obj(arena);
            assert!(matches!(x, Object::String(_)));
            assert_eq!(x, "bar");
        }
    }

    #[test]
    fn vector() {
        let roots = &RootSet::default();
        let arena = &Arena::new(roots);
        let vec = vec_into_object![1, 2, 3.4, "foo"; arena];
        let x: Object = vec.into_obj(arena);
        assert!(matches!(x, Object::Vec(_)));
        let expect = vec_into_object![1, 2, 3.4, "foo"; arena];
        if let Object::Vec(compare) = x {
            for (cmp, exp) in compare.borrow().iter().zip(expect.iter()) {
                assert_eq!(cmp, exp);
            }
        } else {
            unreachable!();
        }
    }

    #[test]
    fn other() {
        let t = Object::TRUE;
        assert!(t != Object::NIL);
        let n = Object::NIL;
        assert!(n == Object::NIL);

        let bool_true: Object = true.into();
        assert!(bool_true == Object::TRUE);
        let bool_false: Object = false.into();
        assert!(bool_false == Object::NIL);
    }

    #[test]
    fn symbol() {
        use crate::symbol::sym::test;
        let symbol = &test::FOO;
        let x: Object = symbol.into();
        assert!(matches!(x, Object::Symbol(_)));
        assert_eq!(x, Object::Symbol(Data::from_ref(&test::FOO)));
    }

    #[test]
    fn mutability() {
        let bk: &Block<true> = &Block::new_local();
        let inner_cons = Cons::new(1.into(), 4.into());
        let vec = vec_into_object![inner_cons, 2, 3, 4; bk];
        let obj = Cons::new(1.into(), bk.add(vec)).into_obj(bk);
        if let Object::Cons(cons) = obj {
            if let Object::Vec(inner_vec) = cons.cdr(bk) {
                assert!(inner_vec.try_borrow_mut().is_err());
                if let Object::Cons(_) = inner_vec.try_borrow().unwrap().get(0).unwrap() {
                } else {
                    unreachable!("Type should be cons");
                }
            } else {
                unreachable!("Type should be vector");
            }
        } else {
            unreachable!("Type should be cons");
        }
    }
}
