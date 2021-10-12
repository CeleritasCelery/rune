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
pub(crate) use func::*;
pub(crate) use sub_type::*;
use data::{Data, Inner};

use crate::arena::Arena;
use crate::cons::Cons;
use crate::symbol::Symbol;
use std::fmt;

#[derive(Copy, Clone, PartialEq)]
pub(crate) enum Object<'ob> {
    Int(Data<i64>),
    Float(Data<&'ob f64>),
    Symbol(Data<Symbol>),
    True(Data<()>),
    Nil(Data<()>),
    Cons(Data<&'ob Cons<'ob>>),
    Vec(Data<&'ob Vec<Object<'ob>>>),
    String(Data<&'ob String>),
    LispFn(Data<&'ob LispFn<'ob>>),
    SubrFn(Data<&'ob SubrFn>),
}

pub(crate) trait Bits {
    fn bits(self) -> u64;
}

impl<'ob> Bits for Object<'ob> {
    fn bits(self) -> u64 {
        unsafe { std::mem::transmute::<Self, u64>(self) }
    }
}

pub(crate) trait IntoObject<'ob, T> {
    fn into_obj(self, arena: &'ob Arena) -> T;
}

impl<'ob> IntoObject<'ob, Object<'ob>> for Object<'ob> {
    fn into_obj(self, _arena: &'ob Arena) -> Object<'ob> {
        self
    }
}

impl<'ob> IntoObject<'ob, Object<'ob>> for &Object<'ob> {
    fn into_obj(self, _arena: &'ob Arena) -> Object<'ob> {
        *self
    }
}

impl<'ob> IntoObject<'ob, Object<'ob>> for Option<Object<'ob>> {
    fn into_obj(self, _arena: &'ob Arena) -> Object<'ob> {
        self.unwrap_or_default()
    }
}

fn vec_clone_in<'old, 'new>(vec: &[Object<'old>], arena: &'new Arena) -> Vec<Object<'new>> {
    vec.iter().map(|x| x.clone_in(arena)).collect()
}

impl<'old, 'new> Object<'old> {
    /// Clone object in a new arena
    pub(crate) fn clone_in(self, arena: &'new Arena) -> Object<'new> {
        // TODO: Handle pointers to the same object
        match self {
            Object::Int(x) => (!x).into(),
            Object::Cons(x) => x.clone_in(arena).into_obj(arena),
            Object::String(x) => (!x).clone().into_obj(arena),
            Object::Symbol(x) => x.inner().into(),
            Object::LispFn(x) => x.clone_in(arena).into_obj(arena),
            Object::SubrFn(x) => (*x).into_obj(arena),
            Object::True(_) => Object::TRUE,
            Object::Nil(_) => Object::NIL,
            Object::Float(x) => x.into_obj(arena),
            Object::Vec(x) => vec_clone_in(&x, arena).into_obj(arena),
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

    pub(crate) fn make_read_only(&mut self) {
        match self {
            Object::Int(_) | Object::Symbol(_) | Object::True(_) | Object::Nil(_) => {}
            Object::Float(x) => x.make_read_only(),
            Object::Cons(cons_obj) => {
                if let Some(cons) = cons_obj.inner_mut() {
                    cons.make_read_only();
                }
                cons_obj.make_read_only();
            }
            Object::Vec(vec_obj) => {
                if let Some(vec) = vec_obj.inner_mut() {
                    for elem in vec {
                        elem.make_read_only();
                    }
                }
                vec_obj.make_read_only();
            }
            Object::String(x) => x.make_read_only(),
            Object::LispFn(func_obj) => {
                if let Some(func) = func_obj.inner_mut() {
                    for elem in &mut func.body.constants {
                        elem.make_read_only();
                    }
                }
                func_obj.make_read_only();
            }
            Object::SubrFn(x) => x.make_read_only(),
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
}

impl<'ob> Default for Object<'ob> {
    fn default() -> Self {
        Object::NIL
    }
}

impl<'ob> fmt::Display for Object<'ob> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Int(x) => write!(f, "{}", x),
            Object::Cons(x) => write!(f, "{}", x),
            Object::Vec(x) => write!(f, "{:?}", x),
            Object::String(x) => write!(f, "\"{}\"", x),
            Object::Symbol(x) => write!(f, "{}", x),
            Object::LispFn(x) => write!(f, "(lambda {:?})", x),
            Object::SubrFn(x) => write!(f, "{:?}", x),
            Object::True(_) => write!(f, "t"),
            Object::Nil(_) => write!(f, "nil"),
            Object::Float(x) => {
                if x.fract() == 0.0_f64 {
                    write!(f, "{:.1}", x)
                } else {
                    write!(f, "{}", x)
                }
            }
        }
    }
}

impl<'ob> fmt::Debug for Object<'ob> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Int(x) => write!(f, "{}", x),
            Object::Cons(x) => write!(f, "{:?}", x),
            Object::Vec(x) => write!(f, "{:?}", x),
            Object::String(x) => {
                write!(
                    f,
                    "\"{}\"",
                    x.chars()
                        .map(|x| if x == '\n' { '↲' } else { x })
                        .collect::<String>()
                )
            }
            Object::Symbol(x) => write!(f, "{}", x),
            Object::LispFn(x) => write!(f, "(lambda {:?})", x),
            Object::SubrFn(x) => write!(f, "{:?}", x),
            Object::True(_) => write!(f, "t"),
            Object::Nil(_) => write!(f, "nil"),
            Object::Float(x) => {
                if x.fract() == 0.0_f64 {
                    write!(f, "{:.1}", x)
                } else {
                    write!(f, "{}", x)
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::{convert::TryInto, mem::size_of};

    #[test]
    fn sizes() {
        assert_eq!(isize::BITS, 64);
        assert_eq!(size_of::<isize>(), size_of::<Object>());
        assert_eq!(size_of::<Object>(), size_of::<Option<Object>>());
        assert_eq!(0x1800_i64, unsafe {
            std::mem::transmute(Object::Int(Data::from_int(0x18)))
        });
    }

    #[test]
    fn integer() {
        let arena = &Arena::new();
        let int: Object = 3.into_obj(arena);
        assert!(matches!(int, Object::Int(_)));
        assert_eq!(int, Object::Int(Data::from_int(3)));
        let int: Object = 0.into_obj(arena);
        assert_eq!(int, Object::Int(Data::from_int(0)));
    }

    #[test]
    fn float() {
        let arena = &Arena::new();
        let x: Object = 1.3.into_obj(arena);
        assert!(matches!(x, Object::Float(_)));
        let float = 1.3;
        assert_eq!(x, Object::Float(Data::from_ref(&float)));
    }

    #[test]
    fn string() {
        let arena = &Arena::new();
        let x: Object = "foo".into_obj(arena);
        assert!(matches!(x, Object::String(_)));
        let cmp = "foo".to_owned();
        assert_eq!(x, Object::String(Data::from_ref(&cmp)));

        let x: Object = "bar".to_owned().into_obj(arena);
        assert!(matches!(x, Object::String(_)));
        let cmp = "bar".to_owned();
        assert_eq!(x, Object::String(Data::from_ref(&cmp)));
    }

    #[test]
    fn vector() {
        let arena = &Arena::new();
        let vec = vec_into_object![1, 2, 3.4, "foo"; arena];
        let x: Object = vec.into_obj(arena);
        assert!(matches!(x, Object::Vec(_)));
        assert_eq!(
            x,
            Object::Vec(Data::from_ref(&vec_into_object![1, 2, 3.4, "foo"; arena]))
        );
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
    fn mutuality() {
        let arena = &Arena::new();
        let inner_cons = Cons::new(1.into(), 4.into());
        let vec = vec_into_object![inner_cons, 2, 3, 4; arena];
        let mut obj = Cons::new(1.into(), arena.add(vec)).into_obj(arena);
        let result: Result<&mut Cons, _> = obj.try_into();
        assert!(result.is_ok());
        obj.make_read_only();
        let result: Result<&mut Cons, _> = obj.try_into();
        assert!(result.is_err());
        if let Object::Cons(cons) = obj {
            let result: Result<&mut Vec<_>, _> = cons.cdr().try_into();
            assert!(result.is_err());
            if let Object::Vec(vec) = cons.cdr() {
                let result: Result<&mut Vec<_>, _> = vec[0].try_into();
                assert!(result.is_err());
            } else {
                unreachable!("object should be vec");
            }
        } else {
            unreachable!("object should be cons");
        }
    }
}
