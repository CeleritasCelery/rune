pub(crate) mod sub_type;
pub(crate) use sub_type::*;
pub(crate) mod func;
pub(crate) use func::*;
pub(crate) mod convert;
pub(crate) use convert::*;

use crate::arena::Arena;
use crate::cons::Cons;
use crate::symbol::Symbol;
use std::fmt;
use std::marker::PhantomData;
use std::ops::{Deref, Not};

#[derive(Copy, Clone)]
pub(crate) struct Data<T> {
    data: [u8; 7],
    marker: PhantomData<T>,
}

pub(crate) trait Inner<T> {
    fn inner(self) -> T;
}

unsafe impl<T> Send for Data<T> {}

type Fixnum = Data<i64>;

impl<T> Data<T> {
    #[inline(always)]
    fn expand(self) -> i64 {
        let data = self.data;
        let whole = [
            0, data[0], data[1], data[2], data[3], data[4], data[5], data[6],
        ];
        i64::from_le_bytes(whole) >> 8
    }

    #[inline(always)]
    fn from_int(data: i64) -> Self {
        let bytes = data.to_le_bytes();
        Data {
            data: [
                bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6],
            ],
            marker: PhantomData,
        }
    }
}

impl<'a, T> Data<&'a T> {
    fn from_ref(rf: &'a T) -> Self {
        let ptr: *const T = rf;
        Self::from_int(ptr as i64)
    }

    fn get(self) -> &'a T {
        self.inner()
    }
}

impl<'a, T> Inner<&'a T> for Data<&'a T> {
    fn inner(self) -> &'a T {
        let bits = self.expand();
        let ptr = bits as *const T;
        unsafe { &*ptr }
    }
}

impl<'a> Data<&'a Cons<'a>> {
    pub(crate) unsafe fn inner_mut(self) -> &'a mut Cons<'a> {
        let bits = self.expand();
        let ptr = bits as *mut Cons<'a>;
        &mut *ptr
    }
}

impl<'a> Data<&'a Vec<Object<'a>>> {
    unsafe fn inner_mut(self) -> &'a mut Vec<Object<'a>> {
        let bits = self.expand();
        let ptr = bits as *mut Vec<Object<'a>>;
        &mut *ptr
    }
}

impl Inner<i64> for Fixnum {
    fn inner(self) -> i64 {
        self.expand()
    }
}

impl<T> Not for Data<T>
where
    Data<T>: Inner<T>,
{
    type Output = T;

    fn not(self) -> Self::Output {
        self.inner()
    }
}

impl<T> PartialEq for Data<T>
where
    T: PartialEq + Copy,
    Data<T>: Inner<T>,
{
    fn eq(&self, other: &Self) -> bool {
        self.inner() == other.inner()
    }
}

impl<T> fmt::Display for Data<T>
where
    T: fmt::Display + Copy,
    Data<T>: Inner<T>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.inner(), f)
    }
}

impl<T> fmt::Debug for Data<T>
where
    T: fmt::Debug + Copy,
    Data<T>: Inner<T>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.inner(), f)
    }
}

impl<'a, T> Deref for Data<&'a T> {
    type Target = T;

    #[inline(always)]
    fn deref(&'_ self) -> &'a Self::Target {
        self.inner()
    }
}

impl<'a, T> AsRef<T> for Data<&'a T> {
    #[inline(always)]
    fn as_ref<'b>(&'b self) -> &'a T {
        self.inner()
    }
}

impl Fixnum {
    fn get(self) -> i64 {
        self.inner()
    }
}

#[derive(Copy, Clone, PartialEq)]
pub(crate) enum Object<'ob> {
    Int(Fixnum),
    Float(Data<&'ob f64>),
    Symbol(Data<Symbol>),
    True,
    Nil,
    Cons(Data<&'ob Cons<'ob>>),
    Vec(Data<&'ob Vec<Object<'ob>>>),
    String(Data<&'ob String>),
    LispFn(Data<&'ob LispFn<'ob>>),
    SubrFn(Data<&'ob SubrFn>),
}

#[derive(Debug, PartialEq)]
pub(crate) enum Value<'ob> {
    Symbol(Symbol),
    Int(i64),
    Float(f64),
    True,
    Nil,
    Cons(&'ob Cons<'ob>),
    Vec(&'ob Vec<Object<'ob>>),
    String(&'ob String),
    LispFn(&'ob LispFn<'ob>),
    SubrFn(&'ob SubrFn),
}

impl<'ob> Value<'ob> {
    pub(crate) const fn get_type(&self) -> crate::error::Type {
        use crate::error::Type;
        match self {
            Value::Symbol(_) => Type::Symbol,
            Value::Float(_) => Type::Float,
            Value::String(_) => Type::String,
            Value::Nil => Type::Nil,
            Value::True => Type::True,
            Value::Cons(_) => Type::Cons,
            Value::Vec(_) => Type::Vec,
            Value::Int(_) => Type::Int,
            Value::LispFn(_) | Value::SubrFn(_) => Type::Func,
        }
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

pub(crate) trait IntoObject<'ob, T> {
    fn into_obj(self, arena: &'ob Arena) -> T;
}

impl<'ob> IntoObject<'ob, Object<'ob>> for Object<'ob> {
    fn into_obj(self, _arena: &'ob Arena) -> Object<'ob> {
        self
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
    pub(crate) fn clone_in(self, arena: &'new Arena) -> Object<'new> {
        match self {
            Object::Int(x) => x.get().into(),
            Object::Cons(x) => x.clone_in(arena).into_obj(arena),
            Object::String(x) => x.get().clone().into_obj(arena),
            Object::Symbol(x) => x.get().into(),
            Object::LispFn(x) => x.clone_in(arena).into_obj(arena),
            Object::SubrFn(x) => (*x).into_obj(arena),
            Object::True => Object::True,
            Object::Nil => Object::Nil,
            Object::Float(x) => x.into_obj(arena),
            Object::Vec(x) => vec_clone_in(x.get(), arena).into_obj(arena),
        }
    }
}

impl<'ob> Object<'ob> {
    #[inline(always)]
    pub(crate) fn val(self) -> Value<'ob> {
        match self {
            Object::Symbol(x) => Value::Symbol(x.get()),
            Object::Float(x) => Value::Float(*x),
            Object::String(x) => Value::String(x.get()),
            Object::LispFn(x) => Value::LispFn(x.get()),
            Object::SubrFn(x) => Value::SubrFn(x.get()),
            Object::Nil => Value::Nil,
            Object::True => Value::True,
            Object::Cons(x) => Value::Cons(x.get()),
            Object::Vec(x) => Value::Vec(x.get()),
            Object::Int(x) => Value::Int(x.get()),
        }
    }

    pub(crate) const fn get_type(self) -> crate::error::Type {
        use crate::error::Type;
        match self {
            Object::Symbol(_) => Type::Symbol,
            Object::Float(_) => Type::Float,
            Object::String(_) => Type::String,
            Object::Nil => Type::Nil,
            Object::True => Type::True,
            Object::Cons(_) => Type::Cons,
            Object::Vec(_) => Type::Vec,
            Object::Int(_) => Type::Int,
            Object::LispFn(_) | Object::SubrFn(_) => Type::Func,
        }
    }

    pub(crate) fn ptr_eq(self, other: Object) -> bool {
        use std::mem::transmute;
        match self {
            Object::Nil => other == Object::Nil,
            Object::True => other == Object::True,
            Object::Int(_)
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

    pub(crate) fn as_symbol(self) -> anyhow::Result<Symbol> {
        use crate::error::{Error, Type};
        match self {
            Object::Symbol(x) => Ok(x.get()),
            _ => anyhow::bail!(Error::from_object(Type::Symbol, self)),
        }
    }
}

impl<'ob> Default for Object<'ob> {
    fn default() -> Self {
        Object::Nil
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
            Object::True => write!(f, "t"),
            Object::Nil => write!(f, "nil"),
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
            Object::True => write!(f, "t"),
            Object::Nil => write!(f, "nil"),
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
    use crate::symbol::intern;
    use std::mem::size_of;

    #[test]
    fn sizes() {
        assert_eq!(size_of::<isize>(), size_of::<Object>());
        assert_eq!(size_of::<Object>(), size_of::<Option<Object>>());
        assert_eq!(
            /* data */ size_of::<Object>() + /* discriminant */ size_of::<isize>(),
            size_of::<Value>()
        );
        unsafe {
            assert_eq!(
                0x1700_i64,
                std::mem::transmute(Object::Int(Data::from_int(0x17)))
            );
        }
    }

    #[test]
    fn integer() {
        let arena = &Arena::new();
        let int: Object = 3.into_obj(arena);
        assert!(matches!(int.val(), Value::Int(_)));
        assert_eq!(int.val(), Value::Int(3));
        let int: Object = 0.into_obj(arena);
        assert_eq!(int.val(), Value::Int(0));
    }

    #[test]
    fn float() {
        let arena = &Arena::new();
        let x: Object = 1.3.into_obj(arena);
        assert!(matches!(x.val(), Value::Float(_)));
        assert_eq!(x.val(), Value::Float(1.3));
    }

    #[test]
    fn string() {
        let arena = &Arena::new();
        let x: Object = "foo".into_obj(arena);
        assert!(matches!(x.val(), Value::String(_)));
        assert_eq!(x.val(), Value::String(&"foo".to_owned()));

        let x: Object = "bar".to_owned().into_obj(arena);
        assert!(matches!(x.val(), Value::String(_)));
        assert_eq!(x.val(), Value::String(&"bar".to_owned()));
    }

    #[test]
    fn vector() {
        let arena = &Arena::new();
        let vec = vec_into_object![1, 2, 3.4, "foo"; arena];
        let x: Object = vec.into_obj(arena);
        assert!(matches!(x, Object::Vec(_)));
        assert!(matches!(x.val(), Value::Vec(_)));
        assert_eq!(
            x.val(),
            Value::Vec(&vec_into_object![1, 2, 3.4, "foo"; arena])
        );
    }

    #[test]
    fn other() {
        let t = Object::True;
        matches!(t.val(), Value::True);
        assert!(t != Object::Nil);
        let n = Object::Nil;
        assert!(n == Object::Nil);

        let bool_true: Object = true.into();
        matches!(bool_true.val(), Value::True);
        let bool_false: Object = false.into();
        assert!(bool_false == Object::Nil);
    }

    #[test]
    fn symbol() {
        let symbol = intern("foo");
        let x: Object = symbol.into();
        assert!(matches!(x.val(), Value::Symbol(_)));
        assert_eq!(x.val(), Value::Symbol(symbol));
    }
}
