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
use std::ops::Deref;

#[derive(Copy, Clone, Debug)]
pub(crate) struct Data<T> {
    data: [u8; 7],
    marker: PhantomData<T>,
}

unsafe impl<T> Send for Data<T> {}
unsafe impl<T> Sync for Data<T> {}

pub(crate) type Fixnum = Data<i64>;

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
        let bits = self.expand();
        let ptr = bits as *const T;
        unsafe { &*ptr }
    }
}

impl<'a, T> Deref for Data<&'a T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        self.get()
    }
}

impl<'a, T> AsRef<T> for Data<&'a T> {
    #[inline(always)]
    fn as_ref(&self) -> &T {
        &*self
    }
}

impl<'a, T> PartialEq for Data<&'a T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.get() == other.get()
    }
}

impl Fixnum {
    fn get(self) -> i64 {
        self.expand()
    }
}

impl Data<Symbol> {
    fn get(self) -> Symbol {
        unsafe { Symbol::from_raw(self.expand() as *const u8) }
    }

    fn from_symbol(x: Symbol) -> Self {
        Data::from_int(x.as_ptr() as i64)
    }
}

impl PartialEq for Fixnum {
    fn eq(&self, other: &Self) -> bool {
        self.get() == other.get()
    }
}

impl PartialEq for Data<Symbol> {
    fn eq(&self, other: &Self) -> bool {
        self.get() == other.get()
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
            Value::Int(_) => Type::Int,
            Value::LispFn(_) | Value::SubrFn(_) => Type::Func,
        }
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

impl<'old, 'new> Object<'old> {
    pub(crate) fn clone_in(self, arena: &'new Arena) -> Object<'new> {
        match self.val() {
            Value::Int(x) => x.into(),
            Value::Cons(x) => x.clone_in(arena).into_obj(arena),
            Value::String(x) => x.clone().into_obj(arena),
            Value::Symbol(x) => x.into(),
            Value::LispFn(x) => x.clone_in(arena).into_obj(arena),
            Value::SubrFn(x) => (*x).into_obj(arena),
            Value::True => Object::TRUE,
            Value::Nil => Object::NIL,
            Value::Float(x) => x.into_obj(arena),
        }
    }
}

impl<'ob> Object<'ob> {
    pub(crate) const NIL: Self = Object::Nil;
    pub(crate) const TRUE: Self = Object::True;

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
            Object::Int(_) => Type::Int,
            Object::LispFn(_) | Object::SubrFn(_) => Type::Func,
        }
    }

    pub(crate) fn is_nil(self) -> bool {
        matches!(self.val(), Value::Nil)
    }

    pub(crate) fn is_non_nil(self) -> bool {
        !matches!(self.val(), Value::Nil)
    }

    pub(crate) fn as_symbol(self) -> anyhow::Result<Symbol> {
        use crate::error::{Error, Type};
        match self.val() {
            Value::Symbol(x) => Ok(x),
            x => anyhow::bail!(Error::Type(Type::Symbol, x.get_type())),
        }
    }
}

impl<'ob> fmt::Display for Value<'ob> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Int(x) => write!(f, "{}", x),
            Value::Cons(x) => write!(f, "{}", x),
            Value::String(x) => write!(f, "\"{}\"", x),
            Value::Symbol(x) => write!(f, "{}", x),
            Value::LispFn(x) => write!(f, "(lambda {:?})", x),
            Value::SubrFn(x) => write!(f, "{:?}", x),
            Value::True => write!(f, "t"),
            Value::Nil => write!(f, "nil"),
            Value::Float(x) => {
                if x.fract() == 0.0_f64 {
                    write!(f, "{:.1}", x)
                } else {
                    write!(f, "{}", x)
                }
            }
        }
    }
}

impl<'ob> fmt::Display for Object<'ob> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.val(), f)
    }
}

impl<'ob> fmt::Debug for Object<'ob> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self, f)
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
    fn other() {
        let t = Object::TRUE;
        matches!(t.val(), Value::True);
        assert!(t.is_non_nil());
        let n = Object::NIL;
        assert!(n.is_nil());

        let bool_true: Object = true.into();
        matches!(bool_true.val(), Value::True);
        let bool_false: Object = false.into();
        assert!(bool_false.is_nil());
    }

    #[test]
    fn symbol() {
        let symbol = intern("foo");
        let x: Object = symbol.into();
        assert!(matches!(x.val(), Value::Symbol(_)));
        assert_eq!(x.val(), Value::Symbol(symbol));
    }
}
