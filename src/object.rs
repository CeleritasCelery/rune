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
use std::mem::size_of;
use std::num::NonZeroI64 as NonZero;

#[derive(Copy, Clone)]
struct InnerObject(NonZero);

#[derive(Copy, Clone, PartialEq)]
pub(crate) struct Object<'ob> {
    data: InnerObject,
    marker: PhantomData<&'ob ()>,
}

pub(crate) const NIL: Object = Object {
    data: InnerObject::from_tag(Tag::Nil),
    marker: PhantomData,
};
pub(crate) const TRUE: Object = Object {
    data: InnerObject::from_tag(Tag::True),
    marker: PhantomData,
};

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

#[derive(Clone, Copy, PartialEq, Debug)]
#[repr(u8)]
pub(crate) enum Tag {
    Symbol = 0,
    Int,
    Float,
    True,
    Nil,
    Cons,
    String,
    LispFn,
    SubrFn,
}

const TAG_SIZE: usize = size_of::<Tag>() * 8;

pub(crate) trait IntoObject<'ob, T> {
    fn into_obj(self, arena: &'ob Arena) -> T;
}

impl<'ob, 'old: 'ob> IntoObject<'ob, Object<'ob>> for Object<'old> {
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
            Value::LispFn(x) => x.clone().into_obj(arena),
            Value::SubrFn(x) => (*x).into_obj(arena),
            Value::True => TRUE,
            Value::Nil => NIL,
            Value::Float(x) => x.into_obj(arena),
        }
    }
}

impl<'ob> Object<'ob> {
    #[inline(always)]
    pub(crate) fn val<'new: 'ob>(self) -> Value<'new> {
        self.data.val()
    }

    pub(crate) unsafe fn drop(self) {
        self.data.drop();
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

impl<'ob> From<InnerObject> for Object<'ob> {
    fn from(data: InnerObject) -> Self {
        Self {
            data,
            marker: PhantomData,
        }
    }
}

impl InnerObject {
    const fn new(bits: i64) -> Self {
        unsafe { Self(NonZero::new_unchecked(bits)) }
    }

    const fn get(self) -> i64 {
        self.0.get()
    }

    const fn from_tag_bits(bits: i64, tag: Tag) -> Self {
        let tagged = (bits << TAG_SIZE) | tag as i64;
        Self::new(tagged)
    }

    fn from_ptr<T>(ptr: *const T, tag: Tag) -> Self {
        Self::from_tag_bits(ptr as i64, tag)
    }

    fn from_type<T>(obj: T, tag: Tag, arena: &Arena) -> Self {
        let ptr = arena.alloc(obj);
        let obj = Self::from_ptr(ptr, tag);
        arena.register(obj.into());
        obj
    }

    const fn from_tag(tag: Tag) -> Self {
        // cast to i64 to zero the high bits
        Self::new(tag as i64)
    }

    fn tag(self) -> Tag {
        #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
        unsafe {
            std::mem::transmute(self.get() as u8)
        }
    }

    const fn get_ptr<T>(self) -> *const T {
        (self.get() >> TAG_SIZE) as *const T
    }

    fn get_mut_ptr<T>(&mut self) -> *mut T {
        (self.get() >> TAG_SIZE) as *mut T
    }

    #[inline(always)]
    pub(crate) fn val<'ob>(self) -> Value<'ob> {
        unsafe {
            match self.tag() {
                Tag::Symbol => Value::Symbol(Symbol::from_raw(self.get_ptr())),
                Tag::Float => Value::Float(*self.get_ptr()),
                Tag::String => Value::String(&*self.get_ptr()),
                Tag::LispFn => Value::LispFn(&*self.get_ptr()),
                Tag::SubrFn => Value::SubrFn(&*self.get_ptr()),
                Tag::Nil => Value::Nil,
                Tag::True => Value::True,
                Tag::Cons => Value::Cons(&*self.get_ptr()),
                Tag::Int => Value::Int(self.get() >> TAG_SIZE),
            }
        }
    }

    pub(crate) unsafe fn drop(mut self) {
        match self.tag() {
            Tag::Float => {
                let x: *mut f64 = self.get_mut_ptr();
                Box::from_raw(x);
            }
            Tag::String => {
                let x: *mut String = self.get_mut_ptr();
                Box::from_raw(x);
            }
            Tag::LispFn => {
                let x: *mut LispFn = self.get_mut_ptr();
                Box::from_raw(x);
            }
            Tag::SubrFn => {
                let x: *mut SubrFn = self.get_mut_ptr();
                Box::from_raw(x);
            }
            Tag::Cons => {
                let x: *mut Cons = self.get_mut_ptr();
                Box::from_raw(x);
            }
            Tag::Symbol | Tag::Nil | Tag::True | Tag::Int => {}
        }
    }
}

impl PartialEq for InnerObject {
    fn eq(&self, rhs: &InnerObject) -> bool {
        self.val() == rhs.val()
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

impl fmt::Display for InnerObject {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.val(), f)
    }
}

impl fmt::Debug for InnerObject {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}
#[cfg(test)]
mod test {
    use super::*;
    use crate::symbol::intern;

    #[test]
    fn sizes() {
        assert_eq!(size_of::<isize>(), size_of::<Object>());
        assert_eq!(size_of::<Object>(), size_of::<Option<Object>>());
        assert_eq!(
            /* data */ size_of::<Object>() + /* discriminant */ size_of::<isize>(),
            size_of::<Value>()
        );
        assert_eq!(1, size_of::<Tag>());
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
        let t = TRUE;
        matches!(t.val(), Value::True);
        assert!(t.is_non_nil());
        let n = NIL;
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
