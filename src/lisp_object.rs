#[macro_use]
pub mod cons;
pub use cons::Cons;
pub mod sub_type;
pub use sub_type::*;
pub mod func;
pub use func::*;
pub mod symbol;
pub use symbol::*;
pub mod convert;
pub use convert::*;

use crate::arena::Arena;
use std::cmp;
use std::fmt;
use std::marker::PhantomData;
use std::mem::size_of;

#[derive(Copy, Clone)]
pub union InnerObject {
    tag: Tag,
    bits: i64,
}

#[derive(Copy, Clone, Debug)]
pub struct Object<'a> {
    data: InnerObject,
    marker: PhantomData<&'a ()>,
}
pub type GcObject = Object<'static>;

impl<'a> cmp::PartialEq for Object<'a> {
    fn eq(&self, rhs: &Object) -> bool {
        self.val() == rhs.val()
    }
}

impl<'obj> cmp::PartialEq<Object<'obj>> for &str {
    fn eq(&self, rhs: &Object) -> bool {
        match rhs.val() {
            Value::String(x) => *self == x,
            _ => false,
        }
    }
}

impl<'obj> cmp::PartialEq<Object<'obj>> for f64 {
    fn eq(&self, rhs: &Object) -> bool {
        match rhs.val() {
            Value::Float(x) => *self == x,
            _ => false,
        }
    }
}

impl<'obj> cmp::PartialEq<Object<'obj>> for i64 {
    fn eq(&self, rhs: &Object) -> bool {
        match rhs.val() {
            Value::Int(x) => *self == x,
            _ => false,
        }
    }
}

impl<'obj> cmp::PartialEq<Object<'obj>> for bool {
    fn eq(&self, rhs: &Object) -> bool {
        match rhs.val() {
            Value::Nil => !*self,
            Value::True => *self,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Value<'a> {
    Int(i64),
    True,
    Nil,
    Cons(&'a Cons<'a>),
    String(&'a String),
    Symbol(Symbol),
    Float(f64),
    LispFn(&'a LispFn),
    SubrFn(&'a SubrFn),
}

impl<'a> Value<'a> {
    pub const fn get_type(&self) -> crate::error::Type {
        use crate::error::Type::*;
        match self {
            Value::Symbol(_) => Symbol,
            Value::Float(_) => Float,
            Value::String(_) => String,
            Value::Nil => Nil,
            Value::True => True,
            Value::Cons(_) => Cons,
            Value::Int(_) => Int,
            Value::LispFn(_) => Func,
            Value::SubrFn(_) => Func,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
#[repr(u8)]
pub enum Tag {
    Int = 0,
    Float = 1,
    True = 3,
    Nil = 4,
    Cons = 5,
    Symbol = 6,
    String = 7,
    LispFn = 8,
    SubrFn,
}

impl<'obj> From<InnerObject> for Object<'obj> {
    fn from(data: InnerObject) -> Self {
        Self {
            data,
            marker: PhantomData,
        }
    }
}

const TAG_SIZE: usize = size_of::<Tag>() * 8;

pub trait IntoObject<'obj, T> {
    fn into_obj(self, arena: &'obj Arena) -> T;
}

impl<'obj, 'old: 'obj> IntoObject<'obj, Object<'obj>> for Object<'old> {
    fn into_obj(self, _arena: &'obj Arena) -> Object<'obj> {
        self
    }
}

impl<'old, 'new> Object<'old> {
    pub fn clone_in(self, arena: &'new Arena) -> Object<'new> {
        match self.val() {
            Value::Int(x) => x.into(),
            Value::Cons(x) => x.clone_in(arena).into_obj(arena),
            Value::String(x) => x.clone().into_obj(arena),
            Value::Symbol(x) => x.into(),
            Value::LispFn(x) => x.clone().into_obj(arena),
            Value::SubrFn(x) => (*x).into_obj(arena),
            Value::True => Object::t(),
            Value::Nil => Object::nil(),
            Value::Float(x) => x.into_obj(arena),
        }
    }
}

impl<'a> Object<'a> {
    #[inline(always)]
    pub fn val<'b: 'a>(self) -> Value<'b> {
        self.data.val()
    }

    pub fn nil() -> Self {
        InnerObject::from_tag(Tag::Nil).into()
    }

    pub fn t() -> Self {
        InnerObject::from_tag(Tag::True).into()
    }

    pub const fn inner(self) -> GcObject {
        GcObject {
            data: self.data,
            marker: PhantomData,
        }
    }

    pub const unsafe fn into_gc(self) -> Object<'static> {
        self.inner()
    }
}

impl<'obj> Object<'obj> {
    pub fn into_raw(self) -> i64 {
        unsafe { self.data.bits }
    }

    const fn from_bits(bits: i64) -> Self {
        Self {
            data: InnerObject { bits },
            marker: PhantomData,
        }
    }

    pub fn as_mut_cons(&mut self) -> Option<&mut Cons> {
        match self.val() {
            Value::Cons(_) => Some(unsafe { &mut *self.data.get_mut_ptr() }),
            _ => None,
        }
    }

    pub unsafe fn drop(mut self) {
        match self.data.tag {
            Tag::Symbol => {}
            Tag::Float => {
                let x: *mut f64 = self.data.get_mut_ptr();
                Box::from_raw(x);
            }
            Tag::String => {
                let x: *mut String = self.data.get_mut_ptr();
                Box::from_raw(x);
            }
            Tag::LispFn => {
                let x: *mut LispFn = self.data.get_mut_ptr();
                Box::from_raw(x);
            }
            Tag::SubrFn => {
                let x: *mut SubrFn = self.data.get_mut_ptr();
                Box::from_raw(x);
            }
            Tag::Nil => {}
            Tag::True => {}
            Tag::Cons => {
                let x: *mut Cons = self.data.get_mut_ptr();
                Box::from_raw(x);
            }
            Tag::Int => {}
        }
    }
}

impl InnerObject {
    fn from_tag_bits(bits: i64, tag: Tag) -> Self {
        let bits = (bits << TAG_SIZE) | tag as i64;
        Self { bits }
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
        Self { bits: tag as i64 }
    }

    fn get_ptr<T>(self) -> *const T {
        (unsafe { self.bits } >> TAG_SIZE) as *const T
    }

    fn get_mut_ptr<T>(&mut self) -> *mut T {
        (unsafe { self.bits } >> TAG_SIZE) as *mut T
    }

    #[inline(always)]
    pub fn val<'a>(self) -> Value<'a> {
        unsafe {
            match self.tag {
                Tag::Symbol => Value::Symbol(Symbol::from_raw(self.get_ptr())),
                Tag::Float => Value::Float(*self.get_ptr()),
                Tag::String => Value::String(&*self.get_ptr()),
                Tag::LispFn => Value::LispFn(&*self.get_ptr()),
                Tag::SubrFn => Value::SubrFn(&*self.get_ptr()),
                Tag::Nil => Value::Nil,
                Tag::True => Value::True,
                Tag::Cons => Value::Cons(&*self.get_ptr()),
                Tag::Int => Value::Int(self.bits >> TAG_SIZE),
            }
        }
    }

    fn into_raw(self) -> i64 {
        unsafe { self.bits }
    }
}

impl fmt::Display for InnerObject {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.val() {
            Value::Int(x) => write!(f, "{}", x),
            Value::Cons(x) => write!(f, "{}", x),
            Value::String(x) => write!(f, "\"{}\"", x),
            Value::Symbol(x) => write!(f, "{}", x),
            Value::LispFn(x) => write!(f, "(lambda {:?})", x),
            Value::SubrFn(x) => write!(f, "{:?}", x),
            Value::True => write!(f, "t"),
            Value::Nil => write!(f, "nil"),
            Value::Float(x) => {
                if x.fract() == 0.0 {
                    write!(f, "{:.1}", x)
                } else {
                    write!(f, "{}", x)
                }
            }
        }
    }
}

impl<'a> fmt::Display for Object<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.data.fmt(f)
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
    use crate::arena::Arena;
    use crate::intern::intern;

    #[test]
    fn sizes() {
        assert_eq!(8, size_of::<Object>());
        assert_eq!(16, size_of::<Value>());
        assert_eq!(1, size_of::<Tag>());
    }

    #[test]
    fn integer() {
        let arena = &Arena::new();
        let int: Object = 3.into_obj(arena);
        assert!(matches!(int.val(), Value::Int(_)));
        assert_eq!(int.val(), Value::Int(3));
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
        let t = Object::t();
        assert_eq!(t.val(), Value::True);
        let n = Object::nil();
        assert_eq!(n.val(), Value::Nil);

        let bool_true: Object = true.into();
        assert_eq!(bool_true.val(), Value::True);
        let bool_false: Object = false.into();
        assert_eq!(bool_false.val(), Value::Nil);
    }

    #[test]
    fn symbol() {
        let symbol = intern("foo");
        let x: Object = symbol.into();
        assert!(matches!(x.val(), Value::Symbol(_)));
        assert_eq!(x.val(), Value::Symbol(symbol));
    }
}
