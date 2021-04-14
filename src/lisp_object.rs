#![allow(dead_code)]

#[macro_use]
pub mod cons;
pub use cons::{Cons, ConsX};
pub mod sub_type;
pub use sub_type::*;
pub mod func;
pub use func::*;
pub mod symbol;
pub use symbol::*;
pub mod convert;
pub use convert::*;

use crate::gc::Gc;
use crate::arena::Arena;
use std::cmp;
use std::fmt;
use std::marker::PhantomData;
use std::mem::size_of;

#[derive(Copy, Clone, Debug)]
pub struct LispObj {
    data: InnerObject,
    marker: PhantomData<&'static ()>,
}

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

impl cmp::PartialEq for LispObj {
    fn eq(&self, rhs: &LispObj) -> bool {
        self.val() == rhs.val()
    }
}

impl<'a> cmp::PartialEq for Object<'a> {
    fn eq(&self, rhs: &Object) -> bool {
        self.val() == rhs.val()
    }
}

impl cmp::PartialEq<LispObj> for &str {
    fn eq(&self, rhs: &LispObj) -> bool {
        match rhs.val() {
            Value::String(x) => *self == x,
            _ => false,
        }
    }
}

impl cmp::PartialEq<LispObj> for f64 {
    fn eq(&self, rhs: &LispObj) -> bool {
        match rhs.val() {
            Value::Float(x) => *self == x,
            _ => false,
        }
    }
}

impl cmp::PartialEq<LispObj> for i64 {
    fn eq(&self, rhs: &LispObj) -> bool {
        match rhs.val() {
            Value::Int(x) => *self == x,
            _ => false,
        }
    }
}

impl cmp::PartialEq<LispObj> for bool {
    fn eq(&self, rhs: &LispObj) -> bool {
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
    Cons(&'a Cons),
    String(&'a String),
    Symbol(Symbol),
    Float(f64),
    LispFn(&'a LispFn),
    SubrFn(&'a SubrFn),
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum ValueX<'a> {
    Int(i64),
    True,
    Nil,
    Cons(&'a ConsX<'a>),
    String(&'a String),
    Symbol(Symbol),
    Float(f64),
    LispFn(&'a LispFn),
    SubrFn(&'a SubrFn),
}

impl<'a> From<&'a LispObj> for Value<'a> {
    fn from(obj: &'a LispObj) -> Self {
        obj.val()
    }
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

impl<'a> ValueX<'a> {
    pub const fn get_type(&self) -> crate::error::Type {
        use crate::error::Type::*;
        match self {
            ValueX::Symbol(_) => Symbol,
            ValueX::Float(_) => Float,
            ValueX::String(_) => String,
            ValueX::Nil => Nil,
            ValueX::True => True,
            ValueX::Cons(_) => Cons,
            ValueX::Int(_) => Int,
            ValueX::LispFn(_) => Func,
            ValueX::SubrFn(_) => Func,
        }
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
#[repr(u8)]
enum Tag {
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

const TAG_SIZE: usize = size_of::<Tag>() * 8;

pub trait IntoObject<'obj> {
    fn into_object(self, alloc: &'obj Arena) -> (Object<'obj>, bool);
}

impl<'old, 'new> Object<'old> {
    pub fn clone_in(self, arena: &'new Arena) -> Object<'new> {
        self.inner().clone_in(arena)
    }
}

impl<'a> Object<'a> {
    unsafe fn from_ptr<T>(ptr: *const T, tag: Tag) -> Self {
        let bits = ((ptr as i64) << TAG_SIZE) | tag as i64;
        Object {
            data: InnerObject { bits },
            marker: PhantomData,
        }
    }

    const fn from_tag(tag: Tag) -> Self {
        // cast to i64 to zero the high bits
        Object {
            data: InnerObject { bits: tag as i64 },
            marker: PhantomData,
        }
    }

    fn from_type<T: IntoObject<'a>>(arena: &Arena, obj: T, tag: Tag) -> (Self, bool) {
        let ptr = arena.alloc(obj);
        unsafe { (Object::from_ptr(ptr, tag), true) }
    }

    pub fn val(self) -> Value<'a> {
        let data = self.data;
        unsafe {
            match data.tag {
                Tag::Symbol => Value::Symbol(Symbol::from_raw(data.get_ptr())),
                Tag::Float => Value::Float(*data.get_ptr()),
                Tag::String => Value::String(&*data.get_ptr()),
                Tag::LispFn => Value::LispFn(&*data.get_ptr()),
                Tag::SubrFn => Value::SubrFn(&*data.get_ptr()),
                Tag::Nil => Value::Nil,
                Tag::True => Value::True,
                Tag::Cons => Value::Cons(&*data.get_ptr()),
                Tag::Int => Value::Int(data.bits >> TAG_SIZE),
            }
        }
    }

    pub fn val_x(self) -> ValueX<'a> {
        let data = self.data;
        unsafe {
            match data.tag {
                Tag::Symbol => ValueX::Symbol(Symbol::from_raw(data.get_ptr())),
                Tag::Float => ValueX::Float(*data.get_ptr()),
                Tag::String => ValueX::String(&*data.get_ptr()),
                Tag::LispFn => ValueX::LispFn(&*data.get_ptr()),
                Tag::SubrFn => ValueX::SubrFn(&*data.get_ptr()),
                Tag::Nil => ValueX::Nil,
                Tag::True => ValueX::True,
                Tag::Cons => ValueX::Cons(&*data.get_ptr()),
                Tag::Int => ValueX::Int(data.bits >> TAG_SIZE),
            }
        }
    }


    pub const fn nil() -> Self {
        Object::from_tag(Tag::Nil)
    }

    pub const fn t() -> Self {
        Object::from_tag(Tag::True)
    }

    pub const fn inner(self) -> LispObj {
        LispObj {
            data: self.data,
            marker: PhantomData,
        }
    }

    pub unsafe fn into_gc(self) -> Object<'static> {
        std::mem::transmute(self)
    }
}

impl<'a> LispObj {
    pub fn val(&'a self) -> Value<'a> {
        self.data.val()
    }

    pub fn clone_in(self, arena: &'a Arena) -> Object<'a> {
        match self.val() {
            Value::Int(x) => arena.insert(x),
            Value::Cons(x) => arena.insert(x.clone()),
            Value::String(x) => arena.insert(x.clone()),
            Value::Symbol(x) => arena.insert(x),
            Value::LispFn(x) => arena.insert(x.clone()),
            Value::SubrFn(x) => arena.insert(*x),
            Value::True => Object::t(),
            Value::Nil => Object::nil(),
            Value::Float(x) => arena.insert(x),
        }
    }

    pub fn into_raw(self) -> i64 {
        unsafe { self.data.bits }
    }

    const fn from_bits(bits: i64) -> Self {
        Self {
            data: InnerObject {bits},
            marker: PhantomData,
        }
    }

    fn from_tagged_ptr<T>(obj: T, tag: Tag) -> Self {
        let ptr = Gc::new(obj).as_ref() as *const T;
        let bits = ((ptr as i64) << TAG_SIZE) | tag as i64;
        Self::from_bits(bits)
    }

    const fn from_tag(tag: Tag) -> Self {
        // cast to i64 to zero the high bits
        Self::from_bits(tag as i64)
    }

    fn tag_eq(self, tag: Tag) -> bool {
        unsafe { self.data.tag == tag }
    }

    fn tag_masked(self, tag: Tag, mask: u16) -> bool {
        unsafe { (self.data.tag as u16) & mask == (tag as u16) }
    }

    pub const fn nil() -> Self {
        LispObj::from_tag(Tag::Nil)
    }

    pub const fn t() -> Self {
        LispObj::from_tag(Tag::True)
    }

    pub fn as_mut_cons(&mut self) -> Option<&mut Cons> {
        match self.val() {
            Value::Cons(_) => Some(unsafe { &mut *self.data.get_mut_ptr() }),
            _ => None,
        }
    }

    pub unsafe fn dealloc(mut self) {
        match self.data.tag {
            Tag::Symbol => { },
            Tag::Float => {
                let x: *mut f64 = self.data.get_mut_ptr();
                Box::from_raw(x);
            },
            Tag::String => {
                let x: *mut String = self.data.get_mut_ptr();
                Box::from_raw(x);
            }
            Tag::LispFn => {
                let x: *mut LispFn = self.data.get_mut_ptr();
                Box::from_raw(x);
            },
            Tag::SubrFn => {
                let x: *mut SubrFn = self.data.get_mut_ptr();
                Box::from_raw(x);
            },
            Tag::Nil => {},
            Tag::True => {},
            Tag::Cons => {
                let x: *mut Cons = self.data.get_mut_ptr();
                Box::from_raw(x);
            },
            Tag::Int => {},
        }
    }
}

impl InnerObject {
    unsafe fn get_ptr<T>(self) -> *const T {
        (self.bits >> TAG_SIZE) as *const T
    }

    unsafe fn get_mut_ptr<T>(&mut self) -> *mut T {
        (self.bits >> TAG_SIZE) as *mut T
    }

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

impl fmt::Display for LispObj {
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
        let arena = Arena::new();
        let int = arena.insert(3);
        assert!(matches!(int.val(), Value::Int(_)));
        assert_eq!(int.val(), Value::Int(3));
    }

    #[test]
    fn float() {
        let arena = Arena::new();
        let x = arena.insert(1.3);
        assert!(matches!(x.val(), Value::Float(_)));
        assert_eq!(x.val(), Value::Float(1.3));
    }

    #[test]
    fn string() {
        let arena = Arena::new();
        let x = arena.insert("foo");
        assert!(matches!(x.val(), Value::String(_)));
        assert_eq!(x.val(), Value::String(&"foo".to_owned()));

        let x = arena.insert("bar".to_owned());
        assert!(matches!(x.val(), Value::String(_)));
        assert_eq!(x.val(), Value::String(&"bar".to_owned()));
    }

    #[test]
    fn other() {
        let t = Object::t();
        assert_eq!(t.val(), Value::True);
        let n = Object::nil();
        assert_eq!(n.val(), Value::Nil);

        let arena = Arena::new();
        let bool_true = arena.insert(true);
        assert_eq!(bool_true.val(), Value::True);
        let bool_false = arena.insert(false);
        assert_eq!(bool_false.val(), Value::Nil);

        // Option
        let opt = arena.insert(Some(3));
        assert_eq!(opt.val(), Value::Int(3));
        let none = arena.insert(None::<bool>);
        assert_eq!(none.val(), Value::Nil);
    }

    #[test]
    fn symbol() {
        let arena = Arena::new();
        let x = arena.insert(intern("foo"));
        assert!(matches!(x.val(), Value::Symbol(_)));
        assert_eq!(x.val(), Value::Symbol(intern("foo")));
    }
}
