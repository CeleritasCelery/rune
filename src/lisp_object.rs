#![allow(dead_code)]

pub mod fixnum;
pub use fixnum::Fixnum;
#[macro_use]
pub mod cons;
pub use cons::Cons;
pub mod func;
pub use func::{LispFn, BuiltInFn, FnArgs};
pub mod sym;
pub use sym::{Symbol, Function, InnerSymbol};

use crate::gc::Gc;
use std::mem::size_of;
use std::cmp;
use std::fmt;

#[derive(Copy, Clone)]
pub union LispObj {
    tag: Tag,
    bits: i64,
    fixnum: Fixnum,
}

impl cmp::PartialEq for LispObj {
    fn eq(&self, rhs: &LispObj) -> bool {
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
            Value::Nil => *self == false,
            Value::True => *self == true,
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
    LispFunc(&'a LispFn),
    SubrFunc(&'a BuiltInFn),
    Void,
}

impl<'a> From<&'a LispObj> for Value<'a> {
    fn from(obj: &'a LispObj) -> Self {
        obj.val()
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
#[repr(u8)]
enum Tag {
    Fixnum = 0,
    Float = 1,
    Marker = 2,
    True = 3,
    Nil = 4,
    Cons = 5,
    Symbol = 6,
    LongStr = 7,
    ShortStr = 8,
    LispFn = 16,
    SubrFn,
    Void,
}

const TAG_SIZE: usize = size_of::<Tag>() * 8;

impl LispObj {
    pub fn val(&self) -> Value {
        unsafe {
            match self.tag {
                Tag::Symbol   => Value::Symbol(&*self.get_ptr()),
                Tag::Float    => Value::Float(*self.get_ptr()),
                Tag::Void     => Value::Void,
                Tag::LongStr  => Value::String(&*self.get_ptr()),
                Tag::ShortStr => Value::String(&*self.get_ptr()),
                Tag::LispFn   => Value::LispFunc(&*self.get_ptr()),
                Tag::SubrFn   => Value::SubrFunc(&*self.get_ptr()),
                Tag::Nil      => Value::Nil,
                Tag::True     => Value::True,
                Tag::Cons     => Value::Cons(&*self.get_ptr()),
                Tag::Fixnum   => Value::Int(self.fixnum.into()),
                Tag::Marker   => todo!(),
            }
        }
    }

    pub fn into_raw(self) -> i64 {
        unsafe{self.bits}
    }

    pub unsafe fn from_raw(bits: i64) -> Self {
        Self{bits}
    }

    unsafe fn get_ptr<T>(&self) -> *const T {
        (self.bits >> TAG_SIZE) as *const T
    }

    unsafe fn get_mut_ptr<T>(&mut self) -> *mut T {
        (self.bits >> TAG_SIZE) as *mut T
    }

    fn from_tagged_ptr<T>(obj: T, tag: Tag) -> Self {
        let ptr = Gc::new(obj).as_ref() as *const T;
        let bits = ((ptr as i64) << TAG_SIZE) | tag as i64;
        LispObj{bits}
    }

    const fn from_tag(tag: Tag) -> Self {
        // cast to i64 to zero the high bits
        LispObj{bits: tag as i64}
    }

    fn tag_eq(&self, tag: Tag) -> bool {
        unsafe {self.tag == tag}
    }

    fn tag_masked(&self, tag: Tag, mask: u16) -> bool {
        unsafe {(self.tag as u16) & mask == (tag as u16)}
    }

    pub const fn void() -> Self {
        LispObj::from_tag(Tag::Void)
    }

    pub const fn nil() -> Self {
        LispObj::from_tag(Tag::Nil)
    }

    pub const fn t() -> Self {
        LispObj::from_tag(Tag::True)
    }

    pub fn as_mut_cons(&mut self) -> Option<&mut Cons> {
        match self.val() {
            Value::Cons(_) => Some(unsafe{&mut *self.get_mut_ptr()}),
            _ => None,
        }
    }
}

impl From<i64> for LispObj {
    fn from(i: i64) -> Self {
        LispObj {fixnum: i.into()}
    }
}

impl From<f64> for LispObj {
    fn from (f: f64) -> Self {
        LispObj::from_tagged_ptr(f, Tag::Float)
    }
}

impl From<bool> for LispObj {
    fn from(b: bool) -> Self {
        LispObj::from_tag(if b {Tag::True} else {Tag::Nil})
    }
}

impl From<&str> for LispObj {
    fn from(s: &str) -> Self {
        LispObj::from_tagged_ptr(s.to_owned(), Tag::LongStr)
    }
}

impl From<String> for LispObj {
    fn from(s: String) -> Self {
        LispObj::from_tagged_ptr(s, Tag::LongStr)
    }
}

impl<T> From<Option<T>> for LispObj where T: Into<LispObj>  {
    fn from(t: Option<T>) -> Self {
        match t {
            Some(x) => x.into(),
            None => LispObj::nil(),
        }
    }
}

impl fmt::Display for LispObj {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.val() {
            Value::Int(x) => write!(f, "{}", x),
            Value::Cons(x) => write!(f, "{}", x),
            Value::String(x) => write!(f, "\"{}\"", x),
            Value::Symbol(x) => write!(f, "'{}", x.get_name()),
            Value::LispFunc(x) => write!(f, "(lambda {:?})", x),
            Value::SubrFunc(x) => write!(f, "{:?}", x),
            Value::Void => write!(f, "Void"),
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

impl fmt::Debug for LispObj {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self, f)
   }
}

pub fn run() {}

#[cfg(test)]
mod test {
    use super::*;
    use crate::symbol::intern;

    #[test]
    fn sizes() {
        assert_eq!(8, size_of::<LispObj>());
        assert_eq!(16, size_of::<Value>());
        assert_eq!(1, size_of::<Tag>());
    }

    #[test]
    fn float() {
        let x = LispObj::from(1.3);
        assert!(matches!(x.val(), Value::Float(_)));
        format!("{}", x);
        assert_eq!(1.3, x);
    }

    #[test]
    fn string() {
        let x = LispObj::from("foo");
        assert!(matches!(x.val(), Value::String(_)));
        format!("{}", x);
        match x.val() {
            Value::String(x) => assert_eq!("foo", x),
            _ => unreachable!(),
        };
        let string = LispObj::from("foo".to_owned());
        assert_eq!(string, LispObj::from("foo"));
    }

    #[test]
    fn other() {
        // Void
        let v = LispObj::void();
        assert_eq!(v.val(), Value::Void);
        // Bool
        let t = LispObj::t();
        assert_eq!(t.val(), Value::True);
        let n = LispObj::nil();
        assert_eq!(n.val(), Value::Nil);
        let bool_true = LispObj::from(true);
        assert_eq!(bool_true.val(), Value::True);
        let bool_false = LispObj::from(false);
        assert_eq!(bool_false.val(), Value::Nil);
        assert_eq!(bool_false, LispObj::from(false));
        // Option
        let opt = LispObj::from(Some(1));
        assert_eq!(opt, 1.into());
        let none = LispObj::from(None::<LispObj>);
        assert_eq!(none, LispObj::nil());
    }

    #[test]
    fn symbol() {
        let x = LispObj::from(intern("foo"));
        assert!(matches!(x.val(), Value::Symbol(_)));
        match x.val() {
            Value::Symbol(y) => assert_eq!("foo", y.get_name()),
            _ => unreachable!(),
        }
    }

    #[test]
    fn lisp_type() {
        assert!(matches!(LispObj::from(1).val(), Value::Int(_)));
        assert!(matches!(LispObj::from(1.5).val(), Value::Float(_)));
        assert!(matches!(LispObj::from("foo").val(), Value::String(_)));
        assert!(matches!(LispObj::from(intern("foo")).val(), Value::Symbol(_)));
        assert!(matches!(LispObj::from(cons!(1, 2)).val(), Value::Cons(_)));
        assert!(matches!(LispObj::from(None::<LispObj>).val(), Value::Nil));
        assert!(matches!(LispObj::from(false).val(), Value::Nil));
        assert!(matches!(LispObj::nil().val(), Value::Nil));
        assert!(matches!(LispObj::from(true).val(), Value::True));
        assert!(matches!(LispObj::t().val(), Value::True));
        assert!(matches!(LispObj::void().val(), Value::Void));
    }
}
