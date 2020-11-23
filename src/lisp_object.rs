#![allow(dead_code)]

use crate::symbol::Symbol;
use crate::gc::Gc;
use std::mem::size_of;
use std::cmp;
use std::fmt;
use std::ops;
use std::convert::{From, TryFrom};

#[derive(Copy, Clone, Debug)]
pub struct Fixnum(i64);

impl From<i64> for Fixnum {
    fn from(i: i64) -> Self {Fixnum(i << TAG_SIZE)}
}

impl From<Fixnum> for i64 {
    fn from(f: Fixnum) -> Self {f.0 >> TAG_SIZE}
}

impl From<Fixnum> for LispObj {
    fn from(fixnum: Fixnum) -> Self {
        LispObj{fixnum}
    }
}

impl TryFrom<LispObj> for Fixnum {
    type Error = i64;
    fn try_from(value: LispObj) -> Result<Self, Self::Error> {
        if matches!(value.val(), Value::Int(_)) {
            Ok(unsafe{value.fixnum})
        } else {
            Err(0)
        }
    }
}

impl cmp::PartialEq for Fixnum {
    fn eq(&self, rhs: &Fixnum) -> bool {
        self.0 == rhs.0
    }
}

impl ops::Add<Fixnum> for Fixnum {
    type Output = Fixnum;
    // i + j
    fn add(self, rhs: Self) -> Self {Self(self.0 + rhs.0)}
}

impl ops::Sub<Fixnum> for Fixnum {
    type Output = Fixnum;
    // i - j
    fn sub(self, rhs: Self) -> Self {Self(self.0 - rhs.0)}
}

impl ops::Mul<Fixnum> for Fixnum {
    type Output = Fixnum;
    // i * (j >> 2)
    fn mul(self, rhs: Self) -> Self {Self(self.0 * i64::from(rhs))}
}

impl ops::Div<Fixnum> for Fixnum {
    type Output = Fixnum;
    // (i/j) << 2
    fn div(self, rhs: Self) -> Self {(self.0 / rhs.0).into()}
}

#[derive(PartialEq, Debug, Clone)]
pub struct Cons {
    pub car: LispObj,
    pub cdr: LispObj,
}

impl Cons {
    pub fn new(car: LispObj, cdr: LispObj) -> Cons {
        Cons{car, cdr}
    }

    pub fn iter(&self) -> ConsIter {
        ConsIter{cons: Some(self)}
    }
}

pub struct ConsIter<'a> {
    cons: Option<&'a Cons>,
}

impl<'a> ConsIter<'a> {
    pub fn size(&mut self) -> usize {
        self.map(|_| 1).sum()
    }
}

impl<'a> Iterator for ConsIter<'a> {
    type Item = LispObj;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(cons) = self.cons {
            let item = cons.car;
            self.cons = match (&cons.cdr).into() {
                Value::Cons(x) => Some(x),
                _ => None,
            };
            Some(item)
        } else {
            None
        }
    }
}

impl<'a> From<Option<&'a Cons>> for ConsIter<'a> {
    fn from(cons: Option<&'a Cons>) -> Self {
        ConsIter{cons}
    }
}

impl fmt::Display for Cons {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}, {})", self.car, self.cdr)
    }
}

impl From<Cons> for LispObj {
    fn from(cons: Cons) -> Self {
        LispObj::from_tagged_ptr(cons, Tag::Cons)
    }
}

#[derive(Clone, Debug)]
pub struct LispFn {
    pub op_codes: Vec<u8>,
    pub constants: Vec<LispObj>,
    pub rest_args: bool,
    pub required_args: u16,
    pub optional_args: u16,
    pub max_stack_usage: u16,
    pub advice: bool,
}

impl LispFn {
    pub fn new(op_codes: Vec<u8>, constants: Vec<LispObj>, required_args: u16, optional_args: u16, rest_args: bool) -> Self {
        LispFn {
            op_codes,
            constants,
            required_args,
            optional_args,
            rest_args,
            max_stack_usage: 0,
            advice: false,
        }
    }
}

impl From<LispFn> for LispObj {
    fn from(func: LispFn) -> Self {
        LispObj::from_tagged_ptr(func, Tag::Fn)
    }
}

#[derive(Copy, Clone)]
pub union LispObj {
    tag: Tag,
    bits: i64,
    fixnum: Fixnum,
}

impl cmp::PartialEq for LispObj {
    fn eq(&self, rhs: &LispObj) -> bool {
        match self.val() {
            Value::String(lhs) => {
                match rhs.val() {
                    Value::String(rhs) => lhs == rhs,
                    _ => false,
                }
            }
            Value::Float(lhs) => {
                match rhs.val() {
                    Value::Float(rhs) => lhs == rhs,
                    _ => false,
                }
            }
            Value::Cons(lhs) => {
                match rhs.val() {
                    Value::Cons(rhs) => lhs == rhs,
                    _ => false,
                }
            }
            _ => {
                unsafe {self.bits == rhs.bits}
            }
        }
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

#[derive(Debug, PartialEq)]
pub enum Value<'a> {
    Int(i64),
    True,
    Nil,
    Cons(&'a Cons),
    String(&'a String),
    Symbol(&'static Symbol),
    Float(f64),
    Void,
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Int,
    True,
    Nil,
    Cons,
    String,
    Symbol,
    Float,
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
    Float,
    Marker,
    True,
    Nil,
    Cons,
    Symbol,
    LongStr = 7,
    ShortStr = 8,
    Fn = 15,
    Void,
}

const TAG_SIZE: usize = size_of::<Tag>() * 8;

impl LispObj {

    pub fn get_type(&self) -> Type {
        use Type::*;
        match unsafe{self.tag} {
            Tag::Symbol => Symbol,
            Tag::Float => Float,
            Tag::Void => Void,
            Tag::LongStr => String,
            Tag::ShortStr => String,
            Tag::Nil => Nil,
            Tag::True => True,
            Tag::Cons => Cons,
            Tag::Fixnum => Int,
            _ => unreachable!(),
        }
    }

    pub fn val(&self) -> Value {
        use Value::*;
        unsafe {
            match self.tag {
                Tag::Symbol => Symbol(&*self.get_ptr()),
                Tag::Float => Float(*self.get_ptr()),
                Tag::Void => Void,
                Tag::LongStr => String(&*self.get_ptr()),
                Tag::ShortStr => String(&*self.get_ptr()),
                Tag::Nil => Nil,
                Tag::True => True,
                Tag::Cons => Cons(&*self.get_ptr()),
                Tag::Fixnum => Int(self.fixnum.into()),
                Tag::Marker => todo!(),
                Tag::Fn => todo!(),
            }
        }
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

impl From<&'static Symbol> for LispObj {
    fn from(s: &'static Symbol) -> Self {
        let ptr = s as *const Symbol;
        let bits = ((ptr as i64) << TAG_SIZE) | Tag::Symbol as i64;
        LispObj{bits}
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
        use Value::*;
        match self.val() {
            Int(x) => write!(f, "{}", x),
            Cons(x) => write!(f, "{}", x),
            String(x) => write!(f, "\"{}\"", x),
            Symbol(x) => write!(f, "'{}", x.get_name()),
            Void => write!(f, "Void"),
            True => write!(f, "t"),
            Nil => write!(f, "nil"),
            Float(x) => {
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
    use crate::symbol::INTERNED_SYMBOLS;
    use crate::symbol;

    #[test]
    fn sizes() {
        assert_eq!(8, size_of::<LispObj>());
        assert_eq!(16, size_of::<Value>());
        assert_eq!(56, size_of::<LispFn>());
        assert_eq!(16, size_of::<Cons>());
        assert_eq!(1, size_of::<Tag>());
    }

    #[test]
    fn fixnum() {
        let x = LispObj::from(7);
        assert!(Fixnum::try_from(x).is_ok());
        format!("{}", x);
        assert_eq!(7, x);
        assert_eq!(Fixnum::from(7), Fixnum::try_from(x).unwrap());
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
        assert!(v.get_type() == Type::Void);
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
        let mut symbol_map = INTERNED_SYMBOLS.lock().unwrap();
        let sym = symbol_map.intern("foo");
        let x = LispObj::from(sym);
        assert!(matches!(x.val(), Value::Symbol(_)));
        match x.val() {
            Value::Symbol(y) => assert_eq!("foo", y.get_name()),
            _ => unreachable!(),
        }
    }

    #[test]
    fn cons() {
        let cons = cons!("start", cons!(7, cons!(5, 3.3)));

        let mut x = LispObj::from(cons);
        assert!(matches!(x.val(), Value::Cons(_)));
        format!("{}", x);

        let cons1 = x.as_mut_cons().unwrap();
        assert_eq!("start", cons1.car);
        (*cons1).car = "start2".into();
        assert_eq!("start2", cons1.car);

        let cons2 = match cons1.cdr.val() {
            Value::Cons(x) => x,
            _ => unreachable!()
        };
        assert_eq!(7, cons2.car);

        let cons3 = match cons2.cdr.val() {
            Value::Cons(x) => x,
            _ => unreachable!(),
        };
        assert_eq!(5, cons3.car);
        assert_eq!(3.3, cons3.cdr);

        assert_eq!(cons!(5, "foo"), cons!(5, "foo"));
        assert_ne!(cons!(5, "foo"), cons!(5, "bar"));
        assert_eq!(list![5, 1, 1.5, "foo"], list![5, 1, 1.5, "foo"]);
        assert_ne!(list![5, 1, 1.5, "foo"], list![5, 1, 1.5, "bar"]);
    }

    #[test]
    fn cons_iter() {
        let compare: Vec<LispObj> = list![1, 2, 3, 4].iter().collect();
        let expect: Vec<LispObj> = vec_into![1, 2, 3, 4];
        assert_eq!(expect, compare);

        let compare: Vec<LispObj> = cons!(1, cons!(2, 3)).iter().collect();
        let expect: Vec<LispObj> = vec_into![1, 2];
        assert_eq!(expect, compare);
    }

    #[test]
    fn lisp_type() {
        assert_eq!(LispObj::from(1).get_type(), Type::Int);
        assert_eq!(LispObj::from(1.5).get_type(), Type::Float);
        assert_eq!(LispObj::from("foo").get_type(), Type::String);
        assert_eq!(LispObj::from(symbol::intern("foo")).get_type(), Type::Symbol);
        assert_eq!(LispObj::from(cons!(1, 2)).get_type(), Type::Cons);
        assert_eq!(LispObj::from(None::<LispObj>).get_type(), Type::Nil);
        assert_eq!(LispObj::from(false).get_type(), Type::Nil);
        assert_eq!(LispObj::nil().get_type(), Type::Nil);
        assert_eq!(LispObj::from(true).get_type(), Type::True);
        assert_eq!(LispObj::t().get_type(), Type::True);
        assert_eq!(LispObj::void().get_type(), Type::Void);
    }

    #[test]
    fn lisp_enum() {
        let obj = LispObj::from(1);
        assert!(Value::from(&obj) == Value::Int(1));
        let obj = LispObj::from("foo");
        assert!(Value::from(&obj) == Value::String(&"foo".to_string()));
    }
}
