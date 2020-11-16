#![allow(dead_code)]

use crate::symbol::Symbol;
use crate::gc::Gc;
use std::mem::size_of;
use std::cmp;
use std::fmt;
use std::ops;
use std::convert::From;

#[derive(Copy, Clone, Debug)]
pub struct Fixnum(i64);

impl From<i64> for Fixnum {
    fn from(i: i64) -> Self {Fixnum(i << 2)}
}

impl From<Fixnum> for i64 {
    fn from(f: Fixnum) -> Self {f.0 >> 2}
}

impl From<Fixnum> for LispObj {
    fn from(fixnum: Fixnum) -> Self {
        LispObj{fixnum}
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

#[derive(PartialEq, Debug)]
pub struct Cons {
    pub car: LispObj,
    pub cdr: LispObj,
}

impl Cons {
    pub fn new(car: LispObj, cdr: LispObj) -> Cons {
        Cons{car, cdr}
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
    bits: u64,
    fixnum: Fixnum,
}

impl cmp::PartialEq for LispObj {
    fn eq(&self, rhs: &LispObj) -> bool {
        if let Some(lhs_str) = self.as_str() {
            match rhs.as_str() {
                Some(rhs_str) => lhs_str == rhs_str,
                None => false,
            }
        } else if let Some(lhs_float) = self.as_float() {
            match rhs.as_float() {
                Some(rhs_float) => lhs_float == rhs_float,
                None => false,
            }
        } else if let Some(lhs_cons) = self.as_cons() {
            match rhs.as_cons() {
                Some(rhs_cons) => lhs_cons == rhs_cons,
                None => false,
            }
        } else {
            unsafe {
                self.bits == rhs.bits
            }
        }
    }
}

impl cmp::PartialEq<LispObj> for &str {
    fn eq(&self, rhs: &LispObj) -> bool {
        match rhs.as_str() {
            Some(x) => *self == x,
            None => false,
        }
    }
}

impl cmp::PartialEq<LispObj> for f64 {
    fn eq(&self, rhs: &LispObj) -> bool {
        match rhs.as_float() {
            Some(x) => *self == x,
            None => false,
        }
    }
}

impl cmp::PartialEq<LispObj> for i64 {
    fn eq(&self, rhs: &LispObj) -> bool {
        match rhs.as_int() {
            Some(x) => *self == x,
            None => false,
        }
    }
}

enum LispObjEnum<'a> {
    Int(i64),
    True,
    Nil,
    Cons(&'a Cons),
    String(&'a str),
    Symbol(&'a Symbol),
    Float(f64),
    Void,
}

impl<'a> LispObjEnum<'a> {
    fn from(l: &'a LispObj) -> Self {
        use LispObjEnum::*;
        if let Some(x) = l.as_int() {Int(x)}
        else if let Some(x) = l.as_cons() {Cons(x)}
        else if let Some(x) = l.as_float() {Float(x)}
        else if let Some(x) = l.as_str() {String(x)}
        else if let Some(x) = l.as_symbol() {Symbol(x)}
        else if l.is_true() {True}
        else if l.is_nil() {Nil}
        else {panic!("Unknown Type")}
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
#[repr(u16)]
enum Tag {
    // Special Tags
    Fixnum   =     0b00,
    True     =     0b10,
    Nil      =   0b1010,
    Cons     =   0b1110,
    LongStr  =  0b10010,
    ShortStr =  0b10110,
    Float    = 0b100010,
    Marker   = 0b100110,
    // General Tags
    Fn = 0x00FE,
    Symbol = 0x01FE,
    Void = 0x02FE,
}

const TAG_SIZE: usize = size_of::<Tag>() * 8;
const FIXNUM_MASK: u16 = 0b11;
const STRING_MASK: u16 = 0b11111;

impl LispObj {

    unsafe fn get_ptr<T>(&self) -> *const T {
        (self.bits >> TAG_SIZE) as *const T
    }

    unsafe fn get_mut_ptr<T>(&mut self) -> *mut T {
        (self.bits >> TAG_SIZE) as *mut T
    }

    fn from_tagged_ptr<T>(obj: T, tag: Tag) -> Self {
        let ptr = Gc::new(obj).as_ref() as *const T;
        let bits = ((ptr as u64) << TAG_SIZE) | tag as u64;
        LispObj{bits}
    }

    const fn from_tag(tag: Tag) -> Self {
        // cast to u64 to zero the high bits
        LispObj{bits: tag as u64}
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

    pub fn is_fixnum(&self) -> bool {
        self.tag_masked(Tag::Fixnum, FIXNUM_MASK)
    }

    pub fn as_fixnum(self) -> Option<Fixnum> {
        if self.is_fixnum() {Some(unsafe{self.fixnum})} else {None}
    }

    pub fn as_int(self) -> Option<i64> {
        if self.is_fixnum() {Some(unsafe{self.fixnum.into()})} else {None}
    }

    pub fn is_nil(&self) -> bool {
        self.tag_eq(Tag::Nil)
    }

    pub fn is_true(&self) -> bool {
        self.tag_eq(Tag::True)
    }

    pub fn is_void(&self) -> bool {
        self.tag_eq(Tag::Void)
    }

    pub fn is_cons(&self) -> bool {
        self.tag_eq(Tag::Cons)
    }

    pub fn as_cons(&self) -> Option<&Cons> {
        if self.is_cons() {Some(unsafe{&*self.get_ptr()})} else {None}
    }

    pub fn as_mut_cons(&mut self) -> Option<&mut Cons> {
        if self.is_cons() {Some(unsafe{&mut *self.get_mut_ptr()})} else {None}
    }

    pub fn is_list(&self) -> bool {
        self.tag_eq(Tag::Cons) || self.tag_eq(Tag::Nil)
    }

    pub fn is_str(&self) -> bool {
        self.tag_masked(Tag::ShortStr, STRING_MASK) ||
        self.tag_masked(Tag::LongStr, STRING_MASK)
    }

    pub fn as_str(&self) -> Option<&str> {
        if self.is_str() {Some(unsafe{*self.get_ptr()})} else {None}
    }

    pub fn as_mut_str(&mut self) -> Option<&mut String> {
        if self.is_str() {Some(unsafe{&mut *self.get_mut_ptr()})} else {None}
    }

    pub fn is_float(&self) -> bool {
        self.tag_eq(Tag::Float)
    }

    pub fn as_float(&self) -> Option<f64> {
        if self.is_float() {unsafe {Some(*self.get_ptr())}} else {None}
    }

    pub fn is_symbol(&self) -> bool {
        self.tag_eq(Tag::Symbol)
    }

    pub fn as_symbol(&self) -> Option<&Symbol> {
        if self.is_symbol() {Some(unsafe {&*self.get_ptr()})} else {None}
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
        let bits = ((ptr as u64) << TAG_SIZE) | Tag::Symbol as u64;
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
        use LispObjEnum::*;
        match LispObjEnum::from(self) {
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

    #[test]
    fn sizes() {
        assert_eq!(8, size_of::<LispObj>());
        assert_eq!(56, size_of::<LispFn>());
        assert_eq!(16, size_of::<Cons>());
        assert_eq!(2, size_of::<Tag>());
    }

    #[test]
    fn fixnum() {
        let x = LispObj::from(7);
        assert!(x.is_fixnum());
        format!("{}", x);
        assert_eq!(7, x);
        assert_eq!(Fixnum::from(7), x.as_fixnum().unwrap());
    }

    #[test]
    fn float() {
        let x = LispObj::from(1.3);
        assert!(x.is_float());
        format!("{}", x);
        assert_eq!(1.3, x);
    }

    #[test]
    fn string() {
        let mut x = LispObj::from("foo");
        assert!(x.is_str());
        format!("{}", x);
        let str_ref = x.as_str().unwrap();
        assert_eq!("foo", str_ref);
        let mut_str = x.as_mut_str().unwrap();
        assert_eq!("foo", mut_str);
        *mut_str = "bar".to_owned();
        assert_eq!("bar", mut_str);
        assert_eq!("bar", x);
        let string = LispObj::from("foo".to_owned());
        assert_eq!(string, LispObj::from("foo"));
    }

    #[test]
    fn other() {
        // Void
        let v = LispObj::void();
        assert!(v.is_void());
        // Bool
        let t = LispObj::t();
        assert!(t.is_true());
        let n = LispObj::nil();
        assert!(n.is_nil());
        let bool_true = LispObj::from(true);
        assert!(bool_true.is_true());
        let bool_false = LispObj::from(false);
        assert!(bool_false.is_nil());
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
        assert!(x.is_symbol());
        assert_eq!("foo", x.as_symbol().unwrap().get_name());
    }

    #[test]
    fn cons() {
        let cons = cons!("start", cons!(7, cons!(5, 3.3)));

        let mut x = LispObj::from(cons);
        assert!(x.is_cons());
        format!("{}", x);

        let cons1 = x.as_mut_cons().unwrap();
        assert_eq!("start", cons1.car);
        (*cons1).car = "start2".into();
        assert_eq!("start2", cons1.car);

        let cons2 = cons1.cdr.as_cons().unwrap();
        assert_eq!(7, cons2.car);

        let cons3 = cons2.cdr.as_cons().unwrap();
        assert_eq!(5, cons3.car);
        assert_eq!(3.3, cons3.cdr);
    }

    #[test]
    fn cons_eq() {
        assert_eq!(cons!(5, "foo"), cons!(5, "foo"));
        assert_ne!(cons!(5, "foo"), cons!(5, "bar"));
        assert_eq!(cons!(5, cons!(1, cons!(1.5, "foo"))), cons!(5, cons!(1, cons!(1.5, "foo"))));
        assert_ne!(cons!(5, cons!(1, cons!(1.5, "foo"))), cons!(5, cons!(1, cons!(1.5, "bar"))));
    }
}
