#![allow(dead_code)]

use std::mem::size_of;
use std::rc::Rc;
use std::fmt;
use std::ops;

use std::convert::From;

#[derive(Copy, Clone)]
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

impl std::cmp::PartialEq for Fixnum {
    fn eq(&self, rhs: &Fixnum) -> bool {
        self.0 == rhs.0
    }
}

impl fmt::Debug for Fixnum {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", i64::from(*self))
    }
}

impl ops::Add<Fixnum> for Fixnum {
    type Output = Fixnum;
    // i + j
    fn add(self, rhs: Fixnum) -> Fixnum {Fixnum(self.0 + rhs.0)}
}

impl ops::Sub<Fixnum> for Fixnum {
    type Output = Fixnum;
    // i - j
    fn sub(self, rhs: Fixnum) -> Fixnum {Fixnum(self.0 - rhs.0)}
}

impl ops::Mul<Fixnum> for Fixnum {
    type Output = Fixnum;
    // i * (j >> 2)
    fn mul(self, rhs: Fixnum) -> Fixnum {Fixnum(self.0 * i64::from(rhs))}
}

impl ops::Div<Fixnum> for Fixnum {
    type Output = Fixnum;
    // (i/j) << 2
    fn div(self, rhs: Fixnum) -> Fixnum {(self.0 / rhs.0).into()}
}

pub struct Cons {
    pub car: LispObj,
    pub cdr: LispObj,
}

impl Cons {
    fn new<T, U>(car: T, cdr: U) -> Cons where
        T: Into<LispObj>, U: Into<LispObj> {
        Cons{car: car.into(), cdr: cdr.into()}
    }
}

impl fmt::Display for Cons {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}, {})", self.car, self.cdr)
    }
}

impl From<Cons> for LispObj {
    fn from(cons: Cons) -> Self {
        LispObj::tag_ptr(cons, Tag::Cons)
    }
}

pub struct Symbol {
    pub name: String,
    pub func: Option<Rc<LispFn>>,
    pub var: LispObj,
}

impl Symbol {
    fn new(name: String) -> Self {
        Symbol{name, func: None, var: LispObj::void()}
    }
}

pub mod symbol_intern {
    use once_cell::unsync::Lazy;
    use std::collections::HashMap;
    use std::hash::BuildHasherDefault;
    use fnv::{FnvHashMap, FnvHasher};
    use super::Symbol;

    type FastHash = Lazy<HashMap<String, Symbol, BuildHasherDefault<FnvHasher>>>;
    static mut INTERNED_SYMBOLS: FastHash = Lazy::new(|| {FnvHashMap::default()});

    pub fn intern(name: &str) -> &Symbol {
        unsafe {
            match INTERNED_SYMBOLS.get(name) {
                Some(x) => {x}
                None => {
                    let sym = Symbol::new(name.to_owned());
                    let x = INTERNED_SYMBOLS.entry(name.to_owned()).or_insert(sym);
                    x
                }
            }
        }
    }

    pub fn intern_mut(name: &str) -> &mut Symbol {
        unsafe {
            match INTERNED_SYMBOLS.get_mut(name) {
                Some(x) => {x}
                None => {
                    let sym = Symbol::new(name.to_owned());
                    let x = INTERNED_SYMBOLS.entry(name.to_owned()).or_insert(sym);
                    x
                }
            }
        }

    }
}

#[derive(Clone)]
pub struct LispFn {
    pub op_codes: Vec<u8>,
    pub name: String,
    pub constants: Vec<LispObj>,
    pub rest_args: bool,
    pub required_args: u16,
    pub optional_args: u16,
    pub max_stack_usage: u16,
}

impl  LispFn {
    pub fn new(code: u8) -> Self {
        LispFn {
            op_codes: vec![code],
            name: "void".to_owned(),
            constants: Vec::new(),
            rest_args: false,
            required_args: 0,
            optional_args: 0,
            max_stack_usage: 0,
        }
    }
}

impl From<LispFn> for LispObj {
    fn from(func: LispFn) -> Self {
        LispObj::tag_ptr(func, Tag::Fn)
    }
}

#[derive(Copy, Clone)]
pub union LispObj {
    tag: Tag,
    bits: u64,
    fixnum: Fixnum,
}

enum Object<'a> {
    Int(i64),
    True,
    Nil,
    Cons(&'a Cons),
    String(&'a str),
    Float(f64),
    Void,
}

impl<'a> Object<'a> {
    fn create(l: &'a LispObj) -> Self {
        if let Some(x) = l.as_int() {
            Object::Int(x)
        } else if let Some(x) = l.as_cons() {
            Object::Cons(x)
        } else if let Some(x) = l.as_float() {
            Object::Float(x)
        } else if let Some(x) = l.as_str() {
            Object::String(x)
        } else if l.is_true() {
            Object::True
        } else if l.is_nil() {
            Object::Nil
        } else {
            panic!("Unknown Type")
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
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

impl ops::BitAnd<u16> for Tag {
    type Output = u16;

    fn bitand(self, rhs: u16) -> u16 {
        self as u16 & rhs
    }

}

const TAG_SIZE: usize = size_of::<Tag>() * 8;

const FIXNUM_MASK: u16 = 0b11;

const LISTP_MASK: u16 = 0b1011;
const LISTP_TAG: u16 = Tag::Nil as u16;

const STRP_MASK: u16 = 0b11011;
const STRP_TAG: u16 = Tag::LongStr as u16;

const NUM_MASK: u16 = 0b111011;
const NUM_TAG: u16 = Tag::Float as u16;

impl LispObj {

    fn get_ptr<T>(&self) -> *const T {
        unsafe {(self.bits >> TAG_SIZE) as *const T}
    }

    fn get_mut_ptr<T>(&mut self) -> *mut T {
        unsafe {(self.bits >> TAG_SIZE) as *mut T}
    }

    fn tag_ptr<T>(obj: T, tag: Tag) -> LispObj {
        let ptr = Box::into_raw(Box::new(obj));
        let bits = ((ptr as u64) << TAG_SIZE) | tag as u64;
        LispObj{bits}
    }

    pub const fn void() -> Self {
        LispObj{tag: Tag::Void}
    }

    pub const fn nil() -> Self {
        LispObj{tag: Tag::Nil}
    }

    pub const fn t() -> Self {
        LispObj{tag: Tag::True}
    }

    pub fn is_fixnum(&self) -> bool {
        unsafe {self.tag & FIXNUM_MASK == Tag::Fixnum as u16}
    }

    pub fn as_fixnum(self) -> Option<Fixnum> {
        if self.is_fixnum() {Some(unsafe{self.fixnum})} else {None}
    }

    pub fn as_int(self) -> Option<i64> {
        if self.is_fixnum() {Some(unsafe{self.fixnum.into()})} else {None}
    }

    pub fn is_nil(&self) -> bool {
        unsafe {self.tag == Tag::Nil}
    }

    pub fn is_true(&self) -> bool {
        unsafe {self.tag == Tag::True}
    }

    pub fn is_cons(&self) -> bool {
        unsafe {self.tag == Tag::Cons}
    }

    pub fn as_cons(&self) -> Option<&Cons> {
        if self.is_cons() {unsafe {
            Some(&*self.get_ptr())
        }} else {None}
    }

    pub fn as_mut_cons(&mut self) -> Option<&mut Cons> {
        if self.is_cons() {unsafe {Some(&mut *self.get_mut_ptr())}} else {None}
    }

    pub fn is_list(&self) -> bool {
        unsafe {self.tag & LISTP_MASK == LISTP_TAG}
    }

    pub fn is_str(&self) -> bool {
        unsafe {self.tag & STRP_MASK == STRP_TAG}
    }

    pub fn as_str(&self) -> Option<&str> {
        if self.is_str() {unsafe {Some(*self.get_ptr())}} else {None}
    }

    pub fn as_mut_str(&mut self) -> Option<&mut String> {
        if self.is_str() {unsafe {Some(&mut *self.get_mut_ptr())}} else {None}
    }

    pub fn is_float(&self) -> bool {
        unsafe {self.tag == Tag::Float}
    }

    pub fn as_float(&self) -> Option<f64> {
        if self.is_float() {unsafe {Some(*self.get_ptr())}} else {None}
    }
}

impl From<i64> for LispObj {
    fn from(i: i64) -> Self {
        LispObj {fixnum: i.into()}
    }
}

impl From<f64> for LispObj {
    fn from (f: f64) -> Self {
        LispObj::tag_ptr(f, Tag::Float)
    }
}

impl From<bool> for LispObj {
    fn from(b: bool) -> Self {
        LispObj{tag: if b {Tag::True} else {Tag::Nil}}
    }
}

impl From<String> for LispObj {
    fn from(s: String) -> Self {
        LispObj::tag_ptr(s, Tag::LongStr)
    }
}

impl fmt::Display for LispObj {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Object as O;
        match Object::create(self) {
            O::Int(x) => write!(f, "{}", x),
            O::Cons(x) => write!(f, "{}", x),
            O::String(x) => write!(f, "\"{}\"", x),
            O::Void => write!(f, "Void"),
            O::True => write!(f, "t"),
            O::Nil => write!(f, "nil"),
            O::Float(x) => {
                if x.fract() == 0.0 {
                    write!(f, "{:.1}", x)
                } else {
                    write!(f, "{}", x)
                }
            }
        }
    }
}

pub fn run() {}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn fixnum() {
        let x = LispObj::from(7);
        assert!(x.is_fixnum());
        format!("{}", x);
        assert_eq!(7, x.as_int().unwrap());
        assert_eq!(Fixnum::from(7), x.as_fixnum().unwrap());
    }

    #[test]
    fn float() {
        let x = LispObj::from(1.3);
        assert!(x.is_float());
        format!("{}", x);
        assert_eq!(1.3, x.as_float().unwrap());
    }

    #[test]
    fn string() {
        let mut x = LispObj::from("foo".to_owned());
        assert!(x.is_str());
        format!("{}", x);
        let str_ref = x.as_str().unwrap();
        assert_eq!("foo", str_ref);
        let mut_str = x.as_mut_str().unwrap();
        assert_eq!("foo", mut_str);
        *mut_str = "bar".to_owned();
        assert_eq!("bar", mut_str);
        assert_eq!("bar", x.as_str().unwrap());
    }

    #[test]
    fn cons() {
        let cons = Cons::new("start".to_owned(), Cons::new(7, Cons::new(5, 3.3)));

        let mut x = LispObj::from(cons);
        assert!(x.is_cons());
        format!("{}", x);

        let cons1 = x.as_mut_cons().unwrap();
        assert_eq!("start", cons1.car.as_str().unwrap());
        (*cons1).car = LispObj::from("start2".to_owned());
        assert_eq!("start2", cons1.car.as_str().unwrap());

        let cons2 = cons1.cdr.as_cons().unwrap();
        assert_eq!(7, cons2.car.as_int().unwrap());

        let cons3 = cons2.cdr.as_cons().unwrap();
        assert_eq!(5, cons3.car.as_int().unwrap());
        assert_eq!(3.3, cons3.cdr.as_float().unwrap());
    }
}
