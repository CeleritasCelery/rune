#![allow(dead_code)]

use crate::symbol::{Symbol, InnerSymbol};
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
    type Error = ();
    fn try_from(value: LispObj) -> Result<Self, Self::Error> {
        if matches!(value.val(), Value::Int(_)) {
            Ok(unsafe{value.fixnum})
        } else {
            Err(())
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

#[macro_export]
macro_rules! cons {
    ($car:expr, $cdr:expr) => (Cons::new($car.into(), $cdr.into()));
    ($car:expr) => (Cons::new($car.into(), false.into()));
}

#[macro_export]
macro_rules! list {
    ($x:expr) => (cons!($x));
    ($x:expr, $($y:expr),+ $(,)?) => (cons!($x, list!($($y),+)));
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct FnArgs {
    pub rest: bool,
    pub required: u16,
    pub optional: u16,
    pub max_stack_usage: u16,
    pub advice: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LispFn {
    pub op_codes: Vec<u8>,
    pub constants: Vec<LispObj>,
    pub args: FnArgs,
}

impl LispFn {
    pub fn new(op_codes: Vec<u8>,
               constants: Vec<LispObj>,
               required: u16,
               optional: u16,
               rest: bool) -> Self {
        LispFn {
            op_codes,
            constants,
            args: FnArgs {
                required,
                optional,
                rest,
                max_stack_usage: 0,
                advice: false,
            }
        }
    }
}

impl From<LispFn> for LispObj {
    fn from(func: LispFn) -> Self {
        LispObj::from_tagged_ptr(func, Tag::LispFn)
    }
}

type BuiltInFn = fn(&[LispObj]) -> LispObj;

#[derive(Clone)]
pub struct CoreFn {
    pub subr: BuiltInFn,
    pub args: FnArgs,
}

impl CoreFn {
    pub fn new(subr: BuiltInFn, required: u16, optional: u16, rest: bool) -> Self {
        Self {
            subr,
            args: FnArgs {
                required,
                optional,
                rest,
                max_stack_usage: 0,
                advice: false,
            }
        }
    }
}

impl std::fmt::Debug for CoreFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Core Fn ({:p} -> {:?})", &self.subr, self.args)
    }
}

impl std::cmp::PartialEq for CoreFn {
    fn eq(&self, other: &Self) -> bool {
        self.subr as fn(&'static _) -> _ == other.subr
    }
}

impl From<CoreFn> for LispObj {
    fn from(func: CoreFn) -> Self {
        LispObj::from_tagged_ptr(func, Tag::CoreFn)
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
    CoreFunc(&'a CoreFn),
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
    CoreFn,
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
                Tag::CoreFn   => Value::CoreFunc(&*self.get_ptr()),
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

impl From<Symbol> for LispObj {
    fn from(s: Symbol) -> Self {
        let ptr = s as *const InnerSymbol;
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
        match self.val() {
            Value::Int(x) => write!(f, "{}", x),
            Value::Cons(x) => write!(f, "{}", x),
            Value::String(x) => write!(f, "\"{}\"", x),
            Value::Symbol(x) => write!(f, "'{}", x.get_name()),
            Value::LispFunc(x) => write!(f, "(lambda {:?})", x),
            Value::CoreFunc(x) => write!(f, "Built-In {:?}", x),
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
    fn function() {
        let x: LispObj = LispFn::new(vec_into![0, 1, 2], vec_into![1], 0, 0, false).into();
        assert!(matches!(x.val(), Value::LispFunc(_)));
        format!("{}", x);
        let func = match x.val() {
            Value::LispFunc(x) => x,
            _ => unreachable!(),
        };
        assert_eq!(func.op_codes, [0, 1, 2]);
        assert_eq!(func.constants, vec_into![1]);
        assert_eq!(func.args.required, 0);
        assert_eq!(func.args.optional, 0);
        assert_eq!(func.args.rest, false);
    }

    #[test]
    fn lisp_type() {
        assert!(matches!(LispObj::from(1).val(), Value::Int(_)));
        assert!(matches!(LispObj::from(1.5).val(), Value::Float(_)));
        assert!(matches!(LispObj::from("foo").val(), Value::String(_)));
        assert!(matches!(LispObj::from(symbol::intern("foo")).val(), Value::Symbol(_)));
        assert!(matches!(LispObj::from(cons!(1, 2)).val(), Value::Cons(_)));
        assert!(matches!(LispObj::from(None::<LispObj>).val(), Value::Nil));
        assert!(matches!(LispObj::from(false).val(), Value::Nil));
        assert!(matches!(LispObj::nil().val(), Value::Nil));
        assert!(matches!(LispObj::from(true).val(), Value::True));
        assert!(matches!(LispObj::t().val(), Value::True));
        assert!(matches!(LispObj::void().val(), Value::Void));
    }
}
