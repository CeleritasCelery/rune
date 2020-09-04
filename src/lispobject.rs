#![allow(dead_code)]

use std::mem::size_of;
use std::mem::transmute;
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
    car: LispObj,
    cdr: LispObj,
}

impl From<Cons> for LispObj {
    fn from(cons: Cons) -> Self {
        LispObj::tag_ptr(&cons, Tag::Cons)
    }
}

pub struct LispFn {
    op_codes: Vec<u8>,
    constants: Vec<LispObj>,
    rest_args: bool,
    required_args: u16,
    optional_args: u16,
    max_stack_usage: u16,
}

impl From<LispFn> for LispObj {
    fn from(func: LispFn) -> Self {
        LispObj::tag_ptr(&func, Tag::Fn)
    }
}

#[derive(Copy, Clone)]
pub union LispObj {
    tag: Tag,
    bits: u64,
    fixnum: Fixnum,
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

trait TryAs<T>: Sized {
    fn try_as(self) -> Option<T>;
}

trait TryAsRef<'a, T>: Sized {
    fn try_as_ref(self) -> Option<&'a T>;
}

trait TryAsMutRef<'a, T>: Sized {
    fn try_as_mut_ref(self) -> Option<&'a mut T>;
}

impl LispObj {

    fn get_ptr<T>(&self) -> *const T {
        unsafe {
            let mut x = self.bits;
            x >>= TAG_SIZE;
            transmute::<u64, *const T>(x)
        }
    }

    fn get_mut_ptr<T>(&self) -> *mut T {
        unsafe {
            let mut x = self.bits;
            x >>= TAG_SIZE;
            transmute::<u64, *mut T>(x)
        }
    }

    fn tag_ptr<T>(ptr: *const T, tag: Tag) -> LispObj {
        unsafe {
            let mut bits = transmute::<*const T, u64>(ptr);
            bits <<= TAG_SIZE;
            bits |= tag as u64;
            LispObj{bits}
        }
    }

    pub fn is_fixnum(&self) -> bool {
        unsafe {self.tag & FIXNUM_MASK == Tag::Fixnum as u16}
    }

    pub fn as_fixnum(self) -> Option<Fixnum> {
        if self.is_fixnum() {Some(unsafe{self.fixnum})} else {None}
    }

    pub fn from_fixnum(fixnum: Fixnum) -> LispObj {
        LispObj{fixnum}
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
        if self.is_cons() {unsafe {Some(*self.get_ptr())}} else {None}
    }

    pub fn as_mut_cons(&self) -> Option<&mut Cons> {
        if self.is_cons() {unsafe {Some(&mut *self.get_mut_ptr())}} else {None}
    }

    pub fn is_list(&self) -> bool {
        unsafe {self.tag & LISTP_MASK == LISTP_TAG}
    }
}

impl From<i64> for LispObj {
    fn from(i: i64) -> Self {
        LispObj {fixnum: i.into()}
    }
}

impl From<bool> for LispObj {
    fn from(b: bool) -> Self {
        LispObj{tag: if b {Tag::True} else {Tag::Nil}}
    }
}

pub fn run() {
    let z = LispObj::from(-1);


    if z.is_fixnum() {
        println!("is int");
    }

    println!("{}", z.as_int().unwrap())
}
