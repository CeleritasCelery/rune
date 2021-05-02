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
pub type LispObj = Object<'static>;

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

pub trait TaggedObject {
    const TAG: Tag;
    fn new_tagged(bits: i64) -> i64 {
        bits << (size_of::<Tag>() * 8) | Self::TAG as i64
    }
    #[allow(clippy::wrong_self_convention)]
    fn is_boxed(self) -> bool;
    fn into_gc(self) -> LispObj;
}

#[derive(Copy, Clone)]
pub struct IntObject(i64);
impl TaggedObject for IntObject {
    const TAG: Tag = Tag::Int;
    fn is_boxed(self) -> bool {
        false
    }
    fn into_gc(self) -> LispObj {
        LispObj::from_bits(self.0)
    }
}
impl<'a> From<IntObject> for Object<'a> {
    fn from(x: IntObject) -> Self {
        Self::from_bits(x.0)
    }
}

#[derive(Copy, Clone)]
pub struct FloatObject(i64);
impl TaggedObject for FloatObject {
    const TAG: Tag = Tag::Float;
    fn is_boxed(self) -> bool {
        true
    }
    fn into_gc(self) -> LispObj {
        LispObj::from_bits(self.0)
    }
}
impl<'a> From<FloatObject> for Object<'a> {
    fn from(x: FloatObject) -> Self {
        Self::from_bits(x.0)
    }
}

#[derive(Copy, Clone)]
pub struct TrueObject(i64);
impl TaggedObject for TrueObject {
    const TAG: Tag = Tag::True;
    fn is_boxed(self) -> bool {
        false
    }
    fn into_gc(self) -> LispObj {
        LispObj::from_bits(self.0)
    }
}

#[derive(Copy, Clone)]
pub struct NilObject(i64);
impl TaggedObject for NilObject {
    const TAG: Tag = Tag::Nil;
    fn is_boxed(self) -> bool {
        false
    }
    fn into_gc(self) -> LispObj {
        LispObj::from_bits(self.0)
    }
}

#[derive(Copy, Clone)]
pub struct BoolObject(i64);
impl TaggedObject for BoolObject {
    const TAG: Tag = Tag::True;
    fn new_tagged(_bits: i64) -> i64 {
        panic!("should not be calling bool object this directly");
    }
    fn is_boxed(self) -> bool {
        false
    }
    fn into_gc(self) -> LispObj {
        LispObj::from_bits(self.0)
    }
}
impl<'a> From<BoolObject> for Object<'a> {
    fn from(x: BoolObject) -> Self {
        Self::from_bits(x.0)
    }
}
impl From<TrueObject> for BoolObject {
    fn from(x: TrueObject) -> Self {
        Self(x.0)
    }
}
impl From<NilObject> for BoolObject {
    fn from(x: NilObject) -> Self {
        Self(x.0)
    }
}

#[derive(Copy, Clone)]
pub struct OptionObject(i64);
impl TaggedObject for OptionObject {
    const TAG: Tag = Tag::Nil;
    // TODO: add correct implementation
    fn is_boxed(self) -> bool {
        false
    }
    fn into_gc(self) -> LispObj {
        LispObj::from_bits(self.0)
    }
}
impl<'a> From<OptionObject> for Object<'a> {
    fn from(x: OptionObject) -> Self {
        Self::from_bits(x.0)
    }
}

#[derive(Copy, Clone)]
pub struct ConsObject(i64);
impl TaggedObject for ConsObject {
    const TAG: Tag = Tag::Cons;
    fn is_boxed(self) -> bool {
        true
    }
    fn into_gc(self) -> LispObj {
        LispObj::from_bits(self.0)
    }
}
impl<'a> From<ConsObject> for Object<'a> {
    fn from(x: ConsObject) -> Self {
        Self::from_bits(x.0)
    }
}

#[derive(Copy, Clone)]
pub struct SymbolObject(i64);
impl TaggedObject for SymbolObject {
    const TAG: Tag = Tag::Symbol;
    fn is_boxed(self) -> bool {
        false
    }
    fn into_gc(self) -> LispObj {
        LispObj::from_bits(self.0)
    }
}
impl<'a> From<SymbolObject> for Object<'a> {
    fn from(x: SymbolObject) -> Self {
        Self::from_bits(x.0)
    }
}

#[derive(Copy, Clone)]
pub struct StringObject(i64);
impl TaggedObject for StringObject {
    const TAG: Tag = Tag::String;
    fn is_boxed(self) -> bool {
        true
    }
    fn into_gc(self) -> LispObj {
        LispObj::from_bits(self.0)
    }
}
impl<'a> From<StringObject> for Object<'a> {
    fn from(x: StringObject) -> Self {
        Self::from_bits(x.0)
    }
}

#[derive(Copy, Clone)]
pub struct LispFnObject(i64);
impl TaggedObject for LispFnObject {
    const TAG: Tag = Tag::LispFn;
    fn is_boxed(self) -> bool {
        true
    }
    fn into_gc(self) -> LispObj {
        LispObj::from_bits(self.0)
    }
}
impl<'a> From<LispFnObject> for Object<'a> {
    fn from(x: LispFnObject) -> Self {
        Self::from_bits(x.0)
    }
}

#[derive(Copy, Clone)]
pub struct SubrFnObject(i64);
impl TaggedObject for SubrFnObject {
    const TAG: Tag = Tag::SubrFn;
    #[inline]
    fn is_boxed(self) -> bool {
        true
    }
    fn into_gc(self) -> LispObj {
        LispObj::from_bits(self.0)
    }
}
impl<'a> From<SubrFnObject> for Object<'a> {
    fn from(x: SubrFnObject) -> Self {
        Self::from_bits(x.0)
    }
}

impl<'obj> TaggedObject for Object<'obj> {
    const TAG: Tag = Tag::Nil;
    fn is_boxed(self) -> bool {
        false
    }
    fn into_gc(self) -> LispObj {
        unsafe { self.into_gc() }
    }
}

pub trait RefObject<'a> {}

impl<'a> RefObject<'a> for Object<'a> {}

const TAG_SIZE: usize = size_of::<Tag>() * 8;

pub trait IntoObject<T> {
    fn into_object(self, arena: &Arena) -> T;
}

impl<'old, 'new> Object<'old> {
    pub fn clone_in(self, arena: &'new Arena) -> Object<'new> {
        match self.val() {
            Value::Int(x) => arena.insert(x),
            Value::Cons(x) => arena.insert(x.clone_in(arena)),
            Value::String(x) => arena.insert(x.clone()),
            Value::Symbol(x) => arena.insert(x),
            Value::LispFn(x) => arena.insert(x.clone()),
            Value::SubrFn(x) => arena.insert(*x),
            Value::True => Object::t(),
            Value::Nil => Object::nil(),
            Value::Float(x) => arena.insert(x),
        }
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

    pub fn val<'b: 'a>(self) -> Value<'b> {
        self.data.val()
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
    fn get_ptr<T>(self) -> *const T {
        (unsafe { self.bits } >> TAG_SIZE) as *const T
    }

    fn get_mut_ptr<T>(&mut self) -> *mut T {
        (unsafe { self.bits } >> TAG_SIZE) as *mut T
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
        let arena = Arena::new();
        let int: Object = arena.insert(3);
        assert!(matches!(int.val(), Value::Int(_)));
        assert_eq!(int.val(), Value::Int(3));
    }

    #[test]
    fn float() {
        let arena = Arena::new();
        let x: Object = arena.insert(1.3);
        assert!(matches!(x.val(), Value::Float(_)));
        assert_eq!(x.val(), Value::Float(1.3));
    }

    #[test]
    fn string() {
        let arena = Arena::new();
        let x: Object = arena.insert("foo");
        assert!(matches!(x.val(), Value::String(_)));
        assert_eq!(x.val(), Value::String(&"foo".to_owned()));

        let x: Object = arena.insert("bar".to_owned());
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
        let bool_true: Object = arena.insert(true);
        assert_eq!(bool_true.val(), Value::True);
        let bool_false: Object = arena.insert(false);
        assert_eq!(bool_false.val(), Value::Nil);
    }

    #[test]
    fn symbol() {
        let arena = Arena::new();
        let x: Object = arena.insert(intern("foo"));
        assert!(matches!(x.val(), Value::Symbol(_)));
        assert_eq!(x.val(), Value::Symbol(intern("foo")));
    }
}
