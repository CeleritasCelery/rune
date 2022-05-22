use sptr::Strict;
use std::fmt;
use std::{cell::RefCell, marker::PhantomData};

use super::super::arena::Block;
use super::super::error::{Error, Type};

use super::super::{
    arena::{AllocObject, Allocation},
    cons::Cons,
    symbol::{GlobalSymbol, Symbol},
};

use super::{LispFn, SubrFn};

pub(crate) type ObjVec<'ob> = Vec<GcObj<'ob>>;
pub(crate) type GcObj<'ob> = Gc<Object<'ob>>;

#[derive(Copy, Clone, Debug)]
pub(crate) struct RawObj {
    ptr: *const u8,
}

unsafe impl Send for RawObj {}

impl Default for RawObj {
    fn default() -> Self {
        Self { ptr: Gc::NIL.ptr }
    }
}

#[derive(Copy, Clone)]
pub(crate) struct Gc<T> {
    ptr: *const u8,
    _data: PhantomData<T>,
}

unsafe impl<T> Send for Gc<T> {}

#[repr(u8)]
enum Tag {
    Symbol,
    Int,
    Float,
    Cons,
    Nil,
    True,
    String,
    Vec,
    SubrFn,
    LispFn,
}

impl<T> Gc<T> {
    const fn new(ptr: *const u8) -> Self {
        Self {
            ptr,
            _data: PhantomData,
        }
    }

    fn from_ptr<U>(ptr: *const U, tag: Tag) -> Self {
        assert_eq!(
            std::mem::size_of::<*const U>(),
            std::mem::size_of::<*const ()>()
        );
        let ptr = Strict::map_addr(ptr.cast::<u8>(), |x| (x << 8) | tag as usize);
        Self::new(ptr)
    }

    const fn from_tag(tag: Tag) -> Self {
        let ptr = sptr::invalid(tag as usize);
        Self::new(ptr)
    }

    fn untag(self) -> (*const u8, Tag) {
        let ptr = Strict::map_addr(self.ptr, |x| x >> 8);
        let tag = self.tag();
        (ptr, tag)
    }

    fn tag(self) -> Tag {
        unsafe { std::mem::transmute(Strict::addr(self.ptr) as u8) }
    }

    pub(crate) fn into_raw(self) -> RawObj {
        RawObj { ptr: self.ptr }
    }

    pub(crate) unsafe fn from_raw(raw: RawObj) -> Self {
        Self::new(raw.ptr)
    }

    unsafe fn transmute<U, V>(e: Gc<U>) -> Gc<V> {
        Gc::new(e.ptr)
    }

    pub(crate) fn ptr_eq<U>(self, other: Gc<U>) -> bool {
        self.ptr == other.ptr
    }
}

unsafe fn transmute<U, V>(e: Gc<U>) -> Gc<V> {
    Gc::new(e.ptr)
}

impl<T> Gc<T> {
    #[allow(dead_code)]
    pub(crate) fn copy_as_obj<'ob>(self) -> Gc<Object<'ob>> {
        Gc::new(self.ptr)
    }

    pub(crate) fn as_obj(&self) -> Gc<Object<'_>> {
        Gc::new(self.ptr)
    }
}

impl<'a, T: 'a> From<Gc<T>> for Object<'a> {
    fn from(x: Gc<T>) -> Self {
        Gc::<Object>::new(x.ptr).get()
    }
}

////////////////////////
// Traits for Objects //
////////////////////////

pub(crate) trait WithLifetime<'new> {
    type Out: 'new;
    unsafe fn with_lifetime(self) -> Self::Out;
}

pub(crate) trait IntoObject<'ob> {
    type Out;
    fn into_obj<const C: bool>(self, block: &'ob Block<C>) -> Gc<Self::Out>;
    unsafe fn from_obj_ptr(ptr: *const u8) -> Self::Out;
}

impl<'ob> IntoObject<'ob> for Gc<Object<'ob>> {
    type Out = Object<'ob>;

    fn into_obj<const C: bool>(self, _block: &'ob Block<C>) -> Gc<Self::Out> {
        unsafe { Self::transmute(self) }
    }

    unsafe fn from_obj_ptr(_ptr: *const u8) -> Self::Out {
        unimplemented!()
    }
}

impl<'ob> IntoObject<'ob> for Option<Gc<Object<'ob>>> {
    type Out = Object<'ob>;

    fn into_obj<const C: bool>(self, _block: &'ob Block<C>) -> Gc<Self::Out> {
        match self {
            Some(x) => unsafe { x.with_lifetime() },
            None => GcObj::NIL,
        }
    }

    unsafe fn from_obj_ptr(_ptr: *const u8) -> Self::Out {
        unimplemented!()
    }
}

impl<'ob> IntoObject<'ob> for f64 {
    type Out = &'ob f64;

    fn into_obj<const C: bool>(self, block: &'ob Block<C>) -> Gc<Self::Out> {
        let ptr = self.alloc_obj(block);
        Gc::from_ptr(ptr, Tag::Float)
    }

    unsafe fn from_obj_ptr(ptr: *const u8) -> Self::Out {
        &*Self::cast_ptr(ptr)
    }
}

impl<'ob> IntoObject<'ob> for i64 {
    type Out = i64;

    fn into_obj<const C: bool>(self, _: &'ob Block<C>) -> Gc<Self::Out> {
        let ptr: *const i64 = sptr::invalid(self as usize);
        unsafe { Self::tag_ptr(ptr) }
    }

    unsafe fn from_obj_ptr(ptr: *const u8) -> Self::Out {
        Strict::addr(ptr) as i64
    }
}

impl<'ob> IntoObject<'ob> for bool {
    type Out = bool;

    fn into_obj<const C: bool>(self, _: &'ob Block<C>) -> Gc<Self::Out> {
        match self {
            true => Gc::from_tag(Tag::True),
            false => Gc::from_tag(Tag::Nil),
        }
    }

    unsafe fn from_obj_ptr(_ptr: *const u8) -> Self::Out {
        unimplemented!()
    }
}

impl<'ob> IntoObject<'ob> for Cons {
    type Out = &'ob Cons;

    fn into_obj<const C: bool>(self, block: &'ob Block<C>) -> Gc<Self::Out> {
        let ptr = self.alloc_obj(block);
        Gc::from_ptr(ptr, Tag::Cons)
    }

    unsafe fn from_obj_ptr(ptr: *const u8) -> Self::Out {
        &*Self::cast_ptr(ptr)
    }
}

impl<'ob> IntoObject<'ob> for SubrFn {
    type Out = &'static SubrFn;

    fn into_obj<const C: bool>(self, block: &'ob Block<C>) -> Gc<Self::Out> {
        let ptr = self.alloc_obj(block);
        Gc::from_ptr(ptr, Tag::SubrFn)
    }

    unsafe fn from_obj_ptr(ptr: *const u8) -> Self::Out {
        &*Self::cast_ptr(ptr)
    }
}

impl<'ob> IntoObject<'ob> for LispFn<'ob> {
    type Out = &'ob LispFn<'ob>;

    fn into_obj<const C: bool>(self, block: &'ob Block<C>) -> Gc<Self::Out> {
        let ptr = self.alloc_obj(block);
        unsafe { Self::tag_ptr(ptr) }
    }

    unsafe fn from_obj_ptr(ptr: *const u8) -> Self::Out {
        &*Self::cast_ptr(ptr)
    }
}

impl<'ob> IntoObject<'ob> for Symbol {
    type Out = Symbol;

    fn into_obj<const C: bool>(self, _: &'ob Block<C>) -> Gc<Self::Out> {
        let ptr: *const u8 = (self as *const GlobalSymbol).cast();
        Gc::from_ptr(ptr, Tag::Symbol)
    }

    unsafe fn from_obj_ptr(ptr: *const u8) -> Self::Out {
        &*Self::cast_ptr(ptr)
    }
}

impl<'ob> IntoObject<'ob> for String {
    type Out = &'ob String;

    fn into_obj<const C: bool>(self, block: &'ob Block<C>) -> Gc<Self::Out> {
        let ptr = self.alloc_obj(block);
        unsafe { Self::tag_ptr(ptr) }
    }

    unsafe fn from_obj_ptr(ptr: *const u8) -> Self::Out {
        &*Self::cast_ptr(ptr)
    }
}

impl<'ob> IntoObject<'ob> for &str {
    type Out = &'ob String;

    fn into_obj<const C: bool>(self, block: &'ob Block<C>) -> Gc<Self::Out> {
        let ptr = self.to_owned().alloc_obj(block);
        unsafe { String::tag_ptr(ptr) }
    }

    unsafe fn from_obj_ptr(ptr: *const u8) -> Self::Out {
        &*String::cast_ptr(ptr)
    }
}

impl<'ob> IntoObject<'ob> for ObjVec<'ob> {
    type Out = &'ob RefCell<ObjVec<'ob>>;

    fn into_obj<const C: bool>(self, block: &'ob Block<C>) -> Gc<Self::Out> {
        let ptr = self.alloc_obj(block);
        unsafe { Self::tag_ptr(ptr) }
    }

    unsafe fn from_obj_ptr(ptr: *const u8) -> Self::Out {
        &*Self::cast_ptr(ptr)
    }
}

// work around for no GAT's
#[allow(unused_lifetimes)]
trait TaggedPtr<'ob>
where
    Self: Sized,
{
    type Ptr;
    type Output;
    const TAG: Tag;
    unsafe fn tag_ptr(ptr: *const Self::Ptr) -> Gc<Self::Output> {
        Gc::from_ptr(ptr, Self::TAG)
    }
    fn cast_ptr(ptr: *const u8) -> *const Self::Ptr {
        ptr.cast()
    }
}

impl TaggedPtr<'_> for i64 {
    type Ptr = i64;
    type Output = i64;
    const TAG: Tag = Tag::Int;
}

impl<'ob> TaggedPtr<'ob> for f64 {
    type Ptr = <Self as AllocObject>::Output;
    type Output = &'ob f64;
    const TAG: Tag = Tag::Float;
}

impl<'ob> TaggedPtr<'ob> for Cons {
    type Ptr = <Self as AllocObject>::Output;
    type Output = &'ob Cons;
    const TAG: Tag = Tag::Cons;
}

impl<'ob> TaggedPtr<'ob> for SubrFn {
    type Ptr = <Self as AllocObject>::Output;
    type Output = &'ob SubrFn;
    const TAG: Tag = Tag::SubrFn;
}

impl TaggedPtr<'_> for Symbol {
    type Ptr = GlobalSymbol;
    type Output = Symbol;
    const TAG: Tag = Tag::Symbol;
}

impl<'ob> TaggedPtr<'ob> for LispFn<'ob> {
    type Ptr = <Self as AllocObject>::Output;
    type Output = &'ob LispFn<'ob>;
    const TAG: Tag = Tag::LispFn;
}

impl<'ob> TaggedPtr<'ob> for String {
    type Ptr = <Self as AllocObject>::Output;
    type Output = &'ob String;
    const TAG: Tag = Tag::String;
}

impl<'ob> TaggedPtr<'ob> for ObjVec<'ob> {
    type Ptr = <Self as AllocObject>::Output;
    type Output = &'ob RefCell<ObjVec<'ob>>;
    const TAG: Tag = Tag::Vec;
}

////////////////////////
// Proc macro section //
////////////////////////

// Number
#[derive(Copy, Clone)]
pub(crate) enum Number<'ob> {
    Int(i64),
    Float(&'ob f64),
}

impl<'old, 'new> WithLifetime<'new> for Gc<Number<'old>> {
    type Out = Gc<Number<'new>>;

    unsafe fn with_lifetime(self) -> Self::Out {
        transmute(self)
    }
}

impl<'ob> From<Gc<i64>> for Gc<Number<'ob>> {
    fn from(x: Gc<i64>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Gc<&'ob f64>> for Gc<Number<'ob>> {
    fn from(x: Gc<&'ob f64>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> Gc<Number<'ob>> {
    pub(crate) fn get(self) -> Number<'ob> {
        let (ptr, tag) = self.untag();
        match tag {
            Tag::Int => Number::Int(unsafe { i64::from_obj_ptr(ptr) }),
            Tag::Float => Number::Float(unsafe { f64::from_obj_ptr(ptr) }),
            _ => unreachable!(),
        }
    }
}

impl<'ob> From<i64> for Gc<Number<'ob>> {
    fn from(x: i64) -> Self {
        let ptr = sptr::invalid(x as usize);
        unsafe { i64::tag_ptr(ptr).into() }
    }
}

// List
#[derive(Copy, Clone, Debug)]
pub(crate) enum List<'ob> {
    Nil,
    Cons(&'ob Cons),
}

impl<'old, 'new> WithLifetime<'new> for Gc<List<'old>> {
    type Out = Gc<List<'new>>;

    unsafe fn with_lifetime(self) -> Self::Out {
        transmute(self)
    }
}

impl<'ob> From<Gc<&'ob Cons>> for Gc<List<'ob>> {
    fn from(x: Gc<&'ob Cons>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Gc<()>> for Gc<List<'ob>> {
    fn from(x: Gc<()>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> Gc<List<'ob>> {
    pub(crate) fn get(self) -> List<'ob> {
        let (ptr, tag) = self.untag();
        match tag {
            Tag::Nil => List::Nil,
            Tag::Cons => List::Cons(unsafe { Cons::from_obj_ptr(ptr) }),
            _ => unreachable!(),
        }
    }
}

// This is safe because cons is a gc heap only type
impl<'ob> From<&'ob Cons> for Gc<List<'ob>> {
    fn from(x: &'ob Cons) -> Self {
        unsafe { Cons::tag_ptr(x as *const _).into() }
    }
}

// Callable
#[derive(Copy, Clone, Debug)]
pub(crate) enum Callable<'ob> {
    LispFn(&'ob LispFn<'ob>),
    SubrFn(&'static SubrFn),
    Cons(&'ob Cons),
}

impl<'old, 'new> WithLifetime<'new> for Gc<Callable<'old>> {
    type Out = Gc<Callable<'new>>;

    unsafe fn with_lifetime(self) -> Self::Out {
        transmute(self)
    }
}

impl<'ob> From<Gc<&'ob Cons>> for Gc<Callable<'ob>> {
    fn from(x: Gc<&'ob Cons>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Gc<&'static SubrFn>> for Gc<Callable<'ob>> {
    fn from(x: Gc<&'static SubrFn>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Gc<&'ob LispFn<'ob>>> for Gc<Callable<'ob>> {
    fn from(x: Gc<&'ob LispFn<'ob>>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> Gc<Callable<'ob>> {
    pub(crate) fn get(self) -> Callable<'ob> {
        let (ptr, tag) = self.untag();
        match tag {
            Tag::Cons => Callable::Cons(unsafe { Cons::from_obj_ptr(ptr) }),
            Tag::SubrFn => Callable::SubrFn(unsafe { SubrFn::from_obj_ptr(ptr) }),
            Tag::LispFn => Callable::LispFn(unsafe { LispFn::from_obj_ptr(ptr) }),
            _ => unreachable!(),
        }
    }
}

// Function
#[derive(Copy, Clone, Debug)]
pub(crate) enum Function<'ob> {
    LispFn(&'ob LispFn<'ob>),
    SubrFn(&'static SubrFn),
    Cons(&'ob Cons),
    Symbol(Symbol),
}

impl<'old, 'new> WithLifetime<'new> for Gc<Function<'old>> {
    type Out = Gc<Function<'new>>;

    unsafe fn with_lifetime(self) -> Self::Out {
        transmute(self)
    }
}

#[cfg(miri)]
extern "Rust" {
    fn miri_static_root(ptr: *const u8);
}

#[cfg(miri)]
impl<'ob> Function<'ob> {
    pub(crate) fn set_as_miri_root(self) {
        match self {
            Function::LispFn(x) => {
                let ptr: *const _ = x;
                unsafe {
                    miri_static_root(ptr as _);
                }
            }
            Function::SubrFn(x) => {
                let ptr: *const _ = x;
                unsafe {
                    miri_static_root(ptr as _);
                }
            }
            Function::Cons(x) => {
                let ptr: *const _ = x;
                unsafe {
                    miri_static_root(ptr as _);
                }
            }
            Function::Symbol(_) => {}
        }
    }
}

impl<'ob> From<Gc<&'ob Cons>> for Gc<Function<'ob>> {
    fn from(x: Gc<&'ob Cons>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Gc<&'static SubrFn>> for Gc<Function<'ob>> {
    fn from(x: Gc<&'static SubrFn>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Gc<&'ob LispFn<'ob>>> for Gc<Function<'ob>> {
    fn from(x: Gc<&'ob LispFn<'ob>>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Gc<Symbol>> for Gc<Function<'ob>> {
    fn from(x: Gc<Symbol>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Symbol> for Gc<Function<'ob>> {
    fn from(x: Symbol) -> Self {
        let ptr = x as *const GlobalSymbol;
        unsafe { Symbol::tag_ptr(ptr).into() }
    }
}

impl From<&'static SubrFn> for Gc<Function<'_>> {
    fn from(x: &'static SubrFn) -> Self {
        let ptr = x as *const SubrFn;
        unsafe { SubrFn::tag_ptr(ptr).into() }
    }
}

impl<'ob> Gc<Function<'ob>> {
    pub(crate) fn get(self) -> Function<'ob> {
        let (ptr, tag) = self.untag();
        match tag {
            Tag::Cons => Function::Cons(unsafe { Cons::from_obj_ptr(ptr) }),
            Tag::SubrFn => Function::SubrFn(unsafe { SubrFn::from_obj_ptr(ptr) }),
            Tag::LispFn => Function::LispFn(unsafe { LispFn::from_obj_ptr(ptr) }),
            Tag::Symbol => Function::Symbol(unsafe { Symbol::from_obj_ptr(ptr) }),
            _ => unreachable!(),
        }
    }
}

impl<'ob> From<Gc<Callable<'ob>>> for Gc<Function<'ob>> {
    fn from(obj: Gc<Callable<'ob>>) -> Self {
        unsafe { Self::transmute(obj) }
    }
}

impl<'ob> TryFrom<Gc<Function<'ob>>> for Gc<Callable<'ob>> {
    type Error = anyhow::Error;

    fn try_from(value: Gc<Function<'ob>>) -> Result<Self, Self::Error> {
        match value.tag() {
            Tag::Symbol => Err(Error::from_object(Type::Func, value).into()),
            _ => unsafe { Ok(Self::transmute(value)) },
        }
    }
}

#[derive(Copy, Clone)]
pub(crate) enum Object<'ob> {
    Int(i64),
    Float(&'ob f64),
    Symbol(Symbol),
    True,
    Nil,
    Cons(&'ob Cons),
    Vec(&'ob RefCell<ObjVec<'ob>>),
    String(&'ob String),
    LispFn(&'ob LispFn<'ob>),
    SubrFn(&'static SubrFn),
}

impl Object<'_> {
    /// Return the type of an object
    pub(crate) fn get_type(self) -> Type {
        match self {
            Object::Int(_) => Type::Int,
            Object::Float(_) => Type::Float,
            Object::Symbol(_) => Type::Symbol,
            Object::True => Type::True,
            Object::Nil => Type::Nil,
            Object::Cons(_) => Type::Cons,
            Object::Vec(_) => Type::Vec,
            Object::String(_) => Type::String,
            Object::LispFn(_) | Object::SubrFn(_) => Type::Func,
        }
    }
}

impl PartialEq for Object<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(l0), Self::Int(r0)) => l0 == r0,
            (Self::Float(l0), Self::Float(r0)) => l0 == r0,
            (Self::Symbol(l0), Self::Symbol(r0)) => l0 == r0,
            (Self::Cons(l0), Self::Cons(r0)) => l0 == r0,
            (Self::Vec(l0), Self::Vec(r0)) => *l0.borrow() == *r0.borrow(),
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::LispFn(l0), Self::LispFn(r0)) => l0 == r0,
            (Self::SubrFn(l0), Self::SubrFn(r0)) => l0 == r0,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

// auto-generated

impl<'old, 'new> WithLifetime<'new> for Gc<Object<'old>> {
    type Out = Gc<Object<'new>>;

    unsafe fn with_lifetime(self) -> Self::Out {
        transmute(self)
    }
}

impl<'ob> Gc<Object<'ob>> {
    pub(crate) fn get(self) -> Object<'ob> {
        let (ptr, tag) = self.untag();
        match tag {
            Tag::Symbol => Object::Symbol(unsafe { Symbol::from_obj_ptr(ptr) }),
            Tag::Cons => Object::Cons(unsafe { Cons::from_obj_ptr(ptr) }),
            Tag::SubrFn => Object::SubrFn(unsafe { SubrFn::from_obj_ptr(ptr) }),
            Tag::LispFn => Object::LispFn(unsafe { LispFn::from_obj_ptr(ptr) }),
            Tag::Int => Object::Int(unsafe { i64::from_obj_ptr(ptr) }),
            Tag::Float => Object::Float(unsafe { f64::from_obj_ptr(ptr) }),
            Tag::Nil => Object::Nil,
            Tag::True => Object::True,
            Tag::String => Object::String(unsafe { String::from_obj_ptr(ptr) }),
            Tag::Vec => Object::Vec(unsafe { ObjVec::from_obj_ptr(ptr) }),
        }
    }
}

impl<'ob> From<Gc<i64>> for Gc<Object<'ob>> {
    fn from(x: Gc<i64>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<i64> for Gc<Object<'ob>> {
    fn from(x: i64) -> Self {
        let ptr = sptr::invalid(x as usize);
        unsafe { i64::tag_ptr(ptr).into() }
    }
}

impl<'ob> From<Gc<&'ob f64>> for Gc<Object<'ob>> {
    fn from(x: Gc<&'ob f64>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Symbol> for Gc<Object<'ob>> {
    fn from(x: Symbol) -> Self {
        let ptr = x as *const GlobalSymbol;
        unsafe { Symbol::tag_ptr(ptr).into() }
    }
}

impl<'ob> From<&Cons> for Gc<Object<'ob>> {
    fn from(x: &Cons) -> Self {
        let ptr = x as *const Cons;
        unsafe { Cons::tag_ptr(ptr).into() }
    }
}

impl<'ob> From<Gc<Symbol>> for Gc<Object<'ob>> {
    fn from(x: Gc<Symbol>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Gc<()>> for Gc<Object<'ob>> {
    fn from(x: Gc<()>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Gc<bool>> for Gc<Object<'ob>> {
    fn from(x: Gc<bool>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Gc<&'ob Cons>> for Gc<Object<'ob>> {
    fn from(x: Gc<&'ob Cons>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Gc<&'ob String>> for Gc<Object<'ob>> {
    fn from(x: Gc<&'ob String>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Gc<&'ob RefCell<Vec<GcObj<'ob>>>>> for Gc<Object<'ob>> {
    fn from(x: Gc<&'ob RefCell<Vec<GcObj<'ob>>>>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Gc<&'ob LispFn<'ob>>> for Gc<Object<'ob>> {
    fn from(x: Gc<&'ob LispFn<'ob>>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<&'ob SubrFn> for Gc<Object<'ob>> {
    fn from(x: &'ob SubrFn) -> Self {
        unsafe { SubrFn::tag_ptr(x as *const _).into() }
    }
}

impl<'ob> From<Gc<&'ob SubrFn>> for Gc<Object<'ob>> {
    fn from(x: Gc<&'ob SubrFn>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Gc<Number<'ob>>> for Gc<Object<'ob>> {
    fn from(x: Gc<Number<'ob>>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> TryFrom<Gc<Object<'ob>>> for Gc<Number<'ob>> {
    type Error = Error;

    fn try_from(value: Gc<Object<'ob>>) -> Result<Self, Self::Error> {
        match value.tag() {
            Tag::Int | Tag::Float => unsafe { Ok(Self::transmute(value)) },
            _ => Err(Error::from_object(Type::Number, value)),
        }
    }
}

impl<'ob> TryFrom<Gc<Object<'ob>>> for Option<Gc<Number<'ob>>> {
    type Error = Error;

    fn try_from(value: Gc<Object<'ob>>) -> Result<Self, Self::Error> {
        match value.tag() {
            Tag::Int | Tag::Float => unsafe { Ok(Some(transmute(value))) },
            Tag::Nil => Ok(None),
            _ => Err(Error::from_object(Type::Number, value)),
        }
    }
}

impl<'ob> From<Gc<List<'ob>>> for Gc<Object<'ob>> {
    fn from(x: Gc<List<'ob>>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> TryFrom<Gc<Object<'ob>>> for Gc<List<'ob>> {
    type Error = Error;

    fn try_from(value: Gc<Object<'ob>>) -> Result<Self, Self::Error> {
        match value.tag() {
            Tag::Nil | Tag::Cons => unsafe { Ok(Self::transmute(value)) },
            _ => Err(Error::from_object(Type::List, value)),
        }
    }
}

impl<'ob> From<Gc<Function<'ob>>> for Gc<Object<'ob>> {
    fn from(x: Gc<Function<'ob>>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> TryFrom<Gc<Object<'ob>>> for Gc<Function<'ob>> {
    type Error = Error;

    fn try_from(value: Gc<Object<'ob>>) -> Result<Self, Self::Error> {
        match value.tag() {
            Tag::LispFn | Tag::SubrFn | Tag::Cons | Tag::Symbol => unsafe {
                Ok(Self::transmute(value))
            },
            _ => Err(Error::from_object(Type::Func, value)),
        }
    }
}

impl<'ob> From<Gc<Callable<'ob>>> for Gc<Object<'ob>> {
    fn from(x: Gc<Callable<'ob>>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> TryFrom<Gc<Object<'ob>>> for Gc<Callable<'ob>> {
    type Error = Error;

    fn try_from(value: Gc<Object<'ob>>) -> Result<Self, Self::Error> {
        match value.tag() {
            Tag::LispFn | Tag::SubrFn | Tag::Cons => unsafe { Ok(Self::transmute(value)) },
            _ => Err(Error::from_object(Type::Func, value)),
        }
    }
}

///////////////////////////
// Other implementations //
///////////////////////////

impl<'ob> TryFrom<Gc<Object<'ob>>> for Gc<i64> {
    type Error = Error;

    fn try_from(value: Gc<Object<'ob>>) -> Result<Self, Self::Error> {
        match value.tag() {
            Tag::Int => unsafe { Ok(Self::transmute(value)) },
            _ => Err(Error::from_object(Type::Int, value)),
        }
    }
}

impl Gc<i64> {
    pub(crate) fn get(self) -> i64 {
        let (ptr, _) = self.untag();
        unsafe { i64::from_obj_ptr(ptr) }
    }
}

impl<'ob> Gc<&'ob Cons> {
    pub(crate) fn get(self) -> &'ob Cons {
        let (ptr, _) = self.untag();
        unsafe { Cons::from_obj_ptr(ptr) }
    }
}

impl From<&Cons> for Gc<&Cons> {
    fn from(x: &Cons) -> Self {
        let ptr = x as *const Cons;
        unsafe { Cons::tag_ptr(ptr) }
    }
}

impl std::ops::Deref for Gc<&Cons> {
    type Target = Cons;

    fn deref(&self) -> &Self::Target {
        self.get()
    }
}

impl<'old, 'new> WithLifetime<'new> for Gc<&'old Cons> {
    type Out = Gc<&'new Cons>;

    unsafe fn with_lifetime(self) -> Self::Out {
        transmute(self)
    }
}

fn vec_clone_in<'old, 'new, const C: bool>(
    vec: &[GcObj<'old>],
    bk: &'new Block<C>,
) -> Vec<GcObj<'new>> {
    vec.iter().map(|x| x.clone_in(bk)).collect()
}

impl<T> Gc<T> {
    pub(crate) fn clone_in<'old, 'new, const C: bool, U, E>(self, bk: &'new Block<C>) -> Gc<U>
    where
        Self: 'old,
        E: fmt::Debug,
        Gc<U>: TryFrom<Gc<Object<'new>>, Error = E>,
        // The WithLifetime bound ensures that T is the same type as U
        Gc<T>: Into<Gc<Object<'old>>> + WithLifetime<'new, Out = Gc<U>>,
    {
        let obj = match self.into().get() {
            Object::Int(x) => x.into(),
            Object::Cons(x) => x.clone_in(bk).into_obj(bk).into(),
            Object::String(x) => x.clone().into_obj(bk).into(),
            Object::Symbol(x) => x.into(),
            Object::LispFn(x) => x.clone_in(bk).into_obj(bk).into(),
            Object::SubrFn(x) => x.into(),
            Object::True => Gc::TRUE,
            Object::Nil => Gc::NIL,
            Object::Float(x) => x.into_obj(bk).into(),
            Object::Vec(x) => vec_clone_in(&x.borrow(), bk).into_obj(bk).into(),
        };
        match Gc::<U>::try_from(obj) {
            Ok(x) => x,
            Err(_) => unreachable!(),
        }
    }
}

impl<'ob> PartialEq<&str> for Gc<Object<'ob>> {
    fn eq(&self, other: &&str) -> bool {
        match self.get() {
            Object::String(x) => x == other,
            _ => false,
        }
    }
}

impl<'ob> PartialEq<Symbol> for Gc<Object<'ob>> {
    fn eq(&self, other: &Symbol) -> bool {
        match self.get() {
            Object::Symbol(x) => x == *other,
            _ => false,
        }
    }
}

impl<'ob> PartialEq<f64> for Gc<Object<'ob>> {
    fn eq(&self, other: &f64) -> bool {
        use float_cmp::ApproxEq;
        match self.get() {
            Object::Float(x) => x.approx_eq(*other, (f64::EPSILON, 2)),
            _ => false,
        }
    }
}

impl<'ob> PartialEq<i64> for Gc<Object<'ob>> {
    fn eq(&self, other: &i64) -> bool {
        match self.get() {
            Object::Int(x) => x == *other,
            _ => false,
        }
    }
}

impl Gc<Object<'_>> {
    pub(crate) const TRUE: Self = Gc::from_tag(Tag::True);
    pub(crate) const NIL: Self = Gc::from_tag(Tag::Nil);
}

impl Default for Gc<Object<'_>> {
    fn default() -> Self {
        Gc::NIL
    }
}

impl Default for &Gc<Object<'_>> {
    fn default() -> Self {
        &Gc::NIL
    }
}

impl<T: fmt::Display> fmt::Display for Gc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let obj = self.as_obj().get();
        write!(f, "{obj}")
    }
}

impl<T: fmt::Debug> fmt::Debug for Gc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let obj = self.as_obj().get();
        write!(f, "Gc: {obj}")
    }
}

impl<T> PartialEq for Gc<T> {
    fn eq(&self, other: &Self) -> bool {
        self.as_obj().get() == other.as_obj().get()
    }
}

impl fmt::Display for Object<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Int(x) => write!(f, "{x}"),
            Object::Cons(x) => write!(f, "{x}"),
            Object::Vec(x) => write!(f, "{x:?}"),
            Object::String(x) => write!(f, "\"{x}\""),
            Object::Symbol(x) => write!(f, "{x}"),
            Object::LispFn(x) => write!(f, "(lambda {x:?})"),
            Object::SubrFn(x) => write!(f, "{x:?}"),
            Object::True => write!(f, "t"),
            Object::Nil => write!(f, "nil"),
            Object::Float(x) => {
                if x.fract() == 0.0_f64 {
                    write!(f, "{x:.1}")
                } else {
                    write!(f, "{x}")
                }
            }
        }
    }
}

impl fmt::Debug for Object<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Int(x) => write!(f, "{x}"),
            Object::Cons(x) => write!(f, "{x:?}"),
            Object::Vec(x) => write!(f, "{x:?}"),
            Object::String(string) => {
                write!(
                    f,
                    "\"{}\"",
                    string
                        .chars()
                        .map(|x| if x == '\n' { '<' } else { x })
                        .collect::<String>()
                )
            }
            Object::Symbol(x) => write!(f, "{x}"),
            Object::LispFn(x) => write!(f, "(lambda {x:?})"),
            Object::SubrFn(x) => write!(f, "{x:?}"),
            Object::True => write!(f, "t"),
            Object::Nil => write!(f, "nil"),
            Object::Float(x) => {
                if x.fract() == 0.0_f64 {
                    write!(f, "{x:.1}")
                } else {
                    write!(f, "{x}")
                }
            }
        }
    }
}

pub(crate) enum ObjectAllocation<'ob> {
    Float(&'ob Allocation<f64>),
    Cons(&'ob Cons),
    Vec(&'ob Allocation<RefCell<ObjVec<'ob>>>),
    String(&'ob Allocation<String>),
    LispFn(&'ob Allocation<LispFn<'ob>>),
    NonAllocated,
}

impl<'ob> Gc<Object<'ob>> {
    pub(crate) fn is_markable(self) -> bool {
        !matches!(self.get_alloc(), ObjectAllocation::NonAllocated)
    }

    fn get_alloc(self) -> ObjectAllocation<'ob> {
        let (ptr, tag) = self.untag();
        match tag {
            Tag::LispFn => ObjectAllocation::LispFn(unsafe { &*LispFn::cast_ptr(ptr) }),
            Tag::Cons => ObjectAllocation::Cons(unsafe { &*Cons::cast_ptr(ptr) }),
            Tag::Float => ObjectAllocation::Float(unsafe { &*f64::cast_ptr(ptr) }),
            Tag::String => ObjectAllocation::String(unsafe { &*String::cast_ptr(ptr) }),
            Tag::Vec => ObjectAllocation::Vec(unsafe { &*Vec::cast_ptr(ptr) }),
            _ => ObjectAllocation::NonAllocated,
        }
    }

    pub(crate) fn is_marked(self) -> bool {
        match self.get_alloc() {
            ObjectAllocation::Float(x) => x.is_marked(),
            ObjectAllocation::Cons(x) => x.is_marked(),
            ObjectAllocation::Vec(x) => x.is_marked(),
            ObjectAllocation::String(x) => x.is_marked(),
            ObjectAllocation::LispFn(x) => x.is_marked(),
            ObjectAllocation::NonAllocated => true,
        }
    }

    pub(crate) fn trace_mark(self, stack: &mut Vec<RawObj>) {
        match self.get_alloc() {
            ObjectAllocation::Float(x) => x.mark(),
            ObjectAllocation::Vec(vec) => {
                let content = vec.borrow();
                let unmarked = content
                    .iter()
                    .filter_map(|x| x.is_markable().then(|| x.into_raw()));
                stack.extend(unmarked);
                vec.mark();
            }
            ObjectAllocation::String(x) => x.mark(),
            ObjectAllocation::Cons(x) => x.mark(stack),
            ObjectAllocation::LispFn(_) => unimplemented!(),
            ObjectAllocation::NonAllocated => {}
        }
    }
}
