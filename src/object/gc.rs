use sptr::Strict;
use std::fmt;
use std::{cell::RefCell, marker::PhantomData};

use crate::arena::Block;
use crate::error::{Error, Type};

use crate::{
    arena::{AllocObject, Allocation},
    cons::Cons,
    symbol::{GlobalSymbol, Symbol},
};

use super::{LispFn, SubrFn};

pub(crate) type ObjVec<'ob> = Vec<Object<'ob>>;
pub(crate) type Object<'ob> = Gc<ObjectX<'ob>>;

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
        let ptr = ptr.cast::<u8>().map_addr(|x| (x << 8) | tag as usize);
        Self::new(ptr)
    }

    const fn from_tag(tag: Tag) -> Self {
        let ptr = sptr::invalid(tag as usize);
        Self::new(ptr)
    }

    fn untag(self) -> (*const u8, Tag) {
        let ptr = self.ptr.map_addr(|x| x >> 8);
        let tag = self.tag();
        (ptr, tag)
    }

    fn tag(self) -> Tag {
        unsafe { std::mem::transmute(self.ptr.addr() as u8) }
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
    #[allow(clippy::wrong_self_convention)]
    pub(crate) fn as_obj<'ob>(self) -> Gc<ObjectX<'ob>> {
        Gc::new(self.ptr)
    }
}

impl<'a, T: Copy + 'a> From<Gc<T>> for ObjectX<'a> {
    fn from(x: Gc<T>) -> Self {
        x.as_obj().get()
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
    unsafe fn from_obj(ptr: *const u8) -> Self::Out;
}

impl<'ob> IntoObject<'ob> for Gc<ObjectX<'ob>> {
    type Out = ObjectX<'ob>;

    fn into_obj<const C: bool>(self, _block: &'ob Block<C>) -> Gc<Self::Out> {
        unsafe { Self::transmute(self) }
    }

    unsafe fn from_obj(_ptr: *const u8) -> Self::Out {
        unimplemented!()
    }
}

impl<'ob> IntoObject<'ob> for Option<Gc<ObjectX<'ob>>> {
    type Out = ObjectX<'ob>;

    fn into_obj<const C: bool>(self, _block: &'ob Block<C>) -> Gc<Self::Out> {
        match self {
            Some(x) => unsafe { x.with_lifetime() },
            None => Object::NIL,
        }
    }

    unsafe fn from_obj(_ptr: *const u8) -> Self::Out {
        unimplemented!()
    }
}

impl<'ob> IntoObject<'ob> for f64 {
    type Out = &'ob f64;

    fn into_obj<const C: bool>(self, block: &'ob Block<C>) -> Gc<Self::Out> {
        let ptr = self.alloc_obj(block);
        Gc::from_ptr(ptr, Tag::Float)
    }

    unsafe fn from_obj(ptr: *const u8) -> Self::Out {
        &*Self::into_ptr(ptr)
    }
}

impl<'ob> IntoObject<'ob> for i64 {
    type Out = i64;

    fn into_obj<const C: bool>(self, _: &'ob Block<C>) -> Gc<Self::Out> {
        let ptr: *const i64 = sptr::invalid(self as usize);
        unsafe { Self::gc_from(ptr) }
    }

    unsafe fn from_obj(ptr: *const u8) -> Self::Out {
        ptr.addr() as i64
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

    unsafe fn from_obj(_ptr: *const u8) -> Self::Out {
        unimplemented!()
    }
}

impl<'ob> IntoObject<'ob> for Cons {
    type Out = &'ob Cons;

    fn into_obj<const C: bool>(self, block: &'ob Block<C>) -> Gc<Self::Out> {
        let ptr = self.alloc_obj(block);
        Gc::from_ptr(ptr, Tag::Cons)
    }

    unsafe fn from_obj(ptr: *const u8) -> Self::Out {
        &*Self::into_ptr(ptr)
    }
}

impl<'ob> IntoObject<'ob> for SubrFn {
    type Out = &'static SubrFn;

    fn into_obj<const C: bool>(self, block: &'ob Block<C>) -> Gc<Self::Out> {
        let ptr = self.alloc_obj(block);
        Gc::from_ptr(ptr, Tag::SubrFn)
    }

    unsafe fn from_obj(ptr: *const u8) -> Self::Out {
        &*Self::into_ptr(ptr)
    }
}

impl<'ob> IntoObject<'ob> for LispFn<'ob> {
    type Out = &'ob LispFn<'ob>;

    fn into_obj<const C: bool>(self, block: &'ob Block<C>) -> Gc<Self::Out> {
        let ptr = self.alloc_obj(block);
        unsafe { Self::gc_from(ptr) }
    }

    unsafe fn from_obj(ptr: *const u8) -> Self::Out {
        &*Self::into_ptr(ptr)
    }
}

impl<'ob> IntoObject<'ob> for Symbol {
    type Out = Symbol;

    fn into_obj<const C: bool>(self, _: &'ob Block<C>) -> Gc<Self::Out> {
        let ptr: *const u8 = (self as *const GlobalSymbol).cast();
        Gc::from_ptr(ptr, Tag::Symbol)
    }

    unsafe fn from_obj(ptr: *const u8) -> Self::Out {
        &*Self::into_ptr(ptr)
    }
}

impl<'ob> IntoObject<'ob> for String {
    type Out = &'ob String;

    fn into_obj<const C: bool>(self, block: &'ob Block<C>) -> Gc<Self::Out> {
        let ptr = self.alloc_obj(block);
        unsafe { Self::gc_from(ptr) }
    }

    unsafe fn from_obj(ptr: *const u8) -> Self::Out {
        &*Self::into_ptr(ptr)
    }
}

impl<'ob> IntoObject<'ob> for &str {
    type Out = &'ob String;

    fn into_obj<const C: bool>(self, block: &'ob Block<C>) -> Gc<Self::Out> {
        let ptr = self.to_owned().alloc_obj(block);
        unsafe { String::gc_from(ptr) }
    }

    unsafe fn from_obj(ptr: *const u8) -> Self::Out {
        &*String::into_ptr(ptr)
    }
}

impl<'ob> IntoObject<'ob> for ObjVec<'ob> {
    type Out = &'ob RefCell<ObjVec<'ob>>;

    fn into_obj<const C: bool>(self, block: &'ob Block<C>) -> Gc<Self::Out> {
        let ptr = self.alloc_obj(block);
        unsafe { Self::gc_from(ptr) }
    }

    unsafe fn from_obj(ptr: *const u8) -> Self::Out {
        &*Self::into_ptr(ptr)
    }
}

// work around for no GAT's
#[allow(unused_lifetimes)]
// TODO: once this is moved to core change this visibility
trait FromPtr<'ob>
where
    Self: Sized,
{
    type Ptr;
    type Output;
    const TAG: Tag;
    unsafe fn gc_from(ptr: *const Self::Ptr) -> Gc<Self::Output> {
        Gc::from_ptr(ptr, Self::TAG)
    }
    fn into_ptr(ptr: *const u8) -> *const Self::Ptr {
        ptr.cast()
    }
}

impl FromPtr<'_> for i64 {
    type Ptr = i64;
    type Output = i64;
    const TAG: Tag = Tag::Int;
}

impl<'ob> FromPtr<'ob> for f64 {
    type Ptr = <Self as AllocObject>::Output;
    type Output = &'ob f64;
    const TAG: Tag = Tag::Float;
}

impl<'ob> FromPtr<'ob> for Cons {
    type Ptr = <Self as AllocObject>::Output;
    type Output = &'ob Cons;
    const TAG: Tag = Tag::Cons;
}

impl<'ob> FromPtr<'ob> for SubrFn {
    type Ptr = <Self as AllocObject>::Output;
    type Output = &'ob SubrFn;
    const TAG: Tag = Tag::SubrFn;
}

impl FromPtr<'_> for Symbol {
    type Ptr = GlobalSymbol;
    type Output = Symbol;
    const TAG: Tag = Tag::Symbol;
}

impl<'ob> FromPtr<'ob> for LispFn<'ob> {
    type Ptr = <Self as AllocObject>::Output;
    type Output = &'ob LispFn<'ob>;
    const TAG: Tag = Tag::LispFn;
}

impl<'ob> FromPtr<'ob> for String {
    type Ptr = <Self as AllocObject>::Output;
    type Output = &'ob String;
    const TAG: Tag = Tag::String;
}

impl<'ob> FromPtr<'ob> for ObjVec<'ob> {
    type Ptr = <Self as AllocObject>::Output;
    type Output = &'ob RefCell<ObjVec<'ob>>;
    const TAG: Tag = Tag::Vec;
}

////////////////////////
// Proc macro section //
////////////////////////

// Number
#[derive(Copy, Clone)]
pub(crate) enum NumberX<'ob> {
    Int(i64),
    Float(&'ob f64),
}

impl<'old, 'new> WithLifetime<'new> for Gc<NumberX<'old>> {
    type Out = Gc<NumberX<'new>>;

    unsafe fn with_lifetime(self) -> Self::Out {
        transmute(self)
    }
}

impl<'ob> From<Gc<i64>> for Gc<NumberX<'ob>> {
    fn from(x: Gc<i64>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Gc<&'ob f64>> for Gc<NumberX<'ob>> {
    fn from(x: Gc<&'ob f64>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> Gc<NumberX<'ob>> {
    pub(crate) fn get(self) -> NumberX<'ob> {
        let (ptr, tag) = self.untag();
        match tag {
            Tag::Int => NumberX::Int(unsafe { i64::from_obj(ptr) }),
            Tag::Float => NumberX::Float(unsafe { f64::from_obj(ptr) }),
            _ => unreachable!(),
        }
    }
}

impl<'ob> From<i64> for Gc<NumberX<'ob>> {
    fn from(x: i64) -> Self {
        let ptr = sptr::invalid(x as usize);
        unsafe { i64::gc_from(ptr).into() }
    }
}

// List
#[derive(Copy, Clone, Debug)]
pub(crate) enum ListX<'ob> {
    Nil,
    Cons(&'ob Cons),
}

impl<'old, 'new> WithLifetime<'new> for Gc<ListX<'old>> {
    type Out = Gc<ListX<'new>>;

    unsafe fn with_lifetime(self) -> Self::Out {
        transmute(self)
    }
}

impl<'ob> From<Gc<&'ob Cons>> for Gc<ListX<'ob>> {
    fn from(x: Gc<&'ob Cons>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Gc<()>> for Gc<ListX<'ob>> {
    fn from(x: Gc<()>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> Gc<ListX<'ob>> {
    pub(crate) fn get(self) -> ListX<'ob> {
        let (ptr, tag) = self.untag();
        match tag {
            Tag::Nil => ListX::Nil,
            Tag::Cons => ListX::Cons(unsafe { Cons::from_obj(ptr) }),
            _ => unreachable!(),
        }
    }
}

// This is safe because cons is a gc heap only type
impl<'ob> From<&'ob Cons> for Gc<ListX<'ob>> {
    fn from(x: &'ob Cons) -> Self {
        unsafe { Cons::gc_from(x as *const _).into() }
    }
}

// Callable
#[derive(Copy, Clone, Debug)]
pub(crate) enum CallableX<'ob> {
    LispFn(&'ob LispFn<'ob>),
    SubrFn(&'static SubrFn),
    Cons(&'ob Cons),
}

impl<'old, 'new> WithLifetime<'new> for Gc<CallableX<'old>> {
    type Out = Gc<CallableX<'new>>;

    unsafe fn with_lifetime(self) -> Self::Out {
        transmute(self)
    }
}

impl<'ob> From<Gc<&'ob Cons>> for Gc<CallableX<'ob>> {
    fn from(x: Gc<&'ob Cons>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Gc<&'static SubrFn>> for Gc<CallableX<'ob>> {
    fn from(x: Gc<&'static SubrFn>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Gc<&'ob LispFn<'ob>>> for Gc<CallableX<'ob>> {
    fn from(x: Gc<&'ob LispFn<'ob>>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> Gc<CallableX<'ob>> {
    pub(crate) fn get(self) -> CallableX<'ob> {
        let (ptr, tag) = self.untag();
        match tag {
            Tag::Cons => CallableX::Cons(unsafe { Cons::from_obj(ptr) }),
            Tag::SubrFn => CallableX::SubrFn(unsafe { SubrFn::from_obj(ptr) }),
            Tag::LispFn => CallableX::LispFn(unsafe { LispFn::from_obj(ptr) }),
            _ => unreachable!(),
        }
    }
}

// FunctionX
#[derive(Copy, Clone, Debug)]
pub(crate) enum FunctionX<'ob> {
    LispFn(&'ob LispFn<'ob>),
    SubrFn(&'static SubrFn),
    Cons(&'ob Cons),
    Symbol(Symbol),
}

impl<'old, 'new> WithLifetime<'new> for Gc<FunctionX<'old>> {
    type Out = Gc<FunctionX<'new>>;

    unsafe fn with_lifetime(self) -> Self::Out {
        transmute(self)
    }
}

#[cfg(miri)]
extern "Rust" {
    fn miri_static_root(ptr: *const u8);
}

#[cfg(miri)]
impl<'ob> FunctionX<'ob> {
    pub(crate) fn set_as_miri_root(self) {
        match self {
            FunctionX::LispFn(x) => {
                let ptr: *const _ = x;
                unsafe {
                    miri_static_root(ptr as _);
                }
            }
            FunctionX::SubrFn(x) => {
                let ptr: *const _ = x;
                unsafe {
                    miri_static_root(ptr as _);
                }
            }
            FunctionX::Cons(x) => {
                let ptr: *const _ = x;
                unsafe {
                    miri_static_root(ptr as _);
                }
            }
            FunctionX::Symbol(_) => {}
        }
    }
}

impl<'ob> From<Gc<&'ob Cons>> for Gc<FunctionX<'ob>> {
    fn from(x: Gc<&'ob Cons>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Gc<&'static SubrFn>> for Gc<FunctionX<'ob>> {
    fn from(x: Gc<&'static SubrFn>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Gc<&'ob LispFn<'ob>>> for Gc<FunctionX<'ob>> {
    fn from(x: Gc<&'ob LispFn<'ob>>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Gc<Symbol>> for Gc<FunctionX<'ob>> {
    fn from(x: Gc<Symbol>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Symbol> for Gc<FunctionX<'ob>> {
    fn from(x: Symbol) -> Self {
        let ptr = x as *const GlobalSymbol;
        unsafe { Symbol::gc_from(ptr).into() }
    }
}

impl From<&'static SubrFn> for Gc<FunctionX<'_>> {
    fn from(x: &'static SubrFn) -> Self {
        let ptr = x as *const SubrFn;
        unsafe { SubrFn::gc_from(ptr).into() }
    }
}

impl<'ob> Gc<FunctionX<'ob>> {
    pub(crate) fn get(self) -> FunctionX<'ob> {
        let (ptr, tag) = self.untag();
        match tag {
            Tag::Cons => FunctionX::Cons(unsafe { Cons::from_obj(ptr) }),
            Tag::SubrFn => FunctionX::SubrFn(unsafe { SubrFn::from_obj(ptr) }),
            Tag::LispFn => FunctionX::LispFn(unsafe { LispFn::from_obj(ptr) }),
            Tag::Symbol => FunctionX::Symbol(unsafe { Symbol::from_obj(ptr) }),
            _ => unreachable!(),
        }
    }
}

impl<'ob> From<Gc<CallableX<'ob>>> for Gc<FunctionX<'ob>> {
    fn from(obj: Gc<CallableX<'ob>>) -> Self {
        unsafe { Self::transmute(obj) }
    }
}

impl<'ob> TryFrom<Gc<FunctionX<'ob>>> for Gc<CallableX<'ob>> {
    type Error = anyhow::Error;

    fn try_from(value: Gc<FunctionX<'ob>>) -> Result<Self, Self::Error> {
        match value.tag() {
            Tag::Symbol => Err(Error::from_object(Type::Func, value).into()),
            _ => unsafe { Ok(Self::transmute(value)) },
        }
    }
}

#[derive(Copy, Clone)]
pub(crate) enum ObjectX<'ob> {
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

impl ObjectX<'_> {
    /// Return the type of an object
    pub(crate) fn get_type(self) -> Type {
        match self {
            ObjectX::Int(_) => Type::Int,
            ObjectX::Float(_) => Type::Float,
            ObjectX::Symbol(_) => Type::Symbol,
            ObjectX::True => Type::True,
            ObjectX::Nil => Type::Nil,
            ObjectX::Cons(_) => Type::Cons,
            ObjectX::Vec(_) => Type::Vec,
            ObjectX::String(_) => Type::String,
            ObjectX::LispFn(_) | ObjectX::SubrFn(_) => Type::Func,
        }
    }
}

impl PartialEq for ObjectX<'_> {
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

impl<'old, 'new> WithLifetime<'new> for Gc<ObjectX<'old>> {
    type Out = Gc<ObjectX<'new>>;

    unsafe fn with_lifetime(self) -> Self::Out {
        transmute(self)
    }
}

impl<'ob> Gc<ObjectX<'ob>> {
    pub(crate) fn get(self) -> ObjectX<'ob> {
        let (ptr, tag) = self.untag();
        match tag {
            Tag::Symbol => ObjectX::Symbol(unsafe { Symbol::from_obj(ptr) }),
            Tag::Cons => ObjectX::Cons(unsafe { Cons::from_obj(ptr) }),
            Tag::SubrFn => ObjectX::SubrFn(unsafe { SubrFn::from_obj(ptr) }),
            Tag::LispFn => ObjectX::LispFn(unsafe { LispFn::from_obj(ptr) }),
            Tag::Int => ObjectX::Int(unsafe { i64::from_obj(ptr) }),
            Tag::Float => ObjectX::Float(unsafe { f64::from_obj(ptr) }),
            Tag::Nil => ObjectX::Nil,
            Tag::True => ObjectX::True,
            Tag::String => ObjectX::String(unsafe { String::from_obj(ptr) }),
            Tag::Vec => ObjectX::Vec(unsafe { ObjVec::from_obj(ptr) }),
        }
    }
}

impl<'ob> From<Gc<i64>> for Gc<ObjectX<'ob>> {
    fn from(x: Gc<i64>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<i64> for Gc<ObjectX<'ob>> {
    fn from(x: i64) -> Self {
        let ptr = sptr::invalid(x as usize);
        unsafe { i64::gc_from(ptr).into() }
    }
}

impl<'ob> From<Gc<&'ob f64>> for Gc<ObjectX<'ob>> {
    fn from(x: Gc<&'ob f64>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Symbol> for Gc<ObjectX<'ob>> {
    fn from(x: Symbol) -> Self {
        let ptr = x as *const GlobalSymbol;
        unsafe { Symbol::gc_from(ptr).into() }
    }
}

impl<'ob> From<&Cons> for Gc<ObjectX<'ob>> {
    fn from(x: &Cons) -> Self {
        let ptr = x as *const Cons;
        unsafe { Cons::gc_from(ptr).into() }
    }
}

impl<'ob> From<Gc<Symbol>> for Gc<ObjectX<'ob>> {
    fn from(x: Gc<Symbol>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Gc<()>> for Gc<ObjectX<'ob>> {
    fn from(x: Gc<()>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Gc<bool>> for Gc<ObjectX<'ob>> {
    fn from(x: Gc<bool>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Gc<&'ob Cons>> for Gc<ObjectX<'ob>> {
    fn from(x: Gc<&'ob Cons>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Gc<&'ob String>> for Gc<ObjectX<'ob>> {
    fn from(x: Gc<&'ob String>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Gc<&'ob RefCell<Vec<Object<'ob>>>>> for Gc<ObjectX<'ob>> {
    fn from(x: Gc<&'ob RefCell<Vec<Object<'ob>>>>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Gc<&'ob LispFn<'ob>>> for Gc<ObjectX<'ob>> {
    fn from(x: Gc<&'ob LispFn<'ob>>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<&'ob SubrFn> for Gc<ObjectX<'ob>> {
    fn from(x: &'ob SubrFn) -> Self {
        unsafe { SubrFn::gc_from(x as *const _).into() }
    }
}

impl<'ob> From<Gc<&'ob SubrFn>> for Gc<ObjectX<'ob>> {
    fn from(x: Gc<&'ob SubrFn>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Gc<NumberX<'ob>>> for Gc<ObjectX<'ob>> {
    fn from(x: Gc<NumberX<'ob>>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> TryFrom<Gc<ObjectX<'ob>>> for Gc<NumberX<'ob>> {
    type Error = Error;

    fn try_from(value: Gc<ObjectX<'ob>>) -> Result<Self, Self::Error> {
        match value.tag() {
            Tag::Int | Tag::Float => unsafe { Ok(Self::transmute(value)) },
            _ => Err(Error::from_object(Type::Number, value)),
        }
    }
}

impl<'ob> TryFrom<Gc<ObjectX<'ob>>> for Option<Gc<NumberX<'ob>>> {
    type Error = Error;

    fn try_from(value: Gc<ObjectX<'ob>>) -> Result<Self, Self::Error> {
        match value.tag() {
            Tag::Int | Tag::Float => unsafe { Ok(Some(transmute(value))) },
            Tag::Nil => Ok(None),
            _ => Err(Error::from_object(Type::Number, value)),
        }
    }
}

impl<'ob> From<Gc<ListX<'ob>>> for Gc<ObjectX<'ob>> {
    fn from(x: Gc<ListX<'ob>>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> TryFrom<Gc<ObjectX<'ob>>> for Gc<ListX<'ob>> {
    type Error = Error;

    fn try_from(value: Gc<ObjectX<'ob>>) -> Result<Self, Self::Error> {
        match value.tag() {
            Tag::Nil | Tag::Cons => unsafe { Ok(Self::transmute(value)) },
            _ => Err(Error::from_object(Type::List, value)),
        }
    }
}

impl<'ob> From<Gc<FunctionX<'ob>>> for Gc<ObjectX<'ob>> {
    fn from(x: Gc<FunctionX<'ob>>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> TryFrom<Gc<ObjectX<'ob>>> for Gc<FunctionX<'ob>> {
    type Error = Error;

    fn try_from(value: Gc<ObjectX<'ob>>) -> Result<Self, Self::Error> {
        match value.tag() {
            Tag::LispFn | Tag::SubrFn | Tag::Cons | Tag::Symbol => unsafe {
                Ok(Self::transmute(value))
            },
            _ => Err(Error::from_object(Type::Func, value)),
        }
    }
}

impl<'ob> From<Gc<CallableX<'ob>>> for Gc<ObjectX<'ob>> {
    fn from(x: Gc<CallableX<'ob>>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> TryFrom<Gc<ObjectX<'ob>>> for Gc<CallableX<'ob>> {
    type Error = Error;

    fn try_from(value: Gc<ObjectX<'ob>>) -> Result<Self, Self::Error> {
        match value.tag() {
            Tag::LispFn | Tag::SubrFn | Tag::Cons => unsafe { Ok(Self::transmute(value)) },
            _ => Err(Error::from_object(Type::Func, value)),
        }
    }
}

///////////////////////////
// Compatibility Section //
///////////////////////////
// use super::{LispFn, Number, SubrFn};

// impl<'ob> TryFrom<Object<'ob>> for Gc<NumberX<'ob>> {
//     type Error = Error;

//     fn try_from(value: Object<'ob>) -> Result<Self, Self::Error> {
//         match value {
//             ObjectX::Int(x) => {
//                 let x: i64 = !x;
//                 let ptr = sptr::invalid(x as usize);
//                 unsafe { Ok(i64::gc_from(ptr).into()) }
//             }
//             ObjectX::Float(x) => unsafe { Ok(f64::gc_from(x.get_alloc() as *const _).into()) },
//             _ => Err(Error::from_object(Type::Number, value)),
//         }
//     }
// }

// impl<'ob> From<Number<'ob>> for Gc<NumberX<'ob>> {
//     fn from(x: Number) -> Self {
//         match x {
//             Number::Int(x) => {
//                 let x: i64 = !x;
//                 let ptr: *const u8 = sptr::invalid(x as usize);
//                 Gc::from_ptr(ptr, Tag::Int)
//             }
//             Number::Float(x) => {
//                 let x: &f64 = !x;
//                 Gc::from_ptr(x as *const _, Tag::Float)
//             }
//         }
//     }
// }

// // List
// impl<'ob> TryFrom<Object<'ob>> for Gc<ListX<'ob>> {
//     type Error = Error;

//     fn try_from(value: Object<'ob>) -> Result<Self, Self::Error> {
//         match value {
//             ObjectX::Nil => Ok(Gc::from_tag(Tag::Nil)),
//             ObjectX::Cons(x) => {
//                 let x: &Cons = !x;
//                 unsafe { Ok(Cons::gc_from(x as *const Cons).into()) }
//             }
//             _ => Err(Error::from_object(Type::List, value)),
//         }
//     }
// }

// impl<'ob> From<Gc<ListX<'ob>>> for Object<'ob> {
//     fn from(x: Gc<ListX<'ob>>) -> Self {
//         match x.get() {
//             ListX::Nil => Object::NIL,
//             ListX::Cons(x) => ObjectX::Cons(super::data::Data::from_ref(x)),
//         }
//     }
// }

// // Callable

// impl<'ob> TryFrom<Object<'ob>> for Gc<CallableX<'ob>> {
//     type Error = Error;

//     fn try_from(value: Object<'ob>) -> Result<Self, Self::Error> {
//         match value {
//             ObjectX::Cons(x) => {
//                 let x: &Cons = !x;
//                 unsafe { Ok(Cons::gc_from(x as *const Cons).into()) }
//             }
//             ObjectX::SubrFn(x) => {
//                 let x: &SubrFn = !x;
//                 unsafe { Ok(SubrFn::gc_from(x as *const SubrFn).into()) }
//             }
//             ObjectX::LispFn(x) => {
//                 let x: &Allocation<LispFn> = x.get_alloc();
//                 unsafe { Ok(LispFn::gc_from(x as *const Allocation<LispFn>).into()) }
//             }
//             _ => Err(Error::from_object(Type::Func, value)),
//         }
//     }
// }

// impl<'ob> From<Gc<CallableX<'ob>>> for Object<'ob> {
//     fn from(x: Gc<CallableX<'ob>>) -> Self {
//         match x.get() {
//             CallableX::Cons(x) => ObjectX::Cons(super::data::Data::from_ref(x)),
//             CallableX::SubrFn(x) => ObjectX::SubrFn(super::data::Data::from_ref(x)),
//             CallableX::LispFn(_) => {
//                 let rf: &Allocation<LispFn> = unsafe { &*x.ptr.cast() };
//                 ObjectX::LispFn(super::data::Data::from_ref(rf))
//             }
//         }
//     }
// }

// // FunctionX

// impl<'ob> TryFrom<Object<'ob>> for Gc<FunctionX<'ob>> {
//     type Error = Error;

//     fn try_from(value: Object<'ob>) -> Result<Self, Self::Error> {
//         match value {
//             ObjectX::Cons(x) => {
//                 let x: &Cons = !x;
//                 unsafe { Ok(Cons::gc_from(x as *const Cons).into()) }
//             }
//             ObjectX::SubrFn(x) => {
//                 let x: &SubrFn = !x;
//                 unsafe { Ok(SubrFn::gc_from(x as *const SubrFn).into()) }
//             }
//             ObjectX::LispFn(x) => {
//                 let x: &Allocation<LispFn> = x.get_alloc();
//                 unsafe { Ok(LispFn::gc_from(x as *const Allocation<LispFn>).into()) }
//             }
//             ObjectX::Symbol(x) => {
//                 let x: Symbol = !x;
//                 unsafe { Ok(Symbol::gc_from(x as *const GlobalSymbol).into()) }
//             }
//             _ => Err(Error::from_object(Type::Func, value)),
//         }
//     }
// }

// impl<'ob> From<Gc<FunctionX<'ob>>> for Object<'ob> {
//     fn from(x: Gc<FunctionX<'ob>>) -> Self {
//         match x.get() {
//             FunctionX::Cons(x) => ObjectX::Cons(super::data::Data::from_ref(x)),
//             FunctionX::SubrFn(x) => ObjectX::SubrFn(super::data::Data::from_ref(x)),
//             FunctionX::Symbol(x) => ObjectX::Symbol(super::data::Data::from_ref(x)),
//             FunctionX::LispFn(_) => {
//                 let rf: &Allocation<LispFn> = unsafe { &*x.ptr.cast() };
//                 ObjectX::LispFn(super::data::Data::from_ref(rf))
//             }
//         }
//     }
// }

// impl<'ob> From<Gc<ObjectX<'ob>>> for Object<'ob> {
//     fn from(x: Gc<ObjectX<'ob>>) -> Self {
//         use super::data::Data;
//         match x.get() {
//             ObjectX::Int(x) => x.into(),
//             ObjectX::Float(_) => {
//                 let rf: &Allocation<f64> = unsafe { &*x.ptr.cast() };
//                 ObjectX::Float(Data::from_ref(rf))
//             }
//             ObjectX::Symbol(x) => x.into(),
//             ObjectX::True => Object::TRUE,
//             ObjectX::Nil => Object::NIL,
//             ObjectX::Cons(x) => ObjectX::Cons(Data::from_ref(x)),
//             ObjectX::Vec(_) => {
//                 let rf: &Allocation<RefCell<Vec<Object>>> = unsafe { &*x.ptr.cast() };
//                 ObjectX::Vec(Data::from_ref(rf))
//             }
//             ObjectX::String(_) => {
//                 let rf: &Allocation<String> = unsafe { &*x.ptr.cast() };
//                 ObjectX::String(Data::from_ref(rf))
//             }
//             ObjectX::LispFn(_) => {
//                 let rf: &Allocation<LispFn> = unsafe { &*x.ptr.cast() };
//                 ObjectX::LispFn(Data::from_ref(rf))
//             }
//             ObjectX::SubrFn(subr) => ObjectX::SubrFn(Data::from_ref(subr)),
//         }
//     }
// }

///////////////////////////
// Other implementations //
///////////////////////////

impl<'ob> TryFrom<Gc<ObjectX<'ob>>> for Gc<i64> {
    type Error = Error;

    fn try_from(value: Gc<ObjectX<'ob>>) -> Result<Self, Self::Error> {
        match value.tag() {
            Tag::Int => unsafe { Ok(Self::transmute(value)) },
            _ => Err(Error::from_object(Type::Int, value)),
        }
    }
}

impl Gc<i64> {
    pub(crate) fn get(self) -> i64 {
        let (ptr, _) = self.untag();
        unsafe { i64::from_obj(ptr) }
    }
}

fn vec_clone_in<'old, 'new, const C: bool>(
    vec: &[Object<'old>],
    bk: &'new Block<C>,
) -> Vec<Object<'new>> {
    vec.iter().map(|x| x.clone_in(bk)).collect()
}

impl<T> Gc<T> {
    pub(crate) fn clone_in<'old, 'new, const C: bool, U, E>(self, bk: &'new Block<C>) -> Gc<U>
    where
        Self: 'old,
        E: fmt::Debug,
        Gc<U>: TryFrom<Gc<ObjectX<'new>>, Error = E>,
        // The WithLifetime bound ensures that T is the same type as U
        Gc<T>: Into<Gc<ObjectX<'old>>> + WithLifetime<'new, Out = Gc<U>>,
    {
        let obj = match self.into().get() {
            ObjectX::Int(x) => x.into(),
            ObjectX::Cons(x) => x.clone_in(bk).into_obj(bk).into(),
            ObjectX::String(x) => x.clone().into_obj(bk).into(),
            ObjectX::Symbol(x) => x.into(),
            ObjectX::LispFn(x) => x.clone_in(bk).into_obj(bk).into(),
            ObjectX::SubrFn(x) => x.into(),
            ObjectX::True => Gc::TRUE,
            ObjectX::Nil => Gc::NIL,
            ObjectX::Float(x) => x.into_obj(bk).into(),
            ObjectX::Vec(x) => vec_clone_in(&x.borrow(), bk).into_obj(bk).into(),
        };
        match Gc::<U>::try_from(obj) {
            Ok(x) => x,
            Err(_) => unreachable!(),
        }
    }
}

#[derive(Copy, Clone, Debug)]
pub(crate) struct RawObjX(*const u8);

impl<'a> From<Gc<ObjectX<'a>>> for RawObjX {
    fn from(x: Gc<ObjectX<'a>>) -> Self {
        Self(x.ptr)
    }
}

impl<'ob> PartialEq<&str> for Gc<ObjectX<'ob>> {
    fn eq(&self, other: &&str) -> bool {
        match self.get() {
            ObjectX::String(x) => x == other,
            _ => false,
        }
    }
}

impl<'ob> PartialEq<Symbol> for Gc<ObjectX<'ob>> {
    fn eq(&self, other: &Symbol) -> bool {
        match self.get() {
            ObjectX::Symbol(x) => x == *other,
            _ => false,
        }
    }
}

impl<'ob> PartialEq<f64> for Gc<ObjectX<'ob>> {
    fn eq(&self, other: &f64) -> bool {
        use float_cmp::ApproxEq;
        match self.get() {
            ObjectX::Float(x) => x.approx_eq(*other, (f64::EPSILON, 2)),
            _ => false,
        }
    }
}

impl<'ob> PartialEq<i64> for Gc<ObjectX<'ob>> {
    fn eq(&self, other: &i64) -> bool {
        match self.get() {
            ObjectX::Int(x) => x == *other,
            _ => false,
        }
    }
}

impl Gc<ObjectX<'_>> {
    pub(crate) const TRUE: Self = Gc::from_tag(Tag::True);
    pub(crate) const NIL: Self = Gc::from_tag(Tag::Nil);
}

impl Default for Gc<ObjectX<'_>> {
    fn default() -> Self {
        Gc::NIL
    }
}

impl Default for &Gc<ObjectX<'_>> {
    fn default() -> Self {
        &Gc::NIL
    }
}

impl<T: fmt::Display + Copy> fmt::Display for Gc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let obj = self.as_obj().get();
        write!(f, "{obj}")
    }
}

impl<T: fmt::Debug + Copy> fmt::Debug for Gc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let obj = self.as_obj().get();
        write!(f, "Gc: {obj}")
    }
}

impl<T: Copy> PartialEq for Gc<T> {
    fn eq(&self, other: &Self) -> bool {
        self.as_obj().get() == other.as_obj().get()
    }
}

impl fmt::Display for ObjectX<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ObjectX::Int(x) => write!(f, "{x}"),
            ObjectX::Cons(x) => write!(f, "{x}"),
            ObjectX::Vec(x) => write!(f, "{x:?}"),
            ObjectX::String(x) => write!(f, "\"{x}\""),
            ObjectX::Symbol(x) => write!(f, "{x}"),
            ObjectX::LispFn(x) => write!(f, "(lambda {x:?})"),
            ObjectX::SubrFn(x) => write!(f, "{x:?}"),
            ObjectX::True => write!(f, "t"),
            ObjectX::Nil => write!(f, "nil"),
            ObjectX::Float(x) => {
                if x.fract() == 0.0_f64 {
                    write!(f, "{x:.1}")
                } else {
                    write!(f, "{x}")
                }
            }
        }
    }
}

impl fmt::Debug for ObjectX<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ObjectX::Int(x) => write!(f, "{x}"),
            ObjectX::Cons(x) => write!(f, "{x:?}"),
            ObjectX::Vec(x) => write!(f, "{x:?}"),
            ObjectX::String(string) => {
                write!(
                    f,
                    "\"{}\"",
                    string
                        .chars()
                        .map(|x| if x == '\n' { '<' } else { x })
                        .collect::<String>()
                )
            }
            ObjectX::Symbol(x) => write!(f, "{x}"),
            ObjectX::LispFn(x) => write!(f, "(lambda {x:?})"),
            ObjectX::SubrFn(x) => write!(f, "{x:?}"),
            ObjectX::True => write!(f, "t"),
            ObjectX::Nil => write!(f, "nil"),
            ObjectX::Float(x) => {
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

impl<'ob> Gc<ObjectX<'ob>> {
    pub(crate) fn is_markable(self) -> bool {
        !matches!(self.get_alloc(), ObjectAllocation::NonAllocated)
    }

    fn get_alloc(self) -> ObjectAllocation<'ob> {
        let (ptr, tag) = self.untag();
        match tag {
            Tag::LispFn => ObjectAllocation::LispFn(unsafe { &*LispFn::into_ptr(ptr) }),
            Tag::Cons => ObjectAllocation::Cons(unsafe { &*Cons::into_ptr(ptr) }),
            Tag::Float => ObjectAllocation::Float(unsafe { &*f64::into_ptr(ptr) }),
            Tag::String => ObjectAllocation::String(unsafe { &*String::into_ptr(ptr) }),
            Tag::Vec => ObjectAllocation::Vec(unsafe { &*Vec::into_ptr(ptr) }),
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
