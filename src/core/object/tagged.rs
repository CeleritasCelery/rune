use sptr::Strict;
use std::fmt;
use std::{cell::RefCell, marker::PhantomData};

use crate::core::env::ConstSymbol;
use crate::core::gc::Trace;

use super::super::{
    cons::Cons,
    env::Symbol,
    error::{Type, TypeError},
    gc::{AllocObject, Allocation, Block},
};

use super::{HashTable, LispFn, LispHashTable, LispVec, Record, RecordBuilder, SubrFn};

pub(crate) type GcObj<'ob> = Gc<Object<'ob>>;

#[derive(Copy, Clone, Debug)]
pub(crate) struct RawObj {
    ptr: *const u8,
}

unsafe impl Send for RawObj {}

impl Default for RawObj {
    fn default() -> Self {
        Self { ptr: nil().ptr }
    }
}

#[inline(always)]
pub(crate) fn nil<'a>() -> GcObj<'a> {
    crate::core::env::sym::NIL.into()
}

pub(crate) fn qtrue<'a>() -> GcObj<'a> {
    crate::core::env::sym::TRUE.into()
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
    String,
    Vec,
    Record,
    HashTable,
    SubrFn,
    LispFn,
    ByteVec,
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

    fn untag(self) -> (*const u8, Tag) {
        let ptr = Strict::map_addr(self.ptr, |x| ((x as isize) >> 8) as usize);
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

    pub(crate) unsafe fn from_raw_ptr(raw: *mut u8) -> Self {
        Self::new(raw)
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

pub(crate) trait RawInto<T> {
    unsafe fn raw_into(self) -> T;
}

impl<'ob> RawInto<GcObj<'ob>> for RawObj {
    unsafe fn raw_into(self) -> GcObj<'ob> {
        GcObj::new(self.ptr)
    }
}

impl<'ob> RawInto<&'ob Cons> for *const Cons {
    unsafe fn raw_into(self) -> &'ob Cons {
        &*self
    }
}

pub(crate) trait IntoObject {
    type Out<'ob>;

    fn into_obj<const C: bool>(self, block: &Block<C>) -> Gc<Self::Out<'_>>;

    unsafe fn from_obj_ptr<'ob>(_ptr: *const u8) -> Self::Out<'ob> {
        unimplemented!()
    }
}

impl<T> IntoObject for Gc<T> {
    type Out<'ob> = Object<'ob>;

    fn into_obj<const C: bool>(self, _block: &Block<C>) -> Gc<Self::Out<'_>> {
        unsafe { Self::transmute(self) }
    }
}

impl<T> IntoObject for Option<Gc<T>> {
    type Out<'ob> = Object<'ob>;

    fn into_obj<const C: bool>(self, _block: &Block<C>) -> Gc<Self::Out<'_>> {
        match self {
            Some(x) => unsafe { transmute(x) },
            None => nil(),
        }
    }
}

impl IntoObject for f64 {
    type Out<'ob> = &'ob f64;

    fn into_obj<const C: bool>(self, block: &Block<C>) -> Gc<Self::Out<'_>> {
        let ptr = self.alloc_obj(block);
        Gc::from_ptr(ptr, Tag::Float)
    }

    unsafe fn from_obj_ptr<'ob>(ptr: *const u8) -> Self::Out<'ob> {
        &(*Self::cast_ptr(ptr)).data
    }
}

impl IntoObject for i64 {
    type Out<'a> = i64;

    fn into_obj<const C: bool>(self, _: &Block<C>) -> Gc<Self::Out<'_>> {
        let ptr: *const i64 = sptr::invalid(self as usize);
        unsafe { Self::tag_ptr(ptr) }
    }

    unsafe fn from_obj_ptr<'ob>(ptr: *const u8) -> Self::Out<'ob> {
        Strict::addr(ptr) as i64
    }
}

impl IntoObject for i32 {
    type Out<'a> = i64;

    fn into_obj<const C: bool>(self, _: &Block<C>) -> Gc<Self::Out<'_>> {
        let ptr: *const i64 = sptr::invalid(self as usize);
        unsafe { i64::tag_ptr(ptr) }
    }

    unsafe fn from_obj_ptr<'ob>(ptr: *const u8) -> Self::Out<'ob> {
        Strict::addr(ptr) as i64
    }
}

impl IntoObject for usize {
    type Out<'a> = i64;

    fn into_obj<const C: bool>(self, _: &Block<C>) -> Gc<Self::Out<'_>> {
        let ptr: *const i64 = sptr::invalid(self);
        unsafe { i64::tag_ptr(ptr) }
    }

    unsafe fn from_obj_ptr<'ob>(ptr: *const u8) -> Self::Out<'ob> {
        Strict::addr(ptr) as i64
    }
}

impl IntoObject for bool {
    type Out<'a> = bool;

    fn into_obj<const C: bool>(self, _: &Block<C>) -> Gc<Self::Out<'_>> {
        match self {
            true => {
                let sym: &Symbol = &crate::core::env::sym::TRUE;
                Gc::from_ptr(sym, Tag::Symbol)
            }
            false => {
                let sym: &Symbol = &crate::core::env::sym::NIL;
                Gc::from_ptr(sym, Tag::Symbol)
            }
        }
    }
}

impl IntoObject for Cons {
    type Out<'ob> = &'ob Cons;

    fn into_obj<const C: bool>(self, block: &Block<C>) -> Gc<Self::Out<'_>> {
        let ptr = self.alloc_obj(block);
        Gc::from_ptr(ptr, Tag::Cons)
    }

    unsafe fn from_obj_ptr<'ob>(ptr: *const u8) -> Self::Out<'ob> {
        &*Self::cast_ptr(ptr)
    }
}

impl IntoObject for LispFn {
    type Out<'ob> = &'ob LispFn;

    fn into_obj<const C: bool>(self, block: &Block<C>) -> Gc<Self::Out<'_>> {
        let ptr = self.alloc_obj(block);
        unsafe { Self::tag_ptr(ptr) }
    }

    unsafe fn from_obj_ptr<'ob>(ptr: *const u8) -> Self::Out<'ob> {
        &(*<LispFn as TaggedPtr>::cast_ptr(ptr)).data
    }
}

impl IntoObject for Symbol {
    type Out<'ob> = &'ob Symbol;

    fn into_obj<const C: bool>(self, block: &Block<C>) -> Gc<Self::Out<'_>> {
        let ptr = self.alloc_obj(block);
        Gc::from_ptr(ptr, Tag::Symbol)
    }

    unsafe fn from_obj_ptr<'ob>(ptr: *const u8) -> Self::Out<'ob> {
        &*Symbol::cast_ptr(ptr)
    }
}

impl IntoObject for &Symbol {
    type Out<'ob> = &'ob Symbol;

    fn into_obj<const C: bool>(self, _: &Block<C>) -> Gc<Self::Out<'_>> {
        Gc::from_ptr(self, Tag::Symbol)
    }

    unsafe fn from_obj_ptr<'ob>(ptr: *const u8) -> Self::Out<'ob> {
        &*Symbol::cast_ptr(ptr)
    }
}

impl IntoObject for ConstSymbol {
    type Out<'ob> = &'ob Symbol;

    fn into_obj<const C: bool>(self, _: &Block<C>) -> Gc<Self::Out<'_>> {
        let ptr: *const u8 = std::ptr::addr_of!(*self).cast();
        Gc::from_ptr(ptr, Tag::Symbol)
    }

    unsafe fn from_obj_ptr<'ob>(ptr: *const u8) -> Self::Out<'ob> {
        &*Symbol::cast_ptr(ptr)
    }
}

impl IntoObject for String {
    type Out<'ob> = &'ob String;

    fn into_obj<const C: bool>(self, block: &Block<C>) -> Gc<Self::Out<'_>> {
        let ptr = self.alloc_obj(block);
        unsafe { Self::tag_ptr(ptr) }
    }

    unsafe fn from_obj_ptr<'ob>(ptr: *const u8) -> Self::Out<'ob> {
        &(*Self::cast_ptr(ptr)).data
    }
}

impl IntoObject for &str {
    type Out<'ob> = &'ob String;

    fn into_obj<const C: bool>(self, block: &Block<C>) -> Gc<Self::Out<'_>> {
        let ptr = self.to_owned().alloc_obj(block);
        unsafe { String::tag_ptr(ptr) }
    }

    unsafe fn from_obj_ptr<'ob>(ptr: *const u8) -> Self::Out<'ob> {
        &(*String::cast_ptr(ptr)).data
    }
}

impl<'a> IntoObject for Vec<GcObj<'a>> {
    type Out<'ob> = &'ob LispVec;

    fn into_obj<const C: bool>(self, block: &Block<C>) -> Gc<Self::Out<'_>> {
        unsafe {
            let ptr = LispVec::new(self).alloc_obj(block);
            LispVec::tag_ptr(ptr)
        }
    }

    unsafe fn from_obj_ptr<'ob>(ptr: *const u8) -> Self::Out<'ob> {
        &(*<LispVec as TaggedPtr>::cast_ptr(ptr)).data
    }
}

impl<'a> IntoObject for RecordBuilder<'a> {
    type Out<'ob> = &'ob Record;

    fn into_obj<const C: bool>(self, block: &Block<C>) -> Gc<Self::Out<'_>> {
        unsafe {
            let ptr = LispVec::new(self.0).alloc_obj(block);
            Self::tag_ptr(ptr)
        }
    }

    unsafe fn from_obj_ptr<'ob>(ptr: *const u8) -> Self::Out<'ob> {
        let vec = &(*<Self as TaggedPtr>::cast_ptr(ptr)).data;
        &*(vec as *const LispVec).cast::<Record>()
    }
}

impl IntoObject for Vec<u8> {
    type Out<'ob> = &'ob RefCell<Vec<u8>>;

    fn into_obj<const C: bool>(self, block: &Block<C>) -> Gc<Self::Out<'_>> {
        let ptr = self.alloc_obj(block);
        unsafe { Self::tag_ptr(ptr) }
    }

    unsafe fn from_obj_ptr<'ob>(ptr: *const u8) -> Self::Out<'ob> {
        &(*Self::cast_ptr(ptr)).data
    }
}

impl<'a> IntoObject for HashTable<'a> {
    type Out<'ob> = &'ob LispHashTable;

    fn into_obj<const C: bool>(self, block: &Block<C>) -> Gc<Self::Out<'_>> {
        unsafe {
            let ptr = LispHashTable::new(self).alloc_obj(block);
            LispHashTable::tag_ptr(ptr)
        }
    }

    unsafe fn from_obj_ptr<'ob>(ptr: *const u8) -> Self::Out<'ob> {
        &(*<LispHashTable as TaggedPtr>::cast_ptr(ptr)).data
    }
}

// work around for no GAT's
#[allow(unused_lifetimes)]
trait TaggedPtr
where
    Self: Sized,
{
    type Ptr;
    type Output<'ob>;
    const TAG: Tag;
    unsafe fn tag_ptr<'ob>(ptr: *const Self::Ptr) -> Gc<Self::Output<'ob>> {
        Gc::from_ptr(ptr, Self::TAG)
    }
    fn cast_ptr(ptr: *const u8) -> *const Self::Ptr {
        ptr.cast()
    }
}

impl TaggedPtr for i64 {
    type Ptr = i64;
    type Output<'a> = i64;
    const TAG: Tag = Tag::Int;
}

impl TaggedPtr for f64 {
    type Ptr = <Self as AllocObject>::Output;
    type Output<'ob> = &'ob f64;
    const TAG: Tag = Tag::Float;
}

impl TaggedPtr for Cons {
    type Ptr = <Self as AllocObject>::Output;
    type Output<'ob> = &'ob Cons;
    const TAG: Tag = Tag::Cons;
}

impl TaggedPtr for SubrFn {
    type Ptr = Self;
    type Output<'ob> = &'ob SubrFn;
    const TAG: Tag = Tag::SubrFn;
}

impl TaggedPtr for Symbol {
    type Ptr = Symbol;
    type Output<'ob> = &'ob Symbol;
    const TAG: Tag = Tag::Symbol;
}

impl TaggedPtr for LispFn {
    type Ptr = <Self as AllocObject>::Output;
    type Output<'ob> = &'ob LispFn;
    const TAG: Tag = Tag::LispFn;
}

impl TaggedPtr for String {
    type Ptr = <Self as AllocObject>::Output;
    type Output<'ob> = &'ob String;
    const TAG: Tag = Tag::String;
}

impl TaggedPtr for LispVec {
    type Ptr = <Self as AllocObject>::Output;
    type Output<'ob> = &'ob LispVec;
    const TAG: Tag = Tag::Vec;
}

impl<'a> TaggedPtr for RecordBuilder<'a> {
    type Ptr = <LispVec as AllocObject>::Output;
    type Output<'ob> = &'ob Record;
    const TAG: Tag = Tag::Record;
}

impl TaggedPtr for Vec<u8> {
    type Ptr = <Self as AllocObject>::Output;
    type Output<'ob> = &'ob RefCell<Vec<u8>>;
    const TAG: Tag = Tag::ByteVec;
}

impl TaggedPtr for LispHashTable {
    type Ptr = <Self as AllocObject>::Output;
    type Output<'ob> = &'ob LispHashTable;
    const TAG: Tag = Tag::HashTable;
}

#[allow(clippy::multiple_inherent_impl)]
impl SubrFn {
    pub(in crate::core) unsafe fn gc_from_raw_ptr(ptr: *mut SubrFn) -> Gc<&'static SubrFn> {
        SubrFn::tag_ptr(ptr)
    }
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

impl List<'_> {
    pub(crate) fn empty() -> Gc<Self> {
        let sym: &Symbol = &crate::core::env::sym::NIL;
        Gc::from_ptr(sym, Tag::Symbol)
    }
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
            Tag::Symbol => List::Nil,
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

// Function
#[derive(Copy, Clone, Debug)]
pub(crate) enum Function<'ob> {
    LispFn(&'ob LispFn),
    SubrFn(&'static SubrFn),
    Cons(&'ob Cons),
    Symbol(&'ob Symbol),
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

impl<'ob> From<Gc<&'ob LispFn>> for Gc<Function<'ob>> {
    fn from(x: Gc<&'ob LispFn>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Gc<&'ob Symbol>> for Gc<Function<'ob>> {
    fn from(x: Gc<&Symbol>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<&'ob Symbol> for Gc<Function<'ob>> {
    fn from(x: &Symbol) -> Self {
        let ptr = x as *const Symbol;
        unsafe { Symbol::tag_ptr(ptr).into() }
    }
}

impl<'ob> From<&'ob Symbol> for Gc<&'ob Symbol> {
    fn from(x: &Symbol) -> Self {
        let ptr = x as *const Symbol;
        unsafe { Symbol::tag_ptr(ptr) }
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
            // SubrFn does not have IntoObject implementation, so we cast it directly
            Tag::SubrFn => Function::SubrFn(unsafe { &*ptr.cast::<SubrFn>() }),
            Tag::LispFn => Function::LispFn(unsafe { LispFn::from_obj_ptr(ptr) }),
            Tag::Symbol => Function::Symbol(unsafe { Symbol::from_obj_ptr(ptr) }),
            _ => unreachable!(),
        }
    }
}

#[allow(dead_code)]
#[derive(Copy, Clone)]
/// The Object defintion that contains all other possible lisp objects. This
/// type must remain covariant over 'ob. This is just an expanded form of our
/// tagged pointer type to take advantage of ergonomics of enums in Rust.
pub(crate) enum Object<'ob> {
    Int(i64),
    Float(&'ob f64),
    Symbol(&'ob Symbol),
    Cons(&'ob Cons),
    Vec(&'ob LispVec),
    Record(&'ob Record),
    ByteVec(&'ob RefCell<Vec<u8>>),
    HashTable(&'ob LispHashTable),
    String(&'ob String),
    LispFn(&'ob LispFn),
    SubrFn(&'static SubrFn),
}

impl Object<'_> {
    /// Return the type of an object
    pub(crate) fn get_type(self) -> Type {
        match self {
            Object::Int(_) => Type::Int,
            Object::Float(_) => Type::Float,
            Object::Symbol(_) => Type::Symbol,
            Object::Cons(_) => Type::Cons,
            Object::Vec(_) | Object::Record(_) => Type::Vec,
            Object::HashTable(_) => Type::HashTable,
            Object::ByteVec(_) | Object::String(_) => Type::String,
            Object::LispFn(_) | Object::SubrFn(_) => Type::Func,
        }
    }
}

impl PartialEq for Object<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Object::Int(l0), Object::Int(r0)) => l0 == r0,
            (Object::Float(l0), Object::Float(r0)) => l0 == r0,
            (Object::Symbol(l0), Object::Symbol(r0)) => l0 == r0,
            (Object::Cons(l0), Object::Cons(r0)) => l0 == r0,
            (Object::Vec(l0), Object::Vec(r0)) => *l0.borrow() == *r0.borrow(),
            (Object::String(l0), Object::String(r0)) => l0 == r0,
            (Object::LispFn(_), Object::LispFn(_)) => todo!(),
            (Object::SubrFn(l0), Object::SubrFn(r0)) => l0 == r0,
            (Object::Record(_), Object::Record(_)) => todo!(),
            (Object::ByteVec(_), Object::ByteVec(_)) => todo!(),
            (Object::HashTable(_), Object::HashTable(_)) => todo!(),
            _ => false,
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
            Tag::SubrFn => Object::SubrFn(unsafe { &*ptr.cast() }),
            Tag::LispFn => Object::LispFn(unsafe { LispFn::from_obj_ptr(ptr) }),
            Tag::Int => Object::Int(unsafe { i64::from_obj_ptr(ptr) }),
            Tag::Float => Object::Float(unsafe { f64::from_obj_ptr(ptr) }),
            Tag::String => Object::String(unsafe { String::from_obj_ptr(ptr) }),
            Tag::Vec => Object::Vec(unsafe { Vec::<GcObj>::from_obj_ptr(ptr) }),
            Tag::Record => Object::Record(unsafe { RecordBuilder::from_obj_ptr(ptr) }),
            Tag::HashTable => Object::HashTable(unsafe { HashTable::from_obj_ptr(ptr) }),
            Tag::ByteVec => Object::ByteVec(unsafe { Vec::<u8>::from_obj_ptr(ptr) }),
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

impl<'ob> From<usize> for Gc<Object<'ob>> {
    fn from(x: usize) -> Self {
        let ptr = sptr::invalid(x);
        unsafe { i64::tag_ptr(ptr).into() }
    }
}

impl<'ob> From<i32> for Gc<Object<'ob>> {
    fn from(x: i32) -> Self {
        let ptr = sptr::invalid(x as usize);
        unsafe { i64::tag_ptr(ptr).into() }
    }
}

impl<'ob> From<Gc<&'ob f64>> for Gc<Object<'ob>> {
    fn from(x: Gc<&'ob f64>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<&Symbol> for Gc<Object<'ob>> {
    fn from(x: &Symbol) -> Self {
        let ptr = x as *const Symbol;
        unsafe { Symbol::tag_ptr(ptr).into() }
    }
}

impl<'ob> From<ConstSymbol> for Gc<Object<'ob>> {
    fn from(x: ConstSymbol) -> Self {
        let sym: &Symbol = &x;
        sym.into()
    }
}

impl<'ob> From<&Cons> for Gc<Object<'ob>> {
    fn from(x: &Cons) -> Self {
        let ptr = x as *const Cons;
        unsafe { Cons::tag_ptr(ptr).into() }
    }
}

impl<'ob> From<Gc<&'ob Symbol>> for Gc<Object<'ob>> {
    fn from(x: Gc<&Symbol>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Gc<()>> for Gc<Object<'ob>> {
    fn from(x: Gc<()>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl From<Gc<Object<'_>>> for () {
    fn from(_: Gc<Object>) {}
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

impl<'ob> From<Gc<&'ob LispVec>> for Gc<Object<'ob>> {
    fn from(x: Gc<&'ob LispVec>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Gc<&'ob LispHashTable>> for Gc<Object<'ob>> {
    fn from(x: Gc<&'ob LispHashTable>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Gc<&'ob Record>> for Gc<Object<'ob>> {
    fn from(x: Gc<&'ob Record>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Gc<&'ob RefCell<Vec<u8>>>> for Gc<Object<'ob>> {
    fn from(x: Gc<&'ob RefCell<Vec<u8>>>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Gc<&'ob RefCell<HashTable<'ob>>>> for Gc<Object<'ob>> {
    fn from(x: Gc<&'ob RefCell<HashTable<'ob>>>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<Gc<&'ob LispFn>> for Gc<Object<'ob>> {
    fn from(x: Gc<&'ob LispFn>) -> Self {
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
    type Error = TypeError;

    fn try_from(value: Gc<Object<'ob>>) -> Result<Self, Self::Error> {
        match value.tag() {
            Tag::Int | Tag::Float => unsafe { Ok(Self::transmute(value)) },
            _ => Err(TypeError::new(Type::Number, value)),
        }
    }
}

impl<'ob> TryFrom<Gc<Object<'ob>>> for Option<Gc<Number<'ob>>> {
    type Error = TypeError;

    fn try_from(value: Gc<Object<'ob>>) -> Result<Self, Self::Error> {
        if value.nil() {
            Ok(None)
        } else {
            value.try_into().map(Some)
        }
    }
}

impl<'ob> From<Gc<List<'ob>>> for Gc<Object<'ob>> {
    fn from(x: Gc<List<'ob>>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> From<&Gc<List<'ob>>> for Gc<Object<'ob>> {
    fn from(x: &Gc<List<'ob>>) -> Self {
        unsafe { Self::transmute(*x) }
    }
}

impl<'ob> TryFrom<Gc<Object<'ob>>> for Gc<List<'ob>> {
    type Error = TypeError;

    fn try_from(value: Gc<Object<'ob>>) -> Result<Self, Self::Error> {
        match value.get() {
            Object::Symbol(s) if s.nil() => unsafe { Ok(Self::transmute(value)) },
            Object::Cons(_) => unsafe { Ok(Self::transmute(value)) },
            _ => Err(TypeError::new(Type::List, value)),
        }
    }
}

impl<'ob> From<Gc<Function<'ob>>> for Gc<Object<'ob>> {
    fn from(x: Gc<Function<'ob>>) -> Self {
        unsafe { Self::transmute(x) }
    }
}

impl<'ob> TryFrom<Gc<Function<'ob>>> for Gc<&'ob Cons> {
    type Error = TypeError;

    fn try_from(value: Gc<Function<'ob>>) -> Result<Self, Self::Error> {
        match value.get() {
            Function::Cons(_) => unsafe { Ok(Self::transmute(value)) },
            _ => Err(TypeError::new(Type::Cons, value)),
        }
    }
}

impl<'ob> TryFrom<Gc<Object<'ob>>> for Gc<&'ob Symbol> {
    type Error = TypeError;
    fn try_from(value: Gc<Object<'ob>>) -> Result<Self, Self::Error> {
        match value.get() {
            Object::Symbol(_) => unsafe { Ok(Self::transmute(value)) },
            _ => Err(TypeError::new(Type::Symbol, value)),
        }
    }
}

impl<'ob> TryFrom<Gc<Object<'ob>>> for Gc<Function<'ob>> {
    type Error = TypeError;

    fn try_from(value: Gc<Object<'ob>>) -> Result<Self, Self::Error> {
        match value.tag() {
            Tag::LispFn | Tag::SubrFn | Tag::Cons | Tag::Symbol => unsafe {
                Ok(Self::transmute(value))
            },
            _ => Err(TypeError::new(Type::Func, value)),
        }
    }
}

///////////////////////////
// Other implementations //
///////////////////////////

impl<'ob> TryFrom<Gc<Object<'ob>>> for Gc<i64> {
    type Error = TypeError;

    fn try_from(value: Gc<Object<'ob>>) -> Result<Self, Self::Error> {
        match value.tag() {
            Tag::Int => unsafe { Ok(Self::transmute(value)) },
            _ => Err(TypeError::new(Type::Int, value)),
        }
    }
}

// This function is needed due to the lack of specialization and there being a
// blanket impl for From<T> for Option<T>
impl<'ob> GcObj<'ob> {
    pub(crate) fn try_from_option<T, E>(value: GcObj<'ob>) -> Result<Option<T>, E>
    where
        GcObj<'ob>: TryInto<T, Error = E>,
    {
        if value.nil() {
            Ok(None)
        } else {
            Ok(Some(value.try_into()?))
        }
    }

    pub(crate) fn nil(self) -> bool {
        self == crate::core::env::sym::NIL
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

impl<'ob> Gc<&'ob Symbol> {
    pub(crate) fn get(self) -> &'ob Symbol {
        let (ptr, _) = self.untag();
        unsafe { Symbol::from_obj_ptr(ptr) }
    }
}

impl From<&Cons> for Gc<&Cons> {
    fn from(x: &Cons) -> Self {
        let ptr = x as *const Cons;
        unsafe { Cons::tag_ptr(ptr) }
    }
}

impl<'ob> TryFrom<GcObj<'ob>> for Gc<&'ob Cons> {
    type Error = TypeError;

    fn try_from(value: GcObj<'ob>) -> Result<Self, Self::Error> {
        match value.tag() {
            Tag::Cons => unsafe { Ok(Self::transmute(value)) },
            _ => Err(TypeError::new(Type::Cons, value)),
        }
    }
}

impl<'old, 'new> WithLifetime<'new> for Gc<&'old Cons> {
    type Out = Gc<&'new Cons>;

    unsafe fn with_lifetime(self) -> Self::Out {
        transmute(self)
    }
}

impl<'old, 'new> WithLifetime<'new> for &'old Symbol {
    type Out = &'new Symbol;

    unsafe fn with_lifetime(self) -> Self::Out {
        &*(self as *const Symbol)
    }
}

impl<'old, 'new> WithLifetime<'new> for Gc<&'old Symbol> {
    type Out = Gc<&'new Symbol>;
    unsafe fn with_lifetime(self) -> Self::Out {
        transmute(self)
    }
}

impl<'ob> TryFrom<GcObj<'ob>> for Gc<&'ob String> {
    type Error = TypeError;

    fn try_from(value: GcObj<'ob>) -> Result<Self, Self::Error> {
        match value.tag() {
            Tag::String => unsafe { Ok(Self::transmute(value)) },
            _ => Err(TypeError::new(Type::String, value)),
        }
    }
}

impl<'ob> TryFrom<GcObj<'ob>> for Gc<&'ob RefCell<HashTable<'ob>>> {
    type Error = TypeError;

    fn try_from(value: GcObj<'ob>) -> Result<Self, Self::Error> {
        match value.tag() {
            Tag::HashTable => unsafe { Ok(Self::transmute(value)) },
            _ => Err(TypeError::new(Type::HashTable, value)),
        }
    }
}

impl<'old, 'new> WithLifetime<'new> for Gc<&'old String> {
    type Out = Gc<&'new String>;

    unsafe fn with_lifetime(self) -> Self::Out {
        transmute(self)
    }
}

impl<'ob> Gc<&'ob String> {
    pub(crate) fn get(self) -> &'ob String {
        let (ptr, _) = self.untag();
        unsafe { String::from_obj_ptr(ptr) }
    }
}

impl<'ob> std::ops::Deref for Gc<&'ob Cons> {
    type Target = Cons;

    fn deref(&self) -> &'ob Self::Target {
        self.get()
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
            Object::Cons(x) => x.clone_in(bk),
            Object::String(x) => x.clone().into_obj(bk).into(),
            Object::Symbol(x) => x.into(),
            Object::LispFn(x) => x.clone_in(bk).into_obj(bk).into(),
            Object::SubrFn(x) => x.into(),
            Object::Float(x) => x.into_obj(bk).into(),
            Object::Vec(x) => vec_clone_in(&x.borrow(), bk).into_obj(bk).into(),
            Object::Record(x) => {
                let vec = vec_clone_in(&x.borrow(), bk);
                RecordBuilder(vec).into_obj(bk).into()
            }
            Object::ByteVec(x) => x.borrow().clone().into_obj(bk).into(),
            Object::HashTable(_) => todo!("implement clone for hashtable"),
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

impl<'ob> PartialEq<&Symbol> for Gc<Object<'ob>> {
    fn eq(&self, other: &&Symbol) -> bool {
        match self.get() {
            Object::Symbol(x) => x == *other,
            _ => false,
        }
    }
}

impl<'ob> PartialEq<Symbol> for Gc<Object<'ob>> {
    fn eq(&self, other: &Symbol) -> bool {
        match self.get() {
            Object::Symbol(x) => x == other,
            _ => false,
        }
    }
}

impl<'ob> PartialEq<ConstSymbol> for Gc<Object<'ob>> {
    fn eq(&self, other: &ConstSymbol) -> bool {
        match self.get() {
            Object::Symbol(x) => x == other,
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

impl<'ob> Gc<Object<'ob>> {
    pub(crate) fn as_cons(self) -> &'ob Cons {
        self.try_into().unwrap()
    }
}

impl Default for Gc<Object<'_>> {
    fn default() -> Self {
        nil()
    }
}

impl Default for Gc<List<'_>> {
    fn default() -> Self {
        List::empty()
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
        write!(f, "{obj:?}")
    }
}

impl<T> PartialEq for Gc<T> {
    fn eq(&self, other: &Self) -> bool {
        self.as_obj().get() == other.as_obj().get()
    }
}

impl<T> Eq for Gc<T> {}

use std::hash::{Hash, Hasher};
impl<T> Hash for Gc<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.ptr.hash(state);
    }
}

impl fmt::Display for Object<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Int(x) => write!(f, "{x}"),
            Object::Cons(x) => write!(f, "{x}"),
            Object::Vec(vec) => {
                write!(f, "[")?;
                for x in vec.borrow().iter() {
                    write!(f, "{x} ")?;
                }
                write!(f, "]")
            }
            Object::Record(vec) => {
                write!(f, "#s(")?;
                for x in vec.borrow().iter() {
                    write!(f, "{x} ")?;
                }
                write!(f, ")")
            }
            Object::ByteVec(vec) => {
                write!(f, "[")?;
                for x in vec.borrow().iter() {
                    write!(f, "{x} ")?;
                }
                write!(f, "]")
            }
            Object::HashTable(x) => write!(f, "{x:?}"),
            Object::String(x) => write!(f, "\"{x}\""),
            Object::Symbol(x) => write!(f, "{x}"),
            Object::LispFn(x) => write!(f, "(lambda {x:?})"),
            Object::SubrFn(x) => write!(f, "{x:?}"),
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
            Object::Record(x) => write!(f, "{x:?}"),
            Object::ByteVec(x) => write!(f, "{x:?}"),
            Object::HashTable(x) => write!(f, "{x:?}"),
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

enum ObjectAllocation<'ob> {
    Float(&'ob Allocation<f64>),
    Cons(&'ob Cons),
    Vec(&'ob Allocation<LispVec>),
    ByteVec(&'ob Allocation<RefCell<Vec<u8>>>),
    HashTable(&'ob Allocation<LispHashTable>),
    String(&'ob Allocation<String>),
    LispFn(&'ob Allocation<LispFn>),
    Symbol(&'ob Symbol),
    NonAllocated,
}

impl<'ob> Gc<Object<'ob>> {
    pub(crate) fn is_markable(self) -> bool {
        !matches!(self.get_alloc(), ObjectAllocation::NonAllocated)
    }

    #[rustfmt::skip]
    fn get_alloc(self) -> ObjectAllocation<'ob> {
        let (ptr, tag) = self.untag();
        match tag {
            Tag::LispFn => ObjectAllocation::LispFn(unsafe { &*LispFn::cast_ptr(ptr) }),
            Tag::Cons => ObjectAllocation::Cons(unsafe { &*Cons::cast_ptr(ptr) }),
            Tag::Float => ObjectAllocation::Float(unsafe { &*f64::cast_ptr(ptr) }),
            Tag::String => ObjectAllocation::String(unsafe { &*String::cast_ptr(ptr) }),
            Tag::Vec | Tag::Record => ObjectAllocation::Vec(unsafe { &*LispVec::cast_ptr(ptr) }),
            Tag::HashTable => ObjectAllocation::HashTable(unsafe { &*LispHashTable::cast_ptr(ptr) }),
            Tag::ByteVec => ObjectAllocation::ByteVec(unsafe { &*Vec::<u8>::cast_ptr(ptr) }),
            Tag::Symbol => ObjectAllocation::Symbol(unsafe { &*Symbol::cast_ptr(ptr) }),
            Tag::Int | Tag::SubrFn => ObjectAllocation::NonAllocated,
        }
    }

    pub(crate) fn is_marked(self) -> bool {
        match self.get_alloc() {
            ObjectAllocation::Float(x) => x.is_marked(),
            ObjectAllocation::Cons(x) => x.is_marked(),
            ObjectAllocation::Vec(x) => x.is_marked(),
            ObjectAllocation::ByteVec(x) => x.is_marked(),
            ObjectAllocation::HashTable(x) => x.is_marked(),
            ObjectAllocation::String(x) => x.is_marked(),
            ObjectAllocation::LispFn(x) => x.is_marked(),
            ObjectAllocation::Symbol(x) => x.is_marked(),
            ObjectAllocation::NonAllocated => true,
        }
    }

    pub(crate) fn trace_mark(self, stack: &mut Vec<RawObj>) {
        match self.get_alloc() {
            ObjectAllocation::Float(x) => x.mark(),
            ObjectAllocation::Vec(vec_alloc) => {
                let vec = vec_alloc.data.borrow();
                let unmarked = vec
                    .iter()
                    .filter_map(|x| x.is_markable().then(|| x.into_raw()));
                stack.extend(unmarked);
                vec_alloc.mark();
            }
            ObjectAllocation::ByteVec(vec) => vec.mark(),
            ObjectAllocation::HashTable(table_alloc) => {
                let table = table_alloc.data.borrow();
                for (k, v) in &*table {
                    if k.is_markable() {
                        stack.push(k.into_raw());
                    }
                    if v.is_markable() {
                        stack.push(v.into_raw());
                    }
                }
                table_alloc.mark();
            }
            ObjectAllocation::String(x) => x.mark(),
            ObjectAllocation::Cons(x) => x.mark(stack),
            ObjectAllocation::Symbol(x) => x.mark(stack),
            ObjectAllocation::LispFn(x) => {
                x.data.mark(stack);
                x.mark();
            }
            ObjectAllocation::NonAllocated => {}
        }
    }
}

impl<'ob> List<'ob> {
    #[cfg(test)]
    pub(crate) fn car(self) -> GcObj<'ob> {
        match self {
            List::Nil => nil(),
            List::Cons(x) => x.car(),
        }
    }
}
