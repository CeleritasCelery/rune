#![allow(unstable_name_collisions)]
use super::super::{
    cons::Cons,
    env::{Symbol, SymbolCell},
    error::{Type, TypeError},
    gc::{AllocObject, Block},
};
use super::{
    ByteFn, HashTable, LispFloat, LispHashTable, LispString, LispVec, Record, RecordBuilder, SubrFn,
};
use crate::core::env::sym;
use crate::core::gc::{GcManaged, Trace};
use private::{Tag, TaggedPtr};
use sptr::Strict;
use std::fmt;
use std::marker::PhantomData;

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
    sym::NIL.into()
}

pub(crate) fn qtrue<'a>() -> GcObj<'a> {
    sym::TRUE.into()
}

#[derive(Copy, Clone)]
pub(crate) struct Gc<T> {
    ptr: *const u8,
    _data: PhantomData<T>,
}

// TODO need to find a better way to handle this
unsafe impl<T> Send for Gc<T> {}

impl<T> Gc<T> {
    const fn new(ptr: *const u8) -> Self {
        Self {
            ptr,
            _data: PhantomData,
        }
    }

    fn from_ptr<U>(ptr: *const U, tag: Tag) -> Self {
        use std::mem::size_of;
        assert_eq!(size_of::<*const U>(), size_of::<*const ()>());
        let ptr = ptr.cast::<u8>().map_addr(|x| (x << 8) | tag as usize);
        Self::new(ptr)
    }

    fn untag_ptr(self) -> (*const u8, Tag) {
        let ptr = self.ptr.map_addr(|x| ((x as isize) >> 8) as usize);
        let tag = self.get_tag();
        (ptr, tag)
    }

    fn get_tag(self) -> Tag {
        unsafe { std::mem::transmute(self.ptr.addr() as u8) }
    }

    pub(crate) fn into_raw(self) -> RawObj {
        RawObj { ptr: self.ptr }
    }

    pub(in crate::core) fn into_ptr(self) -> *const u8 {
        self.ptr
    }

    pub(crate) unsafe fn from_raw(raw: RawObj) -> Self {
        Self::new(raw.ptr)
    }

    pub(crate) unsafe fn from_raw_ptr(raw: *mut u8) -> Self {
        Self::new(raw)
    }

    pub(crate) fn ptr_eq<U>(self, other: Gc<U>) -> bool {
        self.ptr == other.ptr
    }

    pub(crate) fn copy_as_obj<'ob>(self) -> Gc<Object<'ob>> {
        Gc::new(self.ptr)
    }

    pub(crate) fn as_obj(&self) -> Gc<Object<'_>> {
        Gc::new(self.ptr)
    }
}

impl<T: TaggedPtr> Gc<T> {
    pub(crate) fn untag(self) -> T {
        T::untag(self)
    }
}

pub(crate) trait Untag<T> {
    fn untag_erased(self) -> T;
}

impl<T: TaggedPtr> Untag<T> for Gc<T> {
    fn untag_erased(self) -> T {
        T::untag(self)
    }
}

pub(crate) trait TagType
where
    Self: Sized,
{
    type Out;
    fn tag(self) -> Gc<Self::Out>;
}

impl<T: TaggedPtr> TagType for T {
    type Out = Self;
    fn tag(self) -> Gc<Self> {
        self.tag()
    }
}

unsafe fn cast_gc<U, V>(e: Gc<U>) -> Gc<V> {
    Gc::new(e.ptr)
}

impl<'a, T: 'a + Copy> From<Gc<T>> for Object<'a> {
    fn from(x: Gc<T>) -> Self {
        x.copy_as_obj().untag()
    }
}

////////////////////////
// Traits for Objects //
////////////////////////

pub(crate) trait WithLifetime<'new> {
    type Out: 'new;
    unsafe fn with_lifetime(self) -> Self::Out;
}

impl<'new, T: WithLifetime<'new>> WithLifetime<'new> for Gc<T> {
    type Out = Gc<<T as WithLifetime<'new>>::Out>;

    unsafe fn with_lifetime(self) -> Self::Out {
        cast_gc(self)
    }
}

impl<'new, 'old, T: GcManaged + 'new> WithLifetime<'new> for &'old T {
    type Out = &'new T;

    unsafe fn with_lifetime(self) -> Self::Out {
        &*(self as *const T)
    }
}

pub(crate) trait FromRaw<'ob> {
    type Out: 'ob;
    unsafe fn from_raw(self) -> Self::Out;
}

impl<'ob> FromRaw<'ob> for RawObj {
    type Out = GcObj<'ob>;
    unsafe fn from_raw(self) -> GcObj<'ob> {
        GcObj::new(self.ptr)
    }
}

impl<'ob> FromRaw<'ob> for *const Cons {
    type Out = &'ob Cons;
    unsafe fn from_raw(self) -> &'ob Cons {
        &*self
    }
}

pub(crate) trait IntoObject {
    type Out<'ob>;

    fn into_obj<const C: bool>(self, block: &Block<C>) -> Gc<Self::Out<'_>>;
}

impl<T> IntoObject for Gc<T> {
    type Out<'ob> = Object<'ob>;

    fn into_obj<const C: bool>(self, _block: &Block<C>) -> Gc<Self::Out<'_>> {
        unsafe { cast_gc(self) }
    }
}

impl<T> IntoObject for Option<Gc<T>> {
    type Out<'ob> = Object<'ob>;

    fn into_obj<const C: bool>(self, _block: &Block<C>) -> Gc<Self::Out<'_>> {
        match self {
            Some(x) => unsafe { cast_gc(x) },
            None => nil(),
        }
    }
}

impl<T> IntoObject for T
where
    T: TagType,
{
    type Out<'ob> = <T as TagType>::Out;

    fn into_obj<const C: bool>(self, _block: &Block<C>) -> Gc<Self::Out<'_>> {
        self.tag()
    }
}

impl IntoObject for f64 {
    type Out<'ob> = &'ob LispFloat;

    fn into_obj<const C: bool>(self, block: &Block<C>) -> Gc<Self::Out<'_>> {
        let ptr = self.alloc_obj(block);
        unsafe { Self::Out::tag_ptr(ptr) }
    }
}

impl IntoObject for bool {
    type Out<'a> = Symbol<'a>;

    fn into_obj<const C: bool>(self, _: &Block<C>) -> Gc<Self::Out<'_>> {
        let sym = match self {
            true => sym::TRUE,
            false => sym::NIL,
        };
        unsafe { Self::Out::tag_ptr(sym.get_ptr()) }
    }
}

impl IntoObject for Cons {
    type Out<'ob> = &'ob Cons;

    fn into_obj<const C: bool>(self, block: &Block<C>) -> Gc<Self::Out<'_>> {
        let ptr = self.alloc_obj(block);
        unsafe { Self::Out::tag_ptr(ptr) }
    }
}

impl IntoObject for ByteFn {
    type Out<'ob> = &'ob ByteFn;

    fn into_obj<const C: bool>(self, block: &Block<C>) -> Gc<Self::Out<'_>> {
        let ptr = self.alloc_obj(block);
        unsafe { Self::Out::tag_ptr(ptr) }
    }
}

impl IntoObject for SymbolCell {
    type Out<'ob> = Symbol<'ob>;

    fn into_obj<const C: bool>(self, block: &Block<C>) -> Gc<Self::Out<'_>> {
        let ptr = self.alloc_obj(block);
        let sym = unsafe { Symbol::from_ptr(ptr) };
        unsafe { Self::Out::tag_ptr(sym.get_ptr()) }
    }
}

impl IntoObject for LispString {
    type Out<'ob> = &'ob Self;

    fn into_obj<const C: bool>(self, block: &Block<C>) -> Gc<Self::Out<'_>> {
        let ptr = self.alloc_obj(block);
        unsafe { Self::Out::tag_ptr(ptr) }
    }
}

impl IntoObject for String {
    type Out<'ob> = &'ob LispString;

    fn into_obj<const C: bool>(self, block: &Block<C>) -> Gc<Self::Out<'_>> {
        unsafe {
            let ptr = LispString::from_string(self).alloc_obj(block);
            Self::Out::tag_ptr(ptr)
        }
    }
}

impl IntoObject for &str {
    type Out<'ob> = <String as IntoObject>::Out<'ob>;

    fn into_obj<const C: bool>(self, block: &Block<C>) -> Gc<Self::Out<'_>> {
        unsafe {
            let ptr = LispString::from_string(self.to_owned()).alloc_obj(block);
            <&LispString>::tag_ptr(ptr)
        }
    }
}

impl IntoObject for Vec<u8> {
    type Out<'ob> = <String as IntoObject>::Out<'ob>;

    fn into_obj<const C: bool>(self, block: &Block<C>) -> Gc<Self::Out<'_>> {
        unsafe {
            let ptr = LispString::from_bstring(self).alloc_obj(block);
            <&LispString>::tag_ptr(ptr)
        }
    }
}

impl<'a> IntoObject for Vec<GcObj<'a>> {
    type Out<'ob> = &'ob LispVec;

    fn into_obj<const C: bool>(self, block: &Block<C>) -> Gc<Self::Out<'_>> {
        unsafe {
            let ptr = LispVec::new(self).alloc_obj(block);
            <&LispVec>::tag_ptr(ptr)
        }
    }
}

impl<'a> IntoObject for RecordBuilder<'a> {
    type Out<'ob> = &'ob Record;

    fn into_obj<const C: bool>(self, block: &Block<C>) -> Gc<Self::Out<'_>> {
        unsafe {
            let ptr = LispVec::new(self.0).alloc_obj(block);
            <&Record>::tag_ptr(ptr)
        }
    }
}

impl<'a> IntoObject for HashTable<'a> {
    type Out<'ob> = &'ob LispHashTable;

    fn into_obj<const C: bool>(self, block: &Block<C>) -> Gc<Self::Out<'_>> {
        unsafe {
            let ptr = LispHashTable::new(self).alloc_obj(block);
            <&LispHashTable>::tag_ptr(ptr)
        }
    }
}

mod private {
    use super::{Gc, WithLifetime};

    #[repr(u8)]
    pub(crate) enum Tag {
        Symbol,
        Int,
        Float,
        Cons,
        String,
        Vec,
        Record,
        HashTable,
        SubrFn,
        ByteFn,
    }

    pub(crate) trait TaggedPtr: Copy + for<'a> WithLifetime<'a> {
        type Ptr;
        const TAG: Tag;
        unsafe fn tag_ptr(ptr: *const Self::Ptr) -> Gc<Self> {
            Gc::from_ptr(ptr, Self::TAG)
        }

        fn untag(val: Gc<Self>) -> Self {
            let (ptr, _) = val.untag_ptr();
            unsafe { Self::from_obj_ptr(ptr) }
        }

        fn tag(self) -> Gc<Self> {
            unsafe { Self::tag_ptr(self.get_ptr()) }
        }

        fn get_ptr(self) -> *const Self::Ptr {
            unimplemented!()
        }

        unsafe fn from_obj_ptr(_: *const u8) -> Self {
            unimplemented!()
        }
    }
}

impl<'a> TaggedPtr for Object<'a> {
    type Ptr = Object<'a>;
    const TAG: Tag = Tag::Int;

    unsafe fn tag_ptr(_: *const Self::Ptr) -> Gc<Self> {
        unimplemented!()
    }
    fn untag(val: Gc<Self>) -> Self {
        let (ptr, tag) = val.untag_ptr();
        unsafe {
            match tag {
                Tag::Symbol => Object::Symbol(<Symbol>::from_obj_ptr(ptr)),
                Tag::Cons => Object::Cons(<&Cons>::from_obj_ptr(ptr)),
                Tag::SubrFn => Object::SubrFn(&*ptr.cast()),
                Tag::ByteFn => Object::ByteFn(<&ByteFn>::from_obj_ptr(ptr)),
                Tag::Int => Object::Int(i64::from_obj_ptr(ptr)),
                Tag::Float => Object::Float(<&LispFloat>::from_obj_ptr(ptr)),
                Tag::String => Object::String(<&LispString>::from_obj_ptr(ptr)),
                Tag::Vec => Object::Vec(<&LispVec>::from_obj_ptr(ptr)),
                Tag::Record => Object::Record(<&Record>::from_obj_ptr(ptr)),
                Tag::HashTable => Object::HashTable(<&LispHashTable>::from_obj_ptr(ptr)),
            }
        }
    }

    fn tag(self) -> Gc<Self> {
        match self {
            Object::Int(x) => TaggedPtr::tag(x).into(),
            Object::Float(x) => TaggedPtr::tag(x).into(),
            Object::Symbol(x) => TaggedPtr::tag(x).into(),
            Object::Cons(x) => TaggedPtr::tag(x).into(),
            Object::Vec(x) => TaggedPtr::tag(x).into(),
            Object::Record(x) => TaggedPtr::tag(x).into(),
            Object::HashTable(x) => TaggedPtr::tag(x).into(),
            Object::String(x) => TaggedPtr::tag(x).into(),
            Object::ByteFn(x) => TaggedPtr::tag(x).into(),
            Object::SubrFn(x) => TaggedPtr::tag(x).into(),
        }
    }
}

impl<'a> TaggedPtr for List<'a> {
    type Ptr = List<'a>;
    const TAG: Tag = Tag::Int;

    unsafe fn tag_ptr(_: *const Self::Ptr) -> Gc<Self> {
        unimplemented!()
    }

    fn untag(val: Gc<Self>) -> Self {
        let (ptr, tag) = val.untag_ptr();
        match tag {
            Tag::Symbol => List::Nil,
            Tag::Cons => List::Cons(unsafe { <&Cons>::from_obj_ptr(ptr) }),
            _ => unreachable!(),
        }
    }

    fn tag(self) -> Gc<Self> {
        match self {
            List::Nil => unsafe { cast_gc(TaggedPtr::tag(sym::NIL)) },
            List::Cons(x) => TaggedPtr::tag(x).into(),
        }
    }
}

impl<'a> TaggedPtr for Function<'a> {
    type Ptr = Function<'a>;
    const TAG: Tag = Tag::Int;

    unsafe fn tag_ptr(_: *const Self::Ptr) -> Gc<Self> {
        unimplemented!()
    }

    fn untag(val: Gc<Self>) -> Self {
        let (ptr, tag) = val.untag_ptr();
        unsafe {
            match tag {
                Tag::Cons => Function::Cons(<&Cons>::from_obj_ptr(ptr)),
                // SubrFn does not have IntoObject implementation, so we cast it directly
                Tag::SubrFn => Function::SubrFn(&*ptr.cast::<SubrFn>()),
                Tag::ByteFn => Function::ByteFn(<&ByteFn>::from_obj_ptr(ptr)),
                Tag::Symbol => Function::Symbol(<Symbol>::from_obj_ptr(ptr)),
                _ => unreachable!(),
            }
        }
    }

    fn tag(self) -> Gc<Self> {
        match self {
            Function::Cons(x) => TaggedPtr::tag(x).into(),
            Function::SubrFn(x) => TaggedPtr::tag(x).into(),
            Function::ByteFn(x) => TaggedPtr::tag(x).into(),
            Function::Symbol(x) => TaggedPtr::tag(x).into(),
        }
    }
}

impl<'a> TaggedPtr for Number<'a> {
    type Ptr = Number<'a>;
    const TAG: Tag = Tag::Int;

    unsafe fn tag_ptr(_: *const Self::Ptr) -> Gc<Self> {
        unimplemented!()
    }

    fn untag(val: Gc<Self>) -> Self {
        let (ptr, tag) = val.untag_ptr();
        unsafe {
            match tag {
                Tag::Int => Number::Int(i64::from_obj_ptr(ptr)),
                Tag::Float => Number::Float(<&LispFloat>::from_obj_ptr(ptr)),
                _ => unreachable!(),
            }
        }
    }

    fn tag(self) -> Gc<Self> {
        match self {
            Number::Int(x) => TaggedPtr::tag(x).into(),
            Number::Float(x) => TaggedPtr::tag(x).into(),
        }
    }
}

impl TaggedPtr for i64 {
    type Ptr = i64;
    const TAG: Tag = Tag::Int;

    unsafe fn from_obj_ptr(ptr: *const u8) -> Self {
        ptr.addr() as i64
    }

    fn get_ptr(self) -> *const Self::Ptr {
        sptr::invalid(self as usize)
    }
}

impl TaggedPtr for &LispFloat {
    type Ptr = LispFloat;
    const TAG: Tag = Tag::Float;
    unsafe fn from_obj_ptr(ptr: *const u8) -> Self {
        &*ptr.cast::<Self::Ptr>()
    }

    fn get_ptr(self) -> *const Self::Ptr {
        self as *const Self::Ptr
    }
}

impl TaggedPtr for &Cons {
    type Ptr = Cons;
    const TAG: Tag = Tag::Cons;
    unsafe fn from_obj_ptr(ptr: *const u8) -> Self {
        &*ptr.cast::<Self::Ptr>()
    }

    fn get_ptr(self) -> *const Self::Ptr {
        self as *const Self::Ptr
    }
}

impl TaggedPtr for &SubrFn {
    type Ptr = SubrFn;
    const TAG: Tag = Tag::SubrFn;
    unsafe fn from_obj_ptr(ptr: *const u8) -> Self {
        &*ptr.cast::<Self::Ptr>()
    }

    fn get_ptr(self) -> *const Self::Ptr {
        self as *const Self::Ptr
    }
}

impl TaggedPtr for Symbol<'_> {
    type Ptr = u8;
    const TAG: Tag = Tag::Symbol;

    unsafe fn tag_ptr(ptr: *const Self::Ptr) -> Gc<Self> {
        Gc::from_ptr(ptr, Self::TAG)
    }

    unsafe fn from_obj_ptr(ptr: *const u8) -> Self {
        Symbol::from_offset_ptr(ptr)
    }

    fn get_ptr(self) -> *const Self::Ptr {
        self.as_ptr()
    }
}

impl TaggedPtr for &ByteFn {
    type Ptr = ByteFn;
    const TAG: Tag = Tag::ByteFn;
    unsafe fn from_obj_ptr(ptr: *const u8) -> Self {
        &*ptr.cast::<Self::Ptr>()
    }

    fn get_ptr(self) -> *const Self::Ptr {
        self as *const Self::Ptr
    }
}

impl TaggedPtr for &LispString {
    type Ptr = LispString;
    const TAG: Tag = Tag::String;
    unsafe fn from_obj_ptr(ptr: *const u8) -> Self {
        &*ptr.cast::<Self::Ptr>()
    }

    fn get_ptr(self) -> *const Self::Ptr {
        self as *const Self::Ptr
    }
}

impl TaggedPtr for &LispVec {
    type Ptr = LispVec;
    const TAG: Tag = Tag::Vec;
    unsafe fn from_obj_ptr(ptr: *const u8) -> Self {
        &*ptr.cast::<Self::Ptr>()
    }

    fn get_ptr(self) -> *const Self::Ptr {
        self as *const Self::Ptr
    }
}

impl TaggedPtr for &Record {
    type Ptr = LispVec;
    const TAG: Tag = Tag::Record;
    unsafe fn from_obj_ptr(ptr: *const u8) -> Self {
        &*ptr.cast::<Record>()
    }

    fn get_ptr(self) -> *const Self::Ptr {
        (self as *const Record).cast::<Self::Ptr>()
    }
}

impl TaggedPtr for &LispHashTable {
    type Ptr = LispHashTable;
    const TAG: Tag = Tag::HashTable;
    unsafe fn from_obj_ptr(ptr: *const u8) -> Self {
        &*ptr.cast::<Self::Ptr>()
    }

    fn get_ptr(self) -> *const Self::Ptr {
        self as *const Self::Ptr
    }
}

macro_rules! cast_gc {
    ($supertype:ty => $($subtype:ty),+ $(,)?) => {
        $(
            impl<'ob> From<Gc<$subtype>> for Gc<$supertype> {
                fn from(x: Gc<$subtype>) -> Self {
                    unsafe { cast_gc(x) }
                }
            }

            impl<'ob> From<$subtype> for Gc<$supertype> {
                fn from(x: $subtype) -> Self {
                    unsafe { <$subtype>::tag_ptr(x.get_ptr()).into() }
                }
            }
        )+
    };
}

////////////////////////
// Proc macro section //
////////////////////////

// Number
#[derive(Copy, Clone)]
#[repr(u8)]
pub(crate) enum Number<'ob> {
    Int(i64) = Tag::Int as u8,
    Float(&'ob LispFloat) = Tag::Float as u8,
}
cast_gc!(Number<'ob> => i64, &LispFloat);

impl<'old, 'new> WithLifetime<'new> for Number<'old> {
    type Out = Number<'new>;

    unsafe fn with_lifetime(self) -> Self::Out {
        std::mem::transmute::<Number<'old>, Number<'new>>(self)
    }
}

// List
#[derive(Copy, Clone, Debug)]
#[repr(u8)]
pub(crate) enum List<'ob> {
    Nil = 0,
    Cons(&'ob Cons) = Tag::Cons as u8,
}
cast_gc!(List<'ob> => &'ob Cons);

impl List<'_> {
    pub(crate) fn empty() -> Gc<Self> {
        let gc: GcObj = sym::NIL.into();
        unsafe { cast_gc(gc) }
    }
}

impl<'old, 'new> WithLifetime<'new> for List<'old> {
    type Out = List<'new>;

    unsafe fn with_lifetime(self) -> Self::Out {
        std::mem::transmute::<List<'old>, List<'new>>(self)
    }
}

// Function
#[derive(Copy, Clone, Debug)]
#[repr(u8)]
pub(crate) enum Function<'ob> {
    ByteFn(&'ob ByteFn) = Tag::ByteFn as u8,
    SubrFn(&'static SubrFn) = Tag::SubrFn as u8,
    Cons(&'ob Cons) = Tag::Cons as u8,
    Symbol(Symbol<'ob>) = Tag::Symbol as u8,
}
cast_gc!(Function<'ob> => &'ob ByteFn, &'ob SubrFn, &'ob Cons, Symbol<'ob>);

impl<'old, 'new> WithLifetime<'new> for Function<'old> {
    type Out = Function<'new>;

    unsafe fn with_lifetime(self) -> Self::Out {
        std::mem::transmute::<Function<'old>, Function<'new>>(self)
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
            Function::ByteFn(x) => {
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

#[allow(dead_code)]
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(u8)]
/// The Object defintion that contains all other possible lisp objects. This
/// type must remain covariant over 'ob. This is just an expanded form of our
/// tagged pointer type to take advantage of ergonomics of enums in Rust.
pub(crate) enum Object<'ob> {
    Int(i64) = Tag::Int as u8,
    Float(&'ob LispFloat) = Tag::Float as u8,
    Symbol(Symbol<'ob>) = Tag::Symbol as u8,
    Cons(&'ob Cons) = Tag::Cons as u8,
    Vec(&'ob LispVec) = Tag::Vec as u8,
    Record(&'ob Record) = Tag::Record as u8,
    HashTable(&'ob LispHashTable) = Tag::HashTable as u8,
    String(&'ob LispString) = Tag::String as u8,
    ByteFn(&'ob ByteFn) = Tag::ByteFn as u8,
    SubrFn(&'static SubrFn) = Tag::SubrFn as u8,
}
cast_gc!(Object<'ob> => Number<'ob>, List<'ob>, Function<'ob>, i64, Symbol<'_>, &LispFloat, &'ob Cons, &'ob LispVec, &'ob Record, &'ob LispHashTable, &'ob LispString, &'ob ByteFn, &'ob SubrFn);

impl Object<'_> {
    pub(crate) const NIL: Object<'static> = Object::Symbol(sym::NIL);
    pub(crate) const TRUE: Object<'static> = Object::Symbol(sym::TRUE);
    /// Return the type of an object
    pub(crate) fn get_type(self) -> Type {
        match self {
            Object::Int(_) => Type::Int,
            Object::Float(_) => Type::Float,
            Object::Symbol(_) => Type::Symbol,
            Object::Cons(_) => Type::Cons,
            Object::Vec(_) => Type::Vec,
            Object::Record(_) => Type::Record,
            Object::HashTable(_) => Type::HashTable,
            Object::String(_) => Type::String,
            Object::ByteFn(_) | Object::SubrFn(_) => Type::Func,
        }
    }
}

// Object Impl's

impl<'old, 'new> WithLifetime<'new> for Object<'old> {
    type Out = Object<'new>;

    unsafe fn with_lifetime(self) -> Self::Out {
        std::mem::transmute::<Object<'old>, Object<'new>>(self)
    }
}

impl<'new> WithLifetime<'new> for i64 {
    type Out = i64;

    unsafe fn with_lifetime(self) -> Self::Out {
        self
    }
}

impl<'ob> From<usize> for Gc<Object<'ob>> {
    fn from(x: usize) -> Self {
        let ptr = sptr::invalid(x);
        unsafe { i64::tag_ptr(ptr).into() }
    }
}

impl TagType for usize {
    type Out = i64;
    fn tag(self) -> Gc<Self::Out> {
        TagType::tag(self as i64)
    }
}

impl TagType for i32 {
    type Out = i64;
    fn tag(self) -> Gc<Self::Out> {
        TagType::tag(i64::from(self))
    }
}

impl<'ob> From<i32> for Gc<Object<'ob>> {
    fn from(x: i32) -> Self {
        let ptr = sptr::invalid(x as usize);
        unsafe { i64::tag_ptr(ptr).into() }
    }
}

impl From<Gc<Object<'_>>> for () {
    fn from(_: Gc<Object>) {}
}

impl<'ob> TryFrom<Gc<Object<'ob>>> for Gc<Number<'ob>> {
    type Error = TypeError;

    fn try_from(value: Gc<Object<'ob>>) -> Result<Self, Self::Error> {
        match value.get_tag() {
            Tag::Int | Tag::Float => unsafe { Ok(cast_gc(value)) },
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

impl<'ob> TryFrom<Gc<Object<'ob>>> for Gc<List<'ob>> {
    type Error = TypeError;

    fn try_from(value: Gc<Object<'ob>>) -> Result<Self, Self::Error> {
        match value.untag() {
            Object::NIL | Object::Cons(_) => unsafe { Ok(cast_gc(value)) },
            _ => Err(TypeError::new(Type::List, value)),
        }
    }
}

impl<'ob> TryFrom<Gc<Function<'ob>>> for Gc<&'ob Cons> {
    type Error = TypeError;

    fn try_from(value: Gc<Function<'ob>>) -> Result<Self, Self::Error> {
        match value.untag() {
            Function::Cons(_) => unsafe { Ok(cast_gc(value)) },
            _ => Err(TypeError::new(Type::Cons, value)),
        }
    }
}

impl<'ob> TryFrom<Gc<Object<'ob>>> for Gc<Symbol<'ob>> {
    type Error = TypeError;
    fn try_from(value: Gc<Object<'ob>>) -> Result<Self, Self::Error> {
        match value.untag() {
            Object::Symbol(_) => unsafe { Ok(cast_gc(value)) },
            _ => Err(TypeError::new(Type::Symbol, value)),
        }
    }
}

impl<'ob> TryFrom<Gc<Object<'ob>>> for Gc<Function<'ob>> {
    type Error = TypeError;

    fn try_from(value: Gc<Object<'ob>>) -> Result<Self, Self::Error> {
        match value.get_tag() {
            Tag::ByteFn | Tag::SubrFn | Tag::Cons | Tag::Symbol => unsafe { Ok(cast_gc(value)) },
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
        match value.get_tag() {
            Tag::Int => unsafe { Ok(cast_gc(value)) },
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
        self == sym::NIL
    }
}

impl<'ob> TryFrom<GcObj<'ob>> for Gc<&'ob Cons> {
    type Error = TypeError;

    fn try_from(value: GcObj<'ob>) -> Result<Self, Self::Error> {
        match value.get_tag() {
            Tag::Cons => unsafe { Ok(cast_gc(value)) },
            _ => Err(TypeError::new(Type::Cons, value)),
        }
    }
}

impl<'ob> TryFrom<GcObj<'ob>> for Gc<&'ob LispString> {
    type Error = TypeError;

    fn try_from(value: GcObj<'ob>) -> Result<Self, Self::Error> {
        match value.get_tag() {
            Tag::String => unsafe { Ok(cast_gc(value)) },
            _ => Err(TypeError::new(Type::String, value)),
        }
    }
}

impl<'ob> TryFrom<GcObj<'ob>> for Gc<&'ob LispHashTable> {
    type Error = TypeError;

    fn try_from(value: GcObj<'ob>) -> Result<Self, Self::Error> {
        match value.get_tag() {
            Tag::HashTable => unsafe { Ok(cast_gc(value)) },
            _ => Err(TypeError::new(Type::HashTable, value)),
        }
    }
}

impl<'ob> TryFrom<GcObj<'ob>> for Gc<&'ob LispVec> {
    type Error = TypeError;

    fn try_from(value: GcObj<'ob>) -> Result<Self, Self::Error> {
        match value.get_tag() {
            Tag::Vec => unsafe { Ok(cast_gc(value)) },
            _ => Err(TypeError::new(Type::Vec, value)),
        }
    }
}

impl<'ob> std::ops::Deref for Gc<&'ob Cons> {
    type Target = Cons;

    fn deref(&self) -> &'ob Self::Target {
        self.untag()
    }
}

pub(crate) trait CloneIn<'new, T>
where
    T: 'new,
{
    fn clone_in<const C: bool>(&self, bk: &'new Block<C>) -> Gc<T>;
}

impl<'new, T, U, E> CloneIn<'new, U> for Gc<T>
where
    // The WithLifetime bound ensures that T is the same type as U
    T: WithLifetime<'new, Out = U>,
    Gc<U>: TryFrom<Gc<Object<'new>>, Error = E> + 'new,
{
    fn clone_in<const C: bool>(&self, bk: &'new Block<C>) -> Gc<U> {
        let obj = match self.as_obj().untag() {
            Object::Int(x) => x.into(),
            Object::Cons(x) => x.clone_in(bk).into(),
            Object::String(x) => x.clone_in(bk).into(),
            Object::Symbol(x) => x.clone_in(bk).into(),
            Object::ByteFn(x) => x.clone_in(bk).into(),
            Object::SubrFn(x) => x.into(),
            Object::Float(x) => x.into_obj(bk).into(),
            Object::Vec(x) => x.clone_in(bk).into(),
            Object::Record(x) => x.clone_in(bk).into(),
            Object::HashTable(x) => x.clone_in(bk).into(),
        };
        let Ok(x) = Gc::<U>::try_from(obj) else {unreachable!()};
        x
    }
}

impl<'ob> PartialEq<&str> for Gc<Object<'ob>> {
    fn eq(&self, other: &&str) -> bool {
        match self.untag() {
            Object::String(x) => **x == other.as_bytes(),
            _ => false,
        }
    }
}

impl<'ob> PartialEq<Symbol<'_>> for Gc<Object<'ob>> {
    fn eq(&self, other: &Symbol) -> bool {
        match self.untag() {
            Object::Symbol(x) => x == *other,
            _ => false,
        }
    }
}

impl<'ob> PartialEq<f64> for Gc<Object<'ob>> {
    fn eq(&self, other: &f64) -> bool {
        use float_cmp::ApproxEq;
        match self.untag() {
            Object::Float(x) => x.approx_eq(*other, (f64::EPSILON, 2)),
            _ => false,
        }
    }
}

impl<'ob> PartialEq<i64> for Gc<Object<'ob>> {
    fn eq(&self, other: &i64) -> bool {
        match self.untag() {
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
        let obj = self.as_obj().untag();
        write!(f, "{obj}")
    }
}

impl<T: fmt::Debug> fmt::Debug for Gc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let obj = self.as_obj().untag();
        write!(f, "{obj:?}")
    }
}

impl<T> PartialEq for Gc<T> {
    fn eq(&self, other: &Self) -> bool {
        self.as_obj().untag() == other.as_obj().untag()
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
        use fmt::Display as D;
        match self {
            Object::Int(x) => D::fmt(x, f),
            Object::Cons(x) => D::fmt(x, f),
            Object::Vec(x) => D::fmt(x, f),
            Object::Record(x) => D::fmt(x, f),
            Object::HashTable(x) => D::fmt(x, f),
            Object::String(x) => D::fmt(x, f),
            Object::Symbol(x) => D::fmt(x, f),
            Object::ByteFn(x) => D::fmt(x, f),
            Object::SubrFn(x) => D::fmt(x, f),
            Object::Float(x) => D::fmt(x, f),
        }
    }
}

impl<'ob> Gc<Object<'ob>> {
    pub(crate) fn is_markable(self) -> bool {
        !matches!(self.untag(), Object::Int(_) | Object::SubrFn(_))
    }

    pub(crate) fn is_marked(self) -> bool {
        match self.untag() {
            Object::Int(_) | Object::SubrFn(_) => true,
            Object::Float(x) => x.is_marked(),
            Object::Cons(x) => x.is_marked(),
            Object::Vec(x) => x.is_marked(),
            Object::Record(x) => x.is_marked(),
            Object::HashTable(x) => x.is_marked(),
            Object::String(x) => x.is_marked(),
            Object::ByteFn(x) => x.is_marked(),
            Object::Symbol(x) => x.is_marked(),
        }
    }

    pub(crate) fn trace_mark(self, stack: &mut Vec<RawObj>) {
        match self.untag() {
            Object::Int(_) | Object::SubrFn(_) => {}
            Object::Float(x) => x.mark(),
            Object::String(x) => x.mark(),
            Object::Vec(vec) => vec.trace(stack),
            Object::Record(x) => x.trace(stack),
            Object::HashTable(x) => x.trace(stack),
            Object::Cons(x) => x.trace(stack),
            Object::Symbol(x) => x.trace(stack),
            Object::ByteFn(x) => x.trace(stack),
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
