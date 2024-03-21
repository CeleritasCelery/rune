#![allow(unstable_name_collisions)]
use super::{
    super::{
        cons::Cons,
        error::{Type, TypeError},
        gc::Block,
    },
    ByteFnPrototype, ByteString, LispBuffer,
};
use super::{
    ByteFn, HashTable, LispFloat, LispHashTable, LispString, LispVec, Record, RecordBuilder,
    SubrFn, Symbol, SymbolCell,
};
use crate::core::{
    env::sym,
    gc::{DropStackElem, GcState, Markable, Trace},
};
use bumpalo::collections::String as GcString;
use bumpalo::collections::Vec as GcVec;
use private::{Tag, TaggedPtr};
use rune_core::hashmap::HashSet;
use sptr::Strict;
use std::marker::PhantomData;
use std::{fmt, ptr::NonNull};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub(crate) struct RawObj {
    ptr: *const u8,
}

unsafe impl Send for RawObj {}

impl Default for RawObj {
    fn default() -> Self {
        Self { ptr: NIL.ptr }
    }
}

/// A `nil` object.
///
/// The build.rs file guarantees that that `nil` is the first symbol in
/// `BUILTIN_SYMBOLS`, so we know it will always be 0.
pub(crate) const NIL: Object<'static> = unsafe { std::mem::transmute(0u64) };

/// A `t` object.
///
/// The build.rs file guarantees that that `t` is the second symbol in
/// `BUILTIN_SYMBOLS`, so we can rely on its value being constant.
pub(crate) const TRUE: Object<'static> =
    // offset from 0 by size of SymbolCell and then shift 8 to account for
    // tagging
    unsafe { std::mem::transmute(std::mem::size_of::<SymbolCell>() << 8) };

/// This type has two meanings, it is both a value that is tagged as well as
/// something that is managed by the GC. It is intended to be pointer sized, and
/// have a lifetime tied to the context which manages garbage collections. A Gc
/// can be reinterpreted as any type that shares the same tag.
#[derive(Copy, Clone)]
pub(crate) struct Gc<T> {
    ptr: *const u8,
    _data: PhantomData<T>,
}

// TODO need to find a better way to handle this
unsafe impl<T> Send for Gc<T> {}

impl<T> Gc<T> {
    const fn new(ptr: *const u8) -> Self {
        Self { ptr, _data: PhantomData }
    }

    unsafe fn from_ptr<U>(ptr: *const U, tag: Tag) -> Self {
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

    pub(crate) fn copy_as_obj<const C: bool>(self, _: &Block<C>) -> Object {
        Gc::new(self.ptr)
    }

    pub(crate) fn as_obj(&self) -> Object<'_> {
        Gc::new(self.ptr)
    }
}

/// The [TaggedPtr] trait is local to this module (by design). This trait
/// exports the one pubic method we want (untag) so it can be used in other
/// modules.
pub(crate) trait Untag<T> {
    fn untag_erased(self) -> T;
}

impl<T: TaggedPtr> Untag<T> for Gc<T> {
    fn untag_erased(self) -> T {
        T::untag(self)
    }
}

impl<T> Gc<T>
where
    Gc<T>: Untag<T>,
{
    /// A non-trait version of [Untag::untag_erased]. This is useful when we
    /// don't want to import the trait all over the place. The only time we need
    /// to import the trait is in generic code.
    pub(crate) fn untag(self) -> T {
        Self::untag_erased(self)
    }
}

/// A wrapper trait to expose the `tag` method for GC managed references and
/// immediate values. This is convenient when we don't have access to the
/// `Context` but want to retag a value. Doesn't currently have a lot of use.
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

impl<'a, T: 'a + Copy> From<Gc<T>> for ObjectType<'a> {
    fn from(x: Gc<T>) -> Self {
        Gc::new(x.ptr).untag()
    }
}

impl<'a, T: 'a + Copy> From<&Gc<T>> for ObjectType<'a> {
    fn from(x: &Gc<T>) -> Self {
        Gc::new(x.ptr).untag()
    }
}

////////////////////////
// Traits for Objects //
////////////////////////

/// Helper trait to change the lifetime of a Gc mangaged type. This is useful
/// because objects are initially tied to the lifetime of the
/// [Context](crate::core::gc::Context) they are allocated in. But when rooted
/// the lifetime is dissociated from the Context. If we only worked with
/// references, we could just use transmutes or casts to handle this, but
/// generic types don't expose their lifetimes. This trait is used to work
/// around that. Must be used with extreme care, as it is easy to cast it to an
/// invalid lifetime.
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

impl<'new, T, const N: usize> WithLifetime<'new> for [T; N]
where
    T: WithLifetime<'new>,
{
    type Out = [<T as WithLifetime<'new>>::Out; N];
    unsafe fn with_lifetime(self) -> Self::Out {
        // work around since we can't transmute arrays
        let ptr = &self as *const [T; N] as *const Self::Out;
        let value = unsafe { ptr.read() };
        std::mem::forget(self);
        value
    }
}

impl<'new, T> WithLifetime<'new> for Vec<T>
where
    T: WithLifetime<'new>,
{
    type Out = Vec<<T as WithLifetime<'new>>::Out>;

    unsafe fn with_lifetime(self) -> Self::Out {
        std::mem::transmute(self)
    }
}

impl<'new, T> WithLifetime<'new> for std::collections::VecDeque<T>
where
    T: WithLifetime<'new>,
{
    type Out = std::collections::VecDeque<<T as WithLifetime<'new>>::Out>;

    unsafe fn with_lifetime(self) -> Self::Out {
        std::mem::transmute(self)
    }
}

impl<'new, T> WithLifetime<'new> for Option<T>
where
    T: WithLifetime<'new>,
{
    type Out = Option<<T as WithLifetime<'new>>::Out>;

    unsafe fn with_lifetime(self) -> Self::Out {
        self.map(|x| x.with_lifetime())
    }
}

impl<'new, T, U> WithLifetime<'new> for (T, U)
where
    T: WithLifetime<'new>,
    U: WithLifetime<'new>,
{
    type Out = (<T as WithLifetime<'new>>::Out, <U as WithLifetime<'new>>::Out);

    unsafe fn with_lifetime(self) -> Self::Out {
        (self.0.with_lifetime(), self.1.with_lifetime())
    }
}

macro_rules! object_trait_impls {
    ($ty:ty) => {
        impl<'old, 'new> WithLifetime<'new> for &'old $ty {
            type Out = &'new $ty;

            unsafe fn with_lifetime(self) -> Self::Out {
                std::mem::transmute(self)
            }
        }
        impl GcPtr for &$ty {}
    };
}

pub(in crate::core) trait GcPtr {}
impl<T> GcPtr for Gc<T> {}
impl GcPtr for Symbol<'_> {}

object_trait_impls!(LispFloat);
object_trait_impls!(Cons);
object_trait_impls!(ByteFn);
object_trait_impls!(LispString);
object_trait_impls!(ByteString);
object_trait_impls!(LispVec);
object_trait_impls!(Record);
object_trait_impls!(LispHashTable);
object_trait_impls!(LispBuffer);

/// Trait for types that can be managed by the GC. This trait is implemented for
/// as many types as possible, even for types that are already Gc managed, Like
/// `Gc<T>`. This makes it easier to write generic code for working with Gc types.
pub(crate) trait IntoObject {
    type Out<'ob>;

    fn into_obj<const C: bool>(self, block: &Block<C>) -> Gc<Self::Out<'_>>;
}

impl<T> IntoObject for Gc<T> {
    type Out<'ob> = ObjectType<'ob>;

    fn into_obj<const C: bool>(self, _block: &Block<C>) -> Gc<Self::Out<'_>> {
        unsafe { cast_gc(self) }
    }
}

impl<T> IntoObject for Option<T>
where
    T: IntoObject,
{
    type Out<'ob> = ObjectType<'ob>;

    fn into_obj<const C: bool>(self, block: &Block<C>) -> Gc<Self::Out<'_>> {
        match self {
            Some(x) => x.into_obj(block).copy_as_obj(block),
            None => NIL,
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
        let ptr = block.objects.alloc(LispFloat::new(self, C));
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

impl IntoObject for () {
    type Out<'a> = Symbol<'a>;

    fn into_obj<const C: bool>(self, _: &Block<C>) -> Gc<Self::Out<'_>> {
        unsafe { Self::Out::tag_ptr(sym::NIL.get_ptr()) }
    }
}

impl IntoObject for Cons {
    type Out<'ob> = &'ob Cons;

    fn into_obj<const C: bool>(self, block: &Block<C>) -> Gc<Self::Out<'_>> {
        let ptr = block.objects.alloc(self);
        if C {
            ptr.mark_const();
        }
        unsafe { Self::Out::tag_ptr(ptr) }
    }
}

impl IntoObject for ByteFnPrototype {
    type Out<'ob> = &'ob ByteFn;

    fn into_obj<const C: bool>(self, block: &Block<C>) -> Gc<Self::Out<'_>> {
        let ptr = block.objects.alloc(ByteFn::new(self, C));
        unsafe { Self::Out::tag_ptr(ptr) }
    }
}

impl IntoObject for SymbolCell {
    type Out<'ob> = Symbol<'ob>;

    fn into_obj<const C: bool>(self, block: &Block<C>) -> Gc<Self::Out<'_>> {
        let ptr = block.objects.alloc(self);
        let sym = unsafe { Symbol::from_ptr(ptr) };
        unsafe { Self::Out::tag_ptr(sym.get_ptr()) }
    }
}

impl IntoObject for String {
    type Out<'ob> = &'ob LispString;

    fn into_obj<const C: bool>(self, block: &Block<C>) -> Gc<Self::Out<'_>> {
        unsafe {
            let ptr = self.as_str() as *const str;
            block.drop_stack.borrow_mut().push(DropStackElem::String(self));
            let ptr = block.objects.alloc(LispString::new(ptr, C));
            Self::Out::tag_ptr(ptr)
        }
    }
}

impl IntoObject for GcString<'_> {
    type Out<'ob> = <String as IntoObject>::Out<'ob>;

    fn into_obj<const C: bool>(self, block: &Block<C>) -> Gc<Self::Out<'_>> {
        unsafe {
            let ptr = block.objects.alloc(LispString::new(self.into_bump_str(), C));
            Self::Out::tag_ptr(ptr)
        }
    }
}

impl IntoObject for &str {
    type Out<'ob> = <String as IntoObject>::Out<'ob>;

    fn into_obj<const C: bool>(self, block: &Block<C>) -> Gc<Self::Out<'_>> {
        GcString::from_str_in(self, &block.objects).into_obj(block)
    }
}

impl IntoObject for Vec<u8> {
    type Out<'ob> = &'ob ByteString;

    fn into_obj<const C: bool>(self, block: &Block<C>) -> Gc<Self::Out<'_>> {
        unsafe {
            let ptr = block.objects.alloc(ByteString::new(self, C));
            <&ByteString>::tag_ptr(ptr)
        }
    }
}

impl<'a> IntoObject for Vec<Object<'a>> {
    type Out<'ob> = &'ob LispVec;

    fn into_obj<const C: bool>(mut self, block: &Block<C>) -> Gc<Self::Out<'_>> {
        unsafe {
            // having the reference implicity cast a ptr triggers UB
            let ptr = self.as_mut_slice() as *mut [Object];
            block.drop_stack.borrow_mut().push(DropStackElem::Vec(self.with_lifetime()));
            let ptr = block.objects.alloc(LispVec::new(ptr, C));
            <&LispVec>::tag_ptr(ptr)
        }
    }
}

impl<'a> IntoObject for GcVec<'_, Object<'a>> {
    type Out<'ob> = &'ob LispVec;

    fn into_obj<const C: bool>(self, block: &Block<C>) -> Gc<Self::Out<'_>> {
        unsafe {
            // having the reference implicity cast a ptr triggers UB
            let ptr = self.into_bump_slice_mut() as *mut [Object];
            let ptr = block.objects.alloc(LispVec::new(ptr, C));
            <&LispVec>::tag_ptr(ptr)
        }
    }
}

impl<'a> IntoObject for &[Object<'a>] {
    type Out<'ob> = &'ob LispVec;

    fn into_obj<const C: bool>(self, block: &Block<C>) -> Gc<Self::Out<'_>> {
        let mut vec = GcVec::with_capacity_in(self.len(), &block.objects);
        vec.extend_from_slice(self);
        vec.into_obj(block)
    }
}

impl<'a> IntoObject for RecordBuilder<'a> {
    type Out<'ob> = &'ob Record;

    fn into_obj<const C: bool>(self, block: &Block<C>) -> Gc<Self::Out<'_>> {
        unsafe {
            // record is the same layout as lispvec, just a different newtype wrapper
            let ptr = self.0.into_bump_slice_mut() as *mut [Object];
            let ptr = block.objects.alloc(LispVec::new(ptr, C));
            <&Record>::tag_ptr(ptr)
        }
    }
}

impl<'a> IntoObject for HashTable<'a> {
    type Out<'ob> = &'ob LispHashTable;

    fn into_obj<const C: bool>(self, block: &Block<C>) -> Gc<Self::Out<'_>> {
        unsafe {
            let ptr = block.objects.alloc(LispHashTable::new(self, C));
            <&LispHashTable>::tag_ptr(ptr)
        }
    }
}

mod private {
    use super::{Gc, WithLifetime};

    #[repr(u8)]
    pub(crate) enum Tag {
        // Symbol must be 0 to enable nil to be all zeroes
        Symbol = 0,
        Int,
        Float,
        Cons,
        String,
        ByteString,
        Vec,
        Record,
        HashTable,
        SubrFn,
        ByteFn,
        Buffer,
    }

    /// Trait for tagged pointers. Anything that can be stored and passed around
    /// by the lisp machine should implement this trait. There are two "flavors"
    /// of types that implement this trait. First is "base" types, which are
    /// pointers to some memory managed by the GC with a unique tag (e.g
    /// `LispString`, `LispVec`). This may seem stange that we would define a
    /// tagged pointer when the type is known (i.e. `Gc<i64>`), but doing so let's
    /// us reinterpret bits without changing the underlying memory. Base types
    /// are untagged into a pointer.
    ///
    /// The second type of implementor is "sum" types which can represent more
    /// then 1 base types (e.g `Object`, `List`). Sum types are untagged into an
    /// enum. this let's us easily match against them to operate on the possible
    /// value. Multiple sum types are defined (instead of just a single `Object`
    /// type) to allow the rust code to be more precise in what values are
    /// allowed.
    ///
    /// The tagging scheme uses the bottom byte of the `Gc` to represent the
    /// tag, meaning that we have 256 possible values. The data is shifted left
    /// by 8 bits, meaning that are fixnums are limited to 56 bits. This scheme
    /// has the advantage that it is easy to get the tag (just read the byte)
    /// and it maps nicely onto rusts enums. This method needs to be benchmarked
    /// and could change in the future.
    ///
    /// Every method has a default implementation, and the doc string
    /// indicates if it should be reimplemented or left untouched.
    pub(super) trait TaggedPtr: Copy + for<'a> WithLifetime<'a> {
        /// The type of object being pointed to. This will be different for all
        /// implementors.
        type Ptr;
        /// Tag value. This is only applicable to base values. Use Int for sum
        /// types.
        const TAG: Tag;
        /// Given a pointer to `Ptr` return a Tagged pointer.
        ///
        /// Base: default
        /// Sum: implement
        unsafe fn tag_ptr(ptr: *const Self::Ptr) -> Gc<Self> {
            Gc::from_ptr(ptr, Self::TAG)
        }

        /// Remove the tag from the `Gc<T>` and return the inner type. If it is
        /// base type then it will only have a single possible value and can be
        /// untagged without checks, but sum types need to create all values
        /// they can hold. We use tagged base types to let us reinterpret bits
        /// without actually modify them.
        ///
        /// Base: default
        /// Sum: implement
        fn untag(val: Gc<Self>) -> Self {
            let (ptr, _) = val.untag_ptr();
            unsafe { Self::from_obj_ptr(ptr) }
        }

        /// Given the type, return a tagged version of it. When using a sum type
        /// or an immediate value like i64, we override this method to set the
        /// proper tag.
        ///
        /// Base: default
        /// Sum: implement
        fn tag(self) -> Gc<Self> {
            unsafe { Self::tag_ptr(self.get_ptr()) }
        }

        /// Get the underlying pointer.
        ///
        /// Base: implement
        /// Sum: default
        fn get_ptr(self) -> *const Self::Ptr {
            unimplemented!()
        }

        /// Given an untyped pointer, reinterpret to self.
        ///
        /// Base: implement
        /// Sum: default
        unsafe fn from_obj_ptr(_: *const u8) -> Self {
            unimplemented!()
        }
    }
}

impl<'a> TaggedPtr for ObjectType<'a> {
    type Ptr = ObjectType<'a>;
    const TAG: Tag = Tag::Int;

    unsafe fn tag_ptr(_: *const Self::Ptr) -> Gc<Self> {
        unimplemented!()
    }
    fn untag(val: Gc<Self>) -> Self {
        let (ptr, tag) = val.untag_ptr();
        unsafe {
            match tag {
                Tag::Symbol => ObjectType::Symbol(<Symbol>::from_obj_ptr(ptr)),
                Tag::Cons => ObjectType::Cons(<&Cons>::from_obj_ptr(ptr)),
                Tag::SubrFn => ObjectType::SubrFn(&*ptr.cast()),
                Tag::ByteFn => ObjectType::ByteFn(<&ByteFn>::from_obj_ptr(ptr)),
                Tag::Int => ObjectType::Int(i64::from_obj_ptr(ptr)),
                Tag::Float => ObjectType::Float(<&LispFloat>::from_obj_ptr(ptr)),
                Tag::String => ObjectType::String(<&LispString>::from_obj_ptr(ptr)),
                Tag::ByteString => ObjectType::ByteString(<&ByteString>::from_obj_ptr(ptr)),
                Tag::Vec => ObjectType::Vec(<&LispVec>::from_obj_ptr(ptr)),
                Tag::Record => ObjectType::Record(<&Record>::from_obj_ptr(ptr)),
                Tag::HashTable => ObjectType::HashTable(<&LispHashTable>::from_obj_ptr(ptr)),
                Tag::Buffer => ObjectType::Buffer(<&LispBuffer>::from_obj_ptr(ptr)),
            }
        }
    }

    fn tag(self) -> Gc<Self> {
        match self {
            ObjectType::Int(x) => TaggedPtr::tag(x).into(),
            ObjectType::Float(x) => TaggedPtr::tag(x).into(),
            ObjectType::Symbol(x) => TaggedPtr::tag(x).into(),
            ObjectType::Cons(x) => TaggedPtr::tag(x).into(),
            ObjectType::Vec(x) => TaggedPtr::tag(x).into(),
            ObjectType::Record(x) => TaggedPtr::tag(x).into(),
            ObjectType::HashTable(x) => TaggedPtr::tag(x).into(),
            ObjectType::String(x) => TaggedPtr::tag(x).into(),
            ObjectType::ByteString(x) => TaggedPtr::tag(x).into(),
            ObjectType::ByteFn(x) => TaggedPtr::tag(x).into(),
            ObjectType::SubrFn(x) => TaggedPtr::tag(x).into(),
            ObjectType::Buffer(x) => TaggedPtr::tag(x).into(),
        }
    }
}

impl<'a> TaggedPtr for ListType<'a> {
    type Ptr = ListType<'a>;
    const TAG: Tag = Tag::Int;

    unsafe fn tag_ptr(_: *const Self::Ptr) -> Gc<Self> {
        unimplemented!()
    }

    fn untag(val: Gc<Self>) -> Self {
        let (ptr, tag) = val.untag_ptr();
        match tag {
            Tag::Symbol => ListType::Nil,
            Tag::Cons => ListType::Cons(unsafe { <&Cons>::from_obj_ptr(ptr) }),
            _ => unreachable!(),
        }
    }

    fn tag(self) -> Gc<Self> {
        match self {
            ListType::Nil => unsafe { cast_gc(TaggedPtr::tag(sym::NIL)) },
            ListType::Cons(x) => TaggedPtr::tag(x).into(),
        }
    }
}

impl<'a> TaggedPtr for FunctionType<'a> {
    type Ptr = FunctionType<'a>;
    const TAG: Tag = Tag::Int;

    unsafe fn tag_ptr(_: *const Self::Ptr) -> Gc<Self> {
        unimplemented!()
    }

    fn untag(val: Gc<Self>) -> Self {
        let (ptr, tag) = val.untag_ptr();
        unsafe {
            match tag {
                Tag::Cons => FunctionType::Cons(<&Cons>::from_obj_ptr(ptr)),
                // SubrFn does not have IntoObject implementation, so we cast it directly
                Tag::SubrFn => FunctionType::SubrFn(&*ptr.cast::<SubrFn>()),
                Tag::ByteFn => FunctionType::ByteFn(<&ByteFn>::from_obj_ptr(ptr)),
                Tag::Symbol => FunctionType::Symbol(<Symbol>::from_obj_ptr(ptr)),
                _ => unreachable!(),
            }
        }
    }

    fn tag(self) -> Gc<Self> {
        match self {
            FunctionType::Cons(x) => TaggedPtr::tag(x).into(),
            FunctionType::SubrFn(x) => TaggedPtr::tag(x).into(),
            FunctionType::ByteFn(x) => TaggedPtr::tag(x).into(),
            FunctionType::Symbol(x) => TaggedPtr::tag(x).into(),
        }
    }
}

impl<'a> TaggedPtr for NumberType<'a> {
    type Ptr = NumberType<'a>;
    const TAG: Tag = Tag::Int;

    unsafe fn tag_ptr(_: *const Self::Ptr) -> Gc<Self> {
        unimplemented!()
    }

    fn untag(val: Gc<Self>) -> Self {
        let (ptr, tag) = val.untag_ptr();
        unsafe {
            match tag {
                Tag::Int => NumberType::Int(i64::from_obj_ptr(ptr)),
                Tag::Float => NumberType::Float(<&LispFloat>::from_obj_ptr(ptr)),
                _ => unreachable!(),
            }
        }
    }

    fn tag(self) -> Gc<Self> {
        match self {
            NumberType::Int(x) => TaggedPtr::tag(x).into(),
            NumberType::Float(x) => TaggedPtr::tag(x).into(),
        }
    }
}

const MAX_FIXNUM: i64 = i64::MAX >> 8;
const MIN_FIXNUM: i64 = i64::MIN >> 8;

impl TaggedPtr for i64 {
    type Ptr = i64;
    const TAG: Tag = Tag::Int;

    unsafe fn from_obj_ptr(ptr: *const u8) -> Self {
        ptr.addr() as i64
    }

    fn get_ptr(self) -> *const Self::Ptr {
        // prevent wrapping
        let value = self.clamp(MIN_FIXNUM, MAX_FIXNUM);
        sptr::invalid(value as usize)
    }
}

pub(crate) fn int_to_char(int: i64) -> Result<char, TypeError> {
    let err = TypeError::new(Type::Char, TagType::tag(int));
    match u32::try_from(int) {
        Ok(x) => match char::from_u32(x) {
            Some(c) => Ok(c),
            None => Err(err),
        },
        Err(_) => Err(err),
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

impl TaggedPtr for &ByteString {
    type Ptr = ByteString;
    const TAG: Tag = Tag::ByteString;
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

impl TaggedPtr for &LispBuffer {
    type Ptr = LispBuffer;
    const TAG: Tag = Tag::Buffer;
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
/// The enum form of [Number] to take advantage of ergonomics of enums in Rust.
pub(crate) enum NumberType<'ob> {
    Int(i64) = Tag::Int as u8,
    Float(&'ob LispFloat) = Tag::Float as u8,
}
cast_gc!(NumberType<'ob> => i64, &LispFloat);

/// Represents a tagged pointer to a number value
pub(crate) type Number<'ob> = Gc<NumberType<'ob>>;

impl<'old, 'new> WithLifetime<'new> for NumberType<'old> {
    type Out = NumberType<'new>;

    unsafe fn with_lifetime(self) -> Self::Out {
        std::mem::transmute::<NumberType<'old>, NumberType<'new>>(self)
    }
}

// List
#[derive(Copy, Clone)]
#[repr(u8)]
/// The enum form of [List] to take advantage of ergonomics of enums in Rust.
pub(crate) enum ListType<'ob> {
    // Since Tag::Symbol is 0 and sym::NIL is 0, we can use 0 as the nil value
    Nil = Tag::Symbol as u8,
    Cons(&'ob Cons) = Tag::Cons as u8,
}
cast_gc!(ListType<'ob> => &'ob Cons);

/// Represents a tagged pointer to a list value (cons or nil)
pub(crate) type List<'ob> = Gc<ListType<'ob>>;

impl ListType<'_> {
    pub(crate) fn empty() -> Gc<Self> {
        unsafe { cast_gc(NIL) }
    }
}

impl<'old, 'new> WithLifetime<'new> for ListType<'old> {
    type Out = ListType<'new>;

    unsafe fn with_lifetime(self) -> Self::Out {
        std::mem::transmute::<ListType<'old>, ListType<'new>>(self)
    }
}

// Function
#[derive(Copy, Clone, Debug)]
#[repr(u8)]
/// The enum form of [Function] to take advantage of ergonomics of enums in Rust.
pub(crate) enum FunctionType<'ob> {
    ByteFn(&'ob ByteFn) = Tag::ByteFn as u8,
    SubrFn(&'static SubrFn) = Tag::SubrFn as u8,
    Cons(&'ob Cons) = Tag::Cons as u8,
    Symbol(Symbol<'ob>) = Tag::Symbol as u8,
}
cast_gc!(FunctionType<'ob> => &'ob ByteFn, &'ob SubrFn, &'ob Cons, Symbol<'ob>);

/// Represents a tagged pointer to a lisp object that could be interpreted as a
/// function. Note that not all `Function` types are valid functions (it could
/// be a cons cell for example).
pub(crate) type Function<'ob> = Gc<FunctionType<'ob>>;

impl<'old, 'new> WithLifetime<'new> for FunctionType<'old> {
    type Out = FunctionType<'new>;

    unsafe fn with_lifetime(self) -> Self::Out {
        std::mem::transmute::<FunctionType<'old>, FunctionType<'new>>(self)
    }
}

#[cfg(miri)]
extern "Rust" {
    fn miri_static_root(ptr: *const u8);
}

#[cfg(miri)]
impl<'ob> FunctionType<'ob> {
    pub(crate) fn set_as_miri_root(self) {
        // TODO: semispace fix
        // match self {
        //     FunctionType::ByteFn(x) => {
        //         let ptr: *const _ = x;
        //         unsafe {
        //             miri_static_root(ptr as _);
        //         }
        //     }
        //     FunctionType::SubrFn(x) => {
        //         let ptr: *const _ = x;
        //         unsafe {
        //             miri_static_root(ptr as _);
        //         }
        //     }
        //     FunctionType::Cons(x) => {
        //         let ptr: *const _ = x;
        //         unsafe {
        //             miri_static_root(ptr as _);
        //         }
        //     }
        //     FunctionType::Symbol(_) => {}
        // }
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
#[repr(u8)]
/// The enum form of [Object] to take advantage of ergonomics of enums in Rust.
pub(crate) enum ObjectType<'ob> {
    Int(i64) = Tag::Int as u8,
    Float(&'ob LispFloat) = Tag::Float as u8,
    Symbol(Symbol<'ob>) = Tag::Symbol as u8,
    Cons(&'ob Cons) = Tag::Cons as u8,
    Vec(&'ob LispVec) = Tag::Vec as u8,
    Record(&'ob Record) = Tag::Record as u8,
    HashTable(&'ob LispHashTable) = Tag::HashTable as u8,
    String(&'ob LispString) = Tag::String as u8,
    ByteString(&'ob ByteString) = Tag::ByteString as u8,
    ByteFn(&'ob ByteFn) = Tag::ByteFn as u8,
    SubrFn(&'static SubrFn) = Tag::SubrFn as u8,
    Buffer(&'static LispBuffer) = Tag::Buffer as u8,
}

/// The Object defintion that contains all other possible lisp objects. This
/// type must remain covariant over 'ob.
pub(crate) type Object<'ob> = Gc<ObjectType<'ob>>;

cast_gc!(ObjectType<'ob> => NumberType<'ob>,
         ListType<'ob>,
         FunctionType<'ob>,
         i64,
         Symbol<'_>,
         &'ob LispFloat,
         &'ob Cons,
         &'ob LispVec,
         &'ob Record,
         &'ob LispHashTable,
         &'ob LispString,
         &'ob ByteString,
         &'ob ByteFn,
         &'ob SubrFn,
         &'ob LispBuffer
);

impl ObjectType<'_> {
    pub(crate) const NIL: ObjectType<'static> = ObjectType::Symbol(sym::NIL);
    pub(crate) const TRUE: ObjectType<'static> = ObjectType::Symbol(sym::TRUE);
    /// Return the type of an object
    pub(crate) fn get_type(self) -> Type {
        match self {
            ObjectType::Int(_) => Type::Int,
            ObjectType::Float(_) => Type::Float,
            ObjectType::Symbol(_) => Type::Symbol,
            ObjectType::Cons(_) => Type::Cons,
            ObjectType::Vec(_) => Type::Vec,
            ObjectType::Record(_) => Type::Record,
            ObjectType::HashTable(_) => Type::HashTable,
            ObjectType::String(_) => Type::String,
            ObjectType::ByteString(_) => Type::String,
            ObjectType::ByteFn(_) | ObjectType::SubrFn(_) => Type::Func,
            ObjectType::Buffer(_) => Type::Buffer,
        }
    }
}

// Object Impl's

impl<'old, 'new> WithLifetime<'new> for ObjectType<'old> {
    type Out = ObjectType<'new>;

    unsafe fn with_lifetime(self) -> Self::Out {
        std::mem::transmute::<ObjectType<'old>, ObjectType<'new>>(self)
    }
}

impl<'new> WithLifetime<'new> for i64 {
    type Out = i64;

    unsafe fn with_lifetime(self) -> Self::Out {
        self
    }
}

impl<'ob> From<usize> for Object<'ob> {
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

impl TagType for u32 {
    type Out = i64;
    fn tag(self) -> Gc<Self::Out> {
        TagType::tag(i64::from(self))
    }
}

impl TagType for char {
    type Out = i64;
    fn tag(self) -> Gc<Self::Out> {
        TagType::tag(i64::from(self as u32))
    }
}

impl TagType for u64 {
    type Out = i64;
    fn tag(self) -> Gc<Self::Out> {
        TagType::tag(self as i64)
    }
}

impl TagType for u16 {
    type Out = i64;
    fn tag(self) -> Gc<Self::Out> {
        TagType::tag(i64::from(self))
    }
}

impl<'ob> From<i32> for Object<'ob> {
    fn from(x: i32) -> Self {
        i64::from(x).into()
    }
}

impl From<Object<'_>> for () {
    fn from(_: Object) {}
}

impl<'ob> TryFrom<Object<'ob>> for Number<'ob> {
    type Error = TypeError;

    fn try_from(value: Object<'ob>) -> Result<Self, Self::Error> {
        match value.get_tag() {
            Tag::Int | Tag::Float => unsafe { Ok(cast_gc(value)) },
            _ => Err(TypeError::new(Type::Number, value)),
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for Option<Number<'ob>> {
    type Error = TypeError;

    fn try_from(value: Object<'ob>) -> Result<Self, Self::Error> {
        if value.is_nil() {
            Ok(None)
        } else {
            value.try_into().map(Some)
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for List<'ob> {
    type Error = TypeError;

    fn try_from(value: Object<'ob>) -> Result<Self, Self::Error> {
        match value.untag() {
            ObjectType::NIL | ObjectType::Cons(_) => unsafe { Ok(cast_gc(value)) },
            _ => Err(TypeError::new(Type::List, value)),
        }
    }
}

impl<'ob> TryFrom<Function<'ob>> for Gc<&'ob Cons> {
    type Error = TypeError;

    fn try_from(value: Function<'ob>) -> Result<Self, Self::Error> {
        match value.untag() {
            FunctionType::Cons(_) => unsafe { Ok(cast_gc(value)) },
            _ => Err(TypeError::new(Type::Cons, value)),
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for Gc<Symbol<'ob>> {
    type Error = TypeError;
    fn try_from(value: Object<'ob>) -> Result<Self, Self::Error> {
        match value.untag() {
            ObjectType::Symbol(_) => unsafe { Ok(cast_gc(value)) },
            _ => Err(TypeError::new(Type::Symbol, value)),
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for Function<'ob> {
    type Error = TypeError;

    fn try_from(value: Object<'ob>) -> Result<Self, Self::Error> {
        match value.get_tag() {
            Tag::ByteFn | Tag::SubrFn | Tag::Cons | Tag::Symbol => unsafe { Ok(cast_gc(value)) },
            _ => Err(TypeError::new(Type::Func, value)),
        }
    }
}

///////////////////////////
// Other implementations //
///////////////////////////

impl<'ob> TryFrom<Object<'ob>> for Gc<i64> {
    type Error = TypeError;

    fn try_from(value: Object<'ob>) -> Result<Self, Self::Error> {
        match value.get_tag() {
            Tag::Int => unsafe { Ok(cast_gc(value)) },
            _ => Err(TypeError::new(Type::Int, value)),
        }
    }
}

// This function is needed due to the lack of specialization and there being a
// blanket impl for From<T> for Option<T>
impl<'ob> Object<'ob> {
    pub(crate) fn try_from_option<T, E>(value: Object<'ob>) -> Result<Option<T>, E>
    where
        Object<'ob>: TryInto<T, Error = E>,
    {
        if value.is_nil() {
            Ok(None)
        } else {
            Ok(Some(value.try_into()?))
        }
    }

    pub(crate) fn is_nil(self) -> bool {
        self == sym::NIL
    }
}

impl<'ob> TryFrom<Object<'ob>> for Gc<&'ob Cons> {
    type Error = TypeError;

    fn try_from(value: Object<'ob>) -> Result<Self, Self::Error> {
        match value.get_tag() {
            Tag::Cons => unsafe { Ok(cast_gc(value)) },
            _ => Err(TypeError::new(Type::Cons, value)),
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for Gc<&'ob LispString> {
    type Error = TypeError;

    fn try_from(value: Object<'ob>) -> Result<Self, Self::Error> {
        match value.get_tag() {
            Tag::String => unsafe { Ok(cast_gc(value)) },
            _ => Err(TypeError::new(Type::String, value)),
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for Gc<&'ob ByteString> {
    type Error = TypeError;

    fn try_from(value: Object<'ob>) -> Result<Self, Self::Error> {
        match value.get_tag() {
            Tag::ByteString => unsafe { Ok(cast_gc(value)) },
            _ => Err(TypeError::new(Type::String, value)),
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for Gc<&'ob LispHashTable> {
    type Error = TypeError;

    fn try_from(value: Object<'ob>) -> Result<Self, Self::Error> {
        match value.get_tag() {
            Tag::HashTable => unsafe { Ok(cast_gc(value)) },
            _ => Err(TypeError::new(Type::HashTable, value)),
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for Gc<&'ob LispVec> {
    type Error = TypeError;

    fn try_from(value: Object<'ob>) -> Result<Self, Self::Error> {
        match value.get_tag() {
            Tag::Vec => unsafe { Ok(cast_gc(value)) },
            _ => Err(TypeError::new(Type::Vec, value)),
        }
    }
}

impl<'ob> TryFrom<Object<'ob>> for Gc<&'ob LispBuffer> {
    type Error = TypeError;

    fn try_from(value: Object<'ob>) -> Result<Self, Self::Error> {
        match value.get_tag() {
            Tag::Buffer => unsafe { Ok(cast_gc(value)) },
            _ => Err(TypeError::new(Type::Buffer, value)),
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
    Gc<U>: TryFrom<Object<'new>, Error = E> + 'new,
{
    fn clone_in<const C: bool>(&self, bk: &'new Block<C>) -> Gc<U> {
        let obj = match self.as_obj().untag() {
            ObjectType::Int(x) => x.into(),
            ObjectType::Cons(x) => x.clone_in(bk).into(),
            ObjectType::String(x) => x.clone_in(bk).into(),
            ObjectType::ByteString(x) => x.clone_in(bk).into(),
            ObjectType::Symbol(x) => x.clone_in(bk).into(),
            ObjectType::ByteFn(x) => x.clone_in(bk).into(),
            ObjectType::SubrFn(x) => x.into(),
            ObjectType::Float(x) => x.clone_in(bk).into(),
            ObjectType::Vec(x) => x.clone_in(bk).into(),
            ObjectType::Record(x) => x.clone_in(bk).into(),
            ObjectType::HashTable(x) => x.clone_in(bk).into(),
            ObjectType::Buffer(x) => x.clone_in(bk).into(),
        };
        let Ok(x) = Gc::<U>::try_from(obj) else { unreachable!() };
        x
    }
}

impl<T> Trace for Gc<T> {
    fn trace(&self, state: &mut GcState) {
        match self.as_obj().untag() {
            ObjectType::Int(_) | ObjectType::SubrFn(_) => {}
            ObjectType::Float(x) => x.trace(state),
            ObjectType::String(x) => x.trace(state),
            ObjectType::ByteString(x) => x.trace(state),
            ObjectType::Vec(vec) => vec.trace(state),
            ObjectType::Record(x) => x.trace(state),
            ObjectType::HashTable(x) => x.trace(state),
            ObjectType::Cons(x) => x.trace(state),
            ObjectType::Symbol(x) => x.trace(state),
            ObjectType::ByteFn(x) => x.trace(state),
            ObjectType::Buffer(x) => x.trace(state),
        }
    }
}

impl<T> Markable for Gc<T>
where
    Self: Untag<T> + Copy,
    T: Markable<Value = T> + TagType<Out = T>,
{
    type Value = Self;

    fn is_marked(&self) -> bool {
        todo!()
    }

    fn move_value(&self, to_space: &bumpalo::Bump) -> Option<(Self::Value, bool)> {
        self.untag().move_value(to_space).map(|(x, moved)| (x.tag(), moved))
    }
}

impl Markable for Object<'_> {
    type Value = Self;

    fn is_marked(&self) -> bool {
        match self.untag() {
            ObjectType::Int(_) | ObjectType::SubrFn(_) => true,
            ObjectType::Float(x) => x.is_marked(),
            ObjectType::Cons(x) => x.is_marked(),
            ObjectType::Vec(x) => x.is_marked(),
            ObjectType::Record(x) => x.is_marked(),
            ObjectType::HashTable(x) => x.is_marked(),
            ObjectType::String(x) => x.is_marked(),
            ObjectType::ByteString(x) => x.is_marked(),
            ObjectType::ByteFn(x) => x.is_marked(),
            ObjectType::Symbol(x) => x.is_marked(),
            ObjectType::Buffer(x) => x.is_marked(),
        }
    }

    fn move_value(&self, to_space: &bumpalo::Bump) -> Option<(Self::Value, bool)> {
        let data = match self.untag() {
            ObjectType::Int(_) | ObjectType::SubrFn(_) | ObjectType::NIL => return None,
            ObjectType::Float(x) => cast_pair(x.move_value(to_space)?),
            ObjectType::Cons(x) => cast_pair(x.move_value(to_space)?),
            ObjectType::Vec(x) => cast_pair(x.move_value(to_space)?),
            ObjectType::Record(x) => cast_pair(x.move_value(to_space)?),
            ObjectType::HashTable(x) => cast_pair(x.move_value(to_space)?),
            ObjectType::String(x) => cast_pair(x.move_value(to_space)?),
            ObjectType::ByteString(x) => cast_pair(x.move_value(to_space)?),
            ObjectType::ByteFn(x) => cast_pair(x.move_value(to_space)?),
            ObjectType::Buffer(x) => cast_pair(x.move_value(to_space)?),
            ObjectType::Symbol(x) => {
                // Need to handle specially because a symbol is not a pointer,
                // but rather an offset
                let (sym, moved) = x.move_value(to_space)?;
                (sym.as_ptr(), moved)
            }
        };

        let tag = self.get_tag();
        unsafe { Some((Object::from_ptr(data.0, tag), data.1)) }
    }
}

impl Markable for Function<'_> {
    type Value = Self;

    fn is_marked(&self) -> bool {
        match self.untag() {
            FunctionType::SubrFn(_) => true,
            FunctionType::Cons(x) => x.is_marked(),
            FunctionType::ByteFn(x) => x.is_marked(),
            FunctionType::Symbol(x) => x.is_marked(),
        }
    }

    fn move_value(&self, to_space: &bumpalo::Bump) -> Option<(Self::Value, bool)> {
        let data = match self.untag() {
            FunctionType::SubrFn(_) => return None,
            FunctionType::Cons(x) => cast_pair(x.move_value(to_space)?),
            FunctionType::ByteFn(x) => cast_pair(x.move_value(to_space)?),
            FunctionType::Symbol(x) => {
                let (sym, moved) = x.move_value(to_space)?;
                cast_pair((NonNull::from(sym.get()), moved))
            }
        };

        let tag = self.get_tag();
        unsafe { Some((Function::from_ptr(data.0, tag), data.1)) }
    }
}

impl Markable for List<'_> {
    type Value = Self;

    fn is_marked(&self) -> bool {
        todo!()
    }

    fn move_value(&self, to_space: &bumpalo::Bump) -> Option<(Self::Value, bool)> {
        let data = match self.untag() {
            ListType::Cons(x) => cast_pair(x.move_value(to_space)?),
            ListType::Nil => return None,
        };

        let tag = self.get_tag();
        unsafe { Some((List::from_ptr(data.0, tag), data.1)) }
    }
}

fn cast_pair<T>((ptr, moved): (NonNull<T>, bool)) -> (*const u8, bool) {
    (ptr.as_ptr().cast::<u8>(), moved)
}

impl<'ob> PartialEq<&str> for Object<'ob> {
    fn eq(&self, other: &&str) -> bool {
        match self.untag() {
            ObjectType::String(x) => **x == **other,
            _ => false,
        }
    }
}

impl<'ob> PartialEq<Symbol<'_>> for Object<'ob> {
    fn eq(&self, other: &Symbol) -> bool {
        match self.untag() {
            ObjectType::Symbol(x) => x == *other,
            _ => false,
        }
    }
}

impl<'ob> PartialEq<f64> for Object<'ob> {
    fn eq(&self, other: &f64) -> bool {
        use float_cmp::ApproxEq;
        match self.untag() {
            ObjectType::Float(x) => x.approx_eq(*other, (f64::EPSILON, 2)),
            _ => false,
        }
    }
}

impl<'ob> PartialEq<i64> for Object<'ob> {
    fn eq(&self, other: &i64) -> bool {
        match self.untag() {
            ObjectType::Int(x) => x == *other,
            _ => false,
        }
    }
}

impl<'ob> PartialEq<bool> for Object<'ob> {
    fn eq(&self, other: &bool) -> bool {
        if *other {
            matches!(self.untag(), ObjectType::Symbol(sym::TRUE))
        } else {
            matches!(self.untag(), ObjectType::Symbol(sym::NIL))
        }
    }
}

impl<'ob> Object<'ob> {
    pub(crate) fn as_cons(self) -> &'ob Cons {
        self.try_into().unwrap()
    }
}

impl Default for Object<'_> {
    fn default() -> Self {
        NIL
    }
}

impl Default for List<'_> {
    fn default() -> Self {
        ListType::empty()
    }
}

impl<T> fmt::Display for Gc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_obj().untag())
    }
}

impl<T> fmt::Debug for Gc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl<T> PartialEq for Gc<T> {
    fn eq(&self, other: &Self) -> bool {
        self.ptr == other.ptr || self.as_obj().untag() == other.as_obj().untag()
    }
}

impl<T> Eq for Gc<T> {}

use std::hash::{Hash, Hasher};
impl<T> Hash for Gc<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.ptr.hash(state);
    }
}

impl fmt::Display for ObjectType<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.display_walk(f, &mut HashSet::default())
    }
}

impl fmt::Debug for ObjectType<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.display_walk(f, &mut HashSet::default())
    }
}

impl ObjectType<'_> {
    pub(crate) fn display_walk(
        &self,
        f: &mut fmt::Formatter,
        seen: &mut HashSet<*const u8>,
    ) -> fmt::Result {
        use fmt::Display as D;
        match self {
            ObjectType::Int(x) => D::fmt(x, f),
            ObjectType::Cons(x) => x.display_walk(f, seen),
            ObjectType::Vec(x) => x.display_walk(f, seen),
            ObjectType::Record(x) => x.display_walk(f, seen),
            ObjectType::HashTable(x) => x.display_walk(f, seen),
            ObjectType::String(x) => write!(f, "\"{x}\""),
            ObjectType::ByteString(x) => write!(f, "\"{x}\""),
            ObjectType::Symbol(x) => D::fmt(x, f),
            ObjectType::ByteFn(x) => D::fmt(x, f),
            ObjectType::SubrFn(x) => D::fmt(x, f),
            ObjectType::Float(x) => D::fmt(x, f),
            ObjectType::Buffer(x) => D::fmt(x, f),
        }
    }
}

impl<'ob> Object<'ob> {
    pub(crate) fn is_marked(self) -> bool {
        match self.untag() {
            ObjectType::Int(_) | ObjectType::SubrFn(_) => true,
            ObjectType::Float(x) => x.is_marked(),
            ObjectType::Cons(x) => x.is_marked(),
            ObjectType::Vec(x) => x.is_marked(),
            ObjectType::Record(x) => x.is_marked(),
            ObjectType::HashTable(x) => x.is_marked(),
            ObjectType::String(x) => x.is_marked(),
            ObjectType::ByteString(x) => x.is_marked(),
            ObjectType::ByteFn(x) => x.is_marked(),
            ObjectType::Symbol(x) => x.is_marked(),
            ObjectType::Buffer(x) => x.is_marked(),
        }
    }
}

impl<'ob> ListType<'ob> {
    #[cfg(test)]
    pub(crate) fn car(self) -> Object<'ob> {
        match self {
            ListType::Nil => NIL,
            ListType::Cons(x) => x.car(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::{TagType, MAX_FIXNUM, MIN_FIXNUM};
    use crate::core::gc::{Context, RootSet};
    use rune_core::macros::list;

    #[test]
    fn test_clamp_fixnum() {
        assert_eq!(0i64.tag().untag(), 0);
        assert_eq!((-1_i64).tag().untag(), -1);
        assert_eq!(i64::MAX.tag().untag(), MAX_FIXNUM);
        assert_eq!(MAX_FIXNUM.tag().untag(), MAX_FIXNUM);
        assert_eq!(i64::MIN.tag().untag(), MIN_FIXNUM);
        assert_eq!(MIN_FIXNUM.tag().untag(), MIN_FIXNUM);
    }

    #[test]
    fn test_print_circle() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        let cons = list![1; cx];
        cons.as_cons().set_cdr(cons).unwrap();
        assert_eq!(format!("{cons}"), "(1 . #0)");

        cons.as_cons().set_car(cons).unwrap();
        assert_eq!(format!("{cons}"), "(#0 . #0)");
    }
}
