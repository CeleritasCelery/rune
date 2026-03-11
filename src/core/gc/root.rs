use super::{
    super::{cons::Cons, object::Object},
    GcMoveable, GcState, TracePtr,
};
use super::{Block, Context, RootSet, ThreadSafeRootSet, Trace};
use crate::core::object::{Gc, GcPtr, IntoObject, ObjectType, OptionalFlag, Untag, WithLifetime};
use rune_core::hashmap::IndexMap;
use std::hash::{Hash, Hasher};
use std::slice::SliceIndex;
use std::{
    cell::UnsafeCell,
    fmt,
    ops::{Deref, DerefMut, Index, IndexMut, RangeBounds},
};
use std::{
    fmt::{Debug, Display},
    marker::PhantomPinned,
};

/// Helper trait to break the dependency between an object and the lifetimes of
/// it's [traceable](Trace) children. If This trait is implemented, then the
/// object can be traced by the garbage collector. Once it becomes rooted, it as
/// well as all of it's tracable children will be live until it is unrooted.
/// This essentially makes any lifetimes of a tracable objects meaningless. They
/// can be anything, including 'static. When an object is removed from a root it
/// will be given the proper lifetime again. Care must be taken to ensure that
/// any lifetimes that are changed only belong to traceable objects. Object can
/// contain lifetimes parameters for both traceable and untracable children, and
/// only the traceable children's lifetimes can be changed.
///
/// On top of scrubbing the lifetimes, this trait can also do a transformation
/// of the underlying type for convenience, similar to calling `Into::into`.
#[diagnostic::on_unimplemented(
    message = "`{Self}` does not implement `Trace`",
    label = "cannot be rooted",
    note = "Use #[derive(Trace)] to make `{Self}` Rootable",
    note = "If this is a foreign type, implement `Trace` and `IntoRoot` manually"
)]
pub(crate) trait IntoRoot<T> {
    unsafe fn into_root(self) -> T;
}

impl<'new, T, U> IntoRoot<Slot<U>> for T
where
    T: WithLifetime<'new, Out = U> + GcPtr,
{
    unsafe fn into_root(self) -> Slot<U> {
        Slot::new(self.with_lifetime())
    }
}

impl<T, U> IntoRoot<Option<U>> for Option<T>
where
    T: IntoRoot<U>,
{
    unsafe fn into_root(self) -> Option<U> {
        self.map(|x| x.into_root())
    }
}

impl<T, U> IntoRoot<Vec<U>> for Vec<T>
where
    T: IntoRoot<U>,
{
    unsafe fn into_root(self) -> Vec<U> {
        let mut new = Vec::with_capacity(self.len());
        for x in self {
            new.push(x.into_root());
        }
        new
    }
}

impl<T, U, Tx, Ux> IntoRoot<(Tx, Ux)> for (T, U)
where
    T: IntoRoot<Tx>,
    U: IntoRoot<Ux>,
{
    unsafe fn into_root(self) -> (Tx, Ux) {
        (self.0.into_root(), self.1.into_root())
    }
}

impl<'a, T, U> IntoRoot<Slot<U>> for &Rt<Slot<T>>
where
    T: WithLifetime<'a, Out = U> + Copy,
{
    unsafe fn into_root(self) -> Slot<U> {
        Slot::new(self.inner().get().with_lifetime())
    }
}

impl<'a> IntoRoot<Slot<Object<'a>>> for bool {
    unsafe fn into_root(self) -> Slot<Object<'a>> {
        Slot::new(self.into())
    }
}

impl<'a> IntoRoot<Slot<Object<'a>>> for i64 {
    unsafe fn into_root(self) -> Slot<Object<'a>> {
        Slot::new(self.into())
    }
}

// Represents an object T rooted on the Stack. This will remove the the object
// from the root set when dropped.
#[doc(hidden)]
pub(crate) struct __StackRoot<'rt, T> {
    data: &'rt mut Rt<T>,
    root_set: &'rt RootSet,
}

impl<T> AsMut<Rt<T>> for __StackRoot<'_, T> {
    fn as_mut(&mut self) -> &mut Rt<T> {
        self.data
    }
}

impl<T: Debug> Debug for __StackRoot<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Debug::fmt(self.data, f)
    }
}

impl<T: Display> Display for __StackRoot<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(self.data, f)
    }
}

// Do not use this function directly. Use the `root` macro instead.
//
// SAFETY: An owned StackRoot must never be exposed to the rest of the program.
// That could result in calling `mem::forget` on the root, which would
// invalidate the stack property of the root set.
impl<'rt, T: Trace> __StackRoot<'rt, T> {
    pub(crate) unsafe fn new(data: &'rt mut T, root_set: &'rt RootSet) -> __StackRoot<'rt, T> {
        let dyn_ptr = data as &mut dyn Trace as *mut dyn Trace;
        // We are using this transmute to dissociate the `dyn Trace` from the T.
        // Otherwise rust tries to require us to add a 'static bound. We don't
        // need this because stackroot can't outlive data (due to the 'rt
        // lifetime), and therefore it can't outlive T.
        let dyn_ptr = std::mem::transmute::<*mut dyn Trace, *mut dyn Trace>(dyn_ptr);
        let data = &mut *(dyn_ptr.cast::<Rt<T>>());
        let root = Self { data, root_set };
        root_set.roots.borrow_mut().push(dyn_ptr);
        root
    }
}

impl<T> Drop for __StackRoot<'_, T> {
    fn drop(&mut self) {
        self.root_set.roots.borrow_mut().pop();
    }
}

/// Represents an object T rooted on the Heap. Unlike [`__StackRoot`], this type
/// owns its data via a `Box` and can live beyond a single stack frame. When
/// dropped, it searches and removes itself from the root set (rather than just
/// popping like StackRoot).
///
/// This is used for rooting objects that need to live in heap-allocated
/// collections like channels, where stack-based rooting is not possible.
#[doc(hidden)]
pub(crate) struct __HeapRoot<T: Trace> {
    data: Box<Rt<T>>,
    root_ptr: *const dyn Trace,
    root_set: &'static ThreadSafeRootSet,
}

impl<T: Trace> __HeapRoot<T> {
    pub(crate) fn new(value: T, root_set: &'static ThreadSafeRootSet) -> Self {
        let mut boxed = Box::new(Rt { inner: value, _aliasable: PhantomPinned });
        let dyn_ptr = &mut boxed.inner as &mut T as &mut dyn Trace as *mut dyn Trace;
        // Transmute to dissociate the `dyn Trace` from the T, similar to __StackRoot
        // The pointer is safe to keep because `Rt` is pinned
        let dyn_ptr = unsafe { std::mem::transmute::<*mut dyn Trace, *mut dyn Trace>(dyn_ptr) };

        root_set.roots.lock().unwrap().push(dyn_ptr);

        Self { data: boxed, root_ptr: dyn_ptr, root_set }
    }
}

impl<T: Trace> Deref for __HeapRoot<T> {
    type Target = Rt<T>;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T: Trace> DerefMut for __HeapRoot<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

impl<T: Trace> Drop for __HeapRoot<T> {
    fn drop(&mut self) {
        // Search and remove our root_ptr from the roots vector
        let mut roots = self.root_set.roots.lock().unwrap();
        if let Some(pos) = roots.iter().position(|&ptr| std::ptr::addr_eq(ptr, self.root_ptr)) {
            roots.swap_remove(pos);
        }
    }
}

// SAFETY: __HeapRoot<T> is Send/Sync because:
// 1. It owns its data via Box<Rt<T>>, which is heap-allocated and can move between threads
// 2. The root_ptr is a raw pointer to data inside self.data, so it moves with the HeapRoot
// 3. The ThreadSafeRootSet synchronizes all modifications to the root set
// 4. The root_ptr is only dereferenced during GC, which is synchronized by the owner's mutex
// 5. T: Trace ensures the contained data can be safely traced during GC

unsafe impl<T: Trace> Send for __HeapRoot<T> {}
unsafe impl<T: Trace> Sync for __HeapRoot<T> {}

impl<T: Trace + Debug> Debug for __HeapRoot<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Debug::fmt(&**self, f)
    }
}

impl<T: Trace + Display> Display for __HeapRoot<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(&**self, f)
    }
}

/// Trait created to overpass the orphan rule when deriving the
/// [Trace](`rune_macros::Trace`) derive macro. The derive
/// macro contains a blanket `Deref` (and `DerefMut`) like this:
///
/// ```ignore
/// unsafe { &*(rt as *const Rt<Self>).cast::<Self::Target>() }
/// ```
///
/// By creating a trait that the functions defined in the main crate
/// can define, we avoid the orphan rule by implementing `Deref`
/// on the rooted version of the types: [Rt\<T\>](`self::Rt`).
pub trait RootedDeref {
    type Target;
    fn rooted_deref(rooted: &Rt<Self>) -> &Self::Target;
    fn rooted_derefmut(rooted: &mut Rt<Self>) -> &mut Self::Target;
}

impl<T: RootedDeref> Deref for Rt<T> {
    type Target = <T as RootedDeref>::Target;
    fn deref(&self) -> &Self::Target {
        RootedDeref::rooted_deref(self)
    }
}

impl<T: RootedDeref> DerefMut for Rt<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        RootedDeref::rooted_derefmut(self)
    }
}

/// A "Root Tracable" type. If a type is wrapped in Rt, it is known to be rooted
/// and hold items past garbage collection. This type is never used as an owned
/// type, only a reference. This ensures that underlying data does not move. In
/// order to access the inner data, use [`Rt::bind_ref`] or [`Rt::bind_mut`]
/// methods.
#[repr(transparent)]
#[derive(PartialEq, Eq)]
pub struct Rt<T: ?Sized> {
    _aliasable: PhantomPinned,
    inner: T,
}

/// A "Root Tracable Object". This differs from [`Rt`] by wrapping a [`Slot`]
/// which allows the underlying data to be moved during garbage collection. GC
/// owned types will always be wrapped in a `Slot` when rooted. To access the
/// object, use [`Rto::bind`].
pub type Rto<T> = Rt<Slot<T>>;

/// A moveable pointer to the GC heap. This is used to wrap [Rooted](`Rt`)
/// [`Object`]'s so that we don't trigger UB when moving types during
/// collection.
#[repr(transparent)]
#[derive(Default)]
pub struct Slot<T: ?Sized> {
    inner: UnsafeCell<T>,
}

impl<T: Clone> Clone for Slot<T> {
    fn clone(&self) -> Self {
        Self::new(self.get().clone())
    }
}

impl<'new, T: WithLifetime<'new> + Copy> WithLifetime<'new> for Slot<T> {
    type Out = Slot<<T as WithLifetime<'new>>::Out>;

    unsafe fn with_lifetime(self) -> Self::Out {
        Slot::new(self.get().with_lifetime())
    }
}

impl<T: Hash> Hash for Slot<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.get().hash(state);
    }
}

impl<T: PartialEq> PartialEq for Slot<T> {
    fn eq(&self, other: &Self) -> bool {
        self.get() == other.get()
    }
}

// This type signature is so complex due to lifetime restrictions around
// invariance. deriving PartialEq will provide T == T. But if the lifetime is
// invariant than two types with different lifetimes will be different types.
// Using an UnsafeCell makes the lifetime invariant So we have to use
// WithLifetime to convert the lifetime to the same invariant lifetime 'a.
impl<'a, T, U> PartialEq<U> for Slot<T>
where
    U: WithLifetime<'a> + Copy,
    T: PartialEq<<U as WithLifetime<'a>>::Out>,
{
    fn eq(&self, other: &U) -> bool {
        *self.get() == unsafe { other.with_lifetime() }
    }
}

impl<T: Eq> Eq for Slot<T> {}

impl<T> Slot<T> {
    fn get(&self) -> &T {
        unsafe { &*self.inner.get() }
    }

    unsafe fn set(&self, new: T) {
        *self.inner.get() = new
    }

    pub(crate) fn new(val: T) -> Self {
        Slot { inner: UnsafeCell::new(val) }
    }
}

impl<T> Deref for Slot<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.get()
    }
}

// This is the boundary between the traceable structs, and heap objects. Each
// heap object that is rooted needs to live in a Slot, so everything below it is
// also a heap object. We completely trace each object graph before moving to
// the next slot.
impl<T> Trace for Slot<T>
where
    T: TracePtr + GcMoveable<Value = T>,
{
    fn trace(&self, state: &mut GcState) {
        if let Some((new, moved)) = self.get().move_value(&state.to_space) {
            unsafe { self.set(new) };
            if moved {
                self.get().trace_ptr(state);
                // finish tracing anything connected to this object. This will
                // help them be co-located in memory.
                state.trace_stack();
            }
        }
    }
}

impl<T: Debug> Debug for Rt<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Debug::fmt(&self.inner(), f)
    }
}

impl<T: Display> Display for Rt<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(&self.inner(), f)
    }
}

impl<T: Debug> Debug for Slot<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Debug::fmt(self.get(), f)
    }
}

impl<T: Display> Display for Slot<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        Display::fmt(self.get(), f)
    }
}

impl<T, U> PartialEq<U> for Rt<Slot<T>>
where
    Slot<T>: PartialEq<U>,
{
    fn eq(&self, other: &U) -> bool {
        self.inner() == other
    }
}

impl<T> Hash for Rt<T>
where
    T: Hash,
{
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.inner().hash(state);
    }
}

impl<T> Rt<T> {
    fn inner(&self) -> &T {
        &self.inner
    }

    fn inner_mut(&mut self) -> &mut T {
        &mut self.inner
    }

    fn inner_ptr(&self) -> *const T {
        &self.inner as *const T
    }

    fn inner_mut_ptr(&mut self) -> *mut T {
        &mut self.inner as *mut T
    }

    pub(crate) fn bind_ref<'a, 'ob>(&'a self, _: &'ob Context) -> &'a <T as WithLifetime<'ob>>::Out
    where
        T: WithLifetime<'ob>,
    {
        // SAFETY: We are holding a reference to the context
        unsafe { &*self.inner_ptr().cast::<<T as WithLifetime<'ob>>::Out>() }
    }

    pub(crate) fn bind_mut<'a, 'ob>(
        &'a mut self,
        _: &'ob Context,
    ) -> &'a mut <T as WithLifetime<'ob>>::Out
    where
        T: WithLifetime<'ob>,
    {
        // SAFETY: We are holding a reference to the context
        unsafe { &mut *self.inner_mut_ptr().cast::<<T as WithLifetime<'ob>>::Out>() }
    }

    pub(crate) fn set<U: IntoRoot<T>>(&mut self, item: U) {
        // SAFETY: we drop the old type so it never exposed and take the new
        // rooted type and replace it.
        unsafe { *self.inner_mut() = item.into_root() }
    }
}

impl<T> Rt<Slot<T>> {
    pub(crate) fn bind<'ob>(&self, _: &'ob Context) -> <T as WithLifetime<'ob>>::Out
    where
        T: WithLifetime<'ob> + Copy,
    {
        // SAFETY: We are holding a reference to the context
        unsafe { self.inner().get().with_lifetime() }
    }

    pub(crate) unsafe fn bind_unchecked<'ob>(&'ob self) -> <T as WithLifetime<'ob>>::Out
    where
        T: WithLifetime<'ob> + Copy,
    {
        self.inner().get().with_lifetime()
    }

    /// Get a copy of the inner value without requiring a Context.
    /// This is useful when you need to access the value but don't have a Context available.
    pub(crate) fn get_inner(&self) -> T
    where
        T: Copy,
    {
        *self.inner().get()
    }

    pub(crate) fn bind_slice<'brw, 'ob, U>(
        slice: &'brw [Rt<Slot<Gc<T>>>],
        _: &'ob Context,
    ) -> &'brw [Gc<U>]
    where
        Gc<T>: WithLifetime<'ob, Out = Gc<U>>,
        Gc<U>: 'ob,
    {
        // SAFETY: Gc<T> does not have any niche optimizations, so it is safe to
        // cast from a Rt<Slot>
        unsafe { &*(slice as *const [Rt<Slot<Gc<T>>>] as *const [Gc<U>]) }
    }
}

impl<T> Rt<Slot<Gc<T>>> {
    /// Calls [untag](Untag::untag_erased) on the tagged Gc pointer
    pub(crate) fn untag<'ob, U>(&self, cx: &'ob Context) -> U
    where
        Gc<T>: WithLifetime<'ob, Out = Gc<U>> + Copy,
        Gc<U>: Untag<U>,
    {
        cx.bind(*self.inner().get()).untag_erased()
    }

    /// Like `try_into`, but needed to due no specialization
    pub(crate) fn try_as<U, E>(&self) -> Result<&Rt<Slot<Gc<U>>>, E>
    where
        Gc<T>: TryInto<Gc<U>, Error = E> + Copy,
    {
        let _: Gc<U> = (*self.inner().get()).try_into()?;
        // SAFETY: This is safe because all Gc types have the same representation
        unsafe { Ok(&*((self as *const Self).cast::<Rt<Slot<Gc<U>>>>())) }
    }
}

impl TryFrom<&Rt<Slot<Object<'_>>>> for usize {
    type Error = anyhow::Error;

    fn try_from(value: &Rt<Slot<Object>>) -> Result<Self, Self::Error> {
        (*value.inner().get()).try_into()
    }
}

impl<T> Rt<Slot<Gc<T>>> {
    /// Like `try_into().bind(cx)`, but needed to due no specialization
    pub(crate) fn bind_as<'ob, U, E>(&self, _cx: &'ob Context) -> Result<U, E>
    where
        Gc<T>: WithLifetime<'ob> + Copy,
        <Gc<T> as WithLifetime<'ob>>::Out: TryInto<U, Error = E> + Copy,
    {
        unsafe { self.inner().get().with_lifetime().try_into() }
    }

    /// Like `Into`, but needed to due no specialization
    pub(crate) fn cast<U>(&self) -> &Rt<Slot<Gc<U>>>
    where
        Gc<T>: Into<Gc<U>> + Copy,
    {
        // SAFETY: This is safe because all Gc types have the same representation
        unsafe { &*((self as *const Self).cast::<Rt<Slot<Gc<U>>>>()) }
    }

    // TODO: Find a way to remove this method. We should never need to guess
    // if something is cons
    pub(crate) fn as_cons(&self) -> &Rt<Slot<Gc<&Cons>>> {
        match self.inner().as_obj().untag() {
            crate::core::object::ObjectType::Cons(_) => unsafe {
                &*(self as *const Self).cast::<Rt<Slot<Gc<&Cons>>>>()
            },
            x => panic!("attempt to convert type that was not cons: {x}"),
        }
    }
}

impl From<&Rt<Slot<Object<'_>>>> for OptionalFlag {
    fn from(value: &Rt<Slot<Object<'_>>>) -> Self {
        value.inner().is_nil().then_some(())
    }
}

impl<'a> Rt<Slot<Object<'a>>> {
    pub(crate) fn try_as_option<T, E>(&self) -> Result<Option<&Rt<Slot<Gc<T>>>>, E>
    where
        Object<'a>: TryInto<Gc<T>, Error = E>,
    {
        if self.inner().is_nil() {
            Ok(None)
        } else {
            let _: Gc<T> = (*self.inner().get()).try_into()?;
            unsafe { Ok(Some(&*((self as *const Self).cast::<Rt<Slot<Gc<T>>>>()))) }
        }
    }
}

impl IntoObject for &Rt<Slot<Object<'_>>> {
    type Out<'ob> = ObjectType<'ob>;

    fn into_obj<const C: bool>(self, _block: &Block<C>) -> Gc<Self::Out<'_>> {
        unsafe { self.inner().get().with_lifetime() }
    }
}

impl IntoObject for Slot<Object<'_>> {
    type Out<'ob> = ObjectType<'ob>;

    fn into_obj<const C: bool>(self, _block: &Block<C>) -> Gc<Self::Out<'_>> {
        unsafe { self.get().with_lifetime() }
    }
}

impl IntoObject for &mut Rt<Slot<Object<'_>>> {
    type Out<'ob> = ObjectType<'ob>;

    fn into_obj<const C: bool>(self, _block: &Block<C>) -> Gc<Self::Out<'_>> {
        unsafe { self.inner().get().with_lifetime() }
    }
}

impl Rt<Slot<&Cons>> {
    pub(crate) fn car<'ob>(&self, cx: &'ob Context) -> Object<'ob> {
        self.bind(cx).car()
    }

    pub(crate) fn cdr<'ob>(&self, cx: &'ob Context) -> Object<'ob> {
        self.bind(cx).cdr()
    }
}

impl<T, U> Deref for Rt<(T, U)> {
    type Target = (Rt<T>, Rt<U>);

    fn deref(&self) -> &Self::Target {
        unsafe { &*(self as *const Self).cast::<(Rt<T>, Rt<U>)>() }
    }
}

impl<T, U> DerefMut for Rt<(T, U)> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *(self as *mut Rt<(T, U)>).cast::<(Rt<T>, Rt<U>)>() }
    }
}

// Can't implement [`DerefMut`] because it would allow you to call
// [`Option::take`] which would return an owned Rt and break the chain of
// traceability
impl<T> Deref for Rt<Option<T>> {
    type Target = Option<Rt<T>>;
    fn deref(&self) -> &Self::Target {
        unsafe { &*self.inner_ptr().cast::<Self::Target>() }
    }
}

impl<T, I, const N: usize> Index<I> for Rt<[T; N]>
where
    [Rt<T>]: Index<I>,
{
    type Output = <[Rt<T>] as Index<I>>::Output;

    fn index(&self, index: I) -> &Self::Output {
        let slice = unsafe { &*self.inner_ptr().cast::<[Rt<T>; N]>() };
        Index::index(slice, index)
    }
}

impl<T, I, const N: usize> IndexMut<I> for Rt<[T; N]>
where
    [Rt<T>]: IndexMut<I>,
{
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        let slice = unsafe { &mut *self.inner_mut_ptr().cast::<[Rt<T>; N]>() };
        IndexMut::index_mut(slice, index)
    }
}

impl<T, const N: usize> AsRef<[Rt<T>]> for Rt<[T; N]> {
    fn as_ref(&self) -> &[Rt<T>] {
        unsafe { &*self.inner_ptr().cast::<[Rt<T>; N]>() }
    }
}

impl<T> Rt<Vec<T>> {
    // This is not safe to expose pub(crate) because you could call pop and get
    // an owned Rt
    fn as_mut_ref(&mut self) -> &mut Vec<Rt<T>> {
        // SAFETY: `Rt<T>` has the same memory layout as `T`.
        unsafe { &mut *(self as *mut Self).cast::<Vec<Rt<T>>>() }
    }

    pub(crate) fn push<U: IntoRoot<T>>(&mut self, item: U) {
        self.inner_mut().push(unsafe { item.into_root() });
    }

    pub(crate) fn truncate(&mut self, len: usize) {
        self.inner_mut().truncate(len);
    }

    pub(crate) fn pop(&mut self) {
        self.inner_mut().pop();
    }

    pub(crate) fn swap_remove(&mut self, index: usize) {
        self.inner_mut().swap_remove(index);
    }

    pub(crate) fn reserve(&mut self, additional: usize) {
        self.inner_mut().reserve(additional);
    }

    pub(crate) fn capacity(&self) -> usize {
        self.inner().capacity()
    }
}

impl<T> Rt<Vec<T>> {
    pub(crate) fn extend_from_slice<U: IntoRoot<T> + Copy>(&mut self, src: &[U]) {
        // TODO: Slot fix extend_from_slice
        let inner = self.inner_mut();
        for x in src {
            inner.push(unsafe { x.into_root() })
        }
    }
}

impl<T: Clone> Rt<Vec<T>> {
    pub(crate) fn extend_from_within(&mut self, src: impl RangeBounds<usize>) {
        self.inner_mut().extend_from_within(src);
    }
}

impl<T> Deref for Rt<Vec<T>> {
    type Target = [Rt<T>];
    fn deref(&self) -> &Self::Target {
        // SAFETY: `Rt<T>` has the same memory layout as `T`.
        unsafe { &*(self as *const Self).cast::<Vec<Rt<T>>>() }
    }
}

impl<T> DerefMut for Rt<Vec<T>> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // SAFETY: `Rt<T>` has the same memory layout as `T`.
        unsafe { &mut *(self as *mut Self).cast::<Vec<Rt<T>>>() }
    }
}

impl<T, I: SliceIndex<[Rt<T>]>> Index<I> for Rt<Vec<T>> {
    type Output = I::Output;

    fn index(&self, index: I) -> &Self::Output {
        let slice: &[Rt<T>] = self;
        Index::index(slice, index)
    }
}

impl<T, I: SliceIndex<[Rt<T>]>> IndexMut<I> for Rt<Vec<T>> {
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        IndexMut::index_mut(self.as_mut_ref(), index)
    }
}

#[derive(Debug)]
#[repr(transparent)]
/// A HashMap that can hold values past garbage collection.
///
/// This type is needed because Garbage Collection can move keys, which changes
/// their hash value. `ObjectMap` will rehash the keys after collection.
// It is not safe to Deref into the inner IndexMap type because we will be
// constructing a mutable reference during garbage collection. So we have to
// ensure that there cannot exist a &IndexMap to the same location.
pub(crate) struct ObjectMap<K, V>(UnsafeCell<IndexMap<K, V>>);

impl<K, V> Default for ObjectMap<K, V> {
    fn default() -> Self {
        Self(UnsafeCell::new(Default::default()))
    }
}

impl<K, V> Rt<ObjectMap<K, V>>
where
    K: Eq + Hash,
{
    // inner function that should not be exposed
    fn as_ref(&self) -> &IndexMap<K, V> {
        unsafe { &*self.inner().0.get() }
    }

    // inner function that should not be exposed
    fn as_mut(&mut self) -> &mut IndexMap<K, V> {
        unsafe { &mut *self.inner_mut().0.get() }
    }

    pub(crate) fn insert<Kx: IntoRoot<K>, Vx: IntoRoot<V>>(&mut self, k: Kx, v: Vx) {
        unsafe {
            self.as_mut().insert(k.into_root(), v.into_root());
        }
    }

    pub(crate) fn get<Q: IntoRoot<K>>(&self, k: Q) -> Option<&Rt<V>> {
        use std::ptr::from_ref;
        let inner = unsafe { &*from_ref(self.as_ref()).cast::<IndexMap<K, Rt<V>>>() };
        let root = unsafe { k.into_root() };
        inner.get(&root)
    }

    pub(crate) fn get_mut<Q: IntoRoot<K>>(&mut self, k: Q) -> Option<&mut Rt<V>> {
        use std::ptr::from_mut;
        let inner = unsafe { &mut *from_mut(self.as_mut()).cast::<IndexMap<K, Rt<V>>>() };
        let root = unsafe { k.into_root() };
        inner.get_mut(&root)
    }

    pub(crate) fn remove<Q: IntoRoot<K>>(&mut self, k: Q) {
        let root = unsafe { k.into_root() };
        self.as_mut().swap_remove(&root);
    }
}

impl<K, V> Trace for ObjectMap<K, V>
where
    K: Trace + Hash + Eq,
    V: Trace,
{
    fn trace(&self, state: &mut GcState) {
        let map = unsafe { &mut *self.0.get() };
        map.rehash_keys(|key, val| {
            key.trace(state);
            val.trace(state);
        });
    }
}

#[cfg(test)]
mod test {
    use crate::core::object::NIL;
    use rune_core::macros::root;

    use super::*;

    #[test]
    fn mem_swap() {
        let root = &RootSet::default();
        let cx = &mut Context::new(root);
        let outer = cx.add("outer");
        root!(outer, cx);
        {
            let inner = cx.add("inner");
            root!(inner, cx);
            std::mem::swap(outer, inner);
        }
        cx.garbage_collect(true);
        assert_eq!(outer.bind(cx), "inner");
    }

    #[test]
    fn indexing() {
        let root = &RootSet::default();
        let cx = &Context::new(root);
        let mut vec = Rt { inner: vec![], _aliasable: PhantomPinned };

        vec.push(NIL);
        assert_eq!(vec[0], NIL);
        let str1 = cx.add("str1");
        let str2 = cx.add("str2");
        vec.push(str1);
        vec.push(str2);
        assert_eq!(vec.bind_ref(cx)[0..3], vec![NIL, str1, str2]);
    }

    #[test]
    fn test_object_map() {
        type Map<'a> = ObjectMap<Slot<Object<'a>>, Slot<Object<'a>>>;
        let root = &RootSet::default();
        let cx = &mut Context::new(root);
        root!(map, new(Map), cx);
        let key = cx.add("key");

        map.insert(key, cx.add("val"));
        root!(key, cx);
        cx.garbage_collect(true);
        let val = map.get(key.bind(cx)).unwrap().bind(cx);
        assert_eq!(val, "val");
    }
}
