use std::fmt::Debug;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut, Index, IndexMut};
use std::ptr::addr_of;
use std::slice::SliceIndex;

use super::super::{
    cons::Cons,
    env::{Environment, Symbol},
    object::{GcObj, RawObj},
};
use super::{Block, Context, RootSet, Trace};
use crate::core::error::{Type, TypeError};
use crate::core::object::{Gc, IntoObject, Object, WithLifetime};
use crate::hashmap::HashMap;

pub(crate) trait IntoRoot<T> {
    unsafe fn into_root(self) -> T;
}

impl IntoRoot<GcObj<'static>> for GcObj<'_> {
    unsafe fn into_root(self) -> GcObj<'static> {
        self.with_lifetime()
    }
}

impl IntoRoot<GcObj<'static>> for &Rt<GcObj<'_>> {
    unsafe fn into_root(self) -> GcObj<'static> {
        self.inner.with_lifetime()
    }
}

impl IntoRoot<&'static Cons> for &Cons {
    unsafe fn into_root(self) -> &'static Cons {
        self.with_lifetime()
    }
}

impl IntoRoot<Symbol> for Symbol {
    unsafe fn into_root(self) -> Symbol {
        self
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

impl<T: IntoRoot<U>, U> IntoRoot<Vec<U>> for Vec<T> {
    unsafe fn into_root(self) -> Vec<U> {
        self.into_iter().map(|x| x.into_root()).collect()
    }
}

impl<T> Trace for Gc<T> {
    fn mark(&self, stack: &mut Vec<RawObj>) {
        self.as_obj().trace_mark(stack);
    }
}

impl Trace for &Cons {
    fn mark(&self, stack: &mut Vec<RawObj>) {
        Cons::mark(self, stack);
    }
}

impl<T, U> Trace for (T, U)
where
    T: Trace,
    U: Trace,
{
    fn mark(&self, stack: &mut Vec<RawObj>) {
        self.0.mark(stack);
        self.1.mark(stack);
    }
}

impl Trace for Symbol {
    fn mark(&self, _stack: &mut Vec<RawObj>) {
        // TODO: implement
    }
}

/// Represents a Rooted object T. The purpose of this type is we cannot have
/// mutable references to the inner data, because the garbage collector will
/// need to trace it. This type will only give us a mut [`Rt`] (rooted mutable
/// reference) when we are also holding a reference to the Context, meaning that
/// garbage collection cannot happen.
pub(crate) struct Root<'rt, 'a, T> {
    data: *mut T,
    root_set: &'rt RootSet,
    // This lifetime parameter ensures that functions like mem::swap cannot be
    // called in a way that would lead to memory unsafety
    safety: PhantomData<&'a ()>,
}

impl<T: Debug> Debug for Root<'_, '_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(unsafe { &*self.data }, f)
    }
}

impl<'rt, T> Root<'rt, '_, T> {
    pub(crate) unsafe fn new(root_set: &'rt RootSet) -> Self {
        Self {
            data: std::ptr::null_mut(),
            root_set,
            safety: PhantomData,
        }
    }

    pub(crate) fn deref_mut<'a>(&'a mut self, _cx: &'a Context) -> &'a mut Rt<T> {
        unsafe { self.deref_mut_unchecked() }
    }

    pub(crate) unsafe fn deref_mut_unchecked(&mut self) -> &mut Rt<T> {
        assert!(
            !self.data.is_null(),
            "Attempt to mutably deref uninitialzed Root"
        );
        &mut *self.data.cast::<Rt<T>>()
    }
}

impl<T> Deref for Root<'_, '_, T> {
    type Target = Rt<T>;

    fn deref(&self) -> &Self::Target {
        assert!(!self.data.is_null(), "Attempt to deref uninitialzed Root");
        unsafe { &*self.data.cast::<Rt<T>>() }
    }
}

impl<T> AsRef<Rt<T>> for Root<'_, '_, T> {
    fn as_ref(&self) -> &Rt<T> {
        self
    }
}

impl<'rt, T: Trace + 'static> Root<'rt, '_, T> {
    pub(crate) unsafe fn init<'a>(root: &'a mut Self, data: &'a mut T) -> &'a mut Root<'rt, 'a, T> {
        assert!(root.data.is_null(), "Attempt to reinit Root");
        let dyn_ptr = data as &mut dyn Trace as *mut dyn Trace;
        root.data = dyn_ptr.cast::<T>();
        root.root_set.roots.borrow_mut().push(dyn_ptr);
        // We need the safety lifetime to match the borrow
        std::mem::transmute::<&mut Root<'rt, '_, T>, &mut Root<'rt, 'a, T>>(root)
    }
}

impl<T> Drop for Root<'_, '_, T> {
    fn drop(&mut self) {
        if self.data.is_null() {
            eprintln!("Error: Root was dropped while still not set");
        } else {
            self.root_set.roots.borrow_mut().pop();
        }
    }
}

#[macro_export]
macro_rules! root {
    ($ident:ident, $cx:ident) => {
        let mut rooted = unsafe { $crate::core::object::WithLifetime::with_lifetime($ident) };
        let mut root: $crate::core::gc::Root<_> =
            unsafe { $crate::core::gc::Root::new($cx.get_root_set()) };
        let $ident = unsafe { $crate::core::gc::Root::init(&mut root, &mut rooted) };
    };
    ($ident:ident, $value:expr, $cx:ident) => {
        // TODO: see if this can be removed
        #[allow(unused_unsafe)]
        let mut rooted = unsafe { $value };
        let mut root: $crate::core::gc::Root<_> =
            unsafe { $crate::core::gc::Root::new($cx.get_root_set()) };
        let $ident = unsafe { $crate::core::gc::Root::init(&mut root, &mut rooted) };
    };
}

/// A Rooted type. If a type is wrapped in Rt, it is known to be rooted and hold
/// items passed garbage collection. This type is never used as an owned type,
/// only a reference. This ensures that underlying data does not move. In order
/// to access the inner data, the [`Rt::bind`] method must be used.
#[repr(transparent)]
pub(crate) struct Rt<T: ?Sized> {
    inner: T,
}

impl<T: ?Sized + Debug> Debug for Rt<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.inner, f)
    }
}

impl PartialEq for Rt<GcObj<'_>> {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}

impl<T: PartialEq> PartialEq<T> for Rt<T> {
    fn eq(&self, other: &T) -> bool {
        self.inner == *other
    }
}

impl Deref for Rt<Symbol> {
    type Target = Symbol;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T> Rt<T> {
    pub(crate) fn bind<'ob, U>(&self, _: &'ob Context) -> U
    where
        T: WithLifetime<'ob, Out = U> + Copy,
    {
        unsafe { self.inner.with_lifetime() }
    }

    pub(crate) unsafe fn bind_unchecked<'ob, U>(&'ob self) -> U
    where
        T: WithLifetime<'ob, Out = U> + Copy,
    {
        self.inner.with_lifetime()
    }
}

impl<T> Rt<[T]> {
    pub(crate) fn as_ref<'ob, U>(&self, _: &'ob Context) -> &'ob [U]
    where
        T: WithLifetime<'ob, Out = U>,
    {
        unsafe { &*(addr_of!(self.inner) as *const [U]) }
    }

    pub(crate) fn bind_slice<'ob, U>(slice: &[Rt<T>], _: &'ob Context) -> &'ob [U]
    where
        T: WithLifetime<'ob, Out = U>,
    {
        unsafe { &*(slice as *const [Rt<T>] as *const [U]) }
    }
}

impl<T> Rt<Gc<T>> {
    pub(crate) fn try_as<U, E>(&self) -> Result<&Rt<Gc<U>>, E>
    where
        Gc<T>: TryInto<Gc<U>, Error = E> + Copy,
    {
        let _: Gc<U> = self.inner.try_into()?;
        // SAFETY: This is safe because all Gc types have the same representation
        unsafe { Ok(&*((self as *const Self).cast::<Rt<Gc<U>>>())) }
    }

    // TODO: see if this can be removed
    pub(crate) fn as_cons(&self) -> &Rt<Gc<&Cons>> {
        match self.inner.as_obj().get() {
            crate::core::object::Object::Cons(_) => unsafe {
                &*(self as *const Self).cast::<Rt<Gc<&Cons>>>()
            },
            x => panic!("attempt to convert type that was not cons: {x}"),
        }
    }

    pub(crate) fn set<U>(&mut self, item: U)
    where
        U: IntoRoot<Gc<T>>,
    {
        unsafe {
            self.inner = item.into_root();
        }
    }
}

impl TryFrom<&Rt<GcObj<'_>>> for Symbol {
    type Error = anyhow::Error;

    fn try_from(value: &Rt<GcObj>) -> Result<Self, Self::Error> {
        match value.inner.get() {
            Object::Symbol(sym) => Ok(sym),
            x => Err(TypeError::new(Type::Symbol, x).into()),
        }
    }
}

impl From<&Rt<GcObj<'_>>> for Option<()> {
    fn from(value: &Rt<GcObj<'_>>) -> Self {
        match value.inner.get() {
            Object::Nil => None,
            _ => Some(()),
        }
    }
}

impl Rt<GcObj<'static>> {
    pub(crate) fn try_as_option<T, E>(&self) -> Result<Option<&Rt<Gc<T>>>, E>
    where
        GcObj<'static>: TryInto<Gc<T>, Error = E>,
    {
        match self.inner.get() {
            Object::Nil => Ok(None),
            _ => {
                let _: Gc<T> = self.inner.try_into()?;
                unsafe { Ok(Some(&*((self as *const Self).cast::<Rt<Gc<T>>>()))) }
            }
        }
    }
}

impl<'ob> IntoObject<'ob> for &Rt<GcObj<'static>> {
    type Out = Object<'ob>;

    fn into_obj<const C: bool>(self, _block: &'ob Block<C>) -> Gc<Self::Out> {
        unsafe { self.inner.with_lifetime() }
    }
}

impl<'ob> IntoObject<'ob> for &Root<'_, '_, GcObj<'static>> {
    type Out = Object<'ob>;

    fn into_obj<const C: bool>(self, _block: &'ob Block<C>) -> Gc<Self::Out> {
        unsafe { self.inner.with_lifetime() }
    }
}

impl<'ob> IntoObject<'ob> for &mut Root<'_, '_, GcObj<'static>> {
    type Out = Object<'ob>;

    fn into_obj<const C: bool>(self, _block: &'ob Block<C>) -> Gc<Self::Out> {
        unsafe { self.inner.with_lifetime() }
    }
}

impl Rt<&Cons> {
    pub(crate) fn set(&mut self, item: &Cons) {
        self.inner = unsafe { std::mem::transmute(item) }
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

impl<T> Deref for Rt<Option<T>> {
    type Target = Option<Rt<T>>;

    fn deref(&self) -> &Self::Target {
        unsafe { &*(self as *const Self).cast::<Option<Rt<T>>>() }
    }
}

impl<T> DerefMut for Rt<Option<T>> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *(self as *mut Rt<Option<T>>).cast::<Option<Rt<T>>>() }
    }
}

impl Rt<Option<GcObj<'static>>> {
    pub(crate) fn set(&mut self, obj: GcObj) {
        unsafe {
            self.inner = Some(obj.with_lifetime());
        }
    }
}

impl<T, I: SliceIndex<[T]>> Index<I> for Rt<Vec<T>> {
    type Output = Rt<I::Output>;

    fn index(&self, index: I) -> &Self::Output {
        unsafe { &*(Index::index(&self.inner, index) as *const I::Output as *const Rt<I::Output>) }
    }
}

impl<T, I: SliceIndex<[T]>> IndexMut<I> for Rt<Vec<T>> {
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        unsafe {
            &mut *(IndexMut::index_mut(&mut self.inner, index) as *mut I::Output
                as *mut Rt<I::Output>)
        }
    }
}

impl<T, I: SliceIndex<[T]>> Index<I> for Rt<[T]> {
    type Output = Rt<I::Output>;

    fn index(&self, index: I) -> &Self::Output {
        unsafe { &*(Index::index(&self.inner, index) as *const I::Output as *const Rt<I::Output>) }
    }
}

impl<T, I: SliceIndex<[T]>> IndexMut<I> for Rt<[T]> {
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        unsafe {
            &mut *(IndexMut::index_mut(&mut self.inner, index) as *mut I::Output
                as *mut Rt<I::Output>)
        }
    }
}

impl<T> Rt<Vec<T>> {
    pub(crate) fn as_slice(&self) -> &Rt<[T]> {
        // SAFETY: `Gc<T>` has the same memory layout as `T`.
        unsafe { &*(self.inner.as_slice() as *const [T] as *const Rt<[T]>) }
    }

    pub(crate) fn push<U: IntoRoot<T>>(&mut self, item: U) {
        self.inner.push(unsafe { item.into_root() });
    }

    pub(crate) fn pop(&mut self) {
        self.inner.pop();
    }

    pub(crate) fn truncate(&mut self, len: usize) {
        self.inner.truncate(len);
    }

    pub(crate) fn clear(&mut self) {
        self.inner.clear();
    }
}

impl<T> Deref for Rt<Vec<T>> {
    type Target = [Rt<T>];

    fn deref(&self) -> &Self::Target {
        // SAFETY: `Gc<T>` has the same memory layout as `T`.
        unsafe { &*(self.inner.as_slice() as *const [T] as *const [Rt<T>]) }
    }
}

impl<T> DerefMut for Rt<Vec<T>> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // SAFETY: `Gc<T>` has the same memory layout as `T`.
        unsafe { &mut *(self.inner.as_mut_slice() as *mut [T] as *mut [Rt<T>]) }
    }
}

impl<K, V> Rt<HashMap<K, V>>
where
    K: Eq + std::hash::Hash,
{
    pub(crate) fn get<Q>(&self, k: &Q) -> Option<&Rt<V>>
    where
        K: std::borrow::Borrow<Q>,
        Q: ?Sized + std::hash::Hash + Eq,
    {
        self.inner
            .get(k)
            .map(|v| unsafe { &*(v as *const V).cast::<Rt<V>>() })
    }

    pub(crate) fn get_mut<Q>(&mut self, k: &Q) -> Option<&mut Rt<V>>
    where
        K: std::borrow::Borrow<Q>,
        Q: ?Sized + std::hash::Hash + Eq,
    {
        self.inner
            .get_mut(k)
            .map(|v| unsafe { &mut *(v as *mut V).cast::<Rt<V>>() })
    }

    pub(crate) fn insert<R: IntoRoot<V>>(&mut self, k: K, v: R) {
        self.inner.insert(k, unsafe { v.into_root() });
    }
}

#[allow(non_camel_case_types)]
pub(crate) struct Environment__root {
    pub(crate) vars: Rt<HashMap<Symbol, GcObj<'static>>>,
    pub(crate) props: Rt<HashMap<Symbol, Vec<(Symbol, GcObj<'static>)>>>,
    pub(crate) catch_stack: Rt<Vec<GcObj<'static>>>,
    pub(crate) thrown: Rt<(GcObj<'static>, GcObj<'static>)>,
}

impl Deref for Rt<Environment> {
    type Target = Environment__root;

    fn deref(&self) -> &Self::Target {
        unsafe { &*(self as *const Rt<Environment>).cast::<Environment__root>() }
    }
}

impl DerefMut for Rt<Environment> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *(self as *mut Rt<Environment>).cast::<Environment__root>() }
    }
}

#[cfg(test)]
mod test {
    use super::super::super::object::Object;
    use super::super::RootSet;
    use super::*;

    #[test]
    fn indexing() {
        let root = &RootSet::default();
        let cx = &Context::new(root);
        let mut vec: Rt<Vec<GcObj<'static>>> = Rt { inner: vec![] };

        vec.push(GcObj::NIL);
        assert!(matches!(vec[0].bind(cx).get(), Object::Nil));
        let str1 = cx.add("str1");
        let str2 = cx.add("str2");
        vec.push(str1);
        vec.push(str2);
        let slice = &vec[0..3];
        assert_eq!(vec![GcObj::NIL, str1, str2], slice.as_ref(cx));
    }
}
