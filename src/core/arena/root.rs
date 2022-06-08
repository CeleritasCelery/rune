use std::fmt::Debug;
use std::ops::{Deref, DerefMut, Index, IndexMut};
use std::ptr::{addr_of, addr_of_mut};
use std::slice::SliceIndex;

use super::super::{
    cons::Cons,
    env::{Environment, Symbol},
    object::{GcObj, RawObj},
};
use super::{Arena, Block, RootSet, Trace};
use crate::core::error::{Error, Type};
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

impl IntoRoot<&'static Cons> for &Cons {
    unsafe fn into_root(self) -> &'static Cons {
        self.with_lifetime()
    }
}

impl<'ob> IntoRoot<(Symbol, GcObj<'static>)> for (Symbol, GcObj<'ob>) {
    unsafe fn into_root(self) -> (Symbol, GcObj<'static>) {
        (self.0, self.1.with_lifetime())
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

pub(crate) struct Root<'rt, T> {
    data: *mut T,
    root_set: &'rt RootSet,
}

impl<'rt, T> Root<'rt, T> {
    pub(crate) unsafe fn new(root_set: &'rt RootSet) -> Self {
        Self {
            data: std::ptr::null_mut(),
            root_set,
        }
    }

    pub(crate) fn deref(&self) -> &Rt<T> {
        assert!(!self.data.is_null(), "Attempt to deref uninitialzed Root");
        unsafe { &*self.data.cast::<Rt<T>>() }
    }

    pub(crate) fn deref_mut<'a, 'd>(&'a mut self, _: &'a Arena<'d>) -> &'a mut Rt<T> {
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

impl<'b, T: Trace + 'static> Root<'b, T> {
    pub(crate) unsafe fn init<'a>(&'a mut self, data: &'a mut T) -> &'a mut Self {
        assert!(self.data.is_null(), "Attempt to reinit Root");
        let dyn_ptr = data as &mut dyn Trace as *mut dyn Trace;
        self.data = dyn_ptr.cast::<T>();
        self.root_set.root_structs.borrow_mut().push(dyn_ptr);
        self
    }
}

impl<T> Drop for Root<'_, T> {
    fn drop(&mut self) {
        assert!(!self.data.is_null(), "Root was dropped while still not set");
        self.root_set.root_structs.borrow_mut().pop();
    }
}

#[macro_export]
macro_rules! root {
    ($ident:ident, $arena:ident) => {
        let mut rooted = unsafe { $crate::core::object::WithLifetime::with_lifetime($ident) };
        let mut root = unsafe { $crate::core::arena::Root::new($arena.get_root_set()) };
        let $ident = unsafe { root.init(&mut rooted).deref() };
    };
    ($ident:ident, $value:expr, $arena:ident) => {
        // TODO: see if this can be removed
        #[allow(unused_unsafe)]
        let mut rooted = unsafe { $value };
        let mut root = unsafe { $crate::core::arena::Root::new($arena.get_root_set()) };
        let $ident = unsafe { root.init(&mut rooted) };
    };
}

// TODO: see if this type can be made local
#[repr(transparent)]
pub(crate) struct Rt<T: ?Sized> {
    inner: T,
}

impl<T: ?Sized + Debug> Debug for Rt<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.inner, f)
    }
}

impl<T: PartialEq> PartialEq<T> for Rt<T> {
    fn eq(&self, other: &T) -> bool {
        self.inner == *other
    }
}

impl<T> Rt<T> {
    pub(crate) fn bind<'ob, U>(&self, _: &'ob Arena) -> U
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
    pub(crate) fn as_ref<'ob, U>(&self, _: &'ob Arena) -> &'ob [U]
    where
        T: WithLifetime<'ob, Out = U>,
    {
        unsafe { &*(addr_of!(self.inner) as *const [U]) }
    }

    pub(crate) fn bind_slice<'ob, U>(slice: &[Rt<T>], _: &'ob Arena) -> &'ob [U]
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
            x => Err(Error::from_object(Type::Symbol, x).into()),
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

impl<'ob> IntoObject<'ob> for &Rt<GcObj<'static>> {
    type Out = Object<'ob>;

    fn into_obj<const C: bool>(self, _block: &'ob Block<C>) -> Gc<Self::Out> {
        unsafe { self.inner.with_lifetime() }
    }

    unsafe fn from_obj_ptr(_ptr: *const u8) -> Self::Out {
        unimplemented!()
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
    pub(crate) fn as_slice(&self) -> &[Rt<T>] {
        // SAFETY: `Gc<T>` has the same memory layout as `T`.
        unsafe { &*(self.inner.as_slice() as *const [T] as *const [Rt<T>]) }
    }

    pub(crate) fn as_gc(&self) -> &Rt<[T]> {
        // SAFETY: `Gc<T>` has the same memory layout as `T`.
        unsafe { &*(self.inner.as_slice() as *const [T] as *const Rt<[T]>) }
    }

    pub(crate) fn as_mut_slice(&mut self) -> &mut [Rt<T>] {
        // SAFETY: `Gc<T>` has the same memory layout as `T`.
        unsafe { &mut *(self.inner.as_mut_slice() as *mut [T] as *mut [Rt<T>]) }
    }

    pub(crate) fn push<U: IntoRoot<T>>(&mut self, item: U) {
        self.inner.push(unsafe { item.into_root() });
    }
    pub(crate) fn truncate(&mut self, len: usize) {
        self.inner.truncate(len);
    }

    pub(crate) fn append(&mut self, other: &mut Self) {
        self.inner.append(&mut other.inner);
    }

    pub(crate) fn clear(&mut self) {
        self.inner.clear();
    }
}

impl<T> Deref for Rt<Vec<T>> {
    type Target = [Rt<T>];

    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

impl<T> DerefMut for Rt<Vec<T>> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_mut_slice()
    }
}

impl<K, V> Rt<HashMap<K, V>>
where
    K: Eq + std::hash::Hash,
{
    pub(crate) fn get<Q: ?Sized>(&self, k: &Q) -> Option<&Rt<V>>
    where
        K: std::borrow::Borrow<Q>,
        Q: std::hash::Hash + Eq,
    {
        self.inner
            .get(k)
            .map(|v| unsafe { &*(v as *const V).cast::<Rt<V>>() })
    }

    pub(crate) fn get_mut<Q: ?Sized>(&mut self, k: &Q) -> Option<&mut Rt<V>>
    where
        K: std::borrow::Borrow<Q>,
        Q: std::hash::Hash + Eq,
    {
        self.inner
            .get_mut(k)
            .map(|v| unsafe { &mut *(v as *mut V).cast::<Rt<V>>() })
    }

    pub(crate) fn insert<R: IntoRoot<V>>(&mut self, k: K, v: R) {
        self.inner.insert(k, unsafe { v.into_root() });
    }
}

type Prop = Rt<HashMap<Symbol, Vec<(Symbol, GcObj<'static>)>>>;
impl Rt<Environment> {
    pub(crate) fn vars(&self) -> &Rt<HashMap<Symbol, GcObj<'static>>> {
        unsafe { &*addr_of!(self.inner.vars).cast() }
    }

    pub(crate) fn vars_mut(&mut self) -> &mut Rt<HashMap<Symbol, GcObj<'static>>> {
        unsafe { &mut *addr_of_mut!(self.inner.vars).cast() }
    }

    pub(crate) fn props(&self) -> &Prop {
        unsafe { &*addr_of!(self.inner.props).cast() }
    }

    pub(crate) fn props_mut(&mut self) -> &mut Prop {
        unsafe { &mut *addr_of_mut!(self.inner.props).cast() }
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
        let arena = &Arena::new(root);
        let mut vec: Rt<Vec<GcObj<'static>>> = Rt { inner: vec![] };

        vec.push(GcObj::NIL);
        assert!(matches!(vec[0].bind(arena).get(), Object::Nil));
        let str1 = arena.add("str1");
        let str2 = arena.add("str2");
        vec.push(str1);
        vec.push(str2);
        let slice = &vec[0..3];
        assert_eq!(vec![GcObj::NIL, str1, str2], slice.as_ref(arena));
    }
}
