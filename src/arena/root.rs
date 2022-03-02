use std::ops::{Deref, DerefMut, IndexMut};
use std::{ops::Index, slice::SliceIndex};

use crate::cons::Cons;
use crate::data::Environment;
use crate::hashmap::HashMap;
use crate::object::{Object, RawObj};
use crate::symbol::Symbol;

pub(crate) trait IntoRoot<T> {
    unsafe fn into_root(self) -> T;
}

impl<'ob> IntoRoot<RootObj> for Object<'ob> {
    unsafe fn into_root(self) -> RootObj {
        RootObj::new(self)
    }
}

impl<'ob> IntoRoot<RootCons> for &Cons<'ob> {
    unsafe fn into_root(self) -> RootCons {
        RootCons::new(self)
    }
}

impl<'ob> IntoRoot<(Symbol, RootObj)> for (Symbol, Object<'ob>) {
    unsafe fn into_root(self) -> (Symbol, RootObj) {
        (self.0, RootObj::new(self.1))
    }
}

impl<T: IntoRoot<U>, U> IntoRoot<Vec<U>> for Vec<T> {
    unsafe fn into_root(self) -> Vec<U> {
        self.into_iter().map(|x| x.into_root()).collect()
    }
}

#[repr(transparent)]
#[derive(Default, Debug, PartialEq)]
pub(crate) struct RootObj {
    obj: RawObj,
}

impl RootObj {
    fn new(obj: Object) -> Self {
        Self { obj: obj.into() }
    }
}

#[repr(transparent)]
#[derive(Debug, PartialEq)]
pub(crate) struct RootCons {
    obj: *const Cons<'static>,
}

impl RootCons {
    pub(crate) fn new(obj: &Cons) -> Self {
        Self {
            obj: unsafe { std::mem::transmute(obj) },
        }
    }
}

#[repr(transparent)]
#[derive(Debug)]
pub(crate) struct Gc<T: ?Sized> {
    inner: T,
}

impl<T: PartialEq> PartialEq<T> for Gc<T> {
    fn eq(&self, other: &T) -> bool {
        self.inner == *other
    }
}

impl<T> AsRef<T> for Gc<T> {
    fn as_ref(&self) -> &T {
        &self.inner
    }
}

impl<T> Gc<T> {
    pub(crate) unsafe fn new(data: T) -> Self {
        Gc { inner: data }
    }
}

impl Gc<RootObj> {
    pub(crate) fn obj(&self) -> Object {
        unsafe { Object::from_raw(self.inner.obj) }
    }

    pub(crate) fn set(&mut self, item: Object<'_>) {
        self.inner.obj = item.into();
    }
}

impl<'ob> AsRef<Object<'ob>> for Gc<RootObj> {
    fn as_ref(&self) -> &Object<'ob> {
        unsafe { &*(self as *const Self).cast::<Object>() }
    }
}

impl<'ob> AsRef<[Object<'ob>]> for Gc<[RootObj]> {
    fn as_ref(&self) -> &[Object<'ob>] {
        let ptr = self.inner.as_ptr().cast::<Object>();
        let len = self.inner.len();
        unsafe { std::slice::from_raw_parts(ptr, len) }
    }
}

impl Gc<RootCons> {
    // TODO: remove this method and implement Deref with lifetime parameter is removed
    pub(crate) fn obj<'ob>(&'ob self) -> &'ob Cons<'ob> {
        unsafe { std::mem::transmute::<&Cons, &'ob Cons<'ob>>(&*(self.inner.obj)) }
    }

    pub(crate) fn set(&mut self, item: &Cons) {
        self.inner.obj = unsafe { std::mem::transmute(item) }
    }
}

impl<'ob> AsRef<Cons<'ob>> for Gc<RootCons> {
    fn as_ref(&self) -> &Cons<'ob> {
        unsafe { &*(self as *const Self).cast::<Cons>() }
    }
}

impl<'ob> AsRef<[Cons<'ob>]> for Gc<[RootCons]> {
    fn as_ref(&self) -> &[Cons<'ob>] {
        let ptr = self.inner.as_ptr().cast::<Cons>();
        let len = self.inner.len();
        unsafe { std::slice::from_raw_parts(ptr, len) }
    }
}

impl<T, U> Deref for Gc<(T, U)> {
    type Target = (Gc<T>, Gc<U>);

    fn deref(&self) -> &Self::Target {
        unsafe { &*(self as *const Gc<(T, U)>).cast::<(Gc<T>, Gc<U>)>() }
    }
}

impl<T, U> DerefMut for Gc<(T, U)> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *(self as *mut Gc<(T, U)>).cast::<(Gc<T>, Gc<U>)>() }
    }
}

impl<T, I: SliceIndex<[T]>> Index<I> for Gc<Vec<T>> {
    type Output = Gc<I::Output>;

    fn index(&self, index: I) -> &Self::Output {
        unsafe { &*(Index::index(&self.inner, index) as *const I::Output as *const Gc<I::Output>) }
    }
}

impl<T, I: SliceIndex<[T]>> IndexMut<I> for Gc<Vec<T>> {
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        unsafe {
            &mut *(IndexMut::index_mut(&mut self.inner, index) as *mut I::Output
                as *mut Gc<I::Output>)
        }
    }
}

impl<T, I: SliceIndex<[T]>> Index<I> for Gc<[T]> {
    type Output = Gc<I::Output>;

    fn index(&self, index: I) -> &Self::Output {
        unsafe { &*(Index::index(&self.inner, index) as *const I::Output as *const Gc<I::Output>) }
    }
}

impl<T, I: SliceIndex<[T]>> IndexMut<I> for Gc<[T]> {
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        unsafe {
            &mut *(IndexMut::index_mut(&mut self.inner, index) as *mut I::Output
                as *mut Gc<I::Output>)
        }
    }
}

impl<T> Gc<Vec<T>> {
    pub(crate) fn as_slice(&self) -> &[Gc<T>] {
        // SAFETY: `Gc<T>` has the same memory layout as `T`.
        unsafe { &*(self.inner.as_slice() as *const [T] as *const [Gc<T>]) }
    }

    pub(crate) fn as_mut_slice(&mut self) -> &mut [Gc<T>] {
        // SAFETY: `Gc<T>` has the same memory layout as `T`.
        unsafe { &mut *(self.inner.as_mut_slice() as *mut [T] as *mut [Gc<T>]) }
    }

    pub(crate) fn push<U: IntoRoot<T>>(&mut self, item: U) {
        self.inner.push(unsafe { item.into_root() });
    }
    pub(crate) fn truncate(&mut self, len: usize) {
        self.inner.truncate(len);
    }
    pub(crate) fn append<U: IntoRoot<T>>(&mut self, other: &mut Vec<U>) {
        let mut new = other.drain(0..).map(|x| unsafe { x.into_root() }).collect();
        self.inner.append(&mut new);
    }
}

impl<T> Deref for Gc<Vec<T>> {
    type Target = [Gc<T>];

    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

impl<T> DerefMut for Gc<Vec<T>> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_mut_slice()
    }
}

impl<K, V> Gc<HashMap<K, V>>
where
    K: Eq + std::hash::Hash,
{
    pub(crate) fn get<Q: ?Sized>(&self, k: &Q) -> Option<&Gc<V>>
    where
        K: std::borrow::Borrow<Q>,
        Q: std::hash::Hash + Eq,
    {
        self.inner
            .get(k)
            .map(|v| unsafe { &*(v as *const V).cast::<Gc<V>>() })
    }

    pub(crate) fn get_mut<Q: ?Sized>(&mut self, k: &Q) -> Option<&mut Gc<V>>
    where
        K: std::borrow::Borrow<Q>,
        Q: std::hash::Hash + Eq,
    {
        self.inner
            .get_mut(k)
            .map(|v| unsafe { &mut *(v as *mut V).cast::<Gc<V>>() })
    }

    pub(crate) fn insert<R: IntoRoot<V>>(&mut self, k: K, v: R) {
        self.inner.insert(k, unsafe { v.into_root() });
    }
}

type Prop = Gc<HashMap<Symbol, Vec<(Symbol, RootObj)>>>;
impl Gc<Environment> {
    pub(crate) fn vars(&self) -> &Gc<HashMap<Symbol, RootObj>> {
        unsafe { &*(&self.inner.vars as *const HashMap<_, _>).cast() }
    }

    pub(crate) fn vars_mut(&mut self) -> &mut Gc<HashMap<Symbol, RootObj>> {
        unsafe { &mut *(&mut self.inner.vars as *mut HashMap<_, _>).cast() }
    }

    pub(crate) fn props(&self) -> &Prop {
        unsafe { &*(&self.inner.props as *const HashMap<_, _>).cast() }
    }

    pub(crate) fn props_mut(&mut self) -> &mut Prop {
        unsafe { &mut *(&mut self.inner.props as *mut HashMap<_, _>).cast() }
    }
}

#[cfg(test)]
mod test {
    use crate::arena::{Arena, RootSet};

    use super::*;

    #[test]
    fn indexing() {
        let root = &RootSet::default();
        let arena = &Arena::new(root);
        let mut vec: Gc<Vec<RootObj>> = Gc { inner: vec![] };

        vec.push(Object::NIL);
        assert!(matches!(vec[0].obj(), Object::Nil(_)));
        let str1 = arena.add("str1");
        let str2 = arena.add("str2");
        vec.push(str1);
        vec.push(str2);
        let slice = &vec[0..3];
        assert_eq!(vec![Object::NIL, str1, str2], slice.as_ref());
    }
}
