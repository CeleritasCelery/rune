use std::ops::{Deref, IndexMut};
use std::{ops::Index, slice::SliceIndex};

use crate::data::Environment;
use crate::hashmap::HashMap;
use crate::object::{Object, RawObj};
use crate::symbol::Symbol;

pub(crate) trait IntoRoot<T> {
    unsafe fn into_root(self) -> T;
}

impl<'ob> IntoRoot<GcStore> for Object<'ob> {
    unsafe fn into_root(self) -> GcStore {
        GcStore::new(self)
    }
}

#[repr(transparent)]
#[derive(Default, Debug, PartialEq)]
pub(crate) struct GcStore {
    obj: RawObj,
}

impl GcStore {
    fn new(obj: Object) -> Self {
        Self { obj: obj.into() }
    }
}

#[repr(transparent)]
pub(crate) struct Gc<T: ?Sized> {
    inner: T,
}

impl<T> Deref for Gc<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
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

    pub(crate) fn mutate(&mut self, func: fn(&mut T)) {
        let inner = &mut self.inner;
        func(inner);
    }

    pub(crate) fn add<'ob, 'a>(&mut self, obj: Object<'ob>, func: fn(&'a mut T, GcStore)) {
        let inner = unsafe { std::mem::transmute::<&mut T, &'a mut T>(&mut self.inner) };
        let store = unsafe { std::mem::transmute::<Object<'ob>, GcStore>(obj) };
        func(inner, store);
    }

    pub(crate) fn insert_obj<'ob, 'a, K: 'static>(
        &mut self,
        key: K,
        obj: Object<'ob>,
        func: fn(&'a mut T, K, GcStore),
    ) {
        let inner = unsafe { std::mem::transmute::<&mut T, &'a mut T>(&mut self.inner) };
        let store = unsafe { std::mem::transmute::<Object<'ob>, GcStore>(obj) };
        func(inner, key, store);
    }
}

impl Gc<GcStore> {
    pub(crate) fn obj(&self) -> Object {
        unsafe { Object::from_raw(self.inner.obj) }
    }

    pub(crate) fn set(&mut self, item: Object<'_>) {
        self.inner.obj = item.into();
    }
}

impl<'ob> AsRef<Object<'ob>> for Gc<GcStore> {
    fn as_ref(&self) -> &Object<'ob> {
        unsafe { &*(self as *const Self).cast::<Object>() }
    }
}

impl<'ob> AsRef<[Object<'ob>]> for Gc<[GcStore]> {
    fn as_ref(&self) -> &[Object<'ob>] {
        let ptr = self.inner.as_ptr().cast::<Object>();
        let len = self.inner.len();
        unsafe { std::slice::from_raw_parts(ptr, len) }
    }
}

impl<T, U> Gc<(T, U)> {
    pub(crate) fn get(&self) -> &(Gc<T>, Gc<U>) {
        // SAFETY: `Gc<T>` has the same memory layout as `T`.
        unsafe { &*(self as *const Gc<(T, U)>).cast::<(Gc<T>, Gc<U>)>() }
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
    pub(crate) fn as_slice_of_gc(&self) -> &[Gc<T>] {
        // SAFETY: `Gc<T>` has the same memory layout as `T`.
        unsafe { &*(self.inner.as_slice() as *const [T] as *const [Gc<T>]) }
    }

    pub(crate) fn push<U: IntoRoot<T>>(&mut self, item: U) {
        self.inner.push(unsafe { item.into_root() });
    }
}

impl<T> Gc<[T]> {
    pub(crate) fn as_slice_of_gc(&self) -> &[Gc<T>] {
        // SAFETY: `Gc<T>` has the same memory layout as `T`.
        unsafe { &*(self as *const Gc<[T]> as *const [Gc<T>]) }
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

type Prop = Gc<HashMap<Symbol, Vec<(Symbol, GcStore)>>>;
impl Gc<Environment> {
    pub(crate) fn vars(&self) -> &Gc<HashMap<Symbol, GcStore>> {
        unsafe { &*(&self.inner.vars as *const HashMap<_, _>).cast() }
    }

    pub(crate) fn vars_mut(&mut self) -> &mut Gc<HashMap<Symbol, GcStore>> {
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
        let mut vec: Gc<Vec<GcStore>> = Gc { inner: vec![] };

        vec.add(Object::NIL, Vec::push);
        assert!(matches!(vec[0].obj(), Object::Nil(_)));
        let str1 = arena.add("str1");
        let str2 = arena.add("str2");
        vec.add(str1, Vec::push);
        vec.add(str2, Vec::push);
        let slice = &vec[0..3];
        assert_eq!(vec![Object::NIL, str1, str2], slice.as_ref());
    }
}
