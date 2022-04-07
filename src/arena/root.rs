use std::ops::{Deref, DerefMut, IndexMut};
use std::{ops::Index, slice::SliceIndex};

use crate::cons::Cons;
use crate::data::Environment;
use crate::hashmap::HashMap;
use crate::lcell::{LCell, LCellOwner};
use crate::object::{Object, RawObj};
use crate::symbol::Symbol;

use super::{Arena, Trace};

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
    pub(crate) fn new(obj: Object) -> Self {
        Self { obj: obj.into() }
    }
}

impl Trace for RootObj {
    fn mark(&self) {
        let obj = unsafe { Object::from_raw(self.obj) };
        obj.mark();
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
            obj: unsafe { std::mem::transmute::<&Cons, *const Cons<'static>>(obj) },
        }
    }
}

impl Trace for RootCons {
    fn mark(&self) {
        unsafe {
            (*self.obj).mark();
        }
    }
}

#[repr(transparent)]
#[derive(Debug)]
pub(crate) struct Root<'id, T: ?Sized>(LCell<'id, RootHandle<T>>);

impl<'id, T> Root<'id, T> {
    pub(crate) unsafe fn new(obj: T) -> Self {
        Root(LCell::new(RootHandle::new(obj)))
    }

    pub(super) fn deref(&mut self) -> &T {
        unsafe { &*(&self.0 as *const LCell<'id, RootHandle<T>>).cast::<T>() }
    }

    pub(crate) fn borrow<'a>(&'a self, owner: &'a LCellOwner<'id>) -> &'a RootHandle<T> {
        self.0.ro(owner)
    }

    pub(crate) fn borrow_mut<'a>(
        &'a self,
        owner: &'a mut LCellOwner<'id>,
        _: &'a Arena,
    ) -> &'a mut RootHandle<T> {
        self.0.rw(owner)
    }

    pub(crate) fn borrow_mut2<'a, U>(
        gc1: &'a Self,
        gc2: &'a Root<'id, U>,
        owner: &'a mut LCellOwner<'id>,
        _: &'a Arena,
    ) -> (&'a mut RootHandle<T>, &'a mut RootHandle<U>) {
        owner.rw2(&gc1.0, &gc2.0)
    }

    pub(crate) unsafe fn borrow_mut_unchecked2<'a, U>(
        gc1: &'a Self,
        gc2: &'a Root<'id, U>,
        owner: &'a mut LCellOwner<'id>,
    ) -> (&'a mut RootHandle<T>, &'a mut RootHandle<U>) {
        owner.rw2(&gc1.0, &gc2.0)
    }
}

#[macro_export]
macro_rules! root_struct {
    ($ident:ident, $value:expr, $arena:ident) => {
        let mut $ident = unsafe { $crate::arena::Root::new($value) };
        let mut root = unsafe { $crate::arena::RootStruct::new($arena.get_root_set()) };
        let $ident = root.set(&mut $ident);
    };
}

#[repr(transparent)]
#[derive(Debug)]
pub(crate) struct RootHandle<T: ?Sized> {
    inner: T,
}

impl<T: PartialEq> PartialEq<T> for RootHandle<T> {
    fn eq(&self, other: &T) -> bool {
        self.inner == *other
    }
}

impl<T> AsRef<T> for RootHandle<T> {
    fn as_ref(&self) -> &T {
        &self.inner
    }
}

impl<T> RootHandle<T> {
    fn new(data: T) -> Self {
        RootHandle { inner: data }
    }
}

impl RootHandle<RootObj> {
    pub(crate) fn obj(&self) -> Object {
        unsafe { Object::from_raw(self.inner.obj) }
    }

    pub(crate) fn bind<'ob>(&self, gc: &'ob Arena) -> Object<'ob> {
        unsafe { gc.bind(Object::from_raw(self.inner.obj)) }
    }

    pub(crate) fn set(&mut self, item: Object<'_>) {
        self.inner.obj = item.into();
    }
}

impl<'ob> AsRef<Object<'ob>> for RootHandle<RootObj> {
    fn as_ref(&self) -> &Object<'ob> {
        unsafe { &*(self as *const Self).cast::<Object>() }
    }
}

impl<'ob> AsRef<[Object<'ob>]> for RootHandle<[RootObj]> {
    fn as_ref(&self) -> &[Object<'ob>] {
        let ptr = self.inner.as_ptr().cast::<Object>();
        let len = self.inner.len();
        unsafe { std::slice::from_raw_parts(ptr, len) }
    }
}

impl RootHandle<RootCons> {
    // TODO: remove this method and implement Deref with lifetime parameter is removed
    pub(crate) fn obj<'ob>(&'ob self) -> &'ob Cons<'ob> {
        unsafe { std::mem::transmute::<&Cons, &'ob Cons<'ob>>(&*(self.inner.obj)) }
    }

    pub(crate) fn set(&mut self, item: &Cons) {
        self.inner.obj = unsafe { std::mem::transmute(item) }
    }
}

impl<'ob> AsRef<Cons<'ob>> for RootHandle<RootCons> {
    fn as_ref(&self) -> &Cons<'ob> {
        unsafe { &*(self as *const Self).cast::<Cons>() }
    }
}

impl<'ob> AsRef<[Cons<'ob>]> for RootHandle<[RootCons]> {
    fn as_ref(&self) -> &[Cons<'ob>] {
        let ptr = self.inner.as_ptr().cast::<Cons>();
        let len = self.inner.len();
        unsafe { std::slice::from_raw_parts(ptr, len) }
    }
}

impl<T, U> Deref for RootHandle<(T, U)> {
    type Target = (RootHandle<T>, RootHandle<U>);

    fn deref(&self) -> &Self::Target {
        unsafe { &*(self as *const RootHandle<(T, U)>).cast::<(RootHandle<T>, RootHandle<U>)>() }
    }
}

impl<T, U> DerefMut for RootHandle<(T, U)> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *(self as *mut RootHandle<(T, U)>).cast::<(RootHandle<T>, RootHandle<U>)>() }
    }
}

impl<T> Deref for RootHandle<Option<T>> {
    type Target = Option<RootHandle<T>>;

    fn deref(&self) -> &Self::Target {
        unsafe { &*(self as *const RootHandle<Option<T>>).cast::<Option<RootHandle<T>>>() }
    }
}

impl<T> DerefMut for RootHandle<Option<T>> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *(self as *mut RootHandle<Option<T>>).cast::<Option<RootHandle<T>>>() }
    }
}

impl RootHandle<Option<RootObj>> {
    pub(crate) fn set(&mut self, obj: Object) {
        self.inner = Some(RootObj::new(obj));
    }
}

impl<T, I: SliceIndex<[T]>> Index<I> for RootHandle<Vec<T>> {
    type Output = RootHandle<I::Output>;

    fn index(&self, index: I) -> &Self::Output {
        unsafe {
            &*(Index::index(&self.inner, index) as *const I::Output as *const RootHandle<I::Output>)
        }
    }
}

impl<T, I: SliceIndex<[T]>> IndexMut<I> for RootHandle<Vec<T>> {
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        unsafe {
            &mut *(IndexMut::index_mut(&mut self.inner, index) as *mut I::Output
                as *mut RootHandle<I::Output>)
        }
    }
}

impl<T, I: SliceIndex<[T]>> Index<I> for RootHandle<[T]> {
    type Output = RootHandle<I::Output>;

    fn index(&self, index: I) -> &Self::Output {
        unsafe {
            &*(Index::index(&self.inner, index) as *const I::Output as *const RootHandle<I::Output>)
        }
    }
}

impl<T, I: SliceIndex<[T]>> IndexMut<I> for RootHandle<[T]> {
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        unsafe {
            &mut *(IndexMut::index_mut(&mut self.inner, index) as *mut I::Output
                as *mut RootHandle<I::Output>)
        }
    }
}

impl<T> RootHandle<Vec<T>> {
    pub(crate) fn as_slice(&self) -> &[RootHandle<T>] {
        // SAFETY: `Gc<T>` has the same memory layout as `T`.
        unsafe { &*(self.inner.as_slice() as *const [T] as *const [RootHandle<T>]) }
    }

    pub(crate) fn as_gc(&self) -> &RootHandle<[T]> {
        // SAFETY: `Gc<T>` has the same memory layout as `T`.
        unsafe { &*(self.inner.as_slice() as *const [T] as *const RootHandle<[T]>) }
    }

    pub(crate) fn as_mut_slice(&mut self) -> &mut [RootHandle<T>] {
        // SAFETY: `Gc<T>` has the same memory layout as `T`.
        unsafe { &mut *(self.inner.as_mut_slice() as *mut [T] as *mut [RootHandle<T>]) }
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

impl<T> Deref for RootHandle<Vec<T>> {
    type Target = [RootHandle<T>];

    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

impl<T> DerefMut for RootHandle<Vec<T>> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_mut_slice()
    }
}

impl<K, V> RootHandle<HashMap<K, V>>
where
    K: Eq + std::hash::Hash,
{
    pub(crate) fn get<Q: ?Sized>(&self, k: &Q) -> Option<&RootHandle<V>>
    where
        K: std::borrow::Borrow<Q>,
        Q: std::hash::Hash + Eq,
    {
        self.inner
            .get(k)
            .map(|v| unsafe { &*(v as *const V).cast::<RootHandle<V>>() })
    }

    pub(crate) fn get_mut<Q: ?Sized>(&mut self, k: &Q) -> Option<&mut RootHandle<V>>
    where
        K: std::borrow::Borrow<Q>,
        Q: std::hash::Hash + Eq,
    {
        self.inner
            .get_mut(k)
            .map(|v| unsafe { &mut *(v as *mut V).cast::<RootHandle<V>>() })
    }

    pub(crate) fn insert<R: IntoRoot<V>>(&mut self, k: K, v: R) {
        self.inner.insert(k, unsafe { v.into_root() });
    }
}

type Prop = RootHandle<HashMap<Symbol, Vec<(Symbol, RootObj)>>>;
impl RootHandle<Environment> {
    pub(crate) fn vars(&self) -> &RootHandle<HashMap<Symbol, RootObj>> {
        unsafe { &*(&self.inner.vars as *const HashMap<_, _>).cast() }
    }

    pub(crate) fn vars_mut(&mut self) -> &mut RootHandle<HashMap<Symbol, RootObj>> {
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
    use crate::arena::RootSet;

    use super::*;

    #[test]
    fn indexing() {
        let root = &RootSet::default();
        let arena = &Arena::new(root);
        let mut vec: RootHandle<Vec<RootObj>> = RootHandle { inner: vec![] };

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
