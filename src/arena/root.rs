use std::ops::{Deref, DerefMut, IndexMut};
use std::ptr::{addr_of, addr_of_mut};
use std::{ops::Index, slice::SliceIndex};

use crate::cons::Cons;
use crate::data::Environment;
use crate::hashmap::HashMap;
use crate::object::{Object, RawObj};
use crate::symbol::Symbol;
use std::fmt::Debug;

use qcell::{LCell, LCellOwner};

use super::{Arena, RootSet, Trace};

pub(crate) trait IntoRoot<T> {
    unsafe fn into_root(self) -> T;
}

impl<'ob> IntoRoot<RootObj> for Object<'ob> {
    unsafe fn into_root(self) -> RootObj {
        RootObj::new(self)
    }
}

impl IntoRoot<RootCons> for &Cons {
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

#[doc(hidden)]
pub(crate) struct StackRoot<'rt> {
    root_set: &'rt RootSet,
}

impl<'rt> StackRoot<'rt> {
    /// Create a new `StackRoot`
    ///
    /// # Safety
    ///
    /// This function is only safe if [set] is called before use and `StackRoot`
    /// does not move from the stack or drop before it goes out of scope. This
    /// is ensured by a shadow binding in the [root!] macro.
    pub(crate) unsafe fn new(roots: &'rt RootSet) -> Self {
        StackRoot { root_set: roots }
    }

    pub(crate) fn set<'root>(&'root mut self, obj: Object<'_>) -> Object<'root> {
        unsafe {
            self.root_set
                .roots
                .borrow_mut()
                .push(std::intrinsics::transmute::<Object, Object<'static>>(obj));
            std::intrinsics::transmute::<Object, Object<'root>>(obj)
        }
    }
}

impl<'rt> Drop for StackRoot<'rt> {
    // Remove the object bound by this StackRoot from the root set. We know that
    // the top of the stack will correspond to the correct object is the
    // invariants of StackRoot are upheld.
    fn drop(&mut self) {
        self.root_set.roots.borrow_mut().pop();
    }
}

/// Roots an [Object] to the stack. The object will be valid until the end of
/// the current scope. The object's lifetime will no longer be bound to the
/// [Arena].
///
/// # Examples
///
/// ```
/// let object = Object::from(5);
/// root!(object, gc);
/// ```
#[macro_export]
macro_rules! root {
    ($obj:ident, $arena:ident) => {
        let mut root = unsafe { $crate::arena::StackRoot::new($arena.get_root_set()) };
        let $obj = root.set($obj);
    };
}

#[repr(transparent)]
#[derive(Default)]
pub(crate) struct RootObj {
    obj: RawObj,
}

impl RootObj {
    pub(crate) fn new(obj: Object) -> Self {
        Self {
            obj: obj.into_raw(),
        }
    }
}

impl Trace for RootObj {
    fn mark(&self, stack: &mut Vec<RawObj>) {
        let obj = unsafe { Object::from_raw(self.obj) };
        obj.trace_mark(stack);
    }
}

impl Debug for RootObj {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(unsafe { &Object::from_raw(self.obj) }, f)
    }
}

#[repr(transparent)]
#[derive(Debug)]
pub(crate) struct RootCons {
    obj: *const Cons,
}

impl RootCons {
    pub(crate) fn new(obj: &Cons) -> Self {
        Self {
            obj: unsafe { std::mem::transmute::<&Cons, *const Cons>(obj) },
        }
    }
}

impl Trace for RootCons {
    fn mark(&self, stack: &mut Vec<RawObj>) {
        unsafe {
            (*self.obj).mark(stack);
        }
    }
}

pub(crate) struct RootOwner<'id>(LCellOwner<'id>);

impl<'id> RootOwner<'id> {
    pub(crate) fn new(guard: generativity::Guard<'id>) -> Self {
        Self(LCellOwner::new(guard))
    }
}

#[repr(transparent)]
pub(crate) struct Root<'id, T: ?Sized>(LCell<'id, RootRef<T>>);

impl<'id, T> Root<'id, T> {
    /// Create a new Root
    ///
    /// # SAFETY
    ///
    /// This method is only safe to call if Root never moves and drops in stack
    /// order. Use the [`root_struct`] macro.
    pub(crate) unsafe fn new(obj: T) -> Self {
        Root(LCell::new(RootRef::new(obj)))
    }

    pub(super) fn deref(&mut self) -> &T {
        // SAFETY: if we have a &mut self, we know that there are no other
        // owners, so we don't need RootOwner. And we can cast since LCell is
        // repr(transparent)
        unsafe { &*addr_of!(self.0).cast::<T>() }
    }

    pub(crate) fn borrow<'a>(&'a self, owner: &'a RootOwner<'id>) -> &'a RootRef<T> {
        owner.0.ro(&self.0)
    }

    pub(crate) fn borrow_mut<'a>(
        &'a self,
        owner: &'a mut RootOwner<'id>,
        _: &'a Arena,
    ) -> &'a mut RootRef<T> {
        owner.0.rw(&self.0)
    }

    pub(crate) fn borrow_mut2<'a, U>(
        gc1: &'a Self,
        gc2: &'a Root<'id, U>,
        owner: &'a mut RootOwner<'id>,
        _: &'a Arena,
    ) -> (&'a mut RootRef<T>, &'a mut RootRef<U>) {
        owner.0.rw2(&gc1.0, &gc2.0)
    }

    pub(crate) unsafe fn borrow_mut_unchecked2<'a, U>(
        gc1: &'a Self,
        gc2: &'a Root<'id, U>,
        owner: &'a mut RootOwner<'id>,
    ) -> (&'a mut RootRef<T>, &'a mut RootRef<U>) {
        owner.0.rw2(&gc1.0, &gc2.0)
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
pub(crate) struct RootRef<T: ?Sized> {
    inner: T,
}

impl<T: ?Sized + Debug> Debug for RootRef<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.inner, f)
    }
}

impl<T: PartialEq> PartialEq<T> for RootRef<T> {
    fn eq(&self, other: &T) -> bool {
        self.inner == *other
    }
}

impl<T> AsRef<T> for RootRef<T> {
    fn as_ref(&self) -> &T {
        &self.inner
    }
}

impl<T> RootRef<T> {
    fn new(data: T) -> Self {
        RootRef { inner: data }
    }
}

impl RootRef<RootObj> {
    pub(crate) fn obj(&self) -> Object {
        unsafe { Object::from_raw(self.inner.obj) }
    }

    pub(crate) fn bind<'ob>(&self, gc: &'ob Arena) -> Object<'ob> {
        unsafe { gc.bind(Object::from_raw(self.inner.obj)) }
    }

    pub(crate) fn set(&mut self, item: Object<'_>) {
        self.inner.obj = item.into_raw();
    }
}

impl<'ob> AsRef<Object<'ob>> for RootRef<RootObj> {
    fn as_ref(&self) -> &Object<'ob> {
        unsafe { &*(self as *const Self).cast::<Object>() }
    }
}

impl<'ob> AsRef<[Object<'ob>]> for RootRef<[RootObj]> {
    fn as_ref(&self) -> &[Object<'ob>] {
        let ptr = self.inner.as_ptr().cast::<Object>();
        let len = self.inner.len();
        unsafe { std::slice::from_raw_parts(ptr, len) }
    }
}

impl RootRef<RootCons> {
    pub(crate) fn set(&mut self, item: &Cons) {
        self.inner.obj = unsafe { std::mem::transmute(item) }
    }
}

impl Deref for RootRef<RootCons> {
    type Target = Cons;

    fn deref(&self) -> &Self::Target {
        unsafe { &*self.inner.obj }
    }
}

impl AsRef<Cons> for RootRef<RootCons> {
    fn as_ref(&self) -> &Cons {
        unsafe { &*(self as *const Self).cast::<Cons>() }
    }
}

impl AsRef<[Cons]> for RootRef<[RootCons]> {
    fn as_ref(&self) -> &[Cons] {
        let ptr = self.inner.as_ptr().cast::<Cons>();
        let len = self.inner.len();
        unsafe { std::slice::from_raw_parts(ptr, len) }
    }
}

impl<T, U> Deref for RootRef<(T, U)> {
    type Target = (RootRef<T>, RootRef<U>);

    fn deref(&self) -> &Self::Target {
        unsafe { &*(self as *const RootRef<(T, U)>).cast::<(RootRef<T>, RootRef<U>)>() }
    }
}

impl<T, U> DerefMut for RootRef<(T, U)> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *(self as *mut RootRef<(T, U)>).cast::<(RootRef<T>, RootRef<U>)>() }
    }
}

impl<T> Deref for RootRef<Option<T>> {
    type Target = Option<RootRef<T>>;

    fn deref(&self) -> &Self::Target {
        unsafe { &*(self as *const RootRef<Option<T>>).cast::<Option<RootRef<T>>>() }
    }
}

impl<T> DerefMut for RootRef<Option<T>> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *(self as *mut RootRef<Option<T>>).cast::<Option<RootRef<T>>>() }
    }
}

impl RootRef<Option<RootObj>> {
    pub(crate) fn set(&mut self, obj: Object) {
        self.inner = Some(RootObj::new(obj));
    }
}

impl<T, I: SliceIndex<[T]>> Index<I> for RootRef<Vec<T>> {
    type Output = RootRef<I::Output>;

    fn index(&self, index: I) -> &Self::Output {
        unsafe {
            &*(Index::index(&self.inner, index) as *const I::Output as *const RootRef<I::Output>)
        }
    }
}

impl<T, I: SliceIndex<[T]>> IndexMut<I> for RootRef<Vec<T>> {
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        unsafe {
            &mut *(IndexMut::index_mut(&mut self.inner, index) as *mut I::Output
                as *mut RootRef<I::Output>)
        }
    }
}

impl<T, I: SliceIndex<[T]>> Index<I> for RootRef<[T]> {
    type Output = RootRef<I::Output>;

    fn index(&self, index: I) -> &Self::Output {
        unsafe {
            &*(Index::index(&self.inner, index) as *const I::Output as *const RootRef<I::Output>)
        }
    }
}

impl<T, I: SliceIndex<[T]>> IndexMut<I> for RootRef<[T]> {
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        unsafe {
            &mut *(IndexMut::index_mut(&mut self.inner, index) as *mut I::Output
                as *mut RootRef<I::Output>)
        }
    }
}

impl<T> RootRef<Vec<T>> {
    pub(crate) fn as_slice(&self) -> &[RootRef<T>] {
        // SAFETY: `Gc<T>` has the same memory layout as `T`.
        unsafe { &*(self.inner.as_slice() as *const [T] as *const [RootRef<T>]) }
    }

    pub(crate) fn as_gc(&self) -> &RootRef<[T]> {
        // SAFETY: `Gc<T>` has the same memory layout as `T`.
        unsafe { &*(self.inner.as_slice() as *const [T] as *const RootRef<[T]>) }
    }

    pub(crate) fn as_mut_slice(&mut self) -> &mut [RootRef<T>] {
        // SAFETY: `Gc<T>` has the same memory layout as `T`.
        unsafe { &mut *(self.inner.as_mut_slice() as *mut [T] as *mut [RootRef<T>]) }
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

impl<T> Deref for RootRef<Vec<T>> {
    type Target = [RootRef<T>];

    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

impl<T> DerefMut for RootRef<Vec<T>> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_mut_slice()
    }
}

impl<K, V> RootRef<HashMap<K, V>>
where
    K: Eq + std::hash::Hash,
{
    pub(crate) fn get<Q: ?Sized>(&self, k: &Q) -> Option<&RootRef<V>>
    where
        K: std::borrow::Borrow<Q>,
        Q: std::hash::Hash + Eq,
    {
        self.inner
            .get(k)
            .map(|v| unsafe { &*(v as *const V).cast::<RootRef<V>>() })
    }

    pub(crate) fn get_mut<Q: ?Sized>(&mut self, k: &Q) -> Option<&mut RootRef<V>>
    where
        K: std::borrow::Borrow<Q>,
        Q: std::hash::Hash + Eq,
    {
        self.inner
            .get_mut(k)
            .map(|v| unsafe { &mut *(v as *mut V).cast::<RootRef<V>>() })
    }

    pub(crate) fn insert<R: IntoRoot<V>>(&mut self, k: K, v: R) {
        self.inner.insert(k, unsafe { v.into_root() });
    }
}

type Prop = RootRef<HashMap<Symbol, Vec<(Symbol, RootObj)>>>;
impl RootRef<Environment> {
    pub(crate) fn vars(&self) -> &RootRef<HashMap<Symbol, RootObj>> {
        unsafe { &*addr_of!(self.inner.vars).cast() }
    }

    pub(crate) fn vars_mut(&mut self) -> &mut RootRef<HashMap<Symbol, RootObj>> {
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
    use crate::{arena::RootSet, object::ObjectX};

    use super::*;

    #[test]
    fn indexing() {
        let root = &RootSet::default();
        let arena = &Arena::new(root);
        let mut vec: RootRef<Vec<RootObj>> = RootRef { inner: vec![] };

        vec.push(Object::NIL);
        assert!(matches!(vec[0].obj().get(), ObjectX::Nil));
        let str1 = arena.add("str1");
        let str2 = arena.add("str2");
        vec.push(str1);
        vec.push(str2);
        let slice = &vec[0..3];
        assert_eq!(vec![Object::NIL, str1, str2], slice.as_ref());
    }
}
