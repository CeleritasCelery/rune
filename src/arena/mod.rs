#![allow(dead_code)]
use crate::cons::Cons;
use crate::object::{IntoObject, LispFn, Object, RawObj, SubrFn};
use std::cell::{Cell, RefCell};
use std::fmt::Debug;
use std::mem::transmute;
use std::ops::Deref;

mod root;
pub(crate) use root::*;

pub(crate) trait ConstrainLifetime<'new, T> {
    fn constrain_lifetime(self, cx: &'new Arena) -> T;
}

impl<'old, 'new> ConstrainLifetime<'new, Object<'new>> for Object<'old> {
    fn constrain_lifetime(self, _cx: &'new Arena) -> Object<'new> {
        unsafe { transmute::<Object<'old>, Object<'new>>(self) }
    }
}

impl<'old, 'new, 'brw> ConstrainLifetime<'new, &'brw [Object<'new>]> for &'brw [Object<'old>] {
    fn constrain_lifetime(self, _cx: &'new Arena) -> &'brw [Object<'new>] {
        unsafe { transmute::<&[Object<'old>], &[Object<'new>]>(self) }
    }
}

pub(crate) struct StackRoot<'rt> {
    root_set: &'rt RootSet,
}

pub(crate) struct CellRoot<'rt> {
    roots: &'rt RootSet,
    object: Cell<u64>,
}

impl<'rt> StackRoot<'rt> {
    pub(crate) unsafe fn new(roots: &'rt RootSet) -> Self {
        StackRoot { root_set: roots }
    }

    pub(crate) fn set<'root>(&'root mut self, obj: Object<'_>) -> Object<'root> {
        unsafe {
            self.root_set
                .roots
                .borrow_mut()
                .push(transmute::<Object, Object<'static>>(obj));
            transmute::<Object, Object<'root>>(obj)
        }
    }
}

impl<'rt> Drop for StackRoot<'rt> {
    fn drop(&mut self) {
        self.root_set.roots.borrow_mut().pop();
    }
}

#[macro_export]
macro_rules! root {
    ($obj:ident, $arena:ident) => {
        let mut root = unsafe { crate::arena::StackRoot::new($arena.get_root_set()) };
        let $obj = root.set($obj);
    };
}

#[macro_export]
macro_rules! rebind {
    ($item:ident, $arena:ident) => {
        #[allow(unused_qualifications)]
        let bits: crate::object::RawObj = $item.into();
        #[allow(clippy::shadow_unrelated)]
        let $item = unsafe { $arena.rebind_raw_ptr(bits) };
    };
}

#[derive(Default, Debug)]
pub(crate) struct RootSet {
    roots: RefCell<Vec<Object<'static>>>,
}

use std::sync::atomic::AtomicBool;

thread_local! {
    pub(crate) static SINGLETON_CHECK: AtomicBool = AtomicBool::new(false);
}

/// Owns all allocations and creates objects. All objects have
/// a lifetime tied to the borrow of their `Arena`. When the
/// `Arena` goes out of scope, no objects should be accessible.
/// Interior mutability is used to ensure that `&mut` references
/// don't invalid objects.
#[derive(Debug)]
pub(crate) struct Arena<'rt> {
    objects: RefCell<Vec<OwnedObject<'static>>>,
    roots: &'rt RootSet,
    is_const: bool,
}

/// The owner of an object allocation. No references to
/// the object can outlive this.
#[derive(Debug)]
enum OwnedObject<'ob> {
    Float(Box<Allocation<f64>>),
    Cons(Box<Cons<'ob>>),
    Vec(Box<Allocation<RefCell<Vec<Object<'ob>>>>>),
    String(Box<Allocation<String>>),
    LispFn(Box<Allocation<LispFn<'ob>>>),
    SubrFn(Box<SubrFn>),
}

#[derive(Debug, PartialEq)]
pub(crate) struct Allocation<T> {
    marked: Cell<bool>,
    data: T,
}

impl<T> Allocation<T> {
    fn new(data: T) -> Self {
        Allocation {
            marked: Cell::from(false),
            data,
        }
    }
}

impl<T> Deref for Allocation<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<'ob> OwnedObject<'ob> {
    unsafe fn coerce_lifetime(self) -> OwnedObject<'static> {
        transmute::<OwnedObject<'ob>, OwnedObject<'static>>(self)
    }
}

#[cfg(miri)]
extern "Rust" {
    fn miri_static_root(ptr: *const u8);
}

// This is safe here because we will never return mutable overlapping borrows
#[allow(clippy::mut_from_ref)]
impl<'ob, 'rt> Arena<'rt> {
    pub(crate) fn new(roots: &'rt RootSet) -> Self {
        use std::sync::atomic::Ordering::Relaxed;
        SINGLETON_CHECK.with(|x| {
            assert!(
                x.compare_exchange(false, true, Relaxed, Relaxed).is_ok(),
                "There was already and active arena when this arena was created"
            );
        });
        Arena {
            objects: RefCell::new(Vec::new()),
            roots,
            is_const: false,
        }
    }

    pub(crate) const fn new_const(roots: &'rt RootSet) -> Self {
        Arena {
            objects: RefCell::new(Vec::new()),
            roots,
            is_const: true,
        }
    }

    #[cfg(miri)]
    pub(crate) unsafe fn mark_static(&self) {
        let ptr = self.objects.borrow().as_ptr();
        miri_static_root(ptr as _);
    }

    fn register(objects: &mut Vec<OwnedObject<'static>>, obj: OwnedObject<'ob>) {
        objects.push(unsafe { obj.coerce_lifetime() });
    }

    pub(crate) fn alloc_f64(&'ob self, obj: f64) -> &'ob Allocation<f64> {
        let mut objects = self.objects.borrow_mut();
        Self::register(
            &mut objects,
            OwnedObject::Float(Box::new(Allocation::new(obj))),
        );
        if let Some(OwnedObject::Float(x)) = objects.last() {
            unsafe { &*(x.as_ref() as *const Allocation<f64>) }
        } else {
            unreachable!("object was not the type we just inserted");
        }
    }

    pub(crate) fn alloc_cons(&'ob self, mut obj: Cons<'ob>) -> &'ob Cons<'ob> {
        if self.is_const {
            obj.make_const();
        }
        let mut objects = self.objects.borrow_mut();
        Self::register(&mut objects, OwnedObject::Cons(Box::new(obj)));
        if let Some(OwnedObject::Cons(x)) = objects.last() {
            unsafe { transmute::<&Cons, &'ob Cons>(x.as_ref()) }
        } else {
            unreachable!("object was not the type we just inserted");
        }
    }

    pub(crate) fn alloc_string(&'ob self, obj: String) -> &'ob Allocation<String> {
        let mut objects = self.objects.borrow_mut();
        Self::register(
            &mut objects,
            OwnedObject::String(Box::new(Allocation::new(obj))),
        );
        if let Some(OwnedObject::String(x)) = objects.last_mut() {
            unsafe { &*(x.as_ref() as *const Allocation<_>) }
        } else {
            unreachable!("object was not the type we just inserted");
        }
    }

    pub(crate) fn alloc_vec(
        &'ob self,
        obj: Vec<Object<'ob>>,
    ) -> &'ob Allocation<RefCell<Vec<Object<'ob>>>> {
        let mut objects = self.objects.borrow_mut();
        let ref_cell = RefCell::new(obj);
        if self.is_const {
            // Leak a borrow so that the vector cannot be borrowed mutably
            std::mem::forget(ref_cell.borrow());
        }
        Self::register(
            &mut objects,
            OwnedObject::Vec(Box::new(Allocation::new(ref_cell))),
        );
        if let Some(OwnedObject::Vec(x)) = objects.last() {
            unsafe { transmute::<&Allocation<_>, &'ob Allocation<_>>(x.as_ref()) }
        } else {
            unreachable!("object was not the type we just inserted");
        }
    }

    pub(crate) fn alloc_lisp_fn(&'ob self, obj: LispFn<'ob>) -> &'ob Allocation<LispFn<'ob>> {
        let mut objects = self.objects.borrow_mut();
        Self::register(
            &mut objects,
            OwnedObject::LispFn(Box::new(Allocation::new(obj))),
        );
        if let Some(OwnedObject::LispFn(x)) = objects.last() {
            unsafe { transmute::<&Allocation<LispFn>, &'ob Allocation<LispFn>>(x.as_ref()) }
        } else {
            unreachable!("object was not the type we just inserted");
        }
    }

    pub(crate) fn alloc_subr_fn(&'ob self, obj: SubrFn) -> &'ob SubrFn {
        assert!(self.is_const, "Attempt to add subrFn to non-const arena");
        let mut objects = self.objects.borrow_mut();
        Self::register(&mut objects, OwnedObject::SubrFn(Box::new(obj)));
        if let Some(OwnedObject::SubrFn(x)) = objects.last() {
            unsafe { &*(x.as_ref() as *const SubrFn) }
        } else {
            unreachable!("object was not the type we just inserted");
        }
    }

    pub(crate) fn add<Input>(&'ob self, item: Input) -> Object<'ob>
    where
        Input: IntoObject<'ob, Object<'ob>>,
    {
        item.into_obj(self)
    }

    pub(crate) fn bind<T, U>(&'ob self, obj: T) -> U
    where
        T: ConstrainLifetime<'ob, U>,
    {
        obj.constrain_lifetime(self)
    }

    #[allow(clippy::unused_self)]
    pub(crate) unsafe fn rebind_raw_ptr(&'ob self, raw: RawObj) -> Object<'ob> {
        Object::from_raw(raw)
    }

    pub(crate) unsafe fn get_root_set(&'ob self) -> &'rt RootSet {
        self.roots
    }
}

impl<'rt> Drop for Arena<'rt> {
    fn drop(&mut self) {
        use std::sync::atomic::Ordering::Relaxed;
        SINGLETON_CHECK.with(|s| s.store(false, Relaxed));
    }
}

#[cfg(test)]
mod test {
    use super::*;
    fn take_mut_arena(_: &mut Arena) {}
    fn bind_to_mut<'ob>(arena: &'ob mut Arena) -> Object<'ob> {
        arena.add("invariant")
    }

    #[test]
    fn test_stack_root() {
        let roots = &RootSet::default();
        let mut arena = Arena::new(roots);
        let obj = "foo".into_obj(&arena);
        root!(obj, arena);
        take_mut_arena(&mut arena);
        assert_eq!(obj, "foo");
    }

    #[test]
    fn test_reborrow() {
        let roots = &RootSet::default();
        let mut arena = Arena::new(roots);
        let obj = bind_to_mut(&mut arena);
        rebind!(obj, arena);
        let _ = "foo".into_obj(&arena);
        assert_eq!(obj, "invariant");
    }
}
