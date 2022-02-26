#![allow(dead_code)]
use crate::cons::Cons;
use crate::object::{IntoObject, LispFn, Object, RawObj, SubrFn};
use std::cell::{Cell, RefCell};
use std::fmt::Debug;
use std::mem::transmute;

mod root;
pub(crate) use root::*;

pub(crate) trait ConstrainLifetime<'new, T>
where
    T: 'new,
{
    fn constrain_lifetime(self, cx: &'new Arena) -> T;
}

impl<'old, 'new> ConstrainLifetime<'new, Object<'new>> for Object<'old> {
    fn constrain_lifetime(self, _cx: &'new Arena) -> Object<'new> {
        unsafe { transmute::<Object<'old>, Object<'new>>(self) }
    }
}

#[repr(transparent)]
pub(crate) struct GcCell {
    inner: Cell<Object<'static>>,
}

impl GcCell {
    pub(crate) unsafe fn new<'ob>(obj: Object<'ob>) -> Self {
        Self {
            inner: Cell::new(transmute::<Object<'ob>, Object<'static>>(obj)),
        }
    }

    pub(crate) fn get<'ob>(&self, cx: &'ob Arena) -> Object<'ob> {
        self.inner.get().constrain_lifetime(cx)
    }

    pub(crate) fn update<'ob>(&self, obj: Object<'ob>) {
        self.inner
            .set(unsafe { transmute::<Object<'ob>, Object<'static>>(obj) });
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
        let bits: RawObj = $item.into();
        #[allow(clippy::shadow_unrelated)]
        let $item = unsafe { $arena.rebind_raw_ptr(bits) };
    };
}

#[derive(Default, Debug)]
pub(crate) struct RootSet {
    roots: RefCell<Vec<Object<'static>>>,
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
    Float(Box<f64>),
    Cons(Box<Cons<'ob>>),
    Vec(Box<RefCell<Vec<Object<'ob>>>>),
    String(Box<String>),
    LispFn(Box<LispFn<'ob>>),
    SubrFn(Box<SubrFn>),
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
    pub(crate) const fn new(roots: &'rt RootSet) -> Self {
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

    pub(crate) fn alloc_f64(&'ob self, obj: f64) -> &'ob mut f64 {
        let mut objects = self.objects.borrow_mut();
        Self::register(&mut objects, OwnedObject::Float(Box::new(obj)));
        if let Some(OwnedObject::Float(x)) = objects.last_mut() {
            unsafe { transmute::<&mut f64, &'ob mut f64>(x.as_mut()) }
        } else {
            unreachable!("object was not the type we just inserted");
        }
    }

    pub(crate) fn alloc_cons(&'ob self, mut obj: Cons<'ob>) -> &'ob mut Cons<'ob> {
        if self.is_const {
            obj.make_const();
        }
        let mut objects = self.objects.borrow_mut();
        Self::register(&mut objects, OwnedObject::Cons(Box::new(obj)));
        if let Some(OwnedObject::Cons(x)) = objects.last_mut() {
            unsafe { transmute::<&mut Cons, &'ob mut Cons>(x.as_mut()) }
        } else {
            unreachable!("object was not the type we just inserted");
        }
    }

    pub(crate) fn alloc_string(&'ob self, obj: String) -> &'ob mut String {
        let mut objects = self.objects.borrow_mut();
        Self::register(&mut objects, OwnedObject::String(Box::new(obj)));
        if let Some(OwnedObject::String(x)) = objects.last_mut() {
            unsafe { transmute::<&mut String, &'ob mut String>(x.as_mut()) }
        } else {
            unreachable!("object was not the type we just inserted");
        }
    }

    pub(crate) fn alloc_vec(
        &'ob self,
        obj: Vec<Object<'ob>>,
    ) -> &'ob mut RefCell<Vec<Object<'ob>>> {
        let mut objects = self.objects.borrow_mut();
        let ref_cell = RefCell::new(obj);
        if self.is_const {
            // Leak a borrow so that the vector cannot be borrowed mutably
            std::mem::forget(ref_cell.borrow());
        }
        Self::register(&mut objects, OwnedObject::Vec(Box::new(ref_cell)));
        if let Some(OwnedObject::Vec(x)) = objects.last_mut() {
            unsafe {
                transmute::<&mut RefCell<Vec<Object>>, &'ob mut RefCell<Vec<Object>>>(x.as_mut())
            }
        } else {
            unreachable!("object was not the type we just inserted");
        }
    }

    pub(crate) fn alloc_lisp_fn(&'ob self, obj: LispFn<'ob>) -> &'ob mut LispFn<'ob> {
        let mut objects = self.objects.borrow_mut();
        Self::register(&mut objects, OwnedObject::LispFn(Box::new(obj)));
        if let Some(OwnedObject::LispFn(x)) = objects.last_mut() {
            unsafe { transmute::<&mut LispFn, &'ob mut LispFn>(x.as_mut()) }
        } else {
            unreachable!("object was not the type we just inserted");
        }
    }

    pub(crate) fn alloc_subr_fn(&'ob self, obj: SubrFn) -> &'ob mut SubrFn {
        let mut objects = self.objects.borrow_mut();
        Self::register(&mut objects, OwnedObject::SubrFn(Box::new(obj)));
        if let Some(OwnedObject::SubrFn(x)) = objects.last_mut() {
            unsafe { transmute::<&mut SubrFn, &'ob mut SubrFn>(x.as_mut()) }
        } else {
            unreachable!("object was not the type we just inserted");
        }
    }

    pub(crate) fn add<Input, Output>(&'ob self, item: Input) -> Output
    where
        Input: IntoObject<'ob, Output>,
    {
        item.into_obj(self)
    }

    pub(crate) fn bind<'a>(&'ob self, obj: Object<'a>) -> Object<'ob> {
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
