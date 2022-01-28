#![allow(dead_code)]
use crate::cons::Cons;
use crate::object::{Bits, IntoObject, LispFn, Object, SubrFn};
use anyhow::Result;
use std::cell::{Cell, RefCell};
use std::fmt::Debug;
use std::mem::transmute;

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

impl<'old, 'new, T> ConstrainLifetime<'new, &'new T> for &'old T {
    fn constrain_lifetime(self, _cx: &'new Arena) -> &'new T {
        unsafe { transmute::<&'old T, &'new T>(self) }
    }
}

#[repr(transparent)]
#[derive(Debug, PartialEq)]
pub(crate) struct GcRoot {
    inner: Object<'static>,
}

impl GcRoot {
    pub(crate) unsafe fn new<'ob>(obj: Object<'ob>) -> Self {
        Self {
            inner: transmute::<Object<'ob>, Object<'static>>(obj),
        }
    }

    pub(crate) fn get<'ob>(&self, cx: &'ob Arena) -> Object<'ob> {
        self.inner.constrain_lifetime(cx)
    }

    pub(crate) fn as_gc<'ob>(&'ob self) -> Gc<Object<'ob>> {
        Gc {
            inner: unsafe { transmute::<Object<'static>, Object<'ob>>(self.inner) },
        }
    }

    pub(crate) fn as_gc_slice<'ob>(slice: &'ob [Self]) -> &'ob [Gc<Object<'ob>>] {
        let ptr = slice.as_ptr().cast::<Gc<Object<'ob>>>();
        let len = slice.len();
        unsafe { std::slice::from_raw_parts(ptr, len) }
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

#[derive(Copy, Clone)]
pub(crate) struct Gc<T: Copy> {
    inner: T,
}

impl<'ob, T: Copy> Gc<T> {
    pub(crate) fn get<U: 'ob>(self, cx: &'ob Arena) -> U
    where
        T: ConstrainLifetime<'ob, U>,
    {
        self.inner.constrain_lifetime(cx)
    }

    pub(crate) fn try_convert<U: Copy>(self) -> Result<Gc<U>>
    where
        U: TryFrom<T, Error = anyhow::Error>,
    {
        self.inner.try_into().map(|x| Gc { inner: x })
    }
}

impl<T: Copy> Bits for Gc<T>
where
    T: Bits,
{
    fn bits(self) -> u64 {
        self.inner.bits()
    }
}

/// Owns all allocations and creates objects. All objects have
/// a lifetime tied to the borrow of their `Arena`. When the
/// `Arena` goes out of scope, no objects should be accessible.
/// Interior mutability is used to ensure that `&mut` references
/// don't invalid objects.
#[derive(Debug, PartialEq)]
pub(crate) struct Arena {
    objects: RefCell<Vec<OwnedObject<'static>>>,
    is_const: bool,
}

/// The owner of an object allocation. No references to
/// the object can outlive this. This type should not be
/// public, but needs to be marked that way so it can be
/// part of a generic public interface. So long as no
/// functions in this module reference it that should not
/// cause a problem.
#[derive(Debug, PartialEq)]
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

// This is safe here because we will never return mutable overlapping borrows
#[allow(clippy::mut_from_ref)]
impl<'ob> Arena {
    pub(crate) const fn new() -> Self {
        Arena {
            objects: RefCell::new(Vec::new()),
            is_const: false,
        }
    }

    pub(crate) const fn new_const() -> Self {
        Arena {
            objects: RefCell::new(Vec::new()),
            is_const: true,
        }
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
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_gc() {
        let arena = &Arena::new();
        let mut obj = Object::NIL;
        println!("{}", obj);
        {
            let inner = arena.add("foo");
            let root = unsafe { GcRoot::new(inner) };
            let rf = root.as_gc();
            obj = rf.get(arena);
        }
        println!("{}", obj);
        if let Object::String(x) = obj {
            assert_eq!(!x, "foo");
        } else {
            unreachable!();
        }
    }
}
