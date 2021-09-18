use crate::cons::Cons;
use crate::object::{IntoObject, LispFn, Object, SubrFn};
use std::cell::RefCell;

#[derive(Debug, PartialEq)]
pub(crate) struct Arena {
    objects: RefCell<Vec<OwnedObject<'static>>>,
}

#[derive(Debug, PartialEq)]
enum OwnedObject<'ob> {
    Float(Box<f64>),
    Cons(Box<Cons<'ob>>),
    Vec(Box<Vec<Object<'ob>>>),
    String(Box<String>),
    LispFn(Box<LispFn<'ob>>),
    SubrFn(Box<SubrFn>),
}

impl<'ob> OwnedObject<'ob> {
    unsafe fn coerce_lifetime(self) -> OwnedObject<'static> {
        std::mem::transmute::<OwnedObject<'ob>, OwnedObject<'static>>(self)
    }
}

// This is safe here because we will never return mutable overlapping borrows
#[allow(clippy::mut_from_ref)]
impl<'ob> Arena {
    pub(crate) const fn new() -> Self {
        Arena {
            objects: RefCell::new(Vec::new()),
        }
    }

    fn register(objects: &mut Vec<OwnedObject<'static>>, obj: OwnedObject<'ob>) {
        objects.push(unsafe { obj.coerce_lifetime() });
    }

    pub(crate) fn alloc_f64(&'ob self, x: f64) -> &'ob mut f64 {
        let mut objects = self.objects.borrow_mut();
        Self::register(&mut objects, OwnedObject::Float(Box::new(x)));
        if let Some(OwnedObject::Float(x)) = objects.last_mut() {
            unsafe { std::mem::transmute::<&mut f64, &'ob mut f64>(x.as_mut()) }
        } else {
            unreachable!("object was not the type we just inserted");
        }
    }

    pub(crate) fn alloc_cons(&'ob self, x: Cons<'ob>) -> &'ob mut Cons<'ob> {
        let mut objects = self.objects.borrow_mut();
        Self::register(&mut objects, OwnedObject::Cons(Box::new(x)));
        if let Some(OwnedObject::Cons(x)) = objects.last_mut() {
            unsafe { std::mem::transmute::<&mut Cons, &'ob mut Cons>(x.as_mut()) }
        } else {
            unreachable!("object was not the type we just inserted");
        }
    }

    pub(crate) fn alloc_string(&'ob self, x: String) -> &'ob mut String {
        let mut objects = self.objects.borrow_mut();
        Self::register(&mut objects, OwnedObject::String(Box::new(x)));
        if let Some(OwnedObject::String(x)) = objects.last_mut() {
            unsafe { std::mem::transmute::<&mut String, &'ob mut String>(x.as_mut()) }
        } else {
            unreachable!("object was not the type we just inserted");
        }
    }

    pub(crate) fn alloc_vec(&'ob self, x: Vec<Object<'ob>>) -> &'ob mut Vec<Object<'ob>> {
        let mut objects = self.objects.borrow_mut();
        Self::register(&mut objects, OwnedObject::Vec(Box::new(x)));
        if let Some(OwnedObject::Vec(x)) = objects.last_mut() {
            unsafe { std::mem::transmute::<&mut Vec<Object>, &'ob mut Vec<Object>>(x.as_mut()) }
        } else {
            unreachable!("object was not the type we just inserted");
        }
    }

    pub(crate) fn alloc_lisp_fn(&'ob self, x: LispFn<'ob>) -> &'ob mut LispFn<'ob> {
        let mut objects = self.objects.borrow_mut();
        Self::register(&mut objects, OwnedObject::LispFn(Box::new(x)));
        if let Some(OwnedObject::LispFn(x)) = objects.last_mut() {
            unsafe { std::mem::transmute::<&mut LispFn, &'ob mut LispFn>(x.as_mut()) }
        } else {
            unreachable!("object was not the type we just inserted");
        }
    }

    pub(crate) fn alloc_subr_fn(&'ob self, x: SubrFn) -> &'ob mut SubrFn {
        let mut objects = self.objects.borrow_mut();
        Self::register(&mut objects, OwnedObject::SubrFn(Box::new(x)));
        if let Some(OwnedObject::SubrFn(x)) = objects.last_mut() {
            unsafe { std::mem::transmute::<&mut SubrFn, &'ob mut SubrFn>(x.as_mut()) }
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
