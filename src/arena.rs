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

impl<'ob> Arena {
    pub(crate) const fn new() -> Self {
        Arena {
            objects: RefCell::new(Vec::new()),
        }
    }

    fn register(&'ob self, obj: OwnedObject<'ob>) {
        self.objects
            .borrow_mut()
            .push(unsafe { obj.coerce_lifetime() });
    }

    pub(crate) fn alloc_f64(&'ob self, x: f64) -> &'ob f64 {
        self.register(OwnedObject::Float(Box::new(x)));
        if let Some(OwnedObject::Float(x)) = self.objects.borrow().last() {
            unsafe { std::mem::transmute::<&f64, &'ob f64>(x.as_ref()) }
        } else {
            unreachable!("object was not the type we just inserted");
        }
    }

    pub(crate) fn alloc_cons(&'ob self, x: Cons<'ob>) -> &'ob Cons<'ob> {
        self.register(OwnedObject::Cons(Box::new(x)));
        if let Some(OwnedObject::Cons(x)) = self.objects.borrow().last() {
            unsafe { std::mem::transmute::<&Cons, &'ob Cons>(x.as_ref()) }
        } else {
            unreachable!("object was not the type we just inserted");
        }
    }

    pub(crate) fn alloc_string(&'ob self, x: String) -> &'ob String {
        self.register(OwnedObject::String(Box::new(x)));
        if let Some(OwnedObject::String(x)) = self.objects.borrow().last() {
            unsafe { std::mem::transmute::<&String, &'ob String>(x.as_ref()) }
        } else {
            unreachable!("object was not the type we just inserted");
        }
    }

    pub(crate) fn alloc_vec(&'ob self, x: Vec<Object<'ob>>) -> &'ob Vec<Object<'ob>> {
        self.register(OwnedObject::Vec(Box::new(x)));
        if let Some(OwnedObject::Vec(x)) = self.objects.borrow().last() {
            unsafe { std::mem::transmute::<&Vec<Object>, &'ob Vec<Object>>(x.as_ref()) }
        } else {
            unreachable!("object was not the type we just inserted");
        }
    }

    pub(crate) fn alloc_lisp_fn(&'ob self, x: LispFn<'ob>) -> &'ob LispFn<'ob> {
        self.register(OwnedObject::LispFn(Box::new(x)));
        if let Some(OwnedObject::LispFn(x)) = self.objects.borrow().last() {
            unsafe { std::mem::transmute::<&LispFn, &'ob LispFn>(x.as_ref()) }
        } else {
            unreachable!("object was not the type we just inserted");
        }
    }

    pub(crate) fn alloc_subr_fn(&'ob self, x: SubrFn) -> &'ob SubrFn {
        self.register(OwnedObject::SubrFn(Box::new(x)));
        if let Some(OwnedObject::SubrFn(x)) = self.objects.borrow().last() {
            unsafe { std::mem::transmute::<&SubrFn, &'ob SubrFn>(x.as_ref()) }
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
