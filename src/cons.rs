use crate::arena::{Arena, Block, ConstrainLifetime};
use crate::object::{List, Object, RawObj};
use anyhow::Result;
use fn_macros::defun;
use std::cell::Cell;
use std::fmt::{self, Debug, Display, Write};

mod iter;

pub(crate) use iter::*;

pub(crate) struct Cons {
    marked: Cell<bool>,
    car: Cell<RawObj>,
    cdr: Cell<RawObj>,
}

impl PartialEq for Cons {
    fn eq(&self, other: &Self) -> bool {
        self.__car() == other.__car() && self.__cdr() == other.__cdr()
    }
}

#[derive(Debug, Default)]
pub(crate) struct ConstConsError();

impl Display for ConstConsError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Attempt to mutate constant cons cell")
    }
}

impl std::error::Error for ConstConsError {}

impl<'new> Cons {
    pub(crate) fn clone_in<const C: bool>(&self, bk: &'new Block<C>) -> Cons {
        // TODO: this is not sound because we return a Cons directly
        unsafe { Cons::new(self.__car().clone_in(bk), self.__cdr().clone_in(bk)) }
    }
}

impl Cons {
    // SAFETY: Cons must always be allocated in the GC heap, it cannot live on
    // the stack. Otherwise it could outlive it's objects since it has no
    // lifetimes.
    pub(crate) unsafe fn new(car: Object, cdr: Object) -> Self {
        Self {
            marked: Cell::new(false),
            car: Cell::new(car.into()),
            cdr: Cell::new(cdr.into()),
        }
    }

    pub(crate) fn car<'new, const C: bool>(&self, cx: &'new Block<C>) -> Object<'new> {
        self.__car().constrain_lifetime(cx)
    }

    pub(crate) fn cdr<'new, const C: bool>(&self, cx: &'new Block<C>) -> Object<'new> {
        self.__cdr().constrain_lifetime(cx)
    }

    // Private internal function to get car/cdr without arena
    fn __car(&self) -> Object {
        unsafe { Object::from_raw(self.car.get()) }
    }

    fn __cdr(&self) -> Object {
        unsafe { Object::from_raw(self.cdr.get()) }
    }

    pub(crate) fn set_car(&self, new_car: Object) {
        self.car.set(new_car.into());
    }

    pub(crate) fn set_cdr(&self, new_cdr: Object) {
        self.cdr.set(new_cdr.into());
    }

    pub(crate) fn mark(&self) {
        self.marked.set(true);
        self.__car().mark();
        self.__cdr().mark();
    }

    pub(crate) fn unmark(&self) {
        self.marked.set(false);
    }

    pub(crate) fn is_marked(&self) -> bool {
        self.marked.get()
    }
}

impl<'brw, 'new> ConstrainLifetime<'new, &'new Cons> for &'brw Cons {
    fn constrain_lifetime<const C: bool>(self, _cx: &'new Block<C>) -> &'new Cons {
        unsafe { &*(self as *const Cons) }
    }
}

impl Display for Cons {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_char('(')?;
        print_rest(self, f)
    }
}

impl Debug for Cons {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_char('(')?;
        print_rest_debug(self, f)
    }
}

fn print_rest(cons: &Cons, f: &mut fmt::Formatter) -> fmt::Result {
    let car = cons.__car();
    match cons.__cdr() {
        Object::Cons(cdr) => {
            write!(f, "{car} ")?;
            print_rest(&cdr, f)
        }
        Object::Nil(_) => write!(f, "{car})"),
        cdr => write!(f, "{car} . {cdr})"),
    }
}

fn print_rest_debug(cons: &Cons, f: &mut fmt::Formatter) -> fmt::Result {
    let car = cons.__car();
    match cons.__cdr() {
        Object::Cons(cdr) => {
            write!(f, "{car:?} ")?;
            print_rest(&cdr, f)
        }
        Object::Nil(_) => write!(f, "{car:?})"),
        cdr => write!(f, "{car:?} . {cdr:?})"),
    }
}

define_unbox!(Cons, &'ob Cons);

#[defun]
fn car<'ob>(list: List, arena: &'ob Arena) -> Object<'ob> {
    match list {
        List::Cons(cons) => cons.car(arena),
        List::Nil => Object::NIL,
    }
}

#[defun]
fn cdr<'ob>(list: List, arena: &'ob Arena) -> Object<'ob> {
    match list {
        List::Cons(cons) => cons.cdr(arena),
        List::Nil => Object::NIL,
    }
}

#[defun]
fn car_safe<'ob>(object: Object<'ob>, arena: &'ob Arena) -> Object<'ob> {
    match object {
        Object::Cons(cons) => cons.car(arena),
        _ => Object::NIL,
    }
}

#[defun]
fn cdr_safe<'ob>(object: Object, arena: &'ob Arena) -> Object<'ob> {
    match object {
        Object::Cons(cons) => cons.cdr(arena),
        _ => Object::NIL,
    }
}

#[defun]
fn setcar<'ob>(cell: &'ob Cons, newcar: Object<'ob>) -> Object<'ob> {
    cell.set_car(newcar);
    newcar
}

#[defun]
fn setcdr<'ob>(cell: &'ob Cons, newcdr: Object<'ob>) -> Object<'ob> {
    cell.set_cdr(newcdr);
    newcdr
}

#[defun]
fn cons<'ob>(car: Object<'ob>, cdr: Object<'ob>, arena: &'ob Arena) -> Object<'ob> {
    crate::cons!(car, cdr; arena)
}

defsubr!(car, cdr, cons, setcar, setcdr, car_safe, cdr_safe);

#[macro_export]
macro_rules! cons {
    ($car:expr, $cdr:expr; $arena:expr) => {
        $arena.add(
            #[allow(unused_unsafe)]
            unsafe {
                crate::cons::Cons::new($arena.add($car), $arena.add($cdr))
            },
        )
    };
    ($car:expr; $arena:expr) => {
        $arena.add(unsafe { crate::cons::Cons::new($arena.add($car), crate::object::Object::NIL) })
    };
}

#[macro_export]
macro_rules! list {
    ($x:expr; $arena:expr) => (crate::cons!($x; $arena));
    ($x:expr, $($y:expr),+ $(,)? ; $arena:expr) => (crate::cons!($x, list!($($y),+ ; $arena) ; $arena));
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{arena::RootSet, object::IntoObject};

    fn as_cons(obj: Object) -> Option<&Cons> {
        match obj {
            Object::Cons(x) => Some(!x),
            _ => None,
        }
    }

    #[test]
    fn cons() {
        let roots = &RootSet::default();
        let arena = &Arena::new(roots);
        // TODO: Need to find a way to solve this
        // assert_eq!(16, size_of::<Cons>());
        let x = cons!("start", cons!(7, cons!(5, 9; arena); arena); arena);
        assert!(matches!(x, Object::Cons(_)));
        let cons1 = match x {
            Object::Cons(x) => !x,
            _ => unreachable!("Expected cons"),
        };

        let start_str = "start".to_owned();
        assert_eq!(start_str.into_obj(arena), cons1.car(arena));
        cons1.set_car("start2".into_obj(arena));
        let start2_str = "start2".to_owned();
        assert_eq!(start2_str.into_obj(arena), cons1.car(arena));

        let cons2 = as_cons(cons1.cdr(arena)).expect("expected cons");

        let cmp: Object = 7.into();
        assert_eq!(cmp, cons2.car(arena));

        let cons3 = as_cons(cons2.cdr(arena)).expect("expected cons");
        let cmp1: Object = 5.into();
        assert_eq!(cmp1, cons3.car(arena));
        let cmp2: Object = 9.into();
        assert_eq!(cmp2, cons3.cdr(arena));

        assert_eq!(cons!(5, "foo"; arena), cons!(5, "foo"; arena));
        assert_ne!(cons!(5, "foo"; arena), cons!(5, "bar"; arena));
        assert_eq!(
            list![5, 1, 1.5, "foo"; arena],
            list![5, 1, 1.5, "foo"; arena]
        );
        assert_ne!(
            list![5, 1, 1.5, "foo"; arena],
            list![5, 1, 1.5, "bar"; arena]
        );
    }
}
