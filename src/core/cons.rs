use super::arena::{Arena, Block};
use super::object::{Gc, GcObj, List, Object, RawObj, WithLifetime};
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
    pub(crate) unsafe fn new(car: GcObj, cdr: GcObj) -> Self {
        Self {
            marked: Cell::new(false),
            car: Cell::new(car.into_raw()),
            cdr: Cell::new(cdr.into_raw()),
        }
    }

    pub(crate) fn car<'new, const C: bool>(&self, cx: &'new Block<C>) -> GcObj<'new> {
        self.__car().constrain_lifetime(cx)
    }

    pub(crate) fn cdr<'new, const C: bool>(&self, cx: &'new Block<C>) -> GcObj<'new> {
        self.__cdr().constrain_lifetime(cx)
    }

    // Private internal function to get car/cdr without arena
    fn __car(&self) -> GcObj {
        unsafe { self.car_unchecked() }
    }

    fn __cdr(&self) -> GcObj {
        unsafe { self.cdr_unchecked() }
    }

    pub(crate) unsafe fn car_unchecked(&self) -> GcObj {
        GcObj::from_raw(self.car.get())
    }

    pub(crate) unsafe fn cdr_unchecked(&self) -> GcObj {
        GcObj::from_raw(self.cdr.get())
    }

    pub(crate) fn set_car(&self, new_car: GcObj) {
        self.car.set(new_car.into_raw());
    }

    pub(crate) fn set_cdr(&self, new_cdr: GcObj) {
        self.cdr.set(new_cdr.into_raw());
    }

    #[allow(clippy::wrong_self_convention)]
    pub(crate) fn into_raw(&self) -> *const Self {
        self as *const Self
    }

    pub(crate) fn mark(&self, stack: &mut Vec<RawObj>) {
        let cdr = self.__cdr();
        if cdr.is_markable() {
            stack.push(cdr.into_raw());
        }
        let car = self.__car();
        if car.is_markable() {
            stack.push(car.into_raw());
        }
        self.marked.set(true);
    }

    pub(crate) fn unmark(&self) {
        self.marked.set(false);
    }

    pub(crate) fn is_marked(&self) -> bool {
        self.marked.get()
    }
}

impl<'old, 'new> WithLifetime<'new> for &'old Cons {
    type Out = &'new Cons;

    unsafe fn with_lifetime(self) -> Self::Out {
        &*(self as *const Cons)
    }
}

impl<'brw, 'new> ConstrainLifetime<'new, &'new Cons> for &'brw Cons {
    fn constrain_lifetime<const C: bool>(self, _cx: &'new Block<C>) -> &'new Cons {
        unsafe { self.with_lifetime() }
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
    match cons.__cdr().get() {
        Object::Cons(cdr) => {
            write!(f, "{car} ")?;
            print_rest(cdr, f)
        }
        Object::Nil => write!(f, "{car})"),
        cdr => write!(f, "{car} . {cdr})"),
    }
}

fn print_rest_debug(cons: &Cons, f: &mut fmt::Formatter) -> fmt::Result {
    let car = cons.__car();
    match cons.__cdr().get() {
        Object::Cons(cdr) => {
            write!(f, "{car:?} ")?;
            print_rest(cdr, f)
        }
        Object::Nil => write!(f, "{car:?})"),
        cdr => write!(f, "{car:?} . {cdr:?})"),
    }
}

trait ConstrainLifetime<'new, T> {
    fn constrain_lifetime<const C: bool>(self, cx: &'new Block<C>) -> T;
}

impl<'old, 'new> ConstrainLifetime<'new, GcObj<'new>> for GcObj<'old> {
    fn constrain_lifetime<const C: bool>(self, _cx: &'new Block<C>) -> GcObj<'new> {
        // Lifetime is bound to borrow of Block, so it is safe to extend
        unsafe { self.with_lifetime() }
    }
}

impl<'old, 'new, 'brw> ConstrainLifetime<'new, &'brw [GcObj<'new>]> for &'brw [GcObj<'old>] {
    fn constrain_lifetime<const C: bool>(self, _cx: &'new Block<C>) -> &'brw [GcObj<'new>] {
        // Lifetime is bound to borrow of Block, so it is safe to extend
        unsafe { std::mem::transmute::<&[GcObj<'old>], &[GcObj<'new>]>(self) }
    }
}

define_unbox!(Cons, &'ob Cons);

#[defun]
fn car<'ob>(list: Gc<List>, arena: &'ob Arena) -> GcObj<'ob> {
    match list.get() {
        List::Cons(cons) => cons.car(arena),
        List::Nil => GcObj::NIL,
    }
}

#[defun]
fn cdr<'ob>(list: Gc<List>, arena: &'ob Arena) -> GcObj<'ob> {
    match list.get() {
        List::Cons(cons) => cons.cdr(arena),
        List::Nil => GcObj::NIL,
    }
}

#[defun]
fn car_safe<'ob>(object: GcObj<'ob>, arena: &'ob Arena) -> GcObj<'ob> {
    match object.get() {
        Object::Cons(cons) => cons.car(arena),
        _ => GcObj::NIL,
    }
}

#[defun]
fn cdr_safe<'ob>(object: GcObj, arena: &'ob Arena) -> GcObj<'ob> {
    match object.get() {
        Object::Cons(cons) => cons.cdr(arena),
        _ => GcObj::NIL,
    }
}

#[defun]
fn setcar<'ob>(cell: &'ob Cons, newcar: GcObj<'ob>) -> GcObj<'ob> {
    cell.set_car(newcar);
    newcar
}

#[defun]
fn setcdr<'ob>(cell: &'ob Cons, newcdr: GcObj<'ob>) -> GcObj<'ob> {
    cell.set_cdr(newcdr);
    newcdr
}

#[defun]
fn cons<'ob>(car: GcObj<'ob>, cdr: GcObj<'ob>, arena: &'ob Arena) -> GcObj<'ob> {
    crate::cons!(car, cdr; arena)
}

defsubr!(car, cdr, cons, setcar, setcdr, car_safe, cdr_safe);

#[macro_export]
macro_rules! cons {
    ($car:expr, $cdr:expr; $arena:expr) => {
        $arena.add(
            #[allow(unused_unsafe)]
            unsafe {
                crate::core::cons::Cons::new($arena.add($car), $arena.add($cdr))
            },
        )
    };
    ($car:expr; $arena:expr) => {
        $arena.add(unsafe {
            crate::core::cons::Cons::new($arena.add($car), crate::core::object::GcObj::NIL)
        })
    };
}

#[macro_export]
macro_rules! list {
    ($x:expr; $arena:expr) => (crate::cons!($x; $arena));
    ($x:expr, $($y:expr),+ $(,)? ; $arena:expr) => (crate::cons!($x, list!($($y),+ ; $arena) ; $arena));
}

#[cfg(test)]
mod test {
    use super::super::arena::RootSet;
    use super::*;

    fn as_cons(obj: GcObj) -> Option<&Cons> {
        match obj.get() {
            Object::Cons(x) => Some(x),
            _ => None,
        }
    }

    #[test]
    fn cons() {
        let roots = &RootSet::default();
        let arena = &Arena::new(roots);
        // TODO: Need to find a way to solve this
        // assert_eq!(16, size_of::<Cons>());
        let x: GcObj = cons!("start", cons!(7, cons!(5, 9; arena); arena); arena);
        assert!(matches!(x.get(), Object::Cons(_)));
        let cons1 = match x.get() {
            Object::Cons(x) => x,
            _ => unreachable!("Expected cons"),
        };

        let start_str = "start".to_owned();
        assert_eq!(arena.add(start_str), cons1.car(arena));
        cons1.set_car(arena.add("start2"));
        let start2_str = "start2".to_owned();
        assert_eq!(arena.add(start2_str), cons1.car(arena));

        let cons2 = as_cons(cons1.cdr(arena)).expect("expected cons");

        let cmp: GcObj = 7.into();
        assert_eq!(cmp, cons2.car(arena));

        let cons3 = as_cons(cons2.cdr(arena)).expect("expected cons");
        let cmp1: GcObj = 5.into();
        assert_eq!(cmp1, cons3.car(arena));
        let cmp2: GcObj = 9.into();
        assert_eq!(cmp2, cons3.cdr(arena));

        let lhs: GcObj = cons!(5, "foo"; arena);
        assert_eq!(lhs, cons!(5, "foo"; arena));
        assert_ne!(lhs, cons!(5, "bar"; arena));
        let lhs: GcObj = list![5, 1, 1.5, "foo"; arena];
        assert_eq!(lhs, list![5, 1, 1.5, "foo"; arena]);
        assert_ne!(lhs, list![5, 1, 1.5, "bar"; arena]);
    }
}
