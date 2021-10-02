use crate::arena::Arena;
use crate::error::{Error, Type};
use crate::object::{List, Object};
use anyhow::{anyhow, Result};
use fn_macros::defun;
use std::cell::Cell;
use std::convert::TryFrom;
use std::fmt::{self, Debug, Display, Write};

#[derive(PartialEq, Clone)]
pub(crate) struct Cons<'ob> {
    car: Cell<Object<'ob>>,
    cdr: Cell<Object<'ob>>,
}

impl<'ob> TryFrom<Object<'ob>> for &'ob mut Cons<'ob> {
    type Error = anyhow::Error;
    fn try_from(obj: Object<'ob>) -> Result<Self, Self::Error> {
        match obj {
            Object::Cons(x) => x.inner_mut().ok_or_else(|| anyhow!("Object is immutable")),
            _ => Err(Error::from_object(Type::Cons, obj).into()),
        }
    }
}

impl<'old, 'new> Cons<'old> {
    pub(crate) fn clone_in(&self, arena: &'new Arena) -> Cons<'new> {
        Cons::new(self.car().clone_in(arena), self.cdr().clone_in(arena))
    }
}

impl<'ob> Cons<'ob> {
    pub(crate) const fn new(car: Object<'ob>, cdr: Object<'ob>) -> Self {
        Self {
            car: Cell::new(car),
            cdr: Cell::new(cdr),
        }
    }

    pub(crate) fn car(&self) -> Object<'ob> {
        self.car.get()
    }

    pub(crate) fn cdr(&self) -> Object<'ob> {
        self.cdr.get()
    }

    pub(crate) fn set_car(&mut self, new_car: Object<'ob>) {
        self.car.set(new_car);
    }

    pub(crate) fn set_cdr(&mut self, new_cdr: Object<'ob>) {
        self.cdr.set(new_cdr);
    }

    pub(crate) fn make_read_only(&mut self) {
        self.car.get_mut().make_read_only();
        self.cdr.get_mut().make_read_only();
    }
}

impl<'ob> Display for Cons<'ob> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_char('(')?;
        print_rest(self, f)
    }
}

impl<'ob> Debug for Cons<'ob> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_char('(')?;
        print_rest_debug(self, f)
    }
}

fn print_rest(cons: &Cons, f: &mut fmt::Formatter) -> fmt::Result {
    match cons.cdr() {
        Object::Cons(cdr) => {
            write!(f, "{} ", cons.car())?;
            print_rest(&cdr, f)
        }
        Object::Nil => write!(f, "{})", cons.car()),
        cdr => write!(f, "{} . {})", cons.car(), cdr),
    }
}

fn print_rest_debug(cons: &Cons, f: &mut fmt::Formatter) -> fmt::Result {
    match cons.cdr() {
        Object::Cons(cdr) => {
            write!(f, "{:?} ", cons.car())?;
            print_rest(&cdr, f)
        }
        Object::Nil => write!(f, "{:?})", cons.car()),
        cdr => write!(f, "{:?} . {:?})", cons.car(), cdr),
    }
}

define_unbox!(Cons, &Cons<'ob>);

#[derive(Clone)]
pub(crate) struct ElemIter<'borrow, 'ob>(ConsIter<'borrow, 'ob>);

#[derive(Clone)]
pub(crate) struct ConsIter<'borrow, 'ob>(Option<&'borrow Cons<'ob>>);

impl<'borrow, 'ob> IntoIterator for &'borrow Cons<'ob> {
    type Item = Result<Object<'ob>>;
    type IntoIter = ElemIter<'borrow, 'ob>;

    fn into_iter(self) -> Self::IntoIter {
        ElemIter(ConsIter(Some(self)))
    }
}

impl<'ob> IntoIterator for List<'ob> {
    type Item = Result<Object<'ob>>;
    type IntoIter = ElemIter<'ob, 'ob>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            List::Nil => ElemIter(ConsIter(None)),
            List::Cons(cons) => ElemIter(ConsIter(Some(cons))),
        }
    }
}

impl<'borrow, 'ob> Iterator for ConsIter<'borrow, 'ob> {
    type Item = Result<&'borrow Cons<'ob>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.0 {
            Some(cons) => {
                (*self).0 = match cons.cdr() {
                    Object::Cons(next) => Some(!next),
                    Object::Nil => None,
                    _ => return Some(Err(anyhow!("Found non-nil cdr at end of list"))),
                };
                Some(Ok(cons))
            }
            None => None,
        }
    }
}

impl<'borrow, 'ob> Iterator for ElemIter<'borrow, 'ob> {
    type Item = Result<Object<'ob>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(|result| result.map(Cons::car))
    }
}

pub(crate) fn into_iter(obj: Object) -> Result<ElemIter> {
    match obj {
        Object::Cons(cons) => Ok((!cons).into_iter()),
        Object::Nil => Ok(ElemIter::empty()),
        _ => Err(Error::from_object(Type::List, obj).into()),
    }
}

impl<'borrow, 'ob> ElemIter<'borrow, 'ob> {
    pub(crate) fn by_cons(self) -> ConsIter<'borrow, 'ob> {
        self.0
    }

    pub(crate) const fn empty() -> Self {
        ElemIter(ConsIter(None))
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.0 .0 == None
    }

    pub(crate) fn len(&self) -> usize {
        self.clone().count()
    }
}

#[defun]
fn car(list: List) -> Object {
    match list {
        List::Cons(cons) => cons.car(),
        List::Nil => Object::Nil,
    }
}

#[defun]
fn cdr(list: List) -> Object {
    match list {
        List::Cons(cons) => cons.cdr(),
        List::Nil => Object::Nil,
    }
}

#[defun]
fn car_safe(object: Object) -> Object {
    match object {
        Object::Cons(cons) => cons.car(),
        _ => Object::Nil,
    }
}

#[defun]
fn cdr_safe(object: Object) -> Object {
    match object {
        Object::Cons(cons) => cons.cdr(),
        _ => Object::Nil,
    }
}

#[defun]
fn setcar<'ob>(cell: &'ob mut Cons<'ob>, newcar: Object<'ob>) -> Object<'ob> {
    cell.set_car(newcar);
    newcar
}

#[defun]
fn setcdr<'ob>(cell: &'ob mut Cons<'ob>, newcdr: Object<'ob>) -> Object<'ob> {
    cell.set_cdr(newcdr);
    newcdr
}

#[defun]
const fn cons<'ob>(car: Object<'ob>, cdr: Object<'ob>) -> Cons<'ob> {
    Cons::new(car, cdr)
}

defsubr!(car, cdr, cons, setcar, setcdr, car_safe, cdr_safe);

#[macro_export]
macro_rules! cons {
    ($car:expr, $cdr:expr; $arena:expr) => {
        $arena.add(crate::cons::Cons::new($arena.add($car), $arena.add($cdr)))
    };
    ($car:expr; $arena:expr) => {
        $arena.add(crate::cons::Cons::new(
            $arena.add($car),
            crate::object::Object::Nil,
        ))
    };
}

#[macro_export]
macro_rules! list {
    ($x:expr; $arena:expr) => (cons!($x; $arena));
    ($x:expr, $($y:expr),+ $(,)? ; $arena:expr) => (cons!($x, list!($($y),+ ; $arena) ; $arena));
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::object::IntoObject;
    use std::{convert::TryInto, mem::size_of};

    fn as_cons(obj: Object) -> Option<&Cons> {
        match obj {
            Object::Cons(x) => Some(!x),
            _ => None,
        }
    }

    #[test]
    fn cons() {
        let arena = &Arena::new();
        assert_eq!(16, size_of::<Cons>());
        let x = cons!("start", cons!(7, cons!(5, 9; arena); arena); arena);
        assert!(matches!(x, Object::Cons(_)));

        let cons1: &mut Cons = x.try_into().expect("expected cons");

        let start_str = "start".to_owned();
        assert_eq!(start_str.into_obj(arena), cons1.car());
        cons1.set_car("start2".into_obj(arena));
        let start2_str = "start2".to_owned();
        assert_eq!(start2_str.into_obj(arena), cons1.car());

        let cons2 = as_cons(cons1.cdr()).expect("expected cons");

        let cmp: Object = 7.into();
        assert_eq!(cmp, cons2.car());

        let cons3 = as_cons(cons2.cdr()).expect("expected cons");
        let cmp: Object = 5.into();
        assert_eq!(cmp, cons3.car());
        let cmp: Object = 9.into();
        assert_eq!(cmp, cons3.cdr());

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
