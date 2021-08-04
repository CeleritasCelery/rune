use crate::arena::Arena;
use crate::object::{List, Object, Value, NIL};
use anyhow::{anyhow, Result};
use fn_macros::defun;
use std::cell::Cell;
use std::fmt::{self, Display, Write};

#[derive(PartialEq, Debug, Clone)]
pub(crate) struct Cons<'ob> {
    car: Cell<Object<'ob>>,
    cdr: Cell<Object<'ob>>,
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

    pub(crate) fn set_car(&self, new_car: Object<'ob>) {
        self.car.set(new_car);
    }

    pub(crate) fn set_cdr(&self, new_cdr: Object<'ob>) {
        self.cdr.set(new_cdr);
    }
}

impl<'ob> Display for Cons<'ob> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_char('(')?;
        print_rest(self, f)
    }
}

fn print_rest(cons: &Cons, f: &mut fmt::Formatter) -> fmt::Result {
    match cons.cdr().val() {
        Value::Cons(cdr) => {
            write!(f, "{} ", cons.car())?;
            print_rest(cdr, f)
        }
        Value::Nil => write!(f, "{})", cons.car()),
        cdr => write!(f, "{} . {})", cons.car(), cdr),
    }
}

define_unbox!(Cons, Cons, &Cons<'ob>);

#[derive(Clone)]
pub(crate) struct ConsIter<'borrow, 'ob>(Option<&'borrow Cons<'ob>>);

impl<'borrow, 'ob> IntoIterator for &'borrow Cons<'ob> {
    type Item = Result<Object<'ob>>;
    type IntoIter = ConsIter<'borrow, 'ob>;

    fn into_iter(self) -> Self::IntoIter {
        ConsIter(Some(self))
    }
}

impl<'borrow, 'ob> Iterator for ConsIter<'borrow, 'ob> {
    type Item = Result<Object<'ob>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.0 {
            Some(cons) => {
                let val = cons.car();
                let next = match cons.cdr().val() {
                    Value::Cons(next) => Some(next),
                    Value::Nil => None,
                    _ => return Some(Err(anyhow!("Found non-nil cdr at end of list"))),
                };
                *self = ConsIter(next);
                Some(Ok(val))
            }
            None => None,
        }
    }
}

impl<'borrow, 'ob> ConsIter<'borrow, 'ob> {
    pub(crate) const fn empty() -> Self {
        ConsIter(None)
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.0 == None
    }
}

#[defun]
fn car(list: List) -> Object {
    match list {
        List::Cons(cons) => cons.car(),
        List::Nil => NIL,
    }
}

#[defun]
fn cdr(list: List) -> Object {
    match list {
        List::Cons(cons) => cons.cdr(),
        List::Nil => NIL,
    }
}

#[defun]
fn setcar<'ob>(cell: &Cons<'ob>, newcar: Object<'ob>) -> Object<'ob> {
    cell.set_car(newcar);
    newcar
}

#[defun]
fn setcdr<'ob>(cell: &Cons<'ob>, newcdr: Object<'ob>) -> Object<'ob> {
    cell.set_cdr(newcdr);
    newcdr
}

#[defun]
const fn cons<'ob>(car: Object<'ob>, cdr: Object<'ob>) -> Cons<'ob> {
    Cons::new(car, cdr)
}

defsubr!(car, cdr, setcar, setcdr, cons);

#[macro_export]
macro_rules! cons {
    ($car:expr, $cdr:expr; $arena:expr) => {
        crate::cons::Cons::new(
            crate::object::IntoObject::into_obj($car, $arena),
            crate::object::IntoObject::into_obj($cdr, $arena),
        )
    };
    ($car:expr; $arena:expr) => {
        #[allow(unused_qualifications)]
        crate::cons::Cons::new(
            crate::object::IntoObject::into_obj($car, $arena),
            crate::object::NIL,
        )
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
    use std::mem::size_of;

    fn as_cons(obj: Object) -> Option<&Cons> {
        match obj.val() {
            Value::Cons(x) => Some(x),
            _ => None,
        }
    }

    #[test]
    fn cons() {
        let arena = &Arena::new();
        assert_eq!(16, size_of::<Cons>());
        let cons = cons!("start", cons!(7, cons!(5, 9; arena); arena); arena);

        let x: Object = cons.into_obj(arena);
        assert!(matches!(x.val(), Value::Cons(_)));

        let cons1 = as_cons(x).expect("expected cons");

        let start_str = "start".to_owned();
        assert_eq!(Value::String(&start_str), cons1.car().val());
        (*cons1).set_car("start2".into_obj(arena));
        let start2_str = "start2".to_owned();
        assert_eq!(Value::String(&start2_str), cons1.car().val());

        let cons2 = as_cons(cons1.cdr()).expect("expected cons");

        assert_eq!(Value::Int(7), cons2.car().val());

        let cons3 = as_cons(cons2.cdr()).expect("expected cons");
        assert_eq!(Value::Int(5), cons3.car().val());
        assert_eq!(Value::Int(9), cons3.cdr().val());

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
