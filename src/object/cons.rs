use crate::arena::Arena;
use crate::error::{Error, Type};
use crate::object::*;
use fn_macros::lisp_fn;
use std::cell::Cell;
use std::convert::TryFrom;
use std::fmt::{self, Display, Write};

#[derive(PartialEq, Debug, Clone)]
pub struct Cons<'a> {
    car: Cell<Object<'a>>,
    cdr: Cell<Object<'a>>,
}

impl<'old, 'new> Cons<'old> {
    pub fn clone_in(&self, arena: &'new Arena) -> Cons<'new> {
        Cons::new(self.car().clone_in(arena), self.cdr().clone_in(arena))
    }
}

impl<'a> Cons<'a> {
    pub const fn new(car: Object<'a>, cdr: Object<'a>) -> Self {
        Self {
            car: Cell::new(car),
            cdr: Cell::new(cdr),
        }
    }

    pub fn car(&self) -> Object<'a> {
        self.car.get()
    }

    pub fn cdr(&self) -> Object<'a> {
        self.cdr.get()
    }

    pub fn set_car(&self, new_car: Object<'a>) {
        self.car.set(new_car);
    }

    pub fn set_cdr(&self, new_cdr: Object<'a>) {
        self.cdr.set(new_cdr);
    }

    pub fn next(&self) -> Option<&'a Cons<'a>> {
        match self.cdr().val() {
            Value::Cons(cons) => Some(cons),
            _ => None,
        }
    }
}

impl<'obj> fmt::Display for Cons<'obj> {
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

impl<'obj> IntoObject<'obj, Object<'obj>> for Cons<'obj> {
    fn into_obj(self, arena: &'obj Arena) -> Object<'obj> {
        InnerObject::from_type(self, Tag::Cons, arena).into()
    }
}

impl<'a> TryFrom<Object<'a>> for &'a Cons<'a> {
    type Error = Error;
    fn try_from(obj: Object<'a>) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::Cons(x) => Ok(x),
            x => Err(Error::Type(Type::Cons, x.get_type())),
        }
    }
}

impl<'a> TryFrom<Object<'a>> for Option<&'a Cons<'a>> {
    type Error = Error;
    fn try_from(obj: Object<'a>) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::Cons(x) => Ok(Some(x)),
            Value::Nil => Ok(None),
            x => Err(Error::Type(Type::Cons, x.get_type())),
        }
    }
}

#[derive(Clone)]
pub struct ConsIter<'borrow, 'ob>(Option<&'borrow Cons<'ob>>);

#[derive(Debug, PartialEq)]
pub struct ConsIterError;

impl Display for ConsIterError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Found non-nil cdr at end of list")
    }
}

impl std::error::Error for ConsIterError {}

impl<'borrow, 'ob> IntoIterator for &'borrow Cons<'ob> {
    type Item = Result<Object<'ob>, ConsIterError>;
    type IntoIter = ConsIter<'borrow, 'ob>;

    fn into_iter(self) -> Self::IntoIter {
        ConsIter(Some(self))
    }
}

impl<'borrow, 'ob> Iterator for ConsIter<'borrow, 'ob> {
    type Item = Result<Object<'ob>, ConsIterError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.0 {
            Some(cons) => {
                let val = cons.car();
                let next = match cons.cdr().val() {
                    Value::Cons(next) => Some(next),
                    Value::Nil => None,
                    _ => return Some(Err(ConsIterError)),
                };
                *self = ConsIter(next);
                Some(Ok(val))
            }
            None => None,
        }
    }
}

impl<'a, 'ob> ConsIter<'a, 'ob> {
    pub const fn empty() -> Self {
        ConsIter(None)
    }

    pub fn is_empty(&self) -> bool {
        self.0 == None
    }
}

#[lisp_fn]
fn car(list: List) -> Object {
    match list {
        List::Cons(cons) => cons.car(),
        List::Nil => NIL,
    }
}

#[lisp_fn]
fn cdr(list: List) -> Object {
    match list {
        List::Cons(cons) => cons.cdr(),
        List::Nil => NIL,
    }
}

#[lisp_fn]
fn setcar<'obj>(cell: &'obj Cons<'obj>, newcar: Object<'obj>) -> Object<'obj> {
    cell.set_car(newcar);
    newcar
}

#[lisp_fn]
fn setcdr<'obj>(cell: &'obj Cons<'obj>, newcdr: Object<'obj>) -> Object<'obj> {
    cell.set_cdr(newcdr);
    newcdr
}

#[lisp_fn]
const fn cons<'obj>(car: Object<'obj>, cdr: Object<'obj>) -> Cons<'obj> {
    Cons::new(car, cdr)
}

defsubr!(car, cdr, setcar, setcdr, cons);

#[macro_export]
macro_rules! cons {
    ($car:expr, $cdr:expr; $arena:expr) => {
        crate::object::Cons::new(
            crate::object::IntoObject::into_obj($car, $arena),
            crate::object::IntoObject::into_obj($cdr, $arena),
        )
    };
    ($car:expr; $arena:expr) => {
        crate::object::Cons::new(
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
    use crate::object::Value;
    use std::mem::size_of;

    #[test]
    fn cons() {
        let arena = &Arena::new();
        assert_eq!(16, size_of::<Cons>());
        let cons = cons!("start", cons!(7, cons!(5, 9; arena); arena); arena);

        let x: Object = cons.into_obj(arena);
        assert!(matches!(x.val(), Value::Cons(_)));

        let cons1 = x.as_cons().expect("expected cons");

        let start_str = "start".to_owned();
        assert_eq!(Value::String(&start_str), cons1.car().val());
        (*cons1).set_car("start2".into_obj(arena));
        let start2_str = "start2".to_owned();
        assert_eq!(Value::String(&start2_str), cons1.car().val());

        let cons2 = cons1.cdr().as_cons().expect("expected cons");

        assert_eq!(Value::Int(7), cons2.car().val());

        let cons3 = cons2.cdr().as_cons().expect("expected cons");
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
