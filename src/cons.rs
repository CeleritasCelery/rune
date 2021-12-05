use crate::arena::Arena;
use crate::error::{Error, Type};
use crate::object::{List, Object};
use anyhow::{anyhow, Result};
use fn_macros::defun;
use std::cell::Cell;
use std::fmt::{self, Debug, Display, Write};

#[derive(PartialEq)]
pub(crate) struct Cons<'ob> {
    mutable: bool,
    car: Cell<Object<'ob>>,
    cdr: Cell<Object<'ob>>,
}

#[derive(Debug, Default)]
pub(crate) struct ConstConsError();

impl Display for ConstConsError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Attempt to mutate constant cons cell")
    }
}

impl std::error::Error for ConstConsError {}

impl<'old, 'new> Cons<'old> {
    pub(crate) fn clone_in(&self, arena: &'new Arena) -> Cons<'new> {
        Cons::new(self.car().clone_in(arena), self.cdr().clone_in(arena))
    }
}

impl<'ob> Cons<'ob> {
    pub(crate) const fn new(car: Object<'ob>, cdr: Object<'ob>) -> Self {
        Self {
            mutable: true,
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

    pub(crate) fn set_car(&self, new_car: Object<'ob>) -> Result<()> {
        if self.mutable {
            self.car.set(new_car);
            Ok(())
        } else {
            Err(ConstConsError::default().into())
        }
    }

    pub(crate) fn set_cdr(&self, new_cdr: Object<'ob>) -> Result<()> {
        if self.mutable {
            self.cdr.set(new_cdr);
            Ok(())
        } else {
            Err(ConstConsError::default().into())
        }
    }

    pub(crate) fn make_const(&mut self) {
        self.mutable = false;
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
        Object::Nil(_) => write!(f, "{})", cons.car()),
        cdr => write!(f, "{} . {})", cons.car(), cdr),
    }
}

fn print_rest_debug(cons: &Cons, f: &mut fmt::Formatter) -> fmt::Result {
    match cons.cdr() {
        Object::Cons(cdr) => {
            write!(f, "{:?} ", cons.car())?;
            print_rest(&cdr, f)
        }
        Object::Nil(_) => write!(f, "{:?})", cons.car()),
        cdr => write!(f, "{:?} . {:?})", cons.car(), cdr),
    }
}

define_unbox!(Cons, &Cons<'ob>);

#[derive(Clone)]
pub(crate) struct ElemIter<'borrow, 'ob>(Option<&'borrow Cons<'ob>>);

impl<'borrow, 'ob> IntoIterator for &'borrow Cons<'ob> {
    type Item = Result<Object<'ob>>;
    type IntoIter = ElemIter<'borrow, 'ob>;

    fn into_iter(self) -> Self::IntoIter {
        ElemIter(Some(self))
    }
}

impl<'ob> List<'ob> {
    pub(crate) fn elements(&self) -> ElemIter<'_, 'ob> {
        match self {
            List::Nil => ElemIter(None),
            List::Cons(cons) => ElemIter(Some(cons)),
        }
    }
}

impl<'borrow, 'ob> Iterator for ElemIter<'borrow, 'ob> {
    type Item = Result<Object<'ob>>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.0 {
            Some(cons) => {
                (*self).0 = match cons.cdr() {
                    Object::Cons(next) => Some(!next),
                    Object::Nil(_) => None,
                    _ => return Some(Err(anyhow!("Found non-nil cdr at end of list"))),
                };
                Some(Ok(cons.car()))
            }
            None => None,
        }
    }
}

impl<'ob> Object<'ob> {
    pub(crate) fn as_list(self) -> Result<ElemIter<'ob, 'ob>> {
        match self {
            Object::Cons(cons) => Ok((!cons).into_iter()),
            Object::Nil(_) => Ok(ElemIter(None)),
            _ => Err(Error::from_object(Type::List, self).into()),
        }
    }
}

impl<'borrow, 'ob> ElemIter<'borrow, 'ob> {
    pub(crate) fn is_empty(&self) -> bool {
        self.0 == None
    }

    pub(crate) fn len(&self) -> usize {
        self.clone().count()
    }
}

#[defun]
fn car(list: List) -> Object {
    match list {
        List::Cons(cons) => cons.car(),
        List::Nil => Object::NIL,
    }
}

#[defun]
fn cdr(list: List) -> Object {
    match list {
        List::Cons(cons) => cons.cdr(),
        List::Nil => Object::NIL,
    }
}

#[defun]
fn car_safe(object: Object) -> Object {
    match object {
        Object::Cons(cons) => cons.car(),
        _ => Object::NIL,
    }
}

#[defun]
fn cdr_safe(object: Object) -> Object {
    match object {
        Object::Cons(cons) => cons.cdr(),
        _ => Object::NIL,
    }
}

#[defun]
fn setcar<'ob>(cell: &'ob Cons<'ob>, newcar: Object<'ob>) -> Result<Object<'ob>> {
    cell.set_car(newcar)?;
    Ok(newcar)
}

#[defun]
fn setcdr<'ob>(cell: &'ob Cons<'ob>, newcdr: Object<'ob>) -> Result<Object<'ob>> {
    cell.set_cdr(newcdr)?;
    Ok(newcdr)
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
            crate::object::Object::NIL,
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

    fn as_cons(obj: Object) -> Option<&Cons> {
        match obj {
            Object::Cons(x) => Some(!x),
            _ => None,
        }
    }

    #[test]
    fn cons() {
        let arena = &Arena::new();
        // TODO: Need to find a way to solve this
        // assert_eq!(16, size_of::<Cons>());
        let x = cons!("start", cons!(7, cons!(5, 9; arena); arena); arena);
        assert!(matches!(x, Object::Cons(_)));
        let cons1 = match x {
            Object::Cons(x) => !x,
            _ => unreachable!("Expected cons"),
        };

        let start_str = "start".to_owned();
        assert_eq!(start_str.into_obj(arena), cons1.car());
        cons1.set_car("start2".into_obj(arena)).unwrap();
        let start2_str = "start2".to_owned();
        assert_eq!(start2_str.into_obj(arena), cons1.car());

        let cons2 = as_cons(cons1.cdr()).expect("expected cons");

        let cmp: Object = 7.into();
        assert_eq!(cmp, cons2.car());

        let cons3 = as_cons(cons2.cdr()).expect("expected cons");
        let cmp1: Object = 5.into();
        assert_eq!(cmp1, cons3.car());
        let cmp2: Object = 9.into();
        assert_eq!(cmp2, cons3.cdr());

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
