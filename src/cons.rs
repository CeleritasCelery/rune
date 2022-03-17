use crate::arena::{Arena, Block, ConstrainLifetime};
use crate::object::{List, Object};
use anyhow::Result;
use fn_macros::defun;
use std::cell::Cell;
use std::fmt::{self, Debug, Display, Write};

mod iter;

pub(crate) use iter::*;

#[derive(PartialEq)]
pub(crate) struct Cons<'ob> {
    mutable: bool,
    marked: Cell<bool>,
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
    pub(crate) fn clone_in<const C: bool>(&self, bk: &'new Block<C>) -> Cons<'new> {
        Cons::new(self.car.get().clone_in(bk), self.cdr.get().clone_in(bk))
    }
}

impl<'ob> Cons<'ob> {
    pub(crate) const fn new(car: Object<'ob>, cdr: Object<'ob>) -> Self {
        Self {
            mutable: true,
            marked: Cell::new(false),
            car: Cell::new(car),
            cdr: Cell::new(cdr),
        }
    }

    pub(crate) fn car<'new, const C: bool>(&self, cx: &'new Block<C>) -> Object<'new> {
        self.car.get().constrain_lifetime(cx)
    }

    pub(crate) fn cdr<'new, const C: bool>(&self, cx: &'new Block<C>) -> Object<'new> {
        self.cdr.get().constrain_lifetime(cx)
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

impl<'brw, 'old, 'new> ConstrainLifetime<'new, &'new Cons<'new>> for &'brw Cons<'old> {
    fn constrain_lifetime<const C: bool>(self, _cx: &'new Block<C>) -> &'new Cons<'new> {
        unsafe { std::mem::transmute::<&'brw Cons<'old>, &'new Cons<'new>>(self) }
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
    let car = cons.car.get();
    match cons.cdr.get() {
        Object::Cons(cdr) => {
            write!(f, "{car} ")?;
            print_rest(&cdr, f)
        }
        Object::Nil(_) => write!(f, "{car})"),
        cdr => write!(f, "{car} . {cdr})"),
    }
}

fn print_rest_debug(cons: &Cons, f: &mut fmt::Formatter) -> fmt::Result {
    let car = cons.car.get();
    match cons.cdr.get() {
        Object::Cons(cdr) => {
            write!(f, "{car:?} ")?;
            print_rest(&cdr, f)
        }
        Object::Nil(_) => write!(f, "{car:?})"),
        cdr => write!(f, "{car:?} . {cdr:?})"),
    }
}

define_unbox!(Cons, &Cons<'ob>);

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
        cons1.set_car("start2".into_obj(arena)).unwrap();
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
