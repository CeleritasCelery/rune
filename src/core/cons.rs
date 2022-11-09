use super::gc::{Block, Context, GcManaged, GcMark, Trace};
use super::object::{nil, Gc, GcObj, IntoObject, List, Object, RawObj, WithLifetime};
use anyhow::{anyhow, Result};
use fn_macros::defun;
use std::cell::Cell;
use std::fmt::{self, Debug, Display, Write};
use std::ptr::addr_of;

mod iter;

pub(crate) use iter::*;

pub(crate) struct Cons {
    marked: GcMark,
    mutable: bool,
    car: Cell<RawObj>,
    cdr: Cell<RawObj>,
}

impl PartialEq for Cons {
    fn eq(&self, other: &Self) -> bool {
        self.car() == other.car() && self.cdr() == other.cdr()
    }
}

impl Cons {
    // SAFETY: Cons must always be allocated in the GC heap, it cannot live on
    // the stack. Otherwise it could outlive it's objects since it has no
    // lifetimes.
    pub(crate) unsafe fn new(car: GcObj, cdr: GcObj) -> Self {
        Self {
            marked: GcMark::default(),
            mutable: true,
            car: Cell::new(car.into_raw()),
            cdr: Cell::new(cdr.into_raw()),
        }
    }

    pub(in crate::core) fn mark_const(&mut self) {
        self.mutable = false;
    }

    pub(crate) fn car(&self) -> GcObj {
        unsafe { GcObj::from_raw(self.car.get()) }
    }

    pub(crate) fn cdr(&self) -> GcObj {
        unsafe { GcObj::from_raw(self.cdr.get()) }
    }

    pub(crate) fn set_car(&self, new_car: GcObj) -> Result<()> {
        if self.mutable {
            self.car.set(new_car.into_raw());
            Ok(())
        } else {
            Err(anyhow!("Attempt to call set-car on immutable cons cell"))
        }
    }

    pub(crate) fn set_cdr(&self, new_cdr: GcObj) -> Result<()> {
        if self.mutable {
            self.cdr.set(new_cdr.into_raw());
            Ok(())
        } else {
            Err(anyhow!("Attempt to call set-cdr on immutable cons cell"))
        }
    }

    pub(crate) fn clone_in<'new, const C: bool>(&self, bk: &'new Block<C>) -> &'new Cons {
        unsafe {
            Cons::new(self.car().clone_in(bk), self.cdr().clone_in(bk))
                .into_obj(bk)
                .get()
        }
    }

    pub(crate) fn addr_car(&self) -> *const GcObj {
        addr_of!(self.car).cast::<GcObj>()
    }

    pub(crate) fn addr_cdr(&self) -> *const GcObj {
        addr_of!(self.cdr).cast::<GcObj>()
    }
}

impl GcManaged for Cons {
    fn get_mark(&self) -> &GcMark {
        &self.marked
    }
}

impl Trace for Cons {
    fn trace(&self, stack: &mut Vec<RawObj>) {
        let cdr = self.cdr();
        if cdr.is_markable() {
            stack.push(cdr.into_raw());
        }
        let car = self.car();
        if car.is_markable() {
            stack.push(car.into_raw());
        }
        self.mark();
    }
}

impl<'old, 'new> WithLifetime<'new> for &'old Cons {
    type Out = &'new Cons;

    unsafe fn with_lifetime(self) -> Self::Out {
        &*(self as *const Cons)
    }
}

impl Display for Cons {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_char('(')?;
        let mut cons = self;

        loop {
            write!(f, "{}", cons.car())?;
            match cons.cdr().get() {
                Object::Cons(tail) => {
                    cons = tail;
                    f.write_char(' ')?;
                }
                Object::Symbol(s) if s.nil() => break,
                x => {
                    write!(f, ". {x}")?;
                    break;
                }
            }
        }
        f.write_char(')')
    }
}

impl Debug for Cons {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_char('(')?;
        let mut cons = self;
        loop {
            write!(f, "{:?}", cons.car())?;
            match cons.cdr().get() {
                Object::Cons(tail) => {
                    cons = tail;
                    f.write_char(' ')?;
                }
                Object::Symbol(s) if s.nil() => break,
                end => {
                    write!(f, " . {end:?}")?;
                    break;
                }
            }
        }
        f.write_char(')')
    }
}

define_unbox!(Cons, &'ob Cons);

#[defun]
fn car(list: Gc<List>) -> GcObj {
    match list.get() {
        List::Cons(cons) => cons.car(),
        List::Nil => nil(),
    }
}

#[defun]
fn cdr(list: Gc<List>) -> GcObj {
    match list.get() {
        List::Cons(cons) => cons.cdr(),
        List::Nil => nil(),
    }
}

#[defun]
fn car_safe(object: GcObj) -> GcObj {
    match object.get() {
        Object::Cons(cons) => cons.car(),
        _ => nil(),
    }
}

#[defun]
fn cdr_safe(object: GcObj) -> GcObj {
    match object.get() {
        Object::Cons(cons) => cons.cdr(),
        _ => nil(),
    }
}

#[defun]
fn setcar<'ob>(cell: &Cons, newcar: GcObj<'ob>) -> Result<GcObj<'ob>> {
    cell.set_car(newcar)?;
    Ok(newcar)
}

#[defun]
fn setcdr<'ob>(cell: &Cons, newcdr: GcObj<'ob>) -> Result<GcObj<'ob>> {
    cell.set_cdr(newcdr)?;
    Ok(newcdr)
}

#[defun]
fn cons<'ob>(car: GcObj, cdr: GcObj, cx: &'ob Context) -> GcObj<'ob> {
    crate::cons!(car, cdr; cx)
}

define_symbols!(FUNCS => {car, cdr, cons, setcar, setcdr, car_safe, cdr_safe});

#[macro_export]
macro_rules! cons {
    ($car:expr, $cdr:expr; $cx:expr) => {
        $cx.add::<_, _, $crate::core::object::Object>(
            #[allow(unused_unsafe)]
            unsafe {
                $crate::core::cons::Cons::new($cx.add($car), $cx.add($cdr))
            },
        )
    };
    ($car:expr; $cx:expr) => {
        $cx.add::<_, _, $crate::core::object::Object>(unsafe {
            $crate::core::cons::Cons::new($cx.add($car), $crate::core::object::nil())
        })
    };
}

#[macro_export]
macro_rules! list {
    ($x:expr; $cx:expr) => ($crate::cons!($x; $cx));
    ($x:expr, $($y:expr),+ $(,)? ; $cx:expr) => ($crate::cons!($x, list!($($y),+ ; $cx) ; $cx));
}

#[cfg(test)]
mod test {
    use super::super::gc::RootSet;
    use super::*;

    #[test]
    fn cons() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        // TODO: Need to find a way to solve this
        // assert_eq!(16, size_of::<Cons>());
        let x = cons!("start", cons!(7, cons!(5, 9; cx); cx); cx);
        assert!(matches!(x.get(), Object::Cons(_)));
        let Object::Cons(cons1) = x.get() else {unreachable!("Expected cons")};

        let start_str = "start".to_owned();
        assert_eq!(cx.add(start_str), cons1.car());
        cons1.set_car(cx.add("start2")).unwrap();
        let start2_str = "start2".to_owned();
        assert_eq!(cx.add(start2_str), cons1.car());

        let Object::Cons(cons2) = cons1.cdr().get() else {unreachable!("Expected cons")};

        let cmp: GcObj = 7.into();
        assert_eq!(cmp, cons2.car());

        let Object::Cons(cons3) = cons2.cdr().get() else {unreachable!("Expected cons")};
        let cmp1: GcObj = 5.into();
        assert_eq!(cmp1, cons3.car());
        let cmp2: GcObj = 9.into();
        assert_eq!(cmp2, cons3.cdr());

        let lhs = cons!(5, "foo"; cx);
        assert_eq!(lhs, cons!(5, "foo"; cx));
        assert_ne!(lhs, cons!(5, "bar"; cx));
        let lhs = list![5, 1, 1.5, "foo"; cx];
        assert_eq!(lhs, list![5, 1, 1.5, "foo"; cx]);
        assert_ne!(lhs, list![5, 1, 1.5, "bar"; cx]);
    }
}
