use rune_core::hashmap::HashSet;
use rune_macros::Trace;

use super::gc::{Block, GcHeap, GcState, Trace};
use super::object::{CloneIn, Gc, IntoObject, ObjCell, Object, ObjectType, NIL};
use anyhow::{anyhow, Result};
use std::fmt::{self, Debug, Display, Write};

mod iter;

pub(crate) use iter::*;

mod sealed {
    use super::*;
    #[derive(Eq)]
    pub(crate) struct ConsInner {
        pub(super) mutable: bool,
        pub(super) car: ObjCell,
        pub(super) cdr: ObjCell,
    }
}

pub(in crate::core) use sealed::ConsInner;

#[derive(PartialEq, Eq, Trace)]
pub(crate) struct Cons(GcHeap<ConsInner>);

impl std::ops::Deref for Cons {
    type Target = GcHeap<ConsInner>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

// TODO: we need to handle loops in equal
impl PartialEq for ConsInner {
    fn eq(&self, other: &Self) -> bool {
        self.car() == other.car() && self.cdr() == other.cdr()
    }
}

impl Cons {
    // SAFETY: Cons must always be allocated in the GC heap, it cannot live on
    // the stack. Otherwise it could outlive it's objects since it has no
    // lifetimes.
    unsafe fn new_unchecked(car: Object, cdr: Object) -> ConsInner {
        ConsInner { mutable: true, car: ObjCell::new(car), cdr: ObjCell::new(cdr) }
    }

    /// Create a new cons cell
    pub(crate) fn new<'ob, T, Tx, U, Ux, const C: bool>(
        car: T,
        cdr: U,
        cx: &'ob Block<C>,
    ) -> &'ob Self
    where
        T: IntoObject<Out<'ob> = Tx>,
        Gc<Tx>: Into<Object<'ob>>,
        U: IntoObject<Out<'ob> = Ux>,
        Gc<Ux>: Into<Object<'ob>>,
    {
        let car = car.into_obj(cx).into();
        let cdr = cdr.into_obj(cx).into();
        let cons = unsafe { Cons::new_unchecked(car, cdr) };
        Cons(GcHeap::new(cons, C)).into_obj(cx).untag()
    }

    /// Create a new cons cell with the cdr set to nil
    pub(crate) fn new1<'ob, T, Tx, const C: bool>(car: T, cx: &'ob Block<C>) -> &'ob Self
    where
        T: IntoObject<Out<'ob> = Tx>,
        Gc<Tx>: Into<Object<'ob>>,
    {
        let car = car.into_obj(cx).into();
        let cons = unsafe { Cons::new_unchecked(car, NIL) };
        Cons(GcHeap::new(cons, C)).into_obj(cx).untag()
    }

    pub(in crate::core) fn mark_const(&mut self) {
        self.0.mutable = false;
    }
}

impl ConsInner {
    pub(crate) fn car(&self) -> Object {
        self.car.get()
    }

    pub(crate) fn cdr(&self) -> Object {
        self.cdr.get()
    }

    pub(crate) fn set_car(&self, new_car: Object) -> Result<()> {
        if self.mutable {
            unsafe { self.car.as_mut().set(new_car) }
            Ok(())
        } else {
            Err(anyhow!("Attempt to call setcar on immutable cons cell"))
        }
    }

    pub(crate) fn set_cdr(&self, new_cdr: Object) -> Result<()> {
        if self.mutable {
            unsafe { self.cdr.as_mut().set(new_cdr) }
            Ok(())
        } else {
            Err(anyhow!("Attempt to call setcdr on immutable cons cell"))
        }
    }
}

impl<'new> CloneIn<'new, &'new Cons> for Cons {
    fn clone_in<const C: bool>(&self, bk: &'new Block<C>) -> Gc<&'new Cons> {
        Cons::new(self.car().clone_in(bk), self.cdr().clone_in(bk), bk).into_obj(bk)
    }
}

impl Trace for ConsInner {
    fn trace(&self, state: &mut GcState) {
        let cdr = self.cdr();
        if cdr.is_markable() {
            state.push(cdr);
        }
        let car = self.car();
        if car.is_markable() {
            state.push(car);
        }
    }
}

impl Display for Cons {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.display_walk(f, &mut HashSet::default())
    }
}

impl Debug for Cons {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.display_walk(f, &mut HashSet::default())
    }
}

impl Cons {
    pub(super) fn display_walk(
        &self,
        f: &mut fmt::Formatter,
        seen: &mut HashSet<*const u8>,
    ) -> fmt::Result {
        if self.is_backref(seen) {
            return f.write_str("#0");
        }

        f.write_char('(')?;
        let mut cons = self;

        loop {
            cons.car().untag().display_walk(f, seen)?;
            match cons.cdr().untag() {
                ObjectType::Cons(tail) => {
                    cons = tail;
                    f.write_char(' ')?;
                }
                ObjectType::NIL => break,
                x => {
                    write!(f, " . ")?;
                    x.display_walk(f, seen)?;
                    break;
                }
            }
            if cons.is_backref(seen) {
                f.write_str(". #0")?;
                break;
            }
        }
        f.write_char(')')
    }

    fn is_backref(&self, seen: &mut HashSet<*const u8>) -> bool {
        let ptr = (self as *const Self).cast();
        if seen.contains(&ptr) {
            true
        } else {
            seen.insert(ptr);
            false
        }
    }
}

define_unbox!(Cons, &'ob Cons);

#[cfg(test)]
mod test {
    use crate::core::gc::Context;
    use rune_core::macros::list;

    use super::super::gc::RootSet;
    use super::*;

    #[test]
    fn cons() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        // TODO: Need to find a way to solve this
        // assert_eq!(16, size_of::<Cons>());
        let cons1 = Cons::new("start", Cons::new(7, Cons::new(5, 9, cx), cx), cx);

        let start_str = "start".to_owned();
        assert_eq!(cx.add(start_str), cons1.car());
        cons1.set_car(cx.add("start2")).unwrap();
        let start2_str = "start2".to_owned();
        assert_eq!(cx.add(start2_str), cons1.car());

        let ObjectType::Cons(cons2) = cons1.cdr().untag() else { unreachable!("Expected cons") };

        let cmp: Object = 7.into();
        assert_eq!(cmp, cons2.car());

        let ObjectType::Cons(cons3) = cons2.cdr().untag() else { unreachable!("Expected cons") };
        let cmp1: Object = 5.into();
        assert_eq!(cmp1, cons3.car());
        let cmp2: Object = 9.into();
        assert_eq!(cmp2, cons3.cdr());

        let lhs = Cons::new(5, "foo", cx);
        assert_eq!(lhs, Cons::new(5, "foo", cx));
        assert_ne!(lhs, Cons::new(5, "bar", cx));
        let lhs = list![5, 1, 1.5, "foo"; cx];
        assert_eq!(lhs, list![5, 1, 1.5, "foo"; cx]);
        assert_ne!(lhs, list![5, 1, 1.5, "bar"; cx]);
    }
}
