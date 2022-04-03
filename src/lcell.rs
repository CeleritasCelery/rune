#![allow(clippy::unused_self)]
#![allow(dead_code)]
#![allow(clippy::ptr_as_ptr)]
use generativity::{Guard, Id};
use std::cell::UnsafeCell;
use std::marker::PhantomData;

type Invariant<'id> = PhantomData<&'id mut &'id fn(&'id ()) -> &'id ()>;

#[derive(Debug)]
pub(crate) struct LCellOwner<'id> {
    _id: Id<'id>,
}

impl<'id> LCellOwner<'id> {
    pub(crate) unsafe fn new(guard: Guard<'id>) -> Self {
        Self { _id: guard.into() }
    }

    /// Create a new cell owned by this owner instance.  See also
    /// [`LCell::new`].
    ///
    /// [`LCell::new`]: struct.LCell.html
    pub(crate) fn cell<T>(&self, value: T) -> LCell<'id, T> {
        LCell::<T>::new(value)
    }

    /// Borrow contents of a `LCell` immutably (read-only).  Many
    /// `LCell` instances can be borrowed immutably at the same time
    /// from the same owner.
    #[inline]
    pub(crate) fn ro<'a, T: ?Sized>(&'a self, lc: &'a LCell<'id, T>) -> &'a T {
        unsafe { &*lc.value.get() }
    }

    /// Borrow contents of a `LCell` mutably (read-write).  Only one
    /// `LCell` at a time can be borrowed from the owner using this
    /// call.  The returned reference must go out of scope before
    /// another can be borrowed.
    #[inline]
    pub(crate) fn rw<'a, T: ?Sized>(&'a mut self, lc: &'a LCell<'id, T>) -> &'a mut T {
        unsafe { &mut *lc.value.get() }
    }

    /// Borrow contents of two `LCell` instances mutably.  Panics if
    /// the two `LCell` instances point to the same memory.
    #[inline]
    pub(crate) fn rw2<'a, T: ?Sized, U: ?Sized>(
        &'a mut self,
        lc1: &'a LCell<'id, T>,
        lc2: &'a LCell<'id, U>,
    ) -> (&'a mut T, &'a mut U) {
        assert!(
            lc1 as *const _ as *const () as usize != lc2 as *const _ as *const () as usize,
            "Illegal to borrow same LCell twice with rw2()"
        );
        unsafe { (&mut *lc1.value.get(), &mut *lc2.value.get()) }
    }

    /// Borrow contents of three `LCell` instances mutably.  Panics if
    /// any pair of `LCell` instances point to the same memory.
    #[inline]
    pub(crate) fn rw3<'a, T: ?Sized, U: ?Sized, V: ?Sized>(
        &'a mut self,
        lc1: &'a LCell<'id, T>,
        lc2: &'a LCell<'id, U>,
        lc3: &'a LCell<'id, V>,
    ) -> (&'a mut T, &'a mut U, &'a mut V) {
        assert!(
            (lc1 as *const _ as *const () as usize != lc2 as *const _ as *const () as usize)
                && (lc2 as *const _ as *const () as usize != lc3 as *const _ as *const () as usize)
                && (lc3 as *const _ as *const () as usize != lc1 as *const _ as *const () as usize),
            "Illegal to borrow same LCell twice with rw3()"
        );
        unsafe {
            (
                &mut *lc1.value.get(),
                &mut *lc2.value.get(),
                &mut *lc3.value.get(),
            )
        }
    }
}

/// Cell whose contents are owned (for borrowing purposes) by a
/// [`LCellOwner`].
///
/// To borrow from this cell, use the borrowing calls on the
/// [`LCellOwner`] instance that owns it, i.e. that shares the same
/// Rust lifetime.
///
/// See also [crate documentation](index.html).
///
/// [`LCellOwner`]: struct.LCellOwner.html
#[repr(transparent)]
#[derive(Debug)]
pub(crate) struct LCell<'id, T: ?Sized> {
    _id: Invariant<'id>,
    value: UnsafeCell<T>,
}

impl<'id, T> LCell<'id, T> {
    /// Create a new `LCell`.  The owner of this cell is inferred by
    /// Rust from the context.  So the owner lifetime is whatever
    /// lifetime is required by the first use of the new `LCell`.
    #[inline]
    pub(crate) fn new(value: T) -> LCell<'id, T> {
        LCell {
            _id: PhantomData,
            value: UnsafeCell::new(value),
        }
    }
}

impl<'id, T: ?Sized> LCell<'id, T> {
    /// Borrow contents of this cell immutably (read-only).  Many
    /// `LCell` instances can be borrowed immutably at the same time
    /// from the same owner.
    #[inline]
    pub(crate) fn ro<'a>(&'a self, owner: &'a LCellOwner<'id>) -> &'a T {
        owner.ro(self)
    }

    /// Borrow contents of this cell mutably (read-write).  Only one
    /// `LCell` at a time can be borrowed from the owner using this
    /// call.  The returned reference must go out of scope before
    /// another can be borrowed.  To mutably borrow from two or three
    /// cells at the same time, see [`LCellOwner::rw2`] or
    /// [`LCellOwner::rw3`].
    #[inline]
    pub(crate) fn rw<'a>(&'a self, owner: &'a mut LCellOwner<'id>) -> &'a mut T {
        owner.rw(self)
    }
}

// LCell already automatically implements Send, but not
// Sync. We can add these implementations though, since it's fine to
// send a &LCell to another thread, and even mutably borrow the value
// there, as long as T is Send and Sync.
//
// The reason why LCell<T>'s impl of Sync requires T: Send + Sync
// instead of just T: Sync is that LCell provides interior mutability.
// If you send a &LCell<T> (and its owner) to a different thread, you
// can call .rw() to get a &mut T, and use std::mem::swap() to move
// the T, effectively sending the T to that other thread. That's not
// allowed if T: !Send.
//
// Note that the bounds on T for LCell<T>'s impl of Sync are the same
// as those of std::sync::RwLock<T>. That's not a coincidence.
// The way these types let you access T concurrently is the same,
// even though the locking mechanisms are different.
unsafe impl<'id, T: Send + Sync + ?Sized> Sync for LCell<'id, T> {}

#[macro_export]
macro_rules! make_lcell_owner {
    ($name:ident) => {
        generativity::make_guard!(a);
        #[allow(unused_mut)]
        let mut $name = unsafe { $crate::lcell::LCellOwner::new(a) };
    };
}

#[cfg(test)]
mod tests {
    use super::LCell;
    use std::rc::Rc;

    #[test]
    fn lcell() {
        make_lcell_owner!(owner);
        let c1 = LCell::new(100_u32);
        let c2 = owner.cell(200_u32);
        (*owner.rw(&c1)) += 1;
        (*owner.rw(&c2)) += 2;
        let c1ref = owner.ro(&c1);
        let c2ref = owner.ro(&c2);
        let total = *c1ref + *c2ref;
        assert_eq!(total, 303);
    }

    #[test]
    #[should_panic]
    fn lcell_rw2() {
        make_lcell_owner!(owner);
        let c1 = Rc::new(LCell::new(100_u32));
        let (_mutref1, _mutref2) = owner.rw2(&c1, &c1);
    }

    #[test]
    #[should_panic]
    fn lcell_rw3_1() {
        make_lcell_owner!(owner);
        let c1 = Rc::new(LCell::new(100_u32));
        let c2 = Rc::new(LCell::new(200_u32));
        let (_mutref1, _mutref2, _mutref3) = owner.rw3(&c1, &c1, &c2);
    }

    #[test]
    #[should_panic]
    fn lcell_rw3_2() {
        make_lcell_owner!(owner);
        let c1 = Rc::new(LCell::new(100_u32));
        let c2 = Rc::new(LCell::new(200_u32));
        let (_mutref1, _mutref2, _mutref3) = owner.rw3(&c1, &c2, &c1);
    }

    #[test]
    #[should_panic]
    fn lcell_rw3_3() {
        make_lcell_owner!(owner);
        let c1 = Rc::new(LCell::new(100_u32));
        let c2 = Rc::new(LCell::new(200_u32));
        let (_mutref1, _mutref2, _mutref3) = owner.rw3(&c2, &c1, &c1);
    }

    #[test]
    fn lcell_unsized() {
        struct Squares(u32);
        struct Integers(u64);
        trait Series {
            fn step(&mut self);
            fn value(&self) -> u64;
        }
        impl Series for Squares {
            fn step(&mut self) {
                self.0 += 1;
            }
            fn value(&self) -> u64 {
                (u64::from(self.0)) * (u64::from(self.0))
            }
        }
        impl Series for Integers {
            fn step(&mut self) {
                self.0 += 1;
            }
            fn value(&self) -> u64 {
                self.0
            }
        }
        fn series<'id>(init: u32, is_squares: bool) -> Box<LCell<'id, dyn Series>> {
            if is_squares {
                Box::new(LCell::new(Squares(init)))
            } else {
                Box::new(LCell::new(Integers(init.into())))
            }
        }

        make_lcell_owner!(owner);

        let own = &mut owner;
        let cell1 = series(4, false);
        let cell2 = series(7, true);
        let cell3 = series(3, true);
        assert_eq!(cell1.ro(own).value(), 4);
        cell1.rw(own).step();
        assert_eq!(cell1.ro(own).value(), 5);
        assert_eq!(own.ro(&cell2).value(), 49);
        own.rw(&cell2).step();
        assert_eq!(own.ro(&cell2).value(), 64);
        let (r1, r2, r3) = own.rw3(&cell1, &cell2, &cell3);
        r1.step();
        r2.step();
        r3.step();
        assert_eq!(cell1.ro(own).value(), 6);
        assert_eq!(cell2.ro(own).value(), 81);
        assert_eq!(cell3.ro(own).value(), 16);
        let (r1, r2) = own.rw2(&cell1, &cell2);
        r1.step();
        r2.step();
        assert_eq!(cell1.ro(own).value(), 7);
        assert_eq!(cell2.ro(own).value(), 100);
    }
}
