use generativity::{Guard, Id};
use std::cell::UnsafeCell;
use std::marker::PhantomData;

type Invariant<'id> = PhantomData<&'id mut &'id fn(&'id ()) -> &'id ()>;

#[derive(Debug)]
pub(super) struct LCellOwner<'id> {
    _id: Id<'id>,
}

#[allow(clippy::unused_self)]
impl<'id> LCellOwner<'id> {
    pub(super) unsafe fn new(guard: Guard<'id>) -> Self {
        Self { _id: guard.into() }
    }

    pub(super) fn ro<'a, T: ?Sized>(&'a self, lc: &'a LCell<'id, T>) -> &'a T {
        unsafe { &*lc.value.get() }
    }

    pub(super) fn rw<'a, T: ?Sized>(&'a mut self, lc: &'a LCell<'id, T>) -> &'a mut T {
        unsafe { &mut *lc.value.get() }
    }

    #[allow(clippy::ptr_as_ptr)]
    pub(super) fn rw2<'a, T: ?Sized, U: ?Sized>(
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
}

#[repr(transparent)]
#[derive(Debug)]
pub(super) struct LCell<'id, T: ?Sized> {
    _id: Invariant<'id>,
    value: UnsafeCell<T>,
}

impl<'id, T> LCell<'id, T> {
    pub(super) fn new(value: T) -> LCell<'id, T> {
        LCell {
            _id: PhantomData,
            value: UnsafeCell::new(value),
        }
    }
}

#[macro_export]
macro_rules! make_lcell_owner {
    ($name:ident) => {
        generativity::make_guard!(a);
        #[allow(unused_mut)]
        let mut $name = unsafe { $crate::arena::LCellOwner::new(a) };
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
        let c2 = LCell::new(200_u32);
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
}
