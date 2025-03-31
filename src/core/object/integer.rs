use super::{CloneIn, IntoObject};
use crate::core::gc::{Block, GcHeap, GcState, Trace};
use crate::derive_GcMoveable;
use num_bigint::BigInt;
use std::fmt::Display;
use std::ops::Deref;

#[derive(PartialEq, Eq)]
pub(crate) struct LispBigInt(GcHeap<BigInt>);

derive_GcMoveable!(LispBigInt);

impl<'new> CloneIn<'new, &'new Self> for LispBigInt {
    fn clone_in<const C: bool>(&self, bk: &'new Block<C>) -> super::Gc<&'new Self> {
        let big_int = self.0.clone();
        big_int.into_obj(bk)
    }
}

impl LispBigInt {
    pub fn new(big_int: BigInt, constant: bool) -> Self {
        LispBigInt(GcHeap::new(big_int, constant))
    }
}

impl Display for LispBigInt {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Deref for LispBigInt {
    type Target = BigInt;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Trace for LispBigInt {
    fn trace(&self, _: &mut GcState) {}
}
