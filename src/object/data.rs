use std::fmt;
use std::marker::PhantomData;
use std::ops::{Deref, Not};

use crate::arena::Allocation;
use crate::cons::Cons;
use crate::symbol::Symbol;

use super::SubrFn;

/// The inner data type that hold the value for an object variant. This type
/// should be no larger then 56 bits. The lowest bit of data is used to encode
/// the mutability flag: 1 if immutable, 0 if mutable. This should be stored in
/// the alignment bits that bottom of the pointer.
#[derive(Copy, Clone)]
#[repr(transparent)]
pub(crate) struct Data<T> {
    data: [u8; 7],
    marker: PhantomData<T>,
}

// SAFETY: Creating a Data<()> is always safe.
pub(super) const UNUSED: Data<()> = unsafe { Data::from_raw(0) };

/// A trait to access the inner value of a [`Data`]
pub(crate) trait Inner {
    type Target;
    fn inner(self) -> Self::Target;
}

// We still need to determine when this is sound. Sending `Data<T>` across threads
// is not safe unless the values are copied with it. Maybe there is a better way
// to encode that in the type system.
unsafe impl<T> Send for Data<T> {}

impl<T> Data<T> {
    #[inline(always)]
    const fn into_raw(self) -> u64 {
        let x = self.data;
        let whole = [x[0], x[1], x[2], x[3], x[4], x[5], x[6], 0];
        u64::from_le_bytes(whole)
    }

    #[inline(always)]
    // SAFETY: The data passed to from_raw must be a valid bit representation of T.
    const unsafe fn from_raw(data: u64) -> Self {
        let x = data.to_le_bytes();
        // x[7] (the top byte) is removed to make room for the tag
        Data {
            data: [x[0], x[1], x[2], x[3], x[4], x[5], x[6]],
            marker: PhantomData,
        }
    }
}

impl<'a, T> Data<&'a T> {
    pub(super) fn from_ref(rf: &'a T) -> Self {
        let ptr: *const T = rf;
        // SAFETY: We are getting the bits of a valid reference
        unsafe { Self::from_raw(ptr as u64) }
    }
}

impl<'a, T> Data<&'a Allocation<T>> {
    pub(crate) fn get_alloc(self) -> &'a Allocation<T> {
        let ptr = self.into_raw() as *const Allocation<T>;
        // SAFETY: The marker lifetime 'a ensures that raw data is still valid
        // to dereference.
        unsafe { &*ptr }
    }
}

impl From<Symbol> for Data<Symbol> {
    fn from(x: Symbol) -> Self {
        Data::from_ref(x)
    }
}

impl<'a> From<&'a Cons<'a>> for Data<&'a Cons<'a>> {
    fn from(x: &'a Cons<'a>) -> Self {
        Data::from_ref(x)
    }
}

impl<'a, T> From<&'a Allocation<T>> for Data<&'a Allocation<T>> {
    fn from(x: &'a Allocation<T>) -> Self {
        Data::from_ref(x)
    }
}

impl<'a, T> Inner for Data<&'a Allocation<T>> {
    type Target = &'a T;
    #[inline(always)]
    fn inner(self) -> &'a T {
        self.get_alloc()
    }
}

impl Inner for Data<i64> {
    type Target = i64;
    #[inline(always)]
    fn inner(self) -> i64 {
        self.into_raw() as i64
    }
}

impl Inner for Data<Symbol> {
    type Target = Symbol;
    #[inline(always)]
    fn inner(self) -> Self::Target {
        let ptr = self.into_raw() as *const _;
        unsafe { &*ptr }
    }
}

impl<'a> Inner for Data<&'a Cons<'a>> {
    type Target = &'a Cons<'a>;
    #[inline(always)]
    fn inner(self) -> Self::Target {
        let ptr = self.into_raw() as *const _;
        unsafe { &*ptr }
    }
}

impl<'a> Inner for Data<&'a SubrFn> {
    type Target = &'a SubrFn;
    #[inline(always)]
    fn inner(self) -> Self::Target {
        let ptr = self.into_raw() as *const _;
        unsafe { &*ptr }
    }
}

impl Data<i64> {
    pub(super) const fn from_int(data: i64) -> Self {
        // SAFETY: i64 is always valid as a bit pattern
        unsafe { Data::from_raw(data as u64) }
    }
}

impl<T> Not for Data<T>
where
    Data<T>: Inner,
{
    type Output = <Self as Inner>::Target;

    #[inline(always)]
    fn not(self) -> Self::Output {
        self.inner()
    }
}

impl<T> PartialEq for Data<T>
where
    T: PartialEq + Copy,
    Data<T>: Inner,
    <Self as Inner>::Target: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.inner() == other.inner()
    }
}

impl PartialEq for Data<()> {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

impl<T> fmt::Display for Data<T>
where
    T: Copy,
    Data<T>: Inner,
    <Self as Inner>::Target: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.inner(), f)
    }
}

impl<T> fmt::Debug for Data<T>
where
    T: Copy,
    Data<T>: Inner,
    <Self as Inner>::Target: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.inner(), f)
    }
}

impl<'a, T> Deref for Data<&'a T>
where
    Self: Inner,
{
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        let ptr = self.into_raw() as *const T;
        unsafe { &*ptr }
    }
}

impl<'a, T> AsRef<T> for Data<&'a Allocation<T>> {
    #[inline(always)]
    fn as_ref<'b>(&'b self) -> &'a T {
        self.inner()
    }
}
