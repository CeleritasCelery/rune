use super::{CloneIn, Gc, IntoObject, MutObjCell, ObjCell, Object};
use crate::{
    core::gc::{Block, GcHeap, GcState, Trace},
    NewtypeMarkable,
};
use anyhow::{anyhow, Result};
use bumpalo::collections::Vec as GcVec;
use macro_attr_2018::macro_attr;
use newtype_derive_2018::*;
use rune_core::hashmap::HashSet;
use rune_macros::Trace;
use std::{
    cell::Cell,
    fmt::{self, Write},
    ops::Deref,
    ptr::addr_of,
};

#[derive(Eq)]
pub(crate) struct LispVecInner {
    is_const: bool,
    inner: Cell<*const [ObjCell]>,
}

macro_attr! {
    /// A lisp vector. Unlike vectors in other languages this is not resizeable.
    /// This type is represented as slice of [`ObjCell`] which is immutable by
    /// default. However with the [try_mut](LispVecInner::try_mut) method, you can obtain a mutable view
    /// into this slice.
    #[derive(PartialEq, Eq, Trace, NewtypeDebug!, NewtypeDisplay!, NewtypeDeref!, NewtypeMarkable!)]
    pub(crate) struct LispVec(GcHeap<LispVecInner>);
}

impl PartialEq for LispVecInner {
    fn eq(&self, other: &Self) -> bool {
        self.get_slice() == other.get_slice()
    }
}

impl LispVec {
    // SAFETY: Since this type does not have an object lifetime, it is only safe
    // to use in context of the allocator.
    pub(in crate::core) unsafe fn new(ptr: *const [Object], constant: bool) -> Self {
        Self(GcHeap::new(LispVecInner::new(ptr, constant), constant))
    }

    pub(crate) fn to_vec(&self) -> Vec<Object> {
        // SAFETY: ObjCell and GcObj have the same representation.
        let obj_slice = unsafe { &*(addr_of!(*self.inner.get()) as *const [Object]) };
        obj_slice.to_vec()
    }
}

impl LispVecInner {
    fn get_slice(&self) -> &[ObjCell] {
        unsafe { &*self.inner.get() }
    }

    pub(crate) fn try_mut(&self) -> Result<&[MutObjCell]> {
        if self.is_const {
            Err(anyhow!("Attempt to mutate constant Vector"))
        } else {
            // SAFETY: ObjCell and MutObjCell have the same representation.
            unsafe { Ok(&*(self.inner.get() as *const [MutObjCell])) }
        }
    }
}

impl Deref for LispVecInner {
    type Target = [ObjCell];

    fn deref(&self) -> &Self::Target {
        self.get_slice()
    }
}

impl<'new> CloneIn<'new, &'new Self> for LispVec {
    fn clone_in<const C: bool>(&self, bk: &'new Block<C>) -> Gc<&'new Self> {
        let mut vec = GcVec::with_capacity_in(self.len(), &bk.objects);
        vec.extend(self.iter().map(|x| x.get().clone_in(bk)));
        vec.into_obj(bk)
    }
}

impl Trace for LispVecInner {
    fn trace(&self, state: &mut GcState) {
        assert!(!self.is_const, "Attempt to trace mutable vector");
        // Update the object pointers in the vector
        // move the vector to the to-space.
        //
        // TODO: can we update and move in one step? should be able to use
        // `alloc_slice_fill_iter`
        let slice = unsafe { &*(self.inner.get() as *mut [Object]) };
        let new = state.to_space.alloc_slice_copy(slice);
        let new = unsafe { std::mem::transmute::<&mut [Object], &mut [ObjCell]>(new) };
        for x in &mut *new {
            x.trace(state);
        }
        self.inner.set(new);
    }
}

impl fmt::Display for LispVecInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.display_walk(f, &mut HashSet::default())
    }
}

impl fmt::Debug for LispVecInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.display_walk(f, &mut HashSet::default())
    }
}

impl LispVecInner {
    unsafe fn new(ptr: *const [Object], is_const: bool) -> Self {
        let ptr = ptr as *mut [ObjCell];
        Self { is_const, inner: Cell::new(ptr) }
    }

    pub(super) fn display_walk(
        &self,
        f: &mut fmt::Formatter<'_>,
        seen: &mut HashSet<*const u8>,
    ) -> fmt::Result {
        let ptr = (self as *const Self).cast();
        if seen.contains(&ptr) {
            return write!(f, "#0");
        }
        seen.insert(ptr);

        f.write_char('[')?;
        for (i, x) in self.iter().enumerate() {
            if i != 0 {
                f.write_char(' ')?;
            }
            x.get().untag().display_walk(f, seen)?;
        }
        f.write_char(']')
    }
}

#[repr(transparent)]
pub(crate) struct RecordBuilder<'ob>(pub(crate) GcVec<'ob, Object<'ob>>);

macro_attr! {
    #[derive(PartialEq, Eq, Trace, NewtypeDeref!, NewtypeMarkable!)]
    pub(crate) struct Record(GcHeap<LispVecInner>);
}

impl<'new> CloneIn<'new, &'new Self> for Record {
    fn clone_in<const C: bool>(&self, bk: &'new Block<C>) -> Gc<&'new Self> {
        let mut vec = GcVec::with_capacity_in(self.len(), &bk.objects);
        vec.extend(self.iter().map(|x| x.get().clone_in(bk)));
        RecordBuilder(vec).into_obj(bk)
    }
}

impl fmt::Display for Record {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.display_walk(f, &mut HashSet::default())
    }
}

impl Record {
    fn display_walk(
        &self,
        f: &mut fmt::Formatter<'_>,
        seen: &mut HashSet<*const u8>,
    ) -> fmt::Result {
        let ptr = (self as *const Self).cast();
        if seen.contains(&ptr) {
            return write!(f, "#0");
        }
        seen.insert(ptr);
        write!(f, "#s(")?;
        for (i, x) in self.iter().enumerate() {
            if i != 0 {
                f.write_char(' ')?;
            }
            x.get().untag().display_walk(f, seen)?;
        }
        f.write_char(')')
    }
}
