use super::{CloneIn, Gc, IntoObject, MutObjCell, ObjCell, Object};
use crate::{
    core::gc::{Block, GcHeap, GcState, Trace},
    derive_GcMoveable,
};
use anyhow::{Result, anyhow};
use bumpalo::collections::Vec as GcVec;
use rune_core::hashmap::HashSet;
use rune_macros::Trace;
use std::{
    cell::Cell,
    fmt::{self, Write},
    ops::Deref,
    ptr::addr_of,
};

struct LispVecInner {
    is_const: bool,
    inner: Cell<*const [ObjCell]>,
}

/// A lisp vector. Unlike vectors in other languages this is not resizeable. This type is
/// represented as slice of [`ObjCell`] which is immutable by default. However with the
/// [try_mut](LispVec::try_mut) method, you can obtain a mutable view into this slice.
#[derive(PartialEq, Eq, Trace)]
pub(crate) struct LispVec(GcHeap<LispVecInner>);

derive_GcMoveable!(LispVec);

impl Deref for LispVec {
    type Target = [ObjCell];

    fn deref(&self) -> &Self::Target {
        self.0.get_slice()
    }
}

impl PartialEq for LispVecInner {
    fn eq(&self, other: &Self) -> bool {
        self.get_slice() == other.get_slice()
    }
}

impl Eq for LispVecInner {}

impl LispVec {
    // SAFETY: Since this type does not have an object lifetime, it is only safe
    // to use in context of the allocator.
    pub(in crate::core) unsafe fn new(ptr: *const [Object], constant: bool) -> Self {
        Self(GcHeap::new(LispVecInner::new(ptr, constant), constant))
    }

    pub(crate) fn to_vec(&self) -> Vec<Object<'_>> {
        // SAFETY: ObjCell and GcObj have the same representation.
        let obj_slice = unsafe { &*(addr_of!(*self.0.inner.get()) as *const [Object]) };
        obj_slice.to_vec()
    }
}

impl LispVecInner {
    fn get_slice(&self) -> &[ObjCell] {
        unsafe { &*self.inner.get() }
    }
}

impl LispVec {
    pub(crate) fn try_mut(&self) -> Result<&[MutObjCell]> {
        if self.0.is_const {
            Err(anyhow!("Attempt to mutate constant Vector"))
        } else {
            // SAFETY: ObjCell and MutObjCell have the same representation.
            unsafe { Ok(&*(self.0.inner.get() as *const [MutObjCell])) }
        }
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

impl fmt::Display for LispVec {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.display_walk(f, &mut HashSet::default())
    }
}

impl fmt::Debug for LispVec {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.display_walk(f, &mut HashSet::default())
    }
}

impl LispVec {
    pub(super) fn display_walk(
        &self,
        f: &mut fmt::Formatter,
        seen: &mut HashSet<*const u8>,
    ) -> fmt::Result {
        let ptr = (&*self.0 as *const LispVecInner).cast();
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

impl LispVecInner {
    unsafe fn new(ptr: *const [Object], is_const: bool) -> Self {
        let ptr = ptr as *mut [ObjCell];
        Self { is_const, inner: Cell::new(ptr) }
    }
}

#[repr(transparent)]
pub(crate) struct RecordBuilder<'ob>(pub(crate) GcVec<'ob, Object<'ob>>);

#[derive(PartialEq, Eq, Trace)]
pub(crate) struct Record(GcHeap<LispVecInner>);

derive_GcMoveable!(Record);

impl Deref for Record {
    type Target = [ObjCell];

    fn deref(&self) -> &Self::Target {
        self.0.get_slice()
    }
}

impl<'new> CloneIn<'new, &'new Self> for Record {
    fn clone_in<const C: bool>(&self, bk: &'new Block<C>) -> Gc<&'new Self> {
        let mut vec = GcVec::with_capacity_in(self.len(), &bk.objects);
        vec.extend(self.iter().map(|x| x.get().clone_in(bk)));
        RecordBuilder(vec).into_obj(bk)
    }
}

impl fmt::Display for Record {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.display_walk(f, &mut HashSet::default())
    }
}

impl Record {
    pub(crate) fn try_mut(&self) -> Result<&[MutObjCell]> {
        if self.0.is_const {
            Err(anyhow!("Attempt to mutate constant Vector"))
        } else {
            // SAFETY: ObjCell and MutObjCell have the same representation.
            unsafe { Ok(&*(self.0.inner.get() as *const [MutObjCell])) }
        }
    }

    pub(super) fn display_walk(
        &self,
        f: &mut fmt::Formatter,
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
