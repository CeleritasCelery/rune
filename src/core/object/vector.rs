use super::{CloneIn, Gc, IntoObject, MutObjCell, ObjCell, Object};
use crate::{
    core::gc::{Block, GcHeap, GcState, Trace},
    Markable,
};
use anyhow::{anyhow, Result};
use macro_attr_2018::macro_attr;
use newtype_derive_2018::*;
use rune_core::hashmap::HashSet;
use rune_macros::Trace;
use std::{
    fmt::{self, Write},
    mem,
    ops::Deref,
    ptr::addr_of,
};

#[derive(Eq)]
pub(crate) struct LispVecInner {
    is_const: bool,
    inner: Box<[ObjCell]>,
}

macro_attr! {
    /// A lisp vector. Unlike vectors in other languages this is not resizeable.
    /// This type is represented as slice of [`ObjCell`] which is immutable by
    /// default. However with the [try_mut](LispVecInner::try_mut) method, you can obtain a mutable view
    /// into this slice.
    #[derive(PartialEq, Eq, Trace, NewtypeDebug!, NewtypeDisplay!, NewtypeDeref!, Markable!)]
    pub(crate) struct LispVec(GcHeap<LispVecInner>);
}

impl PartialEq for LispVecInner {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}

impl LispVec {
    // SAFETY: Since this type does not have an object lifetime, it is only safe
    // to use in context of the allocator.
    pub(in crate::core) unsafe fn new(vec: Vec<Object>, constant: bool) -> Self {
        Self(GcHeap::new(LispVecInner::new(vec), constant))
    }

    pub(in crate::core) fn make_const(&mut self) {
        self.0.is_const = true;
    }

    pub(crate) fn to_vec(&self) -> Vec<Object> {
        // SAFETY: ObjCell and GcObj have the same representation.
        let obj_slice = unsafe { &*(addr_of!(*self.inner) as *const [Object]) };
        obj_slice.to_vec()
    }
}

impl LispVecInner {
    pub(crate) fn try_mut(&self) -> Result<&[MutObjCell]> {
        if self.is_const {
            Err(anyhow!("Attempt to mutate constant Vector"))
        } else {
            // SAFETY: ObjCell and MutObjCell have the same representation.
            unsafe { Ok(&*(addr_of!(*self.inner) as *const [MutObjCell])) }
        }
    }
}

impl Deref for LispVecInner {
    type Target = [ObjCell];

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'new> CloneIn<'new, &'new Self> for LispVec {
    fn clone_in<const C: bool>(&self, bk: &'new Block<C>) -> Gc<&'new Self> {
        let vec: Vec<Object> = self.iter().map(|x| x.get().clone_in(bk)).collect();
        vec.into_obj(bk)
    }
}

impl Trace for LispVecInner {
    fn trace(&self, state: &mut GcState) {
        let unmarked = self.iter().map(ObjCell::get).filter(|x| x.is_markable()).map(Gc::into_raw);
        state.stack().extend(unmarked);
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
    unsafe fn new(vec: Vec<Object>) -> Self {
        let cell = mem::transmute::<Vec<Object>, Vec<ObjCell>>(vec);
        Self { is_const: false, inner: cell.into_boxed_slice() }
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
pub(crate) struct RecordBuilder<'ob>(pub(crate) Vec<Object<'ob>>);

macro_attr! {
    #[derive(PartialEq, Eq, Trace, NewtypeDeref!, Markable!)]
    pub(crate) struct Record(GcHeap<LispVecInner>);
}

impl<'new> CloneIn<'new, &'new Self> for Record {
    fn clone_in<const C: bool>(&self, bk: &'new Block<C>) -> Gc<&'new Self> {
        let vec: Vec<Object> = self.iter().map(|x| x.get().clone_in(bk)).collect();
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
