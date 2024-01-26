use super::{CloneIn, Gc, GcObj, IntoObject, MutObjCell, ObjCell};
use crate::core::gc::{Block, GcHeap, Trace};
use anyhow::{anyhow, Result};
use rune_core::hashmap::HashSet;
use rune_macros::Trace;
use std::{
    fmt::{self, Write},
    mem,
    ops::Deref,
    ptr::addr_of,
};

mod sealed {
    use super::*;

    #[derive(Eq)]
    pub(crate) struct LispVecInner {
        pub(super) is_const: bool,
        pub(super) inner: Box<[ObjCell]>,
    }

    #[derive(Debug, PartialEq, Eq, Trace)]
    #[repr(transparent)]
    pub(crate) struct RecordInner(pub(super) LispVecInner);
}

pub(in crate::core) use sealed::{LispVecInner, RecordInner};

/// A lisp vector. Unlike vectors in other languages this is not resizeable.
/// This type is represented as slice of [`ObjCell`] which is immutable by
/// default. However with the [try_mut](LispVecInner::try_mut) method, you can obtain a mutable view
/// into this slice.
pub(crate) type LispVec = GcHeap<LispVecInner>;

impl PartialEq for LispVecInner {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}

impl LispVecInner {
    // SAFETY: Since this type does not have an object lifetime, it is only safe
    // to use in context of the allocator.
    pub(in crate::core) unsafe fn new(vec: Vec<GcObj>) -> Self {
        let cell = mem::transmute::<Vec<GcObj>, Vec<ObjCell>>(vec);
        Self { is_const: false, inner: cell.into_boxed_slice() }
    }

    pub(in crate::core) fn make_const(&mut self) {
        self.is_const = true;
    }

    pub(crate) fn try_mut(&self) -> Result<&[MutObjCell]> {
        if self.is_const {
            Err(anyhow!("Attempt to mutate constant Vector"))
        } else {
            // SAFETY: ObjCell and MutObjCell have the same representation.
            unsafe { Ok(&*(addr_of!(*self.inner) as *const [MutObjCell])) }
        }
    }

    pub(crate) fn to_vec(&self) -> Vec<GcObj> {
        // SAFETY: ObjCell and GcObj have the same representation.
        let obj_slice = unsafe { &*(addr_of!(*self.inner) as *const [GcObj]) };
        obj_slice.to_vec()
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
        let vec: Vec<GcObj> = self.iter().map(|x| x.get().clone_in(bk)).collect();
        vec.into_obj(bk)
    }
}

impl Trace for LispVecInner {
    fn trace(&self, stack: &mut Vec<super::RawObj>) {
        let unmarked = self.iter().map(ObjCell::get).filter(|x| x.is_markable()).map(Gc::into_raw);
        stack.extend(unmarked);
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
pub(crate) struct RecordBuilder<'ob>(pub(crate) Vec<GcObj<'ob>>);

pub(crate) type Record = GcHeap<RecordInner>;

impl Deref for RecordInner {
    type Target = LispVecInner;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'new> CloneIn<'new, &'new Self> for Record {
    fn clone_in<const C: bool>(&self, bk: &'new Block<C>) -> Gc<&'new Self> {
        let vec: Vec<GcObj> = self.iter().map(|x| x.get().clone_in(bk)).collect();
        RecordBuilder(vec).into_obj(bk)
    }
}

impl fmt::Display for RecordInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.display_walk(f, &mut HashSet::default())
    }
}

impl RecordInner {
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
