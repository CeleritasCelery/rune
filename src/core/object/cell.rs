use std::{cell::Cell, fmt};

use super::{GcObj, WithLifetime};

/// This type represents and immutable view into an Object. The reason we have
/// an additional type is because there could be other references to this same
/// cell that can change the underlying data, so this is wrapper around
/// `std::cell::Cell` type. It is not valid to mutate the data under a reference
/// unless it is inside an `Unsafe` Cell. However because this struct could also
/// be used in an immutable data structure (function constants), we need to
/// ensure that this cell cannot be mutated by default. This type is not safe to
/// be copy or clone.
#[derive(PartialEq, Eq)]
#[repr(transparent)]
pub(crate) struct ObjCell(Cell<GcObj<'static>>);

impl ObjCell {
    pub(crate) fn get(&self) -> GcObj {
        unsafe { self.0.get().with_lifetime() }
    }

    pub(in crate::core) unsafe fn new(obj: GcObj) -> Self {
        Self(Cell::new(obj.with_lifetime()))
    }

    /// Casts to a `MutObjCell`. Caller must ensure that the data structure is
    /// mutable.
    pub(in crate::core) unsafe fn as_mut(&self) -> &MutObjCell {
        &*(self as *const Self).cast()
    }
}

impl fmt::Display for ObjCell {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.0.get(), f)
    }
}

impl fmt::Debug for ObjCell {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

/// This represents a mutable view into an Object. See [`ObjCell`] for a more
/// detailed explanation. Holding this type means that we confirmed that the
/// data stucture is mutable, and we can use the [`MutObjCell::set`] method update this
/// cell.
#[derive(PartialEq)]
#[repr(transparent)]
pub(crate) struct MutObjCell(ObjCell);

impl std::ops::Deref for MutObjCell {
    type Target = ObjCell;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl MutObjCell {
    pub(crate) fn set(&self, value: GcObj) {
        unsafe {
            self.0 .0.set(value.with_lifetime());
        }
    }
}
