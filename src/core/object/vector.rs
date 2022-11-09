use anyhow::{anyhow, Result};
use std::{
    cell::{BorrowMutError, Cell, Ref, RefCell, RefMut},
    fmt::Display,
    ops::Deref,
};

use crate::{
    core::gc::{GcManaged, GcMark, Trace},
    hashmap::HashMap,
};

use super::{GcObj, WithLifetime};

/// A lisp vector. Unlike vectors in other languages this is not resizeable.
/// This type is represented as slice of [`ObjCell`] which is immutable by
/// default. However with the [`try_mut`] method, you can obtain a mutable view
/// into this slice.
#[derive(Debug)]
pub(crate) struct LispVec {
    gc: GcMark,
    is_const: bool,
    inner: Box<[ObjCell]>,
}

impl PartialEq for LispVec {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}

/// This type represents and immutable view into an Object. The reason we have
/// an additional type is because there could be other references to this same
/// cell that can change the underlying data, so this is wrapper around
/// `std::cell::Cell` type. It is not valid to mutate the data under a reference
/// unless it is inside an `Unsafe` Cell. However because this struct could also
/// be used in an immutable data structure (function constants), we need to
/// ensure that this cell cannot be mutated by default.
#[derive(Debug, PartialEq)]
#[repr(transparent)]
pub(crate) struct ObjCell(Cell<GcObj<'static>>);

impl ObjCell {
    pub(crate) fn get(&self) -> GcObj {
        unsafe { self.0.get().with_lifetime() }
    }
}

impl Display for ObjCell {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.get().fmt(f)
    }
}

/// This represents a mutable view into an Object. See [`ObjCell`] for a more
/// detailed explanation. Holding this type means that we confirmed that the
/// data stucture is mutable, and we can use the [`set`] method update this
/// cell.
#[derive(Debug, PartialEq)]
#[repr(transparent)]
pub(crate) struct MutObjCell(ObjCell);

impl Deref for MutObjCell {
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

impl LispVec {
    // SAFETY: Since this type does not have an object lifetime, it is only safe
    // to use in context of the allocator.
    pub(in crate::core) unsafe fn new(vec: Vec<GcObj>) -> Self {
        let cell = std::mem::transmute::<Vec<GcObj>, Vec<ObjCell>>(vec);
        Self {
            gc: GcMark::default(),
            is_const: false,
            inner: cell.into_boxed_slice(),
        }
    }

    pub(in crate::core) fn make_const(&mut self) {
        self.is_const = true;
    }

    pub(crate) fn try_mut(&self) -> Result<&[MutObjCell]> {
        if self.is_const {
            Err(anyhow!("Attempt to mutate constant Vector"))
        } else {
            let inner: &[ObjCell] = self;
            // SAFETY: ObjCell and MutObjCell have the same representation.
            unsafe { Ok(&*(inner as *const [ObjCell] as *const [MutObjCell])) }
        }
    }

    // TODO: is this safe? it is a shallow clone
    pub(crate) fn clone_vec(&self) -> Vec<GcObj> {
        let cell_slice: &[ObjCell] = &self.inner;
        // SAFETY: ObjCell and GcObj have the same representation.
        let obj_slice: &[GcObj] = unsafe { &*(cell_slice as *const [ObjCell] as *const [GcObj]) };
        obj_slice.to_vec()
    }

    pub(in crate::core) fn clone_in<'new, const C: bool>(
        &self,
        bk: &'new crate::core::gc::Block<C>,
    ) -> Vec<GcObj<'new>> {
        self.iter().map(|x| x.get().clone_in(bk)).collect()
    }
}

impl Deref for LispVec {
    type Target = [ObjCell];

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'old, 'new> WithLifetime<'new> for &'old LispVec {
    type Out = &'new LispVec;

    unsafe fn with_lifetime(self) -> Self::Out {
        &*(self as *const LispVec)
    }
}

impl GcManaged for LispVec {
    fn get_mark(&self) -> &GcMark {
        &self.gc
    }
}

impl Trace for LispVec {
    fn trace(&self, stack: &mut Vec<super::RawObj>) {
        self.mark();
        let unmarked = self
            .iter()
            .filter_map(|x| x.get().is_markable().then(|| x.get().into_raw()));
        stack.extend(unmarked);
    }
}

#[repr(transparent)]
pub(crate) struct RecordBuilder<'ob>(pub(crate) Vec<GcObj<'ob>>);

#[derive(Debug)]
#[repr(transparent)]
pub(crate) struct Record(LispVec);

impl Deref for Record {
    type Target = LispVec;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub(crate) type HashTable<'ob> = HashMap<GcObj<'ob>, GcObj<'ob>>;
#[derive(Debug)]
pub(crate) struct LispHashTable {
    gc: GcMark,
    inner: RefCell<HashTable<'static>>,
}

impl LispHashTable {
    // SAFETY: Since this type does not have an object lifetime, it is only safe
    // to use in context of the allocator.
    pub(in crate::core) unsafe fn new(vec: HashTable) -> Self {
        let cell = std::mem::transmute::<HashTable<'_>, HashTable<'static>>(vec);
        Self {
            gc: GcMark::default(),
            inner: RefCell::new(cell),
        }
    }

    pub(in crate::core) fn make_const(&self) {
        // Leak the borrow so that is cannot be borrowed mutabley
        std::mem::forget(self.inner.borrow());
    }

    pub(crate) fn borrow<'a>(&'a self) -> Ref<'a, HashTable<'a>> {
        unsafe {
            std::mem::transmute::<Ref<'a, HashTable<'static>>, Ref<'a, HashTable<'a>>>(
                self.inner.borrow(),
            )
        }
    }

    pub(crate) fn try_borrow_mut(&self) -> Result<RefMut<'_, HashTable<'_>>, BorrowMutError> {
        unsafe {
            self.inner.try_borrow_mut().map(|x| {
                std::mem::transmute::<RefMut<'_, HashTable<'static>>, RefMut<'_, HashTable<'_>>>(x)
            })
        }
    }
}

impl Trace for LispHashTable {
    fn trace(&self, stack: &mut Vec<super::RawObj>) {
        let table = self.borrow();
        for (k, v) in &*table {
            if k.is_markable() {
                stack.push(k.into_raw());
            }
            if v.is_markable() {
                stack.push(v.into_raw());
            }
        }
        self.mark();
    }
}

impl GcManaged for LispHashTable {
    fn get_mark(&self) -> &GcMark {
        &self.gc
    }
}
