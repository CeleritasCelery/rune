use super::{CloneIn, Gc, GcObj, IntoObject, ObjCell};
use crate::core::gc::{GcManaged, GcMark, Trace};
use crate::hashmap::{HashSet, IndexMap};
use std::cell::{BorrowMutError, Ref, RefCell, RefMut};
use std::fmt::{self, Debug, Display, Write};

pub(crate) type HashTable<'ob> = IndexMap<GcObj<'ob>, GcObj<'ob>>;
// pub(crate) type HashTableView<'ob, T> = IndexMap<GcObj<'ob>, T>;

#[derive(PartialEq, Eq)]
pub(crate) struct HashTableView<'ob, T> {
    pub(crate) iter_next: usize,
    inner: IndexMap<GcObj<'ob>, T>,
}

#[derive(Eq)]
pub(crate) struct LispHashTable {
    gc: GcMark,
    is_const: bool,
    inner: RefCell<HashTableView<'static, ObjCell>>,
}

impl<'ob, T> std::ops::Deref for HashTableView<'ob, T> {
    type Target = IndexMap<GcObj<'ob>, T>;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'ob, T> std::ops::DerefMut for HashTableView<'ob, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl PartialEq for LispHashTable {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner
    }
}

impl LispHashTable {
    // SAFETY: Since this type does not have an object lifetime, it is only safe
    // to create an owned version in context of the allocator.
    pub(in crate::core) unsafe fn new(vec: HashTable) -> Self {
        let cell =
            std::mem::transmute::<IndexMap<GcObj, GcObj>, IndexMap<GcObj<'static>, ObjCell>>(vec);
        Self {
            gc: GcMark::default(),
            is_const: false,
            inner: RefCell::new(HashTableView { iter_next: 0, inner: cell }),
        }
    }

    pub(in crate::core) fn make_const(&mut self) {
        self.is_const = true;
        // Leak the borrow so that is cannot be borrowed mutabley
        std::mem::forget(self.inner.borrow());
    }

    pub(crate) fn borrow<'a>(&'a self) -> Ref<'a, HashTableView<'a, ObjCell>> {
        unsafe {
            std::mem::transmute::<
                Ref<'a, HashTableView<'static, _>>,
                Ref<'a, HashTableView<'a, ObjCell>>,
            >(self.inner.borrow())
        }
    }

    pub(crate) fn try_borrow_mut(
        &self,
    ) -> Result<RefMut<'_, HashTableView<'_, GcObj<'_>>>, BorrowMutError> {
        unsafe {
            self.inner.try_borrow_mut().map(|x| {
                std::mem::transmute::<
                    RefMut<'_, HashTableView<'static, ObjCell>>,
                    RefMut<'_, HashTableView<'_, GcObj<'_>>>,
                >(x)
            })
        }
    }
}

impl<'new> CloneIn<'new, &'new Self> for LispHashTable {
    fn clone_in<const C: bool>(&self, bk: &'new crate::core::gc::Block<C>) -> Gc<&'new Self> {
        let mut table = HashTable::default();
        let borrow = self.borrow();
        for (key, value) in &borrow.inner {
            let new_key = key.clone_in(bk);
            let new_value = value.get().clone_in(bk);
            table.insert(new_key, new_value);
        }
        table.into_obj(bk)
    }
}

impl Trace for LispHashTable {
    fn trace(&self, stack: &mut Vec<super::RawObj>) {
        let table = self.borrow();
        for (k, v) in &table.inner {
            if k.is_markable() {
                stack.push(k.into_raw());
            }
            if v.get().is_markable() {
                stack.push(v.get().into_raw());
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

impl Debug for LispHashTable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.display_walk(f, &mut HashSet::default())
    }
}

impl Display for LispHashTable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.display_walk(f, &mut HashSet::default())
    }
}

impl LispHashTable {
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

        write!(f, "#s(hash-table (")?;
        for (i, (k, v)) in self.borrow().inner.iter().enumerate() {
            if i != 0 {
                f.write_char(' ')?;
            }
            k.untag().display_walk(f, seen)?;
            f.write_char(' ')?;
            v.get().untag().display_walk(f, seen)?;
        }
        write!(f, "))")
    }
}
