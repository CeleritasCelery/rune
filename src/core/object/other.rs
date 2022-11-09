use std::cell::{BorrowMutError, Ref, RefCell, RefMut};
use std::fmt::{Debug, Display};
use std::ops::Deref;

use crate::{
    core::gc::{GcManaged, GcMark, Trace},
    hashmap::HashMap,
};

use super::GcObj;

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

impl Display for LispHashTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

#[derive(PartialEq)]
pub(crate) struct LispFloat {
    gc: GcMark,
    float: f64,
}

impl LispFloat {
    pub(in crate::core) fn new(float: f64) -> Self {
        Self {
            gc: GcMark::default(),
            float,
        }
    }
}

impl Deref for LispFloat {
    type Target = f64;

    fn deref(&self) -> &Self::Target {
        &self.float
    }
}

impl GcManaged for LispFloat {
    fn get_mark(&self) -> &GcMark {
        &self.gc
    }
}

impl Display for LispFloat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let float = self.float;
        if float.fract() == 0.0_f64 {
            write!(f, "{float:.1}")
        } else {
            write!(f, "{float}")
        }
    }
}

impl Debug for LispFloat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}
