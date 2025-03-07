use super::{CloneIn, Gc, IntoObject, NIL, Object, WithLifetime};
use crate::{
    core::gc::{Block, GcHeap, Slot},
    derive_GcMoveable,
};
use rune_core::hashmap::HashMap;
use rune_macros::Trace;
use std::{cell::RefCell, fmt};

#[derive(Debug, Eq, Trace)]
pub struct CharTableInner<'ob> {
    parent: RefCell<Option<Slot<&'ob CharTable>>>,
    data: RefCell<HashMap<usize, Slot<Object<'ob>>>>,
    init: Slot<Object<'ob>>,
}

impl<'ob> CharTableInner<'ob> {
    pub fn new(init: Option<Object<'ob>>) -> Self {
        CharTableInner {
            parent: RefCell::new(None),
            data: RefCell::new(HashMap::default()),
            init: Slot::new(init.unwrap_or(NIL)),
        }
    }
}

#[derive(PartialEq, Eq, Trace, Debug)]
pub(crate) struct CharTable(GcHeap<CharTableInner<'static>>);

derive_GcMoveable!(CharTable);

impl PartialEq for CharTableInner<'_> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl<'new> CloneIn<'new, &'new Self> for CharTable {
    fn clone_in<const C: bool>(&self, bk: &'new Block<C>) -> Gc<&'new Self> {
        let parent_clone =
            self.0.parent.borrow().as_ref().map(|p| Slot::new(p.clone_in(bk).untag()));
        let parent = RefCell::new(parent_clone);

        let mut data = HashMap::default();
        for (key, value) in self.0.data.borrow().iter() {
            let new_value = Slot::new(value.clone_in(bk));
            data.insert(*key, new_value);
        }
        let data = RefCell::new(data);
        let init = Slot::new(self.0.init.clone_in(bk));
        CharTableInner { parent, data, init }.into_obj(bk)
    }
}

impl CharTable {
    pub(in crate::core) unsafe fn new(table: CharTableInner<'_>, constant: bool) -> Self {
        // transmute lifetime to static
        let table =
            unsafe { std::mem::transmute::<CharTableInner<'_>, CharTableInner<'static>>(table) };
        Self(GcHeap::new(table, constant))
    }

    pub fn get(&self, idx: usize) -> Object {
        match self.0.data.borrow().get(&idx) {
            Some(x) => **x,
            None => *self.0.init,
        }
    }

    pub fn set(&self, idx: usize, item: Object) {
        unsafe { self.0.data.borrow_mut().insert(idx, Slot::new(item.with_lifetime())) };
    }

    pub fn set_parent(&self, new: Option<&Self>) {
        let new_ptr = new.map(|n| unsafe { Slot::new(n.with_lifetime()) });
        *self.0.parent.borrow_mut() = new_ptr;
    }
}

impl fmt::Display for CharTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        let data = self.0.data.borrow();

        let mut entries: Vec<_> = data.iter().collect();
        entries.sort_by_key(|&(key, _)| key);

        let mut iter = entries.into_iter();
        if let Some((_, first)) = iter.next() {
            write!(f, "{}", first)?;
            for (_, value) in iter {
                write!(f, " {}", value)?;
            }
        }
        write!(f, "]")
    }
}
