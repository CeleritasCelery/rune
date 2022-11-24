use super::OwnedObject;
use crate::core::object::{Gc, GcObj, IntoObject, RawInto, WithLifetime};
use std::cell::{Cell, RefCell};
use std::fmt::Debug;
use std::ops::Deref;
use std::sync::atomic::AtomicBool;

use super::Trace;

/// A global store of all gc roots. This struct should be passed to the [Context]
/// when it is created.
#[derive(Default, Debug)]
pub(crate) struct RootSet {
    pub(super) roots: RefCell<Vec<*const dyn Trace>>,
}

/// A block of allocations. This type should be owned by [Context] and not used
/// directly.
#[derive(Debug)]
pub(crate) struct Block<const CONST: bool> {
    pub(super) objects: RefCell<Vec<OwnedObject>>,
}

/// Owns all allocations and creates objects. All objects have
/// a lifetime tied to the borrow of their `Context`. When the
/// `Context` goes out of scope, no objects should be accessible.
#[derive(Debug)]
pub(crate) struct Context<'rt> {
    pub(crate) block: Block<false>,
    root_set: &'rt RootSet,
    prev_obj_count: usize,
}

impl<'rt> Drop for Context<'rt> {
    fn drop(&mut self) {
        self.garbage_collect(true);
        assert!(
            std::thread::panicking() || self.block.objects.borrow().is_empty(),
            "Error: Context was dropped while still holding data"
        );
    }
}

#[derive(Debug, Default)]
pub(in crate::core) struct GcMark(Cell<bool>);

impl Trace for GcMark {
    fn trace(&self, _: &mut Vec<crate::core::object::RawObj>) {
        self.0.set(true);
    }
}

/// This trait represents a type that is managed by the Garbage collector and
/// therefore has a markbit to preserve it.
pub(in crate::core) trait GcManaged {
    fn get_mark(&self) -> &GcMark;

    fn mark(&self) {
        self.get_mark().0.set(true);
    }

    fn unmark(&self) {
        self.get_mark().0.set(false);
    }

    fn is_marked(&self) -> bool {
        self.get_mark().0.get()
    }
}

impl PartialEq for GcMark {
    #[inline(always)]
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

thread_local! {
    static SINGLETON_CHECK: Cell<bool> = Cell::new(false);
}

static GLOBAL_CHECK: AtomicBool = AtomicBool::new(false);

impl Block<true> {
    pub(crate) fn new_global() -> Self {
        use std::sync::atomic::Ordering::SeqCst as Rel;
        assert!(GLOBAL_CHECK.compare_exchange(false, true, Rel, Rel).is_ok());
        Self {
            objects: RefCell::new(Vec::new()),
        }
    }
}

impl<const CONST: bool> Block<CONST> {
    pub(crate) fn new_local() -> Self {
        SINGLETON_CHECK.with(|x| {
            assert!(
                !x.get(),
                "There was already and active context when this context was created"
            );
            x.set(true);
        });
        Self {
            objects: RefCell::new(Vec::new()),
        }
    }

    #[allow(clippy::useless_conversion)]
    pub(crate) fn add<'ob, T, U, V>(&'ob self, obj: T) -> Gc<V>
    where
        T: IntoObject<Out<'ob> = U>,
        Gc<U>: Into<Gc<V>>,
    {
        obj.into_obj(self).into()
    }

    pub(super) fn register(objects: &mut Vec<OwnedObject>, obj: OwnedObject) {
        objects.push(obj);
    }
}

impl<'ob, 'rt> Context<'rt> {
    pub(crate) fn new(roots: &'rt RootSet) -> Self {
        Context {
            block: Block::new_local(),
            root_set: roots,
            prev_obj_count: 0,
        }
    }

    #[allow(clippy::unused_self)]
    pub(crate) fn bind<T>(&'ob self, obj: T) -> <T as WithLifetime>::Out
    where
        T: WithLifetime<'ob>,
    {
        unsafe { obj.with_lifetime() }
    }

    #[allow(clippy::unused_self)]
    pub(crate) unsafe fn rebind_raw_ptr<T, U>(&'ob self, raw: T) -> U
    where
        T: RawInto<U>,
    {
        raw.raw_into()
    }

    pub(crate) fn get_root_set(&'ob self) -> &'rt RootSet {
        self.root_set
    }

    pub(crate) fn garbage_collect(&mut self, force: bool) {
        let mut objects = self.block.objects.borrow_mut();
        if cfg!(not(test))
            && !force
            && (objects.len() < 2000 || objects.len() < (self.prev_obj_count * 2))
        {
            return;
        }
        let gray_stack = &mut Vec::new();
        for x in self.root_set.roots.borrow().iter() {
            // SAFETY: The contact of root structs will ensure that it removes
            // itself from this list before it drops.
            unsafe {
                (**x).trace(gray_stack);
            }
        }
        while !gray_stack.is_empty() {
            let raw = gray_stack.pop().unwrap();
            let obj = unsafe { GcObj::from_raw(raw) };
            if !obj.is_marked() {
                obj.trace_mark(gray_stack);
            }
        }

        // let prev = objects.len();
        objects.retain_mut(|x| {
            let marked = x.is_marked();
            if marked {
                x.unmark();
            }
            marked
        });
        // let retained = prev - objects.len();
        // println!("garbage collected: {retained}/{prev}");
        self.prev_obj_count = objects.len();
    }
}

impl OwnedObject {
    fn unmark(&self) {
        match self {
            OwnedObject::Float(x) => x.unmark(),
            OwnedObject::Cons(x) => x.unmark(),
            OwnedObject::Vec(x) => x.unmark(),
            OwnedObject::HashTable(x) => x.unmark(),
            OwnedObject::String(x) => x.unmark(),
            OwnedObject::Symbol(x) => x.unmark(),
            OwnedObject::ByteFn(x) => x.unmark(),
        }
    }

    fn is_marked(&self) -> bool {
        match self {
            OwnedObject::Float(x) => x.is_marked(),
            OwnedObject::Cons(x) => x.is_marked(),
            OwnedObject::Vec(x) => x.is_marked(),
            OwnedObject::HashTable(x) => x.is_marked(),
            OwnedObject::String(x) => x.is_marked(),
            OwnedObject::Symbol(x) => x.is_marked(),
            OwnedObject::ByteFn(x) => x.is_marked(),
        }
    }
}

impl<'rt> Deref for Context<'rt> {
    type Target = Block<false>;

    fn deref(&self) -> &Self::Target {
        &self.block
    }
}

impl<'rt> AsRef<Block<false>> for Context<'rt> {
    fn as_ref(&self) -> &Block<false> {
        &self.block
    }
}

impl<const CONST: bool> Drop for Block<CONST> {
    // Only one block can exist in a thread at a time. This part of that
    // contract.
    fn drop(&mut self) {
        SINGLETON_CHECK.with(|s| {
            assert!(s.get(), "Context singleton check was overwritten");
            s.set(false);
        });
    }
}

/// Rebinds an object so that it is bound to an immutable borrow of [Context]
/// instead of a mutable borrow. This can release the mutable borrow and allow
/// Context to be used for other things.
///
/// # Examples
///
/// ```
/// let object = func_taking_mut_context(&mut cx);
/// rebind!(object, cx);
/// ```
#[macro_export]
macro_rules! rebind {
    ($value:expr, $cx:ident) => {
        unsafe {
            let bits = $value.into_raw();
            $cx.rebind_raw_ptr(bits)
        }
    };
}

#[cfg(test)]
mod test {
    use crate::root;

    use super::*;
    fn bind_to_mut<'ob>(cx: &'ob mut Context) -> GcObj<'ob> {
        cx.add("invariant")
    }

    #[test]
    fn test_reborrow() {
        let roots = &RootSet::default();
        let mut cx = Context::new(roots);
        let obj = rebind!(bind_to_mut(&mut cx), cx);
        let _ = "foo".into_obj(&cx);
        assert_eq!(obj, "invariant");
    }

    #[test]
    fn garbage_collect() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        let vec1: Vec<GcObj> = Vec::new();
        root!(vec, vec1, cx);
        cx.garbage_collect(true);
        let cons = list!["foo", 1, false, "end"; cx];
        vec.as_mut(cx).push(cons);
        cx.garbage_collect(true);
    }
}
