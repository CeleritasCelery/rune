use super::cons::Cons;
use super::env::GlobalSymbol;
use super::object::{Gc, GcObj, HashTable, IntoObject, LispFn, ObjVec, RawInto, WithLifetime};
use std::cell::{Cell, RefCell};
use std::fmt::Debug;
use std::mem::transmute;
use std::ops::Deref;
use std::sync::atomic::AtomicBool;

mod root;
mod trace;
pub(crate) use root::*;
pub(crate) use trace::*;

/// A global store of all gc roots. This struct should be passed to the [Context]
/// when it is created.
#[derive(Default, Debug)]
pub(crate) struct RootSet {
    roots: RefCell<Vec<*const dyn Trace>>,
}

/// A block of allocations. This type should be owned by [Context] and not used
/// directly.
#[derive(Debug)]
pub(crate) struct Block<const CONST: bool> {
    objects: RefCell<Vec<OwnedObject<'static>>>,
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

/// The owner of an object allocation. No references to
/// the object can outlive this.
#[derive(Debug)]
enum OwnedObject<'ob> {
    Float(Box<Allocation<f64>>),
    Cons(Box<Cons>),
    Vec(Box<Allocation<RefCell<Vec<GcObj<'ob>>>>>),
    ByteVec(Box<Allocation<RefCell<Vec<u8>>>>),
    HashTable(Box<Allocation<RefCell<HashTable<'ob>>>>),
    String(Box<Allocation<String>>),
    Symbol(Box<GlobalSymbol>),
    LispFn(Box<Allocation<LispFn<'ob>>>),
}

/// A container type that has a mark bit for garbage collection.
#[derive(Debug)]
pub(in crate::core) struct Allocation<T> {
    marked: Cell<bool>,
    pub(in crate::core) data: T,
}

impl<T> Allocation<T> {
    fn new(data: T) -> Self {
        Allocation {
            marked: Cell::from(false),
            data,
        }
    }

    pub(in crate::core) fn mark(&self) {
        self.marked.set(true);
    }

    fn unmark(&self) {
        self.marked.set(false);
    }

    pub(in crate::core) fn is_marked(&self) -> bool {
        self.marked.get()
    }
}

impl<T: PartialEq> PartialEq for Allocation<T> {
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data
    }
}

impl<'ob> OwnedObject<'ob> {
    unsafe fn coerce_lifetime(self) -> OwnedObject<'static> {
        transmute::<OwnedObject<'ob>, OwnedObject<'static>>(self)
    }

    fn unmark(&self) {
        match self {
            OwnedObject::Float(x) => x.unmark(),
            OwnedObject::Cons(x) => x.unmark(),
            OwnedObject::Vec(x) => x.unmark(),
            OwnedObject::ByteVec(x) => x.unmark(),
            OwnedObject::HashTable(x) => x.unmark(),
            OwnedObject::String(x) => x.unmark(),
            OwnedObject::Symbol(x) => x.unmark(),
            OwnedObject::LispFn(x) => x.unmark(),
        }
    }

    fn is_marked(&self) -> bool {
        match self {
            OwnedObject::Float(x) => x.is_marked(),
            OwnedObject::Cons(x) => x.is_marked(),
            OwnedObject::Vec(x) => x.is_marked(),
            OwnedObject::ByteVec(x) => x.is_marked(),
            OwnedObject::HashTable(x) => x.is_marked(),
            OwnedObject::String(x) => x.is_marked(),
            OwnedObject::Symbol(x) => x.is_marked(),
            OwnedObject::LispFn(x) => x.is_marked(),
        }
    }
}

pub(in crate::core) trait AllocObject
where
    Self: Sized,
{
    type Output;
    fn alloc_obj<const C: bool>(self, block: &Block<C>) -> *const Self::Output;
}

impl AllocObject for f64 {
    type Output = Allocation<f64>;
    fn alloc_obj<const C: bool>(self, block: &Block<C>) -> *const Self::Output {
        let mut objects = block.objects.borrow_mut();
        Block::<C>::register(
            &mut objects,
            OwnedObject::Float(Box::new(Allocation::new(self))),
        );
        if let Some(OwnedObject::Float(x)) = objects.last() {
            x.as_ref()
        } else {
            unreachable!("object was not the type we just inserted");
        }
    }
}

impl AllocObject for Cons {
    type Output = Cons;
    fn alloc_obj<const CONST: bool>(mut self, block: &Block<CONST>) -> *const Self::Output {
        let mut objects = block.objects.borrow_mut();
        if CONST {
            self.mark_const();
        }
        Block::<CONST>::register(&mut objects, OwnedObject::Cons(Box::new(self)));
        if let Some(OwnedObject::Cons(x)) = objects.last() {
            x.as_ref()
        } else {
            unreachable!("object was not the type we just inserted");
        }
    }
}

impl AllocObject for GlobalSymbol {
    type Output = GlobalSymbol;
    fn alloc_obj<const CONST: bool>(self, block: &Block<CONST>) -> *const Self::Output {
        let mut objects = block.objects.borrow_mut();
        Block::<CONST>::register(&mut objects, OwnedObject::Symbol(Box::new(self)));
        if let Some(OwnedObject::Symbol(x)) = objects.last() {
            x.as_ref()
        } else {
            unreachable!("object was not the type we just inserted");
        }
    }
}

impl AllocObject for String {
    type Output = Allocation<String>;

    fn alloc_obj<const C: bool>(self, block: &Block<C>) -> *const Self::Output {
        let mut objects = block.objects.borrow_mut();
        Block::<C>::register(
            &mut objects,
            OwnedObject::String(Box::new(Allocation::new(self))),
        );
        if let Some(OwnedObject::String(x)) = objects.last_mut() {
            x.as_ref()
        } else {
            unreachable!("object was not the type we just inserted");
        }
    }
}

impl<'ob> AllocObject for LispFn<'ob> {
    type Output = Allocation<LispFn<'ob>>;
    fn alloc_obj<const C: bool>(self, block: &Block<C>) -> *const Self::Output {
        let mut objects = block.objects.borrow_mut();
        let boxed = Box::new(Allocation::new(self));
        Block::<C>::register(&mut objects, OwnedObject::LispFn(boxed));
        if let Some(OwnedObject::LispFn(x)) = objects.last() {
            unsafe { transmute::<&Allocation<LispFn>, &'ob Allocation<LispFn>>(x.as_ref()) }
        } else {
            unreachable!("object was not the type we just inserted");
        }
    }
}

impl<'ob> AllocObject for ObjVec<'ob> {
    type Output = Allocation<RefCell<Self>>;

    fn alloc_obj<const CONST: bool>(self, block: &Block<CONST>) -> *const Self::Output {
        let mut objects = block.objects.borrow_mut();
        let ref_cell = RefCell::new(self);
        if CONST {
            // Leak a borrow so that the vector cannot be borrowed mutably
            std::mem::forget(ref_cell.borrow());
        }
        Block::<CONST>::register(
            &mut objects,
            OwnedObject::Vec(Box::new(Allocation::new(ref_cell))),
        );
        if let Some(OwnedObject::Vec(x)) = objects.last() {
            unsafe { transmute::<&Allocation<_>, &'ob Allocation<_>>(x.as_ref()) }
        } else {
            unreachable!("object was not the type we just inserted");
        }
    }
}

impl<'ob> AllocObject for Vec<u8> {
    type Output = Allocation<RefCell<Self>>;

    fn alloc_obj<const CONST: bool>(self, block: &Block<CONST>) -> *const Self::Output {
        let mut objects = block.objects.borrow_mut();
        let ref_cell = RefCell::new(self);
        if CONST {
            // Leak a borrow so that the vector cannot be borrowed mutably
            std::mem::forget(ref_cell.borrow());
        }
        Block::<CONST>::register(
            &mut objects,
            OwnedObject::ByteVec(Box::new(Allocation::new(ref_cell))),
        );
        if let Some(OwnedObject::ByteVec(x)) = objects.last() {
            unsafe { transmute::<&Allocation<_>, &'ob Allocation<_>>(x.as_ref()) }
        } else {
            unreachable!("object was not the type we just inserted");
        }
    }
}

impl<'ob> AllocObject for HashTable<'ob> {
    type Output = Allocation<RefCell<Self>>;

    fn alloc_obj<const CONST: bool>(self, block: &Block<CONST>) -> *const Self::Output {
        let mut objects = block.objects.borrow_mut();
        let ref_cell = RefCell::new(self);
        if CONST {
            // Leak a borrow so that the hash table cannot be borrowed mutably
            std::mem::forget(ref_cell.borrow());
        }
        Block::<CONST>::register(
            &mut objects,
            OwnedObject::HashTable(Box::new(Allocation::new(ref_cell))),
        );
        if let Some(OwnedObject::HashTable(x)) = objects.last() {
            unsafe { transmute::<&Allocation<_>, &'ob Allocation<_>>(x.as_ref()) }
        } else {
            unreachable!("object was not the type we just inserted");
        }
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

    fn register(objects: &mut Vec<OwnedObject<'static>>, obj: OwnedObject) {
        objects.push(unsafe { obj.coerce_lifetime() });
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
                (**x).mark(gray_stack);
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
        cx.garbage_collect(false);
        let cons = list!["foo", 1, false, "end"; cx];
        vec.as_mut(cx).push(cons);
        cx.garbage_collect(false);
    }
}
