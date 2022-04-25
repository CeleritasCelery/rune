use crate::cons::Cons;
use crate::object::{Gc, IntoObject, LispFn, ObjVec, Object, RawObj, SubrFn, WithLifetime};
use std::cell::{Cell, RefCell};
use std::fmt::Debug;
use std::mem::transmute;
use std::ops::Deref;
use std::sync::atomic::AtomicBool;

mod root;
mod trace;
pub(crate) use root::*;
pub(crate) use trace::*;

pub(crate) trait ConstrainLifetime<'new, T> {
    fn constrain_lifetime<const C: bool>(self, cx: &'new Block<C>) -> T;
}

impl<'old, 'new> ConstrainLifetime<'new, Object<'new>> for Object<'old> {
    fn constrain_lifetime<const C: bool>(self, _cx: &'new Block<C>) -> Object<'new> {
        // Lifetime is bound to borrow of Block, so it is safe to extend
        unsafe {self.with_lifetime()}
    }
}

impl<'old, 'new, 'brw> ConstrainLifetime<'new, &'brw [Object<'new>]> for &'brw [Object<'old>] {
    fn constrain_lifetime<const C: bool>(self, _cx: &'new Block<C>) -> &'brw [Object<'new>] {
        // Lifetime is bound to borrow of Block, so it is safe to extend
        unsafe { transmute::<&[Object<'old>], &[Object<'new>]>(self) }
    }
}

/// Rebinds an object so that it is bound to an immutable borrow of [Arena]
/// instead of a mutable borrow. This can release the mutable borrow and allow
/// arena to be used for other things.
///
/// # Examples
///
/// ```
/// let object = func_taking_mut_arena(&mut Arena);
/// rebind!(object, arena);
/// ```
#[macro_export]
macro_rules! rebind {
    ($item:ident, $arena:ident) => {
        #[allow(unused_qualifications)]
        let bits: $crate::object::RawObj = $item.into_raw();
        let $item = unsafe { $arena.rebind_raw_ptr(bits) };
    };
}

/// A global store of all gc roots. This struct should be passed to the [Arena]
/// when it is created.
#[derive(Default, Debug)]
pub(crate) struct RootSet {
    roots: RefCell<Vec<Object<'static>>>,
    root_structs: RefCell<Vec<*const dyn Trace>>,
}

/// A block of allocations. This type should be owned by [Arena] and not used
/// directly.
#[derive(Debug)]
pub(crate) struct Block<const CONST: bool> {
    objects: RefCell<Vec<OwnedObject<'static>>>,
}

/// Owns all allocations and creates objects. All objects have
/// a lifetime tied to the borrow of their `Arena`. When the
/// `Arena` goes out of scope, no objects should be accessible.
#[derive(Debug)]
pub(crate) struct Arena<'rt> {
    pub(crate) block: Block<false>,
    root_set: &'rt RootSet,
    prev_obj_count: usize,
}

impl<'rt> Drop for Arena<'rt> {
    fn drop(&mut self) {
        self.garbage_collect();
        assert!(
            self.block.objects.borrow().is_empty(),
            "Arena was dropped while still holding data"
        );
    }
}

/// The owner of an object allocation. No references to
/// the object can outlive this.
#[derive(Debug)]
enum OwnedObject<'ob> {
    Float(Box<Allocation<f64>>),
    Cons(Box<Cons>),
    Vec(Box<Allocation<RefCell<Vec<Object<'ob>>>>>),
    String(Box<Allocation<String>>),
    LispFn(Box<Allocation<LispFn<'ob>>>),
    SubrFn(Box<SubrFn>),
}

/// A container type that has a mark bit for garbage collection.
#[derive(Debug)]
pub(crate) struct Allocation<T> {
    marked: Cell<bool>,
    pub(crate) data: T,
}

impl<T> Allocation<T> {
    fn new(data: T) -> Self {
        Allocation {
            marked: Cell::from(false),
            data,
        }
    }

    pub(crate) fn mark(&self) {
        self.marked.set(true);
    }

    fn unmark(&self) {
        self.marked.set(false);
    }

    pub(crate) fn is_marked(&self) -> bool {
        self.marked.get()
    }
}

impl<T: PartialEq> PartialEq for Allocation<T> {
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data
    }
}

impl<T> Deref for Allocation<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.data
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
            OwnedObject::String(x) => x.unmark(),
            OwnedObject::LispFn(x) => x.unmark(),
            OwnedObject::SubrFn(_) => {}
        }
    }

    fn is_marked(&self) -> bool {
        match self {
            OwnedObject::Float(x) => x.is_marked(),
            OwnedObject::Cons(x) => x.is_marked(),
            OwnedObject::Vec(x) => x.is_marked(),
            OwnedObject::String(x) => x.is_marked(),
            OwnedObject::LispFn(x) => x.is_marked(),
            OwnedObject::SubrFn(_) => true,
        }
    }
}

pub(crate) trait AllocObject
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
    fn alloc_obj<const C: bool>(self, block: &Block<C>) -> *const Self::Output {
        let mut objects = block.objects.borrow_mut();
        Block::<C>::register(&mut objects, OwnedObject::Cons(Box::new(self)));
        if let Some(OwnedObject::Cons(x)) = objects.last() {
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

impl AllocObject for SubrFn {
    type Output = SubrFn;
    fn alloc_obj<const CONST: bool>(self, block: &Block<CONST>) -> *const Self::Output {
        assert!(CONST, "Attempt to add subrFn to non-const arena");
        let mut objects = block.objects.borrow_mut();
        let boxed = Box::new(self);
        Block::<CONST>::register(&mut objects, OwnedObject::SubrFn(boxed));
        if let Some(OwnedObject::SubrFn(x)) = objects.last() {
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

thread_local! {
    static SINGLETON_CHECK: Cell<bool> = Cell::new(false);
}

static GLOBAL_CHECK: AtomicBool = AtomicBool::new(false);

impl Block<true> {
    pub(crate) fn new_global() -> Self {
        use std::sync::atomic::Ordering::Relaxed as Rel;
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
                "There was already and active arena when this arena was created"
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
        T: IntoObject<'ob, Out = U>,
        Gc<U>: Into<Gc<V>>,
    {
        obj.into_obj(self).into()
    }

    fn register(objects: &mut Vec<OwnedObject<'static>>, obj: OwnedObject) {
        objects.push(unsafe { obj.coerce_lifetime() });
    }
}

impl<'ob, 'rt> Arena<'rt> {
    pub(crate) fn new(roots: &'rt RootSet) -> Self {
        Arena {
            block: Block::new_local(),
            root_set: roots,
            prev_obj_count: 0,
        }
    }

    #[allow(clippy::unused_self)]
    pub(crate) fn bind<T, U>(&'ob self, obj: T) -> U
    where
        T: WithLifetime<'ob, Out = U>,
    {
        unsafe { obj.with_lifetime() }
    }

    #[allow(clippy::unused_self)]
    pub(crate) unsafe fn rebind_raw_ptr(&'ob self, raw: RawObj) -> Object<'ob> {
        Object::from_raw(raw)
    }

    pub(crate) fn get_root_set(&'ob self) -> &'rt RootSet {
        self.root_set
    }

    pub(crate) fn garbage_collect(&mut self) {
        let mut objects = self.block.objects.borrow_mut();
        #[cfg(not(test))]
        if objects.len() < 1000 || objects.len() < (self.prev_obj_count * 2) {
            return;
        }
        let gray_stack = &mut Vec::new();
        for x in self.root_set.roots.borrow().iter() {
            x.trace_mark(gray_stack);
        }
        for x in self.root_set.root_structs.borrow().iter() {
            // SAFETY: The contact of root structs will ensure that it removes
            // itself from this list before it drops.
            unsafe {
                (&**x).mark(gray_stack);
            }
        }
        while !gray_stack.is_empty() {
            let raw = gray_stack.pop().unwrap();
            let obj = unsafe { Object::from_raw(raw) };
            if !obj.is_marked() {
                obj.trace_mark(gray_stack);
            }
        }

        let prev = objects.len();
        objects.retain(OwnedObject::is_marked);
        let retained = prev - objects.len();
        println!("garbage collected: {retained}/{prev}");
        objects.iter().for_each(OwnedObject::unmark);
        self.prev_obj_count = objects.len();
    }
}

impl<'rt> Deref for Arena<'rt> {
    type Target = Block<false>;

    fn deref(&self) -> &Self::Target {
        &self.block
    }
}

impl<'rt> AsRef<Block<false>> for Arena<'rt> {
    fn as_ref(&self) -> &Block<false> {
        &self.block
    }
}

impl<const CONST: bool> Drop for Block<CONST> {
    // Only one block can exist in a thread at a time. This part of that
    // contract.
    fn drop(&mut self) {
        SINGLETON_CHECK.with(|s| {
            assert!(s.get(), "Arena singleton check was overwritten");
            s.set(false);
        });
    }
}

#[cfg(test)]
mod test {
    use super::*;
    fn take_mut_arena(_: &mut Arena) {}
    fn bind_to_mut<'ob>(arena: &'ob mut Arena) -> Object<'ob> {
        arena.add("invariant")
    }

    #[test]
    fn test_stack_root() {
        let roots = &RootSet::default();
        let mut arena = Arena::new(roots);
        let obj = arena.add("foo");
        crate::root!(obj, arena);
        take_mut_arena(&mut arena);
        assert_eq!(obj, "foo");
    }

    #[test]
    fn test_reborrow() {
        let roots = &RootSet::default();
        let mut arena = Arena::new(roots);
        let obj = bind_to_mut(&mut arena);
        rebind!(obj, arena);
        let _ = "foo".into_obj(&arena);
        assert_eq!(obj, "invariant");
    }
}
