use crate::cons::Cons;
use crate::object::{IntoObject, LispFn, Object, RawObj, SubrFn};
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
        unsafe { transmute::<Object<'old>, Object<'new>>(self) }
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
        let bits: $crate::object::RawObj = $item.into();
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
    data: T,
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

    pub(crate) fn add<'ob, Input>(&'ob self, item: Input) -> Object<'ob>
    where
        Input: IntoObject<'ob, Object<'ob>>,
    {
        item.into_obj(self)
    }

    fn register(objects: &mut Vec<OwnedObject<'static>>, obj: OwnedObject) {
        objects.push(unsafe { obj.coerce_lifetime() });
    }

    pub(crate) fn alloc_f64(&self, obj: f64) -> &Allocation<f64> {
        let mut objects = self.objects.borrow_mut();
        Self::register(
            &mut objects,
            OwnedObject::Float(Box::new(Allocation::new(obj))),
        );
        if let Some(OwnedObject::Float(x)) = objects.last() {
            unsafe { &*(x.as_ref() as *const Allocation<f64>) }
        } else {
            unreachable!("object was not the type we just inserted");
        }
    }

    pub(crate) fn alloc_cons<'ob>(&'ob self, obj: Cons) -> &'ob Cons {
        let mut objects = self.objects.borrow_mut();
        Self::register(&mut objects, OwnedObject::Cons(Box::new(obj)));
        if let Some(OwnedObject::Cons(x)) = objects.last() {
            unsafe { transmute::<&Cons, &'ob Cons>(x.as_ref()) }
        } else {
            unreachable!("object was not the type we just inserted");
        }
    }

    pub(crate) fn alloc_string(&self, obj: String) -> &Allocation<String> {
        let mut objects = self.objects.borrow_mut();
        Self::register(
            &mut objects,
            OwnedObject::String(Box::new(Allocation::new(obj))),
        );
        if let Some(OwnedObject::String(x)) = objects.last_mut() {
            unsafe { &*(x.as_ref() as *const Allocation<_>) }
        } else {
            unreachable!("object was not the type we just inserted");
        }
    }

    pub(crate) fn alloc_vec<'ob>(
        &'ob self,
        obj: Vec<Object<'ob>>,
    ) -> &'ob Allocation<RefCell<Vec<Object<'ob>>>> {
        let mut objects = self.objects.borrow_mut();
        let ref_cell = RefCell::new(obj);
        if CONST {
            // Leak a borrow so that the vector cannot be borrowed mutably
            std::mem::forget(ref_cell.borrow());
        }
        Self::register(
            &mut objects,
            OwnedObject::Vec(Box::new(Allocation::new(ref_cell))),
        );
        if let Some(OwnedObject::Vec(x)) = objects.last() {
            unsafe { transmute::<&Allocation<_>, &'ob Allocation<_>>(x.as_ref()) }
        } else {
            unreachable!("object was not the type we just inserted");
        }
    }

    pub(crate) fn alloc_lisp_fn<'ob>(&'ob self, obj: LispFn<'ob>) -> &'ob Allocation<LispFn<'ob>> {
        let mut objects = self.objects.borrow_mut();
        Self::register(
            &mut objects,
            OwnedObject::LispFn(Box::new(Allocation::new(obj))),
        );
        if let Some(OwnedObject::LispFn(x)) = objects.last() {
            unsafe { transmute::<&Allocation<LispFn>, &'ob Allocation<LispFn>>(x.as_ref()) }
        } else {
            unreachable!("object was not the type we just inserted");
        }
    }

    pub(crate) fn alloc_subr_fn(&self, obj: SubrFn) -> &'static SubrFn {
        assert!(CONST, "Attempt to add subrFn to non-const arena");
        let mut objects = self.objects.borrow_mut();
        Self::register(&mut objects, OwnedObject::SubrFn(Box::new(obj)));
        if let Some(OwnedObject::SubrFn(x)) = objects.last() {
            unsafe { &*(x.as_ref() as *const SubrFn) }
        } else {
            unreachable!("object was not the type we just inserted");
        }
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

    pub(crate) fn bind<T, U>(&'ob self, obj: T) -> U
    where
        T: ConstrainLifetime<'ob, U>,
    {
        obj.constrain_lifetime(self)
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
        let obj = "foo".into_obj(&arena);
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
