#![allow(unstable_name_collisions)]
use crate::core::env::sym::BUILTIN_SYMBOLS;
use crate::core::gc::{Block, Context, GcHeap, Trace};
use crate::core::object::{CloneIn, FunctionType, Gc, IntoObject, RawObj, TagType, WithLifetime};
use anyhow::{bail, Result};
use sptr::Strict;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::sync::atomic::{AtomicBool, AtomicPtr, Ordering};

mod sealed {
    use super::{AtomicBool, AtomicPtr, SymbolName};

    pub(crate) struct SymbolCellInner {
        pub(super) name: SymbolName,
        // We can't use AtomicCell due to this issue:
        // https://github.com/crossbeam-rs/crossbeam/issues/748
        pub(super) func: Option<AtomicPtr<u8>>,
        pub(super) special: AtomicBool,
    }
}

pub(in crate::core) use sealed::SymbolCellInner;

use super::Function;

/// The allocation of a global symbol. This is shared between threads, so the
/// interned value of a symbol will be the same location no matter which thread
/// interned it. Functions are safe to share between threads because they are
/// marked immutable by
/// [`ObjectMap::set_func`](`crate::core::env::ObjectMap::set_func`) and they
/// can only be replaced atomically. In order to garbage collect the function we
/// need to halt all running threads. This has not been implemented yet.
pub(crate) type SymbolCell = GcHeap<SymbolCellInner>;

#[derive(Debug)]
enum SymbolName {
    Interned(&'static str),
    Uninterned(Box<str>),
}

#[derive(PartialEq, Eq, Copy, Clone)]
pub(crate) struct Symbol<'a> {
    // Offset from the start of the symbol table
    data: *const SymbolCell,
    marker: PhantomData<&'a SymbolCell>,
}

impl std::ops::Deref for Symbol<'_> {
    type Target = SymbolCell;

    fn deref(&self) -> &Self::Target {
        self.get()
    }
}

impl<'a> Symbol<'a> {
    pub(crate) fn get(self) -> &'a SymbolCell {
        unsafe {
            let base = BUILTIN_SYMBOLS.as_ptr().addr();
            let ptr = self.data.map_addr(|x| x.wrapping_add(base));
            // If type was a static symbol then we need to give it provenance
            if BUILTIN_SYMBOLS.as_ptr_range().contains(&ptr) {
                &*BUILTIN_SYMBOLS.as_ptr().with_addr(ptr.addr())
            } else {
                &*ptr
            }
        }
    }

    pub(in crate::core) fn as_ptr(self) -> *const u8 {
        self.data.cast()
    }

    pub(in crate::core) unsafe fn from_offset_ptr(ptr: *const u8) -> Self {
        Self { data: ptr.cast(), marker: PhantomData }
    }

    pub(in crate::core) unsafe fn from_ptr(ptr: *const SymbolCell) -> Self {
        let base = BUILTIN_SYMBOLS.as_ptr().addr();
        let ptr = ptr.map_addr(|x| (x.wrapping_sub(base)));
        Self { data: ptr, marker: PhantomData }
    }

    pub(in crate::core) const fn new_builtin(idx: usize) -> Self {
        let ptr = sptr::invalid(idx * std::mem::size_of::<SymbolCell>());
        Self { data: ptr, marker: PhantomData }
    }

    pub(crate) fn make_special(self) {
        self.special.store(true, Ordering::Release);
    }

    pub(crate) fn is_special(self) -> bool {
        self.special.load(Ordering::Acquire)
    }
}

unsafe impl Send for Symbol<'_> {}

// implement withlifetime for symbolx
impl<'old, 'new> WithLifetime<'new> for Symbol<'old> {
    type Out = Symbol<'new>;

    unsafe fn with_lifetime(self) -> Self::Out {
        unsafe { std::mem::transmute(self) }
    }
}

impl Trace for Symbol<'_> {
    fn trace(&self, stack: &mut Vec<RawObj>) {
        self.get().trace(stack);
    }
}

impl Trace for SymbolCellInner {
    fn trace(&self, stack: &mut Vec<RawObj>) {
        // interned symbols are not collected yet
        if matches!(self.name, SymbolName::Uninterned(_)) {
            if let Some(func) = self.get() {
                func.as_obj().trace_mark(stack);
            }
        }
    }
}

impl fmt::Display for Symbol<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl fmt::Debug for Symbol<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl Hash for Symbol<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.data.hash(state);
    }
}

impl<'old, 'new> Symbol<'old> {
    pub(in crate::core) fn clone_in<const C: bool>(
        self,
        bk: &'new crate::core::gc::Block<C>,
    ) -> Gc<Symbol<'new>> {
        if let SymbolName::Uninterned(name) = &self.name {
            match bk.uninterned_symbol_map.get(self) {
                Some(new) => new.tag(),
                None => {
                    let sym = Symbol::new_uninterned(name, bk);
                    if let Some(old_func) = self.get().get() {
                        let new_func = old_func.clone_in(bk);
                        unsafe {
                            sym.set_func(new_func).unwrap();
                        }
                    }
                    let new = sym.into_obj(bk);
                    bk.uninterned_symbol_map.insert(self, new.untag());
                    new
                }
            }
        } else {
            unsafe { self.with_lifetime().tag() }
        }
    }
}

// Since symbols are globally unique we can
// compare them with a pointer equal test.
impl PartialEq for SymbolCell {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl Hash for SymbolCell {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let ptr: *const Self = self;
        ptr.hash(state);
    }
}

impl<'ob> Symbol<'ob> {
    pub(in crate::core) fn new(name: &'static str, block: &'ob Block<true>) -> Self {
        SymbolCell::new_normal(name, block).into_obj(block).untag()
    }

    pub(crate) fn new_uninterned<const C: bool>(name: &str, block: &'ob Block<C>) -> Self {
        SymbolCell::new_uninterned(name, block).into_obj(block).untag()
    }
}

impl SymbolCell {
    const NULL: *mut u8 = std::ptr::null_mut();
    #[allow(clippy::declare_interior_mutable_const)]
    const EMTPTY: AtomicPtr<u8> = AtomicPtr::new(Self::NULL);

    fn new_normal(name: &'static str, block: &Block<true>) -> Self {
        // We have to do this workaround because starts_with is not const
        if name.as_bytes()[0] == b':' {
            Self::new_const(name, block)
        } else {
            GcHeap::new(
                SymbolCellInner {
                    name: SymbolName::Interned(name),
                    func: Some(Self::EMTPTY),
                    special: AtomicBool::new(false),
                },
                block,
            )
        }
    }

    pub(in crate::core) const fn new_static(name: &'static str) -> Self {
        // We have to do this workaround because starts_with is not const
        if name.as_bytes()[0] == b':' {
            Self::new_static_const(name)
        } else {
            GcHeap::new_pure(SymbolCellInner {
                name: SymbolName::Interned(name),
                func: Some(Self::EMTPTY),
                special: AtomicBool::new(false),
            })
        }
    }

    pub(in crate::core) const fn new_static_special(name: &'static str) -> Self {
        GcHeap::new_pure(SymbolCellInner {
            name: SymbolName::Interned(name),
            func: Some(Self::EMTPTY),
            special: AtomicBool::new(true),
        })
    }

    fn new_const(name: &'static str, block: &Block<true>) -> Self {
        GcHeap::new(
            SymbolCellInner {
                name: SymbolName::Interned(name),
                func: None,
                special: AtomicBool::new(true),
            },
            block,
        )
    }

    pub(in crate::core) const fn new_static_const(name: &'static str) -> Self {
        GcHeap::new_pure(SymbolCellInner {
            name: SymbolName::Interned(name),
            func: None,
            special: AtomicBool::new(true),
        })
    }

    fn new_uninterned<const C: bool>(name: &str, block: &Block<C>) -> Self {
        GcHeap::new(
            SymbolCellInner {
                name: SymbolName::Uninterned(name.to_owned().into_boxed_str()),
                func: Some(Self::EMTPTY),
                special: AtomicBool::new(false),
            },
            block,
        )
    }
}

impl SymbolCellInner {
    const NULL: *mut u8 = std::ptr::null_mut();

    pub(crate) fn name(&self) -> &str {
        match &self.name {
            SymbolName::Interned(x) => x,
            SymbolName::Uninterned(x) => x,
        }
    }

    pub(crate) fn interned(&self) -> bool {
        matches!(self.name, SymbolName::Interned(_))
    }

    #[inline(always)]
    /// Check if the symbol is constant like nil, t, or :keyword
    pub(crate) fn is_const(&self) -> bool {
        self.func.is_none()
    }

    pub(crate) fn has_func(&self) -> bool {
        match &self.func {
            Some(func) => !func.load(Ordering::Acquire).is_null(),
            None => false,
        }
    }

    fn get(&self) -> Option<Function> {
        if let Some(func) = &self.func {
            let ptr = func.load(Ordering::Acquire);
            // nil is represented as zero (null pointer).
            if !ptr.is_null() {
                return Some(unsafe { Gc::from_raw_ptr(ptr) });
            }
        }
        None
    }

    pub(crate) fn func<'a>(&self, _cx: &'a Context) -> Option<Function<'a>> {
        self.get().map(|x| unsafe { x.with_lifetime() })
    }

    /// Follow the chain of symbols to find the function at the end, if any.
    pub(crate) fn follow_indirect<'ob>(&self, cx: &'ob Context) -> Option<Function<'ob>> {
        let func = self.func(cx)?;
        match func.untag() {
            FunctionType::Symbol(sym) => sym.follow_indirect(cx),
            _ => Some(func),
        }
    }

    /// Set the function for this symbol. This function is unsafe to call and
    /// requires that the caller:
    /// 1. Has marked the entire function as read only
    /// 2. Has cloned the function into the `SymbolMap` block
    /// 3. Ensured the symbol is not constant
    pub(in crate::core) unsafe fn set_func(&self, func: Function) -> Result<()> {
        let Some(fn_cell) = self.func.as_ref() else {
            bail!("Attempt to set a constant symbol: {self}")
        };
        let val = func.into_ptr().cast_mut();
        fn_cell.store(val, Ordering::Release);
        Ok(())
    }

    pub(crate) fn unbind_func(&self) {
        if let Some(func) = &self.func {
            func.store(Self::NULL, Ordering::Release);
        }
    }
}

impl fmt::Display for SymbolCellInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl fmt::Debug for SymbolCellInner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

#[derive(Default)]
/// When copying uninterned symbols, we need to ensure that all instances share
/// the same address if they did originally. This keeps a mapping from old
/// symbols to new.
pub(in crate::core) struct UninternedSymbolMap {
    map: std::cell::RefCell<Vec<(Symbol<'static>, Symbol<'static>)>>,
}

impl UninternedSymbolMap {
    fn get<'a>(&'a self, symbol: Symbol) -> Option<Symbol<'a>> {
        self.map.borrow().iter().find(|x| x.0 == symbol).map(|x| x.1)
    }

    fn insert(&self, old: Symbol, new: Symbol) {
        self.map
            .borrow_mut()
            .push(unsafe { (old.with_lifetime(), new.with_lifetime()) });
    }

    pub(in crate::core) fn clear(&self) {
        self.map.borrow_mut().clear();
    }
}
