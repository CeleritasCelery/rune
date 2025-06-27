use crate::core::env::sym::BUILTIN_SYMBOLS;
use crate::core::gc::{Block, Context, GcHeap, GcMoveable, GcState, Trace, TracePtr};
use crate::core::object::{CloneIn, Function, FunctionType, Gc, IntoObject, TagType, WithLifetime};
use anyhow::{Result, bail};
use std::cell::Cell;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::sync::atomic::{AtomicBool, AtomicPtr, Ordering};

/// The allocation of a global symbol. This is shared between threads, so the
/// interned value of a symbol will be the same location no matter which thread
/// interned it. Functions are safe to share between threads because they are
/// marked immutable by
/// [`SymbolMap::set_func`](`crate::core::env::SymbolMap::set_func`) and they
/// can only be replaced atomically. In order to garbage collect the function we
/// need to halt all running threads. This has not been implemented yet.
pub(crate) struct SymbolCell(GcHeap<SymbolCellData>);

struct SymbolCellData {
    name: SymbolName,
    // We can't use AtomicCell due to this issue:
    // https://github.com/crossbeam-rs/crossbeam/issues/748
    func: Option<AtomicPtr<u8>>,
    special: AtomicBool,
}

#[derive(Debug)]
enum SymbolName {
    Interned(&'static str),
    Uninterned(Cell<&'static str>),
}

impl SymbolName {
    fn as_bytes(&self) -> &[u8] {
        match self {
            SymbolName::Interned(x) => x.as_bytes(),
            SymbolName::Uninterned(x) => x.get().as_bytes(),
        }
    }
}

unsafe impl Sync for SymbolName {}

#[derive(PartialEq, Eq, Copy, Clone)]
pub(crate) struct Symbol<'a> {
    // This is *NOT* a pointer but an offset from the start of the symbol table
    data: *const u8,
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
            let ptr = self.data.map_addr(|x| x.wrapping_add(base)).cast::<SymbolCell>();
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
        Self { data: ptr.cast::<u8>(), marker: PhantomData }
    }

    pub(in crate::core) const fn new_builtin(idx: usize) -> Self {
        let ptr = core::ptr::without_provenance(idx * size_of::<SymbolCell>());
        Self { data: ptr, marker: PhantomData }
    }

    pub(crate) fn make_special(self) {
        self.0.special.store(true, Ordering::Release);
    }

    pub(crate) fn is_special(self) -> bool {
        self.0.special.load(Ordering::Acquire)
    }
}

unsafe impl Send for Symbol<'_> {}

// implement withlifetime for symbol
impl<'new> WithLifetime<'new> for Symbol<'_> {
    type Out = Symbol<'new>;

    unsafe fn with_lifetime(self) -> Self::Out {
        unsafe { std::mem::transmute(self) }
    }
}

impl TracePtr for Symbol<'_> {
    fn trace_ptr(&self, state: &mut GcState) {
        self.get().trace(state);
    }
}

impl<'a> GcMoveable for Symbol<'a> {
    type Value = Symbol<'a>;

    fn move_value(&self, to_space: &bumpalo::Bump) -> Option<(Self::Value, bool)> {
        let val = self.get().0.move_value(to_space);
        val.map(|(ptr, moved)| {
            let symbol = unsafe {
                // SAFETY: They share the same representation
                let ptr = ptr.cast::<SymbolCell>();
                Self::from_ptr(ptr.as_ptr())
            };
            (symbol, moved)
        })
    }
}

impl Trace for SymbolCell {
    fn trace(&self, state: &mut GcState) {
        if let SymbolName::Uninterned(name) = &self.0.name {
            let new = state.to_space.alloc_str(name.get());
            let new = unsafe { std::mem::transmute::<&str, &'static str>(new) };
            name.set(new);
        }
        // The function cell of the symbol is always cloned in the global symbol
        // map
    }
}

impl fmt::Display for Symbol<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl fmt::Debug for Symbol<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl Hash for Symbol<'_> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.data.hash(state);
    }
}

impl<'new> Symbol<'_> {
    pub(in crate::core) fn clone_in<const C: bool>(
        self,
        bk: &'new crate::core::gc::Block<C>,
    ) -> Gc<Symbol<'new>> {
        if let SymbolName::Uninterned(name) = &self.0.name {
            match bk.uninterned_symbol_map.get(self) {
                Some(new) => new.tag(),
                None => {
                    let sym = Symbol::new_uninterned(name.get(), bk);
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
    #[expect(clippy::declare_interior_mutable_const)]
    const EMTPTY: AtomicPtr<u8> = AtomicPtr::new(Self::NULL);

    pub(crate) fn as_bytes(&self) -> &[u8] {
        self.0.name.as_bytes()
    }

    fn new_normal(name: &'static str, block: &Block<true>) -> Self {
        // We have to do this workaround because starts_with is not const
        if name.as_bytes()[0] == b':' {
            Self::new_const(name, block)
        } else {
            Self(GcHeap::new(
                SymbolCellData {
                    name: SymbolName::Interned(name),
                    func: Some(Self::EMTPTY),
                    special: AtomicBool::new(false),
                },
                true,
            ))
        }
    }

    pub(in crate::core) const fn new_static(name: &'static str) -> Self {
        // We have to do this workaround because starts_with is not const
        if name.as_bytes()[0] == b':' {
            Self::new_static_const(name)
        } else {
            Self(GcHeap::new_pure(SymbolCellData {
                name: SymbolName::Interned(name),
                func: Some(Self::EMTPTY),
                special: AtomicBool::new(false),
            }))
        }
    }

    pub(in crate::core) const fn new_static_special(name: &'static str) -> Self {
        Self(GcHeap::new_pure(SymbolCellData {
            name: SymbolName::Interned(name),
            func: Some(Self::EMTPTY),
            special: AtomicBool::new(true),
        }))
    }

    fn new_const(name: &'static str, _block: &Block<true>) -> Self {
        Self(GcHeap::new(
            SymbolCellData {
                name: SymbolName::Interned(name),
                func: None,
                special: AtomicBool::new(true),
            },
            true,
        ))
    }

    pub(in crate::core) const fn new_static_const(name: &'static str) -> Self {
        Self(GcHeap::new_pure(SymbolCellData {
            name: SymbolName::Interned(name),
            func: None,
            special: AtomicBool::new(true),
        }))
    }

    fn new_uninterned<const C: bool>(name: &str, bk: &Block<C>) -> Self {
        let mut owned_name = bk.string_with_capacity(name.len());
        owned_name.push_str(name);
        let name = unsafe { std::mem::transmute::<&str, &'static str>(owned_name.into_bump_str()) };
        Self(GcHeap::new(
            SymbolCellData {
                name: SymbolName::Uninterned(Cell::new(name)),
                func: Some(Self::EMTPTY),
                special: AtomicBool::new(false),
            },
            C,
        ))
    }

    pub(crate) fn name(&self) -> &str {
        match &self.0.name {
            SymbolName::Interned(x) => x,
            SymbolName::Uninterned(x) => x.get(),
        }
    }

    pub(crate) fn interned(&self) -> bool {
        matches!(self.0.name, SymbolName::Interned(_))
    }

    #[inline(always)]
    /// Check if the symbol is constant like nil, t, or :keyword
    pub(crate) fn is_const(&self) -> bool {
        self.0.func.is_none()
    }

    pub(crate) fn has_func(&self) -> bool {
        match &self.0.func {
            Some(func) => !func.load(Ordering::Acquire).is_null(),
            None => false,
        }
    }

    fn get(&self) -> Option<Function<'_>> {
        if let Some(func) = &self.0.func {
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
        let Some(fn_cell) = self.0.func.as_ref() else {
            bail!("Attempt to set a constant symbol: {self}")
        };
        let val = func.into_ptr().cast_mut();
        fn_cell.store(val, Ordering::Release);
        Ok(())
    }

    pub(crate) fn unbind_func(&self) {
        if let Some(func) = &self.0.func {
            func.store(Self::NULL, Ordering::Release);
        }
    }
}

impl fmt::Display for SymbolCell {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl fmt::Debug for SymbolCell {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

/// When copying uninterned symbols, we need to ensure that all instances share
/// the same address if they did originally. This keeps a mapping from old
/// symbols to new.
#[derive(Default)]
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
