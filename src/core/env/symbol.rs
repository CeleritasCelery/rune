use crate::core::gc::GcManaged;
use crate::core::object::IntoObject;
use crate::core::{gc::Context, object::RawObj};
use anyhow::{bail, Result};
use std::sync::atomic::{AtomicBool, AtomicPtr, Ordering};

use super::super::gc::Trace;
use super::super::object::{Function, Gc, SubrFn, WithLifetime};
use super::sym;
use std::fmt;
use std::hash::{Hash, Hasher};

/// The allocation of a global symbol. This is shared
/// between threads, so the interned value of a symbol
/// will be the same location no matter which thread
/// interned it. Functions are safe to share between
/// threads because they are marked immutable by
/// [`ObjectMap::set_func`] and they can only be replaced atomically.
/// In order to garbage collect the function we need to
/// halt all running threads. This has not been implemented
/// yet.
pub(crate) struct Symbol {
    name: SymbolName,
    pub(crate) sym: ConstSymbol,
    marked: AtomicBool,
    func: Option<AtomicPtr<u8>>,
}

#[derive(Debug)]
enum SymbolName {
    Interned(&'static str),
    Uninterned(Box<str>),
}

#[repr(transparent)]
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub(crate) struct ConstSymbol(fn() -> &'static Symbol);

impl ConstSymbol {
    pub(crate) const fn new(f: fn() -> &'static Symbol) -> Self {
        Self(f)
    }
}

impl std::ops::Deref for ConstSymbol {
    type Target = Symbol;

    fn deref(&self) -> &Self::Target {
        self.0()
    }
}

impl AsRef<Symbol> for ConstSymbol {
    fn as_ref(&self) -> &Symbol {
        self
    }
}

// Since global symbols are globally unique we can
// compare them with a pointer equal test.
impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl PartialEq<ConstSymbol> for Symbol {
    fn eq(&self, other: &ConstSymbol) -> bool {
        self == other.0()
    }
}

impl PartialEq<Symbol> for ConstSymbol {
    fn eq(&self, other: &Symbol) -> bool {
        self.0() == other
    }
}

impl Eq for Symbol {}

impl Hash for Symbol {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let ptr: *const Self = self;
        ptr.hash(state);
    }
}

impl Symbol {
    const NULL: *mut u8 = std::ptr::null_mut();
    pub(crate) const fn new(name: &'static str, sym: ConstSymbol) -> Self {
        // We have to do this workaround because starts_with is not const
        let func = if name.as_bytes()[0] == b':' {
            // If func is none then this cannot take a function
            None
        } else {
            Some(AtomicPtr::new(Self::NULL))
        };
        Symbol {
            name: SymbolName::Interned(name),
            sym,
            func,
            marked: AtomicBool::new(true),
        }
    }

    pub(crate) fn name(&self) -> &str {
        match &self.name {
            SymbolName::Interned(x) => x,
            SymbolName::Uninterned(x) => x,
        }
    }

    pub(crate) fn new_uninterned(name: &str) -> Self {
        Symbol {
            name: SymbolName::Uninterned(name.to_owned().into_boxed_str()),
            sym: sym::RUNTIME_SYMBOL,
            func: Some(AtomicPtr::new(Self::NULL)),
            marked: AtomicBool::new(false),
        }
    }

    pub(crate) const fn new_const(name: &'static str, sym: ConstSymbol) -> Self {
        Symbol {
            name: SymbolName::Interned(name),
            func: None,
            sym,
            marked: AtomicBool::new(true),
        }
    }

    pub(crate) const unsafe fn new_with_subr(
        name: &'static str,
        subr: &'static SubrFn,
        func: fn() -> &'static Self,
    ) -> Self {
        Symbol {
            name: SymbolName::Interned(name),
            func: Some(AtomicPtr::new(
                (subr as *const SubrFn).cast::<u8>() as *mut u8
            )),
            sym: ConstSymbol::new(func),
            marked: AtomicBool::new(true),
        }
    }

    #[inline(always)]
    pub(crate) const fn is_const(&self) -> bool {
        self.func.is_none()
    }

    #[inline(always)]
    pub(crate) fn nil(&self) -> bool {
        self == &sym::NIL
    }

    // This is workaround due to the limitations of const functions in rust. We
    // need to bind the function to the symbol when it is declared with
    // #[defun], but tagging the value is not const, so we can't initialize it
    // with a tagged value. Instead we put the untagged value in the function
    // cell and then use this function to ensure it get's tagged before first
    // use.
    pub(super) unsafe fn tag_subr(&self) {
        if let Some(func) = &self.func {
            let ptr = func.load(Ordering::Acquire);
            if !ptr.is_null() {
                let func = SubrFn::gc_from_raw_ptr(ptr.cast::<SubrFn>());
                self.set_func(func.into()).unwrap();
            }
        }
    }

    pub(crate) fn has_func(&self) -> bool {
        match &self.func {
            Some(func) => !func.load(Ordering::Acquire).is_null(),
            None => false,
        }
    }

    fn get(&self) -> Option<Gc<Function>> {
        #[cfg(test)]
        assert!(
            super::SYMBOLS_INIT.load(Ordering::Acquire),
            "Symbols were not initalized"
        );
        if let Some(func) = &self.func {
            let ptr = func.load(Ordering::Acquire);
            if !ptr.is_null() {
                // SAFETY: we ensure that 0 is not representable in the enum
                // Function (by making a reference the first element, which will
                // never be null). So it is safe to use 0 as niche value for
                // `None`. We can't use AtomicCell due to this issue:
                // https://github.com/crossbeam-rs/crossbeam/issues/748
                return Some(unsafe { Gc::from_raw_ptr(ptr) });
            }
        }
        None
    }

    pub(crate) fn func<'a>(&self, _cx: &'a Context) -> Option<Gc<Function<'a>>> {
        self.get().map(|x| unsafe { x.with_lifetime() })
    }

    /// Follow the chain of symbols to find the function at the end, if any.
    pub(crate) fn follow_indirect<'ob>(&self, cx: &'ob Context) -> Option<Gc<Function<'ob>>> {
        let func = self.func(cx)?;
        match func.get() {
            Function::Symbol(sym) => sym.follow_indirect(cx),
            _ => Some(func),
        }
    }

    /// Set the function for this symbol. This function is unsafe to call and
    /// requires that the caller:
    /// 1. Has marked the entire function as read only
    /// 2. Has cloned the function into the `SymbolMap` block
    /// 3. Ensured the symbol is not constant
    pub(super) unsafe fn set_func(&self, func: Gc<Function>) -> Result<()> {
        let Some(fn_cell) = self.func.as_ref() else {bail!("Attempt to set a constant symbol: {self}")};
        let val = func.into_ptr().cast_mut();
        fn_cell.store(val, Ordering::Release);
        Ok(())
    }

    pub(crate) fn unbind_func(&self) {
        if let Some(func) = &self.func {
            func.store(Self::NULL, Ordering::Release);
        }
    }

    pub(in crate::core) fn clone_in<'new, const C: bool>(
        &self,
        bk: &'new crate::core::gc::Block<C>,
    ) -> &'new Self {
        if let SymbolName::Uninterned(name) = &self.name {
            let sym = Self::new_uninterned(name);
            if let Some(old_func) = self.get() {
                let new_func = old_func.clone_in(bk);
                unsafe {
                    sym.set_func(new_func).unwrap();
                }
            }
            return sym.into_obj(bk).get();
        }
        unsafe { self.with_lifetime() }
    }
}

impl GcManaged for Symbol {
    fn get_mark(&self) -> &crate::core::gc::GcMark {
        panic!("Symbol does not use GcMark")
    }

    fn mark(&self) {
        if matches!(self.name, SymbolName::Uninterned(_)) {
            self.marked.store(true, Ordering::Release);
        }
    }

    fn unmark(&self) {
        if matches!(self.name, SymbolName::Uninterned(_)) {
            self.marked.store(false, Ordering::Release);
        }
    }

    fn is_marked(&self) -> bool {
        match self.name {
            SymbolName::Uninterned(_) => self.marked.load(Ordering::Acquire),
            SymbolName::Interned(_) => true,
        }
    }
}

impl Trace for Symbol {
    fn trace(&self, stack: &mut Vec<RawObj>) {
        // interned symbols are not collected yet
        if matches!(self.name, SymbolName::Uninterned(_)) {
            self.mark();
            if let Some(func) = self.get() {
                func.as_obj().trace_mark(stack);
            }
        }
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}
