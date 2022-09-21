use super::gc::{Block, Context, Rt};
use super::object::{Function, Gc, GcObj, SubrFn, WithLifetime};
use crate::hashmap::{HashMap, HashSet};
use anyhow::{anyhow, Result};
use fn_macros::Trace;
use lazy_static::lazy_static;
use sptr::Strict;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::sync::atomic::{AtomicBool, AtomicPtr, Ordering};
use std::sync::Mutex;

#[allow(dead_code)]
#[derive(Debug, Default, Trace)]
pub(crate) struct Env {
    pub(crate) vars: HashMap<Symbol<'static>, GcObj<'static>>,
    pub(crate) props: HashMap<Symbol<'static>, Vec<(Symbol<'static>, GcObj<'static>)>>,
    pub(crate) catch_stack: Vec<GcObj<'static>>,
    pub(crate) thrown: (GcObj<'static>, GcObj<'static>),
    pub(crate) special_variables: HashSet<Symbol<'static>>,
    pub(crate) binding_stack: Vec<(Symbol<'static>, Option<GcObj<'static>>)>,
    pub(crate) match_data: GcObj<'static>,
    local_obarray: Vec<Symbol<'static>>,
}

impl Rt<Env> {
    pub(crate) fn set_var(&mut self, sym: Symbol, value: GcObj) -> Result<()> {
        if sym.is_const() {
            Err(anyhow!("Attempt to set a constant symbol: {sym}"))
        } else {
            self.vars.insert(sym, value);
            Ok(())
        }
    }

    pub(crate) fn set_prop(&mut self, symbol: Symbol, propname: Symbol, value: GcObj) {
        match self.props.get_mut(symbol) {
            Some(plist) => match plist.iter_mut().find(|x| x.0 == propname) {
                Some(x) => x.1.set(value),
                None => plist.push((propname, value)),
            },
            None => {
                self.props.insert(symbol, vec![(propname, value)]);
            }
        }
    }
}

/// The allocation of a global symbol. This is shared
/// between threads, so the interned value of a symbol
/// will be the same location no matter which thread
/// interned it. Functions are safe to share between
/// threads because they are marked immutable by
/// [`ObjectMap::set_func`] and they can only be replaced atomically.
/// In order to garbage collect the function we need to
/// halt all running threads. This has not been implemented
/// yet.
#[derive(Debug)]
pub(crate) struct GlobalSymbol {
    name: SymbolName,
    pub(crate) sym: ConstSymbol,
    marked: AtomicBool,
    func: Option<AtomicPtr<u8>>,
}

/// A static reference to a [`GlobalSymbol`]. These
/// references are shared between threads because
/// `GlobalSymbol` is thread safe. This makes comparing
/// Symbols cheap since it is just a pointer comparison.
/// There are no uninterned symbols.
pub(crate) type Symbol<'a> = &'a GlobalSymbol;

#[derive(Debug)]
enum SymbolName {
    Interned(&'static str),
    Uninterned(Box<str>),
}

#[repr(transparent)]
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub(crate) struct ConstSymbol(fn() -> Symbol<'static>);

impl ConstSymbol {
    pub(crate) const fn new(f: fn() -> Symbol<'static>) -> Self {
        Self(f)
    }
}

impl std::ops::Deref for ConstSymbol {
    type Target = GlobalSymbol;

    fn deref(&self) -> &Self::Target {
        self.0()
    }
}

impl AsRef<GlobalSymbol> for ConstSymbol {
    fn as_ref(&self) -> &GlobalSymbol {
        self
    }
}

// Since global symbols are globally unique we can
// compare them with a pointer equal test.
impl PartialEq for GlobalSymbol {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl PartialEq<ConstSymbol> for GlobalSymbol {
    fn eq(&self, other: &ConstSymbol) -> bool {
        self == other.0()
    }
}

impl PartialEq<GlobalSymbol> for ConstSymbol {
    fn eq(&self, other: &GlobalSymbol) -> bool {
        self.0() == other
    }
}

impl Eq for GlobalSymbol {}

impl Hash for GlobalSymbol {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let ptr: *const Self = self;
        ptr.hash(state);
    }
}

impl GlobalSymbol {
    const NULL: *mut u8 = std::ptr::null_mut();
    pub(crate) const fn new(name: &'static str, sym: ConstSymbol) -> Self {
        // We have to do this workaround because starts_with is not const
        let func = if name.as_bytes()[0] == b':' {
            // If func is none then this cannot take a function
            None
        } else {
            Some(AtomicPtr::new(Self::NULL))
        };
        GlobalSymbol {
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
        GlobalSymbol {
            name: SymbolName::Uninterned(name.to_owned().into_boxed_str()),
            sym: sym::RUNTIME_SYMBOL,
            func: Some(AtomicPtr::new(Self::NULL)),
            marked: AtomicBool::new(false),
        }
    }

    pub(crate) const fn new_const(name: &'static str, sym: ConstSymbol) -> Self {
        GlobalSymbol {
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
        GlobalSymbol {
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
    unsafe fn tag_subr(&self) {
        if let Some(func) = &self.func {
            let ptr = func.load(Ordering::Acquire);
            if Strict::addr(ptr) != 0 {
                let func = SubrFn::gc_from_raw_ptr(ptr.cast::<SubrFn>());
                self.set_func(func.into());
            }
        }
    }

    pub(crate) fn has_func(&self) -> bool {
        match &self.func {
            Some(func) => Strict::addr(func.load(Ordering::Acquire)) != 0,
            None => false,
        }
    }

    fn get(&self) -> Option<Gc<Function>> {
        if let Some(func) = &self.func {
            let ptr = func.load(Ordering::Acquire);
            if Strict::addr(ptr) != 0 {
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
    unsafe fn set_func(&self, func: Gc<Function>) {
        let val = std::mem::transmute(func);
        self.func.as_ref().unwrap().store(val, Ordering::Release);
    }

    pub(crate) fn unbind_func(&self) {
        if let Some(func) = &self.func {
            func.store(Self::NULL, Ordering::Release);
        }
    }

    pub(crate) fn unmark(&self) {
        self.marked.store(false, Ordering::Release);
    }

    pub(crate) fn is_marked(&self) -> bool {
        self.marked.load(Ordering::Acquire)
    }
}

impl super::gc::Trace for Symbol<'_> {
    fn mark(&self, stack: &mut Vec<super::object::RawObj>) {
        // interned symbols are not collected yet
        if matches!(self.name, SymbolName::Uninterned(_)) {
            self.marked.store(true, Ordering::Release);
            if let Some(func) = self.get() {
                func.as_obj().trace_mark(stack);
            }
        }
    }
}

impl fmt::Display for GlobalSymbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

pub(crate) struct ObjectMap {
    map: SymbolMap,
    block: Block<true>,
}

/// Box is marked as unique. However we are freely sharing the pointer to this
/// Symbol amoung threads. So instead of Box we need to use a custom wrapper
/// type for this to be sound.
struct SymbolBox(*const GlobalSymbol);
unsafe impl Send for SymbolBox {}

impl SymbolBox {
    fn new(inner: GlobalSymbol) -> Self {
        let ptr = Box::into_raw(Box::new(inner));
        Self(ptr)
    }

    fn from_static(inner: &'static GlobalSymbol) -> Self {
        Self(inner)
    }
}

impl AsRef<GlobalSymbol> for SymbolBox {
    fn as_ref(&self) -> &GlobalSymbol {
        unsafe { &*self.0 }
    }
}

// `SymbolBox` should not be dropped until we
// have a garbage collector
impl Drop for SymbolBox {
    fn drop(&mut self) {
        eprintln!("Error: Tried to drop Symbol: {:?}", unsafe { &*self.0 });
    }
}

struct SymbolMap {
    map: HashMap<&'static str, SymbolBox>,
}

impl SymbolMap {
    fn with_capacity(cap: usize) -> Self {
        Self {
            map: HashMap::with_capacity_and_hasher(cap, std::hash::BuildHasherDefault::default()),
        }
    }

    fn get(&self, name: &str) -> Option<Symbol> {
        unsafe {
            self.map.get(name).map(|x| {
                let ptr: *const GlobalSymbol = x.as_ref();
                &*ptr
            })
        }
    }

    fn intern<'ob>(&mut self, name: &str, _cx: &'ob Context) -> Symbol<'ob> {
        let sym = match self.map.get(name) {
            Some(x) => x.0,
            None => {
                let name = name.to_owned();
                // Leak the memory so that it is static
                let static_name: &'static str = unsafe {
                    let name_ptr: *const str = Box::into_raw(name.into_boxed_str());
                    &*name_ptr
                };
                let inner = GlobalSymbol::new(static_name, sym::RUNTIME_SYMBOL);
                let sym = SymbolBox::new(inner);
                let ptr: *const GlobalSymbol = sym.as_ref();
                self.map.insert(static_name, sym);
                ptr
            }
        };
        // SAFETY: We can guarantee that the reference is static because we have
        // no methods to remove items from SymbolMap and SymbolMap has a private
        // constructor, so the only one that exists is the one we create in this
        // module, which is static.
        unsafe { &*sym }
    }

    fn pre_init(&mut self, sym: &'static GlobalSymbol) {
        match self.map.get(sym.name()) {
            Some(_) => panic!("Attempt to intitalize {} twice", sym.name()),
            None => {
                self.map.insert(sym.name(), SymbolBox::from_static(sym));
            }
        }
    }
}

impl ObjectMap {
    pub(crate) fn intern<'ob>(&mut self, name: &str, cx: &'ob Context) -> Symbol<'ob> {
        self.map.intern(name, cx)
    }

    pub(crate) fn set_func(&self, symbol: &GlobalSymbol, func: Gc<Function>) -> Result<()> {
        // TODO: Remove once bytecode is implemented. Right now we are ignoring
        // bytecode functions since they can't execute
        if matches!(func.get(), Function::LispFn(_)) {
            return Ok(());
        }
        match symbol.func {
            Some(_) => {
                let new_func = func.clone_in(&self.block);
                #[cfg(miri)]
                new_func.get().set_as_miri_root();
                // SAFETY: The object is marked read-only, we have cloned in the
                // map's context, and it is not const, so calling this function
                // is safe.
                unsafe {
                    symbol.set_func(new_func);
                }
                Ok(())
            }
            None => Err(anyhow!("Attempt to set a constant symbol: {symbol}")),
        }
    }

    pub(crate) fn get(&self, name: &str) -> Option<Symbol> {
        self.map.get(name)
    }
}

macro_rules! create_symbolmap {
    ($($($mod:ident)::*),*) => (
        #[allow(unused_imports)]
        #[allow(non_snake_case)]
        pub(crate) mod sym {
            use super::{Symbol, GlobalSymbol, ConstSymbol};
            // This GlobalSymbol is assinged to every runtime symbol created.
            static __RUNTIME_SYMBOL_GLOBAL: GlobalSymbol = GlobalSymbol::new("_dummy_runtime_symbol", RUNTIME_SYMBOL);
            fn __RUNTIME_SYMBOL_FN () -> Symbol<'static> {&__RUNTIME_SYMBOL_GLOBAL}
            pub(crate) const RUNTIME_SYMBOL: ConstSymbol = ConstSymbol::new(__RUNTIME_SYMBOL_FN);

            // Re-export all symbols in this module
            $(pub(crate) use $($mod::)*__symbol_bindings::*;)*
        }

        #[allow(unused_qualifications)]
        pub(crate) fn init_variables<'ob>(
            cx: &'ob crate::core::gc::Context,
            env: &mut crate::core::gc::Rt<crate::core::env::Env>,
        ) {
            $($($mod::)*__init_vars(cx, env);)*
        }

        lazy_static! {
            pub(crate) static ref INTERNED_SYMBOLS: Mutex<ObjectMap> = Mutex::new({
                let size: usize = 0_usize $(+ $($mod::)*__SYMBOLS.len())*;
                let mut map = SymbolMap::with_capacity(size);
                $(for sym in $($mod::)*__SYMBOLS.iter() {
                    // SAFETY: We know that the function values are un-tagged,
                    // and that only subr's have been defined in the symbols, so
                    // it is safe to tag all function cell's as subr's
                    unsafe { sym.tag_subr(); }
                    map.pre_init(sym);
                })*;
                ObjectMap {
                    map,
                    block: Block::new_global(),
                }
            });
        }
    )
}

create_symbolmap!(
    crate::arith,
    crate::interpreter,
    crate::core::cons,
    crate::lread,
    crate::fileio,
    crate::data,
    crate::fns,
    crate::search,
    crate::eval,
    crate::alloc,
    crate::editfns,
    crate::character,
    crate::floatfns,
    crate::keymap,
    crate::emacs,
    crate::buffer
);

/// Intern a new symbol based on `name`
pub(crate) fn intern<'ob>(name: &str, cx: &'ob Context) -> Symbol<'ob> {
    INTERNED_SYMBOLS.lock().unwrap().intern(name, cx)
}

#[cfg(test)]
mod test {
    use super::*;

    use super::super::gc::{Context, RootSet};
    use std::mem::size_of;

    #[test]
    fn size() {
        assert_eq!(size_of::<isize>(), size_of::<Symbol>());
        assert_eq!(size_of::<isize>(), size_of::<Gc<Function>>());
    }

    unsafe fn fix_lifetime(inner: &GlobalSymbol) -> &'static GlobalSymbol {
        std::mem::transmute::<&GlobalSymbol, &'static GlobalSymbol>(inner)
    }

    #[test]
    fn init() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        intern("foo", cx);
    }

    #[test]
    fn symbol_func() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        let inner = GlobalSymbol::new("foo", super::sym::RUNTIME_SYMBOL);
        let sym = unsafe { fix_lifetime(&inner) };
        assert_eq!("foo", sym.name());
        assert!(sym.func(cx).is_none());
        let func1 = cons!(1; cx);
        unsafe {
            sym.set_func(func1.try_into().unwrap());
        }
        let cell1 = sym.func(cx).unwrap();
        let before = match cell1.get() {
            Function::Cons(x) => x,
            _ => unreachable!("Type should be a lisp function"),
        };
        assert_eq!(before.car(), 1);
        let func2 = cons!(2; cx);
        unsafe {
            sym.set_func(func2.try_into().unwrap());
        }
        let cell2 = sym.func(cx).unwrap();
        let after = match cell2.get() {
            Function::Cons(x) => x,
            _ => unreachable!("Type should be a lisp function"),
        };
        assert_eq!(after.car(), 2);
        assert_eq!(before.car(), 1);
    }

    #[test]
    fn test_mutability() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        let cons = list!(1, 2, 3; cx);
        assert_eq!(cons, list!(1, 2, 3; cx));
        // is mutable
        if let crate::core::object::Object::Cons(cons) = cons.get() {
            cons.set_car(4.into()).unwrap();
        } else {
            unreachable!();
        }
        assert_eq!(cons, list!(4, 2, 3; cx));
        let sym = intern("cons-test", cx);
        crate::data::fset(sym, cons).unwrap();
        // is not mutable
        if let Function::Cons(cons) = sym.func(cx).unwrap().get() {
            assert!(cons.set_car(5.into()).is_err());
            let obj: GcObj = cons.into();
            assert_eq!(obj, list!(4, 2, 3; cx));
        } else {
            unreachable!();
        }
    }
}
