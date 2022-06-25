use super::arena::{Arena, Block, Rt, Trace};
use super::object::{Callable, Function, Gc, GcObj, RawObj, SubrFn};
use crate::hashmap::HashMap;
use lazy_static::lazy_static;
use sptr::Strict;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::sync::atomic::{AtomicPtr, Ordering};
use std::sync::Mutex;

#[derive(Debug, Default)]
pub(crate) struct Environment {
    pub(crate) vars: HashMap<Symbol, GcObj<'static>>,
    pub(crate) props: HashMap<Symbol, Vec<(Symbol, GcObj<'static>)>>,
}

impl Rt<Environment> {
    pub(crate) fn set_var(&mut self, sym: Symbol, value: GcObj) {
        self.vars.insert(sym, value);
    }

    pub(crate) fn set_prop(&mut self, symbol: Symbol, propname: Symbol, value: GcObj) {
        match self.props.get_mut(&symbol) {
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

impl Trace for Environment {
    fn mark(&self, stack: &mut Vec<RawObj>) {
        for x in self.vars.values() {
            x.mark(stack);
        }
        for vec in self.props.values() {
            for x in vec {
                x.1.mark(stack);
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
    pub(crate) name: &'static str,
    pub(crate) sym: ConstSymbol,
    func: AtomicPtr<u8>,
}

/// A static reference to a [`GlobalSymbol`]. These
/// references are shared between threads because
/// `GlobalSymbol` is thread safe. This makes comparing
/// Symbols cheap since it is just a pointer comparison.
/// There are no uninterned symbols.
pub(crate) type Symbol = &'static GlobalSymbol;

#[repr(transparent)]
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub(crate) struct ConstSymbol(fn() -> Symbol);

impl ConstSymbol {
    pub(crate) const fn new(f: fn() -> Symbol) -> Self {
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
        &*self
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
        GlobalSymbol {
            name,
            func: AtomicPtr::new(Self::NULL),
            sym,
        }
    }

    pub(crate) const unsafe fn new_with_subr(
        name: &'static str,
        subr: &'static SubrFn,
        func: fn() -> &'static Self,
    ) -> Self {
        GlobalSymbol {
            name,
            func: AtomicPtr::new((subr as *const SubrFn).cast::<u8>() as *mut u8),
            sym: ConstSymbol::new(func),
        }
    }

    unsafe fn tag_subr(&self) {
        let ptr = self.func.load(Ordering::Acquire);
        if Strict::addr(ptr) != 0 {
            let func = SubrFn::gc_from_raw_ptr(ptr.cast::<SubrFn>());
            self.set_func(func.into());
        }
    }

    pub(crate) fn has_func(&self) -> bool {
        Strict::addr(self.func.load(Ordering::Acquire)) != 0
    }

    fn get(&self) -> Option<Gc<Function>> {
        let ptr = self.func.load(Ordering::Acquire);
        match Strict::addr(ptr) {
            0 => None,
            // SAFETY: we ensure that 0 is not representable in the enum
            // Function (by making a reference the first element, which will
            // never be null). So it is safe to use 0 as niche value for `None`.
            // We can't use AtomicCell due to this issue
            // https://github.com/crossbeam-rs/crossbeam/issues/748 .
            _ => Some(unsafe { Gc::from_raw_ptr(ptr) }),
        }
    }

    pub(crate) fn func<'a>(&self, gc: &'a Arena) -> Option<Gc<Function<'a>>> {
        self.get().map(|x| x.clone_in(gc))
    }

    /// Follow the chain of symbols to find the function at the end, if any.
    pub(crate) fn resolve_callable<'ob>(&self, gc: &'ob Arena) -> Option<Gc<Callable<'ob>>> {
        let func = self.func(gc)?;
        match func.get() {
            Function::Symbol(sym) => sym.resolve_callable(gc),
            // If it is not a symbol this conversion is infallible
            _ => Some(func.try_into().unwrap()),
        }
    }

    /// Set the function for this symbol. This function is unsafe to call and
    /// requires that the caller:
    /// 1. Has marked the entire function as read only
    /// 2. Has cloned the function into the `SymbolMap` block
    unsafe fn set_func(&self, func: Gc<Function>) {
        let val = std::mem::transmute(func);
        self.func.store(val, Ordering::Release);
    }

    pub(crate) fn unbind_func(&self) {
        self.func.store(Self::NULL, Ordering::Release);
    }
}

impl fmt::Display for GlobalSymbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = &self.name;
        write!(f, "{name}")
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
        panic!("Tried to drop Symbol: {:?}", unsafe { &*self.0 });
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

    fn intern(&mut self, name: &str) -> Symbol {
        // SAFETY: We can guarantee that the reference is static because we have
        // no methods to remove items from SymbolMap and SymbolMap has a private
        // constructor, so the only one that exists is the one we create in this
        // module, which is static.
        unsafe { &*self.get_symbol(name) }
    }

    // This is my work around for there being no Entry API that takes a
    // reference.
    // https://internals.rust-lang.org/t/pre-rfc-abandonning-morals-in-the-name-of-performance-the-raw-entry-api/7043
    fn get_symbol(&mut self, name: &str) -> *const GlobalSymbol {
        match self.map.get(name) {
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
        }
    }

    fn pre_init(&mut self, sym: &'static GlobalSymbol) {
        self.map.insert(sym.name, SymbolBox::from_static(sym));
    }
}

impl ObjectMap {
    pub(crate) fn intern(&mut self, name: &str) -> Symbol {
        self.map.intern(name)
    }

    #[allow(clippy::unused_self)]
    pub(crate) fn set_func(&self, symbol: &GlobalSymbol, func: Gc<Function>) {
        let new_func = func.clone_in(&self.block);
        #[cfg(miri)]
        new_func.get().set_as_miri_root();
        // SAFETY: The object is marked read-only and we have cloned
        // in the map's arena, so calling this function is safe.
        unsafe {
            symbol.set_func(new_func);
        }
    }
}

macro_rules! create_symbolmap {
    (SUBR => {$($($subr:ident)::*),*}
     SYMBOLS => {$($sym:ident => $name:expr),* $(,)?}
     TEST_SYMBOLS => {$($test_sym:ident => $test_name:expr),* $(,)?}
    ) => (
        #[allow(unused_imports)]
        #[allow(non_snake_case)]
        pub(crate) mod sym {
            static __RUNTIME_SYMBOL_GLOBAL: GlobalSymbol = GlobalSymbol::new("_dummy_runtime_symbol", RUNTIME_SYMBOL);
            fn __RUNTIME_SYMBOL_FN () -> Symbol {&__RUNTIME_SYMBOL_GLOBAL}
            pub(crate) const RUNTIME_SYMBOL: ConstSymbol = ConstSymbol::new(__RUNTIME_SYMBOL_FN);

            $(pub(crate) use $($subr::)*__symbol_bindings::*;)*

                use super::{Symbol, GlobalSymbol, ConstSymbol};
            $(paste::paste!{
                static [<G $sym>]: GlobalSymbol = GlobalSymbol::new($name, $sym);
                fn [<I $sym>] () -> Symbol {&[<G $sym>]}
                pub(crate) const $sym: ConstSymbol = ConstSymbol::new([<I $sym>]);
            })*

                #[cfg(test)]
            pub(crate) mod test {
                use super::GlobalSymbol;
                $(paste::paste!{
                    static [<G $test_sym>]: GlobalSymbol = GlobalSymbol::new($test_name, $test_sym);
                    fn [<I $test_sym>] () -> super::Symbol {&[<G $test_sym>]}
                    pub(crate) const $test_sym: super::ConstSymbol = super::ConstSymbol::new([<I $test_sym>]);
                })*
            }
        }

        lazy_static! {
            pub(crate) static ref INTERNED_SYMBOLS: Mutex<ObjectMap> = Mutex::new({
                let size: usize = count!($($sym)*) $(+ $($subr::)*DEFSUBR.len())*;
                let mut map = SymbolMap::with_capacity(size);
                $(for sym in $($subr::)*DEFSUBR.iter() {
                    // SAFETY: built-in subroutine are globally immutable, and
                    // so they are safe to share between threads.
                    unsafe { sym.tag_subr(); }
                    map.pre_init(sym);
                })*;
                $(map.pre_init(&sym::$sym);)*
                    #[cfg(test)]
                {
                    $(map.pre_init(&sym::test::$test_sym);)*
                }
                ObjectMap {
                    map,
                    block: Block::new_global(),
                }
            });
        }
    )
}

create_symbolmap!(
    SUBR => {
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
        crate::keymap,
        crate::buffer
    }
    SYMBOLS => {
        FUNCTION => "function",
        QUOTE => "quote",
        MACRO => "macro",
        UNQUOTE => ",",
        SPLICE => ",@",
        BACKQUOTE => "`",
        NIL => "nil",
        TRUE => "t",
        AND_OPTIONAL => "&optional",
        AND_REST => "&rest",
        LAMBDA => "lambda",
        CLOSURE => "closure",
        WHILE => "while",
        PROGN => "progn",
        PROG1 => "prog1",
        PROG2 => "prog2",
        SETQ => "setq",
        DEFCONST => "defconst",
        COND => "cond",
        LET => "let",
        LET_STAR => "let*",
        IF => "if",
        AND => "and",
        OR => "or",
        LEXICAL_BINDING => "lexical-binding",
        EMACS_VERSION => "emacs-version",
        SYSTEM_TYPE => "system-type",
        MINIBUFFER_LOCAL_MAP => "minibuffer-local-map",
        CURRENT_LOAD_LIST => "current-load-list",
        LOAD_PATH => "load-path",
        DUMP_MODE => "dump-mode",
        COMMAND_LINE_ARGS => "command-line-args",
        DEFAULT_DIRECTORY => "default-directory",
        LOAD_HISTORY => "load-history",
        KW_TEST => ":test",
        KW_WEAKNESS => ":weakness",
    }
    TEST_SYMBOLS => {
        FOO => "foo",
        BAR => "bar",
        BAZ => "baz",
    }
);

/// Intern a new symbol based on `name`
pub(crate) fn intern(name: &str) -> Symbol {
    INTERNED_SYMBOLS.lock().unwrap().intern(name)
}

#[cfg(test)]
mod test {
    use super::*;

    use super::super::arena::{Arena, RootSet};
    use super::super::object::{IntoObject, LispFn};
    use std::mem::size_of;

    #[test]
    fn size() {
        assert_eq!(size_of::<isize>(), size_of::<Symbol>());
        assert_eq!(size_of::<isize>(), size_of::<Gc<Function>>());
        assert_eq!(size_of::<isize>() * 4, size_of::<GlobalSymbol>());
    }

    unsafe fn fix_lifetime(inner: &GlobalSymbol) -> &'static GlobalSymbol {
        std::mem::transmute::<&'_ GlobalSymbol, &'static GlobalSymbol>(inner)
    }

    #[test]
    fn symbol_func() {
        let roots = &RootSet::default();
        let gc = &Arena::new(roots);
        let inner = GlobalSymbol::new("foo", super::sym::RUNTIME_SYMBOL);
        let sym = unsafe { fix_lifetime(&inner) };
        assert_eq!("foo", sym.name);
        assert!(sym.func(gc).is_none());
        let func1 = LispFn::new(vec![1].into(), vec![], 0, 0, false);
        unsafe {
            sym.set_func(func1.into_obj(gc).into());
        }
        let cell1 = sym.func(gc).unwrap();
        let before = match cell1.get() {
            Function::LispFn(x) => x,
            _ => unreachable!("Type should be a lisp function"),
        };
        assert_eq!(before.body.op_codes.get(0).unwrap(), &1);
        let func2 = LispFn::new(vec![2].into(), vec![], 0, 0, false);
        unsafe {
            sym.set_func(func2.into_obj(gc).into());
        }
        let cell2 = sym.func(gc).unwrap();
        let after = match cell2.get() {
            Function::LispFn(x) => x,
            _ => unreachable!("Type should be a lisp function"),
        };
        assert_eq!(after.body.op_codes.get(0).unwrap(), &2);
        assert_eq!(before.body.op_codes.get(0).unwrap(), &1);
    }
}
