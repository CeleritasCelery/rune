use super::arena::{Arena, Block, Rt, Trace};
use super::object::{Function, Gc, GcObj, RawObj, SubrFn, WithLifetime};
use crate::hashmap::HashMap;
use lazy_static::lazy_static;
use sptr::Strict;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::sync::atomic::{AtomicPtr, Ordering};
use std::sync::Mutex;

#[allow(dead_code)]
#[derive(Debug, Default)]
pub(crate) struct Environment {
    pub(crate) vars: HashMap<Symbol, GcObj<'static>>,
    pub(crate) props: HashMap<Symbol, Vec<(Symbol, GcObj<'static>)>>,
    pub(crate) catch_stack: Vec<GcObj<'static>>,
    pub(crate) thrown: (GcObj<'static>, GcObj<'static>),
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
        for x in &self.catch_stack {
            x.mark(stack);
        }
        self.thrown.0.mark(stack);
        self.thrown.1.mark(stack);
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
            // We can't use AtomicCell due to this issue:
            // https://github.com/crossbeam-rs/crossbeam/issues/748
            _ => Some(unsafe { Gc::from_raw_ptr(ptr) }),
        }
    }

    pub(crate) fn func<'a>(&self, _gc: &'a Arena) -> Option<Gc<Function<'a>>> {
        self.get().map(|x| unsafe { x.with_lifetime() })
    }

    /// Follow the chain of symbols to find the function at the end, if any.
    pub(crate) fn follow_indirect<'ob>(&self, gc: &'ob Arena) -> Option<Gc<Function<'ob>>> {
        let func = self.func(gc)?;
        match func.get() {
            Function::Symbol(sym) => sym.follow_indirect(gc),
            _ => Some(func),
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

    fn intern(&mut self, name: &str) -> Symbol {
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
        match self.map.get(sym.name) {
            Some(_) => panic!("Attempt to intitalize {} twice", sym.name),
            None => {
                self.map.insert(sym.name, SymbolBox::from_static(sym));
            }
        }
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
    ($($($subr:ident)::*),*) => (
        #[allow(unused_imports)]
        #[allow(non_snake_case)]
        pub(crate) mod sym {
            static __RUNTIME_SYMBOL_GLOBAL: GlobalSymbol = GlobalSymbol::new("_dummy_runtime_symbol", RUNTIME_SYMBOL);
            fn __RUNTIME_SYMBOL_FN () -> Symbol {&__RUNTIME_SYMBOL_GLOBAL}
            pub(crate) const RUNTIME_SYMBOL: ConstSymbol = ConstSymbol::new(__RUNTIME_SYMBOL_FN);

            $(pub(crate) use $($subr::)*__symbol_bindings::*;)*

                use super::{Symbol, GlobalSymbol, ConstSymbol};
        }

        #[allow(unused_qualifications)]
        pub(crate) fn init_variables<'ob>(
            arena: &'ob crate::core::arena::Arena,
            env: &mut crate::core::arena::Rt<crate::core::env::Environment>
        ) {
            $($($subr::)*__init_vars(arena, env);)*
        }

        lazy_static! {
            pub(crate) static ref INTERNED_SYMBOLS: Mutex<ObjectMap> = Mutex::new({
                let size: usize = 0_usize $(+ $($subr::)*DEFSUBR.len())*;
                let mut map = SymbolMap::with_capacity(size);
                $(for sym in $($subr::)*DEFSUBR.iter() {
                    // SAFETY: built-in subroutine are globally immutable, and
                    // so they are safe to share between threads.
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
    crate::keymap,
    crate::emacs,
    crate::buffer
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
        std::mem::transmute::<&GlobalSymbol, &'static GlobalSymbol>(inner)
    }

    #[test]
    fn init() {
        intern("foo");
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
        assert_eq!(before.body.op_codes.first().unwrap(), &1);
        let func2 = LispFn::new(vec![2].into(), vec![], 0, 0, false);
        unsafe {
            sym.set_func(func2.into_obj(gc).into());
        }
        let cell2 = sym.func(gc).unwrap();
        let after = match cell2.get() {
            Function::LispFn(x) => x,
            _ => unreachable!("Type should be a lisp function"),
        };
        assert_eq!(after.body.op_codes.first().unwrap(), &2);
        assert_eq!(before.body.op_codes.first().unwrap(), &1);
    }

    #[test]
    fn test_mutability() {
        let roots = &RootSet::default();
        let arena = &Arena::new(roots);
        let cons = list!(1, 2, 3; arena);
        assert_eq!(cons, list!(1, 2, 3; arena));
        // is mutable
        if let crate::core::object::Object::Cons(cons) = cons.get() {
            cons.set_car(4.into()).unwrap();
        } else {
            unreachable!();
        }
        assert_eq!(cons, list!(4, 2, 3; arena));
        let sym = intern("cons-test");
        crate::data::fset(sym, cons).unwrap();
        // is not mutable
        if let Function::Cons(cons) = sym.func(arena).unwrap().get() {
            assert!(cons.set_car(5.into()).is_err());
            let obj: GcObj = cons.into();
            assert_eq!(obj, list!(4, 2, 3; arena));
        } else {
            unreachable!();
        }
    }
}
