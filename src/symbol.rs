use crate::arena::Arena;
use crate::hashmap::HashMap;
use crate::object::{Callable, FuncCell, Object};
use crossbeam_utils::atomic::AtomicCell;
use lazy_static::lazy_static;
use std::convert::TryInto;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::mem::transmute;
use std::sync::Mutex;

#[derive(Debug)]
pub(crate) struct GlobalSymbol {
    pub(crate) name: &'static str,
    func: AtomicCell<Option<FuncCell<'static>>>,
}

pub(crate) type Symbol = &'static GlobalSymbol;

unsafe fn coerce_callable_lifetime<'a, 'b>(x: FuncCell<'a>) -> FuncCell<'b> {
    transmute(x)
}

impl PartialEq for GlobalSymbol {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(&*self, &*other)
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
    pub(crate) const fn new(name: &'static str) -> Self {
        GlobalSymbol {
            name,
            func: AtomicCell::new(None),
        }
    }

    pub(crate) fn has_func(&self) -> bool {
        self.func.load().is_some()
    }

    // TODO: unbounded lifetime is unsound
    pub(crate) fn func<'a>(&self) -> Option<FuncCell<'a>> {
        unsafe { self.func.load().map(|x| coerce_callable_lifetime(x)) }
    }

    // TODO: unbounded lifetime is unsound
    pub(crate) fn resolved_func<'a>(&self) -> Option<Callable<'a>> {
        match self.func() {
            Some(FuncCell::Symbol(sym)) => sym.resolved_func(),
            Some(FuncCell::LispFn(x)) => Some(Callable::LispFn(x)),
            Some(FuncCell::SubrFn(x)) => Some(Callable::SubrFn(x)),
            Some(FuncCell::Macro(x)) => Some(Callable::Macro(x)),
            None => None,
        }
    }

    unsafe fn set_func(&self, func: FuncCell) {
        self.func.store(Some(coerce_callable_lifetime(func)));
    }

    pub(crate) fn unbind_func(&self) {
        self.func.store(None);
    }
}

impl fmt::Display for GlobalSymbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.name)
    }
}

pub(crate) struct SymbolMap {
    map: InnerSymbolMap,
    arena: Arena,
}

/*
Box needs to follow rust's aliasing rules (references can't outlive the borrow).
Miri checks for this. But we have a requirement for this in our current design.
So we use this work around that I found on the forum.
*/
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

impl Drop for SymbolBox {
    fn drop(&mut self) {
        unsafe {
            Box::from_raw(self.0 as *mut GlobalSymbol);
        }
    }
}

struct InnerSymbolMap {
    map: HashMap<&'static str, SymbolBox>,
}

impl InnerSymbolMap {
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
                let inner = GlobalSymbol::new(static_name);
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

impl SymbolMap {
    pub(crate) fn intern(&mut self, name: &str) -> Symbol {
        self.map.intern(name)
    }

    pub(crate) fn set_func(&self, symbol: Symbol, func: FuncCell) {
        let mut obj: Object = func.clone_in(&self.arena);
        obj.make_read_only();
        let new_func: FuncCell = obj.try_into().expect("return type was not type we put in");
        #[cfg(miri)]
        new_func.set_as_miri_root();
        unsafe {
            symbol.set_func(new_func);
        }
    }
}

macro_rules! create_symbolmap {
    ($($arr:expr),+ $(,)?) => ({
        let size: usize = 0_usize $(+ $arr.len())+;
        let mut map = InnerSymbolMap::with_capacity(size);
        $(for (func, sym) in $arr.iter() {
            sym.func.store(Some(func.into()));
            map.pre_init(sym);
        })+;
        map
    })
}

lazy_static! {
    pub(crate) static ref INTERNED_SYMBOLS: Mutex<SymbolMap> = Mutex::new({
        let mut map = create_symbolmap!(
            crate::arith::DEFSUBR,
            crate::bytecode::DEFSUBR,
            crate::forms::DEFSUBR,
            crate::cons::DEFSUBR,
            crate::lread::DEFSUBR,
            crate::data::DEFSUBR,
            crate::fns::DEFSUBR,
            crate::alloc::DEFSUBR,
            crate::keymap::DEFSUBR,
        );
        map.pre_init(&sym::FUNCTION);
        map.pre_init(&sym::QUOTE);
        map.pre_init(&sym::MACRO);
        map.pre_init(&sym::UNQUOTE);
        map.pre_init(&sym::SPLICE);
        map.pre_init(&sym::BACKQUOTE);
        map.pre_init(&sym::NIL);
        map.pre_init(&sym::AND_OPTIONAL);
        map.pre_init(&sym::AND_REST);
        map.pre_init(&sym::LAMBDA);
        #[cfg(test)]
        {
            map.pre_init(&sym::test::FOO);
            map.pre_init(&sym::test::BAR);
            map.pre_init(&sym::test::BAZ);
        }
        SymbolMap {
            map,
            arena: Arena::new(),
        }
    });
}

pub(crate) fn intern(name: &str) -> Symbol {
    INTERNED_SYMBOLS.lock().unwrap().intern(name)
}

pub(crate) mod sym {
    use super::GlobalSymbol;

    pub(crate) static FUNCTION: GlobalSymbol = GlobalSymbol::new("function");
    pub(crate) static QUOTE: GlobalSymbol = GlobalSymbol::new("quote");
    pub(crate) static MACRO: GlobalSymbol = GlobalSymbol::new("macro");
    pub(crate) static UNQUOTE: GlobalSymbol = GlobalSymbol::new(",");
    pub(crate) static SPLICE: GlobalSymbol = GlobalSymbol::new(",@");
    pub(crate) static BACKQUOTE: GlobalSymbol = GlobalSymbol::new("`");
    pub(crate) static NIL: GlobalSymbol = GlobalSymbol::new("nil");
    pub(crate) static AND_OPTIONAL: GlobalSymbol = GlobalSymbol::new("&optional");
    pub(crate) static AND_REST: GlobalSymbol = GlobalSymbol::new("&rest");
    pub(crate) static LAMBDA: GlobalSymbol = GlobalSymbol::new("lambda");

    #[cfg(test)]
    pub(crate) mod test {
        use super::*;
        pub(crate) static FOO: GlobalSymbol = GlobalSymbol::new("foo");
        pub(crate) static BAR: GlobalSymbol = GlobalSymbol::new("bar");
        pub(crate) static BAZ: GlobalSymbol = GlobalSymbol::new("baz");
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::arena::Arena;
    use crate::data::Environment;
    use crate::object::{IntoObject, LispFn, Object, SubrFn};
    use std::mem::size_of;

    #[test]
    fn size() {
        assert_eq!(size_of::<isize>(), size_of::<Symbol>());
        assert_eq!(size_of::<isize>() * 3, size_of::<GlobalSymbol>());
    }

    unsafe fn fix_lifetime(inner: &GlobalSymbol) -> &'static GlobalSymbol {
        transmute::<&'_ GlobalSymbol, &'static GlobalSymbol>(inner)
    }

    #[test]
    fn symbol_func() {
        let arena = &Arena::new();
        let inner = GlobalSymbol::new("foo");
        let sym = unsafe { fix_lifetime(&inner) };
        assert_eq!("foo", sym.name);
        assert!(sym.func().is_none());
        let func = LispFn::new(vec![1].into(), vec![], 0, 0, false);
        unsafe {
            sym.set_func(func.into_obj(arena));
        }
        let cell = sym.func().unwrap();
        let before = match cell {
            FuncCell::LispFn(x) => !x,
            _ => unreachable!("Type should be a lisp function"),
        };
        assert_eq!(before.body.op_codes.get(0).unwrap(), &1);
        let func = LispFn::new(vec![7].into(), vec![], 0, 0, false);
        unsafe {
            sym.set_func(func.into_obj(arena));
        }
        let cell = sym.func().unwrap();
        let after = match cell {
            FuncCell::LispFn(x) => !x,
            _ => unreachable!("Type should be a lisp function"),
        };
        assert_eq!(after.body.op_codes.get(0).unwrap(), &7);
        assert_eq!(before.body.op_codes.get(0).unwrap(), &1);
    }

    #[allow(clippy::unnecessary_wraps)]
    fn dummy<'ob>(
        vars: &[Object<'ob>],
        _map: &mut Environment,
        _arena: &'ob Arena,
    ) -> anyhow::Result<Object<'ob>> {
        Ok(vars[0])
    }

    #[test]
    #[cfg_attr(miri, ignore)] // crashes Miri
    fn subr() {
        let arena = &Arena::new();

        let inner = GlobalSymbol::new("bar");
        let sym = unsafe { fix_lifetime(&inner) };
        let core_func = SubrFn::new("bar", dummy, 0, 0, false);
        unsafe {
            sym.set_func(core_func.into_obj(arena));
        }

        if let Some(FuncCell::SubrFn(subr)) = sym.func() {
            assert_eq!(*subr, SubrFn::new("bar", dummy, 0, 0, false));
        } else {
            unreachable!("Type should be subr");
        }
    }
}
