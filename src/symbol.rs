use crate::arena::Arena;
use crate::hashmap::HashMap;
use crate::object::{Function, IntoObject};
use lazy_static::lazy_static;
use std::cmp;
use std::convert::TryInto;
use std::fmt;
use std::mem::transmute;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::Mutex;

#[derive(Debug)]
struct InnerSymbol {
    name: &'static str,
    func: FnCell,
}

#[derive(Debug)]
struct FnCell(AtomicI64);

impl FnCell {
    const fn new() -> Self {
        Self(AtomicI64::new(0))
    }

    fn set(&self, func: Option<Function>) {
        let value = unsafe { transmute(func) };
        self.0.store(value, Ordering::Release);
    }

    fn get(&self) -> Option<Function<'static>> {
        let bits = self.0.load(Ordering::Acquire);
        unsafe { transmute(bits) }
    }
}

impl cmp::PartialEq for InnerSymbol {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(&*self, &*other)
    }
}

impl InnerSymbol {
    const fn new(name: &'static str) -> Self {
        InnerSymbol {
            name,
            func: FnCell::new(),
        }
    }
}

#[derive(Copy, Clone)]
pub struct Symbol(&'static InnerSymbol);

impl Symbol {
    #[allow(clippy::missing_const_for_fn)]
    pub unsafe fn from_raw(ptr: *const u8) -> Symbol {
        let ptr = ptr as *const InnerSymbol;
        Symbol(&*ptr)
    }

    pub fn as_ptr(&self) -> *const u8 {
        let ptr: *const _ = self.0;
        ptr as *const _
    }

    pub const fn get_name(&self) -> &str {
        self.0.name
    }

    pub fn get_func(&self) -> Option<Function<'static>> {
        self.0.func.get()
    }

    fn set_func(&self, func: Function) {
        self.0.func.set(Some(func));
    }
}

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "'{}", &self.0.name)
    }
}

impl std::cmp::PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(&*self.0, &*other.0)
    }
}

impl std::cmp::Eq for Symbol {}

impl std::hash::Hash for Symbol {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let inner: *const _ = self.0;
        let bits = inner as u64;
        bits.hash(state);
    }
}

pub struct SymbolMap {
    map: InnerSymbolMap,
    arena: Arena,
}

/*
Box needs to follow rust's aliasing rules (references can't outlive the borrow).
Miri checks for this. But we have a requirement for this in our current design.
So we use this work around that I found on the forum.
*/
struct SymbolBox(*const InnerSymbol);
unsafe impl Send for SymbolBox {}

impl SymbolBox {
    fn new(inner: InnerSymbol) -> Self {
        let ptr = Box::into_raw(Box::new(inner));
        Self(ptr)
    }
}

impl AsRef<InnerSymbol> for SymbolBox {
    fn as_ref(&self) -> &InnerSymbol {
        unsafe { &*self.0 }
    }
}

impl Drop for SymbolBox {
    fn drop(&mut self) {
        unsafe {
            Box::from_raw(self.0 as *mut InnerSymbol);
        }
    }
}

struct InnerSymbolMap {
    map: HashMap<String, SymbolBox>,
}

impl InnerSymbolMap {
    fn with_capacity(cap: usize) -> Self {
        Self {
            map: HashMap::with_capacity_and_hasher(cap, Default::default()),
        }
    }

    fn intern(&mut self, name: &str) -> Symbol {
        // SAFETY: We can guarantee that the reference is static because we have
        // no methods to remove items from SymbolMap and SymbolMap has a private
        // constructor, so the only one that exists is the one we create in this
        // module, which is static.
        unsafe { Symbol::from_raw(self.get_symbol(name) as *const _) }
    }

    // This is my work around for there being no Entry API that takes a
    // reference.
    // https://internals.rust-lang.org/t/pre-rfc-abandonning-morals-in-the-name-of-performance-the-raw-entry-api/7043
    fn get_symbol(&mut self, name: &str) -> *const InnerSymbol {
        match self.map.get(name) {
            Some(x) => x.0,
            None => {
                let name = name.to_owned();
                let sym = {
                    let ptr: &'static str = unsafe { transmute(name.as_str()) };
                    let inner = InnerSymbol::new(ptr);
                    SymbolBox::new(inner)
                };
                let ptr: *const InnerSymbol = sym.as_ref();
                self.map.insert(name, sym);
                ptr
            }
        }
    }
}

impl SymbolMap {
    pub fn intern(&mut self, name: &str) -> Symbol {
        self.map.intern(name)
    }

    pub fn set_func(&self, symbol: Symbol, func: Function) {
        let new_obj = func.clone_in(&self.arena);
        let new_func: Function = new_obj.try_into().unwrap();
        symbol.set_func(new_func);
    }
}

macro_rules! create_symbolmap {
    ($($arr:expr),+ $(,)?) => ({
        const SIZE: usize = 0usize $(+ $arr.len())+;
        let mut map = InnerSymbolMap::with_capacity(SIZE);
        let arena = Arena::new();
        $(for func in $arr.iter() {
            let func_obj: Function = func.into_obj(&arena);
            map.intern(func.name).set_func(func_obj);
        })+;
        SymbolMap{ map, arena }
    })
}

lazy_static! {
    pub static ref INTERNED_SYMBOLS: Mutex<SymbolMap> = Mutex::new({
        use crate::*;
        create_symbolmap!(
            arith::defsubr(),
            eval::defsubr(),
            forms::defsubr(),
            cons::defsubr(),
            lread::defsubr(),
            data::defsubr(),
        )
    });
}

pub fn intern(name: &str) -> Symbol {
    INTERNED_SYMBOLS.lock().unwrap().intern(name)
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
    }

    unsafe fn fix_lifetime(inner: &InnerSymbol) -> &'static InnerSymbol {
        transmute::<&'_ InnerSymbol, &'static InnerSymbol>(inner)
    }

    #[test]
    fn symbol_func() {
        let arena = &Arena::new();
        let inner = InnerSymbol::new("foo");
        let sym = Symbol(unsafe { fix_lifetime(&inner) });
        assert_eq!("foo", sym.get_name());
        assert!(sym.get_func().is_none());
        let func = LispFn::new(vec![1].into(), vec![], 0, 0, false);
        sym.set_func(func.into_obj(arena));
        let cell = sym.get_func().unwrap();
        let before = cell.as_lisp_fn().expect("expected lispfn");
        assert_eq!(before.body.op_codes.get(0).unwrap(), &1);
        let func = LispFn::new(vec![7].into(), vec![], 0, 0, false);
        sym.set_func(func.into_obj(arena));
        let cell = sym.get_func().unwrap();
        let after = cell.as_lisp_fn().expect("expected lispfn");
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

        let inner = InnerSymbol::new("bar");
        let sym = Symbol(unsafe { fix_lifetime(&inner) });
        let core_func = SubrFn::new("bar", dummy, 0, 0, false);
        sym.set_func(core_func.into_obj(arena));

        let subr = sym
            .get_func()
            .unwrap()
            .as_subr_fn()
            .expect("expected subrfn");
        assert_eq!(*subr, SubrFn::new("bar", dummy, 0, 0, false));
    }
}
