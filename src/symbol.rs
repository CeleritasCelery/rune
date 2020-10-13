#![allow(dead_code)]
use crate::lisp_object::LispFn;
use crate::gc::Gc;
use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::hash::BuildHasherDefault;
use fnv::{FnvHashMap, FnvHasher};
use std::mem;
use std::sync::Mutex;
use std::sync::atomic::{AtomicU64, Ordering};

pub struct Symbol {
    name: String,
    func: AtomicU64,
}

impl Symbol {
    fn new(name: String) -> Self {
        Symbol{name, func: AtomicU64::new(0)}
    }

    pub fn set_func(&self, func: LispFn) {
        let ptr: *const LispFn = Gc::new(func).as_ref();
        self.func.store(ptr as u64, Ordering::Release);
    }

    pub fn get_func(&self) -> Option<Gc<LispFn>> {
        let atomic = self.func.load(Ordering::Acquire);
        unsafe { mem::transmute(atomic) }
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }
}

pub struct SymbolMap(HashMap<String, Box<Symbol>, BuildHasherDefault<FnvHasher>>);

impl SymbolMap {
    fn new() -> Self {
        Self(FnvHashMap::default())
    }

    pub fn size(&self) -> usize {
        self.0.keys().len()
    }

    pub fn intern(&mut self, name: &str) -> &'static Symbol {
        // SAFETY: This is my work around for there being no Entry API that
        // takes a reference. Instead we have an inner function that returns a
        // pointer and we cast that to a static reference. We can guarntee that
        // the reference is static because we have no methods to remove items
        // from SymbolMap and SymbolMap has a private constructor, so the only
        // one that exists is the one we create in this module, which is static.
        // https://internals.rust-lang.org/t/pre-rfc-abandonning-morals-in-the-name-of-performance-the-raw-entry-api/7043
        unsafe { mem::transmute(self.get_symbol(name)) }
    }

    fn get_symbol(&mut self, name: &str) -> *const Symbol {
        match self.0.get(name) {
            Some(x) => x.as_ref(),
            None => {
                let sym = Box::new(Symbol::new(name.to_owned()));
                let ptr = sym.as_ref() as *const Symbol;
                self.0.insert(name.to_owned(), sym);
                ptr
            }
        }
    }
}

pub static INTERNED_SYMBOLS: Lazy<Mutex<SymbolMap>> = Lazy::new(||Mutex::new(SymbolMap::new()));

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn size() {
        assert_eq!(32, std::mem::size_of::<Symbol>());
    }

    #[test]
    fn symbol_func() {
        let x = Symbol::new("foo".to_owned());
        assert_eq!("foo", x.get_name());
        assert_eq!(None, x.get_func());
        x.set_func(LispFn::new(vec![1], vec![], 0, 0, false));
        let before = x.get_func().unwrap();
        assert_eq!(before.as_ref().op_codes.get(0).unwrap(), &1);
        x.set_func(LispFn::new(vec![7], vec![], 0, 0, false));
        let after = x.get_func().unwrap();
        assert_eq!(after.as_ref().op_codes.get(0).unwrap(), &7);
        assert_eq!(before.as_ref().op_codes.get(0).unwrap(), &1);
    }

    #[test]
    fn intern() {
        let mut symbol_map = INTERNED_SYMBOLS.lock().unwrap();
        let first = symbol_map.intern("foo");
        assert_eq!("foo", first.get_name());
        assert_eq!(None, first.get_func());
        let second = symbol_map.intern("foo");
        second.set_func(LispFn::new(vec![5], vec![], 0, 0, false));
        assert_eq!(first.get_func().unwrap().as_ref().op_codes.get(0).unwrap(), &5);
    }
}
