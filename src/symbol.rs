#![allow(dead_code)]
use crate::lisp_object::{LispFn, LispObj, Value};
use crate::gc::Gc;
use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::hash::BuildHasherDefault;
use fnv::{FnvHashMap, FnvHasher};
use std::cmp;
use std::mem;
use std::sync::Mutex;
use std::sync::atomic::{AtomicI64, Ordering};

#[derive(Debug)]
pub struct InnerSymbol {
    name: String,
    func: AtomicI64,
}

impl cmp::PartialEq for InnerSymbol {
    fn eq(&self, other: &Self) -> bool {
        (&*self as *const InnerSymbol) == (&*other as *const InnerSymbol)
    }
}

impl InnerSymbol {
    fn new(name: String) -> Self {
        InnerSymbol{name, func: AtomicI64::new(0)}
    }

    pub fn set_func(&self, func: LispFn) {
        self.func.store(LispObj::from(func).into_raw(), Ordering::Release);
    }

    pub fn get_func(&self) -> Option<Gc<LispFn>> {
        let bits = self.func.load(Ordering::Acquire);
        unsafe {
            match LispObj::from_raw(bits).val() {
                Value::Function(x) => Some(mem::transmute(x)),
                _ => None,
            }
        }
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }
}

pub struct SymbolMap(HashMap<String, Box<InnerSymbol>, BuildHasherDefault<FnvHasher>>);

impl SymbolMap {
    fn new() -> Self {
        Self(FnvHashMap::default())
    }

    pub fn size(&self) -> usize {
        self.0.keys().len()
    }

    pub fn intern(&mut self, name: &str) -> &'static InnerSymbol {
        // SAFETY: This is my work around for there being no Entry API that
        // takes a reference. Instead we have an inner function that returns a
        // pointer and we cast that to a static reference. We can guarantee that
        // the reference is static because we have no methods to remove items
        // from SymbolMap and SymbolMap has a private constructor, so the only
        // one that exists is the one we create in this module, which is static.
        // https://internals.rust-lang.org/t/pre-rfc-abandonning-morals-in-the-name-of-performance-the-raw-entry-api/7043
        unsafe { mem::transmute(self.get_symbol(name)) }
    }

    fn get_symbol(&mut self, name: &str) -> *const InnerSymbol {
        match self.0.get(name) {
            Some(x) => x.as_ref(),
            None => {
                let sym = Box::new(InnerSymbol::new(name.to_owned()));
                let ptr = sym.as_ref() as *const InnerSymbol;
                self.0.insert(name.to_owned(), sym);
                ptr
            }
        }
    }
}

pub static INTERNED_SYMBOLS: Lazy<Mutex<SymbolMap>> = Lazy::new(||Mutex::new(SymbolMap::new()));

pub type Symbol = &'static InnerSymbol;

pub fn intern(name: &str) -> &'static InnerSymbol {
    INTERNED_SYMBOLS.lock().unwrap().intern(name)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn size() {
        assert_eq!(32, std::mem::size_of::<InnerSymbol>());
    }

    #[test]
    fn symbol_func() {
        let x = InnerSymbol::new("foo".to_owned());
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
    fn test_intern() {
        let mut symbol_map = INTERNED_SYMBOLS.lock().unwrap();
        let first = symbol_map.intern("foo");
        assert_eq!("foo", first.get_name());
        assert_eq!(None, first.get_func());
        let second = symbol_map.intern("foo");
        second.set_func(LispFn::new(vec![5], vec![], 0, 0, false));
        assert_eq!(first.get_func().unwrap().as_ref().op_codes.get(0).unwrap(), &5);
        assert_eq!(symbol_map.intern("batman"), symbol_map.intern("batman"));
    }
}
