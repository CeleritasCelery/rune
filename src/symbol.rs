#![allow(dead_code)]
use crate::lisp_object::{InnerSymbol, Symbol};
use std::collections::HashMap;
use std::hash::BuildHasherDefault;
use fnv::{FnvHashMap, FnvHasher};
use std::mem;
use std::sync::Mutex;
use lazy_static::lazy_static;

pub struct SymbolMap(HashMap<String, Box<InnerSymbol>, BuildHasherDefault<FnvHasher>>);

impl SymbolMap {
    fn new() -> Self {
        Self(FnvHashMap::default())
    }

    pub fn size(&self) -> usize {
        self.0.keys().len()
    }

    pub fn intern(&mut self, name: &str) -> Symbol {
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

lazy_static!{
    pub static ref INTERNED_SYMBOLS: Mutex<SymbolMap> = Mutex::new({
        use crate::*;
        let mut map = SymbolMap::new();
        for func in arith::defsubr().iter() {
            map.intern(func.name).set_core_func(func.clone());
        }
        map
    });
}

pub fn intern(name: &str) -> Symbol {
    INTERNED_SYMBOLS.lock().unwrap().intern(name)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lisp_object::{Function, LispFn};

    #[test]
    fn test_intern() {
        let mut symbol_map = INTERNED_SYMBOLS.lock().unwrap();
        let first = symbol_map.intern("foo");
        assert_eq!("foo", first.get_name());
        assert_eq!(Function::None, first.get_func());
        let second = symbol_map.intern("foo");
        second.set_lisp_func(LispFn::new(vec![5], vec![], 0, 0, false));
        let func = match first.get_func() {
            Function::Lisp(x) => x,
            _ => unreachable!(),
        };
        assert_eq!(func.op_codes.get(0).unwrap(), &5);
        assert_eq!(symbol_map.intern("batman"), symbol_map.intern("batman"));
    }
}
