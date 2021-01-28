#![allow(dead_code)]
use crate::lisp_object::{InnerSymbol, Symbol};
use crate::hashmap::HashMap;
use std::sync::Mutex;
use lazy_static::lazy_static;

pub struct SymbolMap(HashMap<String, Box<InnerSymbol>>);

impl SymbolMap {
    fn with_capacity(cap: usize) -> Self {
        use crate::hashmap::HashMapDefault;
        Self(HashMap::with_capacity(cap))
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
        unsafe { Symbol::from_raw(self.get_symbol(name)) }
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

macro_rules! create_symbolmap {
    ($($arr:expr),+ $(,)?) => ({
        const SIZE: usize = 0usize $(+ $arr.len())+;
        let mut map = SymbolMap::with_capacity(SIZE);
        $(for func in $arr.iter() {
            map.intern(func.name).set_func(func.clone());
        })+;
        map
    })
}

lazy_static!{
    pub static ref INTERNED_SYMBOLS: Mutex<SymbolMap> = Mutex::new({
        use crate::*;
        create_symbolmap!(
            arith::defsubr(),
            eval::defsubr(),
            func::defsubr(),
        )
    });
}

pub fn intern(name: &str) -> Symbol {
    INTERNED_SYMBOLS.lock().unwrap().intern(name)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lisp_object::{LispFn, FunctionValue};

    #[test]
    fn test_intern() {
        let mut symbol_map = INTERNED_SYMBOLS.lock().unwrap();
        let first = symbol_map.intern("foo");
        assert_eq!("foo", first.get_name());
        assert!(first.get_func().is_none());
        let second = symbol_map.intern("foo");
        second.set_func(LispFn::new(vec![5].into(), vec![], 0, 0, false));
        let func_cell = first.get_func().unwrap();
        let func = match func_cell.val() {
            FunctionValue::LispFn(x) => x,
            _ => unreachable!(),
        };
        assert_eq!(func.op_codes.get(0).unwrap(), &5);
        assert_eq!(symbol_map.intern("batman"), symbol_map.intern("batman"));
    }
}
