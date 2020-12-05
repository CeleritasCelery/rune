#![allow(dead_code)]
#[allow(unused_imports)]
use crate::lisp_object::{LispFn, CoreFn, LispObj};
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
    func: FnCell,
}

#[derive(Debug)]
struct FnCell(AtomicI64);

const LISP_FN_TAG: i64 = 0x1;
const CORE_FN_TAG: i64 = 0x2;
const FN_MASK: i64 = 0b11;

impl FnCell {
    fn new() -> Self { Self(AtomicI64::new(0)) }

    fn set_lisp(&self, func: LispFn) {
        let ptr: *const LispFn = Gc::new(func).as_ref();
        let value = ptr as i64 | LISP_FN_TAG;
        self.0.store(value, Ordering::Release);
    }

    fn set_core(&self, func: CoreFn) {
        let ptr: *const CoreFn = Gc::new(func).as_ref();
        let value = ptr as i64 | CORE_FN_TAG;
        self.0.store(value, Ordering::Release);
    }

    fn remove_tag(bits: i64) -> i64 { bits >> 2 << 2 }

    fn get(&self) -> Function {
        let bits = self.0.load(Ordering::Acquire);
        let ptr = Self::remove_tag(bits);
        match bits & FN_MASK {
            LISP_FN_TAG => unsafe { Function::Lisp(mem::transmute(ptr)) }
            CORE_FN_TAG => unsafe { Function::Subr(mem::transmute(ptr)) }
            _ => Function::None,
        }
    }
}

impl cmp::PartialEq for InnerSymbol {
    fn eq(&self, other: &Self) -> bool {
        (&*self as *const InnerSymbol) == (&*other as *const InnerSymbol)
    }
}

impl InnerSymbol {
    fn new(name: String) -> Self {
        InnerSymbol{name, func: FnCell::new()}
    }

    pub fn set_lisp_func(&self, func: LispFn) {
        self.func.set_lisp(func);
    }

    pub fn set_core_func(&self, func: CoreFn) {
        self.func.set_core(func);
    }

    pub fn get_func(&self) -> Function {
        self.func.get()
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }
}

#[derive(Debug, PartialEq)]
pub enum Function {
    Lisp(Gc<LispFn>),
    Subr(Gc<CoreFn>),
    None,
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
        assert_eq!(Function::None, x.get_func());
        x.set_lisp_func(LispFn::new(vec![1], vec![], 0, 0, false));
        let before = match x.get_func() {
            Function::Lisp(x) => x,
            _ => unreachable!(),
        };
        assert_eq!(before.as_ref().op_codes.get(0).unwrap(), &1);
        x.set_lisp_func(LispFn::new(vec![7], vec![], 0, 0, false));
        let after = match x.get_func() {
            Function::Lisp(x) => x,
            _ => unreachable!(),
        };
        assert_eq!(after.as_ref().op_codes.get(0).unwrap(), &7);
        assert_eq!(before.as_ref().op_codes.get(0).unwrap(), &1);
    }

    #[test]
    fn subr() {
        let func = |x: &[LispObj]| -> LispObj {
            x[0]
        };

        let sym = InnerSymbol::new("bar".to_owned());
        let core_func = CoreFn::new(func, 0, 0, false);
        sym.set_core_func(core_func);
        match sym.get_func() {
            Function::Subr(x) => {
                assert_eq!(*x.as_ref(), CoreFn::new(func, 0, 0, false));
            }
            _ => unreachable!(),
        }
    }

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
        assert_eq!(func.as_ref().op_codes.get(0).unwrap(), &5);
        assert_eq!(symbol_map.intern("batman"), symbol_map.intern("batman"));
    }
}
