#![allow(dead_code)]
use crate::lisp_object::{LispObj, LispFn};
use crate::gc::Gc;
use once_cell::unsync::Lazy;
use std::collections::HashMap;
use std::hash::BuildHasherDefault;
use std::cell::UnsafeCell;
use fnv::{FnvHashMap, FnvHasher};

pub struct Symbol {
    inner: UnsafeCell<InnerSymbol>
}

struct InnerSymbol {
    name: String,
    func: Option<Gc<LispFn>>,
    var: LispObj,
}

impl Symbol {
    fn get(&self) -> &InnerSymbol {
        unsafe {&*self.inner.get()}
    }

    fn get_mut(&self) -> &mut InnerSymbol {
        unsafe {&mut *self.inner.get()}
    }

    fn new(name: String) -> Self {
        Symbol{inner: UnsafeCell::new(InnerSymbol::new(name))}
    }

    pub fn set_func(&self, func: LispFn) {
        self.get_mut().func = Some(Gc::new(func));
    }

    pub fn get_func(&self) -> Option<Gc<LispFn>> {
        self.get().func.clone()
    }

    pub fn set_var(&self, var: LispObj) {
        self.get_mut().var = var;
    }

    pub fn get_var(&self) -> LispObj {
        self.get().var
    }

    pub fn get_name(&self) -> &str {
        &self.get().name
    }
}

impl InnerSymbol {
    fn new(name: String) -> Self {
        Self{name, func: None, var: LispObj::void()}
    }

    fn unbind_func(&mut self) {
    }

}

type FastHash = Lazy<HashMap<String, Box<Symbol>, BuildHasherDefault<FnvHasher>>>;
static mut INTERNED_SYMBOLS: FastHash = Lazy::new(||{FnvHashMap::default()});

pub fn intern(name: &str) -> &Symbol {
    unsafe {
        match INTERNED_SYMBOLS.get(name) {
            Some(x) => {x}
            None => {
                let sym = Symbol::new(name.to_owned());
                INTERNED_SYMBOLS.entry(name.to_owned()).or_insert(Box::new(sym))
            }
        }
    }
}

pub fn intern_mut(name: &str) -> &mut Symbol {
    unsafe {
        match INTERNED_SYMBOLS.get_mut(name) {
            Some(x) => {x}
            None => {
                let sym = Symbol::new(name.to_owned());
                INTERNED_SYMBOLS.entry(name.to_owned()).or_insert(Box::new(sym))
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn size() {
        assert_eq!(40, std::mem::size_of::<InnerSymbol>());
        assert_eq!(40, std::mem::size_of::<InnerSymbol>());
    }

    #[test]
    fn symbol_func() {
        let x = Symbol::new("foo".to_owned());
        assert_eq!("foo", x.get_name());
        assert_eq!(None, x.get_func());
        x.set_func(LispFn::new(1));
        let before = x.get_func().unwrap();
        assert_eq!(before.as_ref().op_codes.get(0).unwrap(), &1);
        x.set_func(LispFn::new(7));
        let after = x.get_func().unwrap();
        assert_eq!(after.as_ref().op_codes.get(0).unwrap(), &7);
        assert_eq!(before.as_ref().op_codes.get(0).unwrap(), &1);
    }

    #[test]
    fn symbol_var() {
        let x = Symbol::new("foo".to_owned());
        assert!(x.get_var().is_void());
        x.set_var(LispObj::from(7));
        assert_eq!(7, x.get_var().as_int().unwrap());
    }

    #[test]
    fn intern() {
        let first = super::intern("foo");
        assert_eq!("foo", first.get_name());
        assert_eq!(None, first.get_func());
        assert!(first.get_var().is_void());
        let second = super::intern("foo");
        second.set_func(LispFn::new(5));
        second.set_var(LispObj::from(7));
        assert_eq!(first.get_func().unwrap().as_ref().op_codes.get(0).unwrap(), &5);
        assert_eq!(7, first.get_var().as_int().unwrap());

    }
}
