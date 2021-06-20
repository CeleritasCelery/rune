use crate::arena::Arena;
use crate::hashmap::HashMap;
use crate::object::{Function, InnerSymbol, IntoObject, Symbol};
use lazy_static::lazy_static;
use std::sync::Mutex;

#[allow(dead_code)]
pub struct SymbolMap {
    map: InnerSymbolMap,
    arena: Arena,
}

struct SymbolBox(*const InnerSymbol);
unsafe impl Send for SymbolBox {}

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
        unsafe { Symbol::from_raw(self.get_symbol(name)) }
    }

    // This is my work around for there being no Entry API that takes a
    // reference.
    // https://internals.rust-lang.org/t/pre-rfc-abandonning-morals-in-the-name-of-performance-the-raw-entry-api/7043
    fn get_symbol(&mut self, name: &str) -> *const InnerSymbol {
        match self.map.get(name) {
            Some(x) => x.0,
            None => {
                let sym = Box::new(InnerSymbol::new(name.to_owned()));
                let ptr: *const InnerSymbol = sym.as_ref();
                self.map
                    .insert(name.to_owned(), SymbolBox(Box::into_raw(sym)));
                ptr
            }
        }
    }
}

impl SymbolMap {
    pub fn intern(&mut self, name: &str) -> Symbol {
        self.map.intern(name)
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
            object::cons::defsubr(),
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
    use crate::object::LispFn;

    #[test]
    fn test_intern() {
        let arena = &Arena::new();
        let mut symbol_map = INTERNED_SYMBOLS.lock().unwrap();
        let first = symbol_map.intern("foo");
        assert_eq!("foo", first.get_name());
        assert!(first.get_func().is_none());
        let second = symbol_map.intern("foo");
        let func = LispFn::new(vec![5].into(), vec![], 0, 0, false);
        let obj: Function = func.into_obj(arena);
        second.set_func(obj);
        let func_cell = first.get_func().unwrap();
        let func = func_cell.as_lisp_fn().expect("expected lispfn");
        assert_eq!(func.op_codes.get(0).unwrap(), &5);
        assert_eq!(symbol_map.intern("batman"), symbol_map.intern("batman"));
    }
}
