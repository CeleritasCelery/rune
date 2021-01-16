use crate::lisp_object::{LispObj, LispFn, SubrFn, TAG_SIZE, Tag, Function};
use std::cmp;
use std::mem;
use std::fmt;
use std::sync::atomic::{AtomicI64, Ordering};
use fn_macros::lisp_fn;

#[derive(Debug)]
pub struct InnerSymbol {
    name: String,
    func: FnCell,
}

#[derive(Debug)]
struct FnCell(AtomicI64);

impl FnCell {
    fn new() -> Self { Self(AtomicI64::new(0)) }

    fn set(&self, func: Function) {
        let value = unsafe {mem::transmute(func)};
        self.0.store(value, Ordering::Release);
    }

    fn get(&self) -> Option<Function> {
        let bits = self.0.load(Ordering::Acquire);
        match bits {
            0 => None,
            _ => Some(unsafe {mem::transmute(bits)})
        }
    }
}

impl cmp::PartialEq for InnerSymbol {
    fn eq(&self, other: &Self) -> bool {
        (&*self as *const InnerSymbol) == (&*other as *const InnerSymbol)
    }
}

impl InnerSymbol {
    pub fn new(name: String) -> Self {
        InnerSymbol{name, func: FnCell::new()}
    }

    pub fn set_lisp_func(&self, func: LispFn) {
        self.func.set(func.into());
    }

    pub fn set_core_func(&self, func: SubrFn) {
        self.func.set(func.into());
    }

    pub fn get_func(&self) -> Option<Function> {
        self.func.get()
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }
}

#[derive(Copy, Clone)]
pub struct Symbol(&'static InnerSymbol);

impl Symbol {
    pub unsafe fn from_raw(ptr: *const InnerSymbol) -> Symbol {
        Symbol(std::mem::transmute(ptr))
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

impl std::ops::Deref for Symbol {
    type Target = InnerSymbol;

    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl From<Symbol> for LispObj {
    fn from(s: Symbol) -> Self {
        let ptr = s.0 as *const _;
        let bits = ((ptr as i64) << TAG_SIZE) | Tag::Symbol as i64;
        LispObj{bits}
    }
}

impl std::cmp::PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
       self.0 as *const _ == other.0 as *const _
    }
}

impl std::cmp::Eq for Symbol {}

impl std::hash::Hash for Symbol {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let bits = (self.0 as *const _) as u64;
        bits.hash(state);
    }
}

#[lisp_fn]
pub fn defalias(symbol: Symbol, definition: Function) -> Symbol {
    symbol.func.set(definition);
    symbol
}

defsubr!(defalias);

#[cfg(test)]
mod test {
    use crate::lisp_object::FunctionValue;
    use super::*;

    #[test]
    fn size() {
        assert_eq!(32, std::mem::size_of::<InnerSymbol>());
        assert_eq!(8, std::mem::size_of::<Symbol>());
    }

    #[test]
    fn symbol_func() {
        let x = InnerSymbol::new("foo".to_owned());
        assert_eq!("foo", x.get_name());
        assert!(x.get_func().is_none());
        x.set_lisp_func(LispFn::new(vec![1], vec![], 0, 0, false));
        let cell = x.get_func().unwrap();
        let before = match cell.val() {
            FunctionValue::LispFn(x) => x,
            _ => unreachable!(),
        };
        assert_eq!(before.op_codes.get(0).unwrap(), &1);
        x.set_lisp_func(LispFn::new(vec![7], vec![], 0, 0, false));
        let cell = x.get_func().unwrap();
        let after = match cell.val() {
            FunctionValue::LispFn(x) => x,
            _ => unreachable!(),
        };
        assert_eq!(after.op_codes.get(0).unwrap(), &7);
        assert_eq!(before.op_codes.get(0).unwrap(), &1);
    }

    #[test]
    fn subr() {
        let func = |x: &[_], _: &mut _| -> _ {
            Ok(x[0])
        };

        let sym = InnerSymbol::new("bar".to_owned());
        let core_func = SubrFn::new("bar", func, 0, 0, false);
        sym.set_core_func(core_func);

        match sym.get_func().unwrap().val() {
            FunctionValue::SubrFn(x) => {
                assert_eq!(*x, SubrFn::new("bar", func, 0, 0, false));
            }
            _ => unreachable!(),
        };
    }
}
