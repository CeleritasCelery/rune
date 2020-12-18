use crate::lisp_object::{LispObj, LispFn, SubrFn, TAG_SIZE, Tag, Function};
use std::cmp;
use std::mem;
use std::sync::atomic::{AtomicI64, Ordering};

#[derive(Debug)]
pub struct InnerSymbol {
    name: String,
    func: FnCell,
}

pub type Symbol = &'static InnerSymbol;

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

impl From<Symbol> for LispObj {
    fn from(s: Symbol) -> Self {
        let ptr = s as *const InnerSymbol;
        let bits = ((ptr as i64) << TAG_SIZE) | Tag::Symbol as i64;
        LispObj{bits}
    }
}

#[cfg(test)]
mod test {
    use crate::lisp_object::FunctionValue;
    use super::*;

    #[test]
    fn size() {
        assert_eq!(32, std::mem::size_of::<InnerSymbol>());
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
        let func = |x: &[LispObj]| -> Result<LispObj, crate::error::Error> {
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
