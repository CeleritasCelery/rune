use crate::lisp_object::{LispObj, LispFn, BuiltInFn, TAG_SIZE, Tag};
use crate::gc::Gc;
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

    fn set_core(&self, func: BuiltInFn) {
        let ptr: *const BuiltInFn = Gc::new(func).as_ref();
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
    pub fn new(name: String) -> Self {
        InnerSymbol{name, func: FnCell::new()}
    }

    pub fn set_lisp_func(&self, func: LispFn) {
        self.func.set_lisp(func);
    }

    pub fn set_core_func(&self, func: BuiltInFn) {
        self.func.set_core(func);
    }

    pub fn get_func(&self) -> Function {
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

#[derive(Debug, PartialEq)]
pub enum Function {
    Lisp(Gc<LispFn>),
    Subr(Gc<BuiltInFn>),
    None,
}

#[cfg(test)]
mod test {
        use super::*;
    use crate::lisp_object::Function;

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
        assert_eq!(before.op_codes.get(0).unwrap(), &1);
        x.set_lisp_func(LispFn::new(vec![7], vec![], 0, 0, false));
        let after = match x.get_func() {
            Function::Lisp(x) => x,
            _ => unreachable!(),
        };
        assert_eq!(after.op_codes.get(0).unwrap(), &7);
        assert_eq!(before.op_codes.get(0).unwrap(), &1);
    }

    #[test]
    fn subr() {
        let func = |x: &[LispObj]| -> LispObj {
            x[0]
        };

        let sym = InnerSymbol::new("bar".to_owned());
        let core_func = BuiltInFn::new("bar", func, 0, 0, false);
        sym.set_core_func(core_func);
        match sym.get_func() {
            Function::Subr(x) => {
                assert_eq!(*x, BuiltInFn::new("bar", func, 0, 0, false));
            }
            _ => unreachable!(),
        }
    }
}
