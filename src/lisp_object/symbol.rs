use crate::lisp_object::{Function, LispObj, Tag, TAG_SIZE, IntoObject, Object};
use crate::arena::Arena;
use std::cmp;
use std::fmt;
use std::mem;
use std::sync::atomic::{AtomicI64, Ordering};

#[derive(Debug)]
pub struct InnerSymbol {
    name: String,
    func: FnCell,
}

#[derive(Debug)]
struct FnCell(AtomicI64);

impl FnCell {
    const fn new() -> Self {
        Self(AtomicI64::new(0))
    }

    fn set(&self, func: Function) {
        let value = unsafe { mem::transmute(func) };
        self.0.store(value, Ordering::Release);
    }

    fn get(&self) -> Option<Function> {
        let bits = self.0.load(Ordering::Acquire);
        match bits {
            0 => None,
            _ => Some(unsafe { mem::transmute(bits) }),
        }
    }
}

impl cmp::PartialEq for InnerSymbol {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(&*self, &*other)
    }
}

impl InnerSymbol {
    pub const fn new(name: String) -> Self {
        InnerSymbol {
            name,
            func: FnCell::new(),
        }
    }

    pub fn set_func<T>(&self, func: T)
    where
        T: Into<Function>,
    {
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
define_unbox!(Symbol);

impl Symbol {
    #[allow(clippy::missing_const_for_fn)]
    pub unsafe fn from_raw(ptr: *const InnerSymbol) -> Symbol {
        Symbol(&*ptr)
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
        LispObj { bits }
    }
}

impl<'obj> IntoObject<'obj> for Symbol {
    fn into_object(self, _alloc: &Arena) -> (Object, bool) {
        let ptr = self.0 as *const _;
        unsafe {
            (Object::from_ptr(ptr as *mut u8, Tag::Symbol), false)
        }
    }
}

impl std::cmp::PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(&*self.0, &*other.0)
    }
}

impl std::cmp::Eq for Symbol {}

impl std::hash::Hash for Symbol {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        let bits = (self.0 as *const _) as u64;
        bits.hash(state);
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lisp_object::{FunctionValue, LispFn, SubrFn};

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
        x.set_func(LispFn::new(vec![1].into(), vec![], 0, 0, false));
        let cell = x.get_func().unwrap();
        let before = match cell.val() {
            FunctionValue::LispFn(x) => x,
            _ => unreachable!(),
        };
        assert_eq!(before.op_codes.get(0).unwrap(), &1);
        x.set_func(LispFn::new(vec![7].into(), vec![], 0, 0, false));
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
        let func = |x: &[_], _: &mut _| -> _ { Ok(x[0]) };

        let sym = InnerSymbol::new("bar".to_owned());
        let core_func = SubrFn::new("bar", func, 0, 0, false);
        sym.set_func(core_func);

        match sym.get_func().unwrap().val() {
            FunctionValue::SubrFn(x) => {
                assert_eq!(*x, SubrFn::new("bar", func, 0, 0, false));
            }
            _ => unreachable!(),
        };
    }
}
