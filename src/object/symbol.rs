use crate::arena::Arena;
use crate::object::*;
use std::cmp;
use std::fmt;
use std::mem::transmute;
use std::sync::atomic::{AtomicI64, Ordering};

#[derive(Debug)]
pub struct InnerSymbol {
    name: &'static str,
    func: FnCell,
}

#[derive(Debug)]
struct FnCell(AtomicI64);

impl FnCell {
    const fn new() -> Self {
        Self(AtomicI64::new(0))
    }

    fn set(&self, func: Option<Function>) {
        let value = unsafe { transmute(func) };
        self.0.store(value, Ordering::Release);
    }

    fn get(&self) -> Option<Function> {
        let bits = self.0.load(Ordering::Acquire);
        unsafe { transmute(bits) }
    }
}

impl cmp::PartialEq for InnerSymbol {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(&*self, &*other)
    }
}

impl InnerSymbol {
    pub const fn new(name: &'static str) -> Self {
        InnerSymbol {
            name,
            func: FnCell::new(),
        }
    }

    pub fn set_func(&self, func: Function) {
        self.func.set(Some(func));
    }

    pub fn get_func(&self) -> Option<Function> {
        self.func.get()
    }

    pub const fn get_name(&self) -> &str {
        self.name
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

impl<'obj> From<Symbol> for Object<'obj> {
    fn from(s: Symbol) -> Self {
        let ptr: *const _ = s.0;
        InnerObject::from_ptr(ptr as *mut u8, Tag::Symbol).into()
    }
}

impl<'obj> IntoObject<'obj, Object<'obj>> for Symbol {
    fn into_obj(self, _arena: &'obj Arena) -> Object<'obj> {
        self.into()
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
        let inner: *const _ = self.0;
        let bits = inner as u64;
        bits.hash(state);
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::data::Environment;
    use crate::object::{LispFn, SubrFn};
    use std::mem::size_of;

    #[test]
    fn size() {
        assert_eq!(size_of::<isize>(), size_of::<Symbol>());
    }

    #[test]
    fn symbol_func() {
        let arena = &Arena::new();
        let x = InnerSymbol::new("foo");
        assert_eq!("foo", x.get_name());
        assert!(x.get_func().is_none());
        let func = LispFn::new(vec![1].into(), vec![], 0, 0, false);
        x.set_func(func.into_obj(arena));
        let cell = x.get_func().unwrap();
        let before = cell.as_lisp_fn().expect("expected lispfn");
        assert_eq!(before.body.op_codes.get(0).unwrap(), &1);
        let func = LispFn::new(vec![7].into(), vec![], 0, 0, false);
        x.set_func(func.into_obj(arena));
        let cell = x.get_func().unwrap();
        let after = cell.as_lisp_fn().expect("expected lispfn");
        assert_eq!(after.body.op_codes.get(0).unwrap(), &7);
        assert_eq!(before.body.op_codes.get(0).unwrap(), &1);
    }

    #[allow(clippy::unnecessary_wraps)]
    fn dummy<'obj>(
        vars: &[Object<'obj>],
        _map: &mut Environment,
        _arena: &'obj Arena,
    ) -> anyhow::Result<Object<'obj>> {
        Ok(vars[0])
    }

    #[test]
    #[cfg_attr(miri, ignore)] // crashes Miri
    fn subr() {
        let arena = &Arena::new();

        let sym = InnerSymbol::new("bar");
        let core_func = SubrFn::new("bar", dummy, 0, 0, false);
        sym.set_func(core_func.into_obj(arena));

        let subr = sym
            .get_func()
            .unwrap()
            .as_subr_fn()
            .expect("expected subrfn");
        assert_eq!(*subr, SubrFn::new("bar", dummy, 0, 0, false));
    }
}
