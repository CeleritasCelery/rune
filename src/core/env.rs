use super::gc::{Block, Context, Rt};
use super::object::{CloneIn, Function, Gc, GcObj};
use crate::hashmap::{HashMap, HashSet};
use anyhow::{anyhow, Result};
use fn_macros::Trace;
use lazy_static::lazy_static;
use std::sync::Mutex;

mod symbol;
pub(crate) use symbol::*;

#[derive(Debug, Default, Trace)]
pub(crate) struct Env {
    pub(crate) vars: HashMap<&'static Symbol, GcObj<'static>>,
    pub(crate) props: HashMap<&'static Symbol, Vec<(&'static Symbol, GcObj<'static>)>>,
    pub(crate) catch_stack: Vec<GcObj<'static>>,
    exception: (GcObj<'static>, GcObj<'static>),
    #[no_trace]
    exception_id: u32,
    pub(crate) special_variables: HashSet<&'static Symbol>,
    binding_stack: Vec<(&'static Symbol, Option<GcObj<'static>>)>,
    pub(crate) match_data: GcObj<'static>,
}

impl Rt<Env> {
    pub(crate) fn set_var(&mut self, sym: &Symbol, value: GcObj) -> Result<()> {
        if sym.is_const() {
            Err(anyhow!("Attempt to set a constant symbol: {sym}"))
        } else {
            self.vars.insert(sym, value);
            Ok(())
        }
    }

    pub(crate) fn set_prop(&mut self, symbol: &Symbol, propname: &Symbol, value: GcObj) {
        match self.props.get_mut(symbol) {
            Some(plist) => match plist.iter_mut().find(|x| x.0 == propname) {
                Some(x) => x.1.set(value),
                None => plist.push((propname, value)),
            },
            None => {
                self.props.insert(symbol, vec![(propname, value)]);
            }
        }
    }

    pub(in crate::core) fn set_exception(&mut self, tag: GcObj, data: GcObj) -> u32 {
        self.exception.0.set(tag);
        self.exception.1.set(data);
        self.exception_id += 1;
        self.exception_id
    }

    pub(crate) fn get_exception(
        &self,
        id: u32,
    ) -> Option<(&Rt<GcObj<'static>>, &Rt<GcObj<'static>>)> {
        (id == self.exception_id).then_some((&self.exception.0, &self.exception.1))
    }

    pub(crate) fn varbind(&mut self, var: &Symbol, value: GcObj, cx: &Context) {
        let prev_value = self.vars.get(var).map(|x| x.bind(cx));
        self.binding_stack.push((var, prev_value));
        self.vars.insert(var, value);
    }

    pub(crate) fn unbind(&mut self, count: u16, cx: &Context) {
        for _ in 0..count {
            match self.binding_stack.pop_obj(cx) {
                Some((sym, val)) => match val {
                    Some(val) => self.vars.insert(sym, val),
                    None => self.vars.remove(sym),
                },
                None => panic!("Binding stack was empty"),
            }
        }
    }

    pub(crate) fn defvar(&mut self, var: &Symbol, value: GcObj) -> Result<()> {
        self.set_var(var, value)?;
        self.special_variables.insert(var);
        // If this variable was unbound previously in the binding stack,
        // we will bind it to the new value
        for binding in self.binding_stack.iter_mut() {
            if binding.0 == var && binding.1.is_none() {
                binding.1.set(value);
            }
        }
        Ok(())
    }
}

pub(crate) struct ObjectMap {
    map: SymbolMap,
    block: Block<true>,
}

/// Box is marked as unique. However we are freely sharing the pointer to this
/// Symbol amoung threads. So instead of Box we need to use a custom wrapper
/// type for this to be sound.
struct SymbolBox(*const Symbol);
unsafe impl Send for SymbolBox {}

impl SymbolBox {
    fn new(inner: Symbol) -> Self {
        let ptr = Box::into_raw(Box::new(inner));
        Self(ptr)
    }

    fn from_static(inner: &'static Symbol) -> Self {
        Self(inner)
    }
}

impl AsRef<Symbol> for SymbolBox {
    fn as_ref(&self) -> &Symbol {
        unsafe { &*self.0 }
    }
}

// `SymbolBox` should not be dropped until we
// have a garbage collector
impl Drop for SymbolBox {
    fn drop(&mut self) {
        assert!(
            !std::thread::panicking(),
            "Error: Tried to drop Symbol: {:?}",
            unsafe { &*self.0 }
        );
    }
}

struct SymbolMap {
    map: HashMap<&'static str, SymbolBox>,
}

impl SymbolMap {
    fn with_capacity(cap: usize) -> Self {
        Self {
            map: HashMap::with_capacity_and_hasher(cap, std::hash::BuildHasherDefault::default()),
        }
    }

    fn get(&self, name: &str) -> Option<&Symbol> {
        unsafe {
            self.map.get(name).map(|x| {
                let ptr: *const Symbol = x.as_ref();
                &*ptr
            })
        }
    }

    fn intern<'ob>(&mut self, name: &str, _cx: &'ob Context) -> &'ob Symbol {
        let sym = match self.map.get(name) {
            Some(x) => x.0,
            None => {
                let name = name.to_owned();
                // Leak the memory so that it is static
                let static_name: &'static str = unsafe {
                    let name_ptr: *const str = Box::into_raw(name.into_boxed_str());
                    &*name_ptr
                };
                let inner = Symbol::new(static_name, sym::RUNTIME_SYMBOL);
                let sym = SymbolBox::new(inner);
                let ptr: *const Symbol = sym.as_ref();
                self.map.insert(static_name, sym);
                ptr
            }
        };
        // SAFETY: We can guarantee that the reference is static because we have
        // no methods to remove items from SymbolMap and SymbolMap has a private
        // constructor, so the only one that exists is the one we create in this
        // module, which is static.
        unsafe { &*sym }
    }

    fn pre_init(&mut self, sym: &'static Symbol) {
        match self.map.get(sym.name()) {
            Some(_) => panic!("Attempt to intitalize {} twice", sym.name()),
            None => {
                self.map.insert(sym.name(), SymbolBox::from_static(sym));
            }
        }
    }
}

impl ObjectMap {
    pub(crate) fn intern<'ob>(&mut self, name: &str, cx: &'ob Context) -> &'ob Symbol {
        self.map.intern(name, cx)
    }

    pub(crate) fn set_func(&self, symbol: &Symbol, func: Gc<Function>) -> Result<()> {
        let new_func = func.clone_in(&self.block);
        self.block.uninterned_symbol_map.clear();
        #[cfg(miri)]
        new_func.get().set_as_miri_root();
        // SAFETY: The object is marked read-only, we have cloned in the
        // map's context, and it is not const, so calling this function
        // is safe.
        unsafe { symbol.set_func(new_func) }
    }

    pub(crate) fn get(&self, name: &str) -> Option<&Symbol> {
        self.map.get(name)
    }
}

// This is a workaround due to the limitations of consts in Rust. Our tagging
// scheme relies on shifting the bits in a pointer, but there is no way to do
// that in a const context. Therefore we can't tag values in const functions.
// However this presents a problem when we initalize a new symbol with #[defun].
// That macro will create a static symbol and initialize it's function cell with
// a SubrFn. However we can't tag that value due to limitations on const. So for
// now we use the function `new_with_subr` to intitalize the symbol with an
// UNTAGGED value. However it would be unsafe to dereference this value, so we
// need to make sure that `INTERNED_SYMBOLS` get's initialized before we deref
// any values from symbols. This will call `tag_subr`, which tags the value at
// runtime. The only place where this is a problem is in tests, where you could
// forget to initialize before trying to read a symbol function value. This
// sanity check makes sure we don't have any tests that Access values before
// `lazy_static::initialize` is called. If there is a better way to handle this,
// I would love to know.
#[cfg(test)]
static SYMBOLS_INIT: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

macro_rules! create_symbolmap {
    ($($($mod:ident)::*),*) => (
        #[allow(unused_imports)]
        #[allow(non_snake_case)]
        pub(crate) mod sym {
            use super::{Symbol, ConstSymbol};
            // This GlobalSymbol is assinged to every runtime symbol created.
            static __RUNTIME_SYMBOL_GLOBAL: Symbol = Symbol::new("_dummy_runtime_symbol", RUNTIME_SYMBOL);
            fn __RUNTIME_SYMBOL_FN () -> &'static Symbol {&__RUNTIME_SYMBOL_GLOBAL}
            pub(crate) const RUNTIME_SYMBOL: ConstSymbol = ConstSymbol::new(__RUNTIME_SYMBOL_FN);

            // Re-export all symbols in this module
            $(pub(crate) use $($mod::)*__symbol_bindings::*;)*
        }

        #[allow(unused_qualifications)]
        pub(crate) fn init_variables<'ob>(
            cx: &'ob crate::core::gc::Context,
            env: &mut crate::core::gc::Rt<crate::core::env::Env>,
        ) {
            $($($mod::)*__init_vars(cx, env);)*
        }

        lazy_static! {
            pub(crate) static ref INTERNED_SYMBOLS: Mutex<ObjectMap> = Mutex::new({
                let size: usize = 0_usize $(+ $($mod::)*__SYMBOLS.len())*;
                let mut map = SymbolMap::with_capacity(size);
                #[cfg(test)]
                SYMBOLS_INIT.store(true, std::sync::atomic::Ordering::Release);
                $(for sym in $($mod::)*__SYMBOLS.iter() {
                    #[allow(clippy::unnecessary_safety_comment)]
                    // SAFETY: We know that the function values are un-tagged,
                    // and that only subr's have been defined in the symbols, so
                    // it is safe to tag all function cell's as subr's
                    unsafe { sym.tag_subr(); }
                    map.pre_init(sym);
                })*;
                ObjectMap {
                    map,
                    block: Block::new_global(),
                }
            });
        }
    )
}

create_symbolmap!(
    // This module needs to be first because it defines byte-boolean-vars
    crate::lread,
    crate::arith,
    crate::bytecode,
    crate::interpreter,
    crate::fileio,
    crate::data,
    crate::print,
    crate::fns,
    crate::search,
    crate::eval,
    crate::alloc,
    crate::editfns,
    crate::character,
    crate::floatfns,
    crate::keymap,
    crate::emacs,
    crate::buffer
);

/// Intern a new symbol based on `name`
pub(crate) fn intern<'ob>(name: &str, cx: &'ob Context) -> &'ob Symbol {
    INTERNED_SYMBOLS.lock().unwrap().intern(name, cx)
}

#[cfg(test)]
mod test {
    use super::*;

    use super::super::gc::{Context, RootSet};
    use std::mem::size_of;

    #[test]
    fn size() {
        assert_eq!(size_of::<isize>(), size_of::<&Symbol>());
        assert_eq!(size_of::<isize>(), size_of::<Gc<Function>>());
    }

    unsafe fn fix_lifetime(inner: &Symbol) -> &'static Symbol {
        std::mem::transmute::<&Symbol, &'static Symbol>(inner)
    }

    #[test]
    fn init() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        intern("foo", cx);
    }

    #[test]
    fn symbol_func() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        lazy_static::initialize(&INTERNED_SYMBOLS);
        let inner = Symbol::new("foo", super::sym::RUNTIME_SYMBOL);
        let sym = unsafe { fix_lifetime(&inner) };
        assert_eq!("foo", sym.name());
        assert!(sym.func(cx).is_none());
        let func1 = cons!(1; cx);
        unsafe {
            sym.set_func(func1.try_into().unwrap()).unwrap();
        }
        let cell1 = sym.func(cx).unwrap();
        let Function::Cons(before) = cell1.untag() else {unreachable!("Type should be a lisp function")};
        assert_eq!(before.car(), 1);
        let func2 = cons!(2; cx);
        unsafe {
            sym.set_func(func2.try_into().unwrap()).unwrap();
        }
        let cell2 = sym.func(cx).unwrap();
        let Function::Cons(after) = cell2.untag() else {unreachable!("Type should be a lisp function")};
        assert_eq!(after.car(), 2);
        assert_eq!(before.car(), 1);
    }

    #[test]
    fn test_mutability() {
        let roots = &RootSet::default();
        let cx = &Context::new(roots);
        let cons = list!(1, 2, 3; cx);
        assert_eq!(cons, list!(1, 2, 3; cx));
        // is mutable
        if let crate::core::object::Object::Cons(cons) = cons.untag() {
            cons.set_car(4.into()).unwrap();
        } else {
            unreachable!();
        }
        assert_eq!(cons, list!(4, 2, 3; cx));
        let sym = intern("cons-test", cx);
        crate::data::fset(sym, cons).unwrap();
        // is not mutable
        if let Function::Cons(cons) = sym.func(cx).unwrap().untag() {
            assert!(cons.set_car(5.into()).is_err());
            let obj: GcObj = cons.into();
            assert_eq!(obj, list!(4, 2, 3; cx));
        } else {
            unreachable!();
        }
    }
}
