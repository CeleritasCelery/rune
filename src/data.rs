use std::cell::RefCell;
use std::sync::Mutex;

use crate::arena::{Arena, Gc, RootObj};
use crate::cons::Cons;
use crate::hashmap::{HashMap, HashSet};
use crate::object::{FuncCell, Object};
use crate::symbol::Symbol;
use crate::symbol::INTERNED_SYMBOLS;
use anyhow::{anyhow, Result};
use fn_macros::defun;
use lazy_static::lazy_static;

#[derive(Debug, Default, PartialEq)]
pub(crate) struct Environment {
    pub(crate) vars: HashMap<Symbol, RootObj>,
    pub(crate) props: HashMap<Symbol, Vec<(Symbol, RootObj)>>,
}

impl Environment {
    pub(crate) fn set_var(env: &mut Gc<Environment>, sym: Symbol, value: Object) {
        env.vars_mut().insert(sym, value);
    }

    pub(crate) fn set_prop(
        env: &mut Gc<Environment>,
        symbol: Symbol,
        propname: Symbol,
        value: Object,
    ) {
        let props = env.props_mut();
        match props.get_mut(&symbol) {
            Some(plist) => match plist.iter_mut().find(|x| x.0 == propname) {
                Some(x) => x.1.set(value),
                None => plist.push((propname, value)),
            },
            None => {
                props.insert(symbol, vec![(propname, value)]);
            }
        }
    }
}

lazy_static! {
    pub(crate) static ref FEATURES: Mutex<HashSet<Symbol>> = Mutex::new({
        HashSet::with_capacity_and_hasher(0, std::hash::BuildHasherDefault::default())
    });
}

fn set_global_function(symbol: Symbol, func: FuncCell) {
    let map = INTERNED_SYMBOLS.lock().unwrap();
    map.set_func(symbol, func);
}

#[defun]
pub(crate) fn fset(symbol: Symbol, definition: Object) -> Result<Symbol> {
    if definition == Object::NIL {
        symbol.unbind_func();
    } else {
        let func = definition.try_into()?;
        set_global_function(symbol, func);
    }
    Ok(symbol)
}

#[defun]
pub(crate) fn defalias(
    symbol: Symbol,
    definition: Object,
    _docstring: Option<&String>,
) -> Result<Symbol> {
    fset(symbol, definition)
}

#[defun]
pub(crate) fn set<'ob>(
    place: Symbol,
    newlet: Object<'ob>,
    env: &mut Gc<Environment>,
) -> Object<'ob> {
    Environment::set_var(env, place, newlet);
    newlet
}

#[defun]
pub(crate) fn put<'ob>(
    symbol: Symbol,
    propname: Symbol,
    value: Object<'ob>,
    env: &mut Gc<Environment>,
) -> Object<'ob> {
    Environment::set_prop(env, symbol, propname, value);
    value
}

#[defun]
pub(crate) fn get<'ob>(
    symbol: Symbol,
    propname: Symbol,
    env: &Gc<Environment>,
    arena: &'ob Arena,
) -> Object<'ob> {
    match env.props().get(&symbol) {
        Some(plist) => match plist.iter().find(|x| x.0 == propname) {
            Some(element) => arena.bind(element.1.obj()),
            None => Object::NIL,
        },
        None => Object::NIL,
    }
}

#[defun]
pub(crate) fn eq(obj1: Object, obj2: Object) -> bool {
    obj1.ptr_eq(obj2)
}

#[defun]
pub(crate) fn equal<'ob>(obj1: Object<'ob>, obj2: Object<'ob>) -> bool {
    obj1 == obj2
}

#[defun]
pub(crate) fn symbol_function<'ob>(symbol: Symbol, gc: &'ob Arena) -> Object<'ob> {
    match symbol.func(gc) {
        Some(f) => f.into(),
        None => Object::NIL,
    }
}

#[defun]
pub(crate) fn symbol_value<'ob>(
    symbol: Symbol,
    env: &mut Gc<Environment>,
    arena: &'ob Arena,
) -> Option<Object<'ob>> {
    env.vars().get(&symbol).map(|x| arena.bind(x.obj()))
}

#[defun]
pub(crate) fn symbol_name(symbol: Symbol) -> &'static str {
    symbol.name
}

#[defun]
pub(crate) fn null(obj: Object) -> bool {
    matches!(obj, Object::Nil(_))
}

#[defun]
pub(crate) fn fboundp(symbol: Symbol) -> bool {
    symbol.has_func()
}

#[defun]
pub(crate) fn fmakunbound(symbol: Symbol) -> Symbol {
    symbol.unbind_func();
    symbol
}

#[defun]
pub(crate) fn boundp(symbol: Symbol, env: &mut Gc<Environment>) -> bool {
    env.vars().get(&symbol).is_some()
}

#[defun]
pub(crate) fn default_boundp(symbol: Symbol, env: &mut Gc<Environment>) -> bool {
    env.vars().get(&symbol).is_some()
}

#[defun]
pub(crate) fn listp(object: Object) -> bool {
    matches!(object, Object::Nil(_) | Object::Cons(_))
}

#[defun]
pub(crate) fn nlistp(object: Object) -> bool {
    !listp(object)
}

#[defun]
pub(crate) fn symbolp(object: Object) -> bool {
    matches!(object, Object::Symbol(_))
}

#[defun]
pub(crate) fn functionp(object: Object) -> bool {
    matches!(object, Object::LispFn(_) | Object::SubrFn(_))
}

#[defun]
pub(crate) fn stringp(object: Object) -> bool {
    matches!(object, Object::String(_))
}

#[defun]
pub(crate) fn numberp(object: Object) -> bool {
    matches!(object, Object::Int(_) | Object::Float(_))
}

#[defun]
pub(crate) fn vectorp(object: Object) -> bool {
    matches!(object, Object::Vec(_))
}

#[defun]
pub(crate) fn consp(object: Object) -> bool {
    matches!(object, Object::Cons(_))
}

#[defun]
pub(crate) fn atom(object: Object) -> bool {
    !consp(object)
}

#[defun]
pub(crate) fn defvar<'ob>(
    symbol: Symbol,
    initvalue: Option<Object<'ob>>,
    _docstring: Option<&String>,
    env: &mut Gc<Environment>,
) -> Object<'ob> {
    let value = initvalue.unwrap_or_default();
    set(symbol, value, env)
}

#[defun]
pub(crate) fn make_variable_buffer_local(variable: Symbol) -> Symbol {
    // TODO: Implement
    variable
}

#[defun]
pub(crate) fn aset<'ob>(
    array: &RefCell<Vec<Object<'ob>>>,
    idx: usize,
    newlet: Object<'ob>,
) -> Result<Object<'ob>> {
    let mut vec = array.try_borrow_mut()?;
    if idx < vec.len() {
        vec[idx] = newlet;
        Ok(newlet)
    } else {
        let len = vec.len();
        Err(anyhow!("index {idx} is out of bounds. Length was {len}"))
    }
}

#[defun]
pub(crate) fn indirect_function<'ob>(object: Object<'ob>, gc: &'ob Arena) -> Object<'ob> {
    match object {
        Object::Symbol(sym) => match sym.resolve_callable(gc) {
            Some(func) => func.into(),
            None => Object::NIL,
        },
        x => x,
    }
}

#[defun]
pub(crate) fn provide(feature: Symbol, _subfeatures: Option<&Cons>) -> Symbol {
    FEATURES.lock().unwrap().insert(feature);
    feature
}

defsubr!(
    eq,
    equal,
    set,
    put,
    get,
    defvar,
    make_variable_buffer_local,
    fset,
    aset,
    defalias,
    provide,
    symbol_function,
    symbol_value,
    symbol_name,
    null,
    fmakunbound,
    fboundp,
    boundp,
    default_boundp,
    listp,
    nlistp,
    stringp,
    symbolp,
    functionp,
    vectorp,
    numberp,
    consp,
    atom,
    indirect_function,
);
