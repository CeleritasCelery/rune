use crate::arena::Arena;
use crate::hashmap::HashMap;
use crate::object::{Callable, List, Object};
use crate::symbol::Symbol;
use crate::symbol::INTERNED_SYMBOLS;
use fn_macros::defun;
use std::convert::TryInto;

#[derive(Debug, Default, PartialEq)]
pub(crate) struct Environment<'ob> {
    pub(crate) vars: HashMap<Symbol, Object<'ob>>,
    pub(crate) funcs: HashMap<Symbol, Object<'ob>>,
    props: HashMap<Symbol, Vec<(Symbol, Object<'ob>)>>,
    pub(crate) macro_callstack: Vec<Symbol>,
}

pub(crate) fn set_global_function<'ob>(
    symbol: Symbol,
    func: Callable<'ob>,
    env: &mut Environment<'ob>,
) {
    let map = INTERNED_SYMBOLS.lock().unwrap();
    env.funcs.remove(&symbol);
    map.set_func(symbol, func);
}

#[defun]
pub(crate) fn defalias<'ob>(
    symbol: Symbol,
    definition: Object<'ob>,
    env: &mut Environment<'ob>,
) -> Symbol {
    match definition.try_into() {
        Ok(func) => {
            set_global_function(symbol, func, env);
        }
        Err(_) => {
            symbol.unbind_func();
            env.funcs.insert(symbol, definition);
        }
    };
    symbol
}

#[defun]
pub(crate) fn set<'ob>(
    place: Symbol,
    newlet: Object<'ob>,
    env: &mut Environment<'ob>,
) -> Object<'ob> {
    env.vars.insert(place, newlet);
    newlet
}

#[defun]
pub(crate) fn put<'ob>(
    symbol: Symbol,
    propname: Symbol,
    value: Object<'ob>,
    env: &mut Environment<'ob>,
) -> Object<'ob> {
    match env.props.get_mut(&symbol) {
        Some(plist) => match plist.iter_mut().find(|x| x.0 == propname) {
            Some(x) => x.1 = value,
            None => plist.push((propname, value)),
        },
        None => {
            let plist = vec![(propname, value)];
            env.props.insert(symbol, plist);
        }
    }
    value
}

#[defun]
pub(crate) fn get<'ob>(symbol: Symbol, propname: Symbol, env: &Environment<'ob>) -> Object<'ob> {
    match env.props.get(&symbol) {
        Some(plist) => match plist.iter().find(|x| x.0 == propname) {
            Some((_, val)) => *val,
            None => Object::Nil,
        },
        None => Object::Nil,
    }
}

#[defun]
pub(crate) fn eq(obj1: Object, obj2: Object) -> bool {
    obj1.ptr_eq(obj2)
}

#[defun]
pub(crate) fn symbol_function<'ob>(
    symbol: Symbol,
    env: &mut Environment<'ob>,
    arena: &'ob Arena,
) -> Object<'ob> {
    symbol.get_func(arena).map_or_else(
        || *env.funcs.get(&symbol).unwrap_or(&Object::Nil),
        |func| func.into(),
    )
}

#[defun]
pub(crate) fn defvar<'ob>(
    symbol: Symbol,
    initvalue: Option<Object<'ob>>,
    _docstring: Option<&String>,
    env: &mut Environment<'ob>,
) -> Object<'ob> {
    let value = initvalue.unwrap_or(Object::Nil);
    set(symbol, value, env)
}

#[defun]
pub(crate) const fn provide(feature: Symbol, _subfeatures: Option<List>) -> Symbol {
    // TODO: implement
    feature
}

#[defun]
pub(crate) fn stringp(object: Object) -> bool {
    matches!(object, Object::String(_))
}

defsubr!(
    eq,
    set,
    put,
    get,
    defvar,
    defalias,
    provide,
    symbol_function,
    stringp,
);
