use crate::cons::Cons;
use crate::hashmap::HashMap;
use crate::object::{FuncCell, Object};
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
    func: FuncCell<'ob>,
    env: &mut Environment<'ob>,
) {
    let map = INTERNED_SYMBOLS.lock().unwrap();
    env.funcs.remove(&symbol);
    map.set_func(symbol, func);
}

#[defun]
pub(crate) fn fset<'ob>(
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
pub(crate) fn defalias<'ob>(
    symbol: Symbol,
    definition: Object<'ob>,
    _docstring: Option<&String>,
    env: &mut Environment<'ob>,
) -> Symbol {
    fset(symbol, definition, env)
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
pub(crate) fn symbol_function<'ob>(symbol: Symbol, env: &mut Environment<'ob>) -> Object<'ob> {
    symbol.func().map_or_else(
        || *env.funcs.get(&symbol).unwrap_or(&Object::Nil),
        |func| func.into(),
    )
}

#[defun]
pub(crate) fn symbol_name(symbol: Symbol) -> &'static str {
    symbol.name()
}

#[defun]
pub(crate) fn null(obj: Object) -> bool {
    matches!(obj, Object::Nil)
}

#[defun]
pub(crate) fn fboundp(symbol: Symbol) -> bool {
    symbol.has_func()
}

#[defun]
pub(crate) fn listp(object: Object) -> bool {
    matches!(object, Object::Nil | Object::Cons(_))
}

#[defun]
pub(crate) fn symbolp(object: Object) -> bool {
    matches!(object, Object::Symbol(_))
}

#[defun]
pub(crate) fn stringp(object: Object) -> bool {
    matches!(object, Object::String(_))
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
    env: &mut Environment<'ob>,
) -> Object<'ob> {
    let value = initvalue.unwrap_or_default();
    set(symbol, value, env)
}

#[defun]
pub(crate) fn indirect_function(object: Object) -> Object {
    match object {
        Object::Symbol(sym) => match (!sym).resolved_func() {
            Some(func) => func.into(),
            None => Object::Nil,
        },
        x => x,
    }
}

#[defun]
pub(crate) const fn provide(feature: Symbol, _subfeatures: Option<&Cons>) -> Symbol {
    // TODO: implement
    feature
}

defsubr!(
    eq,
    set,
    put,
    get,
    defvar,
    fset,
    defalias,
    provide,
    symbol_function,
    symbol_name,
    null,
    fboundp,
    listp,
    stringp,
    symbolp,
    vectorp,
    consp,
    atom,
    indirect_function,
);
