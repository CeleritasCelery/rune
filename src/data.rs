use crate::cons::Cons;
use crate::hashmap::HashMap;
use crate::object::{FuncCell, Object};
use crate::symbol::Symbol;
use crate::symbol::INTERNED_SYMBOLS;
use anyhow::{anyhow, Result};
use fn_macros::defun;

#[derive(Debug, Default, PartialEq)]
pub(crate) struct Environment<'ob> {
    pub(crate) vars: HashMap<Symbol, Object<'ob>>,
    props: HashMap<Symbol, Vec<(Symbol, Object<'ob>)>>,
    pub(crate) macro_callstack: Vec<Symbol>,
}

pub(crate) fn set_global_function(symbol: Symbol, func: FuncCell) {
    let map = INTERNED_SYMBOLS.lock().unwrap();
    map.set_func(symbol, func);
}

#[defun]
pub(crate) fn fset(symbol: Symbol, definition: Object) -> Result<Symbol> {
    if matches!(definition, Object::Nil(_)) {
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
pub(crate) fn symbol_function<'ob>(symbol: Symbol) -> Object<'ob> {
    match symbol.func() {
        Some(f) => f.into(),
        None => Object::NIL,
    }
}

#[defun]
pub(crate) fn symbol_value<'ob>(symbol: Symbol, env: &mut Environment<'ob>) -> Option<Object<'ob>> {
    env.vars.get(&symbol).copied()
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
pub(crate) fn boundp(symbol: Symbol, env: &mut Environment) -> bool {
    env.vars.get(&symbol).is_some()
}

#[defun]
pub(crate) fn default_boundp(symbol: Symbol, env: &mut Environment) -> bool {
    env.vars.get(&symbol).is_some()
}

#[defun]
pub(crate) fn listp(object: Object) -> bool {
    matches!(object, Object::Nil(_) | Object::Cons(_))
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
    env: &mut Environment<'ob>,
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
    array: &mut Vec<Object<'ob>>,
    idx: usize,
    newlet: Object<'ob>,
) -> Result<Object<'ob>> {
    if idx < array.len() {
        array[idx] = newlet;
        Ok(newlet)
    } else {
        Err(anyhow!(
            "index {} is out of bounds. Length was {}",
            idx,
            array.len()
        ))
    }
}

#[defun]
pub(crate) fn indirect_function(object: Object) -> Object {
    match object {
        Object::Symbol(sym) => match sym.resolved_func() {
            Some(func) => func.into(),
            None => Object::NIL,
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
    stringp,
    symbolp,
    functionp,
    vectorp,
    numberp,
    consp,
    atom,
    indirect_function,
);
