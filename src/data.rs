use crate::cons::Cons;
use crate::hashmap::HashMap;
use crate::object::{List, Object, NIL};
use crate::symbol::Symbol;
use fn_macros::defun;

#[derive(Debug, Default)]
pub(crate) struct Environment<'ob> {
    pub(crate) vars: HashMap<Symbol, Object<'ob>>,
    pub(crate) funcs: HashMap<Symbol, &'ob Cons<'ob>>,
    props: HashMap<Symbol, Vec<(Symbol, Object<'ob>)>>,
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
            None => NIL,
        },
        None => NIL,
    }
}

#[defun]
pub(crate) fn defvar<'ob>(
    symbol: Symbol,
    initvalue: Option<Object<'ob>>,
    _docstring: Option<&String>,
    env: &mut Environment<'ob>,
) -> Object<'ob> {
    let value = initvalue.unwrap_or(NIL);
    set(symbol, value, env)
}

#[defun]
pub(crate) const fn provide(feature: Symbol, _subfeatures: Option<List>) -> Symbol {
    // TODO: implement
    feature
}

defsubr!(set, put, get, defvar, provide);
