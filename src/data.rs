use crate::arena::Arena;
use crate::hashmap::HashMap;
use crate::object::{Object, Symbol};
use fn_macros::lisp_fn;

#[derive(Debug, Default)]
pub struct Environment<'ob> {
    pub vars: HashMap<Symbol, Object<'ob>>,
    props: HashMap<Symbol, Vec<(Symbol, Object<'ob>)>>,
}

#[lisp_fn]
pub fn set<'obj>(
    place: Symbol,
    newlet: Object<'obj>,
    arena: &Arena,
    env: &mut Environment,
) -> Object<'obj> {
    let new = newlet.clone_in(arena);
    env.vars.insert(place, unsafe { new.into_gc() });
    newlet
}

#[lisp_fn]
pub fn put<'obj>(
    symbol: Symbol,
    propname: Symbol,
    value: Object<'obj>,
    env: &mut Environment<'obj>,
) -> Object<'obj> {
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

#[lisp_fn]
pub fn get<'obj>(symbol: Symbol, propname: Symbol, env: &Environment<'obj>) -> Object<'obj> {
    match env.props.get(&symbol) {
        Some(plist) => match plist.iter().find(|x| x.0 == propname) {
            Some((_, val)) => *val,
            None => Object::nil(),
        },
        None => Object::nil(),
    }
}

defsubr!(set, put, get);
