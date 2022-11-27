use std::sync::Mutex;

use crate::core::{
    cons::Cons,
    env::{sym, Env, Symbol, INTERNED_SYMBOLS},
    error::{Type, TypeError},
    gc::{Context, IntoRoot, Root},
    object::{nil, Gc, GcObj, Number, Object, SubrFn},
};
use crate::hashmap::HashSet;
use anyhow::{anyhow, Result};
use fn_macros::defun;
use lazy_static::lazy_static;

lazy_static! {
    pub(crate) static ref FEATURES: Mutex<HashSet<&'static Symbol>> = Mutex::new({
        HashSet::with_capacity_and_hasher(0, std::hash::BuildHasherDefault::default())
    });
}

#[defun]
pub(crate) fn fset<'ob>(symbol: &'ob Symbol, definition: GcObj) -> Result<&'ob Symbol> {
    if definition.nil() {
        symbol.unbind_func();
    } else {
        let func = definition.try_into()?;
        let map = INTERNED_SYMBOLS.lock().unwrap();
        map.set_func(symbol, func)?;
    }
    Ok(symbol)
}

#[defun]
pub(crate) fn defalias<'ob>(
    symbol: &'ob Symbol,
    definition: GcObj,
    _docstring: Option<&str>,
) -> Result<&'ob Symbol> {
    fset(symbol, definition)
}

#[defun]
pub(crate) fn set<'ob>(
    place: &Symbol,
    newlet: GcObj<'ob>,
    env: &mut Root<Env>,
    cx: &Context,
) -> Result<GcObj<'ob>> {
    env.as_mut(cx).set_var(place, newlet)?;
    Ok(newlet)
}

#[defun]
pub(crate) fn put<'ob>(
    symbol: &Symbol,
    propname: &Symbol,
    value: GcObj<'ob>,
    env: &mut Root<Env>,
    cx: &Context,
) -> GcObj<'ob> {
    env.as_mut(cx).set_prop(symbol, propname, value);
    value
}

#[defun]
pub(crate) fn get<'ob>(
    symbol: &Symbol,
    propname: &Symbol,
    env: &Root<Env>,
    cx: &'ob Context,
) -> GcObj<'ob> {
    match env.props.get(symbol) {
        Some(plist) => match plist.iter().find(|x| x.0 == propname) {
            Some(element) => cx.bind(element.1.bind(cx)),
            None => nil(),
        },
        None => nil(),
    }
}

#[defun]
pub(crate) fn symbol_function<'ob>(symbol: &Symbol, cx: &'ob Context) -> GcObj<'ob> {
    match symbol.func(cx) {
        Some(f) => f.into(),
        None => nil(),
    }
}

#[defun]
pub(crate) fn symbol_value<'ob>(
    symbol: &Symbol,
    env: &Root<Env>,
    cx: &'ob Context,
) -> Option<GcObj<'ob>> {
    env.vars.get(symbol).map(|x| x.bind(cx))
}

#[defun]
pub(crate) fn symbol_name(symbol: &Symbol) -> &str {
    symbol.name()
}

#[defun]
pub(crate) fn null(obj: GcObj) -> bool {
    obj.nil()
}

#[defun]
pub(crate) fn fboundp(symbol: &Symbol) -> bool {
    symbol.has_func()
}

#[defun]
pub(crate) fn fmakunbound(symbol: &Symbol) -> &Symbol {
    symbol.unbind_func();
    symbol
}

#[defun]
pub(crate) fn boundp(symbol: &Symbol, env: &Root<Env>) -> bool {
    env.vars.get(symbol).is_some()
}

#[defun]
pub(crate) fn makunbound<'ob>(
    symbol: &'ob Symbol,
    env: &mut Root<Env>,
    cx: &'ob Context,
) -> &'ob Symbol {
    env.as_mut(cx).vars.remove(symbol);
    symbol
}

#[defun]
pub(crate) fn default_boundp(symbol: &Symbol, env: &Root<Env>) -> bool {
    env.vars.get(symbol).is_some()
}

#[defun]
pub(crate) fn listp(object: GcObj) -> bool {
    match object.get() {
        Object::Symbol(s) if s.nil() => true,
        Object::Cons(_) => true,
        _ => false,
    }
}

#[defun]
pub(crate) fn nlistp(object: GcObj) -> bool {
    !listp(object)
}

#[defun]
pub(crate) fn symbolp(object: GcObj) -> bool {
    matches!(object.get(), Object::Symbol(_))
}

#[defun]
pub(crate) fn functionp(object: GcObj) -> bool {
    match object.get() {
        Object::ByteFn(_) | Object::SubrFn(_) => true,
        Object::Cons(cons) => cons.car() == sym::CLOSURE,
        Object::Symbol(sym) => sym.has_func(),
        _ => false,
    }
}

#[defun]
pub(crate) fn subrp(object: GcObj) -> bool {
    matches!(object.get(), Object::SubrFn(_))
}

#[defun]
pub(crate) fn stringp(object: GcObj) -> bool {
    matches!(object.get(), Object::String(_))
}

#[defun]
pub(crate) fn numberp(object: GcObj) -> bool {
    matches!(object.get(), Object::Int(_) | Object::Float(_))
}

#[defun]
pub(crate) fn markerp(_: GcObj) -> bool {
    // TODO: implement
    false
}

#[defun]
pub(crate) fn vectorp(object: GcObj) -> bool {
    matches!(object.get(), Object::Vec(_))
}

#[defun]
pub(crate) fn recordp(object: GcObj) -> bool {
    matches!(object.get(), Object::Record(_))
}

#[defun]
pub(crate) fn consp(object: GcObj) -> bool {
    matches!(object.get(), Object::Cons(_))
}

#[defun]
pub(crate) fn keywordp(object: GcObj) -> bool {
    match object.get() {
        Object::Symbol(s) => s.name().starts_with(':'),
        _ => false,
    }
}

#[defun]
pub(crate) fn integerp(object: GcObj) -> bool {
    matches!(object.get(), Object::Int(_))
}

#[defun]
pub(crate) fn floatp(object: GcObj) -> bool {
    matches!(object.get(), Object::Float(_))
}

#[defun]
pub(crate) fn atom(object: GcObj) -> bool {
    !consp(object)
}

#[defun]
fn byte_code_function_p(object: GcObj) -> bool {
    matches!(object.get(), Object::ByteFn(_))
}

#[defun]
fn bufferp(_object: GcObj) -> bool {
    // TODO: Implement once buffers are added
    false
}

#[defun]
fn string_to_number<'ob>(string: &str, base: Option<i64>, cx: &'ob Context) -> Gc<Number<'ob>> {
    // TODO: Handle trailing characters, which should be ignored
    let base = base.unwrap_or(10);
    let string = string.trim();
    match i64::from_str_radix(string, base as u32) {
        Ok(x) => x.into(),
        Err(_) => match string.parse::<f64>() {
            Ok(x) => cx.add(x),
            Err(_) => 0.into(),
        },
    }
}

#[defun]
pub(crate) fn defvar<'ob>(
    symbol: &Symbol,
    initvalue: Option<GcObj<'ob>>,
    _docstring: Option<&str>,
    env: &mut Root<Env>,
    cx: &Context,
) -> Result<GcObj<'ob>> {
    let value = initvalue.unwrap_or_default();
    set(symbol, value, env, cx)
}

#[defun]
pub(crate) fn make_variable_buffer_local(variable: &Symbol) -> &Symbol {
    // TODO: Implement
    variable
}

#[defun]
fn subr_arity<'ob>(subr: &SubrFn, cx: &'ob Context) -> GcObj<'ob> {
    let min = subr.args.required as usize;
    let max: GcObj = {
        if subr.args.rest {
            sym::MANY.into()
        } else {
            (min + subr.args.optional as usize).into()
        }
    };
    cons!(min, max; cx)
}

#[defun]
fn ash(value: i64, count: i64) -> i64 {
    let shift = if count >= 0 {
        std::ops::Shl::shl
    } else {
        std::ops::Shr::shr
    };
    let result = shift(value.abs(), count.abs());
    if value >= 0 {
        result
    } else {
        -result
    }
}

#[defun]
pub(crate) fn aset<'ob>(array: GcObj<'ob>, idx: usize, newlet: GcObj<'ob>) -> Result<GcObj<'ob>> {
    match array.get() {
        Object::Vec(vec) => {
            let vec = vec.try_mut()?;
            if idx < vec.len() {
                vec[idx].set(newlet);
                Ok(newlet)
            } else {
                let len = vec.len();
                Err(anyhow!("index {idx} is out of bounds. Length was {len}"))
            }
        }
        Object::Record(vec) => {
            let vec = vec.try_mut()?;
            if idx < vec.len() {
                vec[idx].set(newlet);
                Ok(newlet)
            } else {
                let len = vec.len();
                Err(anyhow!("index {idx} is out of bounds. Length was {len}"))
            }
        }
        x => Err(TypeError::new(Type::Sequence, x).into()),
    }
}

#[defun]
pub(crate) fn aref(array: GcObj, idx: usize) -> Result<GcObj> {
    match array.get() {
        Object::Vec(vec) => match vec.get(idx) {
            Some(x) => Ok(x.get()),
            None => {
                let len = vec.len();
                Err(anyhow!("index {idx} is out of bounds. Length was {len}"))
            }
        },
        Object::Record(vec) => match vec.get(idx) {
            Some(x) => Ok(x.get()),
            None => {
                let len = vec.len();
                Err(anyhow!("index {idx} is out of bounds. Length was {len}"))
            }
        },
        Object::String(string) => match string.get_char_at(idx) {
            Some(x) => Ok((x as i64).into()),
            None => {
                let len = string.len();
                Err(anyhow!("index {idx} is out of bounds. Length was {len}"))
            }
        },
        Object::ByteFn(fun) => match fun.index(idx) {
            Some(x) => Ok(x),
            None => Err(anyhow!("index {idx} is out of bounds")),
        },
        x => Err(TypeError::new(Type::Sequence, x).into()),
    }
}

#[defun]
fn type_of(object: GcObj) -> GcObj {
    match object.get() {
        Object::Int(_) => sym::INTEGER.into(),
        Object::Float(_) => sym::FLOAT.into(),
        Object::Symbol(_) => sym::SYMBOL.into(),
        Object::Cons(_) => sym::CONS.into(),
        Object::Vec(_) => sym::VECTOR.into(),
        Object::Record(x) => x.get(0).expect("record was missing type").get(),
        Object::ByteFn(_) => sym::COMPILED_FUNCTION.into(),
        Object::HashTable(_) => sym::HASH_TABLE.into(),
        Object::String(_) => sym::STRING.into(),
        Object::SubrFn(_) => sym::SUBR.into(),
    }
}

#[defun]
pub(crate) fn indirect_function<'ob>(object: GcObj<'ob>, cx: &'ob Context) -> GcObj<'ob> {
    match object.get() {
        Object::Symbol(sym) => match sym.follow_indirect(cx) {
            Some(func) => func.into(),
            None => nil(),
        },
        _ => object,
    }
}

#[defun]
pub(crate) fn provide<'ob>(feature: &'ob Symbol, _subfeatures: Option<&Cons>) -> &'ob Symbol {
    let mut features = FEATURES.lock().unwrap();
    // TODO: SYMBOL - need to trace this
    let feat = unsafe { feature.into_root() };
    features.insert(feat);
    feature
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_ash() {
        assert_eq!(ash(4, 1), 8);
        assert_eq!(ash(4, -1), 2);
        assert_eq!(ash(-8, -1), -4);
        assert_eq!(ash(256, -8), 1);
        assert_eq!(ash(-8, 1), -16);
    }
}

define_symbols!(
    FUNCS => {
        set,
        put,
        get,
        defvar,
        make_variable_buffer_local,
        subr_arity,
        ash,
        fset,
        aset,
        aref,
        defalias,
        provide,
        symbol_function,
        symbol_value,
        symbol_name,
        null,
        fmakunbound,
        fboundp,
        boundp,
        makunbound,
        default_boundp,
        listp,
        nlistp,
        stringp,
        symbolp,
        functionp,
        subrp,
        vectorp,
        recordp,
        numberp,
        markerp,
        consp,
        keywordp,
        integerp,
        floatp,
        atom,
        type_of,
        byte_code_function_p,
        bufferp,
        string_to_number,
        indirect_function,
    }
    SYMS => {
        MANY,
        INTEGER,
        SYMBOL,
        COMPILED_FUNCTION,
        HASH_TABLE,
        STRING,
        SUBR,
    }
);
