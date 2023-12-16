use crate::core::{
    cons::Cons,
    env::{interned_symbols, sym, Env, Symbol},
    error::{Type, TypeError},
    gc::{Context, IntoRoot, Rt},
    object::{nil, Gc, GcObj, List, Number, Object, SubrFn},
};
use crate::hashmap::HashSet;
use anyhow::{anyhow, Result};
use rune_macros::defun;
use std::sync::Mutex;
use std::sync::OnceLock;

static FEATURES: OnceLock<Mutex<HashSet<Symbol<'static>>>> = OnceLock::new();

/// Rust translation of the `features` variable: A list of symbols are the features
/// of the executing Emacs. Used by [`featurep`](`crate::fns::featurep`) and [`require`](`crate::fns::require`),
/// altered by [`provide`]. Vended through a helper function to avoid calling `get_or_init` on each of the calls
/// to `lock()` on the Mutex.
///
/// TODO: Use `LazyLock`: <https://github.com/CeleritasCelery/rune/issues/34>
pub(crate) fn features() -> &'static Mutex<HashSet<Symbol<'static>>> {
    FEATURES.get_or_init(Mutex::default)
}

#[defun]
pub(crate) fn fset<'ob>(symbol: Symbol<'ob>, definition: GcObj) -> Result<Symbol<'ob>> {
    if definition.is_nil() {
        symbol.unbind_func();
    } else {
        let func = definition.try_into()?;
        let map = interned_symbols().lock().unwrap();
        map.set_func(symbol, func)?;
    }
    Ok(symbol)
}

#[defun]
pub(crate) fn defalias<'ob>(
    symbol: Symbol<'ob>,
    definition: GcObj,
    _docstring: Option<&str>,
) -> Result<Symbol<'ob>> {
    fset(symbol, definition)
}

#[defun]
pub(crate) fn set<'ob>(place: Symbol, newlet: GcObj<'ob>, env: &mut Rt<Env>) -> Result<GcObj<'ob>> {
    env.set_var(place, newlet)?;
    Ok(newlet)
}

#[defun]
pub(crate) fn put<'ob>(
    symbol: Symbol,
    propname: Symbol,
    value: GcObj<'ob>,
    env: &mut Rt<Env>,
) -> GcObj<'ob> {
    env.set_prop(symbol, propname, value);
    value
}

#[defun]
pub(crate) fn get<'ob>(
    symbol: Symbol,
    propname: Symbol,
    env: &Rt<Env>,
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
pub(crate) fn local_variable_if_set_p(_sym: Symbol) -> bool {
    // TODO: Implement buffer locals
    false
}

#[defun]
pub(crate) fn default_value<'ob>(
    symbol: Symbol,
    env: &Rt<Env>,
    cx: &'ob Context,
) -> Result<GcObj<'ob>> {
    // TODO: Implement buffer locals
    symbol_value(symbol, env, cx).ok_or_else(|| anyhow!("Void variable: {symbol}"))
}

#[defun]
pub(crate) fn symbol_function<'ob>(symbol: Symbol, cx: &'ob Context) -> GcObj<'ob> {
    match symbol.func(cx) {
        Some(f) => f.into(),
        None => nil(),
    }
}

#[defun]
pub(crate) fn symbol_value<'ob>(
    symbol: Symbol,
    env: &Rt<Env>,
    cx: &'ob Context,
) -> Option<GcObj<'ob>> {
    env.vars.get(symbol).map(|x| x.bind(cx))
}

#[defun]
pub(crate) fn symbol_name(symbol: Symbol<'_>) -> &str {
    symbol.get().name()
}

#[defun]
pub(crate) fn null(obj: GcObj) -> bool {
    obj.is_nil()
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
pub(crate) fn boundp(symbol: Symbol, env: &Rt<Env>) -> bool {
    env.vars.get(symbol).is_some()
}

#[defun]
pub(crate) fn makunbound<'ob>(symbol: Symbol<'ob>, env: &mut Rt<Env>) -> Symbol<'ob> {
    env.vars.remove(symbol);
    symbol
}

#[defun]
pub(crate) fn default_boundp(symbol: Symbol, env: &Rt<Env>) -> bool {
    env.vars.get(symbol).is_some()
}

#[defun]
pub(crate) fn listp(object: GcObj) -> bool {
    matches!(object.untag(), Object::NIL | Object::Cons(_))
}

#[defun]
pub(crate) fn nlistp(object: GcObj) -> bool {
    !listp(object)
}

#[defun]
pub(crate) fn symbolp(object: GcObj) -> bool {
    matches!(object.untag(), Object::Symbol(_))
}

#[defun]
pub(crate) fn functionp(object: GcObj) -> bool {
    match object.untag() {
        Object::ByteFn(_) | Object::SubrFn(_) => true,
        Object::Cons(cons) => cons.car() == sym::CLOSURE,
        Object::Symbol(sym) => sym.has_func(),
        _ => false,
    }
}

#[defun]
pub(crate) fn subrp(object: GcObj) -> bool {
    matches!(object.untag(), Object::SubrFn(_))
}

#[defun]
pub(crate) fn stringp(object: GcObj) -> bool {
    matches!(object.untag(), Object::String(_))
}

#[defun]
pub(crate) fn numberp(object: GcObj) -> bool {
    matches!(object.untag(), Object::Int(_) | Object::Float(_))
}

#[defun]
pub(crate) fn markerp(_: GcObj) -> bool {
    // TODO: implement
    false
}

#[defun]
pub(crate) fn vectorp(object: GcObj) -> bool {
    matches!(object.untag(), Object::Vec(_))
}

#[defun]
pub(crate) fn recordp(object: GcObj) -> bool {
    matches!(object.untag(), Object::Record(_))
}

#[defun]
pub(crate) fn consp(object: GcObj) -> bool {
    matches!(object.untag(), Object::Cons(_))
}

#[defun]
pub(crate) fn keywordp(object: GcObj) -> bool {
    match object.untag() {
        Object::Symbol(s) => s.name().starts_with(':'),
        _ => false,
    }
}

#[defun]
pub(crate) fn integerp(object: GcObj) -> bool {
    matches!(object.untag(), Object::Int(_))
}

#[defun]
pub(crate) fn floatp(object: GcObj) -> bool {
    matches!(object.untag(), Object::Float(_))
}

#[defun]
pub(crate) fn atom(object: GcObj) -> bool {
    !consp(object)
}

#[defun]
fn byte_code_function_p(object: GcObj) -> bool {
    matches!(object.untag(), Object::ByteFn(_))
}

#[defun]
fn subr_native_elisp_p(_: GcObj) -> bool {
    false
}

#[defun]
fn bufferp(_object: GcObj) -> bool {
    // TODO: Implement once buffers are added
    false
}

#[defun]
pub(crate) fn multibyte_string_p(object: GcObj) -> bool {
    match object.untag() {
        Object::String(string) => string.is_valid_unicode(),
        _ => false,
    }
}

#[defun]
fn string_to_number<'ob>(string: &str, base: Option<i64>, cx: &'ob Context) -> Gc<Number<'ob>> {
    // TODO: Handle trailing characters, which should be ignored
    let base = base.unwrap_or(10);
    let string = string.trim();
    match i64::from_str_radix(string, base as u32) {
        Ok(x) => x.into(),
        Err(_) => match string.parse::<f64>() {
            Ok(x) => cx.add_as(x),
            Err(_) => 0.into(),
        },
    }
}

#[defun]
pub(crate) fn defvar<'ob>(
    symbol: Symbol,
    initvalue: Option<GcObj<'ob>>,
    _docstring: Option<&str>,
    env: &mut Rt<Env>,
) -> Result<GcObj<'ob>> {
    let value = initvalue.unwrap_or_default();
    set(symbol, value, env)
}

#[defun]
pub(crate) fn make_variable_buffer_local(variable: Symbol) -> Symbol {
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
    let shift = if count >= 0 { std::ops::Shl::shl } else { std::ops::Shr::shr };
    let result = shift(value.abs(), count.abs());
    if value >= 0 {
        result
    } else {
        -result
    }
}

#[defun]
pub(crate) fn aset<'ob>(array: GcObj<'ob>, idx: usize, newlet: GcObj<'ob>) -> Result<GcObj<'ob>> {
    match array.untag() {
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
    match array.untag() {
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
            Some(x) => Ok((i64::from(x)).into()),
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
    match object.untag() {
        Object::Int(_) => sym::INTEGER.into(),
        Object::Float(_) => sym::FLOAT.into(),
        Object::Symbol(_) => sym::SYMBOL.into(),
        Object::Cons(_) => sym::CONS.into(),
        Object::Vec(_) => sym::VECTOR.into(),
        Object::Record(x) => x.first().expect("record was missing type").get(),
        Object::ByteFn(_) => sym::COMPILED_FUNCTION.into(),
        Object::HashTable(_) => sym::HASH_TABLE.into(),
        Object::String(_) => sym::STRING.into(),
        Object::SubrFn(_) => sym::SUBR.into(),
        Object::Buffer(_) => sym::BUFFER.into(),
    }
}

#[defun]
pub(crate) fn indirect_function<'ob>(object: GcObj<'ob>, cx: &'ob Context) -> GcObj<'ob> {
    match object.untag() {
        Object::Symbol(sym) => match sym.follow_indirect(cx) {
            Some(func) => func.into(),
            None => nil(),
        },
        _ => object,
    }
}

#[defun]
pub(crate) fn provide<'ob>(feature: Symbol<'ob>, _subfeatures: Option<&Cons>) -> Symbol<'ob> {
    let mut features = features().lock().unwrap();
    // TODO: SYMBOL - need to trace this
    let feat = unsafe { feature.into_root() };
    features.insert(feat);
    feature
}

#[defun]
pub(crate) fn car(list: Gc<List>) -> GcObj {
    match list.untag() {
        List::Cons(cons) => cons.car(),
        List::Nil => nil(),
    }
}

#[defun]
pub(crate) fn cdr(list: Gc<List>) -> GcObj {
    match list.untag() {
        List::Cons(cons) => cons.cdr(),
        List::Nil => nil(),
    }
}

#[defun]
pub(crate) fn car_safe(object: GcObj) -> GcObj {
    match object.untag() {
        Object::Cons(cons) => cons.car(),
        _ => nil(),
    }
}

#[defun]
pub(crate) fn cdr_safe(object: GcObj) -> GcObj {
    match object.untag() {
        Object::Cons(cons) => cons.cdr(),
        _ => nil(),
    }
}

#[defun]
pub(crate) fn setcar<'ob>(cell: &Cons, newcar: GcObj<'ob>) -> Result<GcObj<'ob>> {
    cell.set_car(newcar)?;
    Ok(newcar)
}

#[defun]
pub(crate) fn setcdr<'ob>(cell: &Cons, newcdr: GcObj<'ob>) -> Result<GcObj<'ob>> {
    cell.set_cdr(newcdr)?;
    Ok(newcdr)
}

#[defun]
pub(crate) fn cons<'ob>(car: GcObj, cdr: GcObj, cx: &'ob Context) -> GcObj<'ob> {
    crate::cons!(car, cdr; cx)
}

// Symbol with position
#[defun]
fn bare_symbol(sym: Symbol) -> Symbol {
    // TODO: implement
    sym
}

#[defun]
fn symbol_with_pos_p(_sym: GcObj) -> bool {
    // TODO: implement
    false
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

defsym!(MANY);
defsym!(INTEGER);
defsym!(SYMBOL);
defsym!(COMPILED_FUNCTION);
defsym!(HASH_TABLE);
defsym!(BUFFER);
defsym!(SUBR);
