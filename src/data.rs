//! Utilities for variables and values.
use crate::core::{
    cons::Cons,
    env::{Env, INTERNED_SYMBOLS, sym},
    error::{Type, TypeError},
    gc::{Context, Rt},
    object::{
        IntoObject, List, ListType, NIL, Number, Object, ObjectType, SubrFn, Symbol, WithLifetime,
    },
};
use anyhow::{Result, anyhow};
use rune_core::{hashmap::HashSet, macros::list};
use rune_macros::defun;
use std::sync::LazyLock;
use std::sync::Mutex;

/// Rust translation of the `features` variable: A list of symbols are the features
/// of the executing Emacs. Used by [`featurep`](`crate::fns::featurep`) and [`require`](`crate::fns::require`),
/// altered by [`provide`].
pub(crate) static FEATURES: LazyLock<Mutex<HashSet<Symbol<'static>>>> =
    LazyLock::new(Mutex::default);

#[defun]
pub(crate) fn fset<'ob>(symbol: Symbol<'ob>, definition: Object) -> Result<Symbol<'ob>> {
    if definition.is_nil() {
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
    symbol: Symbol<'ob>,
    definition: Object,
    _docstring: Option<&str>,
) -> Result<Symbol<'ob>> {
    fset(symbol, definition)
}

#[defun]
pub(crate) fn set<'ob>(
    place: Symbol,
    newlet: Object<'ob>,
    env: &mut Rt<Env>,
) -> Result<Object<'ob>> {
    env.set_var(place, newlet)?;
    Ok(newlet)
}

#[defun]
pub(crate) fn put<'ob>(
    symbol: Symbol,
    propname: Symbol,
    value: Object<'ob>,
    env: &mut Rt<Env>,
) -> Object<'ob> {
    env.set_prop(symbol, propname, value);
    value
}

#[defun]
pub(crate) fn get<'ob>(
    symbol: Symbol,
    propname: Symbol,
    env: &Rt<Env>,
    cx: &'ob Context,
) -> Object<'ob> {
    match env.props.get(symbol) {
        Some(plist) => match plist.iter().find(|x| x.0 == propname) {
            Some(element) => cx.bind(element.1.bind(cx)),
            None => NIL,
        },
        None => NIL,
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
) -> Result<Object<'ob>> {
    // TODO: Implement buffer locals
    symbol_value(symbol, env, cx).ok_or_else(|| anyhow!("Void variable: {symbol}"))
}

#[defun]
pub(crate) fn symbol_function<'ob>(symbol: Symbol, cx: &'ob Context) -> Object<'ob> {
    match symbol.func(cx) {
        Some(f) => f.into(),
        None => NIL,
    }
}

#[defun]
pub(crate) fn symbol_value<'ob>(
    symbol: Symbol,
    env: &Rt<Env>,
    cx: &'ob Context,
) -> Option<Object<'ob>> {
    env.vars.get(symbol).map(|x| x.bind(cx))
}

#[defun]
pub(crate) fn symbol_name(symbol: Symbol<'_>) -> &str {
    symbol.get().name()
}

#[defun]
pub(crate) fn null(obj: Object) -> bool {
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
pub(crate) fn listp(object: Object) -> bool {
    matches!(object.untag(), ObjectType::NIL | ObjectType::Cons(_))
}

#[defun]
pub(crate) fn nlistp(object: Object) -> bool {
    !listp(object)
}

#[defun]
pub(crate) fn symbolp(object: Object) -> bool {
    object.is_symbol()
}

#[defun]
pub(crate) fn functionp(object: Object) -> bool {
    match object.untag() {
        ObjectType::ByteFn(_) | ObjectType::SubrFn(_) => true,
        ObjectType::Cons(cons) => cons.car() == sym::CLOSURE || cons.car() == sym::LAMBDA,
        ObjectType::Symbol(sym) => sym.has_func(),
        _ => false,
    }
}

#[defun]
pub(crate) fn subrp(object: Object) -> bool {
    object.is_subr_fn()
}

#[defun]
pub(crate) fn stringp(object: Object) -> bool {
    object.is_string()
}

#[defun]
pub(crate) fn numberp(object: Object) -> bool {
    matches!(object.untag(), ObjectType::Int(_) | ObjectType::Float(_))
}

#[defun]
pub(crate) fn markerp(_: Object) -> bool {
    // TODO: implement
    false
}

#[defun]
pub(crate) fn vectorp(object: Object) -> bool {
    object.is_vec()
}

#[defun]
pub(crate) fn recordp(object: Object) -> bool {
    object.is_record()
}

#[defun]
pub(crate) fn consp(object: Object) -> bool {
    object.is_cons()
}

#[defun]
pub(crate) fn keywordp(object: Object) -> bool {
    match object.untag() {
        ObjectType::Symbol(s) => s.name().starts_with(':'),
        _ => false,
    }
}

#[defun]
pub(crate) fn integerp(object: Object) -> bool {
    object.is_int()
}

#[defun]
pub(crate) fn floatp(object: Object) -> bool {
    object.is_float()
}

#[defun]
pub(crate) fn atom(object: Object) -> bool {
    object.is_not_cons()
}

#[defun]
fn byte_code_function_p(object: Object) -> bool {
    object.is_byte_fn()
}

#[defun]
fn subr_native_elisp_p(_: Object) -> bool {
    false
}

#[defun]
fn bufferp(_object: Object) -> bool {
    // TODO: Implement once buffers are added
    false
}

#[defun]
pub(crate) fn multibyte_string_p(object: Object) -> bool {
    // TODO: handle multi-line strings
    matches!(object.untag(), ObjectType::String(_))
}

#[defun]
fn string_to_number<'ob>(string: &str, base: Option<i64>, cx: &'ob Context) -> Number<'ob> {
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
    initvalue: Option<Object<'ob>>,
    _docstring: Option<&str>,
    env: &mut Rt<Env>,
) -> Result<Object<'ob>> {
    let value = initvalue.unwrap_or_default();
    set(symbol, value, env)
}

#[defun]
pub(crate) fn make_variable_buffer_local(variable: Symbol) -> Symbol {
    // TODO: Implement
    variable
}

#[defun]
fn subr_arity<'ob>(subr: &SubrFn, cx: &'ob Context) -> Object<'ob> {
    let min = subr.args.required as usize;
    let max: Object = {
        if subr.args.rest {
            sym::MANY.into()
        } else {
            (min + subr.args.optional as usize).into()
        }
    };
    Cons::new(min, max, cx).into()
}

#[defun]
fn ash(value: i64, count: i64) -> i64 {
    let shift = if count >= 0 { std::ops::Shl::shl } else { std::ops::Shr::shr };
    let result = shift(value.abs(), count.abs());
    if value >= 0 { result } else { -result }
}

#[defun]
pub(crate) fn aset<'ob>(
    array: Object<'ob>,
    idx: usize,
    newlet: Object<'ob>,
) -> Result<Object<'ob>> {
    match array.untag() {
        ObjectType::Vec(vec) => {
            let vec = vec.try_mut()?;
            if idx < vec.len() {
                vec[idx].set(newlet);
                Ok(newlet)
            } else {
                let len = vec.len();
                Err(anyhow!("index {idx} is out of bounds. Length was {len}"))
            }
        }
        ObjectType::Record(vec) => {
            let vec = vec.try_mut()?;
            if idx < vec.len() {
                vec[idx].set(newlet);
                Ok(newlet)
            } else {
                let len = vec.len();
                Err(anyhow!("index {idx} is out of bounds. Length was {len}"))
            }
        }
        ObjectType::CharTable(table) => {
            table.set(idx, newlet);
            Ok(newlet)
        }
        x => Err(TypeError::new(Type::Sequence, x).into()),
    }
}

#[defun]
pub(crate) fn aref<'ob>(array: Object<'ob>, idx: usize, cx: &'ob Context) -> Result<Object<'ob>> {
    match array.untag() {
        ObjectType::Vec(vec) => match vec.get(idx) {
            Some(x) => Ok(x.get()),
            None => {
                let len = vec.len();
                Err(anyhow!("index {idx} is out of bounds. Length was {len}"))
            }
        },
        ObjectType::Record(vec) => match vec.get(idx) {
            Some(x) => Ok(x.get()),
            None => {
                let len = vec.len();
                Err(anyhow!("index {idx} is out of bounds. Length was {len}"))
            }
        },
        ObjectType::String(string) => match string.chars().nth(idx) {
            Some(x) => Ok((i64::from(x as u32)).into()),
            None => {
                let len = string.len();
                Err(anyhow!("index {idx} is out of bounds. Length was {len}"))
            }
        },
        ObjectType::ByteString(string) => match string.get(idx) {
            Some(x) => Ok((i64::from(*x)).into()),
            None => {
                let len = string.len();
                Err(anyhow!("index {idx} is out of bounds. Length was {len}"))
            }
        },
        ObjectType::ByteFn(fun) => match fun.index(idx, cx) {
            Some(x) => Ok(x),
            None => Err(anyhow!("index {idx} is out of bounds")),
        },
        ObjectType::CharTable(chartable) => Ok(chartable.get(idx)),
        x => Err(TypeError::new(Type::Sequence, x).into()),
    }
}

#[defun]
fn type_of(object: Object) -> Object {
    match object.untag() {
        ObjectType::Int(_) => sym::INTEGER.into(),
        ObjectType::Float(_) => sym::FLOATP.into(),
        ObjectType::Symbol(_) => sym::SYMBOL.into(),
        ObjectType::Cons(_) => sym::CONS.into(),
        ObjectType::Vec(_) => sym::VECTOR.into(),
        ObjectType::Record(x) => x.first().expect("record was missing type").get(),
        ObjectType::ByteFn(_) => sym::COMPILED_FUNCTION.into(),
        ObjectType::HashTable(_) => sym::HASH_TABLE.into(),
        ObjectType::String(_) | ObjectType::ByteString(_) => sym::STRING.into(),
        ObjectType::SubrFn(_) => sym::SUBR.into(),
        ObjectType::Buffer(_) => sym::BUFFER.into(),
        ObjectType::CharTable(_) => sym::CHAR_TABLE.into(),
        ObjectType::BigInt(_) => sym::BIG_INT.into(),
        ObjectType::ChannelSender(_) => sym::CHANNEL_SENDER.into(),
        ObjectType::ChannelReceiver(_) => sym::CHANNEL_RECEIVER.into(),
    }
}

#[defun]
pub(crate) fn indirect_function<'ob>(object: Object<'ob>, cx: &'ob Context) -> Object<'ob> {
    match object.untag() {
        ObjectType::Symbol(sym) => match sym.follow_indirect(cx) {
            Some(func) => func.into(),
            None => NIL,
        },
        _ => object,
    }
}

#[defun]
pub(crate) fn provide<'ob>(feature: Symbol<'ob>, _subfeatures: Option<&Cons>) -> Symbol<'ob> {
    let mut features = FEATURES.lock().unwrap();
    // TODO: SYMBOL - need to trace this
    let feat = unsafe { feature.with_lifetime() };
    features.insert(feat);
    feature
}

#[defun]
pub(crate) fn car(list: List) -> Object {
    match list.untag() {
        ListType::Cons(cons) => cons.car(),
        ListType::Nil => NIL,
    }
}

#[defun]
pub(crate) fn cdr(list: List) -> Object {
    match list.untag() {
        ListType::Cons(cons) => cons.cdr(),
        ListType::Nil => NIL,
    }
}

#[defun]
pub(crate) fn car_safe(object: Object) -> Object {
    match object.untag() {
        ObjectType::Cons(cons) => cons.car(),
        _ => NIL,
    }
}

#[defun]
pub(crate) fn cdr_safe(object: Object) -> Object {
    match object.untag() {
        ObjectType::Cons(cons) => cons.cdr(),
        _ => NIL,
    }
}

#[defun]
pub(crate) fn setcar<'ob>(cell: &Cons, newcar: Object<'ob>) -> Result<Object<'ob>> {
    cell.set_car(newcar)?;
    Ok(newcar)
}

#[defun]
pub(crate) fn setcdr<'ob>(cell: &Cons, newcdr: Object<'ob>) -> Result<Object<'ob>> {
    cell.set_cdr(newcdr)?;
    Ok(newcdr)
}

#[defun]
pub(crate) fn cons<'ob>(car: Object, cdr: Object, cx: &'ob Context) -> Object<'ob> {
    Cons::new(car, cdr, cx).into()
}

// Symbol with position
#[defun]
fn bare_symbol(sym: Symbol) -> Symbol {
    // TODO: implement
    sym
}

#[defun]
fn symbol_with_pos_p(_sym: Object) -> bool {
    // TODO: implement
    false
}

#[derive(Debug, PartialEq)]
pub(crate) struct LispError {
    message: &'static Cons,
}

impl std::error::Error for LispError {}

impl std::fmt::Display for LispError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let Self { message } = self;
        write!(f, "Error: {message}")
    }
}

defsym!(WRONG_NUMBER_OF_ARGUMENTS);
impl LispError {
    pub(crate) fn new(message: &Cons) -> Self {
        Self { message: unsafe { message.with_lifetime() } }
    }

    pub(crate) fn bind<'ob>(&self, cx: &'ob Context) -> &'ob Cons {
        cx.bind(self.message)
    }

    pub(crate) fn arg_cnt<'ob, T>(
        func: impl IntoObject<Out<'ob> = T>,
        expected: u16,
        actual: u16,
        cx: &'ob Context,
    ) -> Self {
        let func = func.into_obj(cx);
        let list = list![sym::WRONG_NUMBER_OF_ARGUMENTS, func, expected, actual; cx];
        Self::new(list.try_into().unwrap())
    }
}

unsafe impl Send for LispError {}
unsafe impl Sync for LispError {}

#[cfg(test)]
mod test {
    use super::*;
    use crate::interpreter::assert_lisp;

    #[test]
    fn test_ash() {
        assert_eq!(ash(4, 1), 8);
        assert_eq!(ash(4, -1), 2);
        assert_eq!(ash(-8, -1), -4);
        assert_eq!(ash(256, -8), 1);
        assert_eq!(ash(-8, 1), -16);
    }

    #[test]
    fn test_functionp() {
        assert_lisp("(functionp '(lambda nil))", "t");
    }
}

defsym!(MANY);
defsym!(INTEGER);
defsym!(SYMBOL);
defsym!(COMPILED_FUNCTION);
defsym!(HASH_TABLE);
defsym!(BUFFER);
defsym!(SUBR);
defsym!(CHAR_TABLE);
defsym!(BIG_INT);
defsym!(CHANNEL_SENDER);
defsym!(CHANNEL_RECEIVER);
defsym!(CHANNEL_CLOSED);
defsym!(CHANNEL_FULL);
defsym!(CHANNEL_EMPTY);
defsym!(CHANNEL_TIMEOUT);
defsym!(THREAD_ERRORED);
