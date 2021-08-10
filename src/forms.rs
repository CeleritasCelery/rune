use std::convert::TryInto;

use crate::arena::Arena;
use crate::cons::Cons;
use crate::data::Environment;
use crate::object::{IntoObject, LocalFunction, LocalFunctionValue, Object};
use crate::symbol::Symbol;
use crate::symbol::INTERNED_SYMBOLS;
use fn_macros::defun;

#[defun]
pub(crate) fn defalias<'ob>(
    symbol: Symbol,
    definition: LocalFunction<'ob>,
    env: &mut Environment<'ob>,
) -> Symbol {
    match definition.try_into() {
        Ok(func) => {
            let map = INTERNED_SYMBOLS.lock().unwrap();
            map.set_func(symbol, func);
        }
        Err(_) => {
            if let LocalFunctionValue::Cons(cons) = definition.val() {
                env.funcs.insert(symbol, cons);
            } else {
                unreachable!("local function was not function or cons");
            }
        }
    };
    symbol
}

#[defun]
pub(crate) fn list<'ob>(objects: &[Object<'ob>], arena: &'ob Arena) -> Object<'ob> {
    let mut head = Object::Nil;
    for object in objects.iter().rev() {
        head = Cons::new(*object, head).into_obj(arena);
    }
    head
}

#[defun]
pub(crate) fn progn<'ob>(forms: &[Object<'ob>]) -> Object<'ob> {
    match forms.last() {
        Some(form) => *form,
        None => Object::Nil,
    }
}

defsubr!(defalias, progn, list);
