//! builtin lisp data structures.
use crate::core::env::{Symbol, SymbolCell};
use crate::core::gc::Context;
use crate::core::object::{
    nil, ByteFn, FnArgs, Gc, GcObj, IntoObject, LispString, LispVec, RecordBuilder,
};
use anyhow::{ensure, Result};
use rune_core::macros::cons;
use rune_macros::defun;

#[defun]
pub(crate) fn list<'ob>(objects: &[GcObj<'ob>], cx: &'ob Context) -> GcObj<'ob> {
    let mut head = nil();
    for object in objects.iter().rev() {
        head = cons!(*object, head; cx);
    }
    head
}

/// Convert a function to closure by replacing the first N elements with their
/// closure values.
#[defun]
pub(crate) fn make_closure<'ob>(
    prototype: &ByteFn,
    closure_vars: &[GcObj<'ob>],
    cx: &'ob Context,
) -> Result<ByteFn> {
    let const_len = prototype.constants().len();
    let vars = closure_vars.len();
    ensure!(vars <= const_len, "Closure vars do not fit in const vec");
    let mut constants = prototype.constants().to_vec();
    let zipped = constants.iter_mut().zip(closure_vars.iter());
    for (cnst, var) in zipped {
        *cnst = *var;
    }
    let new_constants = constants.into_obj(cx);

    // TODO: returning an owned type is not safe here
    Ok(unsafe {
        ByteFn::new(prototype.codes(), new_constants.untag(), prototype.args, prototype.depth)
    })
}

#[defun]
#[allow(clippy::too_many_arguments)]
pub(crate) fn make_byte_code<'ob>(
    arglist: u64,
    byte_code: &'ob LispString,
    constants: &'ob LispVec,
    depth: usize,
    _docstring: Option<GcObj>,
    _interactive_spec: Option<GcObj>,
    _elements: &[GcObj],
    cx: &'ob Context,
) -> Result<&'ob ByteFn> {
    unsafe {
        let bytefn = ByteFn::new(byte_code, constants, FnArgs::from_arg_spec(arglist)?, depth);
        Ok(bytefn.into_obj(cx).untag())
    }
}

#[defun]
fn make_vector(length: usize, init: GcObj) -> Vec<GcObj> {
    vec![init; length]
}

#[defun]
fn vector<'ob>(objects: &[GcObj<'ob>]) -> Vec<GcObj<'ob>> {
    objects.into()
}

#[defun]
fn record<'ob>(type_: GcObj<'ob>, slots: &[GcObj<'ob>]) -> RecordBuilder<'ob> {
    let mut record = vec![type_];
    record.extend_from_slice(slots);
    RecordBuilder(record)
}

#[defun]
fn purecopy(obj: GcObj) -> GcObj {
    obj
}

#[defun]
fn make_symbol<'ob>(name: &str, cx: &'ob Context) -> Gc<Symbol<'ob>> {
    let sym = SymbolCell::new_uninterned(name);
    sym.into_obj(cx)
}
