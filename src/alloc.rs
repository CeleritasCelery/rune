use crate::core::env::Symbol;
use crate::core::gc::Context;
use crate::core::object::{
    nil, ByteFn, CodeVec, FnArgs, Gc, GcObj, LispString, LispVec, RecordBuilder,
};
use anyhow::{ensure, Result};
use bstr::BStr;
use fn_macros::defun;

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
    let const_len = prototype.constants(cx).len();
    let vars = closure_vars.len();
    ensure!(
        vars <= 5 && vars <= const_len,
        "Closure vars do not fit in const vec"
    );
    let mut constants = prototype.constants(cx).to_vec();
    let zipped = constants.iter_mut().zip(closure_vars.iter());
    for (cnst, var) in zipped {
        *cnst = *var;
    }

    // TODO: returning an owned type is not safe here
    Ok(unsafe { ByteFn::new(prototype.op_codes.clone(), constants, prototype.args) })
}

#[defun]
pub(crate) fn make_byte_code<'ob>(
    arglist: u64,
    byte_code: &LispString,
    constants: &'ob LispVec,
    _depth: usize,
    _docstring: Option<GcObj>,
    _interactive_spec: Option<GcObj>,
    _elements: &[GcObj],
) -> Result<ByteFn> {
    let bstr: &BStr = byte_code;
    unsafe {
        Ok(ByteFn::new(
            CodeVec(bstr.to_vec()),
            constants.clone_vec(),
            FnArgs::from_arg_spec(arglist)?,
        ))
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
fn make_symbol<'ob>(name: &str, cx: &'ob Context) -> Gc<&'ob Symbol> {
    let sym = Symbol::new_uninterned(name);
    cx.add(sym)
}

define_symbols!(
    FUNCS => {
        list,
        make_closure,
        make_vector,
        make_byte_code,
        vector,
        record,
        purecopy,
        make_symbol,
    }
);
