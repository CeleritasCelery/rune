use std::cell::RefCell;

use crate::core::env::Symbol;
use crate::core::gc::Context;
use crate::core::object::{nil, CodeVec, Expression, FnArgs, Gc, GcObj, LispFn, LispVec, Record};
use anyhow::{ensure, Result};
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
    prototype: &LispFn<'ob>,
    closure_vars: &[GcObj<'ob>],
) -> Result<LispFn<'ob>> {
    let const_len = prototype.body.constants.len();
    let vars = closure_vars.len();
    ensure!(
        vars <= 5 && vars <= const_len,
        "Closure vars do not fit in const vec"
    );
    let mut constants = prototype.body.constants.clone();
    let zipped = constants.iter_mut().zip(closure_vars.iter());
    for (cnst, var) in zipped {
        *cnst = *var;
    }

    Ok(LispFn {
        body: Expression {
            op_codes: prototype.body.op_codes.clone(),
            constants,
        },
        args: prototype.args,
    })
}

#[defun]
fn make_byte_code<'ob>(
    arglist: i64,
    byte_code: &RefCell<Vec<u8>>,
    constants: &LispVec<'ob>,
    _depth: usize,
    _docstring: Option<GcObj>,
    _interactive_spec: Option<GcObj>,
    _elements: &[GcObj],
) -> LispFn<'ob> {
    let arglist = arglist as u16;
    let required = arglist & 0x7F;
    let optional = arglist >> 8 & 0x7F;
    let rest = arglist & 0x80 != 0;
    LispFn {
        body: Expression {
            op_codes: CodeVec(byte_code.borrow().clone()),
            constants: constants.borrow().clone(),
        },
        args: FnArgs {
            rest,
            required,
            optional,
            advice: false,
        },
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
fn record<'ob>(type_: GcObj<'ob>, slots: &[GcObj<'ob>]) -> Record<'ob> {
    let mut record = vec![type_];
    record.extend_from_slice(slots);
    Record::new(record)
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
