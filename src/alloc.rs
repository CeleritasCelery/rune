use crate::core::object::{Expression, GcObj, LispFn};
use crate::core::{arena::Arena, env::Symbol};
use anyhow::{ensure, Result};
use fn_macros::defun;

#[defun]
pub(crate) fn list<'ob>(objects: &[GcObj<'ob>], arena: &'ob Arena) -> GcObj<'ob> {
    let mut head = GcObj::NIL;
    for object in objects.iter().rev() {
        head = cons!(*object, head; arena);
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
fn make_vector(length: usize, init: GcObj) -> Vec<GcObj> {
    vec![init; length]
}

#[defun]
fn vector<'ob>(objects: &[GcObj<'ob>]) -> Vec<GcObj<'ob>> {
    objects.into()
}

#[defun]
fn purecopy(obj: GcObj) -> GcObj {
    obj
}

#[defun]
fn make_symbol(name: &str) -> Symbol {
    crate::core::env::intern(name)
}

defsubr!(
    list,
    make_closure,
    make_vector,
    vector,
    purecopy,
    make_symbol
);
