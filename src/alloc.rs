use crate::object::{Expression, LispFn, Object};
use crate::{arena::Arena, symbol::Symbol};
use anyhow::{ensure, Result};
use fn_macros::defun;

#[defun]
pub(crate) fn list<'ob>(objects: &[Object<'ob>], arena: &'ob Arena) -> Object<'ob> {
    let mut head = Object::Nil;
    for object in objects.iter().rev() {
        head = cons!(object, head; arena);
    }
    head
}

/// Convert a function to closure by replacing the first N elements with their
/// closure values.
#[defun]
pub(crate) fn make_closure<'ob>(
    prototype: &LispFn<'ob>,
    closure_vars: &[Object<'ob>],
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
fn make_vector(length: usize, init: Object) -> Vec<Object> {
    vec![init; length]
}

#[defun]
fn purecopy(obj: Object) -> Object {
    obj
}

#[defun]
fn make_symbol(name: &str) -> Symbol {
    crate::symbol::intern(name)
}

defsubr!(list, make_closure, make_vector, purecopy, make_symbol);
