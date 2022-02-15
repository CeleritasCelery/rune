use fn_macros::defun;

use crate::{
    arena::{Arena, Gc},
    data::Environment,
    object::{Function, Object},
};
use anyhow::Result;

#[defun]
pub(crate) fn apply<'ob>(
    function: Function<'ob>,
    arguments: &[Object<'ob>],
    env: &mut Gc<Environment>,
    arena: &'ob Arena,
) -> Result<Object<'ob>> {
    let args = match arguments.len() {
        0 => Vec::new(),
        len => {
            let end = len - 1;
            let last = arguments[end];
            let mut args = arguments[..end].to_vec();
            for element in last.as_list(arena)? {
                args.push(element?);
            }
            args
        }
    };
    function.call(args, env, arena)
}

#[defun]
pub(crate) fn funcall<'ob>(
    function: Function<'ob>,
    arguments: &[Object<'ob>],
    env: &mut Gc<Environment>,
    arena: &'ob Arena,
) -> Result<Object<'ob>> {
    function.call(arguments.to_vec(), env, arena)
}

defsubr!(apply, funcall);
