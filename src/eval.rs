use fn_macros::defun;

use crate::{
    arena::{Arena, Gc, IntoRoot},
    data::Environment,
    object::{Function, Object},
};
use anyhow::Result;

#[defun]
pub(crate) fn apply<'ob>(
    function: Function<'ob>,
    arguments: &[Object<'ob>],
    env: &mut Gc<Environment>,
    arena: &'ob mut Arena,
) -> Result<Object<'ob>> {
    let args = match arguments.len() {
        0 => Vec::new(),
        len => {
            let end = len - 1;
            let last = arguments[end];
            let mut args: Vec<_> = arguments[..end].iter().map(|x| arena.bind(*x)).collect();
            for element in last.as_list(arena)? {
                let e = arena.bind(element?);
                args.push(e);
            }
            args
        }
    };
    let args = unsafe { &mut Gc::new(args.into_root()) };
    function.call(args, env, arena)
}

#[defun]
pub(crate) fn funcall<'ob>(
    function: Function<'ob>,
    arguments: &[Object<'ob>],
    env: &mut Gc<Environment>,
    arena: &'ob mut Arena,
) -> Result<Object<'ob>> {
    let arg_list = unsafe { &mut Gc::new(arguments.to_vec().into_root()) };
    function.call(arg_list, env, arena)
}

defsubr!(apply, funcall);
