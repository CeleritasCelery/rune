use fn_macros::defun;

use crate::{
    arena::RootOwner,
    arena::{Arena, IntoRoot, Root},
    data::Environment,
    object::{FunctionX, Gc, Object},
    root_struct,
};
use anyhow::Result;

#[defun]
pub(crate) fn apply<'ob, 'id>(
    function: Gc<FunctionX<'ob>>,
    arguments: &[Object<'ob>],
    env: &Root<'id, Environment>,
    owner: &mut RootOwner<'id>,
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
    root_struct!(args, args.into_root(), arena);
    function.call(args, env, arena, owner)
}

#[defun]
pub(crate) fn funcall<'ob, 'id>(
    function: Gc<FunctionX<'ob>>,
    arguments: &[Object<'ob>],
    env: &Root<'id, Environment>,
    owner: &mut RootOwner<'id>,
    arena: &'ob mut Arena,
) -> Result<Object<'ob>> {
    root_struct!(arg_list, arguments.to_vec().into_root(), arena);
    function.call(arg_list, env, arena, owner)
}

defsubr!(apply, funcall);
