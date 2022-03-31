use fn_macros::defun;

use crate::{
    arena::{Arena, GcCell, IntoRoot},
    data::Environment,
    lcell::LCellOwner,
    object::{Function, Object},
};
use anyhow::Result;

#[defun]
pub(crate) fn apply<'ob, 'id>(
    function: Function<'ob>,
    arguments: &[Object<'ob>],
    env: &GcCell<'id, Environment>,
    owner: &mut LCellOwner<'id>,
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
    let args = unsafe { &GcCell::new(args.into_root()) };
    function.call(args, env, arena, owner)
}

#[defun]
pub(crate) fn funcall<'ob, 'id>(
    function: Function<'ob>,
    arguments: &[Object<'ob>],
    env: &GcCell<'id, Environment>,
    owner: &mut LCellOwner<'id>,
    arena: &'ob mut Arena,
) -> Result<Object<'ob>> {
    let arg_list = unsafe { &GcCell::new(arguments.to_vec().into_root()) };
    function.call(arg_list, env, arena, owner)
}

defsubr!(apply, funcall);
