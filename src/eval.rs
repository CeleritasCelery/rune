use fn_macros::defun;

use crate::core::arena::Rt;
use crate::core::{
    arena::{Arena, IntoRoot, Root},
    object::{Function, Gc, GcObj},
};
use crate::root;

use crate::core::env::Environment;

use anyhow::Result;

#[defun]
pub(crate) fn apply<'ob>(
    function: &Rt<Gc<Function>>,
    arguments: &[Rt<GcObj>],
    env: &mut Root<Environment>,
    arena: &'ob mut Arena,
) -> Result<GcObj<'ob>> {
    let args = match arguments.len() {
        0 => Vec::new(),
        len => {
            let end = len - 1;
            let last = &arguments[end];
            let mut args: Vec<_> = arguments[..end].iter().map(|x| x.bind(arena)).collect();
            for element in last.bind(arena).as_list(arena)? {
                let e = arena.bind(element?);
                args.push(e);
            }
            args
        }
    };
    root!(args, args.into_root(), arena);
    function.call(args, env, arena, None)
}

#[defun]
pub(crate) fn funcall<'ob>(
    function: &Rt<Gc<Function>>,
    arguments: &[Rt<GcObj>],
    env: &mut Root<Environment>,
    arena: &'ob mut Arena,
) -> Result<GcObj<'ob>> {
    let arguments = unsafe { Rt::bind_slice(arguments, arena).to_vec().into_root() };
    root!(arg_list, arguments, arena);
    function.call(arg_list, env, arena, None)
}

defsubr!(apply, funcall);
