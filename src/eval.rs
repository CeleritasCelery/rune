use fn_macros::defun;

use crate::core::arena::Rt;
use crate::core::object::{Callable, Object};
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
            for element in last.bind(arena).as_list()? {
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

#[defun]
pub(crate) fn macroexpand<'ob>(
    form: &Rt<GcObj>,
    environment: Option<&Rt<GcObj>>,
    gc: &'ob mut Arena,
    env: &mut Root<Environment>,
) -> Result<GcObj<'ob>> {
    if let Some(x) = environment {
        unimplemented!("macroexpand override environment: {x:?}")
    }
    if let Object::Cons(form) = form.bind(gc).get() {
        if let Object::Symbol(name) = form.car().get() {
            if let Some(callable) = name.resolve_callable(gc) {
                if let Callable::Cons(cons) = callable.get() {
                    if let Ok(mcro) = cons.try_as_macro() {
                        let macro_args = form.cdr().as_list()?.collect::<Result<Vec<_>>>()?;
                        root!(args, macro_args.into_root(), gc);
                        let macro_func: Gc<Function> = mcro.into();
                        root!(macro_func, gc);
                        return macro_func.call(args, env, gc, Some(name.name));
                    }
                }
            }
        }
    }
    Ok(form.bind(gc))
}

defsubr!(apply, funcall, macroexpand);
