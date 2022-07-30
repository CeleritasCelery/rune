use fn_macros::defun;
use streaming_iterator::StreamingIterator;

use crate::core::arena::Rt;
use crate::core::error::{Type, TypeError};
use crate::core::object::Object;
use crate::core::{
    arena::{Arena, IntoRoot, Root},
    object::{Function, Gc, GcObj},
};
use crate::fns::assq;
use crate::{element_iter, root};

use crate::core::env::{Environment, Symbol};

use anyhow::{bail, Result};

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
    function.call(args, env, arena, None).map_err(Into::into)
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
    function
        .call(arg_list, env, arena, None)
        .map_err(Into::into)
}

#[defun]
fn run_hooks<'ob>(
    hooks: &[Rt<GcObj>],
    env: &mut Root<Environment>,
    cx: &'ob mut Arena,
) -> Result<GcObj<'ob>> {
    for hook in hooks {
        match hook.bind(cx).get() {
            Object::Symbol(sym) => {
                if let Some(val) = env.vars.get(sym) {
                    let val = val.bind(cx);
                    if let Object::Cons(hook_list) = val.get() {
                        root!(args, Vec::new(), cx);
                        element_iter!(hooks, hook_list, cx);
                        while let Some(hook) = hooks.next() {
                            let func: &Rt<Gc<Function>> = hook.try_as()?;
                            func.call(args, env, cx, None)?;
                        }
                    } else {
                        let func: Gc<Function> = val.try_into()?;
                        root!(func, cx);
                        root!(args, Vec::new(), cx);
                        func.call(args, env, cx, None)?;
                    }
                }
            }
            x => bail!(TypeError::new(Type::Symbol, x)),
        }
    }
    Ok(GcObj::NIL)
}

#[defun]
fn autoload_do_load<'ob>(
    fundef: GcObj<'ob>,
    _funname: Option<GcObj>,
    _macro_only: Option<GcObj>,
) -> GcObj<'ob> {
    // TODO: implement
    fundef
}

#[defun]
fn autoload<'ob>(function: GcObj<'ob>, file: GcObj) -> GcObj<'ob> {
    // TODO: implement
    println!("autoload: function: {function}, file: {file}");
    GcObj::NIL
}

#[defun]
pub(crate) fn macroexpand<'ob>(
    form: &Rt<GcObj>,
    environment: Option<&Rt<GcObj>>,
    gc: &'ob mut Arena,
    env: &mut Root<Environment>,
) -> Result<GcObj<'ob>> {
    if let Object::Cons(form) = form.bind(gc).get() {
        if let Object::Symbol(name) = form.car().get() {
            // shadow the macro based on ENVIRONMENT
            let func: Option<Gc<Function>> = match environment {
                Some(env) => match assq(name.into(), env.bind(gc).try_into()?).get() {
                    Object::Cons(cons) => Some(cons.cdr().try_into()?),
                    _ => get_macro_func(name, gc),
                },
                _ => get_macro_func(name, gc),
            };
            if let Some(macro_func) = func {
                let macro_args = form.cdr().as_list()?.collect::<Result<Vec<_>>>()?;
                root!(args, macro_args.into_root(), gc);
                root!(macro_func, gc);
                let result = macro_func.call(args, env, gc, Some(name.name))?;
                root!(result, gc);
                // recursively expand the macro's
                return macroexpand(result, environment, gc, env);
            }
        }
    }
    Ok(form.bind(gc))
}

fn get_macro_func<'ob>(name: Symbol, gc: &'ob Arena) -> Option<Gc<Function<'ob>>> {
    if let Some(callable) = name.follow_indirect(gc) {
        if let Function::Cons(cons) = callable.get() {
            return cons.try_as_macro().ok();
        }
    }
    None
}

defsym!(FUNCTION, "function");
defsym!(QUOTE, "quote");
defsym!(MACRO, "macro");
defsym!(UNQUOTE, ",");
defsym!(SPLICE, ",@");
defsym!(BACKQUOTE, "`");
defsym!(NIL, "nil");
defsym!(TRUE, "t");
defsym!(AND_OPTIONAL, "&optional");
defsym!(AND_REST, "&rest");
defsym!(LAMBDA, "lambda");
defsym!(CLOSURE, "closure");
defsym!(CONDITION_CASE, "condition-case");
defsym!(WHILE, "while");
defsym!(PROGN, "progn");
defsym!(PROG1, "prog1");
defsym!(PROG2, "prog2");
defsym!(SETQ, "setq");
defsym!(DEFCONST, "defconst");
defsym!(COND, "cond");
defsym!(LET, "let");
defsym!(LET_STAR, "let*");
defsym!(IF, "if");
defsym!(AND, "and");
defsym!(OR, "or");
defsym!(INTERACTIVE, "interactive");
defsym!(CATCH, "catch");
defsym!(THROW, "throw");
defsym!(ERROR, "error");
defsym!(DEBUG, "debug");

defsubr!(
    apply,
    funcall,
    run_hooks,
    autoload,
    autoload_do_load,
    macroexpand,
    FUNCTION,
    QUOTE,
    MACRO,
    UNQUOTE,
    SPLICE,
    BACKQUOTE,
    NIL,
    TRUE,
    AND_OPTIONAL,
    AND_REST,
    LAMBDA,
    CLOSURE,
    CONDITION_CASE,
    WHILE,
    PROGN,
    PROG1,
    PROG2,
    SETQ,
    DEFCONST,
    COND,
    LET,
    LET_STAR,
    IF,
    AND,
    OR,
    INTERACTIVE,
    CATCH,
    THROW,
    ERROR,
    DEBUG,
);
