use fn_macros::defun;
use streaming_iterator::StreamingIterator;

use crate::core::error::{Type, TypeError};
use crate::core::gc::Rt;
use crate::core::object::Object;
use crate::core::{
    gc::{Context, IntoRoot, Root},
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
    cx: &'ob mut Context,
) -> Result<GcObj<'ob>> {
    let args = match arguments.len() {
        0 => Vec::new(),
        len => {
            let end = len - 1;
            let last = &arguments[end];
            let mut args: Vec<_> = arguments[..end].iter().map(|x| x.bind(cx)).collect();
            for element in last.bind(cx).as_list()? {
                let e = cx.bind(element?);
                args.push(e);
            }
            args
        }
    };
    root!(args, args.into_root(), cx);
    function.call(args, env, cx, None).map_err(Into::into)
}

#[defun]
pub(crate) fn funcall<'ob>(
    function: &Rt<Gc<Function>>,
    arguments: &[Rt<GcObj>],
    env: &mut Root<Environment>,
    cx: &'ob mut Context,
) -> Result<GcObj<'ob>> {
    let arguments = unsafe { Rt::bind_slice(arguments, cx).to_vec().into_root() };
    root!(arg_list, arguments, cx);
    function.call(arg_list, env, cx, None).map_err(Into::into)
}

#[defun]
fn run_hooks<'ob>(
    hooks: &[Rt<GcObj>],
    env: &mut Root<Environment>,
    cx: &'ob mut Context,
) -> Result<GcObj<'ob>> {
    for hook in hooks {
        match hook.bind(cx).get() {
            Object::Symbol(sym) => {
                if let Some(val) = env.vars.get(sym) {
                    let val = val.bind(cx);
                    match val.get() {
                        Object::Cons(hook_list) => {
                            element_iter!(hooks, hook_list, cx);
                            while let Some(hook) = hooks.next() {
                                let func: &Rt<Gc<Function>> = hook.try_as()?;
                                root!(args, Vec::new(), cx);
                                func.call(args, env, cx, None)?;
                            }
                        }
                        Object::Nil => {}
                        _ => {
                            let func: Gc<Function> = val.try_into()?;
                            root!(func, cx);
                            root!(args, Vec::new(), cx);
                            func.call(args, env, cx, None)?;
                        }
                    }
                }
            }
            Object::Nil => {}
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
    cx: &'ob mut Context,
    env: &mut Root<Environment>,
) -> Result<GcObj<'ob>> {
    if let Object::Cons(form) = form.bind(cx).get() {
        if let Object::Symbol(name) = form.car().get() {
            // shadow the macro based on ENVIRONMENT
            let func: Option<Gc<Function>> = match environment {
                Some(env) => match assq(name.into(), env.bind(cx).try_into()?).get() {
                    Object::Cons(cons) => Some(cons.cdr().try_into()?),
                    _ => get_macro_func(name, cx),
                },
                _ => get_macro_func(name, cx),
            };
            if let Some(macro_func) = func {
                let macro_args = form.cdr().as_list()?.collect::<Result<Vec<_>>>()?;
                root!(args, macro_args.into_root(), cx);
                root!(macro_func, cx);
                let result = macro_func.call(args, env, cx, Some(name.name))?;
                root!(result, cx);
                // recursively expand the macro's
                return macroexpand(result, environment, cx, env);
            }
        }
    }
    Ok(form.bind(cx))
}

fn get_macro_func<'ob>(name: Symbol, cx: &'ob Context) -> Option<Gc<Function<'ob>>> {
    if let Some(callable) = name.follow_indirect(cx) {
        if let Function::Cons(cons) = callable.get() {
            return cons.try_as_macro().ok();
        }
    }
    None
}

#[defun]
#[allow(non_snake_case)]
fn internal__define_uninitialized_variable(_symbol: Symbol, _doc: Option<GcObj>) -> GcObj {
    // TODO: implement doc strings
    GcObj::NIL
}

#[defun]
fn set_default_toplevel_value<'ob>(
    symbol: Symbol,
    value: GcObj,
    env: &'ob mut Root<Environment>,
    cx: &'ob Context,
) -> GcObj<'ob> {
    env.deref_mut(cx).set_var(symbol, value);
    GcObj::NIL
}

#[defun]
fn set_default<'ob>(
    symbol: Symbol,
    value: GcObj<'ob>,
    env: &'ob mut Root<Environment>,
    cx: &'ob Context,
) -> GcObj<'ob> {
    // TODO: implement buffer local variables
    env.deref_mut(cx).set_var(symbol, value);
    value
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
    internal__define_uninitialized_variable,
    set_default_toplevel_value,
    set_default,
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
