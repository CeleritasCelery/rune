use fn_macros::defun;
use streaming_iterator::StreamingIterator;

use crate::core::error::{EvalError, Type, TypeError};
use crate::core::gc::Rt;
use crate::core::object::{nil, LispString, Object};
use crate::core::{
    gc::{Context, IntoRoot, Root},
    object::{Function, Gc, GcObj},
};
use crate::fns::assq;
use crate::{root, rooted_iter};

use crate::core::env::{sym, Env, SymbolX};

use anyhow::{anyhow, bail, ensure, Result};

#[defun]
pub(crate) fn apply<'ob>(
    function: &Rt<Gc<Function>>,
    arguments: &[Rt<GcObj>],
    env: &mut Root<Env>,
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
    root!(args, move(args), cx);
    function.call(args, env, cx, None).map_err(Into::into)
}

#[defun]
pub(crate) fn funcall<'ob>(
    function: &Rt<Gc<Function>>,
    arguments: &[Rt<GcObj>],
    env: &mut Root<Env>,
    cx: &'ob mut Context,
) -> Result<GcObj<'ob>> {
    let arguments = unsafe { Rt::bind_slice(arguments, cx).to_vec().into_root() };
    root!(arg_list, arguments, cx);
    function.call(arg_list, env, cx, None).map_err(Into::into)
}

#[defun]
fn run_hooks<'ob>(
    hooks: &[Rt<GcObj>],
    env: &mut Root<Env>,
    cx: &'ob mut Context,
) -> Result<GcObj<'ob>> {
    for hook in hooks {
        match hook.get(cx) {
            Object::Symbol(sym) => {
                if let Some(val) = env.vars.get(sym) {
                    let val = val.bind(cx);
                    match val.untag() {
                        Object::Cons(hook_list) => {
                            rooted_iter!(hooks, hook_list, cx);
                            while let Some(hook) = hooks.next() {
                                let func: &Rt<Gc<Function>> = hook.try_into()?;
                                root!(args, Vec::new(), cx);
                                func.call(args, env, cx, None)?;
                            }
                        }
                        Object::Symbol(s) if s.nil() => {}
                        _ => {
                            let func: Gc<Function> = val.try_into()?;
                            root!(func, cx);
                            root!(args, Vec::new(), cx);
                            func.call(args, env, cx, None)?;
                        }
                    }
                }
            }
            x => bail!(TypeError::new(Type::Symbol, x)),
        }
    }
    Ok(nil())
}

#[defun]
pub(crate) fn autoload_do_load<'ob>(
    fundef: &Rt<GcObj>,
    funname: Option<&Rt<Gc<SymbolX>>>,
    macro_only: Option<&Rt<GcObj>>,
    env: &mut Root<Env>,
    cx: &'ob mut Context,
) -> Result<GcObj<'ob>> {
    // TODO: want to handle the case where the file is already loaded.
    match fundef.get(cx) {
        Object::Cons(cons) if cons.car() == sym::AUTOLOAD => {
            ensure!(
                macro_only.is_none(),
                "autoload-do-load macro-only is not yet implemented"
            );
            let mut elem = cons.elements();
            elem.next(); // autoload
            let file: Gc<&LispString> = elem
                .next()
                .ok_or_else(|| anyhow!("Malformed autoload"))??
                .try_into()?;
            ensure!(
                elem.all(|x| match x {
                    Ok(x) => x.nil(),
                    Err(_) => false,
                }),
                "autoload arguments are not yet implemented"
            );
            root!(file, cx);
            crate::lread::load(file, None, None, cx, env)?;
            match funname {
                Some(func) => match func.get(cx).func(cx) {
                    Some(x) => Ok(x.into()),
                    None => Err(anyhow!("autoload of {func} did not provide a definition")),
                },
                _ => Ok(nil()),
            }
        }
        _ => Ok(fundef.bind(cx)),
    }
}

#[defun]
fn autoload<'ob>(
    function: SymbolX<'ob>,
    file: &str,
    docstring: Option<GcObj>,
    interactive: Option<GcObj>,
    load_type: Option<GcObj>,
    cx: &'ob Context,
) -> Result<SymbolX<'ob>> {
    if function.has_func() {
        Ok(&sym::NIL)
    } else {
        let autoload = list![sym::AUTOLOAD, file, docstring, interactive, load_type; cx];
        crate::data::fset(function, autoload)
    }
}

#[defun]
pub(crate) fn macroexpand<'ob>(
    form: &Rt<GcObj>,
    environment: Option<&Rt<GcObj>>,
    cx: &'ob mut Context,
    env: &mut Root<Env>,
) -> Result<GcObj<'ob>> {
    if let Object::Cons(form) = form.get(cx) {
        if let Object::Symbol(sym) = form.car().untag() {
            // shadow the macro based on ENVIRONMENT
            let func: Option<Gc<Function>> = match environment {
                Some(env) => match assq(sym.into(), env.bind(cx).try_into()?)?.untag() {
                    Object::Cons(cons) => Some(cons.cdr().try_into()?),
                    _ => get_macro_func(sym, cx),
                },
                _ => get_macro_func(sym, cx),
            };
            if let Some(macro_func) = func {
                let macro_args = form.cdr().as_list()?.collect::<Result<Vec<_>>>()?;
                root!(args, move(macro_args), cx);
                root!(macro_func, cx);
                let name = sym.name().to_owned();
                let result = macro_func.call(args, env, cx, Some(&name))?;
                root!(result, cx);
                // recursively expand the macro's
                return macroexpand(result, environment, cx, env);
            }
        }
    }
    Ok(form.bind(cx))
}

fn get_macro_func<'ob>(name: SymbolX, cx: &'ob Context) -> Option<Gc<Function<'ob>>> {
    if let Some(callable) = name.follow_indirect(cx) {
        if let Function::Cons(cons) = callable.untag() {
            if cons.car() == sym::MACRO {
                return cons.cdr().try_into().ok();
            }
        }
    }
    None
}

#[defun]
#[allow(non_snake_case)]
fn internal__define_uninitialized_variable<'ob>(
    _symbol: SymbolX<'ob>,
    _doc: Option<GcObj>,
) -> GcObj<'ob> {
    // TODO: implement doc strings
    nil()
}

#[defun]
fn signal(mut error_symbol: GcObj, data: GcObj, env: &mut Root<Env>, cx: &Context) -> Result<bool> {
    if error_symbol.nil() && data.nil() {
        error_symbol = sym::ERROR.into();
    }
    Err(EvalError::signal(error_symbol, data, env.as_mut(cx)).into())
}

#[defun]
fn special_variable_p(symbol: SymbolX, env: &Root<Env>) -> bool {
    env.special_variables.contains(symbol)
}

#[defun]
fn set_default_toplevel_value<'ob>(
    symbol: SymbolX,
    value: GcObj,
    env: &'ob mut Root<Env>,
    cx: &'ob Context,
) -> Result<GcObj<'ob>> {
    env.as_mut(cx).set_var(symbol, value)?;
    Ok(nil())
}

#[defun]
fn set_default<'ob>(
    symbol: SymbolX,
    value: GcObj<'ob>,
    env: &'ob mut Root<Env>,
    cx: &'ob Context,
) -> Result<GcObj<'ob>> {
    // TODO: implement buffer local variables
    env.as_mut(cx).set_var(symbol, value)?;
    Ok(value)
}

defsym!(FUNCTION);
defsym!(QUOTE);
defsym!(MACRO);
defsym!(UNQUOTE, ",");
defsym!(SPLICE, ",@");
defsym!(BACKQUOTE, "`");
defsym!(AND_OPTIONAL, "&optional");
defsym!(AND_REST, "&rest");
defsym!(LAMBDA);
defsym!(CLOSURE);
defsym!(CONDITION_CASE);
defsym!(UNWIND_PROTECT);
defsym!(WHILE);
defsym!(INLINE);
defsym!(PROGN);
defsym!(PROG1);
defsym!(PROG2);
defsym!(SETQ);
defsym!(DEFCONST);
defsym!(COND);
defsym!(LET);
defsym!(LET_STAR, "let*");
defsym!(IF);
defsym!(AND);
defsym!(OR);
defsym!(INTERACTIVE);
defsym!(CATCH);
defsym!(THROW);
defsym!(ERROR);
defsym!(DEBUG);

defvar!(DEBUG_ON_ERROR, false);
