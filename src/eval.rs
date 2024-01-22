//! Lisp evaluation primitives.
use std::fmt::{Display, Formatter};

use crate::core::cons::{Cons, ConsError};
use crate::core::env::{sym, ArgSlice, CallFrame, Env};
use crate::core::error::{ArgError, Type, TypeError};
use crate::core::gc::Rt;
use crate::core::object::{display_slice, FnArgs, LispString, Object, Symbol, NIL};
use crate::core::{
    gc::Context,
    object::{Function, Gc, GcObj},
};
use crate::fns::{assq, eq};
use anyhow::{anyhow, bail, ensure, Result};
use fallible_iterator::FallibleIterator;
use fallible_streaming_iterator::FallibleStreamingIterator;
use rune_core::macros::{bail_err, list, root, rooted_iter};
use rune_macros::defun;

#[derive(Debug)]
pub(crate) struct EvalError {
    backtrace: Vec<Box<str>>,
    pub(crate) error: ErrorType,
}

#[derive(Debug)]
pub(crate) enum ErrorType {
    Throw(u32),
    Signal(u32),
    Err(anyhow::Error),
}

impl std::error::Error for EvalError {}

impl Display for EvalError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.error {
            ErrorType::Err(e) => writeln!(f, "{e}")?,
            ErrorType::Throw(_) => writeln!(f, "No catch for throw")?,
            ErrorType::Signal(_) => writeln!(f, "Signal")?,
        }
        Ok(())
    }
}

impl EvalError {
    pub(crate) fn new_error(error: anyhow::Error) -> Self {
        Self { backtrace: Vec::new(), error: ErrorType::Err(error) }
    }

    pub(crate) fn signal(error_symbol: GcObj, data: GcObj, env: &mut Rt<Env>) -> Self {
        Self {
            backtrace: Vec::new(),
            error: ErrorType::Signal(env.set_exception(error_symbol, data)),
        }
    }

    pub(crate) fn throw(tag: GcObj, data: GcObj, env: &mut Rt<Env>) -> Self {
        Self { backtrace: Vec::new(), error: ErrorType::Throw(env.set_exception(tag, data)) }
    }

    pub(crate) fn new(error: impl Into<Self>) -> Self {
        error.into()
    }

    pub(crate) fn with_trace(error: anyhow::Error, name: &str, args: &[Rt<GcObj>]) -> Self {
        let display = display_slice(args);
        let trace = format!("{name} {display}").into_boxed_str();
        Self { backtrace: vec![trace], error: ErrorType::Err(error) }
    }

    pub(crate) fn add_trace(mut self, name: &str, args: &[Rt<GcObj>]) -> Self {
        let display = display_slice(args);
        self.backtrace.push(format!("{name} {display}").into_boxed_str());
        self
    }

    pub(crate) fn print_backtrace(&self) {
        println!("BEGIN_BACKTRACE");
        for (i, x) in self.backtrace.iter().enumerate() {
            println!("{i}: {x}");
        }
        println!("END_BACKTRACE");
    }
}

impl From<anyhow::Error> for EvalError {
    fn from(e: anyhow::Error) -> Self {
        Self::new_error(e)
    }
}

impl From<ConsError> for EvalError {
    fn from(e: ConsError) -> Self {
        Self::new_error(anyhow::anyhow!(e))
    }
}

impl From<String> for EvalError {
    fn from(e: String) -> Self {
        Self::new_error(anyhow::anyhow!(e))
    }
}

impl From<&'static str> for EvalError {
    fn from(e: &'static str) -> Self {
        Self::new_error(anyhow::anyhow!(e))
    }
}

impl From<TypeError> for EvalError {
    fn from(e: TypeError) -> Self {
        Self::new_error(e.into())
    }
}

impl From<ArgError> for EvalError {
    fn from(e: ArgError) -> Self {
        Self::new_error(e.into())
    }
}

impl From<std::convert::Infallible> for EvalError {
    fn from(e: std::convert::Infallible) -> Self {
        Self::new_error(e.into())
    }
}

pub(crate) type EvalResult<'ob> = Result<GcObj<'ob>, EvalError>;

#[defun]
pub(crate) fn apply<'ob>(
    function: &Rt<Gc<Function>>,
    arguments: ArgSlice,
    env: &mut Rt<Env>,
    cx: &'ob mut Context,
) -> Result<GcObj<'ob>> {
    let arg_slice = env.stack.arg_slice(arguments);
    if !arg_slice.is_empty() {
        let last = arg_slice.last().unwrap().bind(cx);
        let len = env.stack.len();
        let beg = len - arg_slice.len();
        let end = len - 1;
        env.stack.extend_as_vec_from_within(beg..end);
        for element in last.as_list()? {
            let e = cx.bind(element?);
            env.stack.push(e);
        }
        let args = env.stack.len() - len;
        let frame = &mut CallFrame::new_with_args(env, args);
        function.call(frame, None, cx).map_err(Into::into)
    } else {
        function.call(&mut CallFrame::new(env), None, cx).map_err(Into::into)
    }
}

#[defun]
pub(crate) fn funcall<'ob>(
    function: &Rt<Gc<Function>>,
    arguments: ArgSlice,
    env: &mut Rt<Env>,
    cx: &'ob mut Context,
) -> Result<GcObj<'ob>> {
    let beg = env.stack.len() - arguments.len();
    env.stack.extend_as_vec_from_within(beg..);
    let frame = &mut CallFrame::new_with_args(env, arguments.len());
    function.call(frame, None, cx).map_err(Into::into)
}

#[defun]
fn run_hooks<'ob>(hooks: ArgSlice, env: &mut Rt<Env>, cx: &'ob mut Context) -> Result<GcObj<'ob>> {
    let hook_count = hooks.len();
    for i in 0..hook_count {
        let hook = env.stack[hook_count - i - 1].bind(cx);
        match hook.untag() {
            Object::Symbol(sym) => {
                if let Some(val) = env.vars.get(sym) {
                    let val = val.bind(cx);
                    match val.untag() {
                        Object::Cons(hook_list) => {
                            rooted_iter!(hooks, hook_list, cx);
                            while let Some(hook) = hooks.next()? {
                                let func: &Rt<Gc<Function>> = hook.try_into()?;
                                func.call(&mut CallFrame::new(env), None, cx)?;
                            }
                        }
                        Object::NIL => {}
                        _ => {
                            let func: Gc<Function> = val.try_into()?;
                            root!(func, cx);
                            func.call(&mut CallFrame::new(env), None, cx)?;
                        }
                    }
                }
            }
            x => bail!(TypeError::new(Type::Symbol, x)),
        }
    }
    Ok(NIL)
}

#[defun]
fn run_hook_with_args<'ob>(
    hook: &Rt<GcObj>,
    args: ArgSlice,
    env: &mut Rt<Env>,
    cx: &'ob mut Context,
) -> Result<GcObj<'ob>> {
    match hook.untag(cx) {
        Object::Symbol(sym) => {
            if let Some(val) = env.vars.get(sym) {
                let val = val.bind(cx);
                match val.untag() {
                    Object::Cons(hook_list) => {
                        rooted_iter!(hooks, hook_list, cx);
                        while let Some(hook) = hooks.next()? {
                            let func: &Rt<Gc<Function>> = hook.try_into()?;
                            let beg = env.stack.len() - args.len();
                            env.stack.extend_as_vec_from_within(beg..);
                            let frame = &mut CallFrame::new_with_args(env, args.len());
                            func.call(frame, None, cx)?;
                        }
                    }
                    Object::NIL => {}
                    _ => {
                        let func: Gc<Function> = val.try_into()?;
                        root!(func, cx);
                        func.call(&mut CallFrame::new(env), None, cx)?;
                    }
                }
            }
        }
        x => bail!(TypeError::new(Type::Symbol, x)),
    }
    Ok(NIL)
}

#[defun]
pub(crate) fn autoload_do_load<'ob>(
    fundef: &Rt<GcObj>,
    funname: Option<&Rt<Gc<Symbol>>>,
    macro_only: Option<&Rt<GcObj>>,
    env: &mut Rt<Env>,
    cx: &'ob mut Context,
) -> Result<GcObj<'ob>> {
    // TODO: want to handle the case where the file is already loaded.
    match fundef.untag(cx) {
        Object::Cons(cons) if cons.car() == sym::AUTOLOAD => {
            ensure!(macro_only.is_none(), "autoload-do-load macro-only is not yet implemented");
            let mut iter = cons.elements();
            iter.next(); // autoload
            let file: Gc<&LispString> = match iter.next() {
                Some(x) => x?.try_into()?,
                None => bail!("Malformed autoload"),
            };
            ensure!(
                iter.fallible().all(|x| Ok(x.is_nil()))?,
                "autoload arguments are not yet implemented"
            );
            root!(file, cx);
            crate::lread::load(file, None, None, cx, env)?;
            match funname {
                Some(func) => match func.untag(cx).func(cx) {
                    Some(x) => Ok(x.into()),
                    None => Err(anyhow!("autoload of {func} did not provide a definition")),
                },
                _ => Ok(NIL),
            }
        }
        _ => Ok(fundef.bind(cx)),
    }
}

#[defun]
fn autoload<'ob>(
    function: Symbol<'ob>,
    file: &str,
    docstring: Option<GcObj>,
    interactive: Option<GcObj>,
    load_type: Option<GcObj>,
    cx: &'ob Context,
) -> Result<Symbol<'ob>> {
    if function.has_func() {
        Ok(sym::NIL)
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
    env: &mut Rt<Env>,
) -> Result<GcObj<'ob>> {
    let Object::Cons(cons) = form.untag(cx) else { return Ok(form.bind(cx)) };
    let Object::Symbol(sym) = cons.car().untag() else { return Ok(form.bind(cx)) };
    // shadow the macro based on ENVIRONMENT
    let func = match environment {
        Some(env) => match assq(sym.into(), env.bind(cx).try_into()?)?.untag() {
            Object::Cons(cons) => Some(cons.cdr().try_into()?),
            _ => get_macro_func(sym, cx),
        },
        _ => get_macro_func(sym, cx),
    };
    let Some(macro_func) = func else { return Ok(form.bind(cx)) };
    let mut iter = cons.cdr().as_list()?.fallible();
    let mut frame = CallFrame::new(env);
    while let Some(arg) = iter.next()? {
        frame.push_arg(arg);
    }
    root!(macro_func, cx);
    let name = sym.name().to_owned();
    let new_form = macro_func.call(&mut frame, Some(&name), cx)?;
    drop(frame);
    root!(new_form, cx); // polonius
    if eq(new_form.bind(cx), form.bind(cx)) {
        Ok(form.bind(cx))
    } else {
        // recursively expand the macro's
        macroexpand(new_form, environment, cx, env)
    }
}

fn get_macro_func<'ob>(name: Symbol, cx: &'ob Context) -> Option<Gc<Function<'ob>>> {
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
fn func_arity<'ob>(function: Gc<Function>, cx: &'ob Context) -> Result<&'ob Cons> {
    let from_args = |args: FnArgs| {
        let min = args.required;
        if args.rest {
            // TODO: Handle unevalled
            Cons::new(min, sym::MANY, cx)
        } else {
            Cons::new(min, args.optional + min, cx)
        }
    };
    match function.untag() {
        Function::ByteFn(func) => Ok(from_args(func.args)),
        Function::SubrFn(func) => Ok(from_args(func.args)),
        Function::Cons(func) => {
            let arg_pos = match func.car().untag() {
                Object::Symbol(sym::CLOSURE) => 2,
                Object::Symbol(sym::LAMBDA) => 1,
                other => bail!(TypeError::new(Type::Func, other)),
            };
            let Some(args) = func.elements().fallible().nth(arg_pos)? else {
                bail!("Invalid function: {func}")
            };
            let (req, opt, rest) = crate::interpreter::parse_arg_list(args)?;
            let args = FnArgs {
                required: req.len() as u16,
                optional: opt.len() as u16,
                rest: rest.is_some(),
                ..FnArgs::default()
            };
            Ok(from_args(args))
        }
        Function::Symbol(sym) => {
            let Some(func) = sym.follow_indirect(cx) else { bail!("Void Function: {sym}") };
            func_arity(func, cx)
        }
    }
}

#[defun]
#[allow(non_snake_case)]
fn internal__define_uninitialized_variable<'ob>(
    _symbol: Symbol<'ob>,
    _doc: Option<GcObj>,
) -> GcObj<'ob> {
    // TODO: implement doc strings
    NIL
}

#[defun]
fn signal(mut error_symbol: GcObj, data: GcObj, env: &mut Rt<Env>) -> Result<bool> {
    if error_symbol.is_nil() && data.is_nil() {
        error_symbol = sym::ERROR.into();
    }
    Err(EvalError::signal(error_symbol, data, env).into())
}

#[defun]
fn special_variable_p(symbol: Symbol) -> bool {
    symbol.is_special()
}

#[defun]
fn set_default_toplevel_value<'ob>(
    symbol: Symbol,
    value: GcObj,
    env: &'ob mut Rt<Env>,
) -> Result<GcObj<'ob>> {
    env.set_var(symbol, value)?;
    Ok(NIL)
}

#[defun]
fn set_default<'ob>(
    symbol: Symbol,
    value: GcObj<'ob>,
    env: &'ob mut Rt<Env>,
) -> Result<GcObj<'ob>> {
    // TODO: implement buffer local variables
    env.set_var(symbol, value)?;
    Ok(value)
}

impl Rt<Gc<Function<'_>>> {
    pub(crate) fn call<'ob>(
        &self,
        frame: &mut CallFrame<'_, '_>,
        name: Option<&str>,
        cx: &'ob mut Context,
    ) -> EvalResult<'ob> {
        cx.garbage_collect(false);
        let name = name.unwrap_or("lambda");
        frame.finalize_arguments();
        let arg_cnt = frame.arg_count();
        debug!("calling {self:?}");
        match self.untag(cx) {
            Function::ByteFn(f) => {
                root!(f, cx);
                crate::bytecode::call(f, arg_cnt, name, frame, cx)
                    .map_err(|e| e.add_trace(name, frame.arg_slice()))
            }
            Function::SubrFn(f) => {
                (*f).call(arg_cnt, frame, cx).map_err(|e| add_trace(e, name, frame.arg_slice()))
            }
            Function::Cons(_) => {
                crate::interpreter::call_closure(self.try_into().unwrap(), arg_cnt, name, frame, cx)
                    .map_err(|e| e.add_trace(name, frame.arg_slice()))
            }
            Function::Symbol(sym) => {
                let Some(func) = sym.follow_indirect(cx) else { bail_err!("Void Function: {sym}") };
                match func.untag() {
                    Function::Cons(cons) if cons.car() == sym::AUTOLOAD => {
                        // TODO: inifinite loop if autoload does not resolve
                        root!(sym, cx);
                        crate::eval::autoload_do_load(self.use_as(), None, None, frame, cx)
                            .map_err(|e| add_trace(e, name, frame.arg_slice()))?;
                        let Some(func) = sym.bind(cx).follow_indirect(cx) else {
                            bail_err!("autoload for {sym} failed to define function")
                        };
                        root!(func, cx);
                        let name = sym.bind(cx).name().to_owned();
                        func.call(frame, Some(&name), cx)
                    }
                    _ => {
                        root!(func, cx);
                        let name = sym.name().to_owned();
                        func.call(frame, Some(&name), cx)
                    }
                }
            }
        }
    }
}

pub(crate) fn add_trace(err: anyhow::Error, name: &str, args: &[Rt<GcObj>]) -> EvalError {
    match err.downcast::<EvalError>() {
        Ok(err) => err.add_trace(name, args),
        Err(e) => EvalError::with_trace(e, name, args),
    }
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
defsym!(SAVE_EXCURSION);
defsym!(SAVE_CURRENT_BUFFER);
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
defsym!(VOID_VARIABLE);

defvar!(DEBUG_ON_ERROR, false);
defvar!(INTERNAL_MAKE_INTERPRETED_CLOSURE_FUNCTION);
