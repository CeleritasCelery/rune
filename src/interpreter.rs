use std::fmt::Display;

use crate::core::{
    cons::{Cons, ElemStreamIter},
    env::{sym, Env, Symbol},
    error::{ArgError, Type, TypeError},
    gc::{Context, Root, Rt},
    object::{Function, Gc, GcObj, List, Object},
};
use crate::{root, rooted_iter};
use anyhow::Context as _;
use anyhow::Result as AnyResult;
use anyhow::{anyhow, bail, ensure};
use fn_macros::defun;
use streaming_iterator::StreamingIterator;

#[derive(Debug)]
pub(crate) struct EvalError {
    backtrace: Vec<String>,
    error: Option<anyhow::Error>,
}

impl std::error::Error for EvalError {}

impl Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(e) = &self.error {
            writeln!(f, "{e}")?;
        }
        for x in &self.backtrace {
            writeln!(f, "{x}")?;
        }
        Ok(())
    }
}

impl EvalError {
    fn new_error(error: anyhow::Error) -> Self {
        Self {
            backtrace: Vec::new(),
            error: Some(error),
        }
    }

    fn new(error: impl Into<Self>) -> Self {
        error.into()
    }

    fn with_trace(error: anyhow::Error, name: &str, args: &[Rt<GcObj>]) -> Self {
        Self {
            backtrace: vec![format!("{name} {args:?}")],
            error: Some(error),
        }
    }

    fn add_trace(mut self, name: &str, args: &[Rt<GcObj>]) -> Self {
        self.backtrace.push(format!("{name} {args:?}"));
        self
    }
}

impl From<anyhow::Error> for EvalError {
    fn from(e: anyhow::Error) -> Self {
        Self::new_error(e)
    }
}

impl From<String> for EvalError {
    fn from(e: String) -> Self {
        Self::new_error(anyhow!(e))
    }
}

impl From<&'static str> for EvalError {
    fn from(e: &'static str) -> Self {
        Self::new_error(anyhow!(e))
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

macro_rules! error {
    ($msg:literal $(,)?  $($args:expr),* $(,)?) => (EvalError::new_error(anyhow!($msg, $($args),*)));
    ($err:expr) => (EvalError::new($err));
}

macro_rules! bail_err {
    ($($args:expr),* $(,)?) => (return Err(error!($($args),*)));
}

type EvalResult<'ob> = Result<GcObj<'ob>, EvalError>;

struct Interpreter<'brw, '_1, '_2, '_3, '_4> {
    vars: &'brw mut Root<'_1, '_3, Vec<&'static Cons>>,
    env: &'brw mut Root<'_2, '_4, Env>,
}

#[defun]
pub(crate) fn eval<'ob>(
    form: &Rt<GcObj>,
    _lexical: Option<()>,
    env: &mut Root<Env>,
    cx: &'ob mut Context,
) -> Result<GcObj<'ob>, anyhow::Error> {
    cx.garbage_collect(false);
    root!(vars, Vec::new(), cx);
    let mut interpreter = Interpreter { vars, env };
    interpreter.eval_form(form, cx).map_err(Into::into)
}

impl Interpreter<'_, '_, '_, '_, '_> {
    fn eval_form<'ob>(&mut self, rt: &Rt<GcObj>, cx: &'ob mut Context) -> EvalResult<'ob> {
        match rt.get(cx) {
            Object::Symbol(sym) => self.var_ref(sym, cx),
            Object::Cons(_) => {
                let x = rt.try_as().unwrap();
                self.eval_sexp(x, cx)
            }
            _ => Ok(rt.bind(cx)),
        }
    }

    pub(crate) fn eval_sexp<'ob>(
        &mut self,
        cons: &Rt<Gc<&Cons>>,
        cx: &'ob mut Context,
    ) -> EvalResult<'ob> {
        let cons = cons.bind(cx);
        let forms = cons.cdr();
        root!(forms, cx);
        match cons.car().get() {
            Object::Symbol(sym) => match sym.sym {
                sym::QUOTE => self.quote(forms.bind(cx)),
                sym::LET => self.eval_let(forms, true, cx),
                sym::LET_STAR => self.eval_let(forms, false, cx),
                sym::IF => self.eval_if(forms, cx),
                sym::AND => self.eval_and(forms, cx),
                sym::OR => self.eval_or(forms, cx),
                sym::COND => self.eval_cond(forms, cx),
                sym::WHILE => self.eval_while(forms, cx),
                sym::PROGN => self.eval_progn(forms, cx),
                sym::PROG1 => self.eval_progx(forms, 1, cx),
                sym::PROG2 => self.eval_progx(forms, 2, cx),
                sym::SETQ => self.setq(forms, cx),
                sym::DEFVAR | sym::DEFCONST => self.defvar(forms, cx),
                sym::FUNCTION => self.eval_function(forms.bind(cx), cx),
                sym::INTERACTIVE => Ok(GcObj::NIL), // TODO: implement
                sym::CATCH => self.catch(forms, cx),
                sym::THROW => self.throw(forms.bind(cx), cx),
                sym::CONDITION_CASE => self.condition_case(forms, cx),
                _ => self.eval_call(sym, forms, cx),
            },
            other => Err(error!("Invalid Function: {other}")),
        }
    }

    fn catch<'ob>(&mut self, obj: &Rt<GcObj>, cx: &'ob mut Context) -> EvalResult<'ob> {
        rooted_iter!(forms, obj, cx);
        let tag = forms.next().ok_or_else(|| ArgError::new(1, 0, "catch"))?;
        // push this tag on the catch stack
        self.env.deref_mut(cx).catch_stack.push(tag);
        let result = match self.implicit_progn(forms, cx) {
            Ok(x) => Ok(rebind!(x, cx)),
            Err(e) => {
                let tag = self.env.catch_stack.last().unwrap();
                if e.error.is_none() && *tag == self.env.thrown.0 {
                    Ok(self.env.thrown.1.bind(cx))
                } else {
                    // Either this was not a throw or the tag does not match
                    // this catch block
                    Err(e)
                }
            }
        };
        // pop this tag from the catch stack
        self.env.deref_mut(cx).catch_stack.pop();
        result
    }

    fn throw<'ob>(&mut self, obj: GcObj, cx: &'ob Context) -> EvalResult<'ob> {
        let mut forms = obj.as_list()?;
        let len = forms.len() as u16;
        if len != 2 {
            bail_err!(ArgError::new(2, len, "throw"));
        }
        let tag = forms.next().unwrap()?;
        let value = forms.next().unwrap()?;
        let env = self.env.deref_mut(cx);
        if env.catch_stack.iter().any(|x| x.bind(cx) == tag) {
            env.thrown.0.set(tag);
            env.thrown.1.set(value);
            // a None error means this a throw
            Err(EvalError {
                error: None,
                backtrace: Vec::new(),
            })
        } else {
            Err(error!("No catch for {tag}"))
        }
    }

    fn defvar<'ob>(&mut self, obj: &Rt<GcObj>, cx: &'ob mut Context) -> EvalResult<'ob> {
        rooted_iter!(forms, obj, cx);
        match forms.next() {
            // (defvar x ...)
            Some(x) => {
                let name: Symbol = x.bind(cx).try_into()?;
                let value = match forms.next() {
                    // (defvar x y)
                    Some(value) => rebind!(self.eval_form(value, cx)?, cx),
                    // (defvar x)
                    None => GcObj::NIL,
                };
                self.var_set(name, value, cx);
                Ok(value)
            }
            // (defvar)
            None => Err(ArgError::new(1, 0, "defvar").into()),
        }
    }

    fn eval_call<'ob>(
        &mut self,
        name: Symbol,
        args: &Rt<GcObj>,
        cx: &'ob mut Context,
    ) -> EvalResult<'ob> {
        let func = match name.follow_indirect(cx) {
            Some(x) => x,
            None => bail_err!("Invalid function: {name}"),
        };

        if let Function::Cons(form) = func.get() {
            if let Ok(mcro) = form.try_as_macro() {
                let macro_args = args.bind(cx).as_list()?.collect::<AnyResult<Vec<_>>>()?;
                root!(args, move(macro_args), cx);
                root!(mcro, cx);
                let value = mcro.call(args, self.env, cx, Some(name.name))?;
                root!(value, cx);
                return self.eval_form(value, cx);
            }
        }

        rooted_iter!(iter, args, cx);
        root!(func, cx);
        root!(args, Vec::new(), cx);
        while let Some(x) = iter.next() {
            let result = rebind!(self.eval_form(x, cx)?, cx);
            args.deref_mut(cx).push(result);
        }
        func.call(args, self.env, cx, Some(name.name))
    }

    fn eval_function<'ob>(&mut self, obj: GcObj<'ob>, cx: &'ob Context) -> EvalResult<'ob> {
        let mut forms = obj.as_list()?;
        let len = forms.len() as u16;
        if len != 1 {
            bail_err!(ArgError::new(1, len, "function"))
        }

        let form = forms.next().unwrap()?;
        match form.get() {
            Object::Cons(cons) => {
                if cons.car() == sym::LAMBDA {
                    let env = {
                        // TODO: remove temp vector
                        let env: Vec<_> = self.vars.iter().map(|x| x.bind(cx).into()).collect();
                        crate::fns::slice_into_list(env.as_slice(), Some(cons!(true; cx)), cx)
                    };
                    let end = cons!(env, cons.cdr(); cx);
                    Ok(cons!(sym::CLOSURE, end; cx))
                } else {
                    Ok(cons.into())
                }
            }
            _ => Ok(form),
        }
    }

    fn eval_progx<'ob>(
        &mut self,
        obj: &Rt<GcObj>,
        prog_num: u16,
        cx: &'ob mut Context,
    ) -> EvalResult<'ob> {
        let mut count = 0;
        root!(returned_form, None, cx);
        rooted_iter!(forms, obj, cx);
        while let Some(form) = forms.next() {
            let value = rebind!(self.eval_form(form, cx)?, cx);
            count += 1;
            if prog_num == count {
                returned_form.deref_mut(cx).set(value);
            }
        }
        match &***returned_form {
            Some(x) => Ok(x.bind(cx)),
            None => {
                let name = match prog_num {
                    1 => "prog1",
                    2 => "prog2",
                    _ => "progn",
                };
                Err(ArgError::new(prog_num, count, name).into())
            }
        }
    }

    fn eval_progn<'ob>(&mut self, obj: &Rt<GcObj>, cx: &'ob mut Context) -> EvalResult<'ob> {
        rooted_iter!(forms, obj, cx);
        self.implicit_progn(forms, cx)
    }

    fn eval_while<'ob>(&mut self, obj: &Rt<GcObj>, cx: &'ob mut Context) -> EvalResult<'ob> {
        let first: Gc<List> = obj.bind(cx).try_into()?;
        let condition = match first.get() {
            List::Cons(cons) => cons.car(),
            List::Nil => bail_err!(ArgError::new(1, 0, "while")),
        };
        root!(condition, cx);
        while self.eval_form(condition, cx)? != GcObj::NIL {
            rooted_iter!(forms, obj, cx);
            self.implicit_progn(forms, cx)?;
        }
        Ok(GcObj::NIL)
    }

    fn eval_cond<'ob>(&mut self, obj: &Rt<GcObj>, cx: &'ob mut Context) -> EvalResult<'ob> {
        rooted_iter!(forms, obj, cx);
        while let Some(form) = forms.next() {
            rooted_iter!(clause, form, cx);
            if let Some(first) = clause.next() {
                let condition = self.eval_form(first, cx)?;
                if condition != GcObj::NIL {
                    return if clause.is_empty() {
                        Ok(rebind!(condition, cx))
                    } else {
                        self.implicit_progn(clause, cx)
                    };
                }
            }
        }
        Ok(GcObj::NIL)
    }

    fn eval_and<'ob>(&mut self, obj: &Rt<GcObj>, cx: &'ob mut Context) -> EvalResult<'ob> {
        root!(last, GcObj::TRUE, cx);
        rooted_iter!(forms, obj, cx);
        while let Some(form) = forms.next() {
            let result = rebind!(self.eval_form(form, cx)?, cx);
            if result == GcObj::NIL {
                return Ok(GcObj::NIL);
            }
            last.deref_mut(cx).set(result);
        }
        Ok(last.bind(cx))
    }

    fn eval_or<'ob>(&mut self, obj: &Rt<GcObj>, cx: &'ob mut Context) -> EvalResult<'ob> {
        rooted_iter!(forms, obj, cx);
        while let Some(form) = forms.next() {
            let result = self.eval_form(form, cx)?;
            if result != GcObj::NIL {
                return Ok(rebind!(result, cx));
            }
        }
        Ok(GcObj::NIL)
    }

    fn eval_if<'ob>(&mut self, obj: &Rt<GcObj>, cx: &'ob mut Context) -> EvalResult<'ob> {
        rooted_iter!(forms, obj, cx);
        let condition = forms.next().ok_or_else(|| ArgError::new(2, 0, "if"))?;
        root!(condition, cx);
        let true_branch = forms.next().ok_or_else(|| ArgError::new(2, 1, "if"))?;
        root!(true_branch, cx);
        #[allow(clippy::if_not_else)]
        if self.eval_form(condition, cx)? != GcObj::NIL {
            self.eval_form(true_branch, cx)
        } else {
            self.implicit_progn(forms, cx)
        }
    }

    fn setq<'ob>(&mut self, obj: &Rt<GcObj>, cx: &'ob mut Context) -> EvalResult<'ob> {
        rooted_iter!(forms, obj, cx);
        let mut arg_cnt = 0;
        root!(last_value, GcObj::NIL, cx);
        while let Some((var, val)) = Self::pairs(&mut forms, cx) {
            match (var.get(), val) {
                (Object::Symbol(var), Some(val)) => {
                    root!(val, cx);
                    let val = rebind!(self.eval_form(val, cx)?, cx);
                    self.var_set(var, val, cx);
                    last_value.deref_mut(cx).set(val);
                }
                (_, Some(_)) => bail_err!(TypeError::new(Type::Symbol, var)),
                (_, None) => bail_err!(ArgError::new(arg_cnt, arg_cnt + 1, "setq")),
            }
            arg_cnt += 2;
        }
        if arg_cnt < 2 {
            Err(ArgError::new(2, 0, "setq").into())
        } else {
            Ok(last_value.bind(cx))
        }
    }

    fn pairs<'ob>(
        iter: &mut ElemStreamIter<'_, '_>,
        cx: &'ob Context,
    ) -> Option<(GcObj<'ob>, Option<GcObj<'ob>>)> {
        #[allow(clippy::manual_map)]
        if let Some(first) = iter.next() {
            Some((first.bind(cx), iter.next().map(|x| x.bind(cx))))
        } else {
            None
        }
    }

    fn var_ref<'ob>(&self, sym: Symbol, cx: &'ob Context) -> EvalResult<'ob> {
        if sym.name.starts_with(':') {
            Ok(sym.into())
        } else {
            let mut iter = self.vars.iter().rev();
            match iter.find_map(|cons| (cons.car() == sym).then(|| cons.cdr())) {
                Some(value) => Ok(value.bind(cx)),
                None => match self.env.vars.get(sym) {
                    Some(v) => Ok(v.bind(cx)),
                    None => Err(error!("Void variable: {sym}")),
                },
            }
        }
    }

    fn var_set(&mut self, name: Symbol, new_value: GcObj, cx: &Context) {
        let mut iter = self.vars.iter().rev();
        match iter.find(|cons| (cons.car() == name)) {
            Some(value) => {
                value
                    .bind(cx)
                    .set_cdr(new_value)
                    .expect("variables should never be immutable");
            }
            None => {
                self.env.deref_mut(cx).set_var(name, new_value);
            }
        }
    }

    #[allow(clippy::unused_self)]
    fn quote<'ob>(&self, value: GcObj<'ob>) -> EvalResult<'ob> {
        let mut forms = value.as_list()?;
        match forms.len() {
            1 => Ok(forms.next().unwrap()?),
            x => Err(ArgError::new(1, x as u16, "quote").into()),
        }
    }

    fn eval_let<'ob>(
        &mut self,
        form: &Rt<GcObj>,
        parallel: bool,
        cx: &'ob mut Context,
    ) -> EvalResult<'ob> {
        rooted_iter!(iter, form, cx);
        let prev_len = self.vars.len();
        root!(dynamic_bindings, Vec::new(), cx);
        match iter.next() {
            // (let x ...)
            Some(x) => {
                let obj = x;
                if parallel {
                    self.let_bind_parallel(obj, dynamic_bindings, cx)?;
                } else {
                    self.let_bind_serial(obj, dynamic_bindings, cx)?;
                }
            }
            // (let)
            None => bail_err!(ArgError::new(1, 0, "let")),
        }
        let obj = rebind!(self.implicit_progn(iter, cx)?, cx);
        // Remove old bindings
        self.vars.deref_mut(cx).truncate(prev_len);
        for binding in dynamic_bindings.iter() {
            let var: Symbol = &binding.0;
            let val = &binding.1;

            if let Some(current_val) = self.env.deref_mut(cx).vars.get_mut(var) {
                current_val.set(val);
            } else {
                unreachable!("Variable {var} not longer exists");
            }
        }
        Ok(obj)
    }

    fn let_bind_serial(
        &mut self,
        form: &Rt<GcObj>,
        dynamic_bindings: &mut Root<Vec<(Symbol, GcObj<'static>)>>,
        cx: &mut Context,
    ) -> Result<(), EvalError> {
        rooted_iter!(bindings, form, cx);
        while let Some(binding) = bindings.next() {
            let (var, val) = match binding.get(cx) {
                // (let ((x y)))
                Object::Cons(_) => self.let_bind_value(binding.as_cons(), cx)?,
                // (let (x))
                Object::Symbol(sym) => (sym, GcObj::NIL),
                // (let (1))
                x => bail_err!(TypeError::new(Type::Cons, x)),
            };
            let val = rebind!(val, cx);
            if let Some(current_val) = self.env.deref_mut(cx).vars.get_mut(var) {
                dynamic_bindings.deref_mut(cx).push((var, &*current_val));
                current_val.set(val);
            } else {
                let cons = cons!(var, val; cx).as_cons();
                self.vars.deref_mut(cx).push(cons);
            }
        }
        Ok(())
    }

    fn let_bind_parallel(
        &mut self,
        form: &Rt<GcObj>,
        dynamic_bindings: &mut Root<Vec<(Symbol, GcObj<'static>)>>,
        cx: &mut Context,
    ) -> Result<(), EvalError> {
        root!(let_bindings, Vec::new(), cx);
        rooted_iter!(bindings, form, cx);
        while let Some(binding) = bindings.next() {
            match binding.get(cx) {
                // (let ((x y)))
                Object::Cons(_) => {
                    let (sym, var) = self.let_bind_value(binding.as_cons(), cx)?;
                    let var = rebind!(var, cx);
                    let_bindings.deref_mut(cx).push((sym, var));
                }
                // (let (x))
                Object::Symbol(sym) => {
                    let_bindings.deref_mut(cx).push((sym, GcObj::NIL));
                }
                // (let (1))
                x => bail_err!(TypeError::new(Type::Cons, x)),
            }
        }
        for binding in let_bindings.iter() {
            let var: Symbol = &binding.0;
            let val = &binding.1;
            if let Some(current_val) = self.env.deref_mut(cx).vars.get_mut(var) {
                dynamic_bindings.deref_mut(cx).push((var, &*current_val));
                current_val.set(val);
            } else {
                let cons = cons!(var, val; cx).as_cons();
                self.vars.deref_mut(cx).push(cons);
            }
        }
        Ok(())
    }

    fn let_bind_value<'ob>(
        &mut self,
        cons: &Rt<Gc<&Cons>>,
        cx: &'ob mut Context,
    ) -> Result<(Symbol, GcObj<'ob>), EvalError> {
        rooted_iter!(iter, cons.bind(cx).cdr(), cx);
        let value = match iter.next() {
            // (let ((x y)))
            Some(x) => rebind!(self.eval_form(x, cx)?, cx),
            // (let ((x)))
            None => GcObj::NIL,
        };
        // (let ((x y z ..)))
        if !iter.is_empty() {
            bail_err!("Let binding can only have 1 value");
        }
        let name: Symbol = cons
            .bind(cx)
            .car()
            .try_into()
            .context("let variable must be a symbol")?;
        Ok((name, value))
    }

    fn implicit_progn<'ob>(
        &mut self,
        mut forms: ElemStreamIter<'_, '_>,
        cx: &'ob mut Context,
    ) -> EvalResult<'ob> {
        root!(last, GcObj::NIL, cx);
        while let Some(form) = forms.next() {
            let value = rebind!(self.eval_form(form, cx)?, cx);
            last.deref_mut(cx).set(value);
        }
        Ok(last.bind(cx))
    }

    fn condition_case<'ob>(&mut self, form: &Rt<GcObj>, cx: &'ob mut Context) -> EvalResult<'ob> {
        rooted_iter!(forms, form, cx);
        let var = match forms.next() {
            Some(x) => x.bind(cx),
            None => bail_err!(ArgError::new(2, 0, "condition-case")),
        };
        root!(var, cx);
        let bodyform = match forms.next() {
            Some(x) => x,
            None => bail_err!(ArgError::new(2, 1, "condition-case")),
        };
        match self.eval_form(bodyform, cx) {
            Ok(x) => Ok(rebind!(x, cx)),
            Err(e) => {
                const CONDITION_ERROR: &str = "Invalid condition handler:";
                if e.error.is_none() {
                    // This is a throw
                    return Err(e);
                }
                while let Some(handler) = forms.next() {
                    match handler.get(cx) {
                        Object::Cons(cons) => {
                            // Check that conditions match
                            let condition = cons.car();
                            match condition.get() {
                                Object::Symbol(s) if s == &sym::ERROR => {}
                                Object::Cons(cons) => {
                                    for x in cons.elements() {
                                        let x = x?;
                                        if x != sym::DEBUG && x != sym::ERROR {
                                            bail_err!("non-error conditions {x} not yet supported")
                                        }
                                    }
                                }
                                _ => bail_err!("{CONDITION_ERROR} {condition}"),
                            }
                            // Call handlers with error
                            let binding = list!(var, sym::ERROR, format!("{e}"); cx).as_cons();
                            self.vars.deref_mut(cx).push(binding);
                            let list: Gc<List> = match cons.cdr().try_into() {
                                Ok(x) => x,
                                Err(_) => return Ok(GcObj::NIL),
                            };
                            rooted_iter!(handlers, list, cx);
                            let result = rebind!(self.implicit_progn(handlers, cx)?, cx);
                            self.vars.deref_mut(cx).pop();
                            return Ok(result);
                        }
                        Object::Nil => {}
                        invalid => bail_err!("{CONDITION_ERROR} {invalid}"),
                    }
                }
                Err(e)
            }
        }
    }
}

impl Rt<Gc<Function<'_>>> {
    pub(crate) fn call<'ob>(
        &self,
        args: &mut Root<Vec<GcObj<'static>>>,
        env: &mut Root<Env>,
        cx: &'ob mut Context,
        name: Option<&str>,
    ) -> EvalResult<'ob> {
        let name = name.unwrap_or("lambda");
        match self.bind(cx).get() {
            Function::LispFn(_) => todo!("call lisp functions"),
            Function::SubrFn(f) => {
                (*f).call(args, env, cx)
                    .map_err(|e| match e.downcast::<EvalError>() {
                        Ok(err) => err.add_trace(name, args),
                        Err(e) => EvalError::with_trace(e, name, args),
                    })
            }
            Function::Cons(cons) => {
                root!(cons, cx);
                call_closure(cons, args, name, env, cx).map_err(|e| e.add_trace(name, args))
            }
            Function::Symbol(sym) => {
                if let Some(func) = sym.follow_indirect(cx) {
                    root!(func, cx);
                    func.call(args, env, cx, Some(name))
                } else {
                    Err(error!("Void Function: {}", sym))
                }
            }
        }
    }
}

fn call_closure<'ob>(
    closure: &Rt<&Cons>,
    args: &Root<Vec<GcObj>>,
    name: &str,
    env: &mut Root<Env>,
    cx: &'ob mut Context,
) -> EvalResult<'ob> {
    cx.garbage_collect(false);
    match closure.car().get(cx) {
        Object::Symbol(s) if s == &sym::CLOSURE => {
            rooted_iter!(forms, closure.cdr(), cx);
            // TODO: remove this temp vector
            let args = args.iter().map(|x| x.bind(cx)).collect();
            let vars = bind_variables(&mut forms, args, name, cx)?;
            root!(vars, move(vars), cx);
            Interpreter { vars, env }.implicit_progn(forms, cx)
        }
        other => Err(TypeError::new(Type::Func, other).into()),
    }
}

fn bind_variables<'a>(
    forms: &mut ElemStreamIter<'_, '_>,
    args: Vec<GcObj<'a>>,
    name: &str,
    cx: &'a Context,
) -> AnyResult<Vec<&'a Cons>> {
    // Add closure environment to variables
    // (closure ((x . 1) (y . 2) t) ...)
    //          ^^^^^^^^^^^^^^^^^^^
    let env = forms
        .next()
        .ok_or_else(|| anyhow!("Closure missing environment"))?;
    let mut vars = parse_closure_env(env.bind(cx))?;

    // Add function arguments to variables
    // (closure (t) (x y &rest z) ...)
    //              ^^^^^^^^^^^^^
    let arg_list = forms
        .next()
        .ok_or_else(|| anyhow!("Closure missing argument list"))?;
    bind_args(arg_list.bind(cx), args, &mut vars, name, cx)?;
    Ok(vars)
}

fn parse_closure_env(obj: GcObj) -> AnyResult<Vec<&Cons>> {
    let forms = obj.as_list()?;
    let mut env = Vec::new();
    for form in forms {
        match form?.get() {
            Object::Cons(pair) => {
                env.push(pair);
            }
            Object::True => return Ok(env),
            x => bail!("Invalid closure environment member: {x}"),
        }
    }
    Err(anyhow!("Closure env did not end with `t`"))
}

fn bind_args<'a>(
    arg_list: GcObj,
    args: Vec<GcObj<'a>>,
    vars: &mut Vec<&'a Cons>,
    name: &str,
    cx: &'a Context,
) -> AnyResult<()> {
    let (required, optional, rest) = parse_arg_list(arg_list)?;

    let num_required_args = required.len() as u16;
    let num_optional_args = optional.len() as u16;
    let num_actual_args = args.len() as u16;
    // Ensure the minimum number of arguments is present
    ensure!(
        num_actual_args >= num_required_args,
        ArgError::new(num_required_args, num_actual_args, name)
    );

    let mut arg_values = args.into_iter();

    for name in required {
        let val = arg_values.next().unwrap();
        vars.push(cons!(name, val; cx).as_cons());
    }

    for name in optional {
        let val = arg_values.next().unwrap_or_default();
        vars.push(cons!(name, val; cx).as_cons());
    }

    if let Some(rest_name) = rest {
        let values = arg_values.as_slice();
        let list = crate::fns::slice_into_list(values, None, cx);
        vars.push(cons!(rest_name, list; cx).as_cons());
    } else {
        // Ensure too many args were not provided
        ensure!(
            arg_values.next().is_none(),
            ArgError::new(num_required_args + num_optional_args, num_actual_args, name)
        );
    }
    Ok(())
}

fn parse_arg_list(bindings: GcObj) -> AnyResult<(Vec<Symbol>, Vec<Symbol>, Option<Symbol>)> {
    let mut required = Vec::new();
    let mut optional = Vec::new();
    let mut rest = None;
    let mut arg_type = &mut required;
    let mut iter = bindings.as_list()?;
    while let Some(binding) = iter.next() {
        let sym: Symbol = binding?.try_into()?;
        match sym.sym {
            sym::AND_OPTIONAL => arg_type = &mut optional,
            sym::AND_REST => {
                if let Some(last) = iter.next() {
                    rest = Some(last?.try_into()?);
                    ensure!(
                        iter.next().is_none(),
                        "Found multiple arguments after &rest"
                    );
                }
            }
            _ => {
                arg_type.push(sym);
            }
        }
    }
    Ok((required, optional, rest))
}

defsubr!(eval);

#[cfg(test)]
mod test {
    use crate::core::{env::intern, gc::RootSet, object::IntoObject};

    use super::*;

    fn check_interpreter<'ob, T>(test_str: &str, expect: T, cx: &'ob mut Context)
    where
        T: IntoObject<'ob>,
    {
        root!(env, Env::default(), cx);
        println!("Test String: {}", test_str);
        let obj = crate::reader::read(test_str, cx).unwrap().0;
        root!(obj, cx);
        let compare = rebind!(eval(obj, None, env, cx).unwrap(), cx);
        let expect: GcObj = expect.into_obj(cx).copy_as_obj();
        assert_eq!(compare, expect);
    }

    fn check_error<'ob>(test_str: &str, cx: &'ob mut Context) {
        root!(env, Env::default(), cx);
        println!("Test String: {}", test_str);
        let obj = crate::reader::read(test_str, cx).unwrap().0;
        root!(obj, cx);
        assert!(eval(obj, None, env, cx).is_err());
    }

    #[test]
    fn basic() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        check_interpreter("1", 1, cx);
        check_interpreter("1.5", 1.5, cx);
        check_interpreter("nil", false, cx);
        check_interpreter("t", true, cx);
        check_interpreter("\"foo\"", "foo", cx);
        let list = list!(1, 2; cx);
        root!(list, cx);
        check_interpreter("'(1 2)", list, cx);
    }

    #[test]
    fn variables() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        check_interpreter("(let ())", false, cx);
        check_interpreter("(let (x) x)", false, cx);
        check_interpreter("(let ((x 1)) x)", 1, cx);
        check_interpreter("(let ((x 1)))", false, cx);
        check_interpreter("(let ((x 1) (y 2)) x y)", 2, cx);
        check_interpreter("(let ((x 1)) (let ((x 3)) x))", 3, cx);
        check_interpreter("(let ((x 1)) (let ((y 3)) x))", 1, cx);
        check_interpreter("(let ((x 1)) (setq x 2) x)", 2, cx);
        check_interpreter("(let* ())", false, cx);
        check_interpreter("(let* ((x 1) (y x)) y)", 1, cx);
    }

    #[test]
    fn dyn_variables() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        check_interpreter("(progn (defvar foo 1) foo)", 1, cx);
        check_interpreter("(progn (defvar foo 1) (let ((foo 3)) foo))", 3, cx);
        check_interpreter("(progn (defvar foo 1) (let ((foo 3))) foo)", 1, cx);
        check_interpreter(
            "(progn (defvar foo 1) (let (bar) (let ((foo 3)) (setq bar foo)) bar))",
            3,
            cx,
        );
    }

    #[test]
    fn conditionals() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        check_interpreter("(if nil 1)", false, cx);
        check_interpreter("(if t 1)", 1, cx);
        check_interpreter("(if nil 1 2)", 2, cx);
        check_interpreter("(if t 1 2)", 1, cx);
        check_interpreter("(if nil 1 2 3)", 3, cx);
        check_interpreter("(and)", true, cx);
        check_interpreter("(and 1)", 1, cx);
        check_interpreter("(and 1 2)", 2, cx);
        check_interpreter("(and 1 nil)", false, cx);
        check_interpreter("(and nil 1)", false, cx);
        check_interpreter("(or)", false, cx);
        check_interpreter("(or nil)", false, cx);
        check_interpreter("(or nil 1)", 1, cx);
        check_interpreter("(or 1 2)", 1, cx);
        check_interpreter("(cond)", false, cx);
        check_interpreter("(cond nil)", false, cx);
        check_interpreter("(cond (1))", 1, cx);
        check_interpreter("(cond (1 2))", 2, cx);
        check_interpreter("(cond (nil 1) (2 3))", 3, cx);
        check_interpreter("(cond (nil 1) (2 3) (4 5))", 3, cx);
    }

    #[test]
    fn special_forms() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        check_interpreter("(prog1 1 2 3)", 1, cx);
        check_interpreter("(prog2 1 2 3)", 2, cx);
        check_interpreter("(progn 1 2 3 4)", 4, cx);
        check_interpreter("(function 1)", 1, cx);
        check_interpreter("(quote 1)", 1, cx);
        check_interpreter("(if 1 2 3)", 2, cx);
        check_interpreter("(if nil 2 3)", 3, cx);
        check_interpreter("(if (and 1 nil) 2 3)", 3, cx);
    }

    #[test]
    fn test_functions() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        let list = list![sym::CLOSURE, list![true; cx]; cx];
        root!(list, cx);
        check_interpreter("(function (lambda))", list, cx);
        let x = intern("x");
        let y = intern("y");
        let list = list![sym::CLOSURE, list![true; cx], list![x; cx], x; cx];
        root!(list, cx);
        check_interpreter("(function (lambda (x) x))", list, cx);
        let list: GcObj =
            list![sym::CLOSURE, list![cons!(y, 1; cx), true; cx], list![x; cx], x; cx];
        root!(list, cx);
        check_interpreter("(let ((y 1)) (function (lambda (x) x)))", list, cx);

        let list = list!(5, false; cx);
        root!(list, cx);
        check_interpreter(
            "(let ((x #'(lambda (x &optional y &rest z) (cons x (cons y z))))) (funcall x 5))",
            list,
            cx,
        );
        let list = list!(5, 7; cx);
        root!(list, cx);
        check_interpreter(
            "(let ((x #'(lambda (x &optional y &rest z) (cons x (cons y z))))) (funcall x 5 7))",
            list,
            cx,
        );
        let list = list!(5, 7, 11; cx);
        root!(list, cx);
        check_interpreter(
            "(let ((x #'(lambda (x &optional y &rest z) (cons x (cons y z))))) (funcall x 5 7 11))",
            list,
            cx,
        );
    }

    #[test]
    fn test_call() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        check_interpreter("(let ((x #'(lambda (x) x))) (funcall x 5))", 5, cx);
        check_interpreter("(let ((x #'(lambda () 3))) (funcall x))", 3, cx);
        check_interpreter(
            "(progn (defvar foo 1) (let ((x #'(lambda () foo)) (foo 5)) (funcall x)))",
            5,
            cx,
        );
        check_interpreter(
            "(progn (defalias 'int-test-call #'(lambda (x) (+ x 3)))  (int-test-call 7))",
            10,
            cx,
        );
        // Test closures
        check_interpreter("(let* ((y 7)(x #'(lambda () y))) (funcall x))", 7, cx);
        check_interpreter(
            "(let* ((y 7)(x #'(lambda (x) (+ x y)))) (funcall x 3))",
            10,
            cx,
        );
        // Test that closures capture their environments
        check_interpreter(
            "(progn (setq func (let ((x 3)) #'(lambda (y) (+ y x)))) (funcall func 5))",
            8,
            cx,
        );
        // Test multiple closures
        check_interpreter("(progn (setq funcs (let ((x 3)) (cons #'(lambda (y) (+ y x)) #'(lambda (y) (- y x))))) (* (funcall (car funcs) 5) (funcall (cdr funcs) 1)))", -16, cx);
        // Test that closures close over variables
        check_interpreter("(progn (setq funcs (let ((x 3)) (cons #'(lambda (y) (setq x y)) #'(lambda (y) (+ y x))))) (funcall (car funcs) 5) (funcall (cdr funcs) 4))", 9, cx);
        // Test that closures in global function close over values and not
        // variables
        check_interpreter("(progn (setq func (let ((x 3)) (defalias 'int-test-no-cap #'(lambda (y) (+ y x))) #'(lambda (y) (setq x y)))) (funcall func 4) (int-test-no-cap 5))", 8, cx);

        check_interpreter(
            "(progn (read-from-string (prin1-to-string (make-hash-table))) nil)",
            false,
            cx,
        );
    }

    #[test]
    fn test_condition_case() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        check_interpreter("(condition-case nil nil)", false, cx);
        check_interpreter("(condition-case nil 1)", 1, cx);
        check_interpreter("(condition-case nil (if) (error 7))", 7, cx);
        check_interpreter("(condition-case nil (if) (error 7 9 11))", 11, cx);
        check_interpreter("(condition-case nil (if) (error . 7))", false, cx);
        check_interpreter("(condition-case nil (if) ((debug error) 7))", 7, cx);
        check_error("(condition-case nil (if))", cx);
        check_error("(condition-case nil (if) nil)", cx);
        check_error("(condition-case nil (if) 5 (error 7))", cx);
    }

    #[test]
    fn test_throw_catch() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        check_interpreter("(catch nil)", false, cx);
        check_interpreter("(catch nil nil)", false, cx);
        check_interpreter("(catch 1 (throw 1 2))", 2, cx);
        check_interpreter("(catch 1 (throw 1 2) 3)", 2, cx);
        check_interpreter("(catch 1 5 (throw 1 2) 3)", 2, cx);
        check_interpreter("(catch 1 (throw 1 2) (if))", 2, cx);
        check_interpreter("(condition-case nil (throw 1 2) (error 3))", 3, cx);
        check_interpreter(
            "(catch 1 (condition-case nil (throw 1 2) (error 3)))",
            2,
            cx,
        );
        check_interpreter("(catch 1 (catch 2 (throw 1 3)))", 3, cx);
        check_error("(throw 1 2)", cx);
        check_error("(catch 2 (throw 3 4))", cx);
    }
}
