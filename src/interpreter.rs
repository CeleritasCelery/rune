use std::fmt::Display;

use crate::core::env::GlobalSymbol as Q;
use crate::core::{
    arena::{Arena, IntoRoot, Root, Rt},
    cons::{Cons, ElemStreamIter},
    env::{sym, Environment, Symbol},
    error::{ArgError, Type, TypeError},
    object::{Callable, Function, Gc, GcObj, List, Object},
};
use crate::{element_iter, rebind, root};
use anyhow::{anyhow, bail, ensure, Context, Result};
use fn_macros::defun;
use streaming_iterator::StreamingIterator;

#[derive(Debug)]
pub(crate) struct EvalError {
    backtrace: Vec<String>,
    error: anyhow::Error,
}

impl std::error::Error for EvalError {}

impl Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let error = &self.error;
        writeln!(f, "{error}")?;
        for x in &self.backtrace {
            writeln!(f, "{x}")?;
        }
        Ok(())
    }
}

impl EvalError {
    fn new(error: anyhow::Error) -> Self {
        Self {
            backtrace: Vec::new(),
            error,
        }
    }

    fn with_trace(error: anyhow::Error, trace: String) -> Self {
        Self {
            backtrace: vec![trace],
            error,
        }
    }
}

impl From<anyhow::Error> for EvalError {
    fn from(e: anyhow::Error) -> Self {
        Self::new(e)
    }
}

impl From<TypeError> for EvalError {
    fn from(e: TypeError) -> Self {
        Self::new(e.into())
    }
}

type LispResult<'ob> = std::result::Result<GcObj<'ob>, EvalError>;

struct Interpreter<'brw, '_1, '_2, '_3, '_4> {
    vars: &'brw mut Root<'_1, '_3, Vec<&'static Cons>>,
    env: &'brw mut Root<'_2, '_4, Environment>,
}

#[defun]
pub(crate) fn eval<'ob>(
    form: &Rt<GcObj>,
    _lexical: Option<()>,
    env: &mut Root<Environment>,
    arena: &'ob mut Arena,
) -> Result<GcObj<'ob>> {
    arena.garbage_collect(false);
    root!(vars, Vec::new(), arena);
    let mut interpreter = Interpreter { vars, env };
    interpreter.eval_form(form, arena)
}

pub(crate) fn call<'gc>(
    form: &Rt<&Cons>,
    args: &mut Root<Vec<GcObj>>,
    env: &mut Root<Environment>,
    name: &str,
    gc: &'gc mut Arena,
) -> Result<GcObj<'gc>> {
    root!(vars, Vec::new(), gc);
    let mut frame = Interpreter { vars, env };
    frame.call_closure(form, args, name, gc)
}

impl Interpreter<'_, '_, '_, '_, '_> {
    fn eval_form<'a, 'gc>(&mut self, rt: &Rt<GcObj<'a>>, gc: &'gc mut Arena) -> Result<GcObj<'gc>> {
        let obj = rt.bind(gc);
        match obj.get() {
            Object::Symbol(sym) => self.var_ref(sym, gc),
            Object::Cons(_) => {
                let x = rt.try_as().unwrap();
                self.eval_sexp(x, gc)
            }
            _ => Ok(gc.bind(obj)),
        }
    }

    pub(crate) fn eval_sexp<'gc>(
        &mut self,
        cons: &Rt<Gc<&Cons>>,
        gc: &'gc mut Arena,
    ) -> Result<GcObj<'gc>> {
        let cons = cons.bind(gc);
        let forms = cons.cdr();
        root!(forms, gc);
        match cons.car().get() {
            Object::Symbol(sym) => match sym.sym {
                sym::QUOTE => self.quote(forms.bind(gc)),
                sym::LET => self.eval_let(forms, true, gc),
                sym::LET_STAR => self.eval_let(forms, false, gc),
                sym::IF => self.eval_if(forms, gc),
                sym::AND => self.eval_and(forms, gc),
                sym::OR => self.eval_or(forms, gc),
                sym::COND => self.eval_cond(forms, gc),
                sym::WHILE => self.eval_while(forms, gc),
                sym::PROGN => self.eval_progn(forms, gc),
                sym::PROG1 => self.eval_progx(forms, 1, gc),
                sym::PROG2 => self.eval_progx(forms, 2, gc),
                sym::SETQ => self.setq(forms, gc),
                sym::DEFVAR | sym::DEFCONST => self.defvar(forms, gc),
                sym::FUNCTION => self.eval_function(forms.bind(gc), gc),
                sym::INTERACTIVE => Ok(GcObj::NIL), // TODO: implement
                sym::CATCH => self.catch(forms, gc),
                sym::CONDITION_CASE => self.condition_case(forms, gc),
                _ => self.eval_call(sym, forms, gc).map_err(|e| anyhow!(e)),
            },
            other => Err(anyhow!("Invalid Function: {other}")),
        }
    }

    fn catch<'gc>(&mut self, obj: &Rt<GcObj>, gc: &'gc mut Arena) -> Result<GcObj<'gc>> {
        element_iter!(forms, obj.bind(gc), gc);
        let _tag = forms.next().ok_or_else(|| ArgError::new(1, 0, "catch"))?;
        self.implicit_progn(forms, gc)
    }

    fn defvar<'gc>(&mut self, obj: &Rt<GcObj>, gc: &'gc mut Arena) -> Result<GcObj<'gc>> {
        let obj = obj.bind(gc);
        element_iter!(forms, obj, gc);
        match forms.next() {
            // (defvar x ...)
            Some(x) => {
                let name: Symbol = x.bind(gc).try_into()?;
                let value = match forms.next() {
                    // (defvar x y)
                    Some(value) => self.eval_form(value, gc)?,
                    // (defvar x)
                    None => GcObj::NIL,
                };
                rebind!(value, gc);
                self.var_set(name, value, gc);
                Ok(value)
            }
            // (defvar)
            None => Err(ArgError::new(1, 0, "defvar").into()),
        }
    }

    fn parse_closure_env(obj: GcObj) -> Result<Vec<&Cons>> {
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

    fn parse_arg_list(bindings: GcObj) -> Result<(Vec<Symbol>, Vec<Symbol>, Option<Symbol>)> {
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

    #[allow(clippy::unused_self)]
    fn bind_args<'a>(
        &self,
        arg_list: GcObj,
        args: Vec<GcObj<'a>>,
        vars: &mut Vec<&'a Cons>,
        name: &str,
        gc: &'a Arena,
    ) -> Result<()> {
        let (required, optional, rest) = Self::parse_arg_list(arg_list)?;

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
            vars.push(cons!(name, val; gc).as_cons());
        }

        for name in optional {
            let val = arg_values.next().unwrap_or_default();
            vars.push(cons!(name, val; gc).as_cons());
        }

        if let Some(rest_name) = rest {
            let values = arg_values.as_slice();
            let list = crate::fns::slice_into_list(values, None, gc);
            vars.push(cons!(rest_name, list; gc).as_cons());
        } else {
            // Ensure too many args were not provided
            ensure!(
                arg_values.next().is_none(),
                ArgError::new(num_required_args + num_optional_args, num_actual_args, name)
            );
        }
        Ok(())
    }

    fn bind_variables<'a>(
        &self,
        forms: &mut ElemStreamIter<'_, '_>,
        args: Vec<GcObj<'a>>,
        name: &str,
        gc: &'a Arena,
    ) -> Result<Vec<&'a Cons>> {
        // Add closure environment to variables
        // (closure ((x . 1) (y . 2) t) ...)
        //          ^^^^^^^^^^^^^^^^^^^
        let env = forms
            .next()
            .ok_or_else(|| anyhow!("Closure missing environment"))?;
        let mut vars = Self::parse_closure_env(env.bind(gc))?;

        // Add function arguments to variables
        // (closure (t) (x y &rest z) ...)
        //              ^^^^^^^^^^^^^
        let arg_list = forms
            .next()
            .ok_or_else(|| anyhow!("Closure missing argument list"))?;
        self.bind_args(arg_list.bind(gc), args, &mut vars, name, gc)?;
        Ok(vars)
    }

    fn call_closure<'gc>(
        &mut self,
        closure: &Rt<&Cons>,
        args: &Root<Vec<GcObj>>,
        name: &str,
        gc: &'gc mut Arena,
    ) -> Result<GcObj<'gc>> {
        let closure = closure.bind(gc);
        match closure.car().get() {
            Object::Symbol(Q {
                sym: sym::CLOSURE, ..
            }) => {
                element_iter!(forms, closure.cdr(), gc);
                // TODO: remove this temp vector
                let args = args.iter().map(|x| x.bind(gc)).collect();
                let vars = self.bind_variables(&mut forms, args, name, gc)?;
                root!(vars, vars.into_root(), gc);
                let mut call_frame = Interpreter {
                    vars,
                    env: self.env,
                };
                call_frame.implicit_progn(forms, gc)
            }
            other => Err(TypeError::new(Type::Func, other).into()),
        }
    }

    fn eval_call<'gc>(
        &mut self,
        name: Symbol,
        obj: &Rt<GcObj>,
        gc: &'gc mut Arena,
    ) -> LispResult<'gc> {
        let resolved = match name.resolve_callable(gc) {
            Some(x) => x,
            None => {
                return Err(EvalError::new(anyhow!("Invalid function: {name}")));
            }
        };

        match resolved.get() {
            Callable::LispFn(_) => todo!("call lisp functions in interpreter"),
            Callable::SubrFn(func) => {
                let obj = obj.bind(gc);
                element_iter!(iter, obj, gc);
                root!(args, Vec::new(), gc);
                while let Some(x) = iter.next() {
                    let result = self.eval_form(x, gc)?;
                    rebind!(result, gc);
                    args.deref_mut(gc).push(result);
                }
                if crate::debug::debug_enabled() {
                    println!("({name} {args:?})");
                }
                gc.garbage_collect(false);

                (*func)
                    .call(args, self.env, gc)
                    .map_err(|e| EvalError::with_trace(e, format!("({name} {args:?})")))
            }
            Callable::Cons(form) => match form.try_as_macro() {
                Ok(mcro) => {
                    let macro_args = obj.bind(gc).as_list()?.collect::<Result<Vec<_>>>()?;
                    if crate::debug::debug_enabled() {
                        println!("(macro: {name} {macro_args:?})");
                    }
                    root!(args, macro_args.into_root(), gc);
                    let macro_func: Gc<Function> = mcro.into();
                    root!(macro_func, gc);
                    let value = macro_func
                        .call(args, self.env, gc, Some(name.name))
                        .map_err(|e| EvalError::with_trace(e, format!("({name} {args:?})")))?;
                    root!(value, gc);
                    self.eval_form(value, gc).map_err(EvalError::new)
                }
                Err(_) => match form.car().get() {
                    Object::Symbol(Q {
                        sym: sym::CLOSURE, ..
                    }) => {
                        let obj = obj.bind(gc);
                        element_iter!(iter, obj, gc);
                        root!(args, Vec::new(), gc);
                        root!(form, gc);
                        while let Some(x) = iter.next() {
                            let result = self.eval_form(x, gc)?;
                            rebind!(result, gc);
                            args.deref_mut(gc).push(result);
                        }
                        if crate::debug::debug_enabled() {
                            println!("({name} {args:?})");
                        }
                        self.call_closure(form, args, name.name, gc)
                            .map_err(|e| EvalError::with_trace(e, format!("({name} {args:?})")))
                    }
                    other => Err(anyhow!("Invalid Function: {other}").into()),
                },
            },
        }
    }
    fn eval_function<'a>(&mut self, obj: GcObj<'a>, gc: &'a Arena) -> Result<GcObj<'a>> {
        let mut forms = obj.as_list()?;
        let len = forms.len() as u16;
        ensure!(len == 1, ArgError::new(1, len, "function"));

        let form = forms.next().unwrap()?;
        match form.get() {
            Object::Cons(cons) => {
                if cons.car() == sym::LAMBDA {
                    let env = {
                        // TODO: remove temp vector
                        let env: Vec<_> = self.vars.iter().map(|x| x.bind(gc).into()).collect();
                        crate::fns::slice_into_list(env.as_slice(), Some(cons!(true; gc)), gc)
                    };
                    let end = cons!(env, cons.cdr(); gc);
                    let closure = cons!(sym::CLOSURE, end; gc);
                    Ok(gc.bind(closure))
                } else {
                    Ok(cons.into())
                }
            }
            _ => Ok(form),
        }
    }

    fn eval_progx<'gc>(
        &mut self,
        obj: &Rt<GcObj>,
        prog_num: u16,
        gc: &'gc mut Arena,
    ) -> Result<GcObj<'gc>> {
        let mut count = 0;
        root!(returned_form, None, gc);
        let obj = obj.bind(gc);
        element_iter!(forms, obj, gc);
        while let Some(form) = forms.next() {
            let value = self.eval_form(form, gc)?;
            count += 1;
            if prog_num == count {
                rebind!(value, gc);
                returned_form.deref_mut(gc).set(value);
            }
        }
        match &***returned_form {
            Some(x) => Ok(gc.bind(x.bind(gc))),
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

    fn eval_progn<'gc>(&mut self, obj: &Rt<GcObj>, gc: &'gc mut Arena) -> Result<GcObj<'gc>> {
        let obj = obj.bind(gc);
        element_iter!(forms, obj, gc);
        self.implicit_progn(forms, gc)
    }

    fn eval_while<'gc>(&mut self, obj: &Rt<GcObj>, gc: &'gc mut Arena) -> Result<GcObj<'gc>> {
        let first: Gc<List> = obj.bind(gc).try_into()?;
        let condition = match first.get() {
            List::Cons(cons) => cons.car(),
            List::Nil => bail!(ArgError::new(1, 0, "while")),
        };
        root!(condition, gc);
        while self.eval_form(condition, gc)? != GcObj::NIL {
            let obj = obj.bind(gc);
            element_iter!(forms, obj, gc);
            self.implicit_progn(forms, gc)?;
        }
        Ok(GcObj::NIL)
    }

    fn eval_cond<'gc>(&mut self, obj: &Rt<GcObj>, gc: &'gc mut Arena) -> Result<GcObj<'gc>> {
        let obj = obj.bind(gc);
        element_iter!(forms, obj, gc);
        while let Some(form) = forms.next() {
            element_iter!(clause, form.bind(gc), gc);
            if let Some(first) = clause.next() {
                let condition = self.eval_form(first, gc)?;
                if condition != GcObj::NIL {
                    return if clause.is_empty() {
                        rebind!(condition, gc);
                        Ok(condition)
                    } else {
                        self.implicit_progn(clause, gc)
                    };
                }
            }
        }
        Ok(GcObj::NIL)
    }

    fn eval_and<'gc>(&mut self, obj: &Rt<GcObj>, gc: &'gc mut Arena) -> Result<GcObj<'gc>> {
        root!(last, GcObj::TRUE, gc);
        let obj = obj.bind(gc);
        element_iter!(forms, obj, gc);
        while let Some(form) = forms.next() {
            let result = self.eval_form(form, gc)?;
            if result == GcObj::NIL {
                return Ok(GcObj::NIL);
            }
            rebind!(result, gc);
            last.deref_mut(gc).set(result);
        }
        Ok(gc.bind(last.bind(gc)))
    }

    fn eval_or<'gc>(&mut self, obj: &Rt<GcObj>, gc: &'gc mut Arena) -> Result<GcObj<'gc>> {
        let obj = obj.bind(gc);
        element_iter!(forms, obj, gc);
        while let Some(form) = forms.next() {
            let result = self.eval_form(form, gc)?;
            if result != GcObj::NIL {
                rebind!(result, gc);
                return Ok(result);
            }
        }
        Ok(GcObj::NIL)
    }

    fn eval_if<'gc>(&mut self, obj: &Rt<GcObj>, gc: &'gc mut Arena) -> Result<GcObj<'gc>> {
        let obj = obj.bind(gc);
        element_iter!(forms, obj, gc);
        let condition = match forms.next() {
            Some(x) => x.bind(gc),
            None => bail!(ArgError::new(2, 0, "if")),
        };
        root!(condition, gc);
        let true_branch = match forms.next() {
            Some(x) => x.bind(gc),
            None => bail!(ArgError::new(2, 1, "if")),
        };
        root!(true_branch, gc);
        #[allow(clippy::if_not_else)]
        if self.eval_form(condition, gc)? != GcObj::NIL {
            self.eval_form(true_branch, gc)
        } else {
            self.implicit_progn(forms, gc)
        }
    }

    fn setq<'gc>(&mut self, obj: &Rt<GcObj>, gc: &'gc mut Arena) -> Result<GcObj<'gc>> {
        let obj = obj.bind(gc);
        element_iter!(forms, obj, gc);
        let mut arg_cnt = 0;
        root!(last_value, GcObj::NIL, gc);
        while let Some((var, val)) = Self::pairs(&mut forms, gc) {
            match (var.get(), val) {
                (Object::Symbol(var), Some(val)) => {
                    root!(val, gc);
                    let val = self.eval_form(val, gc)?;
                    rebind!(val, gc);
                    self.var_set(var, val, gc);
                    last_value.deref_mut(gc).set(val);
                }
                (_, Some(_)) => bail!(TypeError::new(Type::Symbol, var)),
                (_, None) => bail!(ArgError::new(arg_cnt, arg_cnt + 1, "setq")),
            }
            arg_cnt += 2;
        }
        if arg_cnt < 2 {
            Err(ArgError::new(2, 0, "setq").into())
        } else {
            Ok(last_value.bind(gc))
        }
    }

    fn pairs<'ob>(
        iter: &mut ElemStreamIter<'_, '_>,
        gc: &'ob Arena,
    ) -> Option<(GcObj<'ob>, Option<GcObj<'ob>>)> {
        #[allow(clippy::manual_map)]
        if let Some(first) = iter.next() {
            Some((first.bind(gc), iter.next().map(|x| x.bind(gc))))
        } else {
            None
        }
    }

    fn var_ref<'a>(&self, sym: Symbol, gc: &'a Arena) -> Result<GcObj<'a>> {
        if sym.name.starts_with(':') {
            Ok(sym.into())
        } else {
            let mut iter = self.vars.iter().rev();
            match iter.find_map(|cons| (cons.bind(gc).car() == sym).then(|| cons.bind(gc).cdr())) {
                Some(value) => Ok(value),
                None => match self.env.vars.get(sym) {
                    Some(v) => Ok(v.bind(gc)),
                    None => Err(anyhow!("Void variable: {sym}")),
                },
            }
        }
    }

    fn var_set(&mut self, name: Symbol, new_value: GcObj, gc: &Arena) {
        let mut iter = self.vars.iter().rev();
        match iter.find(|cons| (cons.bind(gc).car() == name)) {
            Some(value) => {
                value
                    .bind(gc)
                    .set_cdr(new_value)
                    .expect("variables should never be immutable");
            }
            None => {
                self.env.deref_mut(gc).set_var(name, new_value);
            }
        }
    }

    #[allow(clippy::unused_self)]
    fn quote<'gc>(&self, value: GcObj<'gc>) -> Result<GcObj<'gc>> {
        let mut forms = value.as_list()?;
        match forms.len() {
            1 => Ok(forms.next().unwrap()?),
            x => Err(ArgError::new(1, x as u16, "quote").into()),
        }
    }

    fn eval_let<'gc>(
        &mut self,
        form: &Rt<GcObj>,
        parallel: bool,
        gc: &'gc mut Arena,
    ) -> Result<GcObj<'gc>> {
        let form = form.bind(gc);
        element_iter!(iter, form, gc);
        let prev_len = self.vars.len();
        root!(dynamic_bindings, Vec::new(), gc);
        match iter.next() {
            // (let x ...)
            Some(x) => {
                let obj = x;
                if parallel {
                    self.let_bind_parallel(obj, dynamic_bindings, gc)?;
                } else {
                    self.let_bind_serial(obj, dynamic_bindings, gc)?;
                }
            }
            // (let)
            None => bail!(ArgError::new(1, 0, "let")),
        }
        let obj = self.implicit_progn(iter, gc)?;
        rebind!(obj, gc);
        // Remove old bindings
        self.vars.deref_mut(gc).truncate(prev_len);
        for binding in dynamic_bindings.iter() {
            let var: Symbol = &binding.0;
            let val = &binding.1;

            if let Some(current_val) = self.env.deref_mut(gc).vars.get_mut(var) {
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
        gc: &mut Arena,
    ) -> Result<()> {
        let form = form.bind(gc);
        element_iter!(bindings, form, gc);
        while let Some(binding) = bindings.next() {
            let obj = binding.bind(gc);
            let (var, val) = match obj.get() {
                // (let ((x y)))
                Object::Cons(_) => self.let_bind_value(binding.as_cons(), gc)?,
                // (let (x))
                Object::Symbol(sym) => (sym, GcObj::NIL),
                // (let (1))
                x => bail!(TypeError::new(Type::Cons, x)),
            };
            rebind!(val, gc);
            if let Some(current_val) = self.env.deref_mut(gc).vars.get_mut(var) {
                let prev_val = current_val.bind(gc);
                dynamic_bindings.deref_mut(gc).push((var, prev_val));
                current_val.set(val);
            } else {
                let cons = cons!(var, val; gc).as_cons();
                self.vars.deref_mut(gc).push(cons);
            }
        }
        Ok(())
    }

    fn let_bind_parallel(
        &mut self,
        form: &Rt<GcObj>,
        dynamic_bindings: &mut Root<Vec<(Symbol, GcObj<'static>)>>,
        gc: &mut Arena,
    ) -> Result<()> {
        root!(let_bindings, Vec::new(), gc);
        let form = form.bind(gc);
        element_iter!(bindings, form, gc);
        while let Some(binding) = bindings.next() {
            let obj = binding.bind(gc);
            match obj.get() {
                // (let ((x y)))
                Object::Cons(_) => {
                    let (sym, var) = self.let_bind_value(binding.as_cons(), gc)?;
                    rebind!(var, gc);
                    let_bindings.deref_mut(gc).push((sym, var));
                }
                // (let (x))
                Object::Symbol(sym) => {
                    let_bindings.deref_mut(gc).push((sym, GcObj::NIL));
                }
                // (let (1))
                x => bail!(TypeError::new(Type::Cons, x)),
            }
        }
        for binding in let_bindings.iter() {
            let var: Symbol = &binding.0;
            let val = &binding.1;
            if let Some(current_val) = self.env.deref_mut(gc).vars.get_mut(var) {
                let prev_val = current_val.bind(gc);
                dynamic_bindings.deref_mut(gc).push((var, prev_val));
                current_val.set(val);
            } else {
                let val = val.bind(gc);
                let cons = cons!(var, val; gc).as_cons();
                self.vars.deref_mut(gc).push(cons);
            }
        }
        Ok(())
    }

    fn let_bind_value<'ob>(
        &mut self,
        cons: &Rt<Gc<&Cons>>,
        gc: &'ob mut Arena,
    ) -> Result<(Symbol, GcObj<'ob>)> {
        element_iter!(iter, gc.bind(cons.bind(gc).cdr()), gc);
        let value = match iter.next() {
            // (let ((x y)))
            Some(x) => self.eval_form(x, gc)?,
            // (let ((x)))
            None => GcObj::NIL,
        };
        // (let ((x y z ..)))
        ensure!(iter.is_empty(), "Let binding can only have 1 value");
        rebind!(value, gc);
        let name: Symbol = cons
            .bind(gc)
            .car()
            .try_into()
            .context("let variable must be a symbol")?;
        Ok((name, value))
    }

    fn implicit_progn<'gc>(
        &mut self,
        mut forms: ElemStreamIter<'_, '_>,
        gc: &'gc mut Arena,
    ) -> Result<GcObj<'gc>> {
        root!(last, GcObj::NIL, gc);
        while let Some(form) = forms.next() {
            let value = self.eval_form(form, gc)?;
            rebind!(value, gc);
            last.deref_mut(gc).set(value);
        }
        Ok(last.deref_mut(gc).bind(gc))
    }

    fn condition_case<'ob>(
        &mut self,
        form: &Rt<GcObj>,
        gc: &'ob mut Arena,
    ) -> Result<GcObj<'ob>> {
        let form = form.bind(gc);
        element_iter!(forms, form, gc);
        let var = match forms.next() {
            Some(x) => x.bind(gc),
            None => bail!(ArgError::new(2, 0, "condition-case")),
        };
        root!(var, gc);
        let bodyform = match forms.next() {
            Some(x) => x,
            None => bail!(ArgError::new(2, 1, "condition-case")),
        };
        match self.eval_form(bodyform, gc) {
            Ok(x) => {
                rebind!(x, gc);
                Ok(x)
            }
            Err(e) => {
                const CONDITION_ERROR: &str = "Invalid condition handler:";
                while let Some(handler) = forms.next() {
                    match handler.bind(gc).get() {
                        Object::Cons(cons) => {
                            // Check that conditions match
                            let condition = cons.car();
                            match condition.get() {
                                Object::Symbol(s) if s == &sym::ERROR => {}
                                Object::Cons(cons) => {
                                    for x in cons.elements() {
                                        let x = x?;
                                        ensure!(
                                            x == sym::DEBUG || x == sym::ERROR,
                                            "non-error conditions {x} not yet supported"
                                        );
                                    }
                                }
                                _ => bail!("{CONDITION_ERROR} {condition}"),
                            }
                            // Call handlers with error
                            let binding = list!(var, sym::ERROR, format!("{e}"); gc).as_cons();
                            self.vars.deref_mut(gc).push(binding);
                            let list: Gc<List> = match cons.cdr().try_into() {
                                Ok(x) => x,
                                Err(_) => return Ok(GcObj::NIL),
                            };
                            element_iter!(handlers, list, gc);
                            let result = self.implicit_progn(handlers, gc)?;
                            rebind!(result, gc);
                            self.vars.deref_mut(gc).pop();
                            return Ok(result);
                        }
                        Object::Nil => {}
                        invalid => bail!("{CONDITION_ERROR} {invalid}"),
                    }
                }
                Err(e)
            }
        }
    }
}

defsubr!(eval);

#[cfg(test)]
mod test {
    use crate::core::{arena::RootSet, env::intern, object::IntoObject};

    use super::*;

    fn check_interpreter<'ob, T>(test_str: &str, expect: T, arena: &'ob mut Arena)
    where
        T: IntoObject<'ob>,
    {
        root!(env, Environment::default(), arena);
        println!("Test String: {}", test_str);
        let obj = crate::reader::read(test_str, arena).unwrap().0;
        root!(obj, arena);
        let compare = eval(obj, None, env, arena).unwrap();
        rebind!(compare, arena);
        let expect: GcObj = expect.into_obj(arena).copy_as_obj();
        assert_eq!(compare, expect);
    }

    fn check_error<'ob>(test_str: &str, arena: &'ob mut Arena) {
        root!(env, Environment::default(), arena);
        println!("Test String: {}", test_str);
        let obj = crate::reader::read(test_str, arena).unwrap().0;
        root!(obj, arena);
        assert!(eval(obj, None, env, arena).is_err());
    }

    #[test]
    fn basic() {
        let roots = &RootSet::default();
        let arena = &mut Arena::new(roots);
        check_interpreter("1", 1, arena);
        check_interpreter("1.5", 1.5, arena);
        check_interpreter("nil", false, arena);
        check_interpreter("t", true, arena);
        check_interpreter("\"foo\"", "foo", arena);
        let list = list!(1, 2; arena);
        root!(list, arena);
        check_interpreter("'(1 2)", list, arena);
    }

    #[test]
    fn variables() {
        let roots = &RootSet::default();
        let arena = &mut Arena::new(roots);
        check_interpreter("(let ())", false, arena);
        check_interpreter("(let (x) x)", false, arena);
        check_interpreter("(let ((x 1)) x)", 1, arena);
        check_interpreter("(let ((x 1)))", false, arena);
        check_interpreter("(let ((x 1) (y 2)) x y)", 2, arena);
        check_interpreter("(let ((x 1)) (let ((x 3)) x))", 3, arena);
        check_interpreter("(let ((x 1)) (let ((y 3)) x))", 1, arena);
        check_interpreter("(let ((x 1)) (setq x 2) x)", 2, arena);
        check_interpreter("(let* ())", false, arena);
        check_interpreter("(let* ((x 1) (y x)) y)", 1, arena);
    }

    #[test]
    fn dyn_variables() {
        let roots = &RootSet::default();
        let arena = &mut Arena::new(roots);
        check_interpreter("(progn (defvar foo 1) foo)", 1, arena);
        check_interpreter("(progn (defvar foo 1) (let ((foo 3)) foo))", 3, arena);
        check_interpreter("(progn (defvar foo 1) (let ((foo 3))) foo)", 1, arena);
        check_interpreter(
            "(progn (defvar foo 1) (let (bar) (let ((foo 3)) (setq bar foo)) bar))",
            3,
            arena,
        );
    }

    #[test]
    fn conditionals() {
        let roots = &RootSet::default();
        let arena = &mut Arena::new(roots);
        check_interpreter("(if nil 1)", false, arena);
        check_interpreter("(if t 1)", 1, arena);
        check_interpreter("(if nil 1 2)", 2, arena);
        check_interpreter("(if t 1 2)", 1, arena);
        check_interpreter("(if nil 1 2 3)", 3, arena);
        check_interpreter("(and)", true, arena);
        check_interpreter("(and 1)", 1, arena);
        check_interpreter("(and 1 2)", 2, arena);
        check_interpreter("(and 1 nil)", false, arena);
        check_interpreter("(and nil 1)", false, arena);
        check_interpreter("(or)", false, arena);
        check_interpreter("(or nil)", false, arena);
        check_interpreter("(or nil 1)", 1, arena);
        check_interpreter("(or 1 2)", 1, arena);
        check_interpreter("(cond)", false, arena);
        check_interpreter("(cond nil)", false, arena);
        check_interpreter("(cond (1))", 1, arena);
        check_interpreter("(cond (1 2))", 2, arena);
        check_interpreter("(cond (nil 1) (2 3))", 3, arena);
        check_interpreter("(cond (nil 1) (2 3) (4 5))", 3, arena);
    }

    #[test]
    fn special_forms() {
        let roots = &RootSet::default();
        let arena = &mut Arena::new(roots);
        check_interpreter("(prog1 1 2 3)", 1, arena);
        check_interpreter("(prog2 1 2 3)", 2, arena);
        check_interpreter("(progn 1 2 3 4)", 4, arena);
        check_interpreter("(function 1)", 1, arena);
        check_interpreter("(quote 1)", 1, arena);
        check_interpreter("(if 1 2 3)", 2, arena);
        check_interpreter("(if nil 2 3)", 3, arena);
        check_interpreter("(if (and 1 nil) 2 3)", 3, arena);
    }

    #[test]
    fn test_functions() {
        let roots = &RootSet::default();
        let arena = &mut Arena::new(roots);
        let list = list![sym::CLOSURE, list![true; arena]; arena];
        root!(list, arena);
        check_interpreter("(function (lambda))", list, arena);
        let x = intern("x");
        let y = intern("y");
        let list = list![sym::CLOSURE, list![true; arena], list![x; arena], x; arena];
        root!(list, arena);
        check_interpreter("(function (lambda (x) x))", list, arena);
        let list: GcObj =
            list![sym::CLOSURE, list![cons!(y, 1; arena), true; arena], list![x; arena], x; arena];
        root!(list, arena);
        check_interpreter("(let ((y 1)) (function (lambda (x) x)))", list, arena);

        let list = list!(5, false; arena);
        root!(list, arena);
        check_interpreter(
            "(let ((x #'(lambda (x &optional y &rest z) (cons x (cons y z))))) (funcall x 5))",
            list,
            arena,
        );
        let list = list!(5, 7; arena);
        root!(list, arena);
        check_interpreter(
            "(let ((x #'(lambda (x &optional y &rest z) (cons x (cons y z))))) (funcall x 5 7))",
            list,
            arena,
        );
        let list = list!(5, 7, 11; arena);
        root!(list, arena);
        check_interpreter(
            "(let ((x #'(lambda (x &optional y &rest z) (cons x (cons y z))))) (funcall x 5 7 11))",
            list,
            arena,
        );
    }

    #[test]
    fn test_call() {
        let roots = &RootSet::default();
        let arena = &mut Arena::new(roots);
        check_interpreter("(let ((x #'(lambda (x) x))) (funcall x 5))", 5, arena);
        check_interpreter("(let ((x #'(lambda () 3))) (funcall x))", 3, arena);
        check_interpreter(
            "(progn (defvar foo 1) (let ((x #'(lambda () foo)) (foo 5)) (funcall x)))",
            5,
            arena,
        );
        check_interpreter(
            "(progn (defalias 'int-test-call #'(lambda (x) (+ x 3)))  (int-test-call 7))",
            10,
            arena,
        );
        // Test closures
        check_interpreter("(let* ((y 7)(x #'(lambda () y))) (funcall x))", 7, arena);
        check_interpreter(
            "(let* ((y 7)(x #'(lambda (x) (+ x y)))) (funcall x 3))",
            10,
            arena,
        );
        // Test that closures capture their environments
        check_interpreter(
            "(progn (setq func (let ((x 3)) #'(lambda (y) (+ y x)))) (funcall func 5))",
            8,
            arena,
        );
        // Test multiple closures
        check_interpreter("(progn (setq funcs (let ((x 3)) (cons #'(lambda (y) (+ y x)) #'(lambda (y) (- y x))))) (* (funcall (car funcs) 5) (funcall (cdr funcs) 1)))", -16, arena);
        // Test that closures close over variables
        check_interpreter("(progn (setq funcs (let ((x 3)) (cons #'(lambda (y) (setq x y)) #'(lambda (y) (+ y x))))) (funcall (car funcs) 5) (funcall (cdr funcs) 4))", 9, arena);
        // Test that closures in global function close over values and not
        // variables
        check_interpreter("(progn (setq func (let ((x 3)) (defalias 'int-test-no-cap #'(lambda (y) (+ y x))) #'(lambda (y) (setq x y)))) (funcall func 4) (int-test-no-cap 5))", 8, arena);

        check_interpreter(
            "(progn (read-from-string (prin1-to-string (make-hash-table))) nil)",
            false,
            arena,
        );
    }

    #[test]
    fn test_condition_case() {
        let roots = &RootSet::default();
        let arena = &mut Arena::new(roots);
        check_interpreter("(condition-case nil nil)", false, arena);
        check_interpreter("(condition-case nil 1)", 1, arena);
        check_interpreter("(condition-case nil (if) (error 7))", 7, arena);
        check_interpreter("(condition-case nil (if) (error 7 9 11))", 11, arena);
        check_interpreter("(condition-case nil (if) (error . 7))", false, arena);
        check_interpreter("(condition-case nil (if) ((debug error) 7))", 7, arena);
        check_error("(condition-case nil (if))", arena);
        check_error("(condition-case nil (if) nil)", arena);
        check_error("(condition-case nil (if) 5 (error 7))", arena);
    }
}
