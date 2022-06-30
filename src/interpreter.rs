use crate::core::env::GlobalSymbol as Q;
use crate::core::{
    arena::{Arena, IntoRoot, Root, Rt},
    cons::{Cons, ElemStreamIter},
    env::{sym, Environment, Symbol},
    error::{Error, Type},
    object::{Callable, Function, Gc, GcObj, List, Object},
};
use crate::{element_iter, rebind, root};
use anyhow::{anyhow, bail, ensure, Result};
use fn_macros::defun;
use streaming_iterator::StreamingIterator;

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
    root!(vars, Vec::<&'static Cons>::new(), arena);
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
                _ => self.eval_call(sym, forms, gc),
            },
            other => Err(anyhow!("Invalid Function: {other}")),
        }
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
            None => Err(Error::arg_count(1, 0, "defvar").into()),
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
            Error::arg_count(num_required_args, num_actual_args, name)
        );

        let mut arg_values = args.into_iter();

        for name in required {
            let val = arg_values.next().unwrap();
            vars.push(cons!(name, val; gc).try_into().unwrap());
        }

        for name in optional {
            let val = arg_values.next().unwrap_or_default();
            vars.push(cons!(name, val; gc).try_into().unwrap());
        }

        if let Some(rest_name) = rest {
            let values = arg_values.as_slice();
            let list = crate::fns::slice_into_list(values, None, gc);
            vars.push(cons!(rest_name, list; gc).try_into().unwrap());
        } else {
            // Ensure too many args were not provided
            ensure!(
                arg_values.next().is_none(),
                Error::arg_count(num_required_args + num_optional_args, num_actual_args, name)
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
            other => Err(Error::from_object(Type::Func, other).into()),
        }
    }

    fn eval_call<'gc>(
        &mut self,
        name: Symbol,
        obj: &Rt<GcObj>,
        gc: &'gc mut Arena,
    ) -> Result<GcObj<'gc>> {
        let resolved = match name.resolve_callable(gc) {
            Some(x) => x,
            None => bail!("Invalid function: {name}"),
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
                (*func).call(args, self.env, gc)
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
                    let value = macro_func.call(args, self.env, gc, Some(name.name))?;
                    root!(value, gc);
                    self.eval_form(value, gc)
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
                    }
                    other => Err(anyhow!("Invalid Function: {other}")),
                },
            },
        }
    }
    fn eval_function<'a>(&mut self, obj: GcObj<'a>, gc: &'a Arena) -> Result<GcObj<'a>> {
        let mut forms = obj.as_list()?;
        let len = forms.len() as u16;
        ensure!(len == 1, Error::arg_count(1, len, "function"));

        let form = forms.next().unwrap()?;
        match form.get() {
            Object::Cons(cons) => {
                if cons.car() == *sym::LAMBDA {
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
                Err(Error::arg_count(prog_num, count, name).into())
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
            List::Nil => bail!(Error::arg_count(1, 0, "while")),
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
            None => bail!(Error::arg_count(2, 0, "if")),
        };
        root!(condition, gc);
        let true_branch = match forms.next() {
            Some(x) => x.bind(gc),
            None => bail!(Error::arg_count(2, 1, "if")),
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
                (_, Some(_)) => bail!(Error::from_object(Type::Symbol, var)),
                (_, None) => bail!(Error::arg_count(arg_cnt, arg_cnt + 1, "setq")),
            }
            arg_cnt += 2;
        }
        if arg_cnt < 2 {
            Err(Error::arg_count(2, 0, "setq").into())
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
                value.bind(gc).set_cdr(new_value);
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
            x => Err(Error::arg_count(1, x as u16, "quote").into()),
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
        match iter.next() {
            // (let x ...)
            Some(x) => {
                let obj = x;
                if parallel {
                    self.let_bind_parallel(obj, gc)?;
                } else {
                    self.let_bind_serial(obj, gc)?;
                }
            }
            // (let)
            None => bail!(Error::arg_count(1, 0, "let")),
        }
        let obj = self.implicit_progn(iter, gc)?;
        rebind!(obj, gc);
        self.vars.deref_mut(gc).truncate(prev_len);
        Ok(obj)
    }

    fn let_bind_serial(&mut self, form: &Rt<GcObj>, gc: &mut Arena) -> Result<()> {
        let form = form.bind(gc);
        element_iter!(bindings, form, gc);
        while let Some(binding) = bindings.next() {
            let obj = binding.bind(gc);
            match obj.get() {
                // (let ((x y)))
                Object::Cons(_) => {
                    let var = self.let_bind_value(binding.as_cons(), gc)?;
                    rebind!(var, gc);
                    self.vars.deref_mut(gc).push(var);
                }
                // (let (x))
                Object::Symbol(_) => {
                    let val = cons!(obj; gc);
                    let obj: &Cons = val.try_into().unwrap();
                    self.vars.deref_mut(gc).push(obj);
                }
                // (let (1))
                x => bail!(Error::from_object(Type::Cons, x)),
            }
        }
        Ok(())
    }

    fn let_bind_parallel(&mut self, form: &Rt<GcObj>, gc: &mut Arena) -> Result<()> {
        root!(let_bindings, Vec::<&'static Cons>::new(), gc);
        let form = form.bind(gc);
        element_iter!(bindings, form, gc);
        while let Some(binding) = bindings.next() {
            let obj = binding.bind(gc);
            match obj.get() {
                // (let ((x y)))
                Object::Cons(_) => {
                    let var = self.let_bind_value(binding.as_cons(), gc)?;
                    rebind!(var, gc);
                    let_bindings.deref_mut(gc).push(var);
                }
                // (let (x))
                Object::Symbol(_) => {
                    let val = cons!(obj; gc);
                    let cons: &Cons = val.try_into().unwrap();
                    let_bindings.deref_mut(gc).push(cons);
                }
                // (let (1))
                x => bail!(Error::from_object(Type::Cons, x)),
            }
        }
        self.vars.deref_mut(gc).append(let_bindings.deref_mut(gc));
        Ok(())
    }

    fn let_bind_value<'gc>(
        &mut self,
        cons: &Rt<Gc<&Cons>>,
        gc: &'gc mut Arena,
    ) -> Result<&'gc Cons> {
        element_iter!(iter, gc.bind(cons.bind(gc).cdr()), gc);
        let value = match iter.next() {
            // (let ((x y)))
            Some(x) => self.eval_form(x, gc)?,
            // (let ((x)))
            None => GcObj::NIL,
        };
        // (let ((x y z ..)))
        ensure!(iter.is_empty(), "Let binding forms can only have 1 value");
        rebind!(value, gc);
        let name: Symbol = cons.bind(gc).car().try_into()?;
        let val = cons!(name, value; gc);
        Ok(val.try_into().unwrap())
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
}
