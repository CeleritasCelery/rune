use crate::arena::{Gc, IntoRoot, RootCons, RootObj};
use crate::cons::ElemStreamIter;
use crate::error::{Error, Type};
use crate::object::{Callable, Function, List};
use crate::symbol::sym;
use crate::{arena::Arena, cons::Cons, data::Environment, object::Object, symbol::Symbol};
use crate::{element_iter, rebind, root};
use anyhow::{anyhow, bail, ensure, Result};
use fn_macros::defun;
use streaming_iterator::StreamingIterator;

struct Interpreter<'brw, 'vars> {
    vars: &'vars mut Gc<Vec<RootCons>>,
    env: &'brw mut Gc<Environment>,
}

#[defun]
pub(crate) fn eval<'ob, 'brw>(
    form: Object<'ob>,
    lexical: Option<Object<'ob>>,
    env: &'brw mut Gc<Environment>,
    arena: &'ob mut Arena,
) -> Result<Object<'ob>> {
    ensure!(
        matches!(lexical, Some(Object::True(_) | Object::Nil(_)) | None),
        "lexical enviroments are not yet supported: found {:?}",
        lexical
    );
    let vars = unsafe { &mut Gc::new(Vec::new()) };
    let mut interpreter = Interpreter { vars, env };
    interpreter.eval_form(form, arena)
}

pub(crate) fn call<'ob, 'gc>(
    form: Object<'ob>,
    args: &mut Gc<Vec<RootObj>>,
    env: &mut Gc<Environment>,
    gc: &'gc mut Arena,
) -> Result<Object<'gc>> {
    let vars = unsafe { &mut Gc::new(Vec::new()) };
    let mut frame = Interpreter { vars, env };
    frame.call_closure(form.try_into()?, args, gc)
}

impl<'brw, 'vars> Interpreter<'brw, 'vars> {
    fn eval_form<'a, 'gc>(&mut self, obj: Object<'a>, gc: &'gc mut Arena) -> Result<Object<'gc>> {
        match obj {
            Object::Symbol(sym) => self.var_ref(!sym, gc),
            Object::Cons(cons) => self.eval_sexp(&cons, gc),
            other => Ok(gc.bind(other)),
        }
    }

    pub(crate) fn eval_sexp<'a, 'gc>(
        &mut self,
        cons: &Cons<'a>,
        gc: &'gc mut Arena,
    ) -> Result<Object<'gc>> {
        let forms = cons.cdr(gc);
        root!(forms, gc);
        match cons.car(gc) {
            Object::Symbol(sym) => symbol_match! {!sym;
                QUOTE => self.quote(forms, gc),
                LET => self.eval_let(forms, true, gc),
                LET_STAR => self.eval_let(forms, false, gc),
                IF => self.eval_if(forms, gc),
                AND => self.eval_and(forms, gc),
                OR => self.eval_or(forms, gc),
                COND => self.eval_cond(forms, gc),
                WHILE => self.eval_while(forms, gc),
                PROGN => self.eval_progn(forms, gc),
                PROG1 => self.eval_progx(forms, 1, gc),
                PROG2 => self.eval_progx(forms, 2, gc),
                SETQ => self.setq(forms, gc),
                DEFVAR => self.defvar(forms, gc),
                DEFCONST => self.defvar(forms, gc),
                FUNCTION => self.eval_function(gc.bind(forms), gc),
                @ func => self.eval_call(func, forms, gc),
            },
            other => Err(anyhow!("Invalid Function: {other}")),
        }
    }

    fn defvar<'a, 'gc>(&mut self, obj: Object<'a>, gc: &'gc mut Arena) -> Result<Object<'gc>> {
        element_iter!(forms, obj);
        match forms.next() {
            // (defvar x ...)
            Some(x) => {
                let name: Symbol = x.obj().try_into()?;
                let value = match forms.next() {
                    // (defvar x y)
                    Some(value) => self.eval_form(value.obj(), gc)?,
                    // (defvar x)
                    None => Object::NIL,
                };
                rebind!(value, gc);
                Ok(self.var_set(name, value, gc))
            }
            // (defvar)
            None => Err(Error::ArgCount(1, 0).into()),
        }
    }

    fn parse_closure_env<'a>(obj: Object<'a>, arena: &'a Arena) -> Result<Vec<&'a Cons<'a>>> {
        let forms = obj.as_list(arena)?;
        let mut env = Vec::new();
        for form in forms {
            match form? {
                Object::Cons(pair) => {
                    env.push(!pair);
                }
                Object::True(_) => return Ok(env),
                x => bail!("Invalid closure environment member: {x}"),
            }
        }
        Err(anyhow!("Closure env did not end with `t`"))
    }

    fn parse_arg_list<'a>(
        bindings: Object<'a>,
        arena: &'a Arena,
    ) -> Result<(Vec<Symbol>, Vec<Symbol>, Option<Symbol>)> {
        let mut required = Vec::new();
        let mut optional = Vec::new();
        let mut rest = None;
        let mut arg_type = &mut required;
        let mut iter = bindings.as_list(arena)?;
        while let Some(binding) = iter.next() {
            symbol_match! {
                binding?.try_into()?;
                AND_OPTIONAL => arg_type = &mut optional,
                AND_REST => {
                    if let Some(last) = iter.next() {
                        rest = Some(last?.try_into()?);
                        ensure!(
                            iter.next().is_none(),
                            "Found multiple arguments after &rest"
                        );
                    }
                },
                @ sym => {
                    arg_type.push(sym);
                }
            }
        }
        Ok((required, optional, rest))
    }

    #[allow(clippy::unused_self)]
    fn bind_args<'a>(
        &self,
        arg_list: Object<'a>,
        args: Vec<Object<'a>>,
        vars: &mut Vec<&'a Cons<'a>>,
        gc: &'a Arena,
    ) -> Result<()> {
        let (required, optional, rest) = Self::parse_arg_list(arg_list, gc)?;

        let num_required_args = required.len() as u16;
        let num_optional_args = optional.len() as u16;
        let num_actual_args = args.len() as u16;
        // Ensure the minimum number of arguments is present
        ensure!(
            num_actual_args >= num_required_args,
            Error::ArgCount(num_required_args, num_actual_args)
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
                Error::ArgCount(num_required_args + num_optional_args, num_actual_args)
            );
        }
        Ok(())
    }

    fn bind_variables<'a, 'rt>(
        &self,
        forms: &mut ElemStreamIter<'rt>,
        args: Vec<Object<'a>>,
        gc: &'a Arena,
    ) -> Result<Vec<&'a Cons<'a>>> {
        // Add closure environment to variables
        // (closure ((x . 1) (y . 2) t) ...)
        //          ^^^^^^^^^^^^^^^^^^^
        let env = forms
            .next()
            .ok_or_else(|| anyhow!("Closure missing environment"))?;
        let mut vars = Self::parse_closure_env(env.bind(gc), gc)?;

        // Add function arguments to variables
        // (closure (t) (x y &rest z) ...)
        //              ^^^^^^^^^^^^^
        let arg_list = forms
            .next()
            .ok_or_else(|| anyhow!("Closure missing argument list"))?;
        self.bind_args(arg_list.bind(gc), args, &mut vars, gc)?;
        Ok(vars)
    }

    fn call_closure<'a, 'gc>(
        &mut self,
        closure: &'a Cons<'a>,
        args: &mut Gc<Vec<RootObj>>,
        gc: &'gc mut Arena,
    ) -> Result<Object<'gc>> {
        match closure.car(gc) {
            Object::Symbol(sym) if !sym == &sym::CLOSURE => {
                let obj = closure.cdr(gc);
                element_iter!(forms, obj);
                // TODO: remove this temp vector
                let args = args.iter().map(|x| x.bind(gc)).collect();
                let vars = self.bind_variables(&mut forms, args, gc)?;
                let vars = unsafe { &mut Gc::new(vars.into_root()) };

                let mut call_frame = Interpreter {
                    vars,
                    env: self.env,
                };
                call_frame.implicit_progn(forms, gc)
            }
            other => Err(Error::from_object(Type::Func, other).into()),
        }
    }

    fn eval_call<'a, 'gc>(
        &mut self,
        name: Symbol,
        obj: Object<'a>,
        gc: &'gc mut Arena,
    ) -> Result<Object<'gc>> {
        let func = match name.resolve_callable() {
            Some(x) => x,
            None => bail!("Invalid function: {name}"),
        };
        match func {
            Callable::LispFn(_) => todo!("call lisp functions in interpreter"),
            Callable::SubrFn(func) => {
                element_iter!(iter, obj);
                let mut args = unsafe { Gc::new(Vec::new()) };
                while let Some(x) = iter.next() {
                    let result = self.eval_form(x.obj(), gc)?;
                    args.push(result);
                }
                if crate::debug::debug_enabled() {
                    println!("({name} {args:?})");
                }
                (*func).call(&mut args, self.env, gc)
            }
            Callable::Macro(mcro) => {
                let macro_args = obj.as_list(gc)?.collect::<Result<Vec<_>>>()?;
                if crate::debug::debug_enabled() {
                    println!("(macro: {name} {macro_args:?})");
                }
                let args = unsafe { &mut Gc::new(macro_args.into_root()) };
                let macro_obj: Object = mcro.get(gc).into();
                root!(macro_obj, gc);
                let macro_func: Function = macro_obj.try_into().unwrap();
                let value = macro_func.call(args, self.env, gc)?;
                rebind!(value, gc);
                root!(value, gc);
                self.eval_form(value, gc)
            }
            Callable::Uncompiled(form) => match form.car(gc) {
                Object::Symbol(sym) if !sym == &sym::CLOSURE => {
                    element_iter!(iter, obj);
                    let mut args = unsafe { Gc::new(Vec::new()) };
                    while let Some(x) = iter.next() {
                        let result = self.eval_form(x.obj(), gc)?;
                        args.push(result);
                    }
                    if crate::debug::debug_enabled() {
                        println!("({name} {args:?})");
                    }
                    if crate::debug::debug_enabled() {
                        println!("({name} {args:?})");
                    }
                    self.call_closure(!form, &mut args, gc)
                }
                other => Err(anyhow!("Invalid Function: {other}")),
            },
        }
    }
    fn eval_function<'a>(&mut self, obj: Object<'a>, gc: &'a Arena) -> Result<Object<'a>> {
        let mut forms = obj.as_list(gc)?;
        let len = forms.len() as u16;
        ensure!(len == 1, Error::ArgCount(1, len));

        match forms.next().unwrap()? {
            Object::Cons(cons) => {
                if cons.car(gc) == &sym::LAMBDA {
                    let env = {
                        // TODO: remove temp vector
                        let env: Vec<_> = self
                            .vars
                            .iter()
                            .map(|x| Object::Cons(x.obj().into()))
                            .collect();
                        crate::fns::slice_into_list(env.as_slice(), Some(cons!(true; gc)), gc)
                    };
                    let end = cons!(env, cons.cdr(gc); gc);
                    let closure = cons!(&sym::CLOSURE, end; gc);
                    Ok(gc.bind(closure))
                } else {
                    Ok(Object::Cons(cons))
                }
            }
            value => Ok(value),
        }
    }

    fn eval_progx<'a, 'gc>(
        &mut self,
        obj: Object<'a>,
        prog_num: u16,
        gc: &'gc mut Arena,
    ) -> Result<Object<'gc>> {
        let mut count = 0;
        let mut returned_form = unsafe { Gc::new(None) };
        element_iter!(forms, obj);
        while let Some(form) = forms.next() {
            let value = self.eval_form(form.obj(), gc)?;
            count += 1;
            if prog_num == count {
                returned_form.set(value);
            }
        }
        match &*returned_form {
            Some(x) => Ok(gc.bind(x.obj())),
            None => Err(Error::ArgCount(prog_num, count).into()),
        }
    }

    fn eval_progn<'a, 'gc>(&mut self, obj: Object<'a>, gc: &'gc mut Arena) -> Result<Object<'gc>> {
        element_iter!(forms, obj);
        self.implicit_progn(forms, gc)
    }

    fn eval_while<'a, 'gc>(&mut self, obj: Object<'a>, gc: &'gc mut Arena) -> Result<Object<'gc>> {
        let first: List = obj.try_into()?;
        let condition = match first {
            List::Cons(cons) => cons.car(gc),
            List::Nil => bail!(Error::ArgCount(1, 0)),
        };
        root!(condition, gc);
        while self.eval_form(condition, gc)? != Object::NIL {
            element_iter!(forms, obj);
            self.implicit_progn(forms, gc)?;
        }
        Ok(Object::NIL)
    }

    fn eval_cond<'a, 'gc>(&mut self, obj: Object<'a>, gc: &'gc mut Arena) -> Result<Object<'gc>> {
        element_iter!(forms, obj);
        while let Some(form) = forms.next() {
            element_iter!(clause, form.obj());
            if let Some(first) = clause.next() {
                let condition = self.eval_form(first.obj(), gc)?;
                if condition != Object::NIL {
                    return if clause.is_empty() {
                        rebind!(condition, gc);
                        Ok(condition)
                    } else {
                        self.implicit_progn(clause, gc)
                    };
                }
            }
        }
        Ok(Object::NIL)
    }

    fn eval_and<'a, 'gc>(&mut self, obj: Object<'a>, gc: &'gc mut Arena) -> Result<Object<'gc>> {
        let mut last = unsafe { Gc::new(RootObj::new(Object::TRUE)) };
        element_iter!(forms, obj);
        while let Some(form) = forms.next() {
            let result = self.eval_form(form.obj(), gc)?;
            if result == Object::NIL {
                return Ok(Object::NIL);
            }
            last.set(result);
        }
        Ok(gc.bind(last.obj()))
    }

    fn eval_or<'a, 'gc>(&mut self, obj: Object<'a>, gc: &'gc mut Arena) -> Result<Object<'gc>> {
        element_iter!(forms, obj);
        while let Some(form) = forms.next() {
            let result = self.eval_form(form.obj(), gc)?;
            if result != Object::NIL {
                rebind!(result, gc);
                return Ok(result);
            }
        }
        Ok(Object::NIL)
    }

    fn eval_if<'a, 'gc>(&mut self, obj: Object<'a>, gc: &'gc mut Arena) -> Result<Object<'gc>> {
        element_iter!(forms, obj);
        let condition = match forms.next() {
            Some(x) => x.obj(),
            None => bail!(Error::ArgCount(2, 0)),
        };
        root!(condition, gc);
        let true_branch = match forms.next() {
            Some(x) => x.obj(),
            None => bail!(Error::ArgCount(2, 1)),
        };
        root!(true_branch, gc);
        #[allow(clippy::if_not_else)]
        if self.eval_form(condition, gc)? != Object::NIL {
            self.eval_form(true_branch, gc)
        } else {
            self.implicit_progn(forms, gc)
        }
    }

    fn setq<'a, 'gc>(&mut self, obj: Object<'a>, gc: &'gc mut Arena) -> Result<Object<'gc>> {
        element_iter!(forms, obj);
        let mut arg_cnt = 0;
        let mut last_value = unsafe { Gc::new(RootObj::default()) };
        loop {
            match Self::pairs(&mut forms, gc) {
                Some((Object::Symbol(var), Some(val))) => {
                    root!(val, gc);
                    let val = self.eval_form(val, gc)?;
                    rebind!(val, gc);
                    last_value.set(self.var_set(!var, val, gc));
                }
                Some((other, Some(_))) => bail!(Error::from_object(Type::Symbol, other)),
                Some((_, None)) => bail!(Error::ArgCount(arg_cnt, arg_cnt + 1)),
                None => {
                    break;
                }
            };
            arg_cnt += 2;
        }
        if arg_cnt < 2 {
            Err(Error::ArgCount(2, 0).into())
        } else {
            Ok(last_value.bind(gc))
        }
    }

    fn pairs<'a, 'ob>(
        iter: &mut ElemStreamIter<'a>,
        gc: &'ob Arena,
    ) -> Option<(Object<'ob>, Option<Object<'ob>>)> {
        #[allow(clippy::manual_map)]
        if let Some(first) = iter.next() {
            Some((first.bind(gc), iter.next().map(|x| x.bind(gc))))
        } else {
            None
        }
    }

    fn var_ref<'a>(&self, sym: Symbol, gc: &'a Arena) -> Result<Object<'a>> {
        if sym.name.starts_with(':') {
            Ok(sym.into())
        } else {
            let mut iter = self.vars.iter().rev();
            match iter.find_map(|cons| (cons.obj().car(gc) == sym).then(|| cons.obj().cdr(gc))) {
                Some(value) => Ok(value),
                None => match self.env.vars().get(sym) {
                    Some(v) => Ok(gc.bind(v.obj())),
                    None => Err(anyhow!("Void variable: {sym}")),
                },
            }
        }
    }

    fn var_set<'a>(&mut self, name: Symbol, new_value: Object<'a>, gc: &'a Arena) -> Object<'a> {
        let mut iter = self.vars.iter().rev();
        match iter.find(|cons| (cons.obj().car(gc) == name)) {
            Some(value) => {
                // TODO: Fix this once cons is managed type
                let new_value = unsafe { std::mem::transmute::<Object<'a>, Object>(new_value) };
                value
                    .obj()
                    .set_cdr(new_value)
                    .expect("env should be mutable");
            }
            None => {
                Environment::set_var(self.env, name, new_value);
            }
        }
        new_value
    }

    #[allow(clippy::unused_self)]
    fn quote<'a, 'gc>(&self, value: Object<'a>, gc: &'gc Arena) -> Result<Object<'gc>> {
        let mut forms = value.as_list(gc)?;
        match forms.len() {
            1 => Ok(forms.next().unwrap()?),
            x => Err(Error::ArgCount(1, x as u16).into()),
        }
    }

    fn eval_let<'a, 'gc>(
        &mut self,
        form: Object<'a>,
        parallel: bool,
        gc: &'gc mut Arena,
    ) -> Result<Object<'gc>> {
        element_iter!(iter, form);
        let prev_len = self.vars.len();
        match iter.next() {
            // (let x ...)
            Some(x) => {
                let obj = x.obj();
                if parallel {
                    self.let_bind_parallel(obj, gc)?;
                } else {
                    self.let_bind_serial(obj, gc)?;
                }
            }
            // (let)
            None => bail!(Error::ArgCount(1, 0)),
        }
        let obj = self.implicit_progn(iter, gc)?;
        self.vars.truncate(prev_len);
        Ok(obj)
    }

    fn let_bind_serial<'a, 'gc>(&mut self, form: Object<'a>, gc: &'gc mut Arena) -> Result<()> {
        element_iter!(bindings, form);
        while let Some(binding) = bindings.next() {
            let binding = binding.obj();
            match binding {
                // (let ((x y)))
                Object::Cons(cons) => {
                    let var = self.let_bind_value(!cons, gc)?;
                    self.vars.push(var);
                }
                // (let (x))
                Object::Symbol(_) => {
                    let val = cons!(binding; gc);
                    let obj: &Cons = val.try_into().unwrap();
                    self.vars.push(obj);
                }
                // (let (1))
                x => bail!(Error::from_object(Type::Cons, x)),
            }
        }
        Ok(())
    }

    fn let_bind_parallel<'a, 'gc>(&mut self, form: Object<'a>, gc: &'gc mut Arena) -> Result<()> {
        let mut let_bindings: Gc<Vec<RootCons>> = unsafe { Gc::new(Vec::new()) };
        element_iter!(bindings, form);
        while let Some(binding) = bindings.next() {
            let binding = binding.obj();
            match binding {
                // (let ((x y)))
                Object::Cons(cons) => {
                    let var = self.let_bind_value(!cons, gc)?;
                    let_bindings.push(var);
                }
                // (let (x))
                Object::Symbol(_) => {
                    let val = cons!(binding; gc);
                    let cons: &Cons = val.try_into().unwrap();
                    let_bindings.push(cons);
                }
                // (let (1))
                x => bail!(Error::from_object(Type::Cons, x)),
            }
        }
        self.vars.append(&mut let_bindings);
        Ok(())
    }

    fn let_bind_value<'a, 'gc>(
        &mut self,
        cons: &'a Cons<'a>,
        gc: &'gc mut Arena,
    ) -> Result<&'gc Cons<'gc>> {
        element_iter!(iter, cons.cdr(gc));
        let value = match iter.next() {
            // (let ((x y)))
            Some(x) => self.eval_form(x.obj(), gc)?,
            // (let ((x)))
            None => Object::NIL,
        };
        // (let ((x y z ..)))
        ensure!(iter.is_empty(), "Let binding forms can only have 1 value");
        rebind!(value, gc);
        let name: Symbol = cons.car(gc).try_into()?;
        let val = cons!(name, value; gc);
        Ok(val.try_into().unwrap())
    }

    fn implicit_progn<'rt, 'gc>(
        &mut self,
        mut forms: ElemStreamIter<'rt>,
        gc: &'gc mut Arena,
    ) -> Result<Object<'gc>> {
        let mut last = unsafe { Gc::new(RootObj::default()) };
        while let Some(form) = forms.next() {
            last.set(self.eval_form(form.obj(), gc)?);
        }
        Ok(last.bind(gc))
    }
}

defsubr!(eval);

#[cfg(test)]
mod test {
    use crate::{arena::RootSet, symbol::intern};

    use super::*;

    macro_rules! check_interpreter {
        ($compare:expr, $expect:expr) => {{
            let roots = &RootSet::default();
            let comp_arena = &mut Arena::new(roots);
            let comp_env = &mut unsafe { Gc::new(Environment::default()) };
            println!("Test String: {}", $compare);
            let obj = crate::reader::read($compare, comp_arena).unwrap().0;
            let expect: Object = comp_arena.add($expect);
            root!(obj, comp_arena);
            root!(expect, comp_arena);
            assert_eq!(eval(obj, None, comp_env, comp_arena).unwrap(), expect);
        }};
    }

    #[test]
    fn basic() {
        let roots = &RootSet::default();
        let arena = &Arena::new(roots);
        check_interpreter!("1", 1);
        check_interpreter!("1.5", 1.5);
        check_interpreter!("nil", false);
        check_interpreter!("t", true);
        check_interpreter!("\"foo\"", "foo");
        check_interpreter!("'(1 2)", list!(1, 2; arena));
    }

    #[test]
    fn variables() {
        check_interpreter!("(let ())", false);
        check_interpreter!("(let (x) x)", false);
        check_interpreter!("(let ((x 1)) x)", 1);
        check_interpreter!("(let ((x 1)))", false);
        check_interpreter!("(let ((x 1) (y 2)) x y)", 2);
        check_interpreter!("(let ((x 1)) (let ((x 3)) x))", 3);
        check_interpreter!("(let ((x 1)) (let ((y 3)) x))", 1);
        check_interpreter!("(let ((x 1)) (setq x 2) x)", 2);
        check_interpreter!("(let* ())", false);
        check_interpreter!("(let* ((x 1) (y x)) y)", 1);
    }

    #[test]
    fn conditionals() {
        check_interpreter!("(if nil 1)", false);
        check_interpreter!("(if t 1)", 1);
        check_interpreter!("(if nil 1 2)", 2);
        check_interpreter!("(if t 1 2)", 1);
        check_interpreter!("(if nil 1 2 3)", 3);
        check_interpreter!("(and)", true);
        check_interpreter!("(and 1)", 1);
        check_interpreter!("(and 1 2)", 2);
        check_interpreter!("(and 1 nil)", false);
        check_interpreter!("(and nil 1)", false);
        check_interpreter!("(or)", false);
        check_interpreter!("(or nil)", false);
        check_interpreter!("(or nil 1)", 1);
        check_interpreter!("(or 1 2)", 1);
        check_interpreter!("(cond)", false);
        check_interpreter!("(cond nil)", false);
        check_interpreter!("(cond (1))", 1);
        check_interpreter!("(cond (1 2))", 2);
        check_interpreter!("(cond (nil 1) (2 3))", 3);
        check_interpreter!("(cond (nil 1) (2 3) (4 5))", 3);
    }

    #[test]
    fn special_forms() {
        check_interpreter!("(prog1 1 2 3)", 1);
        check_interpreter!("(prog2 1 2 3)", 2);
        check_interpreter!("(progn 1 2 3 4)", 4);
        check_interpreter!("(function 1)", 1);
        check_interpreter!("(quote 1)", 1);
        check_interpreter!("(if 1 2 3)", 2);
        check_interpreter!("(if nil 2 3)", 3);
        check_interpreter!("(if (and 1 nil) 2 3)", 3);
    }

    #[test]
    fn test_functions() {
        let roots = &RootSet::default();
        let arena = &Arena::new(roots);
        check_interpreter!(
            "(function (lambda))",
            list![&sym::CLOSURE, list![true; arena]; arena]
        );
        let x = intern("x");
        let y = intern("y");
        check_interpreter!(
            "(function (lambda (x) x))",
            list![&sym::CLOSURE, list![true; arena], list![x; arena], x; arena]
        );
        check_interpreter!(
            "(let ((y 1)) (function (lambda (x) x)))",
            list![&sym::CLOSURE, list![cons!(y, 1; arena), true; arena], list![x; arena], x; arena]
        );

        check_interpreter!(
            "(let ((x #'(lambda (x &optional y &rest z) (cons x (cons y z))))) (funcall x 5))",
            list!(5, false; arena)
        );
        check_interpreter!(
            "(let ((x #'(lambda (x &optional y &rest z) (cons x (cons y z))))) (funcall x 5 7))",
            list!(5, 7; arena)
        );
        check_interpreter!(
            "(let ((x #'(lambda (x &optional y &rest z) (cons x (cons y z))))) (funcall x 5 7 11))",
            list!(5, 7, 11; arena)
        );
    }

    #[test]
    fn test_call() {
        check_interpreter!("(let ((x #'(lambda (x) x))) (funcall x 5))", 5);
        check_interpreter!("(let ((x #'(lambda () 3))) (funcall x))", 3);
        check_interpreter!(
            "(progn (defalias 'int-test-call #'(lambda (x) (+ x 3)))  (int-test-call 7))",
            10
        );
        // Test closures
        check_interpreter!("(let* ((y 7)(x #'(lambda () y))) (funcall x))", 7);
        check_interpreter!("(let* ((y 7)(x #'(lambda (x) (+ x y)))) (funcall x 3))", 10);
        // Test that closures capture their environments
        check_interpreter!(
            "(progn (setq func (let ((x 3)) #'(lambda (y) (+ y x)))) (funcall func 5))",
            8
        );
        // Test multiple closures
        check_interpreter!("(progn (setq funcs (let ((x 3)) (cons #'(lambda (y) (+ y x)) #'(lambda (y) (- y x))))) (* (funcall (car funcs) 5) (funcall (cdr funcs) 1)))", -16);
        // Test that closures close over variables
        check_interpreter!("(progn (setq funcs (let ((x 3)) (cons #'(lambda (y) (setq x y)) #'(lambda (y) (+ y x))))) (funcall (car funcs) 5) (funcall (cdr funcs) 4))", 9);
        // Test that closures in global function close over values and not
        // variables
        check_interpreter!("(progn (setq func (let ((x 3)) (defalias 'int-test-no-cap #'(lambda (y) (+ y x))) #'(lambda (y) (setq x y)))) (funcall func 4) (int-test-no-cap 5))", 8);
    }
}
