use crate::arena::{GcCell, IntoRoot, RootCons, RootObj};
use crate::cons::ElemStreamIter;
use crate::error::{Error, Type};
use crate::lcell::LCellOwner;
use crate::object::{Callable, Function, List};
use crate::symbol::sym;
use crate::{arena::Arena, cons::Cons, data::Environment, object::Object, symbol::Symbol};
use crate::{element_iter, rebind, root, root_struct};
use anyhow::{anyhow, bail, ensure, Result};
use fn_macros::defun;
use streaming_iterator::StreamingIterator;

struct Interpreter<'id, 'brw> {
    vars: &'brw GcCell<'id, Vec<RootCons>>,
    env: &'brw GcCell<'id, Environment>,
    owner: &'brw mut LCellOwner<'id>,
}

#[defun]
pub(crate) fn eval<'ob, 'id>(
    form: Object<'ob>,
    lexical: Option<Object<'ob>>,
    env: &GcCell<'id, Environment>,
    arena: &'ob mut Arena,
    owner: &mut LCellOwner<'id>,
) -> Result<Object<'ob>> {
    ensure!(
        matches!(lexical, Some(Object::True(_) | Object::Nil(_)) | None),
        "lexical enviroments are not yet supported: found {:?}",
        lexical
    );
    root_struct!(vars, Vec::new(), arena);
    let mut interpreter = Interpreter { vars, env, owner };
    interpreter.eval_form(form, arena)
}

pub(crate) fn call<'ob, 'gc, 'id>(
    form: Object<'ob>,
    args: &GcCell<'id, Vec<RootObj>>,
    env: &GcCell<'id, Environment>,
    gc: &'gc mut Arena,
    owner: &mut LCellOwner<'id>,
) -> Result<Object<'gc>> {
    root_struct!(vars, Vec::new(), gc);
    let mut frame = Interpreter { vars, env, owner };
    frame.call_closure(form.try_into()?, args, gc)
}

impl<'id, 'brw> Interpreter<'id, 'brw> {
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
        element_iter!(forms, obj, gc);
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
                self.var_set(name, value, gc);
                Ok(value)
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

    fn bind_variables<'a>(
        &self,
        forms: &mut ElemStreamIter<'_, '_>,
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
        args: &GcCell<'id, Vec<RootObj>>,
        gc: &'gc mut Arena,
    ) -> Result<Object<'gc>> {
        match closure.car(gc) {
            Object::Symbol(sym) if !sym == &sym::CLOSURE => {
                let obj = closure.cdr(gc);
                element_iter!(forms, obj, gc);
                // TODO: remove this temp vector
                let args = args.borrow(self.owner).iter().map(|x| x.bind(gc)).collect();
                let vars = self.bind_variables(&mut forms, args, gc)?;
                root_struct!(vars, vars.into_root(), gc);
                let mut call_frame = Interpreter {
                    vars,
                    env: self.env,
                    owner: self.owner,
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
        let resolved = match name.resolve_callable(gc) {
            Some(x) => x,
            None => bail!("Invalid function: {name}"),
        };

        let tmp: Object = resolved.into();
        root!(tmp, gc); // Root callable
        let callable: Callable = tmp.try_into().unwrap();

        match callable {
            Callable::LispFn(_) => todo!("call lisp functions in interpreter"),
            Callable::SubrFn(func) => {
                element_iter!(iter, obj, gc);
                root_struct!(args, Vec::new(), gc);
                while let Some(x) = iter.next() {
                    let result = self.eval_form(x.obj(), gc)?;
                    rebind!(result, gc);
                    args.borrow_mut(self.owner, gc).push(result);
                }
                if crate::debug::debug_enabled() {
                    println!("({name} {args:?})");
                }
                gc.garbage_collect();
                (*func).call(args, self.env, gc, self.owner)
            }
            Callable::Cons(form) => {
                match form.try_as_macro(gc) {
                    Ok(mcro) => {
                        let macro_args = obj.as_list(gc)?.collect::<Result<Vec<_>>>()?;
                        if crate::debug::debug_enabled() {
                            println!("(macro: {name} {macro_args:?})");
                        }
                        root_struct!(args, macro_args.into_root(), gc);
                        let macro_func: Function = mcro.try_into().unwrap();
                        let tmp: Object = macro_func.into();
                        root!(tmp, gc); // Root callable
                        let func: Function = tmp.try_into().unwrap();
                        let value = func.call(args, self.env, gc, self.owner)?;
                        rebind!(value, gc);
                        root!(value, gc);
                        self.eval_form(value, gc)
                    }
                    Err(_) => {
                        let tmp = Object::Cons(form);
                        root!(tmp, gc); // Root callable
                        match form.car(gc) {
                            Object::Symbol(sym) if !sym == &sym::CLOSURE => {
                                element_iter!(iter, obj, gc);
                                root_struct!(args, Vec::new(), gc);
                                while let Some(x) = iter.next() {
                                    let result = self.eval_form(x.obj(), gc)?;
                                    rebind!(result, gc);
                                    args.borrow_mut(self.owner, gc).push(result);
                                }
                                if crate::debug::debug_enabled() {
                                    println!("({name} {args:?})");
                                }
                                if crate::debug::debug_enabled() {
                                    println!("({name} {args:?})");
                                }
                                if let Object::Cons(closure) = tmp {
                                    self.call_closure(!closure, args, gc)
                                } else {
                                    unreachable!();
                                }
                            }
                            other => Err(anyhow!("Invalid Function: {other}")),
                        }
                    }
                }
            }
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
                            .borrow(self.owner)
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
        root_struct!(returned_form, None, gc);
        element_iter!(forms, obj, gc);
        while let Some(form) = forms.next() {
            let value = self.eval_form(form.obj(), gc)?;
            count += 1;
            if prog_num == count {
                rebind!(value, gc);
                returned_form.borrow_mut(self.owner, gc).set(value);
            }
        }
        match &**returned_form.borrow(self.owner) {
            Some(x) => Ok(gc.bind(x.obj())),
            None => Err(Error::ArgCount(prog_num, count).into()),
        }
    }

    fn eval_progn<'a, 'gc>(&mut self, obj: Object<'a>, gc: &'gc mut Arena) -> Result<Object<'gc>> {
        element_iter!(forms, obj, gc);
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
            element_iter!(forms, obj, gc);
            self.implicit_progn(forms, gc)?;
        }
        Ok(Object::NIL)
    }

    fn eval_cond<'a, 'gc>(&mut self, obj: Object<'a>, gc: &'gc mut Arena) -> Result<Object<'gc>> {
        element_iter!(forms, obj, gc);
        while let Some(form) = forms.next() {
            element_iter!(clause, form.obj(), gc);
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
        root_struct!(last, RootObj::new(Object::TRUE), gc);
        element_iter!(forms, obj, gc);
        while let Some(form) = forms.next() {
            let result = self.eval_form(form.obj(), gc)?;
            if result == Object::NIL {
                return Ok(Object::NIL);
            }
            rebind!(result, gc);
            last.borrow_mut(self.owner, gc).set(result);
        }
        Ok(gc.bind(last.borrow(self.owner).obj()))
    }

    fn eval_or<'a, 'gc>(&mut self, obj: Object<'a>, gc: &'gc mut Arena) -> Result<Object<'gc>> {
        element_iter!(forms, obj, gc);
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
        element_iter!(forms, obj, gc);
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
        element_iter!(forms, obj, gc);
        let mut arg_cnt = 0;
        root_struct!(last_value, RootObj::default(), gc);
        loop {
            match Self::pairs(&mut forms, gc) {
                Some((Object::Symbol(var), Some(val))) => {
                    root!(val, gc);
                    let val = self.eval_form(val, gc)?;
                    rebind!(val, gc);
                    self.var_set(!var, val, gc);
                    last_value.borrow_mut(self.owner, gc).set(val);
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
            Ok(last_value.borrow(self.owner).bind(gc))
        }
    }

    fn pairs<'ob>(
        iter: &mut ElemStreamIter<'_, '_>,
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
            let mut iter = self.vars.borrow(self.owner).iter().rev();
            match iter.find_map(|cons| (cons.obj().car(gc) == sym).then(|| cons.obj().cdr(gc))) {
                Some(value) => Ok(value),
                None => match self.env.borrow(self.owner).vars().get(sym) {
                    Some(v) => Ok(gc.bind(v.obj())),
                    None => Err(anyhow!("Void variable: {sym}")),
                },
            }
        }
    }

    fn var_set<'a>(&mut self, name: Symbol, new_value: Object<'a>, gc: &'a Arena) {
        let mut iter = self.vars.borrow(self.owner).iter().rev();
        match iter.find(|cons| (cons.obj().car(gc) == name)) {
            Some(value) => {
                // TODO: Fix this once cons is managed type
                let new_value = unsafe { std::mem::transmute::<Object<'a>, Object>(new_value) };
                value.obj().set_cdr(new_value);
            }
            None => {
                Environment::set_var(self.env.borrow_mut(self.owner, gc), name, new_value);
            }
        }
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
        element_iter!(iter, form, gc);
        let prev_len = self.vars.borrow(self.owner).len();
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
        rebind!(obj, gc);
        self.vars.borrow_mut(self.owner, gc).truncate(prev_len);
        Ok(obj)
    }

    fn let_bind_serial<'a, 'gc>(&mut self, form: Object<'a>, gc: &'gc mut Arena) -> Result<()> {
        element_iter!(bindings, form, gc);
        while let Some(binding) = bindings.next() {
            let binding = binding.obj();
            match binding {
                // (let ((x y)))
                Object::Cons(cons) => {
                    let var = self.let_bind_value(!cons, gc)?;
                    // TODO: Fix this tmp transmute
                    let tmp: Object = Object::Cons(var.into());
                    rebind!(tmp, gc);
                    let var: &Cons = tmp.try_into().unwrap();
                    self.vars.borrow_mut(self.owner, gc).push(var);
                }
                // (let (x))
                Object::Symbol(_) => {
                    let val = cons!(binding; gc);
                    let obj: &Cons = val.try_into().unwrap();
                    self.vars.borrow_mut(self.owner, gc).push(obj);
                }
                // (let (1))
                x => bail!(Error::from_object(Type::Cons, x)),
            }
        }
        Ok(())
    }

    fn let_bind_parallel<'a, 'gc>(&mut self, form: Object<'a>, gc: &'gc mut Arena) -> Result<()> {
        root_struct!(let_bindings, Vec::new(), gc);
        element_iter!(bindings, form, gc);
        while let Some(binding) = bindings.next() {
            let binding = binding.obj();
            match binding {
                // (let ((x y)))
                Object::Cons(cons) => {
                    let var = self.let_bind_value(!cons, gc)?;
                    // TODO: Fix this tmp transmute
                    let tmp: Object = Object::Cons(var.into());
                    rebind!(tmp, gc);
                    let var: &Cons = tmp.try_into().unwrap();
                    let_bindings.borrow_mut(self.owner, gc).push(var);
                }
                // (let (x))
                Object::Symbol(_) => {
                    let val = cons!(binding; gc);
                    let cons: &Cons = val.try_into().unwrap();
                    let_bindings.borrow_mut(self.owner, gc).push(cons);
                }
                // (let (1))
                x => bail!(Error::from_object(Type::Cons, x)),
            }
        }
        let (vars, let_bindings) = GcCell::borrow_mut2(self.vars, let_bindings, self.owner, gc);
        vars.append(let_bindings);
        Ok(())
    }

    fn let_bind_value<'a, 'gc>(
        &mut self,
        cons: &'a Cons<'a>,
        gc: &'gc mut Arena,
    ) -> Result<&'gc Cons<'gc>> {
        element_iter!(iter, cons.cdr(gc), gc);
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

    fn implicit_progn<'gc>(
        &mut self,
        mut forms: ElemStreamIter<'_, '_>,
        gc: &'gc mut Arena,
    ) -> Result<Object<'gc>> {
        root_struct!(last, RootObj::default(), gc);
        while let Some(form) = forms.next() {
            let value = self.eval_form(form.obj(), gc)?;
            rebind!(value, gc);
            last.borrow_mut(self.owner, gc).set(value);
        }
        Ok(last.borrow_mut(self.owner, gc).bind(gc))
    }
}

defsubr!(eval);

#[cfg(test)]
mod test {
    use crate::{arena::RootSet, make_lcell_owner, object::IntoObject, symbol::intern};

    use super::*;

    fn check_interpreter<'ob, T>(test_str: &str, expect: T, arena: &'ob mut Arena)
    where
        T: IntoObject<'ob, Object<'ob>>,
    {
        root_struct!(env, Environment::default(), arena);
        make_lcell_owner!(owner);
        // Work around for not having GAT's. Currently the IntoObject trait
        // must define the lifetime in it's defintion, but that means that the
        // lifetime in this generic function of the object has to last for the
        // entire function body.
        let expect: Object = {
            let arena: &'ob mut Arena = unsafe { &mut *(arena as *mut Arena) };
            expect.into_obj(arena)
        };
        root!(expect, arena);
        println!("Test String: {}", test_str);
        let obj = crate::reader::read(test_str, arena).unwrap().0;
        root!(obj, arena);
        let compare = eval(obj, None, env, arena, &mut owner).unwrap();
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
        let list = list![&sym::CLOSURE, list![true; arena]; arena];
        root!(list, arena);
        check_interpreter("(function (lambda))", list, arena);
        let x = intern("x");
        let y = intern("y");
        let list = list![&sym::CLOSURE, list![true; arena], list![x; arena], x; arena];
        root!(list, arena);
        check_interpreter("(function (lambda (x) x))", list, arena);
        let list =
            list![&sym::CLOSURE, list![cons!(y, 1; arena), true; arena], list![x; arena], x; arena];
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
    }
}
