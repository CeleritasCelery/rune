//! The basic elisp interpreter.
use crate::core::{
    cons::{Cons, ElemStreamIter},
    env::{sym, Env, Symbol},
    error::{ArgError, ErrorType, EvalError, EvalResult, Type, TypeError},
    gc::{Context, Rt},
    object::{nil, qtrue, Function, Gc, GcObj, List, Object},
};
use anyhow::Context as _;
use anyhow::Result as AnyResult;
use anyhow::{anyhow, bail, ensure};
use fallible_iterator::FallibleIterator;
use fallible_streaming_iterator::FallibleStreamingIterator;
use rune_core::macros::{bail_err, cons, error, rebind, root, rooted_iter};
use rune_macros::defun;

struct Interpreter<'brw> {
    vars: &'brw mut Rt<Vec<&'static Cons>>,
    env: &'brw mut Rt<Env>,
}

#[defun]
pub(crate) fn eval<'ob>(
    form: &Rt<GcObj>,
    _lexical: Option<()>,
    env: &mut Rt<Env>,
    cx: &'ob mut Context,
) -> Result<GcObj<'ob>, anyhow::Error> {
    cx.garbage_collect(false);
    root!(vars, Vec::new(), cx);
    let mut interpreter = Interpreter { vars, env };
    interpreter.eval_form(form, cx).map_err(Into::into)
}

impl Interpreter<'_> {
    fn eval_form<'ob>(&mut self, rt: &Rt<GcObj>, cx: &'ob mut Context) -> EvalResult<'ob> {
        match rt.get(cx) {
            Object::Symbol(sym) => self.var_ref(sym, cx),
            Object::Cons(_) => {
                let x = rt.try_into().unwrap();
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
        match cons.car().untag() {
            Object::Symbol(sym) => match sym {
                sym::QUOTE => self.quote(forms.bind(cx)),
                sym::LET => self.eval_let(forms, true, cx),
                sym::LET_STAR => self.eval_let(forms, false, cx),
                sym::IF => self.eval_if(forms, cx),
                sym::AND => self.eval_and(forms, cx),
                sym::OR => self.eval_or(forms, cx),
                sym::COND => self.eval_cond(forms, cx),
                sym::WHILE => self.eval_while(forms, cx),
                sym::PROGN | sym::INLINE => self.eval_progn(forms, cx),
                sym::PROG1 => self.eval_progx(forms, 1, cx),
                sym::PROG2 => self.eval_progx(forms, 2, cx),
                sym::SETQ => self.setq(forms, cx),
                sym::DEFVAR | sym::DEFCONST => self.defvar(forms, cx),
                sym::FUNCTION => self.eval_function(forms, cx),
                sym::INTERACTIVE => Ok(nil()), // TODO: implement
                sym::CATCH => self.catch(forms, cx),
                sym::THROW => self.throw(forms.bind(cx), cx),
                sym::CONDITION_CASE => self.condition_case(forms, cx),
                sym::SAVE_CURRENT_BUFFER => self.save_current_buffer(forms, cx),
                sym::SAVE_EXCURSION => self.save_excursion(forms, cx),
                sym::UNWIND_PROTECT => self.unwind_protect(forms, cx),
                _ => {
                    root!(sym, cx);
                    self.eval_call(sym, forms, cx)
                }
            },
            other => Err(error!("Invalid Function: {other}")),
        }
    }

    fn catch<'ob>(&mut self, obj: &Rt<GcObj>, cx: &'ob mut Context) -> EvalResult<'ob> {
        rooted_iter!(forms, obj, cx);
        let Some(tag) = forms.next()? else { bail_err!(ArgError::new(1, 0, "catch")) };
        // push this tag on the catch stack
        self.env.catch_stack.push(tag);
        let result = match self.implicit_progn(forms, cx) {
            Ok(x) => Ok(rebind!(x, cx)),
            Err(e) => {
                if let ErrorType::Throw(id) = e.error {
                    if let Some((throw_tag, data)) = self.env.get_exception(id) {
                        let catch_tag = self.env.catch_stack.last().unwrap();
                        if catch_tag == throw_tag {
                            return Ok(data.bind(cx));
                        }
                    }
                }
                Err(e)
            }
        };
        // pop this tag from the catch stack
        self.env.catch_stack.bind_mut(cx).pop();
        result
    }

    fn throw<'ob>(&mut self, obj: GcObj, cx: &'ob Context) -> EvalResult<'ob> {
        let mut forms = obj.as_list()?;
        let len = forms.len()? as u16;
        if len != 2 {
            bail_err!(ArgError::new(2, len, "throw"));
        }
        let tag = forms.next().unwrap()?;
        let value = forms.next().unwrap()?;
        // Need to check now that there is a catch, because we may have a
        // condition-case along the unwind path
        if self.env.catch_stack.iter().any(|x| x.bind(cx) == tag) {
            Err(EvalError::throw(tag, value, self.env))
        } else {
            Err(error!("No catch for {tag}"))
        }
    }

    fn defvar<'ob>(&mut self, obj: &Rt<GcObj>, cx: &'ob mut Context) -> EvalResult<'ob> {
        rooted_iter!(forms, obj, cx);
        // (defvar x ...)                 // (defvar)
        let Some(sym) = forms.next()? else { bail_err!(ArgError::new(1, 0, "defvar")) };
        let name: Symbol = sym.bind(cx).try_into()?;
        root!(name, cx);
        let value = match forms.next()? {
            // (defvar x y)
            Some(value) => rebind!(self.eval_form(value, cx)?),
            // (defvar x)
            None => nil(),
        };
        self.env.defvar(name.bind(cx), value)?;
        Ok(value)
    }

    fn eval_call<'ob>(
        &mut self,
        sym: &Rt<Symbol>,
        args: &Rt<GcObj>,
        cx: &'ob mut Context,
    ) -> EvalResult<'ob> {
        let Some(func) = sym.bind(cx).follow_indirect(cx) else {
            bail_err!("Invalid function: {sym}")
        };
        root!(func, cx);

        match func.get(cx) {
            Function::Cons(cons) if cons.car() == sym::AUTOLOAD => {
                crate::eval::autoload_do_load(func.use_as(), None, None, self.env, cx)
                    .map_err(|e| add_trace(e, "autoload", &[]))?;
                func.set(sym.bind(cx).follow_indirect(cx).unwrap());
            }
            Function::Cons(form) if form.car() == sym::MACRO => {
                let mcro: Gc<Function> = form.cdr().try_into()?;
                let macro_args: Vec<_> = args.bind(cx).as_list()?.fallible().collect()?;
                root!(macro_args, cx);
                root!(mcro, cx);
                let name = sym.bind(cx).name().to_owned();
                let value = mcro.call(macro_args, Some(&name), self.env, cx)?;
                root!(value, cx);
                return self.eval_form(value, cx);
            }
            _ => (),
        }

        rooted_iter!(iter, args, cx);
        root!(args, Vec::new(), cx);
        while let Some(x) = iter.next()? {
            let result = self.eval_form(x, cx)?;
            args.push(result);
        }
        let name = sym.bind(cx).name().to_owned();
        func.call(args, Some(&name), self.env, cx)
    }

    fn eval_function<'ob>(
        &mut self,
        obj: &Rt<GcObj<'ob>>,
        cx: &'ob mut Context,
    ) -> EvalResult<'ob> {
        let mut forms = obj.bind(cx).as_list()?;
        let len = forms.len()? as u16;
        if len != 1 {
            bail_err!(ArgError::new(1, len, "function"))
        }

        let form = forms.next().unwrap()?;
        root!(form, cx); // Polonius
        let Object::Cons(cons) = form.bind(cx).untag() else { return Ok(form.bind(cx)) };
        if cons.car() != sym::LAMBDA {
            return Ok(form.bind(cx));
        }
        let env = {
            let mut tail = cons!(true; cx);
            for var in self.vars.iter().rev() {
                tail = cons!(var.bind(cx), tail; cx);
            }
            tail
        };
        Self::replace_doc_symbol(cons, cx)?;
        if let Some(closure_fn) = self.env.vars.get(sym::INTERNAL_MAKE_INTERPRETED_CLOSURE_FUNCTION)
        {
            if closure_fn.bind(cx) != sym::NIL {
                let closure_fn: Result<&Rt<Gc<Function>>, _> = closure_fn.try_into();
                if let Ok(closure_fn) = closure_fn {
                    root!(closure_fn, cx);
                    root!(args, Vec::new(), cx);
                    args.push(form.bind(cx));
                    args.push(env);
                    return closure_fn.call(args, None, self.env, cx);
                }
            }
        }
        let end = Cons::new(env, cons.cdr(), cx);
        Ok(cons!(sym::CLOSURE, end; cx))
    }

    /// Handle special case of oclosure documentation symbol
    fn replace_doc_symbol(cons: &Cons, cx: &Context) -> Result<(), EvalError> {
        let Some(doc_str) = cons.conses().fallible().nth(2)? else { return Ok(()) };
        let Object::Cons(c) = doc_str.car().untag() else { return Ok(()) };
        if c.car() != sym::KW_DOCUMENTATION {
            return Ok(());
        }
        let Object::Cons(c) = c.cdr().untag() else { return Ok(()) };
        let Object::Cons(c) = c.car().untag() else { return Ok(()) };
        let Some(sym) = c.elements().nth(1) else { return Ok(()) };
        match sym?.untag() {
            Object::Symbol(s) if s != sym::NIL => {
                let doc = cx.add(s.name());
                Ok(doc_str.set_car(doc)?)
            }
            _ => Ok(()),
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
        while let Some(form) = forms.next()? {
            let value = self.eval_form(form, cx)?;
            count += 1;
            if prog_num == count {
                returned_form.set(value);
            }
        }
        match &**returned_form {
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
        let (condition, body) = {
            let list: Gc<List> = obj.bind(cx).try_into()?;
            match list.untag() {
                List::Nil => bail_err!(ArgError::new(1, 0, "while")),
                List::Cons(cons) => (cons.car(), cons.cdr()),
            }
        };
        root!(condition, cx);
        root!(body, cx);
        while self.eval_form(condition, cx)? != nil() {
            rooted_iter!(forms, &*body, cx);
            self.implicit_progn(forms, cx)?;
        }
        Ok(nil())
    }

    fn eval_cond<'ob>(&mut self, obj: &Rt<GcObj>, cx: &'ob mut Context) -> EvalResult<'ob> {
        rooted_iter!(forms, obj, cx);
        while let Some(form) = forms.next()? {
            rooted_iter!(clause, form, cx);
            if let Some(first) = clause.next()? {
                let condition = self.eval_form(first, cx)?;
                if condition != nil() {
                    return if clause.is_empty() {
                        Ok(rebind!(condition, cx))
                    } else {
                        self.implicit_progn(clause, cx)
                    };
                }
            }
        }
        Ok(nil())
    }

    fn eval_and<'ob>(&mut self, obj: &Rt<GcObj>, cx: &'ob mut Context) -> EvalResult<'ob> {
        root!(last, qtrue(), cx);
        rooted_iter!(forms, obj, cx);
        while let Some(form) = forms.next()? {
            let result = self.eval_form(form, cx)?;
            if result == nil() {
                return Ok(nil());
            }
            last.set(result);
        }
        Ok(last.bind(cx))
    }

    fn eval_or<'ob>(&mut self, obj: &Rt<GcObj>, cx: &'ob mut Context) -> EvalResult<'ob> {
        rooted_iter!(forms, obj, cx);
        while let Some(form) = forms.next()? {
            let result = self.eval_form(form, cx)?;
            if result != nil() {
                return Ok(rebind!(result, cx));
            }
        }
        Ok(nil())
    }

    fn eval_if<'ob>(&mut self, obj: &Rt<GcObj>, cx: &'ob mut Context) -> EvalResult<'ob> {
        rooted_iter!(forms, obj, cx);
        let Some(condition) = forms.next()? else { bail_err!(ArgError::new(2, 0, "if")) };
        root!(condition, cx);
        let Some(true_branch) = forms.next()? else { bail_err!(ArgError::new(2, 1, "if")) };
        root!(true_branch, cx);
        #[allow(clippy::if_not_else)]
        if self.eval_form(condition, cx)? != nil() {
            self.eval_form(true_branch, cx)
        } else {
            self.implicit_progn(forms, cx)
        }
    }

    fn setq<'ob>(&mut self, obj: &Rt<GcObj>, cx: &'ob mut Context) -> EvalResult<'ob> {
        rooted_iter!(forms, obj, cx);
        let mut arg_cnt = 0;
        root!(last_value, nil(), cx);
        while let Some((var, val)) = Self::pairs(&mut forms, cx)? {
            match (var.untag(), val) {
                (Object::Symbol(var), Some(val)) => {
                    root!(var, cx);
                    root!(val, cx);
                    let val = rebind!(self.eval_form(val, cx)?);
                    self.var_set(var.bind(cx), val, cx)?;
                    last_value.set(val);
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
        iter: &mut ElemStreamIter<'_>,
        cx: &'ob Context,
    ) -> AnyResult<Option<(GcObj<'ob>, Option<GcObj<'ob>>)>> {
        let first = iter.next()?.map(|x| x.bind(cx));
        let second = iter.next()?.map(|x| x.bind(cx));
        Ok(first.map(|first| (first, second)))
    }

    fn var_ref<'ob>(&self, sym: Symbol, cx: &'ob Context) -> EvalResult<'ob> {
        if sym.is_const() {
            Ok(sym.into())
        } else {
            let mut iter = self.vars.iter().rev();
            match iter.find_map(|cons| (cons.car(cx) == sym).then(|| cons.cdr(cx))) {
                Some(value) => Ok(value),
                None => match self.env.vars.get(sym) {
                    Some(v) => Ok(v.bind(cx)),
                    None => Err(error!("Void variable: {sym}")),
                },
            }
        }
    }

    fn var_set(&mut self, name: Symbol, new_value: GcObj, cx: &Context) -> AnyResult<()> {
        let mut iter = self.vars.iter().rev();
        match iter.find(|cons| (cons.car(cx) == name)) {
            Some(value) => {
                value.bind(cx).set_cdr(new_value).expect("variables should never be immutable");
                Ok(())
            }
            None => self.env.set_var(name, new_value),
        }
    }

    fn quote<'ob>(&self, value: GcObj<'ob>) -> EvalResult<'ob> {
        let mut forms = value.as_list()?;
        match forms.len()? {
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
        // (let x ...)                   // (let)
        let Some(obj) = iter.next()? else { bail_err!(ArgError::new(1, 0, "let")) };
        let varbind_count = if parallel {
            self.let_bind_parallel(obj, cx)
        } else {
            self.let_bind_serial(obj, cx)
        }?;
        let obj = rebind!(self.implicit_progn(iter, cx)?);
        // Remove old bindings
        self.vars.truncate(prev_len);
        self.env.unbind(varbind_count, cx);
        Ok(obj)
    }

    fn let_bind_serial(&mut self, form: &Rt<GcObj>, cx: &mut Context) -> Result<u16, EvalError> {
        let mut varbind_count = 0;
        rooted_iter!(bindings, form, cx);
        while let Some(binding) = bindings.next()? {
            match binding.get(cx) {
                // (let ((x y)))
                Object::Cons(_) => {
                    let cons = binding.as_cons();
                    let val = rebind!(self.let_bind_value(cons, cx)?);
                    let var: Symbol =
                        cons.get(cx).car().try_into().context("let variable must be a symbol")?;
                    varbind_count += self.create_let_binding(var, val, cx);
                }
                // (let (x))
                Object::Symbol(sym) => {
                    varbind_count += self.create_let_binding(sym, nil(), cx);
                }
                // (let (1))
                x => bail_err!(TypeError::new(Type::Cons, x)),
            }
        }
        Ok(varbind_count)
    }

    fn let_bind_parallel(&mut self, form: &Rt<GcObj>, cx: &mut Context) -> Result<u16, EvalError> {
        root!(let_bindings, Vec::new(), cx);
        rooted_iter!(bindings, form, cx);
        while let Some(binding) = bindings.next()? {
            match binding.get(cx) {
                // (let ((x y)))
                Object::Cons(_) => {
                    let cons = binding.as_cons();
                    let var = rebind!(self.let_bind_value(cons, cx)?);
                    let sym: Symbol =
                        cons.get(cx).car().try_into().context("let variable must be a symbol")?;
                    let_bindings.push((sym, var));
                }
                // (let (x))
                Object::Symbol(sym) => {
                    let_bindings.push((sym, nil()));
                }
                // (let (1))
                x => bail_err!(TypeError::new(Type::Cons, x)),
            }
        }
        let mut sum = 0;
        for (var, val) in let_bindings.bind_ref(cx) {
            sum += self.create_let_binding(*var, *val, cx);
        }
        Ok(sum)
    }

    fn create_let_binding(&mut self, var: Symbol, val: GcObj, cx: &Context) -> u16 {
        if var.is_special() {
            self.env.varbind(var, val, cx);
            // return 1 if the variable is bound
            1
        } else {
            self.vars.push(Cons::new(var, val, cx));
            0
        }
    }

    fn let_bind_value<'ob>(
        &mut self,
        cons: &Rt<Gc<&Cons>>,
        cx: &'ob mut Context,
    ) -> Result<GcObj<'ob>, EvalError> {
        rooted_iter!(iter, cons.bind(cx).cdr(), cx);
        let value = match iter.next()? {
            // (let ((x y)))
            Some(x) => self.eval_form(x, cx)?,
            // (let ((x)))
            None => nil(),
        };
        // (let ((x y z ..)))
        if !iter.is_empty() {
            bail_err!("Let binding can only have 1 value");
        }
        Ok(value)
    }

    fn implicit_progn<'ob>(
        &mut self,
        mut forms: ElemStreamIter<'_>,
        cx: &'ob mut Context,
    ) -> EvalResult<'ob> {
        root!(last, nil(), cx);
        while let Some(form) = forms.next()? {
            let value = self.eval_form(form, cx)?;
            last.set(value);
        }
        Ok(last.bind(cx))
    }

    fn unwind_protect<'ob>(&mut self, obj: &Rt<GcObj>, cx: &'ob mut Context) -> EvalResult<'ob> {
        rooted_iter!(forms, obj, cx);
        let Some(body) = forms.next()? else { bail_err!(ArgError::new(1, 0, "unwind-protect")) };
        match self.eval_form(body, cx) {
            Ok(x) => {
                root!(x, cx);
                self.implicit_progn(forms, cx)?;
                Ok(x.bind(cx))
            }
            Err(e) => {
                self.implicit_progn(forms, cx)?;
                Err(e)
            }
        }
    }

    fn save_excursion<'ob>(&mut self, form: &Rt<GcObj>, cx: &'ob mut Context) -> EvalResult<'ob> {
        let point = self.env.current_buffer.as_ref().map(|b| b.text.cursor());
        let buffer = self.env.current_buffer.as_ref().map(|b| (b.lisp_buffer(cx)));
        root!(buffer, cx);
        let result = rebind!(self.eval_progn(form, cx)?);
        if let Some(buffer) = buffer.bind(cx) {
            self.env.set_buffer(buffer)?;
            let buf = self.env.current_buffer.as_mut().unwrap();
            buf.text.set_cursor(point.unwrap().chars());
        }
        Ok(result)
    }

    fn save_current_buffer<'ob>(
        &mut self,
        form: &Rt<GcObj>,
        cx: &'ob mut Context,
    ) -> EvalResult<'ob> {
        let buffer = self.env.current_buffer.as_ref().map(|x| x.lisp_buffer(cx));
        root!(buffer, cx);
        let result = rebind!(self.eval_progn(form, cx)?);
        if let Some(buffer) = buffer.bind(cx) {
            self.env.set_buffer(buffer)?;
        }
        Ok(result)
    }

    fn condition_case<'ob>(&mut self, form: &Rt<GcObj>, cx: &'ob mut Context) -> EvalResult<'ob> {
        rooted_iter!(forms, form, cx);
        let Some(var) = forms.next()? else { bail_err!(ArgError::new(2, 0, "condition-case")) };
        root!(var, cx);
        let Some(bodyform) = forms.next()? else {
            bail_err!(ArgError::new(2, 1, "condition-case"))
        };
        let err = match self.eval_form(bodyform, cx) {
            Ok(x) => return Ok(rebind!(x, cx)),
            Err(e) => e,
        };
        if matches!(err.error, ErrorType::Throw(_)) {
            return Err(err);
        }
        while let Some(handler) = forms.next()? {
            match handler.get(cx) {
                Object::Cons(cons) => {
                    // Check that conditions match
                    let condition = cons.car();
                    match condition.untag() {
                        Object::Symbol(sym::ERROR | sym::VOID_VARIABLE) => {}
                        Object::Cons(conditions) => {
                            for condition in conditions {
                                let condition = condition?;
                                // TODO: Handle different error symbols
                                if condition != sym::DEBUG && condition != sym::ERROR {
                                    bail_err!("non-error conditions {condition} not yet supported")
                                }
                            }
                        }
                        _ => bail_err!("Invalid condition handler: {condition}"),
                    }
                    // Call handlers with error
                    let error = if let ErrorType::Signal(id) = err.error {
                        let Some((sym, data)) = self.env.get_exception(id) else {
                            unreachable!("Exception not found")
                        };
                        cons!(sym, data; cx)
                    } else {
                        // TODO: Need to remove the anyhow branch once
                        // full errors are implemented
                        cons!(sym::ERROR, format!("{err}"); cx)
                    };
                    let binding = Cons::new(var, Cons::new1(error, cx), cx);
                    self.vars.push(binding);
                    let list: Gc<List> = match cons.cdr().try_into() {
                        Ok(x) => x,
                        Err(_) => return Ok(nil()),
                    };
                    rooted_iter!(handlers, list, cx);
                    let result = self.implicit_progn(handlers, cx)?;
                    self.vars.pop();
                    return Ok(result);
                }
                Object::NIL => {}
                invalid => bail_err!("Invalid condition handler: {invalid}"),
            }
        }
        Err(err)
    }
}

impl Rt<Gc<Function<'_>>> {
    pub(crate) fn call<'ob>(
        &self,
        args: &mut Rt<Vec<GcObj<'static>>>,
        name: Option<&str>,
        env: &mut Rt<Env>,
        cx: &'ob mut Context,
    ) -> EvalResult<'ob> {
        let name = name.unwrap_or("lambda");
        let arg_cnt = args.len();
        debug!("calling {self:?}");
        match self.get(cx) {
            Function::ByteFn(f) => {
                root!(f, cx);
                let len = env.stack.len();
                env.stack.push_frame(len);
                let res = crate::bytecode::call(f, args, name, env, cx)
                    .map_err(|e| e.add_trace(name, &args[..arg_cnt]));
                env.stack.pop_frame();
                res
            }
            Function::SubrFn(f) => {
                (*f).call(args, env, cx).map_err(|e| add_trace(e, name, &args[..arg_cnt]))
            }
            Function::Cons(_) => call_closure(self.try_into().unwrap(), args, name, env, cx)
                .map_err(|e| e.add_trace(name, args)),
            Function::Symbol(sym) => {
                let Some(func) = sym.follow_indirect(cx) else { bail_err!("Void Function: {sym}") };
                match func.untag() {
                    Function::Cons(cons) if cons.car() == sym::AUTOLOAD => {
                        // TODO: inifinite loop if autoload does not resolve
                        root!(sym, cx);
                        crate::eval::autoload_do_load(self.use_as(), None, None, env, cx)
                            .map_err(|e| add_trace(e, name, &args[..arg_cnt]))?;
                        let Some(func) = sym.bind(cx).follow_indirect(cx) else {
                            bail_err!("autoload for {sym} failed to define function")
                        };
                        root!(func, cx);
                        let name = sym.bind(cx).name().to_owned();
                        func.call(args, Some(&name), env, cx)
                    }
                    _ => {
                        root!(func, cx);
                        let name = sym.name().to_owned();
                        func.call(args, Some(&name), env, cx)
                    }
                }
            }
        }
    }
}

fn add_trace(err: anyhow::Error, name: &str, args: &[Rt<GcObj>]) -> EvalError {
    match err.downcast::<EvalError>() {
        Ok(err) => err.add_trace(name, args),
        Err(e) => EvalError::with_trace(e, name, args),
    }
}

fn call_closure<'ob>(
    closure: &Rt<Gc<&Cons>>,
    args: &Rt<Vec<GcObj>>,
    name: &str,
    env: &mut Rt<Env>,
    cx: &'ob mut Context,
) -> EvalResult<'ob> {
    cx.garbage_collect(false);
    let closure: &Cons = closure.get(cx);
    match closure.car().untag() {
        Object::Symbol(sym::CLOSURE) => {
            rooted_iter!(forms, closure.cdr(), cx);
            // TODO: remove this temp vector
            let args = args.iter().map(|x| x.bind(cx)).collect();
            let vars = bind_variables(&mut forms, args, name, cx)?;
            root!(vars, cx);
            Interpreter { vars, env }.implicit_progn(forms, cx)
        }
        other => Err(TypeError::new(Type::Func, other).into()),
    }
}

fn bind_variables<'a>(
    forms: &mut ElemStreamIter<'_>,
    args: Vec<GcObj<'a>>,
    name: &str,
    cx: &'a Context,
) -> AnyResult<Vec<&'a Cons>> {
    // Add closure environment to variables
    // (closure ((x . 1) (y . 2) t) ...)
    //          ^^^^^^^^^^^^^^^^^^^
    let Some(env) = forms.next()? else { bail!("Closure missing environment") };
    let mut vars = parse_closure_env(env.bind(cx))?;

    // Add function arguments to variables
    // (closure (t) (x y &rest z) ...)
    //              ^^^^^^^^^^^^^
    let Some(arg_list) = forms.next()? else { bail!("Closure missing argument list") };
    bind_args(arg_list.bind(cx), args, &mut vars, name, cx)?;
    Ok(vars)
}

fn parse_closure_env(obj: GcObj) -> AnyResult<Vec<&Cons>> {
    let forms = obj.as_list()?;
    let mut env = Vec::new();
    for form in forms {
        match form?.untag() {
            Object::Cons(pair) => {
                env.push(pair);
            }
            Object::TRUE => return Ok(env),
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
        vars.push(Cons::new(name, val, cx));
    }

    for name in optional {
        let val = arg_values.next().unwrap_or_default();
        vars.push(Cons::new(name, val, cx));
    }

    if let Some(rest_name) = rest {
        let values = arg_values.as_slice();
        let list = crate::fns::slice_into_list(values, None, cx);
        vars.push(Cons::new(rest_name, list, cx));
    } else {
        // Ensure too many args were not provided
        ensure!(
            arg_values.next().is_none(),
            ArgError::new(num_required_args + num_optional_args, num_actual_args, name)
        );
    }
    Ok(())
}

pub(crate) fn parse_arg_list(
    bindings: GcObj,
) -> AnyResult<(Vec<Symbol>, Vec<Symbol>, Option<Symbol>)> {
    let mut required = Vec::new();
    let mut optional = Vec::new();
    let mut rest = None;
    let mut arg_type = &mut required;
    let mut iter = bindings.as_list()?;
    while let Some(binding) = iter.next() {
        let sym: Symbol = binding?.try_into()?;
        match sym {
            sym::AND_OPTIONAL => arg_type = &mut optional,
            sym::AND_REST => {
                if let Some(last) = iter.next() {
                    rest = Some(last?.try_into()?);
                    ensure!(iter.next().is_none(), "Found multiple arguments after &rest");
                }
            }
            _ => {
                arg_type.push(sym);
            }
        }
    }
    Ok((required, optional, rest))
}

#[cfg(test)]
mod test {
    use crate::core::{env::intern, gc::RootSet, object::IntoObject};
    use rune_core::macros::list;

    use super::*;

    fn check_interpreter<T>(test_str: &str, expect: T, cx: &mut Context)
    where
        T: IntoObject,
    {
        root!(env, Env::default(), cx);
        println!("Test String: {test_str}");
        let obj = crate::reader::read(test_str, cx).unwrap().0;
        root!(obj, cx);
        let compare = rebind!(eval(obj, None, env, cx).unwrap());
        let expect: GcObj = expect.into_obj(cx).copy_as_obj(cx);
        assert_eq!(compare, expect);
    }

    fn check_error(test_str: &str, cx: &mut Context) {
        root!(env, Env::default(), cx);
        println!("Test String: {test_str}");
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
        check_interpreter("(progn (defvar dyn_test1 1) dyn_test1)", 1, cx);
        check_interpreter("(progn (defvar dyn_test2 1) (let ((dyn_test2 3)) dyn_test2))", 3, cx);
        check_interpreter("(progn (defvar dyn_test3 1) (let ((dyn_test3 3))) dyn_test3)", 1, cx);
        check_interpreter("(let ((dyn_test4 7)) (defvar dyn_test4 3) dyn_test4)", 7, cx);
        check_interpreter(
            "(progn (defvar dyn_test5 1) (let (bar) (let ((dyn_test5 3)) (setq bar dyn_test5)) bar))",
            3,
            cx,
        );
        // Special but unbound
        check_interpreter(
            "(progn (defvar dyn_test6 1) (makunbound 'dyn_test6) (let ((fn #'(lambda () (defvar dyn_test6 3))) (dyn_test6 7)) (funcall fn)) dyn_test6)",
            3,
            cx,
        );
        check_interpreter(
            "(progn (defvar dyn_test7 1) (makunbound 'dyn_test7) (let ((dyn_test7 7)) dyn_test7))",
            7,
            cx,
        );
        check_interpreter("(eq (make-symbol \"bar\") 'bar)", false, cx);
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
    fn test_loops() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        check_interpreter(
            "(let ((i 3) (x 0)) (while (> i 0) (setq x (+ x i) i (1- i) )) x)",
            6,
            cx,
        );
        check_interpreter("(let ((i 3) (x 0)) (while (progn (setq x (1- x)) (> i 0)) (setq x (+ x i) i (1- i) )) x)", 2, cx);
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
        let x = intern("x", cx);
        let list = list![sym::CLOSURE, list![true; cx], list![x; cx], x; cx];
        root!(list, cx);
        check_interpreter("(function (lambda (x) x))", list, cx);
        // TODO: fix this duplicate intern
        let x = intern("x", cx);
        let y = intern("y", cx);
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
        check_interpreter("(let* ((y 7)(x #'(lambda (x) (+ x y)))) (funcall x 3))", 10, cx);
        // Test that closures capture their environments
        check_interpreter(
            "(progn (setq func (let ((x 3)) #'(lambda (y) (+ y x)))) (funcall func 5))",
            8,
            cx,
        );
        // Test multiple closures
        check_interpreter(
            "(progn (setq funcs (let ((x 3)) (cons #'(lambda (y) (+ y x)) #'(lambda (y) (- y x))))) (* (funcall (car funcs) 5) (funcall (cdr funcs) 1)))",
            -16,
            cx,
        );
        // Test that closures close over variables
        check_interpreter(
            "(progn (setq funcs (let ((x 3)) (cons #'(lambda (y) (setq x y)) #'(lambda (y) (+ y x))))) (funcall (car funcs) 5) (funcall (cdr funcs) 4))",
            9,
            cx,
        );
        // Test that closures in global function close over values and not
        // variables
        check_interpreter(
            "(progn (setq func (let ((x 3)) (defalias 'int-test-no-cap #'(lambda (y) (+ y x))) #'(lambda (y) (setq x y)))) (funcall func 4) (int-test-no-cap 5))",
            8,
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
        check_interpreter("(catch 1 (condition-case nil (throw 1 2) (error 3)))", 2, cx);
        check_interpreter("(catch 1 (catch 2 (throw 1 3)))", 3, cx);
        check_error("(throw 1 2)", cx);
        check_error("(catch 2 (throw 3 4))", cx);
    }
}
