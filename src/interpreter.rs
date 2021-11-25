#![allow(dead_code)]
use crate::error::{Error, Type};
use crate::{
    arena::Arena,
    cons::{Cons, ElemIter},
    data::Environment,
    object::Object,
    symbol::Symbol,
};
use anyhow::{bail, ensure, Result};

type Variable<'ob> = (Symbol, Object<'ob>);

struct Interpreter<'ob, 'brw> {
    vars: Vec<Variable<'ob>>,
    env: &'brw mut Environment<'ob>,
    arena: &'ob Arena,
}

pub(crate) fn eval<'ob, 'brw>(
    form: Object<'ob>,
    lexical: Option<Object<'ob>>,
    env: &'brw mut Environment<'ob>,
    arena: &'ob Arena,
) -> Result<Object<'ob>> {
    ensure!(
        matches!(lexical, Some(Object::True(_) | Object::Nil(_)) | None),
        "lexical enviroments are not yet supported: found {:?}",
        lexical
    );
    let mut interpreter = Interpreter {
        vars: Vec::new(),
        env,
        arena,
    };
    interpreter.eval_form(form)
}

impl<'ob, 'brw> Interpreter<'ob, 'brw> {
    fn eval_form(&mut self, obj: Object<'ob>) -> Result<Object<'ob>> {
        match obj {
            Object::Symbol(sym) => self.var_ref(!sym),
            Object::Cons(cons) => self.eval_sexp(&cons),
            other => Ok(other),
        }
    }

    pub(crate) fn eval_sexp(&mut self, cons: &Cons<'ob>) -> Result<Object<'ob>> {
        let forms = cons.cdr();
        match cons.car() {
            Object::Symbol(sym) => symbol_match! {!sym;
                QUOTE => Self::quote(forms),
                LET => self.eval_let(forms, true),
                LET_STAR => self.eval_let(forms, false),
                IF => self.eval_if(forms),
                AND => self.eval_and(forms),
                OR => self.eval_or(forms),
                COND => self.eval_cond(forms),
                WHILE => self.eval_while(forms),
                PROGN => self.eval_progn(forms),
                PROG1 => self.eval_progx(forms, 1),
                PROG2 => self.eval_progx(forms, 2),
                SETQ => self.setq(forms),
                _ => todo!("eval symbol function call"),
            },
            _ => todo!("eval function call"),
        }
    }

    fn eval_progx(&mut self, obj: Object<'ob>, prog_num: u16) -> Result<Object<'ob>> {
        let mut count = 0;
        let mut returned_form = None;
        for form in obj.as_list()? {
            let value = self.eval_form(form?)?;
            count += 1;
            if prog_num == count {
                returned_form = Some(value);
            }
        }
        returned_form.ok_or_else(|| Error::ArgCount(prog_num, count).into())
    }

    fn eval_progn(&mut self, obj: Object<'ob>) -> Result<Object<'ob>> {
        self.implicit_progn(obj.as_list()?)
    }

    fn eval_while(&mut self, obj: Object<'ob>) -> Result<Object<'ob>> {
        let mut forms = obj.as_list()?;
        let condition = match forms.next() {
            Some(cond) => cond?,
            None => bail!(Error::ArgCount(1, 0)),
        };
        while self.eval_form(condition)? != Object::NIL {
            self.implicit_progn(forms.clone())?;
        }
        Ok(Object::NIL)
    }

    fn eval_cond(&mut self, obj: Object<'ob>) -> Result<Object<'ob>> {
        let mut last = Object::NIL;
        for form in obj.as_list()? {
            let mut clause = (form?).as_list()?;
            let first = clause.next().unwrap_or(Ok(Object::NIL))?;
            last = self.eval_form(first)?;
            if last != Object::NIL {
                if !clause.is_empty() {
                    last = self.implicit_progn(clause)?;
                }
                break;
            }
        }
        Ok(last)
    }

    fn eval_and(&mut self, obj: Object<'ob>) -> Result<Object<'ob>> {
        let mut last = Object::TRUE;
        for form in obj.as_list()? {
            last = self.eval_form(form?)?;
            if last == Object::NIL {
                break;
            }
        }
        Ok(last)
    }

    fn eval_or(&mut self, obj: Object<'ob>) -> Result<Object<'ob>> {
        let mut last = Object::NIL;
        for form in obj.as_list()? {
            last = self.eval_form(form?)?;
            if last != Object::NIL {
                break;
            }
        }
        Ok(last)
    }

    fn eval_if(&mut self, obj: Object<'ob>) -> Result<Object<'ob>> {
        let mut forms = obj.as_list()?;
        let condition = match forms.next() {
            Some(x) => x?,
            None => bail!(Error::ArgCount(2, 0)),
        };
        let true_branch = match forms.next() {
            Some(x) => x?,
            None => bail!(Error::ArgCount(2, 1)),
        };
        #[allow(clippy::if_not_else)]
        if condition != Object::NIL {
            self.eval_form(true_branch)
        } else {
            self.implicit_progn(forms)
        }
    }

    fn setq(&mut self, obj: Object<'ob>) -> Result<Object<'ob>> {
        let mut forms = obj.as_list()?;
        let mut arg_cnt = 0;
        let mut last_value = None;
        loop {
            match Self::pairs(&mut forms)? {
                Some((Object::Symbol(var), Some(val))) => {
                    last_value = Some(self.var_set(!var, val));
                }
                Some((other, Some(_))) => bail!(Error::from_object(Type::Symbol, other)),
                Some((_, None)) => bail!(Error::ArgCount(arg_cnt, arg_cnt + 1)),
                None => {
                    break;
                }
            };
            arg_cnt += 2;
        }
        // last_value will be None if forms is empty. In that case throw an error
        last_value.ok_or_else(|| Error::ArgCount(2, 0).into())
    }

    fn pairs(iter: &mut ElemIter<'_, 'ob>) -> Result<Option<(Object<'ob>, Option<Object<'ob>>)>> {
        match iter.next() {
            Some(first) => Ok(Some((first?, iter.next().transpose()?))),
            None => Ok(None),
        }
    }

    fn var_ref(&self, name: Symbol) -> Result<Object<'ob>> {
        let mut iter = self.vars.iter().rev();
        match iter.find_map(|(sym, val)| (sym == &name).then(|| val)) {
            Some(value) => Ok(*value),
            None => todo!("global and closure variables"),
        }
    }

    fn var_set(&mut self, name: Symbol, new_value: Object<'ob>) -> Object<'ob> {
        let mut iter = self.vars.iter_mut().rev();
        match iter.find_map(|(sym, val)| (sym == &name).then(|| val)) {
            Some(value) => {
                *value = new_value;
                new_value
            }
            None => todo!("set global and closure variables"),
        }
    }

    fn quote(value: Object<'ob>) -> Result<Object<'ob>> {
        let mut forms = value.as_list()?;
        match forms.len() {
            1 => Ok(forms.next().unwrap()?),
            x => Err(Error::ArgCount(1, x as u16).into()),
        }
    }

    fn eval_let(&mut self, form: Object<'ob>, parallel: bool) -> Result<Object<'ob>> {
        let mut iter = form.as_list()?;
        let prev_len = self.vars.len();
        match iter.next() {
            // (let x ...)
            Some(x) => {
                if parallel {
                    self.let_bind_parallel(x?)?;
                } else {
                    self.let_bind_serial(x?)?;
                }
            }
            // (let)
            None => bail!(Error::ArgCount(1, 0)),
        }
        let obj = self.implicit_progn(iter)?;
        self.vars.truncate(prev_len);
        Ok(obj)
    }

    fn let_bind_serial(&mut self, form: Object<'ob>) -> Result<()> {
        for binding in form.as_list()? {
            match binding? {
                // (let ((x y)))
                Object::Cons(cons) => {
                    let var = self.let_bind_value(!cons)?;
                    self.vars.push(var);
                }
                // (let (x))
                Object::Symbol(sym) => {
                    self.vars.push((!sym, Object::NIL));
                }
                // (let (1))
                x => bail!(Error::from_object(Type::Cons, x)),
            }
        }
        Ok(())
    }

    fn let_bind_parallel(&mut self, form: Object<'ob>) -> Result<()> {
        let mut let_bindings = Vec::new();
        for binding in form.as_list()? {
            match binding? {
                // (let ((x y)))
                Object::Cons(cons) => {
                    let var = self.let_bind_value(!cons)?;
                    let_bindings.push(var);
                }
                // (let (x))
                Object::Symbol(sym) => {
                    let_bindings.push((!sym, Object::NIL));
                }
                // (let (1))
                x => bail!(Error::from_object(Type::Cons, x)),
            }
        }
        self.vars.append(&mut let_bindings);
        Ok(())
    }

    fn let_bind_value(&mut self, cons: &'ob Cons<'ob>) -> Result<Variable<'ob>> {
        let mut iter = cons.cdr().as_list()?;
        let value = match iter.len() {
            // (let ((x)))
            0 => Object::NIL,
            // (let ((x y)))
            1 => self.eval_form(iter.next().unwrap()?)?,
            // (let ((x y z ..)))
            _ => bail!("Let binding forms can only have 1 value"),
        };
        let name = cons.car().try_into()?;
        Ok((name, value))
    }

    fn implicit_progn(&mut self, forms: ElemIter<'_, 'ob>) -> Result<Object<'ob>> {
        let mut last = Object::NIL;
        for form in forms {
            last = self.eval_form(form?)?;
        }
        Ok(last)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    macro_rules! check_interpreter {
        ($compare:expr, $expect:expr) => {{
            let comp_arena = &Arena::new();
            let comp_env = &mut Environment::default();
            println!("Test String: {}", $compare);
            let obj = crate::reader::read($compare, comp_arena).unwrap().0;
            let expect = comp_arena.add($expect);
            assert_eq!(eval(obj, None, comp_env, comp_arena).unwrap(), expect);
        }};
    }

    #[test]
    fn basic() {
        let arena = &Arena::new();
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
    fn progn() {
        check_interpreter!("(prog1 1 2 3)", 1);
        check_interpreter!("(prog2 1 2 3)", 2);
        check_interpreter!("(progn 1 2 3 4)", 4);
    }
}
