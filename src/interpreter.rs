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
                QUOTE => self.quote(forms),
                LET => self.eval_let(forms, true),
                IF => self.eval_if(forms),
                SETQ => self.setq(forms),
                _ => todo!("eval symbol function call"),
            },
            _ => todo!("eval function call"),
        }
    }

    fn eval_if(&mut self, obj: Object<'ob>) -> Result<Object<'ob>> {
        let mut forms = obj.as_list()?;
        match forms.len() {
            // (if) | (if x)
            len @ (0 | 1) => Err(Error::ArgCount(2, len as u16).into()),
            // (if x y z ...)
            _ => {
                let condition = self.eval_form(forms.next().unwrap()?)?;
                let true_cond = forms.next().unwrap()?;
                if condition != Object::NIL {
                    self.eval_form(true_cond)
                } else {
                    self.implicit_progn(forms)
                }
            }

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
                    // last_value will be None if froms is empty. In that case throw an error
                    break last_value.ok_or(Error::ArgCount(2, 0).into());
                }
            };
            arg_cnt += 2;
        }
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
            },
            None => todo!("set global and closure variables"),
        }
    }

    fn quote(&self, value: Object<'ob>) -> Result<Object<'ob>> {
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
                    let mut vars = self.let_bind_parallel(x?)?;
                    self.vars.append(&mut vars);
                } else {
                    todo!("implement let*");
                }
            }
            // (let)
            None => bail!(Error::ArgCount(1, 0)),
        }
        let obj = self.implicit_progn(iter)?;
        self.vars.truncate(prev_len);
        Ok(obj)
    }

    fn let_bind_parallel(&mut self, form: Object<'ob>) -> Result<Vec<Variable<'ob>>> {
        let bindings = form.as_list()?;
        let mut let_bindings = Vec::new();
        for binding in bindings {
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
        Ok(let_bindings)
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
    }

    #[test]
    fn conditionals() {
        check_interpreter!("(if nil 1)", false);
        check_interpreter!("(if t 1)", 1);
        check_interpreter!("(if nil 1 2)", 2);
        check_interpreter!("(if t 1 2)", 1);
        check_interpreter!("(if nil 1 2 3)", 3);
    }
}
