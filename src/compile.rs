use crate::arena::Arena;
use crate::cons::{Cons, ConsIter};
use crate::error::{Error, Type};
use crate::object::{Expression, IntoObject, LispFn, Object, Value, NIL};
use crate::opcode::{CodeVec, OpCode};
use crate::symbol::Symbol;
use anyhow::{bail, ensure, Result};
use paste::paste;
use std::convert::TryInto;
use std::fmt::Display;

#[derive(Debug, PartialEq)]
enum CompError {
    ConstOverflow,
    LetValueCount(usize),
    StackSizeOverflow,
}

impl Display for CompError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompError::ConstOverflow => write!(f, "Too many constants declared in fuction"),
            CompError::LetValueCount(_) => write!(f, "Let forms can only have 1 value"),
            CompError::StackSizeOverflow => write!(f, "Stack size overflow"),
        }
    }
}

impl std::error::Error for CompError {}

#[derive(Debug)]
struct ConstVec<'ob> {
    consts: Vec<Object<'ob>>,
}

impl<'ob> From<Vec<Object<'ob>>> for ConstVec<'ob> {
    fn from(vec: Vec<Object<'ob>>) -> Self {
        let mut consts = ConstVec { consts: Vec::new() };
        for x in vec {
            consts.insert_or_get(x);
        }
        consts
    }
}

impl<'ob> PartialEq for ConstVec<'ob> {
    fn eq(&self, other: &Self) -> bool {
        self.consts == other.consts
    }
}

impl<'ob> ConstVec<'ob> {
    pub(crate) const fn new() -> Self {
        ConstVec { consts: Vec::new() }
    }

    fn insert_or_get(&mut self, obj: Object<'ob>) -> usize {
        match self.consts.iter().position(|&x| obj == x) {
            None => {
                self.consts.push(obj);
                self.consts.len() - 1
            }
            Some(x) => x,
        }
    }

    fn insert(&mut self, obj: Object<'ob>) -> Result<u16, CompError> {
        let idx = self.insert_or_get(obj);
        match idx.try_into() {
            Ok(x) => Ok(x),
            Err(_) => Err(CompError::ConstOverflow),
        }
    }

    fn insert_lambda(&mut self, func: LispFn<'ob>, arena: &'ob Arena) -> Result<u16, CompError> {
        let obj: Object = func.into_obj(arena);
        self.consts.push(obj);
        match (self.consts.len() - 1).try_into() {
            Ok(x) => Ok(x),
            Err(_) => Err(CompError::ConstOverflow),
        }
    }
}

macro_rules! emit_op {
    ($self:ident, $op:ident, $idx:ident) => {
        match $idx {
            0 => $self.push_op(paste! {OpCode::[<$op 0>]}),
            1 => $self.push_op(paste! {OpCode::[<$op 1>]}),
            2 => $self.push_op(paste! {OpCode::[<$op 2>]}),
            3 => $self.push_op(paste! {OpCode::[<$op 3>]}),
            4 => $self.push_op(paste! {OpCode::[<$op 4>]}),
            5 => $self.push_op(paste! {OpCode::[<$op 5>]}),
            _ => match $idx.try_into() {
                Ok(n) => $self.push_op_n(paste! {OpCode::[<$op N>]}, n),
                Err(_) => $self.push_op_n2(paste! {OpCode::[<$op N2>]}, $idx),
            },
        }
    };
}

const JUMP_SLOTS: i16 = 2;

impl CodeVec {
    pub(crate) fn push_op(&mut self, op: OpCode) {
        self.push(op.into());
    }

    fn push_op_n(&mut self, op: OpCode, arg: u8) {
        self.push_op(op);
        self.push(arg);
    }

    fn push_op_n2(&mut self, op: OpCode, arg: u16) {
        self.push_op(op);
        self.push((arg >> 8) as u8);
        self.push(arg as u8);
    }

    fn push_jump_placeholder(&mut self) -> usize {
        let idx = self.len();
        self.push(0);
        self.push(0);
        idx
    }

    fn set_jump_placeholder(&mut self, index: usize) {
        let offset = self.len() as i16 - index as i16 - JUMP_SLOTS;
        self[index] = (offset >> 8) as u8;
        self[index + 1] = offset as u8;
    }

    fn push_back_jump(&mut self, index: usize) {
        let offset = index as i16 - self.len() as i16 - JUMP_SLOTS;
        self.push((offset >> 8) as u8);
        self.push(offset as u8);
    }

    fn emit_const(&mut self, idx: u16) {
        emit_op!(self, Constant, idx);
    }

    fn emit_varref(&mut self, idx: u16) {
        emit_op!(self, VarRef, idx);
    }

    fn emit_varset(&mut self, idx: u16) {
        emit_op!(self, VarSet, idx);
    }

    fn emit_call(&mut self, idx: u16) {
        emit_op!(self, Call, idx);
    }

    fn emit_stack_ref(&mut self, idx: u16) {
        emit_op!(self, StackRef, idx);
    }

    fn emit_stack_set(&mut self, idx: u16) {
        emit_op!(self, StackSet, idx);
    }
}

fn into_iter(obj: Object) -> Result<ConsIter> {
    match obj.val() {
        Value::Cons(cons) => Ok(cons.into_iter()),
        Value::Nil => Ok(ConsIter::empty()),
        x => Err(Error::Type(Type::List, x.get_type()).into()),
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct Exp<'ob> {
    codes: CodeVec,
    constants: ConstVec<'ob>,
    vars: Vec<Option<Symbol>>,
}

impl<'ob> From<Exp<'ob>> for Expression<'ob> {
    fn from(exp: Exp<'ob>) -> Self {
        Self {
            constants: exp.constants.consts,
            op_codes: exp.codes,
        }
    }
}

impl<'ob> Exp<'ob> {
    fn const_ref(&mut self, obj: Object<'ob>, var_ref: Option<Symbol>) -> Result<()> {
        self.vars.push(var_ref);
        let idx = self.constants.insert(obj)?;
        self.codes.emit_const(idx);
        Ok(())
    }

    fn add_const_lambda(&mut self, func: LispFn<'ob>, arena: &'ob Arena) -> Result<()> {
        self.vars.push(None);
        let idx = self.constants.insert_lambda(func, arena)?;
        self.codes.emit_const(idx);
        Ok(())
    }

    fn stack_ref(&mut self, idx: usize, var_ref: Symbol) -> Result<()> {
        match (self.vars.len() - idx - 1).try_into() {
            Ok(x) => {
                self.vars.push(Some(var_ref));
                self.codes.emit_stack_ref(x);
                Ok(())
            }
            Err(_) => Err(CompError::StackSizeOverflow.into()),
        }
    }

    fn stack_set(&mut self, idx: usize) -> Result<(), CompError> {
        match (self.vars.len() - idx - 1).try_into() {
            Ok(x) => {
                self.vars.pop();
                self.codes.emit_stack_set(x);
                Ok(())
            }
            Err(_) => Err(CompError::StackSizeOverflow),
        }
    }

    fn var_set(&mut self, idx: u16) {
        self.codes.emit_varset(idx);
        self.vars.pop();
    }

    fn discard(&mut self) {
        self.codes.push_op(OpCode::Discard);
        self.vars.pop();
    }

    fn duplicate(&mut self) {
        self.codes.push_op(OpCode::Duplicate);
        self.vars.push(None);
    }

    fn quote(&mut self, value: Object<'ob>) -> Result<()> {
        let mut forms = into_iter(value)?;
        match forms.clone().count() {
            1 => self.const_ref(forms.next().unwrap()?, None),
            x => Err(Error::ArgCount(1, x as u16).into()),
        }
    }

    fn compile_let(&mut self, form: Object<'ob>, arena: &'ob Arena) -> Result<()> {
        let mut iter = into_iter(form)?;
        let num_binding_forms = match iter.next() {
            // (let x ...)
            Some(x) => self.let_bind(x?, arena)?,
            // (let)
            None => bail!(Error::ArgCount(1, 0)),
        };
        self.implicit_progn(iter, arena)?;
        // Remove let bindings from the stack
        if num_binding_forms > 0 {
            self.codes
                .push_op_n(OpCode::DiscardNKeepTOS, num_binding_forms as u8);
            let last = self.vars.pop().expect("empty stack in compile");
            self.vars.truncate(self.vars.len() - num_binding_forms);
            self.vars.push(last);
        }
        Ok(())
    }

    fn progn(&mut self, forms: Object<'ob>, arena: &'ob Arena) -> Result<()> {
        self.implicit_progn(into_iter(forms)?, arena)
    }

    fn implicit_progn(&mut self, mut forms: ConsIter<'_, 'ob>, arena: &'ob Arena) -> Result<()> {
        if let Some(form) = forms.next() {
            self.compile_form(form?, arena)?;
        } else {
            return self.const_ref(NIL, None);
        }
        for form in forms {
            self.discard();
            self.compile_form(form?, arena)?;
        }
        Ok(())
    }

    fn let_bind_call(&mut self, cons: &'ob Cons<'ob>, arena: &'ob Arena) -> Result<()> {
        let var: Symbol = cons.car().try_into()?;
        let mut iter = into_iter(cons.cdr())?;
        match iter.next() {
            // (let ((x y)))
            Some(value) => {
                self.compile_form(value?, arena)?;
                let last = self.vars.last_mut();
                let tos = last.expect("stack empty after compile form");
                *tos = Some(var);
            }
            // (let ((x)))
            None => self.const_ref(NIL, Some(var))?,
        };
        let rest = iter.count();
        // (let ((x y z ..)))
        ensure!(rest == 0, CompError::LetValueCount(rest + 1));
        Ok(())
    }

    fn let_bind_nil(&mut self, sym: Symbol) -> Result<()> {
        self.const_ref(NIL, Some(sym))
    }

    fn let_bind(&mut self, obj: Object, arena: &'ob Arena) -> Result<usize> {
        let bindings = into_iter(obj)?;
        let mut len = 0;
        for binding in bindings {
            match binding?.val() {
                // (let ((x y)))
                Value::Cons(cons) => self.let_bind_call(cons, arena)?,
                // (let (x))
                Value::Symbol(sym) => self.let_bind_nil(sym)?,
                x => bail!(Error::Type(Type::Cons, x.get_type())),
            }
            len += 1;
        }
        Ok(len)
    }

    fn setq(&mut self, obj: Object<'ob>, arena: &'ob Arena) -> Result<()> {
        let mut forms = into_iter(obj)?;
        let mut args_processed = 0;
        // (setq)
        ensure!(!forms.is_empty(), Error::ArgCount(2, 0));
        // Iterate over variable/value pairs
        while let Some(var) = forms.next() {
            args_processed += 1;
            // value
            match forms.next() {
                // (setq x y)
                Some(val) => self.compile_form(val?, arena)?,
                // (setq x)
                None => bail!(Error::ArgCount(args_processed + 1, args_processed)),
            }
            args_processed += 1;
            if forms.is_empty() {
                self.duplicate();
            }

            // variable
            let sym = var?.try_into()?;
            match self.vars.iter().rposition(|&x| x == Some(sym)) {
                Some(idx) => self.stack_set(idx)?,
                None => {
                    let idx = self.constants.insert(sym.into())?;
                    self.var_set(idx);
                }
            }
        }
        Ok(())
    }

    fn compile_funcall(&mut self, cons: &Cons<'ob>, arena: &'ob Arena) -> Result<()> {
        self.const_ref(cons.car(), None)?;
        let prev_len = self.vars.len();
        let args = into_iter(cons.cdr())?;
        let mut num_args = 0;
        for arg in args {
            self.compile_form(arg?, arena)?;
            num_args += 1;
        }
        self.codes.emit_call(num_args as u16);
        self.vars.truncate(prev_len);
        Ok(())
    }

    fn jump(&mut self, jump_code: OpCode) -> (usize, OpCode) {
        match jump_code {
            OpCode::JumpNil
            | OpCode::JumpNotNil
            | OpCode::JumpNilElsePop
            | OpCode::JumpNotNilElsePop => {
                self.vars.pop();
            }
            OpCode::Jump => {}
            x => panic!("invalid jump opcode provided: {:?}", x),
        }
        self.codes.push_op(jump_code);
        let place = self.codes.push_jump_placeholder();
        (place, jump_code)
    }

    fn set_jump_target(&mut self, target: (usize, OpCode)) {
        match target.1 {
            // add the non-popped conditional back to the stack, since we are
            // past the "else pop" part of the Code
            OpCode::JumpNilElsePop | OpCode::JumpNotNilElsePop => {
                self.vars.push(None);
            }
            OpCode::JumpNil | OpCode::JumpNotNil | OpCode::Jump => {}
            x => panic!("invalid jump opcode provided: {:?}", x),
        }
        self.codes.set_jump_placeholder(target.0);
    }

    fn jump_back(&mut self, jump_code: OpCode, location: usize) {
        if matches!(jump_code, OpCode::Jump) {
            self.codes.push_op(OpCode::Jump);
            self.codes.push_back_jump(location);
        } else {
            panic!("invalid back jump opcode provided: {:?}", jump_code);
        }
    }

    fn compile_if(&mut self, obj: Object<'ob>, arena: &'ob Arena) -> Result<()> {
        // let list = into_list(obj)?;
        let mut forms = into_iter(obj)?;
        let len = forms.clone().count();
        match len {
            // (if) | (if x)
            0 | 1 => Err(Error::ArgCount(2, len as u16).into()),
            // (if x y)
            2 => {
                self.compile_form(forms.next().unwrap()?, arena)?;
                let target = self.jump(OpCode::JumpNilElsePop);
                self.compile_form(forms.next().unwrap()?, arena)?;
                self.set_jump_target(target);
                Ok(())
            }
            // (if x y z ...)
            _ => {
                self.compile_form(forms.next().unwrap()?, arena)?;
                let else_nil_target = self.jump(OpCode::JumpNil);
                // if branch
                self.compile_form(forms.next().unwrap()?, arena)?;
                let jump_to_end_target = self.jump(OpCode::Jump);
                // else branch
                self.set_jump_target(else_nil_target);
                self.implicit_progn(forms, arena)?;
                self.set_jump_target(jump_to_end_target);
                Ok(())
            }
        }
    }

    fn compile_loop(&mut self, obj: Object<'ob>, arena: &'ob Arena) -> Result<()> {
        let mut forms = into_iter(obj)?;
        let top = self.codes.len();
        match forms.next() {
            Some(form) => self.compile_form(form?, arena)?,
            None => bail!(Error::ArgCount(1, 0)),
        }
        let loop_exit = self.jump(OpCode::JumpNilElsePop);
        self.implicit_progn(forms, arena)?;
        self.discard();
        self.jump_back(OpCode::Jump, top);
        self.set_jump_target(loop_exit);
        Ok(())
    }

    fn parse_fn_binding(bindings: Object) -> Result<(u16, u16, bool, Vec<Symbol>)> {
        let mut args: Vec<Symbol> = vec![];
        let mut required = 0;
        let mut optional = 0;
        let mut rest = false;
        let mut arg_type = &mut required;
        let mut iter = into_iter(bindings)?;
        while let Some(binding) = iter.next() {
            let sym = binding?.as_symbol()?;
            match sym.get_name() {
                "&optional" => arg_type = &mut optional,
                "&rest" => {
                    if let Some(last) = iter.next() {
                        rest = true;
                        args.push(last?.as_symbol()?);
                        ensure!(
                            iter.next().is_none(),
                            "Found multiple arguments after &rest"
                        );
                    }
                }
                _ => {
                    *arg_type += 1;
                    args.push(sym);
                }
            }
        }
        Ok((required, optional, rest, args))
    }

    pub(crate) fn compile_lambda(obj: Object<'ob>, arena: &'ob Arena) -> Result<LispFn<'ob>> {
        let mut iter = into_iter(obj)?;

        let (required, optional, rest, args) = match iter.next() {
            // (lambda ())
            None => return Ok(LispFn::default()),
            // (lambda (x ...) ...)
            Some(bindings) => Self::parse_fn_binding(bindings?)?,
        };
        if iter.is_empty() {
            Ok(LispFn::default())
        } else {
            let exp: Expression<'ob> =
                Self::compile_func_body(iter, args.into_iter().map(Some).collect(), arena)?.into();
            let func = LispFn::new(exp.op_codes, exp.constants, required, optional, rest);
            Ok(func)
        }
    }

    fn compile_lambda_def(&mut self, obj: Object<'ob>, arena: &'ob Arena) -> Result<()> {
        let lambda = Self::compile_lambda(obj, arena)?;
        self.add_const_lambda(lambda, arena)
    }

    fn compile_defvar(&mut self, obj: Object<'ob>, arena: &'ob Arena) -> Result<()> {
        let mut iter = into_iter(obj)?;

        match iter.next() {
            // (defvar x ...)
            Some(x) => {
                let sym = x?.as_symbol()?;
                // TODO: compile this into a lambda like Emacs does
                match iter.next() {
                    // (defvar x y)
                    Some(value) => self.compile_form(value?, arena)?,
                    // (defvar x)
                    None => self.const_ref(NIL, None)?,
                };
                self.duplicate();
                let idx = self.constants.insert(sym.into())?;
                self.var_set(idx);
                Ok(())
            }
            // (defvar)
            None => Err(Error::ArgCount(1, 0).into()),
        }
    }

    fn compile_cond_clause(
        &mut self,
        clause: Object<'ob>,
        jump_targets: &mut Vec<(usize, OpCode)>,
        arena: &'ob Arena,
    ) -> Result<()> {
        let mut cond = into_iter(clause)?;
        let len = cond.clone().count();
        match len {
            // (cond ())
            0 => {}
            // (cond (x))
            1 => {
                self.compile_form(cond.next().unwrap()?, arena)?;
                let target = self.jump(OpCode::JumpNotNilElsePop);
                jump_targets.push(target);
            }
            // (cond (x y ...))
            _ => {
                self.compile_form(cond.next().unwrap()?, arena)?;
                let skip_target = self.jump(OpCode::JumpNil);
                self.implicit_progn(cond, arena)?;
                self.vars.pop();
                let taken_target = self.jump(OpCode::Jump);
                self.set_jump_target(skip_target);
                jump_targets.push(taken_target);
            }
        }
        Ok(())
    }

    fn compile_last_cond_clause(
        &mut self,
        clause: Object<'ob>,
        jump_targets: &mut Vec<(usize, OpCode)>,
        arena: &'ob Arena,
    ) -> Result<()> {
        let mut cond = into_iter(clause)?;
        let len = cond.clone().count();
        match len {
            // (cond ())
            0 => {
                self.const_ref(NIL, None)?;
            }
            // (cond (x))
            1 => {
                self.compile_form(cond.next().unwrap()?, arena)?;
                let target = self.jump(OpCode::JumpNotNilElsePop);
                self.const_ref(NIL, None)?;
                jump_targets.push(target);
            }
            // (cond (x y ...))
            _ => {
                self.compile_form(cond.next().unwrap()?, arena)?;
                let target = self.jump(OpCode::JumpNilElsePop);
                self.implicit_progn(cond, arena)?;
                jump_targets.push(target);
            }
        }
        Ok(())
    }

    fn compile_cond(&mut self, obj: Object<'ob>, arena: &'ob Arena) -> Result<()> {
        let mut clauses = into_iter(obj)?;
        // (cond)
        if clauses.is_empty() {
            return self.const_ref(NIL, None);
        }

        let final_return_targets = &mut Vec::new();
        while let Some(clause) = clauses.next() {
            if clauses.is_empty() {
                self.compile_last_cond_clause(clause?, final_return_targets, arena)?;
            } else {
                self.compile_cond_clause(clause?, final_return_targets, arena)?;
            }
        }

        for target in final_return_targets {
            self.codes.set_jump_placeholder(target.0);
        }
        Ok(())
    }

    fn dispatch_special_form(&mut self, cons: &Cons<'ob>, arena: &'ob Arena) -> Result<()> {
        println!("car = {}", cons.car());
        let sym: Symbol = cons.car().try_into()?;
        match sym.get_name() {
            "lambda" => self.compile_lambda_def(cons.cdr(), arena),
            "while" => self.compile_loop(cons.cdr(), arena),
            "quote" => self.quote(cons.cdr()),
            "progn" => self.progn(cons.cdr(), arena),
            "setq" => self.setq(cons.cdr(), arena),
            "defvar" => self.compile_defvar(cons.cdr(), arena),
            "cond" => self.compile_cond(cons.cdr(), arena),
            "let" => self.compile_let(cons.cdr(), arena),
            "if" => self.compile_if(cons.cdr(), arena),
            _ => self.compile_funcall(cons, arena),
        }
    }

    fn variable_reference(&mut self, sym: Symbol) -> Result<()> {
        match self.vars.iter().rposition(|&x| x == Some(sym)) {
            Some(idx) => self.stack_ref(idx, sym),
            None => {
                let idx = self.constants.insert(sym.into())?;
                self.codes.emit_varref(idx);
                self.vars.push(None);
                Ok(())
            }
        }
    }

    fn compile_form(&mut self, obj: Object<'ob>, arena: &'ob Arena) -> Result<()> {
        match obj.val() {
            Value::Cons(cons) => self.dispatch_special_form(cons, arena),
            Value::Symbol(sym) => self.variable_reference(sym),
            _ => self.const_ref(obj, None),
        }
    }

    fn compile_func_body(
        obj: ConsIter<'_, 'ob>,
        vars: Vec<Option<Symbol>>,
        arena: &'ob Arena,
    ) -> Result<Self> {
        let mut exp = Self {
            codes: CodeVec::default(),
            constants: ConstVec::new(),
            vars,
        };
        exp.implicit_progn(obj, arena)?;
        exp.codes.push_op(OpCode::Ret);
        exp.vars.truncate(0);
        Ok(exp)
    }
}

pub(crate) fn compile<'ob>(obj: Object<'ob>, arena: &'ob Arena) -> Result<Expression<'ob>> {
    let cons = Cons::new(obj, NIL);
    Exp::compile_func_body(cons.into_iter(), vec![], arena).map(|x| x.into())
}

#[cfg(test)]
mod test {

    use super::*;
    use crate::arena::Arena;
    use crate::object::TRUE;
    use crate::reader::Reader;
    use crate::symbol::intern;
    #[allow(clippy::enum_glob_use)]
    use OpCode::*;

    #[allow(clippy::needless_pass_by_value)]
    fn check_error<E>(compare: &str, expect: E)
    where
        E: std::error::Error + PartialEq + Send + Sync + 'static,
    {
        let arena = &Arena::new();
        let obj = Reader::read(compare, arena).unwrap().0;
        assert_eq!(
            compile(obj, arena).err().unwrap().downcast::<E>().unwrap(),
            expect
        );
    }

    macro_rules! check_compiler {
        ($compare:expr, [$($op:expr),+], [$($const:expr),+]) => ({
            let comp_arena = &Arena::new();
            println!("Test String: {}", $compare);
            let obj = Reader::read($compare, comp_arena).unwrap().0;
            let expect = Expression {
                op_codes: vec_into![$($op),+].into(),
                constants: vec_into_object![$($const),+; comp_arena],
            };
            assert_eq!(compile(obj, comp_arena).unwrap(), expect);
        })
    }

    #[test]
    fn test_basic() {
        let arena = &Arena::new();
        check_compiler!("1", [Constant0, Ret], [1]);
        check_compiler!("'foo", [Constant0, Ret], [intern("foo")]);
        check_compiler!("'(1 2)", [Constant0, Ret], [list!(1, 2; arena)]);
        check_compiler!("\"foo\"", [Constant0, Ret], ["foo"]);
    }

    #[test]
    fn test_compile_variable() {
        check_compiler!(
            "(let (foo))",
            [Constant0, Constant0, DiscardNKeepTOS, 1, Ret],
            [false]
        );
        check_compiler!("(let ())", [Constant0, Ret], [false]);
        check_compiler!(
            "(let ((foo 1)(bar 2)(baz 3)))",
            [
                Constant0,
                Constant1,
                Constant2,
                Constant3,
                DiscardNKeepTOS,
                3,
                Ret
            ],
            [1, 2, 3, false]
        );
        check_compiler!(
            "(let ((foo 1)) foo)",
            [Constant0, StackRef0, DiscardNKeepTOS, 1, Ret],
            [1]
        );
        check_compiler!("foo", [VarRef0, Ret], [intern("foo")]);
        check_compiler!("(progn)", [Constant0, Ret], [false]);
        check_compiler!(
            "(progn (set 'foo 5) foo)",
            [Constant0, Constant1, Constant2, Call2, Discard, VarRef1, Ret],
            [intern("set"), intern("foo"), 5]
        );
        check_compiler!(
            "(let ((foo 1)) (setq foo 2) foo)",
            [
                Constant0,
                Constant1,
                Duplicate,
                StackSet2,
                Discard,
                StackRef0,
                DiscardNKeepTOS,
                1,
                Ret
            ],
            [1, 2]
        );
        check_compiler!(
            "(progn (setq foo 2) foo)",
            [Constant0, Duplicate, VarSet1, Discard, VarRef1, Ret],
            [2, intern("foo")]
        );
        check_compiler!(
            "(let ((bar 4)) (+ foo bar))",
            [
                Constant0,
                Constant1,
                VarRef2,
                StackRef2,
                Call2,
                DiscardNKeepTOS,
                1,
                Ret
            ],
            [4, intern("+"), intern("foo")]
        );
        check_compiler!(
            "(defvar foo 1)",
            [Constant0, Duplicate, VarSet1, Ret],
            [1, intern("foo")]
        );
        check_compiler!(
            "(defvar foo)",
            [Constant0, Duplicate, VarSet1, Ret],
            [false, intern("foo")]
        );
        check_error("(let (foo 1))", Error::Type(Type::Cons, Type::Int));
    }

    const fn get_jump_slots(offset: i16) -> (u8, u8) {
        ((offset >> 8) as u8, offset as u8)
    }

    #[test]
    fn conditional() {
        let (high4, low4) = get_jump_slots(4);
        let (high1, low1) = get_jump_slots(1);
        check_compiler!(
            "(if nil 1 2)",
            [Constant0, JumpNil, high4, low4, Constant1, Jump, high1, low1, Constant2, Ret],
            [NIL, 1, 2]
        );
        check_compiler!(
            "(if t 2)",
            [Constant0, JumpNilElsePop, high1, low1, Constant1, Ret],
            [TRUE, 2]
        );
        check_error("(if 1)", Error::ArgCount(2, 1));
    }

    #[test]
    fn cond_stmt() {
        check_compiler!("(cond)", [Constant0, Ret], [NIL]);
        check_compiler!("(cond ())", [Constant0, Ret], [NIL]);
        check_compiler!(
            "(cond (1))",
            [Constant0, JumpNotNilElsePop, 0, 1, Constant1, Ret],
            [1, false]
        );
        check_compiler!(
            "(cond (1 2))",
            [Constant0, JumpNilElsePop, 0, 1, Constant1, Ret],
            [1, 2]
        );
        check_compiler!(
            "(cond (1 2)(3 4))",
            [
                Constant0,
                JumpNil,
                0,
                4,
                Constant1,
                Jump,
                0,
                5,
                Constant2,
                JumpNilElsePop,
                0,
                1,
                Constant3,
                Ret
            ],
            [1, 2, 3, 4]
        );
        check_compiler!(
            "(cond (1)(2))",
            [
                Constant0,
                JumpNotNilElsePop,
                0,
                5,
                Constant1,
                JumpNotNilElsePop,
                0,
                1,
                Constant2,
                Ret
            ],
            [1, 2, false]
        );
    }

    #[test]
    fn while_loop() {
        let (high5, low5) = get_jump_slots(5);
        let (high_9, low_9) = get_jump_slots(-9);
        check_compiler!(
            "(while t)",
            [
                Constant0,
                JumpNilElsePop,
                high5,
                low5,
                Constant1,
                Discard,
                Jump,
                high_9,
                low_9,
                Ret
            ],
            [TRUE, NIL]
        );

        check_compiler!(
            "(while t 1)",
            [
                Constant0,
                JumpNilElsePop,
                high5,
                low5,
                Constant1,
                Discard,
                Jump,
                high_9,
                low_9,
                Ret
            ],
            [TRUE, 1]
        );

        check_compiler!(
            "(while nil 2)",
            [
                Constant0,
                JumpNilElsePop,
                high5,
                low5,
                Constant1,
                Discard,
                Jump,
                high_9,
                low_9,
                Ret
            ],
            [NIL, 2]
        );

        let (high7, low7) = get_jump_slots(7);
        let (high_11, low_11) = get_jump_slots(-11);
        check_compiler!(
            "(while nil 2 3)",
            [
                Constant0,
                JumpNilElsePop,
                high7,
                low7,
                Constant1,
                Discard,
                Constant2,
                Discard,
                Jump,
                high_11,
                low_11,
                Ret
            ],
            [NIL, 2, 3]
        );
        check_error("(while)", Error::ArgCount(1, 0));
    }

    #[test]
    fn function() {
        check_compiler!("(foo)", [Constant0, Call0, Ret], [intern("foo")]);
        check_compiler!(
            "(foo 1 2)",
            [Constant0, Constant1, Constant2, Call2, Ret],
            [intern("foo"), 1, 2]
        );
        check_compiler!(
            "(foo (bar 1) 2)",
            [Constant0, Constant1, Constant2, Call1, Constant3, Call2, Ret],
            [intern("foo"), intern("bar"), 1, 2]
        );
        check_compiler!(
            "(foo (bar 1) (baz 1))",
            [Constant0, Constant1, Constant2, Call1, Constant3, Constant2, Call1, Call2, Ret],
            [intern("foo"), intern("bar"), 1, intern("baz")]
        );
        check_error("(foo . 1)", Error::Type(Type::List, Type::Int));
    }

    #[allow(clippy::needless_pass_by_value)]
    fn check_lambda(sexp: &str, func: LispFn) {
        let comp_arena = &Arena::new();
        println!("Test String: {}", sexp);
        let obj = Reader::read(sexp, comp_arena).unwrap().0;
        let lambda = match obj.val() {
            Value::Cons(cons) => cons.cdr(),
            x => panic!("expected cons, found {}", x),
        };
        assert_eq!(Exp::compile_lambda(lambda, comp_arena).unwrap(), func);
    }

    #[test]
    fn lambda() {
        check_lambda("(lambda)", LispFn::default());
        check_lambda("(lambda ())", LispFn::default());
        check_lambda("(lambda () nil)", LispFn::default());

        check_lambda(
            "(lambda () 1)",
            LispFn::new(vec_into![Constant0, Ret].into(), vec_into![1], 0, 0, false),
        );

        check_lambda(
            "(lambda (x) x)",
            LispFn::new(vec_into![StackRef0, Ret].into(), vec![], 1, 0, false),
        );

        check_lambda(
            "(lambda (x &optional) x)",
            LispFn::new(vec_into![StackRef0, Ret].into(), vec![], 1, 0, false),
        );
        check_lambda(
            "(lambda (x &optional y) x)",
            LispFn::new(vec_into![StackRef1, Ret].into(), vec![], 1, 1, false),
        );
        check_lambda(
            "(lambda (x &optional y z) y)",
            LispFn::new(vec_into![StackRef1, Ret].into(), vec![], 1, 2, false),
        );
        check_lambda(
            "(lambda (x &optional y &optional z) z)",
            LispFn::new(vec_into![StackRef0, Ret].into(), vec![], 1, 2, false),
        );
        check_lambda(
            "(lambda (x &rest) x)",
            LispFn::new(vec_into![StackRef0, Ret].into(), vec![], 1, 0, false),
        );
        check_lambda(
            "(lambda (x &rest y) y)",
            LispFn::new(vec_into![StackRef0, Ret].into(), vec![], 1, 0, true),
        );

        let arena = &Arena::new();
        let obj = Reader::read("(lambda (x &rest y z) y)", arena).unwrap().0;
        assert!(compile(obj, arena)
            .err()
            .unwrap()
            .downcast::<&str>()
            .is_ok());

        check_lambda(
            "(lambda (x y) (+ x y))",
            LispFn::new(
                vec_into![Constant0, StackRef2, StackRef2, Call2, Ret].into(),
                vec_into![intern("+")],
                2,
                0,
                false,
            ),
        );

        let func = LispFn::new(vec_into![StackRef0, Ret].into(), vec![], 1, 0, false);
        check_compiler!(
            "(let ((x 1)(y 2)) (lambda (x) x))",
            [Constant0, Constant1, Constant2, DiscardNKeepTOS, 2, Ret],
            [1, 2, func]
        );

        check_error("(lambda (x 1) x)", Error::Type(Type::Symbol, Type::Int));
    }

    #[test]
    fn errors() {
        check_error("(\"foo\")", Error::Type(Type::Symbol, Type::String));
        check_error("(quote)", Error::ArgCount(1, 0));
        check_error("(quote 1 2)", Error::ArgCount(1, 2));
    }

    #[test]
    fn let_errors() {
        check_error("(let (1))", Error::Type(Type::Cons, Type::Int));
        check_error("(let ((foo 1 2)))", CompError::LetValueCount(2));
        check_error("(let ((foo . 1)))", Error::Type(Type::List, Type::Int));
        check_error("(let ((foo 1 . 2)))", crate::cons::ConsIterError);
        check_error("(let (()))", Error::Type(Type::Cons, Type::Nil));
        check_error("(let)", Error::ArgCount(1, 0));
    }
}
