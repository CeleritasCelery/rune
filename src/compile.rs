#![allow(dead_code)]

use crate::arena::Arena;
use crate::error::{Error, Result, Type};
use crate::object::{Cons, GcObject, IntoObject, LispFn, Object, Symbol, Value};
use crate::opcode::{CodeVec, OpCode};
use paste::paste;
use std::convert::TryInto;

impl OpCode {
    pub unsafe fn from_unchecked(x: u8) -> Self {
        std::mem::transmute(x)
    }
}

impl From<OpCode> for u8 {
    fn from(x: OpCode) -> u8 {
        x as u8
    }
}

impl Default for LispFn {
    fn default() -> Self {
        LispFn::new(
            vec_into![OpCode::Constant0, OpCode::Ret].into(),
            vec![Object::nil()],
            0,
            0,
            false,
        )
    }
}

#[derive(Debug)]
struct ConstVec {
    consts: Vec<GcObject>,
    arena: Arena,
}

impl<'obj> From<Vec<Object<'obj>>> for ConstVec {
    fn from(vec: Vec<Object<'obj>>) -> Self {
        let mut consts = ConstVec {
            consts: Vec::new(),
            arena: Arena::new(),
        };
        for x in vec.into_iter() {
            consts.insert_or_get(x);
        }
        consts
    }
}

impl PartialEq for ConstVec {
    fn eq(&self, other: &Self) -> bool {
        self.consts == other.consts
    }
}

impl ConstVec {
    pub const fn new() -> Self {
        ConstVec {
            consts: Vec::new(),
            arena: Arena::new(),
        }
    }

    fn insert_or_get(&mut self, obj: Object) -> usize {
        match self.consts.iter().position(|&x| obj == x) {
            None => {
                let new_obj = unsafe { obj.clone_in(&self.arena).into_gc() };
                self.consts.push(new_obj);
                self.consts.len() - 1
            }
            Some(x) => x,
        }
    }

    fn insert(&mut self, obj: Object) -> Result<u16> {
        let idx = self.insert_or_get(obj);
        match idx.try_into() {
            Ok(x) => Ok(x),
            Err(_) => Err(Error::ConstOverflow),
        }
    }

    fn insert_lambda(&mut self, func: LispFn) -> Result<u16> {
        let obj: Object = func.into_obj(&self.arena);
        self.consts.push(unsafe { obj.into_gc() });
        match (self.consts.len() - 1).try_into() {
            Ok(x) => Ok(x),
            Err(_) => Err(Error::ConstOverflow),
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
    pub fn push_op(&mut self, op: OpCode) {
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
        emit_op!(self, Constant, idx)
    }

    fn emit_varref(&mut self, idx: u16) {
        emit_op!(self, VarRef, idx)
    }

    fn emit_varset(&mut self, idx: u16) {
        emit_op!(self, VarSet, idx)
    }

    fn emit_call(&mut self, idx: u16) {
        emit_op!(self, Call, idx)
    }

    fn emit_stack_ref(&mut self, idx: u16) {
        emit_op!(self, StackRef, idx)
    }

    fn emit_stack_set(&mut self, idx: u16) {
        emit_op!(self, StackSet, idx)
    }
}

fn push_cons<'obj>(obj: Object<'obj>, mut vec: Vec<Object<'obj>>) -> Result<Vec<Object<'obj>>> {
    match obj.val() {
        Value::Nil => Ok(vec),
        Value::Cons(cons) => {
            vec.push(cons.car());
            push_cons(cons.cdr(), vec)
        }
        x => Err(Error::Type(Type::List, x.get_type())),
    }
}

fn into_list(obj: Object) -> Result<Vec<Object>> {
    push_cons(obj, vec![])
}

#[derive(Debug, PartialEq)]
pub struct Exp {
    codes: CodeVec,
    constants: ConstVec,
    vars: Vec<Option<Symbol>>,
}

impl From<Exp> for LispFn {
    fn from(exp: Exp) -> Self {
        let inner = exp.constants.consts;
        std::mem::forget(exp.constants.arena);
        LispFn::new(exp.codes, inner, 0, 0, false)
    }
}

impl<'obj> Exp {
    fn add_const(&mut self, obj: Object<'obj>, var_ref: Option<Symbol>) -> Result<()> {
        self.vars.push(var_ref);
        let idx = self.constants.insert(obj)?;
        self.codes.emit_const(idx);
        Ok(())
    }

    fn add_const_lambda(&mut self, func: LispFn) -> Result<()> {
        self.vars.push(None);
        let idx = self.constants.insert_lambda(func)?;
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
            Err(_) => Err(Error::StackSizeOverflow),
        }
    }

    fn stack_set(&mut self, idx: usize) -> Result<()> {
        match (self.vars.len() - idx - 1).try_into() {
            Ok(x) => {
                self.vars.pop();
                self.codes.emit_stack_set(x);
                Ok(())
            }
            Err(_) => Err(Error::StackSizeOverflow),
        }
    }

    fn discard(&mut self) {
        self.codes.push_op(OpCode::Discard);
        self.vars.pop();
    }

    fn duplicate(&mut self) {
        self.codes.push_op(OpCode::Duplicate);
        self.vars.push(None);
    }

    fn quote(&mut self, value: Object<'obj>) -> Result<()> {
        let list = into_list(value)?;
        match list.len() {
            1 => self.add_const(list[0], None),
            x => Err(Error::ArgCount(1, x as u16)),
        }
    }

    fn let_form(&mut self, form: Object) -> Result<()> {
        let prev_len = self.vars.len();
        let list = into_list(form)?;
        let mut iter = list.iter();
        match iter.next() {
            Some(x) => self.let_bind(*x),
            None => Err(Error::ArgCount(1, 0)),
        }?;
        self.implicit_progn(iter.as_slice())?;
        self.vars.truncate(prev_len);
        Ok(())
    }

    fn progn(&mut self, forms: Object) -> Result<()> {
        self.implicit_progn(into_list(forms)?.as_ref())
    }

    fn implicit_progn(&mut self, forms: &[Object]) -> Result<()> {
        if forms.is_empty() {
            self.add_const(Object::nil(), None)
        } else {
            // Use take and skip to ensure that the last form does not discard
            for form in forms.iter().take(1) {
                self.compile_form(*form)?;
            }
            for form in forms.iter().skip(1) {
                self.discard();
                self.compile_form(*form)?;
            }
            Ok(())
        }
    }

    fn let_bind_call(&mut self, cons: &Cons) -> Result<()> {
        let var = cons.car().try_into()?;
        let list = into_list(cons.cdr())?;
        let mut iter = list.iter();
        match iter.next() {
            Some(v) => self.add_const(*v, Some(var))?,
            None => self.add_const(Object::nil(), Some(var))?,
        };
        match iter.next() {
            None => Ok(()),
            Some(_) => Err(Error::LetValueCount(list.len() as u16)),
        }
    }

    fn let_bind_nil(&mut self, sym: Symbol) -> Result<()> {
        self.add_const(Object::nil(), Some(sym))
    }

    fn let_bind(&mut self, obj: Object) -> Result<()> {
        for binding in into_list(obj)? {
            match binding.val() {
                Value::Cons(cons) => self.let_bind_call(cons)?,
                Value::Symbol(sym) => self.let_bind_nil(sym)?,
                x => return Err(Error::Type(Type::Cons, x.get_type())),
            }
        }
        Ok(())
    }

    fn setq(&mut self, obj: Object) -> Result<()> {
        let list = into_list(obj)?;
        let last = (list.len() / 2) - 1;
        let pairs = list.chunks_exact(2);
        let is_even = pairs.remainder().is_empty();
        for (idx, pair) in pairs.enumerate() {
            let sym: Symbol = pair[0].try_into()?;
            let val = pair[1];

            self.compile_form(val)?;

            // Duplicate the last value to be the return value of the setq
            // expression
            if idx == last {
                self.duplicate();
            }

            match self.vars.iter().rposition(|&x| x == Some(sym)) {
                Some(idx) => self.stack_set(idx)?,
                None => {
                    let idx = self.constants.insert(sym.into())?;
                    self.codes.emit_varset(idx);
                }
            };
        }
        if is_even {
            Ok(())
        } else {
            let len = list.len() as u16;
            Err(Error::ArgCount(len - 1, len))
        }
    }

    fn compile_funcall(&mut self, cons: &Cons) -> Result<()> {
        self.add_const(cons.car(), None)?;
        let prev_len = self.vars.len();
        let list = into_list(cons.cdr())?;
        for form in &list {
            self.compile_form(*form)?;
        }
        self.codes.emit_call(list.len() as u16);
        self.vars.truncate(prev_len);
        Ok(())
    }

    fn compile_operator(&mut self, obj: Object, op: OpCode) -> Result<()> {
        let list = into_list(obj)?;
        match list.len() {
            2 => {
                self.compile_form(list[0])?;
                self.compile_form(list[1])?;
                self.codes.push_op(op);
                Ok(())
            }
            len => Err(Error::ArgCount(2, len as u16)),
        }
    }

    fn jump_nil_else_pop(&mut self) -> usize {
        self.codes.push_op(OpCode::JumpNilElsePop);
        let place = self.codes.push_jump_placeholder();
        // pop conditional
        self.vars.pop();
        place
    }

    fn set_jump_nil_else_pop_placeholder(&mut self, place: usize) {
        self.codes.set_jump_placeholder(place);
        // add the non-popped conditional back the stack
        self.vars.push(None);
    }

    fn compile_conditional(&mut self, obj: Object) -> Result<()> {
        let list = into_list(obj)?;
        match list.len() {
            len @ 0 | len @ 1 => Err(Error::ArgCount(2, len as u16)),
            2 => {
                self.compile_form(list[0])?;
                let place = self.jump_nil_else_pop();
                self.compile_form(list[1])?;
                self.set_jump_nil_else_pop_placeholder(place);
                Ok(())
            }
            _ => {
                let mut forms = list.iter();
                self.compile_form(*forms.next().unwrap())?;
                self.codes.push_op(OpCode::JumpNil);
                let place = self.codes.push_jump_placeholder();
                self.compile_form(*forms.next().unwrap())?;
                self.codes.push_op(OpCode::Jump);
                let place2 = self.codes.push_jump_placeholder();
                self.codes.set_jump_placeholder(place);
                self.implicit_progn(forms.as_slice())?;
                self.codes.set_jump_placeholder(place2);
                Ok(())
            }
        }
    }

    fn compile_loop(&mut self, obj: Object) -> Result<()> {
        let forms = into_list(obj)?;
        if forms.is_empty() {
            return Err(Error::ArgCount(1, 0));
        }
        let top = self.codes.len();
        self.compile_form(forms[0])?;
        let place = self.jump_nil_else_pop();
        self.implicit_progn(&forms[1..])?;
        self.discard();
        self.codes.push_op(OpCode::Jump);
        self.codes.push_back_jump(top);
        self.set_jump_nil_else_pop_placeholder(place);
        Ok(())
    }

    fn compile_lambda(&mut self, obj: Object) -> Result<()> {
        let list = into_list(obj)?;
        let mut iter = list.iter();
        let mut vars: Vec<Option<Symbol>> = vec![];

        match iter.next() {
            None => {
                return self.add_const_lambda(LispFn::default());
            }
            Some(bindings) => {
                for binding in &into_list(*bindings)? {
                    match binding.val() {
                        Value::Symbol(x) => vars.push(Some(x)),
                        x => return Err(Error::Type(Type::Symbol, x.get_type())),
                    }
                }
            }
        };
        let body = iter.as_slice();
        if body.is_empty() {
            self.add_const_lambda(LispFn::default())
        } else {
            let len = vars.len();
            let mut func: LispFn = Self::compile_func_body(body, vars)?.into();
            func.args.required = len as u16;
            self.add_const_lambda(func)
        }
    }

    fn dispatch_special_form(&mut self, cons: &Cons) -> Result<()> {
        let sym: Symbol = cons.car().try_into()?;
        match sym.get_name() {
            "lambda" => self.compile_lambda(cons.cdr()),
            "while" => self.compile_loop(cons.cdr()),
            "quote" => self.quote(cons.cdr()),
            "progn" => self.progn(cons.cdr()),
            "setq" => self.setq(cons.cdr()),
            "let" => self.let_form(cons.cdr()),
            "if" => self.compile_conditional(cons.cdr()),
            _ => self.compile_funcall(cons),
        }
    }

    fn variable_reference(&mut self, sym: Symbol) -> Result<()> {
        match self.vars.iter().rposition(|&x| x == Some(sym)) {
            Some(idx) => self.stack_ref(idx, sym),
            None => {
                let idx = self.constants.insert(sym.into())?;
                self.codes.emit_varref(idx);
                Ok(())
            }
        }
    }

    fn compile_form(&mut self, obj: Object<'obj>) -> Result<()> {
        match obj.val() {
            Value::Cons(cons) => self.dispatch_special_form(cons),
            Value::Symbol(sym) => self.variable_reference(sym),
            _ => self.add_const(obj, None),
        }
    }

    fn compile_func_body(obj: &[Object], vars: Vec<Option<Symbol>>) -> Result<Self> {
        let mut exp = Self {
            codes: CodeVec::default(),
            constants: ConstVec::new(),
            vars,
        };
        exp.implicit_progn(obj)?;
        exp.codes.push_op(OpCode::Ret);
        exp.vars.truncate(0);
        Ok(exp)
    }

    pub fn compile(obj: Object) -> Result<Self> {
        Self::compile_func_body(&[obj], vec![])
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use crate::arena::Arena;
    use crate::intern::intern;
    use crate::reader::Reader;
    use OpCode::*;

    fn check_error(compare: &str, expect: Error) {
        let arena = &Arena::new();
        let obj = Reader::new(compare).read_into(arena).unwrap().unwrap();
        assert_eq!(Exp::compile(obj).err().unwrap(), expect);
    }

    macro_rules! check_compiler {
        ($compare:expr, [$($op:expr),+], [$($const:expr),+]) => {
            let arena = &Arena::new();
            let obj = Reader::new($compare).read_into(arena).unwrap().unwrap();
            let expect = Exp{
                codes:vec_into![$($op),+].into(),
                constants: ConstVec::from(vec_into_object![$($const),+; arena]),
                vars: Vec::new(),
            };
            assert_eq!(Exp::compile(obj).unwrap(), expect);
        }
    }

    #[test]
    fn test_basic() {
        let arena = &Arena::new();
        check_compiler!("1", [Constant0, Ret], [1]);
        check_compiler!("'foo", [Constant0, Ret], [intern("foo")]);
        check_compiler!("'(1 2)", [Constant0, Ret], [list!(1, 2; arena)]);
    }

    #[test]
    fn variable() {
        check_compiler!("(let (foo))", [Constant0, Constant0, Ret], [false]);
        check_compiler!("(let ())", [Constant0, Ret], [false]);
        check_compiler!(
            "(let ((foo 1)(bar 2)(baz 3)))",
            [Constant0, Constant1, Constant2, Constant3, Ret],
            [1, 2, 3, false]
        );
        check_compiler!("(let ((foo 1)) foo)", [Constant0, StackRef0, Ret], [1]);
        check_compiler!("foo", [VarRef0, Ret], [intern("foo")]);
        check_compiler!("(progn)", [Constant0, Ret], [false]);
        check_compiler!(
            "(progn (set 'foo 5) foo)",
            [Constant0, Constant1, Constant2, Call2, Discard, VarRef1, Ret],
            [intern("set"), intern("foo"), 5]
        );
        check_compiler!(
            "(let ((foo 1)) (setq foo 2) foo)",
            [Constant0, Constant1, Duplicate, StackSet2, Discard, StackRef0, Ret],
            [1, 2]
        );
        check_compiler!(
            "(progn (setq foo 2) foo)",
            [Constant0, Duplicate, VarSet1, Discard, VarRef1, Ret],
            [2, intern("foo")]
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
            [Object::nil(), 1, 2]
        );
        check_compiler!(
            "(if t 2)",
            [Constant0, JumpNilElsePop, high1, low1, Constant1, Ret],
            [Object::t(), 2]
        );
        check_error("(if 1)", Error::ArgCount(2, 1));
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
            [Object::t(), Object::nil()]
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
            [Object::t(), 1]
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
            [Object::nil(), 2]
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
            [Object::nil(), 2, 3]
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

    #[test]
    fn lambda() {
        let arena = &Arena::new();
        check_compiler!("(lambda)", [Constant0, Ret], [LispFn::default()]);
        check_compiler!("(lambda ())", [Constant0, Ret], [LispFn::default()]);
        check_compiler!("(lambda () nil)", [Constant0, Ret], [LispFn::default()]);

        let constant: Object = 1.into_obj(arena);
        let func = LispFn::new(
            vec_into![Constant0, Ret].into(),
            vec![unsafe { constant.into_gc() }],
            0,
            0,
            false,
        );
        check_compiler!("(lambda () 1)", [Constant0, Ret], [func]);

        let func = LispFn::new(vec_into![StackRef0, Ret].into(), vec![], 1, 0, false);
        check_compiler!("(lambda (x) x)", [Constant0, Ret], [func]);

        let func = LispFn::new(
            vec_into![Constant0, StackRef2, StackRef2, Call2, Ret].into(),
            vec_into![intern("+")],
            2,
            0,
            false,
        );
        check_compiler!("(lambda (x y) (+ x y))", [Constant0, Ret], [func]);

        check_error("(lambda (x 1) x)", Error::Type(Type::Symbol, Type::Int));
    }

    #[test]
    fn errors() {
        check_error("(\"foo\")", Error::Type(Type::Symbol, Type::String));
        check_error("(quote)", Error::ArgCount(1, 0));
        check_error("(quote 1 2)", Error::ArgCount(1, 2))
    }

    #[test]
    fn let_errors() {
        check_error("(let (1))", Error::Type(Type::Cons, Type::Int));
        check_error("(let ((foo 1 2)))", Error::LetValueCount(2));
        check_error("(let ((foo . 1)))", Error::Type(Type::List, Type::Int));
        check_error("(let ((foo 1 . 2)))", Error::Type(Type::List, Type::Int));
        check_error("(let (()))", Error::Type(Type::Cons, Type::Nil));
        check_error("(let)", Error::ArgCount(1, 0));
    }
}
