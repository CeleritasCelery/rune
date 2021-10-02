//! Compile a lisp object into bytecode. The returned bytecode is an implicit
//! lambda function that executes the top level code.
//!
//! Note that all functions that compile a special form only take the body of
//! that form. For example, if `compile_if` is called on the form `(if x y z)`,
//! then value passed to the function should be `(x y z)`. The leading symbol is
//! removed. It is assumed that the caller has properly dispatched the on the first symbol.

use crate::arena::Arena;
use crate::bytecode;
use crate::cons::{into_iter, Cons, ElemIter};
use crate::data::Environment;
use crate::error::{Error, Type};
use crate::object::{Callable, Expression, IntoObject, LispFn, Object};
use crate::opcode::{CodeVec, OpCode};
use crate::symbol::{sym, Symbol};
use anyhow::{anyhow, bail, ensure, Result};
use paste::paste;
use std::convert::TryInto;
use std::fmt::Display;

/// Errors that can occur during compilation
#[derive(Debug, PartialEq)]
pub(crate) enum CompError {
    /// The index into the constant vector is u16, so if more then U16_MAX items
    /// are added it will trigger this error
    ConstOverflow,
    /// A let form with more then 1 value i.e. (let ((x y z)))
    LetValueCount,
    /// The stack reference index is a u16, so if stack grows larger then
    /// U16_MAX in a single function call it will trigger this error
    StackSizeOverflow,
    /// Recursive Macro's are not supported in the bootsrap compiler
    RecursiveMacro,
    /// Expected a function object, but found a different type for a macro call
    InvalidMacro(Box<str>),
    /// Expected a function object, but found a different type for a function call
    InvalidFunction(Box<str>),
}

impl Display for CompError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompError::ConstOverflow => write!(f, "Too many constants declared in fuction"),
            CompError::LetValueCount => write!(f, "Let forms can only have 1 value"),
            CompError::StackSizeOverflow => write!(f, "Stack size overflow"),
            CompError::RecursiveMacro => write!(f, "Recursive macros are not supported"),
            CompError::InvalidMacro(x) => write!(f, "Invalid macro type: {}", x),
            CompError::InvalidFunction(x) => write!(f, "Invalid Function : {}", x),
        }
    }
}

impl std::error::Error for CompError {}

/// The constant vector for a function. The first N items of the vector are
/// reserved for upvalue slots to be populated by [`crate::alloc::make_closure`]
#[derive(Debug)]
struct ConstVec<'ob> {
    /// constant vector
    consts: Vec<Object<'ob>>,
    /// The index for the start of constants that are not upvalues
    upvalue_offset: usize,
}

impl<'ob> From<Vec<Object<'ob>>> for ConstVec<'ob> {
    fn from(vec: Vec<Object<'ob>>) -> Self {
        let mut consts = ConstVec {
            consts: Vec::new(),
            upvalue_offset: 0,
        };
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
    fn with_capacity(cap: usize) -> Self {
        ConstVec {
            consts: vec![Object::Nil; cap],
            upvalue_offset: cap,
        }
    }

    /// Return the index of object in the constant vector, otherwise insert it
    fn insert_or_get(&mut self, obj: Object<'ob>) -> usize {
        let mut iter = match obj {
            Object::Nil => self.consts[self.upvalue_offset..].iter(),
            _ => self.consts.iter(),
        };
        match iter.position(|&x| obj == x) {
            None => {
                self.consts.push(obj);
                self.consts.len() - 1
            }
            Some(x) => x,
        }
    }

    /// insert object into constant vector. If object is already present, The
    /// index will be reused
    fn insert(&mut self, obj: Object<'ob>) -> Result<u16, CompError> {
        self.insert_or_get(obj)
            .try_into()
            .map_err(|_e| CompError::ConstOverflow)
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
    /// Push opcode into code vector
    fn push_op(&mut self, op: OpCode) {
        #[cfg(feature = "debug_bytecode")]
        println!("op :{}: {:?}", self.len(), op);
        self.push(op.into());
    }

    /// Push opcode + 1 byte argument
    fn push_op_n(&mut self, op: OpCode, arg: u8) {
        self.push_op(op);
        self.push(arg);
    }

    /// Push opcode + 2 byte argument
    fn push_op_n2(&mut self, op: OpCode, arg: u16) {
        self.push_op(op);
        self.push(arg as u8);
        self.push((arg >> 8) as u8);
    }

    /// Push a 2-byte placeholder to store the jump address once it has been
    /// computed
    fn push_jump_placeholder(&mut self) -> usize {
        let idx = self.len();
        self.push(0);
        self.push(0);
        idx
    }

    /// Set the previously reserved jump placeholder to an offset in the code
    /// vector. index should be the index of in the vector of where to jump to.
    fn set_jump_placeholder(&mut self, index: usize) {
        let offset = self.len() as i16 - index as i16 - JUMP_SLOTS;
        self[index] = offset as u8;
        self[index + 1] = (offset >> 8) as u8;
    }

    /// Push a backward's jump offset. This does not need a placeholder because a
    /// backwards jump can be calculated ahead of time.
    fn push_back_jump(&mut self, index: usize) {
        let offset = index as i16 - self.len() as i16 - JUMP_SLOTS;
        self.push(offset as u8);
        self.push((offset >> 8) as u8);
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

/// Object containg compiler state for the current function. All expressions are
/// treated as part of some function, even if it is implicit
#[derive(Debug, PartialEq)]
struct Compiler<'ob, 'brw> {
    /// function byte codes
    codes: CodeVec,
    /// function constants
    constants: ConstVec<'ob>,
    /// Emulates the runtime stack for correctly calculating stack references.
    /// If an element is Some(X) then the stack slot represents a binding for
    /// variable X.
    vars: Vec<Option<Symbol>>,
    ///  A reference  to  the parent's  variables. This  is  used for  resolving
    /// upvalues  in closures. If  a variable cannot  be found in  this function
    /// frame then it's parent is searched as well. A root function has no parent.
    parent: Option<&'brw [Option<Symbol>]>,
    /// Upvalues that are used in this closure. Not that at this time, the
    /// bootstrap compiler binds *value* and not *variables*.
    upvalues: Vec<Symbol>,
    /// A reference to the current runtime environment
    env: &'brw mut Environment<'ob>,
    /// A reference to the current allocation arean
    arena: &'ob Arena,
}

/// The maximum number of upvalues a function can store. This is a limitation of
/// only having a single pass compiler. Most of these slots will be wasted. We
/// need to put the upvalues at the start of the constant vector so they can be
/// replaced by their values in [`crate::alloc::make_closure`].
const UPVALUE_RESERVE: usize = 8;

impl<'ob, 'brw> Compiler<'ob, 'brw> {
    fn new(
        vars: Vec<Option<Symbol>>,
        parent: Option<&'brw [Option<Symbol>]>,
        env: &'brw mut Environment<'ob>,
        arena: &'ob Arena,
    ) -> Compiler<'ob, 'brw> {
        Self {
            codes: CodeVec::default(),
            constants: ConstVec::with_capacity(if parent.is_some() { UPVALUE_RESERVE } else { 0 }),
            upvalues: vec![],
            parent,
            env,
            vars,
            arena,
        }
    }

    fn into_sexp(self) -> (Vec<Symbol>, Expression<'ob>) {
        (
            self.upvalues,
            Expression {
                constants: self.constants.consts,
                op_codes: self.codes,
            },
        )
    }

    /// Add a value to the emulated stack
    fn grow_stack(&mut self, var: Option<Symbol>) {
        self.vars.push(var);
        #[cfg(feature = "debug_bytecode")]
        {
            println!("growing stack: {:?}", var);
            println!("[");
            for (idx, x) in self.vars.iter().enumerate() {
                println!("    {}: {:?},", idx, x);
            }
            println!("]");
        }
    }

    /// pop the top of the emulated stack
    fn shrink_stack(&mut self) -> Option<Symbol> {
        let var = self.vars.pop().expect("compile stack should not be empty");
        #[cfg(feature = "debug_bytecode")]
        {
            println!("shrinking stack");
            println!("[");
            for (idx, x) in self.vars.iter().enumerate() {
                println!("    {}: {:?},", idx, x);
            }
            println!("]");
        }
        var
    }

    /// Truncate stack to a new length
    fn truncate_stack(&mut self, new_len: usize) {
        self.vars.truncate(new_len);
        #[cfg(feature = "debug_bytecode")]
        {
            println!("truncating stack");
            println!("[");
            for (idx, x) in self.vars.iter().enumerate() {
                println!("    {}: {:?},", idx, x);
            }
            println!("]");
        }
    }

    /// emit the opcode to reference object in the constant vector
    fn const_ref(&mut self, obj: Object<'ob>, var_ref: Option<Symbol>) -> Result<()> {
        let idx = self.constants.insert(obj)?;
        self.codes.emit_const(idx);
        self.grow_stack(var_ref);
        Ok(())
    }

    /// emit the opcode to reference the value at idx and bind it to a Symbol
    fn stack_ref(&mut self, idx: usize, var_ref: Symbol) -> Result<()> {
        match (self.vars.len() - idx - 1).try_into() {
            Ok(x) => {
                self.codes.emit_stack_ref(x);
                self.grow_stack(Some(var_ref));
                Ok(())
            }
            Err(_) => Err(CompError::StackSizeOverflow.into()),
        }
    }

    /// emit the opcode to set a value on the stack at idx
    fn stack_set(&mut self, idx: usize) -> Result<(), CompError> {
        match (self.vars.len() - idx - 1).try_into() {
            Ok(x) => {
                self.codes.emit_stack_set(x);
                self.shrink_stack();
                Ok(())
            }
            Err(_) => Err(CompError::StackSizeOverflow),
        }
    }

    /// Set a global variable
    fn var_set(&mut self, idx: u16) {
        self.codes.emit_varset(idx);
        self.shrink_stack();
    }

    /// Discard the top of stack
    fn discard(&mut self) {
        self.codes.push_op(OpCode::Discard);
        self.shrink_stack();
    }

    /// duplicate the top of stack
    fn duplicate(&mut self) {
        self.codes.push_op(OpCode::Duplicate);
        self.grow_stack(None);
    }

    /// add the quoted object to the constant vector
    fn quote(&mut self, value: Object<'ob>) -> Result<()> {
        let mut forms = into_iter(value)?;
        match forms.len() {
            1 => self.const_ref(forms.next().unwrap()?, None),
            x => Err(Error::ArgCount(1, x as u16).into()),
        }
    }

    /// If backquote is defined then emit a call to the macro, otherwise treat
    /// this as a normal quote. This is a hack needed because backquote is used
    /// in functions before it is defined. The bootstrap compiler does not
    /// support lazy macro expansion.
    fn backquote(&mut self, value: Object<'ob>) -> Result<()> {
        let sym = &crate::symbol::sym::BACKQUOTE;
        if sym.func().is_some() {
            self.compile_call(sym.into(), value)
        } else {
            self.quote(value)
        }
    }

    /// Process a function quote `(function foo)`. If value is a cons, compile it. Otherwise
    /// treat it as a constant reference.
    fn function(&mut self, value: Object<'ob>) -> Result<()> {
        let mut forms = into_iter(value)?;
        let len = forms.len();
        if len == 1 {
            match forms.next().unwrap()? {
                Object::Cons(cons) => self.compile_sexp(&*cons),
                sym => self.const_ref(sym, None),
            }
        } else {
            Err(Error::ArgCount(1, len as u16).into())
        }
    }

    fn compile_let(&mut self, form: Object<'ob>, parallel: bool) -> Result<()> {
        let mut iter = into_iter(form)?;
        let num_binding_forms = match iter.next() {
            // (let x ...)
            Some(x) => self.let_bind(x?, parallel)?,
            // (let)
            None => bail!(Error::ArgCount(1, 0)),
        };
        self.implicit_progn(iter)?;
        // Remove let bindings from the stack
        if num_binding_forms > 0 {
            self.codes
                .push_op_n(OpCode::DiscardNKeepTOS, num_binding_forms as u8);
            let last = self.shrink_stack();
            self.truncate_stack(self.vars.len() - num_binding_forms);
            self.grow_stack(last);
        }
        Ok(())
    }

    fn progn(&mut self, forms: Object<'ob>) -> Result<()> {
        self.implicit_progn(into_iter(forms)?)
    }

    /// Compile prog1 or prog2 special forms
    fn progx(&mut self, forms: Object<'ob>, returned_form: usize) -> Result<()> {
        let mut form_idx = 0;
        for form in into_iter(forms)? {
            form_idx += 1;
            self.compile_form(form?)?;
            if form_idx != returned_form {
                self.discard();
            }
        }
        ensure!(
            form_idx >= returned_form,
            Error::ArgCount(returned_form as u16, form_idx as u16)
        );
        Ok(())
    }

    /// Compile implicit progn generally seen in function bodies
    fn implicit_progn(&mut self, mut forms: ElemIter<'_, 'ob>) -> Result<()> {
        if let Some(form) = forms.next() {
            self.compile_form(form?)?;
        } else {
            return self.const_ref(Object::Nil, None);
        }
        for form in forms {
            self.discard();
            self.compile_form(form?)?;
        }
        Ok(())
    }

    /// Compile the value of a let binding.
    /// ```lisp
    /// (let ((var (val))) ...)
    ///            ^^^^^
    /// ```
    fn let_bind_value(&mut self, cons: &'ob Cons<'ob>) -> Result<Symbol> {
        let mut iter = into_iter(cons.cdr())?;
        match iter.next() {
            // (let ((x y)))
            Some(value) => self.compile_form(value?)?,
            // (let ((x)))
            None => self.const_ref(Object::Nil, None)?,
        };
        // (let ((x y z ..)))
        ensure!(iter.next().is_none(), CompError::LetValueCount);
        Ok(cons.car().try_into()?)
    }

    /// Compile all let bindings.
    /// ```lisp
    /// (let (x y (z a)) ...)
    ///      ^^^^^^^^^^^
    /// ```
    fn let_bind(&mut self, obj: Object<'ob>, parallel: bool) -> Result<usize> {
        let bindings = into_iter(obj)?;
        let mut len = 0;
        let mut let_bindings = Vec::new();
        for binding in bindings {
            let binding = binding?;
            match binding {
                // (let ((x y)))
                Object::Cons(cons) => {
                    let let_bound_var = self.let_bind_value(!cons)?;
                    if parallel {
                        let_bindings.push(Some(let_bound_var));
                    } else {
                        let last = self.vars.last_mut();
                        let tos = last.expect("stack empty after compile form");
                        *tos = Some(let_bound_var);
                    }
                }
                // (let (x))
                Object::Symbol(sym) => self.const_ref(Object::Nil, Some(!sym))?,
                _ => bail!(Error::from_object(Type::Cons, binding)),
            }
            len += 1;
        }
        if parallel {
            let num_unbound_vars = let_bindings.len();
            let stack_size = self.vars.len();
            debug_assert!(stack_size >= num_unbound_vars);
            let binding_start = stack_size - num_unbound_vars;
            self.vars.drain(binding_start..);
            self.vars.append(&mut let_bindings);
        }
        Ok(len)
    }

    /// Compile setq special form.
    fn setq(&mut self, obj: Object<'ob>) -> Result<()> {
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
                Some(val) => self.compile_form(val?)?,
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

    /// Compile a call to a macro.
    fn compile_macro_call(
        &mut self,
        name: Symbol,
        args: Object<'ob>,
        body: Object<'ob>,
    ) -> Result<Object<'ob>> {
        println!("compiling macro : {}", name);
        let arena = self.arena;
        let mut arg_list = vec![];
        for arg in into_iter(args)? {
            arg_list.push(arg?);
        }
        match body {
            Object::LispFn(lisp_macro) => {
                bytecode::call_lisp(&lisp_macro, arg_list, self.env, arena)
            }
            Object::SubrFn(lisp_macro) => {
                bytecode::call_subr(*lisp_macro, arg_list, self.env, arena)
            }
            Object::Cons(macro_form) => {
                if self.env.macro_callstack.iter().any(|&x| x == name) {
                    bail!(CompError::RecursiveMacro);
                }
                self.env.macro_callstack.push(name);

                let lisp_macro = {
                    let func_ident = macro_form.car().as_symbol()?;
                    symbol_match! { func_ident;
                        LAMBDA => compile_lambda(macro_form.cdr(), self.env, arena)?,
                        @ bad_function => bail!(CompError::InvalidFunction(bad_function.to_string().into())),
                    }
                };
                self.env.macro_callstack.pop();
                let func = arena.add(lisp_macro);
                let def = cons!(&sym::MACRO, func; arena);
                crate::data::set_global_function(
                    name,
                    def.try_into().expect("Type should be a valid macro"),
                );
                if let Object::LispFn(func) = func {
                    bytecode::call_lisp(&func, arg_list, self.env, arena)
                } else {
                    unreachable!("Compiled function was not lisp fn");
                }
            }
            x => bail!(CompError::InvalidMacro(x.to_string().into())),
        }
    }

    /// Emit the bytecode for a function call
    fn emit_call(&mut self, arg_cnt: usize) {
        self.codes.emit_call(arg_cnt as u16);
        let new_stack_size = self.vars.len() - arg_cnt;
        self.truncate_stack(new_stack_size);
    }

    fn compile_func_call(&mut self, func: Object<'ob>, args: Object<'ob>) -> Result<()> {
        println!("compiling call : {}", func);
        self.const_ref(func.into_obj(self.arena), None)?;
        let args = into_iter(args)?;
        let mut num_args = 0;
        for arg in args {
            self.compile_form(arg?)?;
            num_args += 1;
        }
        self.emit_call(num_args);
        Ok(())
    }

    /// Compile a call to a macro or function
    fn compile_call(&mut self, func: Object<'ob>, args: Object<'ob>) -> Result<()> {
        if let Object::Symbol(name) = func {
            if let Some(Callable::Macro(cons)) = name.resolved_func() {
                let form = self.compile_macro_call(!name, args, cons.cdr())?;
                return self.compile_form(form);
            }
        }
        self.compile_func_call(func, args)
    }

    /// Emit a jump instruction and reserve a offset placeholder.
    fn jump(&mut self, jump_code: OpCode) -> usize {
        match jump_code {
            OpCode::JumpNil
            | OpCode::JumpNotNil
            | OpCode::JumpNilElsePop
            | OpCode::JumpNotNilElsePop => {
                self.shrink_stack();
            }
            OpCode::Jump => {}
            x => panic!("invalid jump opcode provided: {:?}", x),
        }
        self.codes.push_op(jump_code);
        let place = self.codes.push_jump_placeholder();
        place
    }

    /// Emit a backwards jump
    fn jump_back(&mut self, location: usize) {
        self.codes.push_op(OpCode::Jump);
        self.codes.push_back_jump(location);
    }

    fn compile_if(&mut self, obj: Object<'ob>) -> Result<()> {
        let mut forms = into_iter(obj)?;
        match forms.len() {
            // (if) | (if x)
            len @ (0 | 1) => Err(Error::ArgCount(2, len as u16).into()),
            // (if x y)
            2 => {
                self.compile_form(forms.next().unwrap()?)?;
                let target = self.jump(OpCode::JumpNilElsePop);
                self.compile_form(forms.next().unwrap()?)?;
                self.codes.set_jump_placeholder(target);
                Ok(())
            }
            // (if x y z ...)
            _ => {
                self.compile_form(forms.next().unwrap()?)?;
                let else_nil_target = self.jump(OpCode::JumpNil);
                // if branch
                self.compile_form(forms.next().unwrap()?)?;
                self.shrink_stack();
                let jump_to_end_target = self.jump(OpCode::Jump);
                // else branch
                self.codes.set_jump_placeholder(else_nil_target);
                self.implicit_progn(forms)?;
                self.codes.set_jump_placeholder(jump_to_end_target);
                Ok(())
            }
        }
    }

    fn compile_loop(&mut self, obj: Object<'ob>) -> Result<()> {
        let mut forms = into_iter(obj)?;
        let top = self.codes.len();
        match forms.next() {
            Some(form) => self.compile_form(form?)?,
            None => bail!(Error::ArgCount(1, 0)),
        }
        let loop_exit = self.jump(OpCode::JumpNilElsePop);
        self.implicit_progn(forms)?;
        self.discard();
        self.jump_back(top);
        self.codes.set_jump_placeholder(loop_exit);
        // Add the nil return value
        self.grow_stack(None);
        Ok(())
    }

    /// Return true if this closure is contained inside another closure.
    fn closure_is_nested(&self) -> bool {
        match self.parent {
            None => false,
            Some(vars) => vars.iter().any(Option::is_some),
        }
    }

    #[allow(clippy::branches_sharing_code)] // bug in clippy #7628
    fn compile_lambda(&mut self, obj: Object<'ob>) -> Result<()> {
        if self.closure_is_nested() {
            bail!("Nested closures are not supported in the bootstrap compiler");
        }
        let (upvalues, lambda) = if self.vars.iter().any(Option::is_some) {
            compile_closure(obj, Some(&self.vars), self.env, self.arena)?
        } else {
            compile_closure(obj, None, self.env, self.arena)?
        };

        if upvalues.is_empty() {
            self.const_ref(lambda.into_obj(self.arena), None)?;
        } else {
            self.const_ref((&crate::alloc::MAKE_CLOSURE).into(), None)?;
            self.const_ref(lambda.into_obj(self.arena), None)?;
            let len = upvalues.len();
            for upvalue in upvalues {
                match self.vars.iter().rposition(|&x| x == Some(upvalue)) {
                    Some(idx) => self.stack_ref(idx, upvalue)?,
                    None => {
                        panic!("upvalue `{}' not found", upvalue);
                    }
                }
            }
            self.emit_call(len + 1);
        }
        Ok(())
    }

    fn compile_defvar(&mut self, obj: Object<'ob>) -> Result<()> {
        let mut iter = into_iter(obj)?;

        match iter.next() {
            // (defvar x ...)
            Some(x) => {
                let sym = x?.as_symbol()?;
                // TODO: compile this into a lambda like Emacs does
                match iter.next() {
                    // (defvar x y)
                    Some(value) => self.compile_form(value?)?,
                    // (defvar x)
                    None => self.const_ref(Object::Nil, None)?,
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

    /// Compile a cond clause
    /// ```lisp
    /// (cond (x y) (a b))
    ///       ^^^^^
    /// ```
    fn compile_cond_clause(
        &mut self,
        clause: Object<'ob>,
        jump_targets: &mut Vec<usize>,
    ) -> Result<()> {
        let mut cond = into_iter(clause)?;
        match cond.len() {
            // (cond ())
            0 => {}
            // (cond (x))
            1 => {
                self.compile_form(cond.next().unwrap()?)?;
                let target = self.jump(OpCode::JumpNotNilElsePop);
                jump_targets.push(target);
            }
            // (cond (x y ...))
            _ => {
                self.compile_form(cond.next().unwrap()?)?;
                let skip_target = self.jump(OpCode::JumpNil);
                self.implicit_progn(cond)?;
                self.shrink_stack();
                let taken_target = self.jump(OpCode::Jump);
                self.codes.set_jump_placeholder(skip_target);
                jump_targets.push(taken_target);
            }
        }
        Ok(())
    }

    /// Compile the last cond clause. This differs from [`Self::compile_cond_clause`]
    /// by the way jumps are resolved.
    /// ```lisp
    /// (cond (x y) (a b))
    ///             ^^^^^
    /// ```
    fn compile_last_cond_clause(
        &mut self,
        clause: Object<'ob>,
        jump_targets: &mut Vec<usize>,
    ) -> Result<()> {
        let mut cond = into_iter(clause)?;
        match cond.len() {
            // (cond ())
            0 => {
                self.const_ref(Object::Nil, None)?;
            }
            // (cond (x))
            1 => {
                self.compile_form(cond.next().unwrap()?)?;
                let target = self.jump(OpCode::JumpNotNilElsePop);
                self.const_ref(Object::Nil, None)?;
                jump_targets.push(target);
            }
            // (cond (x y ...))
            _ => {
                self.compile_form(cond.next().unwrap()?)?;
                let target = self.jump(OpCode::JumpNilElsePop);
                self.implicit_progn(cond)?;
                jump_targets.push(target);
            }
        }
        Ok(())
    }

    /// Compile a combinator like and/or. `empty_value` determines what the
    /// value should be if no arguments are given (e.g. `(and)`). This is also
    /// used to determine how forms are combined.
    fn compile_combinator(&mut self, forms: Object<'ob>, empty_value: bool) -> Result<()> {
        let mut conditions = into_iter(forms)?;

        if conditions.is_empty() {
            return self.const_ref(empty_value.into(), None);
        }

        let mut final_return_targets = Vec::new();
        let jump_op = if empty_value {
            OpCode::JumpNilElsePop
        } else {
            OpCode::JumpNotNilElsePop
        };
        while let Some(condition) = conditions.next() {
            self.compile_form(condition?)?;
            if !conditions.is_empty() {
                let target = self.jump(jump_op);
                final_return_targets.push(target);
            }
        }
        for target in final_return_targets {
            self.codes.set_jump_placeholder(target);
        }
        Ok(())
    }

    fn compile_cond(&mut self, obj: Object<'ob>) -> Result<()> {
        let mut clauses = into_iter(obj)?;
        // (cond)
        if clauses.is_empty() {
            return self.const_ref(Object::Nil, None);
        }

        let final_return_targets = &mut Vec::new();
        while let Some(clause) = clauses.next() {
            if clauses.is_empty() {
                self.compile_last_cond_clause(clause?, final_return_targets)?;
            } else {
                self.compile_cond_clause(clause?, final_return_targets)?;
            }
        }

        for target in final_return_targets {
            self.codes.set_jump_placeholder(*target);
        }
        Ok(())
    }

    /// The main dispatch function for all forms. Looks at the symbol in the
    /// form to determine if it is a special form that needs handling by the
    /// compiler or if it is macro. Otherwise it is compiled as a function call.
    fn compile_sexp(&mut self, cons: &Cons<'ob>) -> Result<()> {
        let forms = cons.cdr();
        match cons.car() {
            Object::Symbol(form) => symbol_match! {!form;
                LAMBDA => self.compile_lambda(forms),
                WHILE => self.compile_loop(forms),
                QUOTE => self.quote(forms),
                BACKQUOTE => self.backquote(forms),
                FUNCTION => self.function(forms),
                PROGN => self.progn(forms),
                PROG1 => self.progx(forms, 1),
                PROG2 => self.progx(forms, 2),
                SETQ => self.setq(forms),
                DEFVAR  => self.compile_defvar(forms),
                DEFCONST => self.compile_defvar(forms),
                COND => self.compile_cond(forms),
                LET => self.compile_let(forms, true),
                LET_STAR => self.compile_let(forms, false),
                IF => self.compile_if(forms),
                AND => self.compile_combinator(forms, true),
                OR => self.compile_combinator(forms, false),
                _ => self.compile_call(cons.car(), forms),
            },
            other => self.compile_call(other, forms),
        }
    }

    /// Determine if a symbol is captured by a closure.
    fn is_upvalue(&self, sym: Symbol) -> bool {
        match self.parent {
            None => false,
            Some(vars) => vars.iter().any(|&x| x == Some(sym)),
        }
    }

    /// Add an upvalue to the start of the constant vector.
    fn add_upvalue(&mut self, sym: Symbol) -> Result<()> {
        let idx = match self.upvalues.iter().position(|&x| x == sym) {
            Some(i) => i,
            None => {
                let len = self.upvalues.len();
                ensure!(
                    len < UPVALUE_RESERVE,
                    anyhow!(
                        "Bootstrap compiler can only capture {} values in a closure",
                        UPVALUE_RESERVE
                    )
                );
                self.upvalues.push(sym);
                len
            }
        };
        self.codes.emit_const(idx as u16);
        self.grow_stack(None);
        Ok(())
    }

    /// Emit a variable reference. A variables value can be found in the following places
    /// 1. Self quoted (if the variable starts with `:`)
    /// 2. Stack
    /// 3. Parent frames stack (upvalue)
    /// 4. Globals
    fn variable_reference(&mut self, sym: Symbol) -> Result<()> {
        if sym.name.starts_with(':') {
            self.const_ref(sym.into(), None)
        } else {
            match self.vars.iter().rposition(|&x| x == Some(sym)) {
                Some(idx) => self.stack_ref(idx, sym),
                None => {
                    if self.is_upvalue(sym) {
                        self.add_upvalue(sym)
                    } else {
                        let idx = self.constants.insert(sym.into())?;
                        self.codes.emit_varref(idx);
                        self.grow_stack(None);
                        Ok(())
                    }
                }
            }
        }
    }

    fn compile_form(&mut self, obj: Object<'ob>) -> Result<()> {
        match obj {
            Object::Cons(cons) => self.compile_sexp(&cons),
            Object::Symbol(sym) => self.variable_reference(!sym),
            _ => self.const_ref(obj, None),
        }
    }

    fn compile_func_body(
        obj: ElemIter<'_, 'ob>,
        vars: Vec<Option<Symbol>>,
        parent: Option<&'brw [Option<Symbol>]>,
        env: &'brw mut Environment<'ob>,
        arena: &'ob Arena,
    ) -> Result<Self> {
        let mut exp = Compiler::new(vars, parent, env, arena);
        exp.implicit_progn(obj)?;
        exp.codes.push_op(OpCode::Ret);
        exp.truncate_stack(0);
        Ok(exp)
    }
}

/// Parse the function bindings in a lambda.
/// ```lisp
/// (x &optional y &rest z)
/// ```
fn parse_fn_binding(bindings: Object) -> Result<(u16, u16, bool, Vec<Symbol>)> {
    let mut args: Vec<Symbol> = vec![];
    let mut required = 0;
    let mut optional = 0;
    let mut rest = false;
    let mut arg_type = &mut required;
    let mut iter = into_iter(bindings)?;
    while let Some(binding) = iter.next() {
        symbol_match! { binding?.as_symbol()?;
            AND_OPTIONAL => arg_type = &mut optional,
            AND_REST => {
                if let Some(last) = iter.next() {
                    rest = true;
                    args.push(last?.as_symbol()?);
                    ensure!(
                        iter.next().is_none(),
                        "Found multiple arguments after &rest"
                    );
                }
            },
            @ sym => {
                *arg_type += 1;
                args.push(sym);
            }
        }
    }
    Ok((required, required + optional, rest, args))
}

fn compile_closure<'ob>(
    obj: Object<'ob>,
    parent: Option<&[Option<Symbol>]>,
    env: &mut Environment<'ob>,
    arena: &'ob Arena,
) -> Result<(Vec<Symbol>, LispFn<'ob>)> {
    let mut iter = into_iter(obj)?;

    let (required, optional, rest, args) = match iter.next() {
        // (lambda ())
        None => return Ok((Vec::new(), LispFn::default())),
        // (lambda (x ...) ...)
        Some(bindings) => parse_fn_binding(bindings?)?,
    };
    if iter.is_empty() {
        Ok((Vec::new(), LispFn::default()))
    } else {
        let func_args = args.into_iter().map(Some).collect();
        let compiler = Compiler::compile_func_body(iter, func_args, parent, env, arena)?;
        let (upvalues, exp) = compiler.into_sexp();
        let func = LispFn::new(exp.op_codes, exp.constants, required, optional, rest);
        Ok((upvalues, func))
    }
}

/// Compile a lambda.
pub(crate) fn compile_lambda<'ob>(
    obj: Object<'ob>,
    env: &mut Environment<'ob>,
    arena: &'ob Arena,
) -> Result<LispFn<'ob>> {
    compile_closure(obj, None, env, arena).map(|x| x.1)
}

/// Compile a lisp object.
pub(crate) fn compile<'ob>(
    obj: Object<'ob>,
    env: &mut Environment<'ob>,
    arena: &'ob Arena,
) -> Result<Expression<'ob>> {
    let cons = Cons::new(obj, Object::Nil);
    Compiler::compile_func_body(cons.into_iter(), vec![], None, env, arena).map(|x| x.into_sexp().1)
}

#[cfg(test)]
mod test {

    use super::*;
    use crate::reader::read;
    use crate::symbol::intern;
    #[allow(clippy::enum_glob_use)]
    use OpCode::*;

    fn check_error<E>(compare: &str, expect: E)
    where
        E: std::error::Error + PartialEq + Send + Sync + 'static,
    {
        let arena = &Arena::new();
        let env = &mut Environment::default();
        let obj = read(compare, arena).unwrap().0;
        assert_eq!(
            compile(obj, env, arena)
                .err()
                .unwrap()
                .downcast::<E>()
                .unwrap(),
            expect
        );
    }

    macro_rules! check_compiler {
        ($compare:expr, [$($op:expr),+], [$($const:expr),+]) => ({
            let comp_arena = &Arena::new();
            let comp_env = &mut Environment::default();
            println!("Test String: {}", $compare);
            let obj = read($compare, comp_arena).unwrap().0;
            let expect = Expression {
                op_codes: vec_into![$($op),+].into(),
                constants: vec_into_object![$($const),+; comp_arena],
            };
            assert_eq!(compile(obj, comp_env, comp_arena).unwrap(), expect);
        })
    }

    #[test]
    fn test_basic() {
        let arena = &Arena::new();
        check_compiler!("1", [Constant0, Ret], [1]);
        check_compiler!("'foo", [Constant0, Ret], [&sym::test::FOO]);
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
        check_compiler!("foo", [VarRef0, Ret], [&sym::test::FOO]);
        check_compiler!("(progn)", [Constant0, Ret], [false]);
        check_compiler!(
            "(progn (set 'foo 5) foo)",
            [Constant0, Constant1, Constant2, Call2, Discard, VarRef1, Ret],
            [intern("set"), &sym::test::FOO, 5]
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
            [2, &sym::test::FOO]
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
            [4, intern("+"), &sym::test::FOO]
        );
        check_compiler!(
            "(defvar foo 1)",
            [Constant0, Duplicate, VarSet1, Ret],
            [1, &sym::test::FOO]
        );
        check_compiler!(
            "(defvar foo)",
            [Constant0, Duplicate, VarSet1, Ret],
            [false, &sym::test::FOO]
        );
        check_error("(let (foo 1))", Error::from_object(Type::Cons, 1.into()));
    }

    const fn get_jump_slots(offset: i16) -> (u8, u8) {
        (offset as u8, (offset >> 8) as u8)
    }

    #[test]
    fn conditional() {
        let (high4, low4) = get_jump_slots(4);
        let (high1, low1) = get_jump_slots(1);
        check_compiler!(
            "(if nil 1 2)",
            [Constant0, JumpNil, high4, low4, Constant1, Jump, high1, low1, Constant2, Ret],
            [Object::Nil, 1, 2]
        );
        check_compiler!(
            "(if t 2)",
            [Constant0, JumpNilElsePop, high1, low1, Constant1, Ret],
            [Object::True, 2]
        );
        check_compiler!("(and)", [Constant0, Ret], [Object::True]);
        check_compiler!("(or)", [Constant0, Ret], [Object::Nil]);
        check_error("(if 1)", Error::ArgCount(2, 1));
    }

    #[test]
    fn cond_stmt() {
        let (high1, low1) = get_jump_slots(1);
        check_compiler!("(cond)", [Constant0, Ret], [Object::Nil]);
        check_compiler!("(cond ())", [Constant0, Ret], [Object::Nil]);
        check_compiler!(
            "(cond (1))",
            [Constant0, JumpNotNilElsePop, high1, low1, Constant1, Ret],
            [1, false]
        );
        check_compiler!(
            "(cond (1 2))",
            [Constant0, JumpNilElsePop, high1, low1, Constant1, Ret],
            [1, 2]
        );
        check_compiler!(
            "(cond (1 2)(3 4))",
            [
                Constant0,
                JumpNil,
                4,
                0,
                Constant1,
                Jump,
                5,
                0,
                Constant2,
                JumpNilElsePop,
                high1,
                low1,
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
                5,
                0,
                Constant1,
                JumpNotNilElsePop,
                1,
                0,
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
            [Object::True, Object::Nil]
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
            [Object::True, 1]
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
            [Object::Nil, 2]
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
            [Object::Nil, 2, 3]
        );
        check_error("(while)", Error::ArgCount(1, 0));
    }

    #[test]
    fn function() {
        check_compiler!("(foo)", [Constant0, Call0, Ret], [&sym::test::FOO]);
        check_compiler!(
            "(foo 1 2)",
            [Constant0, Constant1, Constant2, Call2, Ret],
            [&sym::test::FOO, 1, 2]
        );
        check_compiler!(
            "(foo (bar 1) 2)",
            [Constant0, Constant1, Constant2, Call1, Constant3, Call2, Ret],
            [&sym::test::FOO, &sym::test::BAR, 1, 2]
        );
        check_compiler!(
            "(foo (bar 1) (baz 1))",
            [Constant0, Constant1, Constant2, Call1, Constant3, Constant2, Call1, Call2, Ret],
            [&sym::test::FOO, &sym::test::BAR, 1, &sym::test::BAZ]
        );
        check_error("(foo . 1)", Error::from_object(Type::List, 1.into()));
    }

    fn check_lambda<'ob>(sexp: &str, func: LispFn<'ob>, comp_arena: &'ob Arena) {
        println!("Test String: {}", sexp);
        let obj = read(sexp, comp_arena).unwrap().0;
        let env = &mut Environment::default();
        let lambda = match obj {
            Object::Cons(cons) => cons.cdr(),
            x => panic!("expected cons, found {}", x),
        };
        assert_eq!(compile_lambda(lambda, env, comp_arena).unwrap(), func);
    }

    #[test]
    fn lambda() {
        let arena = &Arena::new();
        let env = &mut Environment::default();
        check_lambda("(lambda)", LispFn::default(), arena);
        check_lambda("(lambda ())", LispFn::default(), arena);
        check_lambda("(lambda () nil)", LispFn::default(), arena);

        check_lambda(
            "(lambda () 1)",
            LispFn::new(vec_into![Constant0, Ret].into(), vec_into![1], 0, 0, false),
            arena,
        );

        check_lambda(
            "(lambda (x) x)",
            LispFn::new(vec_into![StackRef0, Ret].into(), vec![], 1, 1, false),
            arena,
        );

        check_lambda(
            "(lambda (x &optional) x)",
            LispFn::new(vec_into![StackRef0, Ret].into(), vec![], 1, 1, false),
            arena,
        );
        check_lambda(
            "(lambda (x &optional y) x)",
            LispFn::new(vec_into![StackRef1, Ret].into(), vec![], 1, 2, false),
            arena,
        );
        check_lambda(
            "(lambda (x &optional y z) y)",
            LispFn::new(vec_into![StackRef1, Ret].into(), vec![], 1, 3, false),
            arena,
        );
        check_lambda(
            "(lambda (x &optional y &optional z) z)",
            LispFn::new(vec_into![StackRef0, Ret].into(), vec![], 1, 3, false),
            arena,
        );
        check_lambda(
            "(lambda (x &rest) x)",
            LispFn::new(vec_into![StackRef0, Ret].into(), vec![], 1, 1, false),
            arena,
        );
        check_lambda(
            "(lambda (x &rest y) y)",
            LispFn::new(vec_into![StackRef0, Ret].into(), vec![], 1, 1, true),
            arena,
        );

        let obj = read("(lambda (x &rest y z) y)", arena).unwrap().0;
        assert!(compile(obj, env, arena)
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
                2,
                false,
            ),
            arena,
        );

        check_error(
            "(lambda (x 1) x)",
            Error::from_object(Type::Symbol, 1.into()),
        );
    }

    #[test]
    fn test_closure() {
        let func = LispFn::new(
            vec_into![Constant0, Ret].into(),
            vec![Object::Nil; UPVALUE_RESERVE],
            0,
            0,
            false,
        );
        check_compiler!(
            "(let ((x 1)(y 2)) (lambda () x))",
            [
                Constant0,
                Constant1,
                Constant2,
                Constant3,
                StackRef3,
                Call2,
                DiscardNKeepTOS,
                2,
                Ret
            ],
            [1, 2, &crate::alloc::MAKE_CLOSURE, func]
        );

        let mut consts = vec![Object::Nil; UPVALUE_RESERVE];
        consts.push(intern("+").into());
        let func = LispFn::new(
            vec_into![ConstantN, 8, Constant0, Constant1, Call2, Ret].into(),
            consts,
            0,
            0,
            false,
        );
        check_compiler!(
            "(let ((x 1)(y 2)) (lambda () (+ y x)))",
            [
                Constant0,
                Constant1,
                Constant2,
                Constant3,
                StackRef2,
                StackRef4,
                Call3,
                DiscardNKeepTOS,
                2,
                Ret
            ],
            [1, 2, &crate::alloc::MAKE_CLOSURE, func]
        );
    }

    #[test]
    fn errors() {
        check_error("(quote)", Error::ArgCount(1, 0));
        check_error("(quote 1 2)", Error::ArgCount(1, 2));
        check_error("(let (1))", Error::from_object(Type::Cons, 1.into()));
        check_error("(let ((foo 1 2)))", CompError::LetValueCount);
        check_error(
            "(let ((foo . 1)))",
            Error::from_object(Type::List, 1.into()),
        );
        check_error("(let (()))", Error::from_object(Type::Cons, Object::Nil));
        check_error("(let)", Error::ArgCount(1, 0));
    }
}
