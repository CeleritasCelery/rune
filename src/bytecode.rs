//! The main bytecode interpeter.

use std::ops::{DerefMut, Index, IndexMut, RangeTo};

use anyhow::{bail, Result};
use bstr::ByteSlice;
use fn_macros::{defun, Trace};

use crate::core::env::{sym, Env, Symbol};
use crate::core::error::{ErrorType, EvalError, EvalResult};
use crate::core::gc::{Context, IntoRoot, Root, Rt, Trace};
use crate::core::object::{nil, ByteFn, Gc, GcObj, LispString, LispVec, Object};
use crate::root;

mod opcode;

/// An instruction pointer. This is implemented as a bound checked range pointer.
#[derive(Clone)]
struct Ip {
    /// Valid range for this instruction pointer.
    range: std::ops::Range<*const u8>,
    /// Points to the next instruction.
    ip: *const u8,
}

impl Ip {
    fn new(vec: &[u8]) -> Self {
        Ip {
            range: vec.as_ptr_range(),
            ip: vec.as_ptr(),
        }
    }

    fn goto(&mut self, offset: u16) {
        unsafe {
            self.ip = self.range.start.add(offset as usize);
            debug_assert!(self.range.contains(&self.ip));
        }
    }

    /// Take the next byte in the stream
    fn next(&mut self) -> u8 {
        unsafe {
            debug_assert!(self.range.contains(&self.ip));
            let value = *self.ip;
            self.ip = self.ip.add(1);
            value
        }
    }

    /// Take the next two bytes in the stream as u16.
    fn next2(&mut self) -> u16 {
        u16::from_le_bytes([self.next(), self.next()])
    }
}

/// A function call frame. This represents the state of the current executing
/// function as well as all it's associated constants.
#[derive(Clone)]
struct CallFrame<'brw> {
    ip: Ip,
    consts: &'brw Rt<&'static LispVec>,
    /// The index where this call frame starts on the stack. The interpreter
    /// should not access elements beyond this index.
    start: usize,
}

impl<'brw> CallFrame<'brw> {
    fn new(func: &'brw Rt<&'static ByteFn>, frame_start: usize, cx: &Context) -> CallFrame<'brw> {
        CallFrame {
            ip: Ip::new(func.code().bind(cx).as_bytes()),
            consts: func.consts(),
            start: frame_start,
        }
    }

    fn get_const<'ob>(&self, i: usize, cx: &'ob Context) -> GcObj<'ob> {
        self.consts
            .bind(cx)
            .get(i)
            .expect("constant had invalid index")
            .get()
    }
}

#[derive(Debug, Default)]
#[repr(transparent)]
struct LispStack(Vec<GcObj<'static>>);

impl std::ops::Deref for Rt<LispStack> {
    type Target = Rt<Vec<GcObj<'static>>>;

    fn deref(&self) -> &Self::Target {
        unsafe { &*(self as *const Self).cast::<Self::Target>() }
    }
}

impl DerefMut for Rt<LispStack> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *(self as *mut Self).cast::<Self::Target>() }
    }
}

// To make this simpler we implement indexing from the top of the stack (end of
// the vec) instead of the bottom. This is the convention that all the bytecode
// functions use.
impl Index<u16> for Rt<LispStack> {
    type Output = Rt<GcObj<'static>>;

    fn index(&self, index: u16) -> &Self::Output {
        let index = self.offset_end(index.into());
        let vec: &Vec<Rt<GcObj>> = self;
        Index::index(vec, index)
    }
}

impl IndexMut<u16> for Rt<LispStack> {
    fn index_mut(&mut self, index: u16) -> &mut Self::Output {
        let index = self.offset_end(index.into());
        let vec: &mut Vec<Rt<GcObj>> = self;
        IndexMut::index_mut(vec, index)
    }
}

impl Index<RangeTo<u16>> for Rt<LispStack> {
    type Output = [Rt<GcObj<'static>>];

    fn index(&self, index: RangeTo<u16>) -> &Self::Output {
        debug_assert!((index.end as usize) <= self.len());
        let end = self.len() - (index.end as usize);
        let vec: &Vec<Rt<GcObj>> = self;
        &vec[end..]
    }
}

impl LispStack {
    fn from_root<'brw, 'a, 'b>(
        value: &'brw mut Root<'a, 'b, Vec<GcObj>>,
    ) -> &'brw mut Root<'a, 'b, LispStack> {
        unsafe {
            &mut *((value as *mut Root<'_, '_, Vec<GcObj>>).cast::<Root<'_, '_, LispStack>>())
        }
    }
}

impl Trace for LispStack {
    fn trace(&self, stack: &mut Vec<crate::core::object::RawObj>) {
        self.0.trace(stack);
    }
}

impl Root<'_, '_, LispStack> {
    fn pop<'ob>(&mut self, cx: &'ob Context) -> GcObj<'ob> {
        self.as_mut(cx).deref_mut().pop(cx).unwrap()
    }

    fn top<'ob>(&'ob mut self, cx: &'ob Context) -> &'ob mut Rt<GcObj<'static>> {
        self.as_mut(cx).last_mut().unwrap()
    }
}

impl Rt<LispStack> {
    fn offset_end(&self, i: usize) -> usize {
        debug_assert!(i < self.len());
        self.len() - (i + 1)
    }

    fn push_ref(&mut self, i: impl Into<i32>, cx: &Context) {
        let i: u16 = i.into() as u16;
        let obj = self[i].bind(cx);
        self.push(obj);
    }

    fn set_ref(&mut self, i: impl Into<usize>) {
        let index = self.offset_end(i.into());
        self.swap_remove(index);
    }

    fn fill_extra_args(&mut self, fill_args: u16) {
        for _ in 0..fill_args {
            self.push(nil());
        }
    }

    fn remove_top(&mut self, i: impl Into<usize>) {
        let offset = self.offset_end(i.into());
        self.truncate(offset + 1);
    }
}

#[derive(Debug, Trace)]
struct Handler<'ob> {
    #[no_trace]
    jump_code: u16,
    #[no_trace]
    stack_size: usize,
    condition: GcObj<'ob>,
}

impl<'ob> IntoRoot<Handler<'static>> for Handler<'ob> {
    unsafe fn into_root(self) -> Handler<'static> {
        Handler {
            jump_code: self.jump_code,
            stack_size: self.stack_size,
            condition: self.condition.into_root(),
        }
    }
}

macro_rules! byte_debug {
    ($msg:literal $(,)?  $($args:expr),* $(,)?) => (
        if cfg!(feature = "debug_bytecode") && crate::debug::debug_enabled() {
            println!($msg, $($args),*);
        }
    );
}

/// An execution routine. This holds all the state of the current interpreter,
/// and could be used to support coroutines.
struct Routine<'brw, '_1, '_2, '_3, '_4> {
    stack: &'brw mut Root<'_1, '_2, LispStack>,
    /// Previous call frames
    call_frames: Vec<CallFrame<'brw>>,
    /// The current call frame.
    frame: CallFrame<'brw>,
    handlers: &'brw mut Root<'_3, '_4, Vec<Handler<'static>>>,
}

impl<'brw, 'ob> Routine<'brw, '_, '_, '_, '_> {
    fn varref(&mut self, idx: u16, env: &Root<Env>, cx: &'ob Context) -> Result<()> {
        let symbol = self.frame.get_const(idx as usize, cx);
        if let Object::Symbol(sym) = symbol.get() {
            let Some(var) = env.vars.get(sym) else {bail!("Void Variable: {sym}")};
            self.stack.as_mut(cx).push(var.bind(cx));
            Ok(())
        } else {
            unreachable!("Varref was not a symbol: {:?}", symbol);
        }
    }

    fn varset(&mut self, idx: usize, env: &mut Root<Env>, cx: &Context) -> Result<()> {
        let obj = self.frame.get_const(idx, cx);
        let symbol: &Symbol = obj.try_into()?;
        let value = self.stack.pop(cx);
        crate::data::set(symbol, value, env, cx)?;
        Ok(())
    }

    #[inline(always)]
    fn debug_enabled() -> bool {
        cfg!(feature = "debug_bytecode") && crate::debug::debug_enabled()
    }

    /// Prepare the arguments for lisp function call. This means filling all
    /// needed stack slots with `nil` and moving all the `&rest` arguments into
    /// a list.
    fn prepare_lisp_args(
        &mut self,
        func: &ByteFn,
        arg_cnt: u16,
        name: &str,
        cx: &'ob Context,
    ) -> Result<u16> {
        let fill_args = func.args.num_of_fill_args(arg_cnt, name)?;
        self.stack.as_mut(cx).fill_extra_args(fill_args);
        let total_args = arg_cnt + fill_args;
        let rest_size = total_args - (func.args.required + func.args.optional);
        if rest_size > 0 {
            let slice = &self.stack[..rest_size];
            let list = crate::fns::slice_into_list(Rt::bind_slice(slice, cx), None, cx);
            let stack = self.stack.as_mut(cx);
            stack.remove_top(rest_size - 1);
            stack[0].set(list);
            Ok(total_args - rest_size + 1)
        } else if func.args.rest {
            self.stack.as_mut(cx).push(nil());
            Ok(total_args + 1)
        } else {
            Ok(total_args)
        }
    }

    fn call(
        &mut self,
        arg_cnt: u16,
        env: &mut Root<Env>,
        cx: &'ob mut Context,
    ) -> Result<(), EvalError> {
        let sym = match self.stack[arg_cnt].get(cx) {
            Object::Symbol(x) => x,
            x => unreachable!("Expected symbol for call found {:?}", x),
        };

        let Some(func) = sym.follow_indirect(cx) else {bail_err!("Void Function: {sym}")};
        let slice = &self.stack[..arg_cnt];
        let args = Rt::bind_slice(slice, cx).to_vec();
        let name = sym.name().to_owned();
        root!(args, cx);
        root!(func, cx);
        let result = rebind!(func.call(args, env, cx, Some(&name))?, cx);
        let stack = self.stack.as_mut(cx);
        stack.remove_top(arg_cnt);
        stack[0].set(result);
        cx.garbage_collect(false);
        Ok(())
    }

    fn run(&mut self, env: &mut Root<Env>, cx: &'ob mut Context) -> EvalResult<'ob> {
        'main: loop {
            let err = match self.execute_bytecode(env, cx) {
                Ok(x) => return Ok(rebind!(x, cx)),
                Err(e) => e,
            };

            // we will fix this once we can handle different error types
            #[allow(clippy::never_loop)]
            for handler in self.handlers.as_mut(cx).drain(..) {
                let condition = &handler.condition;
                match condition.get(cx) {
                    Object::Symbol(s) if s == &sym::ERROR => {}
                    Object::Cons(cons) => {
                        for x in cons.elements() {
                            let x = x?;
                            // TODO: Handle different error symbols
                            if x != sym::DEBUG && x != sym::ERROR {
                                bail_err!("non-error conditions {x} not yet supported")
                            }
                        }
                    }
                    _ => bail_err!("Invalid condition handler: {condition}"),
                }

                let error = if let EvalError {
                    error: ErrorType::Signal(id),
                    ..
                } = err
                {
                    let Some((sym, data)) = env.get_exception(id) else {unreachable!("Exception not found")};
                    cons!(sym, data; cx)
                } else {
                    // TODO: Need to remove the anyhow branch once
                    // full errors are implemented
                    cons!(sym::ERROR, format!("{err}"); cx)
                };
                self.stack.as_mut(cx).truncate(handler.stack_size);
                self.stack.as_mut(cx).push(error);
                self.frame.ip.goto(handler.jump_code);
                continue 'main;
            }
            return Err(err);
        }
    }

    #[allow(clippy::too_many_lines)]
    /// The main bytecode execution loop.
    fn execute_bytecode(&mut self, env: &mut Root<Env>, cx: &'ob mut Context) -> EvalResult<'ob> {
        use crate::{alloc, arith, core, data, fns};
        use opcode::OpCode as op;
        loop {
            let op = match self.frame.ip.next().try_into() {
                Ok(x) => x,
                Err(e) => panic!("Invalid Bytecode: {e}"),
            };

            if Self::debug_enabled() {
                println!("[");
                for (idx, x) in self.stack.iter().rev().enumerate() {
                    println!("    {idx}: {x},");
                }
                println!("]");
                let byte_offset = self.frame.ip.ip as i64 - self.frame.ip.range.start as i64 - 1;
                println!("op :{byte_offset}: {op:?}");
            }
            match op {
                op::StackRef0 => self.stack.as_mut(cx).push_ref(0, cx),
                op::StackRef1 => self.stack.as_mut(cx).push_ref(1, cx),
                op::StackRef2 => self.stack.as_mut(cx).push_ref(2, cx),
                op::StackRef3 => self.stack.as_mut(cx).push_ref(3, cx),
                op::StackRef4 => self.stack.as_mut(cx).push_ref(4, cx),
                op::StackRef5 => self.stack.as_mut(cx).push_ref(5, cx),
                op::StackRefN => {
                    let idx = self.frame.ip.next();
                    byte_debug!("  arg: {idx}");
                    self.stack.as_mut(cx).push_ref(idx, cx);
                }
                op::StackRefN2 => {
                    let idx = self.frame.ip.next2();
                    byte_debug!("  arg: {idx}");
                    self.stack.as_mut(cx).push_ref(idx, cx);
                }
                op::StackSetN => {
                    let idx = self.frame.ip.next();
                    byte_debug!("  arg: {idx}");
                    self.stack.as_mut(cx).set_ref(idx);
                }
                op::StackSetN2 => {
                    let idx = self.frame.ip.next2();
                    byte_debug!("  arg: {idx}");
                    self.stack.as_mut(cx).set_ref(idx);
                }
                op::VarRef0 => self.varref(0, env, cx)?,
                op::VarRef1 => self.varref(1, env, cx)?,
                op::VarRef2 => self.varref(2, env, cx)?,
                op::VarRef3 => self.varref(3, env, cx)?,
                op::VarRef4 => self.varref(4, env, cx)?,
                op::VarRef5 => self.varref(5, env, cx)?,
                op::VarRefN => {
                    let idx = self.frame.ip.next();
                    byte_debug!("  arg: {idx}");
                    self.varref(idx.into(), env, cx)?;
                }
                op::VarRefN2 => {
                    let idx = self.frame.ip.next2();
                    byte_debug!("  arg: {idx}");
                    self.varref(idx, env, cx)?;
                }
                op::VarSet0 => self.varset(0, env, cx)?,
                op::VarSet1 => self.varset(1, env, cx)?,
                op::VarSet2 => self.varset(2, env, cx)?,
                op::VarSet3 => self.varset(3, env, cx)?,
                op::VarSet4 => self.varset(4, env, cx)?,
                op::VarSet5 => self.varset(5, env, cx)?,
                op::VarSetN => {
                    let idx = self.frame.ip.next();
                    byte_debug!("  arg: {idx}");
                    self.varset(idx.into(), env, cx)?;
                }
                op::VarSetN2 => {
                    let idx = self.frame.ip.next2();
                    byte_debug!("  arg: {idx}");
                    self.varset(idx.into(), env, cx)?;
                }
                op::Call0 => self.call(0, env, cx)?,
                op::Call1 => self.call(1, env, cx)?,
                op::Call2 => self.call(2, env, cx)?,
                op::Call3 => self.call(3, env, cx)?,
                op::Call4 => self.call(4, env, cx)?,
                op::Call5 => self.call(5, env, cx)?,
                op::CallN => {
                    let idx = self.frame.ip.next();
                    byte_debug!("  arg: {idx}");
                    self.call(idx.into(), env, cx)?;
                }
                op::CallN2 => {
                    let idx = self.frame.ip.next2();
                    byte_debug!("  arg: {idx}");
                    self.call(idx, env, cx)?;
                }
                op::Plus => {
                    let arg1 = self.stack.pop(cx);
                    let top = self.stack.top(cx);
                    let args = &[arg1.try_into()?, top.bind_as(cx)?];
                    let result: GcObj = cx.add(arith::add(args));
                    top.set(result);
                }
                op::LessThan => {
                    let v1 = self.stack.pop(cx);
                    let top = self.stack.top(cx);
                    top.set(arith::less_than(top.bind_as(cx)?, &[v1.try_into()?]));
                }
                op::GreaterThan => {
                    let v1 = self.stack.pop(cx);
                    let top = self.stack.top(cx);
                    top.set(arith::greater_than(top.bind_as(cx)?, &[v1.try_into()?]));
                }
                op::Sub1 => {
                    let top = self.stack.top(cx);
                    top.set::<GcObj>(cx.add(arith::sub_one(top.bind_as(cx)?)));
                }
                op::Add1 => {
                    let top = self.stack.top(cx);
                    top.set::<GcObj>(cx.add(arith::add_one(top.bind_as(cx)?)));
                }
                op::Discard => {
                    self.stack.pop(cx);
                }
                op::DiscardN => {
                    let arg = self.frame.ip.next();
                    let cur_len = self.stack.as_mut(cx).len();
                    let keep_tos = (arg & 0x80) != 0;
                    let count = (arg & 0x7F) as usize;
                    byte_debug!("  keep_tos: {keep_tos}\n  count: {count}");
                    if keep_tos {
                        let top = self.stack.top(cx).bind(cx);
                        self.stack.as_mut(cx).truncate(cur_len - count);
                        self.stack.top(cx).set(top);
                    } else {
                        self.stack.as_mut(cx).truncate(cur_len - count);
                    }
                }
                op::Duplicate => {
                    let top = self.stack[0].bind(cx);
                    self.stack.as_mut(cx).push(top);
                }
                op::ConstantN2 => {
                    let idx = self.frame.ip.next2();
                    byte_debug!("  arg: {idx}");
                    let stack = self.stack.as_mut(cx);
                    stack.push(self.frame.get_const(idx.into(), cx));
                }
                op::Goto => {
                    let offset = self.frame.ip.next2();
                    byte_debug!("  operand: {offset}");
                    self.frame.ip.goto(offset);
                }
                op::GotoIfNil => {
                    let cond = self.stack.pop(cx);
                    let offset = self.frame.ip.next2();
                    byte_debug!("  operand: {offset}");
                    if cond.nil() {
                        self.frame.ip.goto(offset);
                    }
                }
                op::GotoIfNonNil => {
                    let cond = self.stack.pop(cx);
                    let offset = self.frame.ip.next2();
                    byte_debug!("  operand: {offset}");
                    if !cond.nil() {
                        self.frame.ip.goto(offset);
                    }
                }
                op::GotoIfNilElsePop => {
                    let offset = self.frame.ip.next2();
                    byte_debug!("  operand: {offset}");
                    if self.stack[0].bind(cx).nil() {
                        self.frame.ip.goto(offset);
                    } else {
                        self.stack.pop(cx);
                    }
                }
                op::GotoIfNonNilElsePop => {
                    let offset = self.frame.ip.next2();
                    byte_debug!("  operand: {offset}");
                    if self.stack[0].bind(cx).nil() {
                        self.stack.pop(cx);
                    } else {
                        self.frame.ip.goto(offset);
                    }
                }
                op::Return => {
                    if self.call_frames.is_empty() {
                        return Ok(self.stack.pop(cx));
                    }
                    let var = self.stack.pop(cx);
                    let start = self.frame.start;
                    self.stack.as_mut(cx).truncate(start + 1);
                    self.stack.top(cx).set(var);
                    self.frame = self.call_frames.pop().unwrap();
                }
                op::Switch => {
                    let Object::HashTable(table) = self.stack.pop(cx).get() else {unreachable!("switch table was not a hash table")};
                    let cond = self.stack.pop(cx);
                    if let Some(offset) = table.borrow().get(&cond) {
                        let Object::Int(offset) = offset.get().get() else {unreachable!("switch value was not a int")};
                        self.frame.ip.goto(offset as u16);
                    }
                }
                op::PopHandler => {
                    self.handlers.as_mut(cx).pop();
                }
                op::PushCondtionCase => {
                    // pop before getting stack size
                    let condition = self.stack.pop(cx);
                    let handler = Handler {
                        jump_code: self.frame.ip.next2(),
                        stack_size: self.stack.len(),
                        condition,
                    };
                    self.handlers.as_mut(cx).push(handler);
                }
                op::Nth => {
                    let list = self.stack.pop(cx);
                    let top = self.stack.top(cx);
                    top.set(fns::nth(top.bind_as(cx)?, list.try_into()?)?);
                }
                op::Symbolp => {
                    let top = self.stack.top(cx);
                    top.set(data::symbolp(top.bind(cx)));
                }
                op::Consp => {
                    let top = self.stack.top(cx);
                    top.set(data::consp(top.bind(cx)));
                }
                op::Stringp => {
                    let top = self.stack.top(cx);
                    top.set(data::stringp(top.bind(cx)));
                }
                op::Listp => {
                    let top = self.stack.top(cx);
                    top.set(data::listp(top.bind(cx)));
                }
                op::Eq => {
                    let v1 = self.stack.pop(cx);
                    let top = self.stack.top(cx);
                    top.set(fns::eq(top.bind(cx), v1));
                }
                op::Memq => {
                    let list = self.stack.pop(cx);
                    let elt = self.stack.top(cx);
                    elt.set(fns::memq(elt.bind(cx), list.try_into()?)?);
                }
                op::Not => {
                    let top = self.stack.top(cx);
                    top.set(data::null(top.bind(cx)));
                }
                op::Car => {
                    let top = self.stack.top(cx);
                    top.set(core::cons::car(top.bind_as(cx)?));
                }
                op::Cdr => {
                    let top = self.stack.top(cx);
                    top.set(core::cons::cdr(top.bind_as(cx)?));
                }
                op::Cons => {
                    let cdr = self.stack.pop(cx);
                    let car = self.stack.top(cx);
                    car.set(core::cons::cons(car.bind(cx), cdr, cx));
                }
                op::List1 => {
                    let top = self.stack.top(cx);
                    top.set(alloc::list(&[top.bind(cx)], cx));
                }
                op::List2 => {
                    let a2 = self.stack.pop(cx);
                    let top = self.stack.top(cx);
                    top.set(alloc::list(&[top.bind(cx), a2], cx));
                }
                op::List3 => {
                    let a3 = self.stack.pop(cx);
                    let a2 = self.stack.pop(cx);
                    let top = self.stack.top(cx);
                    top.set(alloc::list(&[top.bind(cx), a2, a3], cx));
                }
                op::Length => {
                    let top = self.stack.top(cx);
                    top.set(fns::length(top.bind(cx))?);
                }
                op::Aref => {
                    let idx = self.stack.pop(cx);
                    let top = self.stack.top(cx);
                    top.set(data::aref(top.bind(cx), idx.try_into()?)?);
                }
                op::Aset => {
                    let newlet = self.stack.pop(cx);
                    let idx = self.stack.pop(cx);
                    let top = self.stack.top(cx);
                    top.set(data::aset(top.bind(cx), idx.try_into()?, newlet)?);
                }
                op::SymbolValue => {
                    let top = self.stack.top(cx);
                    top.set(data::symbol_value(top.bind_as(cx)?, env, cx).unwrap_or_default());
                }
                op::SymbolFunction => {
                    let top = self.stack.top(cx);
                    top.set(data::symbol_function(top.bind_as(cx)?, cx));
                }
                op::Set => {
                    let newlet = self.stack.pop(cx);
                    let top = self.stack.top(cx);
                    top.set(data::set(top.bind_as(cx)?, newlet, env, cx)?);
                }
                op::Fset => {
                    let def = self.stack.pop(cx);
                    let top = self.stack.top(cx);
                    top.set::<GcObj>(data::fset(top.bind_as(cx)?, def)?.into());
                }
                op::Get => {
                    let prop = self.stack.pop(cx);
                    let top = self.stack.top(cx);
                    top.set(data::get(top.bind_as(cx)?, prop.try_into()?, env, cx));
                }
                op::Nthcdr => {
                    let list = self.stack.pop(cx);
                    let top = self.stack.top(cx);
                    top.set(fns::nthcdr(top.bind_as(cx)?, list.try_into()?)?.copy_as_obj());
                }
                op::Nreverse => {
                    let elt = self.stack.top(cx);
                    elt.set(fns::nreverse(elt.bind_as(cx)?)?);
                }
                op::Setcar => {
                    let newcar = self.stack.pop(cx);
                    let top = self.stack.top(cx);
                    top.set(core::cons::setcar(top.bind_as(cx)?, newcar)?);
                }
                op::Setcdr => {
                    let newcdr = self.stack.pop(cx);
                    let top = self.stack.top(cx);
                    top.set(core::cons::setcdr(top.bind_as(cx)?, newcdr)?);
                }
                op::CarSafe => {
                    let top = self.stack.top(cx);
                    top.set(core::cons::car_safe(top.bind(cx)));
                }
                op::CdrSafe => {
                    let top = self.stack.top(cx);
                    top.set(core::cons::cdr_safe(top.bind(cx)));
                }
                op::Constant0
                | op::Constant1
                | op::Constant2
                | op::Constant3
                | op::Constant4
                | op::Constant5
                | op::Constant6
                | op::Constant7
                | op::Constant8
                | op::Constant9
                | op::Constant10
                | op::Constant11
                | op::Constant12
                | op::Constant13
                | op::Constant14
                | op::Constant15
                | op::Constant16
                | op::Constant17
                | op::Constant18
                | op::Constant19
                | op::Constant20
                | op::Constant21
                | op::Constant22
                | op::Constant23
                | op::Constant24
                | op::Constant25
                | op::Constant26
                | op::Constant27
                | op::Constant28
                | op::Constant29
                | op::Constant30
                | op::Constant31
                | op::Constant32
                | op::Constant33
                | op::Constant34
                | op::Constant35
                | op::Constant36
                | op::Constant37
                | op::Constant38
                | op::Constant39
                | op::Constant40
                | op::Constant41
                | op::Constant42
                | op::Constant43
                | op::Constant44
                | op::Constant45
                | op::Constant46
                | op::Constant47
                | op::Constant48
                | op::Constant49
                | op::Constant50
                | op::Constant51
                | op::Constant52
                | op::Constant53
                | op::Constant54
                | op::Constant55
                | op::Constant56
                | op::Constant57
                | op::Constant58
                | op::Constant59
                | op::Constant60
                | op::Constant61
                | op::Constant62
                | op::Constant63 => {
                    let idx = (op as u8) - (op::Constant0 as u8);
                    let stack = self.stack.as_mut(cx);
                    stack.push(self.frame.get_const(idx as usize, cx));
                }
                op => {
                    panic!("Unimplemented opcode: {op:?}");
                }
            }
        }
    }
}

#[defun]
fn byte_code<'ob>(
    bytestr: &Rt<Gc<&LispString>>,
    vector: &Rt<Gc<&LispVec>>,
    maxdepth: usize,
    env: &mut Root<Env>,
    cx: &'ob mut Context,
) -> Result<GcObj<'ob>> {
    let fun = crate::alloc::make_byte_code(
        0,
        bytestr.get(cx),
        vector.get(cx),
        maxdepth,
        None,
        None,
        &[],
        cx,
    )?;
    root!(fun, cx);
    root!(args, Vec::new(), cx);
    Ok(call(fun, args, "unnamed", env, cx)?)
}

pub(crate) fn call<'ob>(
    func: &Rt<&'static ByteFn>,
    args: &mut Root<'_, '_, Vec<GcObj<'static>>>,
    name: &str,
    env: &mut Root<Env>,
    cx: &'ob mut Context,
) -> EvalResult<'ob> {
    let arg_cnt = args.len() as u16;
    let stack = LispStack::from_root(args);
    root!(handlers, Vec::new(), cx);
    let mut rout = Routine {
        stack,
        call_frames: vec![],
        frame: CallFrame::new(func, 0, cx),
        handlers,
    };
    rout.prepare_lisp_args(func.bind(cx), arg_cnt, name, cx)?;
    rout.run(env, cx)
}

#[allow(clippy::enum_glob_use)]
#[cfg(test)]
mod test {
    use crate::core::{
        env::sym,
        gc::RootSet,
        object::{HashTable, IntoObject, LispVec},
    };

    use super::{opcode::OpCode, *};

    macro_rules! make_bytecode { (
        $name:ident,
        $arglist:expr,
        [$($opcodes:expr),* $(,)?],
        [$($constants:expr),* $(,)?],
        $cx:expr $(,)?
    ) => (
        let cx: &Context = $cx;
        let constants: &LispVec = {
            let vec: Vec<GcObj> = vec![$($constants.into_obj(cx).into()),*];
            vec.into_obj(cx).get()
        };
        let opcodes = {
            #[allow(trivial_numeric_casts)]
            let opcodes = vec![$($opcodes as u8),*];
            println!("Test seq: {opcodes:?}");
            opcodes.into_obj(cx).get()
        };
        let bytecode = crate::alloc::make_byte_code($arglist, &opcodes, constants, 0, None, None, &[], cx).unwrap();
        root!(bytecode, cx);
        let $name = bytecode;

        )
    }

    macro_rules! check_bytecode { (
        $bytecode:expr,
        [$($args:expr),* $(,)?],
        $expect:expr,
        $cx:expr $(,)?
    ) => ({
            let bytecode: &Rt<&ByteFn> = $bytecode;
            let cx: &mut Context = $cx;

            let args: Vec<GcObj> = { vec![$($args.into_obj(cx).into()),*] };
            let expect: GcObj = $expect.into_obj(cx).into();

            root!(args, cx);
            root!(expect, cx);

            check_bytecode_internal(
                args,
                bytecode,
                expect,
                cx
            );
        })
    }

    fn check_bytecode_internal<'ob>(
        args: &mut Root<Vec<GcObj<'static>>>,
        bytecode: &Rt<&'static ByteFn>,
        expect: &Rt<GcObj>,
        cx: &'ob mut Context,
    ) {
        root!(env, Env::default(), cx);
        let val = rebind!(call(bytecode, args, "test", env, cx).unwrap(), cx);
        let expect = expect.bind(cx);
        assert_eq!(val, expect);
    }

    #[test]
    fn test_basic() {
        use OpCode::*;
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        // (lambda () 5)
        make_bytecode!(bytecode, 0, [Constant0, Return], [5], cx);
        check_bytecode!(bytecode, [], 5, cx);
        // (lambda (x) (+ x 5))
        make_bytecode!(bytecode, 257, [Duplicate, Constant0, Plus, Return], [5], cx);
        check_bytecode!(bytecode, [7], 12, cx);
        // (lambda (x &optional y) (+ x y))
        make_bytecode!(bytecode, 513, [StackRef1, StackRef1, Plus, Return], [], cx);
        check_bytecode!(bytecode, [3, 4], 7, cx);
        // (lambda (x) (if x 2 3))
        make_bytecode!(
            bytecode,
            257,
            [Duplicate, GotoIfNil, 0x06, 0x00, Constant0, Return, Constant1, Return],
            [2, 3],
            cx
        );
        check_bytecode!(bytecode, [false], 3, cx);
        check_bytecode!(bytecode, [true], 2, cx);

        // (lambda (x) (let ((y 0))
        //          (while (< 0 x)
        //            (setq x (1- x))
        //            (setq y (1+ y)))
        //          y))
        make_bytecode!(
            bytecode,
            257,
            [
                Constant0, Constant0, StackRef2, LessThan, GotoIfNil, VarSet2, 0x00, StackRef1,
                Sub1, StackSetN, 0x02, Duplicate, Add1, StackSetN, 0x01, Goto, 0x01, 0x00, Return
            ],
            [0],
            cx
        );
        check_bytecode!(bytecode, [5], 5, cx);
        check_bytecode!(bytecode, [0], 0, cx);
    }

    #[test]
    fn test_bytecode_call() {
        use OpCode::*;
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        lazy_static::initialize(&crate::core::env::INTERNED_SYMBOLS);
        // (lambda (x) (symbol-name x))
        make_bytecode!(
            bytecode,
            257,
            [Constant0, StackRef1, Call1, Return],
            [sym::SYMBOL_NAME],
            cx
        );
        check_bytecode!(bytecode, [sym::SYMBOL_NAME], "symbol-name", cx);
        check_bytecode!(bytecode, [sym::AREF], "aref", cx);
        check_bytecode!(bytecode, [sym::ADD], "+", cx);

        // (lambda (x y z) (+ x y z))
        make_bytecode!(
            bytecode,
            771,
            [Constant0, StackRef3, StackRef3, StackRef3, Call3, Return],
            [sym::ADD],
            cx
        );
        check_bytecode!(bytecode, [1, 2, 3], 6, cx);

        // (lambda (&rest x) (apply '+ x))
        make_bytecode!(
            bytecode,
            128,
            [Constant0, Constant1, StackRef2, Call2, Return],
            [sym::APPLY, sym::ADD],
            cx
        );
        check_bytecode!(bytecode, [1, 2, 3], 6, cx);

        // (lambda (x &optional y) (+ x y))
        make_bytecode!(bytecode, 513, [StackRef1, StackRef1, Plus, Return], [], cx);
        check_bytecode!(bytecode, [1, 2], 3, cx);
    }

    #[test]
    fn test_bytecode_advanced() {
        use OpCode::*;
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);

        let mut table = HashTable::default();
        table.insert(1.into(), 6.into());
        table.insert(2.into(), 8.into());
        table.insert(3.into(), 10.into());

        // (lambda (n)
        //   (cond ((equal n 1) 1)
        //         ((equal n 2) 2)
        //         ((equal n 3) 3)))
        make_bytecode!(
            bytecode,
            257,
            [
                Duplicate, Constant0, Switch, Goto, 0x0C, 0x00, Constant1, Return, Constant2,
                Return, Constant3, Return, Constant4, Return
            ],
            [table, 4, 5, 6, false],
            cx
        );
        check_bytecode!(bytecode, [1], 4, cx);
        check_bytecode!(bytecode, [2], 5, cx);
        check_bytecode!(bytecode, [3], 6, cx);
        check_bytecode!(bytecode, [4], false, cx);

        // (1+ (let ((a 1) (_b) (_c)) a))
        make_bytecode!(
            bytecode,
            0,
            [Constant0, Constant1, Duplicate, StackRef2, DiscardN, 131, Add1, Return],
            [1, false],
            cx
        );
        check_bytecode!(bytecode, [], 2, cx);
    }

    #[test]
    fn test_handlers() {
        use OpCode::*;
        lazy_static::initialize(&crate::core::env::INTERNED_SYMBOLS);

        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        let err = cons!(sym::ERROR; cx);

        // (lambda (y) (condition-case nil
        //            (floor)
        //              (error (+ y 4))))
        make_bytecode!(
            bytecode,
            257,
            [
                Constant0,
                PushCondtionCase,
                0x09,
                0x0,
                Constant1,
                StackRef1,
                Call1,
                PopHandler,
                Return,
                Discard,
                Duplicate,
                Constant2,
                Plus,
                Return
            ],
            [err, sym::SYMBOL_NAME, 4],
            cx
        );
        check_bytecode!(bytecode, [3], 7, cx);
        check_bytecode!(bytecode, [sym::FLOOR], "floor", cx);
    }
}

define_symbols!(
    FUNCS => {
        byte_code,
    }
);
