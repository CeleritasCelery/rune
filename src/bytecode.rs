//! The main bytecode interpeter.
use crate::core::env::{sym, Env, Symbol};
use crate::core::error::{ErrorType, EvalError, EvalResult};
use crate::core::gc::{Context, Rt};
use crate::core::object::{nil, ByteFn, Gc, GcObj, LispString, LispVec, Object, WithLifetime};
use anyhow::{bail, Result};
use bstr::ByteSlice;
use rune_core::macros::{bail_err, cons, rebind, root};
use rune_macros::{defun, Trace};

mod opcode;

/// An program counter. This is implemented as a bound checked range pointer.
#[derive(Clone)]
struct ProgramCounter {
    /// Valid range for this instruction pointer.
    range: std::ops::Range<*const u8>,
    /// Points to the next instruction.
    pc: *const u8,
}

impl ProgramCounter {
    fn new(vec: &[u8]) -> Self {
        ProgramCounter { range: vec.as_ptr_range(), pc: vec.as_ptr() }
    }

    fn goto(&mut self, offset: u16) {
        unsafe {
            self.pc = self.range.start.add(offset as usize);
            debug_assert!(self.range.contains(&self.pc));
        }
    }

    /// Take the next byte in the stream
    fn next(&mut self) -> u8 {
        unsafe {
            debug_assert!(self.range.contains(&self.pc));
            let value = *self.pc;
            self.pc = self.pc.add(1);
            value
        }
    }

    fn arg1(&mut self) -> u16 {
        unsafe {
            debug_assert!(self.range.contains(&self.pc));
            let value = *self.pc;
            self.pc = self.pc.add(1);
            if cfg!(feature = "debug_bytecode") && crate::debug::debug_enabled() {
                println!("  arg: {value}");
            }
            value.into()
        }
    }

    fn arg2(&mut self) -> u16 {
        unsafe {
            debug_assert!(self.range.contains(&self.pc.add(1)));
            let value = u16::from_le(self.pc.cast::<u16>().read_unaligned());
            self.pc = self.pc.add(2);
            if cfg!(feature = "debug_bytecode") && crate::debug::debug_enabled() {
                println!("  arg: {value}");
            }
            value
        }
    }
}

/// A function call frame. This represents the state of the current executing
/// function as well as all it's associated constants.
#[derive(Clone)]
struct CallFrame<'brw> {
    pc: ProgramCounter,
    consts: &'brw Rt<&'static LispVec>,
    /// The index where this call frame starts on the stack. The interpreter
    /// should not access elements beyond this index.
    start: usize,
}

impl<'brw> CallFrame<'brw> {
    fn new(func: &'brw Rt<&'static ByteFn>, frame_start: usize, cx: &Context) -> CallFrame<'brw> {
        CallFrame {
            pc: ProgramCounter::new(func.code().bind(cx).as_bytes()),
            consts: func.consts(),
            start: frame_start,
        }
    }

    fn get_const<'ob>(&self, i: usize, cx: &'ob Context) -> GcObj<'ob> {
        self.consts.bind(cx).get(i).expect("constant had invalid index").get()
    }
}

#[derive(Debug, Trace)]
/// A handler for a condition-case. These are stored in a vector in the VM and
/// added/removed via bytecodes.
struct Handler<'ob> {
    #[no_trace]
    jump_code: u16,
    #[no_trace]
    stack_size: usize,
    condition: GcObj<'ob>,
}

impl<'old, 'new> WithLifetime<'new> for Handler<'old> {
    type Out = Handler<'new>;

    unsafe fn with_lifetime(self) -> Self::Out {
        std::mem::transmute::<Handler<'old>, Handler<'new>>(self)
    }
}

/// The bytecode VM. This hold all the current call frames and handlers. The
/// execution stack is part of the Environment.
struct VM<'brw> {
    /// Previous call frames
    call_frames: Vec<CallFrame<'brw>>,
    /// The current call frame.
    frame: CallFrame<'brw>,
    /// All currently active condition-case handlers
    handlers: &'brw mut Rt<Vec<Handler<'static>>>,
    /// The runtime environment
    env: &'brw mut Rt<Env>,
}

impl<'brw, 'ob> VM<'brw> {
    fn varref(&mut self, idx: u16, cx: &'ob Context) -> Result<()> {
        let symbol = self.frame.get_const(idx as usize, cx);
        if let Object::Symbol(sym) = symbol.untag() {
            let Some(var) = self.env.vars.get(sym) else { bail!("Void Variable: {sym}") };
            let var = var.bind(cx);
            self.env.stack.push(var);
            Ok(())
        } else {
            unreachable!("Varref was not a symbol: {:?}", symbol);
        }
    }

    fn varset(&mut self, idx: usize, cx: &Context) -> Result<()> {
        let obj = self.frame.get_const(idx, cx);
        let symbol: Symbol = obj.try_into()?;
        let value = self.env.stack.pop(cx);
        crate::data::set(symbol, value, self.env)?;
        Ok(())
    }

    fn varbind(&mut self, idx: u16, cx: &'ob Context) {
        let value = self.env.stack.pop(cx);
        let symbol = self.frame.get_const(idx as usize, cx);
        let Object::Symbol(sym) = symbol.untag() else {
            unreachable!("Varbind was not a symbol: {:?}", symbol)
        };
        self.env.varbind(sym, value, cx);
    }

    fn unbind(&mut self, idx: u16, cx: &'ob Context) {
        self.env.unbind(idx, cx);
    }

    #[inline(always)]
    fn debug_enabled() -> bool {
        cfg!(feature = "debug_bytecode") && crate::debug::debug_enabled()
    }

    /// Prepare ethe arguments for lisp function call. This means filling all
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
        self.env.stack.fill_extra_args(fill_args);
        let total_args = arg_cnt + fill_args;
        let rest_size = total_args - (func.args.required + func.args.optional);
        if rest_size > 0 {
            let slice = &self.env.stack[..rest_size as usize];
            let list = crate::fns::slice_into_list(Rt::bind_slice(slice, cx), None, cx);
            self.env.stack.remove_top(rest_size - 1);
            self.env.stack[0].set(list);
            Ok(total_args - rest_size + 1)
        } else if func.args.rest {
            self.env.stack.push(nil());
            Ok(total_args + 1)
        } else {
            Ok(total_args)
        }
    }

    fn call(&mut self, arg_cnt: u16, cx: &'ob mut Context) -> Result<(), EvalError> {
        let arg_cnt = arg_cnt as usize;
        let sym = match self.env.stack[arg_cnt].get(cx) {
            Object::Symbol(x) => x,
            x => unreachable!("Expected symbol for call found {:?}", x),
        };

        let Some(func) = sym.follow_indirect(cx) else { bail_err!("Void Function: {sym}") };
        let slice = &self.env.stack[..arg_cnt];
        let args = Rt::bind_slice(slice, cx).to_vec();
        let name = sym.name().to_owned();
        root!(args, cx);
        root!(func, cx);
        let result = func.call(args, Some(&name), self.env, cx)?;
        self.env.stack.remove_top(arg_cnt);
        self.env.stack[0].set(result);
        cx.garbage_collect(false);
        Ok(())
    }

    fn run(&mut self, cx: &'ob mut Context) -> EvalResult<'ob> {
        'main: loop {
            let err = match self.execute_bytecode(cx) {
                Ok(x) => return Ok(rebind!(x, cx)),
                Err(e) => e,
            };

            // we will fix this once we can handle different error types
            #[allow(clippy::never_loop)]
            while let Some(handler) = self.handlers.bind_mut(cx).pop() {
                match handler.condition.untag() {
                    Object::Symbol(sym::ERROR) => {}
                    Object::Cons(conditions) => {
                        for condition in conditions {
                            let condition = condition?;
                            // TODO: Handle different error symbols
                            if condition != sym::DEBUG && condition != sym::ERROR {
                                bail_err!("non-error conditions {condition} not yet supported")
                            }
                        }
                    }
                    x => bail_err!("Invalid condition handler: {x}"),
                }

                let error = if let EvalError { error: ErrorType::Signal(id), .. } = err {
                    let Some((sym, data)) = self.env.get_exception(id) else {
                        unreachable!("Exception not found")
                    };
                    cons!(sym, data; cx)
                } else {
                    // TODO: Need to remove the anyhow branch once
                    // full errors are implemented
                    cons!(sym::ERROR, format!("{err}"); cx)
                };
                self.env.stack.truncate(handler.stack_size);
                self.env.stack.push(error);
                self.frame.pc.goto(handler.jump_code);
                continue 'main;
            }
            return Err(err);
        }
    }

    #[allow(clippy::too_many_lines)]
    /// The main bytecode execution loop.
    fn execute_bytecode(&mut self, cx: &'ob mut Context) -> EvalResult<'ob> {
        use crate::{alloc, arith, data, fns};
        use opcode::OpCode as op;
        loop {
            let op = match self.frame.pc.next().try_into() {
                Ok(x) => x,
                Err(e) => panic!("Invalid Bytecode: {e}"),
            };

            if Self::debug_enabled() {
                println!("[");
                for (idx, x) in self.env.stack.iter().rev().enumerate() {
                    println!("    {idx}: {x},");
                }
                println!("]");
                let byte_offset = self.frame.pc.pc as i64 - self.frame.pc.range.start as i64 - 1;
                println!("op :{byte_offset}: {op:?}");
            }
            match op {
                op::StackRef0 => self.env.stack.push_ref(0, cx),
                op::StackRef1 => self.env.stack.push_ref(1, cx),
                op::StackRef2 => self.env.stack.push_ref(2, cx),
                op::StackRef3 => self.env.stack.push_ref(3, cx),
                op::StackRef4 => self.env.stack.push_ref(4, cx),
                op::StackRef5 => self.env.stack.push_ref(5, cx),
                op::StackRefN => {
                    let idx = self.frame.pc.arg1();
                    self.env.stack.push_ref(idx, cx);
                }
                op::StackRefN2 => {
                    let idx = self.frame.pc.arg2();
                    self.env.stack.push_ref(idx, cx);
                }
                op::StackSetN => {
                    let idx = self.frame.pc.arg1();
                    self.env.stack.set_ref(idx);
                }
                op::StackSetN2 => {
                    let idx = self.frame.pc.arg2();
                    self.env.stack.set_ref(idx);
                }
                op::VarRef0 => self.varref(0, cx)?,
                op::VarRef1 => self.varref(1, cx)?,
                op::VarRef2 => self.varref(2, cx)?,
                op::VarRef3 => self.varref(3, cx)?,
                op::VarRef4 => self.varref(4, cx)?,
                op::VarRef5 => self.varref(5, cx)?,
                op::VarRefN => {
                    let idx = self.frame.pc.arg1();
                    self.varref(idx, cx)?;
                }
                op::VarRefN2 => {
                    let idx = self.frame.pc.arg2();
                    self.varref(idx, cx)?;
                }
                op::VarSet0 => self.varset(0, cx)?,
                op::VarSet1 => self.varset(1, cx)?,
                op::VarSet2 => self.varset(2, cx)?,
                op::VarSet3 => self.varset(3, cx)?,
                op::VarSet4 => self.varset(4, cx)?,
                op::VarSet5 => self.varset(5, cx)?,
                op::VarSetN => {
                    let idx = self.frame.pc.arg1();
                    self.varset(idx.into(), cx)?;
                }
                op::VarSetN2 => {
                    let idx = self.frame.pc.arg2();
                    self.varset(idx.into(), cx)?;
                }
                op::VarBind0 => self.varbind(0, cx),
                op::VarBind1 => self.varbind(1, cx),
                op::VarBind2 => self.varbind(2, cx),
                op::VarBind3 => self.varbind(3, cx),
                op::VarBind4 => self.varbind(4, cx),
                op::VarBind5 => self.varbind(5, cx),
                op::VarBindN => {
                    let idx = self.frame.pc.arg1();
                    self.varbind(idx, cx);
                }
                op::VarBindN2 => {
                    let idx = self.frame.pc.arg2();
                    self.varbind(idx, cx);
                }
                op::Call0 => self.call(0, cx)?,
                op::Call1 => self.call(1, cx)?,
                op::Call2 => self.call(2, cx)?,
                op::Call3 => self.call(3, cx)?,
                op::Call4 => self.call(4, cx)?,
                op::Call5 => self.call(5, cx)?,
                op::CallN => {
                    let idx = self.frame.pc.arg1();
                    self.call(idx, cx)?;
                }
                op::CallN2 => {
                    let idx = self.frame.pc.arg2();
                    self.call(idx, cx)?;
                }
                op::Unbind0 => self.unbind(0, cx),
                op::Unbind1 => self.unbind(1, cx),
                op::Unbind2 => self.unbind(2, cx),
                op::Unbind3 => self.unbind(3, cx),
                op::Unbind4 => self.unbind(4, cx),
                op::Unbind5 => self.unbind(5, cx),
                op::UnbindN => {
                    let idx = self.frame.pc.arg1();
                    self.unbind(idx, cx);
                }
                op::UnbindN2 => {
                    let idx = self.frame.pc.arg2();
                    self.unbind(idx, cx);
                }
                op::PopHandler => {
                    self.handlers.pop();
                }
                op::PushCondtionCase => {
                    // pop before getting stack size
                    let condition = self.env.stack.pop(cx);
                    let handler = Handler {
                        jump_code: self.frame.pc.arg2(),
                        stack_size: self.env.stack.len(),
                        condition,
                    };
                    self.handlers.push(handler);
                }
                op::PushCatch => todo!("PushCatch bytecode"),
                op::Nth => {
                    let list = self.env.stack.pop(cx);
                    let top = self.env.stack.top();
                    top.set(fns::nth(top.bind_as(cx)?, list.try_into()?)?);
                }
                op::Symbolp => {
                    let top = self.env.stack.top();
                    top.set(data::symbolp(top.bind(cx)));
                }
                op::Consp => {
                    let top = self.env.stack.top();
                    top.set(data::consp(top.bind(cx)));
                }
                op::Stringp => {
                    let top = self.env.stack.top();
                    top.set(data::stringp(top.bind(cx)));
                }
                op::Listp => {
                    let top = self.env.stack.top();
                    top.set(data::listp(top.bind(cx)));
                }
                op::Eq => {
                    let v1 = self.env.stack.pop(cx);
                    let top = self.env.stack.top();
                    top.set(fns::eq(top.bind(cx), v1));
                }
                op::Memq => {
                    let list = self.env.stack.pop(cx);
                    let elt = self.env.stack.top();
                    elt.set(fns::memq(elt.bind(cx), list.try_into()?)?);
                }
                op::Not => {
                    let top = self.env.stack.top();
                    top.set(data::null(top.bind(cx)));
                }
                op::Car => {
                    let top = self.env.stack.top();
                    top.set(data::car(top.bind_as(cx)?));
                }
                op::Cdr => {
                    let top = self.env.stack.top();
                    top.set(data::cdr(top.bind_as(cx)?));
                }
                op::Cons => {
                    let cdr = self.env.stack.pop(cx);
                    let car = self.env.stack.top();
                    car.set(data::cons(car.bind(cx), cdr, cx));
                }
                op::List1 => {
                    let top = self.env.stack.top();
                    top.set(alloc::list(&[top.bind(cx)], cx));
                }
                op::List2 => {
                    let a2 = self.env.stack.pop(cx);
                    let top = self.env.stack.top();
                    top.set(alloc::list(&[top.bind(cx), a2], cx));
                }
                op::List3 => {
                    let a3 = self.env.stack.pop(cx);
                    let a2 = self.env.stack.pop(cx);
                    let top = self.env.stack.top();
                    top.set(alloc::list(&[top.bind(cx), a2, a3], cx));
                }
                op::List4 => {
                    let a4 = self.env.stack.pop(cx);
                    let a3 = self.env.stack.pop(cx);
                    let a2 = self.env.stack.pop(cx);
                    let top = self.env.stack.top();
                    top.set(alloc::list(&[top.bind(cx), a2, a3, a4], cx));
                }
                op::Length => {
                    let top = self.env.stack.top();
                    top.set(fns::length(top.bind(cx))? as i64);
                }
                op::Aref => {
                    let idx = self.env.stack.pop(cx);
                    let top = self.env.stack.top();
                    top.set(data::aref(top.bind(cx), idx.try_into()?)?);
                }
                op::Aset => {
                    let newlet = self.env.stack.pop(cx);
                    let idx = self.env.stack.pop(cx);
                    let top = self.env.stack.top();
                    top.set(data::aset(top.bind(cx), idx.try_into()?, newlet)?);
                }
                op::SymbolValue => {
                    let top = self.env.stack.top().bind_as(cx)?;
                    let value = data::symbol_value(top, self.env, cx).unwrap_or_default();
                    self.env.stack.top().set(value);
                }
                op::SymbolFunction => {
                    let top = self.env.stack.top();
                    top.set(data::symbol_function(top.bind_as(cx)?, cx));
                }
                op::Set => {
                    let newlet = self.env.stack.pop(cx);
                    let top = self.env.stack.top().bind_as(cx)?;
                    let value = data::set(top, newlet, self.env)?;
                    self.env.stack.top().set(value);
                }
                op::Fset => {
                    let def = self.env.stack.pop(cx);
                    let top = self.env.stack.top();
                    top.set::<GcObj>(data::fset(top.bind_as(cx)?, def)?.into());
                }
                op::Get => {
                    let prop = self.env.stack.pop(cx).try_into()?;
                    let top = self.env.stack.top().bind_as(cx)?;
                    let value = data::get(top, prop, self.env, cx);
                    self.env.stack.top().set(value);
                }
                op::Substring => todo!("Substring bytecode"),
                op::Concat2 => todo!("Concat2 bytecode"),
                op::Concat3 => todo!("Concat3 bytecode"),
                op::Concat4 => todo!("Concat4 bytecode"),
                op::Sub1 => {
                    let top = self.env.stack.top();
                    top.set(cx.add(arith::sub_one(top.bind_as(cx)?)));
                }
                op::Add1 => {
                    let top = self.env.stack.top();
                    top.set(cx.add(arith::add_one(top.bind_as(cx)?)));
                }
                op::EqlSign => {
                    let rhs = self.env.stack.pop(cx);
                    let top = self.env.stack.top();
                    top.set::<GcObj>(arith::num_eq(top.bind_as(cx)?, &[rhs.try_into()?]).into());
                }
                op::GreaterThan => {
                    let v1 = self.env.stack.pop(cx);
                    let top = self.env.stack.top();
                    top.set(arith::greater_than(top.bind_as(cx)?, &[v1.try_into()?]));
                }
                op::LessThan => {
                    let v1 = self.env.stack.pop(cx);
                    let top = self.env.stack.top();
                    top.set(arith::less_than(top.bind_as(cx)?, &[v1.try_into()?]));
                }
                op::LessThanOrEqual => {
                    let v1 = self.env.stack.pop(cx);
                    let top = self.env.stack.top();
                    top.set(arith::less_than_or_eq(top.bind_as(cx)?, &[v1.try_into()?]));
                }
                op::GreaterThanOrEqual => {
                    let v1 = &[self.env.stack.pop(cx).try_into()?];
                    let top = self.env.stack.top();
                    top.set(arith::greater_than_or_eq(top.bind_as(cx)?, v1));
                }
                op::Diff => todo!("Diff bytecode"),
                op::Negate => {
                    let top = self.env.stack.top();
                    top.set(cx.add(arith::sub(top.bind_as(cx)?, &[])));
                }
                op::Plus => {
                    let arg1 = self.env.stack.pop(cx);
                    let top = self.env.stack.top();
                    let args = &[top.bind_as(cx)?, arg1.try_into()?];
                    top.set(cx.add(arith::add(args)));
                }
                op::Max => {
                    let arg1 = self.env.stack.pop(cx);
                    let top = self.env.stack.top();
                    let args = &[arg1.try_into()?];
                    top.set(cx.add(arith::max(top.bind_as(cx)?, args)));
                }
                op::Min => {
                    let arg1 = self.env.stack.pop(cx);
                    let top = self.env.stack.top();
                    let args = &[arg1.try_into()?];
                    top.set(cx.add(arith::min(top.bind_as(cx)?, args)));
                }
                op::Multiply => {
                    let arg1 = self.env.stack.pop(cx);
                    let top = self.env.stack.top();
                    let args = &[top.bind_as(cx)?, arg1.try_into()?];
                    top.set(cx.add(arith::mul(args)));
                }
                op::Point => todo!("Point bytecode"),
                op::GotoChar => todo!("GotoChar bytecode"),
                op::Insert => todo!("Insert bytecode"),
                op::PointMax => todo!("PointMax bytecode"),
                op::PointMin => todo!("PointMin bytecode"),
                op::CharAfter => todo!("CharAfter bytecode"),
                op::FollowingChar => todo!("FollowingChar bytecode"),
                op::PrecedingChar => todo!("PrecedingChar bytecode"),
                op::CurrentColumn => todo!("CurrentColumn bytecode"),
                op::IndentTo => todo!("IndentTo bytecode"),
                op::EndOfLineP => todo!("EndOfLineP bytecode"),
                op::EndOfBufferP => todo!("EndOfBufferP bytecode"),
                op::BeginningOfLineP => todo!("BeginningOfLineP bytecode"),
                op::BeginningOfBufferP => todo!("BeginningOfBufferP bytecode"),
                op::CurrentBuffer => todo!("CurrentBuffer bytecode"),
                op::SetBuffer => todo!("SetBuffer bytecode"),
                op::SaveCurrentBuffer1 => todo!("SaveCurrentBuffer1 bytecode"),
                op::ForwardChar => todo!("ForwardChar bytecode"),
                op::ForwardWord => todo!("ForwardWord bytecode"),
                op::SkipCharsForward => todo!("SkipCharsForward bytecode"),
                op::SkipCharsBackward => todo!("SkipCharsBackward bytecode"),
                op::ForwardLine => todo!("ForwardLine bytecode"),
                op::CharSyntax => todo!("CharSyntax bytecode"),
                op::BufferSubstring => todo!("BufferSubstring bytecode"),
                op::DeleteRegion => todo!("DeleteRegion bytecode"),
                op::NarrowToRegion => todo!("NarrowToRegion bytecode"),
                op::Widen => todo!("Widen bytecode"),
                op::EndOfLine => todo!("EndOfLine bytecode"),
                op::ConstantN2 => {
                    let idx = self.frame.pc.arg2();
                    self.env.stack.push(self.frame.get_const(idx.into(), cx));
                }
                op::Goto => {
                    let offset = self.frame.pc.arg2();
                    self.frame.pc.goto(offset);
                }
                op::GotoIfNil => {
                    let cond = self.env.stack.pop(cx);
                    let offset = self.frame.pc.arg2();
                    if cond.is_nil() {
                        self.frame.pc.goto(offset);
                    }
                }
                op::GotoIfNonNil => {
                    let cond = self.env.stack.pop(cx);
                    let offset = self.frame.pc.arg2();
                    if !cond.is_nil() {
                        self.frame.pc.goto(offset);
                    }
                }
                op::GotoIfNilElsePop => {
                    let offset = self.frame.pc.arg2();
                    if self.env.stack[0].bind(cx).is_nil() {
                        self.frame.pc.goto(offset);
                    } else {
                        self.env.stack.pop(cx);
                    }
                }
                op::GotoIfNonNilElsePop => {
                    let offset = self.frame.pc.arg2();
                    if self.env.stack[0].bind(cx).is_nil() {
                        self.env.stack.pop(cx);
                    } else {
                        self.frame.pc.goto(offset);
                    }
                }
                op::Return => {
                    let Some(frame) = self.call_frames.pop() else {
                        return Ok(self.env.stack.pop(cx));
                    };
                    let start = self.frame.start;
                    let var = self.env.stack.pop(cx);
                    self.env.stack.truncate(start + 1);
                    self.env.stack.top().set(var);
                    self.frame = frame;
                }
                op::Discard => {
                    self.env.stack.pop(cx);
                }
                op::DiscardN => {
                    let arg = self.frame.pc.arg1();
                    let cur_len = self.env.stack.len();
                    let keep_tos = (arg & 0x80) != 0;
                    let count = (arg & 0x7F) as usize;
                    if keep_tos {
                        let top = self.env.stack.top().bind(cx);
                        self.env.stack.truncate(cur_len - count);
                        self.env.stack.top().set(top);
                    } else {
                        self.env.stack.truncate(cur_len - count);
                    }
                }
                op::Duplicate => {
                    let top = self.env.stack[0].bind(cx);
                    self.env.stack.push(top);
                }
                op::SaveExcursion => todo!("SaveExcursion bytecode"),
                op::SaveRestriction => todo!("SaveRestriction bytecode"),
                op::UnwindProtect => todo!("UnwindProtect bytecode"),
                op::SetMarker => todo!("SetMarker bytecode"),
                op::MatchBeginning => todo!("MatchBeginning bytecode"),
                op::MatchEnd => todo!("MatchEnd bytecode"),
                op::Upcase => todo!("Upcase bytecode"),
                op::Downcase => todo!("Downcase bytecode"),
                op::StringEqlSign => todo!("StringEqlSign bytecode"),
                op::StringLessThan => todo!("StringLessThan bytecode"),
                op::Equal => {
                    let rhs = self.env.stack.pop(cx);
                    let top = self.env.stack.top();
                    top.set(fns::equal(top.bind(cx), rhs));
                }
                op::Nthcdr => {
                    let list = self.env.stack.pop(cx);
                    let top = self.env.stack.top();
                    top.set(fns::nthcdr(top.bind_as(cx)?, list.try_into()?)?.copy_as_obj(cx));
                }
                op::Elt => {
                    let n = self.env.stack.pop(cx);
                    let top = self.env.stack.top();
                    top.set(fns::elt(top.bind(cx), n.try_into()?)?);
                }
                op::Member => {
                    let list = self.env.stack.pop(cx);
                    let top = self.env.stack.top();
                    top.set(fns::member(top.bind(cx), list.try_into()?)?);
                }
                op::Assq => {
                    let alist = self.env.stack.pop(cx);
                    let top = self.env.stack.top();
                    top.set(fns::assq(top.bind(cx), alist.try_into()?)?);
                }
                op::Nreverse => {
                    let elt = self.env.stack.top();
                    elt.set(fns::nreverse(elt.bind_as(cx)?)?);
                }
                op::Setcar => {
                    let newcar = self.env.stack.pop(cx);
                    let top = self.env.stack.top();
                    top.set(data::setcar(top.bind_as(cx)?, newcar)?);
                }
                op::Setcdr => {
                    let newcdr = self.env.stack.pop(cx);
                    let top = self.env.stack.top();
                    top.set(data::setcdr(top.bind_as(cx)?, newcdr)?);
                }
                op::CarSafe => {
                    let top = self.env.stack.top();
                    top.set(data::car_safe(top.bind(cx)));
                }
                op::CdrSafe => {
                    let top = self.env.stack.top();
                    top.set(data::cdr_safe(top.bind(cx)));
                }
                op::Nconc => {
                    let list2 = self.env.stack.pop(cx);
                    let top = self.env.stack.top();
                    top.set(fns::nconc(&[top.bind_as(cx)?, list2.try_into()?])?);
                }
                op::Quo => todo!("Quo bytecode"),
                op::Rem => todo!("Rem bytecode"),
                op::Numberp => {
                    let top = self.env.stack.top();
                    top.set(data::numberp(top.bind(cx)));
                }
                op::Integerp => {
                    let top = self.env.stack.top();
                    top.set(data::integerp(top.bind(cx)));
                }
                op::ListN => {
                    let size = self.frame.pc.arg1() as usize;
                    let slice = Rt::bind_slice(&self.env.stack[..size], cx);
                    let list = alloc::list(slice, cx);
                    let len = self.env.stack.len();
                    self.env.stack.truncate(len - (size - 1));
                    self.env.stack.top().set(list);
                }
                op::ConcatN => todo!("ConcatN bytecode"),
                op::InsertN => todo!("InsertN bytecode"),
                op::Switch => {
                    let Object::HashTable(table) = self.env.stack.pop(cx).untag() else {
                        unreachable!("switch table was not a hash table")
                    };
                    let cond = self.env.stack.pop(cx);
                    if let Some(offset) = table.borrow().get(&cond) {
                        let Object::Int(offset) = offset.get().untag() else {
                            unreachable!("switch value was not a int")
                        };
                        self.frame.pc.goto(offset as u16);
                    }
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
                    self.env.stack.push(self.frame.get_const(idx as usize, cx));
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
    env: &mut Rt<Env>,
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

#[defun]
fn fetch_bytecode(_object: GcObj) {
    // TODO: Implement
}

pub(crate) fn call<'ob>(
    func: &Rt<&'static ByteFn>,
    args: &mut Rt<Vec<GcObj<'static>>>,
    name: &str,
    env: &mut Rt<Env>,
    cx: &'ob mut Context,
) -> EvalResult<'ob> {
    let arg_cnt = args.len() as u16;
    let len = env.stack.len();
    for arg in args.bind_ref(cx).iter() {
        env.stack.push(*arg);
    }
    root!(handlers, Vec::new(), cx);
    let mut vm = VM { call_frames: vec![], frame: CallFrame::new(func, len, cx), env, handlers };
    vm.prepare_lisp_args(func.bind(cx), arg_cnt, name, cx)?;
    let res = vm.run(cx).map_err(|e| e.add_trace(name, args));
    res
}

#[allow(clippy::enum_glob_use)]
#[cfg(test)]
mod test {
    use crate::core::{
        env::sym,
        gc::RootSet,
        object::{HashTable, IntoObject, LispVec},
    };
    use rune_core::macros::{cons, list, rebind, root};

    use super::{opcode::OpCode, *};

    macro_rules! make_bytecode { (
        $name:ident,
        $arglist:expr,
        [$($opcodes:expr),* $(,)?],
        [$($constants:expr),* $(,)?],
        $cx:expr $(,)?
    ) => (
        // https://github.com/rust-lang/rust-analyzer/issues/11681
        let cx1: &Context = $cx;
        let constants: &LispVec = {
            let vec: Vec<GcObj> = vec![$(cx1.add($constants)),*];
            vec.into_obj(cx1).untag()
        };
        let opcodes = {
            #[allow(trivial_numeric_casts)]
            let opcodes = vec![$($opcodes as u8),*];
            println!("Test seq: {opcodes:?}");
            opcodes.into_obj(cx1).untag()
        };
        let bytecode = crate::alloc::make_byte_code($arglist, &opcodes, constants, 0, None, None, &[], cx1).unwrap();
        root!(bytecode, cx1);
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

            let args: Vec<GcObj> = { vec![$(cx.add($args)),*] };
            let expect = cx.add($expect);

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

    fn check_bytecode_internal(
        args: &mut Rt<Vec<GcObj<'static>>>,
        bytecode: &Rt<&'static ByteFn>,
        expect: &Rt<GcObj>,
        cx: &mut Context,
    ) {
        root!(env, Env::default(), cx);
        env.stack.push_frame(0);
        let val = rebind!(call(bytecode, args, "test", env, cx).unwrap());
        let expect = expect.bind(cx);
        env.stack.pop_frame();
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
        sym::init_symbols();
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
    fn test_bytecode_variables() {
        use OpCode::*;
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        sym::init_symbols();

        // (lambda () (let ((load-path 5)) load-path))
        make_bytecode!(
            bytecode,
            0,
            [Constant1, VarBind0, VarRef0, Unbind1, Return],
            [sym::LOAD_PATH, 5],
            cx
        );
        check_bytecode!(bytecode, [], 5, cx);
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
            [Constant0, Constant1, Duplicate, StackRef2, DiscardN, 0x83, Add1, Return],
            [1, false],
            cx
        );
        check_bytecode!(bytecode, [], 2, cx);

        // (lambda () (list 1 2 3 4 5 6))
        make_bytecode!(
            bytecode,
            0,
            [Constant0, Constant1, Constant2, Constant3, Constant4, Constant5, ListN, 6, Return],
            [1, 2, 3, 4, 5, 6],
            cx
        );
        let list = list![1, 2, 3, 4, 5, 6; cx];
        root!(list, cx);
        check_bytecode!(bytecode, [], list, cx);

        // hand rolled bytecode
        // (lambda () (list 1 2 3 4 5 6) 7)
        make_bytecode!(
            bytecode,
            0,
            [
                Constant6, Constant0, Constant1, Constant2, Constant3, Constant4, Constant5, ListN,
                6, DiscardN, 1, Return
            ],
            [1, 2, 3, 4, 5, 6, 7],
            cx
        );
        check_bytecode!(bytecode, [], 7, cx);
    }

    #[test]
    fn test_handlers() {
        use OpCode::*;

        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        sym::init_symbols();
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
