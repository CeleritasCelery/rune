//! The main bytecode interpeter.

use std::ops::{DerefMut, Index, IndexMut, RangeTo};

use anyhow::{anyhow, Result};

use crate::core::env::{Env, Symbol};
use crate::core::gc::{Context, Root, Rt, Trace};
use crate::core::object::{nil, Expression, GcObj, IntoObject, LispFn, Object};
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
    code: &'brw Rt<Expression>,
    /// The index where this call frame starts on the stack. The interpreter
    /// should not access elements beyond this index.
    start: usize,
}

impl<'brw> CallFrame<'brw> {
    fn new(func: &'brw Rt<Expression>, frame_start: usize) -> CallFrame<'brw> {
        CallFrame {
            ip: Ip::new(func.op_codes.get()),
            code: func,
            start: frame_start,
        }
    }

    fn get_const<'ob>(&self, i: usize, cx: &'ob Context) -> GcObj<'ob> {
        self.code
            .constants
            .get(i)
            .expect("constant had invalid index")
            .bind(cx)
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
        let end = self.offset_end(index.end as usize - 1);
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
    fn mark(&self, stack: &mut Vec<crate::core::object::RawObj>) {
        self.0.mark(stack);
    }
}

impl Root<'_, '_, LispStack> {
    fn pop<'ob>(&mut self, cx: &'ob Context) -> GcObj<'ob> {
        self.as_mut(cx).deref_mut().pop(cx).unwrap()
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

/// An execution routine. This holds all the state of the current interpreter,
/// and could be used to support coroutines.
pub(crate) struct Routine<'brw, '_1, '_2> {
    stack: &'brw mut Root<'_1, '_2, LispStack>,
    call_frames: Vec<CallFrame<'brw>>,
    /// The current call frame.
    frame: CallFrame<'brw>,
}

impl<'brw, 'ob> Routine<'brw, '_, '_> {
    fn varref(&mut self, idx: u16, env: &Root<Env>, cx: &'ob Context) -> Result<()> {
        let symbol = self.frame.get_const(idx as usize, cx);
        if let Object::Symbol(sym) = symbol.get() {
            let var = env.vars.get(sym).ok_or(anyhow!("Void Variable: {sym}"))?;
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

    /// Prepare the arguments for lisp function call. This means filling all
    /// needed stack slots with `nil` and moving all the `&rest` arguments into
    /// a list.
    fn prepare_lisp_args(
        &mut self,
        func: &LispFn,
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

    fn call(&mut self, arg_cnt: u16, env: &mut Root<Env>, cx: &'ob mut Context) -> Result<()> {
        let sym = match self.stack[arg_cnt].get(cx) {
            Object::Symbol(x) => x,
            x => unreachable!("Expected symbol for call found {:?}", x),
        };

        match sym.follow_indirect(cx) {
            Some(func) => {
                let slice = &self.stack[..arg_cnt];
                let args = Rt::bind_slice(slice, cx).to_vec();
                let name = sym.name().to_owned();
                root!(args, cx);
                root!(func, cx);
                let result = rebind!(func.call(args, env, cx, Some(&name))?, cx);
                let stack = self.stack.as_mut(cx);
                stack.remove_top(arg_cnt);
                stack[0].set(result);
                println!("stack {:?}", stack);
                Ok(())
            }
            None => Err(anyhow!("Void Function: {sym}")),
        }
    }

    #[allow(clippy::too_many_lines)]
    /// The main bytecode execution loop.
    pub(crate) fn run(&mut self, env: &mut Root<Env>, cx: &'ob mut Context) -> Result<GcObj<'ob>> {
        use opcode::OpCode as op;
        let init_stack_size = self.stack.len();
        loop {
            let op = self.frame.ip.next().try_into()?;
            #[cfg(feature = "debug_bytecode")]
            {
                println!("[");
                for (idx, x) in self.stack.iter().enumerate() {
                    println!("    {}: {:?},", idx, x);
                }
                println!("]");
                let byte_offset = self.frame.ip.ip as i64 - self.frame.ip.range.start as i64 - 1;
                println!("op :{}: {:?}", byte_offset, op);
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
                    self.stack.as_mut(cx).push_ref(idx, cx);
                }
                op::StackRefN2 => {
                    let idx = self.frame.ip.next2();
                    self.stack.as_mut(cx).push_ref(idx, cx);
                }
                op::StackSetN => {
                    let idx = self.frame.ip.next();
                    self.stack.as_mut(cx).set_ref(idx);
                }
                op::StackSetN2 => {
                    let idx = self.frame.ip.next2();
                    self.stack.as_mut(cx).set_ref(idx);
                }
                op::Constant0 => self.stack.as_mut(cx).push(self.frame.get_const(0, cx)),
                op::Constant1 => self.stack.as_mut(cx).push(self.frame.get_const(1, cx)),
                op::Constant2 => self.stack.as_mut(cx).push(self.frame.get_const(2, cx)),
                op::Constant3 => self.stack.as_mut(cx).push(self.frame.get_const(3, cx)),
                op::Constant4 => self.stack.as_mut(cx).push(self.frame.get_const(4, cx)),
                op::Constant5 => self.stack.as_mut(cx).push(self.frame.get_const(5, cx)),
                op::VarRef0 => self.varref(0, env, cx)?,
                op::VarRef1 => self.varref(1, env, cx)?,
                op::VarRef2 => self.varref(2, env, cx)?,
                op::VarRef3 => self.varref(3, env, cx)?,
                op::VarRef4 => self.varref(4, env, cx)?,
                op::VarRef5 => self.varref(5, env, cx)?,
                op::VarRefN => {
                    let idx = self.frame.ip.next();
                    self.varref(idx.into(), env, cx)?;
                }
                op::VarRefN2 => {
                    let idx = self.frame.ip.next2();
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
                    self.varset(idx.into(), env, cx)?;
                }
                op::VarSetN2 => {
                    let idx = self.frame.ip.next2();
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
                    self.call(idx.into(), env, cx)?;
                }
                op::CallN2 => {
                    let idx = self.frame.ip.next2();
                    self.call(idx, env, cx)?;
                }
                op::Plus => {
                    let args = {
                        let arg1 = self.stack.pop(cx);
                        let arg2 = self.stack.last().unwrap();
                        &[arg1.try_into()?, arg2.bind(cx).try_into()?]
                    };
                    let result: GcObj = crate::arith::add(args).into_obj(cx).into();
                    self.stack.as_mut(cx).last_mut().unwrap().set(result);
                }
                op::Discard => {
                    self.stack.pop(cx);
                }
                op::DiscardN => {
                    let num: usize = self.frame.ip.next().into();
                    let cur_len = self.stack.as_mut(cx).len();
                    self.stack.as_mut(cx).truncate(cur_len - num);
                }
                op::Duplicate => {
                    let value = self.stack.last().unwrap().bind(cx);
                    self.stack.as_mut(cx).push(value);
                }
                op::Goto => {
                    let offset = self.frame.ip.next2();
                    self.frame.ip.goto(offset);
                }
                op::GotoIfNil => {
                    let cond = self.stack.pop(cx);
                    let offset = self.frame.ip.next2();
                    println!("offset = {offset}");
                    if cond.nil() {
                        self.frame.ip.goto(offset);
                    }
                }
                op::GotoIfNonNil => {
                    let cond = self.stack.pop(cx);
                    let offset = self.frame.ip.next2();
                    if !cond.nil() {
                        self.frame.ip.goto(offset);
                    }
                }
                op::GotoIfNilElsePop => {
                    let cond = self.stack.as_mut(cx).last().unwrap();
                    let offset = self.frame.ip.next2();
                    if cond.nil() {
                        self.frame.ip.goto(offset);
                    } else {
                        self.stack.pop(cx);
                    }
                }
                op::GotoIfNonNilElsePop => {
                    let cond = self.stack.last().unwrap();
                    let offset = self.frame.ip.next2();
                    if cond.nil() {
                        self.stack.pop(cx);
                    } else {
                        self.frame.ip.goto(offset);
                    }
                }
                op::Return => {
                    if self.call_frames.is_empty() {
                        debug_assert_eq!(self.stack.len(), init_stack_size + 1);
                        return Ok(self.stack.pop(cx));
                    }
                    let var = self.stack.pop(cx);
                    self.stack.as_mut(cx)[self.frame.start as u16].set(var);
                    self.stack.as_mut(cx).truncate(self.frame.start + 1);
                    self.frame = self.call_frames.pop().unwrap();
                }
                op => {
                    panic!("Unimplemented opcode: {:?}", op);
                }
            }
        }
    }
}

pub(crate) fn call<'ob>(
    func: &Rt<&'static LispFn>,
    args: &mut Root<'_, '_, Vec<GcObj<'static>>>,
    env: &mut Root<Env>,
    cx: &'ob mut Context,
) -> Result<GcObj<'ob>> {
    let arg_cnt = args.len() as u16;
    let stack = LispStack::from_root(args);
    let mut rout = Routine {
        stack,
        call_frames: vec![],
        frame: CallFrame::new(func.body(), 0),
    };
    rout.prepare_lisp_args(func.bind(cx), arg_cnt, "unnamed", cx)?;
    rout.run(env, cx)
}

#[cfg(test)]
mod test {
    use std::cell::RefCell;

    use crate::core::{
        env::sym,
        gc::RootSet,
        object::{IntoObject, LispVec},
    };

    use super::{opcode::OpCode, *};

    macro_rules! check_bytecode { (
        $arglist:expr,
        [$($args:expr),* $(,)?],
        [$($opcodes:expr),* $(,)?],
        [$($constants:expr),* $(,)?],
        $expect:expr,
        $cx:expr $(,)?
    ) => {
        let cx: &mut Context = $cx;

        let bytecode = {
            let constants: &LispVec = {
                let vec: Vec<GcObj> = vec![$($constants.into_obj(cx).into()),*];
                let obj = cx.add(vec);
                obj.try_into().unwrap()
            };
            let opcodes = {
                #[allow(trivial_numeric_casts)]
                let opcodes = vec![$($opcodes as u8),*];
                println!("Test seq: {:?}", opcodes);
                RefCell::new(opcodes)
            };
            let bytecode = crate::alloc::make_byte_code($arglist, &opcodes, constants, 0, None, None, &[]);
            bytecode.into_obj(cx).get()
        };
        let args: Vec<GcObj> = { vec![$($args.into_obj(cx).into()),*] };
        let expect: GcObj = $expect.into_obj(cx).into();

        root!(args, cx);
        root!(bytecode, cx);
        root!(expect, cx);

        check_bytecode_internal(
            args,
            bytecode,
            expect,
            cx
        );
    }
    }

    fn check_bytecode_internal<'ob>(
        args: &mut Root<Vec<GcObj<'static>>>,
        bytecode: &Rt<&'static LispFn>,
        expect: &Rt<GcObj>,
        cx: &'ob mut Context,
    ) {
        root!(env, Env::default(), cx);
        let val = rebind!(call(bytecode, args, env, cx).unwrap(), cx);
        let expect = expect.bind(cx);
        assert_eq!(val, expect);
    }

    #[test]
    fn test_basic() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        // (lambda () 5)
        check_bytecode!(0, [], [OpCode::Constant0, OpCode::Return], [5], 5, cx);
        // (lambda (x) (+ x 5))
        check_bytecode!(
            257,
            [7],
            [
                OpCode::Duplicate,
                OpCode::Constant0,
                OpCode::Plus,
                OpCode::Return,
            ],
            [5],
            12,
            cx,
        );
        // (lambda (x &optional y) (+ x y))
        check_bytecode!(
            513,
            [3, 4],
            [
                OpCode::StackRef1,
                OpCode::StackRef1,
                OpCode::Plus,
                OpCode::Return,
            ],
            [],
            7,
            cx,
        );
        // (lambda (x) (if x 2 3))
        check_bytecode!(
            257,
            [false],
            [
                OpCode::Duplicate,
                OpCode::GotoIfNil,
                0x06,
                0x00,
                OpCode::Constant0,
                OpCode::Return,
                OpCode::Constant1,
                OpCode::Return
            ],
            [2, 3],
            3,
            cx
        );
    }

    #[test]
    fn test_call() {
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        lazy_static::initialize(&crate::core::env::INTERNED_SYMBOLS);
        // (lambda (x) (symbol-name x))
        check_bytecode!(
            257,
            [sym::SYMBOL_NAME],
            [
                OpCode::Constant0,
                OpCode::StackRef1,
                OpCode::Call1,
                OpCode::Return
            ],
            [sym::SYMBOL_NAME],
            "symbol-name",
            cx
        );

        // (lambda (x y z) (+ x y z))
        check_bytecode!(
            771,
            [1, 2, 3],
            [
                OpCode::Constant0,
                OpCode::StackRef3,
                OpCode::StackRef3,
                OpCode::StackRef3,
                OpCode::Call3,
                OpCode::Return
            ],
            [sym::ADD],
            6,
            cx
        );

        // (lambda (&rest x) (apply '+ x))
        check_bytecode!(
            128,
            [1, 2, 3],
            [
                OpCode::Constant0,
                OpCode::Constant1,
                OpCode::StackRef2,
                OpCode::Call2,
                OpCode::Return
            ],
            [sym::APPLY, sym::ADD],
            6,
            cx
        );

        // (lambda (x &optional y) (+ x y))
        check_bytecode!(
            513,
            [1, 2],
            [
                OpCode::StackRef1,
                OpCode::StackRef1,
                OpCode::Plus,
                OpCode::Return
            ],
            [],
            3,
            cx
        );
    }
}
