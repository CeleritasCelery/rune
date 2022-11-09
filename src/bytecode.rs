//! The main bytecode interpeter.

use std::ops::{DerefMut, Index, IndexMut, RangeTo};

use anyhow::{bail, Result};

use crate::core::env::{Env, Symbol};
use crate::core::gc::{Context, Root, Rt, Trace};
use crate::core::object::{nil, ByteFn, Expression, GcObj, IntoObject, Object};
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
            ip: Ip::new(&func.op_codes.0),
            code: func,
            start: frame_start,
        }
    }

    fn get_const(&self, i: usize) -> &Rt<GcObj> {
        self.code
            .constants
            .get(i)
            .expect("constant had invalid index")
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
    fn trace(&self, stack: &mut Vec<crate::core::object::RawObj>) {
        self.0.trace(stack);
    }
}

impl Root<'_, '_, LispStack> {
    fn pop<'ob>(&mut self, cx: &'ob Context) -> GcObj<'ob> {
        self.as_mut(cx).deref_mut().pop(cx).unwrap()
    }

    fn top<'ob>(&self, cx: &'ob Context) -> GcObj<'ob> {
        self.last().unwrap().bind(cx)
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
        let symbol = self.frame.get_const(idx as usize);
        if let Object::Symbol(sym) = symbol.get(cx) {
            let Some(var) = env.vars.get(sym) else {bail!("Void Variable: {sym}")};
            self.stack.as_mut(cx).push(var.bind(cx));
            Ok(())
        } else {
            unreachable!("Varref was not a symbol: {:?}", symbol);
        }
    }

    fn varset(&mut self, idx: usize, env: &mut Root<Env>, cx: &Context) -> Result<()> {
        let obj = self.frame.get_const(idx);
        let symbol: &Symbol = obj.bind(cx).try_into()?;
        let value = self.stack.pop(cx);
        crate::data::set(symbol, value, env, cx)?;
        Ok(())
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

    fn call(&mut self, arg_cnt: u16, env: &mut Root<Env>, cx: &'ob mut Context) -> Result<()> {
        let sym = match self.stack[arg_cnt].get(cx) {
            Object::Symbol(x) => x,
            x => unreachable!("Expected symbol for call found {:?}", x),
        };

        let Some(func) = sym.follow_indirect(cx) else {bail!("Void Function: {sym}")};
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
                    println!("    {idx}: {x:?},");
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
                op::Constant0 => self.stack.as_mut(cx).push(self.frame.get_const(0)),
                op::Constant1 => self.stack.as_mut(cx).push(self.frame.get_const(1)),
                op::Constant2 => self.stack.as_mut(cx).push(self.frame.get_const(2)),
                op::Constant3 => self.stack.as_mut(cx).push(self.frame.get_const(3)),
                op::Constant4 => self.stack.as_mut(cx).push(self.frame.get_const(4)),
                op::Constant5 => self.stack.as_mut(cx).push(self.frame.get_const(5)),
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
                        let arg2 = self.stack.top(cx);
                        &[arg1.try_into()?, arg2.try_into()?]
                    };
                    let result: GcObj = crate::arith::add(args).into_obj(cx).into();
                    self.stack.as_mut(cx)[0].set(result);
                }
                op::LessThan => {
                    let v2 = self.stack.pop(cx);
                    let v1 = self.stack.top(cx);
                    let result: GcObj =
                        crate::arith::less_than(v1.try_into()?, &[v2.try_into()?]).into();
                    self.stack.as_mut(cx)[0].set(result);
                }
                op::GreaterThan => {
                    let v2 = self.stack.pop(cx);
                    let v1 = self.stack.top(cx);
                    let result: GcObj =
                        crate::arith::greater_than(v1.try_into()?, &[v2.try_into()?]).into();
                    self.stack.as_mut(cx)[0].set(result);
                }
                op::Sub1 => {
                    let arg = self.stack.top(cx);
                    let result: GcObj = cx.add(crate::arith::sub_one(arg.try_into()?));
                    self.stack.as_mut(cx)[0].set(result);
                }
                op::Add1 => {
                    let arg = self.stack.top(cx);
                    let result: GcObj = cx.add(crate::arith::add_one(arg.try_into()?));
                    self.stack.as_mut(cx)[0].set(result);
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
                    let value = self.stack.top(cx);
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
                    let offset = self.frame.ip.next2();
                    if self.stack.top(cx).nil() {
                        self.frame.ip.goto(offset);
                    } else {
                        self.stack.pop(cx);
                    }
                }
                op::GotoIfNonNilElsePop => {
                    let offset = self.frame.ip.next2();
                    if self.stack.top(cx).nil() {
                        self.stack.pop(cx);
                    } else {
                        self.frame.ip.goto(offset);
                    }
                }
                op::Switch => {
                    let Object::HashTable(table) = self.stack.pop(cx).get() else {unreachable!("switch table was not a hash table")};
                    let cond = self.stack.pop(cx);
                    if let Some(offset) = table.borrow().get(&cond) {
                        let Object::Int(offset) = offset.get() else {unreachable!("switch value was not a int")};
                        self.frame.ip.goto(offset as u16);
                    }
                }
                op::Return => {
                    if self.call_frames.is_empty() {
                        debug_assert_eq!(self.stack.len(), init_stack_size + 1);
                        return Ok(self.stack.pop(cx));
                    }
                    let var = self.stack.pop(cx);
                    let start = self.frame.start;
                    self.stack.as_mut(cx).truncate(start + 1);
                    self.stack.as_mut(cx)[0].set(var);
                    self.frame = self.call_frames.pop().unwrap();
                }
                op => {
                    panic!("Unimplemented opcode: {op:?}");
                }
            }
        }
    }
}

pub(crate) fn call<'ob>(
    func: &Rt<&'static ByteFn>,
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

#[allow(clippy::enum_glob_use)]
#[cfg(test)]
mod test {
    use crate::core::object::LispString;

    use crate::core::{
        env::sym,
        gc::RootSet,
        object::{HashTable, IntoObject, LispVec},
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
                LispString::from(opcodes)
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
        bytecode: &Rt<&'static ByteFn>,
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
        use OpCode::*;
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        // (lambda () 5)
        check_bytecode!(0, [], [Constant0, Return], [5], 5, cx);
        // (lambda (x) (+ x 5))
        check_bytecode!(257, [7], [Duplicate, Constant0, Plus, Return,], [5], 12, cx,);
        // (lambda (x &optional y) (+ x y))
        check_bytecode!(513, [3, 4], [StackRef1, StackRef1, Plus, Return], [], 7, cx,);
        // (lambda (x) (if x 2 3))
        check_bytecode!(
            257,
            [false],
            [Duplicate, GotoIfNil, 0x06, 0x00, Constant0, Return, Constant1, Return],
            [2, 3],
            3,
            cx
        );

        // (lambda (x) (let ((y 0))
        //          (while (< 0 x)
        //            (setq x (1- x))
        //            (setq y (1+ y)))
        //          y))
        check_bytecode!(
            257,
            [5],
            [
                Constant0, Constant0, StackRef2, LessThan, GotoIfNil, VarSet2, 0x00, StackRef1,
                Sub1, StackSetN, 0x02, Duplicate, Add1, StackSetN, 0x01, Goto, 0x01, 0x00, Return
            ],
            [0],
            5,
            cx
        );
    }

    #[test]
    fn test_bytecode_call() {
        use OpCode::*;
        let roots = &RootSet::default();
        let cx = &mut Context::new(roots);
        lazy_static::initialize(&crate::core::env::INTERNED_SYMBOLS);
        // (lambda (x) (symbol-name x))
        check_bytecode!(
            257,
            [sym::SYMBOL_NAME],
            [Constant0, StackRef1, Call1, Return],
            [sym::SYMBOL_NAME],
            "symbol-name",
            cx
        );

        // (lambda (x y z) (+ x y z))
        check_bytecode!(
            771,
            [1, 2, 3],
            [Constant0, StackRef3, StackRef3, StackRef3, Call3, Return],
            [sym::ADD],
            6,
            cx
        );

        // (lambda (&rest x) (apply '+ x))
        check_bytecode!(
            128,
            [1, 2, 3],
            [Constant0, Constant1, StackRef2, Call2, Return],
            [sym::APPLY, sym::ADD],
            6,
            cx
        );

        // (lambda (x &optional y) (+ x y))
        check_bytecode!(513, [1, 2], [StackRef1, StackRef1, Plus, Return], [], 3, cx);
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
        check_bytecode!(
            257,
            [2],
            [
                Duplicate, Constant0, Switch, Goto, 0x0C, 0x00, Constant1, Return, Constant2,
                Return, Constant3, Return, Constant4, Return
            ],
            [table, 1, 2, 3, false],
            2,
            cx
        );
    }
}
