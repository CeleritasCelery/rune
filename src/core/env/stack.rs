use crate::core::{
    gc::{Context, IntoRoot, Rt, Rto, Slot},
    object::{ByteFn, NIL, Object, WithLifetime},
};
use rune_macros::Trace;
use std::ops::{Deref, DerefMut, Index, IndexMut, RangeBounds, RangeTo};

/// The stack of lisp objects used to pass and store arguments in the bytecode
/// VM and interpreter. The top of the stack is index 0 and all indexing
/// functions operate from top to bottom. The stack is partitioned into frames.
/// Each frame represents a function call and it's arguments. The API is
/// designed so that code cannot access elements outside of their frame (doing
/// so results in a panic). Frames are added and removed with
/// [push_frame](RootedLispStack::push_frame) and
/// [pop_frame](RootedLispStack::pop_frame) respectively.
#[derive(Debug, Default, Trace)]
pub(crate) struct LispStack<'a> {
    vec: Vec<Slot<Object<'a>>>,
    #[no_trace]
    current: Frame,
    frames: Vec<FrameStore<'a>>,
}

/// A function call frame. These mirror the lisp call stack and are used to
/// display backtraces as well as return.
#[derive(Debug, Clone, Copy)]
struct Frame {
    /// The start of the call frame, as a index from the bottom of the stack
    /// (not the top).
    start: usize,
    /// The maximum size this stack frame can grow.
    end: usize,
    /// Number of arguments in this call frame. The first is the count and the
    /// second is boolean indicating if the last argument is a cons with the
    /// remaining variadic arguments.
    arg_cnt: (u16, bool),
}

impl Default for Frame {
    fn default() -> Self {
        Self { start: 0, end: usize::MAX, arg_cnt: (0, false) }
    }
}

#[derive(Debug, Clone, Trace)]
struct ByteFrame<'a> {
    func: Slot<&'a ByteFn>,
    #[no_trace]
    pc_offset: usize,
}

#[derive(Debug, Clone, Trace)]
struct FrameStore<'a> {
    #[no_trace]
    frame: Frame,
    bytecode: Option<ByteFrame<'a>>,
}

impl<'new> IntoRoot<FrameStore<'new>> for FrameStore<'_> {
    unsafe fn into_root(self) -> FrameStore<'new> {
        self.with_lifetime()
    }
}

impl<'ob> FrameStore<'ob> {
    fn new(frame: Frame) -> Self {
        Self { frame, bytecode: None }
    }

    fn new_bytecode(frame: Frame, func: &'ob ByteFn, pc_offset: usize) -> Self {
        Self { frame, bytecode: Some(ByteFrame { func: Slot::new(func), pc_offset }) }
    }
}

impl<'old, 'new> WithLifetime<'new> for FrameStore<'old> {
    type Out = FrameStore<'new>;

    unsafe fn with_lifetime(self) -> Self::Out {
        std::mem::transmute::<FrameStore<'old>, FrameStore<'new>>(self)
    }
}

/// Type representing a slice of arguments on the stack. Used to avoid
/// allocations and copies when calling functions.
#[derive(Copy, Clone)]
pub(crate) struct ArgSlice(usize);

impl ArgSlice {
    pub(crate) fn new(size: usize) -> Self {
        Self(size)
    }

    pub(crate) fn len(&self) -> usize {
        self.0
    }
}

// To make this simpler we implement indexing from the top of the stack (end of
// the vec) instead of the bottom. This is the convention that all the bytecode
// functions use.
impl<'a> Index<usize> for RootedLispStack<'a> {
    type Output = Rto<Object<'a>>;

    fn index(&self, index: usize) -> &Self::Output {
        let index = self.offset_end(index);
        &self.vec[index]
    }
}

// This impl is specifically for the Stack. It takes the index from the end of
// the vector instead of the start. This matches how the lisp stack behaves.
impl IndexMut<usize> for RootedLispStack<'_> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        let index = self.offset_end(index);
        &mut self.vec[index]
    }
}

// This impl is specifically for the Stack. It takes the range from the end of
// the vector instead of the start. This matches how the lisp stack behaves.
impl<'a> Index<RangeTo<usize>> for RootedLispStack<'a> {
    type Output = [Rto<Object<'a>>];

    fn index(&self, index: RangeTo<usize>) -> &Self::Output {
        assert!(index.end <= self.len());
        let end = self.len() - index.end;
        let vec: &[Rto<Object>] = &self.vec;
        &vec[end..]
    }
}

impl<'a> RootedLispStack<'a> {
    pub(crate) fn push_bytecode_frame(
        &mut self,
        start: usize,
        depth: usize,
        func: &ByteFn,
        pc: usize,
    ) {
        assert!(start <= self.len());
        assert!(self.current.start <= start);
        self.frames.push(FrameStore::new_bytecode(self.current, func, pc));
        let end = start + depth;
        // allocate space so that we don't have to reallocate later. This will
        // also let us do unchecked pushes later.
        if end > self.vec.capacity() {
            assert!(end - self.vec.len() < 100); // make sure this doesn't blow up
            self.vec.reserve(end - self.vec.len());
        }
        self.current = Frame { start, end, ..Frame::default() };
    }

    pub(crate) fn push_frame(&mut self, arg_cnt: usize) {
        assert!(arg_cnt <= self.len());
        let start = self.len() - arg_cnt;
        assert!(self.current.start <= start);
        self.frames.push(FrameStore::new(self.current));
        self.current =
            Frame { start, arg_cnt: (u16::try_from(arg_cnt).unwrap(), false), ..Frame::default() };
    }

    /// Remove all the stack variables in the current frame and switch to the
    /// previous one
    pub(crate) fn pop_frame(&mut self) {
        self.vec.truncate(self.current.start);
        self.current = self.frames.last().unwrap().frame;
        self.frames.pop();
    }

    pub(crate) fn get_bytecode_frame(&self, idx: usize) -> Option<(&Rto<&'a ByteFn>, usize)> {
        let frame = self.frames.get(idx)?;
        let bytecode = frame.bytecode.as_ref()?;
        let func = &bytecode.func;
        let pc = bytecode.pc_offset;
        Some((func, pc))
    }

    pub(crate) fn prev_bytecode_frame(&self) -> Option<(&Rto<&'a ByteFn>, usize)> {
        self.get_bytecode_frame(self.frames.len() - 1)
    }

    pub(crate) fn current_frame(&self) -> usize {
        self.frames.len()
    }

    pub(crate) fn unwind_frames(&mut self, frame: usize) {
        if frame == self.current_frame() {
            return; /* no frames to unwind */
        }
        assert!(frame < self.current_frame());
        self.current = self.frames[frame].frame;
        self.frames.truncate(frame);
    }

    pub(crate) fn len(&self) -> usize {
        self.vec.len()
    }

    pub(crate) fn set_depth(&mut self, depth: usize) {
        let end = self.current.start + depth;
        self.current.end = end;

        if end > self.vec.capacity() {
            assert!(end - self.vec.len() < 1000); // make sure this doesn't blow up if we have a bug
            self.vec.reserve(end - self.vec.len());
        }
    }

    pub(crate) fn set_arg_count(&mut self, arg_cnt: u16, rest: bool) {
        self.current.arg_cnt = (arg_cnt, rest);
    }

    pub(crate) fn push<T: IntoRoot<Slot<Object<'a>>>>(&mut self, value: T) {
        if self.len() >= self.current.end {
            panic!(
                "overflowed max depth - len was {}, but limit was {}",
                self.len(),
                self.current.end
            );
        }
        // could use https://github.com/rust-lang/rust/issues/100486
        self.vec.push(value);
    }

    pub(crate) fn pop<'ob>(&mut self, cx: &'ob Context) -> Object<'ob> {
        assert!(self.len() > self.current.start);
        *self.vec.bind_mut(cx).pop().unwrap()
    }

    pub(crate) fn top(&mut self) -> &mut Rto<Object<'a>> {
        assert!(self.len() > self.current.start);
        self.vec.last_mut().unwrap()
    }

    pub(crate) fn offset_end(&self, i: usize) -> usize {
        assert!(i < self.len());
        let from_end = self.len() - (i + 1);
        assert!(self.current.start <= from_end);
        from_end
    }

    pub(crate) fn push_ref(&mut self, i: impl Into<i32>, cx: &Context) {
        let obj = self[i.into() as usize].bind(cx);
        self.push(obj);
    }

    pub(crate) fn set_ref(&mut self, i: impl Into<usize>) {
        let index = self.offset_end(i.into());
        self.vec.swap_remove(index);
    }

    pub(crate) fn fill_extra_args(&mut self, fill_args: u16) {
        for _ in 0..fill_args {
            self.push(NIL);
        }
    }

    pub(crate) fn remove_top(&mut self, i: usize) {
        if i == 0 {
            return;
        }
        let offset = self.offset_end(i - 1);
        self.truncate(offset);
    }

    pub(crate) fn truncate(&mut self, len: usize) {
        self.vec.truncate(len);
    }

    pub(crate) fn extend_from_slice(&mut self, src: &[Object]) {
        self.vec.extend_from_slice(src);
    }

    // This indexing is backwards from the normal stack sematics, so we add
    // "as_vec" to the name to hopefully make it clearer
    pub(crate) fn extend_as_vec_from_within(&mut self, src: impl RangeBounds<usize>) {
        self.vec.extend_from_within(src);
    }

    pub(crate) fn frames<'b>(&'b self) -> &'b [Rto<Object<'a>>] {
        &self.vec[self.current.start..]
    }

    pub(crate) fn arg_count(&self) -> usize {
        self.len() - self.current.start
    }

    pub(crate) fn current_args(&self) -> &[Rto<Object<'a>>] {
        // index as vec
        &self.vec[self.current.start..]
    }

    pub(crate) fn arg_slice(&self, arg_slice: ArgSlice) -> &[Rto<Object<'a>>] {
        // index as stack
        &self[..arg_slice.0]
    }
}

/// A function call Frame.
///
/// This is a guard type that will pop the frame when it
/// goes out of scope.
pub(crate) struct CallFrame<'brw, 'rt> {
    env: &'brw mut Rt<super::Env<'rt>>,
}

impl<'brw, 'rt> CallFrame<'brw, 'rt> {
    /// Create a new function call frame with a drop guard to be removed when
    /// this goes out of scope.
    pub(crate) fn new(env: &'brw mut Rt<super::Env<'rt>>) -> Self {
        Self::new_with_args(env, 0)
    }

    /// Create a new function call frame with the top args on the stack. This
    /// frame will be removed when the `CallFrame` goes out of scope.
    pub(crate) fn new_with_args(env: &'brw mut Rt<super::Env<'rt>>, args: usize) -> Self {
        env.stack.push_frame(args);
        Self { env }
    }

    /// Push an argument onto the stack as part of this call frame
    pub(crate) fn push_arg(&mut self, arg: impl IntoRoot<Slot<Object<'rt>>>) {
        self.env.stack.push(arg);
    }

    /// Set the total argument count before a function call
    pub(crate) fn finalize_arguments(&mut self) {
        let args = self.env.stack.arg_count().try_into().unwrap();
        self.env.stack.set_arg_count(args, false);
    }

    pub(crate) fn arg_count(&self) -> usize {
        let count1 = self.env.stack.arg_count();
        let count2 = self.env.stack.current.arg_cnt.0 as usize;
        assert_eq!(count1, count2);
        count1
    }

    pub(crate) fn arg_slice(&self) -> &[Rto<Object<'rt>>] {
        &self.env.stack[..self.arg_count()]
    }

    /// Push a slice of arguments onto the stack as part of this call frame.
    pub(crate) fn push_arg_slice(&mut self, src: &[Object]) {
        self.env.stack.extend_from_slice(src);
    }
}

impl Drop for CallFrame<'_, '_> {
    fn drop(&mut self) {
        self.env.stack.pop_frame();
    }
}

impl<'rt> Deref for CallFrame<'_, 'rt> {
    type Target = Rt<super::Env<'rt>>;

    fn deref(&self) -> &Self::Target {
        self.env
    }
}

impl DerefMut for CallFrame<'_, '_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.env
    }
}
