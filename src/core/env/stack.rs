use crate::core::{
    gc::{Context, Rt},
    object::{GcObj, NIL},
};
use rune_macros::Trace;
use std::ops::{Index, IndexMut, RangeTo};

/// The stack of lisp objects used to pass and store arguments in the bytecode
/// VM and interpreter. The top of the stack is index 0 and all indexing
/// functions operate from top to bottom. The stack is partitioned into frames.
/// Each frame represents a function call and it's arguments. The API is
/// designed so that code cannot access elements outside of their frame (doing
/// so results in a panic). Frames are added and removed with [push_frame] and
/// [pop_frame] respectively.
#[derive(Debug, Default, Trace)]
pub(crate) struct LispStack {
    vec: Vec<GcObj<'static>>,
    #[no_trace]
    pub(crate) frame_starts: Vec<usize>,
}

// To make this simpler we implement indexing from the top of the stack (end of
// the vec) instead of the bottom. This is the convention that all the bytecode
// functions use.
impl Index<usize> for RootedLispStack {
    type Output = Rt<GcObj<'static>>;

    fn index(&self, index: usize) -> &Self::Output {
        let index = self.offset_end(index);
        &self.vec[index]
    }
}

// This impl is specifically for the Stack. It takes the index from the end of
// the vector instead of the start. This matches how the lisp stack behaves.
impl IndexMut<usize> for RootedLispStack {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        let index = self.offset_end(index);
        &mut self.vec[index]
    }
}

// This impl is specifically for the Stack. It takes the range from the end of
// the vector instead of the start. This matches how the lisp stack behaves.
impl Index<RangeTo<usize>> for RootedLispStack {
    type Output = [Rt<GcObj<'static>>];

    fn index(&self, index: RangeTo<usize>) -> &Self::Output {
        assert!(index.end <= self.len());
        let end = self.len() - index.end;
        let vec: &[Rt<GcObj>] = &self.vec;
        &vec[end..]
    }
}

impl RootedLispStack {
    fn frame_start(&self) -> usize {
        self.frame_starts.last().copied().unwrap_or(0)
    }

    pub(crate) fn push_frame(&mut self, start: usize) {
        assert!(start <= self.len());
        assert!(self.frame_starts.iter().all(|&s| s <= start));
        self.frame_starts.push(start);
    }

    pub(crate) fn pop_frame(&mut self) {
        let len = self.frame_starts.pop().unwrap();
        self.vec.truncate(len);
    }

    pub(crate) fn return_frame(&mut self) {
        let start = self.frame_starts.pop().unwrap();
        self.vec.swap_remove(start);
        self.vec.truncate(start + 1);
    }

    pub(crate) fn current_frame(&self) -> usize {
        self.frame_starts.len()
    }

    pub(crate) fn unwind_frames(&mut self, frame: usize) {
        assert!(frame <= self.current_frame());
        let Some(len) = self.frame_starts.get(frame) else {
            return; /* no frames to unwind */
        };
        self.vec.truncate(*len);
        self.frame_starts.truncate(frame);
    }

    pub(crate) fn len(&self) -> usize {
        self.vec.len()
    }

    pub(crate) fn push(&mut self, value: GcObj) {
        self.vec.push(value);
    }

    pub(crate) fn pop<'ob>(&mut self, cx: &'ob Context) -> GcObj<'ob> {
        self.vec.bind_mut(cx).pop().unwrap()
    }

    pub(crate) fn top(&mut self) -> &mut Rt<GcObj<'static>> {
        self.vec.last_mut().unwrap()
    }

    pub(crate) fn offset_end(&self, i: usize) -> usize {
        assert!(i < self.len());
        let from_end = self.len() - (i + 1);
        assert!(*self.frame_starts.last().unwrap() <= from_end);
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

    pub(crate) fn remove_top(&mut self, i: impl Into<usize>) {
        let i = i.into();
        let offset = self.offset_end(i);
        self.truncate(offset + 1);
    }

    pub(crate) fn truncate(&mut self, len: usize) {
        self.vec.truncate(len);
    }

    pub(crate) fn frame_iter(&self) -> impl Iterator<Item = &Rt<GcObj>> {
        self.vec[self.frame_start()..].iter().rev()
    }
}
