use std::ops::{Index, IndexMut, RangeTo};

use rune_macros::Trace;

use crate::core::{
    gc::{Context, Rt},
    object::{nil, GcObj},
};

#[derive(Debug, Default, Trace)]
pub(crate) struct LispStack {
    vec: Vec<GcObj<'static>>,
    #[no_trace]
    pub(crate) frame_starts: Vec<usize>,
}

// RootedLispStack is created by #[derive(Trace)]
impl std::ops::Deref for RootedLispStack {
    type Target = Rt<Vec<GcObj<'static>>>;

    fn deref(&self) -> &Self::Target {
        &self.vec
    }
}

impl std::ops::DerefMut for RootedLispStack {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.vec
    }
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
        let vec: &[Rt<GcObj>] = self;
        &vec[end..]
    }
}

impl RootedLispStack {
    pub(crate) fn push_frame(&mut self, start: usize) {
        assert!(start <= self.len());
        assert!(self.frame_starts.iter().all(|&s| s <= start));
        self.frame_starts.push(start);
    }

    pub(crate) fn pop_frame(&mut self) {
        let len = self.frame_starts.pop().unwrap();
        self.vec.truncate(len);
    }

    pub(crate) fn pop<'ob>(&mut self, cx: &'ob Context) -> GcObj<'ob> {
        self.bind_mut(cx).pop().unwrap()
    }

    pub(crate) fn top(&mut self) -> &mut Rt<GcObj<'static>> {
        self.last_mut().unwrap()
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
        self.swap_remove(index);
    }

    pub(crate) fn fill_extra_args(&mut self, fill_args: u16) {
        for _ in 0..fill_args {
            self.push(nil());
        }
    }

    pub(crate) fn remove_top(&mut self, i: impl Into<usize>) {
        let i = i.into();
        let offset = self.offset_end(i);
        self.truncate(offset + 1);
    }
}
