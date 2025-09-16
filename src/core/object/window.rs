use std::fmt::Display;

use rune_macros::Trace;

use crate::{
    core::{
        error::TypeError,
        gc::{Block, GcHeap, IntoRoot, Slot, Trace},
    },
    derive_GcMoveable,
};

use super::{Gc, LispBuffer, LispFrame, Object, TagType, WithLifetime, NIL};

#[derive(PartialEq, Eq, Debug, Trace)]
pub struct LispWindow(GcHeap<LispWindowInner<'static>>);
derive_GcMoveable!(LispWindow);

/// we can impl window in 2 ways: 1 is make window immutable, and every alternation
/// creates a new window; the 2nd approach, just store them in a struct.
#[derive(Debug, Clone, Copy)]
pub struct LispWindowInner<'ob> {
    id: u64,
    parent_frame: &'ob LispFrame,
    // config: Mutex<WindowData<'ob>>,
    // parent_frame: Slot<Object<'ob>>,
}

impl Trace for LispWindowInner<'_> {
    fn trace(&self, _state: &mut crate::core::gc::GcState) {
        // no need to trace here
    }
}

#[derive(Debug, Trace, Clone)]
pub(crate) struct WindowData<'ob> {
    #[no_trace]
    pub(crate) config: WindowConfig,
    pub(crate) params: Slot<Object<'ob>>,
    #[no_trace]
    pub(crate) buffer: Option<&'ob LispBuffer>,
}

#[derive(Debug, Clone)]
pub struct WindowConfig {
    /// A marker pointing to where in the text to start displaying.
    start: u64,

    /// A marker pointing to where in the text point is in this window,
    /// used only when the window is not selected.
    /// This exists so that when multiple windows show one buffer
    /// each one can have its own value of point.
    point: u64,

    left: f32,
    top: f32,

    /// Line number and position of a line somewhere above the top of the
    /// screen.  If this field is zero, it means we don't have a base line.
    ///
    /// used in line-number-mode, ignore for now
    base_line_number: u64,
    base_line_pos: u64,
}

impl<'ob> PartialEq for LispWindowInner<'ob> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl Eq for LispWindowInner<'_> {}
impl<'new> LispWindow {
    pub(in crate::core) fn clone_in<const C: bool>(
        &self,
        _: &'new Block<C>,
    ) -> Gc<&'new LispWindow> {
        unsafe { self.with_lifetime().tag() }
    }
}

impl Display for LispWindow {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let id = self.0.id;
        // TODO should add buffer name
        write!(f, "#<window {id:x}>")
    }
}

impl LispWindow {
    pub(crate) fn new(id: u64, parent_frame: &LispFrame) -> Self {
        let parent_frame = unsafe { parent_frame.with_lifetime() };
        let new = LispWindowInner { id, parent_frame };
        Self(GcHeap::new(new, true))
    }

    pub(crate) fn get_frame(&self) -> &LispFrame {
        let frame = self.0.parent_frame;
        frame
    }

    pub(crate) fn id(&self) -> u64 {
        self.0.id
    }

    pub(crate) fn data(&self) -> Option<WindowData<'static>> {
        let guard = self.get_frame().data();
        let c = guard.components.get(&self.id());
        c.and_then(|c| match &c.data {
            super::ComponentData::Window(w) => Some(w.clone()),
            _ => None,
        })
    }

    pub(crate) fn modify_data<T>(&self, mod_fn: impl Fn(&mut WindowData) -> T) -> Option<T> {
        let mut guard = self.get_frame().data();
        let Some(c) = guard.components.get_mut(&self.id()) else { return None};
        match &mut c.data {
            super::ComponentData::Window(w) => Some(mod_fn(w)),
            _ => None,
        }
    }
}

impl<'ob> WindowData<'ob> {
    pub fn new(params: Slot<Object<'ob>>) -> Self {
        let config = WindowConfig::new();
        Self { config, params, buffer: None }
    }

    /// return whether the buffer is changed
    pub(crate) fn set_buffer(&mut self, new_buffer: &'ob LispBuffer) -> bool {
        let mut changed = true;
        if let Some(old) = self.buffer {
            if old == new_buffer {
                changed = false;
            }
        }
        self.buffer = Some(new_buffer);
        return changed;
    }
}

impl WindowConfig {
    fn new() -> Self {
        Self {
            start: 1,
            point: 1,
            left: 0.,
            top: 0.,
            base_line_number: 0,
            base_line_pos: 0,
        }
    }
}