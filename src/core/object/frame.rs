use std::collections::HashMap;
use std::fmt::Display;
use std::sync::{Arc, Mutex};

use render::viewmodel::{TextBuffer, WindowContent};
use rune_gui::render::frame_layout::FrameLayout;
use rune_gui::render::FaceMap;
use rune_macros::Trace;

use crate::core::env::intern;
use crate::core::gc::{Context, IntoRoot};
use crate::core::object::ObjectType;
use crate::derive_GcMoveable;
use crate::intervals::textget;
use crate::{
    core::gc::{Block, GcHeap, Slot, Trace},
};

use super::{Gc, LispWindow, Object, TagType, WindowConfig, WindowData, WithLifetime, NIL};

#[derive(PartialEq, Eq, Debug, Trace)]
pub struct LispFrame(GcHeap<LispFrameInner<'static>>);
derive_GcMoveable!(LispFrame);

#[derive(Debug)]
pub struct LispFrameInner<'ob> {
    data: Mutex<FrameData<'ob>>,
}

#[derive(Debug)]
pub(crate) struct FrameData<'ob> {
    pub(crate) config: FrameConfig,
    pub(crate) params: Slot<Object<'ob>>,
    pub(crate) components: HashMap<u64, Component<'ob>>,
}

#[derive(Debug)]
pub struct FrameConfig {
    /* Name of this frame: a Lisp string.  It is used for looking up resources,
    as well as for the title in some cases.  */
    pub name: String,
    pub frame_id: u64,

    pub parent: Option<String>,

    // title: String,
    // parent_frame: Option<Arc<Frame>>,
    pub layout: FrameLayout,

    pub face_map: Arc<Mutex<FaceMap>>,

    // collection of windows. window id is converted from taffy's `NodeId`.
    pub windows: HashMap<u64, WindowConfig>,

    pub selected_window: u64,

    pub cursor_pos: usize,

    pub left: f32,
    pub top: f32,
}

#[derive(Debug)]
pub(crate) struct Component<'ob> {
    pub(crate) id: u64,
    pub(crate) data: ComponentData<'ob>,
}

#[derive(Debug)]
pub enum ComponentData<'ob> {
    Window(WindowData<'ob>),
    Modeline,
    Bar,
}

impl Trace for Component<'_> {
    fn trace(&self, state: &mut crate::core::gc::GcState) {
        match &self.data {
            ComponentData::Window(window_data) => window_data.trace(state),
            _ => (),
        }
    }
}

impl<'new> IntoRoot<Component<'new>> for Component<'_> {
    unsafe fn into_root(self) -> Component<'new> {
        let result: Component<'new> = std::mem::transmute(self);
        result
    }
}

impl<'ob> Component<'ob> {
    fn new_window(id: u64, params: Slot<Object<'ob>>) -> Self {
        let data = ComponentData::Window(WindowData::new(params));
        Self { id, data }
    }
}

impl Trace for FrameData<'_> {
    fn trace(&self, state: &mut crate::core::gc::GcState) {
        self.params.trace(state);
        for (_id, data) in self.components.iter() {
            data.trace(state);
        }
    }
}

impl Trace for LispFrameInner<'_> {
    fn trace(&self, state: &mut crate::core::gc::GcState) {
        self.data.lock().unwrap().trace(state);
    }
}

impl<'new> IntoRoot<LispFrameInner<'new>> for LispFrameInner<'_> {
    unsafe fn into_root(self) -> LispFrameInner<'new> {
        self.with_lifetime()
    }
}

impl<'new> WithLifetime<'new> for LispFrameInner<'_> {
    type Out = LispFrameInner<'new>;
    unsafe fn with_lifetime(self) -> LispFrameInner<'new> {
        let result: LispFrameInner<'new> = std::mem::transmute(self);
        result
    }
}

impl<'ob> PartialEq for LispFrameInner<'ob> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

// FIXME is Slot<Object> thread-safe?
unsafe impl Sync for LispFrameInner<'_> {}

impl Eq for LispFrameInner<'_> {}

impl<'ob> FrameData<'ob> {
    fn new(config: FrameConfig, params: Slot<Object<'ob>>) -> Self {
        let mut components = HashMap::new();
        // TODO should make all windows in config
        let id = config.selected_window;
        // TODO determine window params
        components.insert(id, Component::new_window(id, params.clone()));
        // let id = config.layout.main;
        Self { config, params, components }
    }
}

impl<'new> LispFrame {
    pub(in crate::core) fn clone_in<const C: bool>(
        &self,
        _: &'new Block<C>,
    ) -> Gc<&'new LispFrame> {
        unsafe { self.with_lifetime().tag() }
    }
}

impl<'ob> LispFrameInner<'ob> {
    pub fn new(config: FrameConfig, params: Slot<Object<'ob>>) -> Self {
        let data = FrameData::new(config, params);
        let data = Mutex::new(data);
        LispFrameInner { data }
    }
}

impl LispFrame {
    pub fn create<'ob>(
        frame: FrameConfig,
        params: Slot<Object<'ob>>,
        block: &'ob Block<true>,
    ) -> &'ob Self {
        let frame = unsafe { Self::new(frame, params, true) };
        block.objects.alloc(frame)
    }

    pub unsafe fn new<'ob>(frame: FrameConfig, params: Slot<Object<'ob>>, constant: bool) -> Self {
        let params = params.with_lifetime();
        let new = GcHeap::new(LispFrameInner::new(frame, params), constant);

        Self(new)
    }

    pub fn data(&self) -> std::sync::MutexGuard<'_, FrameData<'static>> {
        let guard = self.0.data.lock().unwrap();
        guard
    }

    pub fn viewmodel(&self, cx: &Context) -> render::viewmodel::ViewModel {
        let lock = self.data();
        // lock.config;
        // lock.components.
        let windows = lock.components.iter().map(|(id, c)| {
            match &c.data {
                ComponentData::Window(window_data) => {
                    let mut buf = window_data.buffer.as_ref().unwrap().lock().unwrap();
                    let content = buf.get().text.to_string();
                    let tree = buf.textprops_with_lifetime();
                    let iter  = tree.iter(0, 10000);
                    let spans = iter.map(|(range, prop_list)| {
                        let face = cx.add(intern("face", cx));
                        let face = textget(prop_list, face).unwrap();
                        let s = if let ObjectType::Symbol(s) = face.untag() {
                            s.to_string()
                            
                        } else {
                            "default".to_string()
                        };
                        (range, s)
                    }).collect();
                    let window_content = WindowContent::TextBuffer(TextBuffer {
                        text: content,
                        spans,
                    });
                    ((*id).into(), Arc::new(window_content))
                },
                ComponentData::Modeline => todo!(),
                ComponentData::Bar => todo!(),
            }

        });
        lock.config.layout.gen_viewmodel(lock.config.face_map.clone(), windows)
    }

    pub(crate) fn selected_window(&self) -> LispWindow {
        let guard = self.data();
        let id = guard.config.selected_window;
        LispWindow::new(id, self)
    }

    pub(crate) fn set_selected_window(&self, id: u64) {
        let mut guard = self.data();
        if guard.components.get(&id).is_some() {
            guard.config.selected_window = id;
        }
    }
}

impl Display for LispFrame {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let data = self.0.data.lock().unwrap();
        let name = &data.config.name;
        let id = &data.config.frame_id;
        write!(f, "#<frame {id:x}>")
    }
}

impl FrameConfig {
    pub fn new(width: f32, height: f32) -> Self {
        let layout = FrameLayout::new(width, height);
        let selected_window = layout.main.into();
        Self {
            name: String::new(),
            frame_id: 0,
            parent: None,
            layout,
            face_map: Default::default(),
            windows: HashMap::default(),
            selected_window,
            cursor_pos: 0,
            left: 0.,
            top: 0.,
        }
    }
}