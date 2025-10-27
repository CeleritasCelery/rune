use std::sync::{Arc, Mutex, Weak};

pub use taffy::NodeId as TaffyId;
use taffy::Layout;

pub type HashMap<K, V> = std::collections::HashMap<K, V>;

use crate::{face::Face, FaceMap, FaceName};

pub type FrameId = u64;

#[derive(Debug, Clone, Default)]
pub struct ViewModel {
    pub frames: Vec<FrameState>,
}

#[derive(Debug, Clone)]
pub struct FrameState {
    pub id: FrameId,
    pub root_layout: Layout,
    pub face_map: Arc<Mutex<FaceMap>>,
    pub windows: HashMap<u64, Window>,
}

#[derive(Debug, Clone)]
pub struct Window {
    pub numbering: u64,
    pub id: u64,
    // TODO
    pub content: Weak<WindowContent>,
    pub layout: Layout,
}

impl Window {
    pub fn new(id: u64, numbering: u64, content: &Arc<WindowContent>, layout: Layout) -> Self {
        let content = Arc::downgrade(content);
        Self {
            id,
            numbering,
            content,
            layout,
        }
    }
}

#[derive(Clone, Debug)]
pub enum WindowContent {
    TextBuffer(TextBuffer),
}

#[derive(Clone, Debug)]
pub struct TextBuffer {
    pub text: String,
    pub spans: Vec<(std::ops::Range<usize>, FaceName)>,
}

impl WindowContent {
    pub fn as_text_buffer(&self) -> Option<&TextBuffer> {
        match self {
            Self::TextBuffer(v) => Some(v),
        }
    }
}

impl FrameState {
    pub fn new(root_layout: Layout, face_map: Arc<Mutex<FaceMap>>, windows: HashMap<u64, Window>) -> Self {
        Self {
            id: 0,
            root_layout,
            face_map,
            windows,
        }
    }

    pub fn root_layout(&self) -> taffy::Layout {
        self.root_layout
    }

    pub fn get_window(&self, id: u64) -> Option<&Window> {
        self.windows.get(&id)
    }

    pub fn get_window_mut(&mut self, id: u64) -> Option<&mut Window> {
        self.windows.get_mut(&id)
    }
}

impl ViewModel {
    pub fn new() -> ViewModel {
        let frames = Vec::new();
        ViewModel { frames }
    }

    pub fn get_main_frame(&self) -> Option<&FrameState> {
        self.frames.get(0)
    }

    pub fn get_main_frame_mut(&mut self) -> Option<&mut FrameState> {
        self.frames.get_mut(0)
    }
}
