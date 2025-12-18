use std::sync::{Arc, Mutex};

use taffy::prelude::*;

use crate::{viewmodel::{FrameState, HashMap, ViewModel, Window, WindowContent}, FaceMap};
#[derive(Debug)]
pub struct FrameLayout {
    pub tree: TaffyTree<()>,
    /// the outer frame
    pub root: NodeId,
    pub title_bar: NodeId,
    pub menu_bar: NodeId,
    pub tool_bar: NodeId,
    pub tab_bar: NodeId,
    /// inner frame
    pub main: NodeId,
}

impl FrameLayout {
    pub fn new(width: f32, height: f32) -> Self {
        let outer_frame_style = Style {
            size: Size {
                width: length(width),
                height: length(height),
            },
            border: Rect::length(2.0),
            flex_direction: FlexDirection::Column,
            ..Default::default()
        };

        let title_bar_style = Style {
            size: Size {
                width: Dimension::percent(1.0),
                height: length(30.0),
            },
            border: Rect::length(2.0),
            align_items: Some(AlignItems::Center),
            ..Default::default()
        };

        let menu_bar_style = Style {
            size: Size {
                width: Dimension::percent(1.0),
                height: length(30.0),
            },
            border: Rect::length(2.0),
            align_items: Some(AlignItems::Center),
            ..Default::default()
        };

        let tool_bar_style = Style {
            size: Size {
                width: Dimension::percent(1.0),
                height: length(30.0),
            },
            border: Rect::length(2.0),
            align_items: Some(AlignItems::Center),
            ..Default::default()
        };

        let tab_bar_style = Style {
            size: Size {
                width: Dimension::percent(1.0),
                height: length(20.0),
            },
            border: Rect::length(2.0),
            align_items: Some(AlignItems::Center),
            ..Default::default()
        };

        let inner_frame_style = Style {
            size: Size {
                width: percent(1.),
                height: percent(1.),
            },
            border: Rect::length(2.0),
            flex_direction: FlexDirection::Column,
            flex_grow: 1.0,
            ..Default::default()
        };

        let mut tree = TaffyTree::new();
        let title_bar = tree.new_leaf(title_bar_style).unwrap();
        let menu_bar = tree.new_leaf(menu_bar_style).unwrap();
        let tool_bar = tree.new_leaf(tool_bar_style).unwrap();
        let tab_bar = tree.new_leaf(tab_bar_style).unwrap();
        let inner_frame = tree.new_leaf(inner_frame_style).unwrap();

        let outer_frame = tree
            .new_with_children(
                outer_frame_style,
                &[title_bar, menu_bar, tool_bar, tab_bar, inner_frame],
            )
            .unwrap();
        tree.compute_layout(outer_frame, max_content()).unwrap();

        FrameLayout {
            tree,
            root: outer_frame,
            title_bar,
            menu_bar,
            tool_bar,
            tab_bar,
            main: inner_frame,
        }
    }
}

impl FrameLayout {
    pub fn get(&self, id: NodeId) -> anyhow::Result<&Layout> {
        Ok(self.tree.layout(id)?)
    }

    pub fn resize(&mut self, width: f32, height: f32) -> anyhow::Result<()> {
        // let r = self.tree.set_style(self.root, );
        let mut style = self.tree.style(self.root)?.clone();
        style.size.width = length(width);
        style.size.height = length(height);
        self.tree.set_style(self.root, style)?;

        self.tree.compute_layout(self.root, max_content())?;
        Ok(())
    }

    #[allow(unused)]
    pub(crate) fn print_tree(&mut self) {
        self.tree.print_tree(self.root)
    }

    pub fn split(
        &mut self,
        parent: NodeId,
        direction: FlexDirection,
    ) -> anyhow::Result<(NodeId, NodeId)> {
        let others = self.tree.child_count(parent);
        if others > 0 {
            return Err(taffy::TaffyError::InvalidInputNode(parent).into());
        }

        let Some(_) = self.tree.parent(parent) else {
            return Err(taffy::TaffyError::InvalidInputNode(parent).into());
        };

        let mut style = self.tree.style(parent)?.clone();
        let origin = self.tree.new_leaf(style.clone())?;
        let new = self.tree.new_leaf(Style {
            // justify_items: Some(JustifyItems::Stretch),
            flex_grow: 1.0,
            ..Default::default()
        })?;

        style.flex_direction = direction;
        style.justify_content = Some(JustifyContent::Stretch);
        self.tree.set_style(parent, style)?;
        self.tree.add_child(parent, origin)?;
        self.tree.add_child(parent, new)?;

        self.tree.compute_layout(self.root, Size::max_content())?;
        Ok((origin, new))
    }

    pub fn vsplit(&mut self, parent: NodeId) -> anyhow::Result<(NodeId, NodeId)> {
        self.split(parent, FlexDirection::Row)
    }

    pub fn hsplit(&mut self, parent: NodeId) -> anyhow::Result<(NodeId, NodeId)> {
        self.split(parent, FlexDirection::Column)
    }

    pub fn gen_viewmodel<I: Iterator<Item = (NodeId, Arc<WindowContent>)>>(
        &self,
        face_map: Arc<Mutex<FaceMap>>,
        windows: I,
    ) -> ViewModel {
        let windows = windows
            .enumerate()
            .map(|(numbering, (id, content))| {
                let layout = self.get(id).unwrap();
                let id: u64 = id.into();
                let window = Window::new(id, numbering as u64, &content, *layout);
                (id, window)
            })
            .collect::<HashMap<_, _>>();
        let frame_state = FrameState {
            id: 0,
            root_layout: *self.get(self.root).unwrap(),
            face_map,
            windows,
        };
        ViewModel {
            frames: vec![frame_state],
        }
    }
}
