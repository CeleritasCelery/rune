use std::sync::Arc;

use fxhash::FxHashMap;
use masonry::{
    core::BrushIndex,
    parley::{FontStack, StyleProperty},
    peniko::Brush,
};

use crate::{
    face::{Face, TextBrush},
    Field,
};

/// TODO check "string interner", like ![lasso](https://github.com/Kixiron/lasso),
/// or `IndexMap`
pub type FaceName = String;

/// a wrapper of hashmap of faces, with ID generation.
#[derive(Debug, Clone, Default)]
pub struct FaceMap {
    inner: FxHashMap<FaceName, Face>,
}

impl FaceMap {
    pub fn insert(&mut self, face: Face) -> Option<Face> {
        let name = face.name.clone();
        self.inner.insert(name, face)
    }

    pub fn get(&self, name: &str) -> Option<&Face> {
        self.inner.get(name)
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut Face> {
        self.inner.get_mut(name)
    }

    pub fn get_default_face(&self) -> Option<&Face> {
        self.get("default")
    }

    pub fn resolve<'a>(&'a self, face: &'a Face, brush_map: &mut BrushMap) -> Vec<StyleProperty<'a, BrushIndex>> {
        resolve_one(face, self, brush_map)
    }
}

#[derive(Debug, Clone, Default)]
pub struct BrushMap {
    pub brushes: Vec<Brush>,
    indices: FxHashMap<FaceName, TextBrush<Field<usize>>>,
}

impl BrushMap {
    pub fn gen_id(&mut self, face: &Face) -> TextBrush<Field<usize>> {
        let val = face.brush.map(|field| {
            field.map(|color| {
                let brush = Brush::Solid(color);
                let index = self.brushes.len();
                self.brushes.push(brush);
                index
            })
        });
        self.indices.insert(face.name.clone(), val);
        val
    }

    pub fn build_style(&mut self) {}
}

// #[derive(Debug, Clone, Default)]
// pub struct PropertyMap {
//     prop_ptrs: FxHashMap<Arc<str>, PropertyIndex>,
//     props: Vec<Vec<StyleProperty<'static, BrushIndex>>>,
// }

// #[derive(Debug, Clone, PartialEq, Eq)]
// pub struct PropertyIndex {
//     this: Option<usize>,
//     inherit: Vec<usize>,
// }

pub fn resolve_one<'a>(
    face: &'a Face,
    faces: &'a FaceMap,
    brush_map: &mut BrushMap,
) -> Vec<StyleProperty<'a, BrushIndex>> {
    let mut resolved: Vec<StyleProperty<'a, BrushIndex>> = Vec::new();
    let mut brush = brush_map.gen_id(face);
    brush = resolve_one_inner(face, faces, brush_map, &mut resolved, brush);
    // ignore background and overline for now
    brush
        .foreground
        .map(|i| resolved.push(StyleProperty::Brush(BrushIndex(i))));
    brush.underline.map(|i| {
        resolved.push(StyleProperty::Underline(true));
        resolved.push(StyleProperty::UnderlineBrush(Some(BrushIndex(i))));
    });
    brush.strike_through.map(|i| {
        resolved.push(StyleProperty::Strikethrough(true));
        resolved.push(StyleProperty::StrikethroughBrush(Some(BrushIndex(i))));
    });
    resolved
}

pub fn resolve_one_inner<'a>(
    face: &'a Face,
    faces: &'a FaceMap,
    brush_map: &mut BrushMap,
    resolved: &mut Vec<StyleProperty<'a, BrushIndex>>,
    mut brush: TextBrush<Field<usize>>,
) -> TextBrush<Field<usize>> {
    // First, process inheritance
    for parent in face.inherit.iter() {
        let Some(parent_face) = faces.get(&parent) else {
            continue;
        };
        brush = resolve_one_inner(parent_face, faces, brush_map, resolved, brush);
    }

    // Helper function to remove existing properties of a given type
    let remove_property = |resolved: &mut Vec<StyleProperty<'a, BrushIndex>>, predicate: fn(&StyleProperty<'a, BrushIndex>) -> bool| {
        resolved.retain(|p| !predicate(p));
    };

    // Then, process current face properties (these override inherited ones)
    if face.font_size.is_explicit() {
        // Remove any existing font size properties
        remove_property(resolved, |p| matches!(p, StyleProperty::FontSize(_)));
        
        // Add new property if Some, or leave removed if None
        if let Some(font_size) = face.font_size.as_ref() {
            resolved.push(StyleProperty::FontSize(*font_size));
        }
    }

    if face.stretch.is_explicit() {
        // Remove any existing font width properties
        remove_property(resolved, |p| matches!(p, StyleProperty::FontWidth(_)));
        
        // Add new property if Some, or leave removed if None
        if let Some(width) = face.stretch.as_ref() {
            resolved.push(StyleProperty::FontWidth(*width));
        }
    }
    
    if face.weight.is_explicit() {
        // Remove any existing font weight properties
        remove_property(resolved, |p| matches!(p, StyleProperty::FontWeight(_)));
        
        // Add new property if Some, or leave removed if None
        if let Some(weight) = face.weight.as_ref() {
            resolved.push(StyleProperty::FontWeight(*weight));
        }
    }
    
    if face.style.is_explicit() {
        // Remove any existing font style properties
        remove_property(resolved, |p| matches!(p, StyleProperty::FontStyle(_)));
        
        // Add new property if Some, or leave removed if None
        if let Some(style) = face.style.as_ref() {
            resolved.push(StyleProperty::FontStyle(*style));
        }
    }

    if face.font_family.is_explicit() {
        // Remove any existing font stack properties
        remove_property(resolved, |p| matches!(p, StyleProperty::FontStack(_)));
        
        // Add new property if Some, or leave removed if None
        if let Some(families) = face.font_family.as_ref() {
            resolved.push(StyleProperty::FontStack(FontStack::List(
                families.clone().into(),
            )));
        }
    }

    // Handle brush properties - merge child face brush properties over inherited ones
    let b = match brush_map.indices.get(&face.name).copied() {
        Some(b) => b,
        None => brush_map.gen_id(face),
    };
    
    // Explicit brush properties from child face override parent properties
    // Field::merge correctly handles explicit None values by keeping them as None
    brush = brush.merge(b, Field::merge);

    brush
}
