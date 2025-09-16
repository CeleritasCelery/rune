use std::sync::Arc;

use crate::{peniko::Color, FaceName, Field};
use masonry::{
    parley::{
        FontFamily, FontStyle, FontWeight, FontWidth,
    },
    peniko::color,
};
use thiserror::Error;

// pub use crate::manager::FaceManager;

#[derive(Debug, Clone, Copy, Hash, Eq, PartialEq)]
#[repr(transparent)]
pub struct FaceId(pub u64);
// include!("default_face_id.rs");

impl Default for FaceId {
    fn default() -> Self {
        Self(0)
    }
}

impl std::fmt::Display for FaceId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, Default, Copy)]
pub struct TextBrush<T: masonry::parley::Brush> {
    pub foreground: T,
    pub background: T,
    pub underline: T,
    pub overline: T,
    pub strike_through: T,
}

impl<T: masonry::parley::Brush> TextBrush<T> {
    pub fn map<U: masonry::parley::Brush>(self, mut f: impl FnMut(T) -> U) -> TextBrush<U> {
        let foreground = f(self.foreground);
        let background = f(self.background);
        let underline = f(self.underline);
        let overline = f(self.overline);
        let strike_through = f(self.strike_through);
        TextBrush { foreground, background, underline, overline, strike_through }
    }

    pub fn merge(self, other: Self, f: impl Fn(T, T) -> T) -> Self {
        let foreground = f(self.foreground, other.foreground);
        let background = f(self.background, other.background);
        let underline = f(self.underline, other.underline);
        let overline = f(self.overline, other.overline);
        let strike_through = f(self.strike_through, other.strike_through);
        Self {
            foreground,
            background,
            underline,
            overline,
            strike_through,
        }
    }

}

/// see xfaces.c
#[derive(Debug, Clone, Default)]
pub struct Face {
    pub name: FaceName,

    /// 3. Relative proportionate width, aka character set width or set
    /// width (swidth), e.g. `semi-compressed'.
    pub swidth: u64,
    /// font height, in emacs, it is a integer in 1/10 pt. we just use a float number.
    pub font_height: f32,

    // settings: Option<swash::Setting<u16>>,
    pub font_size: Field<f32>,
    pub font_family: Field<Vec<FontFamily<'static>>>,

    // f: fontdb::Family,
    pub stretch: Field<FontWidth>,
    pub weight: Field<FontWeight>,
    pub style: Field<FontStyle>,

    pub brush: TextBrush<Field<Color>>,

    // pub foregroud_color: Option<Color>,
    // pub background_color: Option<Color>,
    // pub underlined_color: Option<Color>,
    // pub overlined_color: Option<Color>,
    // pub strike_through_color: Option<Color>,

    // 10. Whether or not characters should be displayed in inverse video.
    // pub inverse_video: bool,

    // pub box_around: bool,
    // pub box_type: bool,
    // pub box_color: Option<Color>,

    // 15. Font-spec, or nil.  This is a special attribute.
    // pub font_spec: usize,
    /// 16. A face name or list of face names from which to inherit attributes.
    pub inherit: Vec<Arc<str>>,
    // 18. A "distant background" color, to be used when the foreground
    // is too close to the background and is hard to read.
    // pub distant_background: Option<Color>,

    // 19. Whether to extend the face to end of line when the face
    // "covers" the newline that ends the line.
    // pub extend_to_eol: bool,
    // pub metadata: usize,
}

impl Face {
    /// Convert Color to hex string format
    pub fn color_to_string(color: &Color) -> String {
        let color = color.to_rgba8();
        format!(
            "#{:02X}{:02X}{:02X}{:02X}",
            color.r, color.g, color.b, color.a
        )
    }

    /// Parse hex string to Color
    pub fn string_to_color(s: &str) -> Option<Color> {
        let color = color::parse_color(s).ok()?;
        let color = Color::new(color.components);
        Some(color)
    }

    pub fn families_to_string(&self) -> String {
        match self.font_family.as_ref() {
            Some(family) => family
                .iter()
                .map(|f| f.to_string())
                .reduce(|a, b| format!("{a} {b}"))
                .unwrap_or_default(),
            None => String::new(),
        }
    }

    pub fn new(name: &str, font_size: f32) -> Self {
        Self {
            name: name.into(),
            font_size: Field::from(font_size),
            stretch: Field::from(FontWidth::NORMAL),
            weight: Field::from(FontWeight::NORMAL),
            style: Field::from(FontStyle::Normal),
            ..Default::default()
        }
    }

    pub fn set_font_families(&mut self, font_families: &[FontFamily<'static>]) {
        self.font_family = Field::from(font_families.to_vec())
    }
}

impl Face {
    // pub fn build_style<B: masonry::parley::Brush>(
    //     &self,
    //     builder: &mut RangedBuilder<'_, B>,
    //     range: impl RangeBounds<usize> + Clone,
    // ) {
    //     if let Some(families) = &self.font_family {
    //         for f in families {
    //             builder.push(f.clone(), range.clone());
    //         }
    //     }
    //     if let Some(font_size) = self.font_size {
    //         builder.push(StyleProperty::FontSize(font_size), range.clone());
    //     }

    //     if let Some(width) = self.stretch {
    //         builder.push(StyleProperty::FontWidth(width), range.clone());
    //     }
    //     if let Some(weight) = self.weight {
    //         builder.push(StyleProperty::FontWeight(weight), range.clone());
    //     }
    //     if let Some(style) = self.style {
    //         builder.push(StyleProperty::FontStyle(style), range.clone());
    //     }
    //     if let Some(fg) = self.foregroud_color {
    //         // builder.push(StyleProperty::Brush(Brush::Solid(fg)), range.clone());
    //     }
    //     // builder.push_default();
    //     // builder.push(StyleProperty::FontSize(self.font_size), range);
    // }
}

pub enum FaceAttribute {
    Foreground(Color),
    Background(Color),
    FontSize(f32),
    Family(Vec<FontFamily<'static>>),
    Attribute {
        stretch: u16,
        weight: u16,
        style: u16,
    },
}

#[derive(Debug, Error)]
pub enum FaceError {
    #[error("Face not found")]
    NotFound,
    #[error("Invalid face")]
    Invalid,
    #[error("Face not resolved: font did not load")]
    NotResolved,
    #[error("Other face error: {0}")]
    Other(String),
}

// Calculate line height and width given a font reference and a font size.

// The calculation takes into account the font size and the font's metrics.
// The line height is the sum of the ascent, descent, and leading of the font.
// The width is the average width of the font.

// The function returns a tuple of two values:

// - The first value is the line height.
// - The second value is the width, don't know when it is None.
// pub(crate) fn calc_line_height_and_width(
//     font_ref: skrifa::FontRef,
//     font_size: f32,
// ) -> (f32, Option<f32>) {
//     let settings: Vec<(&str, f32)> = Vec::new();
//     let var_loc = font_ref.axes().location(settings.iter().copied());
//     let font_metric = font_ref.metrics(skrifa::instance::Size::new(font_size), &var_loc);
//     let line_height = font_metric.ascent - font_metric.descent + font_metric.leading;
//     let width = font_metric.average_width;
//     (line_height, width)
// }
