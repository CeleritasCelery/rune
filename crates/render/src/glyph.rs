#![allow(dead_code)]

// use crate::FaceId;

// use skrifa; use skrifa::GlyphId;

use crate::face::FaceId;

/// the glyph for rendering
#[derive(Debug, Clone, Copy, Default)]
pub struct Glyph {
    /// logical position of this glyph in original text
    pub charpos: u64,
    /// logical position of this glyph in its logical line
    pub linepos: u64,

    /// the visual position and area of this glyph
    pub data: GlyphData,
    pub info: GlyphInfo,

    pub face_id: FaceId,
}

impl Glyph {
    pub fn from_char(ch: char) -> Glyph {
        Glyph {
            info: GlyphInfo::char(ch),
            ..Default::default()
        }
    }

    pub fn from_str(content: &str, pos: u64) -> Vec<Glyph> {
        let mut glyphs = Vec::with_capacity(content.len());
        Glyph::append_to(content, pos, &mut glyphs);
        glyphs
    }

    /// append text to existing glyph row. return the length appended.
    pub fn append_to(content: &str, pos: u64, glyphs: &mut Vec<Glyph>) -> Option<usize> {
        glyphs.reserve(content.len());
        for (i, ch) in content.chars().into_iter().enumerate() {
            let glyph = Glyph {
                charpos: i as u64 + pos,
                linepos: i as u64,
                info: GlyphInfo::char(ch),
                ..Default::default()
            };
            glyphs.push(glyph);
        }
        Some(content.len())
    }

    pub fn is_linebreak(&self) -> bool {
        match self.info {
            GlyphInfo::Char(char_glyph) => {
                char_glyph.ch == '\n'
            }
            _ => false
        }
    }
}

#[derive(Debug, Clone, Copy, Default)]
pub struct GlyphData {
    /// X offset of hitbox
    pub x_min: f32,
    pub x_max: f32,
    /// Y offset of hitbox
    pub y_min: f32,
    pub y_max: f32,

    /// Width of this glyph
    pub w: f32,

    pub x: f32,
    pub y: f32,
    //
    pub ascent: f32,
    pub descent: f32,
}

#[derive(Debug, Clone, Copy)]
pub enum GlyphInfo {
    /// Glyph describes a character.
    Char(CharGlyph),

    /// Glyph describes a static or automatic composition.
    Composite(CompositeGlyph),

    /// Glyph describes a glyphless character.
    Glyphless,

    /// Glyph describes an image.
    Image,

    /// Glyph is a space of fractional width and/or height.
    Stretch,

    /// Glyph is an external widget drawn by the GUI toolkit.
    XWidget,
}

impl Default for GlyphInfo {
    fn default() -> Self {
        Self::Glyphless
    }
}

impl GlyphInfo {
    pub fn char(ch: char) -> GlyphInfo {
        GlyphInfo::Char(CharGlyph {
            ch,
            gid: GlyphId::NOTDEF,
        })
    }
    // pub fn char(font_id: fontdb::ID, glyph_id: u16, font_size: f32) -> GlyphInfo {
    //     GlyphInfo::Char(CharGlyph {
    //         font_size,
    //         font_id,
    //         glyph_id,
    //     })
    // }

    pub fn as_char(&self) -> Option<&CharGlyph> {
        if let Self::Char(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CharGlyph {
    // pub font_size: f32,
    // pub font_id: fontdb::ID,
    // // bidi: bool
    pub gid: GlyphId,
    pub ch: char,
}

#[derive(Debug, Clone, Copy)]
pub struct CompositeGlyph {
    /// start positon of this cluster
    pub start: usize,
    /// end positon of this cluster
    pub end: usize,
}

#[derive(Debug, Clone, Copy)]
pub struct ImageGlyph {
    pub id: u64,
    pub x: f32,
    pub y: f32,
    pub width: f32,
    pub height: f32,
}
