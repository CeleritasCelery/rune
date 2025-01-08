// #[repr(i64)]
// pub enum CharBits {
//     CharAlt = 0x0400000,
//     CharSuper = 0x0800000,
//     CharHyper = 0x1000000,
//     CharShift = 0x2000000,
//     CharCtl = 0x4000000,
//     CharMeta = 0x8000000,
// }

// you can't have a custom BitOr in a const context, so this must be a magic number
// https://github.com/rust-lang/rust/issues/67792
pub const CHAR_MODIFIER_MASK: i64 = 0xFC0000; // CharBits::CharAlt | CharBits::CharSuper | CharBits::CharHyper | CharBits::CharShift | CharBits::CharCtl | CharBits::CharMeta;
