#[repr(u64)]
enum CharBits {
    Alt = 0x0400000,
    Super = 0x0800000,
    Hyper = 0x1000000,
    Shift = 0x2000000,
    Ctl = 0x4000000,
    Meta = 0x8000000,
}

pub const CHAR_MODIFIER_MASK: u64 = {
    CharBits::Alt as u64
        | CharBits::Super as u64
        | CharBits::Hyper as u64
        | CharBits::Shift as u64
        | CharBits::Ctl as u64
        | CharBits::Meta as u64
};
