use core::ffi::*;
use either::Either;


use super::{Flags, SignedInt, UnsignedInt};

pub enum PendingAction {
    ReadFromArg,
}

fn next_char(sub: &str) -> Option<&str> {
    sub.get(1..)
}

/// Parse the [Flags field](https://en.wikipedia.org/wiki/Printf_format_string#Flags_field).
pub fn parse_flags(mut sub: &str) -> (Flags, &str) {
    let mut flags: Flags = Flags::empty();
    while let Some(&ch) = sub.as_bytes().get(0) {
        flags.insert(match ch {
            b'-' => Flags::LEFT_ALIGN,
            b'+' => Flags::PREPEND_PLUS,
            b' ' => Flags::PREPEND_SPACE,
            b'0' => Flags::PREPEND_ZERO,
            b'\'' => Flags::THOUSANDS_GROUPING,
            b'#' => Flags::ALTERNATE_FORM,
            _ => break,
        });
        sub = next_char(sub).unwrap()
    }
    (flags, sub)
}

/// Parse the [Width field](https://en.wikipedia.org/wiki/Printf_format_string#Width_field).
pub fn parse_width<'a>(mut sub: &str) -> (Either<PendingAction, c_int>, &str) {
    let mut width: c_int = 0;
    if sub.as_bytes().get(0) == Some(&b'*') {
        return (Either::Left(PendingAction::ReadFromArg), next_char(sub).unwrap());
    }
    while let Some(&ch) = sub.as_bytes().get(0) {
        match ch {
            // https://rust-malaysia.github.io/code/2020/07/11/faster-integer-parsing.html#the-bytes-solution
            b'0'..=b'9' => width = width * 10 + (ch & 0x0f) as c_int,
            _ => break,
        }
        sub = next_char(sub).unwrap();
    }
    (Either::Right(width), sub)
}

/// Parse the [Precision field](https://en.wikipedia.org/wiki/Printf_format_string#Precision_field).
pub fn parse_precision<'a>(sub: &str) -> (Option<Either<PendingAction, c_int>>, &str) {
    match sub.as_bytes().get(0) {
        Some(&b'.') => {
            let (prec, sub) = parse_width(next_char(sub).unwrap());
            (Some(prec), sub)
        }
        _ => (None, sub),
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Length {
    Int,
    /// `hh`
    Char,
    /// `h`
    Short,
    /// `l`
    Long,
    /// `ll`
    LongLong,
    /// `z`
    Usize,
    /// `t`
    Isize,
}

impl Length {
    unsafe fn parse_signed(self, args: &mut VaList) -> SignedInt {
        match self {
            Length::Int => SignedInt::Int(unsafe { args.arg() }),
            Length::Char => SignedInt::Char(unsafe { args.arg::<c_int>() } as c_schar),
            Length::Short => SignedInt::Short(unsafe { args.arg::<c_int>() } as c_short),
            Length::Long => SignedInt::Long(unsafe { args.arg() }),
            Length::LongLong => SignedInt::LongLong(unsafe { args.arg() }),
            // for some reason, these exist as different options, yet produce the same output
            Length::Usize | Length::Isize => SignedInt::Isize(unsafe { args.arg() }),
        }
    }
    unsafe fn parse_unsigned(self, args: &mut VaList) -> UnsignedInt {
        match self {
            Length::Int => UnsignedInt::Int(unsafe { args.arg() }),
            Length::Char => UnsignedInt::Char(unsafe { args.arg::<c_uint>() } as c_uchar),
            Length::Short => UnsignedInt::Short(unsafe { args.arg::<c_uint>() } as c_ushort),
            Length::Long => UnsignedInt::Long(unsafe { args.arg() }),
            Length::LongLong => UnsignedInt::LongLong(unsafe { args.arg() }),
            // for some reason, these exist as different options, yet produce the same output
            Length::Usize | Length::Isize => UnsignedInt::Isize(unsafe { args.arg() }),
        }
    }
}

/// Parse the [Length field](https://en.wikipedia.org/wiki/Printf_format_string#Length_field).
pub fn parse_length(sub: &str) -> (Length, &str) {
    match sub.as_bytes().get(0).copied() {
        Some(b'h') => match sub.as_bytes().get(1).copied() {
            Some(b'h') => (Length::Char, sub.get(2..).unwrap()),
            _ => (Length::Short, next_char(sub).unwrap()),
        },
        Some(b'l') => match sub.as_bytes().get(1).copied() {
            Some(b'l') => (Length::LongLong, sub.get(2..).unwrap()),
            _ => (Length::Long, next_char(sub).unwrap()),
        },
        Some(b'z') => (Length::Usize, next_char(sub).unwrap()),
        Some(b't') => (Length::Isize, next_char(sub).unwrap()),
        _ => (Length::Int, sub),
    }
}
