use core::ffi::*;
use core::fmt;
use core::str::from_utf8;

use super::{Argument, DoubleFormat, Flags, Specifier};

struct DummyWriter(usize);

impl fmt::Write for DummyWriter {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.0 += s.len();
        Ok(())
    }
}

struct WriteCounter<'a, T: fmt::Write>(&'a mut T, usize);

impl<'a, T: fmt::Write> fmt::Write for WriteCounter<'a, T> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.1 += s.len();
        self.0.write_str(s)
    }
}

fn write_str(
    w: &mut impl fmt::Write,
    flags: Flags,
    width: c_int,
    precision: Option<c_int>,
    b: &[u8],
) -> fmt::Result {
    let string = from_utf8(b).map_err(|_| fmt::Error)?;
    let precision = precision.unwrap_or(string.len() as c_int);
    if flags.contains(Flags::LEFT_ALIGN) {
        write!(
            w,
            "{:1$.prec$}",
            string,
            width as usize,
            prec = precision as usize
        )
    } else {
        write!(
            w,
            "{:>1$.prec$}",
            string,
            width as usize,
            prec = precision as usize
        )
    }
}

macro_rules! define_numeric {
    ($w: expr, $data: expr, $flags: expr, $width: expr, $precision: expr) => {
        define_numeric!($w, $data, $flags, $width, $precision, "")
    };
    ($w: expr, $data: expr, $flags: expr, $width: expr, $precision: expr, $ty:expr) => {{
        use fmt::Write;
        if $flags.contains(Flags::LEFT_ALIGN) {
            if $flags.contains(Flags::PREPEND_PLUS) {
                write!(
                    $w,
                    concat!("{:<+width$.prec$", $ty, "}"),
                    $data,
                    width = $width as usize,
                    prec = $precision as usize
                )
            } else if $flags.contains(Flags::PREPEND_SPACE) && !$data.is_sign_negative() {
                write!(
                    $w,
                    concat!(" {:<width$.prec$", $ty, "}"),
                    $data,
                    width = ($width as usize).wrapping_sub(1),
                    prec = $precision as usize
                )
            } else {
                write!(
                    $w,
                    concat!("{:<width$.prec$", $ty, "}"),
                    $data,
                    width = $width as usize,
                    prec = $precision as usize
                )
            }
        } else if $flags.contains(Flags::PREPEND_PLUS) {
            if $flags.contains(Flags::PREPEND_ZERO) {
                write!(
                    $w,
                    concat!("{:+0width$.prec$", $ty, "}"),
                    $data,
                    width = $width as usize,
                    prec = $precision as usize
                )
            } else {
                write!(
                    $w,
                    concat!("{:+width$.prec$", $ty, "}"),
                    $data,
                    width = $width as usize,
                    prec = $precision as usize
                )
            }
        } else if $flags.contains(Flags::PREPEND_ZERO) {
            if $flags.contains(Flags::PREPEND_SPACE) && !$data.is_sign_negative() {
                let mut d = DummyWriter(0);
                let _ = write!(
                    d,
                    concat!("{:.prec$", $ty, "}"),
                    $data,
                    prec = $precision as usize
                );
                if d.0 + 1 > $width as usize {
                    $width += 1;
                }
                write!(
                    $w,
                    concat!(" {:0width$.prec$", $ty, "}"),
                    $data,
                    width = ($width as usize).wrapping_sub(1),
                    prec = $precision as usize
                )
            } else {
                write!(
                    $w,
                    concat!("{:0width$.prec$", $ty, "}"),
                    $data,
                    width = $width as usize,
                    prec = $precision as usize
                )
            }
        } else {
            if $flags.contains(Flags::PREPEND_SPACE) && !$data.is_sign_negative() {
                let mut d = DummyWriter(0);
                let _ = write!(
                    d,
                    concat!("{:.prec$", $ty, "}"),
                    $data,
                    prec = $precision as usize
                );
                if d.0 + 1 > $width as usize {
                    $width = d.0 as i32 + 1;
                }
            }
            write!(
                $w,
                concat!("{:width$.prec$", $ty, "}"),
                $data,
                width = $width as usize,
                prec = $precision as usize
            )
        }
    }};
}

macro_rules! define_unumeric {
    ($w: expr, $data: expr, $flags: expr, $width: expr, $precision: expr) => {
        define_unumeric!($w, $data, $flags, $width, $precision, "")
    };
    ($w: expr, $data: expr, $flags: expr, $width: expr, $precision: expr, $ty:expr) => {{
        if $flags.contains(Flags::LEFT_ALIGN) {
            if $flags.contains(Flags::ALTERNATE_FORM) {
                write!(
                    $w,
                    concat!("{:<#width$", $ty, "}"),
                    $data,
                    width = $width as usize
                )
            } else {
                write!(
                    $w,
                    concat!("{:<width$", $ty, "}"),
                    $data,
                    width = $width as usize
                )
            }
        } else if $flags.contains(Flags::ALTERNATE_FORM) {
            if $flags.contains(Flags::PREPEND_ZERO) {
                write!(
                    $w,
                    concat!("{:#0width$", $ty, "}"),
                    $data,
                    width = $width as usize
                )
            } else {
                write!(
                    $w,
                    concat!("{:#width$", $ty, "}"),
                    $data,
                    width = $width as usize
                )
            }
        } else if $flags.contains(Flags::PREPEND_ZERO) {
            write!(
                $w,
                concat!("{:0width$", $ty, "}"),
                $data,
                width = $width as usize
            )
        } else {
            write!(
                $w,
                concat!("{:width$", $ty, "}"),
                $data,
                width = $width as usize
            )
        }
    }};
}

/// Write to a struct that implements [`fmt::Write`].
///
/// # Differences
///
/// There are a few differences from standard printf format:
///
/// - only valid UTF-8 data can be printed.
/// - an `X` format specifier with a `#` flag prints the hex data in uppercase,
///   but the leading `0x` is still lowercase.
/// - an `o` format specifier with a `#` flag precedes the number with an `o`
///   instead of `0`.
/// - `g`/`G` (shorted floating point) is aliased to `f`/`F`` (decimal floating
///   point).
/// - same for `a`/`A` (hex floating point).
/// - the `n` format specifier, [`Specifier::WriteBytesWritten`], is not
///   implemented and will cause an error if encountered.
/// - precision is ignored for integral types, instead of specifying the
///   minimum number of digits.
pub fn fmt_write(argm: Argument, w: &mut impl fmt::Write) -> c_int {
    use fmt::Write;
    let Argument { flags, mut width, precision, specifier } = argm;
    {
        let mut w = WriteCounter(w, 0);
        let w = &mut w;
        let res = match specifier {
            Specifier::Percent => w.write_char('%'),
            Specifier::Bytes(data) => write_str(w, flags, width, precision, data),
            Specifier::String(data) => write_str(w, flags, width, precision, data.as_bytes()),
            Specifier::Hex(data) => {
                define_unumeric!(w, data, flags, width, precision.unwrap_or(0), "x")
            }
            Specifier::UpperHex(data) => {
                define_unumeric!(w, data, flags, width, precision.unwrap_or(0), "X")
            }
            Specifier::Octal(data) => {
                define_unumeric!(w, data, flags, width, precision.unwrap_or(0), "o")
            }
            Specifier::Uint(data) => {
                define_unumeric!(w, data, flags, width, precision.unwrap_or(0))
            }
            Specifier::Int(data) => define_numeric!(w, data, flags, width, precision.unwrap_or(0)),
            Specifier::Double { value, format } => match format {
                DoubleFormat::Normal
                | DoubleFormat::UpperNormal
                | DoubleFormat::Auto
                | DoubleFormat::UpperAuto
                | DoubleFormat::Hex
                | DoubleFormat::UpperHex => {
                    define_numeric!(w, value, flags, width, precision.unwrap_or(6))
                }
                DoubleFormat::Scientific => {
                    define_numeric!(w, value, flags, width, precision.unwrap_or(6), "e")
                }
                DoubleFormat::UpperScientific => {
                    define_numeric!(w, value, flags, width, precision.unwrap_or(6), "E")
                }
            },
            Specifier::Char(data) => {
                if flags.contains(Flags::LEFT_ALIGN) {
                    write!(w, "{:width$}", data as u8 as char, width = width as usize)
                } else {
                    write!(w, "{:>width$}", data as u8 as char, width = width as usize)
                }
            }
            /* NOTE not doing these for now
            Specifier::Pointer(data) => {
                if flags.contains(Flags::LEFT_ALIGN) {
                    write!(w, "{:<width$p}", data, width = width as usize)
                } else if flags.contains(Flags::PREPEND_ZERO) {
                    write!(w, "{:0width$p}", data, width = width as usize)
                } else {
                    write!(w, "{:width$p}", data, width = width as usize)
                }
            }
            Specifier::WriteBytesWritten(_, _) => Err(Default::default()),
            */
        };
        match res {
            Ok(_) => w.1 as c_int,
            Err(_) => -1,
        }
    }
}
