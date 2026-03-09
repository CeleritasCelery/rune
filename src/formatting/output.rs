//! Various ways to output formatting data.

use core::cell::Cell;
use core::ffi::*;
use core::fmt;
use core::str::from_utf8;

#[cfg(feature = "std")]
pub use yes_std::*;

use crate::{Argument, DoubleFormat, Flags, Specifier};

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
pub fn fmt_write(w: &mut impl fmt::Write) -> impl FnMut(Argument) -> c_int + '_ {
    use fmt::Write;
    move |Argument {
              flags,
              mut width,
              precision,
              specifier,
          }| {
        let mut w = WriteCounter(w, 0);
        let w = &mut w;
        let res = match specifier {
            Specifier::Percent => w.write_char('%'),
            Specifier::Bytes(data) => write_str(w, flags, width, precision, data),
            Specifier::String(data) => write_str(w, flags, width, precision, data.to_bytes()),
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
        };
        match res {
            Ok(_) => w.1 as c_int,
            Err(_) => -1,
        }
    }
}

/// Returns an object that implements [`Display`][fmt::Display] for safely
/// printing formatting data. This is slightly less performant than using
/// [`fmt_write`], but may be the only option.
///
/// This shares the same caveats as [`fmt_write`].
///
/// # Safety
///
/// [`VaList`]s are *very* unsafe. The passed `format` and `args` parameter must be a valid [`printf` format string](http://www.cplusplus.com/reference/cstdio/printf/).
pub unsafe fn display<'a>(format: *const c_char, va_list: VaList<'a>) -> VaListDisplay<'a> {
    VaListDisplay {
        format,
        va_list,
        written: Cell::new(0),
    }
}

/// Helper struct created by [`display`] for safely printing `printf`-style
/// formatting with [`format!`] and `{}`. This can be used with anything that
/// uses [`format_args!`], such as [`println!`] or the `log` crate.
///
/// ```rust
/// #![feature(c_variadic)]
///
/// use core::ffi::{c_char, c_int};
///
/// #[unsafe(no_mangle)]
/// unsafe extern "C" fn c_library_print(str: *const c_char, args: ...) -> c_int {
///     let format = unsafe { printf_compat::output::display(str, args) };
///     println!("{}", format);
///     format.bytes_written()
/// }
/// ```
pub struct VaListDisplay<'a> {
    format: *const c_char,
    va_list: VaList<'a>,
    written: Cell<c_int>,
}

impl VaListDisplay<'_> {
    /// Get the number of bytes written, or 0 if there was an error.
    pub fn bytes_written(&self) -> c_int {
        self.written.get()
    }
}

impl<'a> fmt::Display for VaListDisplay<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        unsafe {
            let bytes = crate::format(self.format, self.va_list.clone(), fmt_write(f));
            self.written.set(bytes);
            if bytes < 0 { Err(fmt::Error) } else { Ok(()) }
        }
    }
}

#[cfg(feature = "std")]
mod yes_std {
    use std::io;

    use super::*;

    struct FmtWriter<T: io::Write>(T, io::Result<()>);

    impl<T: io::Write> fmt::Write for FmtWriter<T> {
        fn write_str(&mut self, s: &str) -> fmt::Result {
            match self.0.write_all(s.as_bytes()) {
                Ok(()) => Ok(()),
                Err(e) => {
                    self.1 = Err(e);
                    Err(fmt::Error)
                }
            }
        }
    }

    struct IoWriteCounter<'a, T: io::Write>(&'a mut T, usize);

    impl<'a, T: io::Write> io::Write for IoWriteCounter<'a, T> {
        fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
            self.0.write_all(buf)?;
            self.1 += buf.len();
            Ok(buf.len())
        }

        fn flush(&mut self) -> io::Result<()> {
            self.0.flush()
        }
    }

    fn write_bytes(
        w: &mut impl io::Write,
        flags: Flags,
        width: c_int,
        precision: Option<c_int>,
        b: &[u8],
    ) -> io::Result<()> {
        let precision = precision.unwrap_or(b.len() as c_int);
        let b = b.get(..(b.len().min(precision as usize))).unwrap_or(&[]);

        if flags.contains(Flags::LEFT_ALIGN) {
            w.write_all(b)?;
            for _ in 0..((width as usize).saturating_sub(b.len())) {
                w.write_all(b" ")?;
            }
            Ok(())
        } else {
            for _ in 0..((width as usize).saturating_sub(b.len())) {
                w.write_all(b" ")?;
            }
            w.write_all(b)
        }
    }

    /// Write to a struct that implements [`io::Write`].
    ///
    /// This shares the same caveats as [`fmt_write`], except that non-UTF-8
    /// data is supported.
    pub fn io_write(w: &mut impl io::Write) -> impl FnMut(Argument) -> c_int + '_ {
        use io::Write;
        move |Argument {
                  flags,
                  width,
                  precision,
                  specifier,
              }| {
            let mut w = IoWriteCounter(w, 0);
            let mut w = &mut w;
            let res = match specifier {
                Specifier::Percent => w.write_all(b"%"),
                Specifier::Bytes(data) => write_bytes(w, flags, width, precision, data),
                Specifier::String(data) => write_bytes(w, flags, width, precision, data.to_bytes()),
                _ => {
                    let mut writer = FmtWriter(&mut w, Ok(()));
                    fmt_write(&mut writer)(Argument {
                        flags,
                        width,
                        precision,
                        specifier,
                    });
                    writer.1
                }
            };
            match res {
                Ok(_) => w.1 as c_int,
                Err(_) => -1,
            }
        }
    }
}
