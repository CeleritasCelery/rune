use core::{ffi::*, fmt};

pub mod output;
mod parser;
use argument::*;
pub use parser::format;
pub mod argument {
    use super::*;

    bitflags::bitflags! {
        /// Flags field.
        ///
        /// Definitions from
        /// [Wikipedia](https://en.wikipedia.org/wiki/Printf_format_string#Flags_field).
        #[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash)]
        pub struct Flags: u8 {
            /// Left-align the output of this placeholder. (The default is to
            /// right-align the output.)
            const LEFT_ALIGN = 0b00000001;
            /// Prepends a plus for positive signed-numeric types. positive =
            /// `+`, negative = `-`.
            ///
            /// (The default doesn't prepend anything in front of positive
            /// numbers.)
            const PREPEND_PLUS = 0b00000010;
            /// Prepends a space for positive signed-numeric types. positive = `
            /// `, negative = `-`. This flag is ignored if the
            /// [`PREPEND_PLUS`][Flags::PREPEND_PLUS] flag exists.
            ///
            /// (The default doesn't prepend anything in front of positive
            /// numbers.)
            const PREPEND_SPACE = 0b00000100;
            /// When the 'width' option is specified, prepends zeros for numeric
            /// types. (The default prepends spaces.)
            ///
            /// For example, `printf("%4X",3)` produces `   3`, while
            /// `printf("%04X",3)` produces `0003`.
            const PREPEND_ZERO = 0b00001000;
            /// The integer or exponent of a decimal has the thousands grouping
            /// separator applied.
            const THOUSANDS_GROUPING = 0b00010000;
            /// Alternate form:
            ///
            /// For `g` and `G` types, trailing zeros are not removed. \
            /// For `f`, `F`, `e`, `E`, `g`, `G` types, the output always
            /// contains a decimal point. \ For `o`, `x`, `X` types,
            /// the text `0`, `0x`, `0X`, respectively, is prepended
            /// to non-zero numbers.
            const ALTERNATE_FORM = 0b00100000;
        }
    }

    #[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
    pub enum DoubleFormat {
        /// `f`
        Normal,
        /// `F`
        UpperNormal,
        /// `e`
        Scientific,
        /// `E`
        UpperScientific,
        /// `g`
        Auto,
        /// `G`
        UpperAuto,
        /// `a`
        Hex,
        /// `A`
        UpperHex,
    }

    impl DoubleFormat {
        /// If the format is uppercase.
        pub fn is_upper(self) -> bool {
            use DoubleFormat::*;
            matches!(self, UpperNormal | UpperScientific | UpperAuto | UpperHex)
        }

        pub fn set_upper(self, upper: bool) -> Self {
            use DoubleFormat::*;
            match self {
                Normal | UpperNormal => {
                    if upper {
                        UpperNormal
                    } else {
                        Normal
                    }
                }
                Scientific | UpperScientific => {
                    if upper {
                        UpperScientific
                    } else {
                        Scientific
                    }
                }
                Auto | UpperAuto => {
                    if upper {
                        UpperAuto
                    } else {
                        Auto
                    }
                }
                Hex | UpperHex => {
                    if upper {
                        UpperHex
                    } else {
                        Hex
                    }
                }
            }
        }
    }

    #[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
    #[non_exhaustive]
    pub enum SignedInt {
        Int(c_int),
        Char(c_schar),
        Short(c_short),
        Long(c_long),
        LongLong(c_longlong),
        Isize(isize),
    }

    impl From<SignedInt> for i64 {
        fn from(num: SignedInt) -> Self {
            // Some casts are only needed on some platforms.
            #[allow(clippy::unnecessary_cast)]
            match num {
                SignedInt::Int(x) => x as i64,
                SignedInt::Char(x) => x as i64,
                SignedInt::Short(x) => x as i64,
                SignedInt::Long(x) => x as i64,
                SignedInt::LongLong(x) => x as i64,
                SignedInt::Isize(x) => x as i64,
            }
        }
    }

    impl SignedInt {
        pub fn is_sign_negative(self) -> bool {
            match self {
                SignedInt::Int(x) => x < 0,
                SignedInt::Char(x) => x < 0,
                SignedInt::Short(x) => x < 0,
                SignedInt::Long(x) => x < 0,
                SignedInt::LongLong(x) => x < 0,
                SignedInt::Isize(x) => x < 0,
            }
        }
    }

    impl fmt::Display for SignedInt {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                SignedInt::Int(x) => fmt::Display::fmt(x, f),
                SignedInt::Char(x) => fmt::Display::fmt(x, f),
                SignedInt::Short(x) => fmt::Display::fmt(x, f),
                SignedInt::Long(x) => fmt::Display::fmt(x, f),
                SignedInt::LongLong(x) => fmt::Display::fmt(x, f),
                SignedInt::Isize(x) => fmt::Display::fmt(x, f),
            }
        }
    }

    #[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
    #[non_exhaustive]
    pub enum UnsignedInt {
        Int(c_uint),
        Char(c_uchar),
        Short(c_ushort),
        Long(c_ulong),
        LongLong(c_ulonglong),
        Isize(usize),
    }

    impl From<UnsignedInt> for u64 {
        fn from(num: UnsignedInt) -> Self {
            // Some casts are only needed on some platforms.
            #[allow(clippy::unnecessary_cast)]
            match num {
                UnsignedInt::Int(x) => x as u64,
                UnsignedInt::Char(x) => x as u64,
                UnsignedInt::Short(x) => x as u64,
                UnsignedInt::Long(x) => x as u64,
                UnsignedInt::LongLong(x) => x as u64,
                UnsignedInt::Isize(x) => x as u64,
            }
        }
    }

    impl fmt::Display for UnsignedInt {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                UnsignedInt::Int(x) => fmt::Display::fmt(x, f),
                UnsignedInt::Char(x) => fmt::Display::fmt(x, f),
                UnsignedInt::Short(x) => fmt::Display::fmt(x, f),
                UnsignedInt::Long(x) => fmt::Display::fmt(x, f),
                UnsignedInt::LongLong(x) => fmt::Display::fmt(x, f),
                UnsignedInt::Isize(x) => fmt::Display::fmt(x, f),
            }
        }
    }

    impl fmt::LowerHex for UnsignedInt {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                UnsignedInt::Int(x) => fmt::LowerHex::fmt(x, f),
                UnsignedInt::Char(x) => fmt::LowerHex::fmt(x, f),
                UnsignedInt::Short(x) => fmt::LowerHex::fmt(x, f),
                UnsignedInt::Long(x) => fmt::LowerHex::fmt(x, f),
                UnsignedInt::LongLong(x) => fmt::LowerHex::fmt(x, f),
                UnsignedInt::Isize(x) => fmt::LowerHex::fmt(x, f),
            }
        }
    }

    impl fmt::UpperHex for UnsignedInt {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                UnsignedInt::Int(x) => fmt::UpperHex::fmt(x, f),
                UnsignedInt::Char(x) => fmt::UpperHex::fmt(x, f),
                UnsignedInt::Short(x) => fmt::UpperHex::fmt(x, f),
                UnsignedInt::Long(x) => fmt::UpperHex::fmt(x, f),
                UnsignedInt::LongLong(x) => fmt::UpperHex::fmt(x, f),
                UnsignedInt::Isize(x) => fmt::UpperHex::fmt(x, f),
            }
        }
    }

    impl fmt::Octal for UnsignedInt {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                UnsignedInt::Int(x) => fmt::Octal::fmt(x, f),
                UnsignedInt::Char(x) => fmt::Octal::fmt(x, f),
                UnsignedInt::Short(x) => fmt::Octal::fmt(x, f),
                UnsignedInt::Long(x) => fmt::Octal::fmt(x, f),
                UnsignedInt::LongLong(x) => fmt::Octal::fmt(x, f),
                UnsignedInt::Isize(x) => fmt::Octal::fmt(x, f),
            }
        }
    }

    /// An argument as passed to [`format()`].
    #[derive(Debug, Copy, Clone, PartialEq)]
    pub struct Argument<'a> {
        pub flags: Flags,
        pub width: c_int,
        pub precision: Option<c_int>,
        pub specifier: Specifier<'a>,
    }

    impl<'a> From<Specifier<'a>> for Argument<'a> {
        fn from(specifier: Specifier<'a>) -> Self {
            Self {
                flags: Flags::empty(),
                width: 0,
                precision: None,
                specifier,
            }
        }
    }

    /// A [format specifier](https://en.wikipedia.org/wiki/Printf_format_string#Type_field).
    #[derive(Debug, Copy, Clone, PartialEq)]
    #[non_exhaustive]
    pub enum Specifier<'a> {
        /// `%`
        Percent,
        /// `d`, `i`
        Int(SignedInt),
        /// `u`
        Uint(UnsignedInt),
        /// `o`
        Octal(UnsignedInt),
        /// `f`, `F`, `e`, `E`, `g`, `G`, `a`, `A`
        Double { value: f64, format: DoubleFormat },
        /// string outside of formatting
        Bytes(&'a [u8]),
        /// `s`
        ///
        /// The same as [`Bytes`][Specifier::Bytes] but guaranteed to be
        /// null-terminated. This can be used for optimizations, where if you
        /// need to null terminate a string to print it, you can skip that step.
        String(&'a CStr),
        /// `c`
        Char(c_char),
        /// `x`
        Hex(UnsignedInt),
        /// `X`
        UpperHex(UnsignedInt),
        /// `p`
        Pointer(*const ()),
        /// `n`
        ///
        /// # Safety
        ///
        /// This can be a serious security vulnerability if the format specifier
        /// of `printf` is allowed to be user-specified. This shouldn't ever
        /// happen, but poorly-written software may do so.
        WriteBytesWritten(c_int, *const c_int),
    }
}
