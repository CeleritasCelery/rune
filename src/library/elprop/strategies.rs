#![allow(dead_code)]
use num_bigint::{BigInt, Sign};
use prop::collection::VecStrategy;
use proptest::prelude::*;

const SYMBOL_CHARS: &str = "[a-zA-Z][a-zA-Z0-9-]*";
const MAX_FIXNUM: i64 = i64::MAX >> 8;
const MIN_FIXNUM: i64 = i64::MIN >> 8;

impl std::fmt::Display for ArbitraryType {
    #[expect(clippy::too_many_lines)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use std::string::ToString;
        match self {
            ArbitraryType::String(s) => {
                write!(f, "\"")?;
                for c in s.chars() {
                    match c {
                        '\0' => write!(f, "\\\\0")?,
                        '\n' => write!(f, "\\n")?,
                        '\t' => write!(f, "\\t")?,
                        '\r' => write!(f, "\\r")?,
                        '\\' => write!(f, "\\\\")?,
                        '\"' => write!(f, "\\\"")?,
                        c => write!(f, "{c}")?,
                    }
                }
                write!(f, "\"")
            }
            ArbitraryType::Float(n) => {
                if n.fract() == 0.0_f64 {
                    write!(f, "{n:.1}")
                } else {
                    write!(f, "{n}")
                }
            }
            ArbitraryType::Cons(list) => {
                let mut cells: Vec<_> = list.0.iter().map(ToString::to_string).collect();
                let len = list.0.len();
                let dot_end = list.1;
                if dot_end && len >= 2 {
                    cells.insert(len - 1, ".".to_owned());
                }
                let string = cells.join(" ");
                write!(f, "'({string})")
            }
            ArbitraryType::Symbol(s) => write!(f, "'{s}"),
            ArbitraryType::Integer(n) => write!(f, "{n}"),
            ArbitraryType::Boolean(b) => {
                if *b {
                    write!(f, "t")
                } else {
                    write!(f, "nil")
                }
            }
            ArbitraryType::Unknown(obj) => write!(f, "{obj}"),
            ArbitraryType::UnibyteString(s) => {
                write!(f, "\"")?;
                for c in s.chars() {
                    match c {
                        '\n' => write!(f, "\\n")?,
                        '\t' => write!(f, "\\t")?,
                        '\r' => write!(f, "\\r")?,
                        '\\' => write!(f, "\\\\")?,
                        '"' => write!(f, "\\\"")?,
                        c => write!(f, "{c}")?,
                    }
                }
                write!(f, "\"")
            }
            ArbitraryType::Nil => write!(f, "nil"),
            ArbitraryType::Vector(vec) => {
                let cells: Vec<_> = vec.iter().map(ToString::to_string).collect();
                let string = cells.join(" ");
                write!(f, "[{string}]")
            }
            ArbitraryType::HashTable(vec) => {
                write!(f, "#s(hash-table data (")?;
                for (key, value) in vec {
                    write!(f, "{key} {value} ")?;
                }
                write!(f, "))")
            }
            ArbitraryType::Record((name, members)) => {
                let cells: Vec<_> = members.iter().map(ToString::to_string).collect();
                let string = cells.join(" ");
                write!(f, "(record '{name} {string})")
            }
            ArbitraryType::Function(arity) => {
                write!(f, "(lambda (")?;
                for i in 0..*arity {
                    write!(f, "arg{i} ")?;
                }
                write!(f, ") nil)")
            }
            ArbitraryType::ByteFn(arity) => {
                write!(f, "(lambda (")?;
                for i in 0..*arity {
                    write!(f, "arg{i} ")?;
                }
                write!(f, ") nil)")
            }
            ArbitraryType::Byte(n) => write!(f, "{n}"),
            ArbitraryType::Buffer(name) => {
                write!(f, "(generate-new-buffer {name})")
            }
            ArbitraryType::Subr(arity) => {
                write!(f, "(lambda (")?;
                for i in 0..*arity {
                    write!(f, "arg{i} ")?;
                }
                write!(f, ") nil)")
            }
            ArbitraryType::Char(chr) => match chr {
                '\n' => write!(f, "?\\n"),
                '\t' => write!(f, "?\\t"),
                '\r' => write!(f, "?\\r"),
                '\u{0B}' => write!(f, "?\\v"),
                '\u{0C}' => write!(f, "?\\f"),
                '\u{1B}' => write!(f, "?\\e"),
                '\u{7F}' => write!(f, "?\\d"),
                '\u{08}' => write!(f, "?\\b"),
                '\u{07}' => write!(f, "?\\a"),
                '(' | ')' | '[' | ']' | '\\' | '"' => write!(f, "?\\{chr}"),
                chr => write!(f, "?{chr}"),
            },
            ArbitraryType::BigInt(n) => write!(f, "{n}"),
        }
    }
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
pub(crate) enum ArbitraryType {
    String(String),
    Float(f64),
    Cons((Vec<ArbitraryType>, bool)),
    Symbol(String),
    Integer(i64),
    Boolean(bool),
    Unknown(Box<ArbitraryType>),
    UnibyteString(String),
    Vector(Vec<ArbitraryType>),
    HashTable(Vec<(ArbitraryType, ArbitraryType)>),
    Record((String, Vec<ArbitraryType>)),
    Nil,
    Function(u8),
    ByteFn(u8),
    Byte(u8),
    Char(char),
    Buffer(String),
    Subr(u8),
    BigInt(BigInt),
}

pub(crate) fn arb_custom_string(string: &str) -> BoxedStrategy<ArbitraryType> {
    proptest::string::string_regex(string)
        .expect("Invalid proptest regex")
        .prop_map(ArbitraryType::String)
        .boxed()
}

pub(crate) fn arb_byte() -> BoxedStrategy<ArbitraryType> {
    any::<u8>().prop_map(ArbitraryType::Byte).boxed()
}

pub(crate) fn arb_string() -> BoxedStrategy<ArbitraryType> {
    any::<String>().prop_map(ArbitraryType::String).boxed()
}

pub(crate) fn arb_float() -> BoxedStrategy<ArbitraryType> {
    any::<f64>().prop_map(ArbitraryType::Float).boxed()
}

pub(crate) fn arb_cons() -> BoxedStrategy<ArbitraryType> {
    cons_strategy().prop_map(ArbitraryType::Cons).boxed()
}

pub(crate) fn arb_symbol() -> BoxedStrategy<ArbitraryType> {
    SYMBOL_CHARS.prop_map(ArbitraryType::Symbol).boxed()
}

pub(crate) fn arb_integer() -> BoxedStrategy<ArbitraryType> {
    fixnum_strategy().prop_map(ArbitraryType::Integer).boxed()
}

pub(crate) fn arb_posinteger() -> BoxedStrategy<ArbitraryType> {
    pos_fixnum_strategy().prop_map(ArbitraryType::Integer).boxed()
}

pub(crate) fn arb_boolean() -> BoxedStrategy<ArbitraryType> {
    any::<bool>().prop_map(ArbitraryType::Boolean).boxed()
}

pub(crate) fn arb_true() -> BoxedStrategy<ArbitraryType> {
    Just(true).prop_map(ArbitraryType::Boolean).boxed()
}

pub(crate) fn arb_false() -> BoxedStrategy<ArbitraryType> {
    Just(false).prop_map(ArbitraryType::Boolean).boxed()
}

pub(crate) fn arb_unknown() -> BoxedStrategy<ArbitraryType> {
    self::any_object_strategy()
}

pub(crate) fn arb_bytestring() -> BoxedStrategy<ArbitraryType> {
    "[a-zA-Z0-9 ]*".prop_map(ArbitraryType::UnibyteString).boxed()
}

pub(crate) fn arb_vector() -> BoxedStrategy<ArbitraryType> {
    prop::collection::vec(any_object_strategy(), 0..10)
        .prop_map(ArbitraryType::Vector)
        .boxed()
}

pub(crate) fn arb_record() -> BoxedStrategy<ArbitraryType> {
    (SYMBOL_CHARS, prop::collection::vec(any_object_strategy(), 0..10))
        .prop_map(ArbitraryType::Record)
        .boxed()
}

pub(crate) fn arb_hashtable() -> BoxedStrategy<ArbitraryType> {
    todo!("Strategy for HashTable not implemented")
}

pub(crate) fn arb_bytefn() -> BoxedStrategy<ArbitraryType> {
    any::<u8>().prop_map(ArbitraryType::ByteFn).boxed()
}

pub(crate) fn arb_subr() -> BoxedStrategy<ArbitraryType> {
    todo!("Strategy for Subr not implemented")
}

pub(crate) fn arb_buffer() -> BoxedStrategy<ArbitraryType> {
    any::<String>().prop_map(ArbitraryType::Buffer).boxed()
}

pub(crate) fn arb_nil() -> BoxedStrategy<ArbitraryType> {
    Just(ArbitraryType::Nil).boxed()
}

pub(crate) fn arb_char() -> BoxedStrategy<ArbitraryType> {
    any::<char>().prop_map(ArbitraryType::Char).boxed()
}

pub(crate) fn arb_chartable() -> BoxedStrategy<ArbitraryType> {
    todo!("Strategy for CharTable not implemented")
}

pub(crate) fn arb_function() -> BoxedStrategy<ArbitraryType> {
    todo!("Strategy for Function not implemented")
}

pub(crate) fn arb_custom_list() -> BoxedStrategy<ArbitraryType> {
    todo!(
        "Does it make sense to have such strategy now, when it's possible to use standard helpers?"
    );
    // let arb_list: Vec<_> = list.iter().map(|x| x.clone().strategy()).collect();
    // (arb_list, Just(false)).prop_map(ArbitraryType::Cons).boxed()
}

pub(crate) fn arb_bigint() -> BoxedStrategy<ArbitraryType> {
    big_int_strategy().prop_map(ArbitraryType::BigInt).boxed()
}

fn cons_strategy() -> (VecStrategy<BoxedStrategy<ArbitraryType>>, BoxedStrategy<bool>) {
    (
        prop::collection::vec(any_object_strategy(), 0..10),
        prop_oneof![
            1 => Just(true),
            3 => Just(false),
        ]
        .boxed(),
    )
}

fn fixnum_strategy() -> BoxedStrategy<i64> {
    any::<i64>()
        .prop_filter("Fixnum", |x| *x >= MIN_FIXNUM && *x <= MAX_FIXNUM)
        .boxed()
}

fn pos_fixnum_strategy() -> BoxedStrategy<i64> {
    any::<i64>().prop_filter("Fixnum", |x| *x >= 0 && *x <= MAX_FIXNUM).boxed()
}

fn big_int_strategy() -> BoxedStrategy<BigInt> {
    (any::<bool>(), any::<Vec<u32>>())
        .prop_map(|(s, v)| {
            let sign = if s { Sign::Plus } else { Sign::Minus };
            BigInt::from_slice(sign, &v)
        })
        .boxed()
}

pub(crate) fn any_object_strategy() -> BoxedStrategy<ArbitraryType> {
    prop_oneof![
        Just(ArbitraryType::Nil),
        any::<bool>().prop_map(ArbitraryType::Boolean),
        fixnum_strategy().prop_map(ArbitraryType::Integer),
        any::<f64>().prop_map(ArbitraryType::Float),
        any::<String>().prop_map(ArbitraryType::String),
        SYMBOL_CHARS.prop_map(ArbitraryType::Symbol),
        "[a-zA-Z0-9 ]*".prop_map(ArbitraryType::UnibyteString),
        any::<char>().prop_map(ArbitraryType::Char),
    ]
    .boxed()
}
