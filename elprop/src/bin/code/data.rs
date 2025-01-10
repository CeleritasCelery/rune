use proc_macro2::{TokenStream, TokenTree};
use prop::collection::VecStrategy;
use proptest::prelude::*;
use serde::{Deserialize, Serialize};
use syn::{FnArg, ItemFn};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub(crate) enum Type {
    String,
    Float,
    Cons,
    Symbol,
    Integer,
    PosInteger,
    Boolean,
    Unknown,
    Function,
    UnibyteString,
    Vector,
    HashTable,
    Record,
    ByteFn,
    Subr,
    Buffer,
    Nil,
    Char,
    CustomString(String),
    Multiple(Vec<Type>),
    CustomList(Vec<Type>),
}

impl Type {
    const SYMBOL_CHARS: &'static str = "[a-zA-Z][a-zA-Z0-9-]*";
    const MAX_FIXNUM: i64 = i64::MAX >> 8;
    const MIN_FIXNUM: i64 = i64::MIN >> 8;
    // New function to create a strategy for a specific type
    fn strategy(self) -> BoxedStrategy<ArbitraryType> {
        match self {
            Type::String => any::<String>().prop_map(ArbitraryType::String).boxed(),
            Type::Float => any::<f64>().prop_map(ArbitraryType::Float).boxed(),
            Type::Cons => Self::cons_strategy().prop_map(ArbitraryType::Cons).boxed(),
            Type::Symbol => Self::SYMBOL_CHARS.prop_map(ArbitraryType::Symbol).boxed(),
            Type::Integer => Self::fixnum_strategy().prop_map(ArbitraryType::Integer).boxed(),
            Type::PosInteger => {
                Self::pos_fixnum_strategy().prop_map(ArbitraryType::Integer).boxed()
            }
            Type::Boolean => any::<bool>().prop_map(ArbitraryType::Boolean).boxed(),
            Type::Unknown => Self::any_object_strategy(),
            Type::UnibyteString => "[a-zA-Z0-9 ]*".prop_map(ArbitraryType::UnibyteString).boxed(),
            Type::Vector => prop::collection::vec(Self::any_object_strategy(), 0..10)
                .prop_map(ArbitraryType::Vector)
                .boxed(),
            Type::Record => {
                (Self::SYMBOL_CHARS, prop::collection::vec(Self::any_object_strategy(), 0..10))
                    .prop_map(ArbitraryType::Record)
                    .boxed()
            }
            Type::HashTable => todo!("Strategy for HashTable not implemented"),
            Type::ByteFn => any::<u8>().prop_map(ArbitraryType::ByteFn).boxed(),
            Type::Subr => todo!("Strategy for Subr not implemented"),
            Type::Buffer => any::<String>().prop_map(ArbitraryType::Buffer).boxed(),
            Type::Nil => Just(ArbitraryType::Nil).boxed(),
            Type::Char => any::<char>().prop_map(ArbitraryType::Char).boxed(),
            Type::Function => todo!("Strategy for Function not implemented"),
            Type::Multiple(s) => combined_strategy(&s),
            Type::CustomString(s) => proptest::string::string_regex(&s)
                .expect("Invalid proptest regex")
                .prop_map(ArbitraryType::String)
                .boxed(),
            Type::CustomList(list) => {
                let arb_list: Vec<_> = list.iter().map(|x| x.clone().strategy()).collect();
                (arb_list, Just(false)).prop_map(ArbitraryType::Cons).boxed()
            }
        }
    }

    fn fixnum_strategy() -> BoxedStrategy<i64> {
        any::<i64>()
            .prop_filter("Fixnum", |x| *x >= Self::MIN_FIXNUM && *x <= Self::MAX_FIXNUM)
            .boxed()
    }

    fn pos_fixnum_strategy() -> BoxedStrategy<i64> {
        any::<i64>()
            .prop_filter("Fixnum", |x| *x >= 0 && *x <= Self::MAX_FIXNUM)
            .boxed()
    }

    fn cons_strategy() -> (VecStrategy<BoxedStrategy<ArbitraryType>>, BoxedStrategy<bool>) {
        (
            prop::collection::vec(Self::any_object_strategy(), 0..10),
            prop_oneof![
                1 => Just(true),
                3 => Just(false),
            ]
            .boxed(),
        )
    }

    pub(crate) fn any_object_strategy() -> BoxedStrategy<ArbitraryType> {
        prop_oneof![
            Just(ArbitraryType::Nil),
            any::<bool>().prop_map(ArbitraryType::Boolean),
            Self::fixnum_strategy().prop_map(ArbitraryType::Integer),
            any::<f64>().prop_map(ArbitraryType::Float),
            any::<String>().prop_map(ArbitraryType::String),
            Self::SYMBOL_CHARS.prop_map(ArbitraryType::Symbol),
            "[a-zA-Z0-9 ]*".prop_map(ArbitraryType::UnibyteString),
            any::<char>().prop_map(ArbitraryType::Char),
        ]
        .boxed()
    }
}

// New function to create a combined strategy from multiple types
pub(crate) fn combined_strategy(types: &[Type]) -> BoxedStrategy<ArbitraryType> {
    // Combine all strategies using prop_oneof!
    match types.len() {
        0 => panic!("At least one type must be provided"),
        1 => types[0].clone().strategy(),
        2 => prop_oneof![types[0].clone().strategy(), types[1].clone().strategy()].boxed(),
        3 => prop_oneof![
            types[0].clone().strategy(),
            types[1].clone().strategy(),
            types[2].clone().strategy()
        ]
        .boxed(),
        4 => prop_oneof![
            types[0].clone().strategy(),
            types[1].clone().strategy(),
            types[2].clone().strategy(),
            types[3].clone().strategy()
        ]
        .boxed(),
        5 => prop_oneof![
            types[0].clone().strategy(),
            types[1].clone().strategy(),
            types[2].clone().strategy(),
            types[3].clone().strategy(),
            types[4].clone().strategy()
        ]
        .boxed(),
        n => panic!("Currently supporting up to 5 combined types, got {n}"),
    }
}

#[derive(Clone, PartialEq, PartialOrd, Debug, Serialize, Deserialize)]
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
    Char(char),
    Buffer(String),
    Subr(u8),
}

pub(crate) fn print_args(args: &[Option<ArbitraryType>]) -> String {
    args.iter()
        .map(|x| match x {
            Some(x) => format!("{x}"),
            None => "nil".to_owned(),
        })
        .collect::<Vec<_>>()
        .join(" ")
}

impl std::fmt::Display for ArbitraryType {
    #[expect(clippy::too_many_lines)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use std::string::ToString;
        match self {
            ArbitraryType::String(s) => {
                write!(f, "\"")?;
                for c in s.chars() {
                    match c {
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
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) enum ArgType {
    Required(Type),
    Optional(Type),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct Function {
    pub(crate) name: String,
    pub(crate) args: Vec<ArgType>,
    pub(crate) ret: Option<Type>,
    pub(crate) fallible: bool,
}

#[derive(Debug, Serialize, Deserialize)]
pub(crate) struct Config {
    pub(crate) test_count: u32,
    pub(crate) functions: Vec<Function>,
}

#[allow(dead_code)]
impl Function {
    pub(crate) fn strategy(self) -> Vec<BoxedStrategy<Option<ArbitraryType>>> {
        self.args
            .into_iter()
            .map(|arg| match arg {
                ArgType::Required(ty) => ty.strategy().prop_map(Some).boxed(),
                ArgType::Optional(ty) => {
                    prop_oneof![1 => Just(None), 3 => ty.strategy().prop_map(Some)].boxed()
                }
            })
            .collect::<Vec<_>>()
    }

    fn process_arg(ty: &syn::Type) -> Result<Type, String> {
        match ty {
            syn::Type::Array(_) => Err("Array not supported".to_string()),
            syn::Type::BareFn(_) => Err("BareFn not supported".to_string()),
            syn::Type::ImplTrait(_) => Err("Impl Trait not supported".to_string()),
            syn::Type::Infer(_) => Err("Infer not supported".to_string()),
            syn::Type::Macro(_) => Err("Macro not supported".to_string()),
            syn::Type::Never(_) => Err("Never not supported".to_string()),
            syn::Type::Paren(_) => Err("Paren not supported".to_string()),
            syn::Type::Path(syn::TypePath { path, .. }) => {
                let segments = &path.segments;
                let last = segments.last().unwrap().ident.to_string();

                match Self::get_simple_type(&last) {
                    Some(x) => Ok(x),
                    None => match last.as_str() {
                        "Rto" | "Rt" | "Gc" => Self::process_arg(get_generic_arg(segments)?),
                        s @ ("Env" | "Context") => Err(format!("{s} type not supported")),
                        s => Err(format!("Unknown type: {s}")),
                    },
                }
            }
            syn::Type::Ptr(_) => Err("Ptr not supported".to_string()),
            syn::Type::Reference(syn::TypeReference { elem, .. })
            | syn::Type::Group(syn::TypeGroup { elem, .. }) => Function::process_arg(elem),
            syn::Type::Slice(syn::TypeSlice { .. }) => Ok(Type::Cons),
            syn::Type::TraitObject(_) => Err("TraitObject not supported".to_string()),
            syn::Type::Tuple(_) => Ok(Type::Nil),
            syn::Type::Verbatim(_) => Err("Verbatim type not supported".to_string()),
            _ => Err("Unknown type".to_string()),
        }
    }

    fn get_simple_type(ty: &str) -> Option<Type> {
        Some(match ty {
            "StringOrChar" => Type::Multiple(vec![Type::String, Type::Char]),
            "StringOrSymbol" => Type::Multiple(vec![Type::String, Type::Symbol]),
            "Symbol" => Type::Symbol,
            "Number" | "NumberValue" => Type::Multiple(vec![Type::Integer, Type::Float]),
            "Object" => Type::Unknown,
            "usize" | "u64" => Type::PosInteger,
            "isize" | "i64" => Type::Integer,
            "str" | "String" | "LispString" => Type::String,
            "bool" | "OptionalFlag" => Type::Boolean,
            "f64" => Type::Float,
            "char" => Type::Char,
            "Function" => Type::Function,
            "Cons" | "List" | "Error" | "TypeError" | "ArgSlice" => Type::Cons,
            "LispVec" | "LispVector" | "Vec" | "RecordBuilder" => Type::Vector,
            "LispHashTable" => Type::HashTable,
            "ByteString" => Type::UnibyteString,
            "Record" => Type::Record,
            "ByteFn" => Type::ByteFn,
            "SubrFn" => Type::Subr,
            "LispBuffer" => Type::Buffer,
            _ => return None,
        })
    }

    fn custom_templates(func: &ItemFn) -> Vec<Option<Type>> {
        for attr in &func.attrs {
            if let syn::Meta::List(list) = &attr.meta {
                if list.path.get_ident().unwrap() == "elprop" {
                    let custom_args = Self::parse_template_stream(list.tokens.clone());
                    return custom_args
                        .into_iter()
                        .map(|x| match x {
                            Type::Nil => None,
                            x => Some(x),
                        })
                        .collect();
                }
            }
        }
        Vec::new()
    }

    fn parse_template_stream(ts: TokenStream) -> Vec<Type> {
        let objects = ts.into_iter().fold(vec![Vec::new()], |mut acc, token| {
            // group by type (a, b | c, e) -> [a [b c] e]
            match token {
                TokenTree::Punct(punct) => match punct.as_char() {
                    ',' => acc.push(Vec::new()),
                    '|' => {}
                    c => panic!("Unexpected punctuation {c}"),
                },
                x => acc.last_mut().unwrap().push(Self::parse_template_type(x)),
            }
            acc
        });
        // combine all groups of types into a MultiType
        objects
            .into_iter()
            .map(|mut x| if x.len() == 1 { x.pop().unwrap() } else { Type::Multiple(x) })
            .collect()
    }

    fn parse_template_type(token: TokenTree) -> Type {
        match token {
            TokenTree::Group(group) => {
                Type::CustomList(Self::parse_template_stream(group.stream()))
            }
            TokenTree::Ident(ident) => {
                let s = ident.to_string();
                match Self::get_simple_type(&s) {
                    Some(x) => x,
                    None => match &*s {
                        "_" => Type::Nil,
                        s => panic!("Unknown type {s}"),
                    },
                }
            }
            TokenTree::Literal(literal) => {
                let string = syn::parse_str::<syn::LitStr>(&literal.to_string())
                    .expect("Invalid Literal")
                    .value();
                if let Err(e) = proptest::string::string_regex(&string) {
                    panic!("{e}: {string}")
                }
                Type::CustomString(string)
            }
            TokenTree::Punct(p) => {
                unreachable!("Unexpected punctuation {p}")
            }
        }
    }

    pub(crate) fn from_item(item: &ItemFn) -> Result<Self, String> {
        let name = item
            .sig
            .ident
            .to_string()
            .chars()
            .map(|c| match c {
                '_' => '-',
                c => c,
            })
            .collect();

        let args = Function::get_args(item);

        let (ret, fallible) = Self::get_output(item)?;
        Ok(Function { name, args, ret, fallible })
    }

    fn get_args(item: &ItemFn) -> Vec<ArgType> {
        let templates = Self::custom_templates(item);

        item.sig
            .inputs
            .iter()
            .map(|x| match x {
                FnArg::Receiver(syn::Receiver { ty, .. })
                | FnArg::Typed(syn::PatType { ty, .. }) => ty,
            })
            .enumerate()
            .filter_map(|(i, arg)| {
                // If a custom template is specified, use it
                if let Some(Some(template)) = templates.get(i) {
                    return Some(ArgType::Required(template.clone()));
                }
                match arg.as_ref() {
                    syn::Type::Group(syn::TypeGroup { group_token, elem }) => {
                        let syn::token::Group { span } = group_token;

                        let source_text = span
                            .source_text()
                            .ok_or_else(|| "Failed to get source text".to_string())
                            .ok()?;
                        let optional = matches!(source_text.as_str(), "Option");
                        Self::wrap_arg(optional, elem)
                    }
                    syn::Type::Path(syn::TypePath { path, .. }) => {
                        let segments = &path.segments;
                        let last = segments.last().unwrap().ident.to_string();
                        if matches!(last.as_str(), "Result" | "Option") {
                            let generic_arg = get_generic_arg(segments).ok()?;
                            Self::wrap_arg(true, generic_arg)
                        } else {
                            Self::wrap_arg(false, arg)
                        }
                    }
                    x => Some(ArgType::Required(Function::process_arg(x).ok()?)),
                }
            })
            .collect()
    }

    fn wrap_arg(optional: bool, ty: &syn::Type) -> Option<ArgType> {
        let arg = Function::process_arg(ty).ok()?;
        match arg {
            Type::Boolean => Some(ArgType::Required(arg)),
            x => Some(if optional { ArgType::Optional(x) } else { ArgType::Required(x) }),
        }
    }

    fn get_output(item: &ItemFn) -> Result<(Option<Type>, bool), String> {
        Ok(match &item.sig.output {
            syn::ReturnType::Default => (None, false),
            syn::ReturnType::Type(_, ty) => match ty.as_ref() {
                syn::Type::Group(syn::TypeGroup { group_token, elem }) => {
                    let syn::token::Group { span } = group_token;

                    let source_text = span
                        .source_text()
                        .ok_or_else(|| "Failed to get source text".to_string())?;
                    let fallible = matches!(source_text.as_str(), "Option" | "Result");

                    let ty = Function::process_arg(elem)?;
                    (Some(ty), fallible)
                }
                syn::Type::Path(syn::TypePath { path, .. }) => {
                    let segments = &path.segments;
                    let last = segments.last().unwrap().ident.to_string();
                    match last.as_str() {
                        "Result" | "Option" => {
                            let ty = get_generic_arg(segments)?;
                            let ty = Function::process_arg(ty)?;
                            (Some(ty), true)
                        }
                        _ => {
                            let ty = Function::process_arg(ty)?;
                            (Some(ty), false)
                        }
                    }
                }
                x => {
                    let ty = Function::process_arg(x)?;
                    (Some(ty), false)
                }
            },
        })
    }
}

fn get_generic_arg<'a>(
    segments: impl IntoIterator<Item = &'a syn::PathSegment>,
) -> Result<&'a syn::Type, String> {
    let syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
        ref args, ..
    }) = segments.into_iter().last().unwrap().arguments
    else {
        unreachable!("Expected angle bracketed arguments");
    };

    args.iter()
        .fold(None, |acc, arg| match arg {
            syn::GenericArgument::Type(ty) => Some(ty),
            _ => acc,
        })
        .ok_or_else(|| "Expected type argument".to_string())
}
