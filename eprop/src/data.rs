#![allow(unused_qualifications)]
use proptest::prelude::*;
use std::hash::Hash;
use syn::ItemFn;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(crate) enum ObjectType {
    String,
    Float,
    Cons,
    Symbol,
    Integer,
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
}

impl ObjectType {
    const SYMBOL_CHARS: &'static str = "[a-zA-Z][a-zA-Z0-9-]*";
    // New function to create a strategy for a specific type
    fn strategy(self) -> BoxedStrategy<ArbitraryObjectType> {
        match self {
            ObjectType::String => any::<String>().prop_map(ArbitraryObjectType::String).boxed(),
            ObjectType::Float => any::<f64>().prop_map(ArbitraryObjectType::Float).boxed(),
            ObjectType::Cons => todo!("Strategy for Cons not implemented"),
            ObjectType::Symbol => Self::SYMBOL_CHARS.prop_map(ArbitraryObjectType::Symbol).boxed(),
            ObjectType::Integer => any::<i64>().prop_map(ArbitraryObjectType::Integer).boxed(),
            ObjectType::Boolean => any::<bool>().prop_map(ArbitraryObjectType::Boolean).boxed(),
            ObjectType::Unknown => Self::any_object_strategy(),
            ObjectType::UnibyteString => {
                "[a-zA-Z0-9 ]*".prop_map(ArbitraryObjectType::UnibyteString).boxed()
            }
            ObjectType::Vector => todo!("Strategy for Vector not implemented"),
            ObjectType::HashTable => todo!("Strategy for HashTable not implemented"),
            ObjectType::Record => todo!("Strategy for Record not implemented"),
            ObjectType::ByteFn => any::<u8>().prop_map(ArbitraryObjectType::ByteFn).boxed(),
            ObjectType::Subr => todo!("Strategy for Subr not implemented"),
            ObjectType::Buffer => any::<String>().prop_map(ArbitraryObjectType::Buffer).boxed(),
            ObjectType::Nil => Just(ArbitraryObjectType::Nil).boxed(),
            ObjectType::Char => any::<char>().prop_map(ArbitraryObjectType::Char).boxed(),
            ObjectType::Function => todo!("Strategy for Function not implemented"),
        }
    }

    pub(crate) fn any_object_strategy() -> BoxedStrategy<ArbitraryObjectType> {
        prop_oneof![
            Just(ArbitraryObjectType::Nil),
            any::<bool>().prop_map(ArbitraryObjectType::Boolean),
            any::<i64>().prop_map(ArbitraryObjectType::Integer),
            any::<f64>().prop_map(ArbitraryObjectType::Float),
            any::<String>().prop_map(ArbitraryObjectType::String),
            Self::SYMBOL_CHARS.prop_map(ArbitraryObjectType::Symbol),
            "[a-zA-Z0-9 ]*".prop_map(ArbitraryObjectType::UnibyteString),
            any::<char>().prop_map(ArbitraryObjectType::Char),
        ]
        .boxed()
    }
}

// New function to create a combined strategy from multiple types
pub(crate) fn combined_strategy(types: &[ObjectType]) -> BoxedStrategy<ArbitraryObjectType> {
    // Combine all strategies using prop_oneof!
    match types.len() {
        0 => panic!("At least one type must be provided"),
        1 => types[0].strategy(),
        2 => prop_oneof![types[0].strategy(), types[1].strategy()].boxed(),
        3 => prop_oneof![types[0].strategy(), types[1].strategy(), types[2].strategy()].boxed(),
        4 => prop_oneof![
            types[0].strategy(),
            types[1].strategy(),
            types[2].strategy(),
            types[3].strategy()
        ]
        .boxed(),
        5 => prop_oneof![
            types[0].strategy(),
            types[1].strategy(),
            types[2].strategy(),
            types[3].strategy(),
            types[4].strategy()
        ]
        .boxed(),
        n => panic!("Currently supporting up to 5 combined types, got {n}"),
    }
}

#[derive(Clone, PartialEq, PartialOrd, Debug)]
#[expect(dead_code)]
pub(crate) enum ArbitraryObjectType {
    String(String),
    Float(f64),
    Cons(Vec<ArbitraryObjectType>),
    Symbol(String),
    Integer(i64),
    Boolean(bool),
    Unknown(Box<ArbitraryObjectType>),
    UnibyteString(String),
    Vector(Vec<ArbitraryObjectType>),
    HashTable(Vec<(ArbitraryObjectType, ArbitraryObjectType)>),
    Record(String, Vec<ArbitraryObjectType>),
    Nil,
    Function(u8),
    ByteFn(u8),
    Char(char),
    Buffer(String),
    Subr(u8),
}

impl std::fmt::Display for ArbitraryObjectType {
    #[expect(clippy::too_many_lines)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArbitraryObjectType::String(s) => {
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
            ArbitraryObjectType::Float(n) => {
                write!(f, "{n}")
            }
            ArbitraryObjectType::Cons(list) => {
                write!(f, "'(")?;
                for i in 0..list.len() {
                    write!(f, "{}", list[i])?;
                    if i < list.len() - 1 {
                        write!(f, " ")?;
                    }
                }
                write!(f, ")")
            }
            ArbitraryObjectType::Symbol(s) => {
                write!(f, "'{s}")
            }
            ArbitraryObjectType::Integer(n) => {
                write!(f, "{n}")
            }
            ArbitraryObjectType::Boolean(b) => {
                if *b {
                    write!(f, "t")
                } else {
                    write!(f, "nil")
                }
            }
            ArbitraryObjectType::Unknown(obj) => {
                write!(f, "{obj}")
            }
            ArbitraryObjectType::UnibyteString(s) => {
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
            ArbitraryObjectType::Nil => {
                write!(f, "nil")
            }
            ArbitraryObjectType::Vector(vec) => {
                write!(f, "{vec:?}")
            }
            ArbitraryObjectType::HashTable(vec) => {
                write!(f, "#s(hash-table data (")?;
                for (key, value) in vec {
                    write!(f, "{key} {value} ")?;
                }
                write!(f, "))")
            }
            ArbitraryObjectType::Record(name, members) => {
                write!(f, "(record '{name} ")?;
                for member in members {
                    write!(f, "{member} ")?;
                }
                write!(f, ")")
            }
            ArbitraryObjectType::Function(arity) => {
                write!(f, "(lambda (")?;
                for i in 0..*arity {
                    write!(f, "arg{i} ")?;
                }
                write!(f, ") nil)")
            }
            ArbitraryObjectType::ByteFn(arity) => {
                write!(f, "(lambda (")?;
                for i in 0..*arity {
                    write!(f, "arg{i} ")?;
                }
                write!(f, ") nil)")
            }
            ArbitraryObjectType::Buffer(name) => {
                write!(f, "(generate-new-buffer {name})")
            }
            ArbitraryObjectType::Subr(arity) => {
                write!(f, "(lambda (")?;
                for i in 0..*arity {
                    write!(f, "arg{i} ")?;
                }
                write!(f, ") nil)")
            }
            ArbitraryObjectType::Char(chr) => {
                write!(f, "?{chr}")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Type {
    Object(Vec<ObjectType>),
    Nil,
}

impl Type {
    fn strategy(&self) -> BoxedStrategy<ArbitraryObjectType> {
        match self {
            Type::Object(types) => combined_strategy(types),
            Type::Nil => Just(ArbitraryObjectType::Nil).boxed(),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum ArgType {
    Required(Type),
    Optional(Type),
}

#[derive(Debug, Clone)]
#[expect(dead_code)]
pub(crate) struct Function {
    pub(crate) name: String,
    pub(crate) args: Vec<ArgType>,
    pub(crate) ret: Type,
    pub(crate) fallible: bool,
}

impl Function {
    pub(crate) fn strategy(&self) -> Vec<BoxedStrategy<Option<ArbitraryObjectType>>> {
        self.args
            .iter()
            .map(|arg| match arg {
                ArgType::Required(ty) => ty.strategy().prop_map(Some).boxed(),
                ArgType::Optional(ty) => {
                    prop_oneof![1 => Just(None), 3 => ty.strategy().prop_map(Some)].boxed()
                }
            })
            .collect::<Vec<_>>()
    }

    #[expect(clippy::too_many_lines)]
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

                match last.as_str() {
                    "StringOrSymbol" => {
                        Ok(Type::Object(vec![ObjectType::String, ObjectType::Symbol]))
                    }
                    "Symbol" => Ok(Type::Object(vec![ObjectType::Symbol])),
                    "Number" | "NumberValue" => {
                        Ok(Type::Object(vec![ObjectType::Integer, ObjectType::Float]))
                    }
                    "Object" => Ok(Type::Object(vec![ObjectType::Unknown])),
                    "usize" | "isize" | "i64" => Ok(Type::Object(vec![ObjectType::Integer])),
                    "str" | "String" | "LispString" => Ok(Type::Object(vec![ObjectType::String])),
                    "bool" => Ok(Type::Object(vec![ObjectType::Boolean])),
                    "f64" => Ok(Type::Object(vec![ObjectType::Float])),
                    "char" => Ok(Type::Object(vec![ObjectType::Char])),
                    "Function" => Ok(Type::Object(vec![ObjectType::Function])),
                    "Cons" | "List" | "Error" => Ok(Type::Object(vec![ObjectType::Cons])),
                    "OptionalFlag" => Ok(Type::Object(vec![
                        ObjectType::Boolean,
                        ObjectType::Nil,
                        ObjectType::Unknown,
                    ])),
                    "ArgSlice" => Ok(Type::Object(vec![ObjectType::Cons, ObjectType::Nil])),
                    "LispVec" | "LispVector" | "Vec" | "RecordBuilder" => {
                        Ok(Type::Object(vec![ObjectType::Vector]))
                    }
                    "LispHashTable" => Ok(Type::Object(vec![ObjectType::HashTable])),
                    "ByteString" => Ok(Type::Object(vec![ObjectType::UnibyteString])),
                    "Record" => Ok(Type::Object(vec![ObjectType::Record])),
                    "ByteFn" => Ok(Type::Object(vec![ObjectType::ByteFn])),
                    "SubrFn" => Ok(Type::Object(vec![ObjectType::Subr])),
                    "LispBuffer" => Ok(Type::Object(vec![ObjectType::Buffer])),
                    "Rto" | "Rt" | "Gc" => {
                        let syn::PathArguments::AngleBracketed(
                            syn::AngleBracketedGenericArguments { args, .. },
                        ) = &segments.last().unwrap().arguments
                        else {
                            return Err("Expected angle bracketed arguments".to_string());
                        };
                        let mut type_argument = None;
                        for arg in args {
                            match arg {
                                syn::GenericArgument::Type(ty) => {
                                    type_argument = Some(ty);
                                }
                                _ => continue,
                            }
                        }

                        let Some(ty) = type_argument else {
                            return Err("Expected type argument".to_string());
                        };

                        Function::process_arg(ty)
                    }
                    "Env" | "Context" => {
                        Err("Environment or Context type not supported".to_string())
                    }
                    _ => Err(format!("Unknown type: {last}")),
                }
            }
            syn::Type::Ptr(_) => Err("Ptr not supported".to_string()),
            syn::Type::Reference(syn::TypeReference { elem, .. })
            | syn::Type::Group(syn::TypeGroup { elem, .. }) => Function::process_arg(elem),
            syn::Type::Slice(syn::TypeSlice { .. }) => {
                Ok(Type::Object(vec![ObjectType::Cons, ObjectType::Nil]))
            }
            syn::Type::TraitObject(_) => Err("TraitObject not supported".to_string()),
            syn::Type::Tuple(_) => Ok(Type::Object(vec![ObjectType::Nil])),
            syn::Type::Verbatim(_) => Err("Verbatim type not supported".to_string()),
            _ => Err("Unknown type".to_string()),
        }
    }

    #[expect(clippy::too_many_lines)]
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

    let args = item
        .sig
        .inputs
        .iter()
        .filter_map(|arg| {
            let ty = match arg {
                syn::FnArg::Receiver(syn::Receiver { ty, .. })
                | syn::FnArg::Typed(syn::PatType { ty, .. }) => ty,
            };

            match ty.as_ref() {
                syn::Type::Group(syn::TypeGroup { group_token, elem }) => {
                    let syn::token::Group { span } = group_token;

                    let source_text = span.source_text()
                        .ok_or_else(|| "Failed to get source text".to_string())
                        .ok()?;
                    let optional = matches!(source_text.as_str(), "Option");
                    let ty = Function::process_arg(elem).ok()?;

                    match ty {
                        Type::Object(ref obj) => {
                            if obj.contains(&ObjectType::Boolean) {
                                Some(ArgType::Required(ty))
                            } else if optional {
                                Some(ArgType::Optional(ty))
                            } else {
                                Some(ArgType::Required(ty))
                            }
                        }
                        Type::Nil => {
                            if optional {
                                Some(ArgType::Optional(ty))
                            } else {
                                Some(ArgType::Required(ty))
                            }
                        }
                    }
                }
                syn::Type::Path(syn::TypePath { path, .. }) => {
                    let segments = &path.segments;
                    let last = segments.last().unwrap().ident.to_string();
                    let optional = matches!(last.as_str(), "Result" | "Option");
                    if optional {
                        let syn::PathArguments::AngleBracketed(
                            syn::AngleBracketedGenericArguments { args, .. },
                        ) = &segments.last().unwrap().arguments
                        else {
                            return None;
                        };
                        let mut type_argument = None;
                        for arg in args {
                            match arg {
                                syn::GenericArgument::Type(ty) => {
                                    type_argument = Some(ty);
                                }
                                _ => continue,
                            }
                        }

                        let Some(ty) = type_argument else {
                            return None;
                        };

                        let ty = Function::process_arg(ty).ok()?;

                        match ty {
                            Type::Object(ref obj) => {
                                if obj.contains(&ObjectType::Boolean) {
                                    Some(ArgType::Required(ty))
                                } else if optional {
                                    Some(ArgType::Optional(ty))
                                } else {
                                    Some(ArgType::Required(ty))
                                }
                            }
                            Type::Nil => {
                                if optional {
                                    Some(ArgType::Optional(ty))
                                } else {
                                    Some(ArgType::Required(ty))
                                }
                            }
                        }
                    } else {
                        let ty = Function::process_arg(ty).ok()?;

                        match ty {
                            Type::Object(ref obj) => {
                                if obj.contains(&ObjectType::Boolean) {
                                    Some(ArgType::Required(ty))
                                } else if optional {
                                    Some(ArgType::Optional(ty))
                                } else {
                                    Some(ArgType::Required(ty))
                                }
                            }
                            Type::Nil => {
                                if optional {
                                    Some(ArgType::Optional(ty))
                                } else {
                                    Some(ArgType::Required(ty))
                                }
                            }
                        }
                    }
                }
                x => {
                    let ty = Function::process_arg(x).ok()?;
                    Some(ArgType::Required(ty))
                }
            }
        })
        .collect();

    let (ret, fallible) = match &item.sig.output {
        syn::ReturnType::Default => (Type::Nil, false),
        syn::ReturnType::Type(_, ty) => {
            match ty.as_ref() {
                syn::Type::Group(syn::TypeGroup { group_token, elem }) => {
                    let syn::token::Group { span } = group_token;

                    let source_text = span.source_text()
                        .ok_or_else(|| "Failed to get source text".to_string())?;
                    let fallible = matches!(source_text.as_str(), "Option" | "Result");

                    let ty = Function::process_arg(elem)?;
                    (ty, fallible)
                }
                syn::Type::Path(syn::TypePath { path, .. }) => {
                    let segments = &path.segments;
                    let last = segments.last().unwrap().ident.to_string();
                    match last.as_str() {
                        "Result" | "Option" => {
                            let syn::PathArguments::AngleBracketed(
                                syn::AngleBracketedGenericArguments { args, .. },
                            ) = &segments.last().unwrap().arguments
                            else {
                                return Err("Expected angle bracketed arguments".to_string());
                            };
                            let mut type_argument = None;
                            for arg in args {
                                match arg {
                                    syn::GenericArgument::Type(ty) => {
                                        type_argument = Some(ty);
                                    }
                                    _ => continue,
                                }
                            }

                            let Some(ty) = type_argument else {
                                return Err("Expected type argument".to_string());
                            };

                            let ty = Function::process_arg(ty)?;
                            (ty, true)
                        }
                        _ => {
                            let ty = Function::process_arg(ty)?;
                            (ty, false)
                        }
                    }
                }
                x => {
                    let ty = Function::process_arg(x)?;
                    (ty, false)
                }
            }
        }
    };

    Ok(Function { name, args, ret, fallible })
}
}
