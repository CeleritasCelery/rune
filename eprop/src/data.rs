use std::hash::Hash;

use arbitrary::Arbitrary;
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
}

impl From<ObjectType> for ArbitraryObjectType {
    fn from(val: ObjectType) -> Self {
        let list = rand::random::<[u8; 32]>();
        let mut unstructured = arbitrary::Unstructured::new(&list);
        match val {
            ObjectType::String => ArbitraryObjectType::generate_string(&mut unstructured),
            ObjectType::Float => ArbitraryObjectType::generate_float(&mut unstructured),
            ObjectType::Cons => ArbitraryObjectType::generate_cons(&mut unstructured),
            ObjectType::Symbol => ArbitraryObjectType::generate_symbol(&mut unstructured),
            ObjectType::Integer => ArbitraryObjectType::generate_integer(&mut unstructured),
            ObjectType::Boolean => ArbitraryObjectType::generate_boolean(&mut unstructured),
            ObjectType::Unknown => ArbitraryObjectType::generate_unknown(&mut unstructured),
            ObjectType::Function => ArbitraryObjectType::generate_function(&mut unstructured),
            ObjectType::UnibyteString => {
                ArbitraryObjectType::generate_unibyte_string(&mut unstructured)
            }
            ObjectType::Vector => ArbitraryObjectType::generate_vector(&mut unstructured),
            ObjectType::HashTable => ArbitraryObjectType::generate_hash_table(&mut unstructured),
            ObjectType::Record => ArbitraryObjectType::generate_record(&mut unstructured),
            ObjectType::ByteFn => ArbitraryObjectType::generate_byte_fn(&mut unstructured),
            ObjectType::Subr => ArbitraryObjectType::generate_subr(&mut unstructured),
            ObjectType::Buffer => ArbitraryObjectType::generate_buffer(&mut unstructured),
            ObjectType::Nil => ArbitraryObjectType::generate_nil(),
        }
    }
}

#[derive(Clone, PartialEq, PartialOrd)]
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
    Buffer(String),
    Subr(u8),
}

impl ArbitraryObjectType {
    fn generate_string(u: &mut arbitrary::Unstructured) -> Self {
        ArbitraryObjectType::String(String::arbitrary(u).unwrap())
    }
    fn generate_float(u: &mut arbitrary::Unstructured) -> Self {
        ArbitraryObjectType::Float(f64::arbitrary(u).unwrap())
    }
    fn generate_cons(u: &mut arbitrary::Unstructured) -> Self {
        let choice = rand::random::<usize>() % 20;
        let mut list = Vec::new();
        for _ in 0..choice {
            list.push(ArbitraryObjectType::arbitrary(u).unwrap());
        }
        ArbitraryObjectType::Cons(list)
    }
    fn generate_symbol(u: &mut arbitrary::Unstructured) -> Self {
        let mut string: String = String::arbitrary(u)
            .unwrap()
            .chars()
            .map(|c| match c {
                c if c.is_whitespace() => '_',
                c => c,
            })
            .collect();
        if string.is_empty() {
            string.push('_');
        }
        ArbitraryObjectType::Symbol(string)
    }
    fn generate_integer(u: &mut arbitrary::Unstructured) -> Self {
        ArbitraryObjectType::Integer(i64::arbitrary(u).unwrap())
    }
    fn generate_boolean(u: &mut arbitrary::Unstructured) -> Self {
        ArbitraryObjectType::Boolean(bool::arbitrary(u).unwrap())
    }
    fn generate_unknown(u: &mut arbitrary::Unstructured) -> Self {
        ArbitraryObjectType::Unknown(Box::new(ArbitraryObjectType::arbitrary(u).unwrap()))
    }
    fn generate_unibyte_string(u: &mut arbitrary::Unstructured) -> Self {
        ArbitraryObjectType::UnibyteString(String::arbitrary(u).unwrap())
    }
    fn generate_vector(u: &mut arbitrary::Unstructured) -> Self {
        ArbitraryObjectType::Vector(Vec::arbitrary(u).unwrap())
    }
    fn generate_hash_table(u: &mut arbitrary::Unstructured) -> Self {
        ArbitraryObjectType::HashTable(Vec::arbitrary(u).unwrap())
    }
    fn generate_record(u: &mut arbitrary::Unstructured) -> Self {
        ArbitraryObjectType::Record(String::arbitrary(u).unwrap(), Vec::arbitrary(u).unwrap())
    }
    fn generate_nil() -> Self {
        ArbitraryObjectType::Nil
    }
    fn generate_function(u: &mut arbitrary::Unstructured) -> Self {
        ArbitraryObjectType::Function(u8::arbitrary(u).unwrap() % 10)
    }
    fn generate_byte_fn(u: &mut arbitrary::Unstructured) -> Self {
        ArbitraryObjectType::ByteFn(u8::arbitrary(u).unwrap() % 10)
    }
    fn generate_buffer(u: &mut arbitrary::Unstructured) -> Self {
        ArbitraryObjectType::Buffer(String::arbitrary(u).unwrap())
    }
    fn generate_subr(u: &mut arbitrary::Unstructured) -> Self {
        ArbitraryObjectType::Subr(u8::arbitrary(u).unwrap() % 10)
    }
}

impl Hash for ArbitraryObjectType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            ArbitraryObjectType::String(s)
            | ArbitraryObjectType::Symbol(s)
            | ArbitraryObjectType::UnibyteString(s) => {
                s.hash(state);
            }
            ArbitraryObjectType::Float(f) => {
                f.to_bits().hash(state);
            }
            ArbitraryObjectType::Cons(list) => {
                list.hash(state);
            }
            ArbitraryObjectType::Integer(i) => {
                i.hash(state);
            }
            ArbitraryObjectType::Boolean(b) => {
                b.hash(state);
            }
            ArbitraryObjectType::Unknown(obj) => {
                obj.hash(state);
            }
            ArbitraryObjectType::Vector(vec) => {
                vec.hash(state);
            }
            ArbitraryObjectType::HashTable(vec) => {
                vec.hash(state);
            }
            ArbitraryObjectType::Record(name, members) => {
                name.hash(state);
                members.hash(state);
            }
            ArbitraryObjectType::Nil => {
                0.hash(state);
            }
            ArbitraryObjectType::Function(arity)
            | ArbitraryObjectType::ByteFn(arity)
            | ArbitraryObjectType::Subr(arity) => {
                arity.hash(state);
            }
            ArbitraryObjectType::Buffer(name) => {
                name.hash(state);
            }
        }
    }
}

impl<'a> Arbitrary<'a> for ArbitraryObjectType {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        let choice = u.int_in_range(0..=15)?;
        match choice {
            0 => Ok(ArbitraryObjectType::generate_string(u)),
            1 => Ok(ArbitraryObjectType::generate_float(u)),
            2 => Ok(ArbitraryObjectType::generate_cons(u)),
            3 => Ok(ArbitraryObjectType::generate_symbol(u)),
            4 => Ok(ArbitraryObjectType::generate_integer(u)),
            5 => Ok(ArbitraryObjectType::generate_boolean(u)),
            6 => Ok(ArbitraryObjectType::generate_unknown(u)),
            7 => Ok(ArbitraryObjectType::generate_unibyte_string(u)),
            8 => Ok(ArbitraryObjectType::generate_vector(u)),
            9 => Ok(ArbitraryObjectType::generate_hash_table(u)),
            10 => Ok(ArbitraryObjectType::generate_record(u)),
            11 => Ok(ArbitraryObjectType::generate_function(u)),
            12 => Ok(ArbitraryObjectType::generate_byte_fn(u)),
            13 => Ok(ArbitraryObjectType::generate_nil()),
            14 => Ok(ArbitraryObjectType::generate_buffer(u)),
            15 => Ok(ArbitraryObjectType::generate_subr(u)),
            _ => {
                todo!()
            }
        }
    }
}

impl std::fmt::Display for ArbitraryObjectType {
    #[allow(clippy::too_many_lines)]
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
        }
    }
}

impl std::fmt::Debug for ArbitraryObjectType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Type {
    Object(Vec<ObjectType>),
    Nil,
}

#[derive(Debug, Clone, PartialEq, Arbitrary, PartialOrd, Hash)]
pub(crate) enum ArbitraryType {
    Object(ArbitraryObjectType),
    Nil,
}

impl From<Type> for ArbitraryType {
    fn from(val: Type) -> Self {
        match val {
            Type::Object(obj) => {
                let choice = rand::random::<usize>() % obj.len();
                let obj = obj[choice];
                ArbitraryType::Object(obj.into())
            }
            Type::Nil => ArbitraryType::Nil,
        }
    }
}

impl std::fmt::Display for ArbitraryType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ArbitraryType::Object(obj) => {
                write!(f, "{obj}")
            }
            ArbitraryType::Nil => {
                write!(f, "nil")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum ArgType {
    Required(Type),
    Optional(Type),
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Hash)]
pub(crate) enum ArbitraryArgType {
    Required(ArbitraryType),
    Optional(ArbitraryType),
}

impl From<ArgType> for ArbitraryArgType {
    fn from(val: ArgType) -> Self {
        match val {
            ArgType::Required(ty) => ArbitraryArgType::Required(ty.into()),
            ArgType::Optional(ty) => ArbitraryArgType::Optional(ty.into()),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Function {
    name: String,
    args: Vec<ArgType>,
    ret: Type,
    fallible: bool,
}

impl Function {
    #[allow(clippy::too_many_lines)]
    fn process_arg(ty: &syn::Type) -> Option<Type> {
        match ty {
            syn::Type::Array(_) => {
                eprintln!("Array not supported");
                None
            }
            syn::Type::BareFn(_) => {
                eprintln!("BareFn not supported");
                None
            }
            syn::Type::ImplTrait(_) => {
                eprintln!("Impl Trait not supported");
                None
            }
            syn::Type::Infer(_) => {
                eprintln!("Infer not supported");
                None
            }
            syn::Type::Macro(_) => {
                eprintln!("Macro not supported");
                None
            }
            syn::Type::Never(_) => {
                eprintln!("Never not supported");
                None
            }
            syn::Type::Paren(_) => {
                eprintln!("Paren not supported");
                None
            }
            syn::Type::Path(syn::TypePath { path, .. }) => {
                let segments = &path.segments;
                let last = segments.last().unwrap().ident.to_string();

                match last.as_str() {
                    "StringOrSymbol" => {
                        Some(Type::Object(vec![ObjectType::String, ObjectType::Symbol]))
                    }
                    "Symbol" => Some(Type::Object(vec![ObjectType::Symbol])),
                    "Number" => Some(Type::Object(vec![ObjectType::Integer, ObjectType::Float])),
                    "Object" => Some(Type::Object(vec![ObjectType::Unknown])),
                    "usize" | "isize" | "i64" => Some(Type::Object(vec![ObjectType::Integer])),
                    "str" | "String" | "LispString" => Some(Type::Object(vec![ObjectType::String])),
                    "bool" => Some(Type::Object(vec![ObjectType::Boolean])),
                    "f64" => Some(Type::Object(vec![ObjectType::Float])),
                    "Function" => Some(Type::Object(vec![ObjectType::Function])),
                    "Cons" | "List" => Some(Type::Object(vec![ObjectType::Cons])),
                    "OptionalFlag" => Some(Type::Object(vec![
                        ObjectType::Boolean,
                        ObjectType::Nil,
                        ObjectType::Unknown,
                    ])),
                    "LispVec" | "LispVector" => Some(Type::Object(vec![ObjectType::Vector])),
                    "LispHashTable" => Some(Type::Object(vec![ObjectType::HashTable])),
                    "ByteString" => Some(Type::Object(vec![ObjectType::UnibyteString])),
                    "Record" => Some(Type::Object(vec![ObjectType::Record])),
                    "ByteFn" => Some(Type::Object(vec![ObjectType::ByteFn])),
                    "SubrFn" => Some(Type::Object(vec![ObjectType::Subr])),
                    "Buffer" => Some(Type::Object(vec![ObjectType::Buffer])),
                    "Rto" | "Rt" | "Gc" => {
                        let syn::PathArguments::AngleBracketed(
                            syn::AngleBracketedGenericArguments { args, .. },
                        ) = &segments.last().unwrap().arguments
                        else {
                            eprintln!("Expected angle bracketed arguments");
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
                            eprintln!("Expected type argument");
                            return None;
                        };

                        let ty = Function::process_arg(ty)?;

                        Some(ty)
                    }
                    "Env" | "Context" => None,
                    _ => {
                        eprintln!("Unknown type: {last}");
                        None
                    }
                }
            }
            syn::Type::Ptr(_) => {
                eprintln!("Ptr not supported");
                None
            }
            syn::Type::Reference(syn::TypeReference { elem, .. })
            | syn::Type::Group(syn::TypeGroup { elem, .. }) => Function::process_arg(elem),
            syn::Type::Slice(syn::TypeSlice { .. }) => {
                Some(Type::Object(vec![ObjectType::Cons, ObjectType::Nil]))
            }
            syn::Type::TraitObject(_) => {
                eprintln!("TraitObject not supported");
                None
            }
            syn::Type::Tuple(_) => Some(Type::Object(vec![ObjectType::Nil])),
            syn::Type::Verbatim(_) => None,
            _ => {
                eprintln!("Unknown type");
                None
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    pub(crate) fn from_item(item: &ItemFn) -> Option<Self> {
        //println!("{}", item.sig.ident.to_string());
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

                        let source_text = span.source_text().unwrap();
                        let optional = matches!(source_text.as_str(), "Option");
                        let ty = Function::process_arg(elem).unwrap();

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
                                eprintln!("Expected angle bracketed arguments");
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
                                eprintln!("Expected type argument");
                                return None;
                            };

                            let ty = Function::process_arg(ty)?;

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
                            let ty = Function::process_arg(ty)?;

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
                        let ty = Function::process_arg(x)?;

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

                        let source_text = span.source_text().unwrap();
                        //println!("{}", source_text);
                        let fallible = matches!(source_text.as_str(), "Option" | "Result");

                        let ty = Function::process_arg(elem).unwrap();
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
                                    panic!("Expected angle bracketed arguments");
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
                                    eprintln!("Expected type argument");
                                    return None;
                                };

                                let ty = Function::process_arg(ty).unwrap();
                                (ty, true)
                            }
                            _ => {
                                let ty = Function::process_arg(ty).unwrap();
                                (ty, false)
                            }
                        }
                    }
                    x => {
                        //println!("return type");
                        let ty = Function::process_arg(x).unwrap();

                        (ty, false)
                    }
                }
            }
        };

        Some(Function { name, args, ret, fallible })
    }
}

#[allow(dead_code)]
#[derive(Clone, PartialEq, PartialOrd, Hash)]
pub(crate) struct ArbitraryFunction {
    name: String,
    args: Vec<ArbitraryArgType>,
    ret: ArbitraryType,
    fallible: bool,
}

impl From<Function> for ArbitraryFunction {
    fn from(val: Function) -> Self {
        ArbitraryFunction {
            name: val.name,
            args: val.args.into_iter().map(Into::into).collect(),
            ret: val.ret.into(),
            fallible: val.fallible,
        }
    }
}

impl std::fmt::Display for ArbitraryFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} ", self.name)?;
        let optionals = rand::random::<bool>();
        for arg in &self.args {
            match arg {
                ArbitraryArgType::Required(ty) => {
                    write!(f, "{ty} ")?;
                }
                ArbitraryArgType::Optional(ty) => {
                    if optionals {
                        write!(f, "{ty} ")?;
                    }
                }
            }
        }
        write!(f, ")")
    }
}
