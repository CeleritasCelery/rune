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
            ObjectType::String => {
                ArbitraryObjectType::String(String::arbitrary(&mut unstructured).unwrap())
            }
            ObjectType::Float => {
                ArbitraryObjectType::Float(f64::arbitrary(&mut unstructured).unwrap())
            }
            ObjectType::Cons => {
                let choice = rand::random::<usize>() % 20;
                let mut list = Vec::new();
                for _ in 0..choice {
                    list.push(ArbitraryObjectType::arbitrary(&mut unstructured).unwrap());
                }
                ArbitraryObjectType::Cons(list)
            }
            ObjectType::Symbol => {
                let mut string: String = String::arbitrary(&mut unstructured)
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
            ObjectType::Integer => {
                ArbitraryObjectType::Integer(i64::arbitrary(&mut unstructured).unwrap())
            }
            ObjectType::Boolean => {
                ArbitraryObjectType::Boolean(bool::arbitrary(&mut unstructured).unwrap())
            }
            ObjectType::Unknown => ArbitraryObjectType::Unknown(Box::new(
                ArbitraryObjectType::arbitrary(&mut unstructured).unwrap(),
            )),
            ObjectType::Function => {
                ArbitraryObjectType::Function(u8::arbitrary(&mut unstructured).unwrap() % 10)
            }
            ObjectType::UnibyteString => {
                ArbitraryObjectType::UnibyteString(String::arbitrary(&mut unstructured).unwrap())
            }
            ObjectType::Vector => {
                ArbitraryObjectType::Vector(Vec::arbitrary(&mut unstructured).unwrap())
            }
            ObjectType::HashTable => {
                ArbitraryObjectType::HashTable(Vec::arbitrary(&mut unstructured).unwrap())
            }
            ObjectType::Record => ArbitraryObjectType::Record(
                String::arbitrary(&mut unstructured).unwrap(),
                Vec::arbitrary(&mut unstructured).unwrap(),
            ),
            ObjectType::ByteFn => {
                ArbitraryObjectType::ByteFn(u8::arbitrary(&mut unstructured).unwrap() % 10)
            }
            ObjectType::Subr => {
                ArbitraryObjectType::Subr(u8::arbitrary(&mut unstructured).unwrap() % 10)
            }
            ObjectType::Buffer => {
                ArbitraryObjectType::Buffer(String::arbitrary(&mut unstructured).unwrap())
            }
            ObjectType::Nil => ArbitraryObjectType::Nil,
        }
    }
}

#[derive(Clone, PartialEq)]
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

impl<'a> Arbitrary<'a> for ArbitraryObjectType {
    fn arbitrary(u: &mut arbitrary::Unstructured<'a>) -> arbitrary::Result<Self> {
        let choice = u.int_in_range(0..=15)?;
        match choice {
            0 => Ok(ArbitraryObjectType::String(String::arbitrary(u)?)),
            1 => Ok(ArbitraryObjectType::Float(f64::arbitrary(u)?)),
            2 => {
                let choice = u.int_in_range(0..=20)?;
                let mut list = Vec::new();
                for _ in 0..choice {
                    list.push(ArbitraryObjectType::arbitrary(u)?);
                }
                Ok(ArbitraryObjectType::Cons(list))
            }
            3 => {
                let mut string: String = String::arbitrary(u)?
                    .chars()
                    .map(|c| match c {
                        c if c.is_whitespace() => '_',
                        c => c,
                    })
                    .collect();
                if string.is_empty() {
                    string.push('_');
                }
                Ok(ArbitraryObjectType::Symbol(string))
            }
            4 => Ok(ArbitraryObjectType::Integer(i64::arbitrary(u)?)),
            5 => Ok(ArbitraryObjectType::Boolean(bool::arbitrary(u)?)),
            6 => Ok(ArbitraryObjectType::Unknown(Box::new(ArbitraryObjectType::arbitrary(u)?))),
            7 => Ok(ArbitraryObjectType::UnibyteString(String::arbitrary(u)?)),
            8 => Ok(ArbitraryObjectType::Vector(Vec::arbitrary(u)?)),
            9 => Ok(ArbitraryObjectType::HashTable(Vec::arbitrary(u)?)),
            10 => {
                let choice = u.int_in_range(0..=10)?;
                let mut list = Vec::new();
                for _ in 0..choice {
                    list.push(ArbitraryObjectType::arbitrary(u)?);
                }
                Ok(ArbitraryObjectType::Record(String::arbitrary(u)?, list))
            }
            11 => Ok(ArbitraryObjectType::Function(u8::arbitrary(u)? % 10)),
            12 => Ok(ArbitraryObjectType::ByteFn(u8::arbitrary(u)? % 10)),
            13 => Ok(ArbitraryObjectType::Nil),
            14 => Ok(ArbitraryObjectType::Buffer(String::arbitrary(u)?)),
            15 => Ok(ArbitraryObjectType::Subr(u8::arbitrary(u)? % 10)),
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

#[derive(Debug, Clone, PartialEq, Arbitrary)]
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
                todo!("Array")
            }
            syn::Type::BareFn(_) => {
                todo!("BareFn")
            }
            syn::Type::ImplTrait(_) => {
                todo!("Impl Trait")
            }
            syn::Type::Infer(_) => {
                todo!("Infer")
            }
            syn::Type::Macro(_) => {
                todo!("Macro")
            }
            syn::Type::Never(_) => {
                todo!("Never")
            }
            syn::Type::Paren(_) => {
                todo!("Paren")
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
                            panic!("Expected type argument");
                        };

                        let ty = Function::process_arg(ty)?;

                        Some(ty)
                    }
                    "Env" | "Context" => None,
                    _ => {
                        panic!("Unknown type: {last}");
                    }
                }
            }
            syn::Type::Ptr(_) => {
                todo!("Ptr")
            }
            syn::Type::Reference(syn::TypeReference { elem, .. })
            | syn::Type::Group(syn::TypeGroup { elem, .. }) => Function::process_arg(elem),
            syn::Type::Slice(syn::TypeSlice { .. }) => {
                Some(Type::Object(vec![ObjectType::Cons, ObjectType::Nil]))
            }
            syn::Type::TraitObject(_) => {
                todo!("TraitObject")
            }
            syn::Type::Tuple(_) => Some(Type::Object(vec![ObjectType::Nil])),
            syn::Type::Verbatim(_) => None,
            _ => {
                todo!("Catch all")
            }
        }
    }
}

impl From<&ItemFn> for Function {
    #[allow(clippy::too_many_lines)]
    fn from(item: &ItemFn) -> Self {
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
                                panic!("Expected type argument");
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
                                    panic!("Expected type argument");
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

        Function { name, args, ret, fallible }
    }
}

#[allow(dead_code)]
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
