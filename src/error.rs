use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub enum Error {
    ConstOverflow,
    ArgOverflow,
    ArgCount(u16, u16),
    LetValueCount(u16),
    StackSizeOverflow,
    Type(Type, Type),
    VoidFunction,
    VoidVariable,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::ConstOverflow => write!(f, "Too many constants declared in fuction"),
            Error::ArgOverflow => write!(f, "Too many arguments declared in function"),
            Error::ArgCount(exp, act) => write!(f, "Expected {} arg(s), found {}", exp, act),
            Error::LetValueCount(_) => write!(f, "Too many arguments declared in function"),
            Error::StackSizeOverflow => write!(f, "Let forms can only have 1 value"),
            Error::Type(exp, act) => write!(f, "expected {:?}, found {:?}", exp, act),
            Error::VoidFunction => write!(f, "Void function"),
            Error::VoidVariable => write!(f, "Void variable"),
        }
    }
}

impl std::error::Error for Error {}

#[derive(Debug, PartialEq)]
pub enum Type {
    Int,
    True,
    Nil,
    Cons,
    String,
    Symbol,
    Float,
    Void,
    Marker,
    Func,
    Number,
    List,
}

pub type Result<T> = std::result::Result<T, Error>;
