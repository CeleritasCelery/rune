use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub enum Error {
    ArgCount(u16, u16),
    Type(Type, Type),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::ArgCount(exp, act) => write!(f, "Expected {} arg(s), found {}", exp, act),
            Error::Type(exp, act) => write!(f, "expected {:?}, found {:?}", exp, act),
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
