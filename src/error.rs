use std::fmt::Display;

#[derive(Debug, PartialEq)]
pub enum Error {
    ConstOverflow,
    ArgOverflow,
    ArgCount(u16, u16),
    LetValueCount(u16),
    StackSizeOverflow,
    InvalidCons,
    Type(Type, Type),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::ConstOverflow => write!(f, "Too many constants declared in fuction"),
            Error::ArgOverflow => write!(f, "Too many arguments declared in function"),
            Error::ArgCount(exp, act) => write!(f, "Expected {} arg(s), found {}", exp, act),
            Error::LetValueCount(_) => write!(f, "Let forms can only have 1 value"),
            Error::StackSizeOverflow => write!(f, "Stack size overflow"),
            Error::Type(exp, act) => write!(f, "expected {:?}, found {:?}", exp, act),
            Error::InvalidCons => write!(f, "Found non-nil cdr at end of list"),
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
