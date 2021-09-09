use std::fmt::Display;

use crate::object::Object;

#[derive(Debug, PartialEq)]
pub(crate) enum Error {
    ArgCount(u16, u16),
    Type(Type, Type, String),
}

impl std::error::Error for Error {}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::ArgCount(exp, act) => write!(f, "Expected {} arg(s), found {}", exp, act),
            Error::Type(exp, act, print) => {
                write!(f, "expected {:?}, found {:?}: {}", exp, act, print)
            }
        }
    }
}

impl Error {
    pub(crate) fn from_object(exp: Type, obj: Object) -> Self {
        Error::Type(exp, obj.get_type(), format!("{}", obj))
    }
}

#[derive(Debug, PartialEq)]
pub(crate) enum Type {
    Int,
    True,
    Nil,
    Cons,
    Vec,
    Sequence,
    String,
    Symbol,
    Float,
    Func,
    Number,
    List,
}
