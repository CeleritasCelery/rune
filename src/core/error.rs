use std::fmt::{Display, Formatter};

/// The function or form has the wrong number of arguments.
#[derive(Debug, PartialEq)]
pub(crate) struct ArgError {
    expect: u16,
    actual: u16,
    name: String,
}

impl std::error::Error for ArgError {}

impl Display for ArgError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let Self {
            expect,
            actual,
            name,
        } = self;
        write!(
            f,
            "Expected {expect} argument(s) for `{name}', but found {actual}"
        )
    }
}

impl ArgError {
    pub(crate) fn new(expect: u16, actual: u16, name: impl AsRef<str>) -> ArgError {
        Self {
            expect,
            actual,
            name: name.as_ref().to_owned(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub(crate) enum Type {
    Int,
    Cons,
    Vec,
    ByteVec,
    HashTable,
    Sequence,
    String,
    Symbol,
    Float,
    Func,
    Number,
    List,
}

/// Error provided if object was the wrong type
#[derive(Debug, PartialEq)]
pub(crate) struct TypeError {
    expect: Type,
    actual: Type,
    print: String,
}

impl std::error::Error for TypeError {}

impl Display for TypeError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let Self {
            expect,
            actual,
            print,
        } = self;
        write!(f, "expected {expect:?}, found {actual:?}: {print}")
    }
}

impl TypeError {
    /// Get a type error from an object.
    pub(crate) fn new<'ob, T>(expect: Type, obj: T) -> Self
    where
        T: Into<super::object::Object<'ob>>,
    {
        let obj = obj.into();
        Self {
            expect,
            actual: obj.get_type(),
            print: obj.to_string(),
        }
    }
}
