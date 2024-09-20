use std::fmt::{Display, Formatter};

use super::{cons::Cons, gc::Context, object::WithLifetime};

#[derive(Debug, PartialEq)]
pub(crate) struct LispError {
    message: &'static Cons,
}

impl std::error::Error for LispError {}

impl Display for LispError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        let Self { message } = self;
        write!(f, "Error: {message}")
    }
}

impl LispError {
    pub(crate) fn new(message: &Cons) -> Self {
        Self { message: unsafe { message.with_lifetime() } }
    }

    pub(crate) fn bind<'ob>(&self, cx: &'ob Context) -> &'ob Cons {
        cx.bind(self.message)
    }
}

unsafe impl Send for LispError {}
unsafe impl Sync for LispError {}

#[derive(Debug, PartialEq)]
pub(crate) enum Type {
    Int,
    Char,
    Cons,
    Vec,
    Record,
    HashTable,
    Sequence,
    BufferOrName,
    String,
    Symbol,
    Float,
    Func,
    Number,
    List,
    Buffer,
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
        let Self { expect, actual, print } = self;
        write!(f, "expected {expect:?}, found {actual:?}: {print}")
    }
}

impl TypeError {
    /// Get a type error from an object.
    pub(crate) fn new<'ob, T>(expect: Type, obj: T) -> Self
    where
        T: Into<super::object::ObjectType<'ob>>,
    {
        let obj = obj.into();
        Self { expect, actual: obj.get_type(), print: obj.to_string() }
    }
}
