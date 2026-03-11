use std::fmt::{Display, Formatter};

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
    BufferOrString,
    String,
    StringOrChar,
    Symbol,
    Float,
    Func,
    Number,
    List,
    Buffer,
    CharTable,
    BigInt,
    ChannelSender,
    ChannelReceiver,
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
