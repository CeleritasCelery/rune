use std::fmt::{Display, Formatter};

use super::{
    cons::ConsError,
    env::Env,
    gc::Rt,
    object::{display_slice, GcObj},
};

#[derive(Debug)]
pub(crate) struct EvalError {
    backtrace: Vec<Box<str>>,
    pub(crate) error: ErrorType,
}

#[derive(Debug)]
pub(crate) enum ErrorType {
    Throw(u32),
    Signal(u32),
    Err(anyhow::Error),
}

impl std::error::Error for EvalError {}

impl Display for EvalError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.error {
            ErrorType::Err(e) => writeln!(f, "{e}")?,
            ErrorType::Throw(_) => writeln!(f, "No catch for throw")?,
            ErrorType::Signal(_) => writeln!(f, "Signal")?,
        }
        Ok(())
    }
}

impl EvalError {
    pub(crate) fn new_error(error: anyhow::Error) -> Self {
        Self { backtrace: Vec::new(), error: ErrorType::Err(error) }
    }

    pub(crate) fn signal(error_symbol: GcObj, data: GcObj, env: &mut Rt<Env>) -> Self {
        Self {
            backtrace: Vec::new(),
            error: ErrorType::Signal(env.set_exception(error_symbol, data)),
        }
    }

    pub(crate) fn throw(tag: GcObj, data: GcObj, env: &mut Rt<Env>) -> Self {
        Self { backtrace: Vec::new(), error: ErrorType::Throw(env.set_exception(tag, data)) }
    }

    pub(crate) fn new(error: impl Into<Self>) -> Self {
        error.into()
    }

    pub(crate) fn with_trace(error: anyhow::Error, name: &str, args: &[Rt<GcObj>]) -> Self {
        let display = display_slice(args);
        let trace = format!("{name} {display}").into_boxed_str();
        Self { backtrace: vec![trace], error: ErrorType::Err(error) }
    }

    pub(crate) fn add_trace(mut self, name: &str, args: &[Rt<GcObj>]) -> Self {
        let display = display_slice(args);
        self.backtrace.push(format!("{name} {display}").into_boxed_str());
        self
    }

    pub(crate) fn print_backtrace(&self) {
        println!("BEGIN_BACKTRACE");
        for (i, x) in self.backtrace.iter().enumerate() {
            println!("{i}: {x}");
        }
        println!("END_BACKTRACE");
    }
}

impl From<anyhow::Error> for EvalError {
    fn from(e: anyhow::Error) -> Self {
        Self::new_error(e)
    }
}

impl From<ConsError> for EvalError {
    fn from(e: ConsError) -> Self {
        Self::new_error(anyhow::anyhow!(e))
    }
}

impl From<String> for EvalError {
    fn from(e: String) -> Self {
        Self::new_error(anyhow::anyhow!(e))
    }
}

impl From<&'static str> for EvalError {
    fn from(e: &'static str) -> Self {
        Self::new_error(anyhow::anyhow!(e))
    }
}

impl From<TypeError> for EvalError {
    fn from(e: TypeError) -> Self {
        Self::new_error(e.into())
    }
}

impl From<ArgError> for EvalError {
    fn from(e: ArgError) -> Self {
        Self::new_error(e.into())
    }
}

impl From<std::convert::Infallible> for EvalError {
    fn from(e: std::convert::Infallible) -> Self {
        Self::new_error(e.into())
    }
}

pub(crate) type EvalResult<'ob> = Result<GcObj<'ob>, EvalError>;

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
        let Self { expect, actual, name } = self;
        write!(f, "Expected {expect} argument(s) for `{name}', but found {actual}")
    }
}

impl ArgError {
    pub(crate) fn new(expect: u16, actual: u16, name: impl AsRef<str>) -> ArgError {
        Self { expect, actual, name: name.as_ref().to_owned() }
    }
}

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
        T: Into<super::object::Object<'ob>>,
    {
        let obj = obj.into();
        Self { expect, actual: obj.get_type(), print: obj.to_string() }
    }
}
