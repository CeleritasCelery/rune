use std::convert::TryFrom;
use crate::lisp_object::{Value, LispObj};

pub type Fixnum = i64;

impl TryFrom<LispObj> for Fixnum {
    type Error = ();
    fn try_from(value: LispObj) -> Result<Self, Self::Error> {
        match value.val() {
            Value::Int(i) => Ok(i),
            _ => Err(())
        }
    }
}

