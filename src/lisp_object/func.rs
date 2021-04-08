use crate::error::Error;
use crate::hashmap::HashMap;
use crate::lisp_object::{LispObj, Symbol, Tag, IntoObject, Object};
use crate::arena::Arena;
use crate::opcode::CodeVec;
use std::fmt;

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct FnArgs {
    pub rest: bool,
    pub required: u16,
    pub optional: u16,
    pub max_stack_usage: u16,
    pub advice: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LispFn {
    pub op_codes: CodeVec,
    pub constants: Vec<LispObj>,
    pub args: FnArgs,
    arena: Arena,
}
define_unbox_ref!(LispFn, Func);

impl LispFn {
    pub fn new(
        op_codes: CodeVec,
        constants: Vec<LispObj>,
        arena: Arena,
        required: u16,
        optional: u16,
        rest: bool,
    ) -> Self {
        LispFn {
            op_codes,
            constants,
            arena,
            args: FnArgs {
                required,
                optional,
                rest,
                max_stack_usage: 0,
                advice: false,
            },
        }
    }
}

impl From<LispFn> for LispObj {
    fn from(func: LispFn) -> Self {
        LispObj::from_tagged_ptr(func, Tag::LispFn)
    }
}

impl<'obj> IntoObject<'obj> for LispFn {
    fn into_object(self, arena: &'obj crate::arena::Arena) -> (Object<'obj>, bool) {
        Object::from_type(arena, self, Tag::LispFn)
    }
}

pub type BuiltInFn = fn(&[LispObj], &mut HashMap<Symbol, LispObj>) -> Result<LispObj, Error>;

#[derive(Copy, Clone)]
pub struct SubrFn {
    pub subr: BuiltInFn,
    pub args: FnArgs,
    pub name: &'static str,
}
define_unbox_ref!(SubrFn, Func);

impl SubrFn {
    pub fn new(
        name: &'static str,
        subr: BuiltInFn,
        required: u16,
        optional: u16,
        rest: bool,
    ) -> Self {
        Self {
            name,
            subr,
            args: FnArgs {
                required,
                optional,
                rest,
                max_stack_usage: 0,
                advice: false,
            },
        }
    }
}

impl std::fmt::Debug for SubrFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} -> {:?})", &self.name, self.args)
    }
}

impl std::cmp::PartialEq for SubrFn {
    fn eq(&self, other: &Self) -> bool {
        self.subr as fn(&'static _, &'static mut _) -> _ == other.subr
    }
}

impl From<SubrFn> for LispObj {
    fn from(func: SubrFn) -> Self {
        LispObj::from_tagged_ptr(func, Tag::SubrFn)
    }
}

impl<'obj> IntoObject<'obj> for SubrFn {
    fn into_object(self, arena: &'obj crate::arena::Arena) -> (Object<'obj>, bool) {
        Object::from_type(arena, self, Tag::SubrFn)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lisp_object::Value;
    use std::mem::size_of;
    #[test]
    fn function() {
        assert_eq!(88, size_of::<LispFn>());
        let x: LispObj = LispFn::new(
            vec_into![0, 1, 2].into(),
            vec_into![1],
            Arena::new(),
            0,
            0,
            false).into();
        assert!(matches!(x.val(), Value::LispFn(_)));
        format!("{}", x);
        let func = match x.val() {
            Value::LispFn(x) => x,
            _ => unreachable!(),
        };
        assert_eq!(func.op_codes, vec_into![0, 1, 2].into());
        assert_eq!(func.constants, vec_into![1]);
        assert_eq!(func.args.required, 0);
        assert_eq!(func.args.optional, 0);
        assert_eq!(func.args.rest, false);
    }
}
