use crate::arena::Arena;
use crate::object::*;
use crate::opcode::CodeVec;
use std::fmt;

use anyhow::Result;

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
    pub constants: Vec<GcObject>,
    pub args: FnArgs,
}
define_unbox_ref!(LispFn, Func);

impl LispFn {
    pub fn new(
        op_codes: CodeVec,
        constants: Vec<GcObject>,
        required: u16,
        optional: u16,
        rest: bool,
    ) -> Self {
        LispFn {
            op_codes,
            constants,
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

impl<'obj> IntoObject<'obj, Object<'obj>> for LispFn {
    fn into_obj(self, arena: &'obj Arena) -> Object<'obj> {
        let x: Function = self.into_obj(arena);
        x.into()
    }
}

pub type BuiltInFn = for<'obj> fn(
    &[Object<'obj>],
    &mut crate::data::Environment<'obj>,
    &'obj Arena,
) -> Result<Object<'obj>>;

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
        let lhs: fn(&'static _, &'static mut _, &'static _) -> _ = self.subr;
        lhs == other.subr
    }
}

impl<'obj> IntoObject<'obj, Object<'obj>> for SubrFn {
    fn into_obj(self, arena: &'obj Arena) -> Object<'obj> {
        let x: Function = self.into_obj(arena);
        x.into()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::object::Value;

    #[test]
    fn function() {
        let arena = &Arena::new();
        let constant: Object = 1.into_obj(arena);
        let func = LispFn::new(
            vec_into![0, 1, 2].into(),
            vec![unsafe { constant.into_gc() }],
            0,
            0,
            false,
        );
        let obj: Object = func.into_obj(arena);
        assert!(matches!(obj.val(), Value::LispFn(_)));
        format!("{}", obj);
        let func = obj.as_lisp_fn().expect("expected lispfn");
        assert_eq!(func.op_codes, vec_into![0, 1, 2].into());
        assert_eq!(func.constants, vec_into_object![1; arena]);
        assert_eq!(func.args.required, 0);
        assert_eq!(func.args.optional, 0);
        assert_eq!(func.args.rest, false);
    }
}
