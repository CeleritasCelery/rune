use crate::arena::Arena;
use crate::cons::Cons;
use crate::error::{Error, Type};
use crate::object::{Function, IntoObject, Object};
use crate::opcode::CodeVec;
use crate::opcode::OpCode;
use crate::symbol::sym;
use std::fmt;

use anyhow::{bail, Result};

use super::data::Data;

/// Argument requirments to a function.
#[derive(Copy, Clone, Debug, PartialEq, Default)]
pub(crate) struct FnArgs {
    /// a &rest argument.
    pub(crate) rest: bool,
    /// minimum required arguments.
    pub(crate) required: u16,
    /// &optional arguments.
    pub(crate) optional: u16,
    /// If this function is advised.
    pub(crate) advice: bool,
}

/// Represents the body of a function that has been byte compiled. Note that
/// this can represent any top level expression, not just functions.
#[derive(Debug, PartialEq)]
pub(crate) struct Expression<'ob> {
    pub(crate) op_codes: CodeVec,
    pub(crate) constants: Vec<Object<'ob>>,
}

/// A function implemented in lisp. Note that all functions are byte compiled,
/// so this contains the byte-code representation of the function.
#[derive(Debug, PartialEq)]
pub(crate) struct LispFn<'ob> {
    pub(crate) body: Expression<'ob>,
    pub(crate) args: FnArgs,
}

impl FnArgs {
    /// Number of arguments needed to fill out the remaining slots on the stack.
    /// If a function has 3 required args and 2 optional, and it is called with
    /// 4 arguments, then 1 will be returned. Indicating that 1 additional `nil`
    /// argument should be added to the stack.
    pub(crate) fn num_of_fill_args(self, args: u16) -> Result<u16> {
        if args < self.required {
            bail!(Error::ArgCount(self.required, args));
        }
        let total_args = self.optional + (self.rest as u16);
        if !self.rest && (args > total_args) {
            bail!(Error::ArgCount(total_args, args));
        }
        Ok(total_args.saturating_sub(args))
    }

    pub(crate) fn rest_args_start(self) -> u16 {
        self.optional
    }
}

define_unbox!(LispFn, Func, &'ob LispFn<'ob>);

impl<'ob> LispFn<'ob> {
    pub(crate) fn new(
        op_codes: CodeVec,
        constants: Vec<Object<'ob>>,
        required: u16,
        optional: u16,
        rest: bool,
    ) -> Self {
        LispFn {
            body: Expression {
                op_codes,
                constants,
            },
            args: FnArgs {
                required,
                optional,
                rest,
                advice: false,
            },
        }
    }
}

impl<'ob> IntoObject<'ob, Object<'ob>> for LispFn<'ob> {
    fn into_obj(self, arena: &'ob Arena) -> Object<'ob> {
        let x: Function = self.into_obj(arena);
        x.into()
    }
}

impl<'old, 'new> LispFn<'old> {
    pub(crate) fn clone_in(&self, arena: &'new Arena) -> LispFn<'new> {
        LispFn {
            body: Expression {
                op_codes: self.body.op_codes.clone(),
                constants: self
                    .body
                    .constants
                    .iter()
                    .map(|x| x.clone_in(arena))
                    .collect(),
            },
            args: self.args,
        }
    }
}

impl<'ob> Default for LispFn<'ob> {
    fn default() -> Self {
        LispFn::new(
            vec_into![OpCode::Constant0, OpCode::Ret].into(),
            vec![Object::NIL],
            0,
            0,
            false,
        )
    }
}

pub(crate) type BuiltInFn = for<'ob> fn(
    &[Object<'ob>],
    &mut crate::data::Environment<'ob>,
    &'ob Arena,
) -> Result<Object<'ob>>;

#[derive(Copy, Clone)]
pub(crate) struct SubrFn {
    pub(crate) subr: BuiltInFn,
    pub(crate) args: FnArgs,
    pub(crate) name: &'static str,
}
define_unbox!(SubrFn, Func, &'ob SubrFn);

#[cfg(test)]
impl SubrFn {
    pub(crate) fn new(
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

impl PartialEq for SubrFn {
    fn eq(&self, other: &Self) -> bool {
        let lhs: fn(&'static _, &'static mut _, &'static _) -> _ = self.subr;
        lhs == other.subr
    }
}

impl<'ob> IntoObject<'ob, Object<'ob>> for SubrFn {
    fn into_obj(self, arena: &'ob Arena) -> Object<'ob> {
        let x: Function = self.into_obj(arena);
        x.into()
    }
}

#[repr(transparent)]
#[derive(Debug)]
pub(crate) struct Macro<'ob>(Cons<'ob>);

impl<'ob> Macro<'ob> {
    pub(crate) fn get(&self) -> Function<'ob> {
        match self.0.cdr().try_into() {
            Ok(f) => f,
            Err(_) => unreachable!("Macro should only contain a valid function"),
        }
    }
}

impl<'ob> TryFrom<&Cons<'ob>> for &Macro<'ob> {
    type Error = anyhow::Error;

    fn try_from(value: &Cons<'ob>) -> Result<Self, Self::Error> {
        match value.car() {
            Object::Symbol(sym) if !sym == &sym::MACRO => {
                let _: Function = value.cdr().try_into()?;
                unsafe {
                    let ptr: *const Cons = value;
                    Ok(&*ptr.cast::<Macro>())
                }
            }
            x => Err(Error::from_object(Type::Symbol, x).into()),
        }
    }
}

impl<'ob> AsRef<Cons<'ob>> for Macro<'ob> {
    fn as_ref(&self) -> &Cons<'ob> {
        &self.0
    }
}

impl<'a, 'ob> From<Data<&'a Macro<'ob>>> for Data<&'a Cons<'ob>> {
    fn from(x: Data<&Macro<'ob>>) -> Data<&'a Cons<'ob>> {
        unsafe { std::mem::transmute::<Data<&Macro>, Data<&Cons>>(x) }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn function() {
        let arena = &Arena::new();
        let constant: Object = 1.into_obj(arena);
        let func = LispFn::new(vec_into![0, 1, 2].into(), vec![constant], 0, 0, false);
        let obj: Object = func.into_obj(arena);
        assert!(matches!(obj, Object::LispFn(_)));
        format!("{}", obj);
        let func = match obj {
            Object::LispFn(x) => x,
            _ => unreachable!("expected lispfn"),
        };
        assert_eq!(func.body.op_codes, vec_into![0, 1, 2].into());
        assert_eq!(func.body.constants, vec_into_object![1; arena]);
        assert_eq!(func.args.required, 0);
        assert_eq!(func.args.optional, 0);
        assert!(!func.args.rest);
    }
}
