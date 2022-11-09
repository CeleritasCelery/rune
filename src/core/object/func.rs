use super::{
    super::{
        error::ArgError,
        gc::{Block, Context, Root},
    },
    display_slice, nil,
};
use super::{GcObj, WithLifetime};
use crate::core::gc::{GcManaged, GcMark, Rt, Trace};
use std::fmt::{self, Debug, Display};

use anyhow::{bail, Result};
use fn_macros::Trace;

/// Argument requirments to a function.
#[derive(Copy, Clone, Debug, PartialEq)]
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
#[derive(Debug, PartialEq, Trace)]
pub(crate) struct Expression {
    #[no_trace]
    pub(crate) op_codes: CodeVec,
    pub(crate) constants: Vec<GcObj<'static>>,
}

impl Expression {
    pub(crate) unsafe fn new(op_codes: CodeVec, consts: Vec<GcObj>) -> Expression {
        let constants = std::mem::transmute::<Vec<GcObj>, Vec<GcObj<'static>>>(consts);
        Expression {
            op_codes,
            constants,
        }
    }

    pub(crate) fn constants<'ob, 'a>(&'a self, _cx: &'ob Context) -> &'a [GcObj<'ob>] {
        unsafe { std::mem::transmute::<&'a [GcObj<'static>], &'a [GcObj<'ob>]>(&self.constants) }
    }
}

/// A function implemented in lisp. Note that all functions are byte compiled,
/// so this contains the byte-code representation of the function.
#[derive(PartialEq)]
pub(crate) struct LispFn {
    gc: GcMark,
    pub(crate) body: Expression,
    pub(crate) args: FnArgs,
}

impl<'new> WithLifetime<'new> for &LispFn {
    type Out = &'new LispFn;

    unsafe fn with_lifetime(self) -> Self::Out {
        &*(self as *const LispFn)
    }
}

define_unbox!(LispFn, Func, &'ob LispFn);

impl<'new> LispFn {
    pub(crate) unsafe fn new(op_codes: CodeVec, consts: Vec<GcObj>, args: FnArgs) -> Self {
        Self {
            gc: GcMark::default(),
            body: unsafe { Expression::new(op_codes, consts) },
            args,
        }
    }

    pub(crate) fn clone_in<const C: bool>(&self, bk: &'new Block<C>) -> LispFn {
        LispFn {
            gc: GcMark::default(),
            body: unsafe {
                Expression::new(
                    self.body.op_codes.clone(),
                    self.body.constants.iter().map(|x| x.clone_in(bk)).collect(),
                )
            },
            args: self.args,
        }
    }
}

impl GcManaged for LispFn {
    fn get_mark(&self) -> &GcMark {
        &self.gc
    }
}

impl Trace for LispFn {
    fn trace(&self, stack: &mut Vec<super::RawObj>) {
        self.mark();
        self.body.constants.trace(stack);
    }
}

impl Display for LispFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let args = &self.args;
        let code = display_slice(&self.body.op_codes.0);
        let consts = display_slice(&self.body.constants);
        write!(f, "#[{args:?} {code:?} [{consts:?}]]")
    }
}

impl Debug for LispFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("LispFn")
            .field("args", &self.args)
            .field("body", &self.body)
            .finish()
    }
}

#[derive(PartialEq, Clone, Default, Debug)]
pub(crate) struct CodeVec(pub(crate) Vec<u8>);

impl FnArgs {
    /// Number of arguments needed to fill out the remaining slots on the stack.
    /// If a function has 3 required args and 2 optional, and it is called with
    /// 4 arguments, then 1 will be returned. Indicating that 1 additional `nil`
    /// argument should be added to the stack.
    pub(crate) fn num_of_fill_args(self, args: u16, name: &str) -> Result<u16> {
        if args < self.required {
            bail!(ArgError::new(self.required, args, name));
        }
        let total = self.required + self.optional;
        if !self.rest && (args > total) {
            bail!(ArgError::new(total, args, name));
        }
        Ok(total.saturating_sub(args))
    }
}

pub(crate) type BuiltInFn = for<'ob> fn(
    &[Rt<GcObj<'static>>],
    &mut Root<crate::core::env::Env>,
    &'ob mut Context,
) -> Result<GcObj<'ob>>;

pub(crate) struct SubrFn {
    pub(crate) subr: BuiltInFn,
    pub(crate) args: FnArgs,
    pub(crate) name: &'static str,
}
define_unbox!(SubrFn, Func, &'ob SubrFn);

impl SubrFn {
    pub(crate) fn call<'ob>(
        &self,
        args: &mut Root<Vec<GcObj<'static>>>,
        env: &mut Root<crate::core::env::Env>,
        cx: &'ob mut Context,
    ) -> Result<GcObj<'ob>> {
        {
            let args = args.as_mut(cx);
            let arg_cnt = args.len() as u16;
            let fill_args = self.args.num_of_fill_args(arg_cnt, self.name)?;
            for _ in 0..fill_args {
                args.push(nil());
            }
        }
        (self.subr)(args, env, cx)
    }
}

impl Display for SubrFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(Builtin: {} -> {:?})", &self.name, self.args)
    }
}

impl Debug for SubrFn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl PartialEq for SubrFn {
    #[allow(clippy::fn_to_numeric_cast_any)]
    fn eq(&self, other: &Self) -> bool {
        let lhs = self.subr as *const BuiltInFn;
        let rhs = other.subr as *const BuiltInFn;
        lhs == rhs
    }
}
