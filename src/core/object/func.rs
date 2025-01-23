use super::{
    super::gc::{Block, Context},
    display_slice, CloneIn, IntoObject, LispVec, ObjCell,
};
use super::{Object, WithLifetime};
use crate::{
    core::{
        env::Env,
        gc::{GcHeap, Rt, Slot},
    },
    derive_markable,
};
use anyhow::{bail, ensure, Result};
use rune_macros::Trace;
use std::fmt::{self, Debug, Display};

#[derive(PartialEq, Eq, Trace)]
pub(crate) struct ByteFnPrototype {
    #[no_trace]
    pub(crate) args: FnArgs,
    #[no_trace]
    pub(crate) depth: usize,
    #[no_trace]
    pub(super) op_codes: Box<[u8]>,
    // TODO: remove a level of pointer indirection here.
    pub(super) constants: Slot<&'static LispVec>,
}

/// A function implemented in lisp. Note that all functions are byte compiled,
/// so this contains the byte-code representation of the function.
#[derive(PartialEq, Eq, Trace)]
pub(crate) struct ByteFn(GcHeap<ByteFnPrototype>);

derive_markable!(ByteFn);

impl std::ops::Deref for ByteFn {
    type Target = ByteFnPrototype;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

define_unbox!(ByteFn, Func, &'ob ByteFn);

impl ByteFn {
    pub(in crate::core) fn new(inner: ByteFnPrototype, constant: bool) -> ByteFn {
        ByteFn(GcHeap::new(inner, constant))
    }
    // SAFETY: The caller must ensure that the constants are part of the same
    // block as the bytecode function. Otherwise they will be collected and we
    // will have a dangling pointer. Also this type must immediatly be put into
    // the GC heap, because holding it past garbage collections is unsafe.
    pub(crate) unsafe fn make(
        op_codes: &[u8],
        consts: &LispVec,
        args: FnArgs,
        depth: usize,
    ) -> ByteFnPrototype {
        let op_codes = op_codes.to_vec().into_boxed_slice();
        #[cfg(miri)]
        unsafe {
            // TODO: the opcodes live outside of the heap because we are relying
            // on them having a stable address. Therefore they get leaked. We
            // need to find some way to address this.
            extern "Rust" {
                fn miri_static_root(ptr: *const u8);
            }
            let ptr: *const u8 = op_codes.as_ptr();
            miri_static_root(ptr)
        }
        ByteFnPrototype {
            constants: unsafe { Slot::new(consts.with_lifetime()) },
            op_codes,
            args,
            depth,
        }
    }
}

impl ByteFnPrototype {
    pub(crate) fn codes(&self) -> &[u8] {
        &self.op_codes
    }

    pub(crate) fn consts<'ob>(&'ob self) -> &'ob [Object<'ob>] {
        unsafe { std::mem::transmute::<&'ob [ObjCell], &'ob [Object<'ob>]>(&self.constants) }
    }

    pub(crate) fn index<'ob>(&self, index: usize, cx: &'ob Context) -> Option<Object<'ob>> {
        match index {
            0 => Some((self.args.into_arg_spec() as i64).into()),
            1 => Some(cx.add(self.codes().to_vec())),
            2 => Some(cx.add(self.consts())),
            3 => Some(self.depth.into()),
            _ => None,
        }
    }

    pub(crate) const fn len(&self) -> usize {
        4
    }
}

impl<'new> CloneIn<'new, &'new Self> for ByteFn {
    fn clone_in<const C: bool>(&self, bk: &'new Block<C>) -> super::Gc<&'new Self> {
        let constants = self.constants.clone_in(bk);
        let byte_fn =
            unsafe { ByteFn::make(&self.op_codes, constants.untag(), self.args, self.depth) };
        byte_fn.into_obj(bk)
    }
}

impl Display for ByteFn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let spec = self.args.into_arg_spec();
        let code = display_slice(&self.op_codes);
        let consts = display_slice(&self.constants);
        let depth = self.depth;
        write!(f, "#[{spec} {code} {consts} {depth}]")
    }
}

impl Debug for ByteFn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("ByteFn")
            .field("args", &self.args)
            .field("op_code", &self.op_codes)
            .field("constants", &self.constants)
            .finish_non_exhaustive()
    }
}

/// Argument requirments to a function.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Default)]
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

impl FnArgs {
    pub(crate) fn from_arg_spec(spec: i64) -> Result<Self> {
        // The spec is an integer of the form NNNNNNNRMMMMMMM where the 7bit
        // MMMMMMM specifies the minimum number of arguments, the 7-bit NNNNNNN
        // specifies the maximum number of arguments (ignoring &rest) and the R
        // bit specifies whether there is a &rest argument to catch the
        // left-over arguments.
        ensure!(spec >= 0, "Invalid bytecode argument spec: bits out of range");
        ensure!(spec <= 0x7FFF, "Invalid bytecode argument spec: bits out of range");
        let spec = spec as u16;
        let required = spec & 0x7F;
        let max = (spec >> 8) & 0x7F;
        let Some(optional) = max.checked_sub(required) else {
            bail!("Invalid bytecode argument spec: max of {max} was smaller then min {required}")
        };
        let rest = spec & 0x80 != 0;
        Ok(FnArgs { required, optional, rest, advice: false })
    }

    pub(crate) fn into_arg_spec(self) -> u64 {
        let mut spec = self.required;
        let max = self.required + self.optional;
        spec |= max << 8;
        spec |= u16::from(self.rest) << 7;
        u64::from(spec)
    }
}

pub(crate) type BuiltInFn =
    for<'ob> fn(usize, &mut Rt<Env>, &'ob mut Context) -> Result<Object<'ob>>;

#[derive(Eq)]
pub(crate) struct SubrFn {
    pub(crate) subr: BuiltInFn,
    pub(crate) args: FnArgs,
    pub(crate) name: &'static str,
}
define_unbox!(SubrFn, Func, &'ob SubrFn);

impl SubrFn {
    pub(crate) fn call<'ob>(
        &self,
        arg_cnt: usize,
        env: &mut Rt<Env>,
        cx: &'ob mut Context,
    ) -> Result<Object<'ob>> {
        (self.subr)(arg_cnt, env, cx)
    }
}

impl<'new> WithLifetime<'new> for &SubrFn {
    type Out = &'new SubrFn;

    unsafe fn with_lifetime(self) -> Self::Out {
        &*(self as *const SubrFn)
    }
}

impl Display for SubrFn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "#<subr {}>", &self.name)
    }
}

impl Debug for SubrFn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl PartialEq for SubrFn {
    #[expect(clippy::fn_to_numeric_cast_any)]
    fn eq(&self, other: &Self) -> bool {
        let lhs = self.subr as *const BuiltInFn;
        let rhs = other.subr as *const BuiltInFn;
        lhs == rhs
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn check_arg_spec(spec: i64) {
        assert_eq!(spec, FnArgs::from_arg_spec(spec).unwrap().into_arg_spec().try_into().unwrap());
    }

    #[test]
    fn test_arg_spec() {
        check_arg_spec(0);
        check_arg_spec(257);
        check_arg_spec(513);
        check_arg_spec(128);
        check_arg_spec(771);

        assert!(FnArgs::from_arg_spec(12345).is_err());
        assert!(FnArgs::from_arg_spec(1).is_err());
        assert!(FnArgs::from_arg_spec(0xFFFF).is_err());
    }
}
