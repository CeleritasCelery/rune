use crate::lisp_object::{LispObj, Fixnum, BuiltInFn, FnArgs};
use std::convert::TryInto;
use fn_macros::lisp_fn;

#[lisp_fn(name = "+")]
pub fn add(lhs: LispObj, rhs: LispObj) -> LispObj {
    let x: Fixnum = lhs.try_into().expect("lhs is not a number");
    let y: Fixnum = rhs.try_into().expect("rhs is not a number");
    LispObj::from(x + y)
}

#[lisp_fn(name = "-")]
pub fn sub(lhs: LispObj, rhs: LispObj) -> LispObj {
    let x: Fixnum = lhs.try_into().expect("lhs is not a number");
    let y: Fixnum = rhs.try_into().expect("rhs is not a number");
    LispObj::from(x - y)
}

#[lisp_fn(name = "*")]
pub fn mul(lhs: LispObj, rhs: LispObj) -> LispObj {
    let x: Fixnum = lhs.try_into().expect("lhs is not a number");
    let y = rhs.try_into().expect("rhs is not a number");
    LispObj::from(x * y)
}

#[lisp_fn(name = "/")]
pub fn div(lhs: LispObj, rhs: LispObj) -> LispObj {
    let x: Fixnum = lhs.try_into().expect("lhs is not a number");
    let y: Fixnum = rhs.try_into().expect("rhs is not a number");
    LispObj::from(x / y)
}

defsubr!(add, sub, mul, div);

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn test_add() {
        assert_eq!(20, add(7.into(), 13.into()));
    }

    #[test]
    fn test_sub() {
        assert_eq!(-6, sub(7.into(), 13.into()));
    }

    #[test]
    fn test_mul() {
        assert_eq!(91, mul(7.into(), 13.into()));
    }

    #[test]
    fn test_div() {
        assert_eq!(2, div(12.into(), 5.into()));
    }
}
