use crate::lisp_object::{LispObj, Fixnum};
use std::convert::TryInto;
use fn_macros::lisp_fn;

#[lisp_fn(name = "+")]
pub fn add(lhs: LispObj, rhs: LispObj) -> Fixnum {
    let x: Fixnum = lhs.try_into().expect("lhs is not a number");
    let y: Fixnum = rhs.try_into().expect("rhs is not a number");
    x + y
}

#[lisp_fn(name = "-")]
pub fn sub(lhs: LispObj, rhs: LispObj) -> Fixnum {
    let x: Fixnum = lhs.try_into().expect("lhs is not a number");
    let y: Fixnum = rhs.try_into().expect("rhs is not a number");
    x - y
}

#[lisp_fn(name = "*")]
pub fn mul(vars: &[LispObj]) -> Fixnum {
    let lhs = *vars.get(0).unwrap();
    let rhs = *vars.get(1).unwrap();
    let x: Fixnum = lhs.try_into().expect("lhs is not a number");
    let y = rhs.try_into().expect("rhs is not a number");
    x * y
}

#[lisp_fn(name = "/")]
pub fn div(lhs: LispObj, rhs: LispObj) -> Fixnum {
    let x: Fixnum = lhs.try_into().expect("lhs is not a number");
    let y: Fixnum = rhs.try_into().expect("rhs is not a number");
    x / y
}

defsubr!(add, sub, mul, div);

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn test_add() {
        assert_eq!(Fixnum::from(20), add(7.into(), 13.into()));
    }

    #[test]
    fn test_sub() {
        assert_eq!(Fixnum::from(-6), sub(7.into(), 13.into()));
    }

    #[test]
    fn test_mul() {
        let args = vec_into![7, 13];
        assert_eq!(Fixnum::from(91), mul(&args));
        assert_eq!(Smul.args.required, 0);
        assert!(Smul.args.rest);
    }

    #[test]
    fn test_div() {
        assert_eq!(Fixnum::from(2), div(12.into(), 5.into()));
    }
}
