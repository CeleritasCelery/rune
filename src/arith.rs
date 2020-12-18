use crate::lisp_object::LispObj;
use std::convert::TryInto;
use fn_macros::lisp_fn;

#[lisp_fn(name = "+")]
pub fn add(lhs: i64, rhs: i64) -> i64 {
    lhs + rhs
}

#[lisp_fn(name = "-")]
pub fn sub(lhs: i64, rhs: i64) -> i64 {
    lhs - rhs
}

#[lisp_fn(name = "*")]
pub fn mul(vars: &[LispObj]) -> i64 {
    let lhs = *vars.get(0).unwrap();
    let rhs = *vars.get(1).unwrap();
    let x: i64 = lhs.try_into().expect("lhs is not a number");
    let y: i64 = rhs.try_into().expect("rhs is not a number");
    x * y
}

#[lisp_fn(name = "/")]
pub fn div(lhs: LispObj, rhs: LispObj) -> i64 {
    let x: i64 = lhs.try_into().expect("lhs is not a number");
    let y: i64 = rhs.try_into().expect("rhs is not a number");
    x / y
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
        let args = vec_into![7, 13];
        assert_eq!(91, mul(&args));
        assert_eq!(Smul.args.required, 0);
        assert!(Smul.args.rest);
    }

    #[test]
    fn test_div() {
        assert_eq!(2, div(12.into(), 5.into()));
    }
}
