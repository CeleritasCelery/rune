use crate::lisp_object::{LispObj, Fixnum};
use std::convert::TryInto;

pub fn add(vars: &[LispObj]) -> LispObj {
    let lhs = *vars.get(0).unwrap();
    let rhs = *vars.get(1).unwrap();
    let x: Fixnum = lhs.try_into().expect("lhs is not a number");
    let y: Fixnum = rhs.try_into().expect("hhs is not a number");
    LispObj::from(x + y)
}

pub fn sub(vars: &[LispObj]) -> LispObj {
    let lhs = *vars.get(0).unwrap();
    let rhs = *vars.get(1).unwrap();
    let x: Fixnum = lhs.try_into().expect("lhs is not a number");
    let y: Fixnum = rhs.try_into().expect("rhs is not a number");
    LispObj::from(x - y)
}

pub fn mul(vars: &[LispObj]) -> LispObj {
    let lhs = *vars.get(0).unwrap();
    let rhs = *vars.get(1).unwrap();
    let x: Fixnum = lhs.try_into().expect("lhs is not a number");
    let y = rhs.try_into().expect("rhs is not a number");
    LispObj::from(x * y)
}

pub fn div(vars: &[LispObj]) -> LispObj {
    let lhs = *vars.get(0).unwrap();
    let rhs = *vars.get(1).unwrap();
    let x: Fixnum = lhs.try_into().expect("lhs is not a number");
    let y: Fixnum = rhs.try_into().expect("rhs is not a number");
    LispObj::from(x / y)
}

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn test_add() {
        let nums = vec![LispObj::from(7), LispObj::from(13)];
        assert_eq!(20, add(&nums));
    }

    #[test]
    fn test_sub() {
        let nums = vec![LispObj::from(7), LispObj::from(13)];
        assert_eq!(-6, sub(&nums));
    }

    #[test]
    fn test_mul() {
        let nums = vec![LispObj::from(7), LispObj::from(13)];
        assert_eq!(91, mul(&nums));
    }

    #[test]
    fn test_div() {
        let nums = vec![LispObj::from(12), LispObj::from(5)];
        assert_eq!(2, div(&nums));
    }
}
