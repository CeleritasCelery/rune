use std::ops;
use std::cmp;
use std::convert::{From, TryFrom};
use crate::lisp_object::{Value, TAG_SIZE, LispObj};

#[derive(Copy, Clone, Debug)]
pub struct Fixnum(i64);

impl From<i64> for Fixnum {
    fn from(i: i64) -> Self {Fixnum(i << TAG_SIZE)}
}

impl From<Fixnum> for i64 {
    fn from(f: Fixnum) -> Self {f.0 >> TAG_SIZE}
}

impl From<Fixnum> for LispObj {
    fn from(fixnum: Fixnum) -> Self {
        LispObj{fixnum}
    }
}

impl TryFrom<LispObj> for Fixnum {
    type Error = ();
    fn try_from(value: LispObj) -> Result<Self, Self::Error> {
        if matches!(value.val(), Value::Int(_)) {
            Ok(unsafe{value.fixnum})
        } else {
            Err(())
        }
    }
}

impl cmp::PartialEq for Fixnum {
    fn eq(&self, rhs: &Fixnum) -> bool {
        self.0 == rhs.0
    }
}

impl ops::Add<Fixnum> for Fixnum {
    type Output = Fixnum;
    // i + j
    fn add(self, rhs: Self) -> Self {Self(self.0 + rhs.0)}
}

impl ops::Sub<Fixnum> for Fixnum {
    type Output = Fixnum;
    // i - j
    fn sub(self, rhs: Self) -> Self {Self(self.0 - rhs.0)}
}

impl ops::Mul<Fixnum> for Fixnum {
    type Output = Fixnum;
    // i * (j >> 2)
    fn mul(self, rhs: Self) -> Self {Self(self.0 * i64::from(rhs))}
}

impl ops::Div<Fixnum> for Fixnum {
    type Output = Fixnum;
    // (i/j) << 2
    fn div(self, rhs: Self) -> Self {(self.0 / rhs.0).into()}
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn fixnum() {
        let x = LispObj::from(7);
        assert!(Fixnum::try_from(x).is_ok());
        format!("{}", x);
        assert_eq!(7, x);
        assert_eq!(Fixnum::from(7), Fixnum::try_from(x).unwrap());
    }
}
