use crate::lisp_object::{Number, NumberValue};

enum NumberFold {
    Int(i64),
    Float(f64),
}

impl NumberFold {
    fn acc(
        cur: Self, next: &Number,
        int_fn: fn(i64, i64) -> i64,
        float_fn: fn(f64, f64) -> f64
    ) -> NumberFold {
        use NumberValue::{Int, Float};
        match cur {
            NumberFold::Float(cur) => {
                match next.val() {
                    Float(next) => float_fn(cur, next).into(),
                    Int(next) => float_fn(cur, next as f64).into(),
                }
            }
            NumberFold::Int(cur) => {
                match next.val() {
                    Float(next) => float_fn(cur as f64, next).into(),
                    Int(next) => int_fn(cur, next).into(),
                }
            }
        }
    }
}

impl From<Number> for NumberFold {
    fn from(num: Number) -> Self {
        match num.val() {
            NumberValue::Int(x) => x.into(),
            NumberValue::Float(x) => x.into(),
        }
    }
}

impl From<NumberFold> for Number {
    fn from(num: NumberFold) -> Self {
        match num {
            NumberFold::Int(x) => x.into(),
            NumberFold::Float(x) => x.into(),
        }
    }
}

impl From<f64> for NumberFold {
    fn from(x: f64) -> Self {
        NumberFold::Float(x)
    }
}

impl From<i64> for NumberFold {
    fn from(x: i64) -> Self {
        NumberFold::Int(x)
    }
}

#[lisp_fn(name = "+")]
pub fn add(vars: &[Number]) -> Number {
    use std::ops::Add;
    vars.iter().fold(0.into(), |acc, x| {
        NumberFold::acc(acc, x, Add::add, Add::add)
    }).into()
}

#[lisp_fn(name = "-")]
pub fn sub(number: Option<Number>, numbers: &[Number]) -> Number {
    use std::ops::Sub;
    let num = match number {
        Some(x) => x.into(),
        None => 0.into(),
    };
    // If one argument given, negate it
    if numbers.is_empty() {
        match num {
            NumberFold::Int(x) => (-x).into(),
            NumberFold::Float(x) => (-x).into(),
        }
    } else {
        numbers.iter().fold(num, |acc, x| {
            NumberFold::acc(acc, x, Sub::sub, Sub::sub)
        }).into()
    }
}

#[lisp_fn(name = "*")]
pub fn mul(numbers: &[Number]) -> Number {
    use std::ops::Mul;
    numbers.iter().fold(1.into(), |acc, x| {
        NumberFold::acc(acc, x, Mul::mul, Mul::mul)
    }).into()
}

#[lisp_fn(name = "/")]
pub fn div(number: Number, divisors: &[Number]) -> Number {
    use std::ops::Div;
    divisors.iter().fold(number.into(), |acc, x| {
        NumberFold::acc(acc, x, Div::div, Div::div)
    }).into()
}

#[lisp_fn(name = "1+")]
pub fn plus_one(number: Number) -> Number {
    use NumberValue::*;
    match number.val() {
        Int(x) => (x+1).into(),
        Float(x) => (x+1.0).into(),
    }
}

#[lisp_fn(name = "1-")]
pub fn minus_one(number: Number) -> Number {
    use NumberValue::*;
    match number.val() {
        Int(x) => (x-1).into(),
        Float(x) => (x-1.0).into(),
    }
}

defsubr!(add, sub, mul, div, plus_one, minus_one);

#[cfg(test)]
mod test {

    use super::*;
    use crate::lisp_object::NumberValue::{Int, Float};

    #[test]
    fn test_add() {
        match add(&[]).val() {
            Int(x) => assert_eq!(0, x),
            _ => unreachable!(),
        };

        let args = vec_into![7, 13];
        match add(&args).val() {
            Int(x) => assert_eq!(20, x),
            _ => unreachable!(),
        };

        let args = vec_into![1, 2.5];
        match add(&args).val() {
            Float(x) => assert_eq!(3.5, x),
            _ => unreachable!(),
        };
    }

    #[test]
    fn test_sub() {
        match sub(None, &[]).val() {
            Int(x) => assert_eq!(0, x),
            _ => unreachable!(),
        };

        match sub(Some(7.into()), &[]).val() {
            Int(x) => assert_eq!(-7, x),
            _ => unreachable!(),
        };

        let args = vec_into![13];
        match sub(Some(7.into()), &args).val() {
            Int(x) => assert_eq!(-6, x),
            _ => unreachable!(),
        };
    }

    #[test]
    fn test_mul() {
        match mul(&[]).val() {
            Int(x) => assert_eq!(1, x),
            _ => unreachable!(),
        };

        let args = vec_into![7, 13];
        match mul(&args).val() {
            Int(x) => assert_eq!(91, x),
            _ => unreachable!(),
        };
    }

    #[test]
    fn test_div() {
        match div(12.0.into(), &[]).val() {
            Float(x) => assert_eq!(12.0, x),
            _ => unreachable!(),
        };

        let args = vec_into![5, 2];
        match div(12.into(), &args).val() {
            Int(x) => assert_eq!(1, x),
            _ => unreachable!(),
        };
    }
}
