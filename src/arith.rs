use crate::lisp_object::NumberValue::{Float, Int};
use crate::lisp_object::{Number, NumberValue};

#[allow(clippy::trivially_copy_pass_by_ref)]
fn arith(
    cur: NumberValue,
    next: &Number,
    int_fn: fn(i64, i64) -> i64,
    float_fn: fn(f64, f64) -> f64,
) -> NumberValue {
    match cur {
        Float(cur) => match next.val() {
            Float(next) => float_fn(cur, next).into(),
            Int(next) => float_fn(cur, next as f64).into(),
        },
        Int(cur) => match next.val() {
            Float(next) => float_fn(cur as f64, next).into(),
            Int(next) => int_fn(cur, next).into(),
        },
    }
}

impl<'obj> From<Number<'obj>> for NumberValue {
    fn from(num: Number) -> Self {
        match num.val() {
            Int(x) => x.into(),
            Float(x) => x.into(),
        }
    }
}

impl From<f64> for NumberValue {
    fn from(x: f64) -> Self {
        Float(x)
    }
}

impl From<i64> for NumberValue {
    fn from(x: i64) -> Self {
        Int(x)
    }
}

#[lisp_fn(name = "+")]
pub fn add(vars: &[Number]) -> NumberValue {
    use std::ops::Add;
    vars.iter()
        .fold(0.into(), |acc, x| arith(acc, x, Add::add, Add::add))
}

#[lisp_fn(name = "-")]
pub fn sub(number: Option<Number>, numbers: &[Number]) -> NumberValue {
    use std::ops::Sub;
    let num = match number {
        Some(x) => x.into(),
        None => 0.into(),
    };
    // If one argument given, negate it
    if numbers.is_empty() {
        match num {
            Int(x) => NumberValue::Int(-x),
            Float(x) => NumberValue::Float(-x),
        }
    } else {
        numbers
            .iter()
            .fold(num, |acc, x| arith(acc, x, Sub::sub, Sub::sub))
    }
}

#[lisp_fn(name = "*")]
pub fn mul(numbers: &[Number]) -> NumberValue {
    use std::ops::Mul;
    numbers
        .iter()
        .fold(1.into(), |acc, x| arith(acc, x, Mul::mul, Mul::mul))
}

#[lisp_fn(name = "/")]
pub fn div(number: Number, divisors: &[Number]) -> NumberValue {
    use std::ops::Div;
    divisors
        .iter()
        .fold(number.into(), |acc, x| arith(acc, x, Div::div, Div::div))
}

#[lisp_fn(name = "1+")]
pub fn plus_one(number: Number) -> NumberValue {
    match number.val() {
        Int(x) => Int(x + 1),
        Float(x) => Float(x + 1.0),
    }
}

#[lisp_fn(name = "1-")]
pub fn minus_one(number: Number) -> NumberValue {
    match number.val() {
        Int(x) => Int(x - 1),
        Float(x) => Float(x - 1.0),
    }
}

impl<'obj> std::cmp::PartialEq<i64> for Number<'obj> {
    fn eq(&self, other: &i64) -> bool {
        match self.val() {
            Int(num) => num == *other,
            Float(num) => num == *other as f64,
        }
    }
}

impl<'obj> std::cmp::PartialEq<f64> for Number<'obj> {
    fn eq(&self, other: &f64) -> bool {
        match self.val() {
            Int(num) => num as f64 == *other,
            Float(num) => num == *other,
        }
    }
}

#[lisp_fn(name = "=")]
#[allow(clippy::float_cmp)]
pub fn num_eq(number: Number, numbers: &[Number]) -> bool {
    match number.val() {
        Int(num) => numbers.iter().all(|&x| x == num),
        Float(num) => numbers.iter().all(|&x| x == num),
    }
}

#[lisp_fn(name = "/=")]
#[allow(clippy::float_cmp)]
pub fn num_ne(number: Number, numbers: &[Number]) -> bool {
    match number.val() {
        Int(num) => numbers.iter().all(|&x| x != num),
        Float(num) => numbers.iter().all(|&x| x != num),
    }
}

impl<'obj> std::cmp::PartialOrd for NumberValue {
    fn partial_cmp(&self, other: &NumberValue) -> Option<std::cmp::Ordering> {
        match self {
            Int(lhs) => match other {
                Int(rhs) => lhs.partial_cmp(rhs),
                Float(rhs) => (*lhs as f64).partial_cmp(rhs),
            },
            Float(lhs) => match other {
                Int(rhs) => lhs.partial_cmp(&(*rhs as f64)),
                Float(rhs) => lhs.partial_cmp(rhs),
            },
        }
    }
}

fn cmp(number: Number, numbers: &[Number], cmp: fn(&NumberValue, &NumberValue) -> bool) -> bool {
    numbers
        .iter()
        .try_fold(number.val(), |acc, &x| {
            if cmp(&acc, &x.val()) {
                Some(Int(0))
            } else {
                None
            }
        })
        .is_some()
}

#[lisp_fn(name = "<")]
pub fn less_than(number: Number, numbers: &[Number]) -> bool {
    cmp(number, numbers, NumberValue::lt)
}

#[lisp_fn(name = "<=")]
pub fn less_than_or_eq(number: Number, numbers: &[Number]) -> bool {
    cmp(number, numbers, NumberValue::le)
}

#[lisp_fn(name = ">")]
pub fn greater_than(number: Number, numbers: &[Number]) -> bool {
    cmp(number, numbers, NumberValue::gt)
}

#[lisp_fn(name = ">=")]
pub fn greater_than_or_eq(number: Number, numbers: &[Number]) -> bool {
    cmp(number, numbers, NumberValue::ge)
}

defsubr!(
    add,
    sub,
    mul,
    div,
    plus_one,
    minus_one,
    num_eq,
    num_ne,
    less_than,
    less_than_or_eq,
    greater_than,
    greater_than_or_eq
);

#[cfg(test)]
mod test {
    use super::*;
    use crate::arena::Arena;
    use crate::lisp_object::IntoObject;

    #[test]
    #[allow(clippy::float_cmp)]
    fn test_add() {
        let arena = &Arena::new();

        let num = add(&[]);
        assert_eq!(num, Int(0));

        let args = vec_into_object![7, 13; arena];
        let num = add(&args);
        assert_eq!(num, Int(20));

        let args = vec_into_object![1, 2.5; arena];
        let num = add(&args);
        assert_eq!(num, Float(3.5));
    }

    #[test]
    fn test_sub() {
        let arena = &Arena::new();

        let num = sub(None, &[]);
        assert_eq!(num, Int(0));

        let num = sub(Some(7.into_obj(arena)), &[]);
        assert_eq!(num, Int(-7));

        let args = vec_into_object![13; arena];
        let num = sub(Some(7.into_obj(arena)), &args);
        assert_eq!(num, Int(-6));
    }

    #[test]
    fn test_mul() {
        let arena = &Arena::new();
        let num = mul(&[]);
        assert_eq!(num, Int(1));

        let args = vec_into_object![7, 13; arena];
        let num = mul(&args);
        assert_eq!(num, Int(91));
    }

    #[test]
    #[allow(clippy::float_cmp)]
    fn test_div() {
        let arena = &Arena::new();
        let num = div(12.0.into_obj(arena), &[]);
        assert_eq!(num, Float(12.0));

        let args = vec_into_object![5, 2; arena];
        let num = div(12.into_obj(arena), &args);
        assert_eq!(num, Int(1));
    }

    #[test]
    fn test_eq() {
        let arena = &Arena::new();
        assert!(num_eq(1.into_obj(arena), &[]));

        let args = vec_into_object![1.0; arena];
        assert!(num_eq(1.into_obj(arena), &args));

        let args = vec_into_object![1; arena];
        assert!(num_eq(1.0.into_obj(arena), &args));

        let args = vec_into_object![1, 1, 1.1; arena];
        assert!(!num_eq(1.0.into_obj(arena), &args));
    }

    #[test]
    fn test_cmp() {
        let arena = &Arena::new();
        assert!(less_than(1.into_obj(arena), &[]));

        let args = vec_into_object![1.1; arena];
        assert!(less_than(1.into_obj(arena), &args));

        let args = vec_into_object![1; arena];
        assert!(!less_than(1.0.into_obj(arena), &args));

        let args = vec_into_object![1.1, 2, 2.1; arena];
        assert!(less_than(1.0.into_obj(arena), &args));
    }
}
