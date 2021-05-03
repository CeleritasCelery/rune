use crate::arena::Arena;
use crate::lisp_object::{Number, NumberValue};

enum NumberFold {
    Int(i64),
    Float(f64),
}

impl NumberFold {
    fn acc(
        cur: Self,
        next: &Number,
        int_fn: fn(i64, i64) -> i64,
        float_fn: fn(f64, f64) -> f64,
    ) -> NumberFold {
        use NumberValue::{Float, Int};
        match cur {
            NumberFold::Float(cur) => match next.val() {
                Float(next) => float_fn(cur, next).into(),
                Int(next) => float_fn(cur, next as f64).into(),
            },
            NumberFold::Int(cur) => match next.val() {
                Float(next) => float_fn(cur as f64, next).into(),
                Int(next) => int_fn(cur, next).into(),
            },
        }
    }
}

impl<'obj> From<Number<'obj>> for NumberFold {
    fn from(num: Number) -> Self {
        match num.val() {
            NumberValue::Int(x) => x.into(),
            NumberValue::Float(x) => x.into(),
        }
    }
}

impl NumberFold {
    fn into_number(self, arena: &Arena) -> Number {
        match self {
            NumberFold::Int(x) => arena.insert(x),
            NumberFold::Float(x) => arena.insert(x),
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
pub fn add<'obj>(vars: &[Number], arena: &'obj Arena) -> Number<'obj> {
    use std::ops::Add;
    vars.iter()
        .fold(0.into(), |acc, x| {
            NumberFold::acc(acc, x, Add::add, Add::add)
        })
        .into_number(arena)
}

#[lisp_fn(name = "-")]
pub fn sub<'obj>(number: Option<Number>, numbers: &[Number], arena: &'obj Arena) -> Number<'obj> {
    use std::ops::Sub;
    let num = match number {
        Some(x) => x.into(),
        None => 0.into(),
    };
    // If one argument given, negate it
    if numbers.is_empty() {
        match num {
            NumberFold::Int(x) => arena.insert(-x),
            NumberFold::Float(x) => arena.insert(-x),
        }
    } else {
        numbers
            .iter()
            .fold(num, |acc, x| NumberFold::acc(acc, x, Sub::sub, Sub::sub))
            .into_number(arena)
    }
}

#[lisp_fn(name = "*")]
pub fn mul<'obj>(numbers: &[Number], arena: &'obj Arena) -> Number<'obj> {
    use std::ops::Mul;
    numbers
        .iter()
        .fold(1.into(), |acc, x| {
            NumberFold::acc(acc, x, Mul::mul, Mul::mul)
        })
        .into_number(arena)
}

#[lisp_fn(name = "/")]
pub fn div<'obj>(number: Number, divisors: &[Number], arena: &'obj Arena) -> Number<'obj> {
    use std::ops::Div;
    divisors
        .iter()
        .fold(number.into(), |acc, x| {
            NumberFold::acc(acc, x, Div::div, Div::div)
        })
        .into_number(arena)
}

#[lisp_fn(name = "1+")]
pub fn plus_one<'obj>(number: Number<'obj>, arena: &'obj Arena) -> Number<'obj> {
    use NumberValue::*;
    match number.val() {
        Int(x) => arena.insert(x + 1),
        Float(x) => arena.insert(x + 1.0),
    }
}

#[lisp_fn(name = "1-")]
pub fn minus_one<'obj>(number: Number<'obj>, arena: &'obj Arena) -> Number<'obj> {
    use NumberValue::*;
    match number.val() {
        Int(x) => arena.insert(x - 1),
        Float(x) => arena.insert(x - 1.0),
    }
}

impl<'obj> std::cmp::PartialEq<i64> for Number<'obj> {
    fn eq(&self, other: &i64) -> bool {
        match self.val() {
            NumberValue::Int(num) => num == *other,
            NumberValue::Float(num) => num == *other as f64,
        }
    }
}

impl<'obj> std::cmp::PartialEq<f64> for Number<'obj> {
    fn eq(&self, other: &f64) -> bool {
        match self.val() {
            NumberValue::Int(num) => num as f64 == *other,
            NumberValue::Float(num) => num == *other,
        }
    }
}

#[lisp_fn(name = "=")]
pub fn num_eq(number: Number, numbers: &[Number]) -> bool {
    match number.val() {
        NumberValue::Int(num) => numbers.iter().all(|&x| x == num),
        NumberValue::Float(num) => numbers.iter().all(|&x| x == num),
    }
}

defsubr!(add, sub, mul, div, plus_one, minus_one, num_eq);

#[cfg(test)]
mod test {

    use super::*;
    use crate::lisp_object::NumberValue::{Float, Int};

    #[test]
    #[allow(clippy::float_cmp)]
    fn test_add() {
        let arena = Arena::new();

        let num = add(&[], &arena).val();
        assert_eq!(num, Int(0));

        let args = vec_into_object![7, 13; arena];
        let num = add(&args, &arena).val();
        assert_eq!(num, Int(20));

        let args = vec_into_object![1, 2.5; arena];
        let num = add(&args, &arena).val();
        assert_eq!(num, Float(3.5));
    }

    #[test]
    fn test_sub() {
        let arena = Arena::new();

        let num = sub(None, &[], &arena).val();
        assert_eq!(num, Int(0));

        let num = sub(Some(arena.insert(7)), &[], &arena).val();
        assert_eq!(num, Int(-7));

        let args = vec_into_object![13; arena];
        let num = sub(Some(arena.insert(7)), &args, &arena).val();
        assert_eq!(num, Int(-6));
    }

    #[test]
    fn test_mul() {
        let arena = Arena::new();
        let num = mul(&[], &arena).val();
        assert_eq!(num, Int(1));

        let args = vec_into_object![7, 13; arena];
        let num = mul(&args, &arena).val();
        assert_eq!(num, Int(91));
    }

    #[test]
    #[allow(clippy::float_cmp)]
    fn test_div() {
        let arena = Arena::new();
        let num = div(arena.insert(12.0), &[], &arena).val();
        assert_eq!(num, Float(12.0));

        let args = vec_into_object![5, 2; arena];
        let num = div(arena.insert(12), &args, &arena).val();
        assert_eq!(num, Int(1));
    }

    #[test]
    fn test_eq() {
        let arena = Arena::new();
        assert!(num_eq(arena.insert(1), &[]));

        let args = vec_into_object![1.0; arena];
        assert!(num_eq(arena.insert(1), &args));

        let args = vec_into_object![1; arena];
        assert!(num_eq(arena.insert(1.0), &args));

        let args = vec_into_object![1, 1, 1.1; arena];
        assert!(!num_eq(arena.insert(1.0), &args));
    }
}
