use crate::lisp_object::{LispObj, Tag};
use std::fmt;

#[derive(PartialEq, Debug, Clone)]
pub struct Cons {
    pub car: LispObj,
    pub cdr: LispObj,
}
define_unbox_ref!(Cons);

impl Cons {
    pub fn new(car: LispObj, cdr: LispObj) -> Cons {
        Cons{car, cdr}
    }
}

impl fmt::Display for Cons {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}, {})", self.car, self.cdr)
    }
}

impl From<Cons> for LispObj {
    fn from(cons: Cons) -> Self {
        LispObj::from_tagged_ptr(cons, Tag::Cons)
    }
}

#[macro_export]
macro_rules! cons {
    ($car:expr, $cdr:expr) => (crate::lisp_object::Cons::new($car.into(), $cdr.into()));
    ($car:expr) => (crate::lisp_object::Cons::new($car.into(), false.into()));
}

#[macro_export]
macro_rules! list {
    ($x:expr) => (cons!($x));
    ($x:expr, $($y:expr),+ $(,)?) => (cons!($x, list!($($y),+)));
}


#[cfg(test)]
mod test {
    use super::*;
    use crate::lisp_object::Value;
    use std::mem::size_of;
    #[test]
    fn cons() {
        assert_eq!(16, size_of::<Cons>());
        let cons = cons!("start", cons!(7, cons!(5, 3.3)));

        let mut x = LispObj::from(cons);
        assert!(matches!(x.val(), Value::Cons(_)));
        format!("{}", x);

        let cons1 = x.as_mut_cons().unwrap();
        assert_eq!("start", cons1.car);
        (*cons1).car = "start2".into();
        assert_eq!("start2", cons1.car);

        let cons2 = match cons1.cdr.val() {
            Value::Cons(x) => x,
            _ => unreachable!()
        };
        assert_eq!(7, cons2.car);

        let cons3 = match cons2.cdr.val() {
            Value::Cons(x) => x,
            _ => unreachable!(),
        };
        assert_eq!(5, cons3.car);
        assert_eq!(3.3, cons3.cdr);

        assert_eq!(cons!(5, "foo"), cons!(5, "foo"));
        assert_ne!(cons!(5, "foo"), cons!(5, "bar"));
        assert_eq!(list![5, 1, 1.5, "foo"], list![5, 1, 1.5, "foo"]);
        assert_ne!(list![5, 1, 1.5, "foo"], list![5, 1, 1.5, "bar"]);
    }

}
