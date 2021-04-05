use crate::lisp_object::{LispObj, Object, IntoObject, Tag};
use crate::arena::Arena;
use std::fmt;

#[derive(PartialEq, Debug, Clone)]
pub struct Cons {
    car: LispObj,
    cdr: LispObj,
}
define_unbox_ref!(Cons);

#[derive(PartialEq, Debug)]
pub struct ConsX<'a> {
    car: Object<'a>,
    cdr: Object<'a>,
}

impl<'a> ConsX<'a> {
    pub const fn new(car: Object<'a>, cdr: Object<'a>) -> Self {
        Self { car, cdr }
    }

    pub const fn car(&self) -> Object {
        self.car
    }

    pub const fn cdr(&self) -> Object {
        self.cdr
    }

    // TODO: into_object
    // remove this
    pub fn from_cons(cons: &'a Cons) -> ConsX<'a> {
        let car = Object {
            data: cons.car,
            marker: std::marker::PhantomData,
        };

        let cdr = Object {
            data: cons.cdr,
            marker: std::marker::PhantomData,
        };
        Self {car, cdr}
    }
}

impl Cons {
    pub const fn new(car: LispObj, cdr: LispObj) -> Self {
        Self { car, cdr }
    }

    pub const fn car(&self) -> LispObj {
        self.car
    }

    pub const fn cdr(&self) -> LispObj {
        self.cdr
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

impl<'obj> IntoObject<'obj> for Cons {
    fn into_object(self, alloc: &Arena) -> (Object, bool) {
        Object::from_type(alloc, self, Tag::Cons)
    }
}

impl<'obj> IntoObject<'obj> for ConsX<'obj> {
    fn into_object(self, alloc: &'obj Arena) -> (Object, bool) {
        Object::from_type(alloc, self, Tag::Cons)
    }
}

#[macro_export]
macro_rules! cons {
    ($car:expr, $cdr:expr) => {
        crate::lisp_object::Cons::new($car.into(), $cdr.into())
    };
    ($car:expr) => {
        crate::lisp_object::Cons::new($car.into(), false.into())
    };
}

#[macro_export]
macro_rules! list {
    ($x:expr) => (cons!($x));
    ($x:expr, $($y:expr),+ $(,)?) => (cons!($x, list!($($y),+)));
}

#[macro_export]
macro_rules! cons_x {
    ($car:expr, $cdr:expr; $alloc:expr) => {
        crate::lisp_object::ConsX::new($alloc.insert($car), $alloc.insert($cdr))
    };
    ($car:expr; $alloc:expr) => {
        crate::lisp_object::ConsX::new($alloc.insert($car), crate::lisp_object::Object::nil())
    };
}

#[macro_export]
macro_rules! list_x {
    ($x:expr; $alloc:expr) => (cons_x!($x; $alloc));
    ($x:expr, $($y:expr),+ $(,)? ; $alloc:expr) => (cons_x!($x, list_x!($($y),+ ; $alloc) ; $alloc));
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lisp_object::Value;
    use std::mem::size_of;
    #[test]
    fn cons() {
        assert_eq!(16, size_of::<Cons>());
        let cons = cons!("start", cons!(7, cons!(5, 9)));

        let mut x = LispObj::from(cons);
        assert!(matches!(x.val(), Value::Cons(_)));

        let cons1 = x.as_mut_cons().unwrap();
        assert_eq!("start", cons1.car);
        (*cons1).car = "start2".into();
        assert_eq!("start2", cons1.car);

        let cons2 = match cons1.cdr.val() {
            Value::Cons(x) => x,
            _ => unreachable!(),
        };
        assert_eq!(7, cons2.car);

        let cons3 = match cons2.cdr.val() {
            Value::Cons(x) => x,
            _ => unreachable!(),
        };
        assert_eq!(5, cons3.car);
        assert_eq!(9, cons3.cdr);

        assert_eq!(cons!(5, "foo"), cons!(5, "foo"));
        assert_ne!(cons!(5, "foo"), cons!(5, "bar"));
        assert_eq!(list![5, 1, 1.5, "foo"], list![5, 1, 1.5, "foo"]);
        assert_ne!(list![5, 1, 1.5, "foo"], list![5, 1, 1.5, "bar"]);
    }

    #[test]
    fn arena_cons() {
        let arena = Arena::new();
        let x = Cons::new(5.into(), 4.into());
        let y = Cons::new("foo".into(), 1.5.into());
        let x_obj = arena.insert(x);
        let y_obj = arena.insert(y);

        if let Value::Cons(cons) = x_obj.val() {
            assert_eq!(cons!(5, 4), *cons);
        } else {
            panic!("not a cons cell");
        }

        if let Value::Cons(cons) = y_obj.val() {
            assert_eq!(cons!("foo", 1.5), *cons);
        } else {
            panic!("not a cons cell");
        }

        let long = Arena::new();
        let x = long.insert(5);
        {
            let short = Arena::new();
            let y = short.insert(1.3);

            let cons = ConsX::new(x, y);
        }

        let cons = cons_x!(1, "foo"; long);
        let list = list_x!(1, 5, 1.5, "bar"; long);
    }
}
