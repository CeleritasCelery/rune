use crate::lisp_object::*;
use crate::arena::Arena;
use crate::error::{Error, Type};
use std::convert::TryFrom;
use std::fmt;

#[derive(PartialEq, Debug, Copy, Clone)]
pub struct Cons<'a> {
    car: Object<'a>,
    cdr: Object<'a>,
}

impl<'old, 'new> Cons<'old> {
    pub fn clone_in(self, arena: &'new Arena) -> Cons<'new> {
        Cons {
            car: self.car.clone_in(arena),
            cdr: self.cdr.clone_in(arena),
        }
    }
}

impl<'a> TryFrom<Object<'a>> for &'a Cons<'a> {
    type Error = Error;
    fn try_from(obj: Object<'a>) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::Cons(x) => Ok(x),
            x => Err(Error::Type(
                Type::Cons,
                x.get_type(),
            )),
        }
    }
}

impl<'a> TryFrom<Object<'a>> for Option<&'a Cons<'a>> {
    type Error = Error;
    fn try_from(obj: Object<'a>) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::Cons(x) => Ok(Some(x)),
            Value::Nil => Ok(None),
            x => Err(Error::Type(
                Type::Cons,
                x.get_type(),
            )),
        }
    }
}

impl<'a> Cons<'a> {
    pub const fn new(car: Object<'a>, cdr: Object<'a>) -> Self {
        Self { car, cdr }
    }

    pub const fn car(&self) -> Object {
        self.car
    }

    pub const fn cdr(&self) -> Object {
        self.cdr
    }
}

impl<'obj> fmt::Display for Cons<'obj> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}, {})", self.car, self.cdr)
    }
}

impl<'obj> From<Cons<'obj>> for LispObj {
    fn from(cons: Cons) -> Self {
        LispObj::from_tagged_ptr(cons, Tag::Cons)
    }
}

impl<'obj> IntoObject<'obj> for Cons<'obj> {
    fn into_object(self, alloc: &Arena) -> (Object<'obj>, bool) {
        Object::from_type(alloc, self, Tag::Cons)
    }
}

impl<'obj> IntoTagObject<ConsObject> for Cons<'obj> {
    fn into_object(self, arena: &Arena) -> ConsObject {
        let ptr = arena.alloc(self);
        ConsObject(ConsObject::new_tagged(ptr as i64))
    }
}

#[macro_export]
macro_rules! cons {
    ($car:expr, $cdr:expr; $alloc:expr) => {
        crate::lisp_object::Cons::new($alloc.insert($car), $alloc.insert($cdr))
    };
    ($car:expr; $alloc:expr) => {
        crate::lisp_object::Cons::new($alloc.insert($car), crate::lisp_object::Object::nil())
    };
}

#[macro_export]
macro_rules! list {
    ($x:expr; $alloc:expr) => (cons!($x; $alloc));
    ($x:expr, $($y:expr),+ $(,)? ; $alloc:expr) => (cons!($x, list!($($y),+ ; $alloc) ; $alloc));
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lisp_object::Value;
    use std::mem::size_of;
    #[test]
    fn cons() {
        let arena = Arena::new();
        assert_eq!(16, size_of::<Cons>());
        let cons = cons!("start", cons!(7, cons!(5, 9; arena); arena); arena);

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

        assert_eq!(cons!(5, "foo"; arena), cons!(5, "foo"; arena));
        assert_ne!(cons!(5, "foo"; arena), cons!(5, "bar"; arena));
        assert_eq!(list![5, 1, 1.5, "foo"; arena], list![5, 1, 1.5, "foo"; arena]);
        assert_ne!(list![5, 1, 1.5, "foo"; arena], list![5, 1, 1.5, "bar"; arena]);
    }
}
