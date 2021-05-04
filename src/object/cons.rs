use crate::arena::Arena;
use crate::error::{Error, Type};
use crate::object::*;
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
            x => Err(Error::Type(Type::Cons, x.get_type())),
        }
    }
}

impl<'a> TryFrom<Object<'a>> for Option<&'a Cons<'a>> {
    type Error = Error;
    fn try_from(obj: Object<'a>) -> Result<Self, Self::Error> {
        match obj.val() {
            Value::Cons(x) => Ok(Some(x)),
            Value::Nil => Ok(None),
            x => Err(Error::Type(Type::Cons, x.get_type())),
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

impl<'obj> IntoObject<'obj, Object<'obj>> for Cons<'obj> {
    fn into_obj(self, arena: &'obj Arena) -> Object<'obj> {
        InnerObject::from_type(self, Tag::Cons, arena).into()
    }
}

#[macro_export]
macro_rules! cons {
    ($car:expr, $cdr:expr; $arena:expr) => {
        crate::object::Cons::new(
            crate::object::IntoObject::into_obj($car, $arena),
            crate::object::IntoObject::into_obj($cdr, $arena),
        )
    };
    ($car:expr; $arena:expr) => {
        crate::object::Cons::new(
            crate::object::IntoObject::into_obj($car, $arena),
            crate::object::Object::nil(),
        )
    };
}

#[macro_export]
macro_rules! list {
    ($x:expr; $arena:expr) => (cons!($x; $arena));
    ($x:expr, $($y:expr),+ $(,)? ; $arena:expr) => (cons!($x, list!($($y),+ ; $arena) ; $arena));
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::object::Value;
    use std::mem::size_of;
    #[test]
    fn cons() {
        let arena = &Arena::new();
        assert_eq!(16, size_of::<Cons>());
        let cons = cons!("start", cons!(7, cons!(5, 9; arena); arena); arena);

        let mut x: Object = cons.into_obj(arena);
        assert!(matches!(x.val(), Value::Cons(_)));

        let cons1 = x.as_mut_cons().unwrap();
        assert_eq!("start", cons1.car);
        (*cons1).car = "start2".into_obj(arena);
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
        assert_eq!(
            list![5, 1, 1.5, "foo"; arena],
            list![5, 1, 1.5, "foo"; arena]
        );
        assert_ne!(
            list![5, 1, 1.5, "foo"; arena],
            list![5, 1, 1.5, "bar"; arena]
        );
    }
}
