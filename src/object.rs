//! The core object defintions.
//!
//! Objects are implemented as rust enum with at max a 56 bit payload. This
//! means that it will always be 64 bits. 32 bit systems are not supported.
//! Because of this it gives us more flexibility in the amount of information we
//! can encode in the object header. For example, we can have 255 variants of
//! objects before we need to box the object header. We are making the
//! assumption that pointers are no bigger then 56 bits and that they are word
//! aligned. All objects should be bound to a lifetime to ensure sound operation
//! of the vm.

pub(crate) mod sub_type;
pub(crate) use sub_type::*;
pub(crate) mod func;
pub(crate) use func::*;
pub(crate) mod convert;
pub(crate) use convert::*;

use crate::arena::Arena;
use crate::cons::Cons;
use crate::symbol::Symbol;
use data::{Data, Inner};
use std::fmt;

mod data {
    use std::fmt;
    use std::marker::PhantomData;
    use std::ops::{Deref, Not};

    /// The inner data type that hold the value for an object variant. This type
    /// should be no larger then 56 bits. The lowest bit of data is used to encode
    /// the mutability flag: 1 if immutable, 0 if mutable. This should be stored in
    /// the alignment bits that bottom of the pointer.
    #[derive(Copy, Clone)]
    pub(crate) struct Data<T> {
        data: [u8; 7],
        marker: PhantomData<T>,
    }
    pub(super) const ZERO: Data<()> = Data::from_raw(0);

    /// A trait to access the inner value of a [`Data`]
    pub(crate) trait Inner<T> {
        fn inner(self) -> T;
    }

    // We still need to determine when this is sound. Sending `Data<T>` across threads
    // is not safe unless the values are copied with it. Maybe there is a better way
    // to encode that in the type system.
    unsafe impl<T> Send for Data<T> {}

    impl<T> Data<T> {
        #[inline(always)]
        const fn into_raw(self) -> i64 {
            let data = self.data;
            // This operation will take the 56 bit data and left shift it so that
            // the bottom byte is zeroed.
            let whole = [
                0, data[0], data[1], data[2], data[3], data[4], data[5], data[6],
            ];
            // We shift it back down so that original value is reconstructed.
            i64::from_le_bytes(whole) >> 8
        }

        #[inline(always)]
        const fn from_raw(data: i64) -> Self {
            let bytes = data.to_le_bytes();
            // Notice bytes[7] is missing. That is the top byte that is removed.
            Data {
                data: [
                    bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6],
                ],
                marker: PhantomData,
            }
        }
    }

    impl<'a, T> Data<&'a T> {
        /// mark the pointer as immutable. This is done by shifting
        /// the value and setting the LSB to 1. 
        fn immut_bit_pattern(ptr: *const T) -> i64 {
            ((ptr as i64) << 1) | 0x1
        }

        fn mut_bit_pattern(ptr: *const T) -> i64 {
            (ptr as i64) << 1
        }

        #[inline(always)]
        pub(super) fn from_ref(rf: &'a T) -> Self {
            let ptr: *const T = rf;
            let bits = Self::immut_bit_pattern(ptr);
            Self::from_raw(bits)
        }

        #[inline(always)]
        pub(super) fn from_mut_ref(rf: &'a mut T) -> Self {
            let ptr: *mut T = rf;
            let bits = Self::mut_bit_pattern(ptr);
            Self::from_raw(bits)
        }

        #[inline(always)]
        pub(crate) fn inner_mut(self) -> Option<&'a mut T> {
            let bits = self.into_raw();
            let mutable = (bits & 0x1) == 0;
            if mutable {
                let ptr = (bits >> 1) as *mut T;
                unsafe { Some(&mut *ptr) }
            } else {
                None
            }
        }

        pub(super) fn make_read_only(&mut self) {
            self.data[0] |= 0x1;
        }
    }

    impl<'a, T> Inner<&'a T> for Data<&'a T> {
        #[inline(always)]
        fn inner(self) -> &'a T {
            // shift by 1 to remove the mutability flag
            let bits = self.into_raw() >> 1;
            let ptr = bits as *const T;
            unsafe { &*ptr }
        }
    }

    impl Inner<i64> for Data<i64> {
        #[inline(always)]
        fn inner(self) -> i64 {
            self.into_raw()
        }
    }

    impl Data<i64> {
        pub(super) fn from_int(data: i64) -> Self {
            Data::from_raw(data)
        }
    }

    impl<T> Not for Data<T>
    where
        Data<T>: Inner<T>,
    {
        type Output = T;

        #[inline(always)]
        fn not(self) -> Self::Output {
            self.inner()
        }
    }

    impl<T> PartialEq for Data<T>
    where
        T: PartialEq + Copy,
        Data<T>: Inner<T>,
    {
        fn eq(&self, other: &Self) -> bool {
            self.inner() == other.inner()
        }
    }

    impl PartialEq for Data<()> {
        fn eq(&self, _: &Self) -> bool {
            true
        }
    }

    impl<T> fmt::Display for Data<T>
    where
        T: fmt::Display + Copy,
        Data<T>: Inner<T>,
    {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            fmt::Display::fmt(&self.inner(), f)
        }
    }

    impl<T> fmt::Debug for Data<T>
    where
        T: fmt::Debug + Copy,
        Data<T>: Inner<T>,
    {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            fmt::Debug::fmt(&self.inner(), f)
        }
    }

    impl<'a, T> Deref for Data<&'a T> {
        type Target = T;

        #[inline(always)]
        fn deref(&'_ self) -> &'a Self::Target {
            self.inner()
        }
    }

    impl<'a, T> AsRef<T> for Data<&'a T> {
        #[inline(always)]
        fn as_ref<'b>(&'b self) -> &'a T {
            self.inner()
        }
    }
}

#[derive(Copy, Clone, PartialEq)]
pub(crate) enum Object<'ob> {
    Int(Data<i64>),
    Float(Data<&'ob f64>),
    Symbol(Data<Symbol>),
    True(Data<()>),
    Nil(Data<()>),
    Cons(Data<&'ob Cons<'ob>>),
    Vec(Data<&'ob Vec<Object<'ob>>>),
    String(Data<&'ob String>),
    LispFn(Data<&'ob LispFn<'ob>>),
    SubrFn(Data<&'ob SubrFn>),
}

pub(crate) trait Bits {
    fn bits(self) -> u64;
}

impl<'ob> Bits for Object<'ob> {
    fn bits(self) -> u64 {
        unsafe { std::mem::transmute::<Self, u64>(self) }
    }
}

pub(crate) trait IntoObject<'ob, T> {
    fn into_obj(self, arena: &'ob Arena) -> T;
}

impl<'ob> IntoObject<'ob, Object<'ob>> for Object<'ob> {
    fn into_obj(self, _arena: &'ob Arena) -> Object<'ob> {
        self
    }
}

impl<'ob> IntoObject<'ob, Object<'ob>> for &Object<'ob> {
    fn into_obj(self, _arena: &'ob Arena) -> Object<'ob> {
        *self
    }
}

impl<'ob> IntoObject<'ob, Object<'ob>> for Option<Object<'ob>> {
    fn into_obj(self, _arena: &'ob Arena) -> Object<'ob> {
        self.unwrap_or_default()
    }
}

fn vec_clone_in<'old, 'new>(vec: &[Object<'old>], arena: &'new Arena) -> Vec<Object<'new>> {
    vec.iter().map(|x| x.clone_in(arena)).collect()
}

impl<'old, 'new> Object<'old> {
    /// Clone object in a new arena
    pub(crate) fn clone_in(self, arena: &'new Arena) -> Object<'new> {
        // TODO: Handle pointers to the same object
        match self {
            Object::Int(x) => (!x).into(),
            Object::Cons(x) => x.clone_in(arena).into_obj(arena),
            Object::String(x) => (!x).clone().into_obj(arena),
            Object::Symbol(x) => x.inner().into(),
            Object::LispFn(x) => x.clone_in(arena).into_obj(arena),
            Object::SubrFn(x) => (*x).into_obj(arena),
            Object::True(_) => Object::TRUE,
            Object::Nil(_) => Object::NIL,
            Object::Float(x) => x.into_obj(arena),
            Object::Vec(x) => vec_clone_in(&x, arena).into_obj(arena),
        }
    }
}

impl<'ob> Object<'ob> {
    pub(crate) const TRUE: Object<'ob> = Object::True(data::ZERO);
    pub(crate) const NIL: Object<'ob> = Object::Nil(data::ZERO);

    /// Return the type of an object
    pub(crate) const fn get_type(self) -> crate::error::Type {
        use crate::error::Type;
        match self {
            Object::Symbol(_) => Type::Symbol,
            Object::Float(_) => Type::Float,
            Object::String(_) => Type::String,
            Object::Nil(_) => Type::Nil,
            Object::True(_) => Type::True,
            Object::Cons(_) => Type::Cons,
            Object::Vec(_) => Type::Vec,
            Object::Int(_) => Type::Int,
            Object::LispFn(_) | Object::SubrFn(_) => Type::Func,
        }
    }

    pub(crate) fn make_read_only(&mut self) {
        match self {
            Object::Int(_) | Object::Symbol(_) | Object::True(_) | Object::Nil(_) => {}
            Object::Float(x) => x.make_read_only(),
            Object::Cons(cons_obj) => {
                if let Some(cons) = cons_obj.inner_mut() {
                    cons.make_read_only();
                }
                cons_obj.make_read_only();
            }
            Object::Vec(vec_obj) => {
                if let Some(vec) = vec_obj.inner_mut() {
                    for elem in vec {
                        elem.make_read_only();
                    }
                }
                vec_obj.make_read_only();
            }
            Object::String(x) => x.make_read_only(),
            Object::LispFn(func_obj) => {
                if let Some(func) = func_obj.inner_mut() {
                    for elem in &mut func.body.constants {
                        elem.make_read_only();
                    }
                }
                func_obj.make_read_only();
            }
            Object::SubrFn(x) => x.make_read_only(),
        }
    }

    pub(crate) fn ptr_eq(self, other: Object) -> bool {
        use std::mem::transmute;
        match self {
            Object::Nil(_)
            | Object::True(_)
            | Object::Int(_)
            | Object::Float(_)
            | Object::Symbol(_)
            | Object::Cons(_)
            | Object::Vec(_)
            | Object::String(_)
            | Object::LispFn(_)
            | Object::SubrFn(_) => unsafe {
                transmute::<Self, i64>(self) == transmute::<Object, i64>(other)
            },
        }
    }
}

impl<'ob> Default for Object<'ob> {
    fn default() -> Self {
        Object::NIL
    }
}

impl<'ob> fmt::Display for Object<'ob> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Int(x) => write!(f, "{}", x),
            Object::Cons(x) => write!(f, "{}", x),
            Object::Vec(x) => write!(f, "{:?}", x),
            Object::String(x) => write!(f, "\"{}\"", x),
            Object::Symbol(x) => write!(f, "{}", x),
            Object::LispFn(x) => write!(f, "(lambda {:?})", x),
            Object::SubrFn(x) => write!(f, "{:?}", x),
            Object::True(_) => write!(f, "t"),
            Object::Nil(_) => write!(f, "nil"),
            Object::Float(x) => {
                if x.fract() == 0.0_f64 {
                    write!(f, "{:.1}", x)
                } else {
                    write!(f, "{}", x)
                }
            }
        }
    }
}

impl<'ob> fmt::Debug for Object<'ob> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Int(x) => write!(f, "{}", x),
            Object::Cons(x) => write!(f, "{:?}", x),
            Object::Vec(x) => write!(f, "{:?}", x),
            Object::String(x) => {
                write!(
                    f,
                    "\"{}\"",
                    x.chars()
                        .map(|x| if x == '\n' { '↲' } else { x })
                        .collect::<String>()
                )
            }
            Object::Symbol(x) => write!(f, "{}", x),
            Object::LispFn(x) => write!(f, "(lambda {:?})", x),
            Object::SubrFn(x) => write!(f, "{:?}", x),
            Object::True(_) => write!(f, "t"),
            Object::Nil(_) => write!(f, "nil"),
            Object::Float(x) => {
                if x.fract() == 0.0_f64 {
                    write!(f, "{:.1}", x)
                } else {
                    write!(f, "{}", x)
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::{convert::TryInto, mem::size_of};

    #[test]
    fn sizes() {
        assert_eq!(size_of::<isize>(), size_of::<Object>());
        assert_eq!(size_of::<Object>(), size_of::<Option<Object>>());
        unsafe {
            assert_eq!(
                0x1800_i64,
                std::mem::transmute(Object::Int(Data::from_int(0x18)))
            );
        }
    }

    #[test]
    fn integer() {
        let arena = &Arena::new();
        let int: Object = 3.into_obj(arena);
        assert!(matches!(int, Object::Int(_)));
        assert_eq!(int, Object::Int(Data::from_int(3)));
        let int: Object = 0.into_obj(arena);
        assert_eq!(int, Object::Int(Data::from_int(0)));
    }

    #[test]
    fn float() {
        let arena = &Arena::new();
        let x: Object = 1.3.into_obj(arena);
        assert!(matches!(x, Object::Float(_)));
        let float = 1.3;
        assert_eq!(x, Object::Float(Data::from_ref(&float)));
    }

    #[test]
    fn string() {
        let arena = &Arena::new();
        let x: Object = "foo".into_obj(arena);
        assert!(matches!(x, Object::String(_)));
        let cmp = "foo".to_owned();
        assert_eq!(x, Object::String(Data::from_ref(&cmp)));

        let x: Object = "bar".to_owned().into_obj(arena);
        assert!(matches!(x, Object::String(_)));
        let cmp = "bar".to_owned();
        assert_eq!(x, Object::String(Data::from_ref(&cmp)));
    }

    #[test]
    fn vector() {
        let arena = &Arena::new();
        let vec = vec_into_object![1, 2, 3.4, "foo"; arena];
        let x: Object = vec.into_obj(arena);
        assert!(matches!(x, Object::Vec(_)));
        assert_eq!(
            x,
            Object::Vec(Data::from_ref(&vec_into_object![1, 2, 3.4, "foo"; arena]))
        );
    }

    #[test]
    fn other() {
        let t = Object::TRUE;
        assert!(t != Object::NIL);
        let n = Object::NIL;
        assert!(n == Object::NIL);

        let bool_true: Object = true.into();
        assert!(bool_true == Object::TRUE);
        let bool_false: Object = false.into();
        assert!(bool_false == Object::NIL);
    }

    #[test]
    fn symbol() {
        use crate::symbol::sym::test;
        let symbol = &test::FOO;
        let x: Object = symbol.into();
        assert!(matches!(x, Object::Symbol(_)));
        assert_eq!(x, Object::Symbol(Data::from_ref(&test::FOO)));
    }

    #[test]
    fn mutuality() {
        let arena = &Arena::new();
        let inner_cons = Cons::new(1.into(), 4.into());
        let vec = vec_into_object![inner_cons, 2, 3, 4; arena];
        let mut obj = Cons::new(1.into(), arena.add(vec)).into_obj(arena);
        let result: Result<&mut Cons, _> = obj.try_into();
        assert!(result.is_ok());
        obj.make_read_only();
        let result: Result<&mut Cons, _> = obj.try_into();
        assert!(result.is_err());
        if let Object::Cons(cons) = obj {
            let result: Result<&mut Vec<_>, _> = cons.cdr().try_into();
            assert!(result.is_err());
            if let Object::Vec(vec) = cons.cdr() {
                let result: Result<&mut Vec<_>, _> = vec[0].try_into();
                assert!(result.is_err());
            } else {
                unreachable!("object should be vec");
            }
        } else {
            unreachable!("object should be cons");
        }
    }
}
