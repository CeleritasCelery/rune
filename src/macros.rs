#[allow(unused_qualifications)]
macro_rules! vec_into {
    ($($x:expr),+ $(,)?) => {vec![$($x.into()),+]};
}

macro_rules! count {
    ( $x:tt $($xs:tt)* ) => (1_usize + count!($($xs)*));
    () => (0_usize);
}

macro_rules! defsubr {
    ($($x:ident),+ $(,)?) => (
        #[allow(unused_qualifications)]
        #[doc(hidden)]
        pub(crate) const DEFSUBR: [crate::core::env::ConstSymbol; count!($($x)+)] = [$(crate::core::env::ConstSymbol::new(paste::paste!{[<I $x>]})),+];
        #[allow(non_snake_case)]
        pub(crate) mod __symbol_bindings {
            bind_symbols!($($x),+);
        }
    );
}

macro_rules! bind_symbols {
    (@step $_idx:expr,) => {};
    (@step $idx:expr, $head:ident, $($tail:ident,)*) => (
        paste::paste!{
            #[allow(dead_code)]
            pub(crate) const [<$head:upper>]: crate::core::env::ConstSymbol = super::DEFSUBR[$idx];
        }
        bind_symbols!(@step $idx + 1_usize, $($tail,)*);
    );
    ($($n:ident),*) => {
        bind_symbols!(@step 0_usize, $($n,)*);
    }
}

macro_rules! define_unbox {
    ($ident:ident, $ty:ty) => {
        define_unbox!($ident, $ident, $ty);
    };
    ($ident:ident, $ty:ident, $self:ty) => {
        #[allow(unused_qualifications)]
        impl<'ob> std::convert::TryFrom<crate::core::object::GcObj<'ob>> for $self {
            type Error = crate::core::error::Error;
            fn try_from(obj: crate::core::object::GcObj<'ob>) -> Result<Self, Self::Error> {
                match obj.get() {
                    crate::core::object::Object::$ident(x) => Ok(x),
                    _ => Err(crate::core::error::Error::from_object(
                        crate::core::error::Type::$ty,
                        obj,
                    )),
                }
            }
        }
        #[allow(unused_qualifications)]
        impl<'ob> std::convert::TryFrom<crate::core::object::GcObj<'ob>> for Option<$self> {
            type Error = crate::core::error::Error;
            fn try_from(obj: crate::core::object::GcObj<'ob>) -> Result<Self, Self::Error> {
                match obj.get() {
                    crate::core::object::Object::$ident(x) => Ok(Some(x)),
                    crate::core::object::Object::Nil => Ok(None),
                    _ => Err(crate::core::error::Error::from_object(
                        crate::core::error::Type::$ty,
                        obj,
                    )),
                }
            }
        }
    };
}

#[cfg(test)]
macro_rules! vec_into_object {
    ($($x:expr),+ $(,)?; $arena:expr) => {vec![$(crate::core::object::IntoObject::into_obj($x, $arena).into()),+]};
}

#[cfg(test)]
macro_rules! into_objects {
    ($($x:expr),+ $(,)?; $arena:expr) => {($(crate::core::object::IntoObject::into_obj($x, $arena).into()),+)};
}
