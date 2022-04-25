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
        pub(crate) static DEFSUBR: [(crate::object::SubrFn, &crate::symbol::GlobalSymbol); count!($($x)+)] = [$((paste::paste!{[<S $x>]}, &paste::paste!{[<$x:upper>]})),+];
    );
}

macro_rules! symbol_match {
    ($v:expr; $($a:ident => $b:expr,)* @ $wildcard:ident => $e:expr $(,)?) => {
        match $v {
            $(v if v == &crate::symbol::sym::$a => $b,)*
            $wildcard => $e,
        }
    };
    ($v:expr; $($a:ident => $b:expr ,)* _ => $e:expr $(,)?) => {
        match $v {
            $(v if v == &crate::symbol::sym::$a => $b,)*
            _ => $e,
        }
    };
}

macro_rules! define_unbox {
    ($ident:ident, $ty:ty) => {
        define_unbox!($ident, $ident, $ty);
    };
    ($ident:ident, $ty:ident, $self:ty) => {
        #[allow(unused_qualifications)]
        impl<'ob> std::convert::TryFrom<crate::object::Object<'ob>> for $self {
            type Error = crate::error::Error;
            fn try_from(obj: crate::object::Object<'ob>) -> Result<Self, Self::Error> {
                match obj.get() {
                    crate::object::ObjectX::$ident(x) => Ok(x),
                    _ => Err(crate::error::Error::from_object(
                        crate::error::Type::$ty,
                        obj,
                    )),
                }
            }
        }
        #[allow(unused_qualifications)]
        impl<'ob> std::convert::TryFrom<crate::object::Object<'ob>> for Option<$self> {
            type Error = crate::error::Error;
            fn try_from(obj: crate::object::Object<'ob>) -> Result<Self, Self::Error> {
                match obj.get() {
                    crate::object::ObjectX::$ident(x) => Ok(Some(x)),
                    crate::object::ObjectX::Nil => Ok(None),
                    _ => Err(crate::error::Error::from_object(
                        crate::error::Type::$ty,
                        obj,
                    )),
                }
            }
        }
    };
}

#[cfg(test)]
macro_rules! vec_into_object {
    ($($x:expr),+ $(,)?; $arena:expr) => {vec![$(crate::object::IntoObject::into_obj($x, $arena).into()),+]};
}

#[cfg(test)]
macro_rules! into_objects {
    ($($x:expr),+ $(,)?; $arena:expr) => {($(crate::object::IntoObject::into_obj($x, $arena).into()),+)};
}
