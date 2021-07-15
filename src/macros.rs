#[macro_export]
macro_rules! vec_into {
    ($($x:expr),+ $(,)?) => {vec![$($x.into()),+]};
}

#[macro_export]
macro_rules! vec_into_object {
    ($($x:expr),+ $(,)?; $arena:expr) => {vec![$(crate::object::IntoObject::into_obj($x, $arena)),+]};
}

#[macro_export]
macro_rules! into_objects {
    ($($x:expr),+ $(,)?; $arena:expr) => {($(crate::object::IntoObject::into_obj($x, $arena)),+)};
}

macro_rules! count {
    (@replace_expr $_t:tt $sub:expr) => {$sub};
    ($($x:expr)+) => {0_usize $(+ count!(@replace_expr $x 1_usize))*};
}

#[macro_export]
macro_rules! defsubr {
    ($($x:ident),+ $(,)?) => (
        pub const fn defsubr() -> [crate::object::SubrFn; count!($($x)+)] {
            [$(paste::paste!{[<S $x>]}),+]
        }
    );
}

#[macro_use]
macro_rules! define_unbox {
    ($ident:ident) => {
        define_unbox!($ident, $ident);
    };
    ($ident:ident, $ty:ident) => {
        impl<'ob> std::convert::TryFrom<crate::object::Object<'ob>> for $ident {
            type Error = crate::error::Error;
            fn try_from(obj: crate::object::Object) -> Result<Self, Self::Error> {
                match obj.val() {
                    crate::object::Value::$ident(x) => Ok(x),
                    x => Err(crate::error::Error::Type(
                        crate::error::Type::$ty,
                        x.get_type(),
                    )),
                }
            }
        }
        impl<'ob> std::convert::TryFrom<crate::object::Object<'ob>> for Option<$ident> {
            type Error = crate::error::Error;
            fn try_from(obj: crate::object::Object) -> Result<Self, Self::Error> {
                match obj.val() {
                    crate::object::Value::$ident(x) => Ok(Some(x)),
                    crate::object::Value::Nil => Ok(None),
                    x => Err(crate::error::Error::Type(
                        crate::error::Type::$ty,
                        x.get_type(),
                    )),
                }
            }
        }
    };
}

#[macro_use]
macro_rules! define_unbox_ref {
    ($ident:ident) => {
        define_unbox_ref!($ident, $ident);
    };
    ($ident:ident, $ty:ident) => {
        impl<'ob> std::convert::TryFrom<crate::object::Object<'ob>> for &'ob $ident {
            type Error = crate::error::Error;
            fn try_from(obj: crate::object::Object<'ob>) -> Result<Self, Self::Error> {
                match obj.val() {
                    crate::object::Value::$ident(x) => Ok(x),
                    x => Err(crate::error::Error::Type(
                        crate::error::Type::$ty,
                        x.get_type(),
                    )),
                }
            }
        }
        impl<'ob> std::convert::TryFrom<crate::object::Object<'ob>> for Option<&'ob $ident> {
            type Error = crate::error::Error;
            fn try_from(obj: crate::object::Object<'ob>) -> Result<Self, Self::Error> {
                match obj.val() {
                    crate::object::Value::$ident(x) => Ok(Some(x)),
                    crate::object::Value::Nil => Ok(None),
                    x => Err(crate::error::Error::Type(
                        crate::error::Type::$ty,
                        x.get_type(),
                    )),
                }
            }
        }
    };
}
