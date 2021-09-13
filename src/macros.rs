#[allow(unused_qualifications)]
macro_rules! vec_into {
    ($($x:expr),+ $(,)?) => {vec![$($x.into()),+]};
}

#[allow(meta_variable_misuse)]
macro_rules! count {
    (@replace_expr $_t:tt $sub:expr) => {$sub};
    ($($x:expr)+) => {0_usize $(+ count!(@replace_expr $x 1_usize))*};
}

macro_rules! defsubr {
    ($($x:ident),+ $(,)?) => (
        #[allow(unused_qualifications)]
        pub(crate) static DEFSUBR: [(crate::object::SubrFn, &crate::symbol::GlobalSymbol); count!($($x)+)] = [$((paste::paste!{[<S $x>]}, &paste::paste!{[<$x:upper>]})),+];
    );
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
                match obj.val() {
                    crate::object::Value::$ident(x) => Ok(x),
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
                match obj.val() {
                    crate::object::Value::$ident(x) => Ok(Some(x)),
                    crate::object::Value::Nil => Ok(None),
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
    ($($x:expr),+ $(,)?; $arena:expr) => {vec![$(crate::object::IntoObject::into_obj($x, $arena)),+]};
}

#[cfg(test)]
macro_rules! into_objects {
    ($($x:expr),+ $(,)?; $arena:expr) => {($(crate::object::IntoObject::into_obj($x, $arena)),+)};
}
