#[macro_export]
macro_rules! vec_into {
    ($($x:expr),+ $(,)?) => {vec![$($x.into()),+]};
}

#[macro_export]
macro_rules! vec_into_object {
    ($($x:expr),+ $(,)?; $arena:expr) => {vec![$($arena.insert($x)),+]};
}

macro_rules! count {
    (@replace_expr $_t:tt $sub:expr) => {$sub};
    ($($x:expr)+) => {0_usize $(+ count!(@replace_expr $x 1_usize))*};
}

#[macro_export]
macro_rules! defsubr {
    ($($x:ident),+ $(,)?) => (
        pub const fn defsubr() -> [crate::lisp_object::SubrFn; count!($($x)+)] {
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
        impl<'obj> std::convert::TryFrom<crate::lisp_object::Object<'obj>> for $ident {
            type Error = crate::error::Error;
            fn try_from(obj: crate::lisp_object::Object) -> Result<Self, Self::Error> {
                match obj.val() {
                    crate::lisp_object::Value::$ident(x) => Ok(x),
                    x => Err(crate::error::Error::Type(
                        crate::error::Type::$ty,
                        x.get_type(),
                    )),
                }
            }
        }
        impl<'obj> std::convert::TryFrom<crate::lisp_object::Object<'obj>> for Option<$ident> {
            type Error = crate::error::Error;
            fn try_from(obj: crate::lisp_object::Object) -> Result<Self, Self::Error> {
                match obj.val() {
                    crate::lisp_object::Value::$ident(x) => Ok(Some(x)),
                    crate::lisp_object::Value::Nil => Ok(None),
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
        impl<'a> std::convert::TryFrom<crate::lisp_object::Object<'a>> for &'a $ident {
            type Error = crate::error::Error;
            fn try_from(obj: crate::lisp_object::Object<'a>) -> Result<Self, Self::Error> {
                match obj.val() {
                    crate::lisp_object::Value::$ident(x) => Ok(x),
                    x => Err(crate::error::Error::Type(
                        crate::error::Type::$ty,
                        x.get_type(),
                    )),
                }
            }
        }
        impl<'a> std::convert::TryFrom<crate::lisp_object::Object<'a>> for Option<&'a $ident> {
            type Error = crate::error::Error;
            fn try_from(obj: crate::lisp_object::Object<'a>) -> Result<Self, Self::Error> {
                match obj.val() {
                    crate::lisp_object::Value::$ident(x) => Ok(Some(x)),
                    crate::lisp_object::Value::Nil => Ok(None),
                    x => Err(crate::error::Error::Type(
                        crate::error::Type::$ty,
                        x.get_type(),
                    )),
                }
            }
        }
    };
}
