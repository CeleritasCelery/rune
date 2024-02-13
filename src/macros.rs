//! Implementations of macros
macro_rules! define_unbox {
    ($ident:ident, $ty:ty) => {
        define_unbox!($ident, $ident, $ty);
    };
    ($ident:ident, $ty:ident, $self:ty) => {
        #[allow(unused_qualifications)]
        impl<'ob> std::convert::TryFrom<crate::core::object::Object<'ob>> for $self {
            type Error = crate::core::error::TypeError;
            fn try_from(obj: crate::core::object::Object<'ob>) -> Result<Self, Self::Error> {
                match obj.untag() {
                    crate::core::object::ObjectType::$ident(x) => Ok(x),
                    _ => {
                        Err(crate::core::error::TypeError::new(crate::core::error::Type::$ty, obj))
                    }
                }
            }
        }
        #[allow(unused_qualifications)]
        impl<'ob> std::convert::TryFrom<crate::core::object::Object<'ob>> for Option<$self> {
            type Error = crate::core::error::TypeError;
            fn try_from(obj: crate::core::object::Object<'ob>) -> Result<Self, Self::Error> {
                match obj.untag() {
                    crate::core::object::ObjectType::NIL => Ok(None),
                    crate::core::object::ObjectType::$ident(x) => Ok(Some(x)),
                    _ => {
                        Err(crate::core::error::TypeError::new(crate::core::error::Type::$ty, obj))
                    }
                }
            }
        }
    };
}

// Implementation in build.rs
macro_rules! defsym {
    ($sym:ident) => {};
    ($sym:ident, $name:literal) => {};
}

macro_rules! defvar_bool {
    ($sym:ident, $value:literal) => {};
    ($sym:ident, $name:literal, $value:literal) => {};
}

macro_rules! defvar {
    ($sym:ident) => {};
    ($sym:ident, list![$($values:expr),+ $(,)?]) => {};
    ($sym:ident, $value:expr) => {};
    ($sym:ident, $name:literal, $value:expr) => {};
}
