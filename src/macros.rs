#[allow(unused_qualifications)]
macro_rules! vec_into {
    ($($x:expr),+ $(,)?) => {vec![$($x.into()),+]};
}

macro_rules! count {
    ( $x:tt $($xs:tt)* ) => (1_usize + count!($($xs)*));
    () => (0_usize);
}

macro_rules! defsubr {
    ($($fn_sym:ident),+ $(,)? $(; VARS => {$($var_sym:ident),+ $(,)?})?) => (
        paste::paste!{
            #[allow(unused_qualifications)]
            #[doc(hidden)]
            pub(crate) const DEFSUBR: [crate::core::env::ConstSymbol; count!($($fn_sym)+ $($($var_sym)+)?)] = [
                $(crate::core::env::ConstSymbol::new([<__FN_PTR_ $fn_sym>])),+ ,
                $($(crate::core::env::ConstSymbol::new([<__FN_PTR_ $var_sym>])),+)?
            ];

            #[allow(unused_qualifications)]
            #[doc(hidden)]
            pub(crate) fn __init_vars<'ob>(
                _arena: &'ob crate::core::arena::Arena,
                _env: &mut crate::core::arena::Rt<crate::core::env::Environment>
            ) {
                $($(_env.vars.insert(&$var_sym, [<__INIT_ $var_sym>](_arena));)+)?
            }
        }

        #[allow(non_snake_case)]
        #[doc(hidden)]
        pub(crate) mod __symbol_bindings {
            bind_symbols!($($fn_sym),+ , $($($var_sym),+)?);
        }
    );
}

macro_rules! defvar {
    (@internal $sym:ident, $name:literal, $arena:ident, $value:expr) => (
        paste::paste!{
            static $sym: crate::core::env::GlobalSymbol = crate::core::env::GlobalSymbol::new(
                $name,
                crate::core::env::ConstSymbol::new([<__FN_PTR_ $sym>])
            );
            #[allow(non_snake_case)]
            #[doc(hidden)]
            fn [<__FN_PTR_ $sym>] () -> &'static crate::core::env::GlobalSymbol { &$sym }
            #[allow(non_snake_case)]
            #[allow(unused_qualifications)]
            #[doc(hidden)]
            fn [<__INIT_ $sym>]<'ob> ($arena: &'ob crate::core::arena::Arena) -> crate::core::object::GcObj<'ob> {
                $value
            }
        }
    );
    ($sym:ident, $name:literal) => (defvar!(@internal $sym, $name, _a, crate::core::object::Gc::NIL););
    ($sym:ident, $name:literal, list!($($values:expr),+ $(,)?)) => (defvar!(@internal $sym, $name, arena, crate::list!($($values),+; arena)););
    ($sym:ident, $name:literal, $value:expr) => (defvar!(@internal $sym, $name, arena, arena.add($value)););
}

macro_rules! defsym {
    ($sym:ident, $name:literal) => {
        paste::paste! {
            static $sym: crate::core::env::GlobalSymbol = crate::core::env::GlobalSymbol::new(
                $name,
                crate::core::env::ConstSymbol::new([<__FN_PTR_ $sym>])
            );
            #[allow(non_snake_case)]
            #[doc(hidden)]
            fn [<__FN_PTR_ $sym>] () -> &'static crate::core::env::GlobalSymbol { &$sym }
        }
    };
}

macro_rules! bind_symbols {
    (@step $_idx:expr,) => {};
    (@step $idx:expr, $head:ident, $($tail:ident,)*) => (
        paste::paste!{
            #[allow(dead_code)]
            #[doc(hidden)]
            pub(crate) const [<$head:upper>]: crate::core::env::ConstSymbol = super::DEFSUBR[$idx];
        }
        bind_symbols!(@step $idx + 1_usize, $($tail,)*);
    );
    ($($n:ident),* $(,)?) => {
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
            type Error = crate::core::error::TypeError;
            fn try_from(obj: crate::core::object::GcObj<'ob>) -> Result<Self, Self::Error> {
                match obj.get() {
                    crate::core::object::Object::$ident(x) => Ok(x),
                    _ => Err(crate::core::error::TypeError::new(
                        crate::core::error::Type::$ty,
                        obj,
                    )),
                }
            }
        }
        #[allow(unused_qualifications)]
        impl<'ob> std::convert::TryFrom<crate::core::object::GcObj<'ob>> for Option<$self> {
            type Error = crate::core::error::TypeError;
            fn try_from(obj: crate::core::object::GcObj<'ob>) -> Result<Self, Self::Error> {
                match obj.get() {
                    crate::core::object::Object::$ident(x) => Ok(Some(x)),
                    crate::core::object::Object::Nil => Ok(None),
                    _ => Err(crate::core::error::TypeError::new(
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
