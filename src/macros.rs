macro_rules! count {
    ( $x:tt $($xs:tt)* ) => (1_usize + count!($($xs)*));
    () => (0_usize);
}

macro_rules! define_symbols {
    (
        FUNCS => {$($fn_sym:ident),+ $(,)?}
        $(VARS => {$($var_sym:ident),+ $(,)?})?
        $(SYMS => {$($raw_sym:ident $(= $raw_name:literal)?),+ $(,)?})?
    ) => (
        paste::paste!{
            $($(__defsym!($raw_sym $(, $raw_name)?);)+)?

            #[allow(unused_qualifications)]
            #[doc(hidden)]
            pub(crate) const DEFSUBR: [crate::core::env::ConstSymbol; count!($($fn_sym)+ $($($var_sym)+)? $($($raw_sym)+)?)] = [
                $(crate::core::env::ConstSymbol::new([<__FN_PTR_ $fn_sym>])),+
                $(, $(crate::core::env::ConstSymbol::new([<__FN_PTR_ $var_sym>])),+)?
                $(, $(crate::core::env::ConstSymbol::new([<__FN_PTR_ $raw_sym>])),+)?
            ];

            #[allow(unused_qualifications)]
            #[doc(hidden)]
            pub(crate) fn __init_vars<'ob>(
                _cx: &'ob crate::core::gc::Context,
                _env: &mut crate::core::gc::Rt<crate::core::env::Env>
            ) {
                $($(_env.vars.insert(&$var_sym, [<__INIT_ $var_sym>](_cx));)+)?
            }
        }

        #[allow(non_snake_case)]
        #[doc(hidden)]
        pub(crate) mod __symbol_bindings {
            __bind_symbols!($($fn_sym),+ $(, $($var_sym),+)? $(, $($raw_sym),+)?);
        }
    );
}

#[doc(hidden)]
macro_rules! __bind_symbols {
    (@step $_idx:expr,) => {};
    (@step $idx:expr, $head:ident, $($tail:ident,)*) => (
        paste::paste!{
            #[allow(dead_code)]
            #[doc(hidden)]
            pub(crate) const [<$head:upper>]: crate::core::env::ConstSymbol = super::DEFSUBR[$idx];
        }
        __bind_symbols!(@step $idx + 1_usize, $($tail,)*);
    );
    ($($n:ident),* $(,)?) => {
        __bind_symbols!(@step 0_usize, $($n,)*);
    }
}

#[doc(hidden)]
macro_rules! __defsym {
    (@internal $sym:ident, $name:expr) => {
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
    ($sym:ident) => (__defsym!(@internal $sym, fn_macros::varname!($sym)););
    ($sym:ident, $name:literal) => (__defsym!(@internal $sym, $name););
}

macro_rules! defvar {
    (@internal $sym:ident, $name:expr, $cx:ident, $value:expr) => (
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
            fn [<__INIT_ $sym>]<'ob> ($cx: &'ob crate::core::gc::Context) -> crate::core::object::GcObj<'ob> {
                $value
            }
        }
    );
    ($sym:ident) => (defvar!(@internal $sym, fn_macros::varname!($sym), _a, crate::core::object::Gc::NIL););
    ($sym:ident, list!($($values:expr),+ $(,)?)) => (defvar!(@internal $sym, fn_macros::varname!($sym), cx, crate::list!($($values),+; cx)););
    ($sym:ident, $value:expr) => (defvar!(@internal $sym, fn_macros::varname!($sym), cx, cx.add($value)););
    ($sym:ident, $name:literal, $value:expr) => (defvar!(@internal $sym, $name, cx, cx.add($value)););
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

macro_rules! vec_into {
    ($($x:expr),+ $(,)?) => {vec![$($x.into()),+]};
}

#[cfg(test)]
macro_rules! vec_into_object {
    ($($x:expr),+ $(,)?; $cx:expr) => {vec![$(crate::core::object::IntoObject::into_obj($x, $cx).into()),+]};
}

#[cfg(test)]
macro_rules! into_objects {
    ($($x:expr),+ $(,)?; $cx:expr) => {($(crate::core::object::IntoObject::into_obj($x, $cx).into()),+)};
}
