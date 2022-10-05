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
            // declare the raw symbols (no function or variable bindings). The
            // other types have been declared previously with defun! and defvar!
            $($(__defsym!($raw_sym $(, $raw_name)?);)+)?

            #[allow(unused_qualifications)]
            #[doc(hidden)]
            #[allow(non_snake_case)]
            // Create a constant list of all symbols being defined
            pub(crate) const __SYMBOLS: [crate::core::env::ConstSymbol; count!($($fn_sym)+ $($($var_sym)+)? $($($raw_sym)+)?)] = [
                $(crate::core::env::ConstSymbol::new([<__FN_PTR_ $fn_sym>])),+
                $(, $(crate::core::env::ConstSymbol::new([<__FN_PTR_ $var_sym>])),+)?
                $(, $(crate::core::env::ConstSymbol::new([<__FN_PTR_ $raw_sym>])),+)?
            ];

            #[allow(unused_qualifications)]
            #[doc(hidden)]
            // Create a function that will initialze all variables
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
            // Create global constant symbols. These will be re-exported in the
            // sym module.
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
            #[allow(non_snake_case)]
            #[allow(non_upper_case_globals)]
            pub(crate) const [<$head:upper>]: crate::core::env::ConstSymbol = super::__SYMBOLS[$idx];
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
            #[allow(unused_qualifications)]
            static $sym: crate::core::env::Symbol = crate::core::env::Symbol::new(
                $name,
                crate::core::env::ConstSymbol::new([<__FN_PTR_ $sym>])
            );
            #[allow(non_snake_case)]
            #[allow(unused_qualifications)]
            #[doc(hidden)]
            fn [<__FN_PTR_ $sym>] () -> &'static crate::core::env::Symbol { &$sym }
        }
    };
    ($sym:ident) => (__defsym!(@internal $sym, fn_macros::varname!($sym)););
    ($sym:ident, $name:literal) => (__defsym!(@internal $sym, $name););
}

macro_rules! defvar {
    (@internal $sym:ident, $name:expr, $cx:ident, $value:expr) => (
        paste::paste!{
            #[allow(non_snake_case)]
            #[allow(unused_qualifications)]
            static $sym: crate::core::env::Symbol = crate::core::env::Symbol::new(
                $name,
                crate::core::env::ConstSymbol::new([<__FN_PTR_ $sym>])
            );
            #[allow(non_snake_case)]
            #[doc(hidden)]
            #[allow(unused_qualifications)]
            fn [<__FN_PTR_ $sym>] () -> &'static crate::core::env::Symbol { &$sym }
            #[allow(non_snake_case)]
            #[allow(unused_qualifications)]
            #[doc(hidden)]
            fn [<__INIT_ $sym>]<'ob> ($cx: &'ob crate::core::gc::Context) -> crate::core::object::GcObj<'ob> {
                $value
            }
        }
    );
    ($sym:ident) => (defvar!(@internal $sym, fn_macros::varname!($sym), _a, crate::core::object::nil()););
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
                    crate::core::object::Object::Symbol(s) if s.nil() => Ok(None),
                    crate::core::object::Object::$ident(x) => Ok(Some(x)),
                    _ => Err(crate::core::error::TypeError::new(
                        crate::core::error::Type::$ty,
                        obj,
                    )),
                }
            }
        }
    };
}
