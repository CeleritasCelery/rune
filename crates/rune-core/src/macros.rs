#![allow(clippy::crate_in_macro_def)]
//! Redefinition of the `macro_exported` macros to avoid namespace
//! colision when the macros have the same name as modules.
//!
//! An example of that is the `crate::error` module, and the [`macro@error`] macro.
#[macro_export]
#[doc(hidden)]
macro_rules! __define_unbox {
    ($ident:ident, $ty:ty) => {
        define_unbox!($ident, $ident, $ty);
    };
    ($ident:ident, $ty:ident, $self:ty) => {
        #[allow(unused_qualifications)]
        impl<'ob> std::convert::TryFrom<crate::object::Object<'ob>> for $self {
            type Error = crate::error::TypeError;
            fn try_from(obj: crate::object::Object<'ob>) -> Result<Self, Self::Error> {
                match obj.untag() {
                    crate::object::Object::$ident(x) => Ok(x),
                    _ => Err(crate::error::TypeError::new(crate::error::Type::$ty, obj)),
                }
            }
        }
        #[allow(unused_qualifications)]
        impl<'ob> std::convert::TryFrom<crate::object::Object<'ob>> for Option<$self> {
            type Error = crate::error::TypeError;
            fn try_from(obj: crate::object::Object<'ob>) -> Result<Self, Self::Error> {
                match obj.untag() {
                    crate::object::Object::NIL => Ok(None),
                    crate::object::Object::$ident(x) => Ok(Some(x)),
                    _ => Err(crate::error::TypeError::new(crate::error::Type::$ty, obj)),
                }
            }
        }
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! __list {
    ($x:expr; $cx:expr) => {
        crate::core::object::Object::from(crate::core::cons::Cons::new1($x, $cx))
    };
    ($x:expr, $($y:expr),+ $(,)? ; $cx:expr) => {
        crate::core::object::Object::from(crate::core::cons::Cons::new($x, list!($($y),+ ; $cx), $cx))
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! __error {
    ($msg:literal $(,)?  $($args:expr),* $(,)?) => (crate::eval::EvalError::new_error(anyhow::anyhow!($msg, $($args),*)));
    ($err:expr) => (crate::eval::EvalError::new($err));
}

#[macro_export]
#[doc(hidden)]
macro_rules! __bail_err {
    ($($args:expr),* $(,)?) => (return Err($crate::macros::error!($($args),*)));
}

#[macro_export]
#[doc(hidden)]
macro_rules! __root {
    ($ident:ident, $cx:ident) => {
        rune_core::macros::root!(@ $ident, unsafe { crate::core::gc::IntoRoot::into_root($ident) }, $cx);
    };
    ($ident:ident, init($init:expr), $cx:ident) => {
        let value = $init;
        rune_core::macros::root!(@ $ident, value, $cx);
    };
    ($ident:ident, new($path:tt$(<$ty:ty>)?), $cx:ident) => {
        let value = $path$(::<$ty>)?::default();
        rune_core::macros::root!(@ $ident, value, $cx);
    };
    ($ident:ident, $value:expr, $cx:ident) => {
        rune_core::macros::root!(@ $ident, unsafe { crate::core::gc::IntoRoot::into_root($value) }, $cx);
    };
    (@ $ident:ident, $value:expr, $cx:ident) => {
        let mut rooted = $value;
        let mut root =
            unsafe { crate::core::gc::__StackRoot::new(&mut rooted, $cx.get_root_set()) };
        let $ident = root.as_mut();
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! __last {
    ($arg:expr) => { $arg };
    ($head:expr, $($rest:expr),+) => {
        $crate::macros::last!($($rest),+)
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! __rebind {
    // rebind!(func(x, cx)?)
    ($($path:ident).*($($arg:expr),+)$($x:tt)?) => {{
        rebind!($($path).*($($arg),+)$($x)?, $crate::macros::last!($($arg),+))
    }};
    // rebind!(func(x, cx))
    ($($path:ident).*($($arg:expr),+)) => {{
        rebind!($($path).*($($arg),+), $crate::macros::last!($($arg),+))
    }};
    // rebind!(func(x, cx).unwrap())
    ($($path:ident).*($($arg:expr),+).unwrap()) => {{
        rebind!($($path).*($($arg),+).unwrap(), $crate::macros::last!($($arg),+))
    }};
    // rebind!(x, cx)
    ($value:expr, $cx:expr) => {{
        // Eval value outside the unsafe block
        let unbound = match $value {
            v => unsafe { crate::core::object::WithLifetime::<'static>::with_lifetime(v) }
        };
        $cx.bind(unbound)
    }};
}

#[macro_export]
#[doc(hidden)]
macro_rules! __call {
    ($fn:ident $(,$args:expr)* ; $env:expr, $cx:expr) => {{
        let frame = &mut crate::core::env::CallFrame::new($env);
        $(frame.push_arg($args);)*
        crate::core::gc::Rt::<crate::core::gc::Slot<crate::core::object::Gc::<crate::core::object::FunctionType>>>::call(
            $fn, frame, None, $cx
        )
    }};
    ($fn:ident $(,$args:expr)* ; $name:expr, $env:expr, $cx:expr) => {{
        let frame = &mut crate::core::env::CallFrame::new($env);
        $(frame.push_arg($args);)*
        crate::core::gc::Rt::<crate::core::gc::Slot<crate::core::object::Gc::<crate::core::object::FunctionType>>>::call(
            $fn, frame, Some($name), $cx
        )
    }};
}

/// TODO: Document
#[doc(inline)]
pub use __bail_err as bail_err;

/// TODO: Document
#[doc(inline)]
pub use __define_unbox as define_unbox;

/// TODO: Document
#[doc(inline)]
pub use __error as error;

/// TODO: Document
#[doc(inline)]
pub use __list as list;

/// Helper macro for the `rebind!` macro
#[doc(inline)]
pub use __last as last;

/// Helper macro to call a function with arguments
#[doc(inline)]
pub use __call as call;

/// Rebinds an object so that it is bound to an immutable borrow of `crate::gc::Context`
/// instead of a mutable borrow. This can release the mutable borrow and allow
/// Context to be used for other things.
///
/// # Examples
///
/// ```ignore
/// let object = rebind!(func1(&mut cx));
/// func2(&mut cx);
/// let object2 = object;
/// ```
///
/// wthout this macro the above code would not compile because `object` can't
/// outlive the call to func2.
#[doc(inline)]
pub use __rebind as rebind;

/// Creates a new root that will be traced during garbage collection. The value
/// returned by this macro is no longer bound to the `Context` and so can be
/// used outside of the `Context`'s lifetime. The root is tied to the stack, and
/// will be unrooted when it goes out of scope.
#[doc(inline)]
pub use __root as root;
