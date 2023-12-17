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
        impl<'ob> std::convert::TryFrom<crate::object::GcObj<'ob>> for $self {
            type Error = crate::error::TypeError;
            fn try_from(obj: crate::object::GcObj<'ob>) -> Result<Self, Self::Error> {
                match obj.untag() {
                    crate::object::Object::$ident(x) => Ok(x),
                    _ => Err(crate::error::TypeError::new(crate::error::Type::$ty, obj)),
                }
            }
        }
        #[allow(unused_qualifications)]
        impl<'ob> std::convert::TryFrom<crate::object::GcObj<'ob>> for Option<$self> {
            type Error = crate::error::TypeError;
            fn try_from(obj: crate::object::GcObj<'ob>) -> Result<Self, Self::Error> {
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
macro_rules! __cons {
    ($car:expr, $cdr:expr; $cx:expr) => {
        $cx.add({
            let car = $cx.add($car);
            let cdr = $cx.add($cdr);
            unsafe { crate::core::cons::Cons::new_unchecked(car, cdr) }
        })
    };
    ($car:expr; $cx:expr) => {
        $cx.add({
            let car = $cx.add($car);
            unsafe { crate::core::cons::Cons::new_unchecked(car, crate::core::object::nil()) }
        })
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! __list {
        ($x:expr; $cx:expr) => ($crate::macros::cons!($x; $cx));
        ($x:expr, $($y:expr),+ $(,)? ; $cx:expr) => ($crate::macros::cons!($x, list!($($y),+ ; $cx) ; $cx));
    }

#[macro_export]
#[doc(hidden)]
macro_rules! __error {
        ($msg:literal $(,)?  $($args:expr),* $(,)?) => (crate::core::error::EvalError::new_error(anyhow::anyhow!($msg, $($args),*)));
        ($err:expr) => (crate::core::error::EvalError::new($err));
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
        rune_core::macros::root!(
            $ident,
            unsafe { crate::core::gc::IntoRoot::into_root($ident) },
            $cx
        );
    };
    // When using this form, `value` should be an intializer that does not need `IntoRoot`
    ($ident:ident, $value:expr, $cx:ident) => {
        let mut rooted = $value;
        let mut root =
            unsafe { crate::core::gc::__StackRoot::new(&mut rooted, $cx.get_root_set()) };
        let $ident = root.as_mut();
    };
}

#[macro_export]
#[doc(hidden)]
macro_rules! __rooted_iter {
    ($ident:ident, $value:expr, $cx:ident) => {
        // Create roots, but don't initialize them
        let mut elem;
        let mut cons;
        let mut root_elem;
        let mut root_cons;
        // use match to ensure that $value is not evaled inside the unsafe block
        let list: crate::core::object::Gc<crate::core::object::List> = match $value {
            // Convert the value into a list
            value => unsafe { crate::core::gc::IntoRoot::into_root(value).try_into()? },
        };
        #[allow(unused_qualifications, unused_mut)]
        let mut $ident = if let crate::core::object::List::Cons(head) = list.untag() {
            use crate::core::{cons, gc, object};
            // If the list is not empty, then initialize the roots and put them
            // in the stack space reserved
            unsafe {
                elem = object::nil();
                cons = object::WithLifetime::with_lifetime(head);
                root_elem = gc::__StackRoot::new(&mut elem, $cx.get_root_set());
                root_cons = gc::__StackRoot::new(&mut cons, $cx.get_root_set());
                cons::ElemStreamIter::new(Some(root_elem.as_mut()), Some(root_cons.as_mut()))
            }
        } else {
            crate::core::cons::ElemStreamIter::new(None, None)
        };
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

/// TODO: Document
#[doc(inline)]
pub use __bail_err as bail_err;

/// TODO: Document
#[doc(inline)]
pub use __cons as cons;

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

/// TODO: Document
#[doc(inline)]
pub use __rooted_iter as rooted_iter;
