use super::super::object::{GcObj, WithLifetime};
use super::RootSet;

#[doc(hidden)]
pub(crate) struct StackRoot<'rt> {
    root_set: &'rt RootSet,
}

impl<'rt> StackRoot<'rt> {
    /// Create a new `StackRoot`
    ///
    /// # Safety
    ///
    /// This function is only safe if [set] is called before use and `StackRoot`
    /// does not move from the stack or drop before it goes out of scope. This
    /// is ensured by a shadow binding in the [root!] macro.
    pub(crate) unsafe fn new(roots: &'rt RootSet) -> Self {
        StackRoot { root_set: roots }
    }

    pub(crate) fn set<'root, 'ob, T, U>(&'root mut self, obj: T) -> U
    where
        T: 'ob + Copy + Into<GcObj<'ob>> + WithLifetime<'root, Out = U>,
    {
        unsafe {
            let root: GcObj = obj.into();
            self.root_set.roots.borrow_mut().push(root.with_lifetime());
            obj.with_lifetime()
        }
    }
}

impl<'rt> Drop for StackRoot<'rt> {
    // Remove the object bound by this StackRoot from the root set. We know that
    // the top of the stack will correspond to the correct object is the
    // invariants of StackRoot are upheld.
    fn drop(&mut self) {
        self.root_set.roots.borrow_mut().pop();
    }
}

/// Roots an [Object] to the stack. The object will be valid until the end of
/// the current scope. The object's lifetime will no longer be bound to the
/// [Arena].
///
/// # Examples
///
/// ```
/// let object = Object::from(5);
/// root!(object, gc);
/// ```
#[macro_export]
macro_rules! root {
    ($obj:ident, $arena:ident) => {
        let mut root = unsafe { $crate::core::arena::StackRoot::new($arena.get_root_set()) };
        let $obj = root.set($obj);
    };
}
