#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub enum Field<T> {
    #[default]
    Unspecified,
    Explicit(Option<T>),
}

impl<T> Field<T> {
    /// Returns `true` if the field is `Unspecified`.
    pub fn is_unspecified(&self) -> bool {
        matches!(self, Self::Unspecified)
    }

    /// Returns `true` if the field is `Explicit`.
    pub fn is_explicit(&self) -> bool {
        matches!(self, Self::Explicit(_))
    }

    /// Returns `true` if the field is `Explicit(Some(..))`.
    pub fn is_some(&self) -> bool {
        matches!(self, Self::Explicit(Some(_)))
    }

    /// Returns `true` if the field is `Explicit(None)`.
    pub fn is_none(&self) -> bool {
        matches!(self, Self::Explicit(None))
    }

    /// Converts from `&Field<T>` to `Option<&T>`.
    ///
    /// Returns `Some(&T)` if the field is `Explicit(Some(T))`,
    /// otherwise returns `None`.
    pub fn as_ref(&self) -> Option<&T> {
        match self {
            Self::Explicit(Some(value)) => Some(value),
            _ => None,
        }
    }

    /// Converts from `&mut Field<T>` to `Option<&mut T>`.
    ///
    /// Returns `Some(&mut T)` if the field is `Explicit(Some(T))`,
    /// otherwise returns `None`.
    pub fn as_mut(&mut self) -> Option<&mut T> {
        match self {
            Self::Explicit(Some(value)) => Some(value),
            _ => None,
        }
    }

    /// Returns the contained `Some` value, consuming the `self` value.
    ///
    /// # Panics
    ///
    /// Panics if the value is not `Explicit(Some(..))`.
    pub fn unwrap(self) -> T {
        match self {
            Self::Explicit(Some(value)) => value,
            _ => panic!("called `Field::unwrap()` on a value that is not `Explicit(Some(..))`"),
        }
    }

    /// Returns the contained `Some` value or a provided default.
    pub fn unwrap_or(self, default: T) -> T {
        match self {
            Self::Explicit(Some(value)) => value,
            _ => default,
        }
    }

    /// Returns the contained `Some` value or computes it from a closure.
    pub fn unwrap_or_else<F: FnOnce() -> T>(self, f: F) -> T {
        match self {
            Self::Explicit(Some(value)) => value,
            _ => f(),
        }
    }

    /// Merges another `Field` with `self`.
    ///
    /// If `self` is `Explicit`, keep its value.
    /// Otherwise, `other`'s value is taken.
    pub fn merge(self, other: Self) -> Self {
        if self.is_explicit() { self } else { other }
    }


    pub fn and_then<U>(self, f: impl FnOnce(T) -> Field<U>) -> Field<U> {
        match self {
            Field::Unspecified | Field::Explicit(None) => Field::Unspecified,
            Field::Explicit(Some(val)) => f(val),
        }
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Field<U> {
        match self {
            Field::Unspecified | Field::Explicit(None) => Field::Unspecified,
            Field::Explicit(Some(val)) => Field::Explicit(Some(f(val))),
        }
    }
}

// You can also implement From conversions for convenience.
impl<T> From<Option<T>> for Field<T> {
    fn from(opt: Option<T>) -> Self {
        Field::Explicit(opt)
    }
}

impl<T> From<T> for Field<T> {
    fn from(value: T) -> Self {
        Field::Explicit(Some(value))
    }
}
