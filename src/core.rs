//! The core modules that define the primitive types and functionality of the
//! language.
#[macro_use]
pub(crate) mod cons;
pub(crate) mod env;
#[macro_use]
pub(crate) mod error;
pub(crate) mod object;
#[macro_use]
pub(crate) mod gc;
