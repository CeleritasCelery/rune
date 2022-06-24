//! The core object defintions.
//!
//! Objects are implemented as rust enum with at max a 56 bit payload. This
//! means that it will always be 64 bits. 32 bit systems are not supported.
//! Because of this it gives us more flexibility in the amount of information we
//! can encode in the object header. For example, we can have 255 variants of
//! objects before we need to box the object header. We are making the
//! assumption that pointers are no bigger then 56 bits and that they are word
//! aligned. All objects should be bound to a lifetime to ensure sound operation
//! of the vm.

mod convert;
mod func;
mod gc;
pub(crate) use gc::*;

pub(crate) use convert::*;
pub(crate) use func::*;
