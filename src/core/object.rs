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

mod buffer;
mod cell;
pub(in crate::core) mod channel;
mod chartab;
mod convert;
mod float;
mod func;
mod hashtable;
mod integer;
mod string;
mod symbol;
mod tagged;
mod vector;

pub(crate) use buffer::*;
pub(super) use cell::*;
pub(crate) use channel::*;
pub(crate) use chartab::*;
pub(crate) use convert::*;
pub(crate) use float::*;
pub(crate) use func::*;
pub(crate) use hashtable::*;
pub(crate) use integer::*;
pub(crate) use string::*;
pub(crate) use symbol::*;
pub(crate) use tagged::*;
pub(crate) use vector::*;

use std::fmt::Write as _;

pub(crate) fn display_slice<T: std::fmt::Display>(slice: &[T]) -> String {
    let mut buffer = String::new();
    buffer.push('[');
    for x in slice {
        write!(&mut buffer, "{x} ").expect("failed to display slice");
    }
    buffer.push(']');
    buffer
}
