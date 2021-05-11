#![warn(rust_2018_idioms)]
#![allow(elided_lifetimes_in_paths)]
#![warn(
    trivial_casts,
    trivial_numeric_casts,
    clippy::trivially_copy_pass_by_ref,
    clippy::explicit_iter_loop,
    clippy::inefficient_to_string,
    clippy::missing_const_for_fn,
    clippy::needless_borrow,
    clippy::unicode_not_nfc
)]
#![deny(
    macro_use_extern_crate,
    keyword_idents,
    absolute_paths_not_starting_with_crate
)]
#![forbid(non_ascii_idents)]

// potential useful lints with too many false positives
// #![deny(unused_qualifications)]
// #![deny(meta_variable_misuse)]

#[macro_use]
mod macros;
#[macro_use]
mod object;
mod arena;
mod arith;
mod compile;
mod error;
mod eval;
mod forms;
mod hashmap;
mod intern;
mod opcode;
mod reader;

#[allow(clippy::missing_const_for_fn)]
fn main() {
    eval::run();
}
