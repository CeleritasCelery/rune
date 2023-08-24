#![warn(
    clippy::all,
    clippy::pedantic,
    clippy::as_ptr_cast_mut,
    clippy::equatable_if_let,
    clippy::nonstandard_macro_braces,
    clippy::or_fun_call,
    clippy::manual_let_else,
    unused_qualifications,
    meta_variable_misuse,
    explicit_outlives_requirements,
    missing_copy_implementations,
    noop_method_call,
    semicolon_in_expressions_from_macros,
    trivial_numeric_casts,
    unreachable_pub,
    unused_lifetimes
)]
mod buffer;
mod metric;

pub use buffer::*;
