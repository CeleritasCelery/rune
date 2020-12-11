#[macro_export]
macro_rules! vec_into {
    ($($x:expr),+ $(,)?) => {vec![$($x.into()),+]};
}

macro_rules! count {
    (@replace_expr $_t:tt $sub:expr) => {$sub};
    ($($x:expr)+) => {0usize $(+ count!(@replace_expr $x 1usize))*};
}

#[macro_export]
macro_rules! defsubr {
    ($($x:ident),+ $(,)?) => (pub const fn defsubr() -> [BuiltInFn; count!($($x)+)] {
        [$($x),+]
    });
}
