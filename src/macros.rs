#[macro_export]
macro_rules! vec_into {
    ($($x:expr),+ $(,)?) => (vec![$($x.into()),+]);
}
