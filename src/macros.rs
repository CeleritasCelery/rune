#[macro_export]
macro_rules! vec_into {
    ($($x:expr),+ $(,)?) => (vec![$($x.into()),+]);
}

#[macro_export]
macro_rules! cons {
    ($car:expr, $cdr:expr) => {
        Cons::new($car.into(), $cdr.into());
    }
}
