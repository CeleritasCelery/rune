use std::hash::BuildHasherDefault;
use fnv::{FnvHashMap, FnvHasher};

pub type HashMap<T, U> = std::collections::HashMap<T, U, BuildHasherDefault<FnvHasher>>;

pub trait HashMapDefault {
    fn create() -> Self;
}

impl<T, U> HashMapDefault for HashMap<T, U> {
    fn create() -> Self {
        FnvHashMap::default()
    }
}
