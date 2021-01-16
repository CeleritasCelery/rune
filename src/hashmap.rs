use std::hash::BuildHasherDefault;
use fnv::{FnvHashMap, FnvHasher};

pub type HashMap<T, U> = std::collections::HashMap<T, U, BuildHasherDefault<FnvHasher>>;

pub trait HashMapDefault {
    fn create() -> Self;
    fn with_capacity(x: usize) -> Self;
}

impl<T, U> HashMapDefault for HashMap<T, U> {
    fn create() -> Self {
        FnvHashMap::default()
    }

    fn with_capacity(cap: usize) -> Self {
        FnvHashMap::with_capacity_and_hasher(cap , Default::default())
    }
}
