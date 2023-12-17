//! Hashmap types used in the crate.
use fxhash::FxBuildHasher;

pub type HashMap<K, V> = std::collections::HashMap<K, V, FxBuildHasher>;
pub type HashSet<K> = std::collections::HashSet<K, FxBuildHasher>;
pub type IndexMap<K, V> = indexmap::IndexMap<K, V, FxBuildHasher>;
