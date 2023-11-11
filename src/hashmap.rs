use fxhash::FxBuildHasher;

pub(crate) type HashMap<K, V> = std::collections::HashMap<K, V, FxBuildHasher>;
pub(crate) type HashSet<K> = std::collections::HashSet<K, FxBuildHasher>;
pub(crate) type IndexMap<K, V> = indexmap::IndexMap<K, V, FxBuildHasher>;
