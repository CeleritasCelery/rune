use rustc_hash::FxHashMap;
use rustc_hash::FxHashSet;

pub(crate) type HashMap<K, V> = FxHashMap<K, V>;
pub(crate) type HashSet<K> = FxHashSet<K>;
