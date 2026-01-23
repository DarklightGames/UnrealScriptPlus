use dashmap::DashMap;
use lru::LruCache;
use std::hash::Hash;
use std::num::NonZeroUsize;
use std::sync::Mutex;
use tracing::debug;

/// A thread-safe LRU cache that combines DashMap for concurrent access
/// with LRU eviction when the cache exceeds its capacity.
///
/// This cache maintains both a DashMap for fast concurrent lookups and
/// an LRU tracker to manage eviction. When the cache is full, the least
/// recently used entry is evicted to make room for new entries.
pub struct LruDashMap<K, V>
where
    K: Eq + Hash + Clone,
{
    /// The actual data storage with concurrent access
    map: DashMap<K, V>,
    /// LRU tracker for eviction policy (protected by mutex)
    lru_tracker: Mutex<LruCache<K, ()>>,
}

impl<K, V> LruDashMap<K, V>
where
    K: Eq + Hash + Clone,
{
    /// Create a new LRU cache with the specified capacity.
    ///
    /// # Arguments
    /// * `capacity` - Maximum number of entries before eviction occurs
    pub fn new(capacity: usize) -> Self {
        Self {
            map: DashMap::new(),
            lru_tracker: Mutex::new(LruCache::new(NonZeroUsize::new(capacity).unwrap())),
        }
    }

    /// Insert a key-value pair into the cache.
    ///
    /// If the cache is at capacity, this will evict the least recently used entry.
    pub fn insert(&self, key: K, value: V) -> Option<V>
    where
        K: std::fmt::Display,
    {
        // Update LRU tracker and handle eviction if needed
        let mut lru = self.lru_tracker.lock().unwrap();
        
        // Check if we need to evict an entry
        if lru.len() >= lru.cap().get() && !lru.contains(&key) {
            if let Some((evicted_key, _)) = lru.pop_lru() {
                debug!("Evicting LRU document from cache: {}", evicted_key);
                self.map.remove(&evicted_key);
            }
        }
        
        // Mark this key as recently used
        lru.put(key.clone(), ());
        drop(lru);

        // Insert into the map
        self.map.insert(key, value)
    }

    /// Get a reference to a value in the cache.
    ///
    /// This marks the key as recently used.
    pub fn get(&self, key: &K) -> Option<dashmap::mapref::one::Ref<'_, K, V>>
    {
        let result = self.map.get(key);
        
        if result.is_some() {
            // Mark as recently used
            let mut lru = self.lru_tracker.lock().unwrap();
            lru.get(key); // Moves to front of LRU
        }
        
        result
    }

    /// Get a mutable reference to a value in the cache.
    ///
    /// This marks the key as recently used.
    pub fn get_mut(&self, key: &K) -> Option<dashmap::mapref::one::RefMut<'_, K, V>>
    {
        let result = self.map.get_mut(key);
        
        if result.is_some() {
            // Mark as recently used
            let mut lru = self.lru_tracker.lock().unwrap();
            lru.get(key); // Moves to front of LRU
        }
        
        result
    }

    /// Remove a key from the cache.
    pub fn remove(&self, key: &K) -> Option<(K, V)>
    {
        // Remove from LRU tracker
        let mut lru = self.lru_tracker.lock().unwrap();
        lru.pop(key);
        drop(lru);

        // Remove from map
        self.map.remove(key)
    }

    /// Get the number of entries in the cache.
    pub fn len(&self) -> usize {
        self.map.len()
    }

    /// Check if the cache is empty.
    pub fn is_empty(&self) -> bool {
        self.map.is_empty()
    }

    /// Get the capacity of the cache.
    pub fn capacity(&self) -> usize {
        self.lru_tracker.lock().unwrap().cap().get()
    }
}
