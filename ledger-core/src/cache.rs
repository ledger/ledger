//! Report caching system for optimizing large journal performance
//!
//! This module provides caching infrastructure to speed up report generation
//! on large datasets by caching intermediate calculations and final results.

use crate::balance::Balance;
use crate::report::ReportOptions;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, VecDeque};
use std::fmt;
use std::hash::{Hash, Hasher};
use std::time::{Duration, SystemTime, UNIX_EPOCH};

/// Errors that can occur during cache operations
#[derive(Debug, Clone)]
pub enum CacheError {
    /// Cache miss - requested item not found
    Miss(String),
    /// Cache is full and cannot accept new entries
    Full,
    /// Serialization error
    SerializationError(String),
    /// IO error during persistent cache operations
    IoError(String),
    /// Invalid cache key
    InvalidKey(String),
}

impl fmt::Display for CacheError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CacheError::Miss(key) => write!(f, "Cache miss for key: {}", key),
            CacheError::Full => write!(f, "Cache is full"),
            CacheError::SerializationError(msg) => write!(f, "Serialization error: {}", msg),
            CacheError::IoError(msg) => write!(f, "IO error: {}", msg),
            CacheError::InvalidKey(key) => write!(f, "Invalid cache key: {}", key),
        }
    }
}

impl std::error::Error for CacheError {}

/// Result type for cache operations
pub type CacheResult<T> = Result<T, CacheError>;

/// Cache key for identifying cached items
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct CacheKey {
    /// Key type identifier
    pub key_type: String,
    /// Parameters that affect the cached result
    pub params: Vec<(String, String)>,
    /// Journal hash (to detect journal changes)
    pub journal_hash: u64,
}

impl CacheKey {
    /// Create a new cache key
    pub fn new(key_type: String, journal_hash: u64) -> Self {
        Self { key_type, params: Vec::new(), journal_hash }
    }

    /// Add a parameter to the cache key
    pub fn with_param<K: ToString, V: ToString>(mut self, key: K, value: V) -> Self {
        self.params.push((key.to_string(), value.to_string()));
        self.params.sort_by(|a, b| a.0.cmp(&b.0)); // Keep parameters sorted for consistency
        self
    }

    /// Create a cache key from report options
    pub fn from_report_options(
        report_type: &str,
        options: &ReportOptions,
        journal_hash: u64,
    ) -> Self {
        let mut key = CacheKey::new(report_type.to_string(), journal_hash);

        key = key.with_param("flat", options.flat);
        if let Some(depth) = options.depth {
            key = key.with_param("depth", depth);
        }
        key = key.with_param("empty", options.empty);
        key = key.with_param("sort_by", format!("{:?}", options.sort_by));
        key = key.with_param("collapse", options.collapse);
        key = key.with_param("running_total", options.running_total);
        if let Some(account_width) = options.account_width {
            key = key.with_param("account_width", account_width);
        }
        if let Some(amount_width) = options.amount_width {
            key = key.with_param("amount_width", amount_width);
        }
        key = key.with_param("show_codes", options.show_codes);
        if let Some(ref currency) = options.currency {
            key = key.with_param("currency", currency);
        }

        key
    }
}

impl fmt::Display for CacheKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = {
            let mut parts = vec![format!("{}:{}", self.key_type, self.journal_hash)];
            for (k, v) in &self.params {
                parts.push(format!("{}={}", k, v));
            }
            parts.join("|")
        };
        write!(f, "{}", s)
    }
}

/// Cache entry with metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CacheEntry<T> {
    /// The cached value
    pub value: T,
    /// When this entry was created
    pub created_at: SystemTime,
    /// When this entry was last accessed
    pub last_accessed: SystemTime,
    /// Number of times this entry has been accessed
    pub access_count: u64,
    /// Size estimate of this entry in bytes
    pub size_estimate: usize,
}

impl<T> CacheEntry<T> {
    /// Create a new cache entry
    pub fn new(value: T, size_estimate: usize) -> Self {
        let now = SystemTime::now();
        Self { value, created_at: now, last_accessed: now, access_count: 0, size_estimate }
    }

    /// Update access metadata
    pub fn access(&mut self) {
        self.last_accessed = SystemTime::now();
        self.access_count += 1;
    }

    /// Check if entry is expired
    pub fn is_expired(&self, max_age: Duration) -> bool {
        if let Ok(age) = self.created_at.elapsed() {
            age > max_age
        } else {
            true // If we can't determine age, consider it expired
        }
    }

    /// Get access frequency (accesses per second since creation)
    pub fn access_frequency(&self) -> f64 {
        if let Ok(age) = self.created_at.elapsed() {
            let seconds = age.as_secs_f64();
            if seconds > 0.0 {
                self.access_count as f64 / seconds
            } else {
                0.0
            }
        } else {
            0.0
        }
    }
}

/// LRU cache with memory limits and TTL support
pub struct LruCache<T> {
    /// Cache storage
    cache: HashMap<CacheKey, CacheEntry<T>>,
    /// Access order for LRU eviction
    access_order: VecDeque<CacheKey>,
    /// Maximum number of entries
    max_entries: usize,
    /// Maximum memory usage in bytes
    max_memory_bytes: usize,
    /// Current memory usage estimate
    current_memory_bytes: usize,
    /// Maximum age for entries
    max_age: Duration,
    /// Cache statistics
    stats: CacheStats,
}

/// Cache statistics for monitoring performance
#[derive(Debug, Clone, Default)]
pub struct CacheStats {
    /// Total number of cache hits
    pub hits: u64,
    /// Total number of cache misses
    pub misses: u64,
    /// Total number of evictions
    pub evictions: u64,
    /// Total number of insertions
    pub insertions: u64,
    /// Total memory usage
    pub memory_usage: usize,
    /// Number of entries
    pub entry_count: usize,
}

impl CacheStats {
    /// Calculate hit rate as a percentage
    pub fn hit_rate(&self) -> f64 {
        let total = self.hits + self.misses;
        if total > 0 {
            (self.hits as f64 / total as f64) * 100.0
        } else {
            0.0
        }
    }

    /// Calculate average memory per entry
    pub fn avg_memory_per_entry(&self) -> f64 {
        if self.entry_count > 0 {
            self.memory_usage as f64 / self.entry_count as f64
        } else {
            0.0
        }
    }
}

impl<T: Clone + Serialize + for<'de> Deserialize<'de>> LruCache<T> {
    /// Create a new LRU cache
    pub fn new(max_entries: usize, max_memory_bytes: usize) -> Self {
        Self {
            cache: HashMap::new(),
            access_order: VecDeque::new(),
            max_entries,
            max_memory_bytes,
            current_memory_bytes: 0,
            max_age: Duration::from_secs(3600), // 1 hour default TTL
            stats: CacheStats::default(),
        }
    }

    /// Set maximum age for cache entries
    pub fn with_max_age(mut self, max_age: Duration) -> Self {
        self.max_age = max_age;
        self
    }

    /// Get an item from the cache
    pub fn get(&mut self, key: &CacheKey) -> CacheResult<T> {
        // Clean up expired entries
        self.cleanup_expired();

        if let Some(entry) = self.cache.get_mut(key) {
            entry.access();

            // Move to end of access order (most recently used)
            if let Some(pos) = self.access_order.iter().position(|k| k == key) {
                self.access_order.remove(pos);
            }
            self.access_order.push_back(key.clone());

            self.stats.hits += 1;
            Ok(entry.value.clone())
        } else {
            self.stats.misses += 1;
            Err(CacheError::Miss(key.to_string()))
        }
    }

    /// Insert an item into the cache
    pub fn insert(&mut self, key: CacheKey, value: T, size_estimate: usize) -> CacheResult<()> {
        // Clean up expired entries first
        self.cleanup_expired();

        // Check if we need to evict entries
        while self.should_evict(size_estimate) {
            self.evict_lru()?;
        }

        // Remove existing entry if present
        if self.cache.contains_key(&key) {
            self.remove(&key);
        }

        // Insert new entry
        let entry = CacheEntry::new(value, size_estimate);
        self.current_memory_bytes += size_estimate;
        self.cache.insert(key.clone(), entry);
        self.access_order.push_back(key);

        self.stats.insertions += 1;
        self.update_stats();

        Ok(())
    }

    /// Remove an item from the cache
    pub fn remove(&mut self, key: &CacheKey) -> Option<T> {
        if let Some(entry) = self.cache.remove(key) {
            self.current_memory_bytes =
                self.current_memory_bytes.saturating_sub(entry.size_estimate);

            // Remove from access order
            if let Some(pos) = self.access_order.iter().position(|k| k == key) {
                self.access_order.remove(pos);
            }

            self.update_stats();
            Some(entry.value)
        } else {
            None
        }
    }

    /// Clear all entries from the cache
    pub fn clear(&mut self) {
        self.cache.clear();
        self.access_order.clear();
        self.current_memory_bytes = 0;
        self.stats = CacheStats::default();
    }

    /// Get cache statistics
    pub fn stats(&self) -> &CacheStats {
        &self.stats
    }

    /// Check if the cache contains a key
    pub fn contains_key(&self, key: &CacheKey) -> bool {
        self.cache.contains_key(key)
    }

    /// Get the number of entries in the cache
    pub fn len(&self) -> usize {
        self.cache.len()
    }

    /// Check if the cache is empty
    pub fn is_empty(&self) -> bool {
        self.cache.is_empty()
    }

    /// Check if we should evict entries to make room
    fn should_evict(&self, new_size: usize) -> bool {
        self.cache.len() >= self.max_entries
            || self.current_memory_bytes + new_size > self.max_memory_bytes
    }

    /// Evict the least recently used entry
    fn evict_lru(&mut self) -> CacheResult<()> {
        if let Some(key) = self.access_order.pop_front() {
            self.remove(&key);
            self.stats.evictions += 1;
            Ok(())
        } else {
            Err(CacheError::Full)
        }
    }

    /// Clean up expired entries
    fn cleanup_expired(&mut self) {
        let mut expired_keys = Vec::new();

        for (key, entry) in &self.cache {
            if entry.is_expired(self.max_age) {
                expired_keys.push(key.clone());
            }
        }

        for key in expired_keys {
            self.remove(&key);
        }
    }

    /// Update cache statistics
    fn update_stats(&mut self) {
        self.stats.memory_usage = self.current_memory_bytes;
        self.stats.entry_count = self.cache.len();
    }
}

/// Specialized cache for report results
pub type ReportCache = LruCache<Vec<u8>>; // Serialized report data

/// Cache manager for different types of cached data
pub struct CacheManager {
    /// Balance caches for different account sets
    balance_cache: LruCache<HashMap<String, Balance>>,
    /// Report result caches
    report_cache: ReportCache,
    /// Running total caches
    running_total_cache: LruCache<Balance>,
    /// Journal hash for cache invalidation
    journal_hash: u64,
}

impl CacheManager {
    /// Create a new cache manager
    pub fn new() -> Self {
        Self {
            balance_cache: LruCache::new(100, 50 * 1024 * 1024), // 50MB limit
            report_cache: LruCache::new(50, 100 * 1024 * 1024),  // 100MB limit
            running_total_cache: LruCache::new(1000, 10 * 1024 * 1024), // 10MB limit
            journal_hash: 0,
        }
    }

    /// Update journal hash (invalidates all caches if changed)
    pub fn update_journal_hash(&mut self, new_hash: u64) {
        if self.journal_hash != new_hash {
            self.invalidate_all();
            self.journal_hash = new_hash;
        }
    }

    /// Get cached balance data
    pub fn get_balance_data(&mut self, key: &CacheKey) -> CacheResult<HashMap<String, Balance>> {
        self.balance_cache.get(key)
    }

    /// Cache balance data
    pub fn cache_balance_data(
        &mut self,
        key: CacheKey,
        data: HashMap<String, Balance>,
    ) -> CacheResult<()> {
        // Estimate size based on number of accounts and average balance size
        let size_estimate = data.len() * 100; // Rough estimate
        self.balance_cache.insert(key, data, size_estimate)
    }

    /// Get cached report result
    pub fn get_report_result(&mut self, key: &CacheKey) -> CacheResult<Vec<u8>> {
        self.report_cache.get(key)
    }

    /// Cache report result
    pub fn cache_report_result(&mut self, key: CacheKey, result: Vec<u8>) -> CacheResult<()> {
        let size_estimate = result.len();
        self.report_cache.insert(key, result, size_estimate)
    }

    /// Get cached running total
    pub fn get_running_total(&mut self, key: &CacheKey) -> CacheResult<Balance> {
        self.running_total_cache.get(key)
    }

    /// Cache running total
    pub fn cache_running_total(&mut self, key: CacheKey, total: Balance) -> CacheResult<()> {
        let size_estimate = 64; // Rough estimate for Balance size
        self.running_total_cache.insert(key, total, size_estimate)
    }

    /// Invalidate all caches
    pub fn invalidate_all(&mut self) {
        self.balance_cache.clear();
        self.report_cache.clear();
        self.running_total_cache.clear();
    }

    /// Get comprehensive cache statistics
    pub fn stats(&self) -> CacheManagerStats {
        CacheManagerStats {
            balance_cache: self.balance_cache.stats().clone(),
            report_cache: self.report_cache.stats().clone(),
            running_total_cache: self.running_total_cache.stats().clone(),
        }
    }

    /// Compute a simple hash for journal state
    pub fn compute_journal_hash(transaction_count: usize, total_postings: usize) -> u64 {
        use std::collections::hash_map::DefaultHasher;

        let mut hasher = DefaultHasher::new();
        transaction_count.hash(&mut hasher);
        total_postings.hash(&mut hasher);
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs()
            .hash(&mut hasher);
        hasher.finish()
    }
}

impl Default for CacheManager {
    fn default() -> Self {
        Self::new()
    }
}

/// Combined statistics for all caches
#[derive(Debug, Clone)]
pub struct CacheManagerStats {
    /// Balance cache statistics
    pub balance_cache: CacheStats,
    /// Report cache statistics
    pub report_cache: CacheStats,
    /// Running total cache statistics
    pub running_total_cache: CacheStats,
}

impl CacheManagerStats {
    /// Calculate total memory usage across all caches
    pub fn total_memory_usage(&self) -> usize {
        self.balance_cache.memory_usage
            + self.report_cache.memory_usage
            + self.running_total_cache.memory_usage
    }

    /// Calculate overall hit rate
    pub fn overall_hit_rate(&self) -> f64 {
        let total_hits =
            self.balance_cache.hits + self.report_cache.hits + self.running_total_cache.hits;
        let total_misses =
            self.balance_cache.misses + self.report_cache.misses + self.running_total_cache.misses;
        let total_requests = total_hits + total_misses;

        if total_requests > 0 {
            (total_hits as f64 / total_requests as f64) * 100.0
        } else {
            0.0
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::report::SortCriteria;

    use super::*;
    use std::thread;
    use std::time::Duration;

    #[test]
    fn test_cache_key_creation() {
        let key = CacheKey::new("balance".to_string(), 12345)
            .with_param("flat", true)
            .with_param("depth", 5);

        assert_eq!(key.key_type, "balance");
        assert_eq!(key.journal_hash, 12345);
        assert_eq!(key.params.len(), 2);
    }

    #[test]
    fn test_cache_key_from_report_options() {
        let mut options = ReportOptions::default();
        options.flat = true;
        options.depth = Some(3);
        options.sort_by = SortCriteria::Amount;

        let key = CacheKey::from_report_options("balance", &options, 67890);

        assert_eq!(key.key_type, "balance");
        assert_eq!(key.journal_hash, 67890);
        assert!(key.params.iter().any(|(k, v)| k == "flat" && v == "true"));
        assert!(key.params.iter().any(|(k, v)| k == "depth" && v == "3"));
    }

    #[test]
    fn test_lru_cache_basic_operations() {
        let mut cache = LruCache::new(2, 1024);
        let key1 = CacheKey::new("test1".to_string(), 1);
        let key2 = CacheKey::new("test2".to_string(), 1);
        let key3 = CacheKey::new("test3".to_string(), 1);

        // Insert two items
        cache.insert(key1.clone(), "value1".to_string(), 10).unwrap();
        cache.insert(key2.clone(), "value2".to_string(), 10).unwrap();

        assert_eq!(cache.len(), 2);
        assert_eq!(cache.get(&key1).unwrap(), "value1");
        assert_eq!(cache.get(&key2).unwrap(), "value2");

        // Insert third item, should evict first
        cache.insert(key3.clone(), "value3".to_string(), 10).unwrap();
        assert_eq!(cache.len(), 2);
        assert!(cache.get(&key1).is_err()); // Should be evicted
        assert_eq!(cache.get(&key3).unwrap(), "value3");
    }

    #[test]
    fn test_cache_memory_limit() {
        let mut cache = LruCache::new(10, 50); // Small memory limit
        let key1 = CacheKey::new("test1".to_string(), 1);
        let key2 = CacheKey::new("test2".to_string(), 1);

        // Insert items that exceed memory limit
        cache.insert(key1.clone(), "value1".to_string(), 30).unwrap();
        cache.insert(key2.clone(), "value2".to_string(), 30).unwrap(); // Should evict key1

        assert!(cache.get(&key1).is_err()); // Should be evicted due to memory
        assert_eq!(cache.get(&key2).unwrap(), "value2");
    }

    #[test]
    fn test_cache_stats() {
        let mut cache = LruCache::new(5, 1024);
        let key = CacheKey::new("test".to_string(), 1);

        cache.insert(key.clone(), "value".to_string(), 10).unwrap();

        // Hit
        let _ = cache.get(&key);
        assert_eq!(cache.stats().hits, 1);
        assert_eq!(cache.stats().misses, 0);

        // Miss
        let other_key = CacheKey::new("other".to_string(), 1);
        let _ = cache.get(&other_key);
        assert_eq!(cache.stats().hits, 1);
        assert_eq!(cache.stats().misses, 1);
        assert_eq!(cache.stats().hit_rate(), 50.0);
    }

    #[test]
    fn test_cache_manager() {
        let mut manager = CacheManager::new();
        manager.update_journal_hash(123);

        let key = CacheKey::new("balance".to_string(), 123);
        let mut balance_data = HashMap::new();
        balance_data.insert("Assets:Cash".to_string(), Balance::new());

        // Cache and retrieve balance data
        manager.cache_balance_data(key.clone(), balance_data.clone()).unwrap();
        let retrieved = manager.get_balance_data(&key).unwrap();
        assert!(retrieved.contains_key("Assets:Cash"));

        // Update journal hash should invalidate cache
        manager.update_journal_hash(456);
        assert!(manager.get_balance_data(&key).is_err());
    }

    #[test]
    fn test_cache_entry_expiration() {
        let mut cache = LruCache::new(5, 1024).with_max_age(Duration::from_millis(50));
        let key = CacheKey::new("test".to_string(), 1);

        cache.insert(key.clone(), "value".to_string(), 10).unwrap();
        assert_eq!(cache.get(&key).unwrap(), "value");

        // Wait for expiration
        thread::sleep(Duration::from_millis(100));

        // Should be expired and removed
        assert!(cache.get(&key).is_err());
    }
}
