//! Advanced caching and lazy evaluation systems
//!
//! This module provides sophisticated caching mechanisms including memoization,
//! LRU caches, and lazy evaluation patterns optimized for accounting operations.

use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::sync::{Arc, Mutex, RwLock};
use std::time::{Duration, Instant};
use lru::LruCache;
use moka::sync::Cache as MokaCache;
use once_cell::sync::{Lazy, OnceCell};
use crate::data_structures::FastHashMap;
use ledger_math::amount::Amount;
use crate::balance::Balance;
use rust_decimal::Decimal;
use std::fmt;

/// Memoization cache for expensive function results
#[derive(Debug)]
pub struct MemoCache<K, V>
where
    K: Hash + Eq + Clone,
    V: Clone,
{
    cache: Mutex<FastHashMap<K, CachedValue<V>>>,
    stats: Arc<Mutex<CacheStats>>,
}

#[derive(Debug, Clone)]
struct CachedValue<V> {
    value: V,
    created_at: Instant,
    access_count: u64,
}

impl<K, V> MemoCache<K, V>
where
    K: Hash + Eq + Clone,
    V: Clone,
{
    /// Create a new memoization cache with specified capacity
    pub fn new(capacity: usize) -> Self {
        Self {
            cache: Mutex::new(FastHashMap::default()),
            stats: Arc::new(Mutex::new(CacheStats::new(capacity))),

        }
    }

    /// Get or compute a value, caching the result
    pub fn get_or_compute<F>(&self, key: K, compute: F) -> V
    where
        F: FnOnce() -> V,
    {
        // Try to get from cache first
        {
            let mut cache = self.cache.lock().unwrap();
            if let Some(cached) = cache.get_mut(&key) {
                cached.access_count += 1;
                self.stats.lock().unwrap().hit_count += 1;
                return cached.value.clone();
            }
        }

        // Cache miss - compute the value
        let value = compute();
        self.stats.lock().unwrap().miss_count += 1;

        // Store in cache (with eviction if necessary)
        {
            let mut cache = self.cache.lock().unwrap();
            
            // Simple eviction strategy - remove oldest if at capacity
            if cache.len() >= self.stats.lock().unwrap().capacity {
                let oldest_key = cache.iter()
                    .min_by_key(|(_, v)| v.created_at)
                    .map(|(k, _)| k.clone());
                
                if let Some(oldest_key) = oldest_key {
                    cache.remove(&oldest_key);
                }
            }

            cache.insert(key, CachedValue {
                value: value.clone(),
                created_at: Instant::now(),
                access_count: 1,
            });
        }

        value
    }

    /// Clear the cache
    pub fn clear(&self) {
        self.cache.lock().unwrap().clear();
        let mut stats = self.stats.lock().unwrap();
        stats.hit_count = 0;
        stats.miss_count = 0;
    }

    /// Get cache statistics
    pub fn stats(&self) -> CacheStats {
        let stats = self.stats.lock().unwrap();
        let total_requests = stats.hit_count + stats.miss_count;
        
        CacheStats {
            hit_count: stats.hit_count,
            miss_count: stats.miss_count,
            hit_ratio: if total_requests > 0 {
                stats.hit_count as f64 / total_requests as f64
            } else {
                0.0
            },
            size: self.cache.lock().unwrap().len(),
            capacity: stats.capacity,
        }
    }
}

/// LRU cache for balance computations
pub struct BalanceCache {
    cache: Arc<Mutex<LruCache<String, CachedBalance>>>,
    stats: Arc<Mutex<CacheStats>>,
}

#[derive(Debug, Clone)]
struct CachedBalance {
    balance: Balance,
    computed_at: Instant,
    dependencies: Vec<String>, // Account names this balance depends on
}

impl BalanceCache {
    /// Create a new balance cache with specified capacity
    pub fn new(capacity: usize) -> Self {
        Self {
            cache: Arc::new(Mutex::new(LruCache::new(capacity.try_into().unwrap()))),
            stats: Arc::new(Mutex::new(CacheStats::default())),
        }
    }

    /// Get a cached balance or None if not found
    pub fn get(&self, account_path: &str) -> Option<Balance> {
        let mut cache = self.cache.lock().unwrap();
        let mut stats = self.stats.lock().unwrap();

        if let Some(cached) = cache.get(account_path) {
            stats.hit_count += 1;
            Some(cached.balance.clone())
        } else {
            stats.miss_count += 1;
            None
        }
    }

    /// Store a balance in the cache
    pub fn put(&self, account_path: String, balance: Balance, dependencies: Vec<String>) {
        let mut cache = self.cache.lock().unwrap();
        
        cache.put(account_path, CachedBalance {
            balance,
            computed_at: Instant::now(),
            dependencies,
        });
    }

    /// Invalidate entries that depend on the given account
    pub fn invalidate_dependent(&self, changed_account: &str) {
        let mut cache = self.cache.lock().unwrap();
        let keys_to_remove: Vec<String> = cache.iter()
            .filter(|(_, cached)| {
                cached.dependencies.iter().any(|dep| 
                    dep == changed_account || dep.starts_with(&format!("{}:", changed_account))
                )
            })
            .map(|(k, _)| k.clone())
            .collect();

        for key in keys_to_remove {
            cache.pop(&key);
        }
    }

    /// Get cache statistics
    pub fn stats(&self) -> CacheStats {
        let stats = self.stats.lock().unwrap();
        let total = stats.hit_count + stats.miss_count;
        let cache = self.cache.lock().unwrap();
        
        CacheStats {
            hit_count: stats.hit_count,
            miss_count: stats.miss_count,
            hit_ratio: if total > 0 { stats.hit_count as f64 / total as f64 } else { 0.0 },
            size: cache.len(),
            capacity: cache.cap().get(),
        }
    }

    /// Clear the entire cache
    pub fn clear(&self) {
        self.cache.lock().unwrap().clear();
        let mut stats = self.stats.lock().unwrap();
        stats.hit_count = 0;
        stats.miss_count = 0;
    }
}

/// High-performance cache using Moka for frequently accessed data
pub struct FrequencyCache<K, V> {
    cache: MokaCache<K, V>,
}

impl<K, V> FrequencyCache<K, V>
where
    K: Hash + Eq + Send + Sync + 'static,
    V: Clone + Send + Sync + 'static,
{
    /// Create a new frequency-based cache
    pub fn new(max_capacity: u64, ttl: Duration) -> Self {
        let cache = MokaCache::builder()
            .max_capacity(max_capacity)
            .time_to_live(ttl)
            .build();

        Self { cache }
    }

    /// Get a value from the cache
    pub fn get(&self, key: &K) -> Option<V> {
        self.cache.get(key)
    }

    /// Insert a value into the cache
    pub fn insert(&self, key: K, value: V) {
        self.cache.insert(key, value);
    }

    /// Remove a value from the cache
    pub fn remove(&self, key: &K) {
        self.cache.invalidate(key);
    }

    /// Get cache statistics
    pub fn stats(&self) -> CacheStats {
        let stats = self.cache.entry_count() as usize;
        CacheStats {
            hit_count: 0, // Moka doesn't expose detailed stats by default
            miss_count: 0,
            hit_ratio: 0.0,
            size: stats,
            capacity: 0, // Capacity not easily accessible in moka
        }
    }

    /// Clear the cache
    pub fn clear(&self) {
        self.cache.invalidate_all();
    }
}

/// Lazy evaluation wrapper for expensive computations
pub struct LazyComputed<T> {
    cell: OnceCell<T>,
    compute: Box<dyn Fn() -> T + Send + Sync>,
}

impl<T> LazyComputed<T> {
    /// Create a new lazy computed value
    pub fn new<F>(compute: F) -> Self
    where
        F: Fn() -> T + Send + Sync + 'static,
    {
        Self {
            cell: OnceCell::new(),
            compute: Box::new(compute),
        }
    }

    /// Get the computed value, computing it if necessary
    pub fn get(&self) -> &T {
        self.cell.get_or_init(|| (self.compute)())
    }

    /// Check if the value has been computed
    pub fn is_computed(&self) -> bool {
        self.cell.get().is_some()
    }

    /// Force recomputation on next access
    pub fn invalidate(&mut self) {
        self.cell = OnceCell::new();
    }
}

/// Lazy account totals that compute on first access
pub struct LazyAccountTotal {
    account_path: String,
    total: LazyComputed<Balance>,
    dependencies: Vec<String>,
    last_invalidated: Instant,
}

impl LazyAccountTotal {
    /// Create a new lazy account total
    pub fn new<F>(account_path: String, compute: F) -> Self
    where
        F: Fn() -> Balance + Send + Sync + 'static,
    {
        Self {
            account_path: account_path.clone(),
            total: LazyComputed::new(compute),
            dependencies: vec![account_path],
            last_invalidated: Instant::now(),
        }
    }

    /// Get the account total, computing if necessary
    pub fn get(&self) -> &Balance {
        self.total.get()
    }

    /// Check if computation is needed
    pub fn needs_computation(&self) -> bool {
        !self.total.is_computed()
    }

    /// Invalidate if any dependencies changed
    pub fn maybe_invalidate(&mut self, changed_accounts: &[String]) -> bool {
        let should_invalidate = changed_accounts.iter().any(|account| {
            self.dependencies.iter().any(|dep| {
                account == dep || account.starts_with(&format!("{}:", dep))
            })
        });

        if should_invalidate {
            self.total.cell = OnceCell::new();
            self.last_invalidated = Instant::now();
            true
        } else {
            false
        }
    }
}

/// Cache statistics for monitoring performance
#[derive(Debug, Clone, Default)]
pub struct CacheStats {
    pub hit_count: u64,
    pub miss_count: u64,
    pub hit_ratio: f64,
    pub size: usize,
    pub capacity: usize,
}

impl CacheStats {
    /// Create a new cache stats with given capacity.
    pub fn new(capacity: usize) -> Self {
        Self {
             hit_count: 0,
             miss_count: 0,
             hit_ratio: 0.0,
             size: 0,
             capacity,
        }
    }
}

impl fmt::Display for CacheStats {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, 
            "Cache Stats: {}/{} hits ({:.1}% hit rate), size: {}/{}",
            self.hit_count,
            self.hit_count + self.miss_count,
            self.hit_ratio * 100.0,
            self.size,
            self.capacity
        )
    }
}

/// Global cache manager for coordinating different cache types
pub struct CacheManager {
    balance_cache: BalanceCache,
    expression_cache: MemoCache<String, Decimal>,
    price_cache: FrequencyCache<(String, chrono::NaiveDate), Decimal>,
}

impl CacheManager {
    /// Create a new cache manager with default sizes
    pub fn new() -> Self {
        Self {
            balance_cache: BalanceCache::new(10000),
            expression_cache: MemoCache::new(5000),
            price_cache: FrequencyCache::new(1000, Duration::from_secs(3600)),
        }
    }

    /// Create a cache manager with custom sizes
    pub fn with_capacities(
        balance_capacity: usize,
        expression_capacity: usize,
        price_capacity: u64,
    ) -> Self {
        Self {
            balance_cache: BalanceCache::new(balance_capacity),
            expression_cache: MemoCache::new(expression_capacity),
            price_cache: FrequencyCache::new(price_capacity, Duration::from_secs(3600)),
        }
    }

    /// Get the balance cache
    pub fn balance_cache(&self) -> &BalanceCache {
        &self.balance_cache
    }

    /// Get the expression cache
    pub fn expression_cache(&self) -> &MemoCache<String, Decimal> {
        &self.expression_cache
    }

    /// Get the price cache
    pub fn price_cache(&self) -> &FrequencyCache<(String, chrono::NaiveDate), Decimal> {
        &self.price_cache
    }

    /// Invalidate all caches (use after major data changes)
    pub fn invalidate_all(&self) {
        self.balance_cache.clear();
        self.expression_cache.clear();
        self.price_cache.clear();
    }

    /// Get comprehensive cache statistics
    pub fn all_stats(&self) -> CacheManagerStats {
        CacheManagerStats {
            balance_cache: self.balance_cache.stats(),
            expression_cache: self.expression_cache.stats(),
            price_cache: self.price_cache.stats(),
        }
    }
}

/// Statistics for all caches in the manager
#[derive(Debug, Clone)]
pub struct CacheManagerStats {
    pub balance_cache: CacheStats,
    pub expression_cache: CacheStats,
    pub price_cache: CacheStats,
}

impl fmt::Display for CacheManagerStats {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Cache Manager Statistics:")?;
        writeln!(f, "  Balance Cache: {}", self.balance_cache)?;
        writeln!(f, "  Expression Cache: {}", self.expression_cache)?;
        writeln!(f, "  Price Cache: {}", self.price_cache)?;
        Ok(())
    }
}

impl Default for CacheManager {
    fn default() -> Self {
        Self::new()
    }
}

/// Global cache instance using lazy initialization
pub static GLOBAL_CACHE: Lazy<CacheManager> = Lazy::new(|| CacheManager::new());

/// Convenience macros for memoization
#[macro_export]
macro_rules! memoize {
    ($cache:expr, $key:expr, $compute:expr) => {
        $cache.get_or_compute($key, || $compute)
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_memo_cache_basic() {
        let cache = MemoCache::new(100);
        let mut call_count = 0;

        // First call should compute
        let result1 = cache.get_or_compute("test", || {
            call_count += 1;
            42
        });
        assert_eq!(result1, 42);
        assert_eq!(call_count, 1);

        // Second call should use cache
        let result2 = cache.get_or_compute("test", || {
            call_count += 1;
            99 // Should not be called
        });
        assert_eq!(result2, 42);
        assert_eq!(call_count, 1);

        let stats = cache.stats();
        assert_eq!(stats.hit_count, 1);
        assert_eq!(stats.miss_count, 1);
        assert!((stats.hit_ratio - 0.5).abs() < 0.01);
    }

    #[test]
    fn test_balance_cache() {
        let cache = BalanceCache::new(100);
        let balance = Balance::new();

        // Cache miss
        assert!(cache.get("Assets:Cash").is_none());

        // Store in cache
        cache.put("Assets:Cash".to_string(), balance.clone(), vec!["Assets".to_string()]);

        // Cache hit
        assert!(cache.get("Assets:Cash").is_some());

        // Invalidation
        cache.invalidate_dependent("Assets");
        assert!(cache.get("Assets:Cash").is_none());

        let stats = cache.stats();
        assert_eq!(stats.hit_count, 1);
        assert_eq!(stats.miss_count, 2);
    }

    #[test]
    fn test_lazy_computed() {
        use std::sync::{atomic::{AtomicU32, Ordering}, Arc};
        let counter = Arc::new(AtomicU32::new(0));
        let counter_clone = counter.clone();
        let lazy = LazyComputed::new(move || {
            counter_clone.fetch_add(1, Ordering::SeqCst);
            42
        });

        assert!(!lazy.is_computed());
        assert_eq!(*lazy.get(), 42);
        assert!(lazy.is_computed());
        assert_eq!(counter.load(Ordering::SeqCst), 1);

        // Second access should not recompute
        assert_eq!(*lazy.get(), 42);
        assert_eq!(counter.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn test_lazy_account_total() {
        let mut total = LazyAccountTotal::new(
            "Assets:Cash".to_string(),
            || Balance::new()
        );

        assert!(total.needs_computation());
        
        // First access computes
        let _balance = total.get();
        assert!(!total.needs_computation());

        // Invalidation based on dependency
        let changed = vec!["Assets:Cash".to_string()];
        assert!(total.maybe_invalidate(&changed));
        assert!(total.needs_computation());

        // Non-dependent change should not invalidate
        let changed = vec!["Expenses:Food".to_string()];
        assert!(!total.maybe_invalidate(&changed));
    }

    #[test]
    fn test_cache_manager() {
        let manager = CacheManager::new();
        let stats = manager.all_stats();

        // All caches should start empty
        assert_eq!(stats.balance_cache.size, 0);
        assert_eq!(stats.expression_cache.size, 0);
        assert_eq!(stats.price_cache.size, 0);

        // Test invalidation
        manager.invalidate_all();
        
        // Should still be empty after invalidation
        let stats = manager.all_stats();
        assert_eq!(stats.balance_cache.size, 0);
    }

    #[test]
    fn test_frequency_cache() {
        let cache = FrequencyCache::new(100, Duration::from_secs(60));

        // Test basic operations
        cache.insert("key1", "value1");
        assert_eq!(cache.get(&"key1"), Some("value1"));
        
        cache.remove(&"key1");
        assert_eq!(cache.get(&"key1"), None);
        
        // Test clear
        cache.insert("key2", "value2");
        cache.clear();
        assert_eq!(cache.get(&"key2"), None);
    }
}