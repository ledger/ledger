//! Optimized data structures for Ledger operations
//!
//! This module provides specialized collections optimized for Ledger's
//! specific access patterns, including sparse vectors for account trees
//! and custom allocators for high-frequency allocations.

use std::collections::HashMap;
use std::hash::BuildHasherDefault;
use fnv::FnvHasher;
use indexmap::IndexMap;

/// Fast HashMap using FNV hasher for small keys
pub type FastHashMap<K, V> = HashMap<K, V, BuildHasherDefault<FnvHasher>>;

/// Order-preserving HashMap for insertion-order sensitive operations
pub type OrderedMap<K, V> = IndexMap<K, V>;

/// Sparse vector implementation optimized for account hierarchies
#[derive(Debug, Clone)]
pub struct SparseVec<T> {
    data: FastHashMap<usize, T>,
    len: usize,
}

impl<T> Default for SparseVec<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> SparseVec<T> {
    /// Create a new sparse vector
    pub fn new() -> Self {
        Self {
            data: FastHashMap::default(),
            len: 0,
        }
    }

    /// Create a sparse vector with a given capacity
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            data: FastHashMap::with_capacity_and_hasher(capacity, BuildHasherDefault::default()),
            len: 0,
        }
    }

    /// Insert an element at the given index
    pub fn insert(&mut self, index: usize, value: T) {
        if index >= self.len {
            self.len = index + 1;
        }
        self.data.insert(index, value);
    }

    /// Get an element at the given index
    pub fn get(&self, index: usize) -> Option<&T> {
        self.data.get(&index)
    }

    /// Get a mutable reference to an element at the given index
    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        self.data.get_mut(&index)
    }

    /// Remove an element at the given index
    pub fn remove(&mut self, index: usize) -> Option<T> {
        self.data.remove(&index)
    }

    /// Get the length (highest index + 1)
    pub fn len(&self) -> usize {
        self.len
    }

    /// Check if the vector is empty
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    /// Get the number of non-empty elements
    pub fn occupied_count(&self) -> usize {
        self.data.len()
    }

    /// Iterate over occupied indices and their values
    pub fn iter(&self) -> impl Iterator<Item = (usize, &T)> {
        self.data.iter().map(|(&k, v)| (k, v))
    }

    /// Iterate mutably over occupied indices and their values
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (usize, &mut T)> {
        self.data.iter_mut().map(|(&k, v)| (k, v))
    }

    /// Clear all elements
    pub fn clear(&mut self) {
        self.data.clear();
        self.len = 0;
    }

    /// Shrink the capacity to fit the current data
    pub fn shrink_to_fit(&mut self) {
        self.data.shrink_to_fit();
    }

    /// Get memory usage statistics
    pub fn memory_usage(&self) -> SparseVecStats {
        SparseVecStats {
            capacity: self.data.capacity(),
            occupied: self.data.len(),
            total_len: self.len,
            sparsity_ratio: if self.len > 0 {
                (self.len - self.data.len()) as f64 / self.len as f64
            } else {
                0.0
            },
        }
    }
}

/// Statistics for sparse vector memory usage
#[derive(Debug, Clone)]
pub struct SparseVecStats {
    /// Hash table capacity
    pub capacity: usize,
    /// Number of occupied slots
    pub occupied: usize,
    /// Total logical length
    pub total_len: usize,
    /// Ratio of empty slots (0.0 = dense, 1.0 = completely sparse)
    pub sparsity_ratio: f64,
}

/// Custom arena allocator for batch allocations
pub struct Arena<T> {
    chunks: Vec<Vec<T>>,
    current_chunk: usize,
    current_pos: usize,
    chunk_size: usize,
}

impl<T> Arena<T> {
    /// Create a new arena with default chunk size
    pub fn new() -> Self {
        Self::with_chunk_size(1024)
    }

    /// Create a new arena with specified chunk size
    pub fn with_chunk_size(chunk_size: usize) -> Self {
        Self {
            chunks: Vec::new(),
            current_chunk: 0,
            current_pos: 0,
            chunk_size,
        }
    }

    /// Allocate space for a value in the arena
    pub fn alloc(&mut self, value: T) -> &mut T {
        if self.chunks.is_empty() || self.current_pos >= self.chunk_size {
            self.chunks.push(Vec::with_capacity(self.chunk_size));
            if !self.chunks.is_empty() {
                self.current_chunk = self.chunks.len() - 1;
            }
            self.current_pos = 0;
        }

        let chunk = &mut self.chunks[self.current_chunk];
        chunk.push(value);
        self.current_pos += 1;

        // Safe because we just pushed the value
        chunk.last_mut().unwrap()
    }

    /// Clear all allocations
    pub fn clear(&mut self) {
        for chunk in &mut self.chunks {
            chunk.clear();
        }
        self.current_chunk = 0;
        self.current_pos = 0;
    }

    /// Get memory usage statistics
    pub fn memory_usage(&self) -> ArenaStats {
        let total_capacity: usize = self.chunks.iter().map(|c| c.capacity()).sum();
        let total_used: usize = self.chunks.iter().map(|c| c.len()).sum();

        ArenaStats {
            chunks: self.chunks.len(),
            total_capacity,
            total_used,
            fragmentation_ratio: if total_capacity > 0 {
                (total_capacity - total_used) as f64 / total_capacity as f64
            } else {
                0.0
            },
        }
    }
}

impl<T> Default for Arena<T> {
    fn default() -> Self {
        Self::new()
    }
}

/// Statistics for arena allocator
#[derive(Debug, Clone)]
pub struct ArenaStats {
    /// Number of chunks allocated
    pub chunks: usize,
    /// Total capacity across all chunks
    pub total_capacity: usize,
    /// Total used space across all chunks
    pub total_used: usize,
    /// Ratio of unused space (0.0 = fully used, 1.0 = completely unused)
    pub fragmentation_ratio: f64,
}

/// Object pool for frequent allocations/deallocations
pub struct ObjectPool<T> {
    objects: Vec<T>,
    factory: Box<dyn Fn() -> T>,
}

impl<T> ObjectPool<T> {
    /// Create a new object pool with a factory function
    pub fn new<F>(factory: F) -> Self
    where
        F: Fn() -> T + 'static,
    {
        Self {
            objects: Vec::new(),
            factory: Box::new(factory),
        }
    }

    /// Get an object from the pool or create a new one
    pub fn get(&mut self) -> T {
        self.objects.pop().unwrap_or_else(|| (self.factory)())
    }

    /// Return an object to the pool
    pub fn put(&mut self, obj: T) {
        self.objects.push(obj);
    }

    /// Pre-allocate objects in the pool
    pub fn preallocate(&mut self, count: usize) {
        self.objects.reserve(count);
        for _ in 0..count {
            self.objects.push((self.factory)());
        }
    }

    /// Get pool statistics
    pub fn stats(&self) -> PoolStats {
        PoolStats {
            available: self.objects.len(),
            capacity: self.objects.capacity(),
        }
    }

    /// Clear the pool
    pub fn clear(&mut self) {
        self.objects.clear();
    }
}

/// Statistics for object pool
#[derive(Debug, Clone)]
pub struct PoolStats {
    /// Number of available objects in pool
    pub available: usize,
    /// Pool capacity
    pub capacity: usize,
}

/// Custom B-tree implementation optimized for account hierarchies
#[derive(Debug, Clone)]
pub struct AccountTree<T> {
    roots: FastHashMap<String, Box<AccountNode<T>>>,
    size: usize,
}

#[derive(Debug, Clone)]
struct AccountNode<T> {
    name: String,
    value: Option<T>,
    children: FastHashMap<String, Box<AccountNode<T>>>,
}

impl<T> Default for AccountTree<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> AccountTree<T> {
    /// Create a new account tree
    pub fn new() -> Self {
        Self {
            roots: FastHashMap::default(),
            size: 0,
        }
    }

    /// Insert a value at the given account path
    pub fn insert(&mut self, path: &str, value: T) {
        let parts: Vec<&str> = path.split(':').collect();
        if parts.is_empty() {
            return;
        }

        let root_name = parts[0].to_string();
        
        // Create root node if it doesn't exist
        if !self.roots.contains_key(&root_name) {
            self.roots.insert(root_name.clone(), Box::new(AccountNode {
                name: root_name.clone(),
                value: None,
                children: FastHashMap::default(),
            }));
        }

        let mut current = self.roots.get_mut(&root_name).unwrap();
        
        // If we only have one part, set the value on the root
        if parts.len() == 1 {
            if current.value.is_none() {
                self.size += 1;
            }
            current.value = Some(value);
            return;
        }
        
        // Navigate through the path starting from the second part
        for part in &parts[1..] {
            // Create child node if it doesn't exist
            if !current.children.contains_key(*part) {
                current.children.insert(part.to_string(), Box::new(AccountNode {
                    name: part.to_string(),
                    value: None,
                    children: FastHashMap::default(),
                }));
            }
            current = current.children.get_mut(*part).unwrap();
        }

        // Insert the value at the final node
        if current.value.is_none() {
            self.size += 1;
        }
        current.value = Some(value);
    }

    /// Get a value at the given account path
    pub fn get(&self, path: &str) -> Option<&T> {
        let parts: Vec<&str> = path.split(':').collect();
        if parts.is_empty() {
            return None;
        }

        let root_name = parts[0];
        let current = self.roots.get(root_name)?;
        
        // If we only have one part, return the root's value
        if parts.len() == 1 {
            return current.value.as_ref();
        }
        
        // Navigate through the path
        let mut current = current.as_ref();
        for part in &parts[1..] {
            if let Some(child) = current.children.get(*part) {
                current = child;
            } else {
                return None;
            }
        }

        current.value.as_ref()
    }

    /// Get the number of accounts with values
    pub fn len(&self) -> usize {
        self.size
    }

    /// Check if the tree is empty
    pub fn is_empty(&self) -> bool {
        self.size == 0
    }

    /// Iterate over all account paths and their values
    pub fn iter(&self) -> AccountTreeIter<'_, T> {
        AccountTreeIter::new(&self.roots)
    }

    /// Get tree statistics
    pub fn stats(&self) -> TreeStats {
        let mut stats = TreeStats::default();
        for root in self.roots.values() {
            self.collect_stats(root, 0, &mut stats);
        }
        stats
    }

    fn collect_stats(&self, node: &AccountNode<T>, depth: usize, stats: &mut TreeStats) {
        stats.total_nodes += 1;
        stats.max_depth = stats.max_depth.max(depth);
        
        if node.value.is_some() {
            stats.value_nodes += 1;
        }

        stats.total_children += node.children.len();
        if !node.children.is_empty() {
            stats.internal_nodes += 1;
        }

        for child in node.children.values() {
            self.collect_stats(child, depth + 1, stats);
        }
    }
}

/// Statistics for account tree
#[derive(Debug, Clone, Default)]
pub struct TreeStats {
    /// Total number of nodes
    pub total_nodes: usize,
    /// Number of nodes with values
    pub value_nodes: usize,
    /// Number of internal nodes (with children)
    pub internal_nodes: usize,
    /// Total number of child relationships
    pub total_children: usize,
    /// Maximum depth of the tree
    pub max_depth: usize,
}

/// Iterator for account tree
pub struct AccountTreeIter<'a, T> {
    stack: Vec<(&'a AccountNode<T>, String)>,
}

impl<'a, T> AccountTreeIter<'a, T> {
    fn new(roots: &'a FastHashMap<String, Box<AccountNode<T>>>) -> Self {
        let mut iter = Self {
            stack: Vec::new(),
        };

        // Add all roots to the stack
        for root in roots.values() {
            iter.stack.push((root, root.name.clone()));
        }

        iter
    }
}

impl<'a, T> Iterator for AccountTreeIter<'a, T> {
    type Item = (String, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        while let Some((node, path)) = self.stack.pop() {
            // Add children to stack for future iteration
            for (name, child) in &node.children {
                let child_path = format!("{}:{}", path, name);
                self.stack.push((child, child_path));
            }

            // Return current node if it has a value
            if let Some(ref value) = node.value {
                return Some((path, value));
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sparse_vec_basic_operations() {
        let mut sv = SparseVec::new();
        
        // Test insertion and retrieval
        sv.insert(10, "ten");
        sv.insert(5, "five");
        sv.insert(100, "hundred");
        
        assert_eq!(sv.get(10), Some(&"ten"));
        assert_eq!(sv.get(5), Some(&"five"));
        assert_eq!(sv.get(100), Some(&"hundred"));
        assert_eq!(sv.get(50), None);
        
        assert_eq!(sv.len(), 101); // highest index + 1
        assert_eq!(sv.occupied_count(), 3);
        
        let stats = sv.memory_usage();
        assert!(stats.sparsity_ratio > 0.9); // Very sparse
    }

    #[test]
    fn test_arena_allocation() {
        let mut arena = Arena::with_chunk_size(2);
        
        // Allocate values and verify they are stored correctly
        arena.alloc(42);
        arena.alloc(84);
        arena.alloc(126); // Should trigger new chunk
        
        let stats = arena.memory_usage();
        assert_eq!(stats.chunks, 2);
        assert_eq!(stats.total_used, 3);
        
        // Check that we can retrieve values from the arena
        arena.clear();
        let val1 = arena.alloc(100);
        assert_eq!(*val1, 100);
    }

    #[test]
    fn test_object_pool() {
        let mut pool = ObjectPool::new(|| Vec::<i32>::new());
        
        // Pre-allocate some objects
        pool.preallocate(5);
        assert_eq!(pool.stats().available, 5);
        
        // Get and use an object
        let mut vec = pool.get();
        vec.push(42);
        
        // Return it to pool
        vec.clear(); // Reset state
        pool.put(vec);
        
        assert_eq!(pool.stats().available, 5);
    }

    #[test]
    fn test_account_tree() {
        let mut tree = AccountTree::new();
        
        tree.insert("Assets", 1000);
        tree.insert("Assets:Cash", 500);
        tree.insert("Assets:Bank:Checking", 300);
        tree.insert("Expenses:Food", -50);
        
        assert_eq!(tree.get("Assets"), Some(&1000));
        assert_eq!(tree.get("Assets:Cash"), Some(&500));
        assert_eq!(tree.get("Assets:Bank:Checking"), Some(&300));
        assert_eq!(tree.get("Expenses:Food"), Some(&-50));
        assert_eq!(tree.get("Assets:Bank"), None); // No value set
        
        assert_eq!(tree.len(), 4);
        
        let stats = tree.stats();
        assert!(stats.max_depth >= 2); // Assets:Bank:Checking is depth 2
        assert!(stats.value_nodes == 4);
    }

    #[test]
    fn test_fast_hash_map() {
        let mut map: FastHashMap<&str, i32> = FastHashMap::default();
        
        map.insert("key1", 100);
        map.insert("key2", 200);
        
        assert_eq!(map.get("key1"), Some(&100));
        assert_eq!(map.get("key2"), Some(&200));
        assert_eq!(map.len(), 2);
    }

    #[test]
    fn test_ordered_map() {
        let mut map: OrderedMap<&str, i32> = OrderedMap::new();
        
        map.insert("third", 3);
        map.insert("first", 1);
        map.insert("second", 2);
        
        let keys: Vec<&str> = map.keys().copied().collect();
        assert_eq!(keys, vec!["third", "first", "second"]); // Insertion order preserved
    }
}