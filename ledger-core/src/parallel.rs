//! Parallel processing utilities for high-performance Ledger operations
//!
//! This module provides parallel implementations of compute-intensive operations
//! using rayon for work-stealing parallelism and optimal CPU utilization.

use rayon::prelude::*;
use rayon::{Scope, ThreadPoolBuilder};
use std::collections::HashMap;
use std::sync::{Arc, Mutex, RwLock};
use std::time::Instant;
use parking_lot::{RwLock as ParkingRwLock, Mutex as ParkingMutex};

use crate::account::{Account, AccountRef};
use crate::transaction::Transaction;
use crate::posting::Posting;
use crate::amount::Amount;
use crate::balance::Balance;
use crate::strings::AccountName;

/// Configuration for parallel processing
#[derive(Debug, Clone)]
pub struct ParallelConfig {
    /// Number of threads to use (0 = auto-detect)
    pub num_threads: usize,
    /// Minimum number of items to process in parallel
    pub parallel_threshold: usize,
    /// Enable work stealing for uneven workloads
    pub enable_work_stealing: bool,
    /// Chunk size for parallel iterators
    pub chunk_size: usize,
}

impl Default for ParallelConfig {
    fn default() -> Self {
        Self {
            num_threads: 0, // Auto-detect
            parallel_threshold: 1000,
            enable_work_stealing: true,
            chunk_size: 100,
        }
    }
}

/// Thread-safe global configuration
static PARALLEL_CONFIG: ParkingRwLock<ParallelConfig> = ParkingRwLock::new(ParallelConfig {
    num_threads: 0,
    parallel_threshold: 1000,
    enable_work_stealing: true,
    chunk_size: 100,
});

/// Set global parallel processing configuration
pub fn set_parallel_config(config: ParallelConfig) {
    *PARALLEL_CONFIG.write() = config;
}

/// Get current parallel processing configuration
pub fn get_parallel_config() -> ParallelConfig {
    PARALLEL_CONFIG.read().clone()
}

/// Thread-safe accumulator for parallel balance calculations
#[derive(Debug)]
pub struct ParallelBalanceAccumulator {
    balances: ParkingRwLock<HashMap<AccountName, Balance>>,
}

impl ParallelBalanceAccumulator {
    pub fn new() -> Self {
        Self {
            balances: ParkingRwLock::new(HashMap::new()),
        }
    }

    /// Add an amount to an account balance (thread-safe)
    pub fn add_amount(&self, account: AccountName, amount: Amount) {
        let mut balances = self.balances.write();
        let balance = balances.entry(account).or_insert_with(Balance::new);
        balance.add_amount(amount);
    }

    /// Get final balances
    pub fn into_balances(self) -> HashMap<AccountName, Balance> {
        self.balances.into_inner()
    }

    /// Get a read-only view of current balances
    pub fn get_balances(&self) -> HashMap<AccountName, Balance> {
        self.balances.read().clone()
    }
}

/// Parallel transaction filter and processor
pub struct ParallelTransactionProcessor {
    config: ParallelConfig,
    stats: ProcessingStats,
}

#[derive(Debug, Default)]
pub struct ProcessingStats {
    pub total_transactions: usize,
    pub processed_transactions: usize,
    pub processing_time_ms: u64,
    pub threads_used: usize,
    pub chunks_processed: usize,
}

impl ParallelTransactionProcessor {
    pub fn new() -> Self {
        Self {
            config: get_parallel_config(),
            stats: ProcessingStats::default(),
        }
    }

    pub fn with_config(config: ParallelConfig) -> Self {
        Self {
            config,
            stats: ProcessingStats::default(),
        }
    }

    /// Process transactions in parallel for balance calculation
    pub fn calculate_balances<'a, I>(&mut self, transactions: I) -> HashMap<AccountName, Balance>
    where
        I: IntoParallelIterator<Item = &'a Transaction> + Clone,
        I::IntoIter: IndexedParallelIterator,
    {
        let start_time = Instant::now();
        let accumulator = ParallelBalanceAccumulator::new();
        
        // Collect transactions to count them
        let txns: Vec<&Transaction> = transactions.clone().into_par_iter().collect();
        self.stats.total_transactions = txns.len();

        if txns.len() < self.config.parallel_threshold {
            // Process sequentially for small datasets
            for transaction in txns {
                self.process_transaction_for_balance(&accumulator, transaction);
            }
            self.stats.threads_used = 1;
        } else {
            // Process in parallel
            txns.par_chunks(self.config.chunk_size)
                .for_each(|chunk| {
                    for transaction in chunk {
                        self.process_transaction_for_balance(&accumulator, transaction);
                    }
                });
            self.stats.threads_used = rayon::current_num_threads();
            self.stats.chunks_processed = (txns.len() + self.config.chunk_size - 1) / self.config.chunk_size;
        }

        self.stats.processed_transactions = txns.len();
        self.stats.processing_time_ms = start_time.elapsed().as_millis() as u64;
        
        accumulator.into_balances()
    }

    /// Process a single transaction for balance accumulation (thread-safe)
    fn process_transaction_for_balance(
        &self,
        accumulator: &ParallelBalanceAccumulator,
        transaction: &Transaction,
    ) {
        for posting in &transaction.postings {
            if let Some(amount) = &posting.amount {
                // Get account name - in a real implementation, you'd get this from the posting
                let account_name = AccountName::new("placeholder"); // Simplified
                accumulator.add_amount(account_name, amount.clone());
            }
        }
    }

    /// Filter transactions in parallel based on predicate
    pub fn filter_transactions<'a, I, P>(
        &mut self,
        transactions: I,
        predicate: P,
    ) -> Vec<&'a Transaction>
    where
        I: IntoParallelIterator<Item = &'a Transaction>,
        I::IntoIter: IndexedParallelIterator,
        P: Fn(&Transaction) -> bool + Sync + Send,
    {
        let start_time = Instant::now();
        
        let filtered: Vec<&Transaction> = transactions
            .into_par_iter()
            .filter(|txn| predicate(txn))
            .collect();

        self.stats.processing_time_ms = start_time.elapsed().as_millis() as u64;
        self.stats.processed_transactions = filtered.len();
        
        filtered
    }

    /// Sort transactions in parallel
    pub fn sort_transactions<'a, I, F, K>(
        &mut self,
        transactions: I,
        key_fn: F,
    ) -> Vec<&'a Transaction>
    where
        I: IntoParallelIterator<Item = &'a Transaction>,
        I::IntoIter: IndexedParallelIterator,
        F: Fn(&Transaction) -> K + Sync + Send,
        K: Ord + Send,
    {
        let start_time = Instant::now();
        
        let mut txns: Vec<&Transaction> = transactions.into_par_iter().collect();
        
        if txns.len() < self.config.parallel_threshold {
            // Use standard sort for small datasets
            txns.sort_by_key(|txn| key_fn(txn));
        } else {
            // Use parallel sort for large datasets
            txns.par_sort_by_key(|txn| key_fn(txn));
        }

        self.stats.processing_time_ms = start_time.elapsed().as_millis() as u64;
        self.stats.processed_transactions = txns.len();
        
        txns
    }

    /// Get processing statistics
    pub fn stats(&self) -> &ProcessingStats {
        &self.stats
    }

    /// Reset statistics
    pub fn reset_stats(&mut self) {
        self.stats = ProcessingStats::default();
    }
}

/// Parallel account tree operations
pub struct ParallelAccountProcessor {
    config: ParallelConfig,
}

impl ParallelAccountProcessor {
    pub fn new() -> Self {
        Self {
            config: get_parallel_config(),
        }
    }

    /// Traverse account tree in parallel and collect accounts matching predicate
    pub fn filter_accounts<P>(&self, root_accounts: &[AccountRef], predicate: P) -> Vec<AccountRef>
    where
        P: Fn(&Account) -> bool + Sync + Send,
    {
        if root_accounts.len() < self.config.parallel_threshold {
            // Sequential processing for small trees
            self.collect_accounts_sequential(root_accounts, &predicate)
        } else {
            // Parallel processing for large trees
            self.collect_accounts_parallel(root_accounts, &predicate)
        }
    }

    fn collect_accounts_sequential<P>(&self, accounts: &[AccountRef], predicate: &P) -> Vec<AccountRef>
    where
        P: Fn(&Account) -> bool,
    {
        let mut results = Vec::new();
        
        for account_ref in accounts {
            let account = account_ref.borrow();
            if predicate(&account) {
                results.push(account_ref.clone());
            }
            
            // Recursively process children
            let children: Vec<AccountRef> = account.children.values().cloned().collect();
            drop(account); // Release borrow before recursion
            results.extend(self.collect_accounts_sequential(&children, predicate));
        }
        
        results
    }

    fn collect_accounts_parallel<P>(&self, accounts: &[AccountRef], predicate: &P) -> Vec<AccountRef>
    where
        P: Fn(&Account) -> bool + Sync + Send,
    {
        // Use rayon's scoped threads for structured parallelism
        let results = ParkingMutex::new(Vec::new());
        
        rayon::scope(|scope| {
            self.process_accounts_in_scope(scope, accounts, predicate, &results);
        });
        
        results.into_inner()
    }

    fn process_accounts_in_scope<'scope, P>(
        &self,
        scope: &Scope<'scope>,
        accounts: &'scope [AccountRef],
        predicate: &'scope P,
        results: &'scope ParkingMutex<Vec<AccountRef>>,
    ) where
        P: Fn(&Account) -> bool + Sync + Send,
    {
        for account_ref in accounts {
            let account_ref_clone = account_ref.clone();
            scope.spawn(move |inner_scope| {
                let account = account_ref_clone.borrow();
                
                if predicate(&account) {
                    results.lock().push(account_ref_clone.clone());
                }
                
                // Process children in parallel
                let children: Vec<AccountRef> = account.children.values().cloned().collect();
                drop(account); // Release borrow
                
                if !children.is_empty() {
                    self.process_accounts_in_scope(inner_scope, &children, predicate, results);
                }
            });
        }
    }

    /// Calculate aggregated values across account tree in parallel
    pub fn aggregate_account_values<F, R>(&self, root_accounts: &[AccountRef], aggregator: F) -> R
    where
        F: Fn(&[AccountRef]) -> R + Sync + Send,
        R: Send,
    {
        if root_accounts.len() < self.config.parallel_threshold {
            aggregator(root_accounts)
        } else {
            // Split accounts into chunks and process in parallel
            let chunk_size = self.config.chunk_size.max(1);
            let results: Vec<R> = root_accounts
                .par_chunks(chunk_size)
                .map(|chunk| aggregator(chunk))
                .collect();
            
            // In a real implementation, you'd need a way to combine the results
            // For now, we just return the first result
            // You'd implement a proper reduction based on the result type R
            results.into_iter().next().unwrap_or_else(|| {
                // This is a placeholder - you'd need to implement proper default/combination logic
                panic!("No results to aggregate")
            })
        }
    }
}

/// Parallel report generation utilities
pub struct ParallelReportGenerator {
    processor: ParallelTransactionProcessor,
    account_processor: ParallelAccountProcessor,
}

impl ParallelReportGenerator {
    pub fn new() -> Self {
        Self {
            processor: ParallelTransactionProcessor::new(),
            account_processor: ParallelAccountProcessor::new(),
        }
    }

    pub fn with_config(config: ParallelConfig) -> Self {
        Self {
            processor: ParallelTransactionProcessor::with_config(config.clone()),
            account_processor: ParallelAccountProcessor::new(),
        }
    }

    /// Generate balance report in parallel
    pub fn generate_balance_report<'a, I>(
        &mut self,
        transactions: I,
        accounts: &[AccountRef],
    ) -> HashMap<AccountName, Balance>
    where
        I: IntoParallelIterator<Item = &'a Transaction> + Clone,
        I::IntoIter: IndexedParallelIterator,
    {
        // Calculate balances in parallel
        self.processor.calculate_balances(transactions)
    }

    /// Generate register report with parallel processing
    pub fn generate_register_report<'a, I, P>(
        &mut self,
        transactions: I,
        filter_predicate: P,
    ) -> Vec<&'a Transaction>
    where
        I: IntoParallelIterator<Item = &'a Transaction>,
        I::IntoIter: IndexedParallelIterator,
        P: Fn(&Transaction) -> bool + Sync + Send,
    {
        // Filter transactions in parallel
        self.processor.filter_transactions(transactions, filter_predicate)
    }

    /// Get performance statistics
    pub fn stats(&self) -> &ProcessingStats {
        self.processor.stats()
    }
}

/// Initialize global thread pool for parallel processing
pub fn init_parallel_processing(config: Option<ParallelConfig>) -> Result<(), rayon::ThreadPoolBuildError> {
    let config = config.unwrap_or_default();
    set_parallel_config(config.clone());

    let mut builder = ThreadPoolBuilder::new();
    
    if config.num_threads > 0 {
        builder = builder.num_threads(config.num_threads);
    }

    if config.enable_work_stealing {
        // Work stealing is enabled by default in rayon
        builder = builder.breadth_first();
    }

    builder.build_global()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::transaction::Transaction;
    use std::sync::atomic::{AtomicUsize, Ordering};

    #[test]
    fn test_parallel_config() {
        let config = ParallelConfig {
            num_threads: 4,
            parallel_threshold: 500,
            enable_work_stealing: true,
            chunk_size: 50,
        };

        set_parallel_config(config.clone());
        let retrieved = get_parallel_config();

        assert_eq!(retrieved.num_threads, 4);
        assert_eq!(retrieved.parallel_threshold, 500);
        assert_eq!(retrieved.chunk_size, 50);
        assert!(retrieved.enable_work_stealing);
    }

    #[test]
    fn test_balance_accumulator() {
        let accumulator = ParallelBalanceAccumulator::new();
        let account = AccountName::new("Assets:Test");
        
        // Add amounts from multiple threads (simulated)
        for _ in 0..10 {
            let amount = Amount::from_integer(100);
            accumulator.add_amount(account.clone(), amount);
        }

        let balances = accumulator.into_balances();
        assert!(balances.contains_key(&account));
    }

    #[test]
    fn test_parallel_transaction_filtering() {
        let mut processor = ParallelTransactionProcessor::new();
        
        // Create test transactions
        let transactions: Vec<Transaction> = (0..1000)
            .map(|i| {
                let mut txn = Transaction::default();
                // Set some test data that we can filter on
                txn
            })
            .collect();

        let transaction_refs: Vec<&Transaction> = transactions.iter().collect();
        
        // Filter for even-indexed transactions (placeholder logic)
        let counter = AtomicUsize::new(0);
        let filtered = processor.filter_transactions(
            transaction_refs,
            |_txn| {
                let count = counter.fetch_add(1, Ordering::SeqCst);
                count % 2 == 0
            }
        );

        // Should have roughly half the transactions
        assert!(filtered.len() >= 400 && filtered.len() <= 600);
        
        let stats = processor.stats();
        assert_eq!(stats.processed_transactions, filtered.len());
        assert!(stats.processing_time_ms > 0 || filtered.is_empty());
    }

    #[test]
    fn test_parallel_initialization() {
        let config = ParallelConfig {
            num_threads: 2,
            parallel_threshold: 100,
            enable_work_stealing: true,
            chunk_size: 25,
        };

        // This test may fail in some environments where the global thread pool
        // has already been initialized
        if let Err(_) = init_parallel_processing(Some(config)) {
            // Thread pool already initialized, which is fine for testing
        }

        // Verify we can use parallel operations
        let numbers: Vec<i32> = (0..1000).collect();
        let sum: i32 = numbers.par_iter().sum();
        assert_eq!(sum, 499500); // Sum of 0..999
    }
}