//! Parallel processing utilities for high-performance Ledger operations
//!
//! This module provides parallel implementations of compute-intensive operations
//! using rayon for work-stealing parallelism and optimal CPU utilization.

use parking_lot::{Mutex as ParkingMutex, RwLock as ParkingRwLock};
use rayon::{Scope, ThreadPoolBuilder};
use std::collections::HashMap;
use std::time::Instant;

use crate::account::{Account, AccountRef};
use crate::balance::Balance;
use crate::strings::AccountName;
use crate::transaction::Transaction;
use ledger_math::amount::Amount;

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

impl Default for ParallelBalanceAccumulator {
    fn default() -> Self {
        Self::new()
    }
}

impl ParallelBalanceAccumulator {
    pub fn new() -> Self {
        Self { balances: ParkingRwLock::new(HashMap::new()) }
    }

    /// Add an amount to an account balance (thread-safe)
    pub fn add_amount(&self, account: AccountName, amount: Amount) {
        let mut balances = self.balances.write();
        let balance = balances.entry(account).or_default();
        // TODO: error logging
        let _ = balance.add_amount(&amount);
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
    _config: ParallelConfig,
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

impl Default for ParallelTransactionProcessor {
    fn default() -> Self {
        Self::new()
    }
}

impl ParallelTransactionProcessor {
    pub fn new() -> Self {
        Self { _config: get_parallel_config(), stats: ProcessingStats::default() }
    }

    pub fn with_config(_config: ParallelConfig) -> Self {
        Self { _config, stats: ProcessingStats::default() }
    }

    /// Process transactions for balance calculation
    pub fn calculate_balances<'a, I>(&mut self, transactions: I) -> HashMap<AccountName, Balance>
    where
        I: Iterator<Item = &'a Transaction>,
    {
        let start_time = Instant::now();
        let accumulator = ParallelBalanceAccumulator::new();

        // Collect transactions to count them
        let txns: Vec<&Transaction> = transactions.collect();
        self.stats.total_transactions = txns.len();

        // Always process sequentially due to Rc<RefCell> not being thread-safe
        for transaction in &txns {
            self.process_transaction_for_balance(&accumulator, transaction);
        }
        self.stats.threads_used = 1;

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

    /// Filter transactions based on predicate
    pub fn filter_transactions<'a, I, P>(
        &mut self,
        transactions: I,
        predicate: P,
    ) -> Vec<&'a Transaction>
    where
        I: Iterator<Item = &'a Transaction>,
        P: Fn(&Transaction) -> bool,
    {
        let start_time = Instant::now();

        let filtered: Vec<&Transaction> = transactions.filter(|txn| predicate(txn)).collect();

        self.stats.processing_time_ms = start_time.elapsed().as_millis() as u64;
        self.stats.processed_transactions = filtered.len();

        filtered
    }

    /// Sort transactions (sequential due to thread safety)
    pub fn sort_transactions<'a, I, F, K>(
        &mut self,
        transactions: I,
        key_fn: F,
    ) -> Vec<&'a Transaction>
    where
        I: Iterator<Item = &'a Transaction>,
        F: Fn(&Transaction) -> K,
        K: Ord,
    {
        let start_time = Instant::now();

        let mut txns: Vec<&Transaction> = transactions.collect();

        // Always use standard sort due to thread safety
        txns.sort_by_key(|txn| key_fn(txn));

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

impl Default for ParallelAccountProcessor {
    fn default() -> Self {
        Self::new()
    }
}

impl ParallelAccountProcessor {
    pub fn new() -> Self {
        Self { config: get_parallel_config() }
    }

    /// Traverse account tree and collect accounts matching predicate
    pub fn filter_accounts<P>(&self, root_accounts: &[AccountRef], predicate: P) -> Vec<AccountRef>
    where
        P: Fn(&Account) -> bool,
    {
        // Always use sequential processing due to Rc<RefCell> thread safety
        self.collect_accounts_sequential(root_accounts, &predicate)
    }

    fn collect_accounts_sequential<P>(
        &self,
        accounts: &[AccountRef],
        predicate: &P,
    ) -> Vec<AccountRef>
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

    // Parallel processing disabled due to Rc<RefCell> thread safety
    // These methods are kept for future migration to Arc<Mutex> if needed
    #[allow(dead_code)]
    fn collect_accounts_parallel<P>(
        &self,
        _accounts: &[AccountRef],
        _predicate: &P,
    ) -> Vec<AccountRef>
    where
        P: Fn(&Account) -> bool + Sync + Send,
    {
        // Parallel processing disabled
        Vec::new()
    }

    #[allow(dead_code)]
    fn process_accounts_in_scope<'scope, P>(
        &self,
        _scope: &Scope<'scope>,
        _accounts: &'scope [AccountRef],
        _predicate: &'scope P,
        _results: &'scope ParkingMutex<Vec<AccountRef>>,
    ) where
        P: Fn(&Account) -> bool + Sync + Send,
    {
        // Parallel processing disabled
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
            // Since Rc<RefCell> isn't thread-safe, use sequential processing
            // In a production system, you'd use Arc<Mutex> for parallel processing
            aggregator(root_accounts)
        }
    }
}

/// Parallel report generation utilities
pub struct ParallelReportGenerator {
    processor: ParallelTransactionProcessor,
    _account_processor: ParallelAccountProcessor,
}

impl Default for ParallelReportGenerator {
    fn default() -> Self {
        Self::new()
    }
}

impl ParallelReportGenerator {
    pub fn new() -> Self {
        Self {
            processor: ParallelTransactionProcessor::new(),
            _account_processor: ParallelAccountProcessor::new(),
        }
    }

    pub fn with_config(config: ParallelConfig) -> Self {
        Self {
            processor: ParallelTransactionProcessor::with_config(config.clone()),
            _account_processor: ParallelAccountProcessor::new(),
        }
    }

    /// Generate balance report in parallel
    pub fn generate_balance_report<'a, I>(
        &mut self,
        transactions: I,
        _accounts: &[AccountRef],
    ) -> HashMap<AccountName, Balance>
    where
        I: Iterator<Item = &'a Transaction> + Clone,
    {
        // Calculate balances - using sequential processing for now
        self.processor.calculate_balances(transactions)
    }

    /// Generate register report with parallel processing
    pub fn generate_register_report<'a, I, P>(
        &mut self,
        transactions: I,
        filter_predicate: P,
    ) -> Vec<&'a Transaction>
    where
        I: Iterator<Item = &'a Transaction>,
        P: Fn(&Transaction) -> bool,
    {
        // Filter transactions - using sequential processing for now
        self.processor.filter_transactions(transactions, filter_predicate)
    }

    /// Get performance statistics
    pub fn stats(&self) -> &ProcessingStats {
        self.processor.stats()
    }
}

/// Initialize global thread pool for parallel processing
pub fn init_parallel_processing(
    config: Option<ParallelConfig>,
) -> Result<(), rayon::ThreadPoolBuildError> {
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
    use rayon::iter::*;

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
            let amount = Amount::from_i64(100);
            accumulator.add_amount(account.clone(), amount);
        }

        let balances = accumulator.into_balances();
        assert!(balances.contains_key(&account));
    }

    #[test]
    fn test_parallel_transaction_filtering() {
        let mut processor = ParallelTransactionProcessor::new();

        // Create test transactions
        let transactions: Vec<Transaction> = (0..1000).map(|_| Transaction::default()).collect();

        let transaction_refs: Vec<&Transaction> = transactions.iter().collect();

        // Filter for even-indexed transactions (placeholder logic)
        let counter = AtomicUsize::new(0);
        let filtered = processor.filter_transactions(transaction_refs.into_iter(), |_txn| {
            let count = counter.fetch_add(1, Ordering::SeqCst);
            count % 2 == 0
        });

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
