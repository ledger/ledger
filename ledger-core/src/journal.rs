//! Journal data structure for storing transactions

use crate::transaction::Transaction;

/// Main journal containing all transactions
#[derive(Debug, Default)]
pub struct Journal {
    /// List of transactions
    pub transactions: Vec<Transaction>,
}

impl Journal {
    /// Create a new empty journal
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a transaction to the journal
    pub fn add_transaction(&mut self, transaction: Transaction) {
        self.transactions.push(transaction);
    }
}
