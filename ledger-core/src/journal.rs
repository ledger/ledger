//! Journal data structure for storing transactions

use crate::account::{Account, AccountRef};
use crate::transaction::Transaction;
use ledger_math::commodity::Commodity;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

/// Main journal containing all transactions
#[derive(Debug, Default, Clone)]
pub struct Journal {
    /// List of transactions
    pub transactions: Vec<Transaction>,
    /// Account registry
    pub accounts: HashMap<String, AccountRef>,
    /// Commodity registry
    pub commodities: HashMap<String, Arc<Commodity>>,
    /// Next account ID
    next_account_id: usize,
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

    /// Find an account by name
    pub fn find_account(&self, name: &str) -> Result<AccountRef, String> {
        self.accounts.get(name).cloned().ok_or_else(|| format!("Account not found: {}", name))
    }

    /// Get or create an account by name
    pub fn get_or_create_account(&mut self, name: &str) -> AccountRef {
        if let Some(account) = self.accounts.get(name) {
            account.clone()
        } else {
            let account_id = self.next_account_id;
            self.next_account_id += 1;
            let account = Rc::new(RefCell::new(Account::new(name.into(), None, account_id)));
            self.accounts.insert(name.to_string(), account.clone());
            account
        }
    }

    /// Add an account to the journal
    pub fn add_account(&mut self, account: AccountRef) -> Result<(), String> {
        let name = account.borrow().name().to_string();
        if self.accounts.contains_key(&name) {
            return Err(format!("Account already exists: {}", name));
        }
        self.accounts.insert(name, account);
        Ok(())
    }

    /// Add a commodity to the journal
    pub fn add_commodity(&mut self, commodity: Arc<Commodity>) -> Result<(), String> {
        let symbol = commodity.symbol().to_string();
        if self.commodities.contains_key(&symbol) {
            return Err(format!("Commodity already exists: {}", symbol));
        }
        self.commodities.insert(symbol, commodity);
        Ok(())
    }

    /// Merge another journal into this one
    pub fn merge(&mut self, other: Journal) -> Result<(), String> {
        // Merge transactions
        self.transactions.extend(other.transactions);

        // Merge accounts
        for (name, account) in other.accounts {
            if !self.accounts.contains_key(&name) {
                self.accounts.insert(name, account);
            }
        }

        // Merge commodities
        for (symbol, commodity) in other.commodities {
            if !self.commodities.contains_key(&symbol) {
                self.commodities.insert(symbol, commodity);
            }
        }

        Ok(())
    }
}
