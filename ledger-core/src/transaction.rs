//! Transaction representation

use chrono::{NaiveDate, NaiveDateTime};
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use rust_decimal::Decimal;
use crate::posting::{Posting, PostingFlags};
use ledger_math::amount::Amount;

/// Position information for source tracking
#[derive(Debug, Clone)]
pub struct Position {
    pub pathname: Option<PathBuf>,
    pub beg_line: usize,
    pub end_line: usize,
    pub sequence: usize,
}

/// Transaction flags
bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct TransactionFlags: u16 {
        const NORMAL = 0x00;
        const GENERATED = 0x01;
        const TEMP = 0x02;
        const NOTE_ON_NEXT_LINE = 0x04;
        const INFERRED = 0x08;
    }
}

/// Transaction status (state)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TransactionStatus {
    /// Uncleared
    Uncleared,
    /// Cleared (*)
    Cleared,
    /// Pending (!)
    Pending,
}

/// Transaction types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TransactionType {
    /// Regular transaction
    Normal,
    /// Automated transaction
    Automated,
    /// Periodic transaction
    Periodic,
}

/// Tag data for metadata
#[derive(Debug, Clone)]
pub struct TagData {
    pub value: Option<String>,
    pub inherited: bool,
}

/// Represents a transaction (xact in C++)
#[derive(Debug, Clone)]
pub struct Transaction {
    /// Transaction date
    pub date: NaiveDate,
    /// Optional auxiliary/effective date
    pub aux_date: Option<NaiveDate>,
    /// Status (cleared, pending, etc.)
    pub status: TransactionStatus,
    /// Transaction code (optional identifier)
    pub code: Option<String>,
    /// Payee/description
    pub payee: String,
    /// Note/comment
    pub note: Option<String>,
    /// List of postings
    pub postings: Vec<Posting>,
    /// Transaction flags
    pub flags: TransactionFlags,
    /// Transaction type
    pub transaction_type: TransactionType,
    /// Source position information
    pub pos: Option<Position>,
    /// Metadata tags
    pub metadata: HashMap<String, TagData>,
    /// Sequence number for ordering
    pub sequence: usize,
}

impl Default for TransactionStatus {
    fn default() -> Self {
        TransactionStatus::Uncleared
    }
}

impl Default for TransactionFlags {
    fn default() -> Self {
        TransactionFlags::NORMAL
    }
}

impl Default for Transaction {
    fn default() -> Self {
        Transaction {
            date: chrono::Local::now().date_naive(),
            aux_date: None,
            status: TransactionStatus::Uncleared,
            code: None,
            payee: String::new(),
            note: None,
            postings: Vec::new(),
            flags: TransactionFlags::default(),
            transaction_type: TransactionType::Normal,
            pos: None,
            metadata: HashMap::new(),
            sequence: 0,
        }
    }
}

impl Default for TransactionType {
    fn default() -> Self {
        TransactionType::Normal
    }
}

impl Transaction {
    /// Create a new transaction with required fields
    pub fn new(date: NaiveDate, payee: String) -> Self {
        Self {
            date,
            aux_date: None,
            status: TransactionStatus::default(),
            code: None,
            payee,
            note: None,
            postings: Vec::new(),
            flags: TransactionFlags::default(),
            transaction_type: TransactionType::default(),
            pos: None,
            metadata: HashMap::new(),
            sequence: 0,
        }
    }

    /// Add a posting to this transaction
    pub fn add_posting(&mut self, posting: Posting) {
        self.postings.push(posting);
    }
    
    /// Set the auxiliary date
    pub fn set_aux_date(&mut self, date: Option<NaiveDate>) {
        self.aux_date = date;
    }
    
    /// Set the transaction status
    pub fn set_status(&mut self, status: TransactionStatus) {
        self.status = status;
    }
    
    /// Set the transaction code
    pub fn set_code(&mut self, code: Option<String>) {
        self.code = code;
    }

    /// Remove a posting from this transaction
    pub fn remove_posting(&mut self, index: usize) -> Option<Posting> {
        if index < self.postings.len() {
            Some(self.postings.remove(index))
        } else {
            None
        }
    }

    /// Get the effective date (aux_date if present, otherwise date)
    pub fn effective_date(&self) -> NaiveDate {
        self.aux_date.unwrap_or(self.date)
    }

    /// Check if transaction has a specific tag
    pub fn has_tag(&self, tag: &str) -> bool {
        self.metadata.contains_key(tag)
    }

    /// Get a tag value
    pub fn get_tag(&self, tag: &str) -> Option<&TagData> {
        self.metadata.get(tag)
    }

    /// Set a tag with optional value
    pub fn set_tag(&mut self, tag: String, value: Option<String>, inherited: bool) {
        self.metadata.insert(tag, TagData { value, inherited });
    }

    /// Get a description of this transaction
    pub fn description(&self) -> String {
        if let Some(ref pos) = self.pos {
            format!("transaction at line {}", pos.beg_line)
        } else {
            "generated transaction".to_string()
        }
    }

    /// Get the ID of this transaction (from UUID tag or sequence)
    pub fn id(&self) -> String {
        if let Some(uuid_tag) = self.get_tag("UUID") {
            if let Some(ref value) = uuid_tag.value {
                return value.clone();
            }
        }
        self.sequence.to_string()
    }

    /// Check if this transaction is valid
    pub fn valid(&self) -> bool {
        // Basic validation - must have at least one posting
        if self.postings.is_empty() {
            return false;
        }
        
        // Validate double-entry balancing
        self.verify_balance().is_ok()
    }

    /// Verify double-entry balance - all postings must sum to zero
    pub fn verify_balance(&self) -> Result<(), String> {
        let mut balances: HashMap<Option<Arc<ledger_math::commodity::Commodity>>, Decimal> = HashMap::new();
        let mut has_amount_postings = false;
        
        // Collect all postings that must balance
        let balancing_postings: Vec<&Posting> = self.postings.iter()
            .filter(|p| p.must_balance())
            .collect();
        
        if balancing_postings.is_empty() {
            return Ok(()); // All postings are virtual or non-balancing
        }
        
        // Calculate balance per commodity
        for posting in &balancing_postings {
            if let Some(ref amount) = posting.amount {
                has_amount_postings = true;
                let commodity = amount.commodity().cloned();
                
                let current_balance = balances.get(&commodity).unwrap_or(&Decimal::ZERO);
                balances.insert(commodity, current_balance + amount.value());
            }
        }
        
        if !has_amount_postings {
            return Ok(()); // No amounts to balance (yet)
        }
        
        // Check that each commodity sums to zero
        for (commodity, balance) in &balances {
            if !balance.is_zero() {
                let commodity_str = commodity.as_ref()
                    .map(|c| c.symbol())
                    .unwrap_or("(none)");
                return Err(format!(
                    "Transaction does not balance: {} commodity '{}' has balance {}",
                    self.description(),
                    commodity_str,
                    balance
                ));
            }
        }
        
        Ok(())
    }

    /// Calculate the magnitude (total absolute value) of this transaction
    pub fn magnitude(&self) -> HashMap<Option<Arc<ledger_math::commodity::Commodity>>, Decimal> {
        let mut magnitudes: HashMap<Option<Arc<ledger_math::commodity::Commodity>>, Decimal> = HashMap::new();
        
        for posting in &self.postings {
            if let Some(ref amount) = posting.amount {
                if posting.must_balance() {
                    let commodity = amount.commodity().cloned();
                    
                    let current_magnitude = magnitudes.get(&commodity).unwrap_or(&Decimal::ZERO);
                    magnitudes.insert(commodity, current_magnitude + amount.value().abs());
                }
            }
        }
        
        magnitudes
    }

    /// Finalize transaction - calculate missing amounts and validate
    pub fn finalize(&mut self) -> Result<(), String> {
        // First, try to auto-balance by calculating missing amounts
        self.auto_balance()?;
        
        // Then verify the transaction balances
        self.verify_balance()?;
        
        // Additional validation
        if self.postings.len() < 2 {
            return Err("Transaction must have at least two postings for double-entry".to_string());
        }
        
        Ok(())
    }

    /// Auto-balance transaction by calculating missing amounts
    pub fn auto_balance(&mut self) -> Result<(), String> {
        // Find postings without amounts that must balance
        let mut null_amount_indices: Vec<usize> = Vec::new();
        let mut balances: HashMap<Option<Arc<ledger_math::commodity::Commodity>>, Decimal> = HashMap::new();
        
        for (i, posting) in self.postings.iter().enumerate() {
            if posting.must_balance() {
                if posting.amount.is_none() {
                    null_amount_indices.push(i);
                } else if let Some(ref amount) = posting.amount {
                    let commodity = amount.commodity().cloned();
                    
                    let current_balance = balances.get(&commodity).unwrap_or(&Decimal::ZERO);
                    balances.insert(commodity, current_balance + amount.value());
                }
            }
        }
        
        // Can only auto-balance if exactly one posting per commodity is missing
        if null_amount_indices.len() > 1 {
            // Check if they're all the same commodity or can be inferred
            return Err("Cannot auto-balance: multiple postings without amounts".to_string());
        }
        
        if null_amount_indices.len() == 1 {
            let posting_index = null_amount_indices[0];
            
            // For now, assume single commodity transactions
            if balances.len() > 1 {
                return Err("Cannot auto-balance multi-commodity transaction with missing amount".to_string());
            }
            
            if balances.len() == 1 {
                let (commodity, balance) = balances.iter().next().unwrap();
                let missing_amount = -balance;
                
                // Set the calculated amount
                let amount = if commodity.is_none() {
                    Amount::new(missing_amount)
                } else {
                    Amount::with_commodity(missing_amount, commodity.clone())
                };
                
                self.postings[posting_index].set_calculated_amount(amount);
            } else if balances.is_empty() {
                // No amounts set yet - this is ok for now
                return Ok(());
            }
        }
        
        Ok(())
    }

    /// Add posting and auto-balance if needed
    pub fn add_posting_with_balance(&mut self, posting: Posting) -> Result<(), String> {
        self.add_posting(posting);
        self.auto_balance()
    }

    /// Check if transaction has any virtual postings
    pub fn has_virtual_postings(&self) -> bool {
        self.postings.iter().any(|p| p.is_virtual())
    }

    /// Check if transaction has any deferred postings  
    pub fn has_deferred_postings(&self) -> bool {
        self.postings.iter().any(|p| p.is_deferred())
    }

    /// Check if transaction has any calculated postings
    pub fn has_calculated_postings(&self) -> bool {
        self.postings.iter().any(|p| p.is_calculated())
    }

    /// Get all postings that must balance
    pub fn balancing_postings(&self) -> Vec<&Posting> {
        self.postings.iter().filter(|p| p.must_balance()).collect()
    }

    /// Get all virtual postings
    pub fn virtual_postings(&self) -> Vec<&Posting> {
        self.postings.iter().filter(|p| p.is_virtual()).collect()
    }

    /// Validate posting relationships and constraints
    pub fn validate_postings(&self) -> Result<(), String> {
        if self.postings.is_empty() {
            return Err("Transaction must have at least one posting".to_string());
        }
        
        let balancing_postings = self.balancing_postings();
        if balancing_postings.len() < 2 && !self.has_virtual_postings() {
            return Err("Transaction must have at least two balancing postings".to_string());
        }
        
        // Validate each posting
        for (i, posting) in self.postings.iter().enumerate() {
            if !posting.valid() {
                return Err(format!("Posting {} is invalid: {}", i, posting.description()));
            }
            
            // Check for duplicate accounts in balancing postings
            if posting.must_balance() {
                let account_name = posting.account_name();
                let duplicate_count = balancing_postings.iter()
                    .filter(|p| p.account_name() == account_name)
                    .count();
                
                if duplicate_count > 1 {
                    return Err(format!(
                        "Multiple balancing postings for account '{}' in transaction",
                        account_name
                    ));
                }
            }
        }
        
        Ok(())
    }

    /// Clear all extended data from postings to save memory
    pub fn clear_xdata(&mut self) {
        for posting in &mut self.postings {
            posting.clear_xdata();
        }
    }

    /// Check if any posting has extended data
    pub fn has_xdata(&self) -> bool {
        self.postings.iter().any(|p| p.has_xdata())
    }
}

/// Builder for ergonomic Transaction construction
#[derive(Debug)]
pub struct TransactionBuilder {
    date: NaiveDate,
    aux_date: Option<NaiveDate>,
    status: TransactionStatus,
    code: Option<String>,
    payee: String,
    note: Option<String>,
    flags: TransactionFlags,
    transaction_type: TransactionType,
    pos: Option<Position>,
    metadata: HashMap<String, TagData>,
    sequence: usize,
    postings: Vec<Posting>,
}

impl TransactionBuilder {
    /// Start building a new transaction with required date and payee
    pub fn new(date: NaiveDate, payee: String) -> Self {
        Self {
            date,
            aux_date: None,
            status: TransactionStatus::default(),
            code: None,
            payee,
            note: None,
            flags: TransactionFlags::default(),
            transaction_type: TransactionType::default(),
            pos: None,
            metadata: HashMap::new(),
            sequence: 0,
            postings: Vec::new(),
        }
    }

    /// Set auxiliary/effective date
    pub fn aux_date(mut self, aux_date: NaiveDate) -> Self {
        self.aux_date = Some(aux_date);
        self
    }

    /// Set transaction status
    pub fn status(mut self, status: TransactionStatus) -> Self {
        self.status = status;
        self
    }

    /// Set transaction code
    pub fn code<S: Into<String>>(mut self, code: S) -> Self {
        self.code = Some(code.into());
        self
    }

    /// Set note/comment
    pub fn note<S: Into<String>>(mut self, note: S) -> Self {
        self.note = Some(note.into());
        self
    }

    /// Set transaction flags
    pub fn flags(mut self, flags: TransactionFlags) -> Self {
        self.flags = flags;
        self
    }

    /// Add transaction flags
    pub fn add_flags(mut self, flags: TransactionFlags) -> Self {
        self.flags.insert(flags);
        self
    }

    /// Set transaction type
    pub fn transaction_type(mut self, transaction_type: TransactionType) -> Self {
        self.transaction_type = transaction_type;
        self
    }

    /// Set position information
    pub fn position(mut self, pos: Position) -> Self {
        self.pos = Some(pos);
        self
    }

    /// Add metadata tag
    pub fn tag<K, V>(mut self, key: K, value: Option<V>) -> Self 
    where
        K: Into<String>,
        V: Into<String>,
    {
        let tag_data = TagData {
            value: value.map(|v| v.into()),
            inherited: false,
        };
        self.metadata.insert(key.into(), tag_data);
        self
    }

    /// Set sequence number
    pub fn sequence(mut self, sequence: usize) -> Self {
        self.sequence = sequence;
        self
    }

    /// Add a posting
    pub fn posting(mut self, posting: Posting) -> Self {
        self.postings.push(posting);
        self
    }

    /// Add a posting with account and amount
    pub fn post_to<A>(mut self, account: A, amount: Amount) -> Self 
    where 
        A: Into<crate::account::AccountRef>,
    {
        let posting = Posting::with_amount(account.into(), amount);
        self.postings.push(posting);
        self
    }

    /// Add a posting with account but no amount (to be calculated)
    pub fn post_to_account<A>(mut self, account: A) -> Self 
    where 
        A: Into<crate::account::AccountRef>,
    {
        let posting = Posting::new(account.into());
        self.postings.push(posting);
        self
    }

    /// Add a virtual posting (enclosed in parentheses)
    pub fn virtual_post_to<A>(mut self, account: A, amount: Amount) -> Self 
    where 
        A: Into<crate::account::AccountRef>,
    {
        let mut posting = Posting::with_amount(account.into(), amount);
        posting.add_flags(PostingFlags::VIRTUAL);
        self.postings.push(posting);
        self
    }

    /// Add a deferred posting (enclosed in angle brackets)
    pub fn deferred_post_to<A>(mut self, account: A, amount: Amount) -> Self 
    where 
        A: Into<crate::account::AccountRef>,
    {
        let mut posting = Posting::with_amount(account.into(), amount);
        posting.add_flags(PostingFlags::DEFERRED);
        self.postings.push(posting);
        self
    }

    /// Build the transaction, performing validation
    pub fn build(self) -> Result<Transaction, String> {
        let mut transaction = Transaction {
            date: self.date,
            aux_date: self.aux_date,
            status: self.status,
            code: self.code,
            payee: self.payee,
            note: self.note,
            postings: self.postings,
            flags: self.flags,
            transaction_type: self.transaction_type,
            pos: self.pos,
            metadata: self.metadata,
            sequence: self.sequence,
        };

        // Attempt to finalize (auto-balance and validate)
        transaction.finalize()?;
        Ok(transaction)
    }

    /// Build the transaction without validation (useful for incomplete transactions)
    pub fn build_unchecked(self) -> Transaction {
        Transaction {
            date: self.date,
            aux_date: self.aux_date,
            status: self.status,
            code: self.code,
            payee: self.payee,
            note: self.note,
            postings: self.postings,
            flags: self.flags,
            transaction_type: self.transaction_type,
            pos: self.pos,
            metadata: self.metadata,
            sequence: self.sequence,
        }
    }

    /// Try to auto-balance and get validation errors without building
    pub fn validate(&mut self) -> Result<(), String> {
        let mut temp_transaction = Transaction {
            date: self.date,
            aux_date: self.aux_date,
            status: self.status,
            code: self.code.clone(),
            payee: self.payee.clone(),
            note: self.note.clone(),
            postings: self.postings.clone(),
            flags: self.flags,
            transaction_type: self.transaction_type,
            pos: self.pos.clone(),
            metadata: self.metadata.clone(),
            sequence: self.sequence,
        };

        temp_transaction.finalize()
    }

    /// Get current postings for inspection
    pub fn postings(&self) -> &[Posting] {
        &self.postings
    }

    /// Get current posting count
    pub fn posting_count(&self) -> usize {
        self.postings.len()
    }

    /// Check if transaction would balance
    pub fn is_balanced(&self) -> bool {
        let temp_transaction = Transaction {
            date: self.date,
            aux_date: self.aux_date,
            status: self.status,
            code: self.code.clone(),
            payee: self.payee.clone(),
            note: self.note.clone(),
            postings: self.postings.clone(),
            flags: self.flags,
            transaction_type: self.transaction_type,
            pos: self.pos.clone(),
            metadata: self.metadata.clone(),
            sequence: self.sequence,
        };

        temp_transaction.verify_balance().is_ok()
    }
}

/// Convenience methods for Transaction
impl Transaction {
    /// Create a transaction builder
    pub fn builder(date: NaiveDate, payee: String) -> TransactionBuilder {
        TransactionBuilder::new(date, payee)
    }

    /// Parse and add metadata from a tag string (e.g., \"key1:value1,key2:value2\")
    pub fn parse_and_add_tags(&mut self, tags_str: &str, overwrite_existing: bool) {
        for tag_pair in tags_str.split(',') {
            let tag_pair = tag_pair.trim();
            if let Some(colon_pos) = tag_pair.find(':') {
                let (tag, value) = tag_pair.split_at(colon_pos);
                let value = value[1..].trim(); // Remove the ':'
                if overwrite_existing || !self.metadata.contains_key(tag) {
                    self.set_tag(tag.to_string(), Some(value.to_string()), false);
                }
            } else {
                if overwrite_existing || !self.metadata.contains_key(tag_pair) {
                    self.set_tag(tag_pair.to_string(), None, false);
                }
            }
        }
    }

    /// Get all metadata keys
    pub fn metadata_keys(&self) -> Vec<String> {
        self.metadata.keys().cloned().collect()
    }

    /// Get metadata as a formatted string
    pub fn metadata_string(&self) -> String {
        if self.metadata.is_empty() {
            return String::new();
        }
        
        let mut parts: Vec<String> = Vec::new();
        for (key, tag_data) in &self.metadata {
            if let Some(ref value) = tag_data.value {
                parts.push(format!("{}:{}", key, value));
            } else {
                parts.push(key.clone());
            }
        }
        parts.join(", ")
    }

    /// Filter postings by metadata tag
    pub fn postings_with_tag(&self, tag: &str) -> Vec<&Posting> {
        self.postings.iter()
            .filter(|p| p.has_tag(tag, false))
            .collect()
    }

    /// Check if transaction has any posting with a specific tag
    pub fn has_posting_with_tag(&self, tag: &str) -> bool {
        self.postings.iter().any(|p| p.has_tag(tag, false))
    }

    /// Get transaction total value for virtual postings (separate from balance)
    pub fn virtual_balance(&self) -> HashMap<Option<Arc<ledger_math::commodity::Commodity>>, Decimal> {
        let mut balances: HashMap<Option<Arc<ledger_math::commodity::Commodity>>, Decimal> = HashMap::new();
        
        for posting in self.virtual_postings() {
            if let Some(ref amount) = posting.amount {
                let commodity = amount.commodity().cloned();
                
                let current_balance = balances.get(&commodity).unwrap_or(&Decimal::ZERO);
                balances.insert(commodity, current_balance + amount.value());
            }
        }
        
        balances
    }

    /// Create a virtual posting balance assertion
    pub fn assert_virtual_balance(&self, expected_balances: &HashMap<Option<Arc<ledger_math::commodity::Commodity>>, Decimal>) -> Result<(), String> {
        let actual_balances = self.virtual_balance();
        
        for (commodity, expected) in expected_balances {
            let actual = actual_balances.get(commodity).unwrap_or(&Decimal::ZERO);
            if actual != expected {
                let commodity_str = commodity.as_ref()
                    .map(|c| c.symbol())
                    .unwrap_or("(none)");
                return Err(format!(
                    "Virtual balance assertion failed for commodity '{}': expected {}, got {}",
                    commodity_str, expected, actual
                ));
            }
        }
        
        Ok(())
    }

    /// Find postings by account pattern
    pub fn postings_for_account_pattern(&self, pattern: &str) -> Vec<&Posting> {
        self.postings.iter()
            .filter(|p| {
                let account_name = p.account_name();
                account_name.contains(pattern) || account_name.starts_with(pattern)
            })
            .collect()
    }

    /// Get postings sorted by account name
    pub fn postings_sorted_by_account(&self) -> Vec<&Posting> {
        let mut postings: Vec<&Posting> = self.postings.iter().collect();
        postings.sort_by(|a, b| a.account_name().cmp(&b.account_name()));
        postings
    }

    /// Get postings sorted by amount (descending)
    pub fn postings_sorted_by_amount(&self) -> Vec<&Posting> {
        let mut postings: Vec<&Posting> = self.postings.iter().collect();
        postings.sort_by(|a, b| {
            let a_val = a.amount.as_ref().map(|amt| amt.value()).unwrap_or(Decimal::ZERO);
            let b_val = b.amount.as_ref().map(|amt| amt.value()).unwrap_or(Decimal::ZERO);
            b_val.cmp(&a_val) // Descending order
        });
        postings
    }

    /// Copy metadata from another transaction
    pub fn copy_metadata_from(&mut self, other: &Transaction, overwrite: bool) {
        for (key, tag_data) in &other.metadata {
            if overwrite || !self.metadata.contains_key(key) {
                self.metadata.insert(key.clone(), tag_data.clone());
            }
        }
    }

    /// Remove metadata tag
    pub fn remove_tag(&mut self, tag: &str) -> Option<TagData> {
        self.metadata.remove(tag)
    }

    /// Clear all metadata
    pub fn clear_metadata(&mut self) {
        self.metadata.clear();
    }

    /// Clone transaction with only virtual postings
    pub fn virtual_only_clone(&self) -> Transaction {
        let virtual_postings: Vec<Posting> = self.postings.iter()
            .filter(|p| p.is_virtual())
            .cloned()
            .collect();
        
        Transaction {
            date: self.date,
            aux_date: self.aux_date,
            status: self.status,
            code: self.code.clone(),
            payee: format!("{} (virtual only)", self.payee),
            note: self.note.clone(),
            postings: virtual_postings,
            flags: self.flags,
            transaction_type: self.transaction_type,
            pos: self.pos.clone(),
            metadata: self.metadata.clone(),
            sequence: self.sequence,
        }
    }

    /// Clone transaction with only balancing postings
    pub fn balancing_only_clone(&self) -> Transaction {
        let balancing_postings: Vec<Posting> = self.postings.iter()
            .filter(|p| p.must_balance())
            .cloned()
            .collect();
        
        Transaction {
            date: self.date,
            aux_date: self.aux_date,
            status: self.status,
            code: self.code.clone(),
            payee: format!("{} (balancing only)", self.payee),
            note: self.note.clone(),
            postings: balancing_postings,
            flags: self.flags,
            transaction_type: self.transaction_type,
            pos: self.pos.clone(),
            metadata: self.metadata.clone(),
            sequence: self.sequence,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::account::AccountTree;
    use rust_decimal::Decimal;
    use std::rc::Rc;
    use std::cell::RefCell;

    fn create_test_accounts() -> (AccountRef, AccountRef) {
        let mut tree = AccountTree::new();
        let assets = tree.find_account("Assets:Checking", true).unwrap();
        let expenses = tree.find_account("Expenses:Food", true).unwrap();
        (assets, expenses)
    }

    #[test]
    fn test_transaction_creation() {
        let date = NaiveDate::from_ymd_opt(2024, 1, 15).unwrap();
        let transaction = Transaction::new(date, "Test Payee".to_string());
        
        assert_eq!(transaction.date, date);
        assert_eq!(transaction.payee, "Test Payee");
        assert_eq!(transaction.status, TransactionStatus::Uncleared);
        assert!(transaction.postings.is_empty());
        assert_eq!(transaction.sequence, 0);
    }

    #[test]
    fn test_transaction_builder_basic() {
        let date = NaiveDate::from_ymd_opt(2024, 1, 15).unwrap();
        let (assets, expenses) = create_test_accounts();
        
        let result = Transaction::builder(date, "Grocery Store".to_string())
            .code("CHECK123")
            .status(TransactionStatus::Cleared)
            .post_to(assets, Amount::with_commodity(Decimal::from(-50), "USD".to_string()))
            .post_to(expenses, Amount::with_commodity(Decimal::from(50), "USD".to_string()))
            .build();
        
        assert!(result.is_ok());
        let transaction = result.unwrap();
        assert_eq!(transaction.payee, "Grocery Store");
        assert_eq!(transaction.code, Some("CHECK123".to_string()));
        assert_eq!(transaction.status, TransactionStatus::Cleared);
        assert_eq!(transaction.postings.len(), 2);
    }

    #[test]
    fn test_transaction_builder_auto_balance() {
        let date = NaiveDate::from_ymd_opt(2024, 1, 15).unwrap();
        let (assets, expenses) = create_test_accounts();
        
        let result = Transaction::builder(date, "Auto Balance Test".to_string())
            .post_to(assets, Amount::with_commodity(Decimal::from(-75), "USD".to_string()))
            .post_to_account(expenses) // No amount - should be calculated
            .build();
        
        assert!(result.is_ok());
        let transaction = result.unwrap();
        assert_eq!(transaction.postings.len(), 2);
        
        // Second posting should have calculated amount
        assert!(transaction.postings[1].is_calculated());
        if let Some(ref amount) = transaction.postings[1].amount {
            assert_eq!(amount.value(), Decimal::from(75));
        }
    }

    #[test]
    fn test_transaction_balance_validation() {
        let date = NaiveDate::from_ymd_opt(2024, 1, 15).unwrap();
        let (assets, expenses) = create_test_accounts();
        
        // Unbalanced transaction should fail
        let result = Transaction::builder(date, "Unbalanced".to_string())
            .post_to(assets, Amount::with_commodity(Decimal::from(-50), "USD".to_string()))
            .post_to(expenses, Amount::with_commodity(Decimal::from(60), "USD".to_string())) // Wrong amount
            .build();
        
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("does not balance"));
    }

    #[test]
    fn test_transaction_virtual_postings() {
        let date = NaiveDate::from_ymd_opt(2024, 1, 15).unwrap();
        let (assets, expenses) = create_test_accounts();
        let virtual_account = create_test_accounts().0; // Use as virtual
        
        let result = Transaction::builder(date, "Virtual Test".to_string())
            .post_to(assets, Amount::with_commodity(Decimal::from(-100), "USD".to_string()))
            .post_to(expenses, Amount::with_commodity(Decimal::from(100), "USD".to_string()))
            .virtual_post_to(virtual_account, Amount::with_commodity(Decimal::from(25), "USD".to_string()))
            .build();
        
        assert!(result.is_ok());
        let transaction = result.unwrap();
        assert!(transaction.has_virtual_postings());
        assert_eq!(transaction.virtual_postings().len(), 1);
        assert_eq!(transaction.balancing_postings().len(), 2);
    }

    #[test]
    fn test_transaction_metadata() {
        let date = NaiveDate::from_ymd_opt(2024, 1, 15).unwrap();
        let (assets, expenses) = create_test_accounts();
        
        let result = Transaction::builder(date, "Metadata Test".to_string())
            .tag("category", Some("groceries"))
            .tag("project", Some("household"))
            .tag("important", None::<String>)
            .post_to(assets, Amount::with_commodity(Decimal::from(-50), "USD".to_string()))
            .post_to(expenses, Amount::with_commodity(Decimal::from(50), "USD".to_string()))
            .build();
        
        assert!(result.is_ok());
        let transaction = result.unwrap();
        assert!(transaction.has_tag("category"));
        assert!(transaction.has_tag("project"));
        assert!(transaction.has_tag("important"));
        
        let category = transaction.get_tag("category").unwrap();
        assert_eq!(category.value, Some("groceries".to_string()));
    }

    #[test]
    fn test_transaction_verify_balance() {
        let (assets, expenses) = create_test_accounts();
        let date = NaiveDate::from_ymd_opt(2024, 1, 15).unwrap();
        
        let mut transaction = Transaction::new(date, "Balance Test".to_string());
        transaction.add_posting(Posting::with_amount(
            assets,
            Amount::with_commodity(Decimal::from(-100), "USD".to_string())
        ));
        transaction.add_posting(Posting::with_amount(
            expenses,
            Amount::with_commodity(Decimal::from(100), "USD".to_string())
        ));
        
        assert!(transaction.verify_balance().is_ok());
        
        // Add unbalancing posting
        transaction.add_posting(Posting::with_amount(
            create_test_accounts().0,
            Amount::with_commodity(Decimal::from(10), "USD".to_string())
        ));
        
        assert!(transaction.verify_balance().is_err());
    }

    #[test]
    fn test_transaction_multi_commodity() {
        let (assets_usd, assets_eur) = create_test_accounts();
        let (expenses, _) = create_test_accounts();
        let date = NaiveDate::from_ymd_opt(2024, 1, 15).unwrap();
        
        let result = Transaction::builder(date, "Multi-Currency".to_string())
            .post_to(assets_usd, Amount::with_commodity(Decimal::from(-100), "USD".to_string()))
            .post_to(expenses, Amount::with_commodity(Decimal::from(100), "USD".to_string()))
            .post_to(assets_eur, Amount::with_commodity(Decimal::from(-50), "EUR".to_string()))
            .post_to(create_test_accounts().1, Amount::with_commodity(Decimal::from(50), "EUR".to_string()))
            .build();
        
        assert!(result.is_ok());
        let transaction = result.unwrap();
        
        let magnitude = transaction.magnitude();
        assert_eq!(magnitude.get("USD"), Some(&Decimal::from(200))); // 100 + 100
        assert_eq!(magnitude.get("EUR"), Some(&Decimal::from(100))); // 50 + 50
    }

    #[test]
    fn test_transaction_finalize() {
        let (assets, expenses) = create_test_accounts();
        let date = NaiveDate::from_ymd_opt(2024, 1, 15).unwrap();
        
        let mut transaction = Transaction::new(date, "Finalize Test".to_string());
        transaction.add_posting(Posting::with_amount(
            assets,
            Amount::with_commodity(Decimal::from(-80), "USD".to_string())
        ));
        transaction.add_posting(Posting::new(expenses)); // No amount
        
        assert!(transaction.finalize().is_ok());
        
        // Should have auto-calculated the missing amount
        if let Some(ref amount) = transaction.postings[1].amount {
            assert_eq!(amount.value(), Decimal::from(80));
        }
    }

    #[test]
    fn test_transaction_validation_failures() {
        let date = NaiveDate::from_ymd_opt(2024, 1, 15).unwrap();
        
        // Empty transaction should fail
        let mut empty = Transaction::new(date, "Empty".to_string());
        assert!(empty.finalize().is_err());
        
        // Single posting should fail
        let (assets, _) = create_test_accounts();
        let mut single = Transaction::new(date, "Single".to_string());
        single.add_posting(Posting::with_amount(
            assets,
            Amount::with_commodity(Decimal::from(100), "USD".to_string())
        ));
        assert!(single.finalize().is_err());
    }

    #[test]
    fn test_posting_flags() {
        let (account, _) = create_test_accounts();
        let mut posting = Posting::new(account);
        
        assert!(!posting.is_virtual());
        assert!(!posting.is_calculated());
        assert!(posting.must_balance());
        
        posting.add_flags(PostingFlags::VIRTUAL);
        assert!(posting.is_virtual());
        assert!(!posting.must_balance()); // Virtual postings don't balance by default
        
        posting.add_flags(PostingFlags::MUST_BALANCE);
        assert!(posting.must_balance()); // Now it must balance
        
        posting.add_flags(PostingFlags::CALCULATED);
        assert!(posting.is_calculated());
    }

    #[test]
    fn test_posting_metadata() {
        let (account, _) = create_test_accounts();
        let mut posting = Posting::new(account);
        
        posting.set_tag("receipt".to_string(), Some("12345".to_string()), false);
        posting.set_tag("category".to_string(), Some("food".to_string()), false);
        posting.set_tag("verified".to_string(), None, false);
        
        assert!(posting.has_tag("receipt", false));
        assert!(posting.has_tag("category", false));
        assert!(posting.has_tag("verified", false));
        assert!(!posting.has_tag("nonexistent", false));
        
        let receipt = posting.get_tag("receipt", false).unwrap();
        assert_eq!(receipt.value, Some("12345".to_string()));
    }

    #[test]
    fn test_posting_extended_data() {
        let (account, _) = create_test_accounts();
        let mut posting = Posting::new(account);
        
        assert!(!posting.has_xdata());
        
        posting.ensure_xdata();
        assert!(posting.has_xdata());
        
        posting.clear_xdata();
        assert!(!posting.has_xdata());
    }

    #[test]
    fn test_transaction_tag_parsing() {
        let date = NaiveDate::from_ymd_opt(2024, 1, 15).unwrap();
        let mut transaction = Transaction::new(date, "Tag Parse Test".to_string());
        
        transaction.parse_and_add_tags("category:food,project:home,urgent", false);
        
        assert!(transaction.has_tag("category"));
        assert!(transaction.has_tag("project"));
        assert!(transaction.has_tag("urgent"));
        
        let category = transaction.get_tag("category").unwrap();
        assert_eq!(category.value, Some("food".to_string()));
        
        let urgent = transaction.get_tag("urgent").unwrap();
        assert_eq!(urgent.value, None);
    }

    #[test]
    fn test_virtual_balance_tracking() {
        let date = NaiveDate::from_ymd_opt(2024, 1, 15).unwrap();
        let (assets, expenses) = create_test_accounts();
        let virtual_account = create_test_accounts().0;
        
        let result = Transaction::builder(date, "Virtual Balance Test".to_string())
            .post_to(assets, Amount::with_commodity(Decimal::from(-100), "USD".to_string()))
            .post_to(expenses, Amount::with_commodity(Decimal::from(100), "USD".to_string()))
            .virtual_post_to(virtual_account.clone(), Amount::with_commodity(Decimal::from(25), "USD".to_string()))
            .virtual_post_to(virtual_account, Amount::with_commodity(Decimal::from(-10), "USD".to_string()))
            .build();
        
        assert!(result.is_ok());
        let transaction = result.unwrap();
        
        let virtual_balance = transaction.virtual_balance();
        assert_eq!(virtual_balance.get("USD"), Some(&Decimal::from(15))); // 25 - 10
    }

    #[test]
    fn test_transaction_sorting_methods() {
        let date = NaiveDate::from_ymd_opt(2024, 1, 15).unwrap();
        let (assets, expenses) = create_test_accounts();
        let (income, _) = create_test_accounts();
        
        let result = Transaction::builder(date, "Sorting Test".to_string())
            .post_to(expenses.clone(), Amount::with_commodity(Decimal::from(50), "USD".to_string()))
            .post_to(assets.clone(), Amount::with_commodity(Decimal::from(-100), "USD".to_string()))
            .post_to(income, Amount::with_commodity(Decimal::from(50), "USD".to_string()))
            .build();
        
        assert!(result.is_ok());
        let transaction = result.unwrap();
        
        let sorted_by_account = transaction.postings_sorted_by_account();
        assert_eq!(sorted_by_account.len(), 3);
        
        let sorted_by_amount = transaction.postings_sorted_by_amount();
        assert_eq!(sorted_by_amount.len(), 3);
        // Should be sorted in descending order by absolute value
    }

    #[test]
    fn test_transaction_cloning_methods() {
        let date = NaiveDate::from_ymd_opt(2024, 1, 15).unwrap();
        let (assets, expenses) = create_test_accounts();
        let virtual_account = create_test_accounts().0;
        
        let result = Transaction::builder(date, "Clone Test".to_string())
            .post_to(assets, Amount::with_commodity(Decimal::from(-100), "USD".to_string()))
            .post_to(expenses, Amount::with_commodity(Decimal::from(100), "USD".to_string()))
            .virtual_post_to(virtual_account, Amount::with_commodity(Decimal::from(25), "USD".to_string()))
            .build();
        
        assert!(result.is_ok());
        let transaction = result.unwrap();
        
        let virtual_only = transaction.virtual_only_clone();
        assert_eq!(virtual_only.postings.len(), 1);
        assert!(virtual_only.postings[0].is_virtual());
        
        let balancing_only = transaction.balancing_only_clone();
        assert_eq!(balancing_only.postings.len(), 2);
        assert!(!balancing_only.postings[0].is_virtual());
        assert!(!balancing_only.postings[1].is_virtual());
    }

    #[test]
    fn test_builder_validation_methods() {
        let date = NaiveDate::from_ymd_opt(2024, 1, 15).unwrap();
        let (assets, expenses) = create_test_accounts();
        
        let mut builder = Transaction::builder(date, "Validation Test".to_string())
            .post_to(assets, Amount::with_commodity(Decimal::from(-100), "USD".to_string()))
            .post_to(expenses, Amount::with_commodity(Decimal::from(100), "USD".to_string()));
        
        assert!(builder.is_balanced());
        assert_eq!(builder.posting_count(), 2);
        assert!(builder.validate().is_ok());
        
        // Add unbalancing posting
        builder = builder.post_to(create_test_accounts().0, 
            Amount::with_commodity(Decimal::from(10), "USD".to_string()));
        
        assert!(!builder.is_balanced());
        assert!(builder.validate().is_err());
    }

    #[test]
    fn test_complex_transaction_scenario() {
        let date = NaiveDate::from_ymd_opt(2024, 1, 15).unwrap();
        let (checking, _) = create_test_accounts();
        let (food, _) = create_test_accounts();
        let (tax, _) = create_test_accounts();
        let (budget_virtual, _) = create_test_accounts();
        
        let result = Transaction::builder(date, "Restaurant with tax and budget tracking".to_string())
            .code("VISA4567")
            .status(TransactionStatus::Pending)
            .tag("category", Some("dining"))
            .tag("receipt", Some("rest_001"))
            .aux_date(NaiveDate::from_ymd_opt(2024, 1, 16).unwrap())
            .note("Dinner with clients")
            .post_to(checking, Amount::with_commodity(Decimal::from(-5425), "USD".to_string())) // $54.25
            .post_to(food, Amount::with_commodity(Decimal::from(5000), "USD".to_string()))  // $50.00
            .post_to(tax, Amount::with_commodity(Decimal::from(425), "USD".to_string()))   // $4.25 tax
            .virtual_post_to(budget_virtual.clone(), Amount::with_commodity(Decimal::from(-5425), "USD".to_string()))
            .virtual_post_to(budget_virtual, Amount::with_commodity(Decimal::from(5425), "USD".to_string()))
            .build();
        
        assert!(result.is_ok());
        let transaction = result.unwrap();
        
        // Verify transaction properties
        assert_eq!(transaction.code, Some("VISA4567".to_string()));
        assert_eq!(transaction.status, TransactionStatus::Pending);
        assert_eq!(transaction.aux_date, Some(NaiveDate::from_ymd_opt(2024, 1, 16).unwrap()));
        assert!(transaction.has_tag("category"));
        assert!(transaction.has_virtual_postings());
        
        // Verify balances
        assert!(transaction.verify_balance().is_ok());
        let virtual_balance = transaction.virtual_balance();
        assert_eq!(virtual_balance.get("USD"), Some(&Decimal::from(0))); // Virtual postings balance
        
        // Verify postings
        assert_eq!(transaction.postings.len(), 5);
        assert_eq!(transaction.balancing_postings().len(), 3);
        assert_eq!(transaction.virtual_postings().len(), 2);
    }
}
