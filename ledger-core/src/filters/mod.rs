//! Filter chain architecture for transaction processing
//!
//! This module provides composable filters for processing transactions
//! and postings, following the chain of responsibility pattern from the
//! C++ implementation.

use crate::posting::Posting;
use crate::transaction::{Transaction, TransactionStatus};
use crate::amount::Amount;
use chrono::NaiveDate;
use regex::Regex;
use std::error::Error;
use std::fmt;

/// Result type for filter operations
pub type FilterResult<T> = Result<T, FilterError>;

/// Errors that can occur during filtering
#[derive(Debug, Clone)]
pub enum FilterError {
    /// Invalid filter configuration
    InvalidConfig(String),
    /// Regular expression compilation error
    RegexError(String),
    /// Date parsing error
    DateError(String),
    /// Amount parsing or comparison error
    AmountError(String),
}

impl fmt::Display for FilterError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FilterError::InvalidConfig(msg) => write!(f, "Invalid filter configuration: {}", msg),
            FilterError::RegexError(msg) => write!(f, "Regex error: {}", msg),
            FilterError::DateError(msg) => write!(f, "Date error: {}", msg),
            FilterError::AmountError(msg) => write!(f, "Amount error: {}", msg),
        }
    }
}

impl Error for FilterError {}

/// Core filter trait for transaction processing
pub trait Filter<T>: Send + Sync {
    /// Apply filter to an item, returning true if it should be included
    fn matches(&self, item: &T) -> bool;
    
    /// Get a description of this filter for debugging
    fn description(&self) -> String;
    
    /// Reset any internal state (useful for stateful filters)
    fn reset(&mut self) {}
}

/// Filter trait for postings
pub trait PostingFilter: Filter<Posting> {}

/// Filter trait for transactions
pub trait TransactionFilter: Filter<Transaction> {}

/// Composite filter chain that applies multiple filters
#[derive(Debug)]
pub struct FilterChain<T> {
    filters: Vec<Box<dyn Filter<T>>>,
}

impl<T> FilterChain<T> {
    /// Create a new empty filter chain
    pub fn new() -> Self {
        Self {
            filters: Vec::new(),
        }
    }
    
    /// Add a filter to the chain
    pub fn add_filter(mut self, filter: Box<dyn Filter<T>>) -> Self {
        self.filters.push(filter);
        self
    }
    
    /// Check if an item passes all filters in the chain
    pub fn matches(&self, item: &T) -> bool {
        self.filters.iter().all(|filter| filter.matches(item))
    }
    
    /// Get descriptions of all filters
    pub fn descriptions(&self) -> Vec<String> {
        self.filters.iter().map(|f| f.description()).collect()
    }
    
    /// Reset all filters in the chain
    pub fn reset(&mut self) {
        for filter in &mut self.filters {
            filter.reset();
        }
    }
    
    /// Get the number of filters in the chain
    pub fn len(&self) -> usize {
        self.filters.len()
    }
    
    /// Check if the chain is empty
    pub fn is_empty(&self) -> bool {
        self.filters.is_empty()
    }
}

impl<T> Default for FilterChain<T> {
    fn default() -> Self {
        Self::new()
    }
}

/// Date range filter for postings and transactions
#[derive(Debug, Clone)]
pub struct DateFilter {
    start_date: Option<NaiveDate>,
    end_date: Option<NaiveDate>,
}

impl DateFilter {
    /// Create a new date filter
    pub fn new(start_date: Option<NaiveDate>, end_date: Option<NaiveDate>) -> Self {
        Self {
            start_date,
            end_date,
        }
    }
    
    /// Create filter for dates after the given date
    pub fn after(date: NaiveDate) -> Self {
        Self::new(Some(date), None)
    }
    
    /// Create filter for dates before the given date
    pub fn before(date: NaiveDate) -> Self {
        Self::new(None, Some(date))
    }
    
    /// Create filter for dates in the given range (inclusive)
    pub fn between(start: NaiveDate, end: NaiveDate) -> Self {
        Self::new(Some(start), Some(end))
    }
    
    fn date_matches(&self, date: NaiveDate) -> bool {
        if let Some(start) = self.start_date {
            if date < start {
                return false;
            }
        }
        
        if let Some(end) = self.end_date {
            if date > end {
                return false;
            }
        }
        
        true
    }
}

impl Filter<Transaction> for DateFilter {
    fn matches(&self, transaction: &Transaction) -> bool {
        self.date_matches(transaction.date)
    }
    
    fn description(&self) -> String {
        match (self.start_date, self.end_date) {
            (Some(start), Some(end)) => format!("date between {} and {}", start, end),
            (Some(start), None) => format!("date >= {}", start),
            (None, Some(end)) => format!("date <= {}", end),
            (None, None) => "no date filter".to_string(),
        }
    }
}

impl Filter<Posting> for DateFilter {
    fn matches(&self, posting: &Posting) -> bool {
        // For postings, we'd typically use the transaction date
        // This would need to be provided via context in a real implementation
        // For now, we'll check if there's extended data with a date
        if let Some(ref xdata) = posting.xdata {
            if let Some(date) = xdata.date {
                return self.date_matches(date);
            }
        }
        
        // If no date in extended data, we can't filter by date at posting level
        // This would typically be handled by transaction-level filtering
        true
    }
    
    fn description(&self) -> String {
        format!("posting {}", Filter::<Transaction>::description(self))
    }
}

impl TransactionFilter for DateFilter {}
impl PostingFilter for DateFilter {}

/// Account name filter using regex patterns
#[derive(Debug)]
pub struct AccountFilter {
    pattern: Regex,
    original_pattern: String,
}

impl AccountFilter {
    /// Create a new account filter with a regex pattern
    pub fn new(pattern: &str) -> FilterResult<Self> {
        let regex = Regex::new(pattern)
            .map_err(|e| FilterError::RegexError(e.to_string()))?;
        
        Ok(Self {
            pattern: regex,
            original_pattern: pattern.to_string(),
        })
    }
    
    /// Create a simple account filter that matches account names containing the given string
    pub fn contains(substring: &str) -> FilterResult<Self> {
        let pattern = regex::escape(substring);
        Self::new(&pattern)
    }
    
    /// Create an account filter that matches accounts starting with the given prefix
    pub fn starts_with(prefix: &str) -> FilterResult<Self> {
        let pattern = format!("^{}", regex::escape(prefix));
        Self::new(&pattern)
    }
}

impl Filter<Posting> for AccountFilter {
    fn matches(&self, posting: &Posting) -> bool {
        if let Some(account) = posting.account.upgrade() {
            let account_name = account.borrow().full_name();
            self.pattern.is_match(&account_name)
        } else {
            false
        }
    }
    
    fn description(&self) -> String {
        format!("account matches '{}'", self.original_pattern)
    }
}

impl PostingFilter for AccountFilter {}

/// Status filter for cleared/pending/uncleared items
#[derive(Debug, Clone)]
pub struct StatusFilter {
    allowed_statuses: Vec<TransactionStatus>,
}

impl StatusFilter {
    /// Create a new status filter
    pub fn new(statuses: Vec<TransactionStatus>) -> Self {
        Self {
            allowed_statuses: statuses,
        }
    }
    
    /// Create filter for cleared items only
    pub fn cleared_only() -> Self {
        Self::new(vec![TransactionStatus::Cleared])
    }
    
    /// Create filter for pending items only
    pub fn pending_only() -> Self {
        Self::new(vec![TransactionStatus::Pending])
    }
    
    /// Create filter for uncleared items only
    pub fn uncleared_only() -> Self {
        Self::new(vec![TransactionStatus::Uncleared])
    }
    
    /// Create filter excluding cleared items
    pub fn exclude_cleared() -> Self {
        Self::new(vec![TransactionStatus::Pending, TransactionStatus::Uncleared])
    }
}

impl Filter<Transaction> for StatusFilter {
    fn matches(&self, transaction: &Transaction) -> bool {
        self.allowed_statuses.contains(&transaction.status)
    }
    
    fn description(&self) -> String {
        let statuses: Vec<String> = self.allowed_statuses.iter()
            .map(|s| format!("{:?}", s))
            .collect();
        format!("status in [{}]", statuses.join(", "))
    }
}

impl TransactionFilter for StatusFilter {}

/// Amount filter for value comparisons
#[derive(Debug, Clone)]
pub struct AmountFilter {
    min_amount: Option<Amount>,
    max_amount: Option<Amount>,
}

impl AmountFilter {
    /// Create a new amount filter
    pub fn new(min_amount: Option<Amount>, max_amount: Option<Amount>) -> Self {
        Self {
            min_amount,
            max_amount,
        }
    }
    
    /// Create filter for amounts greater than the given value
    pub fn greater_than(amount: Amount) -> Self {
        Self::new(Some(amount), None)
    }
    
    /// Create filter for amounts less than the given value
    pub fn less_than(amount: Amount) -> Self {
        Self::new(None, Some(amount))
    }
    
    /// Create filter for amounts in the given range
    pub fn between(min: Amount, max: Amount) -> Self {
        Self::new(Some(min), Some(max))
    }
}

impl Filter<Posting> for AmountFilter {
    fn matches(&self, posting: &Posting) -> bool {
        if let Some(ref amount) = posting.amount {
            if let Some(ref min) = self.min_amount {
                if amount < min {
                    return false;
                }
            }
            
            if let Some(ref max) = self.max_amount {
                if amount > max {
                    return false;
                }
            }
            
            true
        } else {
            // No amount set - this might be valid during parsing
            // Default to including it unless we have specific requirements
            true
        }
    }
    
    fn description(&self) -> String {
        match (&self.min_amount, &self.max_amount) {
            (Some(min), Some(max)) => format!("amount between {} and {}", min, max),
            (Some(min), None) => format!("amount >= {}", min),
            (None, Some(max)) => format!("amount <= {}", max),
            (None, None) => "no amount filter".to_string(),
        }
    }
}

impl PostingFilter for AmountFilter {}

/// Payee filter using regex patterns
#[derive(Debug)]
pub struct PayeeFilter {
    pattern: Regex,
    original_pattern: String,
}

impl PayeeFilter {
    /// Create a new payee filter with a regex pattern
    pub fn new(pattern: &str) -> FilterResult<Self> {
        let regex = Regex::new(pattern)
            .map_err(|e| FilterError::RegexError(e.to_string()))?;
        
        Ok(Self {
            pattern: regex,
            original_pattern: pattern.to_string(),
        })
    }
    
    /// Create a simple payee filter that matches payees containing the given string
    pub fn contains(substring: &str) -> FilterResult<Self> {
        let pattern = regex::escape(substring);
        Self::new(&pattern)
    }
}

impl Filter<Transaction> for PayeeFilter {
    fn matches(&self, transaction: &Transaction) -> bool {
        self.pattern.is_match(&transaction.payee)
    }
    
    fn description(&self) -> String {
        format!("payee matches '{}'", self.original_pattern)
    }
}

impl Filter<Posting> for PayeeFilter {
    fn matches(&self, posting: &Posting) -> bool {
        if let Some(ref payee) = posting.payee {
            self.pattern.is_match(payee)
        } else {
            // If posting doesn't override payee, it would use transaction payee
            // In a full implementation, we'd need access to the transaction
            false
        }
    }
    
    fn description(&self) -> String {
        format!("posting payee matches '{}'", self.original_pattern)
    }
}

impl TransactionFilter for PayeeFilter {}
impl PostingFilter for PayeeFilter {}

/// Note/comment filter using regex patterns
#[derive(Debug)]
pub struct NoteFilter {
    pattern: Regex,
    original_pattern: String,
}

impl NoteFilter {
    /// Create a new note filter with a regex pattern
    pub fn new(pattern: &str) -> FilterResult<Self> {
        let regex = Regex::new(pattern)
            .map_err(|e| FilterError::RegexError(e.to_string()))?;
        
        Ok(Self {
            pattern: regex,
            original_pattern: pattern.to_string(),
        })
    }
    
    /// Create a simple note filter that matches notes containing the given string
    pub fn contains(substring: &str) -> FilterResult<Self> {
        let pattern = regex::escape(substring);
        Self::new(&pattern)
    }
}

impl Filter<Transaction> for NoteFilter {
    fn matches(&self, transaction: &Transaction) -> bool {
        transaction.note.as_ref()
            .map(|note| self.pattern.is_match(note))
            .unwrap_or(false)
    }
    
    fn description(&self) -> String {
        format!("note matches '{}'", self.original_pattern)
    }
}

impl Filter<Posting> for NoteFilter {
    fn matches(&self, posting: &Posting) -> bool {
        posting.note.as_ref()
            .map(|note| self.pattern.is_match(note))
            .unwrap_or(false)
    }
    
    fn description(&self) -> String {
        format!("posting note matches '{}'", self.original_pattern)
    }
}

impl TransactionFilter for NoteFilter {}
impl PostingFilter for NoteFilter {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::account::Account;
    use chrono::NaiveDate;
    use std::rc::Rc;
    use std::cell::RefCell;
    
    fn create_test_transaction() -> Transaction {
        Transaction {
            date: NaiveDate::from_ymd_opt(2024, 1, 15).unwrap(),
            aux_date: None,
            status: TransactionStatus::Cleared,
            code: Some("123".to_string()),
            payee: "Test Payee".to_string(),
            note: Some("Test note".to_string()),
            postings: vec![],
            flags: Default::default(),
            transaction_type: crate::transaction::TransactionType::Normal,
            pos: None,
            metadata: Default::default(),
            sequence: 1,
        }
    }
    
    #[test]
    fn test_date_filter() {
        let transaction = create_test_transaction();
        
        // Test date range
        let filter = DateFilter::between(
            NaiveDate::from_ymd_opt(2024, 1, 1).unwrap(),
            NaiveDate::from_ymd_opt(2024, 1, 31).unwrap()
        );
        assert!(filter.matches(&transaction));
        
        // Test date before range
        let filter = DateFilter::before(NaiveDate::from_ymd_opt(2024, 1, 1).unwrap());
        assert!(!filter.matches(&transaction));
        
        // Test date after
        let filter = DateFilter::after(NaiveDate::from_ymd_opt(2024, 1, 10).unwrap());
        assert!(filter.matches(&transaction));
    }
    
    #[test]
    fn test_status_filter() {
        let transaction = create_test_transaction();
        
        let filter = StatusFilter::cleared_only();
        assert!(filter.matches(&transaction));
        
        let filter = StatusFilter::pending_only();
        assert!(!filter.matches(&transaction));
    }
    
    #[test]
    fn test_payee_filter() {
        let transaction = create_test_transaction();
        
        let filter = PayeeFilter::contains("Test").unwrap();
        assert!(filter.matches(&transaction));
        
        let filter = PayeeFilter::contains("Missing").unwrap();
        assert!(!filter.matches(&transaction));
    }
    
    #[test]
    fn test_filter_chain() {
        let transaction = create_test_transaction();
        
        let mut chain = FilterChain::new();
        chain = chain.add_filter(Box::new(StatusFilter::cleared_only()));
        chain = chain.add_filter(Box::new(PayeeFilter::contains("Test").unwrap()));
        
        assert!(chain.matches(&transaction));
        assert_eq!(chain.len(), 2);
        assert!(!chain.is_empty());
    }
}
