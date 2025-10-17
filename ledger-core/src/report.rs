//! Reporting framework for generating financial reports
//!
//! This module provides the core reporting infrastructure for generating
//! balance reports, register reports, and other financial summaries.

use crate::account::AccountRef;
use crate::cache::{CacheKey, CacheManager};
use crate::filters::{FilterChain, PostingFilter, TransactionFilter};
use crate::journal::Journal;
use crate::posting::Posting;
use crate::transaction::Transaction;
use ledger_math::balance::Balance;
use std::collections::{BTreeMap, HashMap};
use std::fmt;
use std::io::Write;

/// Errors that can occur during report generation
#[derive(Debug, Clone)]
pub enum ReportError {
    /// IO error during output generation
    IoError(String),
    /// Filter error during data processing
    FilterError(String),
    /// Invalid report configuration
    InvalidConfig(String),
    /// Account lookup error
    AccountError(String),
}

impl fmt::Display for ReportError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ReportError::IoError(msg) => write!(f, "IO error: {}", msg),
            ReportError::FilterError(msg) => write!(f, "Filter error: {}", msg),
            ReportError::InvalidConfig(msg) => write!(f, "Invalid configuration: {}", msg),
            ReportError::AccountError(msg) => write!(f, "Account error: {}", msg),
        }
    }
}

impl std::error::Error for ReportError {}

/// Result type for report operations
pub type ReportResult<T> = Result<T, ReportError>;

/// Report display options and formatting configuration
#[derive(Debug, Clone)]
pub struct ReportOptions {
    /// Display accounts in flat format (no hierarchy)
    pub flat: bool,
    /// Maximum depth to display (None for unlimited)
    pub depth: Option<usize>,
    /// Show accounts with zero balances
    pub empty: bool,
    /// Sort criteria
    pub sort_by: SortCriteria,
    /// Collapse accounts with only one posting
    pub collapse: bool,
    /// Show running totals
    pub running_total: bool,
    /// Column width for account names
    pub account_width: Option<usize>,
    /// Column width for amounts
    pub amount_width: Option<usize>,
    /// Show account codes
    pub show_codes: bool,
    /// Currency to display (None for all)
    pub currency: Option<String>,
}

impl Default for ReportOptions {
    fn default() -> Self {
        Self {
            flat: false,
            depth: None,
            empty: false,
            sort_by: SortCriteria::Name,
            collapse: false,
            running_total: false,
            account_width: None,
            amount_width: None,
            show_codes: false,
            currency: None,
        }
    }
}

/// Criteria for sorting report items
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SortCriteria {
    /// Sort by account name
    Name,
    /// Sort by balance amount (absolute value)
    Amount,
    /// Sort by transaction count
    Count,
    /// Sort by account code
    Code,
}

/// Base trait for report generators
pub trait ReportGenerator {
    /// Generate the report with the given options
    fn generate<W: Write>(&mut self, writer: &mut W, options: &ReportOptions) -> ReportResult<()>;

    /// Get a description of this report type
    fn description(&self) -> &str;

    /// Get the name/type of this report
    fn report_type(&self) -> &str;
}

/// Account balance information for reporting
#[derive(Debug, Clone)]
pub struct AccountBalance {
    /// Reference to the account
    pub account: AccountRef,
    /// Current balance
    pub balance: Balance,
    /// Number of postings that contributed to this balance
    pub count: usize,
    /// Depth in the account hierarchy
    pub depth: usize,
    /// Whether this account has children
    pub has_children: bool,
    /// Total balance including all descendants
    pub total_balance: Balance,
}

/// Balance report generator
pub struct BalanceReport {
    /// Journal to generate report from
    pub journal: Journal,
    /// Posting filter chain
    pub posting_filters: FilterChain<Posting>,
    /// Transaction filter chain
    pub transaction_filters: FilterChain<Transaction>,
    /// Cache manager for performance optimization
    pub cache_manager: Option<CacheManager>,
}

impl BalanceReport {
    /// Create a new balance report
    pub fn new(journal: Journal) -> Self {
        Self {
            journal,
            posting_filters: FilterChain::new(),
            transaction_filters: FilterChain::new(),
            cache_manager: None,
        }
    }

    /// Enable caching for this report
    pub fn with_cache(mut self, cache_manager: CacheManager) -> Self {
        self.cache_manager = Some(cache_manager);
        self
    }

    /// Add a posting filter to the report
    pub fn with_posting_filter(mut self, filter: Box<dyn PostingFilter>) -> Self {
        self.posting_filters = self.posting_filters.add_filter(filter);
        self
    }

    /// Add a transaction filter to the report
    pub fn with_transaction_filter(mut self, filter: Box<dyn TransactionFilter>) -> Self {
        self.transaction_filters = self.transaction_filters.add_filter(filter);
        self
    }

    /// Collect and aggregate account balances
    fn collect_balances(
        &mut self,
        options: &ReportOptions,
    ) -> ReportResult<BTreeMap<String, AccountBalance>> {
        // Try cache first if available
        let journal_hash = self.compute_journal_hash();
        if let Some(ref mut cache_manager) = self.cache_manager {
            cache_manager.update_journal_hash(journal_hash);

            let cache_key = CacheKey::from_report_options("balance", options, journal_hash);
            if let Ok(cached_data) = cache_manager.get_balance_data(&cache_key) {
                // Convert cached data to AccountBalance format
                return self.convert_cached_balances(cached_data);
            }
        }

        // Cache miss or no cache - compute fresh
        let mut account_balances: HashMap<String, Balance> = HashMap::new();
        let mut account_counts: HashMap<String, usize> = HashMap::new();

        // Process all transactions
        for transaction in &self.journal.transactions {
            // Apply transaction-level filters
            if !self.transaction_filters.matches(transaction) {
                continue;
            }

            // Process all postings in this transaction
            for posting in &transaction.postings {
                // Apply posting-level filters
                if !self.posting_filters.matches(posting) {
                    continue;
                }

                // Get account name
                let account_name = posting.account.borrow().fullname_immutable();

                // Get posting amount
                if let Some(ref amount) = posting.amount {
                    // Add to account balance
                    let balance = account_balances.entry(account_name.to_string()).or_default();
                    balance.add_amount(amount).map_err(|e| {
                        ReportError::InvalidConfig(format!("Failed to add amount: {}", e))
                    })?;

                    // Update count
                    *account_counts.entry(account_name.to_string()).or_insert(0) += 1;

                    // Also update parent accounts if not flat display
                    if !options.flat {
                        let mut parent_path = String::new();
                        for segment in account_name.split(':') {
                            if !parent_path.is_empty() {
                                parent_path.push(':');
                            }
                            parent_path.push_str(segment);

                            if parent_path != account_name {
                                let parent_balance =
                                    account_balances.entry(parent_path.clone()).or_default();
                                parent_balance.add_amount(amount).map_err(|e| {
                                    ReportError::InvalidConfig(format!(
                                        "Failed to add parent amount: {}",
                                        e
                                    ))
                                })?;
                                *account_counts.entry(parent_path.clone()).or_insert(0) += 1;
                            }
                        }
                    }
                }
            }
        }

        // Convert to AccountBalance structures
        let mut result = BTreeMap::new();
        for (account_name, balance) in &account_balances {
            // Skip zero balances if not showing empty accounts
            if !options.empty && balance.is_zero() {
                continue;
            }

            // Apply depth filtering
            let depth = account_name.matches(':').count();
            if let Some(max_depth) = options.depth {
                if depth >= max_depth {
                    continue;
                }
            }

            // Try to get account reference
            if let Ok(account_ref) = self.journal.find_account(account_name) {
                let has_children = account_name.contains(':')
                    || account_balances
                        .keys()
                        .any(|k| k.starts_with(&format!("{}:", account_name)));

                let account_balance = AccountBalance {
                    account: account_ref,
                    balance: balance.clone(),
                    count: *account_counts.get(account_name).unwrap_or(&0),
                    depth,
                    has_children,
                    total_balance: balance.clone(), // For now, same as balance - would need recursive calculation
                };

                result.insert(account_name.clone(), account_balance);
            }
        }

        // Cache the result if caching is enabled
        let journal_hash = self.compute_journal_hash();
        if let Some(ref mut cache_manager) = self.cache_manager {
            let cache_key = CacheKey::from_report_options("balance", options, journal_hash);
            let cached_data: HashMap<String, Balance> = account_balances.clone();
            let _ = cache_manager.cache_balance_data(cache_key, cached_data);
        }

        Ok(result)
    }

    /// Compute a hash representing the current journal state
    fn compute_journal_hash(&self) -> u64 {
        let transaction_count = self.journal.transactions.len();
        let total_postings: usize =
            self.journal.transactions.iter().map(|tx| tx.postings.len()).sum();
        CacheManager::compute_journal_hash(transaction_count, total_postings)
    }

    /// Convert cached balance data to AccountBalance format
    fn convert_cached_balances(
        &self,
        cached_data: HashMap<String, Balance>,
    ) -> ReportResult<BTreeMap<String, AccountBalance>> {
        let mut result = BTreeMap::new();

        for (account_name, balance) in &cached_data {
            if let Ok(account_ref) = self.journal.find_account(account_name) {
                let depth = account_name.matches(':').count();
                let has_children = account_name.contains(':')
                    || cached_data.keys().any(|k| k.starts_with(&format!("{}:", account_name)));

                let account_balance = AccountBalance {
                    account: account_ref,
                    balance: balance.clone(),
                    count: 0, // We don't cache count information
                    depth,
                    has_children,
                    total_balance: balance.clone(),
                };

                result.insert(account_name.clone(), account_balance);
            }
        }

        Ok(result)
    }

    /// Sort account balances according to criteria
    fn sort_balances(
        &self,
        balances: BTreeMap<String, AccountBalance>,
        criteria: SortCriteria,
    ) -> Vec<(String, AccountBalance)> {
        let mut sorted: Vec<_> = balances.into_iter().collect();

        match criteria {
            SortCriteria::Name => {
                // BTreeMap already sorts by name
            }
            SortCriteria::Amount => {
                sorted.sort_by(|a, b| {
                    let a_total = a.1.balance.total_value().abs();
                    let b_total = b.1.balance.total_value().abs();
                    b_total.partial_cmp(&a_total).unwrap_or(std::cmp::Ordering::Equal)
                });
            }
            SortCriteria::Count => {
                sorted.sort_by(|a, b| b.1.count.cmp(&a.1.count));
            }
            SortCriteria::Code => {
                // Would need account codes in the data structure
                // For now, fall back to name sorting
            }
        }

        sorted
    }

    /// Format account name with proper indentation
    fn format_account_name(
        &self,
        account_balance: &AccountBalance,
        options: &ReportOptions,
    ) -> String {
        let account = account_balance.account.clone();
        let account_ref = account.borrow();
        let name = if options.flat {
            account_ref.fullname_immutable()
        } else {
            // Show only the last segment with proper indentation
            let full_name = account_ref.fullname_immutable();
            let segments: Vec<&str> = full_name.split(':').collect();
            let indent = "  ".repeat(account_balance.depth);
            let last_segment = segments.last().copied().unwrap_or(&full_name);
            format!("{}{}", indent, last_segment)
        };

        if let Some(width) = options.account_width {
            format!("{:<width$}", name, width = width)
        } else {
            name
        }
    }

    /// Format balance amount with proper alignment
    fn format_amount(&self, balance: &Balance, options: &ReportOptions) -> String {
        let formatted = if let Some(ref _currency) = options.currency {
            // Filter by specific currency
            balance.to_string() // FIXME: Would need currency filtering in Balance
        } else {
            balance.to_string()
        };

        if let Some(width) = options.amount_width {
            format!("{:>width$}", formatted, width = width)
        } else {
            formatted
        }
    }
}

impl ReportGenerator for BalanceReport {
    fn generate<W: Write>(&mut self, writer: &mut W, options: &ReportOptions) -> ReportResult<()> {
        // Collect account balances
        let balances = self.collect_balances(options)?;

        // Sort according to criteria
        let sorted_balances = self.sort_balances(balances, options.sort_by);

        // Write header if needed
        if options.show_codes {
            writeln!(writer, "{:<20} {:>15}", "Account", "Balance")
                .map_err(|e| ReportError::IoError(e.to_string()))?;
            writeln!(writer, "{}", "-".repeat(37))
                .map_err(|e| ReportError::IoError(e.to_string()))?;
        }

        // Write each account balance
        for (_account_name, account_balance) in sorted_balances {
            let account_name = self.format_account_name(&account_balance, options);
            let amount_str = self.format_amount(&account_balance.balance, options);

            writeln!(writer, "{} {}", account_name, amount_str)
                .map_err(|e| ReportError::IoError(e.to_string()))?;
        }

        Ok(())
    }

    fn description(&self) -> &str {
        "Balance report showing account balances with optional hierarchy and grouping"
    }

    fn report_type(&self) -> &str {
        "balance"
    }
}

/// Register entry representing a single line in a register report
#[derive(Debug, Clone)]
pub struct RegisterEntry {
    /// Transaction date
    pub date: chrono::NaiveDate,
    /// Transaction payee
    pub payee: String,
    /// Account name for this posting
    pub account: String,
    /// Amount for this posting
    pub amount: ledger_math::amount::Amount,
    /// Running balance after this posting
    pub balance: crate::balance::Balance,
    /// Transaction code (if any)
    pub code: Option<String>,
    /// Note for this posting or transaction
    pub note: Option<String>,
    /// Whether this is a split transaction entry
    pub is_split: bool,
    /// Related accounts (for split transactions)
    pub related_accounts: Vec<String>,
}

/// Configuration for register report columns
#[derive(Debug, Clone)]
pub struct RegisterColumns {
    /// Show date column
    pub date: bool,
    /// Show transaction code column
    pub code: bool,
    /// Show payee column
    pub payee: bool,
    /// Show account column
    pub account: bool,
    /// Show amount column
    pub amount: bool,
    /// Show running balance column
    pub balance: bool,
    /// Show note column
    pub note: bool,
    /// Column widths (None for auto-width)
    pub widths: HashMap<String, usize>,
}

impl Default for RegisterColumns {
    fn default() -> Self {
        Self {
            date: true,
            code: false,
            payee: true,
            account: true,
            amount: true,
            balance: true,
            note: false,
            widths: HashMap::new(),
        }
    }
}

/// Register report generator showing transaction details with running balances
pub struct RegisterReport {
    /// Journal to generate report from
    pub journal: Journal,
    /// Posting filter chain
    pub posting_filters: FilterChain<Posting>,
    /// Transaction filter chain
    pub transaction_filters: FilterChain<Transaction>,
    /// Column configuration
    pub columns: RegisterColumns,
    /// Whether to show wide format
    pub wide_format: bool,
    /// Whether to show related accounts for splits
    pub show_related: bool,
    /// Cache manager for performance optimization
    pub cache_manager: Option<CacheManager>,
}

impl RegisterReport {
    /// Create a new register report
    pub fn new(journal: Journal) -> Self {
        Self {
            journal,
            posting_filters: FilterChain::new(),
            transaction_filters: FilterChain::new(),
            columns: RegisterColumns::default(),
            wide_format: false,
            show_related: true,
            cache_manager: None,
        }
    }

    /// Enable caching for this report
    pub fn with_cache(mut self, cache_manager: CacheManager) -> Self {
        self.cache_manager = Some(cache_manager);
        self
    }

    /// Add a posting filter to the report
    pub fn with_posting_filter(mut self, filter: Box<dyn PostingFilter>) -> Self {
        self.posting_filters = self.posting_filters.add_filter(filter);
        self
    }

    /// Add a transaction filter to the report
    pub fn with_transaction_filter(mut self, filter: Box<dyn TransactionFilter>) -> Self {
        self.transaction_filters = self.transaction_filters.add_filter(filter);
        self
    }

    /// Configure columns
    pub fn with_columns(mut self, columns: RegisterColumns) -> Self {
        self.columns = columns;
        self
    }

    /// Set wide format
    pub fn with_wide_format(mut self, wide: bool) -> Self {
        self.wide_format = wide;
        self
    }

    /// Set whether to show related accounts
    pub fn with_show_related(mut self, show_related: bool) -> Self {
        self.show_related = show_related;
        self
    }

    /// Collect register entries with running balances
    fn collect_entries(&mut self, options: &ReportOptions) -> ReportResult<Vec<RegisterEntry>> {
        let mut entries = Vec::new();
        let mut running_balance = crate::balance::Balance::new();

        // Process all transactions in chronological order
        let mut transactions: Vec<_> = self.journal.transactions.iter().collect();
        transactions.sort_by_key(|tx| tx.date);

        for transaction in transactions {
            // Apply transaction-level filters
            if !self.transaction_filters.matches(transaction) {
                continue;
            }

            // Collect matching postings for this transaction
            let mut matching_postings = Vec::new();
            for posting in &transaction.postings {
                if self.posting_filters.matches(posting) {
                    matching_postings.push(posting);
                }
            }

            if matching_postings.is_empty() {
                continue;
            }

            // Create entries for matching postings
            for posting in matching_postings {
                if let Some(ref amount) = posting.amount {
                    // Update running balance
                    running_balance.add_amount(amount).map_err(|e| {
                        ReportError::InvalidConfig(format!(
                            "Failed to update running balance: {}",
                            e
                        ))
                    })?;

                    // Get account name
                    let account_name = posting.account.borrow().fullname_immutable();

                    // Determine related accounts for split transactions
                    let related_accounts = if self.show_related && transaction.postings.len() > 2 {
                        transaction
                            .postings
                            .iter()
                            .filter(|p| !std::ptr::eq(*p, posting))
                            .filter_map(|p| Some(p.account.borrow().fullname_immutable()))
                            .collect()
                    } else {
                        Vec::new()
                    };

                    let entry = RegisterEntry {
                        date: transaction.date,
                        payee: posting
                            .payee
                            .as_ref()
                            .map(|p| p.to_string())
                            .unwrap_or_else(|| transaction.payee.clone()),
                        account: account_name.to_string(),
                        amount: amount.clone(),
                        balance: running_balance.clone(),
                        code: transaction.code.clone(),
                        note: posting
                            .note
                            .as_ref()
                            .map(|n| n.to_string())
                            .or_else(|| transaction.note.clone()),
                        is_split: transaction.postings.len() > 2,
                        related_accounts,
                    };

                    entries.push(entry);
                }
            }
        }

        // Apply sorting if specified
        match options.sort_by {
            SortCriteria::Amount => {
                entries.sort_by(|a, b| {
                    b.amount.abs().partial_cmp(&a.amount.abs()).unwrap_or(std::cmp::Ordering::Equal)
                });
            }
            SortCriteria::Name => {
                entries.sort_by(|a, b| a.account.cmp(&b.account));
            }
            SortCriteria::Count => {
                // For register, count doesn't apply directly, keep date order
            }
            SortCriteria::Code => {
                entries.sort_by(|a, b| match (&a.code, &b.code) {
                    (Some(a_code), Some(b_code)) => a_code.cmp(b_code),
                    (Some(_), None) => std::cmp::Ordering::Less,
                    (None, Some(_)) => std::cmp::Ordering::Greater,
                    (None, None) => std::cmp::Ordering::Equal,
                });
            }
        }

        Ok(entries)
    }

    /// Format a register entry for display
    fn format_entry(&self, entry: &RegisterEntry, _options: &ReportOptions) -> String {
        let mut parts = Vec::new();

        // Date column
        if self.columns.date {
            let date_str = entry.date.format("%Y-%m-%d").to_string();
            let date_width = self.columns.widths.get("date").copied().unwrap_or(10);
            parts.push(format!("{:<width$}", date_str, width = date_width));
        }

        // Code column
        if self.columns.code {
            let code_str = entry.code.as_deref().unwrap_or("");
            let code_width = self.columns.widths.get("code").copied().unwrap_or(6);
            parts.push(format!("{:<width$}", code_str, width = code_width));
        }

        // Payee column
        if self.columns.payee {
            let payee_width = self
                .columns
                .widths
                .get("payee")
                .copied()
                .unwrap_or(if self.wide_format { 30 } else { 20 });
            let payee = if self.wide_format {
                entry.payee.clone()
            } else {
                // Truncate for narrow format
                if entry.payee.len() > payee_width {
                    format!("{}...", &entry.payee[..payee_width - 3])
                } else {
                    entry.payee.clone()
                }
            };
            parts.push(format!("{:<width$}", payee, width = payee_width));
        }

        // Account column
        if self.columns.account {
            let account_width = self
                .columns
                .widths
                .get("account")
                .copied()
                .unwrap_or(if self.wide_format { 40 } else { 25 });
            let account = if self.wide_format {
                entry.account.clone()
            } else {
                // Truncate account name for narrow format
                if entry.account.len() > account_width {
                    format!("{}...", &entry.account[..account_width - 3])
                } else {
                    entry.account.clone()
                }
            };
            parts.push(format!("{:<width$}", account, width = account_width));
        }

        // Amount column
        if self.columns.amount {
            let amount_width = self.columns.widths.get("amount").copied().unwrap_or(12);
            let amount_str = entry.amount.to_string();
            parts.push(format!("{:>width$}", amount_str, width = amount_width));
        }

        // Balance column
        if self.columns.balance {
            let balance_width = self.columns.widths.get("balance").copied().unwrap_or(15);
            let balance_str = entry.balance.to_string();
            parts.push(format!("{:>width$}", balance_str, width = balance_width));
        }

        // Note column
        if self.columns.note {
            if let Some(ref note) = entry.note {
                let note_width = self.columns.widths.get("note").copied().unwrap_or(20);
                let note_str = if note.len() > note_width && !self.wide_format {
                    format!("{}...", &note[..note_width - 3])
                } else {
                    note.clone()
                };
                parts.push(format!("{:<width$}", note_str, width = note_width));
            }
        }

        parts.join(" ")
    }

    /// Format header line for the register report
    fn format_header(&self) -> String {
        let mut headers = Vec::new();

        if self.columns.date {
            let width = self.columns.widths.get("date").copied().unwrap_or(10);
            headers.push(format!("{:<width$}", "Date", width = width));
        }

        if self.columns.code {
            let width = self.columns.widths.get("code").copied().unwrap_or(6);
            headers.push(format!("{:<width$}", "Code", width = width));
        }

        if self.columns.payee {
            let width = self.columns.widths.get("payee").copied().unwrap_or(if self.wide_format {
                30
            } else {
                20
            });
            headers.push(format!("{:<width$}", "Payee", width = width));
        }

        if self.columns.account {
            let width = self
                .columns
                .widths
                .get("account")
                .copied()
                .unwrap_or(if self.wide_format { 40 } else { 25 });
            headers.push(format!("{:<width$}", "Account", width = width));
        }

        if self.columns.amount {
            let width = self.columns.widths.get("amount").copied().unwrap_or(12);
            headers.push(format!("{:>width$}", "Amount", width = width));
        }

        if self.columns.balance {
            let width = self.columns.widths.get("balance").copied().unwrap_or(15);
            headers.push(format!("{:>width$}", "Balance", width = width));
        }

        if self.columns.note {
            let width = self.columns.widths.get("note").copied().unwrap_or(20);
            headers.push(format!("{:<width$}", "Note", width = width));
        }

        headers.join(" ")
    }
}

impl ReportGenerator for RegisterReport {
    fn generate<W: Write>(&mut self, writer: &mut W, options: &ReportOptions) -> ReportResult<()> {
        // Collect register entries
        let entries = self.collect_entries(options)?;

        // Write header
        let header = self.format_header();
        writeln!(writer, "{}", header).map_err(|e| ReportError::IoError(e.to_string()))?;

        // Write separator line
        writeln!(writer, "{}", "-".repeat(header.len()))
            .map_err(|e| ReportError::IoError(e.to_string()))?;

        // Write each entry
        for entry in entries {
            let line = self.format_entry(&entry, options);
            writeln!(writer, "{}", line).map_err(|e| ReportError::IoError(e.to_string()))?;

            // Show related accounts for split transactions
            if self.show_related && entry.is_split && !entry.related_accounts.is_empty() {
                for related in &entry.related_accounts {
                    let indent = " ".repeat(if self.columns.date { 12 } else { 0 });
                    writeln!(writer, "{}  -> {}", indent, related)
                        .map_err(|e| ReportError::IoError(e.to_string()))?;
                }
            }
        }

        Ok(())
    }

    fn description(&self) -> &str {
        "Register report showing transaction details with running balances"
    }

    fn report_type(&self) -> &str {
        "register"
    }
}

/// Print report for reconstructing transactions
pub struct PrintReport {
    /// Journal to generate report from
    pub journal: Journal,
    /// Transaction filter chain
    pub transaction_filters: FilterChain<Transaction>,
    /// Include related accounts for context
    pub show_related: bool,
}

impl PrintReport {
    /// Create a new print report
    pub fn new(journal: Journal) -> Self {
        Self { journal, transaction_filters: FilterChain::new(), show_related: true }
    }

    /// Add a transaction filter
    pub fn with_transaction_filter(mut self, filter: Box<dyn TransactionFilter>) -> Self {
        self.transaction_filters = self.transaction_filters.add_filter(filter);
        self
    }

    /// Set whether to show related accounts
    pub fn with_show_related(mut self, show_related: bool) -> Self {
        self.show_related = show_related;
        self
    }
}

impl ReportGenerator for PrintReport {
    fn generate<W: Write>(&mut self, writer: &mut W, _options: &ReportOptions) -> ReportResult<()> {
        for transaction in &self.journal.transactions {
            if !self.transaction_filters.matches(transaction) {
                continue;
            }

            // Write transaction date
            writeln!(writer, "{}", transaction.date.format("%Y-%m-%d"))
                .map_err(|e| ReportError::IoError(e.to_string()))?;

            // Write transaction code if present
            if let Some(ref code) = transaction.code {
                writeln!(writer, "    ; Code: {}", code)
                    .map_err(|e| ReportError::IoError(e.to_string()))?;
            }

            // Write payee
            writeln!(writer, "    * {}", transaction.payee)
                .map_err(|e| ReportError::IoError(e.to_string()))?;

            // Write note if present
            if let Some(ref note) = transaction.note {
                writeln!(writer, "    ; {}", note)
                    .map_err(|e| ReportError::IoError(e.to_string()))?;
            }

            // Write postings
            for posting in &transaction.postings {
                {
                    let account_name = posting.account.borrow().fullname_immutable();

                    if let Some(ref amount) = posting.amount {
                        writeln!(writer, "    {}    {}", account_name, amount)
                            .map_err(|e| ReportError::IoError(e.to_string()))?;
                    } else {
                        writeln!(writer, "    {}", account_name)
                            .map_err(|e| ReportError::IoError(e.to_string()))?;
                    }

                    // Write posting note if present
                    if let Some(ref note) = posting.note {
                        writeln!(writer, "        ; {}", note)
                            .map_err(|e| ReportError::IoError(e.to_string()))?;
                    }
                }
            }

            // Empty line between transactions
            writeln!(writer).map_err(|e| ReportError::IoError(e.to_string()))?;
        }

        Ok(())
    }

    fn description(&self) -> &str {
        "Print report for transaction reconstruction"
    }

    fn report_type(&self) -> &str {
        "print"
    }
}

/// Equity report for opening/closing balances
pub struct EquityReport {
    /// Journal to generate report from
    pub journal: Journal,
    /// Balance report for calculating balances
    balance_report: BalanceReport,
    /// Closing date for the equity calculation
    pub closing_date: Option<chrono::NaiveDate>,
}

impl EquityReport {
    /// Create a new equity report
    pub fn new(journal: Journal) -> Self {
        let balance_report = BalanceReport::new(journal.clone());
        Self { journal, balance_report, closing_date: None }
    }

    /// Set closing date
    pub fn with_closing_date(mut self, date: chrono::NaiveDate) -> Self {
        self.closing_date = Some(date);
        self
    }
}

impl ReportGenerator for EquityReport {
    fn generate<W: Write>(&mut self, writer: &mut W, options: &ReportOptions) -> ReportResult<()> {
        writeln!(writer, "Opening/Closing Balance Equity Report")
            .map_err(|e| ReportError::IoError(e.to_string()))?;
        writeln!(writer, "=====================================")
            .map_err(|e| ReportError::IoError(e.to_string()))?;
        writeln!(writer).map_err(|e| ReportError::IoError(e.to_string()))?;

        if let Some(closing_date) = self.closing_date {
            writeln!(writer, "Closing Date: {}", closing_date.format("%Y-%m-%d"))
                .map_err(|e| ReportError::IoError(e.to_string()))?;
            writeln!(writer).map_err(|e| ReportError::IoError(e.to_string()))?;
        }

        // Generate balance report for equity calculation
        let balances = self.balance_report.collect_balances(options)?;

        writeln!(writer, "Closing Balances:").map_err(|e| ReportError::IoError(e.to_string()))?;
        writeln!(writer, "-----------------").map_err(|e| ReportError::IoError(e.to_string()))?;

        for (account_name, account_balance) in balances {
            if !account_balance.balance.is_zero() {
                writeln!(writer, "{:<40} {}", account_name, account_balance.balance)
                    .map_err(|e| ReportError::IoError(e.to_string()))?;
            }
        }

        Ok(())
    }

    fn description(&self) -> &str {
        "Equity report showing opening/closing balances"
    }

    fn report_type(&self) -> &str {
        "equity"
    }
}

/// Statistics report with journal metrics
pub struct StatsReport {
    /// Journal to generate report from
    pub journal: Journal,
    /// Include detailed breakdown
    pub detailed: bool,
}

impl StatsReport {
    /// Create a new stats report
    pub fn new(journal: Journal) -> Self {
        Self { journal, detailed: false }
    }

    /// Enable detailed breakdown
    pub fn with_detailed(mut self, detailed: bool) -> Self {
        self.detailed = detailed;
        self
    }
}

impl ReportGenerator for StatsReport {
    fn generate<W: Write>(&mut self, writer: &mut W, _options: &ReportOptions) -> ReportResult<()> {
        writeln!(writer, "Journal Statistics").map_err(|e| ReportError::IoError(e.to_string()))?;
        writeln!(writer, "==================").map_err(|e| ReportError::IoError(e.to_string()))?;
        writeln!(writer).map_err(|e| ReportError::IoError(e.to_string()))?;

        // Basic statistics
        let transaction_count = self.journal.transactions.len();
        let total_postings: usize =
            self.journal.transactions.iter().map(|tx| tx.postings.len()).sum();
        let account_count = self.journal.accounts.len();

        writeln!(writer, "Total Transactions: {}", transaction_count)
            .map_err(|e| ReportError::IoError(e.to_string()))?;
        writeln!(writer, "Total Postings:     {}", total_postings)
            .map_err(|e| ReportError::IoError(e.to_string()))?;
        writeln!(writer, "Total Accounts:     {}", account_count)
            .map_err(|e| ReportError::IoError(e.to_string()))?;

        if total_postings > 0 {
            let avg_postings = total_postings as f64 / transaction_count as f64;
            writeln!(writer, "Avg Postings/Txn:   {:.2}", avg_postings)
                .map_err(|e| ReportError::IoError(e.to_string()))?;
        }

        // Date range
        if let (Some(earliest), Some(latest)) = (
            self.journal.transactions.iter().map(|tx| tx.date).min(),
            self.journal.transactions.iter().map(|tx| tx.date).max(),
        ) {
            writeln!(
                writer,
                "Date Range:         {} to {}",
                earliest.format("%Y-%m-%d"),
                latest.format("%Y-%m-%d")
            )
            .map_err(|e| ReportError::IoError(e.to_string()))?;

            let duration = latest.signed_duration_since(earliest);
            writeln!(writer, "Duration:           {} days", duration.num_days())
                .map_err(|e| ReportError::IoError(e.to_string()))?;
        }

        if self.detailed {
            writeln!(writer).map_err(|e| ReportError::IoError(e.to_string()))?;
            writeln!(writer, "Account Distribution:")
                .map_err(|e| ReportError::IoError(e.to_string()))?;
            writeln!(writer, "--------------------")
                .map_err(|e| ReportError::IoError(e.to_string()))?;

            // Count accounts by top-level category
            let mut account_categories: HashMap<String, usize> = HashMap::new();
            for account in self.journal.accounts.values() {
                let account_ref = account.borrow();
                let full_name = account_ref.fullname_immutable();
                let category = full_name.split(':').next().unwrap_or("Unknown").to_string();
                *account_categories.entry(category).or_insert(0) += 1;
            }

            for (category, count) in account_categories {
                writeln!(writer, "{:<20} {}", category, count)
                    .map_err(|e| ReportError::IoError(e.to_string()))?;
            }
        }

        Ok(())
    }

    fn description(&self) -> &str {
        "Statistics report with journal metrics"
    }

    fn report_type(&self) -> &str {
        "stats"
    }
}

/// Cleared report for reconciliation
pub struct ClearedReport {
    /// Journal to generate report from
    pub journal: Journal,
    /// Filter chain for transactions
    pub transaction_filters: FilterChain<Transaction>,
    /// Show only cleared transactions
    pub cleared_only: bool,
    /// Show only uncleared transactions
    pub uncleared_only: bool,
}

impl ClearedReport {
    /// Create a new cleared report
    pub fn new(journal: Journal) -> Self {
        Self {
            journal,
            transaction_filters: FilterChain::new(),
            cleared_only: false,
            uncleared_only: false,
        }
    }

    /// Show only cleared transactions
    pub fn cleared_only(mut self) -> Self {
        self.cleared_only = true;
        self.uncleared_only = false;
        self
    }

    /// Show only uncleared transactions
    pub fn uncleared_only(mut self) -> Self {
        self.uncleared_only = true;
        self.cleared_only = false;
        self
    }
}

impl ReportGenerator for ClearedReport {
    fn generate<W: Write>(&mut self, writer: &mut W, _options: &ReportOptions) -> ReportResult<()> {
        writeln!(writer, "Cleared/Uncleared Transaction Report")
            .map_err(|e| ReportError::IoError(e.to_string()))?;
        writeln!(writer, "====================================")
            .map_err(|e| ReportError::IoError(e.to_string()))?;
        writeln!(writer).map_err(|e| ReportError::IoError(e.to_string()))?;

        let mut cleared_count = 0;
        let mut uncleared_count = 0;
        let mut cleared_total = Balance::new();
        let mut uncleared_total = Balance::new();

        for transaction in &self.journal.transactions {
            if !self.transaction_filters.matches(transaction) {
                continue;
            }

            // For now, assume all transactions are cleared (would need cleared flag in transaction)
            let is_cleared = true; // transaction.cleared.unwrap_or(false);

            if self.cleared_only && !is_cleared {
                continue;
            }
            if self.uncleared_only && is_cleared {
                continue;
            }

            if is_cleared {
                cleared_count += 1;
                writeln!(
                    writer,
                    "C {}: {}",
                    transaction.date.format("%Y-%m-%d"),
                    transaction.payee
                )
                .map_err(|e| ReportError::IoError(e.to_string()))?;
            } else {
                uncleared_count += 1;
                writeln!(
                    writer,
                    "U {}: {}",
                    transaction.date.format("%Y-%m-%d"),
                    transaction.payee
                )
                .map_err(|e| ReportError::IoError(e.to_string()))?;
            }

            // Sum amounts for totals
            for posting in &transaction.postings {
                if let Some(ref amount) = posting.amount {
                    if is_cleared {
                        cleared_total
                            .add_amount(amount)
                            .map_err(|e| ReportError::InvalidConfig(e.to_string()))?;
                    } else {
                        uncleared_total
                            .add_amount(amount)
                            .map_err(|e| ReportError::InvalidConfig(e.to_string()))?;
                    }
                }
            }
        }

        writeln!(writer).map_err(|e| ReportError::IoError(e.to_string()))?;
        writeln!(writer, "Summary:").map_err(|e| ReportError::IoError(e.to_string()))?;
        writeln!(writer, "--------").map_err(|e| ReportError::IoError(e.to_string()))?;
        writeln!(writer, "Cleared Transactions:   {}", cleared_count)
            .map_err(|e| ReportError::IoError(e.to_string()))?;
        writeln!(writer, "Uncleared Transactions: {}", uncleared_count)
            .map_err(|e| ReportError::IoError(e.to_string()))?;

        Ok(())
    }

    fn description(&self) -> &str {
        "Cleared report for reconciliation"
    }

    fn report_type(&self) -> &str {
        "cleared"
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::NaiveDate;
    use ledger_math::amount::Amount;
    use std::str::FromStr;

    fn create_test_journal() -> Journal {
        Journal::new()
    }

    #[test]
    fn test_balance_report_creation() {
        let journal = create_test_journal();
        let report = BalanceReport::new(journal);

        assert_eq!(
            report.description(),
            "Balance report showing account balances with optional hierarchy and grouping"
        );
        assert_eq!(report.report_type(), "balance");
    }

    #[test]
    fn test_register_report_creation() {
        let journal = create_test_journal();
        let report = RegisterReport::new(journal);

        assert_eq!(
            report.description(),
            "Register report showing transaction details with running balances"
        );
        assert_eq!(report.report_type(), "register");
        assert!(!report.wide_format);
        assert!(report.show_related);
    }

    #[test]
    fn test_register_columns_default() {
        let columns = RegisterColumns::default();

        assert!(columns.date);
        assert!(!columns.code);
        assert!(columns.payee);
        assert!(columns.account);
        assert!(columns.amount);
        assert!(columns.balance);
        assert!(!columns.note);
    }

    #[test]
    fn test_report_options_default() {
        let options = ReportOptions::default();

        assert!(!options.flat);
        assert!(options.depth.is_none());
        assert!(!options.empty);
        assert_eq!(options.sort_by, SortCriteria::Name);
    }

    #[test]
    fn test_sort_criteria() {
        assert_eq!(SortCriteria::Name, SortCriteria::Name);
        assert_ne!(SortCriteria::Name, SortCriteria::Amount);
    }

    #[test]
    fn test_register_entry() {
        let date = NaiveDate::from_ymd_opt(2024, 1, 15).unwrap();
        let entry = RegisterEntry {
            date,
            payee: "Test Payee".to_string(),
            account: "Assets:Checking".to_string(),
            amount: Amount::from_str("100.00 USD").unwrap_or_default(),
            balance: crate::balance::Balance::new(),
            code: Some("123".to_string()),
            note: Some("Test note".to_string()),
            is_split: false,
            related_accounts: vec![],
        };

        assert_eq!(entry.payee, "Test Payee");
        assert_eq!(entry.account, "Assets:Checking");
        assert!(!entry.is_split);
    }

    #[test]
    fn test_print_report_creation() {
        let journal = create_test_journal();
        let report = PrintReport::new(journal);

        assert_eq!(report.description(), "Print report for transaction reconstruction");
        assert_eq!(report.report_type(), "print");
        assert!(report.show_related);
    }

    #[test]
    fn test_equity_report_creation() {
        let journal = create_test_journal();
        let report = EquityReport::new(journal);

        assert_eq!(report.description(), "Equity report showing opening/closing balances");
        assert_eq!(report.report_type(), "equity");
        assert!(report.closing_date.is_none());
    }

    #[test]
    fn test_stats_report_creation() {
        let journal = create_test_journal();
        let report = StatsReport::new(journal);

        assert_eq!(report.description(), "Statistics report with journal metrics");
        assert_eq!(report.report_type(), "stats");
        assert!(!report.detailed);
    }

    #[test]
    fn test_cleared_report_creation() {
        let journal = create_test_journal();
        let report = ClearedReport::new(journal);

        assert_eq!(report.description(), "Cleared report for reconciliation");
        assert_eq!(report.report_type(), "cleared");
        assert!(!report.cleared_only);
        assert!(!report.uncleared_only);
    }

    #[test]
    fn test_equity_report_with_date() {
        let journal = create_test_journal();
        let date = NaiveDate::from_ymd_opt(2024, 12, 31).unwrap();
        let report = EquityReport::new(journal).with_closing_date(date);

        assert_eq!(report.closing_date, Some(date));
    }

    #[test]
    fn test_stats_report_with_detailed() {
        let journal = create_test_journal();
        let report = StatsReport::new(journal).with_detailed(true);

        assert!(report.detailed);
    }

    #[test]
    fn test_cleared_report_filters() {
        let journal = create_test_journal();

        let cleared_report = ClearedReport::new(journal.clone()).cleared_only();
        assert!(cleared_report.cleared_only);
        assert!(!cleared_report.uncleared_only);

        let uncleared_report = ClearedReport::new(journal).uncleared_only();
        assert!(!uncleared_report.cleared_only);
        assert!(uncleared_report.uncleared_only);
    }
}
