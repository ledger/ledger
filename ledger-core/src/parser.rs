//! Journal parsing engine using nom combinators
//!
//! This module provides a robust parser for Ledger journal files, supporting:
//! - All Ledger directives (account, commodity, include, etc.)
//! - Transaction parsing with proper indentation handling
//! - Comment and metadata extraction
//! - Error recovery with meaningful messages
//! - Include file resolution with cycle detection
//! - Streaming parser for large files

use ledger_math::{commodity::Precision, Annotation, CommodityFlags, CommodityPool};
use log::debug;
use nom::{
    branch::alt,
    bytes::complete::{is_not, take_until, take_while_m_n},
    character::complete::{char, digit1, line_ending, space0, space1},
    combinator::{consumed, map, not, opt, success, value, verify},
    error::{context, ParseError},
    multi::{many0, many1, many_m_n},
    sequence::{delimited, pair, preceded, terminated, tuple},
    AsChar, IResult,
};

// We need to use bytes for tag with byte strings

use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::str::FromStr;
use std::sync::Arc;

use crate::{
    account::Account,
    amount::Amount,
    commodity::Commodity,
    journal::Journal,
    posting::Posting,
    transaction::{TagData, Transaction},
};

use chrono::{Datelike, Local, NaiveDate};
use rust_decimal::Decimal;

/// Custom error type for parser errors with source location tracking
#[derive(Debug, thiserror::Error)]
pub enum JournalParseError {
    #[error("Parse error in {filename} at line {line}, column {column}: {message}")]
    ParseError {
        filename: PathBuf,
        line: usize,
        column: usize,
        message: String,
        context: Option<String>,
        suggestion: Option<String>,
    },
    #[error("Include cycle detected: {path}\nInclude stack: {}", include_stack.iter().map(|p| p.display().to_string()).collect::<Vec<_>>().join(" -> "))]
    IncludeCycle { path: PathBuf, include_stack: Vec<PathBuf> },
    #[error("File not found: {path}")]
    FileNotFound { path: PathBuf },
    #[error("IO error reading {path}: {source}")]
    IoError {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },
    #[error("Transaction validation error in {filename} at line {line}: {message}")]
    ValidationError {
        filename: PathBuf,
        line: usize,
        message: String,
        transaction_description: String,
    },
    #[error("Unbalanced transaction in {filename} at line {line}: {message}\nTransaction: {description}\nDifference: {difference}")]
    UnbalancedTransaction {
        filename: PathBuf,
        line: usize,
        description: String,
        message: String,
        difference: String,
    },
}

/// Custom error type for detailed error reporting
pub type VerboseError<I> = nom::error::VerboseError<I>;

/// Result type for parsing operations
type ParseResult<'a, T> = IResult<&'a str, T, VerboseError<&'a str>>;

// Helper function for string tags that works with VerboseError
fn tag<'a>(s: &'a str) -> impl Fn(&'a str) -> ParseResult<'a, &'a str> + 'a {
    move |input: &'a str| {
        if let Some(stripped) = input.strip_prefix(s) {
            Ok((stripped, s))
        } else {
            Err(nom::Err::Error(VerboseError::from_error_kind(input, nom::error::ErrorKind::Tag)))
        }
    }
}

// thread_local! allows shared global state during parse while still allowing us
// to run tests in parallel. Won't work if parsing is ever multithreaded.
thread_local! {
    /// Global parse state.
    static PARSE_STATE: RefCell<ParseContext> = RefCell::new(ParseContext::default())
}

#[cfg(test)]
pub fn reset_parse_state() {
    PARSE_STATE.with_borrow_mut(|c| c.commodity_pool = CommodityPool::new());
}

/// Parser state structure for context management
#[derive(Debug, Clone)]
pub struct ParseContext {
    /// Current file being parsed
    pub filename: PathBuf,
    /// Current line number
    pub line: usize,
    /// Current column number
    pub column: usize,
    /// Include stack for cycle detection
    pub include_stack: Vec<PathBuf>,
    /// Apply stack for directive state
    pub apply_stack: Vec<ApplyState>,
    /// Current year for date parsing
    pub current_year: Option<i32>,
    /// Default account for unbalanced postings
    pub default_account: Option<String>,
    /// Commodity pool
    pub commodity_pool: CommodityPool,
    /// Commodity aliases
    pub commodity_aliases: HashMap<String, String>,
    /// Account aliases
    pub account_aliases: HashMap<String, String>,
}

impl Default for ParseContext {
    fn default() -> Self {
        ParseContext {
            filename: PathBuf::from("<input>"),
            line: 1,
            column: 1,
            include_stack: Vec::new(),
            apply_stack: Vec::new(),
            current_year: None,
            default_account: None,
            commodity_pool: CommodityPool::new(),
            commodity_aliases: HashMap::new(),
            account_aliases: HashMap::new(),
        }
    }
}

/// Apply directive state types
#[derive(Debug, Clone)]
pub enum ApplyState {
    Account(String),
    Tag(String, String),
    Rate(Commodity, Amount),
    Year(i32),
}

/// Parsed journal entry - either a transaction or a directive
#[derive(Debug, Clone)]
pub enum JournalEntry {
    Transaction(Transaction),
    Directive(Directive),
    Comment(String),
    MetadataComment { comment: String, metadata: HashMap<String, String> },
    EmptyLine,
}

/// All supported Ledger directives
#[derive(Debug, Clone)]
pub enum Directive {
    // Core directives
    Account {
        name: String,
        declarations: Vec<AccountDeclaration>,
    },
    Alias {
        account: String,
        alias: String,
    },
    Apply {
        state: ApplyState,
    },
    End,
    Assert {
        condition: String,
    },
    Check {
        condition: String,
    },

    // Commodity directives
    Commodity {
        symbol: String,
        declarations: Vec<CommodityDeclaration>,
    },
    DefaultCommodity {
        commodity: Arc<Commodity>,
    },

    // Price directives
    Price {
        date: NaiveDate,
        commodity: String,
        price: Amount,
    },

    // Include directives
    Include {
        path: PathBuf,
    },
    ConditionalInclude {
        condition: String,
        path: PathBuf,
    },

    // Payee directives
    Payee {
        name: String,
        declarations: Vec<PayeeDeclaration>,
    },

    // Tag directives
    Tag {
        name: String,
    },

    // Option directives
    Option {
        name: String,
        value: String,
    },

    // Evaluation directives
    Eval {
        expression: String,
    },
    Define {
        name: String,
        expression: String,
    },

    // Year directive
    Year {
        year: i32,
    },

    // Periodic and automated transactions
    PeriodicTransaction {
        period: String,
        postings: Vec<Posting>,
        metadata: HashMap<String, String>,
    },
    AutomatedTransaction {
        condition: String,
        postings: Vec<Posting>,
        metadata: HashMap<String, String>,
    },
}

/// Account declaration sub-directives
#[derive(Debug, Clone)]
pub enum AccountDeclaration {
    Alias(String),
    Payee(String),
    Check(String),
    Assert(String),
    Eval(String),
    Default,
}

/// Commodity declaration sub-directives
#[derive(Debug, Clone)]
pub enum CommodityDeclaration {
    Alias(String),
    Format(String),
    NoMarket,
    Default,
}

/// Payee declaration sub-directives
#[derive(Debug, Clone)]
pub enum PayeeDeclaration {
    Alias(String),
    Uuid(String),
}

/// Main parser interface for streaming large files
pub struct JournalParser {
    context: ParseContext,
}

impl JournalParser {
    /// Create a new parser with default context
    pub fn new() -> Self {
        JournalParser { context: ParseContext::default() }
    }

    /// Create a parser with specified filename
    pub fn with_file<P: AsRef<Path>>(filename: P) -> Self {
        let mut parser = Self::new();
        parser.context.filename = filename.as_ref().to_path_buf();
        parser
    }

    /// Parse a file with include resolution
    pub fn parse_file<P: AsRef<Path>>(&mut self, path: P) -> Result<Journal, JournalParseError> {
        let path_buf = path.as_ref().to_path_buf();

        // Check for include cycles
        if self.context.include_stack.contains(&path_buf) {
            return Err(JournalParseError::IncludeCycle {
                path: path_buf,
                include_stack: self.context.include_stack.clone(),
            });
        }

        // Read file contents
        let contents = std::fs::read_to_string(&path_buf)
            .map_err(|e| JournalParseError::IoError { path: path_buf.clone(), source: e })?;

        // Update context
        let old_filename = self.context.filename.clone();
        self.context.filename = path_buf.clone();
        self.context.include_stack.push(path_buf);

        // Parse the file
        let result = self.parse_journal(&contents);

        // Restore context
        self.context.filename = old_filename;
        self.context.include_stack.pop();

        result
    }

    /// Parse a complete journal from a string
    pub fn parse_journal(&mut self, input: &str) -> Result<Journal, JournalParseError> {
        let entries = {
            PARSE_STATE.with_borrow_mut(|c| c.commodity_pool = self.context.commodity_pool.clone());
            let entries = self.parse_entries(input)?;
            self.context.commodity_pool = PARSE_STATE.with_borrow_mut(|c| c.commodity_pool.clone());
            entries
        };
        let journal = self.build_journal(entries)?;
        Ok(journal)
    }

    /// Parse entries from input string with enhanced error reporting
    pub fn parse_entries(&mut self, input: &str) -> Result<Vec<JournalEntry>, JournalParseError> {
        match journal_entries_with_recovery(input, &mut self.context) {
            Ok((_, entries)) => Ok(entries),
            Err(nom::Err::Error(e) | nom::Err::Failure(e)) => {
                let error_info = self.extract_error_context(input, &e);
                Err(JournalParseError::ParseError {
                    filename: self.context.filename.clone(),
                    line: error_info.line,
                    column: error_info.column,
                    message: error_info.message,
                    context: error_info.context,
                    suggestion: error_info.suggestion,
                })
            }
            Err(nom::Err::Incomplete(_)) => Err(JournalParseError::ParseError {
                filename: self.context.filename.clone(),
                line: self.context.line,
                column: self.context.column,
                message: "Incomplete input - possibly missing data".to_string(),
                context: None,
                suggestion: Some(
                    "Check if the file is complete and properly formatted".to_string(),
                ),
            }),
        }
    }

    /// Build journal from parsed entries
    fn build_journal(&mut self, entries: Vec<JournalEntry>) -> Result<Journal, JournalParseError> {
        let mut journal = Journal::new();
        journal.commodity_pool = self.context.commodity_pool.clone();

        for entry in entries {
            match entry {
                JournalEntry::Transaction(transaction) => {
                    // Register accounts and commodities from transaction before adding
                    self.register_transaction_accounts_and_commodities(&mut journal, &transaction);

                    // Validate transaction before adding
                    match self.validate_transaction(&transaction) {
                        Ok(_) => journal.add_transaction(transaction),
                        Err(validation_error) => {
                            // For now, log error and continue - could be made strict
                            eprintln!("Warning: {}", validation_error);
                            journal.add_transaction(transaction);
                        }
                    }
                }
                JournalEntry::Directive(directive) => {
                    self.process_directive(&mut journal, directive)?;
                }
                JournalEntry::Comment(_) | JournalEntry::EmptyLine => {
                    // Skip comments and empty lines
                }
                JournalEntry::MetadataComment { comment: _, metadata: _ } => {
                    // TODO: Process metadata comments
                }
            }
        }

        Ok(journal)
    }

    /// Register accounts and commodities from a transaction
    fn register_transaction_accounts_and_commodities(
        &mut self,
        journal: &mut Journal,
        transaction: &Transaction,
    ) {
        for posting in &transaction.postings {
            // Get the account name from the posting
            let account_name = {
                let account = posting.account.borrow();
                account.name().to_string()
            };

            // Ensure the account exists in the journal
            // This will create it if it doesn't exist
            let _ = journal.get_or_create_account(&account_name);

            // Register commodity from the amount if present
            // if let Some(amount) = &posting.amount {
            //     self.register_commodity_for_amount(amount, journal);
            //     if let Some(price) =
            //         amount.commodity().and_then(|c| c.annotation().price().as_ref())
            //     {
            //         self.register_commodity_for_amount(price, journal);
            //     }
            // }
            // if let Some(cost) = &posting.given_cost {
            //     self.register_commodity_for_amount(cost, journal);
            //     // NOTE: technically, the commodity of a cost could have a
            //     // price, but I don't believe it ever does in practice
            // }
        }
    }

    // fn register_commodity_for_amount(&mut self, amount: &Amount, journal: &mut Journal) {
    //     if let Some(commodity_ref) = amount.commodity() {
    //         let new_commodity = commodity_ref.clone();
    //         let symbol = new_commodity.symbol().to_string();

    //         // Add commodity if it doesn't exist in the journal
    //         if let Some(previous_commodity) = journal.commodities.get(&symbol) {
    //             if previous_commodity.precision() < new_commodity.precision()
    //                 || previous_commodity.flags() != new_commodity.flags()
    //                 || previous_commodity.annotation() != new_commodity.annotation()
    //             {
    //                 // FIXME: this seems messy
    //                 let mut new_commodity = Arc::unwrap_or_clone(new_commodity);

    //                 if new_commodity.precision() < previous_commodity.precision() {
    //                     new_commodity.set_precision(previous_commodity.precision());
    //                 }
    //                 new_commodity.add_flags(previous_commodity.flags());

    //                 match (
    //                     new_commodity.annotation().price(),
    //                     previous_commodity.annotation().price(),
    //                 ) {
    //                     (Some(new_commodity_price), Some(previous_commodity_price))
    //                         if new_commodity_price < previous_commodity_price =>
    //                     {
    //                         new_commodity
    //                             .annotation_mut()
    //                             .set_price(previous_commodity_price.clone());
    //                     }
    //                     (None, Some(previous_commodity_price)) => {
    //                         new_commodity
    //                             .annotation_mut()
    //                             .set_price(previous_commodity_price.clone());
    //                     }
    //                     (Some(_), Some(_)) | (Some(_), None) | (None, None) => {}
    //                 }

    //                 journal.commodities.insert(symbol.clone(), Arc::new(new_commodity));
    //             }
    //         } else {
    //             journal.commodities.insert(symbol.clone(), new_commodity.clone());
    //         }
    //     }
    // }

    /// Process a parsed directive with include handling. Happens after entire
    /// journal is parsed. For processing directives during parse, see
    /// `process_directive_inline`
    fn process_directive(
        &mut self,
        journal: &mut Journal,
        directive: Directive,
    ) -> Result<(), JournalParseError> {
        match directive {
            Directive::Account { name, declarations } => {
                let account = Account::new(name.clone().into(), None, 0);
                for decl in declarations {
                    match decl {
                        AccountDeclaration::Alias(_alias) => {
                            // journal.add_account_alias(alias, name.clone());
                        }
                        AccountDeclaration::Default => {
                            // journal.set_default_account(name.clone());
                        }
                        // Handle other account declarations
                        _ => {}
                    }
                }
                // TODO: error logging
                let _ = journal.add_account(Rc::new(RefCell::new(account)));
            }
            Directive::Commodity { symbol, declarations } => {
                let mut commodity = Commodity::new(&symbol);
                for decl in declarations {
                    match decl {
                        CommodityDeclaration::Format(format) => {
                            commodity.set_format(format.clone());
                        }
                        CommodityDeclaration::NoMarket => {
                            commodity.set_no_market(true);
                        }
                        // Handle other commodity declarations
                        _ => {}
                    }
                }
                // TODO: error logging
                let _ = journal.add_commodity(commodity);
            }
            Directive::Include { path } => {
                self.process_include_file(journal, &path)?;
            }
            Directive::ConditionalInclude { condition, path } => {
                // TODO: Evaluate condition - for now, always include
                if self.should_include_conditional(&condition) {
                    self.process_include_file(journal, &path)?;
                }
            }
            Directive::PeriodicTransaction { period, postings, metadata: _ } => {
                // TODO: Process periodic transaction
                // For now, just log that we found one
                println!("Found periodic transaction: {} with {} postings", period, postings.len());
            }
            Directive::AutomatedTransaction { condition, postings, metadata: _ } => {
                // TODO: Process automated transaction
                // For now, just log that we found one
                println!(
                    "Found automated transaction: {} with {} postings",
                    condition,
                    postings.len()
                );
            }
            // TODO:
            // Directive::Alias { account, alias } => todo!(),
            // Directive::Apply { state } => todo!(),
            // Directive::End => todo!(),
            // Directive::Assert { condition } => todo!(),
            // Directive::Check { condition } => todo!(),
            // Directive::DefaultCommodity { symbol } => todo!(),
            // Directive::Price { date, commodity, price } => todo!(),
            // Directive::Payee { name, declarations } => todo!(),
            // Directive::Tag { name } => todo!(),
            // Directive::Option { name, value } => todo!(),
            // Directive::Eval { expression } => todo!(),
            // Directive::Define { name, expression } => todo!(),

            // no after-parse processing for these directives
            Directive::Year { .. } => {}

            _ => {}
        }

        Ok(())
    }

    /// Process an include file with path resolution
    fn process_include_file(
        &mut self,
        journal: &mut Journal,
        path: &PathBuf,
    ) -> Result<(), JournalParseError> {
        // Resolve relative paths
        let include_path = if path.is_absolute() {
            path.clone()
        } else {
            self.context.filename.parent().unwrap_or_else(|| Path::new("")).join(path)
        };

        // Parse included file and merge
        let included_journal = self.parse_file(&include_path)?;

        // Merge the included journal into the current one
        // TODO: error logging
        let _ = journal.merge(included_journal);

        Ok(())
    }

    /// Check if a conditional include should be processed
    fn should_include_conditional(&self, condition: &str) -> bool {
        // TODO: Implement proper condition evaluation
        // For now, evaluate simple conditions like file existence

        if condition.starts_with("exists(") && condition.ends_with(")") {
            let file_path = &condition[7..condition.len() - 1];
            let path = PathBuf::from(file_path.trim_matches('"'));

            let resolved_path = if path.is_absolute() {
                path
            } else {
                self.context.filename.parent().unwrap_or_else(|| Path::new("")).join(&path)
            };

            return resolved_path.exists();
        }

        // Default to true for unknown conditions
        true
    }

    /// Extract detailed error context from parse failure
    fn extract_error_context(&self, input: &str, error: &VerboseError<&str>) -> ErrorContext {
        // Get line and column information
        let (line, column) =
            self.get_line_column(input, error.errors.first().map(|(s, _)| s).unwrap_or(&input));

        // Extract context around the error
        let context = self.get_error_context_string(input, line, column);

        // Generate helpful error message and suggestions
        let (message, suggestion) = self.generate_error_message_and_suggestion(error);

        ErrorContext { line, column, message, context: Some(context), suggestion }
    }

    /// Get line and column from input position
    fn get_line_column(&self, input: &str, error_pos: &str) -> (usize, usize) {
        let error_offset = input.len() - error_pos.len();
        let lines: Vec<&str> = input[..error_offset].lines().collect();
        let error_at_start_of_line =
            error_offset > 0 && &input[(error_offset - 1)..(error_offset)] == "\n";

        let line = lines.len() + if error_at_start_of_line { 1 } else { 0 };
        let column =
            if error_at_start_of_line { 0 } else { lines.last().map(|l| l.len()).unwrap_or(0) } + 1;
        (line, column)
    }

    /// Get context string around error position
    fn get_error_context_string(&self, input: &str, line: usize, column: usize) -> String {
        let lines: Vec<&str> = input.lines().collect();
        let mut context = Vec::new();

        // Show 2 lines before error
        for i in (line.saturating_sub(3))..line.saturating_sub(1) {
            if i < lines.len() {
                context.push(format!("{:4} | {}", i + 1, lines[i]));
            }
        }

        // Show error line with pointer
        if line <= lines.len() {
            let error_line = lines.get(line.saturating_sub(1)).unwrap_or(&"");
            context.push(format!("{:4} | {}", line, error_line));
            let pointer = format!("{:4} | {}^", "", " ".repeat(column.saturating_sub(1)));
            context.push(pointer);
        }

        // Show 1 line after error
        if line < lines.len() {
            context.push(format!("{:4} | {}", line + 1, lines[line]));
        }

        context.join("\n")
    }

    /// Generate helpful error messages and suggestions
    fn generate_error_message_and_suggestion(
        &self,
        error: &VerboseError<&str>,
    ) -> (String, Option<String>) {
        if let Some((_, kind)) = error.errors.first() {
            match kind {
                nom::error::VerboseErrorKind::Char(c) => (
                    format!("Expected character '{}'", c),
                    Some("Check for missing punctuation or incorrect syntax".to_string()),
                ),
                nom::error::VerboseErrorKind::Context(ctx) => {
                    let suggestion = match *ctx {
                        "transaction" => Some("Ensure transaction starts with a valid date and has proper indentation for postings".to_string()),
                        "posting" => Some("Check that posting is properly indented and has a valid account name".to_string()),
                        "directive" => Some("Verify directive syntax - should start with a keyword like 'account', 'commodity', etc.".to_string()),
                        _ => Some("Check the syntax for this section".to_string()),
                    };
                    (format!("Error in {}", ctx), suggestion)
                }
                nom::error::VerboseErrorKind::Nom(nom_error) => (
                    format!("Parse error: {:?}", nom_error),
                    Some("Check the input format and syntax".to_string()),
                ),
            }
        } else {
            ("Unknown parse error".to_string(), None)
        }
    }

    /// Validate a transaction for common errors
    fn validate_transaction(&self, transaction: &Transaction) -> Result<(), JournalParseError> {
        // Check for empty transaction
        if transaction.postings.is_empty() {
            return Err(JournalParseError::ValidationError {
                filename: self.context.filename.clone(),
                line: self.context.line,
                message: "Transaction has no postings".to_string(),
                transaction_description: transaction.payee.clone(),
            });
        }

        // Check for unbalanced transaction (simplified check)
        if let Err(balance_error) = self.check_transaction_balance(transaction) {
            return Err(JournalParseError::UnbalancedTransaction {
                filename: self.context.filename.clone(),
                line: self.context.line,
                description: transaction.payee.clone(),
                message: balance_error.message,
                difference: balance_error.difference,
            });
        }

        Ok(())
    }

    /// Check transaction balance (simplified implementation)
    fn check_transaction_balance(
        &self,
        transaction: &Transaction,
    ) -> Result<(), TransactionBalanceError> {
        // TODO: Implement proper balance checking with commodities
        // For now, just check if there are at least 2 postings or one with no amount

        let posting_count = transaction.postings.len();
        let postings_with_amounts =
            transaction.postings.iter().filter(|p| p.amount.is_some()).count();

        if posting_count == 1 {
            return Err(TransactionBalanceError {
                message: "Single posting transaction must have no amount (will be auto-balanced)"
                    .to_string(),
                difference: "Unknown".to_string(),
            });
        }

        if posting_count > 1 && postings_with_amounts == posting_count {
            // All postings have amounts - should check if they balance
            // For now, assume they balance
        }

        Ok(())
    }
}

impl Default for JournalParser {
    fn default() -> Self {
        Self::new()
    }
}

/// Streaming parser for large journal files
pub struct StreamingJournalParser {
    parser: JournalParser,
    buffer_size: usize,
}

impl Default for StreamingJournalParser {
    fn default() -> Self {
        Self::new()
    }
}

impl StreamingJournalParser {
    /// Create a new streaming parser
    pub fn new() -> Self {
        Self {
            parser: JournalParser::new(),
            buffer_size: 8192, // 8KB default buffer
        }
    }

    /// Create a streaming parser with custom buffer size
    pub fn with_buffer_size(buffer_size: usize) -> Self {
        Self { parser: JournalParser::new(), buffer_size }
    }

    /// Parse a file in streaming fashion, yielding entries as they're parsed
    pub fn parse_file_streaming<P: AsRef<Path>>(
        &mut self,
        path: P,
    ) -> Result<JournalEntryIterator<BufReader<File>>, JournalParseError> {
        let file = File::open(&path).map_err(|e| JournalParseError::IoError {
            path: path.as_ref().to_path_buf(),
            source: e,
        })?;

        let reader = BufReader::with_capacity(self.buffer_size, file);

        Ok(JournalEntryIterator {
            reader,
            parser: &mut self.parser,
            current_buffer: String::new(),
            finished: false,
        })
    }

    /// Parse entries from a reader in chunks
    pub fn parse_reader_streaming<R: BufRead>(&mut self, reader: R) -> JournalEntryIterator<R> {
        JournalEntryIterator {
            reader,
            parser: &mut self.parser,
            current_buffer: String::new(),
            finished: false,
        }
    }
}

/// Iterator that yields journal entries as they are parsed
pub struct JournalEntryIterator<R: BufRead> {
    reader: R,
    parser: *mut JournalParser, // Using raw pointer to work around borrow checker
    current_buffer: String,
    finished: bool,
}

// Since we're using a raw pointer, we need to implement Send and Sync carefully
// For now, assume single-threaded usage
impl<R: BufRead> Iterator for JournalEntryIterator<R> {
    type Item = Result<JournalEntry, JournalParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.finished {
            return None;
        }

        // Read more data if needed
        match self.read_next_entry() {
            Ok(Some(entry)) => Some(Ok(entry)),
            Ok(None) => {
                self.finished = true;
                None
            }
            Err(e) => {
                self.finished = true;
                Some(Err(e))
            }
        }
    }
}

impl<R: BufRead> JournalEntryIterator<R> {
    /// Read the next journal entry from the stream
    fn read_next_entry(&mut self) -> Result<Option<JournalEntry>, JournalParseError> {
        // Try to find a complete entry in the current buffer
        loop {
            if let Some(entry) = self.try_parse_entry_from_buffer()? {
                return Ok(Some(entry));
            }

            // Need to read more data
            let mut line = String::new();
            match self.reader.read_line(&mut line) {
                Ok(0) => {
                    // End of file - try to parse any remaining buffer content
                    if self.current_buffer.trim().is_empty() {
                        return Ok(None);
                    }
                    // Try to parse remaining buffer as final entry
                    return self.parse_final_buffer_content();
                }
                Ok(_) => {
                    self.current_buffer.push_str(&line);
                }
                Err(e) => {
                    return Err(JournalParseError::IoError {
                        path: PathBuf::from("<stream>"),
                        source: e,
                    });
                }
            }
        }
    }

    /// Try to parse an entry from the current buffer
    fn try_parse_entry_from_buffer(&mut self) -> Result<Option<JournalEntry>, JournalParseError> {
        // Look for transaction or directive boundaries
        if let Some(boundary) = self.find_entry_boundary() {
            let entry_text = self.current_buffer[..boundary].to_string();
            self.current_buffer = self.current_buffer[boundary..].to_string();

            // Parse the entry
            unsafe {
                match (*self.parser).parse_entries(&entry_text) {
                    Ok(mut entries) => {
                        if !entries.is_empty() {
                            Ok(Some(entries.remove(0)))
                        } else {
                            Ok(None)
                        }
                    }
                    Err(e) => Err(e),
                }
            }
        } else {
            Ok(None)
        }
    }

    /// Find the boundary of the next complete entry
    fn find_entry_boundary(&self) -> Option<usize> {
        let lines: Vec<&str> = self.current_buffer.lines().collect();
        let mut in_transaction = false;
        let mut current_pos = 0;

        for line in lines.iter() {
            let line_start = current_pos;
            current_pos += line.len() + 1; // +1 for newline

            if line.trim().is_empty() {
                if in_transaction {
                    // End of transaction block
                    return Some(current_pos);
                }
                continue;
            }

            // Check if this starts a new entry
            if self.is_entry_start(line) {
                if in_transaction {
                    // Previous transaction ended
                    return Some(line_start);
                } else {
                    in_transaction = true;
                }
            }
        }

        None
    }

    /// Check if a line starts a new entry (transaction or directive)
    fn is_entry_start(&self, line: &str) -> bool {
        let trimmed = line.trim();

        // Check for date pattern (transaction start)
        if trimmed.chars().next().is_some_and(|c| c.is_ascii_digit()) {
            return true;
        }

        // Check for directive keywords
        for keyword in [
            "account",
            "commodity",
            "include",
            "alias",
            "apply",
            "payee",
            "tag",
            "option",
            "eval",
            "define",
            "year",
            "assert",
            "check",
            "P",
            "D",
        ] {
            if trimmed.starts_with(keyword) {
                return true;
            }
        }

        // Check for periodic/automated transactions
        if trimmed.starts_with('~') || trimmed.starts_with('=') {
            return true;
        }

        false
    }

    /// Parse any remaining buffer content as the final entry
    fn parse_final_buffer_content(&mut self) -> Result<Option<JournalEntry>, JournalParseError> {
        if self.current_buffer.trim().is_empty() {
            return Ok(None);
        }

        let content = std::mem::take(&mut self.current_buffer);
        unsafe {
            match (*self.parser).parse_entries(&content) {
                Ok(mut entries) => {
                    if !entries.is_empty() {
                        Ok(Some(entries.remove(0)))
                    } else {
                        Ok(None)
                    }
                }
                Err(e) => Err(e),
            }
        }
    }
}

// ============================================================================
// Core Parsing Combinators
// ============================================================================

/// Error context information
struct ErrorContext {
    line: usize,
    column: usize,
    message: String,
    context: Option<String>,
    suggestion: Option<String>,
}

/// Transaction balance error information
struct TransactionBalanceError {
    message: String,
    difference: String,
}

/// Parse complete journal entries
#[allow(dead_code)]
fn journal_entries(input: &str) -> ParseResult<'_, Vec<JournalEntry>> {
    many0(journal_entry)(input)
}

/// Parse complete journal entries with error recovery
fn journal_entries_with_recovery<'a>(
    input: &'a str,
    context: &mut ParseContext,
) -> ParseResult<'a, Vec<JournalEntry>> {
    let mut entries = Vec::new();
    let mut remaining = input;

    while !remaining.is_empty() {
        match journal_entry_with_recovery(remaining) {
            Ok((rest, entry)) => {
                if let Some(entry) = entry {
                    entries.push(entry);
                }
                remaining = rest;
            }
            Err(nom::Err::Error(_) | nom::Err::Failure(_)) => {
                // Try to recover by skipping to next line
                if let Some(newline_pos) = remaining.find('\n') {
                    remaining = &remaining[newline_pos + 1..];
                    context.line += 1;
                    context.column = 1;
                } else {
                    // Can't recover, return error
                    return Err(nom::Err::Error(VerboseError::from_error_kind(
                        remaining,
                        nom::error::ErrorKind::Many0,
                    )));
                }
            }
            Err(e) => return Err(e),
        }
    }

    Ok((remaining, entries))
}

/// Parse a single journal entry with error recovery
fn journal_entry_with_recovery(input: &str) -> ParseResult<'_, Option<JournalEntry>> {
    alt((
        map(journal_entry, Some),
        // Skip invalid lines and return None
        map(skip_invalid_line, |_| None),
    ))(input)
}

/// Skip an invalid line for error recovery
fn skip_invalid_line(input: &str) -> ParseResult<'_, &str> {
    terminated(take_until("\n"), line_ending)(input)
}

/// Parse a single journal entry
fn journal_entry(input: &str) -> ParseResult<'_, JournalEntry> {
    alt((
        map(transaction_entry, JournalEntry::Transaction),
        map(directive_entry, JournalEntry::Directive),
        map(enhanced_comment_line, |(comment, metadata)| {
            if metadata.is_empty() {
                JournalEntry::Comment(comment)
            } else {
                JournalEntry::MetadataComment { comment, metadata }
            }
        }),
        value(JournalEntry::EmptyLine, empty_line),
    ))(input)
}

/// Parse an empty line
fn empty_line(input: &str) -> ParseResult<'_, &str> {
    terminated(space0, line_ending)(input)
}

/// Parse a comment line with optional metadata extraction
#[allow(dead_code)]
fn comment_line(input: &str) -> ParseResult<'_, String> {
    map(preceded(alt((tag(";"), tag("#"), tag("*"), tag("|"))), take_until("\n")), |s: &str| {
        s.to_string()
    })(input)
}

/// Parse metadata tags from comments with multiple supported patterns
fn parse_metadata_tags(
    comment: &str,
    metadata: Option<HashMap<String, String>>,
) -> HashMap<String, String> {
    let mut metadata = metadata.unwrap_or_default();

    for line in comment.lines() {
        let mut trimmed = line.trim();

        while !trimmed.is_empty() {
            // Pattern 1: :tag: value
            if let Some(start) = trimmed.find(':') {
                if let Some(end) = trimmed[start + 1..].find(':') {
                    let tag_name = &trimmed[start + 1..start + 1 + end];
                    // +2 because of the leading/trailing colons
                    let remaining = &trimmed[tag_name.len() + 2..].trim();

                    let value_end_pos = remaining.find(' ').unwrap_or(remaining.len());

                    let value = remaining[..value_end_pos].trim();

                    if !tag_name.is_empty() {
                        if value.is_empty() {
                            metadata.insert(tag_name.to_string(), String::new());
                        } else {
                            metadata.insert(tag_name.to_string(), value.to_string());
                        }
                    }

                    trimmed = remaining[value_end_pos..].trim();
                    continue;
                }
            }

            // Pattern 2: [key: value] or [key=value]
            if let Some(start) = trimmed.find('[') {
                if let Some(end) = trimmed.find(']') {
                    let bracket_content = &trimmed[start + 1..end];
                    metadata = parse_metadata_tags(bracket_content, Some(metadata));

                    trimmed = trimmed[bracket_content.len() + 2..].trim();
                    continue;
                }
            }

            // Pattern 3: key: value
            if trimmed.chars().filter(|&c| c == ':').count() == 1 {
                if let Some(equals_pos) = trimmed.find(": ") {
                    let key = trimmed[..equals_pos].trim();
                    let remaining = &trimmed[equals_pos + 2..];
                    let value_end_pos = remaining.find(' ').unwrap_or(remaining.len());

                    let value = remaining[..value_end_pos].trim();
                    if !key.is_empty() && !key.contains(' ') {
                        metadata.insert(key.to_string(), value.to_string());
                    }
                    trimmed = remaining[value_end_pos..].trim();
                    continue;
                }
            }

            // Pattern 4: key=value (no brackets/colons)
            if trimmed.contains('=') {
                if let Some(equals_pos) = trimmed.find('=') {
                    let key = trimmed[..equals_pos].trim();
                    let value_end_pos = trimmed.find(' ').unwrap_or(trimmed.len());

                    let value = trimmed[equals_pos + 1..value_end_pos].trim();
                    if !key.is_empty() && !key.contains(' ') {
                        metadata.insert(key.to_string(), value.to_string());
                    }
                    trimmed = trimmed[value_end_pos..].trim();
                    continue;
                }
            }

            break;
        }
    }

    metadata
}

/// Enhanced comment parser with metadata extraction
fn enhanced_comment_line(input: &str) -> ParseResult<'_, (String, HashMap<String, String>)> {
    map(preceded(alt((tag(";"), tag("#"), tag("*"), tag("|"))), take_until("\n")), |s: &str| {
        let comment = s.to_string();
        let metadata = parse_metadata_tags(&comment, None);
        (comment, metadata)
    })(input)
}

// ============================================================================
// Transaction Parsing
// ============================================================================

/// Parse a transaction entry
fn transaction_entry(input: &str) -> ParseResult<'_, Transaction> {
    context("transaction", parse_transaction)(input)
}

/// Parse a complete transaction with metadata support
fn parse_transaction(input: &str) -> ParseResult<'_, Transaction> {
    let mut parser = tuple((
        date_field,
        opt(preceded(tag("="), date_field)), // aux date
        space0,
        opt(alt((tag("*"), tag("!")))), // cleared flag
        space0,
        opt(delimited(tag("("), take_until(")"), tag(")"))), // code
        space0,
        opt(payee_description),
        space0,
        // transaction comment on payee line
        opt(simple_comment_field),
        line_ending,
        // transaction comments between payees and postings
        many0(delimited(alt((space1, tag("\t"))), simple_comment_field, line_ending)),
        many0(posting_line),
    ));

    let (rest, (date, aux_date, _, cleared, _, code, _, payee, _, comment1, _, comment2, postings)) =
        parser(input)?;

    let payee_str = payee.unwrap_or_else(String::new);
    let mut transaction = Transaction::new(date, payee_str);

    if let Some(aux_date) = aux_date {
        transaction.set_aux_date(Some(aux_date));
    }

    if let Some(cleared) = cleared {
        match cleared {
            "*" => transaction.set_status(crate::transaction::TransactionStatus::Cleared),
            "!" => transaction.set_status(crate::transaction::TransactionStatus::Pending),
            _ => {}
        }
    }

    if let Some(code) = code {
        transaction.set_code(Some(code.to_string()));
    }

    // Extract metadata from transaction comment
    if comment1.is_some() || !comment2.is_empty() {
        let comment = [comment1.unwrap_or_default(), comment2.join("\n")].join("\n");
        let metadata = parse_metadata_tags(&comment, None);

        transaction.note = Some(comment);

        if let Some(payee) = metadata.get("Payee") {
            transaction.payee = payee.clone();
        }

        let metadata: HashMap<String, TagData> =
            metadata.into_iter().map(|(key, value)| (key, TagData::new(value))).collect();
        transaction.metadata.extend(metadata);
    }

    for posting in postings {
        transaction.add_posting(posting);
    }

    Ok((rest, transaction))
}

/// Parse a date field
fn date_field(input: &str) -> ParseResult<'_, NaiveDate> {
    let date_with_optional_year = |sep| {
        tuple((
            opt(terminated(verify(digit1, |s: &str| s.len() == 4), tag(sep))),
            terminated(verify(digit1, |s: &str| (1..=2).contains(&s.len())), tag(sep)),
            terminated(verify(digit1, |s: &str| (1..=2).contains(&s.len())), not(tag(sep))),
        ))
    };

    map(
        alt((
            date_with_optional_year("/"),
            date_with_optional_year("-"),
            date_with_optional_year("."),
        )),
        |(year, month, day)| {
            let default_year = PARSE_STATE
                .with_borrow_mut(|c| c.current_year.unwrap_or_else(|| Local::now().year()));

            let year = if let Some(year) = year {
                year.parse::<i32>().unwrap_or(default_year)
            } else {
                default_year
            };
            let month = month.parse::<u32>().unwrap_or(1);
            let day = day.parse::<u32>().unwrap_or(1);

            NaiveDate::from_ymd_opt(year, month, day).unwrap()
        },
    )(input)
}

/// Parse payee description
fn payee_description(input: &str) -> ParseResult<'_, String> {
    map(is_not("\r\n;"), |s: &str| s.trim().to_string())(input)
}

/// Parse a posting line
fn posting_line(input: &str) -> ParseResult<'_, Posting> {
    context("posting", preceded(alt((space1, tag("\t"))), parse_posting))(input)
}

/// Parse a single posting with metadata support
pub(crate) fn parse_posting(input: &str) -> ParseResult<'_, Posting> {
    // For now, create a simplified version that compiles
    // TODO: Fix account reference creation and metadata handling
    let mut parser = tuple((
        account_name,
        opt(tuple((
            // amount
            preceded(pair(alt((tag("  "), tag("\t"))), space0), simple_amount_field),
            // lot price
            opt(delimited(
                space0,
                alt((
                    map(delimited(char('{'), amount_with_minimum_precision, char('}')), |a| {
                        (false, a)
                    }),
                    map(delimited(tag("{{"), amount_with_minimum_precision, tag("}}")), |a| {
                        (true, a)
                    }),
                )),
                space0,
            )),
            // cost
            opt(pair(
                delimited(space0, map(many_m_n(1, 2, char('@')), |r| r.len() == 2), space0),
                amount_with_minimum_precision,
            )),
        ))),
        opt(preceded(space0, simple_comment_field)),
        opt(line_ending),
    ));

    let (rest, (account, amount_price_cost, comment, _)) = parser(input)?;

    // Create a dummy account reference for now
    // TODO: Proper account management
    use compact_str::CompactString;
    let account_ref = std::rc::Rc::new(std::cell::RefCell::new(crate::account::Account::new(
        CompactString::from(account),
        None,
        0,
    )));
    let mut posting = Posting::new(account_ref);

    if let Some((mut amount, lot_price, cost)) = amount_price_cost {
        if let Some((calculate_price, mut price)) = lot_price {
            if calculate_price {
                price.div_amount(&amount).expect("dividing total price by qty");
            }

            if amount.has_commodity() {
                let mut commodity = Arc::unwrap_or_clone(amount.commodity().unwrap().clone());
                let mut annotation = commodity.annotation().clone();
                annotation.set_price(price);
                commodity.set_annotation(annotation);
                amount.set_commodity(Arc::new(commodity.clone()));
            } else {
                let annotation = Annotation::with_price(price);
                let commodity = Commodity::with_annotation("", annotation);
                amount.set_commodity(Arc::new(commodity));
            }
        }

        posting.amount = Some(amount);

        if let Some((calculate_cost, mut cost)) = cost {
            if calculate_cost {
                cost.div_amount(posting.amount.as_ref().unwrap())
                    .expect("dividing total cost by qty");
                posting.cost = Some(cost);
                posting.given_cost = None;
            } else {
                posting.cost = Some(cost.clone());
                posting.given_cost = Some(cost);
            }
        }
    }

    // TODO: confirm that amount.commodity != given_cost.commodity

    if let Some(comment) = comment {
        use compact_str::CompactString;
        posting.note = Some(CompactString::from(comment));
    }

    Ok((rest, posting))
}

/// Parse an account name
fn account_name(input: &str) -> ParseResult<'_, String> {
    fn parse_account_name(input: &str) -> IResult<&str, &str, VerboseError<&str>> {
        let mut last_char_was_space = false;
        for (i, char) in input.char_indices() {
            match char {
                '\t' | '\n' | ';' if i == 0 => {
                    return Err(nom::Err::Incomplete(nom::Needed::new(1)));
                }
                '\t' | '\n' | ';' | ' ' if last_char_was_space => {
                    let (parsed, rest) = input.split_at(i - 1);
                    return Ok((rest, parsed));
                }
                '\t' | '\n' | ';' => {
                    let (parsed, rest) = input.split_at(i);
                    return Ok((rest, parsed));
                }
                _ => {
                    last_char_was_space = char == ' ';
                }
            }
        }

        Ok(("", input))
    }

    map(parse_account_name, |s: &str| s.trim().to_string())(input)
}

/// Parse a commodity
fn commodity_symbol(input: &str) -> ParseResult<'_, String> {
    // Ref invalid_chars in commodity.cc
    let invalid_commodity_chars = " \t\r\n0123456789.,;:?!-+*/^&|=<>{}[]()@";

    alt((
        map(tuple((char('"'), is_not("\""), char('"'))), |(_, c, _)| format!(r#""{c}""#)),
        map(is_not(invalid_commodity_chars), str::to_string),
    ))(input)
}

#[derive(Debug, PartialEq)]
struct ParsedQuantity {
    format: ParsedQuantityFormat,
    decimal: Decimal,
    input: String,
    decimal_format: DecimalFormat,
    flags: Option<CommodityFlags>,
}

#[allow(clippy::enum_variant_names)]
#[derive(Clone, Copy, Debug, PartialEq)]
enum ParsedQuantityFormat {
    ThousandsAndDecimal,
    ThousandsNoDecimal,
    NoThousandsWithDecimal,
    NoThousandsNorDecimal,
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum DecimalFormat {
    /// eg 1.000,00
    Euro,
    /// eg 1,000.00
    US,
}

fn quantity(input: &str) -> IResult<&str, ParsedQuantity, VerboseError<&str>> {
    quantity_impl(input, QuantityFieldOptions::default())
}

fn quantity_with_full_precision(input: &str) -> IResult<&str, ParsedQuantity, VerboseError<&str>> {
    quantity_impl(input, QuantityFieldOptions { keep_full_precision: true, format_hint: None })
}

#[derive(Debug, Default)]
struct QuantityFieldOptions {
    keep_full_precision: bool,
    format_hint: Option<DecimalFormat>,
}

fn quantity_impl(
    input: &str,
    options: QuantityFieldOptions,
) -> IResult<&str, ParsedQuantity, VerboseError<&str>> {
    debug!("quantity_impl: {:?} {:?}", input, &options);

    let number_with_separator_and_decimal = |format| {
        let (separator, decimal) = match format {
            DecimalFormat::US => (',', '.'),
            DecimalFormat::Euro => ('.', ','),
        };
        tuple((
            map(
                pair(
                    take_while_m_n(1, 3, AsChar::is_dec_digit),
                    many1(preceded(char(separator), take_while_m_n(3, 3, AsChar::is_dec_digit))),
                ),
                move |(leading, rest)| {
                    (
                        format!("{}{}", leading, rest.join("")),
                        match format {
                            DecimalFormat::Euro => Some(
                                CommodityFlags::STYLE_DECIMAL_COMMA
                                    | CommodityFlags::STYLE_THOUSANDS,
                            ),
                            DecimalFormat::US => Some(CommodityFlags::STYLE_THOUSANDS),
                        },
                    )
                },
            ),
            preceded(char(decimal), digit1),
            success(ParsedQuantityFormat::ThousandsAndDecimal),
            success(format),
        ))
    };
    let number_with_separator_and_no_decimal = |format| {
        let separator = match format {
            DecimalFormat::US => ',',
            DecimalFormat::Euro => '.',
        };
        terminated(
            tuple((
                map(
                    pair(
                        take_while_m_n(1, 3, AsChar::is_dec_digit),
                        many1(preceded(
                            char(separator),
                            take_while_m_n(3, 3, AsChar::is_dec_digit),
                        )),
                    ),
                    move |(leading, rest)| {
                        (
                            format!("{}{}", leading, rest.join("")),
                            match format {
                                DecimalFormat::Euro => Some(
                                    CommodityFlags::STYLE_DECIMAL_COMMA
                                        | CommodityFlags::STYLE_THOUSANDS,
                                ),
                                DecimalFormat::US => Some(CommodityFlags::STYLE_THOUSANDS),
                            },
                        )
                    },
                ),
                success(""),
                success(ParsedQuantityFormat::ThousandsNoDecimal),
                success(format),
            )),
            // ensure we don't stop matching 1,2345 at the 4
            not(digit1),
        )
    };
    let number_without_separator_with_decimal = |format| {
        let decimal = match format {
            DecimalFormat::US => '.',
            DecimalFormat::Euro => ',',
        };
        tuple((
            map(digit1, move |s: &str| {
                (
                    s.to_string(),
                    match format {
                        DecimalFormat::Euro => Some(CommodityFlags::STYLE_DECIMAL_COMMA),
                        DecimalFormat::US => None,
                    },
                )
            }),
            preceded(char(decimal), digit1),
            success(ParsedQuantityFormat::NoThousandsWithDecimal),
            success(format),
        ))
    };
    let number_without_separator_or_decimal = tuple((
        map(digit1, |s: &str| (s.to_string(), None)),
        success(""), // no decimal portion
        success(ParsedQuantityFormat::NoThousandsNorDecimal),
        success(DecimalFormat::US),
    ));

    let result = match options.format_hint {
        Some(decimal_format) => {
            let mut parser = consumed(tuple((
                opt(tag("-")),
                space0,
                alt((
                    number_with_separator_and_decimal(decimal_format),
                    number_with_separator_and_no_decimal(decimal_format),
                    number_without_separator_with_decimal(decimal_format),
                    number_without_separator_or_decimal,
                )),
            )));

            parser(input)
        }
        None => {
            let mut parser = consumed(tuple((
                opt(tag("-")),
                space0,
                alt((
                    number_with_separator_and_decimal(DecimalFormat::US),
                    number_with_separator_and_decimal(DecimalFormat::Euro),
                    number_with_separator_and_no_decimal(DecimalFormat::US),
                    number_with_separator_and_no_decimal(DecimalFormat::Euro),
                    number_without_separator_with_decimal(DecimalFormat::US),
                    number_without_separator_with_decimal(DecimalFormat::Euro),
                    number_without_separator_or_decimal,
                )),
            )));

            parser(input)
        }
    };

    let (rest, (matched, (maybe_sign, _, ((integer, flags), fraction, format, decimal_format)))) =
        result?;
    let mut decimal_str = format!("{sign}{integer}", sign = maybe_sign.unwrap_or(""));

    if !fraction.is_empty() {
        let fraction = if options.keep_full_precision {
            fraction
        } else {
            fraction.strip_suffix("0").unwrap_or(fraction)
        };

        decimal_str.push('.');
        decimal_str.push_str(fraction);
    }

    Ok((
        rest,
        ParsedQuantity {
            format,
            decimal: Decimal::from_str(&decimal_str).unwrap_or_default(),
            input: matched.to_string(),
            decimal_format,
            flags,
        },
    ))
}

// See C++ version, these use migrate_format_to_commodity: false
fn _cost_amount(input: &str) -> ParseResult<'_, Amount> {
    amount_field_impl(input, AmountFieldOptions::default())
}
fn _balance_assertion_amount(input: &str) -> ParseResult<'_, Amount> {
    amount_field_impl(input, AmountFieldOptions::default())
}

fn default_commodity_amount(input: &str) -> ParseResult<'_, Amount> {
    amount_field_impl(
        input,
        AmountFieldOptions { migrate_format_to_commodity: true, keep_full_precision: true },
    )
}
fn amount_with_minimum_precision(input: &str) -> ParseResult<'_, Amount> {
    amount_field_impl(
        input,
        AmountFieldOptions { migrate_format_to_commodity: true, keep_full_precision: false },
    )
}

/// Parse a simple amount field, containing a quantity (number) and commodity
/// (symbol), in any order.
fn simple_amount_field(input: &str) -> ParseResult<'_, Amount> {
    amount_field_impl(
        input,
        AmountFieldOptions { migrate_format_to_commodity: true, keep_full_precision: true },
    )
}

#[derive(Debug, Default)]
struct AmountFieldOptions {
    migrate_format_to_commodity: bool,
    keep_full_precision: bool,
}

fn amount_field_impl(input: &str, options: AmountFieldOptions) -> ParseResult<'_, Amount> {
    debug!("amount_field_impl: {:?} {:?}", input, &options);

    let quantity_field =
        if options.keep_full_precision { quantity_with_full_precision } else { quantity };

    let mut parser = alt((
        // Commodity before quantity: GBP1, -$1000.00, €-500.50
        // FIXME: what about -$-1?
        map(
            tuple((opt(terminated(char('-'), space0)), commodity_symbol, space0, quantity_field)),
            |(maybe_sign, commodity, maybe_sep, mut quantity)| {
                if maybe_sign.is_some() {
                    quantity.decimal.set_sign_negative(true);
                }

                let mut flags = quantity.flags.unwrap_or(CommodityFlags::empty());
                if !maybe_sep.is_empty() {
                    flags |= CommodityFlags::STYLE_SEPARATED;
                }
                if !flags.is_empty() {
                    quantity.flags = Some(flags)
                }

                (quantity, Some(commodity))
            },
        ),
        // Commodity after quantity, and no commodity at all: 10.00 USD, -10.00, 1$
        map(
            tuple((quantity_field, opt(pair(space0, commodity_symbol)))),
            |(mut quantity, commodity)| {
                let mut flags = quantity.flags.unwrap_or(CommodityFlags::empty());

                let commodity = if let Some((maybe_sep, commodity)) = commodity {
                    flags |= CommodityFlags::STYLE_SUFFIXED;
                    if !maybe_sep.is_empty() {
                        flags |= CommodityFlags::STYLE_SEPARATED;
                    }
                    Some(commodity)
                } else {
                    None
                };

                if !flags.is_empty() {
                    quantity.flags = Some(flags)
                }

                (quantity, commodity)
            },
        ),
    ));

    let (rest, (mut quantity, commodity_symbol)) = parser(input)?;

    debug!("initial qty: {:#?}", &quantity);
    debug!("detected cmdty: {:?}", &commodity_symbol);

    let commodity = if let Some(commodity_symbol) = commodity_symbol {
        let commodity =
            PARSE_STATE.with_borrow_mut(|c| c.commodity_pool.find_or_create(&commodity_symbol));
        if ["€", "$"].contains(&commodity.symbol()) {
            debug!(
                "existing cmdty: {} {} {:#?}",
                commodity.symbol(),
                commodity.precision(),
                commodity.flags()
            );
        }

        let existing_format_is_euro = commodity.has_flags(CommodityFlags::STYLE_DECIMAL_COMMA);
        let try_format = match quantity.decimal_format {
            DecimalFormat::US if existing_format_is_euro => DecimalFormat::Euro,
            DecimalFormat::Euro if !existing_format_is_euro => DecimalFormat::US,
            DecimalFormat::US | DecimalFormat::Euro => quantity.decimal_format,
        };
        let possible_incorrect_parse = try_format != quantity.decimal_format;

        debug!(
            "cmdt euro:{:?} try:{:?} mismatch:{:?}",
            existing_format_is_euro, try_format, possible_incorrect_parse
        );

        match quantity.format {
            // eg 123,456 could be US ThousandsNoDecimal (eg 123,456.00) or
            // Euro NoThousandsWithDecimal (eg 123.456)
            ParsedQuantityFormat::ThousandsNoDecimal
                if !commodity.has_flags(CommodityFlags::STYLE_NO_MIGRATE)
                    && possible_incorrect_parse =>
            {
                if let Ok((_, quantity_retry)) = quantity_impl(
                    &quantity.input,
                    QuantityFieldOptions {
                        keep_full_precision: options.keep_full_precision,
                        format_hint: Some(try_format),
                    },
                ) {
                    if ["€", "$"].contains(&commodity.symbol()) {
                        debug!("2nd attempt qty: {:#?}", &quantity_retry);
                    }

                    quantity.decimal = quantity_retry.decimal;

                    // migrate flags from new/retry qty to first try qty
                    // because first try may have commodity format flags and
                    // retry will not
                    match (quantity.flags, quantity_retry.flags) {
                        // combine 2nd attempt qty flags w/ existing qty flags
                        (Some(mut existing_flags), Some(new_flags)) => {
                            let commodity_format_flags = CommodityFlags::STYLE_THOUSANDS
                                | CommodityFlags::STYLE_DECIMAL_COMMA;
                            existing_flags &= !commodity_format_flags;
                            existing_flags |= new_flags & commodity_format_flags;
                            quantity.flags = Some(existing_flags);
                        }

                        // apply 2nd attempt qty flags
                        (None, Some(new_flags)) => {
                            quantity.flags = Some(new_flags);
                        }

                        // remove 2nd attempt qty flags from existing flags
                        (Some(mut existing_flags), None) => {
                            let commodity_format_flags = CommodityFlags::STYLE_THOUSANDS
                                | CommodityFlags::STYLE_DECIMAL_COMMA;
                            existing_flags &= !commodity_format_flags;
                            quantity.flags = Some(existing_flags);
                        }

                        (None, None) => {}
                    }

                    debug!("updated qty: {:#?}", &quantity);
                }
            }

            ParsedQuantityFormat::ThousandsNoDecimal
            | ParsedQuantityFormat::NoThousandsWithDecimal => {}

            // these are unambiguous
            ParsedQuantityFormat::NoThousandsNorDecimal
            | ParsedQuantityFormat::ThousandsAndDecimal => {}
        }

        let commodity = if options.migrate_format_to_commodity {
            let mut commodity = Arc::unwrap_or_clone(commodity);
            commodity.add_flags(quantity.flags.unwrap_or(CommodityFlags::empty()));
            commodity
                .set_precision(commodity.precision().max(quantity.decimal.scale() as Precision));
            if ["€", "$"].contains(&commodity.symbol()) {
                debug!(
                    "final cmdty: {} {} {:#?}",
                    commodity.symbol(),
                    commodity.precision(),
                    commodity.flags()
                );
            }
            PARSE_STATE.with_borrow_mut(|c| c.commodity_pool.insert(commodity))
        } else {
            commodity
        };

        Some(commodity)
    } else {
        None
    };

    let mut amount = Amount::with_commodity(quantity.decimal, commodity);
    amount.set_keep_precision(!options.migrate_format_to_commodity);

    Ok((rest, amount))
}

/// Parse a comment field with metadata extraction
#[allow(dead_code)]
fn comment_field(input: &str) -> ParseResult<'_, (String, HashMap<String, String>)> {
    map(preceded(tag(";"), take_until("\n")), |s: &str| {
        let comment = s.trim().to_string();
        let metadata = parse_metadata_tags(&comment, None);
        (comment, metadata)
    })(input)
}

/// Simple comment field parser (for backward compatibility)
fn simple_comment_field(input: &str) -> ParseResult<'_, String> {
    map(preceded(tag(";"), take_until("\n")), |s: &str| s.trim().to_string())(input)
}

// ============================================================================
// Directive Parsing
// ============================================================================

/// Parse a directive entry
fn directive_entry(input: &str) -> ParseResult<'_, Directive> {
    context("directive", parse_directive)(input)
}

/// Parse any directive
fn parse_directive(input: &str) -> ParseResult<'_, Directive> {
    map(
        alt((
            account_directive,
            commodity_directive,
            include_directive,
            price_directive,
            alias_directive,
            apply_directive,
            end_directive,
            payee_directive,
            tag_directive,
            option_directive,
            eval_directive,
            define_directive,
            year_directive,
            default_commodity_directive,
            assert_directive,
            check_directive,
            periodic_transaction,
            automated_transaction,
        )),
        process_directive_inline,
    )(input)
}

/// Process directives inline, as they are parsed.
fn process_directive_inline(directive: Directive) -> Directive {
    match directive {
        Directive::Year { year } => {
            PARSE_STATE.with_borrow_mut(|c| c.current_year = Some(year));
        }
        Directive::DefaultCommodity { ref commodity } => {
            PARSE_STATE
                .with_borrow_mut(|c| c.commodity_pool.set_default_commodity(commodity.clone()));
        }

        // no inline processing for these directives
        Directive::Account { .. }
        | Directive::Alias { .. }
        | Directive::Apply { .. }
        | Directive::End
        | Directive::Assert { .. }
        | Directive::Check { .. }
        | Directive::Commodity { .. }
        | Directive::Price { .. }
        | Directive::Include { .. }
        | Directive::ConditionalInclude { .. }
        | Directive::Payee { .. }
        | Directive::Tag { .. }
        | Directive::Option { .. }
        | Directive::Eval { .. }
        | Directive::Define { .. }
        | Directive::PeriodicTransaction { .. }
        | Directive::AutomatedTransaction { .. } => {}
    }
    directive
}

/// Parse account directive
fn account_directive(input: &str) -> ParseResult<'_, Directive> {
    map(
        tuple((tag("account"), space1, account_name, line_ending, many0(account_declaration))),
        |(_, _, name, _, declarations)| Directive::Account { name, declarations },
    )(input)
}

/// Parse account declarations
fn account_declaration(input: &str) -> ParseResult<'_, AccountDeclaration> {
    preceded(
        alt((space1, tag("\t"))),
        alt((
            map(preceded(tag("alias"), preceded(space1, take_until("\n"))), |alias: &str| {
                AccountDeclaration::Alias(alias.trim().to_string())
            }),
            map(preceded(tag("payee"), preceded(space1, take_until("\n"))), |payee: &str| {
                AccountDeclaration::Payee(payee.trim().to_string())
            }),
            value(AccountDeclaration::Default, tag("default")),
        )),
    )(input)
}

/// Parse commodity directive
fn commodity_directive(input: &str) -> ParseResult<'_, Directive> {
    map(
        tuple((
            tag("commodity"),
            space1,
            take_until("\n"),
            line_ending,
            many0(commodity_declaration),
        )),
        |(_, _, symbol, _, declarations)| Directive::Commodity {
            symbol: symbol.trim().to_string(),
            declarations,
        },
    )(input)
}

/// Parse commodity declarations
fn commodity_declaration(input: &str) -> ParseResult<'_, CommodityDeclaration> {
    preceded(
        alt((space1, tag("\t"))),
        alt((
            map(preceded(tag("alias"), preceded(space1, take_until("\n"))), |alias: &str| {
                CommodityDeclaration::Alias(alias.trim().to_string())
            }),
            map(preceded(tag("format"), preceded(space1, take_until("\n"))), |format: &str| {
                CommodityDeclaration::Format(format.trim().to_string())
            }),
            value(CommodityDeclaration::NoMarket, tag("nomarket")),
            value(CommodityDeclaration::Default, tag("default")),
        )),
    )(input)
}

/// Parse include directive
fn include_directive(input: &str) -> ParseResult<'_, Directive> {
    alt((conditional_include_directive, simple_include_directive))(input)
}

/// Parse simple include directive
fn simple_include_directive(input: &str) -> ParseResult<'_, Directive> {
    map(preceded(tag("include"), preceded(space1, take_until("\n"))), |path: &str| {
        Directive::Include { path: PathBuf::from(path.trim()) }
    })(input)
}

/// Parse conditional include directive
fn conditional_include_directive(input: &str) -> ParseResult<'_, Directive> {
    map(
        tuple((
            tag("include"),
            space1,
            delimited(tag("["), take_until("]"), tag("]")),
            space1,
            take_until("\n"),
        )),
        |(_, _, condition, _, path)| Directive::ConditionalInclude {
            condition: condition.trim().to_string(),
            path: PathBuf::from(path.trim()),
        },
    )(input)
}

/// Parse price directive
fn price_directive(input: &str) -> ParseResult<'_, Directive> {
    map(
        tuple((tag("P"), space1, date_field, space1, take_until(" "), space1, simple_amount_field)),
        |(_, _, date, _, commodity, _, price)| Directive::Price {
            date,
            commodity: commodity.to_string(),
            price,
        },
    )(input)
}

/// Parse alias directive
fn alias_directive(input: &str) -> ParseResult<'_, Directive> {
    map(
        tuple((tag("alias"), space1, take_until("="), tag("="), take_until("\n"))),
        |(_, _, account, _, alias)| Directive::Alias {
            account: account.trim().to_string(),
            alias: alias.trim().to_string(),
        },
    )(input)
}

/// Parse apply directive
fn apply_directive(input: &str) -> ParseResult<'_, Directive> {
    map(preceded(tag("apply account"), preceded(space1, take_until("\n"))), |account: &str| {
        Directive::Apply { state: ApplyState::Account(account.trim().to_string()) }
    })(input)
}

/// Parse end directive
fn end_directive(input: &str) -> ParseResult<'_, Directive> {
    value(Directive::End, tag("end"))(input)
}

/// Parse payee directive
fn payee_directive(input: &str) -> ParseResult<'_, Directive> {
    map(
        tuple((tag("payee"), space1, take_until("\n"), line_ending, many0(payee_declaration))),
        |(_, _, name, _, declarations)| Directive::Payee {
            name: name.trim().to_string(),
            declarations,
        },
    )(input)
}

/// Parse payee declarations
fn payee_declaration(input: &str) -> ParseResult<'_, PayeeDeclaration> {
    preceded(
        alt((space1, tag("\t"))),
        alt((
            map(preceded(tag("alias"), preceded(space1, take_until("\n"))), |alias: &str| {
                PayeeDeclaration::Alias(alias.trim().to_string())
            }),
            map(preceded(tag("uuid"), preceded(space1, take_until("\n"))), |uuid: &str| {
                PayeeDeclaration::Uuid(uuid.trim().to_string())
            }),
        )),
    )(input)
}

/// Parse tag directive
fn tag_directive(input: &str) -> ParseResult<'_, Directive> {
    map(preceded(tag("tag"), preceded(space1, take_until("\n"))), |name: &str| Directive::Tag {
        name: name.trim().to_string(),
    })(input)
}

/// Parse option directive
fn option_directive(input: &str) -> ParseResult<'_, Directive> {
    map(
        tuple((tag("option"), space1, take_until(" "), space1, take_until("\n"))),
        |(_, _, name, _, value)| Directive::Option {
            name: name.trim().to_string(),
            value: value.trim().to_string(),
        },
    )(input)
}

/// Parse eval directive
fn eval_directive(input: &str) -> ParseResult<'_, Directive> {
    map(preceded(tag("eval"), preceded(space1, take_until("\n"))), |expression: &str| {
        Directive::Eval { expression: expression.trim().to_string() }
    })(input)
}

/// Parse define directive
fn define_directive(input: &str) -> ParseResult<'_, Directive> {
    map(
        tuple((tag("define"), space1, take_until("="), tag("="), take_until("\n"))),
        |(_, _, name, _, expression)| Directive::Define {
            name: name.trim().to_string(),
            expression: expression.trim().to_string(),
        },
    )(input)
}

/// Parse year directive
fn year_directive(input: &str) -> ParseResult<'_, Directive> {
    // FIXME: invalid year should be an error
    map(
        preceded(
            pair(alt((tag("year"), tag("Y"))), space1),
            verify(digit1, |s: &str| s.len() == 4),
        ),
        |year_str: &str| Directive::Year { year: year_str.parse().unwrap_or(2024) },
    )(input)
}

/// Parse default commodity directive
fn default_commodity_directive(input: &str) -> ParseResult<'_, Directive> {
    map(
        preceded(
            pair(tag("D"), space1),
            verify(default_commodity_amount, |amount| amount.commodity().is_some()),
        ),
        |amount| Directive::DefaultCommodity { commodity: amount.commodity().unwrap().clone() },
    )(input)
}

/// Parse assert directive
fn assert_directive(input: &str) -> ParseResult<'_, Directive> {
    map(preceded(tag("assert"), preceded(space1, take_until("\n"))), |condition: &str| {
        Directive::Assert { condition: condition.trim().to_string() }
    })(input)
}

/// Parse check directive
fn check_directive(input: &str) -> ParseResult<'_, Directive> {
    map(preceded(tag("check"), preceded(space1, take_until("\n"))), |condition: &str| {
        Directive::Check { condition: condition.trim().to_string() }
    })(input)
}

/// Parse periodic transaction (starts with ~)
fn periodic_transaction(input: &str) -> ParseResult<'_, Directive> {
    map(
        tuple((tag("~"), space0, take_until("\n"), line_ending, many0(posting_line))),
        |(_, _, period, _, postings)| Directive::PeriodicTransaction {
            period: period.trim().to_string(),
            postings,
            metadata: HashMap::new(),
        },
    )(input)
}

/// Parse automated transaction (starts with =)
fn automated_transaction(input: &str) -> ParseResult<'_, Directive> {
    map(
        tuple((tag("="), space0, take_until("\n"), line_ending, many0(posting_line))),
        |(_, _, condition, _, postings)| Directive::AutomatedTransaction {
            condition: condition.trim().to_string(),
            postings,
            metadata: HashMap::new(),
        },
    )(input)
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use insta::assert_debug_snapshot;

    use crate::transaction::TransactionStatus;

    use super::*;

    fn init() {
        let _ = env_logger::builder().is_test(true).try_init();
        reset_parse_state();
    }

    #[test]
    fn test_empty_line() {
        let input = "  \n";
        let result = empty_line(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_comment_line() {
        let input = "; This is a comment\n";
        let result = comment_line(input);
        assert!(result.is_ok());
        let (_, comment) = result.unwrap();
        assert_eq!(comment, " This is a comment");
    }

    #[test]
    fn test_account_name() {
        let (_, name) = account_name("Assets:Checking").unwrap();
        assert_eq!(name, "Assets:Checking");

        let (_, name) = account_name("Assets:Savings Account").unwrap();
        assert_eq!(name, "Assets:Savings Account");

        let (_, name) = account_name("Assets:Savings  USD1").unwrap();
        assert_eq!(name, "Assets:Savings");

        let (_, name) = account_name("Assets:Savings 1  USD1").unwrap();
        assert_eq!(name, "Assets:Savings 1");
    }

    #[test]
    fn test_account_directive() {
        let input = "account Assets:Checking\n";
        let (_, directive) = account_directive(input).unwrap();

        if let Directive::Account { name, .. } = directive {
            assert_eq!(name, "Assets:Checking");
        } else {
            panic!("did not parse account directive: {directive:?}");
        }
    }

    #[test]
    fn test_metadata_parsing_colon_style() {
        let comment = " :Receipt: 12345 :Project: ABC";
        let metadata = parse_metadata_tags(comment, None);
        assert_eq!(metadata.get("Receipt"), Some(&"12345".to_string()));
        assert_eq!(metadata.get("Project"), Some(&"ABC".to_string()));
    }

    #[test]
    fn test_metadata_parsing_bracket_style() {
        let comment = " [Receipt: 12345] [Project=ABC]";
        let metadata = parse_metadata_tags(comment, None);
        assert_eq!(metadata.get("Receipt"), Some(&"12345".to_string()));
        assert_eq!(metadata.get("Project"), Some(&"ABC".to_string()));
    }

    #[test]
    fn test_metadata_parsing_equals_style() {
        let comment = " Receipt=12345 Project=ABC";
        let metadata = parse_metadata_tags(comment, None);
        assert_eq!(metadata.get("Receipt"), Some(&"12345".to_string()));
        assert_eq!(metadata.get("Project"), Some(&"ABC".to_string()));
    }

    #[test]
    fn test_enhanced_comment_line() {
        let input = "; This is a comment :Receipt: 12345\n";
        let result = enhanced_comment_line(input);
        assert!(result.is_ok());
        let (_, (comment, metadata)) = result.unwrap();
        assert_eq!(comment, " This is a comment :Receipt: 12345");
        assert_eq!(metadata.get("Receipt"), Some(&"12345".to_string()));
    }

    #[test]
    fn test_include_directive() {
        let input = "include expenses.ledger\n";
        let result = include_directive(input).unwrap();

        if let (_, Directive::Include { path }) = result {
            assert_eq!(path, PathBuf::from("expenses.ledger"));
        } else {
            panic!("include result did not parse correctly: {result:?}");
        }
    }

    #[test]
    fn test_simple_include_directive() {
        let input = "include expenses.dat\n";
        let result = simple_include_directive(input);
        assert!(result.is_ok());
        if let (_, Directive::Include { path }) = result.unwrap() {
            assert_eq!(path, PathBuf::from("expenses.dat"));
        } else {
            panic!("Expected Include directive");
        }
    }

    #[test]
    fn test_conditional_include_directive() {
        let input = "include [exists(\"optional.dat\")] optional.dat\n";
        let result = conditional_include_directive(input);
        assert!(result.is_ok());
        if let (_, Directive::ConditionalInclude { condition, path }) = result.unwrap() {
            assert_eq!(condition, "exists(\"optional.dat\")");
            assert_eq!(path, PathBuf::from("optional.dat"));
        } else {
            panic!("Expected ConditionalInclude directive");
        }
    }

    #[test]
    fn test_include_cycle_detection() {
        let mut parser = JournalParser::with_file("main.dat");
        parser.context.include_stack.push(PathBuf::from("main.dat"));

        // This should detect a cycle
        let result = parser.parse_file("main.dat");
        assert!(result.is_err());

        if let Err(JournalParseError::IncludeCycle { path, include_stack: _ }) = result {
            assert_eq!(path, PathBuf::from("main.dat"));
        } else {
            panic!("Expected IncludeCycle error");
        }
    }

    #[test]
    fn test_error_recovery_skip_invalid_line() {
        let input = "invalid line here\n2024/01/01 Valid transaction\n    Assets:Cash   $100\n";
        let result = skip_invalid_line(input);
        assert!(result.is_ok());
        let (remaining, _) = result.unwrap();
        assert!(remaining.starts_with("2024/01/01"));
    }

    #[test]
    fn test_validation_error_empty_transaction() {
        let parser = JournalParser::new();
        let transaction = Transaction::new(
            chrono::NaiveDate::from_ymd_opt(2024, 1, 1).unwrap(),
            "Test transaction".to_string(),
        );
        // Don't add any postings

        let result = parser.validate_transaction(&transaction);
        assert!(result.is_err());

        if let Err(JournalParseError::ValidationError { message, .. }) = result {
            assert!(message.contains("no postings"));
        } else {
            panic!("Expected ValidationError");
        }
    }

    #[test]
    fn test_line_column_calculation() {
        let parser = JournalParser::new();
        let input = "line 1\nline 2\nline 3 with error here";
        let error_pos = "line 3 with error here";

        let (line, column) = parser.get_line_column(input, error_pos);
        assert_eq!(line, 3);
        assert_eq!(column, 1);
    }

    #[test]
    fn test_year_directive() {
        let (_, year) = year_directive("year 1999").unwrap();
        let Directive::Year { year } = year else {
            panic!("did not parse `year` directive");
        };

        assert_eq!(1999, year);

        let (_, year) = year_directive("Y 2020").unwrap();
        let Directive::Year { year } = year else {
            panic!("did not parse `Y` directive");
        };

        assert_eq!(2020, year);
    }

    #[test]
    fn test_parse_dates() {
        let (_, date) = date_field("2023/01/02").unwrap();
        assert_eq!(2023, date.year());
        assert_eq!(1, date.month());
        assert_eq!(2, date.day());
        let (_, date1) = date_field("2023.01.02").unwrap();
        assert_eq!(date, date1);
        let (_, date2) = date_field("2023-01-02").unwrap();
        assert_eq!(date, date2);

        // TODO: invalid
        // let (_, date) = date_field("2023-01/02").unwrap();

        // year optional
        PARSE_STATE.with_borrow_mut(|c| c.current_year = Some(1999));
        let (_, date) = date_field("02/03").unwrap();
        assert_eq!(1999, date.year());
        assert_eq!(2, date.month());
        assert_eq!(3, date.day());
        let (_, date1) = date_field("02.03").unwrap();
        assert_eq!(date, date1);
        let (_, date2) = date_field("02-03").unwrap();
        assert_eq!(date, date2);

        // 1 digit day & month
        PARSE_STATE.with_borrow_mut(|c| c.current_year = Some(2005));
        let (_, date) = date_field("4/5").unwrap();
        assert_eq!(2005, date.year());
        assert_eq!(4, date.month());
        assert_eq!(5, date.day());

        // TODO: current year is default; mock now() for testing (ie #[cfg(test)])
        // let (_, date) = date_field("02/03").unwrap();
        // assert_eq!(2025, date.year());
        // assert_eq!(2, date.month());
        // assert_eq!(3, date.day());

        // 2 digit years are not allowed
        assert!(date_field("25/04/05").is_err());
    }

    #[test]
    fn test_parse_default_commodity() {
        let (_, commodity) = default_commodity_directive("D 1.00 $").unwrap();

        let Directive::DefaultCommodity { commodity } = commodity else {
            panic!("did not parse `D` (default commodity) directive");
        };

        assert_eq!("$", commodity.symbol());
        assert_eq!(
            CommodityFlags::STYLE_SUFFIXED | CommodityFlags::STYLE_SEPARATED,
            commodity.flags()
        );
    }

    #[test]
    fn test_parse_commodity() {
        let (_, commodity) = commodity_symbol("USD").unwrap();
        assert_eq!("USD", commodity);

        let (_, commodity) = commodity_symbol("$1").unwrap();
        assert_eq!("$", commodity);

        let (_, commodity) = commodity_symbol("€1").unwrap();
        assert_eq!("€", commodity);

        let (_, commodity) = commodity_symbol("€").unwrap();
        assert_eq!("€", commodity);

        let (_, commodity) = commodity_symbol(r#""M&M""#).unwrap();
        assert_eq!(r#""M&M""#, commodity);
    }

    #[test]
    fn test_parse_quantity() {
        assert_eq!(
            quantity("1000"),
            Ok((
                "",
                ParsedQuantity {
                    format: ParsedQuantityFormat::NoThousandsNorDecimal,
                    decimal: Decimal::new(1000, 0),
                    input: "1000".to_string(),
                    decimal_format: DecimalFormat::US,
                    flags: None
                }
            ))
        );

        assert_eq!(
            quantity("2.02"),
            Ok((
                "",
                ParsedQuantity {
                    format: ParsedQuantityFormat::NoThousandsWithDecimal,
                    decimal: Decimal::new(202, 2),
                    input: "2.02".to_string(),
                    decimal_format: DecimalFormat::US,
                    flags: None
                }
            ))
        );

        assert_eq!(
            quantity("-12.13"),
            Ok((
                "",
                ParsedQuantity {
                    format: ParsedQuantityFormat::NoThousandsWithDecimal,
                    decimal: Decimal::new(-1213, 2),
                    input: "-12.13".to_string(),
                    decimal_format: DecimalFormat::US,
                    flags: None
                }
            ))
        );

        assert_eq!(
            quantity("0.1"),
            Ok((
                "",
                ParsedQuantity {
                    format: ParsedQuantityFormat::NoThousandsWithDecimal,
                    decimal: Decimal::new(1, 1),
                    input: "0.1".to_string(),
                    decimal_format: DecimalFormat::US,
                    flags: None
                }
            ))
        );

        assert_eq!(
            quantity("3"),
            Ok((
                "",
                ParsedQuantity {
                    format: ParsedQuantityFormat::NoThousandsNorDecimal,
                    decimal: Decimal::new(3, 0),
                    input: "3".to_string(),
                    decimal_format: DecimalFormat::US,
                    flags: None
                }
            ))
        );

        assert_eq!(
            quantity("1"),
            Ok((
                "",
                ParsedQuantity {
                    format: ParsedQuantityFormat::NoThousandsNorDecimal,
                    decimal: Decimal::new(1, 0),
                    input: "1".to_string(),
                    decimal_format: DecimalFormat::US,
                    flags: None
                }
            ))
        );

        assert_eq!(
            quantity("1 ABC"),
            Ok((
                " ABC",
                ParsedQuantity {
                    format: ParsedQuantityFormat::NoThousandsNorDecimal,
                    decimal: Decimal::new(1, 0),
                    input: "1".to_string(),
                    decimal_format: DecimalFormat::US,
                    flags: None
                }
            ))
        );

        assert_debug_snapshot!(
            quantity("1,000").unwrap(),
            @r#"
                (
                    "",
                    ParsedQuantity {
                        format: ThousandsNoDecimal,
                        decimal: 1000,
                        input: "1,000",
                        decimal_format: US,
                        flags: Some(
                            CommodityFlags(
                                STYLE_THOUSANDS,
                            ),
                        ),
                    },
                )
            "#
        );
        assert_debug_snapshot!(
            quantity("-188,7974").unwrap(),
            @r#"
                (
                    "",
                    ParsedQuantity {
                        format: NoThousandsWithDecimal,
                        decimal: -188.7974,
                        input: "-188,7974",
                        decimal_format: Euro,
                        flags: Some(
                            CommodityFlags(
                                STYLE_DECIMAL_COMMA,
                            ),
                        ),
                    },
                )
            "#
        );
        assert_debug_snapshot!(
            quantity("12,456,132.14").unwrap(),
            @r#"
                (
                    "",
                    ParsedQuantity {
                        format: ThousandsAndDecimal,
                        decimal: 12456132.14,
                        input: "12,456,132.14",
                        decimal_format: US,
                        flags: Some(
                            CommodityFlags(
                                STYLE_THOUSANDS,
                            ),
                        ),
                    },
                )
            "#
        );
        assert_debug_snapshot!(
            quantity("12.456,14").unwrap(),
            @r#"
                (
                    "",
                    ParsedQuantity {
                        format: ThousandsAndDecimal,
                        decimal: 12456.14,
                        input: "12.456,14",
                        decimal_format: Euro,
                        flags: Some(
                            CommodityFlags(
                                STYLE_DECIMAL_COMMA | STYLE_THOUSANDS,
                            ),
                        ),
                    },
                )
            "#
        );
    }

    #[test]
    fn test_parse_amount() {
        init();

        {
            let (_, amount) = simple_amount_field("1").unwrap();
            insta::assert_debug_snapshot!(amount, @r#"
                AMOUNT(1) [prec:0, keep:false, raw:1]
            "#);
        }

        {
            reset_parse_state();
            let (_, amount) = simple_amount_field("-1").unwrap();
            insta::assert_debug_snapshot!(amount, @r#"
                AMOUNT(-1) [prec:0, keep:false, raw:-1]
            "#);
        }

        {
            reset_parse_state();
            let (_, amount) = simple_amount_field("$1").unwrap();
            insta::assert_debug_snapshot!(amount, @r#"
                AMOUNT($1) [prec:0, keep:false, comm:$, raw:1]
            "#);
            assert_eq!(CommodityFlags::empty(), amount.commodity().unwrap().flags());
        }

        {
            reset_parse_state();
            let (_, amount) = simple_amount_field("1$").unwrap();
            insta::assert_debug_snapshot!(amount, @r#"
                AMOUNT(1$) [prec:0, keep:false, comm:$, raw:1]
            "#);
            assert_eq!(CommodityFlags::STYLE_SUFFIXED, amount.commodity().unwrap().flags());
        }

        {
            reset_parse_state();
            let (_, amount) = simple_amount_field("-$1").unwrap();
            insta::assert_debug_snapshot!(amount, @r#"
                AMOUNT($-1) [prec:0, keep:false, comm:$, raw:-1]
            "#);
            assert_eq!(CommodityFlags::empty(), amount.commodity().unwrap().flags());
        }

        {
            reset_parse_state();
            let (_, amount) = simple_amount_field("$-1").unwrap();
            insta::assert_debug_snapshot!(amount, @r#"
                AMOUNT($-1) [prec:0, keep:false, comm:$, raw:-1]
            "#);
            assert_eq!(CommodityFlags::empty(), amount.commodity().unwrap().flags());
        }

        {
            reset_parse_state();
            let (_, amount) = simple_amount_field("$- 1").unwrap();
            insta::assert_debug_snapshot!(amount, @r#"
                AMOUNT($-1) [prec:0, keep:false, comm:$, raw:-1]
            "#);
            assert_eq!(CommodityFlags::empty(), amount.commodity().unwrap().flags());
        }

        {
            reset_parse_state();
            let (_, amount) = simple_amount_field("$ -1").unwrap();
            insta::assert_debug_snapshot!(amount, @r#"
                AMOUNT($ -1) [prec:0, keep:false, comm:$, raw:-1]
            "#);
            assert_eq!(CommodityFlags::STYLE_SEPARATED, amount.commodity().unwrap().flags());
        }

        {
            reset_parse_state();
            let (_, amount) = simple_amount_field("$ 1").unwrap();
            insta::assert_debug_snapshot!(amount, @r#"
                AMOUNT($ 1) [prec:0, keep:false, comm:$, raw:1]
            "#);
            assert_eq!(CommodityFlags::STYLE_SEPARATED, amount.commodity().unwrap().flags());
        }

        {
            reset_parse_state();
            let (_, amount) = simple_amount_field("1 USD").unwrap();
            insta::assert_debug_snapshot!(amount, @r#"
                AMOUNT(1 USD) [prec:0, keep:false, comm:USD, raw:1]
            "#);
            assert_eq!(
                CommodityFlags::STYLE_SEPARATED | CommodityFlags::STYLE_SUFFIXED,
                amount.commodity().unwrap().flags()
            );
        }

        {
            reset_parse_state();
            let (_, amount) = simple_amount_field("1USD").unwrap();
            insta::assert_debug_snapshot!(amount, @r#"
                AMOUNT(1USD) [prec:0, keep:false, comm:USD, raw:1]
            "#);
            assert_eq!(CommodityFlags::STYLE_SUFFIXED, amount.commodity().unwrap().flags());
        }

        {
            reset_parse_state();
            let (_, amount) = simple_amount_field(r#"1000 "M&M""#).unwrap();
            insta::assert_debug_snapshot!(amount, @r#"
                AMOUNT(1000 "M&M") [prec:0, keep:false, comm:"M&M", raw:1000]
            "#);
            assert_eq!(
                CommodityFlags::STYLE_SEPARATED | CommodityFlags::STYLE_SUFFIXED,
                amount.commodity().unwrap().flags()
            );
        }

        {
            reset_parse_state();
            let (_, amount) = simple_amount_field("-1672,42 $").unwrap();
            insta::assert_debug_snapshot!(amount, @r#"
                AMOUNT(-1672.42 $) [prec:2, keep:false, comm:$, raw:-83621/50]
            "#);
            assert_eq!(
                CommodityFlags::STYLE_SEPARATED
                    | CommodityFlags::STYLE_SUFFIXED
                    | CommodityFlags::STYLE_DECIMAL_COMMA,
                amount.commodity().unwrap().flags()
            );
        }

        // FIXME: this parses as 358,800.00 (US) but it should be 358.800 (Euro)
        {
            reset_parse_state();

            let mut commodity = Commodity::new("€");
            commodity.add_flags(CommodityFlags::STYLE_DECIMAL_COMMA);
            PARSE_STATE.with_borrow_mut(|c| c.commodity_pool.insert(commodity));

            let (_, amount) = simple_amount_field("358,800 €").unwrap();
            insta::assert_debug_snapshot!(amount, @r#"
                AMOUNT(358.800 €) [prec:3, keep:false, comm:€, raw:1794/5]
            "#);
            assert_eq!(3, amount.commodity().unwrap().precision());
            assert_eq!(
                CommodityFlags::STYLE_SUFFIXED
                    | CommodityFlags::STYLE_SEPARATED
                    | CommodityFlags::STYLE_DECIMAL_COMMA,
                amount.commodity().unwrap().flags()
            );

            // commodity pool is also updated
            let commodity = PARSE_STATE.with_borrow_mut(|c| c.commodity_pool.find("€").unwrap());
            assert_eq!(
                CommodityFlags::STYLE_SUFFIXED
                    | CommodityFlags::STYLE_SEPARATED
                    | CommodityFlags::STYLE_DECIMAL_COMMA,
                commodity.flags()
            );
        }

        {
            reset_parse_state();
            let (rem, amount) = simple_amount_field("-188,7974 STK @ 14,200 $").unwrap();
            assert_eq!(rem, " @ 14,200 $");
            insta::assert_debug_snapshot!(amount, @r#"
                AMOUNT(-188.7974 STK) [prec:4, keep:false, comm:STK, raw:-943987/5000]
            "#);
            assert_eq!(
                CommodityFlags::STYLE_SUFFIXED
                    | CommodityFlags::STYLE_SEPARATED
                    | CommodityFlags::STYLE_DECIMAL_COMMA,
                amount.commodity().unwrap().flags()
            );
        }
    }

    #[test]
    fn test_parse_posting() {
        init();

        let (_, posting) = parse_posting("A  $12").unwrap();
        assert_eq!(posting.account_name(), "A".to_string());
        insta::assert_debug_snapshot!(posting.amount.unwrap(), @r#"
            AMOUNT($12) [prec:0, keep:false, comm:$, raw:12]
        "#);

        reset_parse_state();
        let (_, posting) = parse_posting("A\t$12").unwrap();
        assert_eq!(posting.account_name(), "A".to_string());
        insta::assert_debug_snapshot!(posting.amount.unwrap(), @r#"
            AMOUNT($12) [prec:0, keep:false, comm:$, raw:12]
        "#);

        reset_parse_state();
        // not enough spaces between account and price
        let (_, posting) = parse_posting("A $12").unwrap();
        assert_eq!(posting.account_name(), "A $12".to_string());
        assert!(posting.amount.is_none());

        reset_parse_state();
        let (_, posting) = parse_posting("A  12 USD").unwrap();
        assert_eq!(posting.account_name(), "A".to_string());
        insta::assert_debug_snapshot!(posting.amount.unwrap(), @r#"
            AMOUNT(12 USD) [prec:0, keep:false, comm:USD, raw:12]
        "#);

        reset_parse_state();
        let (_, posting) = parse_posting("A  3 @ $4").unwrap();
        insta::assert_debug_snapshot!(posting.amount.unwrap(), @r#"
            AMOUNT(3) [prec:0, keep:false, raw:3]
        "#);
        insta::assert_debug_snapshot!(posting.cost, @r#"
            Some(
                AMOUNT($4) [prec:0, keep:false, comm:$, raw:4],
            )
        "#);
        assert_eq!(posting.cost, posting.given_cost);

        reset_parse_state();
        let (_, posting) = parse_posting("A  6@@$12").unwrap();
        insta::assert_debug_snapshot!(posting.amount.unwrap(), @r#"
            AMOUNT(6) [prec:0, keep:false, raw:6]
        "#);
        insta::assert_debug_snapshot!(posting.cost, @r#"
            Some(
                AMOUNT($2.000000) [prec:6, keep:false, comm:$, raw:2],
            )
        "#);
        insta::assert_debug_snapshot!(posting.given_cost, @r#"
            None
        "#);

        // TODO: qty is required w/ cost or total cost
        // TODO: qty and cost different commodities, but only if price not included
        // TODO: virtual cost

        reset_parse_state();
        let (_, posting) = parse_posting("A  3 {$2} @ $4").unwrap();
        insta::assert_debug_snapshot!(posting.amount.as_ref().unwrap(), @r#"
            AMOUNT(3) [prec:0, keep:false, comm:, raw:3]
        "#);
        insta::assert_debug_snapshot!(posting.amount.unwrap().commodity().unwrap().annotation().price(), @r#"
            Some(
                AMOUNT($2) [prec:0, keep:false, comm:$, raw:2],
            )
        "#);
        insta::assert_debug_snapshot!(posting.cost, @r#"
            Some(
                AMOUNT($4) [prec:0, keep:false, comm:$, raw:4],
            )
        "#);

        reset_parse_state();
        let (_, posting) = parse_posting("A  $1.20 {{£5.00}} @@6.00£").unwrap();
        // AMOUNT($1.20) [prec:2, keep:false, comm:$, raw:1.20]
        // amount, as given
        insta::assert_debug_snapshot!(posting.amount.as_ref().unwrap(), @r#"
            AMOUNT($1.20) [prec:2, keep:false, comm:$, raw:6/5]
        "#);
        // price should be 5/1.2
        insta::assert_debug_snapshot!(posting.amount.unwrap().commodity().unwrap().annotation().price(), @r#"
            Some(
                AMOUNT(£4.1666667) [prec:7, keep:false, comm:£, raw:25/6],
            )
        "#);
        // cost should be 6/1.2
        insta::assert_debug_snapshot!(posting.cost, @r#"
            Some(
                AMOUNT(5.0000000£) [prec:7, keep:false, comm:£, raw:5],
            )
        "#);

        reset_parse_state();
        let (_, posting) = parse_posting("Actif:SV  -0,0415 MFE @ 358,80 €").unwrap();
        insta::assert_debug_snapshot!(posting.amount.as_ref().unwrap(), @r#"
            AMOUNT(-0.0415 MFE) [prec:4, keep:false, comm:MFE, raw:-83/2000]
        "#);
        insta::assert_debug_snapshot!(posting.amount.as_ref().unwrap().commodity().unwrap().annotation().price(), @r#"
            None
        "#);
        insta::assert_debug_snapshot!(posting.cost, @r#"
            Some(
                AMOUNT(358.8 €) [prec:1, keep:false, comm:€, raw:1794/5],
            )
        "#);
        assert_eq!(
            CommodityFlags::STYLE_SUFFIXED
                | CommodityFlags::STYLE_SEPARATED
                | CommodityFlags::STYLE_DECIMAL_COMMA,
            posting.amount.unwrap().commodity().unwrap().flags()
        );
    }

    #[test]
    fn test_parse_transaction() {
        let input = "2011-03-01 * Z\n    A  10\n    B\n";
        let (_, Transaction { date, status, payee, postings, .. }) =
            parse_transaction(input).unwrap();

        assert_eq!(date.format("%Y/%m/%d").to_string(), "2011/03/01");
        assert_eq!(status, TransactionStatus::Cleared);
        assert_eq!(payee, "Z".to_string());
        assert_eq!(postings.len(), 2);
    }

    #[test]
    fn test_parse_transaction_with_comment() {
        let input = "2011-03-06 ! 2 ; note line 1\n    ; note line 2\n    E  10\n    F\n";
        let (_, Transaction { date, status, payee, note, .. }) = parse_transaction(input).unwrap();

        assert_eq!(date.format("%Y/%m/%d").to_string(), "2011/03/06");
        assert_eq!(status, TransactionStatus::Pending);
        assert_eq!(payee, "2".to_string());
        assert_eq!(note, Some("note line 1\nnote line 2".to_string()));
    }

    #[test]
    fn test_parse_transaction_with_comment_overide_payee() {
        let input = "2011-03-06 Foo\n    ; Payee: Bar\n    Act1  1\n    Act2\n";
        let (_, Transaction { payee, .. }) = parse_transaction(input).unwrap();

        assert_eq!(payee, "Bar".to_string());
    }

    #[test]
    fn test_periodic_transaction_parsing() {
        let input = "~ monthly\n    Assets:Checking   $1000\n    Expenses:Rent\n";
        let result = periodic_transaction(input);
        assert!(result.is_ok());

        if let (_, Directive::PeriodicTransaction { period, postings, .. }) = result.unwrap() {
            assert_eq!(period, "monthly");
            assert_eq!(postings.len(), 2);
        } else {
            panic!("Expected PeriodicTransaction directive");
        }
    }

    #[test]
    fn test_automated_transaction_parsing() {
        let input = "= expr account =~ /Expenses/\n    (Budget:Expenses)   $-1\n";
        let result = automated_transaction(input);
        assert!(result.is_ok());

        if let (_, Directive::AutomatedTransaction { condition, postings, .. }) = result.unwrap() {
            assert_eq!(condition, "expr account =~ /Expenses/");
            assert_eq!(postings.len(), 1);
        } else {
            panic!("Expected AutomatedTransaction directive");
        }
    }

    #[test]
    fn test_streaming_parser_creation() {
        let streaming_parser = StreamingJournalParser::new();
        assert_eq!(streaming_parser.buffer_size, 8192);

        let custom_parser = StreamingJournalParser::with_buffer_size(4096);
        assert_eq!(custom_parser.buffer_size, 4096);
    }

    #[test]
    fn test_entry_boundary_detection() {
        let mut streaming_parser = StreamingJournalParser::new();
        let buffer = "2024/01/01 Transaction 1\n    Assets:Cash   $100\n\n2024/01/02 Transaction 2\n    Assets:Cash   $200";
        let mut journal_iter = streaming_parser.parse_reader_streaming(buffer.as_bytes());

        if let Some(Ok(JournalEntry::Transaction(Transaction { date, .. }))) = journal_iter.next() {
            assert_eq!(date.format("%Y/%m/%d").to_string(), "2024/01/01");
        } else {
            panic!("date did not match");
        }

        if let Some(Ok(JournalEntry::Transaction(Transaction { date, .. }))) = journal_iter.next() {
            assert_eq!(date.format("%Y/%m/%d").to_string(), "2024/01/02");
        } else {
            panic!("date did not match");
        }

        assert!(journal_iter.next().is_none());
    }
}
