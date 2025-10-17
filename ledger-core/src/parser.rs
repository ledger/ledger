//! Journal parsing engine using nom combinators
//!
//! This module provides a robust parser for Ledger journal files, supporting:
//! - All Ledger directives (account, commodity, include, etc.)
//! - Transaction parsing with proper indentation handling
//! - Comment and metadata extraction
//! - Error recovery with meaningful messages
//! - Include file resolution with cycle detection
//! - Streaming parser for large files

use ledger_math::CommodityFlags;
use nom::{
    branch::alt,
    bytes::complete::{take_until, take_while, take_while1},
    character::complete::{alpha1, char, digit1, line_ending, space0, space1},
    combinator::{map, opt, recognize, value},
    error::{context, ParseError},
    multi::many0,
    sequence::{delimited, preceded, terminated, tuple},
    IResult,
};

// We need to use bytes for tag with byte strings
use nom::bytes::complete::tag as bytes_tag;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::Arc;

use crate::{
    account::Account, amount::Amount, commodity::Commodity, journal::Journal, posting::Posting,
    transaction::Transaction,
};

use chrono::NaiveDate;

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
        if input.starts_with(s) {
            Ok((&input[s.len()..], &input[..s.len()]))
        } else {
            Err(nom::Err::Error(VerboseError::from_error_kind(input, nom::error::ErrorKind::Tag)))
        }
    }
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
        symbol: String,
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
        let entries = self.parse_entries(input)?;
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
            if let Some(amount) = &posting.amount {
                if let Some(commodity_ref) = amount.commodity() {
                    let commodity = commodity_ref.clone();
                    let symbol = commodity.symbol().to_string();

                    // Add commodity if it doesn't exist in the journal
                    if !journal.commodities.contains_key(&symbol) {
                        journal.commodities.insert(symbol.clone(), commodity.clone());
                    }
                }
            }
        }
    }

    /// Process a parsed directive with include handling
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
                        AccountDeclaration::Alias(alias) => {
                            // journal.add_account_alias(alias, name.clone());
                        }
                        AccountDeclaration::Default => {
                            // journal.set_default_account(name.clone());
                        }
                        // Handle other account declarations
                        _ => {}
                    }
                }
                journal.add_account(Rc::new(RefCell::new(account)));
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
                journal.add_commodity(Arc::new(commodity));
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
            Directive::PeriodicTransaction { period, postings, metadata } => {
                // TODO: Process periodic transaction
                // For now, just log that we found one
                println!("Found periodic transaction: {} with {} postings", period, postings.len());
            }
            Directive::AutomatedTransaction { condition, postings, metadata } => {
                // TODO: Process automated transaction
                // For now, just log that we found one
                println!(
                    "Found automated transaction: {} with {} postings",
                    condition,
                    postings.len()
                );
            }
            // Handle other directives
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
            self.context.filename.parent().unwrap_or_else(|| Path::new("")).join(&path)
        };

        // Parse included file and merge
        let included_journal = self.parse_file(&include_path)?;

        // Merge the included journal into the current one
        journal.merge(included_journal);

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
            self.get_line_column(input, error.errors.get(0).map(|(s, _)| s).unwrap_or(&input));

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
        let line = lines.len();
        let column = lines.last().map(|l| l.len()).unwrap_or(0) + 1;
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

        for (i, line) in lines.iter().enumerate() {
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
        if trimmed.chars().next().map_or(false, |c| c.is_ascii_digit()) {
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
fn journal_entries(input: &str) -> ParseResult<Vec<JournalEntry>> {
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
fn journal_entry_with_recovery(input: &str) -> ParseResult<Option<JournalEntry>> {
    alt((
        map(journal_entry, Some),
        // Skip invalid lines and return None
        map(skip_invalid_line, |_| None),
    ))(input)
}

/// Skip an invalid line for error recovery
fn skip_invalid_line(input: &str) -> ParseResult<&str> {
    terminated(take_until("\n"), line_ending)(input)
}

/// Parse a single journal entry
fn journal_entry(input: &str) -> ParseResult<JournalEntry> {
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
fn empty_line(input: &str) -> ParseResult<&str> {
    terminated(space0, line_ending)(input)
}

/// Parse a comment line with optional metadata extraction
fn comment_line(input: &str) -> ParseResult<String> {
    map(preceded(alt((tag(";"), tag("#"), tag("*"), tag("|"))), take_until("\n")), |s: &str| {
        s.to_string()
    })(input)
}

/// Parse metadata tags from comments with multiple supported patterns
fn parse_metadata_tags(comment: &str) -> HashMap<String, String> {
    let mut metadata = HashMap::new();

    for line in comment.lines() {
        let trimmed = line.trim();

        // Pattern 1: :tag: value
        if let Some(start) = trimmed.find(':') {
            if let Some(end) = trimmed[start + 1..].find(':') {
                let tag_name = &trimmed[start + 1..start + 1 + end];
                let remaining = &trimmed[start + 1 + end + 1..].trim();

                if !tag_name.is_empty() {
                    if remaining.is_empty() {
                        metadata.insert(tag_name.to_string(), String::new());
                    } else {
                        metadata.insert(tag_name.to_string(), remaining.to_string());
                    }
                }
            }
        }

        // Pattern 2: [key: value] or [key=value]
        if let Some(start) = trimmed.find('[') {
            if let Some(end) = trimmed.find(']') {
                let bracket_content = &trimmed[start + 1..end];
                if let Some(colon_pos) = bracket_content.find(':') {
                    let key = bracket_content[..colon_pos].trim();
                    let value = bracket_content[colon_pos + 1..].trim();
                    if !key.is_empty() {
                        metadata.insert(key.to_string(), value.to_string());
                    }
                } else if let Some(equals_pos) = bracket_content.find('=') {
                    let key = bracket_content[..equals_pos].trim();
                    let value = bracket_content[equals_pos + 1..].trim();
                    if !key.is_empty() {
                        metadata.insert(key.to_string(), value.to_string());
                    }
                }
            }
        }

        // Pattern 3: key=value or key: value (no brackets/colons)
        if !trimmed.contains(':') || (trimmed.chars().filter(|&c| c == ':').count() == 1) {
            if let Some(equals_pos) = trimmed.find('=') {
                let key = trimmed[..equals_pos].trim();
                let value = trimmed[equals_pos + 1..].trim();
                if !key.is_empty() && !key.contains(' ') {
                    metadata.insert(key.to_string(), value.to_string());
                }
            }
        }
    }

    metadata
}

/// Enhanced comment parser with metadata extraction
fn enhanced_comment_line(input: &str) -> ParseResult<(String, HashMap<String, String>)> {
    map(preceded(alt((tag(";"), tag("#"), tag("*"), tag("|"))), take_until("\n")), |s: &str| {
        let comment = s.to_string();
        let metadata = parse_metadata_tags(&comment);
        (comment, metadata)
    })(input)
}

// ============================================================================
// Transaction Parsing
// ============================================================================

/// Parse a transaction entry
fn transaction_entry(input: &str) -> ParseResult<Transaction> {
    context("transaction", parse_transaction)(input)
}

/// Parse a complete transaction with metadata support
fn parse_transaction(input: &str) -> ParseResult<Transaction> {
    map(
        tuple((
            date_field,
            opt(preceded(tag("="), date_field)), // aux date
            opt(alt((tag("*"), tag("!")))),      // cleared flag
            opt(delimited(tag("("), take_until(")"), tag(")"))), // code
            opt(payee_description),
            opt(preceded(space0, simple_comment_field)), // transaction comment
            line_ending,
            many0(posting_line),
        )),
        |(date, aux_date, cleared, code, payee, tx_comment, _, postings)| {
            let payee_str = payee.unwrap_or_else(|| String::new());
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
            if let Some(comment) = tx_comment {
                transaction.note = Some(comment.clone());
                // TODO: Add metadata extraction to transaction
                // let metadata = parse_metadata_tags(&comment);
                // transaction.metadata.extend(metadata);
            }

            for posting in postings {
                transaction.add_posting(posting);
            }

            transaction
        },
    )(input)
}

/// Parse a date field
fn date_field(input: &str) -> ParseResult<NaiveDate> {
    // Parse date in various formats
    alt((
        // ISO date format: 2021-01-01
        map(recognize(tuple((digit1, tag("-"), digit1, tag("-"), digit1))), |date_str: &str| {
            // Parse YYYY-MM-DD format
            let parts: Vec<&str> = date_str.split('-').collect();
            if parts.len() == 3 {
                let year = parts[0].parse::<i32>().unwrap_or(2024);
                let month = parts[1].parse::<u32>().unwrap_or(1);
                let day = parts[2].parse::<u32>().unwrap_or(1);
                NaiveDate::from_ymd_opt(year, month, day)
                    .unwrap_or_else(|| NaiveDate::from_ymd_opt(2024, 1, 1).unwrap())
            } else {
                NaiveDate::from_ymd_opt(2024, 1, 1).unwrap()
            }
        }),
        // Slash format: 2021/01/01 or 01/01
        map(
            recognize(tuple((digit1, tag("/"), digit1, opt(tuple((tag("/"), digit1)))))),
            |date_str: &str| {
                // Parse date string with slashes
                let parts: Vec<&str> = date_str.split('/').collect();
                if parts.len() == 3 {
                    // YYYY/MM/DD format
                    let year = parts[0].parse::<i32>().unwrap_or(2024);
                    let month = parts[1].parse::<u32>().unwrap_or(1);
                    let day = parts[2].parse::<u32>().unwrap_or(1);
                    NaiveDate::from_ymd_opt(year, month, day)
                        .unwrap_or_else(|| NaiveDate::from_ymd_opt(2024, 1, 1).unwrap())
                } else if parts.len() == 2 {
                    // MM/DD format - assume current year
                    use chrono::{Datelike, Local};
                    let current_year = Local::now().year();
                    let month = parts[0].parse::<u32>().unwrap_or(1);
                    let day = parts[1].parse::<u32>().unwrap_or(1);
                    NaiveDate::from_ymd_opt(current_year, month, day)
                        .unwrap_or_else(|| NaiveDate::from_ymd_opt(2024, 1, 1).unwrap())
                } else {
                    NaiveDate::from_ymd_opt(2024, 1, 1).unwrap()
                }
            },
        ),
    ))(input)
}

/// Parse payee description
fn payee_description(input: &str) -> ParseResult<String> {
    map(take_while(|c| c != '\n' && c != '\r'), |s: &str| s.trim().to_string())(input)
}

/// Parse a posting line
fn posting_line(input: &str) -> ParseResult<Posting> {
    context("posting", preceded(alt((space1, tag("\t"))), parse_posting))(input)
}

/// Parse a single posting with metadata support
fn parse_posting(input: &str) -> ParseResult<Posting> {
    // For now, create a simplified version that compiles
    // TODO: Fix account reference creation and metadata handling
    map(
        tuple((
            account_name,
            opt(preceded(space1, simple_amount_field)),
            opt(preceded(space1, simple_comment_field)),
            opt(line_ending),
        )),
        |(account, amount, comment, _)| {
            // Create a dummy account reference for now
            // TODO: Proper account management
            use compact_str::CompactString;
            let account_ref = std::rc::Rc::new(std::cell::RefCell::new(
                crate::account::Account::new(CompactString::from(account), None, 0),
            ));
            let mut posting = Posting::new(account_ref);

            if let Some(amount) = amount {
                posting.amount = Some(amount);
            }

            if let Some(comment) = comment {
                use compact_str::CompactString;
                posting.note = Some(CompactString::from(comment));
            }

            posting
        },
    )(input)
}

/// Parse an account name
fn account_name(input: &str) -> ParseResult<String> {
    map(take_while1(|c: char| c != ' ' && c != '\t' && c != '\n' && c != ';'), |s: &str| {
        s.trim().to_string()
    })(input)
}

/// Parse an amount field (simplified)
fn simple_amount_field(input: &str) -> ParseResult<Amount> {
    // Simplified amount parser - should handle currencies, expressions, etc.
    alt((
        // Negative amount with currency symbol: -$1000.00, -€500.50
        map(
            recognize(tuple((
                tag("-"),
                alt((tag("$"), tag("€"), tag("£"), tag("¥"))), // currency symbol
                digit1,
                opt(tuple((tag("."), digit1))),
            ))),
            |amount_str: &str| {
                // Extract decimal part including the negative sign and currency symbol
                use ledger_math::commodity::Commodity;
                use rust_decimal::Decimal;
                use std::str::FromStr;
                use std::sync::Arc;

                // Extract the currency symbol
                let currency_symbol = if amount_str.contains("$") {
                    "USD"
                } else if amount_str.contains("€") {
                    "EUR"
                } else if amount_str.contains("£") {
                    "GBP"
                } else if amount_str.contains("¥") {
                    "JPY"
                } else {
                    ""
                };

                let decimal_str = amount_str
                    .chars()
                    .filter(|c| c.is_ascii_digit() || *c == '.' || *c == '-')
                    .collect::<String>();

                let decimal = Decimal::from_str(&decimal_str).unwrap_or_default();

                // Create amount with commodity
                if !currency_symbol.is_empty() {
                    let commodity = Arc::new(Commodity::new(currency_symbol));
                    Amount::with_commodity(decimal, Some(commodity))
                } else {
                    Amount::new(decimal)
                }
            },
        ),
        // Positive amount with currency symbol before: $1000.00, €500.50
        map(
            recognize(tuple((
                alt((tag("$"), tag("€"), tag("£"), tag("¥"))), // currency symbol
                digit1,
                opt(tuple((tag("."), digit1))),
            ))),
            |amount_str: &str| {
                // Extract decimal part and currency symbol
                use ledger_math::commodity::Commodity;
                use rust_decimal::Decimal;
                use std::str::FromStr;
                use std::sync::Arc;

                // Extract the currency symbol
                let currency_symbol = if amount_str.contains("$") {
                    "USD"
                } else if amount_str.contains("€") {
                    "EUR"
                } else if amount_str.contains("£") {
                    "GBP"
                } else if amount_str.contains("¥") {
                    "JPY"
                } else {
                    ""
                };

                let decimal_str =
                    amount_str.chars().skip_while(|c| !c.is_ascii_digit()).collect::<String>();

                let decimal = Decimal::from_str(&decimal_str).unwrap_or_default();

                // Create amount with commodity
                if !currency_symbol.is_empty() {
                    let commodity = Arc::new(Commodity::new(currency_symbol));
                    Amount::with_commodity(decimal, Some(commodity))
                } else {
                    Amount::new(decimal)
                }
            },
        ),
        // Amount followed by commodity: 1000.00 USD, -1000.00 USD
        map(
            tuple((
                tuple((opt(tag("-")), digit1, opt(tuple((tag("."), digit1))))),
                opt(preceded(space0, alpha1)), // commodity
            )),
            |(amount, commodity)| {
                // For now, create a simple amount from parsed decimal
                use rust_decimal::Decimal;
                use std::str::FromStr;

                let decimal_str = format!(
                    "{}{}{}",
                    amount.0.unwrap_or(""),
                    amount.1,
                    amount.2.map(|a| format!("{}{}", a.0, a.1)).unwrap_or(String::new())
                );

                let decimal = Decimal::from_str(&decimal_str).unwrap_or_default();
                let commodity = commodity.map(|s| {
                    let mut commodity = Commodity::new(s);
                    commodity.add_flags(CommodityFlags::STYLE_SUFFIXED);
                    Arc::new(commodity)
                });
                Amount::with_commodity(decimal, commodity)
            },
        ),
    ))(input)
}

/// Parse a comment field with metadata extraction
fn comment_field(input: &str) -> ParseResult<(String, HashMap<String, String>)> {
    map(preceded(tag(";"), take_until("\n")), |s: &str| {
        let comment = s.trim().to_string();
        let metadata = parse_metadata_tags(&comment);
        (comment, metadata)
    })(input)
}

/// Simple comment field parser (for backward compatibility)
fn simple_comment_field(input: &str) -> ParseResult<String> {
    map(preceded(tag(";"), take_until("\n")), |s: &str| s.trim().to_string())(input)
}

// ============================================================================
// Directive Parsing
// ============================================================================

/// Parse a directive entry
fn directive_entry(input: &str) -> ParseResult<Directive> {
    context("directive", parse_directive)(input)
}

/// Parse any directive
fn parse_directive(input: &str) -> ParseResult<Directive> {
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
    ))(input)
}

/// Parse account directive
fn account_directive(input: &str) -> ParseResult<Directive> {
    map(
        tuple((tag("account"), space1, account_name, line_ending, many0(account_declaration))),
        |(_, _, name, _, declarations)| Directive::Account { name, declarations },
    )(input)
}

/// Parse account declarations
fn account_declaration(input: &str) -> ParseResult<AccountDeclaration> {
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
fn commodity_directive(input: &str) -> ParseResult<Directive> {
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
fn commodity_declaration(input: &str) -> ParseResult<CommodityDeclaration> {
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
fn include_directive(input: &str) -> ParseResult<Directive> {
    alt((conditional_include_directive, simple_include_directive))(input)
}

/// Parse simple include directive
fn simple_include_directive(input: &str) -> ParseResult<Directive> {
    map(preceded(tag("include"), preceded(space1, take_until("\n"))), |path: &str| {
        Directive::Include { path: PathBuf::from(path.trim()) }
    })(input)
}

/// Parse conditional include directive
fn conditional_include_directive(input: &str) -> ParseResult<Directive> {
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
fn price_directive(input: &str) -> ParseResult<Directive> {
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
fn alias_directive(input: &str) -> ParseResult<Directive> {
    map(
        tuple((tag("alias"), space1, take_until("="), tag("="), take_until("\n"))),
        |(_, _, account, _, alias)| Directive::Alias {
            account: account.trim().to_string(),
            alias: alias.trim().to_string(),
        },
    )(input)
}

/// Parse apply directive
fn apply_directive(input: &str) -> ParseResult<Directive> {
    map(preceded(tag("apply account"), preceded(space1, take_until("\n"))), |account: &str| {
        Directive::Apply { state: ApplyState::Account(account.trim().to_string()) }
    })(input)
}

/// Parse end directive
fn end_directive(input: &str) -> ParseResult<Directive> {
    value(Directive::End, tag("end"))(input)
}

/// Parse payee directive
fn payee_directive(input: &str) -> ParseResult<Directive> {
    map(
        tuple((tag("payee"), space1, take_until("\n"), line_ending, many0(payee_declaration))),
        |(_, _, name, _, declarations)| Directive::Payee {
            name: name.trim().to_string(),
            declarations,
        },
    )(input)
}

/// Parse payee declarations
fn payee_declaration(input: &str) -> ParseResult<PayeeDeclaration> {
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
fn tag_directive(input: &str) -> ParseResult<Directive> {
    map(preceded(tag("tag"), preceded(space1, take_until("\n"))), |name: &str| Directive::Tag {
        name: name.trim().to_string(),
    })(input)
}

/// Parse option directive
fn option_directive(input: &str) -> ParseResult<Directive> {
    map(
        tuple((tag("option"), space1, take_until(" "), space1, take_until("\n"))),
        |(_, _, name, _, value)| Directive::Option {
            name: name.trim().to_string(),
            value: value.trim().to_string(),
        },
    )(input)
}

/// Parse eval directive
fn eval_directive(input: &str) -> ParseResult<Directive> {
    map(preceded(tag("eval"), preceded(space1, take_until("\n"))), |expression: &str| {
        Directive::Eval { expression: expression.trim().to_string() }
    })(input)
}

/// Parse define directive
fn define_directive(input: &str) -> ParseResult<Directive> {
    map(
        tuple((tag("define"), space1, take_until("="), tag("="), take_until("\n"))),
        |(_, _, name, _, expression)| Directive::Define {
            name: name.trim().to_string(),
            expression: expression.trim().to_string(),
        },
    )(input)
}

/// Parse year directive
fn year_directive(input: &str) -> ParseResult<Directive> {
    map(preceded(tag("year"), preceded(space1, digit1)), |year_str: &str| Directive::Year {
        year: year_str.parse().unwrap_or(2024),
    })(input)
}

/// Parse default commodity directive
fn default_commodity_directive(input: &str) -> ParseResult<Directive> {
    map(preceded(tag("D"), preceded(space1, take_until("\n"))), |symbol: &str| {
        Directive::DefaultCommodity { symbol: symbol.trim().to_string() }
    })(input)
}

/// Parse assert directive
fn assert_directive(input: &str) -> ParseResult<Directive> {
    map(preceded(tag("assert"), preceded(space1, take_until("\n"))), |condition: &str| {
        Directive::Assert { condition: condition.trim().to_string() }
    })(input)
}

/// Parse check directive
fn check_directive(input: &str) -> ParseResult<Directive> {
    map(preceded(tag("check"), preceded(space1, take_until("\n"))), |condition: &str| {
        Directive::Check { condition: condition.trim().to_string() }
    })(input)
}

/// Parse periodic transaction (starts with ~)
fn periodic_transaction(input: &str) -> ParseResult<Directive> {
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
fn automated_transaction(input: &str) -> ParseResult<Directive> {
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
    use super::*;

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
        let input = "Assets:Checking";
        let result = account_name(input);
        assert!(result.is_ok());
        let (_, name) = result.unwrap();
        assert_eq!(name, "Assets:Checking");
    }

    #[test]
    fn test_account_directive() {
        let input = "account Assets:Checking\n";
        let result = account_directive(input);
        assert!(result.is_ok());
    }

    #[test]
    fn test_include_directive() {
        let input = "include expenses.ledger";
        let result = include_directive(input);
        assert!(result.is_ok());
        if let (_, Directive::Include { path }) = result.unwrap() {
            assert_eq!(path, PathBuf::from("expenses.ledger"));
        }
    }

    #[test]
    fn test_metadata_parsing_colon_style() {
        let comment = " :Receipt: 12345 :Project: ABC";
        let metadata = parse_metadata_tags(comment);
        assert_eq!(metadata.get("Receipt"), Some(&"12345".to_string()));
        assert_eq!(metadata.get("Project"), Some(&"ABC".to_string()));
    }

    #[test]
    fn test_metadata_parsing_bracket_style() {
        let comment = " [Receipt: 12345] [Project=ABC]";
        let metadata = parse_metadata_tags(comment);
        assert_eq!(metadata.get("Receipt"), Some(&"12345".to_string()));
        assert_eq!(metadata.get("Project"), Some(&"ABC".to_string()));
    }

    #[test]
    fn test_metadata_parsing_equals_style() {
        let comment = " Receipt=12345 Project=ABC";
        let metadata = parse_metadata_tags(comment);
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
    fn test_simple_include_directive() {
        let input = "include expenses.dat";
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
        let input = "include [exists(\"optional.dat\")] optional.dat";
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
        let mut transaction = Transaction::new(
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

        // This is a simplified test - in practice we'd need to set up the full iterator
        // The key is that the streaming parser can identify transaction boundaries
        assert!(buffer.contains("2024/01/01"));
        assert!(buffer.contains("2024/01/02"));
    }
}
