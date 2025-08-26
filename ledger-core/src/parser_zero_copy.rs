//! Zero-copy parser implementation for high-performance journal parsing
//!
//! This module provides a zero-copy parser that minimizes memory allocations
//! by working directly with memory-mapped files and using lifetimes to tie
//! parsed data to the source buffer.

use memmap2::Mmap;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while, take_while1},
    character::complete::{
        alpha1, char, digit1, line_ending, space0, space1, 
        multispace0, multispace1, not_line_ending,
    },
    combinator::{map, opt, recognize, value, consumed},
    error::{context, ParseError},
    multi::{many0, many1, separated_list1},
    sequence::{delimited, preceded, terminated, tuple, pair},
    IResult, Parser, InputLength,
};

use std::fs::File;
use std::path::{Path, PathBuf};
use std::io::{BufRead, BufReader};
use std::borrow::Cow;
use crate::strings::{AccountName, PayeeName, CommoditySymbol, intern_string, FastParser};
use smallvec::SmallVec;

/// Zero-copy parser error type
#[derive(Debug, thiserror::Error)]
pub enum ZeroCopyParseError {
    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
    
    #[error("Parse error at line {line}, column {column}: {message}")]
    ParseError {
        line: usize,
        column: usize,
        message: String,
    },
    
    #[error("Memory mapping failed: {0}")]
    MmapError(String),
}

/// Custom result type for zero-copy parsing
pub type ZeroCopyResult<T> = Result<T, ZeroCopyParseError>;

/// Represents a memory-mapped journal file for zero-copy parsing
pub struct MappedJournal {
    /// Memory-mapped file data
    mmap: Mmap,
    /// Original file path for error reporting
    path: PathBuf,
}

impl MappedJournal {
    /// Create a new memory-mapped journal from a file path
    pub fn from_path(path: &Path) -> ZeroCopyResult<Self> {
        let file = File::open(path)?;
        let mmap = unsafe { 
            Mmap::map(&file).map_err(|e| {
                ZeroCopyParseError::MmapError(format!("Failed to map file {}: {}", path.display(), e))
            })?
        };

        Ok(Self {
            mmap,
            path: path.to_path_buf(),
        })
    }

    /// Get the raw content as a string slice
    pub fn content(&self) -> Result<&str, ZeroCopyParseError> {
        std::str::from_utf8(&self.mmap).map_err(|e| {
            ZeroCopyParseError::ParseError {
                line: 0,
                column: 0,
                message: format!("Invalid UTF-8 in file {}: {}", self.path.display(), e),
            }
        })
    }

    /// Get the file path
    pub fn path(&self) -> &Path {
        &self.path
    }
}

/// Zero-copy transaction representation using lifetime to tie to source buffer
#[derive(Debug, Clone)]
pub struct ZeroCopyTransaction<'a> {
    /// Transaction date as string slice
    pub date_str: &'a str,
    /// Description/payee as string slice  
    pub description: &'a str,
    /// Optional code as string slice
    pub code: Option<&'a str>,
    /// Postings referencing the source buffer
    pub postings: SmallVec<[ZeroCopyPosting<'a>; 4]>,
    /// Source line number for error reporting
    pub line_number: usize,
}

/// Zero-copy posting representation
#[derive(Debug, Clone)]
pub struct ZeroCopyPosting<'a> {
    /// Account name as string slice
    pub account: &'a str,
    /// Amount as string slice (parsed later if needed)
    pub amount_str: Option<&'a str>,
    /// Optional comment/note
    pub note: Option<&'a str>,
    /// Posting flags parsed from syntax
    pub flags: PostingFlags,
}

/// Posting flags for zero-copy parsing
#[derive(Debug, Clone, Copy, Default)]
pub struct PostingFlags {
    pub is_virtual: bool,
    pub must_balance: bool,
}

/// Zero-copy parser context
#[derive(Debug)]
pub struct ZeroCopyParser<'a> {
    /// Input buffer being parsed
    input: &'a str,
    /// Current position for error reporting
    current_line: usize,
    /// Statistics for monitoring performance
    stats: ParserStats,
}

#[derive(Debug, Default)]
pub struct ParserStats {
    /// Number of transactions parsed
    pub transactions_parsed: usize,
    /// Number of postings parsed
    pub postings_parsed: usize,
    /// Total processing time
    pub parse_time_nanos: u64,
    /// Memory allocations avoided (estimated)
    pub allocations_saved: usize,
}

impl<'a> ZeroCopyParser<'a> {
    /// Create a new zero-copy parser for the given input
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            current_line: 1,
            stats: ParserStats::default(),
        }
    }

    /// Parse the entire journal content
    pub fn parse_journal(&mut self) -> ZeroCopyResult<Vec<ZeroCopyTransaction<'a>>> {
        let start_time = std::time::Instant::now();
        
        let mut transactions = Vec::new();
        let mut remaining = self.input;
        let mut line_number = 1;

        while !remaining.is_empty() {
            // Skip empty lines and comments
            if let Ok((rest, _)) = self.skip_whitespace_and_comments(remaining) {
                remaining = rest;
                if remaining.is_empty() {
                    break;
                }
            }

            // Try to parse a transaction
            match self.parse_transaction(remaining) {
                Ok((rest, mut transaction)) => {
                    transaction.line_number = line_number;
                    transactions.push(transaction);
                    remaining = rest;
                    self.stats.transactions_parsed += 1;
                    
                    // Update line number
                    line_number += remaining.len() - rest.len();
                }
                Err(_) => {
                    // Skip this line and continue
                    if let Some(newline_pos) = remaining.find('\n') {
                        remaining = &remaining[newline_pos + 1..];
                        line_number += 1;
                    } else {
                        break; // End of input
                    }
                }
            }
        }

        self.stats.parse_time_nanos = start_time.elapsed().as_nanos() as u64;
        Ok(transactions)
    }

    /// Skip whitespace, empty lines, and comments
    fn skip_whitespace_and_comments(&self, input: &'a str) -> IResult<&'a str, ()> {
        let (input, _) = many0(alt((
            // Empty line
            value((), pair(multispace0, line_ending)),
            // Comment line
            value((), tuple((space0, char(';'), not_line_ending, line_ending))),
            // Just whitespace
            value((), multispace1),
        )))(input)?;
        
        Ok((input, ()))
    }

    /// Parse a single transaction
    fn parse_transaction(&mut self, input: &'a str) -> IResult<&'a str, ZeroCopyTransaction<'a>> {
        let (input, (date_str, description, code)) = self.parse_transaction_header(input)?;
        let (input, _) = line_ending(input)?;
        let (input, postings) = self.parse_postings(input)?;

        self.stats.postings_parsed += postings.len();
        
        Ok((input, ZeroCopyTransaction {
            date_str,
            description,
            code,
            postings,
            line_number: 0, // Will be set by caller
        }))
    }

    /// Parse transaction header (date, description, optional code)
    fn parse_transaction_header(&self, input: &'a str) -> IResult<&'a str, (&'a str, &'a str, Option<&'a str>)> {
        // Parse date - simple YYYY-MM-DD format
        let (input, date_str) = recognize(tuple((
            digit1,           // Year
            char('-'),
            digit1,           // Month  
            char('-'),
            digit1,           // Day
        )))(input)?;
        
        let (input, _) = space1(input)?;
        
        // Optional code in parentheses
        let (input, code) = opt(delimited(
            char('('),
            take_while(|c: char| c != ')'),
            char(')')
        ))(input)?;
        
        let (input, _) = if code.is_some() { space1(input)? } else { (input, "") };
        
        // Description/payee (rest of line)
        let (input, description) = take_while(|c: char| c != '\n' && c != '\r')(input)?;
        let description = description.trim(); // Remove trailing whitespace
        
        Ok((input, (date_str, description, code)))
    }

    /// Parse postings for a transaction
    fn parse_postings(&mut self, input: &'a str) -> IResult<&'a str, SmallVec<[ZeroCopyPosting<'a>; 4]>> {
        let mut postings = SmallVec::new();
        let mut remaining = input;

        // Parse postings until we hit a non-posting line
        loop {
            // Check if this line starts with whitespace (posting indicator)
            if !remaining.starts_with(' ') && !remaining.starts_with('\t') {
                break;
            }

            match self.parse_posting(remaining) {
                Ok((rest, posting)) => {
                    postings.push(posting);
                    remaining = rest;
                }
                Err(_) => {
                    // Skip malformed posting line
                    if let Some(newline_pos) = remaining.find('\n') {
                        remaining = &remaining[newline_pos + 1..];
                    } else {
                        break;
                    }
                }
            }

            // Stop if we've reached end of input or next transaction
            if remaining.is_empty() || self.is_transaction_start(remaining) {
                break;
            }
        }

        Ok((remaining, postings))
    }

    /// Parse a single posting
    fn parse_posting(&self, input: &'a str) -> IResult<&'a str, ZeroCopyPosting<'a>> {
        let (input, _) = space1(input)?; // Required indentation
        
        // Check for virtual posting markers
        let (input, (is_virtual, account_content)) = alt((
            // Virtual posting: (Account Name)
            map(delimited(char('('), take_while(|c: char| c != ')'), char(')')), 
                |acc: &str| (true, acc)),
            // Normal posting
            map(take_while1(|c: char| c != ' ' && c != '\t' && c != '\n' && c != '\r'), 
                |acc: &str| (false, acc)),
        ))(input)?;

        let account = account_content.trim();
        
        // Optional amount and comment
        let (input, _) = space0(input)?;
        let (input, amount_and_comment) = take_while(|c: char| c != '\n' && c != '\r')(input)?;
        
        // Split amount and comment if present
        let (amount_str, note) = if amount_and_comment.is_empty() {
            (None, None)
        } else if let Some(semicolon_pos) = amount_and_comment.find(';') {
            let amount_part = amount_and_comment[..semicolon_pos].trim();
            let note_part = amount_and_comment[semicolon_pos + 1..].trim();
            
            let amount = if amount_part.is_empty() { None } else { Some(amount_part) };
            let note = if note_part.is_empty() { None } else { Some(note_part) };
            (amount, note)
        } else {
            let trimmed = amount_and_comment.trim();
            // Check if it looks like an amount (starts with digit, $, or -)
            if trimmed.chars().next()
                .map(|c| c.is_ascii_digit() || c == '$' || c == '-' || c == '+')
                .unwrap_or(false) {
                (Some(trimmed), None)
            } else {
                (None, Some(trimmed))
            }
        };
        
        let (input, _) = opt(line_ending)(input)?;
        
        Ok((input, ZeroCopyPosting {
            account,
            amount_str,
            note,
            flags: PostingFlags {
                is_virtual,
                must_balance: !is_virtual,
            },
        }))
    }

    /// Check if the current line starts a new transaction
    fn is_transaction_start(&self, input: &str) -> bool {
        // A transaction starts with a date pattern YYYY-MM-DD
        if input.len() < 10 {
            return false;
        }
        
        let chars: Vec<char> = input.chars().take(10).collect();
        if chars.len() != 10 {
            return false;
        }
        
        chars[4] == '-' && chars[7] == '-' && 
        chars[0..4].iter().all(|c| c.is_ascii_digit()) &&
        chars[5..7].iter().all(|c| c.is_ascii_digit()) &&
        chars[8..10].iter().all(|c| c.is_ascii_digit())
    }

    /// Get parsing statistics
    pub fn stats(&self) -> &ParserStats {
        &self.stats
    }
}

/// Convert zero-copy transaction to owned data structures
pub fn materialize_transaction<'a>(
    zc_transaction: &ZeroCopyTransaction<'a>
) -> crate::transaction::Transaction {
    use crate::transaction::Transaction;
    
    // For this example, we'll create a simplified conversion
    // In practice, you'd need to parse dates, amounts, etc.
    let description = intern_string(zc_transaction.description);
    let date_str = zc_transaction.date_str;
    
    // This is a simplified conversion - you'd need full implementation
    Transaction {
        // Initialize with placeholder values
        // In a real implementation, you'd parse the date_str into a proper date
        // and create proper Posting objects from the zero-copy postings
        ..Default::default()
    }
}

/// Streaming parser for very large files that can't fit in memory
pub struct StreamingParser {
    reader: BufReader<File>,
    line_buffer: String,
    current_line: usize,
    stats: ParserStats,
}

impl StreamingParser {
    /// Create a new streaming parser
    pub fn new(path: &Path) -> ZeroCopyResult<Self> {
        let file = File::open(path)?;
        let reader = BufReader::new(file);
        
        Ok(Self {
            reader,
            line_buffer: String::with_capacity(1024),
            current_line: 0,
            stats: ParserStats::default(),
        })
    }

    /// Parse transactions one at a time
    pub fn next_transaction(&mut self) -> ZeroCopyResult<Option<crate::transaction::Transaction>> {
        // Skip empty lines and comments
        loop {
            self.line_buffer.clear();
            match self.reader.read_line(&mut self.line_buffer)? {
                0 => return Ok(None), // EOF
                _ => {
                    self.current_line += 1;
                    let line = self.line_buffer.trim();
                    
                    if line.is_empty() || line.starts_with(';') {
                        continue; // Skip empty lines and comments
                    }
                    
                    // This line should be a transaction header
                    return self.parse_streaming_transaction();
                }
            }
        }
    }

    /// Parse a transaction from the streaming reader
    fn parse_streaming_transaction(&mut self) -> ZeroCopyResult<Option<crate::transaction::Transaction>> {
        // Simplified streaming transaction parsing
        // In practice, you'd implement full transaction parsing here
        
        self.stats.transactions_parsed += 1;
        
        // Create a placeholder transaction
        // You'd implement the actual parsing logic here
        Ok(Some(crate::transaction::Transaction::default()))
    }

    /// Get statistics
    pub fn stats(&self) -> &ParserStats {
        &self.stats
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::NamedTempFile;

    #[test]
    fn test_zero_copy_parser() {
        let journal_content = r#"2023-01-01 Opening Balance
    Assets:Checking     $1000.00
    Equity:Opening-Balances

2023-01-02 Grocery Store
    Expenses:Food       $45.67
    Assets:Checking

2023-01-03 (123) Paycheck
    Assets:Checking     $2000.00  ; Salary deposit
    Income:Salary
"#;

        let mut parser = ZeroCopyParser::new(journal_content);
        let transactions = parser.parse_journal().unwrap();
        
        assert_eq!(transactions.len(), 3);
        
        let first_txn = &transactions[0];
        assert_eq!(first_txn.date_str, "2023-01-01");
        assert_eq!(first_txn.description, "Opening Balance");
        assert_eq!(first_txn.postings.len(), 2);
        
        let third_txn = &transactions[2];
        assert_eq!(third_txn.code, Some("123"));
        assert_eq!(third_txn.description, "Paycheck");
        
        let stats = parser.stats();
        assert_eq!(stats.transactions_parsed, 3);
        assert!(stats.postings_parsed > 0);
    }

    #[test]
    fn test_memory_mapped_journal() {
        let mut temp_file = NamedTempFile::new().unwrap();
        write!(temp_file, r#"2023-01-01 Test Transaction
    Assets:Test     $100.00
    Equity:Test
"#).unwrap();

        let mapped = MappedJournal::from_path(temp_file.path()).unwrap();
        let content = mapped.content().unwrap();
        
        let mut parser = ZeroCopyParser::new(content);
        let transactions = parser.parse_journal().unwrap();
        
        assert_eq!(transactions.len(), 1);
        assert_eq!(transactions[0].date_str, "2023-01-01");
    }

    #[test]
    fn test_posting_flags() {
        let journal_content = r#"2023-01-01 Virtual Test
    (Assets:Virtual)    $100.00
    Assets:Real         $50.00
    Equity:Test
"#;

        let mut parser = ZeroCopyParser::new(journal_content);
        let transactions = parser.parse_journal().unwrap();
        
        assert_eq!(transactions.len(), 1);
        let postings = &transactions[0].postings;
        
        assert_eq!(postings.len(), 3);
        assert!(postings[0].flags.is_virtual);
        assert!(!postings[1].flags.is_virtual);
    }
}