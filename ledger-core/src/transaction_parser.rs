//! Transaction parsing implementation using nom combinators
//!
//! This module focuses specifically on parsing Ledger transaction format:
//! - Transaction headers with dates, codes, payees
//! - Multi-line postings with proper indentation
//! - Amount parsing with commodities and lot pricing
//! - Balance assertions and assignments
//! - Transaction metadata and tags

use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while, take_while1},
    character::complete::{
        alpha1, char, digit1, line_ending, space0, space1, not_line_ending
    },
    combinator::{map, opt, recognize},
    error::context,
    multi::many0,
    sequence::{delimited, preceded, tuple, pair},
    IResult,
};

use chrono::NaiveDate;
use rust_decimal::Decimal;
use std::collections::HashMap;

/// Transaction parsing result type
type ParseResult<'a, T> = IResult<&'a str, T>;

/// Simplified transaction structure for parsing
#[derive(Debug, Clone, PartialEq)]
pub struct ParsedTransaction {
    pub date: NaiveDate,
    pub aux_date: Option<NaiveDate>,
    pub status: Option<TransactionStatus>,
    pub code: Option<String>,
    pub payee: String,
    pub note: Option<String>,
    pub postings: Vec<ParsedPosting>,
    pub metadata: HashMap<String, String>,
}

/// Transaction status flags
#[derive(Debug, Clone, PartialEq)]
pub enum TransactionStatus {
    Cleared,   // *
    Pending,   // !
}

/// Simplified posting structure for parsing
#[derive(Debug, Clone, PartialEq)]
pub struct ParsedPosting {
    pub account: String,
    pub amount: Option<ParsedAmount>,
    pub balance_assertion: Option<ParsedAmount>,
    pub balance_assignment: Option<ParsedAmount>,
    pub lot_price: Option<ParsedAmount>,
    pub lot_date: Option<NaiveDate>,
    pub lot_note: Option<String>,
    pub comment: Option<String>,
    pub metadata: HashMap<String, String>,
}

/// Simplified amount structure for parsing
#[derive(Debug, Clone, PartialEq)]
pub struct ParsedAmount {
    pub value: Decimal,
    pub commodity: Option<String>,
    pub commodity_position: CommodityPosition,
}

/// Position of commodity symbol relative to amount
#[derive(Debug, Clone, PartialEq)]
pub enum CommodityPosition {
    Before,
    After,
}

// ============================================================================
// Date Parsing
// ============================================================================

/// Parse various date formats supported by Ledger
pub fn parse_date(input: &str) -> ParseResult<NaiveDate> {
    context("date", alt((
        parse_iso_date,        // YYYY-MM-DD
        parse_slash_date,      // YYYY/MM/DD
        parse_dot_date,        // YYYY.MM.DD
        parse_short_date,      // MM/DD (current year assumed)
    )))(input)
}

/// Parse ISO date format: YYYY-MM-DD
fn parse_iso_date(input: &str) -> ParseResult<NaiveDate> {
    map(
        tuple((
            digit1,      // year
            char('-'),
            digit1,      // month
            char('-'),
            digit1,      // day
        )),
        |(year, _, month, _, day): (&str, _, &str, _, &str)| {
            let y: i32 = year.parse().unwrap_or(2024);
            let m: u32 = month.parse().unwrap_or(1);
            let d: u32 = day.parse().unwrap_or(1);
            NaiveDate::from_ymd_opt(y, m, d).unwrap_or_else(|| {
                NaiveDate::from_ymd_opt(2024, 1, 1).unwrap()
            })
        }
    )(input)
}

/// Parse slash date format: YYYY/MM/DD
fn parse_slash_date(input: &str) -> ParseResult<NaiveDate> {
    map(
        tuple((
            digit1,      // year
            char('/'),
            digit1,      // month
            char('/'),
            digit1,      // day
        )),
        |(year, _, month, _, day)| {
            let y: i32 = year.parse().unwrap_or(2024);
            let m: u32 = month.parse().unwrap_or(1);
            let d: u32 = day.parse().unwrap_or(1);
            NaiveDate::from_ymd_opt(y, m, d).unwrap_or_else(|| {
                NaiveDate::from_ymd_opt(2024, 1, 1).unwrap()
            })
        }
    )(input)
}

/// Parse dot date format: YYYY.MM.DD
fn parse_dot_date(input: &str) -> ParseResult<NaiveDate> {
    map(
        tuple((
            digit1,      // year
            char('.'),
            digit1,      // month
            char('.'),
            digit1,      // day
        )),
        |(year, _, month, _, day)| {
            let y: i32 = year.parse().unwrap_or(2024);
            let m: u32 = month.parse().unwrap_or(1);
            let d: u32 = day.parse().unwrap_or(1);
            NaiveDate::from_ymd_opt(y, m, d).unwrap_or_else(|| {
                NaiveDate::from_ymd_opt(2024, 1, 1).unwrap()
            })
        }
    )(input)
}

/// Parse short date format: MM/DD (assumes current year)
fn parse_short_date(input: &str) -> ParseResult<NaiveDate> {
    map(
        tuple((
            digit1,      // month
            char('/'),
            digit1,      // day
        )),
        |(month, _, day)| {
            let m: u32 = month.parse().unwrap_or(1);
            let d: u32 = day.parse().unwrap_or(1);
            // Default to 2024 for now - should use current year
            NaiveDate::from_ymd_opt(2024, m, d).unwrap_or_else(|| {
                NaiveDate::from_ymd_opt(2024, 1, 1).unwrap()
            })
        }
    )(input)
}

// ============================================================================
// Transaction Header Parsing
// ============================================================================

/// Parse complete transaction including header and postings
pub fn parse_transaction(input: &str) -> ParseResult<ParsedTransaction> {
    context("transaction", map(
        tuple((
            parse_transaction_header,
            many0(preceded(line_ending, parse_posting_line)),
        )),
        |(mut transaction, postings)| {
            transaction.postings = postings;
            transaction
        }
    ))(input)
}

/// Parse transaction header line
fn parse_transaction_header(input: &str) -> ParseResult<ParsedTransaction> {
    map(
        tuple((
            parse_date,
            opt(preceded(char('='), parse_date)), // aux date
            space0,
            opt(alt((char('*'), char('!')))),     // status
            space0,
            opt(delimited(char('('), take_until(")"), char(')'))), // code
            space0,
            parse_payee_and_note,
        )),
        |(date, aux_date, _, status, _, code, _, (payee, note))| {
            ParsedTransaction {
                date,
                aux_date,
                status: status.map(|s| match s {
                    '*' => TransactionStatus::Cleared,
                    '!' => TransactionStatus::Pending,
                    _ => TransactionStatus::Cleared,
                }),
                code: code.map(|s| s.to_string()),
                payee,
                note,
                postings: Vec::new(),
                metadata: HashMap::new(),
            }
        }
    )(input)
}

/// Parse payee and optional note
fn parse_payee_and_note(input: &str) -> ParseResult<(String, Option<String>)> {
    map(
        pair(
            take_while(|c| c != ';' && c != '\n' && c != '\r'),
            opt(preceded(char(';'), not_line_ending))
        ),
        |(payee, note): (&str, Option<&str>)| {
            (
                payee.trim().to_string(),
                note.map(|s| s.trim().to_string())
            )
        }
    )(input)
}

// ============================================================================
// Posting Parsing
// ============================================================================

/// Parse a posting line (must start with whitespace)  
fn parse_posting_line(input: &str) -> ParseResult<ParsedPosting> {
    let (input, _) = alt((
        map(space1, |_| ' '), 
        char('\t')
    ))(input)?; // consume whitespace
    parse_posting(input)
}

/// Parse posting content
fn parse_posting(input: &str) -> ParseResult<ParsedPosting> {
    context("posting", map(
        tuple((
            parse_account_name,
            space0,
            opt(parse_amount_spec),
            space0,
            opt(preceded(char(';'), not_line_ending)), // comment
        )),
        |(account, _, amount_spec, _, comment)| {
            let mut posting = ParsedPosting {
                account,
                amount: None,
                balance_assertion: None,
                balance_assignment: None,
                lot_price: None,
                lot_date: None,
                lot_note: None,
                comment: comment.map(|s| s.trim().to_string()),
                metadata: HashMap::new(),
            };

            // Parse amount specification if present
            if let Some((amount, assertion, assignment, lot_info)) = amount_spec {
                posting.amount = amount;
                posting.balance_assertion = assertion;
                posting.balance_assignment = assignment;
                if let Some((price, date, note)) = lot_info {
                    posting.lot_price = price;
                    posting.lot_date = date;
                    posting.lot_note = note;
                }
            }

            posting
        }
    ))(input)
}

/// Parse account name
fn parse_account_name(input: &str) -> ParseResult<String> {
    map(
        take_while1(|c: char| !c.is_whitespace() && c != ';'),
        |s: &str| s.to_string()
    )(input)
}

/// Parse amount specification (amount + optional assertions/assignments/lot info)
fn parse_amount_spec(input: &str) -> ParseResult<(
    Option<ParsedAmount>, 
    Option<ParsedAmount>,  // balance assertion
    Option<ParsedAmount>,  // balance assignment  
    Option<(Option<ParsedAmount>, Option<NaiveDate>, Option<String>)> // lot info
)> {
    map(
        tuple((
            opt(parse_amount),
            space0,
            opt(parse_balance_assertion),
            space0,
            opt(parse_balance_assignment),
            space0,
            opt(parse_lot_info),
        )),
        |(amount, _, assertion, _, assignment, _, lot_info)| {
            (amount, assertion, assignment, lot_info)
        }
    )(input)
}

/// Parse balance assertion: = amount
fn parse_balance_assertion(input: &str) -> ParseResult<ParsedAmount> {
    preceded(char('='), preceded(space0, parse_amount))(input)
}

/// Parse balance assignment: := amount  
fn parse_balance_assignment(input: &str) -> ParseResult<ParsedAmount> {
    preceded(tag(":="), preceded(space0, parse_amount))(input)
}

/// Parse lot information: {price} [date] (note)
fn parse_lot_info(input: &str) -> ParseResult<(
    Option<ParsedAmount>,
    Option<NaiveDate>, 
    Option<String>
)> {
    map(
        tuple((
            opt(delimited(char('{'), parse_amount, char('}'))), // lot price
            space0,
            opt(delimited(char('['), parse_date, char(']'))),   // lot date
            space0,
            opt(delimited(char('('), take_until(")"), char(')'))), // lot note
        )),
        |(price, _, date, _, note)| {
            (price, date, note.map(|s| s.to_string()))
        }
    )(input)
}

// ============================================================================
// Amount Parsing
// ============================================================================

/// Parse monetary amount with optional commodity
pub fn parse_amount(input: &str) -> ParseResult<ParsedAmount> {
    context("amount", alt((
        parse_amount_commodity_before,
        parse_amount_commodity_after,
        parse_amount_no_commodity,
    )))(input)
}

/// Parse amount with commodity before: $100.50
fn parse_amount_commodity_before(input: &str) -> ParseResult<ParsedAmount> {
    map(
        tuple((
            parse_commodity_symbol,
            space0,
            parse_number,
        )),
        |(commodity, _, value)| {
            ParsedAmount {
                value,
                commodity: Some(commodity),
                commodity_position: CommodityPosition::Before,
            }
        }
    )(input)
}

/// Parse amount with commodity after: 100.50 USD
fn parse_amount_commodity_after(input: &str) -> ParseResult<ParsedAmount> {
    map(
        tuple((
            parse_number,
            space1,
            parse_commodity_symbol,
        )),
        |(value, _, commodity)| {
            ParsedAmount {
                value,
                commodity: Some(commodity),
                commodity_position: CommodityPosition::After,
            }
        }
    )(input)
}

/// Parse amount without commodity: 100.50
fn parse_amount_no_commodity(input: &str) -> ParseResult<ParsedAmount> {
    map(
        parse_number,
        |value| {
            ParsedAmount {
                value,
                commodity: None,
                commodity_position: CommodityPosition::After,
            }
        }
    )(input)
}

/// Parse commodity symbol (letters or special symbols)
fn parse_commodity_symbol(input: &str) -> ParseResult<String> {
    alt((
        // Special symbols like $, €, £, etc.
        map(char('$'), |_| "$".to_string()),
        map(char('€'), |_| "€".to_string()),
        map(char('£'), |_| "£".to_string()),
        map(char('¥'), |_| "¥".to_string()),
        
        // Alpha commodity codes like USD, EUR, etc.
        map(alpha1, |s: &str| s.to_string()),
        
        // Quoted commodity symbols like "US Dollar"
        delimited(
            char('"'),
            map(take_until("\""), |s: &str| s.to_string()),
            char('"')
        ),
    ))(input)
}

/// Parse numeric value with optional decimal places
fn parse_number(input: &str) -> ParseResult<Decimal> {
    map(
        recognize(tuple((
            opt(char('-')),
            digit1,
            opt(tuple((
                alt((char('.'), char(','))), // decimal separator
                digit1
            ))),
        ))),
        |s: &str| {
            // Handle comma as decimal separator
            let normalized = s.replace(',', ".");
            normalized.parse::<Decimal>().unwrap_or_else(|_| Decimal::ZERO)
        }
    )(input)
}

// ============================================================================
// Utilities
// ============================================================================

/// Check if line starts with whitespace (posting indicator)
pub fn is_posting_line(input: &str) -> bool {
    input.starts_with(' ') || input.starts_with('\t')
}

/// Parse metadata tags from comment
pub fn parse_metadata_tags(comment: &str) -> HashMap<String, String> {
    let mut metadata = HashMap::new();
    
    // Simple tag parsing: key:value, key2:value2
    for part in comment.split(',') {
        let part = part.trim();
        if let Some(colon_pos) = part.find(':') {
            let key = part[..colon_pos].trim().to_string();
            let value = part[colon_pos + 1..].trim().to_string();
            metadata.insert(key, value);
        } else if !part.is_empty() {
            metadata.insert(part.to_string(), String::new());
        }
    }
    
    metadata
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_iso_date() {
        let result = parse_iso_date("2024-03-15");
        assert!(result.is_ok());
        let (_, date) = result.unwrap();
        assert_eq!(date, NaiveDate::from_ymd_opt(2024, 3, 15).unwrap());
    }

    #[test]
    fn test_parse_slash_date() {
        let result = parse_slash_date("2024/03/15");
        assert!(result.is_ok());
        let (_, date) = result.unwrap();
        assert_eq!(date, NaiveDate::from_ymd_opt(2024, 3, 15).unwrap());
    }

    #[test]
    fn test_parse_amount_with_dollar() {
        let result = parse_amount("$100.50");
        assert!(result.is_ok());
        let (_, amount) = result.unwrap();
        assert_eq!(amount.value, Decimal::new(10050, 2));
        assert_eq!(amount.commodity, Some("$".to_string()));
        assert_eq!(amount.commodity_position, CommodityPosition::Before);
    }

    #[test]
    fn test_parse_amount_with_commodity_after() {
        let result = parse_amount("100.50 USD");
        assert!(result.is_ok());
        let (_, amount) = result.unwrap();
        assert_eq!(amount.value, Decimal::new(10050, 2));
        assert_eq!(amount.commodity, Some("USD".to_string()));
        assert_eq!(amount.commodity_position, CommodityPosition::After);
    }

    #[test]
    fn test_parse_account_name() {
        let result = parse_account_name("Assets:Checking");
        assert!(result.is_ok());
        let (_, account) = result.unwrap();
        assert_eq!(account, "Assets:Checking");
    }

    #[test]
    fn test_parse_transaction_header() {
        let input = "2024-03-15 * (123) Grocery shopping";
        let result = parse_transaction_header(input);
        assert!(result.is_ok());
        let (_, transaction) = result.unwrap();
        assert_eq!(transaction.date, NaiveDate::from_ymd_opt(2024, 3, 15).unwrap());
        assert_eq!(transaction.status, Some(TransactionStatus::Cleared));
        assert_eq!(transaction.code, Some("123".to_string()));
        assert_eq!(transaction.payee, "Grocery shopping");
    }

    #[test]
    fn test_parse_posting() {
        let result = parse_posting("Assets:Checking  $-100.00");
        assert!(result.is_ok());
        let (_, posting) = result.unwrap();
        assert_eq!(posting.account, "Assets:Checking");
        assert!(posting.amount.is_some());
        let amount = posting.amount.unwrap();
        assert_eq!(amount.value, Decimal::new(-10000, 2));
        assert_eq!(amount.commodity, Some("$".to_string()));
    }

    #[test]
    fn test_parse_balance_assertion() {
        let result = parse_balance_assertion("= $500.00");
        assert!(result.is_ok());
        let (_, amount) = result.unwrap();
        assert_eq!(amount.value, Decimal::new(50000, 2));
        assert_eq!(amount.commodity, Some("$".to_string()));
    }

    #[test]
    fn test_metadata_parsing() {
        let metadata = parse_metadata_tags("category:food, project:kitchen, urgent");
        assert_eq!(metadata.get("category"), Some(&"food".to_string()));
        assert_eq!(metadata.get("project"), Some(&"kitchen".to_string()));
        assert_eq!(metadata.get("urgent"), Some(&String::new()));
    }
}