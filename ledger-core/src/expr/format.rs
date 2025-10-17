//! Format string processor for report formatting
//!
//! This module provides format string parsing and evaluation capabilities
//! for customizing report output with % directives similar to printf-style formatting.

use crate::balance::Balance;
use crate::posting::Posting;
use crate::transaction::Transaction;
use ledger_math::amount::Amount;
use std::collections::HashMap;
use std::fmt;

/// Errors that can occur during format string processing
#[derive(Debug, Clone)]
pub enum FormatError {
    /// Invalid format directive
    InvalidDirective(String),
    /// Missing field value
    MissingField(String),
    /// Width/alignment specification error
    InvalidSpecification(String),
    /// Expression evaluation error
    ExpressionError(String),
}

impl fmt::Display for FormatError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FormatError::InvalidDirective(msg) => write!(f, "Invalid format directive: {}", msg),
            FormatError::MissingField(msg) => write!(f, "Missing field: {}", msg),
            FormatError::InvalidSpecification(msg) => write!(f, "Invalid specification: {}", msg),
            FormatError::ExpressionError(msg) => write!(f, "Expression error: {}", msg),
        }
    }
}

impl std::error::Error for FormatError {}

/// Result type for format operations
pub type FormatResult<T> = Result<T, FormatError>;

/// Format specification for a field directive
#[derive(Debug, Clone)]
pub struct FormatSpec {
    /// Width of the field (None for variable width)
    pub width: Option<usize>,
    /// Left alignment flag
    pub left_align: bool,
    /// Zero padding flag
    pub zero_pad: bool,
    /// Plus sign flag for positive numbers
    pub show_plus: bool,
    /// Field name or directive
    pub field: String,
}

impl FormatSpec {
    /// Parse a format specification from a string
    pub fn parse(spec: &str) -> FormatResult<Self> {
        let mut chars = spec.chars().peekable();
        let mut width = None;
        let mut left_align = false;
        let mut zero_pad = false;
        let mut show_plus = false;
        let mut position = 0;

        // Skip the % character
        if chars.next() != Some('%') {
            return Err(FormatError::InvalidDirective(format!(
                "Format spec must start with %: {}",
                spec
            )));
        }
        position += 1;

        // Parse flags
        while let Some(&ch) = chars.peek() {
            match ch {
                '-' => {
                    left_align = true;
                    chars.next();
                }
                '0' => {
                    // Only zero-pad if not left-aligned and not followed by digit
                    if !left_align {
                        chars.next();
                        if chars.peek().map_or(true, |c| !c.is_ascii_digit()) {
                            zero_pad = true;
                            continue;
                        } else {
                            // This '0' is part of width, put it back
                            // We can't put it back, so we'll handle width differently
                            break;
                        }
                    } else {
                        break;
                    }
                }
                '+' => {
                    show_plus = true;
                    chars.next();
                }
                _ => break,
            }
        }

        // Parse width
        let mut width_digits = String::new();
        while let Some(&ch) = chars.peek() {
            if ch.is_ascii_digit() {
                width_digits.push(ch);
                chars.next();
            } else {
                break;
            }
        }

        if !width_digits.is_empty() {
            width = Some(width_digits.parse::<usize>().map_err(|_| {
                FormatError::InvalidSpecification(format!("Invalid width: {}", width_digits))
            })?);
        }

        // Rest is the field name
        let field = chars.collect::<String>();
        if field.is_empty() {
            return Err(FormatError::InvalidDirective("Missing field name".to_string()));
        }

        Ok(FormatSpec { width, left_align, zero_pad, show_plus, field })
    }
}

/// Context for format string evaluation
#[derive(Debug)]
pub struct FormatContext<'a> {
    /// Current transaction being formatted
    pub transaction: Option<&'a Transaction>,
    /// Current posting being formatted
    pub posting: Option<&'a Posting>,
    /// Additional variables
    pub variables: HashMap<String, String>,
    /// Running balance (for register reports)
    pub balance: Option<&'a Balance>,
}

impl<'a> FormatContext<'a> {
    /// Create a new format context
    pub fn new() -> Self {
        Self { transaction: None, posting: None, variables: HashMap::new(), balance: None }
    }

    /// Set transaction context
    pub fn with_transaction(mut self, transaction: &'a Transaction) -> Self {
        self.transaction = Some(transaction);
        self
    }

    /// Set posting context
    pub fn with_posting(mut self, posting: &'a Posting) -> Self {
        self.posting = Some(posting);
        self
    }

    /// Set running balance
    pub fn with_balance(mut self, balance: &'a Balance) -> Self {
        self.balance = Some(balance);
        self
    }

    /// Add a variable
    pub fn add_variable<S: Into<String>>(&mut self, name: S, value: S) {
        self.variables.insert(name.into(), value.into());
    }
}

/// Format string processor
pub struct FormatProcessor;

impl FormatProcessor {
    /// Process a format string with the given context
    pub fn format(format_str: &str, context: &FormatContext) -> FormatResult<String> {
        let mut result = String::new();
        let mut chars = format_str.chars().peekable();

        while let Some(ch) = chars.next() {
            if ch == '%' {
                if chars.peek() == Some(&'%') {
                    // Escaped %
                    result.push('%');
                    chars.next();
                    continue;
                }

                // Parse format specification
                let mut spec_str = String::new();
                spec_str.push('%');

                // Collect the format specification
                while let Some(&next_ch) = chars.peek() {
                    spec_str.push(next_ch);
                    chars.next();

                    // Break on field name characters
                    if next_ch.is_ascii_alphabetic() || next_ch == '_' {
                        // Continue until we have a complete field name
                        while let Some(&name_ch) = chars.peek() {
                            if name_ch.is_ascii_alphanumeric() || name_ch == '_' {
                                spec_str.push(name_ch);
                                chars.next();
                            } else {
                                break;
                            }
                        }
                        break;
                    }
                }

                // Parse and apply the format specification
                let spec = FormatSpec::parse(&spec_str)?;
                let field_value = Self::extract_field(&spec.field, context)?;
                let formatted = Self::apply_formatting(&field_value, &spec)?;
                result.push_str(&formatted);
            } else {
                result.push(ch);
            }
        }

        Ok(result)
    }

    /// Extract a field value from the context
    fn extract_field(field_name: &str, context: &FormatContext) -> FormatResult<String> {
        match field_name {
            // Date fields
            "d" | "date" => {
                if let Some(tx) = context.transaction {
                    Ok(tx.date.format("%Y-%m-%d").to_string())
                } else {
                    Err(FormatError::MissingField("transaction".to_string()))
                }
            }
            "D" => {
                if let Some(tx) = context.transaction {
                    Ok(tx.date.format("%Y/%m/%d").to_string())
                } else {
                    Err(FormatError::MissingField("transaction".to_string()))
                }
            }
            "y" => {
                if let Some(tx) = context.transaction {
                    Ok(tx.date.format("%y").to_string())
                } else {
                    Err(FormatError::MissingField("transaction".to_string()))
                }
            }
            "Y" => {
                if let Some(tx) = context.transaction {
                    Ok(tx.date.format("%Y").to_string())
                } else {
                    Err(FormatError::MissingField("transaction".to_string()))
                }
            }
            "m" => {
                if let Some(tx) = context.transaction {
                    Ok(tx.date.format("%m").to_string())
                } else {
                    Err(FormatError::MissingField("transaction".to_string()))
                }
            }

            // Payee fields
            "p" | "payee" => {
                if let Some(posting) = context.posting {
                    Ok(posting
                        .payee
                        .as_ref()
                        .map(|s| s.to_string())
                        .or(context.transaction.map(|tx| tx.payee.to_string()))
                        .unwrap_or_else(|| String::new()))
                } else if let Some(tx) = context.transaction {
                    Ok(tx.payee.to_string())
                } else {
                    Err(FormatError::MissingField("payee".to_string()))
                }
            }

            // Account fields
            "a" | "account" => {
                if let Some(posting) = context.posting {
                    let account = posting.account.borrow();
                    Ok(account.fullname_immutable())
                } else {
                    Err(FormatError::MissingField("posting".to_string()))
                }
            }
            "A" => {
                if let Some(posting) = context.posting {
                    let account = posting.account.borrow();
                    let full_name = account.fullname_immutable();
                    Ok(full_name.split(':').last().unwrap_or(&full_name).to_string())
                } else {
                    Err(FormatError::MissingField("posting".to_string()))
                }
            }

            // Amount fields
            "t" | "amount" => {
                if let Some(posting) = context.posting {
                    if let Some(ref amount) = posting.amount {
                        Ok(amount.to_string())
                    } else {
                        Ok(String::new())
                    }
                } else {
                    Err(FormatError::MissingField("posting".to_string()))
                }
            }
            "T" | "total" => {
                if let Some(balance) = context.balance {
                    Ok(balance.to_string())
                } else {
                    Err(FormatError::MissingField("balance".to_string()))
                }
            }

            // Code fields
            "c" | "code" => {
                if let Some(tx) = context.transaction {
                    Ok(tx.code.as_ref().unwrap_or(&String::new()).clone())
                } else {
                    Err(FormatError::MissingField("transaction".to_string()))
                }
            }

            // Note fields
            "n" | "note" => {
                if let Some(posting) = context.posting {
                    Ok(posting
                        .note
                        .as_ref()
                        .map(|s| s.to_string())
                        .or(context
                            .transaction
                            .and_then(|tx| tx.note.as_ref().map(|s| s.to_string())))
                        .unwrap_or_else(|| String::new()))
                } else if let Some(tx) = context.transaction {
                    Ok(tx.note.as_ref().map(|s| s.to_string()).unwrap_or_else(|| String::new()))
                } else {
                    Err(FormatError::MissingField("note".to_string()))
                }
            }

            // Custom variables
            name => {
                if let Some(value) = context.variables.get(name) {
                    Ok(value.clone())
                } else {
                    Err(FormatError::MissingField(name.to_string()))
                }
            }
        }
    }

    /// Apply formatting specification to a value
    fn apply_formatting(value: &str, spec: &FormatSpec) -> FormatResult<String> {
        let mut result = value.to_string();

        // Apply width and alignment if specified
        if let Some(width) = spec.width {
            if result.len() < width {
                if spec.left_align {
                    // Left-align: pad on the right
                    result.push_str(&" ".repeat(width - result.len()));
                } else if spec.zero_pad && result.parse::<f64>().is_ok() {
                    // Zero-pad numbers: pad with zeros on the left
                    result = format!("{:0width$}", result.parse::<f64>().unwrap(), width = width);
                } else {
                    // Right-align: pad on the left
                    result = format!("{:>width$}", result, width = width);
                }
            } else if result.len() > width {
                // Truncate if too long
                result = result[..width].to_string();
            }
        }

        // Apply plus sign for numbers if requested
        if spec.show_plus {
            if let Ok(num) = result.parse::<f64>() {
                if num > 0.0 && !result.starts_with('+') {
                    result = format!("+{}", result);
                }
            }
        }

        Ok(result)
    }

    /// Get available format fields
    pub fn available_fields() -> Vec<(&'static str, &'static str)> {
        vec![
            ("d", "Date (YYYY-MM-DD)"),
            ("D", "Date (YYYY/MM/DD)"),
            ("y", "Year (YY)"),
            ("Y", "Year (YYYY)"),
            ("m", "Month (MM)"),
            ("p", "Payee"),
            ("a", "Account (full name)"),
            ("A", "Account (leaf name only)"),
            ("t", "Amount"),
            ("T", "Total/Balance"),
            ("c", "Code"),
            ("n", "Note"),
        ]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::account::Account;
    use crate::posting::Posting;
    use crate::transaction::Transaction;
    use chrono::NaiveDate;
    use ledger_math::amount::Amount;
    use std::cell::RefCell;
    use std::rc::Rc;
    use std::str::FromStr;

    fn create_test_transaction() -> Transaction {
        let date = NaiveDate::from_ymd_opt(2024, 3, 15).unwrap();
        let mut tx = Transaction::new(date, "Test Store".to_string());
        tx.code = Some("123".to_string());
        tx.note = Some("Test transaction".to_string());
        tx
    }

    #[test]
    fn test_format_spec_parsing() {
        let spec = FormatSpec::parse("%10d").unwrap();
        assert_eq!(spec.width, Some(10));
        assert!(!spec.left_align);
        assert_eq!(spec.field, "d");

        let spec = FormatSpec::parse("%-20payee").unwrap();
        assert_eq!(spec.width, Some(20));
        assert!(spec.left_align);
        assert_eq!(spec.field, "payee");

        let spec = FormatSpec::parse("%+t").unwrap();
        assert!(spec.show_plus);
        assert_eq!(spec.field, "t");
    }

    #[test]
    fn test_basic_formatting() {
        let tx = create_test_transaction();
        let context = FormatContext::new().with_transaction(&tx);

        let result = FormatProcessor::format("Date: %d", &context).unwrap();
        assert_eq!(result, "Date: 2024-03-15");

        let result = FormatProcessor::format("Payee: %p", &context).unwrap();
        assert_eq!(result, "Payee: Test Store");
    }

    #[test]
    fn test_width_formatting() {
        let tx = create_test_transaction();
        let context = FormatContext::new().with_transaction(&tx);

        let result = FormatProcessor::format("%10d", &context).unwrap();
        assert_eq!(result, "2024-03-15");

        let result = FormatProcessor::format("%-20p", &context).unwrap();
        assert_eq!(result, "Test Store          ");
    }

    #[test]
    fn test_posting_fields() {
        let tx = create_test_transaction();
        let mut tree = crate::account::AccountTree::new();
        let account_ref = tree.find_account("Assets:Checking", true).unwrap();
        let mut posting = Posting::new(account_ref);
        posting.amount = Some(Amount::from_str("100.00 USD").unwrap_or_default());
        posting.note = Some("Test posting".into());

        let context = FormatContext::new().with_transaction(&tx).with_posting(&posting);

        let result = FormatProcessor::format("Account: %a", &context).unwrap();
        assert_eq!(result, "Account: Assets:Checking");

        let result = FormatProcessor::format("Amount: %t", &context).unwrap();
        assert!(result.starts_with("Amount: "));
    }

    #[test]
    fn test_escaped_percent() {
        let context = FormatContext::new();
        let result = FormatProcessor::format("100%% complete", &context).unwrap();
        assert_eq!(result, "100% complete");
    }

    #[test]
    fn test_missing_field_error() {
        let context = FormatContext::new();
        let result = FormatProcessor::format("%d", &context);
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), FormatError::MissingField(_)));
    }

    #[test]
    fn test_custom_variables() {
        let mut context = FormatContext::new();
        context.add_variable("custom", "Custom Value");

        let result = FormatProcessor::format("Custom: %custom", &context).unwrap();
        assert_eq!(result, "Custom: Custom Value");
    }

    #[test]
    fn test_available_fields() {
        let fields = FormatProcessor::available_fields();
        assert!(!fields.is_empty());
        assert!(fields.iter().any(|(field, _)| *field == "d"));
        assert!(fields.iter().any(|(field, _)| *field == "p"));
        assert!(fields.iter().any(|(field, _)| *field == "a"));
    }
}
