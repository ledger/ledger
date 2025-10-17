//! Optimized string handling for Ledger performance
//!
//! This module provides high-performance string types and utilities optimized for
//! the most common string usage patterns in Ledger accounting operations.

use ahash::AHashMap;
use bumpalo::Bump;
use compact_str::CompactString as CompactStr;
use once_cell::sync::Lazy;
use smallvec::{smallvec, SmallVec};
use std::collections::HashMap;
use std::sync::Mutex;

/// A string type optimized for short strings commonly found in account names
/// Uses inline storage for strings up to 24 bytes on 64-bit platforms
pub type AccountName = CompactStr;

/// A string type optimized for commodity symbols (typically 3-4 characters)  
pub type CommoditySymbol = CompactStr;

/// A string type optimized for payee names with good memory locality
pub type PayeeName = CompactStr;

/// Buffer type for temporary string building operations
pub type StringBuffer = SmallVec<[u8; 128]>;

/// Optimized buffer for small string operations like formatting numbers
pub type SmallBuffer = SmallVec<[u8; 32]>;

/// String interning pool for frequently used strings
static STRING_INTERNER: Lazy<Mutex<StringInterner>> =
    Lazy::new(|| Mutex::new(StringInterner::new()));

/// String interner for deduplicating commonly used strings
#[derive(Debug)]
pub struct StringInterner {
    /// Map from string content to interned string
    strings: AHashMap<String, CompactStr>,
    /// Statistics for monitoring effectiveness
    stats: InternerStats,
}

#[derive(Debug, Default, Clone)]
pub struct InternerStats {
    /// Number of strings interned
    pub interned_count: usize,
    /// Number of intern requests
    pub intern_requests: usize,
    /// Number of cache hits
    pub cache_hits: usize,
    /// Estimated memory saved by interning
    pub memory_saved_bytes: usize,
}

impl StringInterner {
    pub fn new() -> Self {
        Self {
            strings: AHashMap::with_capacity(1024), // Pre-allocate for common cases
            stats: InternerStats::default(),
        }
    }

    /// Intern a string, returning a CompactStr that may be shared
    pub fn intern(&mut self, s: &str) -> CompactStr {
        self.stats.intern_requests += 1;

        if let Some(existing) = self.strings.get(s) {
            self.stats.cache_hits += 1;
            existing.clone()
        } else {
            let compact = CompactStr::new(s);
            let memory_saved = if s.len() > std::mem::size_of::<CompactStr>() {
                s.len() - std::mem::size_of::<CompactStr>()
            } else {
                0
            };

            self.stats.memory_saved_bytes += memory_saved;
            self.stats.interned_count += 1;

            self.strings.insert(s.to_owned(), compact.clone());
            compact
        }
    }

    /// Get current statistics
    pub fn stats(&self) -> &InternerStats {
        &self.stats
    }

    /// Clear the interner (useful for tests)
    pub fn clear(&mut self) {
        self.strings.clear();
        self.stats = InternerStats::default();
    }
}

/// Intern a string using the global interner
pub fn intern_string(s: &str) -> CompactStr {
    STRING_INTERNER.lock().unwrap().intern(s)
}

/// Get interning statistics  
pub fn intern_stats() -> InternerStats {
    STRING_INTERNER.lock().unwrap().stats().clone()
}

/// Zero-allocation string formatting utilities
pub struct FastFormatter {
    buffer: StringBuffer,
}

impl FastFormatter {
    pub fn new() -> Self {
        Self { buffer: SmallVec::new() }
    }

    /// Format a number with optional decimal places
    pub fn format_number(&mut self, number: i64, decimals: Option<u8>) -> &str {
        self.buffer.clear();

        // Handle negative numbers
        if number < 0 {
            self.buffer.push(b'-');
        }

        let abs_number = number.unsigned_abs();

        match decimals {
            Some(decimals) if decimals > 0 => {
                let divisor = 10_u64.pow(decimals as u32);
                let integer_part = abs_number / divisor;
                let fractional_part = abs_number % divisor;

                self.write_integer(integer_part);
                self.buffer.push(b'.');

                // Write fractional part with leading zeros
                let mut temp = fractional_part;
                let mut digits = SmallVec::<[u8; 8]>::new();

                for _ in 0..decimals {
                    digits.push((temp % 10) as u8 + b'0');
                    temp /= 10;
                }

                // Reverse to get correct order
                digits.reverse();
                self.buffer.extend_from_slice(&digits);
            }
            _ => {
                self.write_integer(abs_number);
            }
        }

        // Safe because we only wrote valid UTF-8
        unsafe { std::str::from_utf8_unchecked(&self.buffer) }
    }

    /// Write an integer to the buffer
    fn write_integer(&mut self, mut number: u64) {
        if number == 0 {
            self.buffer.push(b'0');
            return;
        }

        let mut digits = SmallVec::<[u8; 20]>::new(); // u64 max is 20 digits

        while number > 0 {
            digits.push((number % 10) as u8 + b'0');
            number /= 10;
        }

        // Reverse to get correct order
        digits.reverse();
        self.buffer.extend_from_slice(&digits);
    }

    /// Format a path by joining components with '/'
    pub fn format_path(&mut self, components: &[&str]) -> &str {
        self.buffer.clear();

        for (i, component) in components.iter().enumerate() {
            if i > 0 {
                self.buffer.push(b':');
            }
            self.buffer.extend_from_slice(component.as_bytes());
        }

        // Safe because we only wrote valid UTF-8
        unsafe { std::str::from_utf8_unchecked(&self.buffer) }
    }

    /// Clear the internal buffer for reuse
    pub fn clear(&mut self) {
        self.buffer.clear();
    }

    /// Get a string slice of the current buffer contents
    pub fn as_str(&self) -> &str {
        // Safe because we only write valid UTF-8
        unsafe { std::str::from_utf8_unchecked(&self.buffer) }
    }
}

impl Default for FastFormatter {
    fn default() -> Self {
        Self::new()
    }
}

/// String builder optimized for account path construction
#[derive(Debug)]
pub struct AccountPathBuilder {
    components: SmallVec<[CompactStr; 8]>, // Most accounts have < 8 levels
    separator: char,
}

impl AccountPathBuilder {
    pub fn new() -> Self {
        Self { components: SmallVec::new(), separator: ':' }
    }

    pub fn with_separator(separator: char) -> Self {
        Self { components: SmallVec::new(), separator }
    }

    /// Add a component to the path
    pub fn push(&mut self, component: &str) -> &mut Self {
        self.components.push(CompactStr::new(component));
        self
    }

    /// Add a component using a CompactStr directly
    pub fn push_compact(&mut self, component: CompactStr) -> &mut Self {
        self.components.push(component);
        self
    }

    /// Pop the last component
    pub fn pop(&mut self) -> Option<CompactStr> {
        self.components.pop()
    }

    /// Build the final path string
    pub fn build(&self) -> CompactStr {
        if self.components.is_empty() {
            return CompactStr::new("");
        }

        if self.components.len() == 1 {
            return self.components[0].clone();
        }

        // Estimate capacity needed
        let capacity: usize =
            self.components.iter().map(|c| c.len()).sum::<usize>() + (self.components.len() - 1);

        let mut result = String::with_capacity(capacity);

        for (i, component) in self.components.iter().enumerate() {
            if i > 0 {
                result.push(self.separator);
            }
            result.push_str(component);
        }

        CompactStr::new(result)
    }

    /// Get the depth (number of components)
    pub fn depth(&self) -> usize {
        self.components.len()
    }

    /// Get a component at a specific index
    pub fn component(&self, index: usize) -> Option<&CompactStr> {
        self.components.get(index)
    }

    /// Clear all components for reuse
    pub fn clear(&mut self) {
        self.components.clear();
    }

    /// Iterate over components
    pub fn components(&self) -> impl Iterator<Item = &CompactStr> {
        self.components.iter()
    }
}

impl Default for AccountPathBuilder {
    fn default() -> Self {
        Self::new()
    }
}

/// Optimized parsing utilities for common string patterns
pub struct FastParser;

impl FastParser {
    /// Parse a decimal number from a string slice (optimized for common cases)
    pub fn parse_decimal(s: &str) -> Result<(i64, u8), &'static str> {
        if s.is_empty() {
            return Err("Empty string");
        }

        let mut chars = s.chars();
        let mut negative = false;
        let mut result = 0i64;
        let mut decimal_places = 0u8;
        let mut found_decimal = false;

        // Handle sign
        if let Some(first) = chars.next() {
            match first {
                '-' => negative = true,
                '+' => {}
                '0'..='9' => {
                    result = (first as u8 - b'0') as i64;
                }
                '.' => {
                    found_decimal = true;
                }
                _ => return Err("Invalid character"),
            }
        }

        // Parse remaining digits
        for ch in chars {
            match ch {
                '0'..='9' => {
                    let digit = (ch as u8 - b'0') as i64;

                    if found_decimal {
                        decimal_places += 1;
                        if decimal_places > 6 {
                            // Limit precision
                            break;
                        }
                    }

                    result = result
                        .checked_mul(10)
                        .and_then(|r| r.checked_add(digit))
                        .ok_or("Number overflow")?;
                }
                '.' => {
                    if found_decimal {
                        return Err("Multiple decimal points");
                    }
                    found_decimal = true;
                }
                ',' => {
                    // Ignore thousands separator
                }
                _ => return Err("Invalid character"),
            }
        }

        if negative {
            result = -result;
        }

        Ok((result, decimal_places))
    }

    /// Fast account name validation (checks for invalid characters)
    pub fn validate_account_name(name: &str) -> bool {
        if name.is_empty() {
            return false;
        }

        // Account names can't start or end with separator
        if name.starts_with(':') || name.ends_with(':') {
            return false;
        }

        // Check for invalid characters
        for ch in name.chars() {
            match ch {
                'a'..='z' | 'A'..='Z' | '0'..='9' | ':' | ' ' | '-' | '_' | '.' => {
                    // Valid characters
                }
                _ => return false,
            }
        }

        true
    }

    /// Split account path into components without allocation
    pub fn split_account_path(path: &str) -> SmallVec<[&str; 8]> {
        if path.is_empty() {
            return SmallVec::new();
        }

        let mut components = SmallVec::new();
        let mut start = 0;

        for (i, ch) in path.char_indices() {
            if ch == ':' {
                if i > start {
                    components.push(&path[start..i]);
                }
                start = i + 1;
            }
        }

        // Add final component
        if start < path.len() {
            components.push(&path[start..]);
        }

        components
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_interning() {
        let mut interner = StringInterner::new();

        let s1 = interner.intern("Assets:Checking");
        let s2 = interner.intern("Assets:Checking");
        let s3 = interner.intern("Assets:Savings");

        assert_eq!(s1, s2);
        assert_ne!(s1, s3);

        let stats = interner.stats();
        assert_eq!(stats.interned_count, 2);
        assert_eq!(stats.cache_hits, 1);
    }

    #[test]
    fn test_fast_formatter() {
        let mut formatter = FastFormatter::new();

        assert_eq!(formatter.format_number(12345, Some(2)), "123.45");
        assert_eq!(formatter.format_number(-12345, Some(2)), "-123.45");
        assert_eq!(formatter.format_number(100, None), "100");

        assert_eq!(formatter.format_path(&["Assets", "Checking"]), "Assets:Checking");
        assert_eq!(
            formatter.format_path(&["Expenses", "Food", "Groceries"]),
            "Expenses:Food:Groceries"
        );
    }

    #[test]
    fn test_account_path_builder() {
        let mut builder = AccountPathBuilder::new();

        builder.push("Assets").push("Checking");

        assert_eq!(builder.build(), "Assets:Checking");
        assert_eq!(builder.depth(), 2);

        builder.pop();
        assert_eq!(builder.build(), "Assets");
    }

    #[test]
    fn test_fast_parser() {
        assert_eq!(FastParser::parse_decimal("123.45"), Ok((12345, 2)));
        assert_eq!(FastParser::parse_decimal("-123.45"), Ok((-12345, 2)));
        assert_eq!(FastParser::parse_decimal("100"), Ok((100, 0)));

        assert!(FastParser::validate_account_name("Assets:Checking"));
        assert!(!FastParser::validate_account_name(":Assets"));
        assert!(!FastParser::validate_account_name("Assets:"));

        let components = FastParser::split_account_path("Assets:Checking:Main");
        assert_eq!(components.as_slice(), &["Assets", "Checking", "Main"]);
    }

    #[test]
    fn test_account_name_types() {
        let account_name = AccountName::new("Assets:Checking");
        let commodity_symbol = CommoditySymbol::new("USD");
        let payee_name = PayeeName::new("Grocery Store");

        assert_eq!(account_name.len(), 15);
        assert_eq!(commodity_symbol.len(), 3);
        assert_eq!(payee_name.len(), 13);
    }
}
