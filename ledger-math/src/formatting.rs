//! Formatting utilities for amounts and balances
//!
//! This module provides display formatting functionality that matches the
//! C++ ledger implementation, including precision control, commodity formatting,
//! width/padding control, and columnar alignment.

use num_bigint::BigInt;
use num_rational::BigRational;
use num_traits::{Signed, Zero};

use crate::amount::{Amount, Precision};
use crate::balance::Balance;
use crate::commodity::CommodityRef;
use crate::CommodityFlags;

/// Formatting flags matching C++ AMOUNT_PRINT_* constants
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FormatFlags(u8);

impl FormatFlags {
    pub const NO_FLAGS: FormatFlags = FormatFlags(0x00);
    pub const RIGHT_JUSTIFY: FormatFlags = FormatFlags(0x01);
    pub const COLORIZE: FormatFlags = FormatFlags(0x02);
    pub const NO_COMPUTED_ANNOTATIONS: FormatFlags = FormatFlags(0x04);
    pub const ELIDE_COMMODITY_QUOTES: FormatFlags = FormatFlags(0x08);

    pub fn has_flag(self, flag: FormatFlags) -> bool {
        (self.0 & flag.0) != 0
    }

    pub fn set_flag(&mut self, flag: FormatFlags) {
        self.0 |= flag.0;
    }

    pub fn clear_flag(&mut self, flag: FormatFlags) {
        self.0 &= !flag.0;
    }
}

impl Default for FormatFlags {
    fn default() -> Self {
        Self::NO_FLAGS
    }
}

/// Format configuration for displaying amounts and balances
#[derive(Debug, Clone, Default)]
pub struct FormatConfig {
    /// Display precision override (None uses commodity/amount precision)
    pub precision: Option<Precision>,

    /// Minimum width for output
    pub min_width: Option<usize>,

    /// Maximum width for output (truncation)
    pub max_width: Option<usize>,

    /// Use thousands separators
    pub thousands_sep: bool,

    /// Use decimal comma instead of decimal point
    pub decimal_comma: bool,

    /// Use colon for time formatting (h:m format)
    pub time_colon: bool,

    /// Format flags
    pub flags: FormatFlags,
}

impl FormatConfig {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn with_precision(mut self, precision: Precision) -> Self {
        self.precision = Some(precision);
        self
    }

    pub fn with_width(mut self, min: usize, max: Option<usize>) -> Self {
        self.min_width = Some(min);
        self.max_width = max;
        self
    }

    pub fn with_thousands_sep(mut self, sep: bool) -> Self {
        self.thousands_sep = sep;
        self
    }

    pub fn with_decimal_comma(mut self, comma: bool) -> Self {
        self.decimal_comma = comma;
        self
    }

    pub fn with_time_colon(mut self, colon: bool) -> Self {
        self.time_colon = colon;
        self
    }

    pub fn with_flags(mut self, flags: FormatFlags) -> Self {
        self.flags = flags;
        self
    }

    pub fn right_justify(mut self) -> Self {
        self.flags.set_flag(FormatFlags::RIGHT_JUSTIFY);
        self
    }
}

/// Format a rational number to string with specified precision and options
pub fn format_rational(
    rational: &BigRational,
    precision: Precision,
    config: &FormatConfig,
    commodity: Option<&CommodityRef>,
) -> String {
    if rational.is_zero() {
        return if precision > 0 {
            format!("0.{}", "0".repeat(precision as usize))
        } else {
            "0".to_string()
        };
    }

    // Convert to decimal representation with specified precision
    let scale = BigInt::from(10).pow(precision as u32);
    let scaled = rational * &BigRational::from_integer(scale.clone());

    // Round to nearest integer (banker's rounding)
    let rounded = if scaled.is_positive() {
        (&scaled + &BigRational::new(BigInt::from(1), BigInt::from(2))).floor()
    } else {
        (&scaled - &BigRational::new(BigInt::from(1), BigInt::from(2))).ceil()
    };

    let rounded_int = rounded.to_integer();

    // Convert back to decimal string
    let mut digits = rounded_int.to_string();
    let negative = digits.starts_with('-');
    if negative {
        digits = digits[1..].to_string();
    }

    // Ensure we have enough digits for the precision
    while digits.len() <= precision as usize {
        digits = format!("0{}", digits);
    }

    // Insert decimal point if needed
    let result = if precision > 0 {
        let split_pos = digits.len() - precision as usize;
        let (integer_part, decimal_part) = digits.split_at(split_pos);

        let mut formatted_integer = format_integer_with_separators(integer_part, config, commodity);
        if formatted_integer.is_empty() {
            formatted_integer = "0".to_string();
        }

        let decimal_point = if config.time_colon && commodity.is_some_and(is_time_commodity) {
            ":"
        } else if config.decimal_comma {
            ","
        } else {
            "."
        };

        format!("{}{}{}", formatted_integer, decimal_point, decimal_part)
    } else {
        format_integer_with_separators(&digits, config, commodity)
    };

    if negative {
        format!("-{}", result)
    } else {
        format!(" {}", result)
    }
}

/// Format integer part with thousands separators
fn format_integer_with_separators(
    digits: &str,
    config: &FormatConfig,
    commodity: Option<&CommodityRef>,
) -> String {
    if !config.thousands_sep || digits.len() <= 3 {
        return digits.to_string();
    }

    let separator = if config.time_colon && commodity.is_some_and(is_time_commodity) {
        ":"
    } else if config.decimal_comma {
        "."
    } else {
        ","
    };

    let mut result = String::new();
    let chars: Vec<char> = digits.chars().collect();

    for (i, &ch) in chars.iter().enumerate() {
        result.push(ch);
        let remaining = chars.len() - i - 1;
        if remaining > 0 && remaining % 3 == 0 {
            result.push_str(separator);
        }
    }

    result
}

/// Check if a commodity represents time (hours, minutes, seconds)
fn is_time_commodity(commodity: &CommodityRef) -> bool {
    let symbol = commodity.symbol();
    matches!(symbol, "h" | "m" | "s")
}

/// Apply width formatting and justification
pub fn apply_width_formatting(text: &str, config: &FormatConfig) -> String {
    let min_width = config.min_width.unwrap_or(0);
    let max_width = config.max_width;

    // First truncate if max_width is specified
    let mut result = if let Some(max_w) = max_width {
        if text.len() > max_w {
            if max_w > 3 {
                format!("{}...", &text[..max_w - 3])
            } else if max_w > 0 {
                text[..max_w].to_string()
            } else {
                String::new()
            }
        } else {
            text.to_string()
        }
    } else {
        text.to_string()
    };

    // Then apply minimum width padding
    if result.len() < min_width {
        let padding = min_width - result.len();
        if config.flags.has_flag(FormatFlags::RIGHT_JUSTIFY) {
            result = format!("{}{}", " ".repeat(padding), result);
        } else {
            result = format!("{}{}", result, " ".repeat(padding));
        }
    }

    result
}

/// Format an amount with full formatting options
pub fn format_amount(amount: &Amount, config: &FormatConfig) -> String {
    if amount.is_null() {
        return apply_width_formatting("<null>", config);
    }

    let precision = config.precision.unwrap_or_else(|| amount.display_precision());

    let quantity_str = if let Some(rational) = amount.to_rational() {
        format_rational(rational, precision, config, amount.commodity())
    } else {
        "0".to_string()
    };

    // Add commodity formatting
    let formatted = if let Some(commodity_ref) = amount.commodity() {
        format_amount_with_commodity(&quantity_str, commodity_ref, config)
    } else {
        quantity_str
    };

    apply_width_formatting(&formatted, config)
}

/// Format amount with commodity symbol and annotations
fn format_amount_with_commodity(
    quantity_str: &str,
    commodity: &CommodityRef,
    config: &FormatConfig,
) -> String {
    let symbol = commodity.symbol();

    // For now, assume prefixed style (like $100.00)
    // In a full implementation, we'd check commodity formatting flags
    if symbol.is_empty() {
        quantity_str.to_string()
    } else {
        let formatted_commodity = if config.flags.has_flag(FormatFlags::ELIDE_COMMODITY_QUOTES)
            || symbol.chars().all(|c| c.is_alphabetic())
        {
            symbol.to_string()
        } else {
            // Add quotes if symbol contains special characters
            format!("\"{}\"", symbol)
        };

        if commodity.has_flags(CommodityFlags::STYLE_SUFFIXED) {
            format!("{} {}", quantity_str, formatted_commodity)
        } else {
            format!("{}{}", formatted_commodity, quantity_str)
        }
    }
}

/// Format a balance with multiple commodities
pub fn format_balance(balance: &Balance, config: &FormatConfig) -> String {
    if balance.is_empty() {
        return apply_width_formatting("0", config);
    }

    if balance.single_amount() {
        // Single commodity balance - format as amount
        if let Ok(amount) = balance.to_amount() {
            return format_amount(&amount, config);
        }
    }

    // Multi-commodity balance - format each amount on separate lines
    let mut lines = Vec::new();
    let mut amounts: Vec<_> = balance.amounts().collect();

    // Sort by commodity symbol for consistent output
    amounts.sort_by(|a, b| a.0.symbol().cmp(b.0.symbol()));

    for (_, amount) in amounts {
        lines.push(format_amount(amount, config));
    }

    apply_width_formatting(&lines.join("\n"), config)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::amount::Amount;
    use crate::balance::Balance;

    #[test]
    fn test_format_rational_zero() {
        let zero = BigRational::zero();
        let config = FormatConfig::default();

        assert_eq!(format_rational(&zero, 0, &config, None), "0");
        assert_eq!(format_rational(&zero, 2, &config, None), "0.00");
    }

    #[test]
    fn test_format_rational_integer() {
        let val = BigRational::from_integer(BigInt::from(123));
        let config = FormatConfig::default();

        assert_eq!(format_rational(&val, 0, &config, None), "123");
        assert_eq!(format_rational(&val, 2, &config, None), "123.00");
    }

    #[test]
    fn test_format_rational_decimal() {
        let val = BigRational::new(BigInt::from(12345), BigInt::from(100)); // 123.45
        let config = FormatConfig::default();

        assert_eq!(format_rational(&val, 2, &config, None), "123.45");
        assert_eq!(format_rational(&val, 1, &config, None), "123.5");
        assert_eq!(format_rational(&val, 3, &config, None), "123.450");
    }

    #[test]
    fn test_format_rational_negative() {
        let val = BigRational::new(BigInt::from(-12345), BigInt::from(100)); // -123.45
        let config = FormatConfig::default();

        assert_eq!(format_rational(&val, 2, &config, None), "-123.45");
    }

    #[test]
    fn test_format_rational_with_thousands_sep() {
        let val = BigRational::from_integer(BigInt::from(1234567));
        let config = FormatConfig::default().with_thousands_sep(true);

        assert_eq!(format_rational(&val, 0, &config, None), "1,234,567");
        assert_eq!(format_rational(&val, 2, &config, None), "1,234,567.00");
    }

    #[test]
    fn test_format_rational_with_decimal_comma() {
        let val = BigRational::new(BigInt::from(12345), BigInt::from(100)); // 123.45
        let config = FormatConfig::default().with_decimal_comma(true);

        assert_eq!(format_rational(&val, 2, &config, None), "123,45");
    }

    #[test]
    fn test_format_rational_with_decimal_comma_and_thousands() {
        let val = BigRational::new(BigInt::from(123456789), BigInt::from(100)); // 1234567.89
        let config = FormatConfig::default().with_decimal_comma(true).with_thousands_sep(true);

        assert_eq!(format_rational(&val, 2, &config, None), "1.234.567,89");
    }

    #[test]
    fn test_width_formatting() {
        let config = FormatConfig::default().with_width(10, None);

        assert_eq!(apply_width_formatting("123", &config), "123       ");

        let config = FormatConfig::default().with_width(10, None).right_justify();

        assert_eq!(apply_width_formatting("123", &config), "       123");
    }

    #[test]
    fn test_width_formatting_truncation() {
        let config = FormatConfig::default().with_width(5, Some(8));

        assert_eq!(apply_width_formatting("123", &config), "123  ");
        assert_eq!(apply_width_formatting("very long text", &config), "very ...");
    }

    #[test]
    fn test_format_amount_simple() {
        let amount = Amount::from_i64(12345);
        let config = FormatConfig::default();

        assert_eq!(format_amount(&amount, &config), "12345");
    }

    #[test]
    fn test_format_amount_with_precision() {
        let amount = Amount::from_decimal(rust_decimal::Decimal::new(12345, 2)).unwrap(); // 123.45
        let config = FormatConfig::default().with_precision(2);

        assert_eq!(format_amount(&amount, &config), "123.45");
    }

    #[test]
    fn test_format_null_amount() {
        let amount = Amount::null();
        let config = FormatConfig::default();

        assert_eq!(format_amount(&amount, &config), "<null>");
    }

    #[test]
    fn test_format_balance_empty() {
        let balance = Balance::new();
        let config = FormatConfig::default();

        assert_eq!(format_balance(&balance, &config), "0");
    }

    #[test]
    fn test_format_balance_single() {
        let amount = Amount::from_i64(12345);
        let balance = Balance::from_amount(amount).unwrap();
        let config = FormatConfig::default();

        assert_eq!(format_balance(&balance, &config), "12345");
    }
}
