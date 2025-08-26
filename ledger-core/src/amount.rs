//! Amount type with commodity support and arbitrary precision arithmetic

use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};
use std::fmt;

/// Represents a monetary amount with optional commodity
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Amount {
    /// The numeric value
    value: Decimal,
    /// Optional commodity reference
    commodity: Option<String>,
    /// Display precision
    display_precision: Option<u32>,
}

impl Amount {
    /// Create a new Amount with a value
    pub fn new(value: Decimal) -> Self {
        Self { value, commodity: None, display_precision: None }
    }

    /// Create an Amount with a commodity
    pub fn with_commodity(value: Decimal, commodity: String) -> Self {
        Self { value, commodity: Some(commodity), display_precision: None }
    }

    /// Get the numeric value
    pub fn value(&self) -> Decimal {
        self.value
    }

    /// Get the commodity if present
    pub fn commodity(&self) -> Option<&String> {
        self.commodity.as_ref()
    }
}

impl fmt::Display for Amount {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(ref commodity) = self.commodity {
            write!(f, "{} {}", commodity, self.value)
        } else {
            write!(f, "{}", self.value)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_amount_creation() {
        let amount = Amount::new(Decimal::from(100));
        assert_eq!(amount.value(), Decimal::from(100));
        assert_eq!(amount.commodity(), None);
    }

    #[test]
    fn test_amount_with_commodity() {
        let amount = Amount::with_commodity(Decimal::from(100), "USD".to_string());
        assert_eq!(amount.value(), Decimal::from(100));
        assert_eq!(amount.commodity(), Some(&"USD".to_string()));
    }
}