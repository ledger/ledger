//! Multi-commodity balance management

use crate::amount::Amount;
use std::collections::HashMap;

/// Represents a balance containing amounts in multiple commodities
#[derive(Debug, Clone, Default)]
pub struct Balance {
    /// Map of commodity to amount
    amounts: HashMap<String, Amount>,
}

impl Balance {
    /// Create a new empty balance
    pub fn new() -> Self {
        Self::default()
    }

    /// Add an amount to the balance
    pub fn add_amount(&mut self, amount: Amount) {
        // TODO: Implement balance addition logic
        if let Some(commodity) = amount.commodity() {
            self.amounts.insert(commodity.clone(), amount);
        }
    }

    /// Get the total number of commodities
    pub fn commodity_count(&self) -> usize {
        self.amounts.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rust_decimal::Decimal;

    #[test]
    fn test_balance_creation() {
        let balance = Balance::new();
        assert_eq!(balance.commodity_count(), 0);
    }

    #[test]
    fn test_balance_add_amount() {
        let mut balance = Balance::new();
        let amount = Amount::with_commodity(Decimal::from(100), "USD".to_string());
        balance.add_amount(amount);
        assert_eq!(balance.commodity_count(), 1);
    }
}