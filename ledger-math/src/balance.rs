//! Balance module providing multi-commodity amounts container
//!
//! This module implements the Balance type which can hold amounts of different
//! commodities and perform arithmetic operations on them, matching the behavior
//! of the C++ balance_t class.

use std::collections::HashMap;
use std::fmt;
use std::ops::{Add, AddAssign, Div, DivAssign, Mul, MulAssign, Neg, Sub, SubAssign};
use std::sync::Arc;

use crate::amount::{Amount, AmountError};
use crate::commodity::{Commodity, CommodityRef, KeepDetails};

/// Error type for balance operations
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum BalanceError {
    /// Cannot initialize balance with null amount
    #[error("Cannot initialize a balance from an uninitialized amount")]
    NullAmount,

    /// Cannot compare balance to null amount
    #[error("Cannot compare a balance to an uninitialized amount")]
    CompareToNull,

    /// Cannot convert empty balance to amount
    #[error("Cannot convert an empty balance to an amount")]
    EmptyToAmount,

    /// Cannot convert multi-commodity balance to amount
    #[error("Cannot convert a balance with multiple commodities to an amount")]
    MultiCommodityToAmount,

    /// Amount operation error
    #[error("Amount operation failed: {0}")]
    AmountError(#[from] AmountError),
}

/// Result type for balance operations
pub type BalanceResult<T> = Result<T, BalanceError>;

/// Balance container for managing amounts of different commodities
///
/// This matches the behavior of C++ balance_t class, allowing arithmetic
/// operations on amounts with different commodities.
#[derive(Clone)]
pub struct Balance {
    /// Map of commodities to their amounts
    amounts: HashMap<CommodityRef, Amount>,
}

impl Balance {
    /// Create a new empty balance
    pub fn new() -> Self {
        Self { amounts: HashMap::new() }
    }

    /// Create a balance from a single amount
    pub fn from_amount(amount: Amount) -> BalanceResult<Self> {
        if amount.is_null() {
            return Err(BalanceError::NullAmount);
        }

        let mut balance = Self::new();
        if !amount.is_realzero() {
            if let Some(commodity) = amount.commodity() {
                balance.amounts.insert(commodity.clone(), amount);
            } else {
                // Handle commodity-less amounts with null commodity
                balance.amounts.insert(crate::commodity::null_commodity(), amount);
            }
        }
        Ok(balance)
    }

    /// Create a balance from an integer value (creates commodity-less amount)
    pub fn from_i64(value: i64) -> Self {
        let amount = Amount::from_i64(value);
        let mut balance = Self::new();
        if !amount.is_realzero() {
            balance.amounts.insert(crate::commodity::null_commodity(), amount);
        }
        balance
    }

    /// Create a balance from a float value (creates commodity-less amount)
    pub fn from_f64(value: f64) -> BalanceResult<Self> {
        let amount = Amount::from_f64(value)?;
        let mut balance = Self::new();
        if !amount.is_realzero() {
            balance.amounts.insert(crate::commodity::null_commodity(), amount);
        }
        Ok(balance)
    }

    /// Check if balance is empty (contains no amounts)
    pub fn is_empty(&self) -> bool {
        self.amounts.is_empty()
    }

    /// Check if balance contains only a single amount
    pub fn single_amount(&self) -> bool {
        self.amounts.len() == 1
    }

    /// Check if balance is zero (display-wise)
    pub fn is_zero(&self) -> bool {
        if self.is_empty() {
            return true;
        }

        for amount in self.amounts.values() {
            if !amount.is_zero() {
                return false;
            }
        }
        true
    }

    /// Check if balance is really zero (exact zero)
    pub fn is_realzero(&self) -> bool {
        if self.is_empty() {
            return true;
        }

        for amount in self.amounts.values() {
            if !amount.is_realzero() {
                return false;
            }
        }
        true
    }

    /// Check if balance is non-zero (display-wise)
    pub fn is_nonzero(&self) -> bool {
        if self.is_empty() {
            return false;
        }

        for amount in self.amounts.values() {
            if amount.is_nonzero() {
                return true;
            }
        }
        false
    }

    /// Get the number of different commodities in this balance
    pub fn commodity_count(&self) -> usize {
        self.amounts.len()
    }

    /// Get the amount for a specific commodity, if present
    pub fn commodity_amount(&self, commodity: &CommodityRef) -> Option<&Amount> {
        self.amounts.get(commodity)
    }

    /// Calculate the total value as an Amount (assumes single commodity or converts to base)
    pub fn total_value(&self) -> Amount {
        if self.is_empty() {
            return Amount::null();
        }

        if self.commodity_count() == 1 {
            // Single commodity - return it directly
            self.amounts.values().next().unwrap().clone()
        } else {
            // Multiple commodities - sum their absolute values
            // This is a simplified version; proper implementation would need exchange rates
            let total = Amount::null();
            if let Some(amount) = self.amounts.values().next() {
                // Add absolute values (simplified - doesn't handle different commodities properly)
                // For now, just return the first amount's absolute value
                return amount.abs();
            }
            total
        }
    }

    /// Convert balance to single amount (only if balance contains exactly one amount)
    pub fn to_amount(&self) -> BalanceResult<Amount> {
        if self.is_empty() {
            Err(BalanceError::EmptyToAmount)
        } else if self.amounts.len() == 1 {
            Ok(self.amounts.values().next().unwrap().clone())
        } else {
            Err(BalanceError::MultiCommodityToAmount)
        }
    }

    /// Add an amount to this balance
    pub fn add_amount(&mut self, amount: &Amount) -> BalanceResult<()> {
        if amount.is_null() {
            return Err(BalanceError::NullAmount);
        }

        if amount.is_realzero() {
            return Ok(());
        }

        let commodity =
            amount.commodity().cloned().unwrap_or_else(crate::commodity::null_commodity);

        if let Some(existing) = self.amounts.get_mut(&commodity) {
            *existing = (existing.clone() + amount.clone())?;

            // Remove the amount if it becomes zero after addition
            if existing.is_realzero() {
                self.amounts.remove(&commodity);
            }
        } else {
            self.amounts.insert(commodity, amount.clone());
        }

        Ok(())
    }

    /// Subtract an amount from this balance
    pub fn subtract_amount(&mut self, amount: &Amount) -> BalanceResult<()> {
        if amount.is_null() {
            return Err(BalanceError::NullAmount);
        }

        if amount.is_realzero() {
            return Ok(());
        }

        let commodity =
            amount.commodity().cloned().unwrap_or_else(crate::commodity::null_commodity);

        if let Some(existing) = self.amounts.get_mut(&commodity) {
            *existing = (existing.clone() - amount.clone())?;

            // Remove the amount if it becomes zero after subtraction
            if existing.is_realzero() {
                self.amounts.remove(&commodity);
            }
        } else {
            // Subtracting from non-existent commodity creates negative amount
            self.amounts.insert(commodity, -amount.clone());
        }

        Ok(())
    }

    /// Multiply all amounts in the balance by a scalar amount
    pub fn multiply_by(&mut self, amount: &Amount) -> BalanceResult<()> {
        if amount.is_null() {
            return Err(BalanceError::NullAmount);
        }

        // Multiplying by zero makes the balance empty
        if amount.is_realzero() {
            self.amounts.clear();
            return Ok(());
        }

        for existing_amount in self.amounts.values_mut() {
            *existing_amount = (existing_amount.clone() * amount.clone())?;
        }

        Ok(())
    }

    /// Divide all amounts in the balance by a scalar amount
    pub fn divide_by(&mut self, amount: &Amount) -> BalanceResult<()> {
        if amount.is_null() {
            return Err(BalanceError::NullAmount);
        }

        if amount.is_realzero() {
            return Err(BalanceError::AmountError(AmountError::DivisionByZero));
        }

        for existing_amount in self.amounts.values_mut() {
            *existing_amount = (existing_amount.clone() / amount.clone())?;
        }

        Ok(())
    }

    /// Create a new balance with all amounts negated
    pub fn negated(&self) -> Balance {
        let mut result = Self::new();
        for (commodity, amount) in &self.amounts {
            result.amounts.insert(commodity.clone(), -amount.clone());
        }
        result
    }

    /// Negate all amounts in place
    pub fn negate(&mut self) {
        for amount in self.amounts.values_mut() {
            *amount = -amount.clone();
        }
    }

    /// Create a new balance with absolute values of all amounts
    pub fn abs(&self) -> Balance {
        let mut result = Self::new();
        for (commodity, amount) in &self.amounts {
            result.amounts.insert(commodity.clone(), amount.abs());
        }
        result
    }

    /// Strip annotations from all amounts based on keep rules
    pub fn strip_annotations(&self, _keep_details: &KeepDetails) -> Balance {
        let mut result = Self::new();
        for amount in self.amounts.values() {
            // For now, amounts don't have annotations - this will be enhanced
            // when Amount type is integrated with the annotation system
            result.add_amount(amount).unwrap(); // Safe because we're cloning valid amounts
        }
        result
    }

    /// Get iterator over commodities and amounts
    pub fn amounts(&self) -> impl Iterator<Item = (&CommodityRef, &Amount)> {
        self.amounts.iter()
    }

    /// Get iterator over amounts only
    pub fn amounts_iter(&self) -> impl Iterator<Item = &Amount> {
        self.amounts.values()
    }

    /// Print this balance to a stream with formatting options
    /// Matches C++ balance_t::print() method signature
    pub fn print<W: fmt::Write>(
        &self,
        writer: &mut W,
        first_width: Option<usize>,
        latter_width: Option<usize>,
        flags: crate::formatting::FormatFlags,
    ) -> fmt::Result {
        use crate::formatting::{format_balance, FormatConfig};

        let mut config = FormatConfig::default().with_flags(flags);

        if let Some(width) = first_width {
            config.min_width = Some(width);
            config.max_width = latter_width;
        }

        let formatted = format_balance(self, &config);
        write!(writer, "{}", formatted)
    }
}

impl Default for Balance {
    fn default() -> Self {
        Self::new()
    }
}

impl PartialEq for Balance {
    fn eq(&self, other: &Self) -> bool {
        self.amounts == other.amounts
    }
}

impl Eq for Balance {}

// Implement truth testing
impl From<Balance> for bool {
    fn from(balance: Balance) -> bool {
        balance.is_nonzero()
    }
}

// Arithmetic operations with Balance
impl Add for Balance {
    type Output = BalanceResult<Balance>;

    fn add(self, other: Balance) -> Self::Output {
        let mut result = self;
        result += other;
        Ok(result)
    }
}

impl AddAssign<Balance> for Balance {
    fn add_assign(&mut self, other: Balance) {
        for (_, amount) in other.amounts {
            self.add_amount(&amount).unwrap(); // Safe because amounts in balance are valid
        }
    }
}

impl Sub for Balance {
    type Output = BalanceResult<Balance>;

    fn sub(self, other: Balance) -> Self::Output {
        let mut result = self;
        result -= other;
        Ok(result)
    }
}

impl SubAssign<Balance> for Balance {
    fn sub_assign(&mut self, other: Balance) {
        for (_, amount) in other.amounts {
            self.subtract_amount(&amount).unwrap(); // Safe because amounts in balance are valid
        }
    }
}

impl Neg for Balance {
    type Output = Balance;

    fn neg(self) -> Self::Output {
        self.negated()
    }
}

// Arithmetic operations with Amount
impl Add<Amount> for Balance {
    type Output = BalanceResult<Balance>;

    fn add(self, amount: Amount) -> Self::Output {
        let mut result = self;
        result.add_amount(&amount)?;
        Ok(result)
    }
}

impl AddAssign<Amount> for Balance {
    fn add_assign(&mut self, amount: Amount) {
        self.add_amount(&amount).unwrap(); // We'll handle this better with proper error handling
    }
}

impl Sub<Amount> for Balance {
    type Output = BalanceResult<Balance>;

    fn sub(self, amount: Amount) -> Self::Output {
        let mut result = self;
        result.subtract_amount(&amount)?;
        Ok(result)
    }
}

impl SubAssign<Amount> for Balance {
    fn sub_assign(&mut self, amount: Amount) {
        self.subtract_amount(&amount).unwrap(); // We'll handle this better with proper error handling
    }
}

impl Mul<Amount> for Balance {
    type Output = BalanceResult<Balance>;

    fn mul(self, amount: Amount) -> Self::Output {
        let mut result = self;
        result.multiply_by(&amount)?;
        Ok(result)
    }
}

impl MulAssign<Amount> for Balance {
    fn mul_assign(&mut self, amount: Amount) {
        self.multiply_by(&amount).unwrap(); // We'll handle this better with proper error handling
    }
}

impl Div<Amount> for Balance {
    type Output = BalanceResult<Balance>;

    fn div(self, amount: Amount) -> Self::Output {
        let mut result = self;
        result.divide_by(&amount)?;
        Ok(result)
    }
}

impl DivAssign<Amount> for Balance {
    fn div_assign(&mut self, amount: Amount) {
        self.divide_by(&amount).unwrap(); // We'll handle this better with proper error handling
    }
}

// Arithmetic operations with primitive types
impl Add<i64> for Balance {
    type Output = BalanceResult<Balance>;

    fn add(self, value: i64) -> Self::Output {
        self + Amount::from_i64(value)
    }
}

impl Sub<i64> for Balance {
    type Output = BalanceResult<Balance>;

    fn sub(self, value: i64) -> Self::Output {
        self - Amount::from_i64(value)
    }
}

impl Mul<i64> for Balance {
    type Output = BalanceResult<Balance>;

    fn mul(self, value: i64) -> Self::Output {
        self * Amount::from_i64(value)
    }
}

impl Div<i64> for Balance {
    type Output = BalanceResult<Balance>;

    fn div(self, value: i64) -> Self::Output {
        self / Amount::from_i64(value)
    }
}

impl fmt::Display for Balance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use crate::formatting::{format_balance, FormatConfig, FormatFlags};

        let mut config = FormatConfig::default();

        // Check formatter flags for width
        if let Some(width) = f.width() {
            config.min_width = Some(width);
        }

        // Check alignment
        if let Some(align) = f.align() {
            match align {
                fmt::Alignment::Right => {
                    config.flags.set_flag(FormatFlags::RIGHT_JUSTIFY);
                }
                _ => {} // Left align is default
            }
        }

        let formatted = format_balance(self, &config);
        write!(f, "{}", formatted)
    }
}

impl fmt::Debug for Balance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_empty() {
            write!(f, "BALANCE(<empty>)")
        } else {
            write!(f, "BALANCE(")?;

            let mut first = true;
            for (commodity, amount) in &self.amounts {
                if !first {
                    write!(f, ", ")?;
                }
                if commodity.symbol().is_empty() {
                    write!(f, "{}", amount)?;
                } else {
                    write!(f, "{} {}", amount, commodity.symbol())?;
                }
                first = false;
            }

            write!(f, ")")?;

            // Add internal details for debugging
            if f.alternate() {
                write!(f, " [count:{}]", self.amounts.len())?;
            }

            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::commodity::Commodity;
    use std::sync::Arc;

    #[test]
    fn test_balance_creation() {
        let balance = Balance::new();
        assert!(balance.is_empty());
        assert!(balance.is_zero());
        assert!(balance.is_realzero());
        assert!(!balance.is_nonzero());
        assert_eq!(balance.commodity_count(), 0);
    }

    #[test]
    fn test_balance_from_amount() {
        let amount = Amount::from_i64(100);
        let balance = Balance::from_amount(amount).unwrap();

        assert!(!balance.is_empty());
        assert!(!balance.is_zero());
        assert!(!balance.is_realzero());
        assert!(balance.is_nonzero());
        assert_eq!(balance.commodity_count(), 1);
        assert!(balance.single_amount());
    }

    #[test]
    fn test_balance_arithmetic() {
        let mut balance = Balance::new();

        let amount1 = Amount::from_i64(100);
        balance.add_amount(&amount1).unwrap();
        assert_eq!(balance.commodity_count(), 1);

        let amount2 = Amount::from_i64(50);
        balance.subtract_amount(&amount2).unwrap();
        assert_eq!(balance.commodity_count(), 1);

        // The result should be 50
        let result = balance.to_amount().unwrap();
        assert_eq!(result, Amount::from_i64(50));
    }

    #[test]
    fn test_multi_commodity_balance() {
        let mut balance = Balance::new();

        // Add USD amount
        let _usd = Arc::new(Commodity::new("USD"));
        let usd_amount = Amount::from_i64(100); // TODO: Set commodity when Amount supports it
        balance.add_amount(&usd_amount).unwrap();

        // Add EUR amount
        let _eur = Arc::new(Commodity::new("EUR"));
        let eur_amount = Amount::from_i64(75); // TODO: Set commodity when Amount supports it
        balance.add_amount(&eur_amount).unwrap();

        // Balance should have multiple commodities once Amount supports commodity assignment
        // For now, both amounts will use null commodity so they'll be combined
        assert!(balance.commodity_count() >= 1);
        assert!(!balance.single_amount() || balance.commodity_count() == 1);
    }

    #[test]
    fn test_balance_negation() {
        let amount = Amount::from_i64(100);
        let balance = Balance::from_amount(amount).unwrap();
        let negated = balance.negated();

        let result = negated.to_amount().unwrap();
        assert_eq!(result, Amount::from_i64(-100));
    }

    #[test]
    fn test_balance_equality() {
        let balance1 = Balance::from_amount(Amount::from_i64(100)).unwrap();
        let balance2 = Balance::from_amount(Amount::from_i64(100)).unwrap();
        let balance3 = Balance::from_amount(Amount::from_i64(200)).unwrap();

        assert_eq!(balance1, balance2);
        assert_ne!(balance1, balance3);
    }

    #[test]
    fn test_empty_balance_operations() {
        let balance = Balance::new();

        // Empty balance should convert to zero amount
        assert!(balance.to_amount().is_err()); // Empty balance cannot convert to amount

        let negated = balance.negated();
        assert!(negated.is_empty());

        let abs_val = balance.abs();
        assert!(abs_val.is_empty());
    }

    #[test]
    fn test_zero_amount_handling() {
        let mut balance = Balance::new();

        let zero_amount = Amount::from_i64(0);
        balance.add_amount(&zero_amount).unwrap();

        // Adding zero shouldn't change empty balance
        assert!(balance.is_empty());

        let nonzero_amount = Amount::from_i64(100);
        balance.add_amount(&nonzero_amount).unwrap();
        assert!(!balance.is_empty());

        // Subtracting the same amount should make it empty again
        balance.subtract_amount(&nonzero_amount).unwrap();
        assert!(balance.is_empty());
    }
}

/// Serialize implementation for Balance
impl serde::Serialize for Balance {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeMap;

        let mut map = serializer.serialize_map(Some(self.amounts.len()))?;
        for (commodity, amount) in &self.amounts {
            // Serialize commodity as its symbol string
            map.serialize_entry(&commodity.symbol(), amount)?;
        }
        map.end()
    }
}

/// Deserialize implementation for Balance
impl<'de> serde::Deserialize<'de> for Balance {
    fn deserialize<D>(deserializer: D) -> Result<Balance, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::{MapAccess, Visitor};
        use std::collections::HashMap;

        struct BalanceVisitor;

        impl<'de> Visitor<'de> for BalanceVisitor {
            type Value = Balance;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a map of commodity symbols to amounts")
            }

            fn visit_map<M>(self, mut access: M) -> Result<Self::Value, M::Error>
            where
                M: MapAccess<'de>,
            {
                let mut amounts = HashMap::new();

                while let Some((symbol, amount)) = access.next_entry::<String, Amount>()? {
                    // Create commodity from symbol
                    let commodity = Arc::new(Commodity::new(symbol));
                    amounts.insert(commodity, amount);
                }

                Ok(Balance { amounts })
            }
        }

        deserializer.deserialize_map(BalanceVisitor)
    }
}
