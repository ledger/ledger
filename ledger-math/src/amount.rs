//! Amount module providing arbitrary precision arithmetic with commodity support
//!
//! This module implements the Amount type which uses BigRational for exact
//! arithmetic operations to match the precision of the C++ MPFR implementation.

use std::cmp::Ordering;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::ops::{Add, Div, Mul, Neg, Sub};
use std::str::FromStr;
use std::sync::Arc;

use num_bigint::BigInt;
use num_rational::BigRational;
use num_traits::{Signed, ToPrimitive, Zero};
use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::commodity::{Commodity, CommodityRef};

/// Precision type for tracking decimal places
pub type Precision = u16;

/// Number of extra digits of precision to extend calculations by to avoid
/// losing precision during division and multiplication operations
pub const EXTEND_BY_DIGITS: usize = 6;

/// Errors that can occur during amount operations
#[derive(Error, Debug, Clone, PartialEq, Eq)]
pub enum AmountError {
    #[error("Division by zero")]
    DivisionByZero,
    #[error("Invalid numeric conversion: {0}")]
    InvalidConversion(String),
    #[error("Arithmetic overflow")]
    ArithmeticOverflow,
    #[error("Cannot perform operation on amounts with different commodities")]
    CommodityMismatch,
    #[error("Cannot parse amount from string: {0}")]
    ParseError(String),
    #[error("Amount is null (uninitialized)")]
    NullAmount,
    #[error("Precision loss detected during float conversion: {0}")]
    PrecisionLoss(String),
    #[error("Value too large for target type: {0}")]
    Overflow(String),
}

/// Result type for amount operations
pub type AmountResult<T> = Result<T, AmountError>;

/// An amount represents an arbitrary precision number with optional commodity annotation
///
/// This matches the behavior of the C++ amount_t class, using BigRational
/// for exact arithmetic to avoid floating point precision issues.
#[derive(Clone)]
pub struct Amount {
    /// The exact rational value
    quantity: Option<BigRational>,

    /// Optional commodity reference
    commodity: Option<CommodityRef>,

    /// Internal precision tracking
    precision: Precision,

    /// Whether to keep full precision when displaying
    keep_precision: bool,
}

impl Amount {
    /// Create a null amount (uninitialized)
    pub fn null() -> Self {
        Self { quantity: None, commodity: None, precision: 0, keep_precision: false }
    }

    /// Create an amount from a BigRational value
    pub fn from_rational(rational: BigRational) -> Self {
        Self { quantity: Some(rational), commodity: None, precision: 0, keep_precision: false }
    }

    /// Create an amount from an integer
    pub fn from_i64(value: i64) -> Self {
        Self::from_rational(BigRational::from_integer(BigInt::from(value)))
    }

    /// Create a new amount from a Decimal value
    pub fn new(value: Decimal) -> Self {
        Self::from_decimal(value).unwrap_or_else(|_| Self::null())
    }

    /// Create a new amount with a specific commodity
    pub fn with_commodity(value: Decimal, commodity: Option<Arc<Commodity>>) -> Self {
        let mut amount = Self::new(value);
        if let Some(c) = commodity {
            amount.set_commodity(c);
        }
        amount
    }

    /// Create an amount from a double with appropriate precision
    pub fn from_f64(value: f64) -> AmountResult<Self> {
        if !value.is_finite() {
            return Err(AmountError::InvalidConversion(format!(
                "Cannot create amount from non-finite value: {}",
                value
            )));
        }

        // Convert double to rational using the same algorithm as C++
        let decimal =
            Decimal::try_from(value).map_err(|e| AmountError::InvalidConversion(e.to_string()))?;
        Self::from_decimal(decimal)
    }

    /// Create an amount from a rust_decimal::Decimal
    pub fn from_decimal(decimal: Decimal) -> AmountResult<Self> {
        // Convert decimal to rational representation
        let scale = decimal.scale();
        let mantissa = decimal.mantissa();

        let numerator = BigInt::from(mantissa);
        let denominator = BigInt::from(10_i64.pow(scale));

        let rational = BigRational::new(numerator, denominator);

        Ok(Self {
            quantity: Some(rational),
            commodity: None,
            precision: scale as Precision,
            keep_precision: false,
        })
    }

    /// Create an exact amount that preserves full precision
    /// This is equivalent to the C++ amount_t::exact() method
    pub fn exact(value_str: &str) -> AmountResult<Self> {
        let mut amount = Self::parse(value_str)?;
        amount.keep_precision = true;
        Ok(amount)
    }

    /// Parse an amount from a string
    /// This is a simplified version - will need to be expanded for full C++ compatibility
    pub fn parse(value_str: &str) -> AmountResult<Self> {
        let trimmed = value_str.trim();

        // For now, handle simple numeric values
        // TODO: Add commodity parsing, thousands separators, etc.
        let decimal =
            trimmed.parse::<Decimal>().map_err(|e| AmountError::ParseError(e.to_string()))?;

        Self::from_decimal(decimal)
    }

    /// Check if this amount is null (uninitialized)
    pub fn is_null(&self) -> bool {
        self.quantity.is_none()
    }

    /// Check if this amount is exactly zero
    pub fn is_realzero(&self) -> bool {
        match &self.quantity {
            None => true, // null amounts are considered zero
            Some(q) => q.is_zero(),
        }
    }

    /// Check if this amount is zero (considering display precision)
    pub fn is_zero(&self) -> bool {
        if self.is_null() {
            return true;
        }

        if self.keep_precision {
            return self.is_realzero();
        }

        // TODO: Implement rounding to display precision and check if result is zero
        self.is_realzero()
    }

    /// Check if this amount is non-zero
    pub fn is_nonzero(&self) -> bool {
        !self.is_zero()
    }

    /// Get the sign of this amount
    /// Returns -1, 0, or 1
    pub fn sign(&self) -> i32 {
        match &self.quantity {
            None => 0,
            Some(q) => {
                if q.is_zero() {
                    0
                } else if q.is_positive() {
                    1
                } else {
                    -1
                }
            }
        }
    }

    /// Get the absolute value of this amount
    pub fn abs(&self) -> Self {
        match &self.quantity {
            None => self.clone(),
            Some(q) => {
                let mut result = self.clone();
                result.quantity = Some(q.abs());
                result
            }
        }
    }

    /// Negate this amount in place
    pub fn in_place_negate(&mut self) {
        if let Some(q) = &mut self.quantity {
            *q = -q.clone();
        }
    }

    /// Get the negated value of this amount
    pub fn negated(&self) -> Self {
        let mut result = self.clone();
        result.in_place_negate();
        result
    }

    /// Get the precision of this amount
    pub fn precision(&self) -> Precision {
        self.precision
    }

    /// Check if this amount keeps full precision
    pub fn keep_precision(&self) -> bool {
        self.keep_precision
    }

    /// Set whether to keep full precision
    pub fn set_keep_precision(&mut self, keep: bool) {
        self.keep_precision = keep;
    }

    /// Get display precision (for now same as precision, will be enhanced later)
    pub fn display_precision(&self) -> Precision {
        // TODO: Consider commodity precision settings
        self.precision
    }

    /// Get the commodity reference
    pub fn commodity(&self) -> Option<&CommodityRef> {
        self.commodity.as_ref()
    }

    /// Check if this amount has a commodity
    pub fn has_commodity(&self) -> bool {
        self.commodity.is_some()
    }

    /// Set the commodity for this amount
    pub fn set_commodity(&mut self, commodity: CommodityRef) {
        // Ensure we have a quantity if setting a commodity
        if self.quantity.is_none() {
            self.quantity = Some(BigRational::zero());
        }
        self.commodity = Some(commodity);
    }

    /// Clear the commodity from this amount
    pub fn clear_commodity(&mut self) {
        self.commodity = None;
    }

    /// Get a copy of this amount without commodity information
    pub fn number(&self) -> Self {
        let mut result = self.clone();
        result.clear_commodity();
        result
    }

    /// Check if this amount is valid
    pub fn valid(&self) -> bool {
        // An amount is valid if:
        // 1. If it has a quantity, the quantity should be valid
        // 2. Precision should be reasonable (< 1024 like in C++)
        self.precision <= 1024
    }

    /// Convert to f64 - may lose precision
    pub fn to_f64(&self) -> AmountResult<f64> {
        match &self.quantity {
            None => Ok(0.0),
            Some(q) => q.to_f64().ok_or_else(|| {
                AmountError::InvalidConversion("Cannot convert amount to f64".to_string())
            }),
        }
    }

    /// Convert to f64, alias for compatibility with C++ interface
    pub fn to_double(&self) -> AmountResult<f64> {
        self.to_f64()
    }

    /// Convert to f32 - may lose precision
    pub fn to_f32(&self) -> AmountResult<f32> {
        match &self.quantity {
            None => Ok(0.0),
            Some(q) => q.to_f32().ok_or_else(|| {
                AmountError::InvalidConversion("Cannot convert amount to f32".to_string())
            }),
        }
    }

    /// Convert to i64 if possible
    pub fn to_i64(&self) -> AmountResult<i64> {
        match &self.quantity {
            None => Ok(0),
            Some(q) => {
                if q.is_integer() {
                    q.to_integer().to_i64().ok_or_else(|| {
                        AmountError::InvalidConversion("Amount too large for i64".to_string())
                    })
                } else {
                    Err(AmountError::InvalidConversion("Amount is not an integer".to_string()))
                }
            }
        }
    }

    /// Convert to i64, alias for compatibility with C++ interface
    pub fn to_long(&self) -> AmountResult<i64> {
        self.to_i64()
    }

    /// Get the value as a Decimal (for use in calculations)
    pub fn value(&self) -> Decimal {
        match &self.quantity {
            None => Decimal::ZERO,
            Some(q) => {
                // Convert BigRational to Decimal
                // This may lose precision for very large or very precise numbers
                if let (Some(numer), Some(denom)) = (q.numer().to_i128(), q.denom().to_i128()) {
                    Decimal::from_i128_with_scale(numer, 0)
                        / Decimal::from_i128_with_scale(denom, 0)
                } else {
                    // Fallback for values that don't fit in i128
                    self.to_f64().unwrap_or(0.0).try_into().unwrap_or(Decimal::ZERO)
                }
            }
        }
    }

    /// Convert to i32 if possible
    pub fn to_i32(&self) -> AmountResult<i32> {
        match &self.quantity {
            None => Ok(0),
            Some(q) => {
                if q.is_integer() {
                    q.to_integer().to_i32().ok_or_else(|| {
                        AmountError::InvalidConversion("Amount too large for i32".to_string())
                    })
                } else {
                    Err(AmountError::InvalidConversion("Amount is not an integer".to_string()))
                }
            }
        }
    }

    /// Convert to u64 if possible
    pub fn to_u64(&self) -> AmountResult<u64> {
        match &self.quantity {
            None => Ok(0),
            Some(q) => {
                if q.is_integer() && !q.is_negative() {
                    q.to_integer().to_u64().ok_or_else(|| {
                        AmountError::InvalidConversion("Amount too large for u64".to_string())
                    })
                } else if q.is_negative() {
                    Err(AmountError::InvalidConversion(
                        "Cannot convert negative amount to unsigned integer".to_string(),
                    ))
                } else {
                    Err(AmountError::InvalidConversion("Amount is not an integer".to_string()))
                }
            }
        }
    }

    /// Convert to u32 if possible
    pub fn to_u32(&self) -> AmountResult<u32> {
        match &self.quantity {
            None => Ok(0),
            Some(q) => {
                if q.is_integer() && !q.is_negative() {
                    q.to_integer().to_u32().ok_or_else(|| {
                        AmountError::InvalidConversion("Amount too large for u32".to_string())
                    })
                } else if q.is_negative() {
                    Err(AmountError::InvalidConversion(
                        "Cannot convert negative amount to unsigned integer".to_string(),
                    ))
                } else {
                    Err(AmountError::InvalidConversion("Amount is not an integer".to_string()))
                }
            }
        }
    }

    /// Get the underlying BigRational if available
    pub fn to_rational(&self) -> Option<&BigRational> {
        self.quantity.as_ref()
    }

    /// Get the underlying BigRational, consuming the Amount
    pub fn into_rational(self) -> Option<BigRational> {
        self.quantity
    }

    /// Check if this amount fits in an i64
    pub fn fits_in_i64(&self) -> bool {
        self.to_i64().is_ok()
    }

    /// Add another amount to this amount (in-place)
    /// Commodities must match or one must be null
    pub fn add_amount(&mut self, other: &Amount) -> AmountResult<()> {
        // Check commodity compatibility
        self.check_commodity_compatibility(other)?;

        // Handle null amounts
        if self.is_null() {
            if other.is_null() {
                return Ok(());
            } else {
                *self = other.clone();
                return Ok(());
            }
        }

        if other.is_null() {
            return Ok(());
        }

        // Perform addition
        if let (Some(a), Some(b)) = (&self.quantity, &other.quantity) {
            self.quantity = Some(a + b);
            // Update precision to accommodate result
            self.precision = self.precision.max(other.precision);

            // Set commodity if this amount doesn't have one
            if self.commodity.is_none() && other.commodity.is_some() {
                self.commodity = other.commodity.clone();
            }
        }

        Ok(())
    }

    /// Subtract another amount from this amount (in-place)
    pub fn sub_amount(&mut self, other: &Amount) -> AmountResult<()> {
        // Check commodity compatibility
        self.check_commodity_compatibility(other)?;

        // Handle null amounts
        if self.is_null() {
            if other.is_null() {
                return Ok(());
            } else {
                *self = other.negated();
                return Ok(());
            }
        }

        if other.is_null() {
            return Ok(());
        }

        // Perform subtraction
        if let (Some(a), Some(b)) = (&self.quantity, &other.quantity) {
            self.quantity = Some(a - b);
            // Update precision to accommodate result
            self.precision = self.precision.max(other.precision);

            // Set commodity if this amount doesn't have one
            if self.commodity.is_none() && other.commodity.is_some() {
                self.commodity = other.commodity.clone();
            }
        }

        Ok(())
    }

    /// Multiply this amount by another amount (in-place)
    /// For multiplication, only one operand can have a commodity
    pub fn mul_amount(&mut self, other: &Amount) -> AmountResult<()> {
        if self.has_commodity() && other.has_commodity() {
            return Err(AmountError::CommodityMismatch);
        }

        // Handle null amounts
        if self.is_null() || other.is_null() {
            *self = Amount::null();
            return Ok(());
        }

        // Perform multiplication
        if let (Some(a), Some(b)) = (&self.quantity, &other.quantity) {
            self.quantity = Some(a * b);
            // For multiplication, precision is additive up to a reasonable limit
            let new_precision = (self.precision + other.precision).min(1024);
            self.precision = new_precision;

            // Preserve commodity from either operand
            if self.commodity.is_none() && other.commodity.is_some() {
                self.commodity = other.commodity.clone();
            }
        }

        Ok(())
    }

    /// Divide this amount by another amount (in-place)
    pub fn div_amount(&mut self, other: &Amount) -> AmountResult<()> {
        if other.is_realzero() {
            return Err(AmountError::DivisionByZero);
        }

        if self.has_commodity() && other.has_commodity() {
            return Err(AmountError::CommodityMismatch);
        }

        // Handle null amounts
        if self.is_null() {
            return Ok(()); // 0 / anything = 0
        }

        if other.is_null() {
            return Err(AmountError::DivisionByZero);
        }

        // Perform division
        if let (Some(a), Some(b)) = (&self.quantity, &other.quantity) {
            self.quantity = Some(a / b);
            // For division, extend precision to avoid loss
            let new_precision = (self.precision + EXTEND_BY_DIGITS as Precision).min(1024);
            self.precision = new_precision;

            // Preserve commodity from dividend
            if self.commodity.is_none() && other.commodity.is_some() {
                self.commodity = other.commodity.clone();
            }
        }

        Ok(())
    }

    /// Check if two amounts have compatible commodities for arithmetic
    fn check_commodity_compatibility(&self, other: &Amount) -> AmountResult<()> {
        match (&self.commodity, &other.commodity) {
            (None, None) => Ok(()),
            (Some(_), None) | (None, Some(_)) => Ok(()),
            (Some(a), Some(b)) => {
                if std::ptr::eq(a.as_ref(), b.as_ref()) {
                    Ok(())
                } else {
                    Err(AmountError::CommodityMismatch)
                }
            }
        }
    }

    /// Round this amount to commodity precision (in-place)
    pub fn in_place_round(&mut self) {
        if self.is_null() {
            return;
        }

        let precision = self.display_precision();
        self.in_place_roundto(precision as i32);
    }

    /// Round this amount to a specific number of decimal places (in-place)
    pub fn in_place_roundto(&mut self, places: i32) {
        if self.is_null() || places < 0 {
            return;
        }

        if let Some(q) = &mut self.quantity {
            let scale = BigInt::from(10).pow(places as u32);
            let scaled = &*q * &BigRational::from_integer(scale.clone());
            let rounded_int = if scaled.is_positive() {
                scaled
                    + BigRational::from_integer(BigInt::from(1))
                        / BigRational::from_integer(BigInt::from(2))
            } else {
                scaled
                    - BigRational::from_integer(BigInt::from(1))
                        / BigRational::from_integer(BigInt::from(2))
            };

            let truncated = rounded_int.to_integer();
            *q = BigRational::new(truncated, scale);
        }

        self.precision = places as Precision;
    }

    /// Get the rounded version of this amount
    pub fn rounded(&self) -> Self {
        let mut result = self.clone();
        result.in_place_round();
        result
    }

    /// Get the rounded version of this amount to specific places
    pub fn roundto(&self, places: i32) -> Self {
        let mut result = self.clone();
        result.in_place_roundto(places);
        result
    }

    /// Truncate this amount to display precision (in-place)
    pub fn in_place_truncate(&mut self) {
        if self.is_null() {
            return;
        }

        let precision = self.display_precision();
        if let Some(q) = &mut self.quantity {
            let scale = BigInt::from(10).pow(precision as u32);
            let scaled = &*q * &BigRational::from_integer(scale.clone());
            let truncated = scaled.to_integer();
            *q = BigRational::new(truncated, scale);
        }
    }

    /// Get the truncated version of this amount
    pub fn truncated(&self) -> Self {
        let mut result = self.clone();
        result.in_place_truncate();
        result
    }

    /// Floor this amount (round towards negative infinity) (in-place)
    pub fn in_place_floor(&mut self) {
        if self.is_null() {
            return;
        }

        if let Some(q) = &mut self.quantity {
            *q = BigRational::from_integer(q.floor().to_integer());
        }
        self.precision = 0;
    }

    /// Get the floored version of this amount
    pub fn floored(&self) -> Self {
        let mut result = self.clone();
        result.in_place_floor();
        result
    }

    /// Ceiling this amount (round towards positive infinity) (in-place)
    pub fn in_place_ceiling(&mut self) {
        if self.is_null() {
            return;
        }

        if let Some(q) = &mut self.quantity {
            *q = BigRational::from_integer(q.ceil().to_integer());
        }
        self.precision = 0;
    }

    /// Get the ceilinged version of this amount
    pub fn ceilinged(&self) -> Self {
        let mut result = self.clone();
        result.in_place_ceiling();
        result
    }

    /// Set this amount to keep full precision when displaying (in-place)
    pub fn in_place_unround(&mut self) {
        self.keep_precision = true;
    }

    /// Get the unrounded version of this amount (keeps full precision)
    pub fn unrounded(&self) -> Self {
        let mut result = self.clone();
        result.in_place_unround();
        result
    }

    /// Print this amount to a stream with formatting options
    /// Matches C++ amount_t::print() method signature
    pub fn print<W: fmt::Write>(
        &self,
        writer: &mut W,
        flags: crate::formatting::FormatFlags,
    ) -> fmt::Result {
        use crate::formatting::{format_amount, FormatConfig};

        let config = FormatConfig::default().with_flags(flags);
        let formatted = format_amount(self, &config);
        write!(writer, "{}", formatted)
    }

    /// Convert amount to string with default formatting
    /// Equivalent to C++ to_string() method
    pub fn to_string(&self) -> String {
        format!("{}", self)
    }

    /// Convert amount to string with full precision
    /// Equivalent to C++ to_fullstring() method  
    pub fn to_fullstring(&self) -> String {
        use crate::formatting::{format_amount, FormatConfig};

        let config = FormatConfig::default().with_precision(if self.keep_precision {
            self.precision
        } else {
            self.display_precision()
        });
        format_amount(self, &config)
    }

    /// Convert amount to string representation showing internal rational
    /// Useful for exact serialization
    pub fn quantity_string(&self) -> String {
        match &self.quantity {
            None => "0".to_string(),
            Some(rational) => rational.to_string(),
        }
    }
}

impl Default for Amount {
    fn default() -> Self {
        Self::null()
    }
}

impl PartialEq for Amount {
    fn eq(&self, other: &Self) -> bool {
        // First check commodity compatibility
        match (&self.commodity, &other.commodity) {
            (None, None) => {}
            (Some(a), Some(b)) => {
                if !std::ptr::eq(a.as_ref(), b.as_ref()) {
                    return false;
                }
            }
            _ => return false,
        }

        // Then check quantity
        match (&self.quantity, &other.quantity) {
            (None, None) => true,
            (Some(a), Some(b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for Amount {}

impl Hash for Amount {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Hash the quantity
        match &self.quantity {
            None => 0u8.hash(state),
            Some(q) => {
                1u8.hash(state);
                // For BigRational, we hash the numerator and denominator
                q.numer().hash(state);
                q.denom().hash(state);
            }
        }

        // Hash commodity pointer address
        if let Some(commodity) = &self.commodity {
            (commodity.as_ref() as *const Commodity as usize).hash(state);
        }
    }
}

impl PartialOrd for Amount {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        // Can only compare amounts with compatible commodities
        match (&self.commodity, &other.commodity) {
            (None, None) => {}
            (Some(a), Some(b)) => {
                if !std::ptr::eq(a.as_ref(), b.as_ref()) {
                    return None; // Cannot compare different commodities
                }
            }
            _ => return None,
        }

        match (&self.quantity, &other.quantity) {
            (None, None) => Some(Ordering::Equal),
            (None, Some(q)) => {
                if q.is_zero() {
                    Some(Ordering::Equal)
                } else {
                    Some(Ordering::Less)
                }
            }
            (Some(q), None) => {
                if q.is_zero() {
                    Some(Ordering::Equal)
                } else {
                    Some(Ordering::Greater)
                }
            }
            (Some(a), Some(b)) => a.partial_cmp(b),
        }
    }
}

// Note: We can't implement Ord because Amount comparison can fail for different commodities
// Ord requires that comparison always succeeds, but PartialOrd allows returning None

impl fmt::Display for Amount {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use crate::formatting::{format_amount, FormatConfig, FormatFlags};

        let mut config = FormatConfig::default();

        // Check formatter flags for precision
        if let Some(precision) = f.precision() {
            config.precision = Some(precision as crate::amount::Precision);
        }

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

        let formatted = format_amount(self, &config);
        write!(f, "{}", formatted)
    }
}

impl fmt::Debug for Amount {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_null() {
            write!(f, "AMOUNT(<null>)")
        } else {
            let display_value = format!("{}", self);
            write!(f, "AMOUNT({})", display_value)?;

            // Add internal details for debugging
            if f.alternate() {
                write!(f, " [prec:{}, keep:{}", self.precision, self.keep_precision)?;
                if let Some(commodity) = &self.commodity {
                    write!(f, ", comm:{}", commodity.symbol())?;
                }
                if let Some(rational) = &self.quantity {
                    write!(f, ", raw:{}", rational)?;
                }
                write!(f, "]")?;
            }

            Ok(())
        }
    }
}

// Implement From traits for infallible conversions
impl From<i32> for Amount {
    fn from(value: i32) -> Self {
        Self::from_i64(i64::from(value))
    }
}

impl From<i64> for Amount {
    fn from(value: i64) -> Self {
        Self::from_i64(value)
    }
}

impl From<u32> for Amount {
    fn from(value: u32) -> Self {
        Self::from_i64(i64::from(value))
    }
}

impl TryFrom<u64> for Amount {
    type Error = AmountError;

    fn try_from(value: u64) -> AmountResult<Self> {
        if value <= i64::MAX as u64 {
            Ok(Self::from_i64(value as i64))
        } else {
            // For values larger than i64::MAX, create directly from BigInt
            Ok(Self::from_rational(BigRational::from_integer(BigInt::from(value))))
        }
    }
}

// Implement conversion from BigInt and BigRational directly
impl From<BigInt> for Amount {
    fn from(value: BigInt) -> Self {
        Self::from_rational(BigRational::from_integer(value))
    }
}

impl From<BigRational> for Amount {
    fn from(value: BigRational) -> Self {
        Self::from_rational(value)
    }
}

impl TryFrom<f32> for Amount {
    type Error = AmountError;

    fn try_from(value: f32) -> AmountResult<Self> {
        if !value.is_finite() {
            return Err(AmountError::InvalidConversion(format!(
                "Cannot create amount from non-finite f32 value: {}",
                value
            )));
        }

        // Convert f32 to f64 first, then use f64 conversion path for consistency
        Self::from_f64(f64::from(value))
    }
}

impl TryFrom<f64> for Amount {
    type Error = AmountError;

    fn try_from(value: f64) -> AmountResult<Self> {
        Self::from_f64(value)
    }
}

impl TryFrom<Decimal> for Amount {
    type Error = AmountError;

    fn try_from(value: Decimal) -> AmountResult<Self> {
        Self::from_decimal(value)
    }
}

impl TryFrom<&str> for Amount {
    type Error = AmountError;

    fn try_from(value: &str) -> AmountResult<Self> {
        Self::parse(value)
    }
}

impl TryFrom<String> for Amount {
    type Error = AmountError;

    fn try_from(value: String) -> AmountResult<Self> {
        Self::parse(&value)
    }
}

impl FromStr for Amount {
    type Err = AmountError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::parse(s)
    }
}

// AsRef implementations for accessing underlying data without copies
impl AsRef<Amount> for Amount {
    fn as_ref(&self) -> &Amount {
        self
    }
}

impl AsRef<Option<BigRational>> for Amount {
    fn as_ref(&self) -> &Option<BigRational> {
        &self.quantity
    }
}

// Implement Deref to BigRational when available for convenient access
// Note: This is only available for non-null amounts
impl Amount {
    /// Get a reference to the underlying BigRational if this Amount is not null
    pub fn as_rational(&self) -> Option<&BigRational> {
        self.quantity.as_ref()
    }
}

// Custom serde implementation
impl Serialize for Amount {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeStruct;
        let mut state = serializer.serialize_struct("Amount", 4)?;

        // Serialize quantity as string to preserve precision
        let quantity_str = match &self.quantity {
            None => None,
            Some(q) => Some(q.to_string()),
        };
        state.serialize_field("quantity", &quantity_str)?;

        // Serialize commodity as None for now (complex to handle Arc properly)
        // In a full implementation, this would need custom handling
        state.serialize_field("commodity", &None::<String>)?;

        state.serialize_field("precision", &self.precision)?;
        state.serialize_field("keep_precision", &self.keep_precision)?;
        state.end()
    }
}

impl<'de> Deserialize<'de> for Amount {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        use serde::de::{self, MapAccess, Visitor};
        use std::fmt;

        #[derive(Deserialize)]
        #[serde(field_identifier, rename_all = "lowercase")]
        enum Field {
            Quantity,
            Commodity,
            Precision,
            KeepPrecision,
        }

        struct AmountVisitor;

        impl<'de> Visitor<'de> for AmountVisitor {
            type Value = Amount;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("struct Amount")
            }

            fn visit_map<V>(self, mut map: V) -> Result<Amount, V::Error>
            where
                V: MapAccess<'de>,
            {
                let mut quantity = None;
                let mut precision = None;
                let mut keep_precision = None;

                while let Some(key) = map.next_key()? {
                    match key {
                        Field::Quantity => {
                            let quantity_str: Option<String> = map.next_value()?;
                            quantity = match quantity_str {
                                None => None,
                                Some(s) => {
                                    let parsed =
                                        s.parse::<BigRational>().map_err(de::Error::custom)?;
                                    Some(parsed)
                                }
                            };
                        }
                        Field::Commodity => {
                            let _: Option<String> = map.next_value()?; // Ignore for now
                        }
                        Field::Precision => {
                            precision = Some(map.next_value()?);
                        }
                        Field::KeepPrecision => {
                            keep_precision = Some(map.next_value()?);
                        }
                    }
                }

                let precision = precision.ok_or_else(|| de::Error::missing_field("precision"))?;
                let keep_precision =
                    keep_precision.ok_or_else(|| de::Error::missing_field("keep_precision"))?;

                Ok(Amount {
                    quantity,
                    commodity: None, // Cannot deserialize commodity without proper handling
                    precision,
                    keep_precision,
                })
            }
        }

        const FIELDS: &'static [&'static str] =
            &["quantity", "commodity", "precision", "keep_precision"];
        deserializer.deserialize_struct("Amount", FIELDS, AmountVisitor)
    }
}

// Arithmetic operator implementations
impl Add for Amount {
    type Output = AmountResult<Amount>;

    fn add(self, other: Amount) -> Self::Output {
        let mut result = self;
        result.add_amount(&other)?;
        Ok(result)
    }
}

impl Add<&Amount> for Amount {
    type Output = AmountResult<Amount>;

    fn add(self, other: &Amount) -> Self::Output {
        let mut result = self;
        result.add_amount(other)?;
        Ok(result)
    }
}

impl Add<Amount> for &Amount {
    type Output = AmountResult<Amount>;

    fn add(self, other: Amount) -> Self::Output {
        let mut result = self.clone();
        result.add_amount(&other)?;
        Ok(result)
    }
}

impl Add<&Amount> for &Amount {
    type Output = AmountResult<Amount>;

    fn add(self, other: &Amount) -> Self::Output {
        let mut result = self.clone();
        result.add_amount(other)?;
        Ok(result)
    }
}

// Note: We don't implement AddAssign from std::ops because it conflicts
// with our fallible operations. Use add_amount() method instead.

impl Sub for Amount {
    type Output = AmountResult<Amount>;

    fn sub(self, other: Amount) -> Self::Output {
        let mut result = self;
        result.sub_amount(&other)?;
        Ok(result)
    }
}

impl Sub<&Amount> for Amount {
    type Output = AmountResult<Amount>;

    fn sub(self, other: &Amount) -> Self::Output {
        let mut result = self;
        result.sub_amount(other)?;
        Ok(result)
    }
}

impl Sub<Amount> for &Amount {
    type Output = AmountResult<Amount>;

    fn sub(self, other: Amount) -> Self::Output {
        let mut result = self.clone();
        result.sub_amount(&other)?;
        Ok(result)
    }
}

impl Sub<&Amount> for &Amount {
    type Output = AmountResult<Amount>;

    fn sub(self, other: &Amount) -> Self::Output {
        let mut result = self.clone();
        result.sub_amount(other)?;
        Ok(result)
    }
}

// Note: We don't implement SubAssign from std::ops because it conflicts
// with our fallible operations. Use sub_amount() method instead.

impl Mul for Amount {
    type Output = AmountResult<Amount>;

    fn mul(self, other: Amount) -> Self::Output {
        let mut result = self;
        result.mul_amount(&other)?;
        Ok(result)
    }
}

impl Mul<&Amount> for Amount {
    type Output = AmountResult<Amount>;

    fn mul(self, other: &Amount) -> Self::Output {
        let mut result = self;
        result.mul_amount(other)?;
        Ok(result)
    }
}

impl Mul<Amount> for &Amount {
    type Output = AmountResult<Amount>;

    fn mul(self, other: Amount) -> Self::Output {
        let mut result = self.clone();
        result.mul_amount(&other)?;
        Ok(result)
    }
}

impl Mul<&Amount> for &Amount {
    type Output = AmountResult<Amount>;

    fn mul(self, other: &Amount) -> Self::Output {
        let mut result = self.clone();
        result.mul_amount(other)?;
        Ok(result)
    }
}

// Note: We don't implement MulAssign from std::ops because it conflicts
// with our fallible operations. Use mul_amount() method instead.

impl Div for Amount {
    type Output = AmountResult<Amount>;

    fn div(self, other: Amount) -> Self::Output {
        let mut result = self;
        result.div_amount(&other)?;
        Ok(result)
    }
}

impl Div<&Amount> for Amount {
    type Output = AmountResult<Amount>;

    fn div(self, other: &Amount) -> Self::Output {
        let mut result = self;
        result.div_amount(other)?;
        Ok(result)
    }
}

impl Div<Amount> for &Amount {
    type Output = AmountResult<Amount>;

    fn div(self, other: Amount) -> Self::Output {
        let mut result = self.clone();
        result.div_amount(&other)?;
        Ok(result)
    }
}

impl Div<&Amount> for &Amount {
    type Output = AmountResult<Amount>;

    fn div(self, other: &Amount) -> Self::Output {
        let mut result = self.clone();
        result.div_amount(other)?;
        Ok(result)
    }
}

// Note: We don't implement DivAssign from std::ops because it conflicts
// with our fallible operations. Use div_amount() method instead.

impl Neg for Amount {
    type Output = Amount;

    fn neg(self) -> Self::Output {
        self.negated()
    }
}

impl Neg for &Amount {
    type Output = Amount;

    fn neg(self) -> Self::Output {
        self.negated()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_null_amount() {
        let amount = Amount::null();
        assert!(amount.is_null());
        assert!(amount.is_zero());
        assert!(amount.is_realzero());
        assert_eq!(amount.sign(), 0);
    }

    #[test]
    fn test_integer_amount() {
        let amount = Amount::from_i64(42);
        assert!(!amount.is_null());
        assert!(!amount.is_zero());
        assert!(!amount.is_realzero());
        assert_eq!(amount.sign(), 1);
        assert_eq!(amount.to_i64().unwrap(), 42);
    }

    #[test]
    fn test_negative_amount() {
        let amount = Amount::from_i64(-42);
        assert_eq!(amount.sign(), -1);
        assert_eq!(amount.abs().to_i64().unwrap(), 42);
        assert_eq!(amount.negated().to_i64().unwrap(), 42);
    }

    #[test]
    fn test_zero_amount() {
        let amount = Amount::from_i64(0);
        assert!(!amount.is_null());
        assert!(amount.is_zero());
        assert!(amount.is_realzero());
        assert_eq!(amount.sign(), 0);
    }

    #[test]
    fn test_decimal_amount() {
        let decimal = Decimal::new(12345, 2); // 123.45
        let amount = Amount::from_decimal(decimal).unwrap();
        assert!(!amount.is_null());
        assert!(!amount.is_zero());
        assert_eq!(amount.precision(), 2);
    }

    #[test]
    fn test_amount_equality() {
        let amount1 = Amount::from_i64(42);
        let amount2 = Amount::from_i64(42);
        let amount3 = Amount::from_i64(43);

        assert_eq!(amount1, amount2);
        assert_ne!(amount1, amount3);
    }

    #[test]
    fn test_parse_simple() {
        let amount = Amount::parse("123.45").unwrap();
        assert!(!amount.is_null());
        assert!(!amount.is_zero());
        assert_eq!(amount.precision(), 2);
    }

    #[test]
    fn test_exact_amount() {
        let amount = Amount::exact("123.45").unwrap();
        assert!(amount.keep_precision());
        assert_eq!(amount.precision(), 2);
    }

    #[test]
    fn test_arithmetic_addition() {
        let a = Amount::from_i64(10);
        let b = Amount::from_i64(20);
        let result = (a + b).unwrap();
        assert_eq!(result.to_i64().unwrap(), 30);
    }

    #[test]
    fn test_arithmetic_subtraction() {
        let a = Amount::from_i64(30);
        let b = Amount::from_i64(10);
        let result = (a - b).unwrap();
        assert_eq!(result.to_i64().unwrap(), 20);
    }

    #[test]
    fn test_arithmetic_multiplication() {
        let a = Amount::from_i64(6);
        let b = Amount::from_i64(7);
        let result = (a * b).unwrap();
        assert_eq!(result.to_i64().unwrap(), 42);
    }

    #[test]
    fn test_arithmetic_division() {
        let a = Amount::from_i64(84);
        let b = Amount::from_i64(2);
        let result = (a / b).unwrap();
        assert_eq!(result.to_i64().unwrap(), 42);
    }

    #[test]
    fn test_division_by_zero() {
        let a = Amount::from_i64(10);
        let b = Amount::from_i64(0);
        let result = a / b;
        assert!(matches!(result, Err(AmountError::DivisionByZero)));
    }

    #[test]
    fn test_negation() {
        let amount = Amount::from_i64(42);
        let negated = -amount;
        assert_eq!(negated.to_i64().unwrap(), -42);
    }

    #[test]
    fn test_in_place_operations() {
        let mut amount = Amount::from_i64(10);
        let other = Amount::from_i64(5);

        amount.add_amount(&other).unwrap();
        assert_eq!(amount.to_i64().unwrap(), 15);

        amount.sub_amount(&other).unwrap();
        assert_eq!(amount.to_i64().unwrap(), 10);

        amount.mul_amount(&other).unwrap();
        assert_eq!(amount.to_i64().unwrap(), 50);

        amount.div_amount(&other).unwrap();
        assert_eq!(amount.to_i64().unwrap(), 10);
    }

    #[test]
    fn test_rounding() {
        let amount = Amount::from_decimal(Decimal::new(12345, 3)).unwrap(); // 12.345
        let rounded = amount.roundto(2);
        // Should round to 12.35
        assert_eq!(rounded.precision(), 2);
    }

    #[test]
    fn test_truncation() {
        let amount = Amount::from_decimal(Decimal::new(12349, 3)).unwrap(); // 12.349
        let truncated = amount.truncated();
        // Should truncate based on display precision
        assert_eq!(truncated.precision(), amount.display_precision());
    }

    #[test]
    fn test_floor_ceil() {
        let amount = Amount::from_decimal(Decimal::new(1234, 2)).unwrap(); // 12.34

        let floored = amount.floored();
        assert_eq!(floored.to_i64().unwrap(), 12);

        let ceilinged = amount.ceilinged();
        assert_eq!(ceilinged.to_i64().unwrap(), 13);
    }

    // Additional tests for new conversion traits
    #[test]
    fn test_from_i32() {
        let amount = Amount::from(42i32);
        assert_eq!(amount.to_i64().unwrap(), 42);
    }

    #[test]
    fn test_from_u32() {
        let amount = Amount::from(42u32);
        assert_eq!(amount.to_i64().unwrap(), 42);
    }

    #[test]
    fn test_try_from_u64() {
        // Test small u64
        let amount = Amount::try_from(42u64).unwrap();
        assert_eq!(amount.to_i64().unwrap(), 42);

        // Test large u64 that fits in i64
        let large_val = i64::MAX as u64;
        let amount = Amount::try_from(large_val).unwrap();
        assert_eq!(amount.to_i64().unwrap(), i64::MAX);

        // Test u64 larger than i64::MAX
        let very_large = u64::MAX;
        let amount = Amount::try_from(very_large).unwrap();
        // Should succeed but won't fit in i64
        assert!(!amount.fits_in_i64());
    }

    #[test]
    fn test_try_from_f32() {
        let amount = Amount::try_from(42.5f32).unwrap();
        assert!((amount.to_f64().unwrap() - 42.5).abs() < f64::EPSILON);

        // Test non-finite values
        assert!(Amount::try_from(f32::INFINITY).is_err());
        assert!(Amount::try_from(f32::NAN).is_err());
    }

    #[test]
    fn test_from_bigint() {
        let big = BigInt::from(12345);
        let amount = Amount::from(big);
        assert_eq!(amount.to_i64().unwrap(), 12345);
    }

    #[test]
    fn test_from_bigrational() {
        let rational = BigRational::new(BigInt::from(22), BigInt::from(7)); // 22/7
        let amount = Amount::from(rational);
        assert!(!amount.is_null());
        // Should be approximately 3.14...
        let approx = amount.to_f64().unwrap();
        assert!((approx - (22.0 / 7.0)).abs() < 1e-10);
    }

    #[test]
    fn test_try_from_string() {
        let amount = Amount::try_from("123.45".to_string()).unwrap();
        assert_eq!(amount.precision(), 2);

        // Test parsing error
        assert!(Amount::try_from("not a number".to_string()).is_err());
    }

    #[test]
    fn test_from_str_trait() {
        let amount: Amount = "42.50".parse().unwrap();
        assert_eq!(amount.precision(), 2);

        // Test parsing error
        assert!("invalid".parse::<Amount>().is_err());
    }

    #[test]
    fn test_to_conversions() {
        let amount = Amount::from_i64(42);

        // Test to_long() alias
        assert_eq!(amount.to_long().unwrap(), 42);

        // Test to_double() alias
        assert_eq!(amount.to_double().unwrap(), 42.0);

        // Test to_i32()
        assert_eq!(amount.to_i32().unwrap(), 42);

        // Test to_u32()
        assert_eq!(amount.to_u32().unwrap(), 42);

        // Test to_u64()
        assert_eq!(amount.to_u64().unwrap(), 42);

        // Test to_f32()
        assert_eq!(amount.to_f32().unwrap(), 42.0);
    }

    #[test]
    fn test_to_unsigned_from_negative() {
        let amount = Amount::from_i64(-42);

        // Should fail to convert negative to unsigned
        assert!(amount.to_u32().is_err());
        assert!(amount.to_u64().is_err());
    }

    #[test]
    fn test_to_integer_from_non_integer() {
        let amount = Amount::from_decimal(Decimal::new(425, 1)).unwrap(); // 42.5

        // Should fail to convert non-integer to integer types
        assert!(amount.to_i32().is_err());
        assert!(amount.to_i64().is_err());
        assert!(amount.to_u32().is_err());
        assert!(amount.to_u64().is_err());
    }

    #[test]
    fn test_overflow_conversions() {
        // Create an amount larger than i32::MAX
        let large = Amount::from_i64(i64::from(i32::MAX) + 1);

        // Should fail to convert to i32
        assert!(large.to_i32().is_err());

        // But should succeed for i64
        assert!(large.to_i64().is_ok());
    }

    #[test]
    fn test_as_ref_implementations() {
        let amount = Amount::from_i64(42);

        // Test AsRef<Amount>
        let amt_ref: &Amount = amount.as_ref();
        assert_eq!(amt_ref.to_i64().unwrap(), 42);

        // Test AsRef<Option<BigRational>>
        let rational_ref: &Option<BigRational> = amount.as_ref();
        assert!(rational_ref.is_some());
    }

    #[test]
    fn test_as_rational_method() {
        let amount = Amount::from_i64(42);
        let rational = amount.as_rational();
        assert!(rational.is_some());

        let null_amount = Amount::null();
        assert!(null_amount.as_rational().is_none());
    }

    #[test]
    fn test_to_into_rational() {
        let amount = Amount::from_i64(42);

        // Test to_rational() - non-consuming
        let rational_ref = amount.to_rational();
        assert!(rational_ref.is_some());
        assert_eq!(rational_ref.unwrap().to_i64().unwrap(), 42);

        // Original amount should still be usable
        assert_eq!(amount.to_i64().unwrap(), 42);

        // Test into_rational() - consuming
        let rational_opt = amount.into_rational();
        assert!(rational_opt.is_some());
        assert_eq!(rational_opt.unwrap().to_i64().unwrap(), 42);

        // amount is now moved and cannot be used
    }

    #[test]
    fn test_serde_serialization() {
        // This is a basic test to ensure the Serialize/Deserialize implementations compile
        // Full serde_json testing would require adding serde_json as a dev dependency
        let amount = Amount::from_decimal(Decimal::new(12345, 2)).unwrap(); // 123.45

        // The serde implementations are available and will be used by serialization libraries
        // We just verify the Amount can be created and basic properties work
        assert_eq!(amount.precision(), 2);
        assert!(!amount.is_null());

        // The custom serde implementation should handle the quantity properly
        let null_amount = Amount::null();
        assert!(null_amount.is_null());
    }
}
