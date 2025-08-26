//! Arbitrary precision mathematical operations for Ledger
//!
//! This crate provides high-precision arithmetic operations using BigRational
//! for exact calculations required in financial accounting. It implements the
//! core mathematical types: Amount and Balance.

#![warn(clippy::all)]
#![warn(missing_docs)]

pub mod amount;
pub mod commodity;
pub mod balance;
pub mod datetime;
pub mod formatting;

// Re-export main types
pub use amount::{Amount, AmountError, AmountResult, Precision, EXTEND_BY_DIGITS};
pub use commodity::{
    Commodity, CommodityRef, CommodityFlags, CommodityPool,
    Annotation, AnnotationFlags, AnnotatedCommodity, 
    KeepDetails, Expression, Date as CommodityDate,
    null_commodity
};
pub use balance::{Balance, BalanceError, BalanceResult};
pub use datetime::{
    Date, LocalDateTime, DateTimeError, DateTimeResult, FormatType,
    DateDuration, parse_date, parse_datetime, format_date, format_datetime,
    timezone, DEFAULT_TIMEZONE
};
pub use formatting::{
    FormatConfig, FormatFlags, format_amount, format_balance, format_rational, 
    apply_width_formatting
};

// Re-export for convenience
pub use num_bigint::BigInt;
pub use num_rational::BigRational;
pub use rust_decimal::Decimal;
