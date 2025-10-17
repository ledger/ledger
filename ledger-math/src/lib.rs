//! Arbitrary precision mathematical operations for Ledger
//!
//! This crate provides high-precision arithmetic operations using BigRational
//! for exact calculations required in financial accounting. It implements the
//! core mathematical types: Amount and Balance.

#![warn(clippy::all)]
// FIXME: temporarily allow missing docs
// #![warn(missing_docs)]

pub mod amount;
pub mod balance;
pub mod commodity;
pub mod datetime;
pub mod formatting;

// Re-export main types
pub use amount::{Amount, AmountError, AmountResult, Precision, EXTEND_BY_DIGITS};
pub use balance::{Balance, BalanceError, BalanceResult};
pub use commodity::{
    null_commodity, AnnotatedCommodity, Annotation, AnnotationFlags, Commodity, CommodityFlags,
    CommodityPool, CommodityRef, Date as CommodityDate, Expression, KeepDetails,
};
pub use datetime::{
    format_date, format_datetime, parse_date, parse_datetime, timezone, Date, DateDuration,
    DateTimeError, DateTimeResult, FormatType, LocalDateTime, DEFAULT_TIMEZONE,
};
pub use formatting::{
    apply_width_formatting, format_amount, format_balance, format_rational, FormatConfig,
    FormatFlags,
};

// Re-export for convenience
pub use num_bigint::BigInt;
pub use num_rational::BigRational;
pub use rust_decimal::Decimal;
