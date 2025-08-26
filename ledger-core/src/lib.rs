//! Core accounting engine for Ledger
//!
//! This crate provides the fundamental data structures and algorithms
//! for double-entry bookkeeping, including transactions, accounts,
//! amounts with commodity support, and journal processing.

#![warn(clippy::all)]
#![warn(missing_docs)]

/// Module for amount arithmetic with commodity support
pub mod amount;

/// Module for multi-commodity balance management
pub mod balance;

/// Module for commodity definitions and handling
pub mod commodity;

/// Module for hierarchical account structure
pub mod account;

/// Module for journal data structure
pub mod journal;

/// Module for transaction representation
pub mod transaction;

/// Module for posting/entry representation
pub mod posting;

/// Module for data filtering operations
pub mod filters;

/// Module for output formatting
pub mod output;

/// Module for report generation
pub mod report;

/// Module for expression evaluation
pub mod expr;

/// Module for date/time parsing and handling  
pub mod datetime;

/// Module for journal file parsing using nom combinators
pub mod parser;

/// Module for transaction parsing using nom combinators
pub mod transaction_parser;

/// Module for C Foreign Function Interface
pub mod ffi;

/// Module for report caching system
pub mod cache;

/// Module for optimized string handling
pub mod strings;

/// Module for zero-copy parsing optimizations
pub mod parser_zero_copy;

/// Module for parallel processing optimizations
pub mod parallel;

/// Module for optimized data structures
pub mod data_structures;

/// Module for advanced caching and lazy evaluation
pub mod advanced_cache;

/// Module for SIMD-accelerated arithmetic operations
pub mod simd_math;

/// Module for comprehensive performance benchmarking and comparison
pub mod performance_suite;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_placeholder() {
        assert_eq!(2 + 2, 4);
    }
}
