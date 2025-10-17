//! # Ledger Test Framework
//! 
//! A comprehensive test framework for the Ledger Rust implementation,
//! designed to be compatible with the original Python test harness.
//!
//! ## Architecture
//!
//! The framework is organized into several modules:
//! - `harness`: Core test execution engine
//! - `discovery`: Test file discovery and parsing
//! - `execution`: Test runner implementations
//! - `comparison`: Output comparison and diff generation  
//! - `reporting`: Test result reporting and statistics
//! - `config`: Configuration and settings management
//! - `python`: PyO3 bindings for Python compatibility (optional)
//! - `fuzzing`: Fuzzing infrastructure (optional)

pub mod config;
pub mod comparison;
pub mod discovery;
pub mod execution;
pub mod harness;
pub mod reporting;
pub mod baseline;

// FIXME: failed to resolve mod `python`: .../python.rs does not exist
// #[cfg(feature = "python-bindings")]
// pub mod python;

// FIXME: Error writing files: failed to resolve mod `fuzzing`: .../fuzzing.rs does not exist
// #[cfg(feature = "fuzzing")]
// pub mod fuzzing;

// Re-exports for easier access
pub use harness::{LedgerHarness, TestRunner};
pub use config::{TestConfig, TestCategory};
pub use discovery::{TestCase, TestSuite};
pub use execution::{TestResult, TestStatus};
pub use comparison::OutputComparison;
pub use reporting::TestReport;
pub use baseline::{BaselineRunner, BaselineCoverage};

/// Current version of the test framework
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// Test framework errors
#[derive(thiserror::Error, Debug)]
pub enum TestError {
    #[error("Test discovery failed: {0}")]
    Discovery(String),
    
    #[error("Test execution failed: {0}")]
    Execution(String),
    
    #[error("Output comparison failed: {0}")]
    Comparison(String),
    
    #[error("Configuration error: {0}")]
    Config(String),
    
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
    
    #[error("Parse error: {0}")]
    Parse(String),
    
    #[error("Template error: {0}")]
    Template(#[from] indicatif::style::TemplateError),
}