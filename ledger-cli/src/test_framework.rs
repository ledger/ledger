//! Test Framework Module
//!
//! This module provides a comprehensive test framework for running ledger tests,
//! including baseline tests, regression tests, manual tests, and performance benchmarks.

pub mod baseline_runner;
pub mod regression_runner;
pub mod manual_runner;
pub mod test_harness;
pub mod diff_reporter;
pub mod test_parser;
pub mod output_validator;
pub mod bench_framework;

use std::path::Path;
use anyhow::Result;

/// Main test runner orchestrator
pub struct TestRunner {
    pub harness: test_harness::TestHarness,
}

impl TestRunner {
    /// Create a new test runner
    pub fn new<P: AsRef<Path>>(ledger_path: P, source_path: P) -> Result<Self> {
        Ok(TestRunner {
            harness: test_harness::TestHarness::new(ledger_path, source_path)?,
        })
    }

    /// Run baseline tests
    pub fn run_baseline_tests<P: AsRef<Path>>(&self, test_dir: P) -> Result<()> {
        let runner = baseline_runner::BaselineRunner::new(&self.harness);
        runner.run_tests(test_dir)
    }

    /// Run regression tests
    pub fn run_regression_tests<P: AsRef<Path>>(&self, test_dir: P) -> Result<()> {
        let runner = regression_runner::RegressionRunner::new(&self.harness);
        runner.run_tests(test_dir)
    }

    /// Run manual tests
    pub fn run_manual_tests<P: AsRef<Path>>(&self, test_dir: P) -> Result<()> {
        let runner = manual_runner::ManualRunner::new(&self.harness);
        runner.run_tests(test_dir)
    }
}