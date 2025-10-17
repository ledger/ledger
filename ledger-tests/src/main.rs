//! Main binary for running Ledger tests

use clap::Parser;
use std::process;

use ledger_tests::{config::TestConfig, harness::LedgerHarness, TestError};

#[tokio::main]
async fn main() {
    let config = match TestConfig::try_parse() {
        Ok(config) => config,
        Err(e) => {
            eprintln!("Configuration error: {}", e);
            process::exit(1);
        }
    };

    let harness = match LedgerHarness::new(config) {
        Ok(harness) => harness,
        Err(e) => {
            eprintln!("Failed to create test harness: {}", e);
            process::exit(1);
        }
    };

    match harness.run_all_tests().await {
        Ok(report) => {
            if !harness.config().quiet {
                report.print_summary();
            }

            if harness.config().verbose {
                report.print_detailed();
            }

            // Exit with non-zero if tests failed
            if !report.statistics.all_passed() {
                process::exit(1);
            }
        }
        Err(e) => {
            eprintln!("Test execution failed: {}", e);
            process::exit(1);
        }
    }
}
