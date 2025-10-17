//! Manual Test Runner Binary
//!
//! This binary provides a command-line interface for running manual tests,
//! which can contain embedded test data and require interactive verification.

use anyhow::Result;
use clap::Parser;
use log::{error, info};
use std::path::PathBuf;

use ledger_cli::test_framework::manual_runner::{ManualConfig, ManualRunner};
use ledger_cli::test_framework::test_harness::TestHarness;

#[derive(Parser)]
#[command(name = "manual_runner")]
#[command(about = "Run ledger manual tests")]
struct Args {
    /// Path to the ledger binary
    #[arg(long, default_value = "./build/ledger")]
    ledger: PathBuf,

    /// Source path for the ledger project
    #[arg(long, default_value = ".")]
    sourcepath: PathBuf,

    /// Enable verification mode
    #[arg(long)]
    verify: bool,

    /// Use gmalloc for memory debugging
    #[arg(long)]
    gmalloc: bool,

    /// Enable Python support for Python-based tests
    #[arg(long)]
    python: bool,

    /// Run tests interactively (prompt user for verification)
    #[arg(short, long)]
    interactive: bool,

    /// Disable colored output
    #[arg(long)]
    no_color: bool,

    /// Floating point tolerance for numeric comparisons
    #[arg(long, default_value_t = 1e-10)]
    fp_tolerance: f64,

    /// Test file pattern filter
    #[arg(long)]
    filter: Option<String>,

    /// Generate test report instead of running tests
    #[arg(long)]
    report: bool,

    /// Test directory or file to run
    tests: PathBuf,
}

fn main() -> Result<()> {
    // Initialize logging
    env_logger::init();

    let args = Args::parse();

    info!("Running manual tests with ledger: {}", args.ledger.display());
    info!("Source path: {}", args.sourcepath.display());
    info!("Tests path: {}", args.tests.display());

    // Create test harness
    let harness = TestHarness::new(&args.ledger, &args.sourcepath)?
        .with_verify(args.verify)
        .with_gmalloc(args.gmalloc)
        .with_python(args.python);

    // Configure manual runner
    let config = ManualConfig {
        interactive: args.interactive,
        colored_output: !args.no_color,
        fp_tolerance: Some(args.fp_tolerance),
        timeout_seconds: 30,
        test_filter: args.filter,
    };

    let runner = ManualRunner::with_config(&harness, config);

    if args.report {
        // Generate report instead of running tests
        if args.tests.is_dir() {
            runner.generate_report(&args.tests)?;
        } else {
            eprintln!("Report generation requires a directory, not a single file");
        }
        return Ok(());
    }

    // Run tests
    if args.tests.is_dir() {
        info!("Running all manual tests in directory: {}", args.tests.display());
        runner.run_tests(&args.tests)?;
    } else if args.tests.is_file() {
        info!("Running single manual test file: {}", args.tests.display());
        if !runner.run_test_file(&args.tests)? {
            error!("Test file failed: {}", args.tests.display());
        }
    } else {
        // Try to resolve test pattern similar to regression runner
        let test_name = args.tests.file_name().unwrap().to_string_lossy();
        if let Some(resolved_path) = resolve_test_pattern(&test_name)? {
            info!("Resolved test pattern to: {}", resolved_path.display());
            if resolved_path.is_dir() {
                runner.run_tests(&resolved_path)?;
            } else if !runner.run_test_file(&resolved_path)? {
                error!("Test file failed: {}", resolved_path.display());
            }
        } else {
            anyhow::bail!("Test path does not exist: {}", args.tests.display());
        }
    }

    // Test harness will exit with appropriate code
    harness.exit();
}

/// Resolve test patterns like "ManualTest_transaction-codes-1" to "test/manual/transaction-codes-1.test"
fn resolve_test_pattern(pattern: &str) -> Result<Option<PathBuf>> {
    if pattern.starts_with("ManualTest_") || pattern.starts_with("Manual_") {
        let test_name = if let Some(name) = pattern.strip_prefix("ManualTest_") {
            name
        } else {
            pattern.strip_prefix("Manual_").unwrap()
        };

        let test_path = PathBuf::from("test/manual").join(format!("{}.test", test_name));
        if test_path.exists() {
            return Ok(Some(test_path));
        }
    }

    Ok(None)
}
