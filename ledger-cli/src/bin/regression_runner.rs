//! Regression Test Runner Binary
//!
//! This binary provides a command-line interface for running regression tests,
//! equivalent to the Python RegressTests.py script.

use clap::Parser;
use anyhow::Result;
use std::path::PathBuf;
use log::{info, error};

use ledger_cli::test_framework::{TestRunner};
use ledger_cli::test_framework::regression_runner::{RegressionRunner, RegressionConfig};

#[derive(Parser)]
#[command(name = "regression_runner")]
#[command(about = "Run ledger regression tests")]
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

    /// Number of parallel jobs
    #[arg(short, long, default_value_t = num_cpus::get())]
    jobs: usize,

    /// Update baselines on failure
    #[arg(long)]
    update_baselines: bool,

    /// Disable colored output
    #[arg(long)]
    no_color: bool,

    /// Floating point tolerance for numeric comparisons
    #[arg(long, default_value_t = 1e-10)]
    fp_tolerance: f64,

    /// Test file pattern filter
    #[arg(long)]
    filter: Option<String>,

    /// Test directory or file to run
    tests: PathBuf,
}

fn main() -> Result<()> {
    // Initialize logging
    env_logger::init();

    let args = Args::parse();

    info!("Running regression tests with ledger: {}", args.ledger.display());
    info!("Source path: {}", args.sourcepath.display());
    info!("Tests path: {}", args.tests.display());

    // Create test harness
    let harness = ledger_cli::test_framework::test_harness::TestHarness::new(&args.ledger, &args.sourcepath)?
        .with_verify(args.verify)
        .with_gmalloc(args.gmalloc)
        .with_python(args.python);

    // Configure regression runner
    let config = RegressionConfig {
        parallel_jobs: args.jobs,
        update_baselines: args.update_baselines,
        colored_output: !args.no_color,
        fp_tolerance: Some(args.fp_tolerance),
        timeout_seconds: 30,
        test_filter: args.filter,
    };

    let runner = RegressionRunner::with_config(&harness, config);

    // Check if tests path is a directory or file
    if args.tests.is_dir() {
        info!("Running all regression tests in directory: {}", args.tests.display());
        runner.run_tests(&args.tests)?;
    } else if args.tests.is_file() {
        info!("Running single regression test file: {}", args.tests.display());
        if !runner.run_test_file(&args.tests)? {
            error!("Test file failed: {}", args.tests.display());
        }
    } else {
        // Try to match pattern like "Baseline_TestName" or "Regress_TestName"
        let test_name = args.tests.file_name().unwrap().to_string_lossy();
        if let Some(resolved_path) = resolve_test_pattern(&test_name)? {
            info!("Resolved test pattern to: {}", resolved_path.display());
            if resolved_path.is_dir() {
                runner.run_tests(&resolved_path)?;
            } else {
                if !runner.run_test_file(&resolved_path)? {
                    error!("Test file failed: {}", resolved_path.display());
                }
            }
        } else {
            anyhow::bail!("Test path does not exist: {}", args.tests.display());
        }
    }

    // Test harness will exit with appropriate code
    harness.exit();
}

/// Resolve test patterns like "RegressTest_1036" to "test/regress/1036.test"
fn resolve_test_pattern(pattern: &str) -> Result<Option<PathBuf>> {
    if pattern.starts_with("RegressTest_") || pattern.starts_with("Regress_") {
        let test_name = if let Some(name) = pattern.strip_prefix("RegressTest_") {
            name
        } else {
            pattern.strip_prefix("Regress_").unwrap()
        };
        
        let test_path = PathBuf::from("test/regress").join(format!("{}.test", test_name));
        if test_path.exists() {
            return Ok(Some(test_path));
        }
    } else if pattern.starts_with("BaselineTest_") || pattern.starts_with("Baseline_") {
        let test_name = if let Some(name) = pattern.strip_prefix("BaselineTest_") {
            name
        } else {
            pattern.strip_prefix("Baseline_").unwrap()
        };
        
        let test_path = PathBuf::from("test/baseline").join(format!("{}.test", test_name));
        if test_path.exists() {
            return Ok(Some(test_path));
        }
    } else if pattern.starts_with("ManualTest_") || pattern.starts_with("Manual_") {
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