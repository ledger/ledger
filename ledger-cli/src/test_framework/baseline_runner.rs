//! Baseline Test Runner
//!
//! This module implements the baseline test runner for running tests from
//! the test/baseline directory. These tests verify core functionality and
//! serve as acceptance criteria for the Rust implementation.

use anyhow::{Context, Result};
use colored::*;
use log::{debug, error, info, warn};
use std::fs;
use std::path::{Path, PathBuf};

use super::diff_reporter::{DiffConfig, DiffFormat, DiffReporter};
use super::test_harness::{ProcessResult, TestHarness};
use super::test_parser::{BaselineTestParser, TestCase};

/// Configuration for baseline test execution
#[derive(Debug, Clone)]
pub struct BaselineConfig {
    /// Whether to update expected results on mismatch
    pub update_expected: bool,
    /// Whether to use colored output
    pub colored_output: bool,
    /// Floating point tolerance for numeric comparisons
    pub fp_tolerance: Option<f64>,
    /// Maximum test execution timeout in seconds
    pub timeout_seconds: u64,
}

impl Default for BaselineConfig {
    fn default() -> Self {
        Self {
            update_expected: false,
            colored_output: true,
            fp_tolerance: Some(1e-10),
            timeout_seconds: 30,
        }
    }
}

/// Baseline test runner
pub struct BaselineRunner<'a> {
    harness: &'a TestHarness,
    config: BaselineConfig,
    diff_reporter: DiffReporter,
}

impl<'a> BaselineRunner<'a> {
    /// Create a new baseline test runner
    pub fn new(harness: &'a TestHarness) -> Self {
        let diff_config = DiffConfig {
            format: DiffFormat::Unified,
            context_lines: 3,
            show_line_numbers: true,
            use_color: true,
            floating_point_tolerance: Some(1e-10),
            ignore_whitespace: false,
            tab_width: 4,
        };

        Self {
            harness,
            config: BaselineConfig::default(),
            diff_reporter: DiffReporter::with_config(diff_config),
        }
    }

    /// Create baseline runner with custom configuration
    pub fn with_config(harness: &'a TestHarness, config: BaselineConfig) -> Self {
        let diff_config = DiffConfig {
            use_color: config.colored_output,
            floating_point_tolerance: config.fp_tolerance,
            ..Default::default()
        };

        Self { harness, config, diff_reporter: DiffReporter::with_config(diff_config) }
    }

    /// Run all baseline tests in the specified directory
    pub fn run_tests<P: AsRef<Path>>(&self, test_dir: P) -> Result<()> {
        let test_dir = test_dir.as_ref();

        info!("Running baseline tests from: {}", test_dir.display());

        // Find all test files
        let test_files = self.find_test_files(test_dir)?;

        if test_files.is_empty() {
            warn!("No test files found in: {}", test_dir.display());
            return Ok(());
        }

        info!("Found {} test files", test_files.len());

        // Run tests sequentially for baseline tests (they're usually fewer and more critical)
        for test_file in &test_files {
            self.run_test_file(test_file)?;
        }

        Ok(())
    }

    /// Run a single baseline test file
    pub fn run_test_file<P: AsRef<Path>>(&self, test_file: P) -> Result<bool> {
        let test_file = test_file.as_ref();

        debug!("Running baseline test: {}", test_file.display());

        // Check if test file is empty
        if fs::metadata(test_file)?.len() == 0 {
            warn!("Empty test file detected: {}", test_file.display());
            self.harness.failure(&test_file.file_name().unwrap().to_string_lossy());
            return Ok(false);
        }

        // Parse test file
        let parser = BaselineTestParser::new(test_file);
        let test_case = parser.parse().with_context(|| {
            format!("Failed to parse baseline test file: {}", test_file.display())
        })?;

        // Run the test case
        self.run_test_case(test_file, &test_case)
    }

    /// Run a single baseline test case
    fn run_test_case(&self, test_file: &Path, test_case: &TestCase) -> Result<bool> {
        let test_name = test_file.file_name().unwrap().to_string_lossy();

        info!("Running baseline test: {}", test_name);

        // For baseline tests, we often need to use the sample data
        let command = self.prepare_command(test_case, test_file)?;

        // Execute command
        let result = self.harness.run_command(&command, !command.contains("--columns"))?;

        // For baseline tests, if there's no expected output, we compare against
        // stored expected output file or create a new baseline
        let success = if test_case.expected_output.is_empty() {
            self.handle_baseline_without_expected(test_file, test_case, &result)?
        } else {
            self.compare_against_expected(test_file, test_case, &result)?
        };

        if success {
            self.harness.success();
            info!("✓ {}", test_name.green());
        } else {
            self.harness.failure(&test_name);
            error!("✗ {}", test_name.red());
        }

        Ok(success)
    }

    /// Prepare command for baseline test execution
    fn prepare_command(&self, test_case: &TestCase, test_file: &Path) -> Result<String> {
        let mut command = test_case.command.clone();

        // Many baseline tests use the standard test data
        let test_data_file = self.find_test_data_file(test_file)?;

        // Add file argument if not present
        if !command.contains("-f ") {
            command = format!("{} -f \"{}\"", command, test_data_file.display());
        }

        // Add ledger prefix if not present
        if !command.starts_with("$ledger") && !command.contains("ledger") {
            command = format!("$ledger {}", command);
        }

        Ok(command)
    }

    /// Find the appropriate test data file for a baseline test
    fn find_test_data_file(&self, test_file: &Path) -> Result<PathBuf> {
        let test_dir = test_file.parent().unwrap();
        let input_dir = test_dir.parent().unwrap().join("input");

        // Common test data files in order of preference
        let data_files = ["sample.dat", "standard.dat", "demo.ledger"];

        for data_file in &data_files {
            let path = input_dir.join(data_file);
            if path.exists() {
                return Ok(path);
            }
        }

        // Look for test-specific data file
        let test_stem = test_file.file_stem().unwrap().to_string_lossy();
        let specific_data = input_dir.join(format!("{}.dat", test_stem));
        if specific_data.exists() {
            return Ok(specific_data);
        }

        // Default to sample.dat path (may not exist, but command will handle error)
        Ok(input_dir.join("sample.dat"))
    }

    /// Handle baseline tests without expected output (create or compare against stored baseline)
    fn handle_baseline_without_expected(
        &self,
        test_file: &Path,
        test_case: &TestCase,
        result: &ProcessResult,
    ) -> Result<bool> {
        let expected_file = self.get_expected_output_file(test_file);

        if expected_file.exists() {
            // Compare against stored expected output
            let expected_content = fs::read_to_string(&expected_file)?;
            let expected_lines: Vec<String> =
                expected_content.lines().map(|s| s.to_string()).collect();
            let actual_lines = self.harness.normalize_output(&result.stdout);

            let diff_result = if self.config.fp_tolerance.is_some() {
                self.diff_reporter.compare_with_tolerance(&expected_lines, &actual_lines)
            } else {
                self.diff_reporter.compare(&expected_lines, &actual_lines)
            };

            if diff_result.has_differences {
                println!("{}", format!("FAILURE in baseline test: {}", test_file.display()).red());
                println!("{}", diff_result.diff_output);

                if self.config.update_expected {
                    info!("Updating expected output for: {}", test_file.display());
                    fs::write(&expected_file, &result.stdout)?;
                    return Ok(true);
                }
                return Ok(false);
            }

            Ok(true)
        } else {
            // Create new baseline
            info!("Creating new baseline for: {}", test_file.display());
            if let Some(parent) = expected_file.parent() {
                fs::create_dir_all(parent)?;
            }
            fs::write(&expected_file, &result.stdout)?;
            Ok(true)
        }
    }

    /// Compare test results against expected output embedded in test file
    fn compare_against_expected(
        &self,
        test_file: &Path,
        test_case: &TestCase,
        result: &ProcessResult,
    ) -> Result<bool> {
        let mut success = true;

        // Compare exit code
        if result.exit_code != test_case.expected_exit_code {
            success = false;
            println!(
                "{}",
                format!(
                    "FAILURE in exit code for {}: {} != {}",
                    test_file.display(),
                    result.exit_code,
                    test_case.expected_exit_code
                )
                .red()
            );
        }

        // Compare stdout
        let actual_output = self.harness.normalize_output(&result.stdout);
        let diff_result = if self.config.fp_tolerance.is_some() {
            self.diff_reporter.compare_with_tolerance(&test_case.expected_output, &actual_output)
        } else {
            self.diff_reporter.compare(&test_case.expected_output, &actual_output)
        };

        if diff_result.has_differences {
            success = false;
            println!("{}", format!("FAILURE in output for {}: ", test_file.display()).red());
            println!("{}", diff_result.diff_output);
        }

        // Compare stderr if expected
        if !test_case.expected_error.is_empty() || !result.stderr.trim().is_empty() {
            let actual_error = self.harness.normalize_output(&result.stderr);
            let diff_result = self.diff_reporter.compare(&test_case.expected_error, &actual_error);

            if diff_result.has_differences {
                success = false;
                println!(
                    "{}",
                    format!("FAILURE in error output for {}: ", test_file.display()).red()
                );
                println!("{}", diff_result.diff_output);
            }
        }

        Ok(success)
    }

    /// Get expected output file path for a baseline test
    fn get_expected_output_file(&self, test_file: &Path) -> PathBuf {
        let test_stem = test_file.file_stem().unwrap().to_string_lossy();
        let expected_dir = test_file.parent().unwrap().join("expected");
        expected_dir.join(format!("{}.expected", test_stem))
    }

    /// Find all baseline test files in directory
    fn find_test_files<P: AsRef<Path>>(&self, test_dir: P) -> Result<Vec<PathBuf>> {
        let test_dir = test_dir.as_ref();
        let mut test_files = Vec::new();

        for entry in fs::read_dir(test_dir)? {
            let entry = entry?;
            let path = entry.path();

            if path.is_file() && path.extension().is_some_and(|ext| ext == "test") {
                // Skip Python tests unless Python support is enabled
                if path.file_name().unwrap().to_string_lossy().contains("_py.test")
                    && !self.harness.python_support
                {
                    continue;
                }

                test_files.push(path);
            }
        }

        // Sort test files for consistent execution order
        test_files.sort();

        Ok(test_files)
    }

    /// Generate baseline report showing test coverage and results
    pub fn generate_report<P: AsRef<Path>>(&self, test_dir: P) -> Result<()> {
        let test_files = self.find_test_files(test_dir)?;

        println!("{}", "Baseline Test Coverage Report".cyan().bold());
        println!("{}", "=============================".cyan());
        println!();

        // Analyze test categories
        let mut cmd_tests = 0;
        let mut opt_tests = 0;
        let mut dir_tests = 0;
        let mut feat_tests = 0;

        for test_file in &test_files {
            let filename = test_file.file_name().unwrap().to_string_lossy();
            if filename.starts_with("cmd-") {
                cmd_tests += 1;
            } else if filename.starts_with("opt-") {
                opt_tests += 1;
            } else if filename.starts_with("dir-") {
                dir_tests += 1;
            } else if filename.starts_with("feat-") {
                feat_tests += 1;
            }
        }

        println!("Test Categories:");
        println!("  Command tests (cmd-*):   {}", cmd_tests);
        println!("  Option tests (opt-*):    {}", opt_tests);
        println!("  Directive tests (dir-*): {}", dir_tests);
        println!("  Feature tests (feat-*):  {}", feat_tests);
        println!("  Total tests:             {}", test_files.len());
        println!();

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::{NamedTempFile, TempDir};

    #[test]
    fn test_baseline_runner_creation() {
        let temp_dir = TempDir::new().unwrap();
        let ledger_path = temp_dir.path().join("ledger");
        std::fs::write(&ledger_path, "#!/bin/bash\necho 'test'").unwrap();

        let harness = TestHarness::new(&ledger_path, &temp_dir.path().to_path_buf()).unwrap();
        let runner = BaselineRunner::new(&harness);

        assert!(!runner.config.update_expected);
        assert!(runner.config.colored_output);
    }

    #[test]
    fn test_find_test_data_file() {
        let temp_dir = TempDir::new().unwrap();

        // Create test structure
        let input_dir = temp_dir.path().join("input");
        fs::create_dir_all(&input_dir).unwrap();
        let sample_file = input_dir.join("sample.dat");
        fs::write(&sample_file, "test data").unwrap();

        let baseline_dir = temp_dir.path().join("baseline");
        fs::create_dir_all(&baseline_dir).unwrap();
        let test_file = baseline_dir.join("cmd-balance.test");
        fs::write(&test_file, "test balance").unwrap();

        let ledger_path = temp_dir.path().join("ledger");
        std::fs::write(&ledger_path, "#!/bin/bash\necho 'test'").unwrap();

        let harness = TestHarness::new(&ledger_path, &temp_dir.path().to_path_buf()).unwrap();
        let runner = BaselineRunner::new(&harness);

        let data_file = runner.find_test_data_file(&test_file).unwrap();
        assert_eq!(data_file, sample_file);
    }
}
