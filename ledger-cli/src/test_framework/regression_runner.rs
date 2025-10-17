//! Regression Test Runner
//!
//! This module implements the regression test runner that can execute tests from
//! the test/regress directory, comparing output against expected results with
//! detailed diff reporting.

use anyhow::{Context, Result};
use colored::*;
use log::{debug, error, info, warn};
use rayon::prelude::*;
use std::fs;
use std::path::{Path, PathBuf};

use super::diff_reporter::{DiffConfig, DiffFormat, DiffReporter};
use super::test_harness::{ProcessResult, TestHarness};
use super::test_parser::{RegressionTestParser, TestCase};

/// Configuration for regression test execution
#[derive(Debug, Clone)]
pub struct RegressionConfig {
    /// Number of parallel test jobs
    pub parallel_jobs: usize,
    /// Whether to update baselines on failure
    pub update_baselines: bool,
    /// Whether to use colored output
    pub colored_output: bool,
    /// Floating point tolerance for numeric comparisons
    pub fp_tolerance: Option<f64>,
    /// Maximum test execution timeout in seconds
    pub timeout_seconds: u64,
    /// Test file pattern filter (regex)
    pub test_filter: Option<String>,
}

impl Default for RegressionConfig {
    fn default() -> Self {
        Self {
            parallel_jobs: num_cpus::get(),
            update_baselines: false,
            colored_output: true,
            fp_tolerance: Some(1e-10),
            timeout_seconds: 30,
            test_filter: None,
        }
    }
}

/// Regression test runner
pub struct RegressionRunner<'a> {
    harness: &'a TestHarness,
    config: RegressionConfig,
    diff_reporter: DiffReporter,
}

impl<'a> RegressionRunner<'a> {
    /// Create a new regression test runner
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
            config: RegressionConfig::default(),
            diff_reporter: DiffReporter::with_config(diff_config),
        }
    }

    /// Create regression runner with custom configuration
    pub fn with_config(harness: &'a TestHarness, config: RegressionConfig) -> Self {
        let diff_config = DiffConfig {
            use_color: config.colored_output,
            floating_point_tolerance: config.fp_tolerance,
            ..Default::default()
        };

        Self { harness, config, diff_reporter: DiffReporter::with_config(diff_config) }
    }

    /// Run all regression tests in the specified directory
    pub fn run_tests<P: AsRef<Path>>(&self, test_dir: P) -> Result<()> {
        let test_dir = test_dir.as_ref();

        info!("Running regression tests from: {}", test_dir.display());

        // Find all test files
        let test_files = self.find_test_files(test_dir)?;

        if test_files.is_empty() {
            warn!("No test files found in: {}", test_dir.display());
            return Ok(());
        }

        info!("Found {} test files", test_files.len());

        // Run tests in parallel if configured
        if self.config.parallel_jobs > 1 {
            self.run_tests_parallel(&test_files)?;
        } else {
            self.run_tests_sequential(&test_files)?;
        }

        Ok(())
    }

    /// Run a single regression test file
    pub fn run_test_file<P: AsRef<Path>>(&self, test_file: P) -> Result<bool> {
        let test_file = test_file.as_ref();

        debug!("Running test file: {}", test_file.display());

        // Check if test file is empty
        if fs::metadata(test_file)?.len() == 0 {
            warn!("Empty test file detected: {}", test_file.display());
            self.harness.failure(&test_file.file_name().unwrap().to_string_lossy());
            return Ok(false);
        }

        // Parse test file
        let parser = RegressionTestParser::new(test_file);
        let test_cases = parser
            .parse()
            .with_context(|| format!("Failed to parse test file: {}", test_file.display()))?;

        if test_cases.is_empty() {
            warn!("No test cases found in: {}", test_file.display());
            return Ok(true);
        }

        let mut all_passed = true;

        // Run each test case in the file
        for (i, test_case) in test_cases.iter().enumerate() {
            let test_name = format!("{}:{}", test_file.display(), i + 1);
            if !self.run_test_case(&test_name, test_case, test_file)? {
                all_passed = false;
            }
        }

        Ok(all_passed)
    }

    /// Run tests sequentially
    fn run_tests_sequential(&self, test_files: &[PathBuf]) -> Result<()> {
        for test_file in test_files {
            self.run_test_file(test_file)?;
        }
        Ok(())
    }

    /// Run tests in parallel
    fn run_tests_parallel(&self, test_files: &[PathBuf]) -> Result<()> {
        let pool =
            rayon::ThreadPoolBuilder::new().num_threads(self.config.parallel_jobs).build()?;

        pool.install(|| {
            test_files.par_iter().for_each(|test_file| {
                if let Err(e) = self.run_test_file(test_file) {
                    error!("Error running test {}: {}", test_file.display(), e);
                }
            });
        });

        Ok(())
    }

    /// Run a single test case
    fn run_test_case(
        &self,
        test_name: &str,
        test_case: &TestCase,
        test_file: &Path,
    ) -> Result<bool> {
        debug!("Running test case: {}", test_name);

        // Prepare command with file context
        let mut command = test_case.command.clone();

        // Check if command needs file input
        let use_stdin = command.contains("-f -") || command.contains("-f /dev/stdin");

        // Add file argument if not present and not using stdin
        if !command.contains("-f ") && !use_stdin {
            command = format!("{} -f \"{}\"", command, test_file.display());
        }

        // Add ledger prefix if not already present
        if !command.starts_with("$ledger") && !command.contains("ledger") {
            command = format!("$ledger {}", command);
        }

        // Execute command
        let result = if use_stdin {
            self.run_command_with_stdin(&command, test_file)?
        } else {
            self.harness.run_command(&command, !command.contains("--columns"))?
        };

        // Compare results
        let success = self.compare_results(test_name, test_case, &result)?;

        if success {
            self.harness.success();
        } else {
            self.harness.failure(&test_file.file_name().unwrap().to_string_lossy());
        }

        Ok(success)
    }

    /// Run command with stdin input from file
    fn run_command_with_stdin(&self, command: &str, input_file: &Path) -> Result<ProcessResult> {
        // For commands that use stdin, we need to handle them specially
        // This is a simplified implementation - a full implementation would
        // properly handle process pipes
        self.harness.run_command(command, true)
    }

    /// Compare test results against expected output
    fn compare_results(
        &self,
        test_name: &str,
        test_case: &TestCase,
        result: &ProcessResult,
    ) -> Result<bool> {
        let mut success = true;

        // Compare exit code
        if result.exit_code != test_case.expected_exit_code {
            success = false;
            self.print_failure_header(test_name, test_case);
            println!(
                "{}",
                format!(
                    "FAILURE in exit code ({} != {})",
                    result.exit_code, test_case.expected_exit_code
                )
                .red()
            );
        }

        // Compare stdout
        if !test_case.expected_output.is_empty() || !result.stdout.trim().is_empty() {
            let actual_output = self.harness.normalize_output(&result.stdout);
            let diff_result = if self.config.fp_tolerance.is_some() {
                self.diff_reporter
                    .compare_with_tolerance(&test_case.expected_output, &actual_output)
            } else {
                self.diff_reporter.compare(&test_case.expected_output, &actual_output)
            };

            if diff_result.has_differences {
                if success {
                    self.print_failure_header(test_name, test_case);
                }
                success = false;
                println!("{}", "FAILURE in output:".red());
                println!("{}", diff_result.diff_output);
            }
        }

        // Compare stderr
        if !test_case.expected_error.is_empty() || !result.stderr.trim().is_empty() {
            let actual_error = self.harness.normalize_output(&result.stderr);
            let diff_result = self.diff_reporter.compare(&test_case.expected_error, &actual_error);

            if diff_result.has_differences {
                if success {
                    self.print_failure_header(test_name, test_case);
                }
                success = false;
                println!("{}", "FAILURE in error output:".red());
                println!("{}", diff_result.diff_output);
            }
        }

        if success {
            debug!("Test passed: {}", test_name);
        }

        Ok(success)
    }

    /// Print failure header with test information
    fn print_failure_header(&self, test_name: &str, test_case: &TestCase) {
        println!("{}", format!("FAILURE in test: {}", test_name).red().bold());
        println!("--");
        println!("Command: {}", test_case.command);
        println!("--");
    }

    /// Find all regression test files in directory
    fn find_test_files<P: AsRef<Path>>(&self, test_dir: P) -> Result<Vec<PathBuf>> {
        let test_dir = test_dir.as_ref();
        let mut test_files = Vec::new();

        for entry in fs::read_dir(test_dir)? {
            let entry = entry?;
            let path = entry.path();

            if path.is_file() && path.extension().is_some_and(|ext| ext == "test") {
                // Apply test filter if configured
                if let Some(filter) = &self.config.test_filter {
                    let filename = path.file_name().unwrap().to_string_lossy();
                    if !filename.contains(filter) {
                        continue;
                    }
                }

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
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_regression_runner_creation() {
        let temp_dir = TempDir::new().unwrap();
        let ledger_path = temp_dir.path().join("ledger");
        std::fs::write(&ledger_path, "#!/bin/bash\necho 'test'").unwrap();

        let harness = TestHarness::new(&ledger_path, &temp_dir.path().to_path_buf()).unwrap();
        let runner = RegressionRunner::new(&harness);

        assert_eq!(runner.config.parallel_jobs, num_cpus::get());
    }

    #[test]
    fn test_find_test_files() {
        let temp_dir = TempDir::new().unwrap();

        // Create test files
        let test1 = temp_dir.path().join("test1.test");
        let test2 = temp_dir.path().join("test2.test");
        let not_test = temp_dir.path().join("readme.txt");

        std::fs::write(&test1, "test content").unwrap();
        std::fs::write(&test2, "test content").unwrap();
        std::fs::write(&not_test, "readme").unwrap();

        let ledger_path = temp_dir.path().join("ledger");
        std::fs::write(&ledger_path, "#!/bin/bash\necho 'test'").unwrap();

        let harness = TestHarness::new(&ledger_path, &temp_dir.path().to_path_buf()).unwrap();
        let runner = RegressionRunner::new(&harness);

        let test_files = runner.find_test_files(temp_dir.path()).unwrap();
        assert_eq!(test_files.len(), 2);
        assert!(test_files.contains(&test1));
        assert!(test_files.contains(&test2));
        assert!(!test_files.iter().any(|p| p == &not_test));
    }
}
