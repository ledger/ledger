//! Manual Test Runner
//!
//! This module implements the manual test runner for running tests from
//! the test/manual directory. Manual tests often require interactive verification
//! or special handling of test data embedded within the test file.

use anyhow::{Context, Result};
use colored::*;
use log::{debug, error, info, warn};
use std::fs;
use std::path::{Path, PathBuf};

use super::diff_reporter::{DiffConfig, DiffFormat, DiffReporter};
use super::test_harness::{ProcessResult, TestHarness};
use super::test_parser::{ManualTestParser, TestCase};

/// Configuration for manual test execution
#[derive(Debug, Clone)]
pub struct ManualConfig {
    /// Whether to run tests interactively (ask user for verification)
    pub interactive: bool,
    /// Whether to use colored output
    pub colored_output: bool,
    /// Floating point tolerance for numeric comparisons
    pub fp_tolerance: Option<f64>,
    /// Maximum test execution timeout in seconds
    pub timeout_seconds: u64,
    /// Test file pattern filter
    pub test_filter: Option<String>,
}

impl Default for ManualConfig {
    fn default() -> Self {
        Self {
            interactive: false,
            colored_output: true,
            fp_tolerance: Some(1e-10),
            timeout_seconds: 30,
            test_filter: None,
        }
    }
}

/// Manual test runner
pub struct ManualRunner<'a> {
    harness: &'a TestHarness,
    config: ManualConfig,
    diff_reporter: DiffReporter,
}

impl<'a> ManualRunner<'a> {
    /// Create a new manual test runner
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
            config: ManualConfig::default(),
            diff_reporter: DiffReporter::with_config(diff_config),
        }
    }

    /// Create manual runner with custom configuration
    pub fn with_config(harness: &'a TestHarness, config: ManualConfig) -> Self {
        let diff_config = DiffConfig {
            use_color: config.colored_output,
            floating_point_tolerance: config.fp_tolerance,
            ..Default::default()
        };

        Self { harness, config, diff_reporter: DiffReporter::with_config(diff_config) }
    }

    /// Run all manual tests in the specified directory
    pub fn run_tests<P: AsRef<Path>>(&self, test_dir: P) -> Result<()> {
        let test_dir = test_dir.as_ref();

        info!("Running manual tests from: {}", test_dir.display());

        // Find all test files
        let test_files = self.find_test_files(test_dir)?;

        if test_files.is_empty() {
            warn!("No manual test files found in: {}", test_dir.display());
            return Ok(());
        }

        info!("Found {} manual test files", test_files.len());

        // Manual tests are run sequentially for better control
        for test_file in &test_files {
            self.run_test_file(test_file)?;
        }

        Ok(())
    }

    /// Run a single manual test file
    pub fn run_test_file<P: AsRef<Path>>(&self, test_file: P) -> Result<bool> {
        let test_file = test_file.as_ref();

        debug!("Running manual test: {}", test_file.display());

        // Check if test file is empty
        if fs::metadata(test_file)?.len() == 0 {
            warn!("Empty test file detected: {}", test_file.display());
            self.harness.failure(&test_file.file_name().unwrap().to_string_lossy());
            return Ok(false);
        }

        // Parse test file - manual tests can contain embedded data and test commands
        let parser = ManualTestParser::new(test_file);
        let (test_data, test_case) = parser.parse().with_context(|| {
            format!("Failed to parse manual test file: {}", test_file.display())
        })?;

        // Run the test case
        self.run_test_case(test_file, &test_data, &test_case)
    }

    /// Run a single manual test case
    fn run_test_case(
        &self,
        test_file: &Path,
        test_data: &str,
        test_case: &TestCase,
    ) -> Result<bool> {
        let test_name = test_file.file_name().unwrap().to_string_lossy();

        info!("Running manual test: {}", test_name);

        // Create temporary file with test data if needed
        let temp_file =
            if !test_data.is_empty() { Some(self.create_temp_test_file(test_data)?) } else { None };

        // Prepare command
        let command = self.prepare_command(test_case, test_file, temp_file.as_deref())?;

        // Execute command
        let result = self.harness.run_command(&command, !command.contains("--columns"))?;

        // Compare results
        let success = if self.config.interactive {
            self.interactive_verification(&test_name, &result)?
        } else {
            self.automatic_verification(&test_name, test_case, &result)?
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

    /// Create temporary file with test data
    fn create_temp_test_file(&self, test_data: &str) -> Result<PathBuf> {
        use std::io::Write;

        let temp_dir = std::env::temp_dir();
        let temp_file = temp_dir.join(format!("ledger_manual_test_{}.dat", std::process::id()));

        let mut file = std::fs::File::create(&temp_file)?;
        file.write_all(test_data.as_bytes())?;
        file.flush()?;

        Ok(temp_file)
    }

    /// Prepare command for manual test execution
    fn prepare_command(
        &self,
        test_case: &TestCase,
        test_file: &Path,
        data_file: Option<&Path>,
    ) -> Result<String> {
        let mut command = test_case.command.clone();

        // Use provided data file or infer from test structure
        if let Some(data_file) = data_file {
            // Replace or add file argument
            if !command.contains("-f ") {
                command = format!("{} -f \"{}\"", command, data_file.display());
            }
        } else if !command.contains("-f ") {
            // Try to find appropriate test data
            let data_file = self.find_test_data_file(test_file)?;
            command = format!("{} -f \"{}\"", command, data_file.display());
        }

        // Add ledger prefix if not present
        if !command.starts_with("$ledger") && !command.contains("ledger") {
            command = format!("$ledger {}", command);
        }

        Ok(command)
    }

    /// Find appropriate test data file for a manual test
    fn find_test_data_file(&self, test_file: &Path) -> Result<PathBuf> {
        let test_dir = test_file.parent().unwrap();
        let input_dir = test_dir.parent().unwrap().join("input");

        // Common test data files
        let data_files = ["sample.dat", "standard.dat", "demo.ledger"];

        for data_file in &data_files {
            let path = input_dir.join(data_file);
            if path.exists() {
                return Ok(path);
            }
        }

        // Default to sample.dat path
        Ok(input_dir.join("sample.dat"))
    }

    /// Interactive verification - ask user to verify results
    fn interactive_verification(&self, test_name: &str, result: &ProcessResult) -> Result<bool> {
        println!("{}", format!("Manual test: {}", test_name).cyan().bold());
        println!("{}", "=".repeat(50));
        println!("Exit code: {}", result.exit_code);
        println!();

        if !result.stdout.is_empty() {
            println!("{}:", "STDOUT".green().bold());
            println!("{}", result.stdout);
        }

        if !result.stderr.is_empty() {
            println!("{}:", "STDERR".red().bold());
            println!("{}", result.stderr);
        }

        println!();
        println!("Does this output look correct? (y/n/s for skip): ");

        use std::io::{self, Write};
        io::stdout().flush()?;

        let mut input = String::new();
        io::stdin().read_line(&mut input)?;

        match input.trim().to_lowercase().as_str() {
            "y" | "yes" => Ok(true),
            "s" | "skip" => {
                self.harness.skipped();
                Ok(true) // Don't count as failure
            }
            _ => Ok(false),
        }
    }

    /// Automatic verification against expected output
    fn automatic_verification(
        &self,
        test_name: &str,
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
                    test_name, result.exit_code, test_case.expected_exit_code
                )
                .red()
            );
        }

        // Compare stdout if expected
        if !test_case.expected_output.is_empty() || !result.stdout.trim().is_empty() {
            let actual_output = self.harness.normalize_output(&result.stdout);
            let diff_result = if self.config.fp_tolerance.is_some() {
                self.diff_reporter
                    .compare_with_tolerance(&test_case.expected_output, &actual_output)
            } else {
                self.diff_reporter.compare(&test_case.expected_output, &actual_output)
            };

            if diff_result.has_differences {
                success = false;
                println!("{}", format!("FAILURE in output for {}: ", test_name).red());
                println!("{}", diff_result.diff_output);
            }
        }

        // Compare stderr if expected
        if !test_case.expected_error.is_empty() || !result.stderr.trim().is_empty() {
            let actual_error = self.harness.normalize_output(&result.stderr);
            let diff_result = self.diff_reporter.compare(&test_case.expected_error, &actual_error);

            if diff_result.has_differences {
                success = false;
                println!("{}", format!("FAILURE in error output for {}: ", test_name).red());
                println!("{}", diff_result.diff_output);
            }
        }

        Ok(success)
    }

    /// Find all manual test files in directory
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

                test_files.push(path);
            }
        }

        // Sort test files for consistent execution order
        test_files.sort();

        Ok(test_files)
    }

    /// Generate manual test report
    pub fn generate_report<P: AsRef<Path>>(&self, test_dir: P) -> Result<()> {
        let test_files = self.find_test_files(test_dir)?;

        println!("{}", "Manual Test Report".cyan().bold());
        println!("{}", "==================".cyan());
        println!();

        // Analyze test categories
        let mut categories = std::collections::HashMap::new();

        for test_file in &test_files {
            let filename = test_file.file_name().unwrap().to_string_lossy();
            let category = filename.split('-').next().unwrap_or("other");
            *categories.entry(category.to_string()).or_insert(0) += 1;
        }

        println!("Test Categories:");
        for (category, count) in categories {
            println!("  {}: {}", category, count);
        }
        println!("  Total manual tests: {}", test_files.len());
        println!();

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_manual_runner_creation() {
        let temp_dir = TempDir::new().unwrap();
        let ledger_path = temp_dir.path().join("ledger");
        std::fs::write(&ledger_path, "#!/bin/bash\necho 'test'").unwrap();

        let harness = TestHarness::new(&ledger_path, &temp_dir.path().to_path_buf()).unwrap();
        let runner = ManualRunner::new(&harness);

        assert!(!runner.config.interactive);
        assert!(runner.config.colored_output);
    }

    #[test]
    fn test_create_temp_test_file() {
        let temp_dir = TempDir::new().unwrap();
        let ledger_path = temp_dir.path().join("ledger");
        std::fs::write(&ledger_path, "#!/bin/bash\necho 'test'").unwrap();

        let harness = TestHarness::new(&ledger_path, &temp_dir.path().to_path_buf()).unwrap();
        let runner = ManualRunner::new(&harness);

        let test_data = "2023-01-01 Test\n  Assets:Bank  $100\n  Income:Test\n";
        let temp_file = runner.create_temp_test_file(test_data).unwrap();

        assert!(temp_file.exists());
        let contents = std::fs::read_to_string(&temp_file).unwrap();
        assert_eq!(contents, test_data);

        // Cleanup
        std::fs::remove_file(&temp_file).ok();
    }
}
