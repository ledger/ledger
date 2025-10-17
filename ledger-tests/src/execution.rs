//! Test execution functionality

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::io::Write;
use std::process::Stdio;
use std::time::{Duration, Instant};
use tokio::io::AsyncWriteExt;
use tokio::process::Command as TokioCommand;
use tokio::time::timeout;

use crate::comparison::{OutputComparator, OutputComparison};
use crate::config::TestConfig;
use crate::discovery::{TestCase, TestSuite};
use crate::TestError;

/// Status of a test execution
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TestStatus {
    /// Test passed successfully
    Passed,
    /// Test failed
    Failed,
    /// Test was skipped
    Skipped,
    /// Test timed out
    Timeout,
    /// Test execution error
    Error,
}

/// Result of executing a single test case
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestResult {
    /// Test case that was executed
    pub test_case: TestCase,
    /// Test suite this belongs to
    pub suite_name: String,
    /// Test execution status
    pub status: TestStatus,
    /// Actual stdout output
    pub actual_output: Vec<String>,
    /// Actual stderr output
    pub actual_error: Vec<String>,
    /// Actual exit code
    pub actual_exit_code: i32,
    /// Execution duration
    pub duration: Duration,
    /// Error message if test failed
    pub error_message: Option<String>,
    /// Output comparison results
    pub output_comparison: Option<OutputComparison>,
    /// Error comparison results
    pub error_comparison: Option<OutputComparison>,
}

impl TestResult {
    /// Create a new successful test result
    pub fn success(
        test_case: TestCase,
        suite_name: String,
        actual_output: Vec<String>,
        actual_error: Vec<String>,
        actual_exit_code: i32,
        duration: Duration,
    ) -> Self {
        Self {
            test_case,
            suite_name,
            status: TestStatus::Passed,
            actual_output,
            actual_error,
            actual_exit_code,
            duration,
            error_message: None,
            output_comparison: None,
            error_comparison: None,
        }
    }

    /// Create a new failed test result
    #[allow(clippy::too_many_arguments)]
    pub fn failure(
        test_case: TestCase,
        suite_name: String,
        actual_output: Vec<String>,
        actual_error: Vec<String>,
        actual_exit_code: i32,
        duration: Duration,
        error_message: String,
        output_comparison: Option<OutputComparison>,
        error_comparison: Option<OutputComparison>,
    ) -> Self {
        Self {
            test_case,
            suite_name,
            status: TestStatus::Failed,
            actual_output,
            actual_error,
            actual_exit_code,
            duration,
            error_message: Some(error_message),
            output_comparison,
            error_comparison,
        }
    }

    /// Create a skipped test result
    pub fn skipped(test_case: TestCase, suite_name: String, reason: String) -> Self {
        Self {
            test_case,
            suite_name,
            status: TestStatus::Skipped,
            actual_output: Vec::new(),
            actual_error: Vec::new(),
            actual_exit_code: 0,
            duration: Duration::ZERO,
            error_message: Some(reason),
            output_comparison: None,
            error_comparison: None,
        }
    }

    /// Create an error test result
    pub fn error(test_case: TestCase, suite_name: String, error_message: String) -> Self {
        Self {
            test_case,
            suite_name,
            status: TestStatus::Error,
            actual_output: Vec::new(),
            actual_error: Vec::new(),
            actual_exit_code: -1,
            duration: Duration::ZERO,
            error_message: Some(error_message),
            output_comparison: None,
            error_comparison: None,
        }
    }

    /// Create a timeout test result
    pub fn timeout(test_case: TestCase, suite_name: String, duration: Duration) -> Self {
        Self {
            test_case,
            suite_name,
            status: TestStatus::Timeout,
            actual_output: Vec::new(),
            actual_error: Vec::new(),
            actual_exit_code: -1,
            duration,
            error_message: Some("Test timed out".to_string()),
            output_comparison: None,
            error_comparison: None,
        }
    }
}

/// Test executor for running individual test cases
#[derive(Clone)]
pub struct TestExecutor {
    config: TestConfig,
    output_comparator: OutputComparator,
}

impl TestExecutor {
    /// Create a new test executor
    pub fn new(config: TestConfig) -> Self {
        Self { config, output_comparator: OutputComparator::new() }
    }

    /// Execute a single test case
    pub async fn execute_test_case(
        &self,
        test_case: &TestCase,
        test_suite: &TestSuite,
        variables: &HashMap<String, String>,
    ) -> TestResult {
        let suite_name = test_suite.name();

        // Check for platform-specific skips
        if self.should_skip_test(test_case, test_suite) {
            return TestResult::skipped(
                test_case.clone(),
                suite_name,
                "Platform-specific skip".to_string(),
            );
        }

        // Prepare the command with variable substitution
        let command = self.prepare_command(test_case, test_suite, variables);

        // Execute the command
        match self.run_command(&command, &test_suite.ledger_data).await {
            Ok((stdout, stderr, exit_code, duration)) => self.evaluate_test_result(
                test_case.clone(),
                suite_name,
                stdout,
                stderr,
                exit_code,
                duration,
            ),
            Err(e) => TestResult::error(
                test_case.clone(),
                suite_name,
                format!("Command execution failed: {}", e),
            ),
        }
    }

    /// Check if a test should be skipped
    fn should_skip_test(&self, test_case: &TestCase, _test_suite: &TestSuite) -> bool {
        // Skip tests with /dev/std* on Windows (matching Python behavior)
        if cfg!(windows) && test_case.command.contains("/dev/std") {
            return true;
        }

        // Skip Python tests if Python mode is not enabled
        if !self.config.python && test_case.command.contains("python") {
            return true;
        }

        false
    }

    /// Prepare command with variable substitution and ledger setup
    fn prepare_command(
        &self,
        test_case: &TestCase,
        _test_suite: &TestSuite,
        variables: &HashMap<String, String>,
    ) -> String {
        let mut command = test_case.command.clone();

        // Apply variable substitutions
        for (var, value) in variables {
            command = command.replace(&format!("${}", var), value);
        }

        // Add ledger binary and file if not already specified
        if !command.contains("-f ") {
            // Need to write ledger data to temp file
            command = format!(
                "{} -f <temp_file> {}",
                variables.get("ledger").unwrap_or(&String::new()),
                command
            );
        } else if command.starts_with("-f ") || !command.contains("$ledger") {
            // Prepend ledger binary
            command = format!("{} {}", variables.get("ledger").unwrap_or(&String::new()), command);
        }

        command
    }

    /// Execute a command with optional stdin data
    async fn run_command(
        &self,
        command_str: &str,
        stdin_data: &str,
    ) -> Result<(Vec<String>, Vec<String>, i32, Duration), TestError> {
        let start_time = Instant::now();

        // Create temporary file for ledger data if needed
        let temp_file = if command_str.contains("<temp_file>") {
            let mut temp = tempfile::NamedTempFile::new()
                .map_err(|e| TestError::Execution(format!("Failed to create temp file: {}", e)))?;
            temp.as_file_mut()
                .write_all(stdin_data.as_bytes())
                .map_err(|e| TestError::Execution(format!("Failed to write temp file: {}", e)))?;
            Some(temp)
        } else {
            None
        };

        // Replace temp file placeholder
        let command_str = if let Some(ref temp) = temp_file {
            command_str.replace("<temp_file>", &temp.path().to_string_lossy())
        } else {
            command_str.to_string()
        };

        // Parse command into parts
        let mut parts = shlex::split(&command_str)
            .ok_or_else(|| TestError::Execution("Failed to parse command".to_string()))?;

        if parts.is_empty() {
            return Err(TestError::Execution("Empty command".to_string()));
        }

        let program = parts.remove(0);
        let args = parts;

        // Set up the command
        let mut cmd = TokioCommand::new(&program);
        cmd.args(&args).stdin(Stdio::piped()).stdout(Stdio::piped()).stderr(Stdio::piped());

        // Add environment variables for memory debugging
        for (key, value) in self.config.memory_env_vars() {
            cmd.env(key, value);
        }

        // Handle Windows MSYS2 environment
        if cfg!(windows) {
            if let Ok(msystem) = std::env::var("MSYSTEM") {
                if !msystem.is_empty() {
                    // Use bash to execute on MSYS2
                    if let Ok(mingw_prefix) = std::env::var("MINGW_PREFIX") {
                        let bash_path = format!("{}/../usr/bin/bash.exe", mingw_prefix);
                        cmd = TokioCommand::new(bash_path);
                        cmd.args(["-c", &command_str]);
                    }
                }
            }
        }

        // Execute with timeout
        let child_future = cmd
            .spawn()
            .map_err(|e| TestError::Execution(format!("Failed to spawn command: {}", e)))?;

        let timeout_duration = Duration::from_secs(self.config.timeout);

        let result = timeout(timeout_duration, async {
            let mut child = child_future;

            // Write stdin data if needed and no temp file
            if temp_file.is_none() && !stdin_data.is_empty() {
                if let Some(mut stdin) = child.stdin.take() {
                    let _ = stdin.write_all(stdin_data.as_bytes()).await;
                    let _ = stdin.shutdown().await;
                }
            }

            // Wait for completion
            let output = child
                .wait_with_output()
                .await
                .map_err(|e| TestError::Execution(format!("Command execution error: {}", e)))?;

            let duration = start_time.elapsed();

            // Process output
            let stdout_lines =
                String::from_utf8_lossy(&output.stdout).lines().map(|s| s.to_string()).collect();

            let stderr_lines = String::from_utf8_lossy(&output.stderr)
                .lines()
                .filter(|line| !line.starts_with("GuardMalloc")) // Filter out malloc debug messages
                .map(|s| s.to_string())
                .collect();

            let exit_code = output.status.code().unwrap_or(-1);

            Ok((stdout_lines, stderr_lines, exit_code, duration))
        })
        .await;

        match result {
            Ok(result) => result,
            Err(_) => Err(TestError::Execution("Command timed out".to_string())),
        }
    }

    /// Evaluate test results and create TestResult
    fn evaluate_test_result(
        &self,
        test_case: TestCase,
        suite_name: String,
        actual_output: Vec<String>,
        actual_error: Vec<String>,
        actual_exit_code: i32,
        duration: Duration,
    ) -> TestResult {
        // Check exit code
        if actual_exit_code != test_case.expected_exit_code {
            let error_msg = format!(
                "Exit code mismatch: expected {}, got {}",
                test_case.expected_exit_code, actual_exit_code
            );
            return TestResult::failure(
                test_case,
                suite_name,
                actual_output,
                actual_error,
                actual_exit_code,
                duration,
                error_msg,
                None,
                None,
            );
        }

        // Compare output
        let output_comparison =
            self.output_comparator.compare_output(&test_case.expected_output, &actual_output);

        let error_comparison =
            self.output_comparator.compare_output(&test_case.expected_error, &actual_error);

        if output_comparison.has_differences() || error_comparison.has_differences() {
            let mut error_msg = String::new();

            if output_comparison.has_differences() {
                error_msg.push_str("Output differences found");
            }

            if error_comparison.has_differences() {
                if !error_msg.is_empty() {
                    error_msg.push_str("; ");
                }
                error_msg.push_str("Error output differences found");
            }

            return TestResult::failure(
                test_case,
                suite_name,
                actual_output,
                actual_error,
                actual_exit_code,
                duration,
                error_msg,
                output_comparison.has_differences().then_some(output_comparison),
                error_comparison.has_differences().then_some(error_comparison),
            );
        }

        TestResult::success(
            test_case,
            suite_name,
            actual_output,
            actual_error,
            actual_exit_code,
            duration,
        )
    }
}

/// Batch test executor for running multiple tests
#[derive(Clone)]
pub struct BatchTestExecutor {
    executor: TestExecutor,
}

impl BatchTestExecutor {
    /// Create a new batch executor
    pub fn new(config: TestConfig) -> Self {
        Self { executor: TestExecutor::new(config) }
    }

    /// Execute all test cases in a test suite
    pub async fn execute_test_suite(
        &self,
        test_suite: &TestSuite,
        variables: &HashMap<String, String>,
    ) -> Vec<TestResult> {
        let mut results = Vec::new();

        for test_case in &test_suite.test_cases {
            let result = self.executor.execute_test_case(test_case, test_suite, variables).await;
            results.push(result);
        }

        results
    }

    /// Execute multiple test suites in parallel
    pub async fn execute_test_suites(
        &self,
        test_suites: &[TestSuite],
        variables: &HashMap<String, String>,
    ) -> Vec<TestResult> {
        use rayon::prelude::*;

        let rt = tokio::runtime::Handle::current();

        if self.executor.config.execution_mode == crate::config::ExecutionMode::Sequential {
            // Sequential execution
            let mut all_results = Vec::new();
            for suite in test_suites {
                let suite_results = rt.block_on(self.execute_test_suite(suite, variables));
                all_results.extend(suite_results);
            }
            all_results
        } else {
            // Parallel execution using rayon
            test_suites
                .par_iter()
                .map(|suite| rt.block_on(self.execute_test_suite(suite, variables)))
                .flatten()
                .collect()
        }
    }
}
