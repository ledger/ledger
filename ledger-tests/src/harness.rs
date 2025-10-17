//! Main test harness orchestrating the test framework

use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;
use std::time::Instant;

use crate::config::{ExecutionMode, TestCategory, TestConfig};
use crate::discovery::{TestDiscovery, TestSuite};
use crate::execution::{BatchTestExecutor, TestResult, TestStatus};
use crate::reporting::{TestReport, TestStatistics};
use crate::TestError;

/// Main test harness for running Ledger tests
pub struct LedgerHarness {
    config: TestConfig,
    discovery: TestDiscovery,
    executor: BatchTestExecutor,
}

impl LedgerHarness {
    /// Create a new ledger test harness
    pub fn new(config: TestConfig) -> Result<Self, TestError> {
        config.validate()?;

        let discovery = TestDiscovery::new()?;
        let executor = BatchTestExecutor::new(config.clone());

        Ok(Self { config, discovery, executor })
    }

    /// Get the test configuration
    pub fn config(&self) -> &TestConfig {
        &self.config
    }

    /// Run all configured tests
    pub async fn run_all_tests(&self) -> Result<TestReport, TestError> {
        let start_time = Instant::now();

        // Discover test suites
        let test_suites = self.discovery.discover_tests(&self.config)?;

        if test_suites.is_empty() {
            return Err(TestError::Discovery("No test files found".to_string()));
        }

        // Filter out empty test suites
        let valid_suites: Vec<_> =
            test_suites.into_iter().filter(|suite| !suite.is_empty()).collect();

        if valid_suites.is_empty() {
            return Err(TestError::Discovery("No valid test cases found".to_string()));
        }

        println!(
            "Discovered {} test suites with {} total test cases",
            valid_suites.len(),
            valid_suites.iter().map(|s| s.test_cases.len()).sum::<usize>()
        );

        // Execute tests
        let results = self.execute_test_suites(&valid_suites).await?;

        let duration = start_time.elapsed();

        // Generate report
        let statistics = TestStatistics::from_results(&results);
        let report = TestReport::new(results, statistics, duration);

        Ok(report)
    }

    /// Run tests for specific categories
    pub async fn run_category_tests(
        &self,
        categories: &[TestCategory],
    ) -> Result<TestReport, TestError> {
        let mut config = self.config.clone();
        config.categories = categories.to_vec();

        let harness = Self::new(config)?;
        harness.run_all_tests().await
    }

    /// Run specific test files
    pub async fn run_test_files(&self, test_patterns: &[String]) -> Result<TestReport, TestError> {
        let mut config = self.config.clone();
        config.tests = test_patterns.to_vec();

        let harness = Self::new(config)?;
        harness.run_all_tests().await
    }

    /// Execute test suites with progress reporting
    async fn execute_test_suites(
        &self,
        test_suites: &[TestSuite],
    ) -> Result<Vec<TestResult>, TestError> {
        // Create progress bar
        let total_tests: usize = test_suites.iter().map(|s| s.test_cases.len()).sum();
        let progress = if !self.config.quiet {
            Some(indicatif::ProgressBar::new(total_tests as u64))
        } else {
            None
        };

        if let Some(pb) = &progress {
            pb.set_style(indicatif::ProgressStyle::with_template(
                "{spinner:.green} [{elapsed_precise}] [{bar:40.cyan/blue}] {pos}/{len} ({eta})",
            )?);
        }

        // Prepare variable substitution maps
        let substitution_maps: HashMap<String, HashMap<String, String>> = test_suites
            .iter()
            .map(|suite| {
                let variables =
                    self.discovery.create_substitution_map(&self.config, &suite.file_path);
                (suite.name(), variables)
            })
            .collect();

        // Execute tests based on execution mode
        let results = match self.config.execution_mode {
            ExecutionMode::Sequential => {
                self.execute_sequential(test_suites, &substitution_maps, &progress).await?
            }
            ExecutionMode::Parallel => {
                self.execute_parallel(test_suites, &substitution_maps, &progress).await?
            }
        };

        if let Some(pb) = progress {
            pb.finish_with_message("Tests completed");
        }

        Ok(results)
    }

    /// Execute tests sequentially
    async fn execute_sequential(
        &self,
        test_suites: &[TestSuite],
        substitution_maps: &HashMap<String, HashMap<String, String>>,
        progress: &Option<indicatif::ProgressBar>,
    ) -> Result<Vec<TestResult>, TestError> {
        let mut all_results = Vec::new();

        for suite in test_suites {
            let variables = substitution_maps.get(&suite.name()).ok_or_else(|| {
                TestError::Execution("Missing substitution variables".to_string())
            })?;

            let suite_results = self.executor.execute_test_suite(suite, variables).await;

            if let Some(pb) = progress {
                pb.inc(suite_results.len() as u64);
            }

            all_results.extend(suite_results);

            // Check for early termination on failure
            if !self.config.continue_on_failure {
                let has_failures = all_results.iter().any(|r| r.status == TestStatus::Failed);
                if has_failures {
                    break;
                }
            }
        }

        Ok(all_results)
    }

    /// Execute tests in parallel  
    async fn execute_parallel(
        &self,
        test_suites: &[TestSuite],
        substitution_maps: &HashMap<String, HashMap<String, String>>,
        progress: &Option<indicatif::ProgressBar>,
    ) -> Result<Vec<TestResult>, TestError> {
        use futures::future::join_all;

        // Create owned copies for async execution
        let suite_data: Vec<_> = test_suites
            .iter()
            .map(|suite| {
                let variables = substitution_maps.get(&suite.name()).cloned().ok_or_else(|| {
                    TestError::Execution("Missing substitution variables".to_string())
                })?;
                Ok((suite.clone(), variables))
            })
            .collect::<Result<Vec<_>, TestError>>()?;

        // Create futures for each test suite
        let suite_futures: Vec<_> = suite_data
            .into_iter()
            .map(|(suite, variables)| {
                let executor = self.executor.clone();
                async move { executor.execute_test_suite(&suite, &variables).await }
            })
            .collect();

        // Execute all suites concurrently
        let suite_results = join_all(suite_futures).await;

        // Flatten results and update progress
        let mut all_results = Vec::new();
        for suite_result in suite_results {
            if let Some(pb) = progress {
                pb.inc(suite_result.len() as u64);
            }
            all_results.extend(suite_result);
        }

        Ok(all_results)
    }
}

/// Test runner trait for different test execution strategies
pub trait TestRunner {
    /// Run tests and return results
    fn run_tests(&self) -> Result<TestReport, TestError>;

    /// Get test configuration
    fn config(&self) -> &TestConfig;
}

impl TestRunner for LedgerHarness {
    fn run_tests(&self) -> Result<TestReport, TestError> {
        // Create a new tokio runtime for synchronous interface
        let rt = tokio::runtime::Runtime::new()
            .map_err(|e| TestError::Execution(format!("Failed to create async runtime: {}", e)))?;

        rt.block_on(self.run_all_tests())
    }

    fn config(&self) -> &TestConfig {
        &self.config
    }
}

/// Builder pattern for creating test harnesses
pub struct HarnessBuilder {
    config: TestConfig,
}

impl HarnessBuilder {
    /// Start building a harness with the given ledger and source paths
    pub fn new(ledger_path: impl AsRef<Path>, source_path: impl AsRef<Path>) -> Self {
        let config =
            TestConfig::new(ledger_path.as_ref().to_path_buf(), source_path.as_ref().to_path_buf());

        Self { config }
    }

    /// Set test categories to run
    pub fn categories(mut self, categories: Vec<TestCategory>) -> Self {
        self.config.categories = categories;
        self
    }

    /// Set specific test patterns
    pub fn tests(mut self, tests: Vec<String>) -> Self {
        self.config.tests = tests;
        self
    }

    /// Enable verification mode
    pub fn verify(mut self, verify: bool) -> Self {
        self.config.verify = verify;
        self
    }

    /// Enable memory debugging
    pub fn memory_debug(mut self, enabled: bool) -> Self {
        self.config.memory_debug = enabled;
        self
    }

    /// Set execution mode
    pub fn execution_mode(mut self, mode: ExecutionMode) -> Self {
        self.config.execution_mode = mode;
        self
    }

    /// Set number of parallel jobs
    pub fn jobs(mut self, jobs: usize) -> Self {
        self.config.jobs = jobs;
        self
    }

    /// Set test timeout
    pub fn timeout(mut self, timeout_secs: u64) -> Self {
        self.config.timeout = timeout_secs;
        self
    }

    /// Enable verbose output
    pub fn verbose(mut self, verbose: bool) -> Self {
        self.config.verbose = verbose;
        self
    }

    /// Enable quiet mode
    pub fn quiet(mut self, quiet: bool) -> Self {
        self.config.quiet = quiet;
        self
    }

    /// Build the harness
    pub fn build(self) -> Result<LedgerHarness, TestError> {
        LedgerHarness::new(self.config)
    }
}
