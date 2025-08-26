//! Baseline test runner implementation
//!
//! This module implements the baseline test runner specifically for running
//! the core functional tests from test/baseline/. These tests validate the
//! fundamental ledger functionality.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::time::Instant;
use tokio::fs;
use walkdir::WalkDir;

use crate::config::{TestConfig, TestCategory};
use crate::discovery::{TestDiscovery, TestSuite, TestCase};
use crate::execution::{TestExecutor, TestResult, TestStatus};
use crate::reporting::TestReport;
use crate::TestError;

/// Specialized baseline test runner
pub struct BaselineRunner {
    config: TestConfig,
    discovery: TestDiscovery,
    executor: TestExecutor,
}

impl BaselineRunner {
    /// Create a new baseline test runner
    pub fn new(config: TestConfig) -> Result<Self, TestError> {
        // Ensure we're configured for baseline tests
        let mut baseline_config = config;
        baseline_config.categories = vec![TestCategory::Baseline];
        
        let discovery = TestDiscovery::new()?;
        let executor = TestExecutor::new(baseline_config.clone());
        
        Ok(Self {
            config: baseline_config,
            discovery,
            executor,
        })
    }
    
    /// Run all baseline tests
    pub async fn run_baseline_tests(&self) -> Result<TestReport, TestError> {
        let start_time = Instant::now();
        
        // Discover baseline test suites
        let baseline_dir = self.config.source_path.join("test/baseline");
        if !baseline_dir.exists() {
            return Err(TestError::Discovery(
                format!("Baseline test directory not found: {}", baseline_dir.display())
            ));
        }
        
        let test_suites = self.discover_baseline_tests(&baseline_dir).await?;
        
        if test_suites.is_empty() {
            return Err(TestError::Discovery("No baseline tests found".to_string()));
        }
        
        println!("Running {} baseline test suites...", test_suites.len());
        
        // Execute tests
        let results = self.execute_baseline_tests(&test_suites).await?;
        
        let duration = start_time.elapsed();
        let statistics = crate::reporting::TestStatistics::from_results(&results);
        let report = TestReport::new(results, statistics, duration);
        
        Ok(report)
    }
    
    /// Discover baseline test files
    async fn discover_baseline_tests(&self, baseline_dir: &Path) -> Result<Vec<TestSuite>, TestError> {
        let mut test_suites = Vec::new();
        
        // Walk the baseline directory
        for entry in WalkDir::new(baseline_dir).into_iter().filter_map(|e| e.ok()) {
            let path = entry.path();
            
            // Only process .test files
            if !path.is_file() || path.extension().and_then(|s| s.to_str()) != Some("test") {
                continue;
            }
            
            // Apply test pattern filters if specified
            if !self.config.tests.is_empty() {
                let file_name = path.file_name()
                    .and_then(|n| n.to_str())
                    .unwrap_or("");
                    
                let matches = self.config.tests.iter().any(|pattern| {
                    file_name.contains(pattern) || 
                    path.to_string_lossy().contains(pattern)
                });
                
                if !matches {
                    continue;
                }
            }
            
            // Parse the test file
            let test_suite = self.discovery.parser.parse_file(path, TestCategory::Baseline)?;
            
            // Skip empty test files
            if test_suite.is_empty() {
                if self.config.verbose {
                    eprintln!("WARNING: Empty test file detected: {}", path.display());
                }
                continue;
            }
            
            test_suites.push(test_suite);
        }
        
        // Sort test suites by name for consistent ordering
        test_suites.sort_by(|a, b| a.file_path.cmp(&b.file_path));
        
        Ok(test_suites)
    }
    
    /// Execute baseline tests with proper handling
    async fn execute_baseline_tests(&self, test_suites: &[TestSuite]) -> Result<Vec<TestResult>, TestError> {
        let mut all_results = Vec::new();
        let total_tests: usize = test_suites.iter().map(|s| s.test_cases.len()).sum();
        
        println!("Executing {} baseline tests from {} files", total_tests, test_suites.len());
        
        // Progress tracking
        let mut completed = 0;
        
        for suite in test_suites {
            if self.config.verbose {
                println!("Running test suite: {}", suite.name());
            }
            
            // Create variable substitution map for this suite
            let variables = self.create_baseline_variables(suite);
            
            // Execute each test case in the suite
            for test_case in &suite.test_cases {
                let result = self.executor.execute_test_case(test_case, suite, &variables).await;
                
                // Print progress indicator
                if !self.config.quiet {
                    let status_char = match result.status {
                        TestStatus::Passed => ".",
                        TestStatus::Failed => "F",
                        TestStatus::Skipped => "S",
                        TestStatus::Error => "E",
                        TestStatus::Timeout => "T",
                    };
                    print!("{}", status_char);
                    
                    completed += 1;
                    if completed % 50 == 0 {
                        println!(" ({}/{})", completed, total_tests);
                    }
                }
                
                // Handle immediate failure mode
                if !self.config.continue_on_failure && result.status == TestStatus::Failed {
                    all_results.push(result);
                    if !self.config.quiet && completed % 50 != 0 {
                        println!(" ({}/{})", completed, total_tests);
                    }
                    return Ok(all_results);
                }
                
                all_results.push(result);
            }
        }
        
        // Final progress line
        if !self.config.quiet && completed % 50 != 0 {
            println!(" ({}/{})", completed, total_tests);
        }
        
        Ok(all_results)
    }
    
    /// Create variable substitution map for baseline tests
    fn create_baseline_variables(&self, suite: &TestSuite) -> HashMap<String, String> {
        let mut variables = HashMap::new();
        
        // Standard substitutions
        variables.insert("sourcepath".to_string(), 
                        self.config.source_path.to_string_lossy().to_string());
        variables.insert("FILE".to_string(), 
                        suite.file_path.to_string_lossy().to_string());
        
        // Ledger command with baseline-specific arguments
        let mut ledger_cmd = vec![self.config.ledger_path.to_string_lossy().to_string()];
        ledger_cmd.push("--args-only".to_string());
        
        if self.config.verify {
            ledger_cmd.push("--verify".to_string());
        }
        
        // Always set columns for consistent output
        ledger_cmd.push(format!("--columns={}", self.config.columns));
        
        variables.insert("ledger".to_string(), ledger_cmd.join(" "));
        
        variables
    }
    
    /// Run a specific baseline test file
    pub async fn run_test_file(&self, test_file: &Path) -> Result<Vec<TestResult>, TestError> {
        if !test_file.exists() {
            return Err(TestError::Discovery(
                format!("Test file not found: {}", test_file.display())
            ));
        }
        
        let test_suite = self.discovery.parser.parse_file(test_file, TestCategory::Baseline)?;
        
        if test_suite.is_empty() {
            return Err(TestError::Discovery("Empty test file".to_string()));
        }
        
        let variables = self.create_baseline_variables(&test_suite);
        let mut results = Vec::new();
        
        for test_case in &test_suite.test_cases {
            let result = self.executor.execute_test_case(test_case, &test_suite, &variables).await;
            results.push(result);
        }
        
        Ok(results)
    }
    
    /// Get list of all baseline test files
    pub fn list_baseline_tests(&self) -> Result<Vec<PathBuf>, TestError> {
        let baseline_dir = self.config.source_path.join("test/baseline");
        if !baseline_dir.exists() {
            return Err(TestError::Discovery(
                format!("Baseline directory not found: {}", baseline_dir.display())
            ));
        }
        
        let mut test_files = Vec::new();
        
        for entry in WalkDir::new(&baseline_dir).into_iter().filter_map(|e| e.ok()) {
            let path = entry.path();
            
            if path.is_file() && path.extension().and_then(|s| s.to_str()) == Some("test") {
                test_files.push(path.to_path_buf());
            }
        }
        
        test_files.sort();
        Ok(test_files)
    }
    
    /// Validate baseline test coverage
    pub async fn validate_coverage(&self) -> Result<BaselineCoverage, TestError> {
        let test_files = self.list_baseline_tests()?;
        let test_suites = self.discover_baseline_tests(&self.config.source_path.join("test/baseline")).await?;
        
        let mut coverage = BaselineCoverage {
            total_files: test_files.len(),
            valid_files: 0,
            empty_files: Vec::new(),
            total_tests: 0,
            command_coverage: HashMap::new(),
        };
        
        for suite in &test_suites {
            if suite.is_empty() {
                coverage.empty_files.push(suite.file_path.clone());
            } else {
                coverage.valid_files += 1;
                coverage.total_tests += suite.test_cases.len();
                
                // Track command usage for coverage analysis
                for test_case in &suite.test_cases {
                    let command = test_case.command.split_whitespace().next()
                        .unwrap_or("unknown").to_string();
                    *coverage.command_coverage.entry(command).or_insert(0) += 1;
                }
            }
        }
        
        Ok(coverage)
    }
}

/// Baseline test coverage information
#[derive(Debug)]
pub struct BaselineCoverage {
    pub total_files: usize,
    pub valid_files: usize,
    pub empty_files: Vec<PathBuf>,
    pub total_tests: usize,
    pub command_coverage: HashMap<String, usize>,
}

impl BaselineCoverage {
    /// Print coverage report
    pub fn print_report(&self) {
        println!("Baseline Test Coverage Report");
        println!("============================");
        println!("Total test files: {}", self.total_files);
        println!("Valid test files: {}", self.valid_files);
        println!("Empty test files: {}", self.empty_files.len());
        println!("Total test cases: {}", self.total_tests);
        println!();
        
        if !self.empty_files.is_empty() {
            println!("Empty test files:");
            for file in &self.empty_files {
                println!("  - {}", file.display());
            }
            println!();
        }
        
        println!("Command coverage:");
        let mut commands: Vec<_> = self.command_coverage.iter().collect();
        commands.sort_by(|a, b| b.1.cmp(a.1));
        
        for (command, count) in commands {
            println!("  {:20} {}", command, count);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;
    use std::fs;
    
    #[tokio::test]
    async fn test_baseline_runner_creation() {
        let temp_dir = TempDir::new().unwrap();
        let temp_path = temp_dir.path();
        
        // Create minimal test structure
        fs::create_dir_all(temp_path.join("test/baseline")).unwrap();
        fs::write(temp_path.join("ledger"), "#!/bin/sh\necho test").unwrap();
        
        let config = TestConfig::new(
            temp_path.join("ledger"),
            temp_path.to_path_buf(),
        );
        
        let runner = BaselineRunner::new(config);
        assert!(runner.is_ok());
    }
    
    #[test]
    fn test_baseline_coverage() {
        let coverage = BaselineCoverage {
            total_files: 5,
            valid_files: 4,
            empty_files: vec![PathBuf::from("empty.test")],
            total_tests: 20,
            command_coverage: {
                let mut map = HashMap::new();
                map.insert("balance".to_string(), 10);
                map.insert("register".to_string(), 5);
                map.insert("print".to_string(), 3);
                map
            },
        };
        
        // This would normally print to stdout, but we'll just verify it doesn't panic
        coverage.print_report();
    }
}