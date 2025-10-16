//! Test reporting and statistics functionality

use std::collections::HashMap;
use std::fmt;
use std::time::Duration;
use serde::{Deserialize, Serialize};
use console::style;

use crate::config::TestCategory;
use crate::execution::{TestResult, TestStatus};

/// Overall test statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestStatistics {
    /// Total number of tests
    pub total: usize,
    /// Number of passed tests
    pub passed: usize,
    /// Number of failed tests
    pub failed: usize,
    /// Number of skipped tests
    pub skipped: usize,
    /// Number of error tests
    pub errors: usize,
    /// Number of timeout tests
    pub timeouts: usize,
    /// Statistics by category
    pub by_category: HashMap<TestCategory, CategoryStatistics>,
    /// Total execution time
    pub total_duration: Duration,
    /// Average test duration
    pub average_duration: Duration,
}

impl TestStatistics {
    /// Create statistics from test results
    pub fn from_results(results: &[TestResult]) -> Self {
        let mut stats = Self {
            total: results.len(),
            passed: 0,
            failed: 0,
            skipped: 0,
            errors: 0,
            timeouts: 0,
            by_category: HashMap::new(),
            total_duration: Duration::ZERO,
            average_duration: Duration::ZERO,
        };
        
        let mut total_duration = Duration::ZERO;
        
        for result in results {
            // Update overall counts
            match result.status {
                TestStatus::Passed => stats.passed += 1,
                TestStatus::Failed => stats.failed += 1,
                TestStatus::Skipped => stats.skipped += 1,
                TestStatus::Error => stats.errors += 1,
                TestStatus::Timeout => stats.timeouts += 1,
            }
            
            total_duration += result.duration;
            
            // Update category statistics (would need category info in TestResult)
            // For now, we'll skip this until we add category to TestResult
        }
        
        stats.total_duration = total_duration;
        stats.average_duration = if stats.total > 0 {
            total_duration / stats.total as u32
        } else {
            Duration::ZERO
        };
        
        stats
    }
    
    /// Check if all tests passed
    pub fn all_passed(&self) -> bool {
        self.failed == 0 && self.errors == 0 && self.timeouts == 0
    }
    
    /// Get success rate as percentage
    pub fn success_rate(&self) -> f64 {
        if self.total == 0 {
            100.0
        } else {
            (self.passed as f64 / self.total as f64) * 100.0
        }
    }
}

/// Statistics for a specific test category
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CategoryStatistics {
    pub category: TestCategory,
    pub total: usize,
    pub passed: usize,
    pub failed: usize,
    pub skipped: usize,
    pub errors: usize,
    pub timeouts: usize,
}

/// Complete test report
#[derive(Debug, Serialize, Deserialize)]
pub struct TestReport {
    /// All test results
    pub results: Vec<TestResult>,
    /// Overall statistics
    pub statistics: TestStatistics,
    /// Total execution time
    pub duration: Duration,
    /// Timestamp when tests were run
    pub timestamp: chrono::DateTime<chrono::Utc>,
}

impl TestReport {
    /// Create a new test report
    pub fn new(results: Vec<TestResult>, statistics: TestStatistics, duration: Duration) -> Self {
        Self {
            results,
            statistics,
            duration,
            timestamp: chrono::Utc::now(),
        }
    }
    
    /// Get failed test results
    pub fn failed_tests(&self) -> impl Iterator<Item = &TestResult> {
        self.results.iter().filter(|r| r.status == TestStatus::Failed)
    }
    
    /// Get error test results
    pub fn error_tests(&self) -> impl Iterator<Item = &TestResult> {
        self.results.iter().filter(|r| r.status == TestStatus::Error)
    }
    
    /// Get timeout test results
    pub fn timeout_tests(&self) -> impl Iterator<Item = &TestResult> {
        self.results.iter().filter(|r| r.status == TestStatus::Timeout)
    }
    
    /// Print a summary to stdout
    pub fn print_summary(&self) {
        self.print_header();
        self.print_statistics();
        
        if !self.statistics.all_passed() {
            self.print_failures();
        }
        
        self.print_footer();
    }
    
    /// Print detailed report to stdout
    pub fn print_detailed(&self) {
        self.print_header();
        self.print_statistics();
        self.print_all_results();
        self.print_footer();
    }
    
    /// Print just the failures
    pub fn print_failures(&self) {
        let failed_tests: Vec<_> = self.failed_tests().collect();
        let error_tests: Vec<_> = self.error_tests().collect();
        let timeout_tests: Vec<_> = self.timeout_tests().collect();
        
        if !failed_tests.is_empty() {
            println!("\n{}", style("FAILED TESTS:").bold().red());
            for test in failed_tests {
                self.print_test_failure(test);
            }
        }
        
        if !error_tests.is_empty() {
            println!("\n{}", style("ERROR TESTS:").bold().red());
            for test in error_tests {
                self.print_test_error(test);
            }
        }
        
        if !timeout_tests.is_empty() {
            println!("\n{}", style("TIMEOUT TESTS:").bold().red());
            for test in timeout_tests {
                self.print_test_timeout(test);
            }
        }
    }
    
    fn print_header(&self) {
        println!("{}", style("LEDGER TEST REPORT").bold().cyan());
        println!("{}", style(format!("Run at: {}", self.timestamp.format("%Y-%m-%d %H:%M:%S UTC"))).dim());
        println!("{}", style(format!("Duration: {:.2?}", self.duration)).dim());
        println!();
    }
    
    fn print_statistics(&self) {
        let stats = &self.statistics;
        
        println!("{}", style("SUMMARY").bold());
        println!("  Total tests: {}", stats.total);
        println!("  {} {}", 
                style("Passed:").green(), 
                style(stats.passed).bold().green());
        
        if stats.failed > 0 {
            println!("  {} {}", 
                    style("Failed:").red(), 
                    style(stats.failed).bold().red());
        }
        
        if stats.skipped > 0 {
            println!("  {} {}", 
                    style("Skipped:").yellow(), 
                    style(stats.skipped).bold().yellow());
        }
        
        if stats.errors > 0 {
            println!("  {} {}", 
                    style("Errors:").red(), 
                    style(stats.errors).bold().red());
        }
        
        if stats.timeouts > 0 {
            println!("  {} {}", 
                    style("Timeouts:").red(), 
                    style(stats.timeouts).bold().red());
        }
        
        println!("  Success rate: {:.1}%", stats.success_rate());
        println!("  Average duration: {:.2?}", stats.average_duration);
        println!();
    }
    
    fn print_all_results(&self) {
        println!("{}", style("ALL RESULTS").bold());
        
        for result in &self.results {
            let status_icon = match result.status {
                TestStatus::Passed => style("✓").green(),
                TestStatus::Failed => style("✗").red(),
                TestStatus::Skipped => style("~").yellow(),
                TestStatus::Error => style("!").red(),
                TestStatus::Timeout => style("⏱").red(),
            };
            
            println!("  {} {} ({}) [{:.2?}]", 
                    status_icon,
                    result.suite_name,
                    result.test_case.line_number,
                    result.duration);
        }
        
        println!();
    }
    
    fn print_test_failure(&self, test: &TestResult) {
        println!("\n  {} {} (line {})", 
                style("✗").red(),
                test.suite_name, 
                test.test_case.line_number);
        
        println!("    Command: {}", style(&test.test_case.command).dim());
        
        if let Some(error) = &test.error_message {
            println!("    Error: {}", style(error).red());
        }
        
        if let Some(comparison) = &test.output_comparison {
            if !comparison.diff().is_empty() {
                println!("    Output diff:");
                for line in comparison.diff().lines() {
                    if line.starts_with('+') {
                        println!("      {}", style(line).green());
                    } else if line.starts_with('-') {
                        println!("      {}", style(line).red());
                    } else {
                        println!("      {}", style(line).dim());
                    }
                }
            }
        }
        
        if let Some(comparison) = &test.error_comparison {
            if !comparison.diff().is_empty() {
                println!("    Error diff:");
                for line in comparison.diff().lines() {
                    if line.starts_with('+') {
                        println!("      {}", style(line).green());
                    } else if line.starts_with('-') {
                        println!("      {}", style(line).red());
                    } else {
                        println!("      {}", style(line).dim());
                    }
                }
            }
        }
    }
    
    fn print_test_error(&self, test: &TestResult) {
        println!("\n  {} {} (line {})", 
                style("!").red(),
                test.suite_name, 
                test.test_case.line_number);
        
        println!("    Command: {}", style(&test.test_case.command).dim());
        
        if let Some(error) = &test.error_message {
            println!("    Error: {}", style(error).red());
        }
    }
    
    fn print_test_timeout(&self, test: &TestResult) {
        println!("\n  {} {} (line {})", 
                style("⏱").red(),
                test.suite_name, 
                test.test_case.line_number);
        
        println!("    Command: {}", style(&test.test_case.command).dim());
        println!("    Duration: {:.2?}", test.duration);
    }
    
    fn print_footer(&self) {
        let success_rate = self.statistics.success_rate();
        let styled_result = if success_rate == 100.0 {
            style(format!("RESULT: {:.1}% success rate", success_rate)).bold().green()
        } else if success_rate >= 90.0 {
            style(format!("RESULT: {:.1}% success rate", success_rate)).bold().yellow()
        } else {
            style(format!("RESULT: {:.1}% success rate", success_rate)).bold().red()
        };
        
        println!("{}", styled_result);
    }
    
    /// Export report as JSON
    pub fn to_json(&self) -> Result<String, serde_json::Error> {
        serde_json::to_string_pretty(self)
    }
    
    /// Save report to file
    pub fn save_to_file(&self, path: &std::path::Path) -> Result<(), Box<dyn std::error::Error>> {
        let json = self.to_json()?;
        std::fs::write(path, json)?;
        Ok(())
    }
}

impl fmt::Display for TestReport {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Test Results Summary:")?;
        writeln!(f, "  Total: {}", self.statistics.total)?;
        writeln!(f, "  Passed: {}", self.statistics.passed)?;
        writeln!(f, "  Failed: {}", self.statistics.failed)?;
        writeln!(f, "  Skipped: {}", self.statistics.skipped)?;
        writeln!(f, "  Errors: {}", self.statistics.errors)?;
        writeln!(f, "  Timeouts: {}", self.statistics.timeouts)?;
        writeln!(f, "  Success Rate: {:.1}%", self.statistics.success_rate())?;
        writeln!(f, "  Duration: {:.2?}", self.duration)?;
        Ok(())
    }
}

/// Progress reporter for live test execution updates
pub struct ProgressReporter {
    show_progress: bool,
    current_suite: String,
    total_tests: usize,
    completed_tests: usize,
}

impl ProgressReporter {
    /// Create a new progress reporter
    pub fn new(show_progress: bool, total_tests: usize) -> Self {
        Self {
            show_progress,
            current_suite: String::new(),
            total_tests,
            completed_tests: 0,
        }
    }
    
    /// Update current test suite
    pub fn set_current_suite(&mut self, suite_name: String) {
        self.current_suite = suite_name;
    }
    
    /// Report test completion
    pub fn test_completed(&mut self, result: &TestResult) {
        if !self.show_progress {
            return;
        }
        
        self.completed_tests += 1;
        
        let status_char = match result.status {
            TestStatus::Passed => ".",
            TestStatus::Failed => "F",
            TestStatus::Skipped => "S",
            TestStatus::Error => "E",
            TestStatus::Timeout => "T",
        };
        
        print!("{}", status_char);
        
        // Print newline every 50 tests
        if self.completed_tests % 50 == 0 {
            println!(" ({}/{})", self.completed_tests, self.total_tests);
        }
    }
    
    /// Finish progress reporting
    pub fn finish(&self) {
        if self.show_progress && self.completed_tests % 50 != 0 {
            println!(" ({}/{})", self.completed_tests, self.total_tests);
        }
    }
}