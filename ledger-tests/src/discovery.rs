//! Test discovery and parsing functionality

use crate::config::{TestCategory, TestConfig};
use crate::TestError;
use regex::Regex;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

/// Represents a single test case within a .test file
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TestCase {
    /// The command to execute (with variable substitutions)
    pub command: String,
    /// Expected stdout output lines
    pub expected_output: Vec<String>,
    /// Expected stderr output lines
    pub expected_error: Vec<String>,
    /// Expected exit code (0 if not specified)
    pub expected_exit_code: i32,
    /// Line number in the test file where this test starts
    pub line_number: usize,
}

/// Represents a test file with associated test cases
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestSuite {
    /// Path to the test file
    pub file_path: PathBuf,
    /// Category of this test suite
    pub category: TestCategory,
    /// Ledger data content (before the first test)
    pub ledger_data: String,
    /// Individual test cases
    pub test_cases: Vec<TestCase>,
    /// File size for empty file detection
    pub file_size: u64,
}

impl TestSuite {
    /// Check if this test suite is empty
    pub fn is_empty(&self) -> bool {
        self.file_size == 0 || self.test_cases.is_empty()
    }

    /// Get the test file name without extension
    pub fn name(&self) -> String {
        self.file_path.file_stem().and_then(|s| s.to_str()).unwrap_or("unknown").to_string()
    }
}

/// Test file parser
pub struct TestParser {
    /// Regex for parsing test commands with exit codes
    command_regex: Regex,
    /// Regex for variable substitution patterns
    _substitution_regex: Regex,
}

impl TestParser {
    /// Create a new test parser
    pub fn new() -> Result<Self, TestError> {
        let command_regex = Regex::new(r"^test\s+(.+?)(?:\s+->\s+(\d+))?$")
            .map_err(|e| TestError::Parse(format!("Invalid command regex: {}", e)))?;

        let _substitution_regex = Regex::new(r"\$(\w+)")
            .map_err(|e| TestError::Parse(format!("Invalid substitution regex: {}", e)))?;

        Ok(Self { command_regex, _substitution_regex })
    }

    /// Parse a .test file into a TestSuite
    pub fn parse_file(
        &self,
        file_path: &Path,
        category: TestCategory,
    ) -> Result<TestSuite, TestError> {
        let content = fs::read_to_string(file_path).map_err(|e| {
            TestError::Discovery(format!("Failed to read {}: {}", file_path.display(), e))
        })?;

        let metadata = fs::metadata(file_path).map_err(|e| {
            TestError::Discovery(format!(
                "Failed to get metadata for {}: {}",
                file_path.display(),
                e
            ))
        })?;

        let file_size = metadata.len();

        if file_size == 0 {
            return Ok(TestSuite {
                file_path: file_path.to_path_buf(),
                category,
                ledger_data: String::new(),
                test_cases: Vec::new(),
                file_size,
            });
        }

        let lines: Vec<&str> = content.lines().collect();
        let mut ledger_data = String::new();
        let mut test_cases = Vec::new();
        let mut i = 0;

        // Parse ledger data (everything before first "test" command)
        while i < lines.len() && !lines[i].trim().starts_with("test ") {
            ledger_data.push_str(lines[i]);
            ledger_data.push('\n');
            i += 1;
        }

        // Parse test cases
        while i < lines.len() {
            if let Some(test_case) = self.parse_test_case(&lines, &mut i)? {
                test_cases.push(test_case);
            } else {
                i += 1;
            }
        }

        Ok(TestSuite {
            file_path: file_path.to_path_buf(),
            category,
            ledger_data,
            test_cases,
            file_size,
        })
    }

    /// Parse a single test case starting at the given line index
    fn parse_test_case(
        &self,
        lines: &[&str],
        index: &mut usize,
    ) -> Result<Option<TestCase>, TestError> {
        if *index >= lines.len() {
            return Ok(None);
        }

        let line = lines[*index].trim();
        if !line.starts_with("test ") {
            return Ok(None);
        }

        let line_number = *index + 1;

        // Parse the test command and optional exit code
        let (command, expected_exit_code) = if let Some(captures) =
            self.command_regex.captures(line)
        {
            let command = captures.get(1).unwrap().as_str().trim().to_string();
            let exit_code = captures.get(2).map(|m| m.as_str().parse().unwrap_or(0)).unwrap_or(0);
            (command, exit_code)
        } else {
            return Err(TestError::Parse(format!(
                "Invalid test command at line {}: {}",
                line_number, line
            )));
        };

        *index += 1;

        let mut expected_output = Vec::new();
        let mut expected_error = Vec::new();
        let mut in_error_section = false;

        // Parse expected output and error
        while *index < lines.len() {
            let line = lines[*index];

            if line.trim() == "end test" {
                *index += 1;
                break;
            }

            if line.trim() == "__ERROR__" {
                in_error_section = true;
                *index += 1;
                continue;
            }

            if in_error_section {
                expected_error.push(line.to_string());
            } else {
                expected_output.push(line.to_string());
            }

            *index += 1;
        }

        Ok(Some(TestCase {
            command,
            expected_output,
            expected_error,
            expected_exit_code,
            line_number,
        }))
    }

    /// Apply variable substitutions to a command
    pub fn substitute_variables(
        &self,
        command: &str,
        variables: &HashMap<String, String>,
    ) -> String {
        let mut result = command.to_string();
        for (var_name, value) in variables {
            let pattern = format!("${}", var_name);
            result = result.replace(&pattern, value);
        }
        result
    }
}

impl Default for TestParser {
    fn default() -> Self {
        Self::new().expect("Failed to create default test parser")
    }
}

/// Test discovery engine
pub struct TestDiscovery {
    pub parser: TestParser,
}

impl TestDiscovery {
    /// Create a new test discovery engine
    pub fn new() -> Result<Self, TestError> {
        Ok(Self { parser: TestParser::new()? })
    }

    /// Discover all test files in the given configuration
    pub fn discover_tests(&self, config: &TestConfig) -> Result<Vec<TestSuite>, TestError> {
        let mut test_suites = Vec::new();

        for category in &config.categories {
            let test_dir = config.source_path.join("test").join(category.directory());

            if !test_dir.exists() {
                continue;
            }

            let suites = self.discover_category_tests(&test_dir, *category, &config.tests)?;
            test_suites.extend(suites);
        }

        Ok(test_suites)
    }

    /// Discover tests for a specific category
    fn discover_category_tests(
        &self,
        test_dir: &Path,
        category: TestCategory,
        test_patterns: &[String],
    ) -> Result<Vec<TestSuite>, TestError> {
        let mut test_suites = Vec::new();

        let entries = walkdir::WalkDir::new(test_dir)
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|e| e.file_type().is_file())
            .filter(|e| {
                e.path()
                    .extension()
                    .and_then(|ext| ext.to_str())
                    .map(|ext| ext == "test")
                    .unwrap_or(false)
            });

        for entry in entries {
            let path = entry.path();

            // Apply test patterns filter if specified
            if !test_patterns.is_empty() {
                let file_name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");

                let matches_pattern = test_patterns.iter().any(|pattern| {
                    file_name.contains(pattern) || path.to_string_lossy().contains(pattern)
                });

                if !matches_pattern {
                    continue;
                }
            }

            // Special handling for Python tests
            if category == TestCategory::Regress || category == TestCategory::Baseline {
                let file_name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");

                // Skip Python tests unless explicitly enabled
                if file_name.ends_with("_py.test") {
                    // This would be handled by Python integration
                    continue;
                }
            }

            let test_suite = self.parser.parse_file(path, category)?;
            test_suites.push(test_suite);
        }

        Ok(test_suites)
    }

    /// Create variable substitution map for test execution
    pub fn create_substitution_map(
        &self,
        config: &TestConfig,
        test_file: &Path,
    ) -> HashMap<String, String> {
        let mut variables = HashMap::new();

        // Standard substitutions matching Python harness
        variables
            .insert("sourcepath".to_string(), config.source_path.to_string_lossy().to_string());
        variables.insert("FILE".to_string(), test_file.to_string_lossy().to_string());
        variables.insert(
            "ledger".to_string(),
            format!(
                "{} --args-only{} --columns={}",
                config.ledger_path.to_string_lossy(),
                if config.verify { " --verify" } else { "" },
                config.columns
            ),
        );

        variables
    }
}

impl Default for TestDiscovery {
    fn default() -> Self {
        Self::new().expect("Failed to create default test discovery")
    }
}
