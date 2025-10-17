//! Test File Parser
//!
//! This module provides parsing functionality for different test file formats,
//! including regression test files and baseline test files.

use anyhow::{Context, Result};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

/// Represents a single test case within a test file
#[derive(Debug, Clone)]
pub struct TestCase {
    pub command: String,
    pub expected_output: Vec<String>,
    pub expected_error: Vec<String>,
    pub expected_exit_code: i32,
    pub line_number: usize,
}

impl Default for TestCase {
    fn default() -> Self {
        Self::new()
    }
}

impl TestCase {
    pub fn new() -> Self {
        Self {
            command: String::new(),
            expected_output: Vec::new(),
            expected_error: Vec::new(),
            expected_exit_code: 0,
            line_number: 0,
        }
    }
}

/// Test file parser for regression tests
pub struct RegressionTestParser {
    file_path: std::path::PathBuf,
}

impl RegressionTestParser {
    pub fn new<P: AsRef<Path>>(file_path: P) -> Self {
        Self { file_path: file_path.as_ref().to_path_buf() }
    }

    /// Parse a regression test file and return all test cases
    pub fn parse(&self) -> Result<Vec<TestCase>> {
        let file = File::open(&self.file_path)
            .with_context(|| format!("Failed to open test file: {}", self.file_path.display()))?;

        let reader = BufReader::new(file);
        let mut test_cases = Vec::new();
        let mut current_test: Option<TestCase> = None;
        let mut in_output = false;
        let mut in_error = false;
        let mut line_number = 0;

        for line_result in reader.lines() {
            line_number += 1;
            let line = line_result.with_context(|| {
                format!("Failed to read line {} from {}", line_number, self.file_path.display())
            })?;

            // Skip empty lines and comments at the top level
            if !in_output && (line.trim().is_empty() || line.starts_with('#')) {
                continue;
            }

            if let Some(command_part) = line.strip_prefix("test ") {
                // Start of a new test case
                if let Some(test) = current_test.take() {
                    test_cases.push(test);
                }

                current_test = Some(TestCase::new());
                in_output = true;
                in_error = false;

                // Parse command and expected exit code
                // Remove "test "
                if let Some(arrow_pos) = command_part.find(" -> ") {
                    let (command, exit_code_str) = command_part.split_at(arrow_pos);
                    let exit_code_str = &exit_code_str[4..]; // Remove " -> "

                    if let Some(test) = &mut current_test {
                        test.command = self.transform_line(command);
                        test.expected_exit_code = exit_code_str.trim().parse().unwrap_or(0);
                        test.line_number = line_number;
                    }
                } else if let Some(test) = &mut current_test {
                    test.command = self.transform_line(command_part);
                    test.line_number = line_number;
                }
            } else if line.starts_with("end test") {
                // End of current test case
                if let Some(test) = current_test.take() {
                    test_cases.push(test);
                }
                in_output = false;
                in_error = false;
            } else if in_output {
                if line.starts_with("__ERROR__") {
                    in_error = true;
                } else if in_error {
                    if let Some(test) = &mut current_test {
                        test.expected_error.push(self.transform_line(&line));
                    }
                } else if let Some(test) = &mut current_test {
                    test.expected_output.push(self.transform_line(&line));
                }
            }
        }

        // Don't forget the last test case if file doesn't end with "end test"
        if let Some(test) = current_test {
            test_cases.push(test);
        }

        Ok(test_cases)
    }

    /// Transform line by replacing variables
    fn transform_line(&self, line: &str) -> String {
        line.replace("$FILE", &self.file_path.display().to_string())
            .replace("$sourcepath", "${SOURCEPATH}") // Will be replaced by test harness
    }
}

/// Test file parser for manual tests
pub struct ManualTestParser {
    file_path: std::path::PathBuf,
}

impl ManualTestParser {
    pub fn new<P: AsRef<Path>>(file_path: P) -> Self {
        Self { file_path: file_path.as_ref().to_path_buf() }
    }

    /// Parse a manual test file and return test data and test case
    /// Manual tests can contain embedded ledger data followed by test commands
    pub fn parse(&self) -> Result<(String, TestCase)> {
        let file = File::open(&self.file_path).with_context(|| {
            format!("Failed to open manual test file: {}", self.file_path.display())
        })?;

        let reader = BufReader::new(file);
        let mut test_data = String::new();
        let mut test_case = TestCase::new();
        let mut in_test_section = false;
        let mut in_output_section = false;
        let mut in_error_section = false;
        let mut line_number = 0;

        for line_result in reader.lines() {
            line_number += 1;
            let line = line_result.with_context(|| {
                format!("Failed to read line {} from {}", line_number, self.file_path.display())
            })?;

            if let Some(command_part) = line.strip_prefix("test ") {
                // Start of test section
                in_test_section = true;
                in_output_section = true;
                in_error_section = false;

                // Remove "test "
                if let Some(arrow_pos) = command_part.find(" -> ") {
                    let (command, exit_code_str) = command_part.split_at(arrow_pos);
                    let exit_code_str = &exit_code_str[4..]; // Remove " -> "
                    test_case.command = self.transform_line(command);
                    test_case.expected_exit_code = exit_code_str.trim().parse().unwrap_or(0);
                } else {
                    test_case.command = self.transform_line(command_part);
                }
                test_case.line_number = line_number;
            } else if line.starts_with("end test") {
                // End of test section
                break;
            } else if in_test_section {
                if line.starts_with("__ERROR__") {
                    in_error_section = true;
                    in_output_section = false;
                } else if in_error_section {
                    test_case.expected_error.push(self.transform_line(&line));
                } else if in_output_section {
                    test_case.expected_output.push(self.transform_line(&line));
                }
            } else if !in_test_section {
                // Everything before test section is considered test data
                test_data.push_str(&line);
                test_data.push('\n');
            }
        }

        // Trim trailing newline from test data
        if test_data.ends_with('\n') {
            test_data.pop();
        }

        Ok((test_data, test_case))
    }

    /// Transform line by replacing variables
    fn transform_line(&self, line: &str) -> String {
        line.replace("$FILE", &self.file_path.display().to_string())
            .replace("$sourcepath", "${SOURCEPATH}")
    }
}

/// Test file parser for baseline tests  
pub struct BaselineTestParser {
    file_path: std::path::PathBuf,
}

impl BaselineTestParser {
    pub fn new<P: AsRef<Path>>(file_path: P) -> Self {
        Self { file_path: file_path.as_ref().to_path_buf() }
    }

    /// Parse a baseline test file and return test case
    /// Baseline tests typically have a simpler format than regression tests
    pub fn parse(&self) -> Result<TestCase> {
        let file = File::open(&self.file_path).with_context(|| {
            format!("Failed to open baseline test file: {}", self.file_path.display())
        })?;

        let reader = BufReader::new(file);
        let mut test_case = TestCase::new();
        let mut in_command = false;
        let mut in_output = false;
        let mut line_number = 0;

        // For baseline tests, we need to determine the format
        // Some might be simple command files, others might have expected output

        for line_result in reader.lines() {
            line_number += 1;
            let line = line_result.with_context(|| {
                format!("Failed to read line {} from {}", line_number, self.file_path.display())
            })?;

            if line_number == 1 {
                test_case.line_number = line_number;
            }

            // Skip comments and empty lines
            if line.trim().is_empty() || line.starts_with('#') {
                continue;
            }

            // Look for specific baseline test patterns
            if let Some(command_part) = line.strip_prefix("test ") {
                // Regression-style test in baseline directory
                in_command = true;
                if let Some(arrow_pos) = command_part.find(" -> ") {
                    let (command, exit_code_str) = command_part.split_at(arrow_pos);
                    let exit_code_str = &exit_code_str[4..];
                    test_case.command = self.transform_line(command);
                    test_case.expected_exit_code = exit_code_str.trim().parse().unwrap_or(0);
                } else {
                    test_case.command = self.transform_line(command_part);
                }
            } else if line.starts_with("end test") {
                break;
            } else if in_command {
                if line.starts_with("__ERROR__") {
                    in_output = false; // Switch to error mode (not implemented in simple version)
                } else if !line.starts_with("__") {
                    if in_output {
                        test_case.expected_output.push(self.transform_line(&line));
                    } else {
                        // First non-command line starts output section
                        in_output = true;
                        test_case.expected_output.push(self.transform_line(&line));
                    }
                }
            } else {
                // For simple baseline tests, treat the file as a command
                // The file name often indicates the command (e.g., cmd-balance.test)
                if test_case.command.is_empty() {
                    test_case.command = self.infer_command_from_filename(&line);
                }
            }
        }

        // If no command was found, infer from filename
        if test_case.command.is_empty() {
            test_case.command = self.infer_command_from_filename("");
        }

        Ok(test_case)
    }

    /// Infer command from filename for simple baseline tests
    fn infer_command_from_filename(&self, _first_line: &str) -> String {
        let filename = self.file_path.file_stem().and_then(|s| s.to_str()).unwrap_or("balance");

        // Parse filename patterns like "cmd-balance.test" -> "balance"
        if let Some(cmd_part) = filename.strip_prefix("cmd-") {
            cmd_part.to_string()
        } else if let Some(opt_part) = filename.strip_prefix("opt-") {
            format!("balance --{}", opt_part.replace('-', "-"))
        } else {
            "balance".to_string()
        }
    }

    /// Transform line by replacing variables
    fn transform_line(&self, line: &str) -> String {
        line.replace("$FILE", &self.file_path.display().to_string())
            .replace("$sourcepath", "${SOURCEPATH}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::NamedTempFile;

    #[test]
    fn test_regression_parser() {
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "# Test file comment").unwrap();
        writeln!(temp_file, "").unwrap();
        writeln!(temp_file, "test balance -> 0").unwrap();
        writeln!(temp_file, "Assets  $100.00").unwrap();
        writeln!(temp_file, "Liabilities  $-50.00").unwrap();
        writeln!(temp_file, "__ERROR__").unwrap();
        writeln!(temp_file, "No error expected").unwrap();
        writeln!(temp_file, "end test").unwrap();

        let parser = RegressionTestParser::new(temp_file.path());
        let test_cases = parser.parse().unwrap();

        assert_eq!(test_cases.len(), 1);
        assert_eq!(test_cases[0].command, "balance");
        assert_eq!(test_cases[0].expected_exit_code, 0);
        assert_eq!(test_cases[0].expected_output.len(), 2);
        assert_eq!(test_cases[0].expected_error.len(), 1);
    }

    #[test]
    fn test_baseline_parser() {
        let mut temp_file = NamedTempFile::new().unwrap();
        writeln!(temp_file, "test register").unwrap();
        writeln!(temp_file, "2023/01/01 Opening Balance    Assets:Checking         $1000.00")
            .unwrap();
        writeln!(temp_file, "end test").unwrap();

        let parser = BaselineTestParser::new(temp_file.path());
        let test_case = parser.parse().unwrap();

        assert_eq!(test_case.command, "register");
        assert_eq!(test_case.expected_output.len(), 1);
    }
}
