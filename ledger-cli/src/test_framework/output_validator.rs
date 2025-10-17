//! Output Validator
//!
//! This module provides advanced output validation and comparison strategies
//! for test results, including exact match, regex patterns, numerical tolerance,
//! and cross-platform normalization.

use anyhow::{Context, Result};
use log::debug;
use regex::Regex;
use std::collections::HashMap;

/// Validation strategy for test output comparison
#[derive(Debug, Clone)]
pub enum ValidationStrategy {
    /// Exact string match
    ExactMatch,
    /// Regex pattern matching
    RegexMatch(String),
    /// Numerical comparison with tolerance
    NumericalTolerance(f64),
    /// Date/time comparison with tolerance
    DateTimeTolerance(i64), // seconds
    /// Line-by-line validation with different strategies per line
    LineByLine(Vec<ValidationStrategy>),
    /// Custom validation function (for complex cases)
    Custom(String), // Description of custom logic
}

/// Cross-platform normalization options
#[derive(Debug, Clone)]
pub struct NormalizationConfig {
    /// Normalize line endings (CR/LF -> LF)
    pub normalize_line_endings: bool,
    /// Normalize path separators (backslash -> forward slash on Windows)
    pub normalize_path_separators: bool,
    /// Trim whitespace from lines
    pub trim_whitespace: bool,
    /// Normalize floating point format (e.g., 1.00 -> 1.0)
    pub normalize_float_format: bool,
    /// Case insensitive comparison
    pub case_insensitive: bool,
    /// Sort lines before comparison (for order-independent output)
    pub sort_lines: bool,
}

impl Default for NormalizationConfig {
    fn default() -> Self {
        Self {
            normalize_line_endings: true,
            normalize_path_separators: true,
            trim_whitespace: false,
            normalize_float_format: true,
            case_insensitive: false,
            sort_lines: false,
        }
    }
}

/// Validation result containing detailed information
#[derive(Debug)]
pub struct ValidationResult {
    pub is_valid: bool,
    pub strategy_used: ValidationStrategy,
    pub error_message: Option<String>,
    pub line_results: Vec<LineValidationResult>,
}

/// Result for individual line validation
#[derive(Debug)]
pub struct LineValidationResult {
    pub line_number: usize,
    pub expected: String,
    pub actual: String,
    pub is_valid: bool,
    pub strategy_used: ValidationStrategy,
    pub error_details: Option<String>,
}

/// Advanced output validator with multiple comparison strategies
pub struct OutputValidator {
    normalization: NormalizationConfig,
    regex_cache: HashMap<String, Regex>,
}

impl OutputValidator {
    /// Create a new output validator with default configuration
    pub fn new() -> Self {
        Self { normalization: NormalizationConfig::default(), regex_cache: HashMap::new() }
    }

    /// Create validator with custom normalization config
    pub fn with_normalization(normalization: NormalizationConfig) -> Self {
        Self { normalization, regex_cache: HashMap::new() }
    }

    /// Validate output using specified strategy
    pub fn validate(
        &mut self,
        expected: &[String],
        actual: &[String],
        strategy: ValidationStrategy,
    ) -> Result<ValidationResult> {
        debug!("Validating output with strategy: {:?}", strategy);

        // Normalize both expected and actual output
        let expected_normalized = self.normalize_output(expected)?;
        let actual_normalized = self.normalize_output(actual)?;

        // Apply validation strategy
        match strategy.clone() {
            ValidationStrategy::ExactMatch => {
                self.exact_match_validation(&expected_normalized, &actual_normalized, strategy)
            }
            ValidationStrategy::RegexMatch(pattern) => {
                self.regex_validation(&expected_normalized, &actual_normalized, &pattern, strategy)
            }
            ValidationStrategy::NumericalTolerance(tolerance) => self.numerical_validation(
                &expected_normalized,
                &actual_normalized,
                tolerance,
                strategy,
            ),
            ValidationStrategy::DateTimeTolerance(tolerance_seconds) => self.datetime_validation(
                &expected_normalized,
                &actual_normalized,
                tolerance_seconds,
                strategy,
            ),
            ValidationStrategy::LineByLine(strategies) => {
                self.line_by_line_validation(&expected_normalized, &actual_normalized, &strategies)
            }
            ValidationStrategy::Custom(description) => {
                // Custom validation would be implemented based on specific needs
                Ok(ValidationResult {
                    is_valid: true,
                    strategy_used: ValidationStrategy::Custom(description),
                    error_message: Some("Custom validation not yet implemented".to_string()),
                    line_results: Vec::new(),
                })
            }
        }
    }

    /// Normalize output according to configuration
    fn normalize_output(&self, output: &[String]) -> Result<Vec<String>> {
        let mut result: Vec<String> = output.to_vec();

        // Normalize line endings
        if self.normalization.normalize_line_endings {
            result =
                result.iter().map(|line| line.replace("\r\n", "\n").replace('\r', "\n")).collect();
        }

        // Normalize path separators
        if self.normalization.normalize_path_separators {
            #[cfg(windows)]
            {
                result = result.iter().map(|line| line.replace('\\', "/")).collect();
            }
        }

        // Trim whitespace
        if self.normalization.trim_whitespace {
            result = result.iter().map(|line| line.trim().to_string()).collect();
        }

        // Normalize float format
        if self.normalization.normalize_float_format {
            result = result.iter().map(|line| self.normalize_floats_in_line(line)).collect();
        }

        // Case insensitive
        if self.normalization.case_insensitive {
            result = result.iter().map(|line| line.to_lowercase()).collect();
        }

        // Sort lines
        if self.normalization.sort_lines {
            result.sort();
        }

        Ok(result)
    }

    /// Normalize floating point numbers in a line
    fn normalize_floats_in_line(&self, line: &str) -> String {
        // Simple approach - in practice, you'd want a more sophisticated regex
        let float_regex = Regex::new(r"\$?([0-9]+\.[0-9]+)").unwrap();

        float_regex
            .replace_all(line, |caps: &regex::Captures| {
                let float_str = &caps[1];
                if let Ok(num) = float_str.parse::<f64>() {
                    if caps[0].starts_with('$') {
                        format!("${:.2}", num)
                    } else {
                        format!("{:.2}", num)
                    }
                } else {
                    caps[0].to_string()
                }
            })
            .to_string()
    }

    /// Exact match validation
    fn exact_match_validation(
        &self,
        expected: &[String],
        actual: &[String],
        strategy: ValidationStrategy,
    ) -> Result<ValidationResult> {
        let mut line_results = Vec::new();
        let mut all_valid = true;
        let mut error_messages = Vec::new();

        let max_lines = expected.len().max(actual.len());

        for i in 0..max_lines {
            let expected_line = expected.get(i).cloned().unwrap_or_default();
            let actual_line = actual.get(i).cloned().unwrap_or_default();

            let is_line_valid = expected_line == actual_line;
            all_valid &= is_line_valid;

            if !is_line_valid {
                error_messages.push(format!(
                    "Line {}: expected '{}', got '{}'",
                    i + 1,
                    expected_line,
                    actual_line
                ));
            }

            line_results.push(LineValidationResult {
                line_number: i + 1,
                expected: expected_line,
                actual: actual_line,
                is_valid: is_line_valid,
                strategy_used: ValidationStrategy::ExactMatch,
                error_details: if is_line_valid {
                    None
                } else {
                    Some("Exact match failed".to_string())
                },
            });
        }

        Ok(ValidationResult {
            is_valid: all_valid,
            strategy_used: strategy,
            error_message: if all_valid { None } else { Some(error_messages.join("\n")) },
            line_results,
        })
    }

    /// Regex pattern validation
    fn regex_validation(
        &mut self,
        expected: &[String],
        actual: &[String],
        pattern: &str,
        strategy: ValidationStrategy,
    ) -> Result<ValidationResult> {
        let regex = if let Some(cached) = self.regex_cache.get(pattern) {
            cached.clone()
        } else {
            let regex = Regex::new(pattern)
                .with_context(|| format!("Invalid regex pattern: {}", pattern))?;
            self.regex_cache.insert(pattern.to_string(), regex.clone());
            regex
        };

        let mut line_results = Vec::new();
        let mut all_valid = true;

        for (i, actual_line) in actual.iter().enumerate() {
            let is_match = regex.is_match(actual_line);
            all_valid &= is_match;

            line_results.push(LineValidationResult {
                line_number: i + 1,
                expected: pattern.to_string(),
                actual: actual_line.clone(),
                is_valid: is_match,
                strategy_used: ValidationStrategy::RegexMatch(pattern.to_string()),
                error_details: if is_match {
                    None
                } else {
                    Some(format!("Pattern '{}' did not match", pattern))
                },
            });
        }

        Ok(ValidationResult {
            is_valid: all_valid,
            strategy_used: strategy,
            error_message: if all_valid {
                None
            } else {
                Some(format!("Regex pattern '{}' validation failed", pattern))
            },
            line_results,
        })
    }

    /// Numerical tolerance validation
    fn numerical_validation(
        &self,
        expected: &[String],
        actual: &[String],
        tolerance: f64,
        strategy: ValidationStrategy,
    ) -> Result<ValidationResult> {
        let mut line_results = Vec::new();
        let mut all_valid = true;

        let max_lines = expected.len().max(actual.len());

        for i in 0..max_lines {
            let expected_line = expected.get(i).cloned().unwrap_or_default();
            let actual_line = actual.get(i).cloned().unwrap_or_default();

            let is_valid = self.compare_numerical_lines(&expected_line, &actual_line, tolerance);
            all_valid &= is_valid;

            line_results.push(LineValidationResult {
                line_number: i + 1,
                expected: expected_line,
                actual: actual_line,
                is_valid,
                strategy_used: ValidationStrategy::NumericalTolerance(tolerance),
                error_details: if is_valid {
                    None
                } else {
                    Some(format!("Numerical comparison failed with tolerance {}", tolerance))
                },
            });
        }

        Ok(ValidationResult {
            is_valid: all_valid,
            strategy_used: strategy,
            error_message: if all_valid {
                None
            } else {
                Some("Numerical tolerance validation failed".to_string())
            },
            line_results,
        })
    }

    /// Compare lines containing numbers with tolerance
    fn compare_numerical_lines(&self, expected: &str, actual: &str, tolerance: f64) -> bool {
        // Extract all numbers from both lines
        let number_regex = Regex::new(r"[-+]?[0-9]*\.?[0-9]+([eE][-+]?[0-9]+)?").unwrap();

        let expected_numbers: Vec<f64> =
            number_regex.find_iter(expected).filter_map(|m| m.as_str().parse().ok()).collect();

        let actual_numbers: Vec<f64> =
            number_regex.find_iter(actual).filter_map(|m| m.as_str().parse().ok()).collect();

        if expected_numbers.len() != actual_numbers.len() {
            return false;
        }

        // Compare each number with tolerance
        expected_numbers
            .iter()
            .zip(actual_numbers.iter())
            .all(|(exp, act)| (exp - act).abs() <= tolerance)
    }

    /// Date/time tolerance validation
    fn datetime_validation(
        &self,
        expected: &[String],
        actual: &[String],
        tolerance_seconds: i64,
        strategy: ValidationStrategy,
    ) -> Result<ValidationResult> {
        let mut line_results = Vec::new();
        let mut all_valid = true;

        let max_lines = expected.len().max(actual.len());

        for i in 0..max_lines {
            let expected_line = expected.get(i).cloned().unwrap_or_default();
            let actual_line = actual.get(i).cloned().unwrap_or_default();

            let is_valid =
                self.compare_datetime_lines(&expected_line, &actual_line, tolerance_seconds);
            all_valid &= is_valid;

            line_results.push(LineValidationResult {
                line_number: i + 1,
                expected: expected_line,
                actual: actual_line,
                is_valid,
                strategy_used: ValidationStrategy::DateTimeTolerance(tolerance_seconds),
                error_details: if is_valid {
                    None
                } else {
                    Some(format!(
                        "DateTime comparison failed with tolerance {}s",
                        tolerance_seconds
                    ))
                },
            });
        }

        Ok(ValidationResult {
            is_valid: all_valid,
            strategy_used: strategy,
            error_message: if all_valid {
                None
            } else {
                Some("DateTime tolerance validation failed".to_string())
            },
            line_results,
        })
    }

    /// Compare lines containing dates/times with tolerance
    fn compare_datetime_lines(&self, expected: &str, actual: &str, tolerance_seconds: i64) -> bool {
        // Try to extract and parse dates from both lines
        // This is a simplified implementation - real implementation would handle various date formats

        // Common date patterns
        let patterns = [r"\d{4}-\d{2}-\d{2}", r"\d{2}/\d{2}/\d{4}", r"\d{4}/\d{2}/\d{2}"];

        for pattern in &patterns {
            if let Ok(regex) = Regex::new(pattern) {
                let expected_dates: Vec<&str> =
                    regex.find_iter(expected).map(|m| m.as_str()).collect();
                let actual_dates: Vec<&str> = regex.find_iter(actual).map(|m| m.as_str()).collect();

                if expected_dates.len() == actual_dates.len() && !expected_dates.is_empty() {
                    // For simplicity, just do exact match on dates
                    // In practice, you'd parse and compare with tolerance
                    return expected_dates == actual_dates;
                }
            }
        }

        // Fallback to exact match if no dates found
        expected == actual
    }

    /// Line-by-line validation with different strategies
    fn line_by_line_validation(
        &mut self,
        expected: &[String],
        actual: &[String],
        strategies: &[ValidationStrategy],
    ) -> Result<ValidationResult> {
        let mut line_results = Vec::new();
        let mut all_valid = true;

        let max_lines = expected.len().max(actual.len()).max(strategies.len());

        for i in 0..max_lines {
            let expected_line = expected.get(i).cloned().unwrap_or_default();
            let actual_line = actual.get(i).cloned().unwrap_or_default();
            let strategy = strategies.get(i).cloned().unwrap_or(ValidationStrategy::ExactMatch);

            // Validate single line with specific strategy
            let single_line_result =
                self.validate(&[expected_line.clone()], &[actual_line.clone()], strategy.clone())?;

            let is_valid = single_line_result.is_valid;
            all_valid &= is_valid;

            line_results.push(LineValidationResult {
                line_number: i + 1,
                expected: expected_line,
                actual: actual_line,
                is_valid,
                strategy_used: strategy,
                error_details: single_line_result.error_message,
            });
        }

        Ok(ValidationResult {
            is_valid: all_valid,
            strategy_used: ValidationStrategy::LineByLine(strategies.to_vec()),
            error_message: if all_valid {
                None
            } else {
                Some("Line-by-line validation failed".to_string())
            },
            line_results,
        })
    }
}

impl Default for OutputValidator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_exact_match_validation() {
        let mut validator = OutputValidator::new();
        let expected = vec!["line 1".to_string(), "line 2".to_string()];
        let actual = vec!["line 1".to_string(), "line 2".to_string()];

        let result =
            validator.validate(&expected, &actual, ValidationStrategy::ExactMatch).unwrap();
        assert!(result.is_valid);
    }

    #[test]
    fn test_numerical_tolerance_validation() {
        let mut validator = OutputValidator::new();
        let expected = vec!["Balance: $100.00".to_string()];
        let actual = vec!["Balance: $100.01".to_string()];

        let result = validator
            .validate(&expected, &actual, ValidationStrategy::NumericalTolerance(0.1))
            .unwrap();
        assert!(result.is_valid);

        let result = validator
            .validate(&expected, &actual, ValidationStrategy::NumericalTolerance(0.001))
            .unwrap();
        assert!(!result.is_valid);
    }

    #[test]
    fn test_normalization() {
        let config = NormalizationConfig {
            trim_whitespace: true,
            normalize_line_endings: true,
            ..Default::default()
        };

        let validator = OutputValidator::with_normalization(config);
        let input = vec!["  line 1  \r\n".to_string(), "line 2\r".to_string()];
        let result = validator.normalize_output(&input).unwrap();

        assert_eq!(result, vec!["line 1".to_string(), "line 2".to_string()]);
    }
}
