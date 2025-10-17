//! Output comparison and diff generation functionality

use serde::{Deserialize, Serialize};
use similar::{ChangeTag, TextDiff};

/// Represents the result of comparing expected vs actual output
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OutputComparison {
    /// Whether there are differences between expected and actual output
    pub has_differences: bool,
    /// Unified diff string (empty if no differences)
    pub diff: String,
    /// Number of added lines
    pub added_lines: usize,
    /// Number of removed lines
    pub removed_lines: usize,
    /// Number of changed lines
    pub changed_lines: usize,
}

impl OutputComparison {
    /// Create a new comparison result with no differences
    pub fn no_differences() -> Self {
        Self {
            has_differences: false,
            diff: String::new(),
            added_lines: 0,
            removed_lines: 0,
            changed_lines: 0,
        }
    }

    /// Create a new comparison result with differences
    pub fn with_differences(
        diff: String,
        added_lines: usize,
        removed_lines: usize,
        changed_lines: usize,
    ) -> Self {
        Self { has_differences: true, diff, added_lines, removed_lines, changed_lines }
    }

    /// Check if this comparison has differences
    pub fn has_differences(&self) -> bool {
        self.has_differences
    }

    /// Get the diff string
    pub fn diff(&self) -> &str {
        &self.diff
    }

    /// Get total number of changed items
    pub fn total_changes(&self) -> usize {
        self.added_lines + self.removed_lines + self.changed_lines
    }
}

/// Output comparison engine
#[derive(Clone)]
pub struct OutputComparator {
    /// Whether to normalize whitespace
    normalize_whitespace: bool,
    /// Whether to ignore case
    ignore_case: bool,
    /// Whether to treat Windows paths specially
    normalize_paths: bool,
}

impl OutputComparator {
    /// Create a new output comparator with default settings
    pub fn new() -> Self {
        Self { normalize_whitespace: false, ignore_case: false, normalize_paths: cfg!(windows) }
    }

    /// Create a comparator with custom settings
    pub fn with_options(
        normalize_whitespace: bool,
        ignore_case: bool,
        normalize_paths: bool,
    ) -> Self {
        Self { normalize_whitespace, ignore_case, normalize_paths }
    }

    /// Compare expected output with actual output
    pub fn compare_output(&self, expected: &[String], actual: &[String]) -> OutputComparison {
        let expected_text = self.prepare_text(expected);
        let actual_text = self.prepare_text(actual);

        if expected_text == actual_text {
            return OutputComparison::no_differences();
        }

        self.generate_diff(&expected_text, &actual_text)
    }

    /// Prepare text for comparison by applying normalization
    fn prepare_text(&self, lines: &[String]) -> String {
        let mut text = lines.join("\n");

        if self.normalize_whitespace {
            text = text.split_whitespace().collect::<Vec<_>>().join(" ");
        }

        if self.ignore_case {
            text = text.to_lowercase();
        }

        if self.normalize_paths {
            text = self.normalize_windows_paths(&text);
        }

        text
    }

    /// Normalize Windows paths to use forward slashes (matching Python behavior)
    fn normalize_windows_paths(&self, text: &str) -> String {
        text.replace('\\', "/")
    }

    /// Generate a unified diff between expected and actual text
    fn generate_diff(&self, expected: &str, actual: &str) -> OutputComparison {
        let diff = TextDiff::from_lines(expected, actual);

        let mut diff_output = String::new();
        let mut added_lines = 0;
        let mut removed_lines = 0;

        // Generate unified diff format
        diff_output.push_str("--- expected\n");
        diff_output.push_str("+++ actual\n");

        for group in diff.grouped_ops(3) {
            if let Some((first, _last)) = group.first().zip(group.last()) {
                diff_output.push_str(&format!(
                    "@@ -{},{} +{},{} @@\n",
                    first.old_range().start + 1,
                    first.old_range().len(),
                    first.new_range().start + 1,
                    first.new_range().len(),
                ));
            }

            for op in group {
                for change in diff.iter_changes(&op) {
                    let prefix = match change.tag() {
                        ChangeTag::Delete => {
                            removed_lines += 1;
                            "-"
                        }
                        ChangeTag::Insert => {
                            added_lines += 1;
                            "+"
                        }
                        ChangeTag::Equal => " ",
                    };

                    diff_output.push_str(&format!("{}{}", prefix, change));
                }
            }
        }

        // Count changed lines as max of added/removed for line-level changes
        let changed_lines = added_lines.min(removed_lines);

        OutputComparison::with_differences(diff_output, added_lines, removed_lines, changed_lines)
    }
}

impl Default for OutputComparator {
    fn default() -> Self {
        Self::new()
    }
}

// Re-export the result type
pub use self::OutputComparison as ComparisonResult;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_no_differences() {
        let comparator = OutputComparator::new();
        let expected = vec!["line1".to_string(), "line2".to_string()];
        let actual = vec!["line1".to_string(), "line2".to_string()];

        let result = comparator.compare_output(&expected, &actual);
        assert!(!result.has_differences());
    }

    #[test]
    fn test_with_differences() {
        let comparator = OutputComparator::new();
        let expected = vec!["line1".to_string(), "line2".to_string()];
        let actual = vec!["line1".to_string(), "different".to_string()];

        let result = comparator.compare_output(&expected, &actual);
        assert!(result.has_differences());
        assert!(!result.diff().is_empty());
    }

    #[test]
    fn test_normalize_whitespace() {
        let comparator = OutputComparator::with_options(true, false, false);
        let expected = vec!["  line1  ".to_string(), "line2".to_string()];
        let actual = vec!["line1".to_string(), " line2 ".to_string()];

        let result = comparator.compare_output(&expected, &actual);
        assert!(!result.has_differences());
    }

    #[test]
    fn test_ignore_case() {
        let comparator = OutputComparator::with_options(false, true, false);
        let expected = vec!["Line1".to_string(), "LINE2".to_string()];
        let actual = vec!["line1".to_string(), "line2".to_string()];

        let result = comparator.compare_output(&expected, &actual);
        assert!(!result.has_differences());
    }

    #[test]
    fn test_normalize_paths() {
        let comparator = OutputComparator::with_options(false, false, true);
        let expected = vec![r"C:\path\to\file".to_string()];
        let actual = vec!["C:/path/to/file".to_string()];

        let result = comparator.compare_output(&expected, &actual);
        assert!(!result.has_differences());
    }
}
