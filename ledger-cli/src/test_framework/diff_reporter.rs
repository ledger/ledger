//! Diff Reporter for Test Output Comparison
//!
//! This module provides sophisticated diff reporting for comparing expected vs actual
//! test output, with support for multiple output formats and colored display.

use std::fmt;
use colored::*;
use similar::{ChangeTag, TextDiff, Algorithm};

/// Diff output format options
#[derive(Debug, Clone)]
pub enum DiffFormat {
    /// Unified diff format (default)
    Unified,
    /// Side-by-side comparison
    SideBySide,
    /// Context diff format
    Context,
    /// Inline diff with color highlighting
    Inline,
}

/// Configuration for diff reporting
#[derive(Debug, Clone)]
pub struct DiffConfig {
    pub format: DiffFormat,
    pub context_lines: usize,
    pub show_line_numbers: bool,
    pub use_color: bool,
    pub floating_point_tolerance: Option<f64>,
    pub ignore_whitespace: bool,
    pub tab_width: usize,
}

impl Default for DiffConfig {
    fn default() -> Self {
        Self {
            format: DiffFormat::Unified,
            context_lines: 3,
            show_line_numbers: true,
            use_color: true,
            floating_point_tolerance: Some(1e-10),
            ignore_whitespace: false,
            tab_width: 4,
        }
    }
}

/// Diff result containing comparison information
#[derive(Debug)]
pub struct DiffResult {
    pub has_differences: bool,
    pub similarity_score: f64,
    pub added_lines: usize,
    pub removed_lines: usize,
    pub modified_lines: usize,
    pub diff_output: String,
}

/// Advanced diff reporter with multiple output formats
pub struct DiffReporter {
    config: DiffConfig,
}

impl DiffReporter {
    /// Create a new diff reporter with default configuration
    pub fn new() -> Self {
        Self {
            config: DiffConfig::default(),
        }
    }

    /// Create a diff reporter with custom configuration
    pub fn with_config(config: DiffConfig) -> Self {
        Self { config }
    }

    /// Compare two text outputs and generate diff report
    pub fn compare(&self, expected: &[String], actual: &[String]) -> DiffResult {
        let expected_text = expected.join("\n");
        let actual_text = actual.join("\n");

        // Preprocess text if needed
        let expected_processed = self.preprocess_text(&expected_text);
        let actual_processed = self.preprocess_text(&actual_text);

        // Create text diff
        let diff = TextDiff::configure()
            .algorithm(Algorithm::Myers)
            .diff_lines(&expected_processed, &actual_processed);

        // Calculate statistics
        let mut added_lines = 0;
        let mut removed_lines = 0;
        let mut modified_lines = 0;

        for change in diff.iter_all_changes() {
            match change.tag() {
                ChangeTag::Insert => added_lines += 1,
                ChangeTag::Delete => removed_lines += 1,
                ChangeTag::Equal => {}
            }
        }

        // Count modified lines as pairs of add/delete
        let paired_changes = added_lines.min(removed_lines);
        modified_lines = paired_changes;
        added_lines -= paired_changes;
        removed_lines -= paired_changes;

        let has_differences = added_lines > 0 || removed_lines > 0 || modified_lines > 0;
        
        // Calculate similarity score
        let total_lines = expected.len().max(actual.len());
        let unchanged_lines = total_lines - added_lines - removed_lines - modified_lines;
        let similarity_score = if total_lines > 0 {
            unchanged_lines as f64 / total_lines as f64
        } else {
            1.0
        };

        // Generate diff output based on format
        let diff_output = match self.config.format {
            DiffFormat::Unified => self.format_unified_diff(&diff),
            DiffFormat::SideBySide => self.format_side_by_side(&diff),
            DiffFormat::Context => self.format_context_diff(&diff),
            DiffFormat::Inline => self.format_inline_diff(&diff),
        };

        DiffResult {
            has_differences,
            similarity_score,
            added_lines,
            removed_lines,
            modified_lines,
            diff_output,
        }
    }

    /// Compare floating point values with tolerance
    pub fn compare_with_tolerance(&self, expected: &[String], actual: &[String]) -> DiffResult {
        if let Some(tolerance) = self.config.floating_point_tolerance {
            let expected_normalized = self.normalize_numbers(expected, tolerance);
            let actual_normalized = self.normalize_numbers(actual, tolerance);
            self.compare(&expected_normalized, &actual_normalized)
        } else {
            self.compare(expected, actual)
        }
    }

    /// Preprocess text according to configuration
    fn preprocess_text(&self, text: &str) -> String {
        let mut result = text.to_string();

        if self.config.ignore_whitespace {
            result = result.lines()
                .map(|line| line.trim())
                .collect::<Vec<_>>()
                .join("\n");
        }

        // Normalize tabs
        if self.config.tab_width > 0 {
            result = result.replace('\t', &" ".repeat(self.config.tab_width));
        }

        result
    }

    /// Normalize numbers with floating point tolerance
    fn normalize_numbers(&self, lines: &[String], tolerance: f64) -> Vec<String> {
        lines.iter()
            .map(|line| {
                let mut result = line.clone();
                
                // Simple regex-like approach to find numbers
                // In a real implementation, you'd use a proper regex crate
                let words: Vec<&str> = line.split_whitespace().collect();
                for word in words {
                    if let Ok(num) = word.parse::<f64>() {
                        let rounded = (num / tolerance).round() * tolerance;
                        result = result.replace(word, &format!("{:.10}", rounded));
                    }
                }
                
                result
            })
            .collect()
    }

    /// Format unified diff output
    fn format_unified_diff<'a>(&self, diff: &TextDiff<'a, 'a, 'a, str>) -> String {
        let mut output = String::new();
        
        output.push_str("--- expected\n");
        output.push_str("+++ actual\n");

        for group in diff.grouped_ops(self.config.context_lines) {
            if let Some(first_op) = group.first() {
                let old_start = first_op.old_range().start + 1;
                let old_len = first_op.old_range().len();
                let new_start = first_op.new_range().start + 1;
                let new_len = first_op.new_range().len();
                
                output.push_str(&format!("@@ -{},{} +{},{} @@\n", 
                    old_start, old_len, new_start, new_len));
            }

            for op in group {
                for change in diff.iter_changes(&op) {
                    let sign = match change.tag() {
                        ChangeTag::Equal => " ",
                        ChangeTag::Delete => "-",
                        ChangeTag::Insert => "+",
                    };
                    
                    let line = if self.config.use_color {
                        match change.tag() {
                            ChangeTag::Equal => format!(" {}", change.value()),
                            ChangeTag::Delete => format!("-{}", change.value()).red().to_string(),
                            ChangeTag::Insert => format!("+{}", change.value()).green().to_string(),
                        }
                    } else {
                        format!("{}{}", sign, change.value())
                    };
                    
                    output.push_str(&line);
                    if !change.value().ends_with('\n') {
                        output.push('\n');
                    }
                }
            }
        }

        output
    }

    /// Format side-by-side diff output
    fn format_side_by_side<'a>(&self, diff: &TextDiff<'a, 'a, 'a, str>) -> String {
        let mut output = String::new();
        output.push_str(&format!("{:<40} | {:<40}\n", "Expected", "Actual"));
        output.push_str(&format!("{:-<40}-+-{:-<40}\n", "", ""));

        for change in diff.iter_all_changes() {
            match change.tag() {
                ChangeTag::Equal => {
                    let line = change.value().trim_end();
                    output.push_str(&format!("{:<40} | {:<40}\n", line, line));
                }
                ChangeTag::Delete => {
                    let line = change.value().trim_end();
                    let formatted = if self.config.use_color {
                        line.red().to_string()
                    } else {
                        format!("-{}", line)
                    };
                    output.push_str(&format!("{:<40} | {:<40}\n", formatted, ""));
                }
                ChangeTag::Insert => {
                    let line = change.value().trim_end();
                    let formatted = if self.config.use_color {
                        line.green().to_string()
                    } else {
                        format!("+{}", line)
                    };
                    output.push_str(&format!("{:<40} | {:<40}\n", "", formatted));
                }
            }
        }

        output
    }

    /// Format context diff output
    fn format_context_diff<'a>(&self, diff: &TextDiff<'a, 'a, 'a, str>) -> String {
        // Context diff implementation would be similar to unified diff
        // but with different header format and context markers
        self.format_unified_diff(diff) // Simplified for now
    }

    /// Format inline diff with character-level highlighting
    fn format_inline_diff<'a>(&self, diff: &TextDiff<'a, 'a, 'a, str>) -> String {
        let mut output = String::new();
        
        for change in diff.iter_all_changes() {
            let prefix = match change.tag() {
                ChangeTag::Equal => "  ",
                ChangeTag::Delete => "- ",
                ChangeTag::Insert => "+ ",
            };

            let line = if self.config.use_color {
                match change.tag() {
                    ChangeTag::Equal => change.value().to_string(),
                    ChangeTag::Delete => change.value().red().to_string(),
                    ChangeTag::Insert => change.value().green().to_string(),
                }
            } else {
                change.value().to_string()
            };

            output.push_str(&format!("{}{}", prefix, line));
            if !change.value().ends_with('\n') {
                output.push('\n');
            }
        }

        output
    }
}

impl Default for DiffReporter {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for DiffResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Diff Summary:")?;
        writeln!(f, "  Has differences: {}", self.has_differences)?;
        writeln!(f, "  Similarity: {:.1}%", self.similarity_score * 100.0)?;
        writeln!(f, "  Added lines: {}", self.added_lines)?;
        writeln!(f, "  Removed lines: {}", self.removed_lines)?;
        writeln!(f, "  Modified lines: {}", self.modified_lines)?;
        writeln!(f)?;
        write!(f, "{}", self.diff_output)
    }
}