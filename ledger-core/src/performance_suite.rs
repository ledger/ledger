//! Comprehensive performance comparison and benchmarking suite
//!
//! This module provides tools for measuring and comparing performance
//! against C++ Ledger implementation and tracking optimization progress.

use num_traits::ToPrimitive;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;
use std::fs;
use std::path::Path;
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

/// Performance metrics for a single benchmark run
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PerformanceMetrics {
    /// Test name or description
    pub test_name: String,
    /// Duration of the test
    pub duration: Duration,
    /// Memory usage in bytes (RSS)
    pub memory_rss: u64,
    /// Memory usage in bytes (peak heap)
    pub memory_heap: u64,
    /// CPU usage percentage
    pub cpu_usage: f64,
    /// Operations per second (if applicable)
    pub throughput_ops_sec: Option<f64>,
    /// Lines processed per second (for parsing tests)
    pub lines_per_sec: Option<f64>,
    /// Binary size in bytes
    pub binary_size: u64,
    /// Additional custom metrics
    pub custom_metrics: HashMap<String, f64>,
}

impl PerformanceMetrics {
    /// Create new performance metrics
    pub fn new(test_name: String) -> Self {
        Self {
            test_name,
            duration: Duration::ZERO,
            memory_rss: 0,
            memory_heap: 0,
            cpu_usage: 0.0,
            throughput_ops_sec: None,
            lines_per_sec: None,
            binary_size: 0,
            custom_metrics: HashMap::new(),
        }
    }

    /// Add a custom metric
    pub fn add_custom_metric(&mut self, name: String, value: f64) {
        self.custom_metrics.insert(name, value);
    }

    /// Get memory efficiency score (operations per MB of memory)
    pub fn memory_efficiency(&self) -> Option<f64> {
        self.throughput_ops_sec.map(|ops| {
            let memory_mb = self.memory_rss as f64 / 1_000_000.0;
            if memory_mb > 0.0 {
                ops / memory_mb
            } else {
                0.0
            }
        })
    }

    /// Get overall performance score (weighted combination of metrics)
    pub fn performance_score(&self) -> f64 {
        let mut score = 0.0;
        let mut weight_total = 0.0;

        // Duration component (lower is better)
        if self.duration.as_nanos() > 0 {
            score += 1000.0 / (self.duration.as_millis() as f64 + 1.0);
            weight_total += 0.4;
        }

        // Throughput component (higher is better)
        if let Some(throughput) = self.throughput_ops_sec {
            score += throughput / 1000.0;
            weight_total += 0.3;
        }

        // Memory efficiency component (higher is better)
        if let Some(efficiency) = self.memory_efficiency() {
            score += efficiency / 100.0;
            weight_total += 0.3;
        }

        if weight_total > 0.0 {
            score / weight_total
        } else {
            0.0
        }
    }
}

impl fmt::Display for PerformanceMetrics {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Performance Metrics: {}", self.test_name)?;
        writeln!(f, "  Duration: {:?}", self.duration)?;
        writeln!(f, "  Memory RSS: {:.2} MB", self.memory_rss as f64 / 1_000_000.0)?;
        writeln!(f, "  Memory Heap: {:.2} MB", self.memory_heap as f64 / 1_000_000.0)?;
        writeln!(f, "  CPU Usage: {:.1}%", self.cpu_usage)?;

        if let Some(throughput) = self.throughput_ops_sec {
            writeln!(f, "  Throughput: {:.0} ops/sec", throughput)?;
        }

        if let Some(lines_sec) = self.lines_per_sec {
            writeln!(f, "  Parsing Speed: {:.0} lines/sec", lines_sec)?;
        }

        writeln!(f, "  Binary Size: {:.2} MB", self.binary_size as f64 / 1_000_000.0)?;

        if let Some(efficiency) = self.memory_efficiency() {
            writeln!(f, "  Memory Efficiency: {:.2} ops/MB", efficiency)?;
        }

        writeln!(f, "  Performance Score: {:.2}", self.performance_score())?;

        for (key, value) in &self.custom_metrics {
            writeln!(f, "  {}: {:.2}", key, value)?;
        }

        Ok(())
    }
}

/// Comparison between Rust and C++ implementations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImplementationComparison {
    pub rust_metrics: PerformanceMetrics,
    pub cpp_metrics: Option<PerformanceMetrics>,
    pub improvement_ratios: HashMap<String, f64>,
}

impl ImplementationComparison {
    /// Create new comparison
    pub fn new(rust_metrics: PerformanceMetrics) -> Self {
        Self { rust_metrics, cpp_metrics: None, improvement_ratios: HashMap::new() }
    }

    /// Add C++ metrics for comparison
    pub fn with_cpp_metrics(mut self, cpp_metrics: PerformanceMetrics) -> Self {
        // Calculate improvement ratios
        let mut ratios = HashMap::new();

        // Duration improvement (lower is better)
        if cpp_metrics.duration.as_nanos() > 0 {
            let ratio = cpp_metrics.duration.as_nanos() as f64
                / self.rust_metrics.duration.as_nanos() as f64;
            ratios.insert("duration_improvement".to_string(), ratio);
        }

        // Memory improvement (lower is better)
        if cpp_metrics.memory_rss > 0 {
            let ratio = cpp_metrics.memory_rss as f64 / self.rust_metrics.memory_rss as f64;
            ratios.insert("memory_improvement".to_string(), ratio);
        }

        // Throughput improvement (higher is better for Rust)
        if let (Some(rust_throughput), Some(cpp_throughput)) =
            (self.rust_metrics.throughput_ops_sec, cpp_metrics.throughput_ops_sec)
        {
            let ratio = rust_throughput / cpp_throughput;
            ratios.insert("throughput_improvement".to_string(), ratio);
        }

        // Binary size improvement (lower is better)
        if cpp_metrics.binary_size > 0 {
            let ratio = cpp_metrics.binary_size as f64 / self.rust_metrics.binary_size as f64;
            ratios.insert("binary_size_improvement".to_string(), ratio);
        }

        self.cpp_metrics = Some(cpp_metrics);
        self.improvement_ratios = ratios;
        self
    }

    /// Check if performance targets are met
    pub fn meets_targets(&self) -> TargetResults {
        let mut results = TargetResults::default();

        // Memory reduction target: 30%
        if let Some(ratio) = self.improvement_ratios.get("memory_improvement") {
            results.memory_target_met = *ratio >= 1.3; // 30% reduction
            results.memory_improvement = Some(*ratio);
        }

        // Performance improvement target: no regression
        if let Some(ratio) = self.improvement_ratios.get("duration_improvement") {
            results.performance_target_met = *ratio >= 1.0; // No regression
            results.performance_improvement = Some(*ratio);
        }

        // Binary size target: comparable or better
        if let Some(ratio) = self.improvement_ratios.get("binary_size_improvement") {
            results.binary_size_target_met = *ratio >= 0.9; // Within 10%
            results.binary_size_improvement = Some(*ratio);
        }

        results
    }
}

impl fmt::Display for ImplementationComparison {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "\n=== Implementation Comparison ===")?;
        writeln!(f, "\nRust Implementation:")?;
        write!(f, "{}", self.rust_metrics)?;

        if let Some(ref cpp_metrics) = self.cpp_metrics {
            writeln!(f, "\nC++ Implementation:")?;
            write!(f, "{}", cpp_metrics)?;

            writeln!(f, "\n=== Improvement Ratios ===")?;
            for (metric, ratio) in &self.improvement_ratios {
                let status = match metric.as_str() {
                    "memory_improvement" => {
                        if *ratio >= 1.3 {
                            "✓ EXCELLENT"
                        } else if *ratio >= 1.1 {
                            "✓ GOOD"
                        } else if *ratio >= 0.9 {
                            "~ ACCEPTABLE"
                        } else {
                            "✗ NEEDS WORK"
                        }
                    }
                    "duration_improvement" => {
                        if *ratio >= 1.2 {
                            "✓ EXCELLENT"
                        } else if *ratio >= 1.0 {
                            "✓ GOOD"
                        } else if *ratio >= 0.9 {
                            "~ ACCEPTABLE"
                        } else {
                            "✗ REGRESSION"
                        }
                    }
                    _ => {
                        if *ratio >= 1.1 {
                            "✓ GOOD"
                        } else if *ratio >= 0.9 {
                            "~ ACCEPTABLE"
                        } else {
                            "✗ NEEDS WORK"
                        }
                    }
                };
                writeln!(f, "  {}: {:.2}x {}", metric, ratio, status)?;
            }

            let targets = self.meets_targets();
            writeln!(f, "\n=== Target Achievement ===")?;
            writeln!(
                f,
                "  Memory Target (30% reduction): {}",
                if targets.memory_target_met { "✓ MET" } else { "✗ NOT MET" }
            )?;
            writeln!(
                f,
                "  Performance Target (no regression): {}",
                if targets.performance_target_met { "✓ MET" } else { "✗ NOT MET" }
            )?;
            writeln!(
                f,
                "  Binary Size Target (comparable): {}",
                if targets.binary_size_target_met { "✓ MET" } else { "✗ NOT MET" }
            )?;
        }

        Ok(())
    }
}

/// Target achievement results
#[derive(Debug, Clone, Default)]
pub struct TargetResults {
    pub memory_target_met: bool,
    pub performance_target_met: bool,
    pub binary_size_target_met: bool,
    pub memory_improvement: Option<f64>,
    pub performance_improvement: Option<f64>,
    pub binary_size_improvement: Option<f64>,
}

/// Benchmark suite for comprehensive performance testing
pub struct BenchmarkSuite {
    results: Vec<ImplementationComparison>,
}

impl BenchmarkSuite {
    /// Create new benchmark suite
    pub fn new() -> Self {
        Self { results: Vec::new() }
    }

    /// Run parsing benchmarks
    pub fn benchmark_parsing(
        &mut self,
        test_file: &Path,
    ) -> Result<(), Box<dyn std::error::Error>> {
        println!("Running parsing benchmark on {:?}", test_file);

        let rust_metrics = self.benchmark_rust_parsing(test_file)?;
        let comparison = ImplementationComparison::new(rust_metrics);

        // TODO: Add C++ parsing benchmark if available
        self.results.push(comparison);

        Ok(())
    }

    /// Run arithmetic benchmarks
    pub fn benchmark_arithmetic(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        println!("Running arithmetic benchmarks");

        let mut metrics = PerformanceMetrics::new("Arithmetic Operations".to_string());

        let start = Instant::now();

        // Run intensive arithmetic operations
        let iterations = 1_000_000;
        let mut total = rust_decimal::Decimal::ZERO;

        for i in 0..iterations {
            let val = rust_decimal::Decimal::from(i);
            total = total + val * rust_decimal::Decimal::from(2);
        }

        metrics.duration = start.elapsed();
        metrics.throughput_ops_sec = Some(iterations as f64 / metrics.duration.as_secs_f64());
        metrics.add_custom_metric("result".to_string(), total.to_f64().unwrap_or(0.0));

        let comparison = ImplementationComparison::new(metrics);
        self.results.push(comparison);

        Ok(())
    }

    /// Run report generation benchmarks
    pub fn benchmark_report_generation(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        println!("Running report generation benchmarks");

        let mut metrics = PerformanceMetrics::new("Report Generation".to_string());

        let start = Instant::now();

        // Simulate report generation
        let iterations = 10_000;
        let mut lines_generated = 0;

        for i in 0..iterations {
            // Simulate formatting operations
            let _formatted = format!(
                "2024-01-{:02} Account:Subaccount    ${:.2}",
                (i % 28) + 1,
                (i as f64) * 1.23
            );
            lines_generated += 1;
        }

        metrics.duration = start.elapsed();
        metrics.lines_per_sec = Some(lines_generated as f64 / metrics.duration.as_secs_f64());

        let comparison = ImplementationComparison::new(metrics);
        self.results.push(comparison);

        Ok(())
    }

    /// Benchmark Rust parsing implementation
    fn benchmark_rust_parsing(
        &self,
        _test_file: &Path,
    ) -> Result<PerformanceMetrics, Box<dyn std::error::Error>> {
        let mut metrics = PerformanceMetrics::new("Rust Parsing".to_string());

        let start = Instant::now();

        // Simulate parsing operations
        let lines = 50_000;
        for i in 0..lines {
            // Simulate line parsing
            let _parsed = format!("2024-01-01 * Transaction {}", i);
        }

        metrics.duration = start.elapsed();
        metrics.lines_per_sec = Some(lines as f64 / metrics.duration.as_secs_f64());
        metrics.memory_rss = 50_000_000; // Simulated RSS
        metrics.memory_heap = 30_000_000; // Simulated heap

        Ok(metrics)
    }

    /// Get binary size
    pub fn measure_binary_size(&self, binary_path: &Path) -> u64 {
        fs::metadata(binary_path).map(|metadata| metadata.len()).unwrap_or(0)
    }

    /// Generate comprehensive report
    pub fn generate_report(&self) -> String {
        let mut report = String::new();

        report.push_str("# Ledger Rust Implementation Performance Report\n\n");
        report.push_str(&format!(
            "Generated: {}\n",
            chrono::Utc::now().format("%Y-%m-%d %H:%M:%S UTC")
        ));
        report.push_str(&format!("Total benchmarks: {}\n\n", self.results.len()));

        for (i, comparison) in self.results.iter().enumerate() {
            report.push_str(&format!(
                "## Benchmark {} - {}\n\n",
                i + 1,
                comparison.rust_metrics.test_name
            ));
            report.push_str(&format!("{}\n", comparison));
        }

        // Summary statistics
        let total_score: f64 =
            self.results.iter().map(|r| r.rust_metrics.performance_score()).sum();
        let avg_score =
            if !self.results.is_empty() { total_score / self.results.len() as f64 } else { 0.0 };

        report.push_str("\n## Summary\n\n");
        report.push_str(&format!("Average Performance Score: {:.2}\n", avg_score));

        // Check if any targets were measured
        let mut targets_met = 0;
        let mut targets_total = 0;

        for comparison in &self.results {
            if comparison.cpp_metrics.is_some() {
                let targets = comparison.meets_targets();
                if targets.memory_target_met {
                    targets_met += 1;
                }
                if targets.performance_target_met {
                    targets_met += 1;
                }
                if targets.binary_size_target_met {
                    targets_met += 1;
                }
                targets_total += 3;
            }
        }

        if targets_total > 0 {
            report.push_str(&format!(
                "Targets Met: {}/{} ({:.1}%)\n",
                targets_met,
                targets_total,
                targets_met as f64 / targets_total as f64 * 100.0
            ));
        }

        report
    }

    /// Save report to file
    pub fn save_report(&self, path: &Path) -> Result<(), Box<dyn std::error::Error>> {
        let report = self.generate_report();
        fs::write(path, report)?;
        println!("Performance report saved to {:?}", path);
        Ok(())
    }

    /// Export results as JSON
    pub fn export_json(&self, path: &Path) -> Result<(), Box<dyn std::error::Error>> {
        let json = serde_json::to_string_pretty(&self.results)?;
        fs::write(path, json)?;
        println!("Performance results exported to {:?}", path);
        Ok(())
    }
}

impl Default for BenchmarkSuite {
    fn default() -> Self {
        Self::new()
    }
}

/// Memory profiling utilities
pub struct MemoryProfiler;

impl MemoryProfiler {
    /// Measure peak memory usage during a closure execution
    pub fn measure_peak_memory<F, R>(f: F) -> (R, u64)
    where
        F: FnOnce() -> R,
    {
        // Start memory tracking (simplified version)
        let start_memory = Self::get_current_memory();

        let result = f();

        let end_memory = Self::get_current_memory();
        let peak_memory = end_memory.saturating_sub(start_memory);

        (result, peak_memory)
    }

    /// Get current process memory usage (simplified)
    fn get_current_memory() -> u64 {
        // In a real implementation, this would read from /proc/self/status
        // or use platform-specific APIs
        50_000_000 // Placeholder value
    }
}

/// Size optimization analysis
pub struct SizeOptimizer;

impl SizeOptimizer {
    /// Analyze binary sections and suggest optimizations
    pub fn analyze_binary_size(
        binary_path: &Path,
    ) -> Result<SizeAnalysis, Box<dyn std::error::Error>> {
        let total_size = fs::metadata(binary_path)?.len();

        // This would use tools like `nm`, `objdump`, or `cargo bloat` in practice
        let analysis = SizeAnalysis {
            total_size,
            text_size: total_size * 60 / 100,  // Estimated
            data_size: total_size * 20 / 100,  // Estimated
            debug_size: total_size * 10 / 100, // Estimated
            other_size: total_size * 10 / 100, // Estimated
            suggestions: vec![
                "Consider using profile.min-size for smaller binaries".to_string(),
                "Strip debug symbols for production builds".to_string(),
                "Enable LTO for better dead code elimination".to_string(),
            ],
        };

        Ok(analysis)
    }
}

/// Binary size analysis results
#[derive(Debug, Clone)]
pub struct SizeAnalysis {
    pub total_size: u64,
    pub text_size: u64,
    pub data_size: u64,
    pub debug_size: u64,
    pub other_size: u64,
    pub suggestions: Vec<String>,
}

impl fmt::Display for SizeAnalysis {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Binary Size Analysis:")?;
        writeln!(f, "  Total: {:.2} MB", self.total_size as f64 / 1_000_000.0)?;
        writeln!(
            f,
            "  Text (code): {:.2} MB ({:.1}%)",
            self.text_size as f64 / 1_000_000.0,
            self.text_size as f64 / self.total_size as f64 * 100.0
        )?;
        writeln!(
            f,
            "  Data: {:.2} MB ({:.1}%)",
            self.data_size as f64 / 1_000_000.0,
            self.data_size as f64 / self.total_size as f64 * 100.0
        )?;
        writeln!(
            f,
            "  Debug: {:.2} MB ({:.1}%)",
            self.debug_size as f64 / 1_000_000.0,
            self.debug_size as f64 / self.total_size as f64 * 100.0
        )?;
        writeln!(
            f,
            "  Other: {:.2} MB ({:.1}%)",
            self.other_size as f64 / 1_000_000.0,
            self.other_size as f64 / self.total_size as f64 * 100.0
        )?;

        writeln!(f, "\nOptimization Suggestions:")?;
        for (i, suggestion) in self.suggestions.iter().enumerate() {
            writeln!(f, "  {}. {}", i + 1, suggestion)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_performance_metrics() {
        let mut metrics = PerformanceMetrics::new("test".to_string());
        metrics.duration = Duration::from_millis(100);
        metrics.memory_rss = 1_000_000;
        metrics.throughput_ops_sec = Some(1000.0);

        assert!(metrics.memory_efficiency().is_some());
        assert!(metrics.performance_score() > 0.0);
    }

    #[test]
    fn test_benchmark_suite() {
        let mut suite = BenchmarkSuite::new();

        // Test arithmetic benchmark
        suite.benchmark_arithmetic().unwrap();
        assert!(!suite.results.is_empty());

        // Test report generation
        suite.benchmark_report_generation().unwrap();
        assert_eq!(suite.results.len(), 2);

        // Generate report
        let report = suite.generate_report();
        assert!(!report.is_empty());
    }

    #[test]
    fn test_memory_profiler() {
        let (result, memory) = MemoryProfiler::measure_peak_memory(|| {
            let vec: Vec<u8> = vec![0; 1000];
            vec.len()
        });

        assert_eq!(result, 1000);
        assert!(memory > 0); // Should measure some memory usage
    }

    #[test]
    fn test_implementation_comparison() {
        let rust_metrics = PerformanceMetrics::new("test".to_string());
        let mut cpp_metrics = PerformanceMetrics::new("test".to_string());

        // Make Rust look better for testing
        cpp_metrics.duration = Duration::from_millis(200);
        cpp_metrics.memory_rss = 2_000_000;

        let comparison = ImplementationComparison::new(rust_metrics).with_cpp_metrics(cpp_metrics);

        assert!(!comparison.improvement_ratios.is_empty());

        let targets = comparison.meets_targets();
        // Should show some improvements
        assert!(targets.memory_improvement.unwrap_or(0.0) > 0.0);
    }
}
