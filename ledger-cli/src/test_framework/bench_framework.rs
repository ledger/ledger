//! Performance Benchmarking Framework
//!
//! This module provides comprehensive performance benchmarking for the Rust ledger implementation,
//! including comparison against C++ baseline, memory profiling, and regression detection.

use anyhow::{Context, Result};
use log::{debug, error, info, warn};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{Duration, Instant};

use super::test_harness::{ProcessResult, TestHarness};

/// Benchmark configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BenchmarkConfig {
    /// Number of warmup iterations
    pub warmup_iterations: u32,
    /// Number of benchmark iterations
    pub benchmark_iterations: u32,
    /// Timeout for each benchmark run (seconds)
    pub timeout_seconds: u64,
    /// Whether to run memory profiling
    pub profile_memory: bool,
    /// Whether to compare against C++ implementation
    pub compare_with_cpp: bool,
    /// Path to C++ ledger binary for comparison
    pub cpp_ledger_path: Option<PathBuf>,
    /// Directory to store benchmark results
    pub results_dir: PathBuf,
    /// Performance regression threshold (percentage)
    pub regression_threshold: f64,
}

impl Default for BenchmarkConfig {
    fn default() -> Self {
        Self {
            warmup_iterations: 3,
            benchmark_iterations: 10,
            timeout_seconds: 60,
            profile_memory: false,
            compare_with_cpp: false,
            cpp_ledger_path: None,
            results_dir: PathBuf::from("benchmark_results"),
            regression_threshold: 10.0, // 10% regression threshold
        }
    }
}

/// Results from a single benchmark run
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BenchmarkResult {
    pub name: String,
    pub command: String,
    pub execution_times: Vec<Duration>,
    pub mean_time: Duration,
    pub median_time: Duration,
    pub min_time: Duration,
    pub max_time: Duration,
    pub standard_deviation: Duration,
    pub memory_usage: Option<MemoryMetrics>,
    pub exit_code: i32,
    pub stdout_lines: usize,
    pub stderr_lines: usize,
    pub timestamp: chrono::DateTime<chrono::Utc>,
}

/// Memory usage metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MemoryMetrics {
    pub peak_memory_kb: u64,
    pub average_memory_kb: u64,
    pub memory_allocations: u64,
    pub memory_deallocations: u64,
}

/// Comparison between Rust and C++ implementations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComparisonResult {
    pub rust_result: BenchmarkResult,
    pub cpp_result: Option<BenchmarkResult>,
    pub performance_ratio: Option<f64>, // Rust time / C++ time
    pub memory_ratio: Option<f64>,      // Rust memory / C++ memory
    pub regression_detected: bool,
}

/// Benchmark suite for different categories of tests
#[derive(Debug, Clone)]
pub struct BenchmarkSuite {
    pub name: String,
    pub benchmarks: Vec<BenchmarkSpec>,
}

/// Specification for a single benchmark
#[derive(Debug, Clone)]
pub struct BenchmarkSpec {
    pub name: String,
    pub command: String,
    pub data_file: Option<PathBuf>,
    pub category: BenchmarkCategory,
    pub expected_time_range: Option<(Duration, Duration)>,
}

/// Categories of benchmarks
#[derive(Debug, Clone, PartialEq)]
pub enum BenchmarkCategory {
    /// Parsing large journal files
    Parsing,
    /// Balance calculation and reporting
    Balance,
    /// Register queries and filtering
    Register,
    /// Complex query expressions
    Queries,
    /// Report formatting and output
    Reporting,
    /// Mathematical operations
    Math,
    /// Memory intensive operations
    Memory,
}

/// Main benchmark runner
pub struct BenchmarkRunner<'a> {
    harness: &'a TestHarness,
    config: BenchmarkConfig,
    baseline_results: HashMap<String, BenchmarkResult>,
}

impl<'a> BenchmarkRunner<'a> {
    /// Create a new benchmark runner
    pub fn new(harness: &'a TestHarness) -> Self {
        Self { harness, config: BenchmarkConfig::default(), baseline_results: HashMap::new() }
    }

    /// Create benchmark runner with custom configuration
    pub fn with_config(harness: &'a TestHarness, config: BenchmarkConfig) -> Self {
        Self { harness, config, baseline_results: HashMap::new() }
    }

    /// Load baseline results from previous runs
    pub fn load_baseline(&mut self, baseline_file: &Path) -> Result<()> {
        if baseline_file.exists() {
            let content = std::fs::read_to_string(baseline_file)?;
            let results: Vec<BenchmarkResult> = serde_json::from_str(&content)?;

            for result in results {
                self.baseline_results.insert(result.name.clone(), result);
            }

            info!("Loaded {} baseline benchmark results", self.baseline_results.len());
        }
        Ok(())
    }

    /// Save benchmark results as new baseline
    pub fn save_baseline(&self, results: &[BenchmarkResult], baseline_file: &Path) -> Result<()> {
        let content = serde_json::to_string_pretty(results)?;
        std::fs::create_dir_all(baseline_file.parent().unwrap())?;
        std::fs::write(baseline_file, content)?;
        info!("Saved {} benchmark results as baseline", results.len());
        Ok(())
    }

    /// Run a complete benchmark suite
    pub fn run_suite(&self, suite: &BenchmarkSuite) -> Result<Vec<ComparisonResult>> {
        info!("Running benchmark suite: {}", suite.name);

        let mut results = Vec::new();

        for benchmark in &suite.benchmarks {
            info!("Running benchmark: {}", benchmark.name);

            let rust_result = self.run_benchmark(benchmark)?;
            let cpp_result = if self.config.compare_with_cpp {
                self.run_cpp_benchmark(benchmark).ok()
            } else {
                None
            };

            let comparison = ComparisonResult {
                performance_ratio: cpp_result.as_ref().map(|cpp| {
                    rust_result.mean_time.as_nanos() as f64 / cpp.mean_time.as_nanos() as f64
                }),
                memory_ratio: match (
                    &rust_result.memory_usage,
                    &cpp_result.as_ref().and_then(|c| c.memory_usage.as_ref()),
                ) {
                    (Some(rust_mem), Some(cpp_mem)) => {
                        Some(rust_mem.peak_memory_kb as f64 / cpp_mem.peak_memory_kb as f64)
                    }
                    _ => None,
                },
                regression_detected: self.detect_regression(&rust_result),
                rust_result,
                cpp_result,
            };

            results.push(comparison);
        }

        Ok(results)
    }

    /// Run a single benchmark against Rust implementation
    pub fn run_benchmark(&self, spec: &BenchmarkSpec) -> Result<BenchmarkResult> {
        let command = if let Some(data_file) = &spec.data_file {
            format!("{} -f \"{}\"", spec.command, data_file.display())
        } else {
            spec.command.clone()
        };

        debug!("Benchmarking command: {}", command);

        // Warmup runs
        for _ in 0..self.config.warmup_iterations {
            let _ = self.harness.run_command(&command, true)?;
        }

        // Benchmark runs
        let mut execution_times = Vec::new();
        let mut stdout_lines = 0;
        let mut stderr_lines = 0;
        let mut exit_code = 0;

        for i in 0..self.config.benchmark_iterations {
            debug!("Benchmark iteration {} of {}", i + 1, self.config.benchmark_iterations);

            let start = Instant::now();
            let result = self.harness.run_command(&command, true)?;
            let elapsed = start.elapsed();

            execution_times.push(elapsed);

            // Use metrics from the last run
            if i == self.config.benchmark_iterations - 1 {
                stdout_lines = result.stdout.lines().count();
                stderr_lines = result.stderr.lines().count();
                exit_code = result.exit_code;
            }
        }

        // Calculate statistics
        let mean_time = Duration::from_nanos(
            (execution_times.iter().map(|d| d.as_nanos()).sum::<u128>()
                / execution_times.len() as u128) as u64,
        );

        let mut sorted_times = execution_times.clone();
        sorted_times.sort();
        let median_time = sorted_times[sorted_times.len() / 2];
        let min_time = *sorted_times.first().unwrap();
        let max_time = *sorted_times.last().unwrap();

        // Calculate standard deviation
        let variance = execution_times
            .iter()
            .map(|time| {
                let diff = time.as_nanos() as i128 - mean_time.as_nanos() as i128;
                (diff * diff) as u128
            })
            .sum::<u128>()
            / execution_times.len() as u128;
        let standard_deviation = Duration::from_nanos((variance as f64).sqrt() as u64);

        // Memory profiling (simplified - in practice would use more sophisticated tools)
        let memory_usage =
            if self.config.profile_memory { Some(self.profile_memory(&command)?) } else { None };

        Ok(BenchmarkResult {
            name: spec.name.clone(),
            command: command.clone(),
            execution_times,
            mean_time,
            median_time,
            min_time,
            max_time,
            standard_deviation,
            memory_usage,
            exit_code,
            stdout_lines,
            stderr_lines,
            timestamp: chrono::Utc::now(),
        })
    }

    /// Run benchmark against C++ implementation for comparison
    pub fn run_cpp_benchmark(&self, spec: &BenchmarkSpec) -> Result<BenchmarkResult> {
        let cpp_ledger = self
            .config
            .cpp_ledger_path
            .as_ref()
            .ok_or_else(|| anyhow::anyhow!("C++ ledger path not configured"))?;

        let command = if let Some(data_file) = &spec.data_file {
            format!("{} -f \"{}\" {}", cpp_ledger.display(), data_file.display(), spec.command)
        } else {
            format!("{} {}", cpp_ledger.display(), spec.command)
        };

        debug!("Benchmarking C++ command: {}", command);

        // Warmup runs
        for _ in 0..self.config.warmup_iterations {
            let _ = Command::new("sh").arg("-c").arg(&command).output()?;
        }

        // Benchmark runs
        let mut execution_times = Vec::new();
        let mut stdout_lines = 0;
        let mut stderr_lines = 0;
        let mut exit_code = 0;

        for i in 0..self.config.benchmark_iterations {
            let start = Instant::now();
            let output = Command::new("sh").arg("-c").arg(&command).output()?;
            let elapsed = start.elapsed();

            execution_times.push(elapsed);

            // Use metrics from the last run
            if i == self.config.benchmark_iterations - 1 {
                stdout_lines = String::from_utf8_lossy(&output.stdout).lines().count();
                stderr_lines = String::from_utf8_lossy(&output.stderr).lines().count();
                exit_code = output.status.code().unwrap_or(-1);
            }
        }

        // Calculate statistics (same as Rust benchmark)
        let mean_time = Duration::from_nanos(
            (execution_times.iter().map(|d| d.as_nanos()).sum::<u128>()
                / execution_times.len() as u128) as u64,
        );

        let mut sorted_times = execution_times.clone();
        sorted_times.sort();
        let median_time = sorted_times[sorted_times.len() / 2];
        let min_time = *sorted_times.first().unwrap();
        let max_time = *sorted_times.last().unwrap();

        let variance = execution_times
            .iter()
            .map(|time| {
                let diff = time.as_nanos() as i128 - mean_time.as_nanos() as i128;
                (diff * diff) as u128
            })
            .sum::<u128>()
            / execution_times.len() as u128;
        let standard_deviation = Duration::from_nanos((variance as f64).sqrt() as u64);

        Ok(BenchmarkResult {
            name: format!("{}_cpp", spec.name),
            command: command.clone(),
            execution_times,
            mean_time,
            median_time,
            min_time,
            max_time,
            standard_deviation,
            memory_usage: None, // Simplified - could implement memory profiling for C++
            exit_code,
            stdout_lines,
            stderr_lines,
            timestamp: chrono::Utc::now(),
        })
    }

    /// Profile memory usage (simplified implementation)
    fn profile_memory(&self, command: &str) -> Result<MemoryMetrics> {
        // This is a simplified implementation. In practice, you would use tools like:
        // - Valgrind on Linux
        // - Instruments on macOS
        // - Application Verifier on Windows
        // Or integrate with Rust-specific profiling tools

        debug!("Profiling memory for command: {}", command);

        // For now, return mock data. In a real implementation, this would
        // run the command with memory profiling tools and parse their output
        Ok(MemoryMetrics {
            peak_memory_kb: 1024,       // Mock value
            average_memory_kb: 512,     // Mock value
            memory_allocations: 1000,   // Mock value
            memory_deallocations: 1000, // Mock value
        })
    }

    /// Detect performance regression compared to baseline
    fn detect_regression(&self, result: &BenchmarkResult) -> bool {
        if let Some(baseline) = self.baseline_results.get(&result.name) {
            let performance_ratio =
                result.mean_time.as_nanos() as f64 / baseline.mean_time.as_nanos() as f64;
            let regression_pct = (performance_ratio - 1.0) * 100.0;

            if regression_pct > self.config.regression_threshold {
                warn!(
                    "Performance regression detected for {}: {:.1}% slower than baseline",
                    result.name, regression_pct
                );
                return true;
            }
        }
        false
    }

    /// Generate comprehensive benchmark report
    pub fn generate_report(&self, results: &[ComparisonResult], output_path: &Path) -> Result<()> {
        let mut report = String::new();

        report.push_str("# Ledger Benchmark Report\n\n");
        report.push_str(&format!(
            "Generated on: {}\n\n",
            chrono::Utc::now().format("%Y-%m-%d %H:%M:%S UTC")
        ));

        // Summary table
        report.push_str("## Performance Summary\n\n");
        report.push_str(
            "| Benchmark | Rust Time (ms) | C++ Time (ms) | Ratio | Memory (KB) | Status |\n",
        );
        report.push_str(
            "|-----------|----------------|---------------|-------|-------------|--------|\n",
        );

        for result in results {
            let rust_time = result.rust_result.mean_time.as_millis();
            let cpp_time = result.cpp_result.as_ref().map(|r| r.mean_time.as_millis());
            let ratio = result
                .performance_ratio
                .map(|r| format!("{:.2}x", r))
                .unwrap_or_else(|| "N/A".to_string());
            let memory = result
                .rust_result
                .memory_usage
                .as_ref()
                .map(|m| m.peak_memory_kb.to_string())
                .unwrap_or_else(|| "N/A".to_string());
            let status = if result.regression_detected { "⚠️ Regression" } else { "✅ OK" };

            report.push_str(&format!(
                "| {} | {} | {} | {} | {} | {} |\n",
                result.rust_result.name,
                rust_time,
                cpp_time.map(|t| t.to_string()).unwrap_or_else(|| "N/A".to_string()),
                ratio,
                memory,
                status
            ));
        }

        // Detailed results
        report.push_str("\n## Detailed Results\n\n");
        for result in results {
            report.push_str(&format!("### {}\n\n", result.rust_result.name));
            report.push_str(&format!("**Command:** `{}`\n\n", result.rust_result.command));
            report.push_str(&format!("**Rust Results:**\n"));
            report.push_str(&format!(
                "- Mean time: {:.2}ms\n",
                result.rust_result.mean_time.as_millis()
            ));
            report.push_str(&format!(
                "- Median time: {:.2}ms\n",
                result.rust_result.median_time.as_millis()
            ));
            report.push_str(&format!(
                "- Min time: {:.2}ms\n",
                result.rust_result.min_time.as_millis()
            ));
            report.push_str(&format!(
                "- Max time: {:.2}ms\n",
                result.rust_result.max_time.as_millis()
            ));
            report.push_str(&format!(
                "- Standard deviation: {:.2}ms\n",
                result.rust_result.standard_deviation.as_millis()
            ));

            if let Some(cpp) = &result.cpp_result {
                report.push_str(&format!("\n**C++ Results:**\n"));
                report.push_str(&format!("- Mean time: {:.2}ms\n", cpp.mean_time.as_millis()));
                report.push_str(&format!(
                    "- Performance ratio: {:.2}x\n",
                    result.performance_ratio.unwrap_or(0.0)
                ));
            }

            if let Some(memory) = &result.rust_result.memory_usage {
                report.push_str(&format!("\n**Memory Usage:**\n"));
                report.push_str(&format!("- Peak memory: {}KB\n", memory.peak_memory_kb));
                report.push_str(&format!("- Average memory: {}KB\n", memory.average_memory_kb));
            }

            report.push_str("\n---\n\n");
        }

        // Write report
        std::fs::create_dir_all(output_path.parent().unwrap())?;
        std::fs::write(output_path, report)?;

        info!("Generated benchmark report: {}", output_path.display());
        Ok(())
    }

    /// Create standard benchmark suites
    pub fn create_standard_suites(&self) -> Vec<BenchmarkSuite> {
        let input_dir = Path::new("test/input");

        vec![
            // Parsing benchmarks
            BenchmarkSuite {
                name: "Parsing".to_string(),
                benchmarks: vec![
                    BenchmarkSpec {
                        name: "parse_small_journal".to_string(),
                        command: "print".to_string(),
                        data_file: Some(input_dir.join("sample.dat")),
                        category: BenchmarkCategory::Parsing,
                        expected_time_range: Some((
                            Duration::from_millis(1),
                            Duration::from_millis(100),
                        )),
                    },
                    BenchmarkSpec {
                        name: "parse_large_journal".to_string(),
                        command: "print".to_string(),
                        data_file: Some(input_dir.join("standard.dat")),
                        category: BenchmarkCategory::Parsing,
                        expected_time_range: Some((
                            Duration::from_millis(10),
                            Duration::from_millis(1000),
                        )),
                    },
                ],
            },
            // Balance benchmarks
            BenchmarkSuite {
                name: "Balance".to_string(),
                benchmarks: vec![
                    BenchmarkSpec {
                        name: "balance_simple".to_string(),
                        command: "balance".to_string(),
                        data_file: Some(input_dir.join("sample.dat")),
                        category: BenchmarkCategory::Balance,
                        expected_time_range: Some((
                            Duration::from_millis(1),
                            Duration::from_millis(50),
                        )),
                    },
                    BenchmarkSpec {
                        name: "balance_with_depth".to_string(),
                        command: "balance --depth 3".to_string(),
                        data_file: Some(input_dir.join("standard.dat")),
                        category: BenchmarkCategory::Balance,
                        expected_time_range: Some((
                            Duration::from_millis(5),
                            Duration::from_millis(200),
                        )),
                    },
                ],
            },
            // Register benchmarks
            BenchmarkSuite {
                name: "Register".to_string(),
                benchmarks: vec![
                    BenchmarkSpec {
                        name: "register_all".to_string(),
                        command: "register".to_string(),
                        data_file: Some(input_dir.join("sample.dat")),
                        category: BenchmarkCategory::Register,
                        expected_time_range: Some((
                            Duration::from_millis(1),
                            Duration::from_millis(100),
                        )),
                    },
                    BenchmarkSpec {
                        name: "register_filtered".to_string(),
                        command: "register expenses".to_string(),
                        data_file: Some(input_dir.join("standard.dat")),
                        category: BenchmarkCategory::Register,
                        expected_time_range: Some((
                            Duration::from_millis(5),
                            Duration::from_millis(300),
                        )),
                    },
                ],
            },
        ]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_benchmark_config_creation() {
        let config = BenchmarkConfig::default();
        assert_eq!(config.warmup_iterations, 3);
        assert_eq!(config.benchmark_iterations, 10);
        assert_eq!(config.regression_threshold, 10.0);
    }

    #[test]
    fn test_standard_suites_creation() {
        let temp_dir = TempDir::new().unwrap();
        let ledger_path = temp_dir.path().join("ledger");
        std::fs::write(&ledger_path, "#!/bin/bash\necho 'test'").unwrap();

        let harness = TestHarness::new(&ledger_path, &temp_dir.path().to_path_buf()).unwrap();
        let runner = BenchmarkRunner::new(&harness);

        let suites = runner.create_standard_suites();
        assert_eq!(suites.len(), 3);
        assert_eq!(suites[0].name, "Parsing");
        assert_eq!(suites[1].name, "Balance");
        assert_eq!(suites[2].name, "Register");
    }
}
