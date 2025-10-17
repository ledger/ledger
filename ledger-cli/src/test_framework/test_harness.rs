//! Test Harness for Ledger Testing
//!
//! This module provides the core test harness functionality, equivalent to the Python
//! LedgerHarness.py. It handles running ledger commands, capturing output, and managing
//! test execution state.

use anyhow::{Context, Result};
use log::{debug, warn};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::{Duration, Instant};

/// Test execution statistics
#[derive(Debug, Default)]
pub struct TestStats {
    pub total: AtomicUsize,
    pub passed: AtomicUsize,
    pub failed: AtomicUsize,
    pub skipped: AtomicUsize,
    pub start_time: Option<Instant>,
}

impl Clone for TestStats {
    fn clone(&self) -> Self {
        Self {
            total: AtomicUsize::new(self.total.load(std::sync::atomic::Ordering::SeqCst)),
            passed: AtomicUsize::new(self.passed.load(std::sync::atomic::Ordering::SeqCst)),
            failed: AtomicUsize::new(self.failed.load(std::sync::atomic::Ordering::SeqCst)),
            skipped: AtomicUsize::new(self.skipped.load(std::sync::atomic::Ordering::SeqCst)),
            start_time: self.start_time,
        }
    }
}

impl TestStats {
    pub fn new() -> Self {
        Self { start_time: Some(Instant::now()), ..Default::default() }
    }

    pub fn success(&self) {
        self.passed.fetch_add(1, Ordering::SeqCst);
        self.total.fetch_add(1, Ordering::SeqCst);
    }

    pub fn failure(&self, test_name: &str) {
        self.failed.fetch_add(1, Ordering::SeqCst);
        self.total.fetch_add(1, Ordering::SeqCst);
        warn!("FAILED: {}", test_name);
    }

    pub fn skipped(&self) {
        self.skipped.fetch_add(1, Ordering::SeqCst);
        self.total.fetch_add(1, Ordering::SeqCst);
    }

    pub fn print_summary(&self) {
        let total = self.total.load(Ordering::SeqCst);
        let passed = self.passed.load(Ordering::SeqCst);
        let failed = self.failed.load(Ordering::SeqCst);
        let skipped = self.skipped.load(Ordering::SeqCst);

        let elapsed =
            self.start_time.map(|start| start.elapsed()).unwrap_or_else(|| Duration::from_secs(0));

        println!("\nTest Summary:");
        println!("=============");
        println!("Total:   {}", total);
        println!("Passed:  {} ({:.1}%)", passed, (passed as f64 / total as f64) * 100.0);
        println!("Failed:  {} ({:.1}%)", failed, (failed as f64 / total as f64) * 100.0);
        println!("Skipped: {} ({:.1}%)", skipped, (skipped as f64 / total as f64) * 100.0);
        println!("Time:    {:.2}s", elapsed.as_secs_f64());

        if failed > 0 {
            std::process::exit(1);
        }
    }
}

/// Test harness for running ledger commands
#[derive(Clone)]
pub struct TestHarness {
    pub ledger_path: PathBuf,
    pub source_path: PathBuf,
    pub verify_mode: bool,
    pub gmalloc: bool,
    pub python_support: bool,
    pub stats: TestStats,
}

/// Process execution result
pub struct ProcessResult {
    pub stdout: String,
    pub stderr: String,
    pub exit_code: i32,
    pub duration: Duration,
}

impl TestHarness {
    /// Create a new test harness
    pub fn new<P: AsRef<Path>>(ledger_path: P, source_path: P) -> Result<Self> {
        let ledger_path = ledger_path.as_ref().to_path_buf();
        let source_path = source_path.as_ref().to_path_buf();

        // Verify ledger binary exists
        if !ledger_path.exists() {
            anyhow::bail!("Ledger binary not found: {}", ledger_path.display());
        }

        Ok(TestHarness {
            ledger_path,
            source_path,
            verify_mode: false,
            gmalloc: false,
            python_support: false,
            stats: TestStats::new(),
        })
    }

    /// Set verification mode
    pub fn with_verify(mut self, verify: bool) -> Self {
        self.verify_mode = verify;
        self
    }

    /// Set gmalloc usage
    pub fn with_gmalloc(mut self, gmalloc: bool) -> Self {
        self.gmalloc = gmalloc;
        self
    }

    /// Set python support
    pub fn with_python(mut self, python: bool) -> Self {
        self.python_support = python;
        self
    }

    /// Run a ledger command with given arguments
    pub fn run_command(&self, command: &str, add_columns: bool) -> Result<ProcessResult> {
        let start_time = Instant::now();

        // Replace variables in command
        let command = self.substitute_variables(command);

        debug!("Running command: {}", command);

        // Parse command into arguments
        let args = self.parse_command(&command)?;

        let mut cmd = Command::new(&self.ledger_path);
        cmd.args(&args).stdout(Stdio::piped()).stderr(Stdio::piped()).stdin(Stdio::piped());

        // Add columns option if requested and not already present
        if add_columns && !command.contains("--columns") {
            cmd.env("COLUMNS", "80");
        }

        // Set environment for consistent test runs
        cmd.env("TZ", "America/Chicago");
        cmd.env("LC_ALL", "C");

        let output = cmd
            .output()
            .with_context(|| format!("Failed to execute ledger command: {}", command))?;

        let duration = start_time.elapsed();

        Ok(ProcessResult {
            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
            exit_code: output.status.code().unwrap_or(-1),
            duration,
        })
    }

    /// Substitute variables in command string
    pub fn substitute_variables(&self, command: &str) -> String {
        command
            .replace("$ledger", &self.ledger_path.to_string_lossy())
            .replace("$sourcepath", &self.source_path.to_string_lossy())
    }

    /// Parse command string into arguments
    fn parse_command(&self, command: &str) -> Result<Vec<String>> {
        let mut args = Vec::new();
        let mut current_arg = String::new();
        let mut in_quotes = false;
        let mut escape_next = false;

        for ch in command.chars() {
            if escape_next {
                current_arg.push(ch);
                escape_next = false;
            } else {
                match ch {
                    '\\' => escape_next = true,
                    '"' => in_quotes = !in_quotes,
                    ' ' if !in_quotes => {
                        if !current_arg.is_empty() {
                            args.push(current_arg);
                            current_arg = String::new();
                        }
                    }
                    _ => current_arg.push(ch),
                }
            }
        }

        if !current_arg.is_empty() {
            args.push(current_arg);
        }

        Ok(args)
    }

    /// Convert output lines for cross-platform compatibility
    pub fn normalize_output(&self, output: &str) -> Vec<String> {
        output
            .lines()
            .map(|line| {
                #[cfg(windows)]
                let line = line.replace('\r', "");
                #[cfg(windows)]
                let line = line.replace('\\', "/");

                line.to_string()
            })
            .collect()
    }

    /// Record test success
    pub fn success(&self) {
        self.stats.success();
    }

    /// Record test failure
    pub fn failure(&self, test_name: &str) {
        self.stats.failure(test_name);
    }

    /// Record test skip
    pub fn skipped(&self) {
        self.stats.skipped();
    }

    /// Print final test summary and exit
    pub fn exit(&self) -> ! {
        self.stats.print_summary();
        std::process::exit(0);
    }
}
