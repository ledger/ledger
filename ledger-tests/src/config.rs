//! Configuration and settings for the test framework

use std::path::PathBuf;
use serde::{Deserialize, Serialize};
use clap::{Parser, ValueEnum};

/// Test category types matching the Python harness
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, ValueEnum)]
pub enum TestCategory {
    /// Baseline functional tests
    Baseline,
    /// Regression tests for bug fixes
    Regress,
    /// Manual/documentation tests
    Manual,
    /// Unit tests
    Unit,
    /// Python API tests
    Python,
}

impl TestCategory {
    /// Get the directory name for this test category
    pub fn directory(&self) -> &'static str {
        match self {
            Self::Baseline => "baseline",
            Self::Regress => "regress", 
            Self::Manual => "manual",
            Self::Unit => "unit",
            Self::Python => "python",
        }
    }
}

/// Test execution mode
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, ValueEnum)]
pub enum ExecutionMode {
    /// Run tests sequentially
    Sequential,
    /// Run tests in parallel
    Parallel,
}

/// Memory debugging options (equivalent to Python gmalloc flags)
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct MemoryDebugConfig {
    pub enabled: bool,
    pub guard_edges: bool,
    pub scribble: bool,
    pub pre_scribble: bool,
    pub heap_start_check: u32,
    pub heap_each_check: u32,
    pub protect_before: bool,
    pub fill_space: bool,
    pub strict_size: bool,
}

impl Default for MemoryDebugConfig {
    fn default() -> Self {
        Self {
            enabled: false,
            guard_edges: true,
            scribble: true,
            pre_scribble: true,
            heap_start_check: 1000,
            heap_each_check: 10000,
            protect_before: true,
            fill_space: true,
            strict_size: true,
        }
    }
}

/// Configuration for the test framework
#[derive(Debug, Clone, Serialize, Deserialize, Parser)]
#[command(name = "ledger-tests")]
#[command(about = "Ledger Rust Test Framework")]
pub struct TestConfig {
    /// Path to the ledger binary to test
    #[arg(short, long)]
    pub ledger_path: PathBuf,
    
    /// Path to the source directory containing tests
    #[arg(short, long)]
    pub source_path: PathBuf,
    
    /// Categories of tests to run
    #[arg(long, value_enum, value_delimiter = ',', default_values_t = [TestCategory::Baseline, TestCategory::Regress])]
    pub categories: Vec<TestCategory>,
    
    /// Specific test files or patterns to run
    #[arg(long)]
    pub tests: Vec<String>,
    
    /// Enable verification mode (equivalent to --verify flag)
    #[arg(long)]
    pub verify: bool,
    
    /// Enable memory debugging (equivalent to gmalloc)
    #[arg(long)]
    pub memory_debug: bool,
    
    /// Enable Python mode for Python tests
    #[arg(long)]
    pub python: bool,
    
    /// Execution mode
    #[arg(long, value_enum, default_value_t = ExecutionMode::Parallel)]
    pub execution_mode: ExecutionMode,
    
    /// Number of parallel jobs (0 = auto-detect)
    #[arg(short, long, default_value_t = 0)]
    pub jobs: usize,
    
    /// Timeout for individual tests in seconds
    #[arg(long, default_value_t = 60)]
    pub timeout: u64,
    
    /// Column width for ledger output
    #[arg(long, default_value_t = 80)]
    pub columns: u32,
    
    /// Enable verbose output
    #[arg(short, long)]
    pub verbose: bool,
    
    /// Enable quiet mode (minimal output)
    #[arg(short, long)]
    pub quiet: bool,
    
    /// Continue running tests after failures
    #[arg(long)]
    pub continue_on_failure: bool,
    
    /// Generate coverage reports
    #[arg(long)]
    pub coverage: bool,
    
    /// Generate performance benchmarks
    #[arg(long)]
    pub benchmark: bool,
    
    /// Memory debugging configuration (not exposed as CLI arg)
    #[clap(skip)]
    #[serde(default)]
    pub memory_config: MemoryDebugConfig,
}

impl TestConfig {
    /// Create a new configuration with sensible defaults
    pub fn new(ledger_path: PathBuf, source_path: PathBuf) -> Self {
        Self {
            ledger_path,
            source_path,
            categories: vec![TestCategory::Baseline, TestCategory::Regress],
            tests: Vec::new(),
            verify: false,
            memory_debug: false,
            python: false,
            execution_mode: ExecutionMode::Parallel,
            jobs: 0,
            timeout: 60,
            columns: 80,
            verbose: false,
            quiet: false,
            continue_on_failure: false,
            coverage: false,
            benchmark: false,
            memory_config: MemoryDebugConfig::default(),
        }
    }
    
    /// Get the number of parallel jobs to use
    pub fn effective_jobs(&self) -> usize {
        if self.jobs == 0 {
            // Auto-detect based on CPU count
            std::thread::available_parallelism()
                .map(|p| p.get())
                .unwrap_or(1)
        } else {
            self.jobs
        }
    }
    
    /// Check if we should run tests for the given category
    pub fn should_run_category(&self, category: TestCategory) -> bool {
        self.categories.contains(&category)
    }
    
    /// Get environment variables for memory debugging
    pub fn memory_env_vars(&self) -> Vec<(String, String)> {
        if !self.memory_debug || !self.memory_config.enabled {
            return Vec::new();
        }
        
        let mut env = Vec::new();
        
        if self.memory_config.guard_edges {
            env.push(("MallocGuardEdges".to_string(), "1".to_string()));
        }
        if self.memory_config.scribble {
            env.push(("MallocScribble".to_string(), "1".to_string()));
        }
        if self.memory_config.pre_scribble {
            env.push(("MallocPreScribble".to_string(), "1".to_string()));
        }
        if self.memory_config.heap_start_check > 0 {
            env.push(("MallocCheckHeapStart".to_string(), 
                     self.memory_config.heap_start_check.to_string()));
        }
        if self.memory_config.heap_each_check > 0 {
            env.push(("MallocCheckHeapEach".to_string(), 
                     self.memory_config.heap_each_check.to_string()));
        }
        if self.memory_config.protect_before {
            env.push(("MALLOC_PROTECT_BEFORE".to_string(), "1".to_string()));
        }
        if self.memory_config.fill_space {
            env.push(("MALLOC_FILL_SPACE".to_string(), "1".to_string()));
        }
        if self.memory_config.strict_size {
            env.push(("MALLOC_STRICT_SIZE".to_string(), "1".to_string()));
        }
        
        // Add dylib path for macOS
        if cfg!(target_os = "macos") {
            env.push(("DYLD_INSERT_LIBRARIES".to_string(), 
                     "/usr/lib/libgmalloc.dylib".to_string()));
        }
        
        env
    }
    
    /// Validate the configuration
    pub fn validate(&self) -> Result<(), crate::TestError> {
        if !self.ledger_path.exists() {
            return Err(crate::TestError::Config(
                format!("Ledger binary not found at: {}", self.ledger_path.display())
            ));
        }
        
        if !self.source_path.exists() || !self.source_path.is_dir() {
            return Err(crate::TestError::Config(
                format!("Source path not found or not a directory: {}", self.source_path.display())
            ));
        }
        
        if self.timeout == 0 {
            return Err(crate::TestError::Config(
                "Timeout must be greater than 0".to_string()
            ));
        }
        
        Ok(())
    }
}