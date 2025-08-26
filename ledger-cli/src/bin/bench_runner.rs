//! Benchmark Runner Binary
//!
//! This binary provides a command-line interface for running performance benchmarks
//! against the Rust ledger implementation, with optional comparison to C++ baseline.

use clap::Parser;
use anyhow::Result;
use std::path::PathBuf;
use log::{info, error};

use ledger_cli::test_framework::test_harness::TestHarness;
use ledger_cli::test_framework::bench_framework::{BenchmarkRunner, BenchmarkConfig, BenchmarkCategory};

#[derive(Parser)]
#[command(name = "bench_runner")]
#[command(about = "Run ledger performance benchmarks")]
struct Args {
    /// Path to the ledger binary
    #[arg(long, default_value = "./build/ledger")]
    ledger: PathBuf,

    /// Source path for the ledger project  
    #[arg(long, default_value = ".")]
    sourcepath: PathBuf,

    /// Path to C++ ledger binary for comparison
    #[arg(long)]
    cpp_ledger: Option<PathBuf>,

    /// Number of warmup iterations
    #[arg(long, default_value_t = 3)]
    warmup: u32,

    /// Number of benchmark iterations
    #[arg(long, default_value_t = 10)]
    iterations: u32,

    /// Enable memory profiling
    #[arg(long)]
    profile_memory: bool,

    /// Compare against C++ implementation
    #[arg(long)]
    compare_cpp: bool,

    /// Performance regression threshold (percentage)
    #[arg(long, default_value_t = 10.0)]
    regression_threshold: f64,

    /// Directory to store benchmark results
    #[arg(long, default_value = "benchmark_results")]
    results_dir: PathBuf,

    /// Load baseline results for regression detection
    #[arg(long)]
    baseline_file: Option<PathBuf>,

    /// Save results as new baseline
    #[arg(long)]
    save_baseline: bool,

    /// Generate HTML report
    #[arg(long)]
    html_report: bool,

    /// Benchmark category filter (parsing, balance, register, queries, reporting, math, memory)
    #[arg(long)]
    category: Option<String>,

    /// Run only specific benchmark by name
    #[arg(long)]
    benchmark: Option<String>,

    /// Generate flamegraph (requires perf and flamegraph tools)
    #[arg(long)]
    flamegraph: bool,

    /// Output file for benchmark report
    #[arg(long, default_value = "benchmark_report.md")]
    output: PathBuf,
}

fn main() -> Result<()> {
    // Initialize logging
    env_logger::init();

    let args = Args::parse();

    info!("Running benchmarks with ledger: {}", args.ledger.display());
    info!("Source path: {}", args.sourcepath.display());

    // Create test harness
    let harness = TestHarness::new(&args.ledger, &args.sourcepath)?;

    // Configure benchmark runner
    let config = BenchmarkConfig {
        warmup_iterations: args.warmup,
        benchmark_iterations: args.iterations,
        timeout_seconds: 120,
        profile_memory: args.profile_memory,
        compare_with_cpp: args.compare_cpp,
        cpp_ledger_path: args.cpp_ledger.clone(),
        results_dir: args.results_dir.clone(),
        regression_threshold: args.regression_threshold,
    };

    let mut runner = BenchmarkRunner::with_config(&harness, config);

    // Load baseline if specified
    if let Some(baseline_file) = &args.baseline_file {
        info!("Loading baseline from: {}", baseline_file.display());
        runner.load_baseline(baseline_file)?;
    }

    // Create benchmark suites
    let mut suites = runner.create_standard_suites();

    // Filter by category if specified
    if let Some(category_filter) = &args.category {
        let category = parse_category(category_filter)?;
        for suite in &mut suites {
            suite.benchmarks.retain(|b| b.category == category);
        }
        suites.retain(|s| !s.benchmarks.is_empty());
    }

    // Filter by specific benchmark name if specified
    if let Some(benchmark_name) = &args.benchmark {
        for suite in &mut suites {
            suite.benchmarks.retain(|b| b.name == *benchmark_name);
        }
        suites.retain(|s| !s.benchmarks.is_empty());
    }

    if suites.is_empty() {
        error!("No benchmarks to run after filtering");
        return Ok(());
    }

    // Run benchmarks
    let mut all_results = Vec::new();
    for suite in &suites {
        info!("Running benchmark suite: {}", suite.name);
        let results = runner.run_suite(suite)?;
        all_results.extend(results);
    }

    // Generate report
    let report_path = if args.html_report {
        args.output.clone().with_extension("html")
    } else {
        args.output.clone()
    };

    runner.generate_report(&all_results, &report_path)?;
    info!("Generated benchmark report: {}", report_path.display());

    // Save as baseline if requested
    if args.save_baseline {
        let baseline_path = args.results_dir.join("baseline.json");
        let rust_results: Vec<_> = all_results.iter()
            .map(|r| r.rust_result.clone())
            .collect();
        runner.save_baseline(&rust_results, &baseline_path)?;
    }

    // Generate flamegraph if requested
    if args.flamegraph {
        generate_flamegraph(&args)?;
    }

    // Print summary
    print_summary(&all_results);

    Ok(())
}

fn parse_category(category: &str) -> Result<BenchmarkCategory> {
    match category.to_lowercase().as_str() {
        "parsing" => Ok(BenchmarkCategory::Parsing),
        "balance" => Ok(BenchmarkCategory::Balance),
        "register" => Ok(BenchmarkCategory::Register),
        "queries" => Ok(BenchmarkCategory::Queries),
        "reporting" => Ok(BenchmarkCategory::Reporting),
        "math" => Ok(BenchmarkCategory::Math),
        "memory" => Ok(BenchmarkCategory::Memory),
        _ => anyhow::bail!("Invalid category: {}. Valid categories: parsing, balance, register, queries, reporting, math, memory", category),
    }
}

fn generate_flamegraph(args: &Args) -> Result<()> {
    info!("Generating flamegraph...");
    
    // This is a simplified implementation. In practice, you would:
    // 1. Run the benchmark with perf record
    // 2. Convert perf data to flamegraph format
    // 3. Generate SVG flamegraph
    
    let flamegraph_path = args.results_dir.join("flamegraph.svg");
    std::fs::create_dir_all(&args.results_dir)?;
    
    // Mock flamegraph generation
    let mock_svg = r#"<svg>
        <text x="10" y="20">Flamegraph generation requires perf and flamegraph tools</text>
        <text x="10" y="40">Install with: cargo install flamegraph</text>
        <text x="10" y="60">Then run: cargo flamegraph --bin ledger -- [args]</text>
    </svg>"#;
    
    std::fs::write(&flamegraph_path, mock_svg)?;
    info!("Generated flamegraph placeholder: {}", flamegraph_path.display());
    
    Ok(())
}

fn print_summary(results: &[ledger_cli::test_framework::bench_framework::ComparisonResult]) {
    println!("\n🚀 Benchmark Summary");
    println!("====================");
    
    let total_benchmarks = results.len();
    let regressions = results.iter().filter(|r| r.regression_detected).count();
    let with_cpp_comparison = results.iter().filter(|r| r.cpp_result.is_some()).count();
    
    println!("Total benchmarks: {}", total_benchmarks);
    println!("Regressions detected: {}", regressions);
    println!("C++ comparisons: {}", with_cpp_comparison);
    
    if !results.is_empty() {
        let avg_time = results.iter()
            .map(|r| r.rust_result.mean_time.as_millis() as f64)
            .sum::<f64>() / results.len() as f64;
        println!("Average execution time: {:.2}ms", avg_time);
        
        if with_cpp_comparison > 0 {
            let avg_ratio = results.iter()
                .filter_map(|r| r.performance_ratio)
                .sum::<f64>() / with_cpp_comparison as f64;
            println!("Average performance ratio (Rust/C++): {:.2}x", avg_ratio);
            
            if avg_ratio < 1.0 {
                println!("🎉 Rust is {:.1}% faster on average!", (1.0 - avg_ratio) * 100.0);
            } else if avg_ratio > 1.0 {
                println!("🐌 Rust is {:.1}% slower on average", (avg_ratio - 1.0) * 100.0);
            }
        }
    }
    
    if regressions > 0 {
        println!("\n⚠️  Performance Regressions Detected:");
        for result in results.iter().filter(|r| r.regression_detected) {
            println!("  - {}", result.rust_result.name);
        }
    } else {
        println!("\n✅ No performance regressions detected");
    }
}