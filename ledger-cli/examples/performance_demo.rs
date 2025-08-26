//! Performance demonstration and benchmarking example
//!
//! This example showcases the performance optimizations implemented
//! and runs a comprehensive benchmark suite.

use ledger_core::performance_suite::{BenchmarkSuite, MemoryProfiler, SizeOptimizer};
use std::path::Path;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Ledger Rust Performance Demonstration ===\n");

    // Initialize benchmark suite
    let mut suite = BenchmarkSuite::new();

    // Run arithmetic benchmarks
    println!("Running arithmetic performance tests...");
    suite.benchmark_arithmetic()?;

    // Run report generation benchmarks  
    println!("Running report generation benchmarks...");
    suite.benchmark_report_generation()?;

    // Memory profiling demonstration
    println!("\nDemonstrating memory profiling...");
    let (result, memory_used) = MemoryProfiler::measure_peak_memory(|| {
        // Simulate memory-intensive operation
        let mut data = Vec::new();
        for i in 0..100_000 {
            data.push(format!("Transaction line {}", i));
        }
        data.len()
    });
    println!("Memory profiling result: {} items used {} bytes peak memory", 
             result, memory_used);

    // Binary size analysis (if binary exists)
    if let Ok(current_exe) = std::env::current_exe() {
        println!("\nAnalyzing binary size...");
        match SizeOptimizer::analyze_binary_size(&current_exe) {
            Ok(analysis) => println!("{}", analysis),
            Err(e) => println!("Size analysis failed: {}", e),
        }
    }

    // Generate comprehensive report
    println!("\n=== Performance Report ===");
    let report = suite.generate_report();
    println!("{}", report);

    // Save results
    let results_dir = Path::new("benchmark_results/performance_demo");
    std::fs::create_dir_all(results_dir)?;
    
    let timestamp = chrono::Utc::now().format("%Y%m%d_%H%M%S");
    let report_file = results_dir.join(format!("performance_demo_{}.md", timestamp));
    let json_file = results_dir.join(format!("performance_demo_{}.json", timestamp));
    
    suite.save_report(&report_file)?;
    suite.export_json(&json_file)?;

    println!("\n=== Optimization Summary ===");
    println!("✓ String interning and compact representations");
    println!("✓ Zero-copy parsing optimizations");  
    println!("✓ Parallel processing where beneficial");
    println!("✓ Optimized data structures (sparse vectors, arenas)");
    println!("✓ Intelligent caching with LRU eviction");
    println!("✓ SIMD-accelerated arithmetic operations");
    println!("✓ Link-time optimization and size reduction");

    println!("\nPerformance demonstration complete!");
    println!("Detailed results saved to: {:?}", results_dir);

    Ok(())
}