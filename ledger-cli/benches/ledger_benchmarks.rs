//! Criterion benchmarks for ledger performance testing
//!
//! This file defines microbenchmarks using the Criterion framework for precise
//! performance measurement with statistical analysis.

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use std::path::PathBuf;
use std::time::Duration;
use tempfile::TempDir;

use ledger_cli::test_framework::test_harness::TestHarness;

/// Create sample test data for benchmarking
fn create_test_data() -> (TempDir, PathBuf, PathBuf) {
    let temp_dir = TempDir::new().unwrap();

    // Create a mock ledger binary for testing
    let ledger_path = temp_dir.path().join("ledger");
    std::fs::write(&ledger_path, "#!/bin/bash\necho 'mock ledger output'").unwrap();

    // Create sample journal data
    let journal_path = temp_dir.path().join("sample.dat");
    let sample_data = r#"
2023-01-01 Opening Balances
    Assets:Checking        $1,000.00
    Equity:Opening-Balances

2023-01-02 Grocery Store
    Expenses:Food           $45.67
    Assets:Checking

2023-01-03 Gas Station
    Expenses:Auto:Fuel      $32.10
    Assets:Checking

2023-01-04 Salary
    Assets:Checking      $2,500.00
    Income:Salary

2023-01-05 Rent
    Expenses:Housing:Rent   $800.00
    Assets:Checking

2023-01-06 Utilities
    Expenses:Housing:Electric  $89.45
    Expenses:Housing:Gas       $65.23
    Assets:Checking
"#;
    std::fs::write(&journal_path, sample_data).unwrap();

    (temp_dir, ledger_path, journal_path)
}

/// Benchmark command parsing and execution
fn benchmark_command_execution(c: &mut Criterion) {
    let (_temp_dir, ledger_path, journal_path) = create_test_data();
    let harness =
        TestHarness::new(&ledger_path, &ledger_path.parent().unwrap().to_path_buf()).unwrap();

    let mut group = c.benchmark_group("command_execution");

    // Benchmark different command types
    let commands = vec![
        ("balance", format!("balance -f \"{}\"", journal_path.display())),
        ("register", format!("register -f \"{}\"", journal_path.display())),
        ("print", format!("print -f \"{}\"", journal_path.display())),
    ];

    for (name, command) in commands {
        group.bench_with_input(BenchmarkId::new("execute", name), &command, |b, cmd| {
            b.iter(|| {
                let result = harness.run_command(black_box(cmd), true);
                black_box(result)
            })
        });
    }

    group.finish();
}

/// Benchmark output parsing and normalization
fn benchmark_output_processing(c: &mut Criterion) {
    let (_temp_dir, ledger_path, _journal_path) = create_test_data();
    let harness =
        TestHarness::new(&ledger_path, &ledger_path.parent().unwrap().to_path_buf()).unwrap();

    // Create sample outputs of different sizes
    let small_output = "Assets:Checking  $1,000.00\nIncome:Salary   $-2,500.00\n";
    let medium_output = small_output.repeat(50);
    let large_output = small_output.repeat(1000);

    let mut group = c.benchmark_group("output_processing");

    for (name, output) in [
        ("small", small_output),
        ("medium", medium_output.as_str()),
        ("large", large_output.as_str()),
    ] {
        group.bench_with_input(BenchmarkId::new("normalize", name), output, |b, output| {
            b.iter(|| {
                let result = harness.normalize_output(black_box(output));
                black_box(result)
            })
        });
    }

    group.finish();
}

/// Benchmark test harness creation and configuration
fn benchmark_harness_setup(c: &mut Criterion) {
    let temp_dir = TempDir::new().unwrap();
    let ledger_path = temp_dir.path().join("ledger");
    std::fs::write(&ledger_path, "#!/bin/bash\necho 'test'").unwrap();

    c.bench_function("harness_creation", |b| {
        b.iter(|| {
            let harness = TestHarness::new(
                black_box(&ledger_path),
                black_box(&temp_dir.path().to_path_buf()),
            );
            black_box(harness)
        })
    });

    // Benchmark harness configuration
    let harness = TestHarness::new(&ledger_path, &temp_dir.path().to_path_buf()).unwrap();

    c.bench_function("harness_configuration", |b| {
        b.iter(|| {
            let configured = black_box(harness.clone())
                .with_verify(black_box(true))
                .with_gmalloc(black_box(false))
                .with_python(black_box(true));
            black_box(configured)
        })
    });
}

/// Benchmark command parsing
fn benchmark_command_parsing(c: &mut Criterion) {
    let (_temp_dir, ledger_path, _journal_path) = create_test_data();
    let harness =
        TestHarness::new(&ledger_path, &ledger_path.parent().unwrap().to_path_buf()).unwrap();

    let mut group = c.benchmark_group("command_parsing");

    let commands = vec![
        ("simple", "balance"),
        ("with_options", "balance --depth 3 --real --cleared"),
        ("complex", "register --begin 2023-01-01 --end 2023-12-31 --monthly --format \"%d %p %l\""),
        ("with_query", "balance expenses and not food"),
    ];

    for (name, command) in commands {
        group.bench_with_input(BenchmarkId::new("parse", name), command, |b, cmd| {
            b.iter(|| {
                // This would benchmark the actual command parsing logic
                // For now, we'll benchmark string manipulation as a proxy
                let result = harness.substitute_variables(black_box(cmd));
                black_box(result)
            })
        });
    }

    group.finish();
}

/// Benchmark different journal sizes
fn benchmark_journal_sizes(c: &mut Criterion) {
    let temp_dir = TempDir::new().unwrap();
    let ledger_path = temp_dir.path().join("ledger");
    std::fs::write(&ledger_path, "#!/bin/bash\necho 'mock output'").unwrap();

    let harness =
        TestHarness::new(&ledger_path, &ledger_path.parent().unwrap().to_path_buf()).unwrap();

    let mut group = c.benchmark_group("journal_sizes");
    group.measurement_time(Duration::from_secs(30)); // Longer measurement time for large data

    // Generate journals of different sizes
    let base_transaction = r#"
2023-01-01 Transaction
    Expenses:Test    $10.00
    Assets:Checking
"#;

    let sizes = vec![("tiny", 10), ("small", 100), ("medium", 1000), ("large", 5000)];

    for (name, transaction_count) in sizes {
        let journal_content = base_transaction.repeat(transaction_count);
        let journal_path = temp_dir.path().join(format!("{}.dat", name));
        std::fs::write(&journal_path, &journal_content).unwrap();

        let command = format!("balance -f \"{}\"", journal_path.display());

        group.bench_with_input(BenchmarkId::new("process", name), &command, |b, cmd| {
            b.iter(|| {
                let result = harness.run_command(black_box(cmd), true);
                black_box(result)
            })
        });
    }

    group.finish();
}

/// Benchmark memory usage patterns
fn benchmark_memory_patterns(c: &mut Criterion) {
    let (_temp_dir, ledger_path, journal_path) = create_test_data();
    let harness =
        TestHarness::new(&ledger_path, &ledger_path.parent().unwrap().to_path_buf()).unwrap();

    let mut group = c.benchmark_group("memory_patterns");

    // Benchmark different operations that might have different memory patterns
    let operations = vec![
        ("balance_calculation", format!("balance -f \"{}\"", journal_path.display())),
        ("register_query", format!("register -f \"{}\" expenses", journal_path.display())),
        ("print_format", format!("print -f \"{}\"", journal_path.display())),
    ];

    for (name, command) in operations {
        group.bench_with_input(BenchmarkId::new("memory", name), &command, |b, cmd| {
            b.iter(|| {
                let result = harness.run_command(black_box(cmd), true);
                black_box(result)
            })
        });
    }

    group.finish();
}

criterion_group!(
    benches,
    benchmark_command_execution,
    benchmark_output_processing,
    benchmark_harness_setup,
    benchmark_command_parsing,
    benchmark_journal_sizes,
    benchmark_memory_patterns
);

criterion_main!(benches);
