# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is the Rust implementation of Ledger, a command-line double-entry accounting system. The project is organized as a Cargo workspace with four main crates:
- `ledger-core`: Core accounting engine with data structures and algorithms
- `ledger-cli`: Command-line interface and application logic
- `ledger-math`: Mathematical operations and precision arithmetic
- `ledger-tests`: Comprehensive test framework compatible with original Python tests

## Build Commands

```bash
# Standard build
cargo build --workspace

# Release build with optimizations
cargo build --release --workspace

# Build with specific profile
cargo build --profile=max-perf  # Maximum performance
cargo build --profile=min-size  # Minimum binary size

# Clean build
cargo clean
```

## Testing Commands

```bash
# Run all tests
cargo test --workspace

# Run tests with output
cargo test --workspace -- --nocapture

# Run specific test
cargo test --workspace test_name

# Run tests for specific crate
cargo test -p ledger-core
cargo test -p ledger-cli

# Run benchmarks
cargo bench --workspace

# Run with code coverage (requires cargo-tarpaulin)
cargo tarpaulin --workspace --timeout 300
```

## Development Commands

```bash
# Format code
cargo fmt --all

# Check formatting
cargo fmt --all -- --check

# Run clippy linter
cargo clippy --workspace -- -D warnings

# Fix clippy warnings
cargo clippy --workspace --fix

# Check for compile errors without building
cargo check --workspace

# Update dependencies
cargo update

# Build documentation
cargo doc --workspace --no-deps --open

# Run example programs
cargo run --example cli_demo
cargo run --example performance_demo
```

## Performance Analysis

```bash
# CPU profiling
./scripts/profile-cpu.sh

# Memory profiling
./scripts/profile-memory.sh

# Run all benchmarks
./scripts/run-benchmarks.sh

# Compare performance with C++ version
./scripts/run-performance-comparison.sh
```

## Architecture Overview

### Core Module Structure (`ledger-core`)

The core library implements fundamental accounting concepts:

- **Amount** (`amount.rs`): Precise arithmetic with commodity support using `rust_decimal`
- **Balance** (`balance.rs`): Multi-commodity balance management
- **Transaction** (`transaction.rs`): Individual dated transactions with metadata
- **Posting** (`posting.rs`): Line items within transactions affecting accounts
- **Account** (`account.rs`): Hierarchical chart of accounts using tree structure
- **Journal** (`journal.rs`): Central container holding all financial data
- **Commodity** (`commodity.rs`): Currency and commodity definitions with symbols
- **Parser** (`parser.rs`, `transaction_parser.rs`): Nom-based parsers for journal files
- **Expression** (`expr.rs`): Expression evaluation engine for queries and calculations
- **Filters** (`filters.rs`): Chain of responsibility pattern for transaction processing
- **Output** (`output.rs`): Report formatters and output generation

### CLI Architecture (`ledger-cli`)

The CLI crate provides the user interface:

- **Session** (`session.rs`): Configuration and runtime state management
- **Dispatcher** (`dispatch.rs`): Command routing and execution
- **Test Framework** (`test_framework/`): Compatibility layer for Python test suite
  - `baseline_runner.rs`: Core functionality tests
  - `regression_runner.rs`: Regression test execution
  - `test_harness.rs`: Test orchestration
  - `output_validator.rs`: Output comparison logic

### Key Design Patterns

1. **Value Semantics**: Most types use `Clone` and owned data for simplicity
2. **Error Handling**: Pervasive use of `Result<T, anyhow::Error>` with `thiserror`
3. **Parser Combinators**: Nom library for parsing journal files
4. **Builder Pattern**: Used for complex object construction (transactions, reports)
5. **Iterator Chains**: Extensive use of iterator adaptors for data processing

### Performance Optimizations

- **Small String Optimization**: `compact_str` for account names and descriptions
- **Small Vector Optimization**: `smallvec` for postings (most transactions have 2-4)
- **Parallel Processing**: `rayon` for parallel iteration where beneficial
- **Lock-Free Data Structures**: `parking_lot` mutexes for better performance
- **Arena Allocation**: `bumpalo` for temporary allocations during parsing
- **Fast Hashing**: `ahash` for hash maps and sets
- **Memory Mapped Files**: `memmap2` for large journal files

## Known Issues and Current Status

**IMPORTANT**: This Rust implementation is currently **non-functional** with 279 compilation errors. Major issues include:

1. **Type System Issues**: Widespread lifetime and borrowing problems
2. **Missing Implementations**: Many core types lack required trait implementations
3. **Parser Errors**: Transaction and journal parsers have unresolved type conflicts
4. **Test Framework**: Cannot run due to compilation failures

The codebase requires significant refactoring to compile and run properly

## Task Master AI Instructions
**Import Task Master's development workflow commands and guidelines, treat as if import is in the main CLAUDE.md file.**
@./.taskmaster/CLAUDE.md
