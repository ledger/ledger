#!/bin/bash
# Main benchmarking script

set -e

echo "Running comprehensive Ledger benchmarks..."

# Ensure we're in the right directory
cd "$(dirname "$0")/.."

# Build optimized version
echo "Building optimized version..."
cargo build --release

# Run Criterion benchmarks
echo "Running Criterion benchmarks..."
cargo bench --bench ledger_benchmarks

# Run hyperfine comparisons if available
if command -v hyperfine &>/dev/null; then
    echo "Running hyperfine comparisons..."
    
    # Test different journal sizes
    for size in small medium large; do
        case $size in
            small) file="test/input/sample.dat" ;;
            medium) file="test/input/large/medium.dat" ;;
            large) file="test/input/large/large.dat" ;;
        esac
        
        echo "Benchmarking $size journal ($file)..."
        hyperfine --warmup 3 --min-runs 10 \
            --export-json "benchmark_results/hyperfine_${size}_$(date +%Y%m%d_%H%M%S).json" \
            "./target/release/ledger -f \"$file\" balance" \
            "./target/release/ledger -f \"$file\" register" \
            "./target/release/ledger -f \"$file\" print"
    done
fi

echo "Benchmarks complete. Results saved to benchmark_results/"
