#!/bin/bash
# Performance profiling setup script for Ledger Rust implementation

set -e

echo "Setting up performance profiling tools for Ledger..."

# Check operating system
OS=$(uname -s)
echo "Detected OS: $OS"

# Install cargo profiling tools
echo "Installing Rust profiling tools..."
cargo install cargo-flamegraph || echo "cargo-flamegraph already installed"
cargo install cargo-profile || echo "cargo-profile already installed"
cargo install cargo-llvm-lines || echo "cargo-llvm-lines already installed"
cargo install cargo-bloat || echo "cargo-bloat already installed"

# Platform-specific tool installation
if [[ "$OS" == "Darwin" ]]; then
    echo "Setting up macOS profiling tools..."
    
    # Check if Xcode Command Line Tools are installed
    if ! xcode-select -p &>/dev/null; then
        echo "Installing Xcode Command Line Tools..."
        xcode-select --install
    fi
    
    # Install Homebrew tools
    if command -v brew &>/dev/null; then
        echo "Installing profiling tools via Homebrew..."
        brew install valgrind || echo "Note: Valgrind may not work on newer macOS versions"
        brew install hyperfine
    else
        echo "Homebrew not found. Please install manually:"
        echo "  - Instruments (part of Xcode)"
        echo "  - Hyperfine: https://github.com/sharkdp/hyperfine"
    fi
    
elif [[ "$OS" == "Linux" ]]; then
    echo "Setting up Linux profiling tools..."
    
    # Try to install with common package managers
    if command -v apt-get &>/dev/null; then
        echo "Installing via apt-get..."
        sudo apt-get update
        sudo apt-get install -y \
            valgrind \
            perf-tools \
            linux-tools-common \
            linux-tools-generic \
            heaptrack \
            hyperfine
    elif command -v yum &>/dev/null; then
        echo "Installing via yum..."
        sudo yum install -y \
            valgrind \
            perf \
            hyperfine
    elif command -v pacman &>/dev/null; then
        echo "Installing via pacman..."
        sudo pacman -S --noconfirm \
            valgrind \
            perf \
            heaptrack \
            hyperfine
    else
        echo "Please install manually:"
        echo "  - valgrind"
        echo "  - perf"
        echo "  - heaptrack"
        echo "  - hyperfine"
    fi
fi

# Create profiling directories
echo "Creating profiling directories..."
mkdir -p benchmark_results/flamegraphs
mkdir -p benchmark_results/memory_profiles
mkdir -p benchmark_results/baseline
mkdir -p benchmark_results/reports
mkdir -p test/input/large

# Create test data for benchmarks
echo "Generating test data..."

# Small test data (already exists in sample.dat)
# Create medium test data
cat > test/input/large/medium.dat << 'EOF'
; Medium-sized test journal for performance testing
; Contains 500 transactions across multiple accounts

2023-01-01 Opening Balances
    Assets:Checking        $5,000.00
    Assets:Savings         $10,000.00
    Equity:Opening-Balances

EOF

# Generate 500 transactions
for i in {1..500}; do
    date=$(date -d "2023-01-01 + $i days" "+%Y-%m-%d" 2>/dev/null || gdate -d "2023-01-01 + $i days" "+%Y-%m-%d" 2>/dev/null || echo "2023-$(printf '%02d' $((i / 28 + 1)))-$(printf '%02d' $((i % 28 + 1)))")
    amount=$(echo "scale=2; ($RANDOM % 10000) / 100" | bc)
    cat >> test/input/large/medium.dat << EOF

$date Transaction $i
    Expenses:Random        \$$amount
    Assets:Checking

EOF
done

# Create large test data  
echo "Generating large test data (this may take a moment)..."
cat > test/input/large/large.dat << 'EOF'
; Large test journal for stress testing
; Contains 5000 transactions across multiple accounts

2023-01-01 Opening Balances
    Assets:Checking        $50,000.00
    Assets:Savings         $100,000.00
    Assets:Investment:Stocks  $25,000.00
    Assets:Investment:Bonds   $15,000.00
    Equity:Opening-Balances

EOF

# Generate 5000 transactions
for i in {1..5000}; do
    if (( i % 500 == 0 )); then
        echo "Generated $i/5000 transactions..."
    fi
    
    day=$((i % 365 + 1))
    date=$(date -d "2023-01-01 + $day days" "+%Y-%m-%d" 2>/dev/null || gdate -d "2023-01-01 + $day days" "+%Y-%m-%d" 2>/dev/null || echo "2023-$(printf '%02d' $((day / 28 + 1)))-$(printf '%02d' $((day % 28 + 1)))")
    amount=$(echo "scale=2; ($RANDOM % 100000) / 100" | bc)
    
    # Random account selection
    accounts=("Expenses:Food" "Expenses:Housing:Rent" "Expenses:Transportation" "Expenses:Entertainment" "Expenses:Healthcare" "Income:Salary" "Income:Investment")
    account=${accounts[$((RANDOM % ${#accounts[@]}))]}
    
    cat >> test/input/large/large.dat << EOF

$date Transaction $i
    $account        \$$amount
    Assets:Checking

EOF
done

echo "Generated test data complete."

# Create benchmark configuration
cat > benchmark_results/config.json << 'EOF'
{
  "warmup_iterations": 3,
  "benchmark_iterations": 10,
  "timeout_seconds": 60,
  "profile_memory": true,
  "compare_with_cpp": false,
  "regression_threshold": 10.0,
  "test_files": {
    "small": "test/input/sample.dat",
    "medium": "test/input/large/medium.dat", 
    "large": "test/input/large/large.dat"
  },
  "commands": [
    "balance",
    "register",
    "print",
    "balance --depth 3",
    "register expenses"
  ]
}
EOF

# Create profiling helper scripts
cat > scripts/profile-memory.sh << 'EOF'
#!/bin/bash
# Memory profiling helper script

if [[ "$#" -lt 1 ]]; then
    echo "Usage: $0 <command> [args...]"
    exit 1
fi

OS=$(uname -s)
TIMESTAMP=$(date +%Y%m%d_%H%M%S)

if [[ "$OS" == "Darwin" ]]; then
    echo "Running memory profiling on macOS..."
    # Use leaks and malloc debugging
    export MallocStackLogging=1
    export MallocScribble=1
    "$@"
    echo "Memory profile complete. Check system logs for malloc info."
elif [[ "$OS" == "Linux" ]]; then
    if command -v valgrind &>/dev/null; then
        echo "Running valgrind memory profiling..."
        valgrind --tool=massif --massif-out-file=benchmark_results/memory_profiles/massif_$TIMESTAMP.out "$@"
        ms_print benchmark_results/memory_profiles/massif_$TIMESTAMP.out > benchmark_results/memory_profiles/massif_$TIMESTAMP.txt
        echo "Memory profile saved to benchmark_results/memory_profiles/massif_$TIMESTAMP.txt"
    else
        echo "Valgrind not available. Running basic memory tracking..."
        /usr/bin/time -v "$@" 2>&1 | tee benchmark_results/memory_profiles/time_$TIMESTAMP.txt
    fi
fi
EOF

chmod +x scripts/profile-memory.sh

cat > scripts/profile-cpu.sh << 'EOF'
#!/bin/bash
# CPU profiling helper script

if [[ "$#" -lt 1 ]]; then
    echo "Usage: $0 <command> [args...]"
    exit 1
fi

TIMESTAMP=$(date +%Y%m%d_%H%M%S)

echo "Running CPU profiling..."
cargo flamegraph --output benchmark_results/flamegraphs/flamegraph_$TIMESTAMP.svg -- "$@"
echo "Flamegraph saved to benchmark_results/flamegraphs/flamegraph_$TIMESTAMP.svg"
EOF

chmod +x scripts/profile-cpu.sh

cat > scripts/run-benchmarks.sh << 'EOF'
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
EOF

chmod +x scripts/run-benchmarks.sh

# Create gitignore for benchmark results
cat > benchmark_results/.gitignore << 'EOF'
# Benchmark result files
*.json
*.svg
*.txt
*.out
flamegraphs/
memory_profiles/
reports/
baseline/

# Keep structure
!.gitignore
!config.json
EOF

echo "Profiling infrastructure setup complete!"
echo ""
echo "Available tools:"
echo "  - scripts/run-benchmarks.sh    - Run comprehensive benchmarks"
echo "  - scripts/profile-cpu.sh       - CPU profiling with flamegraphs"
echo "  - scripts/profile-memory.sh    - Memory profiling"
echo "  - cargo bench                  - Run Criterion benchmarks"
echo "  - cargo flamegraph             - Generate flamegraphs"
echo ""
echo "Test data created:"
echo "  - test/input/sample.dat        - Small test data (existing)"
echo "  - test/input/large/medium.dat  - Medium test data (500 transactions)"
echo "  - test/input/large/large.dat   - Large test data (5000 transactions)"
echo ""
echo "Configuration:"
echo "  - benchmark_results/config.json - Benchmark configuration"
echo ""
echo "To get started:"
echo "  ./scripts/run-benchmarks.sh"