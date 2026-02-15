#!/usr/bin/env bash
# Clang profiling workflow helper for Ledger.
#
# Usage:
#   ./benchmark/profile.sh generate <build-dir>   # Run workloads, collect profiles
#   ./benchmark/profile.sh report <build-dir>      # Generate human-readable report
#   ./benchmark/profile.sh pgo <build-dir>         # Rebuild with PGO using collected data
#
# Prerequisites:
#   - Build with: cmake -B build -DCMAKE_BUILD_TYPE=Release -DUSE_CLANG_PROFILING=ON
#   - Or:         ./acprep prof make
#
# The generate step runs representative ledger commands and produces a merged
# .profdata file.  The report step converts that into annotated source.  The
# pgo step rebuilds the project using the collected profile data.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
BENCHMARK_DAT="$SCRIPT_DIR/benchmark.dat"

# These tools must be on PATH.  The Nix dev shell (flake.nix) provides them
# via llvmPackages.  Outside Nix, install the llvm/clang package for your OS.
for tool in llvm-profdata llvm-cov; do
    if ! command -v "$tool" &>/dev/null; then
        echo "Error: $tool not found on PATH." >&2
        echo "Enter the Nix dev shell (nix develop) or install LLVM tools." >&2
        exit 1
    fi
done

usage() {
    echo "Usage: $0 {generate|report|pgo} <build-dir>"
    exit 1
}

ACTION="${1:-}"
BUILD_DIR="${2:-}"

if [ -z "$ACTION" ] || [ -z "$BUILD_DIR" ]; then
    usage
fi

BUILD_DIR="$(cd "$BUILD_DIR" 2>/dev/null && pwd)" || {
    echo "Error: build directory not found: $2" >&2
    exit 1
}

LEDGER="$BUILD_DIR/ledger"
PROFRAW_DIR="$BUILD_DIR/profiles"
PROFDATA="$BUILD_DIR/default.profdata"

if [ ! -x "$LEDGER" ]; then
    echo "Error: ledger binary not found at $LEDGER" >&2
    echo "Build the project first with USE_CLANG_PROFILING=ON" >&2
    exit 1
fi

# Generate benchmark data if needed
if [ ! -f "$BENCHMARK_DAT" ]; then
    echo "Generating benchmark data..."
    python3 "$SCRIPT_DIR/gen_benchmark.py"
fi

case "$ACTION" in
generate)
    echo "=== Collecting profile data ==="
    mkdir -p "$PROFRAW_DIR"
    rm -f "$PROFRAW_DIR"/*.profraw

    export LLVM_PROFILE_FILE="$PROFRAW_DIR/ledger-%p-%m.profraw"

    echo "Running balance..."
    "$LEDGER" -f "$BENCHMARK_DAT" balance >/dev/null
    echo "Running register..."
    "$LEDGER" -f "$BENCHMARK_DAT" register >/dev/null
    echo "Running print..."
    "$LEDGER" -f "$BENCHMARK_DAT" print >/dev/null
    echo "Running csv..."
    "$LEDGER" -f "$BENCHMARK_DAT" csv >/dev/null
    echo "Running equity..."
    "$LEDGER" -f "$BENCHMARK_DAT" equity >/dev/null
    echo "Running stats..."
    "$LEDGER" -f "$BENCHMARK_DAT" stats >/dev/null
    echo "Running balance --depth 2..."
    "$LEDGER" -f "$BENCHMARK_DAT" balance --depth 2 >/dev/null
    echo "Running register --monthly..."
    "$LEDGER" -f "$BENCHMARK_DAT" register --monthly >/dev/null

    # Also run with the standard test data for broader coverage
    STANDARD="$PROJECT_DIR/test/input/standard.dat"
    if [ -f "$STANDARD" ]; then
        echo "Running balance on standard.dat..."
        "$LEDGER" -f "$STANDARD" balance >/dev/null
        echo "Running register on standard.dat..."
        "$LEDGER" -f "$STANDARD" register >/dev/null
    fi

    echo ""
    echo "Merging profiles..."
    llvm-profdata merge -sparse "$PROFRAW_DIR"/*.profraw -o "$PROFDATA"
    echo "Profile data written to: $PROFDATA"
    echo ""
    echo "Profile summary:"
    llvm-profdata show --counts --all-functions "$PROFDATA" 2>/dev/null | tail -5 || \
        llvm-profdata show "$PROFDATA" 2>/dev/null | head -10
    ;;

report)
    if [ ! -f "$PROFDATA" ]; then
        echo "Error: no profile data found at $PROFDATA" >&2
        echo "Run '$0 generate $BUILD_DIR' first" >&2
        exit 1
    fi

    REPORT_DIR="$BUILD_DIR/profile-report"
    mkdir -p "$REPORT_DIR"

    echo "=== Generating profile report ==="

    # Text summary of hottest functions
    llvm-profdata show --topn=30 "$PROFDATA" > "$REPORT_DIR/top_functions.txt" 2>/dev/null || true

    # Source-annotated coverage report
    llvm-cov show "$LEDGER" \
        -instr-profile="$PROFDATA" \
        -format=html \
        -output-dir="$REPORT_DIR/html" \
        -show-line-counts-or-regions \
        -Xdemangler=c++filt 2>/dev/null || \
    llvm-cov show "$LEDGER" \
        -instr-profile="$PROFDATA" \
        -format=html \
        -output-dir="$REPORT_DIR/html" 2>/dev/null || true

    # Text report for quick viewing
    llvm-cov report "$LEDGER" \
        -instr-profile="$PROFDATA" \
        > "$REPORT_DIR/coverage_summary.txt" 2>/dev/null || true

    echo "Reports written to: $REPORT_DIR/"
    echo "  top_functions.txt     - Hottest functions by execution count"
    echo "  html/                 - Source-annotated HTML report"
    echo "  coverage_summary.txt  - Per-file execution summary"
    ;;

pgo)
    if [ ! -f "$PROFDATA" ]; then
        echo "Error: no profile data found at $PROFDATA" >&2
        echo "Run '$0 generate $BUILD_DIR' first" >&2
        exit 1
    fi

    echo "=== Rebuilding with Profile-Guided Optimization ==="
    PGO_DIR="$BUILD_DIR-pgo"
    mkdir -p "$PGO_DIR"

    cmake -B "$PGO_DIR" -S "$PROJECT_DIR" \
        -DCMAKE_BUILD_TYPE=Release \
        -DDISABLE_ASSERTS=ON \
        -DCLANG_PROFILE_DATA="$PROFDATA"

    cmake --build "$PGO_DIR" -- -j"$(nproc 2>/dev/null || sysctl -n hw.ncpu)"

    echo ""
    echo "PGO build complete: $PGO_DIR/ledger"
    echo ""
    echo "Compare performance:"
    echo "  hyperfine '$LEDGER -f $BENCHMARK_DAT balance' '$PGO_DIR/ledger -f $BENCHMARK_DAT balance'"
    ;;

*)
    usage
    ;;
esac
