#!/usr/bin/env bash
# Run ledger benchmarks and output JSON results.
#
# Usage: ./benchmark/run_benchmark.sh <ledger-binary> [output.json]
#
# Requires: hyperfine (https://github.com/sharkdp/hyperfine)
#
# The script generates benchmark data (if missing), then runs a suite of
# ledger commands and writes timing results to a JSON file.
#
# Environment variables:
#   HYPERFINE_RUNS  - Override number of measured runs (default: 10)

set -euo pipefail

LEDGER="${1:?Usage: $0 <ledger-binary> [output.json]}"
OUTPUT="${2:-benchmark_results.json}"
WARMUP=3
RUNS="${HYPERFINE_RUNS:-10}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
BENCHMARK_DAT="$SCRIPT_DIR/benchmark.dat"

if ! command -v hyperfine &>/dev/null; then
    echo "Error: hyperfine is required. Install via: cargo install hyperfine" >&2
    echo "  or: brew install hyperfine / apt install hyperfine" >&2
    exit 1
fi

if ! [ -x "$LEDGER" ] && ! command -v "$LEDGER" &>/dev/null; then
    echo "Error: ledger binary not found or not executable: $LEDGER" >&2
    exit 1
fi

# Generate benchmark data if it doesn't exist
if [ ! -f "$BENCHMARK_DAT" ]; then
    echo "Generating benchmark data..."
    python3 "$SCRIPT_DIR/gen_benchmark.py"
fi

echo "Running benchmarks with: $LEDGER" >&2
echo "Data file: $BENCHMARK_DAT" >&2
echo "Warmup runs: $WARMUP, Measured runs: $RUNS" >&2

# Define benchmark commands
declare -a NAMES=(
    "balance"
    "register"
    "print"
    "stats"
    "csv"
    "balance-depth-2"
    "register-monthly"
    "equity"
)

declare -a COMMANDS=(
    "$LEDGER -f $BENCHMARK_DAT balance"
    "$LEDGER -f $BENCHMARK_DAT register"
    "$LEDGER -f $BENCHMARK_DAT print"
    "$LEDGER -f $BENCHMARK_DAT stats"
    "$LEDGER -f $BENCHMARK_DAT csv"
    "$LEDGER -f $BENCHMARK_DAT balance --depth 2"
    "$LEDGER -f $BENCHMARK_DAT register --monthly"
    "$LEDGER -f $BENCHMARK_DAT equity"
)

TMPDIR="$(mktemp -d)"
trap 'rm -rf "$TMPDIR"' EXIT

# Run each benchmark and collect individual JSON results
for i in "${!NAMES[@]}"; do
    NAME="${NAMES[$i]}"
    CMD="${COMMANDS[$i]}"
    RESULT_FILE="$TMPDIR/${NAME}.json"

    echo "Benchmarking: $NAME" >&2
    hyperfine \
        --warmup "$WARMUP" \
        --runs "$RUNS" \
        --export-json "$RESULT_FILE" \
        --command-name "$NAME" \
        "$CMD" >/dev/null 2>&1
done

# Assemble results into a single JSON file
python3 -c "
import json, glob, os, sys

results = []
tmpdir = sys.argv[1]
names = sys.argv[2:]

for name in names:
    path = os.path.join(tmpdir, name + '.json')
    with open(path) as f:
        data = json.load(f)
    r = data['results'][0]
    results.append({
        'name': r['command'],
        'median': r['median'],
        'mean': r['mean'],
        'stddev': r['stddev'],
        'min': r['min'],
        'max': r['max'],
        'times': r['times'],
    })

json.dump({'benchmarks': results}, sys.stdout, indent=2)
print()
" "$TMPDIR" "${NAMES[@]}" > "$OUTPUT"

echo "Results written to: $OUTPUT" >&2
