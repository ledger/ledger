#!/usr/bin/env bash
# Run ledger benchmarks comparing PR and base binaries.
#
# Both binaries are benchmarked simultaneously (interleaved runs) to ensure
# fair comparison under equal cache conditions.
#
# Usage: ./benchmark/run_benchmark.sh <pr-binary> <base-binary> <pr-output.json> <base-output.json>
#
# Requires: hyperfine (https://github.com/sharkdp/hyperfine)
#
# Environment variables:
#   HYPERFINE_RUNS  - Override number of measured runs (default: 10)

set -euo pipefail

PR_BIN="${1:?Usage: $0 <pr-binary> <base-binary> <pr-output.json> <base-output.json>}"
BASE_BIN="${2:?Usage: $0 <pr-binary> <base-binary> <pr-output.json> <base-output.json>}"
PR_OUTPUT="${3:-pr_results.json}"
BASE_OUTPUT="${4:-base_results.json}"
WARMUP=3
RUNS="${HYPERFINE_RUNS:-10}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
BENCHMARK_DAT="$SCRIPT_DIR/benchmark.dat"

if ! command -v hyperfine &>/dev/null; then
    echo "Error: hyperfine is required. Install via: cargo install hyperfine" >&2
    echo "  or: brew install hyperfine / apt install hyperfine" >&2
    exit 1
fi

for BIN in "$PR_BIN" "$BASE_BIN"; do
    if ! [ -x "$BIN" ] && ! command -v "$BIN" &>/dev/null; then
        echo "Error: binary not found or not executable: $BIN" >&2
        exit 1
    fi
done

# Generate benchmark data if it doesn't exist
if [ ! -f "$BENCHMARK_DAT" ]; then
    echo "Generating benchmark data..." >&2
    python3 "$SCRIPT_DIR/gen_benchmark.py"
fi

echo "Benchmarking PR binary:   $PR_BIN" >&2
echo "Benchmarking base binary: $BASE_BIN" >&2
echo "Data file: $BENCHMARK_DAT" >&2
echo "Warmup runs: $WARMUP, Measured runs: $RUNS" >&2
echo "Note: Both binaries are benchmarked simultaneously (interleaved) for fair comparison." >&2

# Define benchmark command suffixes (binary path prepended per benchmark)
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

declare -a ARGS=(
    "-f $BENCHMARK_DAT balance"
    "-f $BENCHMARK_DAT register"
    "-f $BENCHMARK_DAT print"
    "-f $BENCHMARK_DAT stats"
    "-f $BENCHMARK_DAT csv"
    "-f $BENCHMARK_DAT balance --depth 2"
    "-f $BENCHMARK_DAT register --monthly"
    "-f $BENCHMARK_DAT equity"
)

TMPDIR_BENCH="$(mktemp -d)"
trap 'rm -rf "$TMPDIR_BENCH"' EXIT

# Run each benchmark pair simultaneously (interleaved by hyperfine)
for i in "${!NAMES[@]}"; do
    NAME="${NAMES[$i]}"
    PR_CMD="$PR_BIN ${ARGS[$i]}"
    BASE_CMD="$BASE_BIN ${ARGS[$i]}"
    RESULT_FILE="$TMPDIR_BENCH/${NAME}.json"

    echo "Benchmarking: $NAME" >&2
    hyperfine \
        --warmup "$WARMUP" \
        --runs "$RUNS" \
        --export-json "$RESULT_FILE" \
        --command-name "${NAME}-pr" \
        --command-name "${NAME}-base" \
        "$PR_CMD" \
        "$BASE_CMD" >/dev/null 2>&1
done

# Split combined results into separate PR and base output files
python3 -c "
import json, os, sys

pr_results = []
base_results = []
tmpdir = sys.argv[1]
pr_out = sys.argv[2]
base_out = sys.argv[3]
names = sys.argv[4:]

for name in names:
    path = os.path.join(tmpdir, name + '.json')
    with open(path) as f:
        data = json.load(f)

    # results[0] = PR (first command), results[1] = base (second command)
    pr = data['results'][0]
    base = data['results'][1]

    pr_results.append({
        'name': name,
        'median': pr['median'],
        'mean': pr['mean'],
        'stddev': pr['stddev'],
        'min': pr['min'],
        'max': pr['max'],
        'times': pr['times'],
    })
    base_results.append({
        'name': name,
        'median': base['median'],
        'mean': base['mean'],
        'stddev': base['stddev'],
        'min': base['min'],
        'max': base['max'],
        'times': base['times'],
    })

with open(pr_out, 'w') as f:
    json.dump({'benchmarks': pr_results}, f, indent=2)
    f.write('\n')
with open(base_out, 'w') as f:
    json.dump({'benchmarks': base_results}, f, indent=2)
    f.write('\n')
" "$TMPDIR_BENCH" "$PR_OUTPUT" "$BASE_OUTPUT" "${NAMES[@]}"

echo "PR results written to: $PR_OUTPUT" >&2
echo "Base results written to: $BASE_OUTPUT" >&2
