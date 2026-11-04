#!/usr/bin/env bash
# Run ledger benchmarks comparing PR and base binaries.
#
# The two binaries take turns, one measured run at a time, and swap which
# goes first each round, so both see the same cache conditions and any drift
# in the machine's speed during a benchmark affects them alike.
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
echo "Note: The binaries alternate run by run, for a fair comparison." >&2

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

# hyperfine finishes all of one command's runs before starting the next
# command's, so it is given one run of each binary per round, PR first in
# odd rounds and base first in even ones.  -N runs the commands without a
# shell, which spares each round hyperfine's measurement of shell startup.
for i in "${!NAMES[@]}"; do
    NAME="${NAMES[$i]}"
    PR_CMD="$PR_BIN ${ARGS[$i]}"
    BASE_CMD="$BASE_BIN ${ARGS[$i]}"

    echo "Benchmarking: $NAME" >&2
    for ((ROUND = 1; ROUND <= RUNS; ROUND++)); do
        if ((ROUND % 2)); then
            ORDER=(--command-name "${NAME}-pr" --command-name "${NAME}-base" "$PR_CMD" "$BASE_CMD")
        else
            ORDER=(--command-name "${NAME}-base" --command-name "${NAME}-pr" "$BASE_CMD" "$PR_CMD")
        fi
        hyperfine -N \
            --warmup "$((ROUND == 1 ? WARMUP : 0))" \
            --runs 1 \
            --export-json "$TMPDIR_BENCH/${NAME}-${ROUND}.json" \
            "${ORDER[@]}" >/dev/null 2>&1
    done
done

# Gather each binary's times from every round into separate PR and base
# output files
python3 -c "
import json, os, statistics, sys

pr_results = []
base_results = []
tmpdir = sys.argv[1]
pr_out = sys.argv[2]
base_out = sys.argv[3]
runs = int(sys.argv[4])
names = sys.argv[5:]

def summary(name, times):
    return {
        'name': name,
        'median': statistics.median(times),
        'mean': statistics.fmean(times),
        'stddev': statistics.stdev(times) if len(times) > 1 else None,
        'min': min(times),
        'max': max(times),
        'times': times,
    }

for name in names:
    times = {name + '-pr': [], name + '-base': []}
    for run in range(1, runs + 1):
        with open(os.path.join(tmpdir, f'{name}-{run}.json')) as f:
            for result in json.load(f)['results']:
                times[result['command']].extend(result['times'])

    pr_results.append(summary(name, times[name + '-pr']))
    base_results.append(summary(name, times[name + '-base']))

with open(pr_out, 'w') as f:
    json.dump({'benchmarks': pr_results}, f, indent=2)
    f.write('\n')
with open(base_out, 'w') as f:
    json.dump({'benchmarks': base_results}, f, indent=2)
    f.write('\n')
" "$TMPDIR_BENCH" "$PR_OUTPUT" "$BASE_OUTPUT" "$RUNS" "${NAMES[@]}"

echo "PR results written to: $PR_OUTPUT" >&2
echo "Base results written to: $BASE_OUTPUT" >&2
