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
