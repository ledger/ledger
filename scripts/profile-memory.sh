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
