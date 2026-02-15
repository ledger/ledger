#!/bin/bash
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
BUILD_DIR="${PROJECT_ROOT}/build-coverage"

usage() {
    echo "Usage: $0 [--clean]"
    echo "  --clean  Remove previous coverage data before running"
    echo ""
    echo "Builds ledger with gcov instrumentation, runs tests,"
    echo "and generates an HTML coverage report."
    exit 0
}

for arg in "$@"; do
    case "$arg" in
        -h|--help) usage ;;
        --clean)
            if [ -d "$BUILD_DIR" ]; then
                echo "Cleaning previous coverage data..."
                find "$BUILD_DIR" -name '*.gcda' -delete 2>/dev/null || true
            fi
            ;;
        *) echo "Unknown option: $arg"; usage ;;
    esac
done

# Check dependencies
for cmd in lcov genhtml; do
    if ! command -v "$cmd" &>/dev/null; then
        echo "Error: $cmd not found. Install lcov (e.g., brew install lcov)."
        exit 1
    fi
done

echo "==> Configuring coverage build..."
mkdir -p "$BUILD_DIR"
cd "$BUILD_DIR"
cmake -DUSE_GCOV=ON -DCMAKE_BUILD_TYPE=Debug "$PROJECT_ROOT"

echo "==> Building..."
make -j$(sysctl -n hw.ncpu 2>/dev/null || nproc 2>/dev/null || echo 4)

echo "==> Running tests..."
ctest --output-on-failure

echo "==> Generating coverage report..."
make coverage

echo ""
echo "Coverage report: ${BUILD_DIR}/coverage_html/index.html"
