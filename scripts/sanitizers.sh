#!/bin/bash
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
BUILD_DIR="${PROJECT_ROOT}/build-sanitizers"

usage() {
    echo "Usage: $0 [--asan-only|--ubsan-only]"
    echo "  --asan-only   Build with AddressSanitizer only"
    echo "  --ubsan-only  Build with UndefinedBehaviorSanitizer only"
    echo "  (default)     Build with both ASan and UBSan"
    exit 0
}

CMAKE_OPTS="-DUSE_SANITIZERS=ON"
for arg in "$@"; do
    case "$arg" in
        -h|--help) usage ;;
        --asan-only) CMAKE_OPTS="-DUSE_ASAN=ON" ;;
        --ubsan-only) CMAKE_OPTS="-DUSE_UBSAN=ON" ;;
        *) echo "Unknown option: $arg"; usage ;;
    esac
done

echo "==> Configuring sanitizer build ($CMAKE_OPTS)..."
mkdir -p "$BUILD_DIR"
cd "$BUILD_DIR"
cmake $CMAKE_OPTS -DCMAKE_BUILD_TYPE=Debug "$PROJECT_ROOT"

echo "==> Building..."
make -j$(sysctl -n hw.ncpu 2>/dev/null || nproc 2>/dev/null || echo 4)

echo "==> Running tests with sanitizers..."
if [ "$(uname)" = "Darwin" ]; then
    # LeakSanitizer is not supported on macOS (Apple clang)
    export ASAN_OPTIONS="detect_leaks=0:detect_stack_use_after_return=1:check_initialization_order=1"
else
    export ASAN_OPTIONS="detect_leaks=1:detect_stack_use_after_return=1:check_initialization_order=1"
    export LSAN_OPTIONS="suppressions=${PROJECT_ROOT}/.lsan-suppressions"
fi
export UBSAN_OPTIONS="print_stacktrace=1:halt_on_error=0"

ctest --output-on-failure

echo ""
echo "Sanitizer build and test complete. No issues found."
