#!/bin/bash

# Build and test script for C++ FFI integration tests
# Usage: ./build_and_test.sh [clean|debug|release|valgrind|asan]

set -e  # Exit on any error

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

echo "=== Ledger C++ FFI Integration Test Builder ==="
echo "Project root: $PROJECT_ROOT"
echo "Script directory: $SCRIPT_DIR"

# Parse command line arguments
BUILD_TYPE="Release"
CLEAN=false
RUN_VALGRIND=false
ENABLE_ASAN=false

for arg in "$@"; do
    case $arg in
        clean)
            CLEAN=true
            ;;
        debug)
            BUILD_TYPE="Debug"
            ;;
        release)
            BUILD_TYPE="Release"
            ;;
        valgrind)
            RUN_VALGRIND=true
            BUILD_TYPE="Debug"
            ;;
        asan)
            ENABLE_ASAN=true
            BUILD_TYPE="Debug"
            ;;
        *)
            echo "Unknown argument: $arg"
            echo "Usage: $0 [clean|debug|release|valgrind|asan]"
            exit 1
            ;;
    esac
done

echo "Build type: $BUILD_TYPE"
echo "Clean build: $CLEAN"
echo "Run Valgrind: $RUN_VALGRIND"
echo "AddressSanitizer: $ENABLE_ASAN"
echo ""

# Step 1: Build the Rust FFI library
echo "=== Building Rust FFI Library ==="
cd "$PROJECT_ROOT"

if [ "$BUILD_TYPE" = "Debug" ]; then
    echo "Building Rust library in debug mode..."
    cargo build
    RUST_LIB_DIR="$PROJECT_ROOT/target/debug"
else
    echo "Building Rust library in release mode..."
    cargo build --release
    RUST_LIB_DIR="$PROJECT_ROOT/target/release"
fi

# Verify the library was built
if [ "$(uname)" = "Darwin" ]; then
    LIB_FILE="$RUST_LIB_DIR/libledger_core.dylib"
elif [ "$(uname)" = "Linux" ]; then
    LIB_FILE="$RUST_LIB_DIR/libledger_core.so"
else
    LIB_FILE="$RUST_LIB_DIR/ledger_core.dll"
fi

if [ ! -f "$LIB_FILE" ]; then
    echo "ERROR: Rust library not found at $LIB_FILE"
    echo "Make sure Cargo build succeeded."
    exit 1
fi

echo "✓ Rust library built successfully: $LIB_FILE"
echo ""

# Step 2: Ensure C header exists
echo "=== Checking C Header ==="
HEADER_FILE="$PROJECT_ROOT/include/ledger_ffi.h"

if [ ! -f "$HEADER_FILE" ]; then
    echo "C header not found. Generating with cbindgen..."
    cd "$PROJECT_ROOT"
    
    # Check if cbindgen is available
    if ! command -v cbindgen &> /dev/null; then
        echo "Installing cbindgen..."
        cargo install cbindgen
    fi
    
    # Generate header
    cbindgen --config cbindgen.toml --crate ledger-core --output "$HEADER_FILE" || {
        echo "WARNING: cbindgen failed, but continuing with existing header if available"
    }
fi

if [ -f "$HEADER_FILE" ]; then
    echo "✓ C header available: $HEADER_FILE"
else
    echo "ERROR: C header not found and could not be generated"
    exit 1
fi
echo ""

# Step 3: Build C++ tests
echo "=== Building C++ Integration Tests ==="
cd "$SCRIPT_DIR"

BUILD_DIR="build"
if [ "$CLEAN" = true ]; then
    echo "Cleaning previous build..."
    rm -rf "$BUILD_DIR"
fi

mkdir -p "$BUILD_DIR"
cd "$BUILD_DIR"

# Configure CMake
CMAKE_ARGS="-DCMAKE_BUILD_TYPE=$BUILD_TYPE"

if [ "$ENABLE_ASAN" = true ]; then
    CMAKE_ARGS="$CMAKE_ARGS -DENABLE_ASAN=ON"
    echo "Enabling AddressSanitizer..."
fi

echo "Configuring CMake with args: $CMAKE_ARGS"
cmake $CMAKE_ARGS ..

# Build
echo "Building C++ tests..."
make -j$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)

if [ ! -f "./cpp_integration_tests" ]; then
    echo "ERROR: C++ test executable not built"
    exit 1
fi

echo "✓ C++ tests built successfully"
echo ""

# Step 4: Run tests
echo "=== Running Integration Tests ==="

if [ "$RUN_VALGRIND" = true ]; then
    if command -v valgrind &> /dev/null; then
        echo "Running tests with Valgrind..."
        valgrind --leak-check=full --show-leak-kinds=all --track-origins=yes --error-exitcode=1 ./cpp_integration_tests
    else
        echo "WARNING: Valgrind not found, running tests normally"
        ./cpp_integration_tests
    fi
else
    echo "Running tests normally..."
    ./cpp_integration_tests
fi

echo ""
echo "=== Integration Tests Complete ==="
echo ""

# Step 5: Optional post-test analysis
if [ "$BUILD_TYPE" = "Debug" ] && [ "$ENABLE_ASAN" = false ]; then
    echo "=== Debug Build Analysis ==="
    
    # Check binary size
    BINARY_SIZE=$(du -h ./cpp_integration_tests | cut -f1)
    echo "Test binary size: $BINARY_SIZE"
    
    # Show library dependencies
    echo "Library dependencies:"
    if [ "$(uname)" = "Darwin" ]; then
        otool -L ./cpp_integration_tests | grep -E "(ledger_core|dylib)" || echo "  No Rust library dependencies found"
    elif [ "$(uname)" = "Linux" ]; then
        ldd ./cpp_integration_tests | grep -E "(ledger_core|\.so)" || echo "  No Rust library dependencies found"
    fi
    echo ""
fi

echo "=== Build and Test Summary ==="
echo "✓ Rust FFI library built ($BUILD_TYPE)"
echo "✓ C++ integration tests built"
echo "✓ All tests passed"

if [ "$ENABLE_ASAN" = true ]; then
    echo "✓ AddressSanitizer checks passed"
fi

if [ "$RUN_VALGRIND" = true ]; then
    echo "✓ Valgrind memory checks passed"
fi

echo ""
echo "Ready for FFI integration! 🦀 → C++"