# C++ FFI Integration Tests

This directory contains comprehensive C++ integration tests for the Ledger Rust FFI bridge. These tests verify that the FFI functions work correctly from C++ code and demonstrate proper usage patterns.

## Overview

The test suite covers:

- **Basic FFI functionality**: Object creation, manipulation, and basic operations
- **Memory management**: Ownership transfer, reference counting, and cleanup
- **Callback mechanisms**: Iterator callbacks, filtering, batch processing, and progress reporting
- **Error handling**: Error state management, recovery, and thread-local error storage
- **RAII wrappers**: C++ wrapper classes for automatic memory management
- **Performance**: Benchmarking FFI overhead and scalability

## Building and Running

### Prerequisites

- CMake 3.16 or later
- C++17 compatible compiler (GCC 7+, Clang 5+, MSVC 2019+)
- Rust toolchain (to build the FFI library)
- Optional: Valgrind (for memory leak testing)
- Optional: AddressSanitizer support

### Building

1. First, build the Rust FFI library:
   ```bash
   cd ../..  # Go to ledger-core root
   cargo build
   ```

2. Build the C++ test suite:
   ```bash
   cd tests/cpp_integration
   mkdir build
   cd build
   cmake ..
   make
   ```

3. For debug builds with AddressSanitizer:
   ```bash
   cmake -DCMAKE_BUILD_TYPE=Debug -DENABLE_ASAN=ON ..
   make
   ```

### Running Tests

#### Basic test run:
```bash
./cpp_integration_tests
```

#### With Valgrind (memory leak detection):
```bash
make run_valgrind
```

#### Just build and run:
```bash
make run_tests
```

## Test Structure

### test_main.cpp
- Main test runner with assertion macros
- Coordinates execution of all test modules
- Provides common helper functions

### test_basic_ffi.cpp
- Tests core object creation and manipulation
- Journal, Transaction, and Amount operations
- String handling and basic data access
- Reference-counted journal operations

### test_memory_management.cpp
- Memory lifecycle testing
- Ownership transfer verification
- Reference counting behavior
- Null pointer safety
- String memory management

### test_callbacks.cpp
- Iterator callback functionality
- Transaction and posting iteration
- Filtering and counting operations
- Batch processing and progress callbacks
- Early termination and error conditions

### test_error_handling.cpp
- FFI error state management
- Error code verification
- Error message retrieval
- Error persistence and clearing
- Recovery after errors

### test_raii_wrappers.cpp
- C++ RAII wrapper classes for automatic cleanup
- Move semantics and copy behavior
- Exception safety
- Container usage patterns
- Smart pointer integration

### test_performance.cpp
- Object creation/destruction benchmarks
- Bulk operation performance
- Iterator vs batch processing comparison
- String operation overhead
- Memory allocation patterns

## RAII Wrapper Classes

The test suite demonstrates C++ RAII wrapper classes that provide automatic memory management:

```cpp
// Automatic journal management
LedgerJournal journal;
// No need to manually call ledger_journal_free()

// Transaction with ownership transfer
LedgerTransaction transaction(date, payee);
journal.add_transaction(transaction.release());

// Amount with automatic cleanup
LedgerAmount amount(123.45, "USD");
std::string commodity = amount.get_commodity();
```

These wrapper classes follow C++ best practices:
- RAII for automatic resource management
- Move semantics for efficient transfers
- Disabled copying to prevent double-free errors
- Exception safety guarantees

## Memory Safety Features

The tests verify several memory safety features:

1. **Null pointer handling**: All FFI functions gracefully handle null pointers
2. **Double-free prevention**: RAII wrappers prevent accidental double-free
3. **Use-after-free detection**: Tests verify proper ownership transfer
4. **String lifetime management**: Safe string copying and freeing patterns
5. **Reference counting**: Proper shared ownership via reference counting

## Performance Characteristics

The performance tests measure:

- Object creation overhead (typically < 1μs per object)
- Iteration performance (scales linearly with collection size)
- Batch processing efficiency (better for large datasets)
- String operation costs (copying and cleanup overhead)
- Memory allocation patterns (rapid alloc/dealloc performance)

## Integration with Existing C++ Code

These tests demonstrate how to integrate the Rust FFI with existing C++ ledger code:

1. **Gradual migration**: Start with read-only operations
2. **RAII integration**: Use wrapper classes for automatic memory management
3. **Error handling**: Integrate FFI error reporting with existing error systems
4. **Performance monitoring**: Benchmark FFI overhead vs native implementations

## Debugging and Development

### Debug Builds
```bash
cmake -DCMAKE_BUILD_TYPE=Debug ..
make
```

### Memory Leak Detection
```bash
# With Valgrind
valgrind --leak-check=full ./cpp_integration_tests

# With AddressSanitizer
cmake -DENABLE_ASAN=ON ..
make
./cpp_integration_tests
```

### Verbose Error Reporting
The tests include detailed error reporting and will print FFI error messages when failures occur.

## Common Issues and Solutions

### Library Not Found
If you get "ledger-core library not found" errors:
```bash
# Make sure Rust library is built first
cd ../.. && cargo build
# Then rebuild C++ tests
cd tests/cpp_integration/build && make
```

### Linker Errors on macOS
On macOS, you might need additional system frameworks:
```bash
# These are automatically included in CMakeLists.txt
-framework Security -framework CoreFoundation
```

### Threading Issues
The current FFI is single-threaded. For multi-threaded usage:
- Use separate FFI objects per thread
- Implement external synchronization for shared objects
- Consider reference-counted objects for cross-thread sharing

## Future Enhancements

Planned improvements to the test suite:

1. **Multi-threading tests**: Verify thread safety and concurrent access
2. **Fuzzing integration**: Automated boundary condition testing
3. **Cross-platform testing**: Windows, Linux, and macOS validation
4. **ABI stability tests**: Verify compatibility across compiler versions
5. **Integration examples**: Real-world usage patterns and best practices

## Contributing

When adding new FFI functions or modifying existing ones:

1. Add corresponding C++ tests in the appropriate test file
2. Update RAII wrappers if new object types are added
3. Add performance benchmarks for significant new functionality
4. Verify memory safety with Valgrind and AddressSanitizer
5. Update documentation and examples

The test suite serves as both validation and documentation for the FFI bridge, ensuring correctness and demonstrating proper usage patterns for C++ integration.