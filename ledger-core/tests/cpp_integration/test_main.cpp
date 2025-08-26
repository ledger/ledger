/*
 * C++ Integration Test Suite for Ledger FFI
 * 
 * This test suite verifies that the Rust FFI functions work correctly
 * from C++ code, including memory management, error handling, and
 * callback functionality.
 */

#include <iostream>
#include <cassert>
#include <string>
#include <vector>
#include <chrono>

// Include the generated C header
extern "C" {
#include "ledger_ffi.h"
}

// Forward declarations of test functions
void test_basic_ffi();
void test_memory_management();
void test_callbacks();
void test_error_handling();
void test_raii_wrappers();
void test_performance();

int main() {
    std::cout << "=== Ledger C++ FFI Integration Tests ===" << std::endl;
    std::cout << std::endl;

    try {
        std::cout << "Running basic FFI tests..." << std::endl;
        test_basic_ffi();
        std::cout << "✓ Basic FFI tests passed" << std::endl;
        std::cout << std::endl;

        std::cout << "Running memory management tests..." << std::endl;
        test_memory_management();
        std::cout << "✓ Memory management tests passed" << std::endl;
        std::cout << std::endl;

        std::cout << "Running callback tests..." << std::endl;
        test_callbacks();
        std::cout << "✓ Callback tests passed" << std::endl;
        std::cout << std::endl;

        std::cout << "Running error handling tests..." << std::endl;
        test_error_handling();
        std::cout << "✓ Error handling tests passed" << std::endl;
        std::cout << std::endl;

        std::cout << "Running RAII wrapper tests..." << std::endl;
        test_raii_wrappers();
        std::cout << "✓ RAII wrapper tests passed" << std::endl;
        std::cout << std::endl;

        std::cout << "Running performance tests..." << std::endl;
        test_performance();
        std::cout << "✓ Performance tests passed" << std::endl;
        std::cout << std::endl;

        std::cout << "=== ALL TESTS PASSED ===" << std::endl;
        return 0;

    } catch (const std::exception& e) {
        std::cerr << "Test failed with exception: " << e.what() << std::endl;
        return 1;
    } catch (...) {
        std::cerr << "Test failed with unknown exception" << std::endl;
        return 1;
    }
}

// Simple assertion macro with better error reporting
#define TEST_ASSERT(condition, message) \
    do { \
        if (!(condition)) { \
            std::cerr << "ASSERTION FAILED: " << message << std::endl; \
            std::cerr << "  at " << __FILE__ << ":" << __LINE__ << std::endl; \
            std::cerr << "  condition: " << #condition << std::endl; \
            throw std::runtime_error("Test assertion failed"); \
        } \
    } while(0)

#define TEST_ASSERT_EQ(actual, expected, message) \
    do { \
        if ((actual) != (expected)) { \
            std::cerr << "ASSERTION FAILED: " << message << std::endl; \
            std::cerr << "  at " << __FILE__ << ":" << __LINE__ << std::endl; \
            std::cerr << "  expected: " << (expected) << std::endl; \
            std::cerr << "  actual: " << (actual) << std::endl; \
            throw std::runtime_error("Test assertion failed"); \
        } \
    } while(0)

// Helper function to check if error is set and optionally print it
bool check_and_print_error(const std::string& context = "") {
    if (ledger_has_error()) {
        std::cout << "Error in " << context << ": ";
        const char* error_msg = ledger_get_last_error();
        if (error_msg) {
            std::cout << error_msg << std::endl;
        } else {
            std::cout << "Unknown error" << std::endl;
        }
        ledger_clear_last_error();
        return true;
    }
    return false;
}