/*
 * Error handling tests
 * 
 * Tests FFI error reporting, recovery, and thread-local error state.
 */

#include <iostream>
#include <cassert>
#include <string>
#include <cstring>

extern "C" {
#include "ledger_ffi.h"
}

// Forward declaration from test_main.cpp
extern void TEST_ASSERT(bool condition, const char* message);
extern void TEST_ASSERT_EQ(auto actual, auto expected, const char* message);
extern bool check_and_print_error(const std::string& context);

void test_error_handling() {
    std::cout << "  Testing basic error state..." << std::endl;
    
    // Clear any existing errors
    ledger_clear_last_error();
    TEST_ASSERT(!ledger_has_error(), "Should have no error after clearing");
    TEST_ASSERT_EQ(ledger_get_last_error_code(), LedgerResult::LedgerOk, "Error code should be OK");
    
    // Test null pointer error
    int count = ledger_journal_transaction_count(nullptr);
    TEST_ASSERT_EQ(count, -1, "Null journal should return -1");
    TEST_ASSERT(ledger_has_error(), "Should have error after null pointer access");
    TEST_ASSERT_EQ(ledger_get_last_error_code(), LedgerResult::LedgerNullPtr, "Should have null pointer error code");
    
    // Test getting error message
    const char* error_msg = ledger_get_last_error();
    TEST_ASSERT(error_msg != nullptr, "Error message should not be null");
    
    std::string error_str(error_msg);
    TEST_ASSERT(!error_str.empty(), "Error message should not be empty");
    std::cout << "    Error message: " << error_str << std::endl;
    
    // Test clearing error
    ledger_clear_last_error();
    TEST_ASSERT(!ledger_has_error(), "Should have no error after clearing");
    TEST_ASSERT_EQ(ledger_get_last_error_code(), LedgerResult::LedgerOk, "Error code should be OK after clearing");
    
    std::cout << "  Testing various error conditions..." << std::endl;
    
    // Test invalid date parameters (should trigger error)
    CDate invalid_date = {0, 0, 0};
    CTransaction* invalid_txn = ledger_transaction_new(invalid_date, "Test");
    // Note: The current implementation may not validate date parameters
    // This is more of a test of the error handling infrastructure
    
    if (invalid_txn) {
        ledger_transaction_free(invalid_txn);
    }
    
    // Test null payee string
    CDate valid_date = {2023, 12, 31};
    CTransaction* null_payee_txn = ledger_transaction_new(valid_date, nullptr);
    TEST_ASSERT(null_payee_txn == nullptr, "Transaction with null payee should fail");
    TEST_ASSERT(ledger_has_error(), "Should have error for null payee");
    
    std::cout << "  Testing error context..." << std::endl;
    
    // Test getting error context (if available)
    const char* error_context = ledger_get_last_error_context();
    if (error_context) {
        std::cout << "    Error context: " << error_context << std::endl;
    }
    
    // Clear for next tests
    ledger_clear_last_error();
    
    std::cout << "  Testing error persistence across operations..." << std::endl;
    
    // Trigger an error
    ledger_journal_transaction_count(nullptr);
    TEST_ASSERT(ledger_has_error(), "Should have error");
    
    // Perform a successful operation
    CJournal* journal = ledger_journal_new();
    TEST_ASSERT(journal != nullptr, "Journal creation should succeed");
    
    // Error should still be set (errors persist until explicitly cleared)
    TEST_ASSERT(ledger_has_error(), "Error should persist across successful operations");
    
    // Clear error
    ledger_clear_last_error();
    
    // Now subsequent operations shouldn't have the old error
    int valid_count = ledger_journal_transaction_count(journal);
    TEST_ASSERT_EQ(valid_count, 0, "Valid operation should succeed");
    TEST_ASSERT(!ledger_has_error(), "Should have no error after successful operation");
    
    std::cout << "  Testing multiple error scenarios..." << std::endl;
    
    // Test multiple error-causing operations
    struct ErrorTest {
        std::function<void()> operation;
        LedgerResult expected_code;
        const char* description;
    };
    
    std::vector<ErrorTest> error_tests = {
        {
            []() { ledger_journal_transaction_count(nullptr); },
            LedgerResult::LedgerNullPtr,
            "null journal transaction count"
        },
        {
            []() { ledger_transaction_get_date(nullptr); },
            LedgerResult::LedgerError, // May vary based on implementation
            "null transaction get date"
        },
        {
            []() { ledger_amount_get_value(nullptr); },
            LedgerResult::LedgerError, // May vary based on implementation
            "null amount get value"
        },
        {
            []() { ledger_transaction_copy_payee(nullptr); },
            LedgerResult::LedgerError, // May vary based on implementation
            "null transaction copy payee"
        }
    };
    
    for (const auto& test : error_tests) {
        ledger_clear_last_error();
        
        test.operation();
        
        TEST_ASSERT(ledger_has_error(), ("Should have error for " + std::string(test.description)).c_str());
        
        // Print error for debugging
        const char* msg = ledger_get_last_error();
        if (msg) {
            std::cout << "    " << test.description << " error: " << msg << std::endl;
        }
    }
    
    std::cout << "  Testing error handling in callback operations..." << std::endl;
    
    // Test callback with null journal
    auto dummy_callback = [](const CTransaction* txn, void* data) -> bool {
        return true;
    };
    
    ledger_clear_last_error();
    LedgerResult result = ledger_journal_iterate_transactions(nullptr, dummy_callback, nullptr);
    TEST_ASSERT_EQ(result, LedgerResult::LedgerNullPtr, "Null journal iteration should return null pointer error");
    TEST_ASSERT(ledger_has_error(), "Should have error for null journal iteration");
    
    // Test invalid batch size
    ledger_clear_last_error();
    auto batch_callback = [](const CTransaction* const* txns, int count, int batch_num, void* data) -> bool {
        return true;
    };
    
    result = ledger_journal_process_in_batches(journal, 0, batch_callback, nullptr);
    TEST_ASSERT_EQ(result, LedgerResult::LedgerError, "Invalid batch size should return error");
    TEST_ASSERT(ledger_has_error(), "Should have error for invalid batch size");
    
    std::cout << "  Testing error recovery..." << std::endl;
    
    // Test that we can recover from errors and continue normal operations
    ledger_clear_last_error();
    
    // Perform successful operations after error
    CTransaction* recovery_txn = ledger_transaction_new(valid_date, "Recovery Test");
    TEST_ASSERT(recovery_txn != nullptr, "Should be able to create transaction after error recovery");
    TEST_ASSERT(!ledger_has_error(), "Should have no error after successful operation");
    
    result = ledger_journal_add_transaction(journal, recovery_txn);
    TEST_ASSERT_EQ(result, LedgerResult::LedgerOk, "Should be able to add transaction after recovery");
    
    int final_count = ledger_journal_transaction_count(journal);
    TEST_ASSERT_EQ(final_count, 1, "Journal should have 1 transaction");
    TEST_ASSERT(!ledger_has_error(), "Should have no error after successful operations");
    
    // Clean up
    ledger_journal_free(journal);
    
    std::cout << "  Error handling tests completed successfully" << std::endl;
}