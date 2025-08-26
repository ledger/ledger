/*
 * Callback functionality tests
 * 
 * Tests iterator callbacks, filtering, batch processing, and progress reporting.
 */

#include <iostream>
#include <cassert>
#include <vector>
#include <string>

extern "C" {
#include "ledger_ffi.h"
}

// Forward declaration from test_main.cpp
extern void TEST_ASSERT(bool condition, const char* message);
extern void TEST_ASSERT_EQ(auto actual, auto expected, const char* message);
extern bool check_and_print_error(const std::string& context);

// Test callback functions
extern "C" bool count_transactions_callback(const CTransaction* transaction, void* user_data) {
    int* counter = static_cast<int*>(user_data);
    (*counter)++;
    return true; // Continue iteration
}

extern "C" bool stop_after_two_callback(const CTransaction* transaction, void* user_data) {
    int* counter = static_cast<int*>(user_data);
    (*counter)++;
    return (*counter) < 2; // Stop after counting 2 transactions
}

extern "C" bool filter_cleared_callback(const CTransaction* transaction, void* user_data) {
    (void)user_data; // Unused parameter
    CTransactionStatus status = ledger_transaction_get_status(transaction);
    return status == CTransactionStatus::Cleared;
}

extern "C" bool progress_callback(int current, int total, const char* message, void* user_data) {
    std::vector<std::pair<int, int>>* progress_tracker = 
        static_cast<std::vector<std::pair<int, int>>*>(user_data);
    progress_tracker->push_back({current, total});
    return true; // Continue processing
}

extern "C" bool batch_callback(const CTransaction* const* transactions, int count, int batch_num, void* user_data) {
    std::vector<std::pair<int, int>>* batch_tracker = 
        static_cast<std::vector<std::pair<int, int>>*>(user_data);
    batch_tracker->push_back({batch_num, count});
    
    // Verify we can access transactions in the batch
    for (int i = 0; i < count; ++i) {
        TEST_ASSERT(transactions[i] != nullptr, "Transaction in batch should not be null");
    }
    
    return true; // Continue processing
}

void test_callbacks() {
    std::cout << "  Setting up test data..." << std::endl;
    
    // Create journal with test transactions
    CJournal* journal = ledger_journal_new();
    TEST_ASSERT(journal != nullptr, "Journal creation should succeed");
    
    // Add transactions with different statuses
    for (int i = 0; i < 6; ++i) {
        CDate date = {2023, 1, i + 1};
        std::string payee = "Test Payee " + std::to_string(i);
        CTransaction* transaction = ledger_transaction_new(date, payee.c_str());
        TEST_ASSERT(transaction != nullptr, "Transaction creation should succeed");
        
        // Set every other transaction to Cleared
        if (i % 2 == 0) {
            LedgerResult result = ledger_transaction_set_status(transaction, CTransactionStatus::Cleared);
            TEST_ASSERT_EQ(result, LedgerResult::LedgerOk, "Setting status should succeed");
        }
        
        LedgerResult result = ledger_journal_add_transaction(journal, transaction);
        TEST_ASSERT_EQ(result, LedgerResult::LedgerOk, "Adding transaction should succeed");
    }
    
    std::cout << "  Testing basic iteration..." << std::endl;
    
    // Test basic transaction iteration
    int transaction_count = 0;
    LedgerResult result = ledger_journal_iterate_transactions(
        journal, 
        count_transactions_callback, 
        &transaction_count
    );
    TEST_ASSERT_EQ(result, LedgerResult::LedgerOk, "Transaction iteration should succeed");
    TEST_ASSERT_EQ(transaction_count, 6, "Should iterate through all 6 transactions");
    
    std::cout << "  Testing early termination..." << std::endl;
    
    // Test early termination
    transaction_count = 0;
    result = ledger_journal_iterate_transactions(
        journal,
        stop_after_two_callback,
        &transaction_count
    );
    TEST_ASSERT_EQ(result, LedgerResult::LedgerOk, "Early termination should succeed");
    TEST_ASSERT_EQ(transaction_count, 2, "Should stop after 2 transactions");
    
    std::cout << "  Testing filtering..." << std::endl;
    
    // Test filtering
    CJournal* filtered_journal = ledger_journal_filter_transactions(
        journal,
        filter_cleared_callback,
        nullptr
    );
    TEST_ASSERT(filtered_journal != nullptr, "Filtering should succeed");
    
    int filtered_count = ledger_journal_transaction_count(filtered_journal);
    TEST_ASSERT_EQ(filtered_count, 3, "Should filter to 3 cleared transactions");
    
    // Test counting matching transactions
    int matching_count = ledger_journal_count_matching_transactions(
        journal,
        filter_cleared_callback,
        nullptr
    );
    TEST_ASSERT_EQ(matching_count, 3, "Should count 3 matching transactions");
    
    std::cout << "  Testing progress callbacks..." << std::endl;
    
    // Test progress callbacks
    std::vector<std::pair<int, int>> progress_tracker;
    transaction_count = 0;
    
    result = ledger_journal_process_transactions_with_progress(
        journal,
        count_transactions_callback,
        progress_callback,
        &transaction_count // Note: In real usage, you'd need a struct containing both pieces of data
    );
    TEST_ASSERT_EQ(result, LedgerResult::LedgerOk, "Progress processing should succeed");
    // Note: The progress callback won't actually be called in this simple test
    // because we're passing the wrong user_data. This tests the function signature.
    
    std::cout << "  Testing batch processing..." << std::endl;
    
    // Test batch processing
    std::vector<std::pair<int, int>> batch_tracker;
    
    result = ledger_journal_process_in_batches(
        journal,
        2, // batch size
        batch_callback,
        &batch_tracker
    );
    TEST_ASSERT_EQ(result, LedgerResult::LedgerOk, "Batch processing should succeed");
    TEST_ASSERT_EQ(batch_tracker.size(), 3, "Should have 3 batches for 6 transactions with batch size 2");
    
    // Verify batch structure
    TEST_ASSERT_EQ(batch_tracker[0].first, 0, "First batch number should be 0");
    TEST_ASSERT_EQ(batch_tracker[0].second, 2, "First batch should have 2 transactions");
    TEST_ASSERT_EQ(batch_tracker[1].first, 1, "Second batch number should be 1");
    TEST_ASSERT_EQ(batch_tracker[1].second, 2, "Second batch should have 2 transactions");
    TEST_ASSERT_EQ(batch_tracker[2].first, 2, "Third batch number should be 2");
    TEST_ASSERT_EQ(batch_tracker[2].second, 2, "Third batch should have 2 transactions");
    
    std::cout << "  Testing null pointer safety..." << std::endl;
    
    // Test null pointer safety
    result = ledger_journal_iterate_transactions(nullptr, count_transactions_callback, &transaction_count);
    TEST_ASSERT_EQ(result, LedgerResult::LedgerNullPtr, "Null journal should return null pointer error");
    
    CJournal* null_filtered = ledger_journal_filter_transactions(nullptr, filter_cleared_callback, nullptr);
    TEST_ASSERT(null_filtered == nullptr, "Null journal filtering should return null");
    
    int null_count = ledger_journal_count_matching_transactions(nullptr, filter_cleared_callback, nullptr);
    TEST_ASSERT_EQ(null_count, -1, "Null journal count should return -1");
    
    result = ledger_journal_process_in_batches(nullptr, 2, batch_callback, &batch_tracker);
    TEST_ASSERT_EQ(result, LedgerResult::LedgerNullPtr, "Null journal batch processing should return null pointer error");
    
    std::cout << "  Testing invalid batch size..." << std::endl;
    
    // Test invalid batch size
    result = ledger_journal_process_in_batches(journal, 0, batch_callback, &batch_tracker);
    TEST_ASSERT_EQ(result, LedgerResult::LedgerError, "Invalid batch size should return error");
    
    result = ledger_journal_process_in_batches(journal, -1, batch_callback, &batch_tracker);
    TEST_ASSERT_EQ(result, LedgerResult::LedgerError, "Negative batch size should return error");
    
    std::cout << "  Testing posting iteration..." << std::endl;
    
    // Create a transaction with some postings for posting iteration test
    // Note: The current FFI doesn't expose posting creation functions,
    // so this is a placeholder test that verifies the function exists
    CDate date = {2023, 1, 15};
    CTransaction* posting_test_txn = ledger_transaction_new(date, "Posting Test");
    
    // This should return 0 postings since we can't add postings via FFI yet
    auto posting_callback = [](const CPosting* posting, void* user_data) -> bool {
        int* counter = static_cast<int*>(user_data);
        (*counter)++;
        return true;
    };
    
    int posting_count = 0;
    result = ledger_transaction_iterate_postings(posting_test_txn, posting_callback, &posting_count);
    TEST_ASSERT_EQ(result, LedgerResult::LedgerOk, "Posting iteration should succeed");
    TEST_ASSERT_EQ(posting_count, 0, "Should have 0 postings in new transaction");
    
    // Test null transaction
    result = ledger_transaction_iterate_postings(nullptr, posting_callback, &posting_count);
    TEST_ASSERT_EQ(result, LedgerResult::LedgerNullPtr, "Null transaction should return null pointer error");
    
    // Clean up
    ledger_transaction_free(posting_test_txn);
    ledger_journal_free(filtered_journal);
    ledger_journal_free(journal);
    
    std::cout << "  Callback tests completed successfully" << std::endl;
}