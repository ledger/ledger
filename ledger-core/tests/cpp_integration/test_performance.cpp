/*
 * Performance tests
 * 
 * Benchmarks FFI overhead and tests performance characteristics.
 */

#include <iostream>
#include <cassert>
#include <chrono>
#include <vector>
#include <string>

extern "C" {
#include "ledger_ffi.h"
}

// Forward declaration from test_main.cpp
extern void TEST_ASSERT(bool condition, const char* message);
extern void TEST_ASSERT_EQ(auto actual, auto expected, const char* message);
extern bool check_and_print_error(const std::string& context);

using namespace std::chrono;

class PerformanceTimer {
private:
    high_resolution_clock::time_point start_time;
    std::string operation_name;

public:
    PerformanceTimer(const std::string& name) : operation_name(name) {
        start_time = high_resolution_clock::now();
    }
    
    ~PerformanceTimer() {
        auto end_time = high_resolution_clock::now();
        auto duration = duration_cast<microseconds>(end_time - start_time);
        std::cout << "    " << operation_name << " took: " 
                  << duration.count() << " microseconds" << std::endl;
    }
    
    double elapsed_seconds() const {
        auto end_time = high_resolution_clock::now();
        auto duration = duration_cast<microseconds>(end_time - start_time);
        return duration.count() / 1000000.0;
    }
};

void test_performance() {
    std::cout << "  Testing object creation performance..." << std::endl;
    
    const int NUM_OBJECTS = 1000;
    
    // Benchmark journal creation
    {
        PerformanceTimer timer("Creating " + std::to_string(NUM_OBJECTS) + " journals");
        std::vector<CJournal*> journals;
        journals.reserve(NUM_OBJECTS);
        
        for (int i = 0; i < NUM_OBJECTS; ++i) {
            CJournal* journal = ledger_journal_new();
            TEST_ASSERT(journal != nullptr, "Journal creation should succeed");
            journals.push_back(journal);
        }
        
        // Clean up
        for (CJournal* journal : journals) {
            ledger_journal_free(journal);
        }
    }
    
    // Benchmark transaction creation
    {
        PerformanceTimer timer("Creating " + std::to_string(NUM_OBJECTS) + " transactions");
        std::vector<CTransaction*> transactions;
        transactions.reserve(NUM_OBJECTS);
        
        CDate date = {2023, 12, 31};
        for (int i = 0; i < NUM_OBJECTS; ++i) {
            std::string payee = "Performance Test Payee " + std::to_string(i);
            CTransaction* transaction = ledger_transaction_new(date, payee.c_str());
            TEST_ASSERT(transaction != nullptr, "Transaction creation should succeed");
            transactions.push_back(transaction);
        }
        
        // Clean up
        for (CTransaction* transaction : transactions) {
            ledger_transaction_free(transaction);
        }
    }
    
    // Benchmark amount creation
    {
        PerformanceTimer timer("Creating " + std::to_string(NUM_OBJECTS) + " amounts");
        std::vector<CAmount*> amounts;
        amounts.reserve(NUM_OBJECTS);
        
        for (int i = 0; i < NUM_OBJECTS; ++i) {
            double value = i * 1.5 + 0.01; // Vary the values
            CAmount* amount = ledger_amount_new(value);
            TEST_ASSERT(amount != nullptr, "Amount creation should succeed");
            amounts.push_back(amount);
        }
        
        // Clean up
        for (CAmount* amount : amounts) {
            ledger_amount_free(amount);
        }
    }
    
    std::cout << "  Testing bulk operations performance..." << std::endl;
    
    // Create a journal with many transactions for bulk operation testing
    CJournal* large_journal = ledger_journal_new();
    const int BULK_SIZE = 500;
    
    {
        PerformanceTimer timer("Adding " + std::to_string(BULK_SIZE) + " transactions to journal");
        
        for (int i = 0; i < BULK_SIZE; ++i) {
            CDate date = {2023, (i % 12) + 1, (i % 28) + 1};
            std::string payee = "Bulk Test Payee " + std::to_string(i);
            CTransaction* transaction = ledger_transaction_new(date, payee.c_str());
            
            LedgerResult result = ledger_journal_add_transaction(large_journal, transaction);
            TEST_ASSERT_EQ(result, LedgerResult::LedgerOk, "Adding transaction should succeed");
        }
    }
    
    // Verify transaction count
    int count = ledger_journal_transaction_count(large_journal);
    TEST_ASSERT_EQ(count, BULK_SIZE, "Journal should have correct number of transactions");
    
    std::cout << "  Testing iteration performance..." << std::endl;
    
    // Benchmark transaction iteration
    auto iteration_callback = [](const CTransaction* transaction, void* user_data) -> bool {
        int* counter = static_cast<int*>(user_data);
        (*counter)++;
        
        // Perform some work to make the callback realistic
        CDate date = ledger_transaction_get_date(transaction);
        CTransactionStatus status = ledger_transaction_get_status(transaction);
        (void)date; (void)status; // Suppress unused variable warnings
        
        return true; // Continue iteration
    };
    
    {
        PerformanceTimer timer("Iterating through " + std::to_string(BULK_SIZE) + " transactions");
        
        int iteration_count = 0;
        LedgerResult result = ledger_journal_iterate_transactions(
            large_journal, 
            iteration_callback, 
            &iteration_count
        );
        
        TEST_ASSERT_EQ(result, LedgerResult::LedgerOk, "Iteration should succeed");
        TEST_ASSERT_EQ(iteration_count, BULK_SIZE, "Should iterate through all transactions");
    }
    
    std::cout << "  Testing batch processing performance..." << std::endl;
    
    // Benchmark batch processing
    auto batch_callback = [](const CTransaction* const* transactions, int count, int batch_num, void* user_data) -> bool {
        int* batch_counter = static_cast<int*>(user_data);
        (*batch_counter)++;
        
        // Process each transaction in the batch
        for (int i = 0; i < count; ++i) {
            CDate date = ledger_transaction_get_date(transactions[i]);
            (void)date; // Suppress unused variable warning
        }
        
        return true; // Continue processing
    };
    
    {
        PerformanceTimer timer("Batch processing " + std::to_string(BULK_SIZE) + " transactions (batch size 50)");
        
        int batch_count = 0;
        LedgerResult result = ledger_journal_process_in_batches(
            large_journal,
            50, // batch size
            batch_callback,
            &batch_count
        );
        
        TEST_ASSERT_EQ(result, LedgerResult::LedgerOk, "Batch processing should succeed");
        TEST_ASSERT_EQ(batch_count, BULK_SIZE / 50, "Should have correct number of batches");
    }
    
    std::cout << "  Testing string operation performance..." << std::endl;
    
    // Create transaction for string benchmarks
    CDate test_date = {2023, 6, 15};
    CTransaction* string_test_txn = ledger_transaction_new(test_date, "String Performance Test Payee");
    
    {
        PerformanceTimer timer("Copying transaction payee 1000 times");
        
        for (int i = 0; i < 1000; ++i) {
            char* copied_payee = ledger_transaction_copy_payee(string_test_txn);
            TEST_ASSERT(copied_payee != nullptr, "String copying should succeed");
            
            // Verify string (adds realistic work)
            std::string payee_str(copied_payee);
            TEST_ASSERT(!payee_str.empty(), "Copied string should not be empty");
            
            ledger_free_string(copied_payee);
        }
    }
    
    // Test amount with commodity string operations
    CAmount* commodity_amount = ledger_amount_new_with_commodity(75.5, "GBP");
    
    {
        PerformanceTimer timer("Copying amount commodity 1000 times");
        
        for (int i = 0; i < 1000; ++i) {
            char* copied_commodity = ledger_amount_copy_commodity(commodity_amount);
            TEST_ASSERT(copied_commodity != nullptr, "Commodity copying should succeed");
            
            std::string commodity_str(copied_commodity);
            TEST_ASSERT(commodity_str == "GBP", "Commodity should match");
            
            ledger_free_string(copied_commodity);
        }
    }
    
    std::cout << "  Testing reference counting performance..." << std::endl;
    
    {
        PerformanceTimer timer("Creating and cloning RC journal 1000 times");
        
        RcJournal* original = ledger_rc_journal_new();
        std::vector<RcJournal*> clones;
        
        for (int i = 0; i < 1000; ++i) {
            RcJournal* clone = ledger_rc_journal_clone(original);
            TEST_ASSERT(clone != nullptr, "RC journal cloning should succeed");
            clones.push_back(clone);
        }
        
        // Free all clones
        for (RcJournal* clone : clones) {
            ledger_rc_journal_free(clone);
        }
        
        ledger_rc_journal_free(original);
    }
    
    std::cout << "  Testing memory allocation patterns..." << std::endl;
    
    // Test rapid allocation and deallocation
    {
        PerformanceTimer timer("Rapid allocation/deallocation cycle");
        
        for (int i = 0; i < 100; ++i) {
            // Allocate objects
            CJournal* journal = ledger_journal_new();
            CDate date = {2023, 7, 20};
            CTransaction* transaction = ledger_transaction_new(date, "Rapid Test");
            CAmount* amount = ledger_amount_new(i * 1.1);
            
            // Use objects briefly
            TEST_ASSERT(journal != nullptr, "Journal should be valid");
            TEST_ASSERT(transaction != nullptr, "Transaction should be valid");
            TEST_ASSERT(amount != nullptr, "Amount should be valid");
            
            // Deallocate immediately
            ledger_amount_free(amount);
            ledger_transaction_free(transaction);
            ledger_journal_free(journal);
        }
    }
    
    std::cout << "  Performance comparison summary..." << std::endl;
    
    // Compare different approaches for transaction processing
    const int COMPARISON_SIZE = 200;
    CJournal* comparison_journal = ledger_journal_new();
    
    // Add test transactions
    for (int i = 0; i < COMPARISON_SIZE; ++i) {
        CDate date = {2023, 8, (i % 28) + 1};
        std::string payee = "Comparison Test " + std::to_string(i);
        CTransaction* transaction = ledger_transaction_new(date, payee.c_str());
        ledger_journal_add_transaction(comparison_journal, transaction);
    }
    
    // Method 1: Individual iteration
    {
        PerformanceTimer timer("Method 1: Individual iteration");
        int counter = 0;
        ledger_journal_iterate_transactions(comparison_journal, iteration_callback, &counter);
    }
    
    // Method 2: Batch processing (small batches)
    {
        PerformanceTimer timer("Method 2: Small batch processing (size 10)");
        int batch_counter = 0;
        ledger_journal_process_in_batches(comparison_journal, 10, batch_callback, &batch_counter);
    }
    
    // Method 3: Batch processing (large batches)
    {
        PerformanceTimer timer("Method 3: Large batch processing (size 100)");
        int batch_counter = 0;
        ledger_journal_process_in_batches(comparison_journal, 100, batch_callback, &batch_counter);
    }
    
    // Clean up
    ledger_transaction_free(string_test_txn);
    ledger_amount_free(commodity_amount);
    ledger_journal_free(large_journal);
    ledger_journal_free(comparison_journal);
    
    std::cout << "  Performance tests completed successfully" << std::endl;
}