/*
 * Memory management tests
 * 
 * Tests proper memory allocation, deallocation, and ownership transfer.
 */

#include <iostream>
#include <cassert>
#include <vector>
#include <memory>

extern "C" {
#include "ledger_ffi.h"
}

// Forward declaration from test_main.cpp
extern void TEST_ASSERT(bool condition, const char* message);
extern void TEST_ASSERT_EQ(auto actual, auto expected, const char* message);
extern bool check_and_print_error(const std::string& context);

void test_memory_management() {
    std::cout << "  Testing basic memory lifecycle..." << std::endl;
    
    // Test multiple journal creations and deletions
    std::vector<CJournal*> journals;
    for (int i = 0; i < 10; ++i) {
        CJournal* journal = ledger_journal_new();
        TEST_ASSERT(journal != nullptr, "Journal creation should succeed");
        journals.push_back(journal);
    }
    
    // Free all journals
    for (CJournal* journal : journals) {
        ledger_journal_free(journal);
    }
    
    std::cout << "  Testing ownership transfer..." << std::endl;
    
    // Create journal and transactions
    CJournal* journal = ledger_journal_new();
    std::vector<CTransaction*> transactions;
    
    // Create multiple transactions
    for (int i = 0; i < 5; ++i) {
        CDate date = {2023, 1, i + 1};
        std::string payee = "Test Payee " + std::to_string(i);
        CTransaction* transaction = ledger_transaction_new(date, payee.c_str());
        TEST_ASSERT(transaction != nullptr, "Transaction creation should succeed");
        transactions.push_back(transaction);
    }
    
    // Transfer ownership to journal (after this, transaction pointers become invalid)
    for (CTransaction* transaction : transactions) {
        LedgerResult result = ledger_journal_add_transaction(journal, transaction);
        TEST_ASSERT_EQ(result, LedgerResult::LedgerOk, "Adding transaction should succeed");
    }
    
    // Verify transaction count
    int count = ledger_journal_transaction_count(journal);
    TEST_ASSERT_EQ(count, 5, "Journal should have 5 transactions");
    
    // Free journal (this should free all owned transactions)
    ledger_journal_free(journal);
    // Note: transaction pointers are now invalid, don't call ledger_transaction_free on them
    
    std::cout << "  Testing string memory management..." << std::endl;
    
    // Create transaction for string testing
    CDate date = {2023, 12, 31};
    CTransaction* transaction = ledger_transaction_new(date, "Memory Test Payee");
    
    // Test multiple string copies and frees
    for (int i = 0; i < 10; ++i) {
        char* copied_payee = ledger_transaction_copy_payee(transaction);
        TEST_ASSERT(copied_payee != nullptr, "String copying should succeed");
        
        // Verify string content
        std::string payee_str(copied_payee);
        TEST_ASSERT(payee_str == "Memory Test Payee", "Copied string should match");
        
        // Free the string
        ledger_free_string(copied_payee);
    }
    
    // Test amount with commodity string copying
    CAmount* amount = ledger_amount_new_with_commodity(50.0, "EUR");
    for (int i = 0; i < 10; ++i) {
        char* copied_commodity = ledger_amount_copy_commodity(amount);
        TEST_ASSERT(copied_commodity != nullptr, "Commodity copying should succeed");
        
        std::string commodity_str(copied_commodity);
        TEST_ASSERT(commodity_str == "EUR", "Copied commodity should match");
        
        ledger_free_string(copied_commodity);
    }
    
    // Clean up
    ledger_amount_free(amount);
    ledger_transaction_free(transaction);
    
    std::cout << "  Testing reference counting..." << std::endl;
    
    // Test reference counting with multiple clones
    RcJournal* original = ledger_rc_journal_new();
    TEST_ASSERT(original != nullptr, "RC journal creation should succeed");
    
    std::vector<RcJournal*> clones;
    
    // Create multiple clones
    for (int i = 0; i < 5; ++i) {
        RcJournal* clone = ledger_rc_journal_clone(original);
        TEST_ASSERT(clone != nullptr, "RC journal cloning should succeed");
        clones.push_back(clone);
    }
    
    // Free original first
    ledger_rc_journal_free(original);
    
    // Clones should still be valid, free them in reverse order
    for (auto it = clones.rbegin(); it != clones.rend(); ++it) {
        ledger_rc_journal_free(*it);
    }
    
    std::cout << "  Testing null pointer handling..." << std::endl;
    
    // Test that functions handle null pointers gracefully
    int null_count = ledger_journal_transaction_count(nullptr);
    TEST_ASSERT_EQ(null_count, -1, "Null journal should return -1 transaction count");
    TEST_ASSERT(check_and_print_error("null journal count"), "Error should be set for null pointer");
    
    CDate null_date = ledger_transaction_get_date(nullptr);
    TEST_ASSERT_EQ(null_date.year, 0, "Null transaction should return zero date");
    TEST_ASSERT(check_and_print_error("null transaction date"), "Error should be set for null pointer");
    
    double null_value = ledger_amount_get_value(nullptr);
    TEST_ASSERT_EQ(null_value, 0.0, "Null amount should return 0.0 value");
    TEST_ASSERT(check_and_print_error("null amount value"), "Error should be set for null pointer");
    
    // Test that free functions handle null pointers gracefully (should not crash)
    ledger_journal_free(nullptr);
    ledger_transaction_free(nullptr);
    ledger_amount_free(nullptr);
    ledger_rc_journal_free(nullptr);
    ledger_free_string(nullptr);
    
    std::cout << "  Memory management tests completed successfully" << std::endl;
}