/*
 * Basic FFI functionality tests
 * 
 * Tests core object creation, manipulation, and basic operations.
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

void test_basic_ffi() {
    std::cout << "  Testing journal creation and destruction..." << std::endl;
    
    // Test journal creation
    CJournal* journal = ledger_journal_new();
    TEST_ASSERT(journal != nullptr, "Journal creation should succeed");
    TEST_ASSERT(!check_and_print_error("journal creation"), "No error should be set");
    
    // Test initial transaction count
    int count = ledger_journal_transaction_count(journal);
    TEST_ASSERT_EQ(count, 0, "New journal should have 0 transactions");
    
    std::cout << "  Testing transaction creation..." << std::endl;
    
    // Test transaction creation
    CDate date = {2023, 12, 31};
    const char* payee = "Test Grocery Store";
    CTransaction* transaction = ledger_transaction_new(date, payee);
    TEST_ASSERT(transaction != nullptr, "Transaction creation should succeed");
    TEST_ASSERT(!check_and_print_error("transaction creation"), "No error should be set");
    
    // Test transaction properties
    CDate retrieved_date = ledger_transaction_get_date(transaction);
    TEST_ASSERT_EQ(retrieved_date.year, 2023, "Transaction year should match");
    TEST_ASSERT_EQ(retrieved_date.month, 12, "Transaction month should match");
    TEST_ASSERT_EQ(retrieved_date.day, 31, "Transaction day should match");
    
    // Test transaction status
    CTransactionStatus status = ledger_transaction_get_status(transaction);
    TEST_ASSERT_EQ(status, CTransactionStatus::Uncleared, "New transaction should be uncleared");
    
    // Test setting transaction status
    LedgerResult result = ledger_transaction_set_status(transaction, CTransactionStatus::Cleared);
    TEST_ASSERT_EQ(result, LedgerResult::LedgerOk, "Setting transaction status should succeed");
    
    status = ledger_transaction_get_status(transaction);
    TEST_ASSERT_EQ(status, CTransactionStatus::Cleared, "Transaction status should be updated");
    
    // Test posting count (should be 0 for new transaction)
    int posting_count = ledger_transaction_posting_count(transaction);
    TEST_ASSERT_EQ(posting_count, 0, "New transaction should have 0 postings");
    
    std::cout << "  Testing journal-transaction integration..." << std::endl;
    
    // Test adding transaction to journal (transfers ownership)
    result = ledger_journal_add_transaction(journal, transaction);
    TEST_ASSERT_EQ(result, LedgerResult::LedgerOk, "Adding transaction to journal should succeed");
    
    // Test transaction count after adding
    count = ledger_journal_transaction_count(journal);
    TEST_ASSERT_EQ(count, 1, "Journal should have 1 transaction after adding");
    
    // Note: transaction pointer is now invalid (owned by journal)
    // Don't call ledger_transaction_free(transaction) after this point
    
    std::cout << "  Testing amount operations..." << std::endl;
    
    // Test amount creation
    CAmount* amount1 = ledger_amount_new(123.45);
    TEST_ASSERT(amount1 != nullptr, "Amount creation should succeed");
    
    double value = ledger_amount_get_value(amount1);
    TEST_ASSERT(std::abs(value - 123.45) < 0.001, "Amount value should match");
    
    // Test amount with commodity
    CAmount* amount2 = ledger_amount_new_with_commodity(100.0, "USD");
    TEST_ASSERT(amount2 != nullptr, "Amount with commodity creation should succeed");
    
    value = ledger_amount_get_value(amount2);
    TEST_ASSERT(std::abs(value - 100.0) < 0.001, "Amount with commodity value should match");
    
    const char* commodity = ledger_amount_get_commodity(amount2);
    // Note: This function has a memory leak in current implementation
    // In production, we should use ledger_amount_copy_commodity instead
    TEST_ASSERT(commodity != nullptr, "Amount commodity should not be null");
    
    std::cout << "  Testing string copying functions..." << std::endl;
    
    // Create a new transaction to test string copying
    CTransaction* transaction2 = ledger_transaction_new(date, "Test Payee 2");
    TEST_ASSERT(transaction2 != nullptr, "Second transaction creation should succeed");
    
    // Test copying payee string (safe version)
    char* copied_payee = ledger_transaction_copy_payee(transaction2);
    TEST_ASSERT(copied_payee != nullptr, "Copying payee should succeed");
    TEST_ASSERT(strcmp(copied_payee, "Test Payee 2") == 0, "Copied payee should match");
    
    // Free the copied string (safe to do)
    ledger_free_string(copied_payee);
    
    // Test copying commodity string
    char* copied_commodity = ledger_amount_copy_commodity(amount2);
    TEST_ASSERT(copied_commodity != nullptr, "Copying commodity should succeed");
    TEST_ASSERT(strcmp(copied_commodity, "USD") == 0, "Copied commodity should match");
    
    // Free the copied commodity string
    ledger_free_string(copied_commodity);
    
    std::cout << "  Testing reference-counted journal..." << std::endl;
    
    // Test reference-counted journal
    RcJournal* rc_journal = ledger_rc_journal_new();
    TEST_ASSERT(rc_journal != nullptr, "RC journal creation should succeed");
    
    // Test cloning reference
    RcJournal* rc_journal_clone = ledger_rc_journal_clone(rc_journal);
    TEST_ASSERT(rc_journal_clone != nullptr, "RC journal cloning should succeed");
    
    // Free both references (order shouldn't matter)
    ledger_rc_journal_free(rc_journal);
    ledger_rc_journal_free(rc_journal_clone);
    
    // Clean up
    ledger_transaction_free(transaction2); // This one wasn't added to a journal
    ledger_amount_free(amount1);
    ledger_amount_free(amount2);
    ledger_journal_free(journal); // This also frees the transaction we added
    
    std::cout << "  Basic FFI tests completed successfully" << std::endl;
}