// Example C++ program demonstrating Ledger Core FFI usage
// This shows how C++ code would use the generated headers

#include "../include/ledger_ffi.h"
#include <iostream>
#include <cstring>

int main() {
    std::cout << "Ledger Core FFI Example\n";
    std::cout << "=======================\n\n";
    
    // Create a new journal
    std::cout << "Creating new journal...\n";
    CJournal* journal = ledger_journal_new();
    if (!journal) {
        std::cerr << "Failed to create journal\n";
        return 1;
    }
    
    // Check initial transaction count
    int count = ledger_journal_transaction_count(journal);
    std::cout << "Initial transaction count: " << count << "\n";
    
    // Create a date
    CDate date = {2023, 12, 31};
    
    // Create a new transaction
    std::cout << "Creating new transaction...\n";
    CTransaction* transaction = ledger_transaction_new(date, "Example Transaction");
    if (!transaction) {
        std::cerr << "Failed to create transaction\n";
        if (ledger_has_error()) {
            std::cerr << "Error: " << ledger_get_last_error() << "\n";
        }
        ledger_journal_free(journal);
        return 1;
    }
    
    // Check transaction properties
    CDate retrieved_date = ledger_transaction_get_date(transaction);
    std::cout << "Transaction date: " << retrieved_date.year 
              << "-" << retrieved_date.month 
              << "-" << retrieved_date.day << "\n";
    
    CTransactionStatus status = ledger_transaction_get_status(transaction);
    std::cout << "Transaction status: " << 
        (status == Uncleared ? "Uncleared" : 
         status == Cleared ? "Cleared" : "Pending") << "\n";
    
    // Set transaction status to cleared
    LedgerResult result = ledger_transaction_set_status(transaction, Cleared);
    if (result != LedgerOk) {
        std::cerr << "Failed to set transaction status\n";
        if (ledger_has_error()) {
            std::cerr << "Error: " << ledger_get_last_error() << "\n";
        }
    } else {
        std::cout << "Transaction status updated to Cleared\n";
    }
    
    // Add transaction to journal
    std::cout << "Adding transaction to journal...\n";
    result = ledger_journal_add_transaction(journal, transaction);
    if (result != LedgerOk) {
        std::cerr << "Failed to add transaction to journal\n";
        if (ledger_has_error()) {
            std::cerr << "Error: " << ledger_get_last_error() << "\n";
        }
        ledger_transaction_free(transaction);  // Only free if not added
        ledger_journal_free(journal);
        return 1;
    }
    
    // Check updated transaction count
    count = ledger_journal_transaction_count(journal);
    std::cout << "Updated transaction count: " << count << "\n";
    
    // Create and work with amounts
    std::cout << "\nWorking with amounts...\n";
    CAmount* amount1 = ledger_amount_new(123.45);
    if (amount1) {
        double value = ledger_amount_get_value(amount1);
        std::cout << "Amount 1 value: " << value << "\n";
        ledger_amount_free(amount1);
    }
    
    CAmount* amount2 = ledger_amount_new_with_commodity(100.0, "USD");
    if (amount2) {
        double value = ledger_amount_get_value(amount2);
        const char* commodity = ledger_amount_get_commodity(amount2);
        std::cout << "Amount 2: " << value;
        if (commodity) {
            std::cout << " " << commodity;
        }
        std::cout << "\n";
        ledger_amount_free(amount2);
    }
    
    // Test reference counting
    std::cout << "\nTesting reference counting...\n";
    RcJournal* rc_journal = ledger_rc_journal_new();
    if (rc_journal) {
        RcJournal* rc_clone = ledger_rc_journal_clone(rc_journal);
        if (rc_clone) {
            std::cout << "Successfully cloned reference-counted journal\n";
            ledger_rc_journal_free(rc_clone);
        }
        ledger_rc_journal_free(rc_journal);
    }
    
    // Clean up
    ledger_journal_free(journal);  // This will also free the transaction
    
    // Clear any pending errors
    ledger_clear_last_error();
    
    std::cout << "\nExample completed successfully!\n";
    return 0;
}