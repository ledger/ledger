/*
 * RAII wrapper tests
 * 
 * Tests C++ RAII wrappers for automatic memory management of FFI objects.
 */

#include <iostream>
#include <cassert>
#include <memory>
#include <vector>

extern "C" {
#include "ledger_ffi.h"
}

// Forward declaration from test_main.cpp
extern void TEST_ASSERT(bool condition, const char* message);
extern void TEST_ASSERT_EQ(auto actual, auto expected, const char* message);
extern bool check_and_print_error(const std::string& context);

// RAII wrapper classes for automatic memory management

class LedgerJournal {
private:
    CJournal* journal_;

public:
    // Constructor
    LedgerJournal() : journal_(ledger_journal_new()) {
        if (!journal_) {
            throw std::runtime_error("Failed to create journal");
        }
    }
    
    // Destructor
    ~LedgerJournal() {
        if (journal_) {
            ledger_journal_free(journal_);
        }
    }
    
    // Disable copying to prevent double-free
    LedgerJournal(const LedgerJournal&) = delete;
    LedgerJournal& operator=(const LedgerJournal&) = delete;
    
    // Enable moving
    LedgerJournal(LedgerJournal&& other) noexcept : journal_(other.journal_) {
        other.journal_ = nullptr;
    }
    
    LedgerJournal& operator=(LedgerJournal&& other) noexcept {
        if (this != &other) {
            if (journal_) {
                ledger_journal_free(journal_);
            }
            journal_ = other.journal_;
            other.journal_ = nullptr;
        }
        return *this;
    }
    
    // Access to raw pointer
    CJournal* get() const { return journal_; }
    
    // Convenience methods
    int transaction_count() const {
        return ledger_journal_transaction_count(journal_);
    }
    
    LedgerResult add_transaction(CTransaction* transaction) {
        return ledger_journal_add_transaction(journal_, transaction);
    }
    
    // Iterator method
    LedgerResult iterate_transactions(TransactionCallback callback, void* user_data) const {
        return ledger_journal_iterate_transactions(journal_, callback, user_data);
    }
};

class LedgerTransaction {
private:
    CTransaction* transaction_;
    bool owned_; // Track if we still own the transaction

public:
    LedgerTransaction(const CDate& date, const char* payee) 
        : transaction_(ledger_transaction_new(date, payee)), owned_(true) {
        if (!transaction_) {
            throw std::runtime_error("Failed to create transaction");
        }
    }
    
    ~LedgerTransaction() {
        if (transaction_ && owned_) {
            ledger_transaction_free(transaction_);
        }
    }
    
    // Disable copying
    LedgerTransaction(const LedgerTransaction&) = delete;
    LedgerTransaction& operator=(const LedgerTransaction&) = delete;
    
    // Enable moving
    LedgerTransaction(LedgerTransaction&& other) noexcept 
        : transaction_(other.transaction_), owned_(other.owned_) {
        other.transaction_ = nullptr;
        other.owned_ = false;
    }
    
    LedgerTransaction& operator=(LedgerTransaction&& other) noexcept {
        if (this != &other) {
            if (transaction_ && owned_) {
                ledger_transaction_free(transaction_);
            }
            transaction_ = other.transaction_;
            owned_ = other.owned_;
            other.transaction_ = nullptr;
            other.owned_ = false;
        }
        return *this;
    }
    
    // Transfer ownership (e.g., to a journal)
    CTransaction* release() {
        owned_ = false;
        return transaction_;
    }
    
    CTransaction* get() const { return transaction_; }
    
    // Convenience methods
    CDate get_date() const {
        return ledger_transaction_get_date(transaction_);
    }
    
    CTransactionStatus get_status() const {
        return ledger_transaction_get_status(transaction_);
    }
    
    LedgerResult set_status(CTransactionStatus status) {
        return ledger_transaction_set_status(transaction_, status);
    }
};

class LedgerAmount {
private:
    CAmount* amount_;

public:
    // Constructors
    explicit LedgerAmount(double value) : amount_(ledger_amount_new(value)) {
        if (!amount_) {
            throw std::runtime_error("Failed to create amount");
        }
    }
    
    LedgerAmount(double value, const char* commodity) 
        : amount_(ledger_amount_new_with_commodity(value, commodity)) {
        if (!amount_) {
            throw std::runtime_error("Failed to create amount with commodity");
        }
    }
    
    ~LedgerAmount() {
        if (amount_) {
            ledger_amount_free(amount_);
        }
    }
    
    // Disable copying
    LedgerAmount(const LedgerAmount&) = delete;
    LedgerAmount& operator=(const LedgerAmount&) = delete;
    
    // Enable moving
    LedgerAmount(LedgerAmount&& other) noexcept : amount_(other.amount_) {
        other.amount_ = nullptr;
    }
    
    LedgerAmount& operator=(LedgerAmount&& other) noexcept {
        if (this != &other) {
            if (amount_) {
                ledger_amount_free(amount_);
            }
            amount_ = other.amount_;
            other.amount_ = nullptr;
        }
        return *this;
    }
    
    CAmount* get() const { return amount_; }
    
    // Convenience methods
    double get_value() const {
        return ledger_amount_get_value(amount_);
    }
    
    std::string get_commodity() const {
        char* commodity = ledger_amount_copy_commodity(amount_);
        if (!commodity) {
            return {};
        }
        std::string result(commodity);
        ledger_free_string(commodity);
        return result;
    }
};

// Reference-counted journal wrapper
class RcLedgerJournal {
private:
    RcJournal* journal_;

public:
    RcLedgerJournal() : journal_(ledger_rc_journal_new()) {
        if (!journal_) {
            throw std::runtime_error("Failed to create RC journal");
        }
    }
    
    // Copy constructor - clones the reference
    RcLedgerJournal(const RcLedgerJournal& other) 
        : journal_(ledger_rc_journal_clone(other.journal_)) {
        if (!journal_) {
            throw std::runtime_error("Failed to clone RC journal");
        }
    }
    
    // Copy assignment
    RcLedgerJournal& operator=(const RcLedgerJournal& other) {
        if (this != &other) {
            if (journal_) {
                ledger_rc_journal_free(journal_);
            }
            journal_ = ledger_rc_journal_clone(other.journal_);
            if (!journal_) {
                throw std::runtime_error("Failed to clone RC journal");
            }
        }
        return *this;
    }
    
    // Move constructor
    RcLedgerJournal(RcLedgerJournal&& other) noexcept : journal_(other.journal_) {
        other.journal_ = nullptr;
    }
    
    // Move assignment
    RcLedgerJournal& operator=(RcLedgerJournal&& other) noexcept {
        if (this != &other) {
            if (journal_) {
                ledger_rc_journal_free(journal_);
            }
            journal_ = other.journal_;
            other.journal_ = nullptr;
        }
        return *this;
    }
    
    ~RcLedgerJournal() {
        if (journal_) {
            ledger_rc_journal_free(journal_);
        }
    }
    
    RcJournal* get() const { return journal_; }
};

void test_raii_wrappers() {
    std::cout << "  Testing basic RAII wrapper functionality..." << std::endl;
    
    // Test journal RAII wrapper
    {
        LedgerJournal journal;
        TEST_ASSERT(journal.get() != nullptr, "Journal wrapper should have valid pointer");
        TEST_ASSERT_EQ(journal.transaction_count(), 0, "New journal should have 0 transactions");
    } // journal should be automatically freed here
    
    // Test transaction RAII wrapper
    {
        CDate date = {2023, 12, 31};
        LedgerTransaction transaction(date, "RAII Test Transaction");
        TEST_ASSERT(transaction.get() != nullptr, "Transaction wrapper should have valid pointer");
        
        CDate retrieved_date = transaction.get_date();
        TEST_ASSERT_EQ(retrieved_date.year, 2023, "Transaction date should match");
        
        CTransactionStatus status = transaction.get_status();
        TEST_ASSERT_EQ(status, CTransactionStatus::Uncleared, "New transaction should be uncleared");
        
        LedgerResult result = transaction.set_status(CTransactionStatus::Cleared);
        TEST_ASSERT_EQ(result, LedgerResult::LedgerOk, "Setting status should succeed");
    } // transaction should be automatically freed here
    
    // Test amount RAII wrapper
    {
        LedgerAmount amount1(123.45);
        TEST_ASSERT(amount1.get() != nullptr, "Amount wrapper should have valid pointer");
        
        double value = amount1.get_value();
        TEST_ASSERT(std::abs(value - 123.45) < 0.001, "Amount value should match");
        
        LedgerAmount amount2(100.0, "USD");
        std::string commodity = amount2.get_commodity();
        TEST_ASSERT(commodity == "USD", "Amount commodity should match");
    } // amounts should be automatically freed here
    
    std::cout << "  Testing ownership transfer..." << std::endl;
    
    // Test ownership transfer from transaction to journal
    {
        LedgerJournal journal;
        
        // Create transaction and transfer ownership
        CDate date = {2023, 1, 15};
        LedgerTransaction transaction(date, "Ownership Transfer Test");
        
        // Transfer ownership to journal
        LedgerResult result = journal.add_transaction(transaction.release());
        TEST_ASSERT_EQ(result, LedgerResult::LedgerOk, "Adding transaction should succeed");
        TEST_ASSERT_EQ(journal.transaction_count(), 1, "Journal should have 1 transaction");
        
        // transaction wrapper will not free the transaction now (ownership transferred)
    } // journal will free the transaction when it's destroyed
    
    std::cout << "  Testing move semantics..." << std::endl;
    
    // Test move constructor
    {
        CDate date = {2023, 2, 20};
        LedgerTransaction original(date, "Move Test Original");
        
        // Move to new wrapper
        LedgerTransaction moved = std::move(original);
        TEST_ASSERT(moved.get() != nullptr, "Moved transaction should be valid");
        TEST_ASSERT(original.get() == nullptr, "Original transaction should be null after move");
        
        CDate moved_date = moved.get_date();
        TEST_ASSERT_EQ(moved_date.day, 20, "Moved transaction should have correct data");
    }
    
    // Test move assignment
    {
        CDate date1 = {2023, 3, 10};
        CDate date2 = {2023, 3, 20};
        
        LedgerTransaction txn1(date1, "Transaction 1");
        LedgerTransaction txn2(date2, "Transaction 2");
        
        // Move assign
        txn1 = std::move(txn2);
        
        CDate final_date = txn1.get_date();
        TEST_ASSERT_EQ(final_date.day, 20, "Move assignment should transfer correct data");
    }
    
    std::cout << "  Testing reference-counted wrapper..." << std::endl;
    
    // Test reference-counted journal wrapper
    {
        RcLedgerJournal original;
        TEST_ASSERT(original.get() != nullptr, "RC journal should be valid");
        
        // Copy (should increment reference count)
        RcLedgerJournal copy = original;
        TEST_ASSERT(copy.get() != nullptr, "Copied RC journal should be valid");
        
        // Both should be valid until both are destroyed
        std::vector<RcLedgerJournal> copies;
        for (int i = 0; i < 5; ++i) {
            copies.push_back(original);
        }
        
        // All copies should be valid
        for (const auto& copy_instance : copies) {
            TEST_ASSERT(copy_instance.get() != nullptr, "All RC journal copies should be valid");
        }
        
    } // All references go out of scope here, journal should be freed
    
    std::cout << "  Testing exception safety..." << std::endl;
    
    // Test that RAII wrappers clean up properly even when exceptions are thrown
    try {
        LedgerJournal journal;
        LedgerAmount amount(50.0, "EUR");
        
        // Simulate an exception
        throw std::runtime_error("Test exception");
        
    } catch (const std::exception& e) {
        // Objects should be automatically cleaned up when exception is thrown
        TEST_ASSERT(std::string(e.what()) == "Test exception", "Should catch test exception");
    }
    
    std::cout << "  Testing containers with RAII wrappers..." << std::endl;
    
    // Test using RAII wrappers in containers
    {
        std::vector<LedgerAmount> amounts;
        
        // Add amounts to container (using move semantics)
        for (int i = 0; i < 5; ++i) {
            amounts.emplace_back(i * 10.0);
        }
        
        TEST_ASSERT_EQ(amounts.size(), 5, "Should have 5 amounts in container");
        
        // Verify values
        for (size_t i = 0; i < amounts.size(); ++i) {
            double expected = i * 10.0;
            double actual = amounts[i].get_value();
            TEST_ASSERT(std::abs(actual - expected) < 0.001, "Amount values should match");
        }
        
    } // All amounts in the container should be automatically freed
    
    std::cout << "  RAII wrapper tests completed successfully" << std::endl;
}