/*
 * Example C++ Application Using Ledger FFI
 * 
 * This example demonstrates how to integrate the Rust FFI into
 * a real C++ application, showing proper usage patterns.
 */

#include <iostream>
#include <string>
#include <vector>
#include <memory>
#include <iomanip>

extern "C" {
#include "ledger_ffi.h"
}

// RAII wrapper for automatic memory management (from test_raii_wrappers.cpp)
class LedgerJournal {
private:
    CJournal* journal_;

public:
    LedgerJournal() : journal_(ledger_journal_new()) {
        if (!journal_) {
            throw std::runtime_error("Failed to create journal");
        }
    }
    
    ~LedgerJournal() {
        if (journal_) {
            ledger_journal_free(journal_);
        }
    }
    
    // Disable copying, enable moving
    LedgerJournal(const LedgerJournal&) = delete;
    LedgerJournal& operator=(const LedgerJournal&) = delete;
    LedgerJournal(LedgerJournal&& other) noexcept : journal_(other.journal_) {
        other.journal_ = nullptr;
    }
    
    CJournal* get() const { return journal_; }
    
    int transaction_count() const {
        return ledger_journal_transaction_count(journal_);
    }
    
    LedgerResult add_transaction(CTransaction* transaction) {
        return ledger_journal_add_transaction(journal_, transaction);
    }
    
    template<typename Callback>
    void iterate_transactions(Callback callback) const {
        // This would need a more sophisticated wrapper for real C++ integration
        // For now, we'll demonstrate the basic FFI call
        auto c_callback = [](const CTransaction* txn, void* data) -> bool {
            auto* cpp_callback = static_cast<Callback*>(data);
            return (*cpp_callback)(txn);
        };
        
        ledger_journal_iterate_transactions(journal_, c_callback, &callback);
    }
};

// Transaction RAII wrapper
class LedgerTransaction {
private:
    CTransaction* transaction_;
    bool owned_;

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
    
    LedgerTransaction(const LedgerTransaction&) = delete;
    LedgerTransaction& operator=(const LedgerTransaction&) = delete;
    
    CTransaction* release() {
        owned_ = false;
        return transaction_;
    }
    
    std::string get_payee() const {
        char* payee = ledger_transaction_copy_payee(transaction_);
        if (!payee) return {};
        std::string result(payee);
        ledger_free_string(payee);
        return result;
    }
    
    CDate get_date() const {
        return ledger_transaction_get_date(transaction_);
    }
    
    void set_status(CTransactionStatus status) {
        ledger_transaction_set_status(transaction_, status);
    }
};

// Simple error handler
class FFIErrorHandler {
public:
    static bool check_and_throw(const std::string& operation) {
        if (ledger_has_error()) {
            std::string error_msg = "FFI Error in " + operation + ": ";
            
            const char* ffi_error = ledger_get_last_error();
            if (ffi_error) {
                error_msg += ffi_error;
            } else {
                error_msg += "Unknown error";
            }
            
            ledger_clear_last_error();
            throw std::runtime_error(error_msg);
        }
        return false;
    }
};

int main() {
    try {
        std::cout << "=== Ledger FFI Example Application ===" << std::endl;
        
        // Create a journal using RAII wrapper
        std::cout << "Creating journal..." << std::endl;
        LedgerJournal journal;
        FFIErrorHandler::check_and_throw("journal creation");
        
        // Add some sample transactions
        std::cout << "Adding transactions..." << std::endl;
        
        struct SampleTransaction {
            CDate date;
            std::string payee;
            CTransactionStatus status;
        };
        
        std::vector<SampleTransaction> samples = {
            {{2023, 12, 1}, "Grocery Store", CTransactionStatus::Cleared},
            {{2023, 12, 5}, "Gas Station", CTransactionStatus::Cleared},
            {{2023, 12, 10}, "Coffee Shop", CTransactionStatus::Uncleared},
            {{2023, 12, 15}, "Online Purchase", CTransactionStatus::Pending},
            {{2023, 12, 20}, "Restaurant", CTransactionStatus::Cleared}
        };
        
        for (const auto& sample : samples) {
            LedgerTransaction transaction(sample.date, sample.payee.c_str());
            transaction.set_status(sample.status);
            
            LedgerResult result = journal.add_transaction(transaction.release());
            if (result != LedgerResult::LedgerOk) {
                FFIErrorHandler::check_and_throw("adding transaction");
            }
        }
        
        std::cout << "Added " << journal.transaction_count() << " transactions" << std::endl;
        
        // Demonstrate iteration using callbacks
        std::cout << "\nIterating through transactions:" << std::endl;
        
        int transaction_index = 0;
        auto print_transaction = [](const CTransaction* txn, void* data) -> bool {
            int* index = static_cast<int*>(data);
            (*index)++;
            
            // Get transaction details
            CDate date = ledger_transaction_get_date(txn);
            CTransactionStatus status = ledger_transaction_get_status(txn);
            
            // Get payee string (demonstrates string handling)
            char* payee = ledger_transaction_copy_payee(txn);
            std::string payee_str = payee ? payee : "Unknown";
            if (payee) {
                ledger_free_string(payee);
            }
            
            // Format status
            const char* status_str = "Unknown";
            switch (status) {
                case CTransactionStatus::Uncleared: status_str = "Uncleared"; break;
                case CTransactionStatus::Cleared: status_str = "Cleared"; break;
                case CTransactionStatus::Pending: status_str = "Pending"; break;
            }
            
            std::cout << "  " << *index << ". " << date.year << "-" 
                      << std::setfill('0') << std::setw(2) << date.month << "-"
                      << std::setfill('0') << std::setw(2) << date.day
                      << " " << payee_str << " [" << status_str << "]" << std::endl;
            
            return true; // Continue iteration
        };
        
        LedgerResult result = ledger_journal_iterate_transactions(
            journal.get(), 
            print_transaction, 
            &transaction_index
        );
        
        if (result != LedgerResult::LedgerOk) {
            FFIErrorHandler::check_and_throw("transaction iteration");
        }
        
        // Demonstrate filtering
        std::cout << "\nFiltering cleared transactions:" << std::endl;
        
        auto filter_cleared = [](const CTransaction* txn, void* data) -> bool {
            (void)data; // Unused
            CTransactionStatus status = ledger_transaction_get_status(txn);
            return status == CTransactionStatus::Cleared;
        };
        
        int cleared_count = ledger_journal_count_matching_transactions(
            journal.get(),
            filter_cleared,
            nullptr
        );
        
        if (cleared_count < 0) {
            FFIErrorHandler::check_and_throw("counting cleared transactions");
        }
        
        std::cout << "Found " << cleared_count << " cleared transactions" << std::endl;
        
        // Create filtered journal
        CJournal* filtered = ledger_journal_filter_transactions(
            journal.get(),
            filter_cleared,
            nullptr
        );
        
        if (!filtered) {
            FFIErrorHandler::check_and_throw("filtering transactions");
        }
        
        int filtered_count = ledger_journal_transaction_count(filtered);
        std::cout << "Filtered journal has " << filtered_count << " transactions" << std::endl;
        
        // Clean up filtered journal
        ledger_journal_free(filtered);
        
        // Demonstrate batch processing
        std::cout << "\nBatch processing demonstration:" << std::endl;
        
        auto process_batch = [](const CTransaction* const* transactions, int count, int batch_num, void* data) -> bool {
            std::cout << "  Processing batch " << batch_num << " with " << count << " transactions:" << std::endl;
            
            for (int i = 0; i < count; ++i) {
                char* payee = ledger_transaction_copy_payee(transactions[i]);
                std::string payee_str = payee ? payee : "Unknown";
                if (payee) {
                    ledger_free_string(payee);
                }
                
                std::cout << "    - " << payee_str << std::endl;
            }
            
            return true; // Continue processing
        };
        
        result = ledger_journal_process_in_batches(
            journal.get(),
            2, // batch size
            process_batch,
            nullptr
        );
        
        if (result != LedgerResult::LedgerOk) {
            FFIErrorHandler::check_and_throw("batch processing");
        }
        
        // Demonstrate reference counting
        std::cout << "\nReference counting demonstration:" << std::endl;
        
        RcJournal* rc_journal = ledger_rc_journal_new();
        if (!rc_journal) {
            FFIErrorHandler::check_and_throw("RC journal creation");
        }
        
        std::vector<RcJournal*> journal_refs;
        
        // Create multiple references
        for (int i = 0; i < 3; ++i) {
            RcJournal* ref = ledger_rc_journal_clone(rc_journal);
            if (!ref) {
                FFIErrorHandler::check_and_throw("RC journal cloning");
            }
            journal_refs.push_back(ref);
        }
        
        std::cout << "Created " << journal_refs.size() << " additional references" << std::endl;
        
        // Free references (can be done in any order)
        for (RcJournal* ref : journal_refs) {
            ledger_rc_journal_free(ref);
        }
        ledger_rc_journal_free(rc_journal);
        
        std::cout << "All references freed successfully" << std::endl;
        
        std::cout << "\n=== Example Application Completed Successfully ===" << std::endl;
        
    } catch (const std::exception& e) {
        std::cerr << "Application error: " << e.what() << std::endl;
        return 1;
    } catch (...) {
        std::cerr << "Unknown application error" << std::endl;
        return 1;
    }
    
    return 0;
}