# FFI Memory Management Rules

This document outlines the memory management rules and ownership patterns for the Ledger FFI (Foreign Function Interface) bridge between Rust and C++.

## Overview

The FFI bridge uses opaque pointers to safely expose Rust types to C++. All memory is managed by Rust, but specific rules must be followed to ensure memory safety across the language boundary.

## Core Principles

1. **Rust owns all memory** - All objects are allocated and freed by Rust
2. **Opaque pointers** - C++ code never directly accesses Rust structs
3. **Clear ownership transfer** - Functions clearly indicate when ownership is transferred
4. **No shared mutability** - Only one mutable reference exists at any time
5. **Fail-safe error handling** - All FFI functions handle panics and return proper error codes

## Object Lifecycle Rules

### Journal Objects

#### Creation and Destruction
```c++
// Creates new journal - returns owned handle
CJournal* journal = ledger_journal_new();

// Frees journal and all owned transactions - do not use handle after this
ledger_journal_free(journal);
```

**Ownership Rules:**
- `ledger_journal_new()` returns an **owned** pointer - C++ must call `ledger_journal_free()`
- Once freed, the handle becomes invalid and must not be used
- Journal owns all transactions added to it

#### Reference Counted Journals
```c++
// Create reference-counted journal
RcJournal* rc_journal = ledger_rc_journal_new();

// Clone creates additional reference (safe to share)
RcJournal* rc_journal2 = ledger_rc_journal_clone(rc_journal);

// Each handle must be freed independently
ledger_rc_journal_free(rc_journal);   // Still safe to use rc_journal2
ledger_rc_journal_free(rc_journal2);  // Now journal is fully freed
```

**Reference Counting Rules:**
- Use reference-counted journals for shared ownership scenarios
- Each `ledger_rc_journal_clone()` increments the reference count
- Each `ledger_rc_journal_free()` decrements the reference count
- Journal is freed when reference count reaches zero

### Transaction Objects

#### Creation and Ownership Transfer
```c++
// Create transaction - returns owned handle
CTransaction* txn = ledger_transaction_new(date, "Grocery Store");

// Transfer ownership to journal - txn handle becomes invalid after this call
LedgerResult result = ledger_journal_add_transaction(journal, txn);

// DO NOT call ledger_transaction_free(txn) after adding to journal
// The journal now owns the transaction and will free it
```

**Ownership Transfer Rules:**
- `ledger_transaction_new()` returns an **owned** transaction pointer
- `ledger_journal_add_transaction()` **takes ownership** of the transaction
- After ownership transfer, the original pointer becomes invalid
- Only call `ledger_transaction_free()` if you have NOT transferred ownership

#### Standalone Transactions
```c++
// For transactions not added to a journal
CTransaction* txn = ledger_transaction_new(date, "Test Transaction");

// Use the transaction...
CDate txn_date = ledger_transaction_get_date(txn);

// Free manually since not owned by journal
ledger_transaction_free(txn);
```

### Amount Objects

#### Simple Amount Management
```c++
// Create amount - returns owned handle
CAmount* amount = ledger_amount_new(123.45);

// Use amount
double value = ledger_amount_get_value(amount);

// Free when done
ledger_amount_free(amount);
```

**Amount Ownership Rules:**
- `ledger_amount_new()` and `ledger_amount_new_with_commodity()` return **owned** pointers
- C++ code is responsible for calling `ledger_amount_free()`
- Amounts are value types and not owned by other objects

## String Handling

### Input Strings (C++ to Rust)
```c++
const char* payee = "Grocery Store";
CTransaction* txn = ledger_transaction_new(date, payee);
// Rust copies the string data - payee can be freed by C++
```

**Input String Rules:**
- Rust functions copy input string data
- C++ retains ownership of input strings
- Input strings must be null-terminated and valid UTF-8

### Output Strings (Rust to C++)

⚠️ **MEMORY LEAK WARNING**: Current implementation leaks string memory

```c++
// Current implementation (has memory leak)
const char* payee = ledger_transaction_get_payee(transaction);
// DO NOT call free() on this pointer - it will cause undefined behavior
// String becomes invalid when transaction is freed
```

**Output String Rules:**
- Output strings are **borrowed** from Rust objects
- Do NOT call `free()` on returned string pointers
- Strings become invalid when the parent object is freed
- Copy string data if you need it beyond the object's lifetime

### String Memory Management (Future Enhancement)

For production use, string handling should be improved:

```c++
// Proposed better API (not yet implemented)
char* ledger_transaction_copy_payee(CTransaction* txn);  // Returns owned string
void ledger_free_string(char* str);                     // Free copied string
```

## Error Handling and Memory Safety

### Error State Management
```c++
// Check for errors after each FFI call
LedgerResult result = ledger_journal_add_transaction(journal, transaction);
if (result != LedgerOk) {
    const char* error_msg = ledger_get_last_error();
    const char* context = ledger_get_last_error_context();
    // Handle error - error strings managed by Rust
    
    // Clear error when done
    ledger_clear_last_error();
}
```

### Panic Safety
All FFI functions are panic-safe:
- Panics are caught at the FFI boundary
- Error information is stored in thread-local storage
- Default values are returned on panic
- Use error checking to detect panic conditions

## Threading Model

### Thread Safety Rules
- **Single-threaded by default** - FFI objects are not thread-safe
- Each thread should have its own object instances
- Use reference-counted objects for cross-thread sharing (with external synchronization)

### Multi-threading Guidelines
```c++
// Wrong: sharing objects across threads without synchronization
CJournal* shared_journal = ledger_journal_new();
// Thread 1 and Thread 2 both use shared_journal - UNSAFE

// Better: use reference counting with mutex
RcJournal* rc_journal = ledger_rc_journal_new();
std::mutex journal_mutex;

// Thread 1:
{
    std::lock_guard<std::mutex> lock(journal_mutex);
    RcJournal* local_ref = ledger_rc_journal_clone(rc_journal);
    // Use local_ref...
    ledger_rc_journal_free(local_ref);
}
```

## Buffer Management

### Large Data Returns
For variable-length data (lists, arrays), use iterator patterns:

```c++
// Example: iterating through transactions (conceptual)
typedef bool (*TransactionCallback)(CTransaction* txn, void* user_data);
ledger_journal_iterate_transactions(journal, callback, user_data);
```

**Buffer Rules:**
- Large datasets should use callback-based iteration
- Avoid returning large arrays that require complex memory management
- Use streaming/pagination for very large datasets

## Memory Pool Allocation (Future)

For high-performance scenarios:

```c++
// Proposed API for memory pools (not yet implemented)
CMemoryPool* pool = ledger_memory_pool_new(1024 * 1024);  // 1MB pool
CJournal* journal = ledger_journal_new_with_pool(pool);   // Use pool

// All objects use the pool
// Free pool when all objects are done
ledger_memory_pool_free(pool);
```

## Debugging and Leak Detection

### Debug Builds
In debug builds, additional checks are performed:
- Double-free detection
- Use-after-free detection
- Memory leak tracking

### Memory Leak Detection
```bash
# Run with valgrind for leak detection
valgrind --leak-check=full ./your_cpp_program

# Use AddressSanitizer for development
g++ -fsanitize=address -g your_program.cpp -o your_program
```

## Best Practices

### Resource Management
1. **Use RAII wrappers** in C++ for automatic cleanup:
```c++
class LedgerJournal {
    CJournal* journal_;
public:
    LedgerJournal() : journal_(ledger_journal_new()) {}
    ~LedgerJournal() { if (journal_) ledger_journal_free(journal_); }
    
    // Disable copying, enable moving
    LedgerJournal(const LedgerJournal&) = delete;
    LedgerJournal& operator=(const LedgerJournal&) = delete;
    LedgerJournal(LedgerJournal&& other) : journal_(other.journal_) {
        other.journal_ = nullptr;
    }
};
```

2. **Check all return values** and handle errors appropriately
3. **Clear errors** after handling to prevent error state accumulation
4. **Use reference counting** for shared ownership scenarios
5. **Never mix ownership patterns** - don't reference count and manual free the same object

### Common Pitfalls to Avoid

1. **Double Free**: Don't call free functions on transferred ownership
```c++
// WRONG:
CTransaction* txn = ledger_transaction_new(date, payee);
ledger_journal_add_transaction(journal, txn);
ledger_transaction_free(txn);  // ERROR: journal now owns txn
```

2. **Use After Free**: Don't use objects after freeing
```c++
// WRONG:
ledger_journal_free(journal);
int count = ledger_journal_transaction_count(journal);  // ERROR: use after free
```

3. **String Lifetime**: Don't use strings after parent object is freed
```c++
// WRONG:
const char* payee = ledger_transaction_get_payee(txn);
ledger_transaction_free(txn);
printf("%s", payee);  // ERROR: payee is now invalid
```

## Platform Considerations

### ABI Stability
- C structures are `#[repr(C)]` for stable ABI
- Opaque pointers maintain ABI compatibility
- Error codes use stable numeric values

### Platform-Specific Notes
- **Windows**: Use proper calling conventions (`extern "C"`)
- **macOS**: Consider codesigning requirements for dynamic libraries
- **Linux**: Ensure proper symbol visibility

## Migration Strategies

### Gradual Migration
1. **Start with read-only operations** using the FFI
2. **Add write operations** gradually as confidence builds
3. **Use reference counting** for objects shared between Rust and C++
4. **Profile memory usage** to identify bottlenecks
5. **Plan for eventual full migration** to Rust

This document will be updated as the FFI implementation evolves and new memory management patterns are introduced.