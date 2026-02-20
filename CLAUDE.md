# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with
code in this repository.

## Project Overview

Ledger is a command-line double-entry accounting system written in C++. It
uses plain text files for input and generates financial reports without
requiring a database.

## Key Commands

### Building the Project

```bash
# Initial setup and build
./acprep dependencies  # Install dependencies (platform-specific)
./acprep update --output=build # Configure and build in one step

# Alternative configuration commands
./acprep opt --output=build   # Optimized build
./acprep debug --output=build # Debug build
./acprep gcov --output=build  # Build with code coverage

# Once the project is configured using the opt, debug or gcov commands, then
# it may be built using make:
cd build && make -j$(nproc)

# Or a manual CMake build can be used:
mkdir build && cd build
cmake ..
make -j$(nproc)
```

Useful CMake flags:

- `-DUSE_PYTHON=ON` -- Enable Python bindings (requires Boost.Python)
- `-DUSE_SANITIZERS=ON` -- Enable AddressSanitizer + UBSan
- `-DBUILD_DEBUG=ON` -- Enable runtime debugging support
- `-DDISABLE_ASSERTS=ON` -- Disable assertion checks

### Running Tests

```bash
# Run all tests
cd build && ctest

# Run specific test categories
cd build && ctest -R baseline     # Run baseline tests
cd build && ctest -R regress      # Run regression tests
cd build && ctest -R manual       # Run manual tests

# Run a single test by name
cd build && ctest -R 2413         # Run regression test for issue #2413

# Run a .test file directly via the harness
python test/RegressTests.py --ledger ./build/ledger \
  --sourcepath . test/regress/2413.test
```

Python 3.10+ is required to run the test harness. All tests run with
`TZ=America/Chicago`.

### Development Commands

```bash
# Run ledger from build directory
./build/ledger -f test/input/sample.dat balance
./build/ledger -f test/input/sample.dat register

# Clean build
cd build && make clean
```

## Architecture Overview

### Core Components

The codebase is organized around these key abstractions:

- **Journal** (`src/journal.h/cc`): Central data structure holding all transactions and accounts
- **Transaction/Xact** (`src/xact.h/cc`): Individual dated transactions containing postings
- **Posting/Post** (`src/post.h/cc`): Line items within transactions affecting accounts
- **Account** (`src/account.h/cc`): Hierarchical chart of accounts structure
- **Amount** (`src/amount.h/cc`): Precise arithmetic for monetary values with commodity support
- **Value** (`src/value.h/cc`): Polymorphic container for amounts, balances, and expressions
- **Expression Engine** (`src/expr.h/cc`, `src/op.h/cc`): Query and calculation language

### Data Flow

1. **Parsing**: Text files are parsed by `src/textual.cc` into the journal structure
2. **Processing**: Transactions flow through filters (`src/filters.h/cc`) for transformation
3. **Reporting**: Output formatters (`src/output.h/cc`, `src/print.h/cc`) generate reports
4. **Commands**: User commands are implemented in `src/report.cc` and related files

### Key Design Patterns

- **Chain of Responsibility**: Filters process transactions in a pipeline
- **Visitor Pattern**: Used for traversing account hierarchies and transactions
- **Expression Trees**: Mathematical and logical expressions are AST-based
- **Value Semantics**: Most objects use value semantics with careful memory management

## Coding Conventions

### Style

- 2-space indentation, no tabs, 100-column limit
- LLVM-based formatting enforced by `.clang-format` (checked in CI)
- K&R braces (attach style)
- Pointer alignment: left (`int* p`, not `int *p`)
- Do not reorder `#include` directives (`SortIncludes: Never`)

### Naming

- Classes/types: `snake_case` with `_t` suffix (`journal_t`, `xact_t`, `account_t`)
- Functions/methods: `snake_case` (`add_post()`, `finalize()`)
- Macros: `UPPER_CASE` (`TRACE_CTOR`, `DECLARE_EXCEPTION`)

### Key Macros

- `TRACE_CTOR(class, "args")` / `TRACE_DTOR(class)` -- constructor/destructor tracing
- `DECLARE_EXCEPTION(name, base)` -- declare a custom exception type

### Memory Management

- Use `shared_ptr` and `unique_ptr` for ownership
- Many classes inherit from `noncopyable`

## Test File Format

Tests in `test/baseline/` and `test/regress/` use a `.test` format:

```
; Journal data (transactions, directives, etc.)
2024/01/01 Payee
    Expenses:Food    $10.00
    Assets:Cash

test reg
24-Jan-01 Payee                 Expenses:Food                $10.00       $10.00
                                Assets:Cash                 $-10.00            0
end test
```

Key details:

- Journal data appears at the top of the file
- `test <ledger-command>` begins a test block; expected output follows
- `test <command> -> <exit_code>` tests for a specific exit code
- `end test` closes the block
- `__ERROR__` marks expected stderr output within a test block
- `$FILE` is replaced with the test file path at runtime
- Multiple `test`/`end test` blocks can appear in one file

### Regression Test Naming

- Named by GitHub issue number: `2413.test`, `1036.test`
- Some use hex hashes: `012ADB60.test`
- Python-dependent tests use a `_py.test` suffix

When fixing a bug, add a regression test in `test/regress/` named after the
issue number.

## Important Notes

- The project uses CMake 3.16.2+ and requires Boost 1.72+
- Python bindings are optional but enable additional features
- The `acprep` script automates most build configurations
- Use `./acprep --help` to see all build options
- All source files live in a flat `src/` directory (no subdirectories)
- PRs should target `master` (see `CONTRIBUTING.md`)

## Task Master AI Instructions
**Import Task Master's development workflow commands and guidelines, treat as if import is in the main CLAUDE.md file.**
@./.taskmaster/CLAUDE.md
