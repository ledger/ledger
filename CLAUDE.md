# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with
code in this repository.

## Project Overview

Ledger is a powerful command-line double-entry accounting system written in
C++. It uses plain text files for input and generates financial reports
without requiring a database.

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

### Running Tests

```bash
# Run all tests
cd build && ctest

# Run specific test categories
cd build && ctest -R baseline     # Run baseline tests
cd build && ctest -R regress      # Run regression tests
cd build && ctest -R manual       # Run manual tests

# Run a single test file
./build/ledger -f test/input/sample.dat reg
```

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

## Testing Approach

- **Unit Tests**: C++ tests in `test/unit/` using Boost.Test
- **Regression Tests**: Python-based tests in `test/regress/` comparing output
- **Baseline Tests**: Core functionality tests in `test/baseline/`
- **DocTests**: Examples extracted from documentation in `doc/`

Run tests after any code changes to ensure compatibility.

## Important Notes

- The project uses CMake 3.16.2+ and requires Boost 1.72+
- Python bindings are optional but enable additional features
- The `acprep` script automates most build configurations
- Timezone for tests is set to America/Chicago by default
- Use `./acprep --help` to see all build options
