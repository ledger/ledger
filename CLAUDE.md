# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Development Commands

- Build: `./acprep update`
- Run tests: `make check` or `cd build/ledger/debug && make check`
- Run single test: `cd build/ledger/debug && ./test/regress/test_name.test`
- Install: `make install`
- Clean: `./acprep clean`

## Code Architecture

Ledger is a command-line double-entry accounting system implemented in C++. The core architecture consists of:

1. Main application logic in src/ directory (account.cc, amount.cc, balance.cc, etc.)
2. Test suite in test/ directory with regress tests, baseline tests, and unit tests
3. Documentation in doc/ directory including ledger3.texi and manual files
4. Build system using CMake and acprep script
5. Python bindings in src/py_* files for extending functionality
6. Various contributed tools in contrib/ directory

## Key Components

- Journal: Handles accounting entries (journal.cc, xact.cc, post.cc)
- Amount: Represents monetary values with currency support (amount.cc, commodity.cc)
- Balance: Manages account balances and calculations (balance.cc)
- Parser: Processes ledger files (parser.cc)
- Report generation: Creates various output formats (print.cc, report.cc)

The codebase is organized around the core accounting concepts of accounts, transactions, postings, and amounts, with extensive use of Boost libraries for C++ utilities.
