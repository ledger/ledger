# Ledger Documentation Overhaul — Remaining Work Plan

## Progress So Far

| Session | Layer | Files | Lines Added | Status |
|---------|-------|-------|-------------|--------|
| 1 | Core Data Model | 13 files (amount, value, balance, commodity, pool, annotate, manual, manpage) | +2,154 | **Complete** |
| 2 | Journal Model | 10 files (item, xact, post, account, journal) | +1,610 | **Complete** |
| 3 | Expression Engine + Time | 12 files | +1,850 | **Complete** |
| 4 | Filter Pipeline | 12 files | +1,920 | **Complete** |
| 5 | Parsing & Input | 11 files | +1,107 | **Complete** |
| 6 | Output, Reporting & Commands | 23 files | +1,884 | **Complete** |
| 7 | Utilities, Python & Final Harmonization | 29 files | +1,130 | **Complete** |

## Documentation Standard

Every session follows the same pattern established in Sessions 1–2:

- **File headers**: `@file` / `@brief` / `@ingroup` with architectural context
- **Class docs**: `@brief` with purpose, design rationale, relationship to other types
- **Function docs**: `@brief`, plus `@param`/`@return` where non-obvious
- **Flag macros**: `///< One-line description` after each `#define`
- **Member variables**: `///< One-line description`
- **Section dividers**: `/*--- Section Name ---*/` in `.cc` files between logical groups
- **Code groups**: Comments within large functions explaining phases/steps
- **Voice**: Educational, "why before what", grounded in accounting terms, connected to user-facing behavior

## Verification (every session)

```bash
cd build && make -j$(nproc)                    # Must compile cleanly
cd build && ctest --output-on-failure -j$(nproc)  # All tests must pass
```

Cross-check relevant manual (`doc/ledger3.texi`) and manpage (`doc/ledger.1`)
sections for consistency with new source documentation.

---

## Session 3: Expression Engine + Time

**~5,983 lines across 12 files** — The calculation and query layer

### Files

| File | Lines | Priority | Notes |
|------|-------|----------|-------|
| `times.cc` | 1,899 | High | Date/time parsing, period expressions, largest file |
| `op.cc` | 977 | High | AST node evaluation, the expression interpreter |
| `query.cc` | 564 | High | Command-line query parsing (`--limit`, `--display`, predicate syntax) |
| `times.h` | 505 | Medium | Date interval types, period enums |
| `scope.h` | 431 | Medium | Scope hierarchy for expression variable resolution |
| `query.h` | 355 | Medium | Query lexer tokens, query_t class |
| `expr.cc` | 321 | Medium | Expression compilation and evaluation entry points |
| `op.h` | 318 | Medium | AST node types (op_t), operator enum |
| `exprbase.h` | 256 | Medium | CRTP base template for expr_t |
| `expr.h` | 183 | Medium | expr_t class, fast_path optimization |
| `scope.cc` | 89 | Low | Minimal implementation |
| `predicate.h` | 85 | Low | Thin wrapper around expr_t for boolean predicates |

### Execution Strategy

Launch 3 parallel cpp-pro agents:
1. **Agent 1** (times.h/cc): Date/time parsing (the complex `parse_date`,
   `parse_datetime` functions), date intervals (`date_interval_t`), period
   expressions. Cross-check with manual's date format documentation.
2. **Agent 2** (op.h/cc + expr.h/cc + exprbase.h): AST node types, operator
   evaluation (`calc` method on each op type), expression compilation pipeline,
   fast_path optimization for common expressions like `amount`.
3. **Agent 3** (query.h/cc + scope.h/cc + predicate.h): Query parsing (how
   `ledger bal food` becomes `account =~ /food/`), scope chain (how `amount`
   resolves through post_t → xact_t → item_t → session_t), predicate wrapper.

### Cross-Reference Checks
- Manual section on "Value Expressions" and expression syntax
- Manual section on "Period Expressions" and date formats
- Manpage descriptions of `--limit`, `--display`, `--period`
- Manpage date format specifications

---

## Session 4: Filter Pipeline

**~4,659 lines across 12 files** — The transformation pipeline

### Files

| File | Lines | Priority | Notes |
|------|-------|----------|-------|
| `filters.cc` | 1,658 | High | Filter implementations (calc, sort, collapse, subtotal, etc.) |
| `filters.h` | 1,189 | Low | **Already well-documented** — review only, fix gaps |
| `select.cc` | 409 | Medium | SQL-like SELECT statement parsing |
| `chain.cc` | 295 | High | Pipeline construction — how options become filter chains |
| `iterators.h` | 268 | Medium | Journal/account traversal iterators |
| `iterators.cc` | 258 | Medium | Iterator implementations |
| `convert.cc` | 145 | Medium | CSV import conversion |
| `compare.cc` | 141 | Medium | Comparison functors for sorting |
| `chain.h` | 102 | Medium | Chain construction declarations |
| `compare.h` | 90 | Low | Thin header |
| `convert.h` | 52 | Low | Thin header |
| `select.h` | 52 | Low | Thin header |

### Execution Strategy

Launch 2 parallel cpp-pro agents:
1. **Agent 1** (filters.h/cc + chain.h/cc): Review existing filters.h docs,
   add section dividers and function docs to filters.cc, document chain.cc
   (the critical `post_chain` and `account_chain` builders that wire options
   like `--sort`, `--collapse`, `--subtotal` into the filter pipeline).
2. **Agent 2** (iterators.h/cc + compare.h/cc + convert.h/cc + select.h/cc):
   Journal/account traversal, comparison functors, CSV conversion, SELECT parsing.

### Cross-Reference Checks
- Manual's "Reporting Commands" sections
- How `--sort`, `--subtotal`, `--collapse`, `--display` map to filters
- Manpage option descriptions vs. chain.cc pipeline construction

---

## Session 5: Parsing & Input

**~4,441 lines across 11 files** — How journal files become data structures

### Files

| File | Lines | Priority | Notes |
|------|-------|----------|-------|
| `textual_xacts.cc` | 1,030 | High | Transaction/posting parsing (the core parser) |
| `textual_directives.cc` | 905 | High | Directive parsing (`account`, `commodity`, `include`, `alias`, etc.) |
| `token.cc` | 707 | Medium | Expression tokenizer |
| `parser.cc` | 542 | Medium | Expression parser (Pratt parser) |
| `textual.cc` | 288 | Medium | Top-level textual parser entry point |
| `csv.cc` | 271 | Medium | CSV file parser |
| `textual_internal.h` | 208 | Medium | Internal parser state and helpers |
| `context.h` | 154 | Medium | Parse context (file position, scope, error reporting) |
| `token.h` | 130 | Low | Token types |
| `csv.h` | 110 | Low | CSV parser header |
| `parser.h` | 96 | Low | Expression parser header |

### Execution Strategy

Launch 2 parallel cpp-pro agents:
1. **Agent 1** (textual*.cc + textual_internal.h + context.h): The journal
   parser — how `2024/01/01 Payee` is parsed into xact_t, how directives like
   `account`, `commodity`, `include`, `apply account` are handled, how
   `textual_internal.h` manages parse state.
2. **Agent 2** (token.h/cc + parser.h/cc + csv.h/cc): Expression tokenizer
   (how `amount > 100` is lexed), Pratt parser (operator precedence climbing),
   CSV import parser.

### Cross-Reference Checks
- Manual's "Journal File Format" section vs. parser behavior
- All directive documentation (`account`, `commodity`, `include`, `alias`,
  `payee`, `tag`, `apply`, `bucket`, `comment`, `define`, `year`)
- CSV import documentation

---

## Session 6: Output, Reporting & Commands

**~8,528 lines across 23 files** — Where everything comes together

### Files

| File | Lines | Priority | Notes |
|------|-------|----------|-------|
| `report.cc` | 1,884 | High | Report option handlers, command dispatch |
| `report.h` | 1,118 | High | Report class, all option declarations |
| `format.cc` | 689 | High | Format string parser and evaluator |
| `draft.cc` | 530 | Medium | Transaction drafting (`xact` command) |
| `global.cc` | 502 | Medium | Application lifecycle, initialization |
| `session.cc` | 421 | Medium | Session management, journal loading |
| `output.cc` | 399 | Medium | Post/account output handlers |
| `print.cc` | 339 | Medium | `print` command implementation |
| `lookup.cc` | 279 | Medium | Command name resolution |
| `option.cc` | 235 | Medium | Option processing |
| `main.cc` | 225 | Medium | Entry point |
| `output.h` | 216 | Low | Output handler declarations |
| `session.h` | 207 | Low | Session class |
| `precmd.cc` | 201 | Low | Pre-commands (`args`, `eval`, `parse`, `template`) |
| `global.h` | 176 | Low | Global scope class |
| `format.h` | 155 | Low | Format element types |
| `option.h` | 439 | Low | Option template machinery |
| `draft.h` | 115 | Low | Draft class |
| `print.h` | 85 | Low | Print handler |
| `emacs.cc` | 127 | Low | Emacs Lisp output |
| `emacs.h` | 77 | Low | Emacs handler |
| `precmd.h` | 56 | Low | Pre-command declarations |
| `lookup.h` | 53 | Low | Lookup declarations |

### Execution Strategy

Launch 3 parallel cpp-pro agents:
1. **Agent 1** (report.h/cc + option.h/cc): The massive report class with all
   option handlers. Document how `--sort`, `--limit`, `--format` etc. are
   defined and processed. Document the option template system.
2. **Agent 2** (format.h/cc + output.h/cc + print.h/cc + emacs.h/cc): Format
   string parsing (`%(account)`, `%12(total)`), output handlers for balance/
   register/print/emacs commands.
3. **Agent 3** (global.h/cc + session.h/cc + draft.h/cc + lookup.h/cc +
   precmd.h/cc + main.cc): Application lifecycle, session management, command
   dispatch, transaction drafting, entry point.

### Cross-Reference Checks
- Manual's format string documentation vs. format.cc behavior
- All `--option` descriptions in manpage vs. report.h option definitions
- Command names in manpage vs. lookup.cc dispatch
- `xact` command documentation vs. draft.cc

---

## Session 7: Utilities, Python Bindings & Final Harmonization

**~6,799 lines across 29 files** — Supporting infrastructure + final pass

### Files

| File | Lines | Priority | Notes |
|------|-------|----------|-------|
| `utils.cc` | 832 | Medium | Utility implementations (logging, timing, temp files) |
| `utils.h` | 608 | Medium | Utility declarations, debug macros, TRACE_* system |
| `history.cc` | 529 | Medium | Commodity price history graph and lookups |
| `py_commodity.cc` | 414 | Low | Python bindings for commodity_t |
| `mask.cc` | 402 | Low | Regular expression wrapper |
| `py_value.cc` | 397 | Low | Python bindings for value_t |
| `py_journal.cc` | 353 | Low | Python bindings for journal_t |
| `py_amount.cc` | 292 | Low | Python bindings for amount_t |
| `py_balance.cc` | 238 | Low | Python bindings for balance_t |
| `py_account.cc` | 218 | Low | Python bindings for account_t |
| `py_times.cc` | 214 | Low | Python bindings for date/time |
| `py_utils.cc` | 194 | Low | Python bindings for utilities |
| `unistring.h` | 190 | Low | Unicode string width calculation |
| `py_post.cc` | 166 | Low | Python bindings for post_t |
| `py_item.cc` | 163 | Low | Python bindings for item_t |
| `stream.h` | 150 | Low | Output stream abstraction |
| `stream.cc` | 160 | Low | Stream implementation |
| `mask.h` | 150 | Low | Regex mask class |
| `flags.h` | 147 | Low | Flags mixin template |
| `py_xact.cc` | 132 | Low | Python bindings for xact_t |
| `error.cc` | 126 | Low | Error context management |
| `py_session.cc` | 117 | Low | Python bindings for session_t |
| `pstream.h` | 115 | Low | Process stream (popen wrapper) |
| `ptree.cc` | 96 | Low | Property tree XML serialization |
| `ptree.h` | 92 | Low | Property tree declarations |
| `error.h` | 89 | Low | Exception hierarchy |
| `history.h` | 83 | Low | Price history declarations |
| `py_expr.cc` | 66 | Low | Python bindings for expr_t |
| `py_format.cc` | 66 | Low | Python bindings for format_t |

### Execution Strategy

Launch 3 parallel cpp-pro agents:
1. **Agent 1** (utils.h/cc + error.h/cc + flags.h + mask.h/cc + unistring.h +
   pstream.h + stream.h/cc + ptree.h/cc): Core utilities — debug/trace system,
   exception hierarchy, flags mixin, regex masks, Unicode width, streams,
   XML serialization.
2. **Agent 2** (history.h/cc): Price history graph — commodity price lookups,
   exchange rate chains, price point interpolation. This is the engine behind
   `--market` and `--exchange`.
3. **Agent 3** (all py_*.cc files): Python bindings — file headers, module
   docstrings, brief function-level docs explaining which C++ methods each
   binding exposes.

### Final Harmonization Pass

After all source files are documented:
1. **README.md review**: Ensure it provides a clear entry point that references
   the manual, source docs, and architecture
2. **Manual cross-reference**: Scan `doc/ledger3.texi` for any remaining
   mismatches with code documentation discovered during Sessions 3–7
3. **Manpage cross-reference**: Scan `doc/ledger.1` for option descriptions
   that don't match code behavior
4. **Doxygen group structure**: Verify `@defgroup`/`@ingroup` tags create a
   coherent module hierarchy (math, data, expr, report, util)
5. **Voice consistency**: Spot-check that documentation across all files uses
   consistent terminology and the same educational tone

---

## How to Continue

Each session follows this workflow:

1. **Start a new Claude Code conversation** on the `johnw/doc-revision` branch
2. **Reference this file**: "Continue with Session N per `DOC-WORK-PLAN.md`"
3. Claude will:
   - Read all files listed for that session
   - Launch parallel cpp-pro agents to add documentation
   - Verify the build compiles and all tests pass
   - Perform cross-reference checks against manual/manpage
4. **Verify**: `cd build && make -j$(nproc) && ctest --output-on-failure -j$(nproc)`
5. **Commit** when satisfied

Typical session duration: 10–15 minutes wall-clock time per session.
