/*
 * Copyright (c) 2003-2025, John Wiegley.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 *
 * - Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * - Neither the name of New Artisans LLC nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/**
 * @defgroup report Reporting
 */

/**
 * @file   session.h
 * @author John Wiegley
 *
 * @ingroup report
 *
 * @brief Session management: journal loading, commodity pool setup, and
 *        data-file option handling.
 *
 * A session_t represents a single "open document" -- all the journal data
 * loaded from one or more files, plus the options that control how that
 * data is interpreted (strictness level, date format, lot matching, etc.).
 *
 * In the three-tier scope hierarchy (global > session > report), the
 * session sits in the middle.  It owns the journal_t and provides the
 * built-in functions visible to value expressions (min, max, int, str,
 * lot_price, lot_date, lot_tag, account).
 *
 * Key responsibilities:
 *   - Processing the --file / -f option and its "flush on next" semantics,
 *     so that a command-line -f completely overrides files from the
 *     environment or init file.
 *   - Reading the price database before the main journal files.
 *   - Applying session-level journal flags (day_break, recursive_aliases,
 *     checking_style, etc.) before parsing begins.
 *   - Providing symbol lookup for session-scoped functions and options.
 */
#pragma once

#include "account.h"
#include "journal.h"
#include "context.h"
#include "option.h"
#include "commodity.h"

namespace ledger {

class xact_t;

/**
 * @brief Path comparator that considers filesystem equivalence.
 *
 * Two paths compare equal if they resolve to the same filesystem entity
 * (via std::filesystem::equivalent), preventing duplicate journal loads
 * when the same file is specified by different relative or absolute paths.
 */
struct ComparePaths {
  bool operator()(const path& p1, const path& p2) const {
    return p1 < p2 && !std::filesystem::equivalent(p1, p2);
  }
};

#define COMMA ,

/**
 * @brief Holds all state for a single "open document" -- the journal,
 *        commodity pool, and session-level options.
 *
 * session_t extends symbol_scope_t, which means it can define named
 * symbols (functions and options) that are visible to value expressions
 * evaluated in this session's context.
 *
 * The session owns its journal via a shared_ptr, and maintains a
 * parsing_context stack that tracks the current file being parsed,
 * enabling nested includes and price-db loading.
 *
 * @see global_scope_t, report_t, journal_t
 */
class session_t : public symbol_scope_t {
  friend void set_session_context(session_t* session);

public:
  /// When true, the next -f option clears the data_files list before
  /// appending.  This implements the semantics where command-line -f
  /// completely overrides files from environment/init-file sources.
  bool flush_on_next_data_file;

  boost::shared_ptr<journal_t> journal;  ///< The loaded journal data
  parse_context_stack_t parsing_context; ///< Stack of active parse contexts (for includes)
  optional<expr_t> value_expr;           ///< User-specified value expression (--value-expr)

  explicit session_t();
  ~session_t() override {
    TRACE_DTOR(session_t);
    parsing_context.pop();
  }

  string description() override { return _("current session"); }

  void set_flush_on_next_data_file(const bool truth) { flush_on_next_data_file = truth; }

  /// Read a single journal file, resetting the journal first.
  /// Used by the programmatic API (e.g., Python bindings).
  journal_t* read_journal(const path& pathname);

  /// Read journal data from an in-memory string, resetting the journal first.
  /// Used by the programmatic API and test harnesses.
  journal_t* read_journal_from_string(const string& data);

  /// Read all data files specified by the --file option(s) plus the
  /// price database.  This is the core data-loading method.
  /// @param master_account  If non-empty, all postings are placed under
  ///                        this account (--master-account).
  /// @return The number of transactions read.
  std::size_t read_data(const string& master_account = "");

  /// High-level entry point: read all journal files configured via options.
  /// Calls read_data() with the --master-account value if set.
  journal_t* read_journal_files();

  /// Reset the journal, discarding all loaded data.
  void close_journal_files();

  /// @return A raw pointer to the current journal.
  journal_t* get_journal();

  /*--- Built-in Functions for Value Expressions ---*/

  /// Look up an account by name (string) or pattern (mask).
  value_t fn_account(call_scope_t& scope);
  /// Return the minimum of two values (null-safe).
  value_t fn_min(call_scope_t& scope);
  /// Return the maximum of two values (null-safe).
  value_t fn_max(call_scope_t& scope);
  /// Convert a value to an integer (long).
  value_t fn_int(call_scope_t& scope);
  /// Convert a value to a string.
  value_t fn_str(call_scope_t& scope);
  /// Extract the lot price annotation from an amount, if present.
  value_t fn_lot_price(call_scope_t& scope);
  /// Extract the lot date annotation from an amount, if present.
  value_t fn_lot_date(call_scope_t& scope);
  /// Extract the lot tag annotation from an amount, if present.
  value_t fn_lot_tag(call_scope_t& scope);

  /// Dump all session-level option values to the output stream.
  void report_options(std::ostream& out) {
    HANDLER(check_payees).report(out);
    HANDLER(day_break).report(out);
    HANDLER(download).report(out);
    HANDLER(decimal_comma).report(out);
    HANDLER(time_colon).report(out);
    HANDLER(file_).report(out);
    HANDLER(hashes_).report(out);
    HANDLER(input_date_format_).report(out);
    HANDLER(explicit).report(out);
    HANDLER(master_account_).report(out);
    HANDLER(pedantic).report(out);
    HANDLER(permissive).report(out);
    HANDLER(price_db_).report(out);
    HANDLER(price_exp_).report(out);
    HANDLER(recursive_aliases).report(out);
    HANDLER(no_aliases).report(out);
    HANDLER(strict).report(out);
    HANDLER(value_expr_).report(out);
    HANDLER(lot_matching_).report(out);
  }

  /// Resolve an option name to its handler, using first-character dispatch.
  option_t<session_t>* lookup_option(const char* p);

  /// Scope lookup override: resolves session-level functions, options,
  /// and then delegates to symbol_scope_t for user-defined symbols.
  expr_t::ptr_op_t lookup(const symbol_t::kind_t kind, const string& name) override;

  /**
   * Option handlers
   */

  OPTION(session_t, check_payees); ///< Warn on unknown payees (with --strict)
  OPTION(session_t, day_break);    ///< Insert day-break transactions between dates
  OPTION(session_t, download);     ///< Download commodity prices (-Q)
  OPTION(session_t, getquote_);    ///< Path to the price-fetching script

  /// Use comma as decimal separator for commodity amounts.
  OPTION_(session_t, decimal_comma, DO() { commodity_t::decimal_comma_by_default = true; });

  /// Use colon as time separator in time amounts (e.g., 1:30 instead of 1.5).
  OPTION_(session_t, time_colon, DO() { commodity_t::time_colon_by_default = true; });

  /// Maximum age of cached prices before re-fetching (default: 24 hours).
  OPTION_CTOR(
      session_t, price_exp_, // -Z
      CTOR(session_t, price_exp_) { value = "24"; });

  /// Journal data file(s) to read.  Multiple -f options accumulate unless
  /// flush_on_next_data_file causes a reset (implementing the override
  /// semantics for command-line -f over environment/init-file sources).
  OPTION_CTOR(
      session_t, file_, // -f
      std::list<path> data_files;
      CTOR(session_t, file_) {} DO_() {
        if (parent->flush_on_next_data_file) {
          data_files.clear();
          parent->flush_on_next_data_file = false;
        }
        data_files.push_back(str);
      });

  /// Hash algorithm to use for journal file integrity checking.
  OPTION_CTOR(
      session_t, hashes_, hash_type_t hash_type = NO_HASHES; CTOR(session_t, hashes_) {} DO_() {
        if (str == "sha512" || str == "SHA512") {
          hash_type = HASH_SHA512;
        } else if (str == "sha512_half" || str == "SHA512_Half") {
          hash_type = HASH_SHA512_Half;
        } else {
          throw_(std::invalid_argument, _f("Unrecognized hash type"));
        }
      });

  /// Override the default input date format for journal parsing.
  OPTION_(
      session_t, input_date_format_, DO_() {
        // This changes static variables inside times.h, which affects the
        // basic date parser.
        set_input_date_format(str.c_str());
      });

  OPTION(session_t, explicit);        ///< Only accept explicitly declared accounts/commodities
  OPTION(session_t, master_account_); ///< Parent account for all postings
  OPTION_(session_t, pedantic, DO() { parent->journal->checking_style = journal_t::CHECK_ERROR; });
  OPTION_(
      session_t, permissive,
      DO() { parent->journal->checking_style = journal_t::CHECK_PERMISSIVE; });
  OPTION(session_t, price_db_); ///< Path to the price history database file
  OPTION_(session_t, strict, DO() { parent->journal->checking_style = journal_t::CHECK_WARNING; });
  OPTION(session_t, value_expr_);       ///< Expression used to compute posting values
  OPTION(session_t, recursive_aliases); ///< Allow aliases to reference other aliases
  OPTION(session_t, no_aliases);        ///< Disable all account alias expansion

  /// Lot matching policy for automatic commodity disposal: "fifo", "lifo", or "none".
  OPTION_CTOR(
      session_t, lot_matching_, lot_policy_t policy = lot_policy_t::none;
      CTOR(session_t, lot_matching_) {} DO_() {
        if (str == "fifo")
          policy = lot_policy_t::fifo;
        else if (str == "lifo")
          policy = lot_policy_t::lifo;
        else if (str == "none")
          policy = lot_policy_t::none;
        else
          throw_(std::invalid_argument,
                 _f("Unknown lot-matching policy '%1%': must be 'fifo', 'lifo', or 'none'") % str);
      });
};

/**
 * Set the current session context, transferring all static globals to point
 * at the data structures related to this session.  Although Ledger itself is
 * not thread-safe, by locking, switching session context, then unlocking
 * after an operation is done, multiple threads can sequentially make use of
 * the library.  Thus, a session_t maintains all of the information relating
 * to a single usage of the Ledger library.
 */
void set_session_context(session_t* session);

} // namespace ledger
