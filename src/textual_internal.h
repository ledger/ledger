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
 * @file   textual_internal.h
 * @author John Wiegley
 * @brief  Internal parser state and helpers for the textual journal parser.
 *
 * @ingroup data
 *
 * This header defines the core parsing machinery that transforms plain-text
 * journal files into the in-memory data structures (xact_t, post_t, etc.).
 * It is internal to the parser implementation and is included only by the
 * three .cc files that compose the textual parser:
 *
 *   - textual.cc           -- top-level parse loop and line dispatch
 *   - textual_xacts.cc     -- transaction and posting parsing
 *   - textual_directives.cc -- directive parsing (account, commodity, etc.)
 *
 * The central type is instance_t, which holds all mutable state for a
 * single file being parsed: the input stream, the apply-directive stack,
 * the parent instance (for include chains), and the time-log state.
 *
 * The apply stack is a key concept: directives like `apply account`,
 * `apply tag`, `apply fixed`, and `apply year` push entries onto this
 * stack, and their effects are inherited by all transactions and postings
 * parsed until a matching `end apply` is encountered.  The stack uses
 * std::variant so that a single list can hold all apply types.
 */
#pragma once

#include <system.hh>

#include "journal.h"
#include "context.h"
#include "xact.h"
#include "post.h"
#include "account.h"
#include "option.h"
#include "query.h"
#include "pstream.h"
#include "pool.h"
#include "session.h"
#include <algorithm>
#include <utility>
#if HAVE_BOOST_PYTHON
#include "pyinterp.h"
#endif

#define TIMELOG_SUPPORT 1 ///< Enable time-tracking (clock-in/out) directives
#if TIMELOG_SUPPORT
#include "timelog.h"
#endif

namespace ledger::detail {

/// A fixed exchange rate: a commodity paired with a per-unit price amount.
/// Used by `apply fixed` / `apply rate` to override market pricing.
using fixed_rate_t = std::pair<commodity_t*, amount_t>;

/**
 * @brief One entry on the apply-directive stack.
 *
 * Each `apply` directive (account, tag, year, fixed/rate) pushes an
 * application_t onto instance_t::apply_stack.  The variant holds the
 * type-specific payload:
 *
 *   - optional<datetime_t> -- `apply year` (saves/restores the epoch)
 *   - account_t*           -- `apply account` (narrows the account root)
 *   - string               -- `apply tag` (tags added to every item)
 *   - fixed_rate_t         -- `apply fixed` / `apply rate` (price override)
 *
 * The label field records the directive keyword (e.g. "account", "tag") so
 * that `end apply <keyword>` can verify it matches the most recent entry.
 */
struct application_t {
  string label; ///< The apply keyword ("account", "tag", "year", "fixed")
  std::variant<optional<datetime_t>, account_t*, string, fixed_rate_t>
      value;                          ///< Type-specific payload
  optional<int> saved_year_directive; ///< Year directive value saved before `apply year` changed it

  application_t(string _label, optional<datetime_t> epoch)
      : label(std::move(_label)), value(epoch) {}
  application_t(string _label, account_t* acct) : label(std::move(_label)), value(acct) {}
  application_t(string _label, string tag) : label(std::move(_label)), value(tag) {}
  application_t(string _label, fixed_rate_t rate) : label(std::move(_label)), value(rate) {}
};

/**
 * @brief The parser instance for a single journal file.
 *
 * instance_t is the workhorse of the textual parser.  One instance is
 * created per file (including files brought in by `include` directives).
 * It owns the apply stack, references the shared parse_context_t for
 * stream/position state, and provides the methods that implement every
 * directive and transaction construct in the Ledger grammar.
 *
 * The parse() method drives a line-by-line loop, dispatching each line to
 * read_next_directive(), which in turn calls the appropriate handler based
 * on the first character of the line.
 *
 * Inherits from scope_t so that expression evaluation inside directives
 * can look up symbols through the parser's scope chain.
 */
class instance_t : public noncopyable, public scope_t {
public:
  parse_context_stack_t& context_stack; ///< Stack of all active file contexts
  parse_context_t& context;             ///< Context for the file this instance is parsing
  std::istream& in;                     ///< Convenience reference to context.stream
  instance_t* parent; ///< Parent instance when parsing an included file (nullptr for top-level)
  std::list<application_t>
      apply_stack;       ///< Stack of active `apply` directives (front = most recent)
  bool no_assertions;    ///< True when --permissive is active (skip balance assertions)
  hash_type_t hash_type; ///< Hash algorithm for transaction chaining (NO_HASHES to disable)
#if TIMELOG_SUPPORT
  time_log_t timelog; ///< Accumulates clock-in/out events for time tracking
#endif

  instance_t(parse_context_stack_t& _context_stack, parse_context_t& _context,
             instance_t* _parent = nullptr, const bool _no_assertions = false,
             const hash_type_t _hash_type = NO_HASHES)
      : context_stack(_context_stack), context(_context), in(*context.stream.get()),
        parent(_parent), no_assertions(_no_assertions), hash_type(_hash_type), timelog(context) {}

  string description() override { return _("textual parser"); }

  /**
   * @brief Collect all apply-stack entries of type T, walking up the
   *        include chain to gather inherited applications.
   */
  template <typename T>
  void get_applications(std::vector<T>& result) {
    for (application_t& state : apply_stack) {
      if (std::holds_alternative<T>(state.value))
        result.push_back(std::get<T>(state.value));
    }
    if (parent)
      parent->get_applications<T>(result);
  }

  /// @brief Return the most recent apply-stack entry of type T, or none.
  template <typename T>
  optional<T> get_application() {
    for (application_t& state : apply_stack) {
      if (std::holds_alternative<T>(state.value))
        return std::get<T>(state.value);
    }
    return parent ? parent->get_application<T>() : none;
  }

  /// @brief Return the innermost `apply account` target, or nullptr if none.
  account_t* top_account() {
    if (optional<account_t*> acct = get_application<account_t*>())
      return *acct;
    else
      return nullptr;
  }

  /// @brief Main parse loop: reads lines until EOF, dispatching each to read_next_directive().
  void parse();

  /**
   * @brief Read the next line from the input stream into context.linebuf.
   * @param[out] line  Set to point into context.linebuf (past any BOM on line 1).
   * @return Length of the line (excluding trailing whitespace), or 0 at EOF.
   */
  std::streamsize read_line(char*& line);

  /// @brief True if the next character in the stream is whitespace (continuation line).
  bool peek_whitespace_line() {
    return (in.good() && !in.eof() && (in.peek() == ' ' || in.peek() == '\t'));
  }
#if HAVE_BOOST_PYTHON
  bool peek_blank_line() {
    return (in.good() && !in.eof() && (in.peek() == '\n' || in.peek() == '\r'));
  }
#endif

  /**
   * @brief Read one line and dispatch it to the appropriate directive or
   *        transaction handler based on the first character.
   * @param error_flag  Sticky flag; when true, continuation lines are silently skipped.
   * @param previous_xact  The last successfully parsed transaction (for hash chaining).
   * @return The parsed xact_t if the line was a transaction, nullptr otherwise.
   */
  xact_t* read_next_directive(bool& error_flag, xact_t* previous_xact);

#if TIMELOG_SUPPORT
  void clock_in_directive(char* line, bool capitalized);
  void clock_out_directive(char* line, bool capitalized);
#endif

  /// @brief Dispatch table for word-based directives (account, commodity, include, etc.).
  /// @return true if a known directive was handled, false to fall through to legacy single-char
  /// directives.
  bool general_directive(char* line);

  /*--- Account Directives ---*/

  /// @brief Handle the `account` directive and its indented sub-directives (alias, payee, value,
  /// default, assert, check, note).
  void account_directive(char* line);
  void account_alias_directive(account_t* account,
                               string alias); ///< Register an alias name that maps to this account
  void account_payee_directive(
      account_t* account,
      string payee); ///< Map a payee pattern to this account for unknown-account resolution
  void account_value_directive(
      account_t* account,
      const string& expr_str); ///< Set a value expression override for the account
  void account_default_directive(
      account_t* account); ///< Make this the default (bucket) account for unbalanced postings

  void default_account_directive(
      char* args); ///< Handle legacy `A` directive (default account for unbalanced postings)
  void alias_directive(char* line); ///< Handle top-level `alias Name=Account` directive
  void command_alias_directive(
      char* line); ///< Handle top-level `command Name = verb [options]` directive

  /*--- Payee Directives ---*/

  void
  payee_directive(char* line); ///< Handle `payee` directive and its sub-directives (alias, uuid)
  void payee_alias_directive(const string& payee,
                             string alias); ///< Map an alias pattern to a canonical payee name
  void payee_uuid_directive(const string& payee,
                            string uuid);     ///< Map a UUID to a canonical payee name
  void payee_rewrite_directive(char* line);   ///< Handle `payee-rewrite PATTERN REPLACEMENT`
  void account_rewrite_directive(char* line); ///< Handle `account-rewrite PATTERN REPLACEMENT`

  /*--- Commodity Directives ---*/

  void commodity_directive(char* line); ///< Handle `commodity` directive and its sub-directives
  void commodity_alias_directive(commodity_t& comm,
                                 string alias); ///< Register an alternate symbol for a commodity
  void commodity_value_directive(
      commodity_t& comm,
      const string& expr_str); ///< Set a value expression for commodity valuation
  void commodity_format_directive(
      commodity_t& comm, string format); ///< Set the display format (precision, symbol placement)
  void commodity_nomarket_directive(
      commodity_t& comm); ///< Mark the commodity as having no market price data
  void commodity_default_directive(
      commodity_t& comm); ///< Make this the default commodity for bare amounts

  void default_commodity_directive(
      char* line); ///< Handle legacy `D` directive (default commodity and its format)

  void tag_directive(
      char* line); ///< Handle `tag` directive (pre-declare metadata tags for --strict mode)

  /*--- Apply / Scope Directives ---*/

  void apply_directive(char* line); ///< Dispatch `apply <keyword>` to the appropriate sub-handler
  void apply_account_directive(char* line); ///< Push an account prefix onto the apply stack
  void apply_tag_directive(char* line);  ///< Push a tag to be auto-applied to all subsequent items
  void apply_rate_directive(char* line); ///< Push a fixed exchange rate onto the apply stack
  void apply_year_directive(
      char* line, bool use_apply_stack =
                      false); ///< Set the default year (optionally via apply stack for `end apply`)
  void end_apply_directive(
      char* line); ///< Pop the most recent apply-stack entry, verifying the keyword matches

  /*--- Transaction Directives ---*/

  xact_t*
  xact_directive(char* line, std::streamsize len,
                 xact_t* previous_xact); ///< Parse a regular transaction (date + payee + postings)
  void
  period_xact_directive(char* line); ///< Parse a periodic (budget) transaction starting with `~`
  void automated_xact_directive(char* line);   ///< Parse an automated transaction starting with `=`
  void price_xact_directive(char* line);       ///< Handle `P` directive (historical price entry)
  void price_conversion_directive(char* line); ///< Handle `C` directive (commodity conversion rate)
  void nomarket_directive(char* line); ///< Handle `N` directive (mark commodity as no-market)

  /*--- File and Evaluation Directives ---*/

  void include_directive(char* line); ///< Handle `include` directive (supports glob patterns)
  void option_directive(char* line);  ///< Handle `--option` lines in journal files
  void comment_directive(char* line); ///< Handle `comment` / `test` block (skip lines until `end
                                      ///< comment` / `end test`)

  void
  eval_directive(char* line); ///< Evaluate a value expression (`eval`, `expr`, `def`, `define`)
  void assert_directive(char* line); ///< Evaluate an expression; throw if false (`assert`)
  void check_directive(char* line);  ///< Evaluate an expression; warn if false (`check`)
  void value_directive(char* line);  ///< Set the journal-wide value expression override

  void import_directive(char* line); ///< Import a Python module (`import`, requires Boost.Python)
  void
  python_directive(char* line); ///< Execute inline Python code (`python`, requires Boost.Python)

  /*--- Core Parsing Methods ---*/

  /**
   * @brief Parse a single posting line into a post_t.
   *
   * A posting line has the form: `  [*|!] Account  Amount [@ Cost] [= Assertion] [; Note]`
   * This method handles all optional components: state flags, virtual account
   * brackets, amount expressions, per-unit and total costs, balance
   * assignments/assertions, and inline notes with metadata tags.
   *
   * @param line       Pointer into the line buffer (past leading whitespace).
   * @param len        Remaining length of the line.
   * @param account    The current root account (from apply account or master).
   * @param xact       The owning transaction (nullptr for automated/period xacts).
   * @param defer_expr If true, amount expressions are stored but not evaluated yet.
   * @return A heap-allocated post_t, or nullptr on failure.
   */
  post_t* parse_post(char* line, std::streamsize len, account_t* account, xact_t* xact,
                     bool defer_expr = false);

  /**
   * @brief Read and parse all indented posting lines following a transaction header.
   * @return true if at least one posting was added.
   */
  bool parse_posts(account_t* account, xact_base_t& xact, const bool defer_expr = false);

  /**
   * @brief Parse a complete transaction: date line, metadata, and all postings.
   *
   * The transaction header line has the form: `DATE[=AUX_DATE] [*|!] [(CODE)] PAYEE [; NOTE]`
   * After parsing the header, this method reads continuation lines for
   * metadata (`;` comments with tags), inline assert/check/expr directives,
   * and posting lines.  It also handles UUID-based payee resolution and
   * hash-chain verification.
   *
   * @param line           The header line (starts with a date).
   * @param len            Length of the header line.
   * @param account        Current root account.
   * @param previous_xact  Previous transaction for hash chaining (may be nullptr).
   * @return A heap-allocated xact_t, or nullptr on failure.
   */
  xact_t* parse_xact(char* line, std::streamsize len, account_t* account, xact_t* previous_xact);

  /// @brief Delegate symbol lookup to the parse context's scope.
  expr_t::ptr_op_t lookup(const symbol_t::kind_t kind, const string& name) override;
};

/**
 * @brief Parse a parenthesized amount expression from the posting line.
 *
 * When a posting's amount begins with `(`, it is treated as a value
 * expression rather than a literal amount.  This function parses and
 * optionally evaluates that expression.
 *
 * @param in          Stream positioned at the start of the expression.
 * @param scope       Scope for evaluating the expression.
 * @param post        The posting being constructed (used for resolve_expr).
 * @param amount      Output: the evaluated amount (unchanged if deferred).
 * @param flags       Parse flags forwarded to the expression parser.
 * @param defer_expr  If true, store the expression without evaluating it.
 * @param amount_expr If non-null, receives a copy of the parsed expression.
 */
void parse_amount_expr(std::istream& in, scope_t& scope, post_t& post, amount_t& amount,
                       const parse_flags_t& flags = PARSE_DEFAULT, const bool defer_expr = false,
                       optional<expr_t>* amount_expr = nullptr);

} // namespace ledger::detail
