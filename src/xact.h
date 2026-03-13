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
 * @addtogroup data
 */

/**
 * @file   xact.h
 * @author John Wiegley
 *
 * @ingroup data
 *
 * @brief  Transaction class hierarchy for Ledger's journal entries.
 *
 * This file defines the three kinds of transactions that appear in a
 * Ledger journal file:
 *
 *   - **xact_t** -- A regular dated transaction, the primary entry that
 *     users write.  In journal syntax this is a date line followed by
 *     postings:
 *     @code
 *       2024/01/15 * Grocery Store
 *           Expenses:Food       $42.50
 *           Assets:Checking
 *     @endcode
 *
 *   - **auto_xact_t** -- An automated transaction, written with `=`:
 *     @code
 *       = /Expenses:Food/
 *           (Budget:Food)       (amount * -1)
 *     @endcode
 *     When a regular posting matches the predicate, the auto_xact
 *     generates additional postings.
 *
 *   - **period_xact_t** -- A periodic (budget) transaction, written
 *     with `~`:
 *     @code
 *       ~ Monthly
 *           Expenses:Food       $500.00
 *           Assets:Checking
 *     @endcode
 *     Used by the `--budget` report to set spending targets.
 *
 * All three derive from xact_base_t, which itself inherits from item_t.
 * xact_base_t holds the common journal pointer and posts list, and
 * provides finalize() -- the function that enforces double-entry
 * balance checking.
 */
#pragma once

#include <utility>

#include "item.h"
#include "predicate.h"
#include "types.h"

namespace ledger {

class journal_t;
class parse_context_t;

/**
 * @brief Abstract base for all transaction types.
 *
 * xact_base_t provides the shared infrastructure for transactions:
 *   - A pointer to the owning journal
 *   - A list of postings (posts)
 *   - finalize() -- the double-entry balance-checking algorithm
 *   - verify() -- a lighter-weight post-finalization balance check
 *   - magnitude() -- the absolute value of the positive side
 *
 * Concrete subclasses (xact_t, auto_xact_t, period_xact_t) add
 * type-specific fields like payee, predicate, or date interval.
 */
class xact_base_t : public item_t {
public:
  journal_t* journal; ///< The journal that owns this transaction (nullptr for temporaries).
  posts_list posts;   ///< All postings belonging to this transaction.

  xact_base_t() : item_t(), journal(nullptr) { TRACE_CTOR(xact_base_t, ""); }
  xact_base_t(const xact_base_t& e);

  ~xact_base_t() override;

  /** @brief Add a posting to this transaction.
   *
   * The posting is appended to the posts list.  Real postings cannot be
   * added to temporary transactions (enforced by assertion), but
   * temporary postings may be added to real transactions.
   */
  virtual void add_post(post_t* post);

  /// Remove a posting from this transaction.  Returns false if not found.
  virtual bool remove_post(post_t* post);

  posts_list::iterator posts_begin() { return posts.begin(); }
  posts_list::iterator posts_end() { return posts.end(); }

  /**
   * @brief Compute the absolute value of the positive side of the transaction.
   *
   * Sums the cost (or amount, if no cost) of all postings with positive
   * amounts.  Used in error messages to give context about the
   * transaction's overall size when reporting imbalances.
   */
  value_t magnitude() const;

  /**
   * @brief Finalize the transaction: infer amounts, compute costs, check balance.
   *
   * This is the core of Ledger's double-entry accounting enforcement.
   * Called after all postings have been added.  The algorithm:
   *   1. Scan all postings, sum amounts, track null-amount postings
   *   2. If exactly one posting has a null amount, infer it from the balance
   *   3. Handle two-commodity transactions by computing conversion prices
   *   4. Apply fixated price annotations to derive costs
   *   5. Process lot matching (FIFO/LIFO) for commodity sales
   *   6. Exchange amounts through the commodity pool for cost basis
   *   7. Compute capital gains/losses from price annotations
   *   8. Verify the transaction balances (sum equals zero per commodity)
   *   9. Register postings with their accounts
   *
   * @return true if the transaction is valid, false if all amounts are null
   *         (indicating the transaction should be ignored).
   * @throws balance_error if the transaction does not balance.
   */
  bool finalize();

  /**
   * @brief Verify that a finalized transaction still balances.
   *
   * A lighter-weight version of finalize() used after automated
   * transactions add new postings.  Recomputes the balance without
   * performing amount inference or cost calculations.
   *
   * @throws balance_error if the transaction does not balance.
   */
  bool verify();

  /// Return true if any posting in this transaction has extended data.
  bool has_xdata();
  /// Clear extended data from all non-temporary postings.
  void clear_xdata();

  virtual bool valid() const { return true; }
};

/**
 * @brief A regular dated transaction -- the primary journal entry.
 *
 * xact_t represents what users write in their journal files: a date,
 * an optional clearing state, an optional code (check number), a payee
 * description, and two or more postings that must balance.
 *
 * In journal syntax:
 * @code
 *   2024/01/15 * (1042) Grocery Store
 *       Expenses:Food       $42.50
 *       Assets:Checking
 * @endcode
 *
 * Here `*` is the clearing state (CLEARED), `(1042)` is the code,
 * and `Grocery Store` is the payee.
 */
class xact_t : public xact_base_t {
public:
  std::optional<string> code; ///< Optional check number or transaction code, e.g., "(1042)".
  string payee;               ///< The payee/description of the transaction.

  xact_t() { TRACE_CTOR(xact_t, ""); }
  xact_t(const xact_t& e);

  ~xact_t() override { TRACE_DTOR(xact_t); }

  string description() override {
    if (pos) {
      std::ostringstream buf;
      buf << _f("transaction at line %1%") % pos->beg_line;
      return buf.str();
    } else {
      return string(_("generated transaction"));
    }
  }

  /** @brief Add a posting and set its xact/parent pointers to this transaction. */
  void add_post(post_t* post) override;

  /**
   * @brief Resolve expression names specific to transactions.
   *
   * Adds bindings for transaction-specific properties:
   *   - `payee` / `p` -- the payee string
   *   - `code` -- the check number / transaction code
   *   - `magnitude` -- absolute value of the positive side
   *   - `any(expr)` -- true if any posting satisfies the expression
   *   - `all(expr)` -- true if all postings satisfy the expression
   *
   * Falls through to item_t::lookup() for base properties.
   */
  expr_t::ptr_op_t lookup(const symbol_t::kind_t kind, const string& name) override;

  bool valid() const override;

  /**
   * @brief Compute a cryptographic hash of this transaction.
   *
   * Produces a SHA-512 hash of the transaction's date, auxiliary date,
   * code, payee, and all postings (sorted for determinism).  The nonce
   * parameter can salt the hash for different purposes.
   *
   * @param nonce      Salt value prepended to the hash input.
   * @param hash_type  HASH_SHA512 for full 64-byte hash, or
   *                   HASH_SHA512_HALF for the first 32 bytes.
   */
  string hash(const string& nonce, hash_type_t hash_type) const;
};

/**
 * @brief An automated transaction triggered by a predicate match.
 *
 * In journal syntax, automated transactions begin with `=` followed
 * by a predicate expression:
 * @code
 *   = /Expenses:Food/
 *       (Budget:Food)       (amount * -1)
 * @endcode
 *
 * When a regular transaction is finalized, each auto_xact_t evaluates
 * its predicate against each posting.  For every match, the auto_xact's
 * template postings are cloned and added to the regular transaction.
 * This enables automatic categorization, budgeting entries, and tax
 * tracking without manual duplication.
 *
 * Key features:
 *   - **Predicate matching**: the predicate is an expression tested
 *     against each posting in a regular transaction
 *   - **Amount expressions**: template posting amounts can be
 *     expressions (e.g., `(50%)` means 50% of the matched posting)
 *   - **Deferred notes**: metadata tags on the auto_xact are applied
 *     to the generated postings (or to matched postings)
 *   - **Quick match optimization**: for common account-name predicates,
 *     a fast AST-walking matcher is used instead of full expression
 *     evaluation, with results cached in memoized_results
 *   - **Account name templating**: `$variable` and `%(format)` in
 *     account names are expanded against the matched posting's scope
 */
class auto_xact_t : public xact_base_t {
public:
  predicate_t
      predicate; ///< Expression that must be true for a posting to trigger this auto transaction.
  optional<string> name; ///< Optional descriptive name for the automated transaction.
  bool try_quick_match;  ///< If true, attempt the fast AST-based predicate matcher before falling
                         ///< back to full evaluation. Disabled after a failure.
  std::map<string, bool>
      memoized_results; ///< Cache of account fullname -> predicate result, for the quick matcher.
                        ///< Cleared if quick matching is disabled.
  bool enabled;         ///< If false, this auto transaction is skipped entirely. Can be toggled by
                        ///< directives.

  optional<expr_t::check_expr_list>
      check_exprs; ///< Optional assertion/check expressions evaluated when the predicate matches.

  /**
   * @brief Holds a deferred metadata tag to be applied to generated postings.
   *
   * When an automated transaction has comment lines with metadata tags,
   * those tags cannot be applied immediately during parsing because the
   * target postings do not exist yet.  Instead, the raw tag text is
   * stored here and applied later during extend_xact().
   *
   * If apply_to_post is non-null, the tag is applied only to the
   * generated posting that corresponds to that template posting.
   * If null, the tag is applied to the matched posting itself.
   */
  struct deferred_tag_data_t {
    string tag_data;         ///< Raw tag text (same format as comment lines).
    bool overwrite_existing; ///< Whether to overwrite existing tags with the same name.
    post_t* apply_to_post;   ///< Template posting this tag applies to, or nullptr for the matched
                             ///< posting.

    deferred_tag_data_t(string _tag_data, bool _overwrite_existing)
        : tag_data(std::move(_tag_data)), overwrite_existing(_overwrite_existing),
          apply_to_post(nullptr) {}
  };

  using deferred_notes_list = std::list<deferred_tag_data_t>;

  optional<deferred_notes_list> deferred_notes; ///< Tags deferred until extend_xact() applies them.
  post_t* active_post; ///< During parsing, the template posting currently being built (used by
                       ///< parse_tags to set apply_to_post).

  auto_xact_t() : try_quick_match(true), active_post(nullptr) { TRACE_CTOR(auto_xact_t, ""); }
  auto_xact_t(const auto_xact_t& other)
      : xact_base_t(other), predicate(other.predicate), name(other.name),
        try_quick_match(other.try_quick_match), enabled(other.enabled),
        active_post(other.active_post) {
    TRACE_CTOR(auto_xact_t, "copy");
  }
  auto_xact_t(const predicate_t& _predicate)
      : predicate(_predicate), name(none), try_quick_match(true), enabled(true),
        active_post(nullptr) {
    TRACE_CTOR(auto_xact_t, "const predicate_t&");
  }
  auto_xact_t(const predicate_t& _predicate, optional<string> _name)
      : predicate(_predicate), name(std::move(_name)), try_quick_match(true), enabled(true),
        active_post(nullptr) {
    TRACE_CTOR(auto_xact_t, "const predicate_t&");
  }

  ~auto_xact_t() override { TRACE_DTOR(auto_xact_t); }

  string description() override {
    if (pos) {
      std::ostringstream buf;
      buf << _f("automated transaction at line %1%") % pos->beg_line;
      return buf.str();
    } else {
      return string(_("generated automated transaction"));
    }
  }

  /**
   * @brief Defer tag parsing until extend_xact() time.
   *
   * Unlike item_t::parse_tags() which applies tags immediately, the
   * auto_xact version stores the raw tag text for later application
   * to generated postings.  The active_post pointer records which
   * template posting the tag was associated with.
   */
  void parse_tags(const char* p, scope_t&, bool overwrite_existing = true) override {
    if (!deferred_notes)
      deferred_notes = deferred_notes_list();
    deferred_notes->push_back(deferred_tag_data_t(p, overwrite_existing));
    deferred_notes->back().apply_to_post = active_post;
  }

  /**
   * @brief Apply this automated transaction to a regular transaction.
   *
   * For each posting in @p xact that matches this auto_xact's predicate,
   * clone the template postings (adjusting amounts, resolving account
   * name templates) and add them to the transaction.  Also applies
   * deferred notes/tags and evaluates any check expressions.
   *
   * After all postings are added, if any must-balance posting was
   * generated, verify() is called to ensure the transaction still
   * balances.
   */
  virtual void extend_xact(xact_base_t& xact, parse_context_t& context);
};

/**
 * @brief A periodic (budget) transaction defined with `~`.
 *
 * Periodic transactions establish recurring budget entries:
 * @code
 *   ~ Monthly
 *       Expenses:Food       $500.00
 *       Assets:Checking
 * @endcode
 *
 * The period string is parsed into a date_interval_t that specifies
 * the recurrence pattern.  The `--budget` report option generates
 * synthetic transactions from these templates at each interval,
 * allowing actual spending to be compared against budgeted amounts.
 *
 * period_xact_t instances are stored in journal_t::period_xacts and
 * are not finalized like regular transactions -- they serve only as
 * templates.
 */
class period_xact_t : public xact_base_t {
public:
  date_interval_t period; ///< Parsed recurrence interval (e.g., "Monthly", "Every 2 weeks").
  string period_string;   ///< Original period text from the journal file.
  string payee;           ///< Optional payee for generated budget/forecast transactions.

  period_xact_t() { TRACE_CTOR(period_xact_t, ""); }
  period_xact_t(const period_xact_t& e)
      : xact_base_t(e), period(e.period), period_string(e.period_string), payee(e.payee) {
    TRACE_CTOR(period_xact_t, "copy");
  }
  period_xact_t(const string& _period) : period(_period), period_string(_period) {
    TRACE_CTOR(period_xact_t, "const string&");
  }

  ~period_xact_t() override { TRACE_DTOR(period_xact_t); }

  string description() override {
    if (pos) {
      std::ostringstream buf;
      buf << _f("periodic transaction at line %1%") % pos->beg_line;
      return buf.str();
    } else {
      return string(_("generated periodic transaction"));
    }
  }
};

/// Serialize a transaction to a property tree for XML/JSON output.
void put_xact(property_tree::ptree& pt, const xact_t& xact);

} // namespace ledger
