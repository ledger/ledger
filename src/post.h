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
 * @file   post.h
 * @author John Wiegley
 *
 * @ingroup data
 *
 * @brief  Postings: the line items within transactions that affect accounts
 *
 * A posting is the fundamental unit of accounting change in Ledger.  Each
 * transaction contains one or more postings, and each posting records a
 * debit or credit to a specific account.  In journal syntax, a posting
 * is a single indented line within a transaction:
 *
 *     2024/01/15 Grocery Store
 *         Expenses:Food      $50.00     ; <-- this is a posting
 *         Assets:Checking               ; <-- so is this (amount inferred)
 *
 * Postings inherit from item_t, gaining metadata (tags), notes, dates,
 * and state (cleared/pending).  On top of that, post_t adds the account
 * pointer, the monetary amount, optional cost basis (for currency
 * conversions and lot tracking), and flags describing how the posting
 * was parsed and computed.
 *
 * Tags and metadata use an inheritance model: when a posting is asked
 * for a tag it does not have, it checks the parent transaction (when
 * inherit=true).  This lets users attach metadata at the transaction
 * level and have it apply to all postings within.
 *
 * The payee can be overridden per-posting via a `Payee:` metadata tag
 * or the set_payee() method, falling back to the transaction's payee.
 */
#pragma once

#include "item.h"

namespace ledger {

class xact_t;
class account_t;

/**
 * @brief A single line item within a transaction, recording a debit or credit
 * to an account.
 *
 * A post_t represents one posting in a double-entry transaction.  It holds
 * a pointer to the parent transaction (xact), the target account, the
 * amount being debited or credited, and optional cost/lot information.
 *
 * Postings participate in two key relationships:
 *   - Each posting belongs to exactly one xact_t (via the xact pointer)
 *   - Each posting targets exactly one account_t (via the account pointer)
 *
 * The amount may be null until transaction finalization, at which point
 * Ledger infers it from the other postings to balance the transaction.
 * The cost fields track currency conversions and lot prices:
 *   - cost: the total cost in the target commodity (e.g., $150 for 10 AAPL)
 *   - given_cost: the cost as originally written by the user, before
 *     per-unit conversion
 *   - assigned_amount: a balance assertion or assignment (= $500)
 *
 * Virtual postings (parenthesized accounts) and balanced virtual postings
 * (bracketed accounts) allow off-balance-sheet tracking without affecting
 * the real balance.
 */
class post_t : public item_t {
public:
#define POST_VIRTUAL                                                                               \
  0x0010 ///< Account was specified with (parens) -- a virtual posting not requiring balance.
#define POST_MUST_BALANCE 0x0020 ///< Virtual posting specified with [brackets] that MUST balance.
#define POST_CALCULATED                                                                            \
  0x0040 ///< Amount was computed (e.g., inferred null amount to balance the transaction).
#define POST_COST_CALCULATED 0x0080 ///< Cost was computed rather than explicitly given by the user.
#define POST_COST_IN_FULL                                                                          \
  0x0100 ///< Cost was given as total cost (@@); stored internally as per-unit.
#define POST_COST_FIXATED                                                                          \
  0x0200 ///< Cost is fixated with = indicator ({=...}), locking the lot price.
#define POST_COST_VIRTUAL                                                                          \
  0x0400 ///< Cost was virtualized via (@), meaning the cost is informational only.
#define POST_ANONYMIZED 0x0800 ///< Temporary anonymous posting created during anonymization.
#define POST_DEFERRED 0x1000 ///< Account was specified with <angle brackets> for deferred posting.
#define POST_IS_TIMELOG 0x2000 ///< Posting was generated from timeclock check-in/check-out data.
#define POST_AMOUNT_USER_ANNOTATED                                                                 \
  0x4000 ///< Amount has user-supplied lot annotations (price, date, or tag).
#define POST_AMOUNT_USER_DATE 0x8000 ///< Amount has a user-supplied lot date annotation.
  // Note: FIFO/LIFO auto-matching is indicated by the presence of an
  // annotation without POST_AMOUNT_USER_ANNOTATED

  xact_t* xact;       ///< Parent transaction; only set for posts of regular xacts.
  account_t* account; ///< Target account this posting debits or credits.

  amount_t amount;              ///< The posting amount; can be null until finalization infers it.
  optional<expr_t> amount_expr; ///< Expression that computed the amount, if any.
  std::optional<amount_t>
      cost; ///< Total cost in the cost commodity (e.g., $1500 for 10 AAPL @ $150).
  std::optional<amount_t> given_cost; ///< Cost as originally written, before per-unit conversion.
  std::optional<amount_t> assigned_amount; ///< Balance assertion or assignment amount (= $500).
  optional<datetime_t> checkin;            ///< Timeclock check-in time (for timelog postings).
  optional<datetime_t> checkout;           ///< Timeclock check-out time (for timelog postings).

private:
  optional<string> _payee; ///< Per-posting payee override; falls back to xact payee if unset.

public:
  post_t(account_t* _account = nullptr, flags_t _flags = ITEM_NORMAL)
      : item_t(_flags), xact(nullptr), account(_account) {
    TRACE_CTOR(post_t, "account_t *, flags_t");
  }
  post_t(account_t* _account, const amount_t& _amount, flags_t _flags = ITEM_NORMAL,
         const optional<string>& _note = none)
      : item_t(_flags, _note), xact(nullptr), account(_account), amount(_amount) {
    TRACE_CTOR(post_t, "account_t *, amount_t, flags_t, optional<string>");
  }
  post_t(const post_t& post)
      : item_t(post), xact(post.xact), account(post.account), amount(post.amount), cost(post.cost),
        assigned_amount(post.assigned_amount), checkin(post.checkin), checkout(post.checkout),
        xdata_(post.xdata_) {
    copy_details(post);
    TRACE_CTOR(post_t, "copy");
  }
  ~post_t() override { TRACE_DTOR(post_t); }

  string description() override {
    if (pos) {
      std::ostringstream buf;
      buf << _f("posting at line %1%") % pos->beg_line;
      return buf.str();
    } else {
      return string(_("generated posting"));
    }
  }

  /**
   * @brief Check whether this posting (or its parent transaction) has a tag.
   *
   * First checks the posting's own metadata.  If not found and inherit is
   * true, delegates to the parent transaction.  This inheritance model lets
   * users tag a transaction once and have it apply to all its postings.
   */
  bool has_tag(const string& tag, bool inherit = true) const override;
  bool has_tag(const mask_t& tag_mask, const std::optional<mask_t>& value_mask = {},
               bool inherit = true) const override;

  /**
   * @brief Retrieve a tag value from this posting or its parent transaction.
   *
   * Follows the same inheritance logic as has_tag(): posting first, then
   * parent transaction if inherit is true.
   */
  std::optional<value_t> get_tag(const string& tag, bool inherit = true) const override;
  std::optional<value_t> get_tag(const mask_t& tag_mask,
                                 const std::optional<mask_t>& value_mask = {},
                                 bool inherit = true) const override;

  /**
   * @brief The date to use for valuation purposes.
   *
   * Returns the xdata override if set, otherwise falls back to date().
   * This allows the reporting pipeline to assign a custom valuation date
   * (e.g., for --now or --value-date) separate from the posting's actual date.
   */
  virtual date_t value_date() const;

  /**
   * @brief The effective date for this posting.
   *
   * Resolution priority: (1) xdata date override, (2) auxiliary date if
   * use_aux_date is enabled globally, (3) primary_date().  The --aux-date
   * flag controls whether auxiliary dates take precedence.
   */
  date_t date() const override;

  /**
   * @brief The primary (transaction) date for this posting.
   *
   * Returns: xdata date override if set, else the posting's own _date if
   * set, else the parent transaction's date, else CURRENT_DATE().
   */
  date_t primary_date() const override;

  /**
   * @brief The auxiliary (secondary) date, checking posting then transaction.
   */
  optional<date_t> aux_date() const override;

  /**
   * @brief Extract a payee name from the Payee metadata tag, if present.
   * @return The tag value as a string, or empty string if not found.
   */
  string payee_from_tag() const;

  /**
   * @brief Resolve the payee for this posting.
   *
   * Priority: (1) explicit _payee set via set_payee(), (2) Payee metadata
   * tag on the posting or inherited from the transaction, (3) the parent
   * transaction's payee.
   */
  string payee() const;
  void set_payee(const string& payee) { _payee = payee; }

  /**
   * @brief Whether this posting must participate in transaction balancing.
   *
   * Real postings and balanced virtual postings ([brackets]) must balance.
   * Virtual postings ((parens)) and timelog postings are exempt unless
   * POST_MUST_BALANCE is explicitly set.
   */
  bool must_balance() const {
    return !(has_flags(POST_VIRTUAL) || has_flags(POST_IS_TIMELOG)) || has_flags(POST_MUST_BALANCE);
  }

  /**
   * @brief Resolve an expression name to its implementation in the posting scope.
   *
   * Maps expression identifiers to getter functions.  Single-character
   * aliases provide shortcuts for common fields:
   *   - `a` = amount, `b` = cost, `n`/`N` = count, `O` = total, `R` = real
   *
   * Key named bindings include: amount, cost, price, total, count, account,
   * display_account, payee, virtual, real, has_cost, commodity, any(), all().
   * Falls through to item_t::lookup() for inherited bindings (date, note, etc.).
   */
  expr_t::ptr_op_t lookup(const symbol_t::kind_t kind, const string& name) override;

  /** @brief Evaluate an expression in a scope bound to this posting. */
  amount_t resolve_expr(scope_t& scope, expr_t& expr);

  /** @brief 1-based index of this posting within its parent transaction. */
  std::size_t xact_id() const;
  /** @brief 1-based index of this posting within its account's posting list. */
  std::size_t account_id() const;

  void copy_details(const item_t& item) override {
    const post_t& post(dynamic_cast<const post_t&>(item));
    xdata_ = post.xdata_;
    item_t::copy_details(item);
  }

  bool valid() const;

  /**
   * @brief Extended data computed during the reporting pipeline.
   *
   * The xdata_t struct holds transient state accumulated as postings flow
   * through the filter chain: running totals, display totals, sort keys,
   * visit flags, and an optional account override for display purposes.
   * It is allocated lazily (only when first accessed) to save memory for
   * postings that are never reported.
   */
  struct xdata_t : public supports_flags<uint_least16_t> {
#define POST_EXT_RECEIVED 0x0001 ///< Posting was received by a handler in the filter pipeline.
#define POST_EXT_HANDLED 0x0002  ///< Posting was fully handled by a filter (will not pass further).
#define POST_EXT_DISPLAYED 0x0004 ///< Posting has been output to the user.
#define POST_EXT_DIRECT_AMT                                                                        \
  0x0008 ///< Amount was set directly on xdata, not evaluated from an expression.
#define POST_EXT_SORT_CALC 0x0010  ///< Sort key value has been computed and cached in sort_values.
#define POST_EXT_COMPOUND 0x0020   ///< compound_value is set and should be used instead of amount.
#define POST_EXT_VISITED 0x0040    ///< Posting was visited during report traversal.
#define POST_EXT_MATCHES 0x0080    ///< Posting matches the current query predicate.
#define POST_EXT_CONSIDERED 0x0100 ///< Posting has already been counted toward account totals.
#define POST_EXT_DISPLAY_TOTAL_CACHED 0x0200 ///< display_total has been computed and cached.
#define POST_EXT_PAYEE_CHANGED 0x0400 ///< Payee differs from previous posting in display order.

    value_t visited_value;  ///< Value recorded when the posting was visited.
    value_t compound_value; ///< Compound value set by filters (overrides amount when
                            ///< POST_EXT_COMPOUND).
    value_t total;          ///< Running total accumulated through the filter chain.
    value_t display_total;  ///< Cached stripped display total for output formatting.
    std::size_t count;      ///< Posting index or count within the current report context.
    date_t date;            ///< Overridden date assigned by the reporting pipeline.
    date_t value_date;      ///< Overridden valuation date for price lookups.
    datetime_t datetime;    ///< Full datetime for time-aware reporting.
    account_t* account;     ///< Overridden account for display (e.g., after account rewriting).

    std::list<sort_value_t> sort_values; ///< Cached sort key values for multi-key sorting.

    xdata_t() : supports_flags<uint_least16_t>(), count(0), account(nullptr) {
      TRACE_CTOR(post_t::xdata_t, "");
    }
    xdata_t(const xdata_t& other)
        : supports_flags<uint_least16_t>(other.flags()), visited_value(other.visited_value),
          compound_value(other.compound_value), total(other.total),
          display_total(other.display_total), count(other.count), date(other.date),
          account(other.account), sort_values(other.sort_values) {
      TRACE_CTOR(post_t::xdata_t, "copy");
    }
    ~xdata_t() noexcept { TRACE_DTOR(post_t::xdata_t); }
  };

  // This variable holds optional "extended data" which is usually produced
  // only during reporting, and only for the posting set being reported.
  // It's a memory-saving measure to delay allocation until the last possible
  // moment.
  mutable optional<xdata_t> xdata_;

  bool has_xdata() const { return static_cast<bool>(xdata_); }
  void clear_xdata() { xdata_ = none; }
  xdata_t& xdata() {
    if (!xdata_)
      xdata_ = xdata_t();
    return *xdata_;
  }
  const xdata_t& xdata() const { return const_cast<post_t*>(this)->xdata(); }

  /**
   * @brief Accumulate this posting's value into a running total.
   *
   * Has a fast path optimization: when the expression is simply `amount`
   * (detected via POST_AMOUNT fast_path), the posting amount is read
   * directly without creating scope objects or evaluating through the AST.
   * Otherwise evaluates the expression in a bound scope.  The compound_value
   * in xdata takes precedence if POST_EXT_COMPOUND is set.
   */
  void add_to_value(value_t& value, const optional<expr_t&>& expr = none) const;

  void set_reported_account(account_t* account);

  account_t* reported_account() {
    if (xdata_)
      if (account_t* acct = xdata_->account)
        return acct;
    assert(account);
    return account;
  }

  const account_t* reported_account() const {
    return const_cast<post_t*>(this)->reported_account();
  }

  friend class xact_t;

  /**
   * @brief Comparator for sorting postings by date, then by file sequence.
   *
   * Used to present postings in chronological order.  When two postings
   * share the same date, the tie is broken by their source file sequence
   * number, preserving the order in which they appeared in the journal.
   */
  struct compare_by_date_and_sequence {
    bool operator()(const post_t* left, const post_t* right) const {
      gregorian::date_duration duration = left->primary_date() - right->primary_date();
      if (duration.days() == 0) {
        return ((left->pos ? left->pos->sequence : 0) < (right->pos ? right->pos->sequence : 0));
      } else {
        return duration.days() < 0;
      }
    }
  };
};

class journal_t;

/**
 * @brief Attach a valuation expression to the posting's commodity.
 *
 * Implements the valuation expression cascade: when the user runs
 * `--market` or uses a `Value:` tag, this function determines which
 * valuation expression to attach.  Priority order:
 *   1. `Value:` metadata tag on the posting
 *   2. The account's value_expr
 *   3. The commodity's value_expr
 *   4. The journal's value_expr
 *
 * If the commodity already has a valuation expression in its annotation,
 * this function does nothing (the existing expression takes precedence).
 *
 * When a typed expression (`Value::`) returns an amount, it is divided
 * by the posting quantity to convert from total value to per-unit price.
 */
void extend_post(post_t& post, journal_t& journal);

/**
 * @brief Serialize a posting to an XML property tree.
 *
 * Emits the posting's state, flags, dates, payee override, account
 * reference, amount (or compound value), cost, balance assertion/assignment,
 * note, metadata, and running total into the property tree for XML output.
 */
void put_post(property_tree::ptree& pt, const post_t& post);

} // namespace ledger
