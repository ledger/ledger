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
 * @addtogroup report
 */

/**
 * @file   filters.h
 * @author John Wiegley
 *
 * @ingroup report
 *
 * @brief Declarations for all filter handlers in the posting/account pipeline.
 */
#pragma once

/**
 * @defgroup filters Filter Pipeline
 * @ingroup report
 *
 * @brief Chain of Responsibility pipeline for processing postings and accounts.
 *
 * @section overview Pipeline Overview
 *
 * Ledger's reporting engine processes postings through a chain of handler
 * objects, each implementing the Chain of Responsibility pattern.  Every
 * handler wraps the next handler in the chain via a std::shared_ptr; when a
 * posting is passed to operator(), the handler may transform, filter,
 * accumulate, or forward it to the next handler.
 *
 * The chain is constructed by chain_handlers() (chain.h), which calls:
 *   -# chain_post_handlers()   -- builds the inner (downstream) portion
 *   -# chain_pre_post_handlers() -- wraps with outer (upstream) handlers
 *
 * Since each new handler wraps the previous, the last handler added is the
 * outermost (first to receive postings).  Postings flow from the outermost
 * handler inward toward the base output handler.
 *
 * @section base_classes Base Class Hierarchy
 *
 * All handlers derive from `item_handler<T>` (defined in chain.h):
 * @code
 *   template <typename T>
 *   class item_handler : public noncopyable {
 *   protected:
 *     std::shared_ptr<item_handler> handler;  // next handler in chain
 *   public:
 *     virtual void flush();
 *     virtual void operator()(T& item);
 *     virtual void clear();
 *   };
 * @endcode
 *
 * Two type aliases are provided:
 * - `post_handler_ptr = std::shared_ptr<item_handler<post_t>>`
 * - `acct_handler_ptr = std::shared_ptr<item_handler<account_t>>`
 *
 * @section handler_inventory Complete Handler Inventory
 *
 * @subsection cat_collection Collection and Splitting
 * - **post_splitter** -- Groups postings by expression value, flushing a
 *   sub-chain per group.  Used by --group-by.
 * - **collect_posts** -- Accumulates post_t* into a vector; does not forward.
 * - **push_to_posts_list** -- Pushes postings into an external posts_list.
 *
 * @subsection cat_passthrough Pass-Through / Iteration
 * - **pass_down_posts\<Iterator\>** -- Iterates an iterator of postings,
 *   passing each through the handler chain, then flushes.
 * - **pass_down_accounts\<Iterator\>** -- Same for accounts, with optional
 *   predicate filtering.
 * - **ignore_posts** -- Discards all postings (no-op operator()).
 *
 * @subsection cat_filtering Filtering
 * - **filter_posts** -- Only forwards postings matching a predicate_t
 *   expression.  Used for --limit, --display, --only, and internal filters.
 * - **related_posts** -- Collects postings, then on flush() passes through
 *   the related (sibling) postings from each transaction.  Used by --related.
 *
 * @subsection cat_sorting Sorting
 * - **sort_posts** -- Accumulates all postings, sorts by a sort expression on
 *   flush(), then forwards in sorted order.  Used by --sort.
 * - **sort_xacts** -- Sorts postings within each transaction boundary.
 *   Used by --sort-xacts or period-based sorting.
 *
 * @subsection cat_truncation Truncation
 * - **truncate_xacts** -- Limits output to the first head_count and/or last
 *   tail_count transactions.  Used by --head and --tail.
 *
 * @subsection cat_calculation Calculation
 * - **calc_posts** -- Evaluates the amount expression and optionally computes
 *   running totals.  Stores results in posting xdata.  This is the core
 *   calculation stage that most display handlers depend on.
 *
 * @subsection cat_display Display and Revaluation
 * - **display_filter_posts** -- Inserts rounding adjustment postings to keep
 *   displayed totals consistent.  Requires calc_posts downstream.
 * - **changed_value_posts** -- Inserts virtual revaluation postings when
 *   commodity market values change between postings.  Used by --revalued
 *   and --unrealized.  Requires calc_posts downstream.
 *
 * @subsection cat_aggregation Aggregation and Subtotaling
 * - **subtotal_posts** -- Combines all postings into one subtotal transaction
 *   per commodity per account.  Used by --subtotal.
 * - **interval_posts** (extends subtotal_posts) -- Groups postings by time
 *   intervals (daily, weekly, monthly, etc.).  Used by --period.
 * - **collapse_posts** -- Collapses multi-posting transactions into single
 *   subtotaled postings per commodity.  Used by --collapse and --depth.
 * - **by_payee_posts** -- Subtotals postings grouped by payee.  Used by
 *   --by-payee.
 * - **day_of_week_posts** (extends subtotal_posts) -- Accumulates postings
 *   by day of week (0=Sunday through 6=Saturday).  Used by --dow.
 *
 * @subsection cat_generation Synthetic Posting Generation
 * - **generate_posts** -- Base class for generating synthetic postings from
 *   period_xacts (automated periodic transactions in the journal).
 * - **budget_posts** (extends generate_posts) -- Generates budget postings
 *   that balance against reported actuals.  Flags control budgeted-only,
 *   unbudgeted-only, or both.  Used by --budget/--add-budget/--unbudgeted.
 * - **forecast_posts** (extends generate_posts) -- Generates forecast postings
 *   into the future, governed by a while-predicate and year limit.  Used by
 *   --forecast-while.
 *
 * @subsection cat_transformation Transformation
 * - **transfer_details** -- Rewrites a posting's date, account, or payee based
 *   on an expression.  Used by --date, --account, --payee, and --pivot.
 * - **anonymize_posts** -- Replaces all identifying information (payee, account
 *   names, commodity symbols) with anonymized data.  Used by --anon.
 * - **posts_as_equity** (extends subtotal_posts) -- Converts postings into
 *   equity opening-balance format.  Used by --equity.
 *
 * @subsection cat_injection Injection
 * - **inject_posts** -- Injects additional postings based on tag values found
 *   on transactions.  Used by --inject.
 *
 * @section handler_ordering Handler Ordering (from chain.cc)
 *
 * The full posting flow for a register/posting report is shown below.
 * Postings enter at the top and flow downward to the output handler.
 * Handlers marked with (conditional) are only present when the corresponding
 * option is active.
 *
 * @subsection pre_post chain_pre_post_handlers (outermost, added last)
 *
 * @code
 *   [posting source]
 *     → budget_posts or forecast_posts    (conditional, mutually exclusive)
 *       → filter_posts(limit)             (re-applied if budget/forecast active)
 *     → filter_posts(limit)               (conditional: --limit)
 *     → anonymize_posts                   (conditional: --anon)
 * @endcode
 *
 * @subsection post chain_post_handlers (innermost, added first)
 *
 * @code
 *     → inject_posts                      (conditional: --inject)
 *     → related_posts                     (conditional: --related)
 *     → transfer_details(SET_PAYEE)       (conditional: --payee)
 *     → transfer_details(SET_ACCOUNT)     (conditional: --account or --pivot)
 *     → transfer_details(SET_DATE)        (conditional: --date)
 *     → interval_posts                    (conditional: --period)
 *     → day_of_week_posts / by_payee_posts (conditional, mutually exclusive)
 *     → posts_as_equity / subtotal_posts  (conditional, mutually exclusive)
 *     → collapse_posts                    (conditional: --collapse or --depth)
 *     → sort_posts / sort_xacts           (conditional: --sort / --sort-xacts)
 *     → filter_posts(only)                (conditional: --only)
 *     → calc_posts                        (always present)
 *     → changed_value_posts               (conditional: --revalued)
 *     → filter_posts(display)             (conditional: --display, not accounts)
 *     → display_filter_posts              (not accounts report)
 *     → truncate_xacts                    (conditional: --head/--tail, not accounts)
 *     → filter_posts(forecast_while)      (conditional: --forecast-while, not accounts)
 *     → [output handler]
 * @endcode
 *
 * For accounts reports (for_accounts_report=true), the following stages are
 * skipped: forecast_while filter, truncate_xacts, display_filter_posts,
 * display filter, sort, collapse, subtotal/equity.
 *
 * @section mutual_exclusions Mutual Exclusions
 *
 * - **budget_posts** vs **forecast_posts** -- Only one can be active; controlled
 *   by budget_flags vs --forecast-while in chain_pre_post_handlers().
 * - **posts_as_equity** vs **subtotal_posts** -- --equity takes precedence over
 *   --subtotal.
 * - **day_of_week_posts** vs **by_payee_posts** -- --dow takes precedence over
 *   --by-payee.
 * - **sort_posts** vs **sort_xacts** (partial) -- When --sort and --sort-xacts
 *   are both specified, sort_xacts runs within-transaction first, then
 *   sort_posts applies the global sort.  When period sorting activates
 *   sort_xacts alone, sort_posts is skipped.
 *
 * @section adding_handler Adding a New Handler
 *
 * To add a new filter to the pipeline:
 *
 * 1. Define a new class in this file inheriting from `item_handler<post_t>`.
 *    Implement operator()(post_t&) and optionally flush() and clear().
 *
 * 2. In chain.cc, add a conditional block in chain_pre_post_handlers() or
 *    chain_post_handlers() that wraps the handler chain:
 *    @code
 *      if (report.HANDLED(my_option))
 *        handler.reset(new my_handler(handler, ...));
 *    @endcode
 *
 * 3. Position matters: handlers added later in the function execute earlier
 *    in the posting flow (they are outermost in the chain).
 *
 * 4. If the handler needs calc_posts results, place it after (i.e., add it
 *    before) the calc_posts creation in chain_post_handlers().
 *
 * 5. Add a corresponding OPTION declaration in report.h to expose the
 *    command-line flag.  See option.h for the macro system.
 *
 * 6. Implement clear() to reset all mutable state so the handler can be
 *    reused across multiple report runs.
 */

#include "chain.h"
#include "xact.h"
#include "post.h"
#include "account.h"
#include "temps.h"

#include <boost/random/mersenne_twister.hpp>
#include <boost/random/uniform_int.hpp>
#include <boost/random/variate_generator.hpp>
#include <utility>

namespace ledger {

using namespace boost::placeholders;

//////////////////////////////////////////////////////////////////////
//
// Posting collector
//

/**
 * @brief Groups postings by the result of an expression, flushing a
 *        sub-chain per group.
 *
 * Used by --group-by to partition postings into buckets keyed by the
 * group_by_expr value.  Each bucket is flushed through the post_chain
 * independently, with optional pre- and post-flush callbacks (used, for
 * example, to print group titles).
 */
class post_splitter : public item_handler<post_t> {
public:
  using value_to_posts_map = std::map<value_t, posts_list>;
  using custom_flusher_t = function<void(const value_t&)>;

protected:
  value_to_posts_map posts_map;   ///< Postings bucketed by group_by_expr result.
  post_handler_ptr post_chain;    ///< Handler chain to flush each group through.
  report_t& report;               ///< Report context for expression evaluation.
  expr_t& group_by_expr;          ///< Expression whose value partitions postings into groups.
  custom_flusher_t preflush_func; ///< Called before each group is flushed (default: print_title).
  optional<custom_flusher_t> postflush_func; ///< Called after each group is flushed, if set.

public:
  post_splitter(post_handler_ptr _post_chain, report_t& _report, expr_t& _group_by_expr)
      : post_chain(std::move(_post_chain)), report(_report), group_by_expr(_group_by_expr) {
    preflush_func = bind(&post_splitter::print_title, this, _1);
    TRACE_CTOR(post_splitter, "scope_t&, post_handler_ptr, expr_t");
  }
  ~post_splitter() override { TRACE_DTOR(post_splitter); }

  void set_preflush_func(custom_flusher_t functor) { preflush_func = std::move(functor); }
  void set_postflush_func(custom_flusher_t functor) { postflush_func = functor; }

  virtual void print_title(const value_t& val);

  void flush() override;
  void operator()(post_t& post) override;

  void clear() override {
    posts_map.clear();
    post_chain->clear();
    item_handler<post_t>::clear();
  }
};

//////////////////////////////////////////////////////////////////////
//
// Posting filters
//

/// Discards all postings -- a no-op terminal handler.
class ignore_posts : public item_handler<post_t> {
public:
  void operator()(post_t&) override {}
};

/// Accumulates posting pointers into a vector without forwarding downstream.
class collect_posts : public item_handler<post_t> {
public:
  std::vector<post_t*> posts;

  collect_posts() : item_handler<post_t>() { TRACE_CTOR(collect_posts, ""); }
  ~collect_posts() override { TRACE_DTOR(collect_posts); }

  std::size_t length() const { return posts.size(); }

  std::vector<post_t*>::iterator begin() { return posts.begin(); }
  std::vector<post_t*>::iterator end() { return posts.end(); }

  void flush() override {}
  void operator()(post_t& post) override { posts.push_back(&post); }

  void clear() override {
    posts.clear();
    item_handler<post_t>::clear();
  }
};

/**
 * @brief Iterates an external posting iterator, passing each posting through
 *        the handler chain, then flushes.
 *
 * This is the entry point that drives postings from a journal iterator into
 * the filter pipeline.  All work happens in the constructor.
 */
template <typename Iterator>
class pass_down_posts : public item_handler<post_t> {
  pass_down_posts();

public:
  pass_down_posts(post_handler_ptr handler, Iterator& iter) : item_handler<post_t>(handler) {
    while (post_t* post = *iter) {
      try {
        item_handler<post_t>::operator()(*post);
      } catch (const std::exception&) {
        add_error_context(item_context(*post, _("While handling posting")));
        throw;
      }
      iter.increment();
    }

    item_handler<post_t>::flush();

    TRACE_CTOR(pass_down_posts, "post_handler_ptr, posts_iterator");
  }

  ~pass_down_posts() override { TRACE_DTOR(pass_down_posts); }
};

/// Appends each posting pointer to an external posts_list (no forwarding).
class push_to_posts_list : public item_handler<post_t> {
  push_to_posts_list();

public:
  posts_list& posts;

  push_to_posts_list(posts_list& _posts) : posts(_posts) {
    TRACE_CTOR(push_to_posts_list, "posts_list&");
  }
  ~push_to_posts_list() override { TRACE_DTOR(push_to_posts_list); }

  void operator()(post_t& post) override { posts.push_back(&post); }
};

/**
 * @brief Limits output to the first and/or last N transactions.
 *
 * Used by --head and --tail.  Postings are accumulated until the transaction
 * boundaries can be determined, then only those within the head/tail window
 * are forwarded on flush().  Negative counts invert the direction (e.g.,
 * head_count=-2 skips the first 2 transactions).
 */
class truncate_xacts : public item_handler<post_t> {
  int head_count; ///< Number of transactions to keep from the start (--head).
  int tail_count; ///< Number of transactions to keep from the end (--tail).
  bool completed; ///< True once early termination has occurred.

  posts_list posts;       ///< All postings seen so far (for deferred flush).
  std::size_t xacts_seen; ///< Count of distinct transactions encountered.
  xact_t* last_xact;      ///< Previous transaction, for boundary detection.

  truncate_xacts();

public:
  truncate_xacts(post_handler_ptr handler, int _head_count, int _tail_count)
      : item_handler<post_t>(std::move(handler)), head_count(_head_count), tail_count(_tail_count),
        completed(false), xacts_seen(0), last_xact(nullptr) {
    TRACE_CTOR(truncate_xacts, "post_handler_ptr, int, int");
  }
  ~truncate_xacts() override { TRACE_DTOR(truncate_xacts); }

  void flush() override;
  void operator()(post_t& post) override;

  void clear() override {
    completed = false;
    posts.clear();
    xacts_seen = 0;
    last_xact = nullptr;

    item_handler<post_t>::clear();
  }
};

/**
 * @brief Accumulates all postings and sorts them by a value expression.
 *
 * Used by --sort.  All postings are collected, then sorted on flush() using
 * std::stable_sort with compare_items, and forwarded in sorted order.
 */
class sort_posts : public item_handler<post_t> {
  using posts_deque = std::deque<post_t*>;

  posts_deque posts; ///< Accumulated postings awaiting sort.
  expr_t sort_order; ///< Expression defining the sort key.
  report_t& report;  ///< Report context for expression evaluation.

  sort_posts();

public:
  sort_posts(post_handler_ptr handler, const expr_t& _sort_order, report_t& _report)
      : item_handler<post_t>(std::move(handler)), sort_order(_sort_order), report(_report) {
    TRACE_CTOR(sort_posts, "post_handler_ptr, const value_expr&, report_t&");
  }
  sort_posts(post_handler_ptr handler, const string& _sort_order, report_t& _report)
      : item_handler<post_t>(std::move(handler)), sort_order(_sort_order), report(_report) {
    TRACE_CTOR(sort_posts, "post_handler_ptr, const string&, report_t&");
  }
  ~sort_posts() override { TRACE_DTOR(sort_posts); }

  virtual void post_accumulated_posts();

  void flush() override {
    post_accumulated_posts();
    item_handler<post_t>::flush();
  }

  void operator()(post_t& post) override { posts.push_back(&post); }

  void clear() override {
    posts.clear();
    sort_order.mark_uncompiled();

    item_handler<post_t>::clear();
  }
};

/**
 * @brief Sorts postings within each transaction boundary.
 *
 * Used by --sort-xacts or when period sorting activates within-transaction
 * ordering.  Delegates to an internal sort_posts instance, flushing it each
 * time a new transaction is encountered.
 */
class sort_xacts : public item_handler<post_t> {
  sort_posts sorter; ///< Internal sorter flushed per transaction.
  xact_t* last_xact; ///< Previous transaction, for boundary detection.

  sort_xacts();

public:
  sort_xacts(post_handler_ptr handler, const expr_t& _sort_order, report_t& _report)
      : sorter(std::move(handler), _sort_order, _report) {
    TRACE_CTOR(sort_xacts, "post_handler_ptr, const value_expr&, report_t&");
  }
  sort_xacts(post_handler_ptr handler, const string& _sort_order, report_t& _report)
      : sorter(std::move(handler), _sort_order, _report) {
    TRACE_CTOR(sort_xacts, "post_handler_ptr, const string&, report_t&");
  }
  ~sort_xacts() override { TRACE_DTOR(sort_xacts); }

  void flush() override {
    sorter.flush();
    item_handler<post_t>::flush();
  }

  void operator()(post_t& post) override {
    if (last_xact && post.xact != last_xact)
      sorter.post_accumulated_posts();

    sorter(post);

    last_xact = post.xact;
  }

  void clear() override {
    sorter.clear();
    last_xact = nullptr;

    item_handler<post_t>::clear();
  }
};

/**
 * @brief Forwards only postings that match a predicate expression.
 *
 * Used for --limit, --display, --only, and internal filter predicates.
 * Matching postings are flagged with POST_EXT_MATCHES so downstream handlers
 * (e.g., forecast_posts) can detect which postings matched the report query.
 */
class filter_posts : public item_handler<post_t> {
  predicate_t pred; ///< The predicate expression to evaluate.
  scope_t& context; ///< Scope for binding the predicate to each posting.

  filter_posts();

public:
  filter_posts(post_handler_ptr handler, const predicate_t& predicate, scope_t& _context)
      : item_handler<post_t>(std::move(handler)), pred(predicate), context(_context) {
    TRACE_CTOR(filter_posts, "post_handler_ptr, predicate_t, scope_t&");
  }
  ~filter_posts() override { TRACE_DTOR(filter_posts); }

  void operator()(post_t& post) override {
    bind_scope_t bound_scope(context, post);
    if (pred(bound_scope)) {
      post.xdata().add_flags(POST_EXT_MATCHES);
      (*handler)(post);
    }
  }

  void clear() override {
    pred.mark_uncompiled();
    item_handler<post_t>::clear();
  }
};

/**
 * @brief Replaces all identifying information with anonymized data.
 *
 * Used by --anon for creating sanitized bug reports.  Payees, account names,
 * and commodity symbols are replaced with deterministic hashes or sequential
 * labels.  Amounts are preserved but their commodities are relabeled (A, B,
 * C, ...).  Notes, tags, and transaction codes are stripped.
 */
class anonymize_posts : public item_handler<post_t> {
  using commodity_index_map = std::map<commodity_t*, std::size_t>;
  using int_generator_t = variate_generator<mt19937&, uniform_int<>>;

  temporaries_t temps;         ///< Temporary storage for anonymized xacts/posts.
  commodity_index_map comms;   ///< Maps original commodities to sequential IDs.
  std::size_t next_comm_id;    ///< Next available commodity ID.
  xact_t* last_xact;           ///< Previous xact, to avoid re-anonymizing within the same xact.
  mt19937 rnd_gen;             ///< Mersenne Twister RNG for hash salting.
  uniform_int<> integer_range; ///< Range for random integer generation.
  int_generator_t integer_gen; ///< Random integer generator for hash input.

  anonymize_posts();

public:
  anonymize_posts(post_handler_ptr handler)
      : item_handler<post_t>(std::move(handler)), next_comm_id(0), last_xact(nullptr),
        rnd_gen(static_cast<unsigned int>(static_cast<boost::uintmax_t>(std::time(nullptr)))),
        integer_range(1, 2000000000L), integer_gen(rnd_gen, integer_range) {
    TRACE_CTOR(anonymize_posts, "post_handler_ptr");
  }
  ~anonymize_posts() override {
    TRACE_DTOR(anonymize_posts);
    handler.reset();
  }

  void render_commodity(amount_t& amt);

  void operator()(post_t& post) override;

  void clear() override {
    temps.clear();
    comms.clear();
    last_xact = nullptr;

    item_handler<post_t>::clear();
  }
};

/**
 * @brief Core calculation stage: evaluates amounts and computes running totals.
 *
 * This is the fundamental calculation handler that most display handlers
 * depend on.  For each posting it evaluates the amount expression, stores
 * the result in the posting's xdata, and optionally maintains a running
 * total across postings.  The running total is what the "register" command
 * displays in its rightmost column.
 *
 * When maintain_stripped_total is true, a pre-stripped version of the
 * running total is maintained incrementally (O(1) per posting rather than
 * O(K) where K = number of annotated commodities), allowing
 * display_filter_posts to skip expensive full-stripping.
 */
class calc_posts : public item_handler<post_t> {
  post_t* last_post;            ///< Previous posting, for running total propagation.
  expr_t& amount_expr;          ///< Expression evaluated to compute each posting's value.
  bool calc_running_total;      ///< Whether to maintain a running total across postings.
  bool maintain_stripped_total; ///< Whether to incrementally maintain a stripped display total.
  keep_details_t what_to_keep;  ///< Annotation details to preserve when stripping.

  /// When true, use per-account running totals for synthetic (ITEM_GENERATED)
  /// postings instead of the global running total.
  ///
  /// This is enabled when --average (-A) is active.  Without it, a period
  /// report with multiple accounts per interval (e.g. --monthly with two
  /// expense accounts) would blend all accounts into a single global total,
  /// causing --average to produce wrong per-account averages.  With it, each
  /// account accumulates only its own postings, so every account's average is
  /// computed independently.
  bool period_average_;

  /// Per-account running totals, populated only when period_average_ is true.
  std::map<account_t*, value_t> account_period_totals_;

  calc_posts();

public:
  calc_posts(post_handler_ptr handler, expr_t& _amount_expr, bool _calc_running_total = false,
             bool _maintain_stripped = false,
             const keep_details_t& _what_to_keep = keep_details_t(), bool _period_average = false)
      : item_handler<post_t>(std::move(handler)), last_post(nullptr), amount_expr(_amount_expr),
        calc_running_total(_calc_running_total), maintain_stripped_total(_maintain_stripped),
        what_to_keep(_what_to_keep), period_average_(_period_average) {
    TRACE_CTOR(calc_posts, "post_handler_ptr, expr_t&, bool");
  }
  ~calc_posts() override { TRACE_DTOR(calc_posts); }

  void operator()(post_t& post) override;

  void clear() override {
    last_post = nullptr;
    account_period_totals_.clear();
    amount_expr.mark_uncompiled();

    item_handler<post_t>::clear();
  }
};

/**
 * @brief Collapses multi-posting transactions into single subtotaled postings.
 *
 * Used by --collapse and --depth.  Within each transaction, all component
 * postings are accumulated and replaced by a single synthetic posting (or
 * one per account when --depth is active).  The collapse can be conditional:
 * --collapse-if-zero suppresses output entirely when the subtotal is zero,
 * and passes component postings through uncollapsed otherwise.
 */
class collapse_posts : public item_handler<post_t> {

  using totals_map = std::map<account_t*, value_t>;

  expr_t& amount_expr;                ///< Amount expression for evaluating each posting's value.
  predicate_t display_predicate;      ///< Display predicate from --display.
  predicate_t only_predicate;         ///< Secondary predicate from --only.
  value_t subtotal;                   ///< Running subtotal of the current transaction.
  std::size_t count;                  ///< Number of postings accumulated for the current xact.
  xact_t* last_xact;                  ///< Previous transaction, for boundary detection.
  post_t* last_post;                  ///< Last posting seen in the current transaction.
  temporaries_t temps;                ///< Temporary storage for synthetic xacts/posts.
  account_t* global_totals_account;   ///< Account used when collapse_depth is 0.
  totals_map totals;                  ///< Per-account totals within the current transaction.
  bool only_collapse_if_zero;         ///< When true, only collapse when subtotal is zero
                                      ///< (--collapse-if-zero).
  unsigned short collapse_depth;      ///< Account depth at which to collapse (0 = all into one).
  std::list<post_t*> component_posts; ///< Original postings accumulated for the current xact.
  report_t& report;                   ///< Report context.

  collapse_posts();

public:
  collapse_posts(post_handler_ptr handler, report_t& _report, expr_t& _amount_expr,
                 const predicate_t& _display_predicate, const predicate_t& _only_predicate,
                 bool _only_collapse_if_zero = false, unsigned short _collapse_depth = 0)
      : item_handler<post_t>(std::move(handler)), amount_expr(_amount_expr),
        display_predicate(_display_predicate), only_predicate(_only_predicate), count(0),
        last_xact(nullptr), last_post(nullptr), only_collapse_if_zero(_only_collapse_if_zero),
        collapse_depth(_collapse_depth), report(_report) {
    create_accounts();
    TRACE_CTOR(collapse_posts, "post_handler_ptr, ...");
  }
  ~collapse_posts() override {
    TRACE_DTOR(collapse_posts);
    handler.reset();
  }

  void create_accounts();

  value_t& find_totals(account_t* account);

  void flush() override {
    report_subtotal();
    item_handler<post_t>::flush();
  }

  void report_subtotal();

  void operator()(post_t& post) override;

  void clear() override {
    amount_expr.mark_uncompiled();
    display_predicate.mark_uncompiled();
    only_predicate.mark_uncompiled();

    subtotal = value_t();
    count = 0;
    last_xact = nullptr;
    last_post = nullptr;

    temps.clear();
    create_accounts();
    totals.clear();
    component_posts.clear();

    item_handler<post_t>::clear();
  }
};

/**
 * @brief Collects postings, then on flush() forwards the related (sibling)
 *        postings from each transaction.
 *
 * Used by --related.  For each collected posting, all other postings in the
 * same transaction are forwarded (unless already handled).  When
 * also_matching is true, the original matching postings are also included.
 */
class related_posts : public item_handler<post_t> {
  posts_list posts;   ///< Collected postings whose siblings will be forwarded.
  bool also_matching; ///< If true, include the originally matched postings too (--related-all).

  related_posts();

public:
  related_posts(post_handler_ptr handler, const bool _also_matching = false)
      : item_handler<post_t>(std::move(handler)), also_matching(_also_matching) {
    TRACE_CTOR(related_posts, "post_handler_ptr, const bool");
  }
  ~related_posts() noexcept override { TRACE_DTOR(related_posts); }

  void flush() override;
  void operator()(post_t& post) override {
    post.xdata().add_flags(POST_EXT_RECEIVED);
    posts.push_back(&post);
  }

  void clear() override {
    posts.clear();
    item_handler<post_t>::clear();
  }
};

/**
 * @brief Inserts rounding adjustment postings to keep displayed totals consistent.
 *
 * This filter sits downstream of calc_posts and upstream of the output
 * handler.  It compares the stripped display total against the last known
 * total and, if they differ due to rounding in display_rounded(), emits a
 * synthetic posting to the <Adjustment> account to compensate.  This
 * prevents the running total column from showing misleading jumps.
 *
 * Also acts as gatekeeper: postings whose display_amount rounds to zero
 * are suppressed unless --empty is specified.
 *
 * @note Requires calc_posts somewhere downstream in the chain.
 */
class display_filter_posts : public item_handler<post_t> {
  report_t& report;                    ///< Report context.
  expr_t& display_amount_expr;         ///< Expression for computing the display amount.
  expr_t& display_total_expr;          ///< Expression for computing the display total.
  bool show_rounding;                  ///< Whether to emit rounding adjustment postings.
  value_t last_display_total;          ///< Previous posting's display total (for diff computation).
  value_t last_stripped_display_total; ///< Cached stripped total for incremental optimization.
  bool has_stripped_cache;             ///< Whether the incremental stripped cache is valid.
  bool incremental_strip_eligible; ///< Whether the O(1) incremental stripping optimization can be
                                   ///< used.
  keep_details_t what_to_keep;     ///< Annotation details to preserve when stripping.
  temporaries_t temps;             ///< Temporary storage for synthetic adjustment postings.
  account_t* rounding_account;     ///< The <Adjustment> account for rounding postings.

  display_filter_posts();

public:
  account_t* revalued_account;

  display_filter_posts(post_handler_ptr handler, report_t& _report, bool _show_rounding);

  ~display_filter_posts() override {
    TRACE_DTOR(display_filter_posts);
    handler.reset();
  }

  void create_accounts() {
    rounding_account = &temps.create_account(_("<Adjustment>"));
    revalued_account = &temps.create_account(_("<Revalued>"));
  }

  bool output_rounding(post_t& post);

  void operator()(post_t& post) override;

  void clear() override {
    display_amount_expr.mark_uncompiled();
    display_total_expr.mark_uncompiled();

    last_display_total = value_t();
    last_stripped_display_total = value_t();
    has_stripped_cache = false;

    temps.clear();
    item_handler<post_t>::clear();

    create_accounts();
  }
};

/**
 * @brief Inserts virtual revaluation postings when commodity market values change.
 *
 * Used by --revalued and --unrealized.  Between consecutive postings, this
 * filter re-evaluates the running total at market prices.  If the value has
 * changed, it emits a synthetic posting to the <Revalued> account (for
 * register reports) or to equity gain/loss accounts (for balance reports
 * with --unrealized).
 *
 * The filter also handles intermediate price changes by scanning the
 * commodity price database for price points between posting dates and
 * generating a revaluation for each.
 *
 * @note Requires calc_posts somewhere downstream in the chain.
 */
class changed_value_posts : public item_handler<post_t> {
  report_t& report;   ///< Report context.
  expr_t& total_expr; ///< Expression for computing the total to compare (may be revalued_total_).
  expr_t& display_total_expr; ///< Expression for computing display totals.
  bool changed_values_only; ///< If true, only show postings where value changed (--revalued-only).
  bool historical_prices_only; ///< If true, skip intermediate price lookups (--historical).
  bool for_accounts_report;    ///< Whether this is an accounts (balance) report.
  bool show_unrealized;        ///< Whether to generate equity gain/loss postings (--unrealized).
  post_t* last_post;           ///< Previous posting for comparison.
  value_t last_total;          ///< Total after the previous posting.
  value_t repriced_total;      ///< Total after repricing (set by output_revaluation).
  temporaries_t temps;         ///< Temporary storage for synthetic revaluation postings.
  account_t* revalued_account; ///< The <Revalued> account for register revaluations.
  account_t* gains_equity_account;  ///< Equity account for unrealized gains.
  account_t* losses_equity_account; ///< Equity account for unrealized losses.

  display_filter_posts*
      display_filter; ///< Pointer to display_filter_posts to share the <Revalued> account.

  changed_value_posts();

public:
  changed_value_posts(post_handler_ptr handler, report_t& _report, bool _for_accounts_report,
                      bool _show_unrealized, display_filter_posts* _display_filter);

  ~changed_value_posts() override {
    TRACE_DTOR(changed_value_posts);
    temps.clear();
    handler.reset();
  }

  void create_accounts() {
    revalued_account = (display_filter ? display_filter->revalued_account
                                       : &temps.create_account(_("<Revalued>")));
  }

  void flush() override;

  void output_revaluation(post_t& post, const date_t& current);
  void output_intermediate_prices(post_t& post, const date_t& current);

  void operator()(post_t& post) override;

  void clear() override {
    total_expr.mark_uncompiled();
    display_total_expr.mark_uncompiled();

    last_post = nullptr;
    last_total = value_t();

    temps.clear();
    item_handler<post_t>::clear();

    create_accounts();
  }
};

/**
 * @brief Combines all postings into one subtotal transaction per account.
 *
 * Used by --subtotal.  All incoming postings are accumulated by account
 * (with virtual postings tracked separately).  On flush(), a single
 * synthetic transaction is emitted containing one posting per
 * account/commodity combination, with the payee showing the date range.
 *
 * This class also serves as the base for interval_posts, posts_as_equity,
 * and day_of_week_posts.
 */
class subtotal_posts : public item_handler<post_t> {
  subtotal_posts();

protected:
  /// Holds the accumulated value for a single account within the subtotal.
  class acct_value_t {
    acct_value_t();

  public:
    account_t* account; ///< The account being subtotaled.
    value_t value;      ///< Accumulated value for this account.
    bool is_virtual;    ///< Whether these postings are virtual (unbalanced).
    bool must_balance;  ///< Whether virtual postings must still balance.

    acct_value_t(account_t* a, bool _is_virtual = false, bool _must_balance = false)
        : account(a), is_virtual(_is_virtual), must_balance(_must_balance) {
      TRACE_CTOR(acct_value_t, "account_t *, bool, bool");
    }
    acct_value_t(account_t* a, value_t& v, bool _is_virtual = false, bool _must_balance = false)
        : account(a), value(v), is_virtual(_is_virtual), must_balance(_must_balance) {
      TRACE_CTOR(acct_value_t, "account_t *, value_t&, bool, bool");
    }
    acct_value_t(const acct_value_t& av)
        : account(av.account), value(av.value), is_virtual(av.is_virtual),
          must_balance(av.must_balance) {
      TRACE_CTOR(acct_value_t, "copy");
    }
    acct_value_t& operator=(const acct_value_t&) = default;
    ~acct_value_t() noexcept { TRACE_DTOR(acct_value_t); }
  };

  using values_map = std::map<string, acct_value_t>;
  using values_pair = std::pair<string, acct_value_t>;

protected:
  expr_t& amount_expr;                 ///< Expression for evaluating each posting's value.
  values_map values;                   ///< Accumulated values keyed by account name.
  optional<string> date_format;        ///< Custom date format for the subtotal payee string.
  temporaries_t temps;                 ///< Temporary storage for synthetic xacts/posts.
  std::deque<post_t*> component_posts; ///< Original postings contributing to this subtotal.

public:
  subtotal_posts(post_handler_ptr handler, expr_t& _amount_expr,
                 const optional<string>& _date_format = none)
      : item_handler<post_t>(std::move(handler)), amount_expr(_amount_expr),
        date_format(_date_format) {
    TRACE_CTOR(subtotal_posts, "post_handler_ptr, expr_t&, const optional<string>&");
  }
  ~subtotal_posts() override {
    TRACE_DTOR(subtotal_posts);
    handler.reset();
  }

  void report_subtotal(const char* spec_fmt = nullptr,
                       const optional<date_interval_t>& interval = none);

  void flush() override {
    if (values.size() > 0)
      report_subtotal();
    item_handler<post_t>::flush();
  }
  void operator()(post_t& post) override;

  void clear() override {
    amount_expr.mark_uncompiled();
    values.clear();
    temps.clear();
    component_posts.clear();

    item_handler<post_t>::clear();
  }
};

/**
 * @brief Groups postings by time intervals and subtotals each period.
 *
 * Used by --period (e.g., -M for monthly, -W for weekly).  When a duration
 * is present, all postings are collected in operator() and then sorted by
 * date in flush(), which walks the intervals forward, subtotaling each
 * period.  Empty periods can optionally generate zero-amount postings
 * (--empty).
 *
 * Extends subtotal_posts, reusing its accumulation and report_subtotal
 * infrastructure but calling it once per interval rather than once overall.
 */
class interval_posts : public subtotal_posts {
  date_interval_t start_interval; ///< The original interval specification (preserved for clear()).
  date_interval_t interval;       ///< Working copy advanced as periods are processed.
  account_t* empty_account;       ///< The <None> account used for empty-period postings.
  bool exact_periods;             ///< If true, use exact period boundaries for report_subtotal.
  bool generate_empty_posts; ///< If true, emit zero-amount postings for empty periods (--empty).
  bool align_intervals;      ///< If true, align periods to calendar boundaries (--align-intervals).
  optional<date_t>
      begin_of_report_; ///< Report begin date (-b) for anchoring empty leading periods.
  optional<date_t>
      end_of_report_; ///< Report end date (-e) for generating empty trailing periods.

  std::deque<post_t*> all_posts; ///< All postings seen, sorted by date in flush().

  interval_posts();

public:
  interval_posts(post_handler_ptr _handler, expr_t& amount_expr, const date_interval_t& _interval,
                 bool _exact_periods = false, bool _generate_empty_posts = false,
                 bool _align_intervals = false, const optional<date_t>& _begin_of_report = none,
                 const optional<date_t>& _end_of_report = none)
      : subtotal_posts(std::move(_handler), amount_expr), start_interval(_interval),
        interval(start_interval), exact_periods(_exact_periods),
        generate_empty_posts(_generate_empty_posts), align_intervals(_align_intervals),
        begin_of_report_(_begin_of_report), end_of_report_(_end_of_report) {
    create_accounts();
    TRACE_CTOR(interval_posts, "post_handler_ptr, expr_t&, date_interval_t, bool, bool");
  }
  ~interval_posts() noexcept override { TRACE_DTOR(interval_posts); }

  void create_accounts() { empty_account = &temps.create_account(_("<None>")); }

  void report_subtotal(const date_interval_t& ival);

#if DEBUG_ON
  void debug_interval(const date_interval_t& ival) {
    if (ival.start)
      DEBUG("filters.interval", "start  = " << *ival.start);
    else
      DEBUG("filters.interval", "no start");

    if (ival.finish)
      DEBUG("filters.interval", "finish = " << *ival.finish);
    else
      DEBUG("filters.interval", "no finish");
  }
#endif

  void operator()(post_t& post) override;
  void flush() override;

  void clear() override {
    interval = start_interval;
    all_posts.clear();

    subtotal_posts::clear();
    create_accounts();
  }
};

/**
 * @brief Converts postings into equity opening-balance format.
 *
 * Used by --equity.  Accumulates all postings like subtotal_posts, then on
 * flush() emits them as postings from each account balanced against
 * Equity:Opening Balances.  This produces output suitable for initializing
 * a new journal with pre-existing balances.
 */
class posts_as_equity : public subtotal_posts {
  report_t& report;           ///< Report context.
  post_t* last_post;          ///< Last posting seen (for date tracking).
  account_t* equity_account;  ///< The synthetic Equity account.
  account_t* balance_account; ///< The Equity:Opening Balances sub-account.
  bool unround;               ///< If true, unround amounts before output (--unround).

  posts_as_equity();

public:
  posts_as_equity(post_handler_ptr _handler, report_t& _report, expr_t& amount_expr, bool _unround)
      : subtotal_posts(std::move(_handler), amount_expr), report(_report), unround(_unround) {
    create_accounts();
    TRACE_CTOR(posts_as_equity, "post_handler_ptr, expr_t&");
  }
  ~posts_as_equity() noexcept override { TRACE_DTOR(posts_as_equity); }

  void create_accounts() {
    equity_account = &temps.create_account(_("Equity"));
    balance_account = equity_account->find_account(_("Opening Balances"));
  }

  void report_subtotal();

  void flush() override {
    report_subtotal();
    subtotal_posts::flush();
  }

  void clear() override {
    last_post = nullptr;
    subtotal_posts::clear();
    create_accounts();
  }
};

/**
 * @brief Subtotals postings grouped by payee.
 *
 * Used by --by-payee.  Each distinct payee gets its own subtotal_posts
 * instance.  On flush(), each payee's subtotal is reported separately.
 */
class by_payee_posts : public item_handler<post_t> {
  using payee_subtotals_map = std::map<string, std::shared_ptr<subtotal_posts>>;
  using payee_subtotals_pair = std::pair<string, std::shared_ptr<subtotal_posts>>;

  expr_t& amount_expr;                 ///< Amount expression shared with sub-totals.
  payee_subtotals_map payee_subtotals; ///< Per-payee subtotal_posts instances.

  by_payee_posts();

public:
  by_payee_posts(post_handler_ptr handler, expr_t& _amount_expr)
      : item_handler<post_t>(std::move(handler)), amount_expr(_amount_expr) {
    TRACE_CTOR(by_payee_posts, "post_handler_ptr, expr_t&");
  }
  ~by_payee_posts() override { TRACE_DTOR(by_payee_posts); }

  void flush() override;
  void operator()(post_t& post) override;

  void clear() override {
    amount_expr.mark_uncompiled();
    payee_subtotals.clear();

    item_handler<post_t>::clear();
  }
};

/**
 * @brief Rewrites a posting's date, account, or payee based on an expression.
 *
 * Used by --date, --account, --payee, and --pivot.  For each posting, the
 * expression is evaluated and the result replaces the specified element.
 * For SET_ACCOUNT, the expression result is prepended to the existing
 * account hierarchy.
 */
class transfer_details : public item_handler<post_t> {
  account_t* master;   ///< Journal master account for resolving account paths.
  expr_t expr;         ///< Expression to evaluate for the replacement value.
  scope_t& scope;      ///< Scope for expression evaluation.
  temporaries_t temps; ///< Temporary storage for rewritten xacts/posts.

  transfer_details();

public:
  /// Which posting element to rewrite.
  enum element_t : uint8_t {
    SET_DATE,    ///< Rewrite the posting date.
    SET_ACCOUNT, ///< Rewrite the account (prepending expression result).
    SET_PAYEE    ///< Rewrite the transaction payee.
  } which_element;

  transfer_details(post_handler_ptr handler, element_t _which_element, account_t* _master,
                   const expr_t& _expr, scope_t& _scope)
      : item_handler<post_t>(std::move(handler)), master(_master), expr(_expr), scope(_scope),
        which_element(_which_element) {
    TRACE_CTOR(transfer_details, "post_handler_ptr, element_t, account_t *, expr_t, scope_t&");
  }
  ~transfer_details() override {
    TRACE_DTOR(transfer_details);
    handler.reset();
  }

  void operator()(post_t& post) override;

  void clear() override {
    expr.mark_uncompiled();
    temps.clear();

    item_handler<post_t>::clear();
  }
};

/**
 * @brief Accumulates postings by day of week (0=Sunday through 6=Saturday).
 *
 * Used by --dow.  On flush(), subtotals each day's postings separately using
 * the "%As" format (abbreviated day name as the payee).
 */
class day_of_week_posts : public subtotal_posts {
  posts_list days_of_the_week[7]; ///< Postings bucketed by day of week (0=Sun, 6=Sat).

  day_of_week_posts();

public:
  day_of_week_posts(post_handler_ptr handler, expr_t& amount_expr)
      : subtotal_posts(std::move(handler), amount_expr) {
    TRACE_CTOR(day_of_week_posts, "post_handler_ptr, bool");
  }
  ~day_of_week_posts() noexcept override { TRACE_DTOR(day_of_week_posts); }

  void flush() override;
  void operator()(post_t& post) override {
    days_of_the_week[post.date().day_of_week()].push_back(&post);
  }

  void clear() override {
    for (int i = 0; i < 7; i++)
      days_of_the_week[i].clear();

    subtotal_posts::clear();
  }
};

/**
 * @brief Base class for generating synthetic postings from periodic transactions.
 *
 * Provides shared infrastructure for budget_posts and forecast_posts.  Periodic
 * transactions (~ directives in the journal) are decomposed into individual
 * pending postings, each paired with a date_interval_t that tracks which
 * period to generate next.
 */
class generate_posts : public item_handler<post_t> {
  generate_posts();

protected:
  using pending_posts_pair = std::pair<date_interval_t, post_t*>;
  using pending_posts_list = std::list<pending_posts_pair>;

  pending_posts_list
      pending_posts;   ///< Periodic postings awaiting generation, each with its interval state.
  temporaries_t temps; ///< Temporary storage for synthetic xacts/posts.

public:
  generate_posts(post_handler_ptr handler) : item_handler<post_t>(std::move(handler)) {
    TRACE_CTOR(generate_posts, "post_handler_ptr");
  }

  ~generate_posts() override {
    TRACE_DTOR(generate_posts);
    handler.reset();
  }

  void add_period_xacts(period_xacts_list& period_xacts);

  virtual void add_post(const date_interval_t& period, post_t& post);

  void clear() override {
    pending_posts.clear();
    temps.clear();

    item_handler<post_t>::clear();
  }
};

/**
 * @brief Generates budget postings that balance against reported actuals.
 *
 * Used by --budget, --add-budget, and --unbudgeted.  For each periodic
 * transaction in the journal, budget_posts generates negated postings at
 * each period boundary up to the current date.  Actual postings are then
 * matched against these budget accounts.
 *
 * The flags field controls which postings are forwarded:
 * - BUDGET_BUDGETED: forward postings that match a budget account.
 * - BUDGET_UNBUDGETED: forward postings that do not match any budget.
 * - BUDGET_WRAP_VALUES: wrap amounts in a sequence for compound display.
 */
class budget_posts : public generate_posts {
#define BUDGET_NO_BUDGET 0x00   ///< No budget mode active.
#define BUDGET_BUDGETED 0x01    ///< Show postings matching a budget account.
#define BUDGET_UNBUDGETED 0x02  ///< Show postings not matching any budget account.
#define BUDGET_WRAP_VALUES 0x04 ///< Wrap values as (amount, budget) sequences for compound display.

  uint_least8_t flags; ///< Combination of BUDGET_* flags controlling behavior.
  date_t terminus;     ///< End date for budget generation (typically report terminus).

  budget_posts();

public:
  budget_posts(post_handler_ptr handler, date_t _terminus, uint_least8_t _flags = BUDGET_BUDGETED)
      : generate_posts(std::move(handler)), flags(_flags), terminus(_terminus) {
    TRACE_CTOR(budget_posts, "post_handler_ptr, date_t, uint_least8_t");
  }
  ~budget_posts() noexcept override { TRACE_DTOR(budget_posts); }

  void report_budget_items(const date_t& date);

  void flush() override;
  void operator()(post_t& post) override;
};

/**
 * @brief Generates forecast postings into the future until a predicate fails.
 *
 * Used by --forecast-while.  Periodic transactions are projected forward in
 * time, generating synthetic postings.  Generation stops for each periodic
 * posting when it either fails the continuation predicate or exceeds the
 * forecast_years limit (default 5 years beyond the last valid posting).
 */
class forecast_posts : public generate_posts {
  predicate_t pred; ///< Continuation predicate; generation stops when this returns false.
  scope_t& context; ///< Scope for evaluating the predicate.
  const std::size_t forecast_years; ///< Maximum years to project beyond the last valid posting.

public:
  forecast_posts(post_handler_ptr handler, const predicate_t& predicate, scope_t& _context,
                 const std::size_t _forecast_years)
      : generate_posts(std::move(handler)), pred(predicate), context(_context),
        forecast_years(_forecast_years) {
    TRACE_CTOR(forecast_posts, "post_handler_ptr, predicate_t, scope_t&, std::size_t");
  }
  ~forecast_posts() noexcept override { TRACE_DTOR(forecast_posts); }

  void add_post(const date_interval_t& period, post_t& post) override;
  void flush() override;

  void clear() override {
    pred.mark_uncompiled();
    generate_posts::clear();
  }
};

/**
 * @brief Injects additional postings based on tag values found on transactions.
 *
 * Used by --inject.  The tag_list string (comma-separated) specifies which
 * tags to look for.  For each tag found on a posting or its transaction,
 * a synthetic posting is created with the tag's value as the amount, posted
 * to the account path derived from the tag name.  Each transaction is only
 * injected once per tag.
 */
class inject_posts : public item_handler<post_t> {
  using tag_injected_set = std::set<xact_t*>;
  using tag_mapping_pair = std::pair<account_t*, tag_injected_set>;
  using tags_list_pair = std::pair<string, tag_mapping_pair>;

  std::list<tags_list_pair>
      tags_list;       ///< Tag names paired with their target accounts and injection tracking.
  temporaries_t temps; ///< Temporary storage for synthetic xacts/posts.

public:
  inject_posts(post_handler_ptr handler, const string& tag_list, account_t* master);

  ~inject_posts() noexcept override {
    TRACE_DTOR(inject_posts);
    handler.reset();
  }

  void operator()(post_t& post) override;
};

/**
 * @brief Applies payee and account rewrite rules from the journal.
 *
 * When the journal contains `payee` or `account` rewrite mapping directives,
 * this handler checks each posting against those rules.  If a rule matches,
 * the posting is copied and the payee or account is replaced according to
 * the mapping.  Non-matching postings pass through unchanged.
 */
class rewrite_posts : public item_handler<post_t> {
  report_t& report;    ///< Report context (provides access to the journal's rewrite mappings).
  temporaries_t temps; ///< Temporary storage for rewritten xacts/posts.

  rewrite_posts();

public:
  rewrite_posts(post_handler_ptr handler, report_t& _report)
      : item_handler<post_t>(std::move(handler)), report(_report) {
    TRACE_CTOR(rewrite_posts, "post_handler_ptr, report_t&");
  }
  ~rewrite_posts() override {
    TRACE_DTOR(rewrite_posts);
    handler.reset();
  }

  void operator()(post_t& post) override;

  void clear() override {
    temps.clear();
    item_handler<post_t>::clear();
  }
};

//////////////////////////////////////////////////////////////////////
//
// Account filters
//

/**
 * @brief Iterates an external account iterator, passing each account through
 *        the handler chain with optional predicate filtering.
 *
 * Analogous to pass_down_posts but for the account pipeline.  All work
 * happens in the constructor: accounts are iterated, optionally filtered
 * by a predicate, forwarded to the handler chain, and then flushed.
 */
template <typename Iterator>
class pass_down_accounts : public item_handler<account_t> {
  pass_down_accounts();

  optional<predicate_t> pred;
  optional<scope_t&> context;

public:
  pass_down_accounts(acct_handler_ptr handler, Iterator& iter,
                     const optional<predicate_t>& _pred = none,
                     const optional<scope_t&>& _context = none)
      : item_handler<account_t>(handler), pred(_pred), context(_context) {
    TRACE_CTOR(pass_down_accounts, "acct_handler_ptr, accounts_iterator, ...");

    while (account_t* account = *iter++) {
      if (!pred) {
        item_handler<account_t>::operator()(*account);
      } else {
        bind_scope_t bound_scope(*context, *account);
        if ((*pred)(bound_scope))
          item_handler<account_t>::operator()(*account);
      }
    }

    item_handler<account_t>::flush();
  }

  ~pass_down_accounts() override { TRACE_DTOR(pass_down_accounts); }

  void clear() override {
    if (pred)
      pred->mark_uncompiled();

    item_handler<account_t>::clear();
  }
};

} // namespace ledger
