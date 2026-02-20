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

namespace ledger {

using namespace boost::placeholders;

//////////////////////////////////////////////////////////////////////
//
// Posting collector
//

class post_splitter : public item_handler<post_t> {
public:
  typedef std::map<value_t, posts_list> value_to_posts_map;
  typedef function<void(const value_t&)> custom_flusher_t;

protected:
  value_to_posts_map posts_map;
  post_handler_ptr post_chain;
  report_t& report;
  expr_t& group_by_expr;
  custom_flusher_t preflush_func;
  optional<custom_flusher_t> postflush_func;

public:
  post_splitter(post_handler_ptr _post_chain, report_t& _report, expr_t& _group_by_expr)
      : post_chain(_post_chain), report(_report), group_by_expr(_group_by_expr) {
    preflush_func = bind(&post_splitter::print_title, this, _1);
    TRACE_CTOR(post_splitter, "scope_t&, post_handler_ptr, expr_t");
  }
  virtual ~post_splitter() { TRACE_DTOR(post_splitter); }

  void set_preflush_func(custom_flusher_t functor) { preflush_func = functor; }
  void set_postflush_func(custom_flusher_t functor) { postflush_func = functor; }

  virtual void print_title(const value_t& val);

  virtual void flush() override;
  virtual void operator()(post_t& post) override;

  virtual void clear() override {
    posts_map.clear();
    post_chain->clear();
    item_handler<post_t>::clear();
  }
};

//////////////////////////////////////////////////////////////////////
//
// Posting filters
//

class ignore_posts : public item_handler<post_t> {
public:
  virtual void operator()(post_t&) override {}
};

class collect_posts : public item_handler<post_t> {
public:
  std::vector<post_t*> posts;

  collect_posts() : item_handler<post_t>() { TRACE_CTOR(collect_posts, ""); }
  virtual ~collect_posts() { TRACE_DTOR(collect_posts); }

  std::size_t length() const { return posts.size(); }

  std::vector<post_t*>::iterator begin() { return posts.begin(); }
  std::vector<post_t*>::iterator end() { return posts.end(); }

  virtual void flush() override {}
  virtual void operator()(post_t& post) override { posts.push_back(&post); }

  virtual void clear() override {
    posts.clear();
    item_handler<post_t>::clear();
  }
};

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

  virtual ~pass_down_posts() { TRACE_DTOR(pass_down_posts); }
};

class push_to_posts_list : public item_handler<post_t> {
  push_to_posts_list();

public:
  posts_list& posts;

  push_to_posts_list(posts_list& _posts) : posts(_posts) {
    TRACE_CTOR(push_to_posts_list, "posts_list&");
  }
  virtual ~push_to_posts_list() { TRACE_DTOR(push_to_posts_list); }

  virtual void operator()(post_t& post) override { posts.push_back(&post); }
};

class truncate_xacts : public item_handler<post_t> {
  int head_count;
  int tail_count;
  bool completed;

  posts_list posts;
  std::size_t xacts_seen;
  xact_t* last_xact;

  truncate_xacts();

public:
  truncate_xacts(post_handler_ptr handler, int _head_count, int _tail_count)
      : item_handler<post_t>(handler), head_count(_head_count), tail_count(_tail_count),
        completed(false), xacts_seen(0), last_xact(NULL) {
    TRACE_CTOR(truncate_xacts, "post_handler_ptr, int, int");
  }
  virtual ~truncate_xacts() { TRACE_DTOR(truncate_xacts); }

  virtual void flush() override;
  virtual void operator()(post_t& post) override;

  virtual void clear() override {
    completed = false;
    posts.clear();
    xacts_seen = 0;
    last_xact = NULL;

    item_handler<post_t>::clear();
  }
};

class sort_posts : public item_handler<post_t> {
  typedef std::deque<post_t*> posts_deque;

  posts_deque posts;
  expr_t sort_order;
  report_t& report;

  sort_posts();

public:
  sort_posts(post_handler_ptr handler, const expr_t& _sort_order, report_t& _report)
      : item_handler<post_t>(handler), sort_order(_sort_order), report(_report) {
    TRACE_CTOR(sort_posts, "post_handler_ptr, const value_expr&, report_t&");
  }
  sort_posts(post_handler_ptr handler, const string& _sort_order, report_t& _report)
      : item_handler<post_t>(handler), sort_order(_sort_order), report(_report) {
    TRACE_CTOR(sort_posts, "post_handler_ptr, const string&, report_t&");
  }
  virtual ~sort_posts() { TRACE_DTOR(sort_posts); }

  virtual void post_accumulated_posts();

  virtual void flush() override {
    post_accumulated_posts();
    item_handler<post_t>::flush();
  }

  virtual void operator()(post_t& post) override { posts.push_back(&post); }

  virtual void clear() override {
    posts.clear();
    sort_order.mark_uncompiled();

    item_handler<post_t>::clear();
  }
};

class sort_xacts : public item_handler<post_t> {
  sort_posts sorter;
  xact_t* last_xact;

  sort_xacts();

public:
  sort_xacts(post_handler_ptr handler, const expr_t& _sort_order, report_t& _report)
      : sorter(handler, _sort_order, _report) {
    TRACE_CTOR(sort_xacts, "post_handler_ptr, const value_expr&, report_t&");
  }
  sort_xacts(post_handler_ptr handler, const string& _sort_order, report_t& _report)
      : sorter(handler, _sort_order, _report) {
    TRACE_CTOR(sort_xacts, "post_handler_ptr, const string&, report_t&");
  }
  virtual ~sort_xacts() { TRACE_DTOR(sort_xacts); }

  virtual void flush() override {
    sorter.flush();
    item_handler<post_t>::flush();
  }

  virtual void operator()(post_t& post) override {
    if (last_xact && post.xact != last_xact)
      sorter.post_accumulated_posts();

    sorter(post);

    last_xact = post.xact;
  }

  virtual void clear() override {
    sorter.clear();
    last_xact = NULL;

    item_handler<post_t>::clear();
  }
};

class filter_posts : public item_handler<post_t> {
  predicate_t pred;
  scope_t& context;

  filter_posts();

public:
  filter_posts(post_handler_ptr handler, const predicate_t& predicate, scope_t& _context)
      : item_handler<post_t>(handler), pred(predicate), context(_context) {
    TRACE_CTOR(filter_posts, "post_handler_ptr, predicate_t, scope_t&");
  }
  virtual ~filter_posts() { TRACE_DTOR(filter_posts); }

  virtual void operator()(post_t& post) override {
    bind_scope_t bound_scope(context, post);
    if (pred(bound_scope)) {
      post.xdata().add_flags(POST_EXT_MATCHES);
      (*handler)(post);
    }
  }

  virtual void clear() override {
    pred.mark_uncompiled();
    item_handler<post_t>::clear();
  }
};

class anonymize_posts : public item_handler<post_t> {
  typedef std::map<commodity_t*, std::size_t> commodity_index_map;
  typedef variate_generator<mt19937&, uniform_int<>> int_generator_t;

  temporaries_t temps;
  commodity_index_map comms;
  std::size_t next_comm_id;
  xact_t* last_xact;
  mt19937 rnd_gen;
  uniform_int<> integer_range;
  int_generator_t integer_gen;

  anonymize_posts();

public:
  anonymize_posts(post_handler_ptr handler)
      : item_handler<post_t>(handler), next_comm_id(0), last_xact(NULL),
        rnd_gen(static_cast<unsigned int>(static_cast<boost::uintmax_t>(std::time(0)))),
        integer_range(1, 2000000000L), integer_gen(rnd_gen, integer_range) {
    TRACE_CTOR(anonymize_posts, "post_handler_ptr");
  }
  virtual ~anonymize_posts() {
    TRACE_DTOR(anonymize_posts);
    handler.reset();
  }

  void render_commodity(amount_t& amt);

  virtual void operator()(post_t& post) override;

  virtual void clear() override {
    temps.clear();
    comms.clear();
    last_xact = NULL;

    item_handler<post_t>::clear();
  }
};

class calc_posts : public item_handler<post_t> {
  post_t* last_post;
  expr_t& amount_expr;
  bool calc_running_total;
  bool maintain_stripped_total;
  keep_details_t what_to_keep;

  calc_posts();

public:
  calc_posts(post_handler_ptr handler, expr_t& _amount_expr, bool _calc_running_total = false,
             bool _maintain_stripped = false, const keep_details_t& _what_to_keep = keep_details_t())
      : item_handler<post_t>(handler), last_post(NULL), amount_expr(_amount_expr),
        calc_running_total(_calc_running_total),
        maintain_stripped_total(_maintain_stripped), what_to_keep(_what_to_keep) {
    TRACE_CTOR(calc_posts, "post_handler_ptr, expr_t&, bool");
  }
  virtual ~calc_posts() { TRACE_DTOR(calc_posts); }

  virtual void operator()(post_t& post) override;

  virtual void clear() override {
    last_post = NULL;
    amount_expr.mark_uncompiled();

    item_handler<post_t>::clear();
  }
};

class collapse_posts : public item_handler<post_t> {

  typedef std::map<account_t*, value_t> totals_map;

  expr_t& amount_expr;
  predicate_t display_predicate;
  predicate_t only_predicate;
  value_t subtotal;
  std::size_t count;
  xact_t* last_xact;
  post_t* last_post;
  temporaries_t temps;
  account_t* global_totals_account;
  totals_map totals;
  bool only_collapse_if_zero;
  unsigned short collapse_depth;
  std::list<post_t*> component_posts;
  report_t& report;

  collapse_posts();

public:
  collapse_posts(post_handler_ptr handler, report_t& _report, expr_t& _amount_expr,
                 predicate_t _display_predicate, predicate_t _only_predicate,
                 bool _only_collapse_if_zero = false, unsigned short _collapse_depth = 0)
      : item_handler<post_t>(handler), amount_expr(_amount_expr),
        display_predicate(_display_predicate), only_predicate(_only_predicate), count(0),
        last_xact(NULL), last_post(NULL), only_collapse_if_zero(_only_collapse_if_zero),
        collapse_depth(_collapse_depth), report(_report) {
    create_accounts();
    TRACE_CTOR(collapse_posts, "post_handler_ptr, ...");
  }
  virtual ~collapse_posts() {
    TRACE_DTOR(collapse_posts);
    handler.reset();
  }

  void create_accounts() { global_totals_account = &temps.create_account(_("<Total>")); }

  value_t& find_totals(account_t* account);

  virtual void flush() override {
    report_subtotal();
    item_handler<post_t>::flush();
  }

  void report_subtotal();

  virtual void operator()(post_t& post) override;

  virtual void clear() override {
    amount_expr.mark_uncompiled();
    display_predicate.mark_uncompiled();
    only_predicate.mark_uncompiled();

    subtotal = value_t();
    count = 0;
    last_xact = NULL;
    last_post = NULL;

    temps.clear();
    create_accounts();
    totals.clear();
    component_posts.clear();

    item_handler<post_t>::clear();
  }
};

class related_posts : public item_handler<post_t> {
  posts_list posts;
  bool also_matching;

  related_posts();

public:
  related_posts(post_handler_ptr handler, const bool _also_matching = false)
      : item_handler<post_t>(handler), also_matching(_also_matching) {
    TRACE_CTOR(related_posts, "post_handler_ptr, const bool");
  }
  virtual ~related_posts() noexcept { TRACE_DTOR(related_posts); }

  virtual void flush() override;
  virtual void operator()(post_t& post) override {
    post.xdata().add_flags(POST_EXT_RECEIVED);
    posts.push_back(&post);
  }

  virtual void clear() override {
    posts.clear();
    item_handler<post_t>::clear();
  }
};

class display_filter_posts : public item_handler<post_t> {
  // This filter requires that calc_posts be used at some point
  // later in the chain.

  report_t& report;
  expr_t& display_amount_expr;
  expr_t& display_total_expr;
  bool show_rounding;
  value_t last_display_total;
  value_t last_stripped_display_total;
  bool has_stripped_cache;
  bool incremental_strip_eligible;
  keep_details_t what_to_keep;
  temporaries_t temps;
  account_t* rounding_account;

  display_filter_posts();

public:
  account_t* revalued_account;

  display_filter_posts(post_handler_ptr handler, report_t& _report, bool _show_rounding);

  virtual ~display_filter_posts() {
    TRACE_DTOR(display_filter_posts);
    handler.reset();
  }

  void create_accounts() {
    rounding_account = &temps.create_account(_("<Adjustment>"));
    revalued_account = &temps.create_account(_("<Revalued>"));
  }

  bool output_rounding(post_t& post);

  virtual void operator()(post_t& post) override;

  virtual void clear() override {
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

class changed_value_posts : public item_handler<post_t> {
  // This filter requires that calc_posts be used at some point
  // later in the chain.

  report_t& report;
  expr_t& total_expr;
  expr_t& display_total_expr;
  bool changed_values_only;
  bool historical_prices_only;
  bool for_accounts_report;
  bool show_unrealized;
  post_t* last_post;
  value_t last_total;
  value_t repriced_total;
  temporaries_t temps;
  account_t* revalued_account;
  account_t* gains_equity_account;
  account_t* losses_equity_account;

  display_filter_posts* display_filter;

  changed_value_posts();

public:
  changed_value_posts(post_handler_ptr handler, report_t& _report, bool _for_accounts_report,
                      bool _show_unrealized, display_filter_posts* _display_filter);

  virtual ~changed_value_posts() {
    TRACE_DTOR(changed_value_posts);
    temps.clear();
    handler.reset();
  }

  void create_accounts() {
    revalued_account = (display_filter ? display_filter->revalued_account
                                       : &temps.create_account(_("<Revalued>")));
  }

  virtual void flush() override;

  void output_revaluation(post_t& post, const date_t& current);
  void output_intermediate_prices(post_t& post, const date_t& current);

  virtual void operator()(post_t& post) override;

  virtual void clear() override {
    total_expr.mark_uncompiled();
    display_total_expr.mark_uncompiled();

    last_post = NULL;
    last_total = value_t();

    temps.clear();
    item_handler<post_t>::clear();

    create_accounts();
  }
};

class subtotal_posts : public item_handler<post_t> {
  subtotal_posts();

protected:
  class acct_value_t {
    acct_value_t();

  public:
    account_t* account;
    value_t value;
    bool is_virtual;
    bool must_balance;

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
    ~acct_value_t() noexcept { TRACE_DTOR(acct_value_t); }
  };

  typedef std::map<string, acct_value_t> values_map;
  typedef std::pair<string, acct_value_t> values_pair;

protected:
  expr_t& amount_expr;
  values_map values;
  optional<string> date_format;
  temporaries_t temps;
  std::deque<post_t*> component_posts;

public:
  subtotal_posts(post_handler_ptr handler, expr_t& _amount_expr,
                 const optional<string>& _date_format = none)
      : item_handler<post_t>(handler), amount_expr(_amount_expr), date_format(_date_format) {
    TRACE_CTOR(subtotal_posts, "post_handler_ptr, expr_t&, const optional<string>&");
  }
  virtual ~subtotal_posts() {
    TRACE_DTOR(subtotal_posts);
    handler.reset();
  }

  void report_subtotal(const char* spec_fmt = NULL,
                       const optional<date_interval_t>& interval = none);

  virtual void flush() override {
    if (values.size() > 0)
      report_subtotal();
    item_handler<post_t>::flush();
  }
  virtual void operator()(post_t& post) override;

  virtual void clear() override {
    amount_expr.mark_uncompiled();
    values.clear();
    temps.clear();
    component_posts.clear();

    item_handler<post_t>::clear();
  }
};

class interval_posts : public subtotal_posts {
  date_interval_t start_interval;
  date_interval_t interval;
  account_t* empty_account;
  bool exact_periods;
  bool generate_empty_posts;
  bool align_intervals;

  std::deque<post_t*> all_posts;

  interval_posts();

public:
  interval_posts(post_handler_ptr _handler, expr_t& amount_expr, const date_interval_t& _interval,
                 bool _exact_periods = false, bool _generate_empty_posts = false,
                 bool _align_intervals = false)
      : subtotal_posts(_handler, amount_expr), start_interval(_interval), interval(start_interval),
        exact_periods(_exact_periods), generate_empty_posts(_generate_empty_posts),
        align_intervals(_align_intervals) {
    create_accounts();
    TRACE_CTOR(interval_posts, "post_handler_ptr, expr_t&, date_interval_t, bool, bool");
  }
  virtual ~interval_posts() noexcept { TRACE_DTOR(interval_posts); }

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

  virtual void operator()(post_t& post) override;
  virtual void flush() override;

  virtual void clear() override {
    interval = start_interval;
    all_posts.clear();

    subtotal_posts::clear();
    create_accounts();
  }
};

class posts_as_equity : public subtotal_posts {
  report_t& report;
  post_t* last_post;
  account_t* equity_account;
  account_t* balance_account;
  bool unround;

  posts_as_equity();

public:
  posts_as_equity(post_handler_ptr _handler, report_t& _report, expr_t& amount_expr, bool _unround)
      : subtotal_posts(_handler, amount_expr), report(_report), unround(_unround) {
    create_accounts();
    TRACE_CTOR(posts_as_equity, "post_handler_ptr, expr_t&");
  }
  virtual ~posts_as_equity() noexcept { TRACE_DTOR(posts_as_equity); }

  void create_accounts() {
    equity_account = &temps.create_account(_("Equity"));
    balance_account = equity_account->find_account(_("Opening Balances"));
  }

  void report_subtotal();

  virtual void flush() override {
    report_subtotal();
    subtotal_posts::flush();
  }

  virtual void clear() override {
    last_post = NULL;
    subtotal_posts::clear();
    create_accounts();
  }
};

class by_payee_posts : public item_handler<post_t> {
  typedef std::map<string, std::shared_ptr<subtotal_posts>> payee_subtotals_map;
  typedef std::pair<string, std::shared_ptr<subtotal_posts>> payee_subtotals_pair;

  expr_t& amount_expr;
  payee_subtotals_map payee_subtotals;

  by_payee_posts();

public:
  by_payee_posts(post_handler_ptr handler, expr_t& _amount_expr)
      : item_handler<post_t>(handler), amount_expr(_amount_expr) {
    TRACE_CTOR(by_payee_posts, "post_handler_ptr, expr_t&");
  }
  virtual ~by_payee_posts() { TRACE_DTOR(by_payee_posts); }

  virtual void flush() override;
  virtual void operator()(post_t& post) override;

  virtual void clear() override {
    amount_expr.mark_uncompiled();
    payee_subtotals.clear();

    item_handler<post_t>::clear();
  }
};

class transfer_details : public item_handler<post_t> {
  account_t* master;
  expr_t expr;
  scope_t& scope;
  temporaries_t temps;

  transfer_details();

public:
  enum element_t { SET_DATE, SET_ACCOUNT, SET_PAYEE } which_element;

  transfer_details(post_handler_ptr handler, element_t _which_element, account_t* _master,
                   const expr_t& _expr, scope_t& _scope)
      : item_handler<post_t>(handler), master(_master), expr(_expr), scope(_scope),
        which_element(_which_element) {
    TRACE_CTOR(transfer_details, "post_handler_ptr, element_t, account_t *, expr_t, scope_t&");
  }
  virtual ~transfer_details() {
    TRACE_DTOR(transfer_details);
    handler.reset();
  }

  virtual void operator()(post_t& post) override;

  virtual void clear() override {
    expr.mark_uncompiled();
    temps.clear();

    item_handler<post_t>::clear();
  }
};

class day_of_week_posts : public subtotal_posts {
  posts_list days_of_the_week[7];

  day_of_week_posts();

public:
  day_of_week_posts(post_handler_ptr handler, expr_t& amount_expr)
      : subtotal_posts(handler, amount_expr) {
    TRACE_CTOR(day_of_week_posts, "post_handler_ptr, bool");
  }
  virtual ~day_of_week_posts() noexcept { TRACE_DTOR(day_of_week_posts); }

  virtual void flush() override;
  virtual void operator()(post_t& post) override {
    days_of_the_week[post.date().day_of_week()].push_back(&post);
  }

  virtual void clear() override {
    for (int i = 0; i < 7; i++)
      days_of_the_week[i].clear();

    subtotal_posts::clear();
  }
};

class generate_posts : public item_handler<post_t> {
  generate_posts();

protected:
  typedef std::pair<date_interval_t, post_t*> pending_posts_pair;
  typedef std::list<pending_posts_pair> pending_posts_list;

  pending_posts_list pending_posts;
  temporaries_t temps;

public:
  generate_posts(post_handler_ptr handler) : item_handler<post_t>(handler) {
    TRACE_CTOR(generate_posts, "post_handler_ptr");
  }

  virtual ~generate_posts() {
    TRACE_DTOR(generate_posts);
    handler.reset();
  }

  void add_period_xacts(period_xacts_list& period_xacts);

  virtual void add_post(const date_interval_t& period, post_t& post);

  virtual void clear() override {
    pending_posts.clear();
    temps.clear();

    item_handler<post_t>::clear();
  }
};

class budget_posts : public generate_posts {
#define BUDGET_NO_BUDGET 0x00
#define BUDGET_BUDGETED 0x01
#define BUDGET_UNBUDGETED 0x02
#define BUDGET_WRAP_VALUES 0x04

  uint_least8_t flags;
  date_t terminus;

  budget_posts();

public:
  budget_posts(post_handler_ptr handler, date_t _terminus, uint_least8_t _flags = BUDGET_BUDGETED)
      : generate_posts(handler), flags(_flags), terminus(_terminus) {
    TRACE_CTOR(budget_posts, "post_handler_ptr, date_t, uint_least8_t");
  }
  virtual ~budget_posts() noexcept { TRACE_DTOR(budget_posts); }

  void report_budget_items(const date_t& date);

  virtual void flush() override;
  virtual void operator()(post_t& post) override;
};

class forecast_posts : public generate_posts {
  predicate_t pred;
  scope_t& context;
  const std::size_t forecast_years;

public:
  forecast_posts(post_handler_ptr handler, const predicate_t& predicate, scope_t& _context,
                 const std::size_t _forecast_years)
      : generate_posts(handler), pred(predicate), context(_context),
        forecast_years(_forecast_years) {
    TRACE_CTOR(forecast_posts, "post_handler_ptr, predicate_t, scope_t&, std::size_t");
  }
  virtual ~forecast_posts() noexcept { TRACE_DTOR(forecast_posts); }

  virtual void add_post(const date_interval_t& period, post_t& post) override;
  virtual void flush() override;

  virtual void clear() override {
    pred.mark_uncompiled();
    generate_posts::clear();
  }
};

class inject_posts : public item_handler<post_t> {
  typedef std::set<xact_t*> tag_injected_set;
  typedef std::pair<account_t*, tag_injected_set> tag_mapping_pair;
  typedef std::pair<string, tag_mapping_pair> tags_list_pair;

  std::list<tags_list_pair> tags_list;
  temporaries_t temps;

public:
  inject_posts(post_handler_ptr handler, const string& tag_list, account_t* master);

  virtual ~inject_posts() noexcept {
    TRACE_DTOR(inject_posts);
    handler.reset();
  }

  virtual void operator()(post_t& post) override;
};

//////////////////////////////////////////////////////////////////////
//
// Account filters
//

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

  virtual ~pass_down_accounts() { TRACE_DTOR(pass_down_accounts); }

  virtual void clear() override {
    if (pred)
      pred->mark_uncompiled();

    item_handler<account_t>::clear();
  }
};

} // namespace ledger
