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
 * @file   chain.cc
 * @author John Wiegley
 *
 * @ingroup report
 *
 * @brief Pipeline construction -- translating command-line options into filter chains.
 *
 * This file implements the two builder functions that assemble the posting
 * filter pipeline at report time:
 *
 * - **chain_pre_post_handlers()** adds the outermost filters: anonymization,
 *   the --limit predicate, and budget/forecast generation.
 *
 * - **chain_post_handlers()** adds the inner filters: injection, related
 *   posts, transfer_details, interval grouping, aggregation (subtotal,
 *   by-payee, day-of-week, equity), collapse, sorting, the --only predicate,
 *   calc_posts, revaluation, display filtering, truncation, and the
 *   forecast-while predicate.
 *
 * Both functions build the chain from the inside out: each new handler wraps
 * the previous one, so the last handler added is the first to receive
 * postings.  The inline chain_handlers() in chain.h calls
 * chain_post_handlers first, then chain_pre_post_handlers, producing the
 * complete pipeline described in the handler ordering diagram in filters.h.
 */

#include <system.hh>
#include <utility>

#include "chain.h"
#include "predicate.h"
#include "filters.h"
#include "report.h"
#include "session.h"

namespace ledger {

/*--- Pre-post handlers (outermost filters) ---*/

/**
 * @brief Build the outer (upstream) portion of the posting filter chain.
 *
 * Handlers are added in inside-out order.  The resulting chain, from
 * outermost to innermost, is:
 *
 * 1. **filter_posts(limit)** -- re-applied after budget/forecast so only
 *    matching postings contribute to budget calculations.
 * 2. **budget_posts** or **forecast_posts** -- generate synthetic periodic
 *    postings (mutually exclusive; budget takes precedence).
 * 3. **filter_posts(limit)** -- primary --limit predicate.
 * 4. **anonymize_posts** -- strip identifying data (--anon).
 * 5. [base_handler from chain_post_handlers]
 */
post_handler_ptr chain_pre_post_handlers(post_handler_ptr base_handler, report_t& report) {
  post_handler_ptr handler(std::move(base_handler));

  // anonymize_posts removes all meaningful information from xact payee's and
  // account names, for the sake of creating useful bug reports.
  if (report.HANDLED(anon))
    handler = std::make_shared<anonymize_posts>(handler);

  // This filter_posts will only pass through posts matching the `predicate'.
  if (report.HANDLED(limit_)) {
    DEBUG("report.predicate", "Report predicate expression = " << report.HANDLER(limit_).str());
    handler = std::make_shared<filter_posts>(
        handler, predicate_t(report.HANDLER(limit_).str(), report.what_to_keep()), report);
  }

  // budget_posts takes a set of posts from a data file and uses them to
  // generate "budget posts" which balance against the reported posts.
  //
  // forecast_posts is a lot like budget_posts, except that it adds xacts
  // only for the future, and does not balance them against anything but the
  // future balance.

  if (report.budget_flags != BUDGET_NO_BUDGET) {
    auto budget_handler =
        std::make_shared<budget_posts>(handler, report.terminus.date(), report.budget_flags);
    budget_handler->add_period_xacts(report.session.journal->period_xacts);
    handler = budget_handler;

    // budget_posts assumes postings arrive in chronological order because
    // its date_interval_t can only advance forward.  When the journal has
    // transactions in reverse (or arbitrary) order, budget entries are
    // silently skipped for earlier periods.  Pre-sorting by date ensures
    // every covered period gets its budget entry.  (#590)
    handler = std::make_shared<sort_posts>(handler, "date", report);

    // Apply this before the budget handler, so that only matching posts are
    // calculated toward the budget.  The use of filter_posts above will
    // further clean the results so that no automated posts that don't match
    // the filter get reported.
    if (report.HANDLED(limit_))
      handler = std::make_shared<filter_posts>(
          handler, predicate_t(report.HANDLER(limit_).str(), report.what_to_keep()), report);
  } else if (report.HANDLED(forecast_while_)) {
    auto forecast_handler = std::make_shared<forecast_posts>(
        handler, predicate_t(report.HANDLER(forecast_while_).str(), report.what_to_keep()), report,
        (report.HANDLED(forecast_years_)
             ? lexical_cast<std::size_t>(report.HANDLER(forecast_years_).value)
             : 5UL));
    forecast_handler->add_period_xacts(report.session.journal->period_xacts);
    handler = forecast_handler;

    // See above, under budget_posts.
    if (report.HANDLED(limit_))
      handler = std::make_shared<filter_posts>(
          handler, predicate_t(report.HANDLER(limit_).str(), report.what_to_keep()), report);
  }

  return handler;
}

/*--- Post handlers (inner/downstream filters) ---*/

/**
 * @brief Build the inner (downstream) portion of the posting filter chain.
 *
 * This function adds handlers from the output handler inward.  Because each
 * new handler wraps the previous, the code reads bottom-up relative to the
 * data flow.  The construction order (and therefore data flow) is documented
 * in the handler ordering diagram in filters.h.
 *
 * Key design notes:
 * - The amount expression context is set here so that all downstream filters
 *   share the same evaluation environment.
 * - display_predicate and only_predicate are captured as local variables and
 *   passed to collapse_posts so it can determine whether collapsed postings
 *   would actually be displayed.
 * - The display_filter raw pointer is threaded through to changed_value_posts
 *   so that revaluation postings share the same <Revalued> account.
 */
post_handler_ptr chain_post_handlers(post_handler_ptr base_handler, report_t& report,
                                     bool for_accounts_report) {
  post_handler_ptr handler(std::move(base_handler));
  predicate_t display_predicate;
  predicate_t only_predicate;
  display_filter_posts* display_filter = nullptr;

  /* Set the evaluation context for amount and display expressions so that
     all filters in the chain share the same scope. */
  expr_t& expr(report.HANDLER(amount_).expr);
  expr.set_context(&report);

  report.HANDLER(total_).expr.set_context(&report);
  report.HANDLER(display_amount_).expr.set_context(&report);
  report.HANDLER(display_total_).expr.set_context(&report);

  /* Forecast-while filter: must be innermost so it can stop forecast
     postings from reaching the output handler.  This applies to both
     posting reports (register) and accounts reports (balance) so that
     --forecast-while correctly bounds forecast entries for all report
     types.  (#1605) */
  if (report.HANDLED(forecast_while_)) {
    handler = std::make_shared<filter_posts>(
        handler, predicate_t(report.HANDLER(forecast_while_).str(), report.what_to_keep()), report);
  }

  if (!for_accounts_report) {
    // truncate_xacts cuts off a certain number of _xacts_ from being
    // displayed.  It does not affect calculation.
    if (report.HANDLED(head_) || report.HANDLED(tail_))
      handler = std::make_shared<truncate_xacts>(
          handler, report.HANDLED(head_) ? lexical_cast<int>(report.HANDLER(head_).value) : 0,
          report.HANDLED(tail_) ? lexical_cast<int>(report.HANDLER(tail_).value) : 0);

    // filter_posts will only pass through posts matching the
    // `display_predicate'.
    if (report.HANDLED(display_)) {
      display_predicate = predicate_t(report.HANDLER(display_).str(), report.what_to_keep());
      handler = std::make_shared<filter_posts>(handler, display_predicate, report);
    }

    // display_filter_posts adds virtual posts to the list to account
    // for changes in value of commodities, which otherwise would affect
    // the running total unpredictably.  Placed before filter_posts so
    // it sees all postings and can correctly detect rounding gaps.
    //
    // Rounding adjustments are only useful for commands that display
    // running totals (register), not for print/csv/equity.  The --base
    // option (auto-set for non-balance/register commands) gates this.
    // Additionally, rounding is safe only when the display expressions
    // use simple additive accumulation (the defaults).  Non-additive
    // transforms like --average, --deviation, or user-supplied
    // --display-total/--display-amount break the assumption that
    // total == prev_total + amount, so we disable rounding for those.
    bool default_exprs = report.HANDLER(display_total_).expr.exprs.empty() &&
                         report.HANDLER(display_total_).expr.base_expr == "total_expr" &&
                         report.HANDLER(total_).expr.exprs.empty() &&
                         report.HANDLER(total_).expr.base_expr == "total" &&
                         report.HANDLER(display_amount_).expr.exprs.empty() &&
                         report.HANDLER(display_amount_).expr.base_expr == "amount_expr" &&
                         report.HANDLER(amount_).expr.exprs.empty() &&
                         report.HANDLER(amount_).expr.base_expr == "amount";

    auto display_filter_sp = std::make_shared<display_filter_posts>(
        handler, report,
        !report.HANDLED(no_rounding) &&
            (report.HANDLED(revalued) || (!report.HANDLED(base) && default_exprs)));
    display_filter = display_filter_sp.get();
    handler = std::move(display_filter_sp);
  }

  /*--- Revaluation and calculation ---*/

  // changed_value_posts adds virtual posts to the list to account for changes
  // in market value of commodities, which otherwise would affect the running
  // total unpredictably.
  if (report.HANDLED(revalued) && (!for_accounts_report || report.HANDLED(unrealized)))
    handler = std::make_shared<changed_value_posts>(handler, report, for_accounts_report,
                                                    report.HANDLED(unrealized), display_filter);

  // calc_posts computes the running total.  When this appears will determine,
  // for example, whether filtered posts are included or excluded from the
  // running total.
  {
    bool calc_running =
        (!for_accounts_report || (report.HANDLED(revalued) && report.HANDLED(unrealized)));

    // Enable incremental stripped total maintenance when the display_total
    // and total expressions are unmodified (default "total" accumulation)
    // and annotations need stripping.  This allows the format's
    // scrub(display_total) to use the pre-stripped value from xdata,
    // avoiding expensive O(K) GMP arithmetic per posting.
    keep_details_t wtk = report.what_to_keep();
    bool maintain_stripped = calc_running && report.HANDLER(display_total_).expr.exprs.empty() &&
                             report.HANDLER(display_total_).expr.base_expr == "total_expr" &&
                             report.HANDLER(total_).expr.exprs.empty() &&
                             report.HANDLER(total_).expr.base_expr == "total" && !wtk.keep_all();

    // Per-account period averaging (period_average_) tracks independent
    // running totals for each account across interval periods, so that
    // multi-account period groups each show their own average.  Disable it
    // when --collapse or --depth will be in the chain: collapse reduces
    // multi-account periods to a single post, making per-account seeding
    // unnecessary and harmful (the changing account identity causes the
    // accumulator to miss, resetting the running total).
    bool will_collapse = report.HANDLED(collapse) || report.HANDLED(depth_);
    bool period_average = report.HANDLED(average) && !will_collapse;
    handler = std::make_shared<calc_posts>(handler, expr, calc_running, maintain_stripped, wtk,
                                           period_average);
  }

  /*--- Only predicate ---*/

  // filter_posts will only pass through posts matching the
  // `secondary_predicate'.
  if (report.HANDLED(only_)) {
    only_predicate = predicate_t(report.HANDLER(only_).str(), report.what_to_keep());
    handler = std::make_shared<filter_posts>(handler, only_predicate, report);
  }

  /*--- Sorting, collapsing, and aggregation ---*/

  if (!for_accounts_report) {
    // sort_posts will sort all the posts it sees, based on the `sort_order'
    // value expression.  sort_xacts sorts posts within each transaction.
    if (report.HANDLED(sort_)) {
      bool xacts_only = false;

      if (report.HANDLED(sort_xacts_)) {
        // sort_xacts_ may have its own expression (from --sort-xacts), or
        // it may have been activated without a value (from period sorting
        // via normalize_period).  Check value directly since str() throws
        // on empty values.
        const string& sort_xacts_value = report.HANDLER(sort_xacts_).value;
        if (sort_xacts_value.empty()) {
          // Activated from period sorting: use sort_'s expression for
          // within-transaction sorting only, no global sort.
          handler =
              std::make_shared<sort_xacts>(handler, expr_t(report.HANDLER(sort_).str()), report);
          xacts_only = true;
        } else {
          // --sort-xacts given explicitly with its own expression: apply
          // within-transaction sort first, then global sort (from --sort).
          handler = std::make_shared<sort_xacts>(handler, expr_t(sort_xacts_value), report);
        }
      }

      if (!xacts_only)
        handler = std::make_shared<sort_posts>(handler, report.HANDLER(sort_).str(), report);
    } else if (report.HANDLED(sort_xacts_)) {
      const string& sort_xacts_value = report.HANDLER(sort_xacts_).value;
      if (!sort_xacts_value.empty())
        handler = std::make_shared<sort_xacts>(handler, expr_t(sort_xacts_value), report);
    }

    // collapse_posts causes xacts with multiple posts to appear as xacts
    // with a subtotaled post for each commodity used.
    if (report.HANDLED(collapse) || report.HANDLED(depth_)) {
      unsigned short collapse_depth = 0;
      if (report.HANDLED(depth_))
        collapse_depth = lexical_cast<int>(report.HANDLER(depth_).str());

      handler =
          std::make_shared<collapse_posts>(handler, report, expr, display_predicate, only_predicate,
                                           report.HANDLED(collapse_if_zero), collapse_depth);
    }

    // subtotal_posts combines all the posts it receives into one subtotal
    // xact, which has one post for each commodity in each account.
    //
    // period_posts is like subtotal_posts, but it subtotals according to time
    // periods rather than totaling everything.
    //
    // day_of_week_posts is like period_posts, except that it reports
    // all the posts that fall on each subsequent day of the week.
    if (report.HANDLED(equity))
      handler = std::make_shared<posts_as_equity>(handler, report, expr, report.HANDLED(unround));
    else if (report.HANDLED(subtotal))
      handler = std::make_shared<subtotal_posts>(handler, expr);
  }

  /*--- Day-of-week, by-payee, and interval grouping ---*/

  if (report.HANDLED(dow))
    handler = std::make_shared<day_of_week_posts>(handler, expr);
  else if (report.HANDLED(by_payee))
    handler = std::make_shared<by_payee_posts>(handler, expr);

  // interval_posts groups posts together based on a time period, such as
  // weekly or monthly.
  if (report.HANDLED(period_)) {
    date_interval_t interval(report.HANDLER(period_).str());

    // When -b is specified but the period string has no explicit range start,
    // pass the report begin date to interval_posts so that flush() can start
    // generating empty leading periods from that date when --empty is active.
    // This makes the running average (-A) count from the requested start date
    // rather than from the first transaction's date.
    optional<date_t> begin_of_report;
    if (report.HANDLED(begin_) && !interval.begin()) {
      date_interval_t begin_interval(report.HANDLER(begin_).str());
      begin_of_report = begin_interval.begin();
    }

    handler = std::make_shared<interval_posts>(handler, expr, interval, report.HANDLED(exact),
                                               report.HANDLED(empty),
                                               report.HANDLED(align_intervals), begin_of_report);
  }

  /*--- Detail transfer (--date, --account, --payee, --pivot) ---*/

  if (report.HANDLED(date_))
    handler = std::make_shared<transfer_details>(handler, transfer_details::SET_DATE,
                                                 report.session.journal->master,
                                                 report.HANDLER(date_).str(), report);

  if (report.HANDLED(account_)) {
    handler = std::make_shared<transfer_details>(handler, transfer_details::SET_ACCOUNT,
                                                 report.session.journal->master,
                                                 report.HANDLER(account_).str(), report);
  } else if (report.HANDLED(pivot_)) {
    string pivot = report.HANDLER(pivot_).str();
    pivot = string("\"") + pivot + ":\" + tag(\"" + pivot + "\")";
    handler = std::make_shared<transfer_details>(handler, transfer_details::SET_ACCOUNT,
                                                 report.session.journal->master, pivot, report);
  }

  if (report.HANDLED(payee_))
    handler = std::make_shared<transfer_details>(handler, transfer_details::SET_PAYEE,
                                                 report.session.journal->master,
                                                 report.HANDLER(payee_).str(), report);

  /*--- Payee/account rewriting and related/inject ---*/

  if (!report.session.journal->payee_rewrite_mappings.empty() ||
      !report.session.journal->account_rewrite_mappings.empty())
    handler = std::make_shared<rewrite_posts>(handler, report);

  // related_posts will pass along all posts related to the post received.  If
  // the `related_all' handler is on, then all the xact's posts are passed;
  // meaning that if one post of an xact is to be printed, all the post for
  // that xact will be printed.
  if (report.HANDLED(related))
    handler = std::make_shared<related_posts>(handler, report.HANDLED(related_all));

  if (report.HANDLED(inject_))
    handler = std::make_shared<inject_posts>(handler, report.HANDLED(inject_).str(),
                                             report.session.journal->master);

  return handler;
}

} // namespace ledger
