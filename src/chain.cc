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
 * filter pipeline at report time: chain_pre_post_handlers() adds the
 * outermost filters and chain_post_handlers() the inner ones.
 * chain_selection_handlers() and chain_transfer_handlers() build two
 * stretches of the inner chain that posts_report_quick() shares.  Each
 * builds its part of the chain from the inside out: each new handler wraps
 * the previous one, so the last handler added is the first to receive
 * postings.
 * The inline chain_handlers() in chain.h calls chain_post_handlers first,
 * then chain_pre_post_handlers.  The resulting order is described once, in
 * the handler_ordering section of filters.h.
 */

#include <system.hh>
#include <utility>

#include "chain.h"
#include "predicate.h"
#include "filters.h"
#include "report.h"
#include "session.h"

namespace ledger {

namespace {
/// Whether @p name is a value of a posting: a name the posting scope
/// resolves (item_t::lookup, post_t::lookup) that is called without
/// arguments and yields a value.  The metadata accessors and any/all take
/// arguments, and post, xact and parent yield a scope; a name added to those
/// lookups that does either belongs in this list.
bool is_posting_value(const string& name) {
  static const std::set<string> not_values = {"all",    "any",  "has_meta", "has_tag", "meta",
                                              "parent", "post", "tag",      "xact"};
  return post_t().lookup(symbol_t::FUNCTION, name) && not_values.count(name) == 0;
}

/// The account expression for --pivot and --pivot-only on @p name: the
/// tag's value, or, for postings without the tag, the posting's value
/// @p name when there is one (#1048).  The choice is made once here, not by
/// re-reading each posting's result.
string pivot_account_expr(const string& name) {
  string quoted = string("\"") + name + "\"";
  string tag_value = "tag(" + quoted + ")";
  if (is_posting_value(name))
    tag_value = "(has_tag(" + quoted + ") ? " + tag_value + " : " + name + ")";
  return string("\"") + name + ":\" + " + tag_value;
}
} // namespace

/*--- Pre-post handlers (outermost filters) ---*/

/**
 * @brief Build the outer (upstream) portion of the posting filter chain.
 *
 * Handlers are added inside out.  The resulting order, and why the --limit
 * filter sits between --pivot and --pivot-only, is in the handler_ordering
 * section of filters.h.
 */
post_handler_ptr chain_pre_post_handlers(post_handler_ptr base_handler, report_t& report) {
  post_handler_ptr handler(std::move(base_handler));

  // anonymize_posts removes all meaningful information from xact payee's and
  // account names, for the sake of creating useful bug reports.
  if (report.HANDLED(anon))
    handler = std::make_shared<anonymize_posts>(handler, report.temp_accounts());

  // --pivot-only (issue #1153) replaces the original account entirely with
  // "Tag:Value" rather than prepending it.  Because the original account
  // hierarchy is dropped, command-line account queries must be evaluated
  // against the ORIGINAL account name (before the rename); otherwise filters
  // like `bal Assets --pivot-only commodity` would never match the pivoted
  // "commodity:CAD" accounts and produce empty output (#3210).  Adding the
  // transformer here -- before the --limit filter below -- makes the filter
  // wrap the pivot in the data flow, so the predicate sees the original
  // account.
  //
  // --account takes precedence over --pivot and --pivot-only (neither is
  // installed when it is given).  Given both pivots, --pivot-only runs after
  // --pivot and replaces the account it produced.  --account stays in
  // chain_post_handlers because it does not interact with query filtering.
  if (report.HANDLED(pivot_only_) && !report.HANDLED(account_)) {
    handler = std::make_shared<transfer_details>(
        handler, transfer_details::SET_ACCOUNT_REPLACE,
        pivot_account_expr(report.HANDLER(pivot_only_).str()), report, report.temp_accounts());
  }

  // This filter_posts will only pass through posts matching the `predicate'.
  if (report.HANDLED(limit_)) {
    DEBUG("report.predicate", "Report predicate expression = " << report.HANDLER(limit_).str());
    handler = std::make_shared<filter_posts>(
        handler, predicate_t(report.HANDLER(limit_).str(), report.what_to_keep()), report);
  }

  // When --pivot is active, apply the account-name transformation before the
  // --limit predicate so that command-line account queries (which become
  // --limit terms) match the pivoted account names rather than the original
  // ones.  This makes `bal --pivot Tag Value` filter by the pivoted name
  // "Tag:Value:..." instead of the original account name.  (See #1154.)
  // Because --pivot keeps the original account as a suffix, a query like
  // `bal --pivot Project Expenses` still matches the original "Expenses"
  // segment, so the same filter works for both pivoted and unpivoted names.
  std::shared_ptr<transfer_details> pivot_handler;
  if (report.HANDLED(pivot_) && !report.HANDLED(account_)) {
    pivot_handler = std::make_shared<transfer_details>(
        handler, transfer_details::SET_ACCOUNT, pivot_account_expr(report.HANDLER(pivot_).str()),
        report, report.temp_accounts());
    handler = pivot_handler;
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

    if (report.origin)
      budget_handler->set_origin(*report.origin);

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
    // the filter get reported.  Under --pivot it evaluates the predicate on
    // the pivoted copy (as the primary filter does) but passes the original
    // posting on, because budget_posts matches original accounts; each
    // posting it sees therefore gets one extra temporary copy in the pivot
    // handler's temps, released by clear().
    if (report.HANDLED(limit_))
      handler = std::make_shared<filter_posts>(
          handler, predicate_t(report.HANDLER(limit_).str(), report.what_to_keep()), report,
          pivot_handler);
  } else if (report.HANDLED(forecast_while_)) {
    auto forecast_handler = std::make_shared<forecast_posts>(
        handler, predicate_t(report.HANDLER(forecast_while_).str(), report.what_to_keep()), report,
        (report.HANDLED(forecast_years_)
             ? static_cast<std::size_t>(report.HANDLER(forecast_years_).as_int())
             : 5UL));
    forecast_handler->add_period_xacts(report.session.journal->period_xacts);
    handler = forecast_handler;

    // See above, under budget_posts.
    if (report.HANDLED(limit_))
      handler = std::make_shared<filter_posts>(
          handler, predicate_t(report.HANDLER(limit_).str(), report.what_to_keep()), report,
          pivot_handler);
  }

  return handler;
}

/*--- Selection handlers (--head/--tail, --display, --only) ---*/

/**
 * @brief Wrap the chain with the stages that select which postings reach
 * the output: truncation (--head/--tail), the --display predicate and the
 * --only predicate.  Shared by chain_post_handlers() and
 * posts_report_quick().
 */
post_handler_ptr chain_selection_handlers(post_handler_ptr base_handler, report_t& report,
                                          bool has_collapse_stage, predicate_t& display_predicate,
                                          predicate_t& only_predicate) {
  post_handler_ptr handler(std::move(base_handler));

  // truncate_xacts cuts off a certain number of _xacts_ from being
  // displayed.  It does not affect calculation.
  if (report.HANDLED(head_) || report.HANDLED(tail_))
    handler = std::make_shared<truncate_xacts>(
        handler, report.HANDLED(head_) ? report.HANDLER(head_).as_int() : 0,
        report.HANDLED(tail_) ? report.HANDLER(tail_).as_int() : 0);

  // filter_posts will only pass through posts matching the
  // `display_predicate'.
  //
  // --depth N limits accounts through display_ (`post|depth<=N`, see
  // report.h).  Postings are limited by collapse_posts, so the depth term
  // applies to postings only where that stage runs.  There it also hides
  // the uncollapsed postings that --collapse-if-zero passes through, and it
  // keeps collapse_posts' single-post shortcut off under --depth 0 (#3198).
  // The listing commands have no collapse stage, and their postings keep
  // their journal accounts.
  if (report.HANDLED(display_)) {
    string display_expr = report.HANDLER(display_).str();
    if (has_collapse_stage && report.HANDLED(depth_))
      display_expr =
          "(" + display_expr + ")&(depth<=" + std::to_string(report.HANDLER(depth_).as_int()) + ")";
    display_predicate = predicate_t(display_expr, report.what_to_keep());
    handler = std::make_shared<filter_posts>(handler, display_predicate, report);
  }

  // filter_posts will only pass through posts matching the `only' (a.k.a.
  // "secondary") predicate.  Placed downstream of calc_posts, revaluation,
  // and display_filter_posts so that running totals (and the rounding
  // adjustments fed by display_filter_posts' incremental accumulator)
  // reflect ALL postings; --only then suppresses non-matching posts from
  // the displayed output without altering the accumulated total.  This is
  // what distinguishes --only from --limit: --limit excludes postings
  // from calculation entirely, while --only keeps them in the running
  // total but hides them from the report.  See #762.
  if (report.HANDLED(only_)) {
    only_predicate = predicate_t(report.HANDLER(only_).str(), report.what_to_keep());
    handler = std::make_shared<filter_posts>(handler, only_predicate, report);
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
  // Ask the output handler, before it is wrapped, whether it reads the
  // running totals calc_posts would store in every posting (see
  // item_handler::wants_running_total).
  const bool output_wants_total = base_handler->wants_running_total();
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

  if (!for_accounts_report) {
    // Forecast-while filter: innermost, so that forecast postings failing
    // the predicate never reach the output handler.  Accounts reports need
    // no such filter: their output handler ignores postings, and
    // forecast_posts::flush withholds a failing forecast group from the
    // account totals by dropping POST_EXT_VISITED (#1605).
    if (report.HANDLED(forecast_while_)) {
      handler = std::make_shared<filter_posts>(
          handler, predicate_t(report.HANDLER(forecast_while_).str(), report.what_to_keep()),
          report);
    }

    handler = chain_selection_handlers(handler, report, /*has_collapse_stage=*/true,
                                       display_predicate, only_predicate);

    // display_filter_posts adds virtual posts to the list to account
    // for changes in value of commodities, which otherwise would affect
    // the running total unpredictably.  Placed before filter_posts so
    // it sees all postings and can correctly detect rounding gaps.
    //
    // Rounding adjustments are only useful for commands that display
    // running totals (register), not for print/csv/equity.  normalize_options
    // decides which commands those are (report_t::show_rounding_adjustments).
    // Additionally, rounding is safe only when the display expressions
    // use simple additive accumulation (the defaults).  Non-additive
    // transforms like --average, --deviation, or user-supplied
    // --display-total/--display-amount break the assumption that
    // total == prev_total + amount, so we disable rounding for those.
    bool default_exprs = report.default_total_exprs() && report.default_amount_exprs();

    auto display_filter_sp = std::make_shared<display_filter_posts>(
        handler, report,
        !report.HANDLED(no_rounding) &&
            (report.HANDLED(revalued) || (report.show_rounding_adjustments && default_exprs)));
    display_filter = display_filter_sp.get();
    handler = std::move(display_filter_sp);
  }

  /*--- Only predicate (accounts reports) ---*/

  // For accounts reports there is no display_filter_posts, so the --only
  // filter sits directly between calc_posts and the output handler.  This
  // mirrors the position used for register reports above, while keeping
  // the running total computed over ALL postings.  See #762.
  if (for_accounts_report && report.HANDLED(only_)) {
    only_predicate = predicate_t(report.HANDLER(only_).str(), report.what_to_keep());
    handler = std::make_shared<filter_posts>(handler, only_predicate, report);
  }

  /*--- Revaluation and calculation ---*/

  // changed_value_posts adds virtual posts to the list to account for changes
  // in market value of commodities, which otherwise would affect the running
  // total unpredictably.
  if (report.HANDLED(revalued) && (!for_accounts_report || report.HANDLED(unrealized)))
    handler =
        std::make_shared<changed_value_posts>(handler, report, for_accounts_report, display_filter);

  // calc_posts computes the running total.  When this appears will determine,
  // for example, whether filtered posts are included or excluded from the
  // running total.
  {
    // A running total costs a balance update per posting and keeps every
    // version of the total alive for the whole report, so compute it only
    // when something downstream of calc_posts can read it: the output
    // handler itself (register, csv, xml, the Python collectors; print
    // declines), an option that works from totals (--average, --deviation,
    // --percent, --revalued), or an expression evaluated from calc_posts
    // on: the predicates of --display, --only and --forecast-while
    // (--forecast), and the --amount and --display-amount expressions,
    // which calc_posts and display_filter_posts evaluate for every posting.
    // Such an expression may reach the total under any of its names or
    // through a function of its own, so its presence alone decides.  A
    // false positive merely restores the old behaviour.  Sorting and
    // grouping run upstream of calc_posts and never see a running total.
    bool totals_requested = output_wants_total || report.HANDLED(average) ||
                            report.HANDLED(deviation) || report.HANDLED(percent) ||
                            report.HANDLED(revalued) || report.HANDLED(display_) ||
                            report.HANDLED(only_) || report.HANDLED(forecast_while_) ||
                            report.HANDLED(amount_) || report.HANDLED(display_amount_);

    bool calc_running =
        (!for_accounts_report || (report.HANDLED(revalued) && report.HANDLED(unrealized))) &&
        totals_requested;

    // Enable incremental stripped total maintenance when the display_total
    // and total expressions are unmodified (default "total" accumulation)
    // and annotations need stripping.  This lets display_filter_posts and
    // scrubbed_display_total use the pre-stripped value from xdata,
    // avoiding expensive O(K) GMP arithmetic per posting.
    keep_details_t wtk = report.what_to_keep();
    bool maintain_stripped = calc_running && report.default_total_exprs() && !wtk.keep_all();

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

  /*--- Sorting, collapsing, and aggregation ---*/

  if (!for_accounts_report) {
    // sort_xacts sorts the postings within each transaction by --sort-xacts;
    // sort_posts, upstream of it, sorts all postings by --sort.  A period
    // report sorts by --sort only within each period, which interval_posts
    // emits as one transaction (report_t::normalize_period).
    if (report.HANDLED(sort_xacts_))
      handler =
          std::make_shared<sort_xacts>(handler, expr_t(report.HANDLER(sort_xacts_).str()), report);
    if (report.HANDLED(sort_)) {
      if (report.sort_within_periods)
        handler =
            std::make_shared<sort_xacts>(handler, expr_t(report.HANDLER(sort_).str()), report);
      else
        handler =
            std::make_shared<sort_posts>(handler, expr_t(report.HANDLER(sort_).str()), report);
    }

    // collapse_posts causes xacts with multiple posts to appear as xacts
    // with a subtotaled post for each commodity used.
    if (report.HANDLED(collapse) || report.HANDLED(depth_)) {
      unsigned short collapse_depth = 0;
      if (report.HANDLED(depth_))
        collapse_depth = static_cast<unsigned short>(report.HANDLER(depth_).as_int());

      handler =
          std::make_shared<collapse_posts>(handler, report, expr, display_predicate, only_predicate,
                                           report.HANDLED(collapse_if_zero), collapse_depth);
    }

    // subtotal_posts combines all the posts it receives into one subtotal
    // xact, which has one post for each commodity in each account.
    //
    // interval_posts is like subtotal_posts, but it subtotals according to time
    // periods rather than totaling everything.
    //
    // day_of_week_posts is like interval_posts, except that it reports
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
      begin_of_report = report.report_begin;
    }

    // When -e is specified and the period string has no explicit range end,
    // pass the report end date to interval_posts so that flush() can generate
    // empty trailing periods up to that date when --empty is active.  Skip
    // this when --average (-A) is the sole reason --empty is active, because
    // trailing zero-amount periods would incorrectly dilute the running average.
    optional<date_t> end_of_report;
    if (report.HANDLED(end_) && !interval.end() && report.HANDLED(empty) &&
        !report.HANDLED(average)) {
      end_of_report = report.report_end;
    }

    handler = std::make_shared<interval_posts>(
        handler, expr, interval, report.HANDLED(exact), report.HANDLED(empty),
        report.HANDLED(align_intervals), begin_of_report, end_of_report);
  }

  return chain_transfer_handlers(handler, report);
}

/**
 * @brief Wrap the chain with the stages that rename, relate or inject
 * postings: --date/--account/--payee, payee/account rewrites, --related and
 * --inject.  Shared by chain_post_handlers() and posts_report_quick().
 */
post_handler_ptr chain_transfer_handlers(post_handler_ptr base_handler, report_t& report) {
  post_handler_ptr handler(std::move(base_handler));

  /*--- Detail transfer (--date, --account, --payee, --pivot) ---*/

  if (report.HANDLED(date_))
    handler = std::make_shared<transfer_details>(handler, transfer_details::SET_DATE,
                                                 report.HANDLER(date_).str(), report,
                                                 report.temp_accounts());

  if (report.HANDLED(account_)) {
    handler = std::make_shared<transfer_details>(handler, transfer_details::SET_ACCOUNT,
                                                 report.HANDLER(account_).str(), report,
                                                 report.temp_accounts());
  }
  // Note: --pivot is handled in chain_pre_post_handlers() so that the
  // account name transformation occurs before the --limit predicate is
  // evaluated.  This allows command-line account queries to match
  // pivoted account names (see issue #1154).

  if (report.HANDLED(payee_))
    handler = std::make_shared<transfer_details>(handler, transfer_details::SET_PAYEE,
                                                 report.HANDLER(payee_).str(), report,
                                                 report.temp_accounts());

  /*--- Payee/account rewriting and related/inject ---*/

  if (rewrite_posts::has_rules(report))
    handler = std::make_shared<rewrite_posts>(handler, report, report.temp_accounts());

  // related_posts will pass along all posts related to the post received.  If
  // the `related_all' handler is on, then all the xact's posts are passed;
  // meaning that if one post of an xact is to be printed, all the post for
  // that xact will be printed.
  if (report.HANDLED(related))
    handler = std::make_shared<related_posts>(handler, report.HANDLED(related_all));

  if (report.HANDLED(inject_))
    handler =
        std::make_shared<inject_posts>(handler, report.HANDLED(inject_).str(),
                                       report.session.journal->master, report.temp_accounts());

  return handler;
}

} // namespace ledger
