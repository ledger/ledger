/*
 * Copyright (c) 2003-2014, John Wiegley.  All rights reserved.
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

#include <system.hh>

#include "chain.h"
#include "predicate.h"
#include "filters.h"
#include "report.h"
#include "session.h"

namespace ledger {

post_handler_ptr chain_pre_post_handlers(post_handler_ptr base_handler,
                                         report_t&        report)
{
  post_handler_ptr handler(base_handler);

  // anonymize_posts removes all meaningful information from xact payee's and
  // account names, for the sake of creating useful bug reports.
  if (report.HANDLED(anon))
    handler.reset(new anonymize_posts(handler));

  // This filter_posts will only pass through posts matching the `predicate'.
  if (report.HANDLED(limit_)) {
    DEBUG("report.predicate",
          "Report predicate expression = " << report.HANDLER(limit_).str());
    handler.reset(new filter_posts
                  (handler, predicate_t(report.HANDLER(limit_).str(),
                                        report.what_to_keep()),
                   report));
  }

  // budget_posts takes a set of posts from a data file and uses them to
  // generate "budget posts" which balance against the reported posts.
  //
  // forecast_posts is a lot like budget_posts, except that it adds xacts
  // only for the future, and does not balance them against anything but the
  // future balance.

  if (report.budget_flags != BUDGET_NO_BUDGET) {
    budget_posts * budget_handler =
      new budget_posts(handler, report.terminus.date(), report.budget_flags);
    budget_handler->add_period_xacts(report.session.journal->period_xacts);
    handler.reset(budget_handler);

    // Apply this before the budget handler, so that only matching posts are
    // calculated toward the budget.  The use of filter_posts above will
    // further clean the results so that no automated posts that don't match
    // the filter get reported.
    if (report.HANDLED(limit_))
      handler.reset(new filter_posts
                    (handler, predicate_t(report.HANDLER(limit_).str(),
                                          report.what_to_keep()),
                     report));
  }
  else if (report.HANDLED(forecast_while_)) {
    forecast_posts * forecast_handler
      = new forecast_posts(handler,
                           predicate_t(report.HANDLER(forecast_while_).str(),
                                       report.what_to_keep()),
                           report,
                           (report.HANDLED(forecast_years_) ?
                            lexical_cast<std::size_t>
                            (report.HANDLER(forecast_years_).value) : 5UL));
    forecast_handler->add_period_xacts(report.session.journal->period_xacts);
    handler.reset(forecast_handler);

    // See above, under budget_posts.
    if (report.HANDLED(limit_))
      handler.reset(new filter_posts
                    (handler, predicate_t(report.HANDLER(limit_).str(),
                                          report.what_to_keep()),
                     report));
  }

  return handler;
}

post_handler_ptr chain_post_handlers(post_handler_ptr base_handler,
                                     report_t&        report,
                                     bool             for_accounts_report)
{
  post_handler_ptr       handler(base_handler);
  predicate_t            display_predicate;
  predicate_t            only_predicate;
  display_filter_posts * display_filter = NULL;

  expr_t& expr(report.HANDLER(amount_).expr);
  expr.set_context(&report);

  report.HANDLER(total_).expr.set_context(&report);
  report.HANDLER(display_amount_).expr.set_context(&report);
  report.HANDLER(display_total_).expr.set_context(&report);

  if (! for_accounts_report) {
    // Make sure only forecast postings which match are allowed through
    if (report.HANDLED(forecast_while_)) {
      handler.reset(new filter_posts
                    (handler, predicate_t(report.HANDLER(forecast_while_).str(),
                                          report.what_to_keep()),
                     report));
    }

    // truncate_xacts cuts off a certain number of _xacts_ from being
    // displayed.  It does not affect calculation.
    if (report.HANDLED(head_) || report.HANDLED(tail_))
      handler.reset
        (new truncate_xacts(handler,
                            report.HANDLED(head_) ?
                            lexical_cast<int>(report.HANDLER(head_).value) : 0,
                            report.HANDLED(tail_) ?
                            lexical_cast<int>(report.HANDLER(tail_).value) : 0));

    // display_filter_posts adds virtual posts to the list to account
    // for changes in value of commodities, which otherwise would affect
    // the running total unpredictably.
    display_filter = new display_filter_posts(handler, report,
                                              report.HANDLED(revalued) &&
                                              ! report.HANDLED(no_rounding));
    handler.reset(display_filter);

    // filter_posts will only pass through posts matching the
    // `display_predicate'.
    if (report.HANDLED(display_)) {
      display_predicate = predicate_t(report.HANDLER(display_).str(),
                                      report.what_to_keep());
      handler.reset(new filter_posts(handler, display_predicate, report));
    }
  }

  // changed_value_posts adds virtual posts to the list to account for changes
  // in market value of commodities, which otherwise would affect the running
  // total unpredictably.
  if (report.HANDLED(revalued) &&
      (! for_accounts_report || report.HANDLED(unrealized)))
    handler.reset(new changed_value_posts(handler, report, for_accounts_report,
                                          report.HANDLED(unrealized),
                                          display_filter));

  // calc_posts computes the running total.  When this appears will determine,
  // for example, whether filtered posts are included or excluded from the
  // running total.
  handler.reset(new calc_posts(handler, expr, (! for_accounts_report ||
                                               (report.HANDLED(revalued) &&
                                                report.HANDLED(unrealized)))));

  // filter_posts will only pass through posts matching the
  // `secondary_predicate'.
  if (report.HANDLED(only_)) {
    only_predicate = predicate_t(report.HANDLER(only_).str(),
                                 report.what_to_keep());
    handler.reset(new filter_posts(handler, only_predicate, report));
  }

  if (! for_accounts_report) {
    // sort_posts will sort all the posts it sees, based on the `sort_order'
    // value expression.
    if (report.HANDLED(sort_)) {
      if (report.HANDLED(sort_xacts_))
        handler.reset(new sort_xacts(handler, report.HANDLER(sort_).str()));
      else
        handler.reset(new sort_posts(handler, report.HANDLER(sort_).str()));
    }

    // collapse_posts causes xacts with multiple posts to appear as xacts
    // with a subtotaled post for each commodity used.
    if (report.HANDLED(collapse))
      handler.reset(new collapse_posts(handler, report, expr,
                                       display_predicate, only_predicate,
                                       report.HANDLED(collapse_if_zero)));

    // subtotal_posts combines all the posts it receives into one subtotal
    // xact, which has one post for each commodity in each account.
    //
    // period_posts is like subtotal_posts, but it subtotals according to time
    // periods rather than totalling everything.
    //
    // day_of_week_posts is like period_posts, except that it reports
    // all the posts that fall on each subsequent day of the week.
    if (report.HANDLED(equity))
      handler.reset(new posts_as_equity(handler, report, expr));
    else if (report.HANDLED(subtotal))
      handler.reset(new subtotal_posts(handler, expr));
  }

  if (report.HANDLED(dow))
    handler.reset(new day_of_week_posts(handler, expr));
  else if (report.HANDLED(by_payee))
    handler.reset(new by_payee_posts(handler, expr));

  // interval_posts groups posts together based on a time period, such as
  // weekly or monthly.
  if (report.HANDLED(period_))
    handler.reset(new interval_posts(handler, expr,
                                     report.HANDLER(period_).str(),
                                     report.HANDLED(exact),
                                     report.HANDLED(empty)));

  if (report.HANDLED(date_))
    handler.reset(new transfer_details(handler, transfer_details::SET_DATE,
                                       report.session.journal->master,
                                       report.HANDLER(date_).str(),
                                       report));

  if (report.HANDLED(account_)) {
    handler.reset(new transfer_details(handler, transfer_details::SET_ACCOUNT,
                                       report.session.journal->master,
                                       report.HANDLER(account_).str(),
                                       report));
  }
  else if (report.HANDLED(pivot_)) {
    string pivot = report.HANDLER(pivot_).str();
    pivot = string("\"") + pivot + ":\" + tag(\"" + pivot + "\")";
    handler.reset(new transfer_details(handler, transfer_details::SET_ACCOUNT,
                                       report.session.journal->master, pivot,
                                       report));
  }

  if (report.HANDLED(payee_))
    handler.reset(new transfer_details(handler, transfer_details::SET_PAYEE,
                                       report.session.journal->master,
                                       report.HANDLER(payee_).str(),
                                       report));

  // related_posts will pass along all posts related to the post received.  If
  // the `related_all' handler is on, then all the xact's posts are passed;
  // meaning that if one post of an xact is to be printed, all the post for
  // that xact will be printed.
  if (report.HANDLED(related))
    handler.reset(new related_posts(handler, report.HANDLED(related_all)));

  if (report.HANDLED(inject_))
    handler.reset(new inject_posts(handler, report.HANDLED(inject_).str(),
                                   report.session.journal->master));

  return handler;
}

} // namespace ledger
