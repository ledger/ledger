/*
 * Copyright (c) 2003-2009, John Wiegley.  All rights reserved.
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

#include "chain.h"
#include "report.h"
#include "filters.h"

namespace ledger {

xact_handler_ptr chain_xact_handlers(report_t&	      report,
				     xact_handler_ptr base_handler,
				     bool             only_preliminaries)
{
  xact_handler_ptr handler(base_handler);
  item_predicate   display_predicate;
  item_predicate   only_predicate;

  assert(report.HANDLED(amount_));
  expr_t& expr(report.HANDLER(amount_).expr);
  expr.set_context(&report);

  if (! only_preliminaries) {
    // Make sure only forecast transactions which match are allowed through
    if (report.HANDLED(forecast_while_)) {
      handler.reset(new filter_xacts
		    (handler, item_predicate(report.HANDLER(forecast_while_).str(),
					     report.what_to_keep()),
		     report));
    }

    // truncate_entries cuts off a certain number of _entries_ from being
    // displayed.  It does not affect calculation.
    if (report.HANDLED(head_) || report.HANDLED(tail_))
      handler.reset
	(new truncate_entries(handler,
			      report.HANDLED(head_) ?
			      report.HANDLER(head_).value.to_long() : 0,
			      report.HANDLED(tail_) ?
			      report.HANDLER(tail_).value.to_long() : 0));

    // filter_xacts will only pass through xacts matching the
    // `display_predicate'.
    if (report.HANDLED(display_)) {
      display_predicate = item_predicate(report.HANDLER(display_).str(),
					 report.what_to_keep());
      handler.reset(new filter_xacts(handler, display_predicate, report));
    }

    // calc_xacts computes the running total.  When this appears will
    // determine, for example, whether filtered xacts are included or excluded
    // from the running total.
    handler.reset(new calc_xacts(handler, expr));
  }

  // filter_xacts will only pass through xacts matching the
  // `secondary_predicate'.
  if (report.HANDLED(only_)) {
    only_predicate = item_predicate(report.HANDLER(only_).str(),
				    report.what_to_keep());
    handler.reset(new filter_xacts(handler, only_predicate, report));
  }

  if (! only_preliminaries) {
    // sort_xacts will sort all the xacts it sees, based on the `sort_order'
    // value expression.
    if (report.HANDLED(sort_)) {
      if (report.HANDLED(sort_entries_))
	handler.reset(new sort_entries(handler, report.HANDLER(sort_).str()));
      else
	handler.reset(new sort_xacts(handler, report.HANDLER(sort_).str()));
    }

    // changed_value_xacts adds virtual xacts to the list to account for
    // changes in market value of commodities, which otherwise would affect
    // the running total unpredictably.
    if (report.HANDLED(revalued))
      handler.reset(new changed_value_xacts(handler,
					    report.HANDLER(total_).expr,
					    report.HANDLED(revalued_only)));

    // collapse_xacts causes entries with multiple xacts to appear as entries
    // with a subtotaled xact for each commodity used.
    if (report.HANDLED(collapse))
      handler.reset(new collapse_xacts(handler, expr,
				       display_predicate, only_predicate,
				       report.HANDLED(collapse_if_zero)));

    // subtotal_xacts combines all the xacts it receives into one subtotal
    // entry, which has one xact for each commodity in each account.
    //
    // period_xacts is like subtotal_xacts, but it subtotals according to time
    // periods rather than totalling everything.
    //
    // dow_xacts is like period_xacts, except that it reports all the xacts
    // that fall on each subsequent day of the week.
    if (report.HANDLED(equity))
      handler.reset(new xacts_as_equity(handler, expr));
    else if (report.HANDLED(subtotal))
      handler.reset(new subtotal_xacts(handler, expr));
  }

  if (report.HANDLED(dow))
    handler.reset(new dow_xacts(handler, expr));
  else if (report.HANDLED(by_payee))
    handler.reset(new by_payee_xacts(handler, expr));

  // interval_xacts groups xacts together based on a time period, such as
  // weekly or monthly.
  if (report.HANDLED(period_)) {
    handler.reset(new interval_xacts(handler, expr,
				     report.HANDLER(period_).str(),
				     report.session.master.get(),
				     report.HANDLED(exact),
				     report.HANDLED(empty)));
    handler.reset(new sort_xacts(handler, "date"));
  }

  // related_xacts will pass along all xacts related to the xact received.  If
  // the `related_all' handler is on, then all the entry's xacts are passed;
  // meaning that if one xact of an entry is to be printed, all the xact for
  // that entry will be printed.
  if (report.HANDLED(related))
    handler.reset(new related_xacts(handler, report.HANDLED(related_all)));

  // anonymize_xacts removes all meaningful information from entry payee's and
  // account names, for the sake of creating useful bug reports.
  if (report.HANDLED(anon))
    handler.reset(new anonymize_xacts(handler));

  // This filter_xacts will only pass through xacts matching the `predicate'.
  if (report.HANDLED(limit_)) {
    DEBUG("report.predicate",
	  "Report predicate expression = " << report.HANDLER(limit_).str());
    handler.reset(new filter_xacts
		  (handler, item_predicate(report.HANDLER(limit_).str(),
					   report.what_to_keep()),
		   report));
  }

  // budget_xacts takes a set of xacts from a data file and uses them to
  // generate "budget xacts" which balance against the reported xacts.
  //
  // forecast_xacts is a lot like budget_xacts, except that it adds entries
  // only for the future, and does not balance them against anything but the
  // future balance.

  if (report.budget_flags != BUDGET_NO_BUDGET) {
    budget_xacts * budget_handler = new budget_xacts(handler,
						     report.budget_flags);
    budget_handler->add_period_entries(report.session.journal->period_entries);
    handler.reset(budget_handler);

    // Apply this before the budget handler, so that only matching xacts are
    // calculated toward the budget.  The use of filter_xacts above will
    // further clean the results so that no automated xacts that don't match
    // the filter get reported.
    if (report.HANDLED(limit_))
      handler.reset(new filter_xacts
		    (handler, item_predicate(report.HANDLER(limit_).str(),
					     report.what_to_keep()),
		     report));
  }
  else if (report.HANDLED(forecast_while_)) {
    forecast_xacts * forecast_handler
      = new forecast_xacts(handler,
			   item_predicate(report.HANDLER(forecast_while_).str(),
					  report.what_to_keep()),
			   report);
    forecast_handler->add_period_entries(report.session.journal->period_entries);
    handler.reset(forecast_handler);

    // See above, under budget_xacts.
    if (report.HANDLED(limit_))
      handler.reset(new filter_xacts
		    (handler, item_predicate(report.HANDLER(limit_).str(),
					     report.what_to_keep()),
		     report));
  }

  if (report.HANDLED(set_account_))
    handler.reset(new transfer_details(handler, transfer_details::SET_ACCOUNT,
				       report.session.master.get(),
				       report.HANDLER(set_account_).str(),
				       report));
  else if (report.HANDLED(set_payee_))
    handler.reset(new transfer_details(handler, transfer_details::SET_PAYEE,
				       report.session.master.get(),
				       report.HANDLER(set_payee_).str(),
				       report));
  else if (report.HANDLED(comm_as_payee))
    handler.reset(new transfer_details(handler, transfer_details::SET_PAYEE,
				       report.session.master.get(),
				       expr_t("commodity"), report));
  else if (report.HANDLED(code_as_payee))
    handler.reset(new transfer_details(handler, transfer_details::SET_PAYEE,
				       report.session.master.get(),
				       expr_t("code"), report));
  else if (report.HANDLED(payee_as_account))
    handler.reset(new transfer_details(handler, transfer_details::SET_ACCOUNT,
				       report.session.master.get(),
				       expr_t("payee"), report));
  else if (report.HANDLED(comm_as_account))
    handler.reset(new transfer_details(handler, transfer_details::SET_ACCOUNT,
				       report.session.master.get(),
				       expr_t("commodity"), report));
  else if (report.HANDLED(code_as_account))
    handler.reset(new transfer_details(handler, transfer_details::SET_ACCOUNT,
				       report.session.master.get(),
				       expr_t("code"), report));

  return handler;
}

} // namespace ledger
