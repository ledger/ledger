/*
 * Copyright (c) 2003-2008, John Wiegley.  All rights reserved.
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

#include "report.h"
#include "reconcile.h"

namespace ledger {

xact_handler_ptr
report_t::chain_xact_handlers(xact_handler_ptr base_handler,
			      const bool handle_individual_transactions)
{
  bool remember_components = false;

  xact_handler_ptr handler(base_handler);

  // format_transactions write each transaction received to the
  // output stream.
  if (handle_individual_transactions) {
    // truncate_entries cuts off a certain number of _entries_ from
    // being displayed.  It does not affect calculation.
    if (head_entries || tail_entries)
      handler.reset(new truncate_entries(handler, head_entries, tail_entries));

    // filter_transactions will only pass through transactions
    // matching the `display_predicate'.
    if (! display_predicate.empty())
      handler.reset(new filter_transactions(handler, display_predicate));

    // calc_transactions computes the running total.  When this
    // appears will determine, for example, whether filtered
    // transactions are included or excluded from the running total.
    handler.reset(new calc_transactions(handler));

    // component_transactions looks for reported transaction that
    // match the given `descend_expr', and then reports the
    // transactions which made up the total for that reported
    // transaction.
    if (! descend_expr.empty()) {
      std::list<std::string> descend_exprs;

      std::string::size_type beg = 0;
      for (std::string::size_type pos = descend_expr.find(';');
	   pos != std::string::npos;
	   beg = pos + 1, pos = descend_expr.find(';', beg))
	descend_exprs.push_back(std::string(descend_expr, beg, pos - beg));
      descend_exprs.push_back(std::string(descend_expr, beg));

      for (std::list<std::string>::reverse_iterator i =
	     descend_exprs.rbegin();
	   i != descend_exprs.rend();
	   i++)
	handler.reset(new component_transactions(handler, *i));

      remember_components = true;
    }

    // reconcile_transactions will pass through only those
    // transactions which can be reconciled to a given balance
    // (calculated against the transactions which it receives).
    if (! reconcile_balance.empty()) {
      datetime_t cutoff = current_moment;
      if (! reconcile_date.empty())
	cutoff = parse_datetime(reconcile_date);
      handler.reset(new reconcile_transactions
		    (handler, value_t(reconcile_balance), cutoff));
    }

    // filter_transactions will only pass through transactions
    // matching the `secondary_predicate'.
    if (! secondary_predicate.empty())
      handler.reset(new filter_transactions(handler, secondary_predicate));

    // sort_transactions will sort all the transactions it sees, based
    // on the `sort_order' value expression.
    if (! sort_string.empty()) {
      if (entry_sort)
	handler.reset(new sort_entries(handler, sort_string));
      else
	handler.reset(new sort_transactions(handler, sort_string));
    }

    // changed_value_transactions adds virtual transactions to the
    // list to account for changes in market value of commodities,
    // which otherwise would affect the running total unpredictably.
    if (show_revalued)
      handler.reset(new changed_value_transactions(handler, show_revalued_only));

    // collapse_transactions causes entries with multiple transactions
    // to appear as entries with a subtotaled transaction for each
    // commodity used.
    if (show_collapsed)
      handler.reset(new collapse_transactions(handler));

    // subtotal_transactions combines all the transactions it receives
    // into one subtotal entry, which has one transaction for each
    // commodity in each account.
    //
    // period_transactions is like subtotal_transactions, but it
    // subtotals according to time periods rather than totalling
    // everything.
    //
    // dow_transactions is like period_transactions, except that it
    // reports all the transactions that fall on each subsequent day
    // of the week.
    if (show_subtotal)
      handler.reset(new subtotal_transactions(handler, remember_components));

    if (days_of_the_week)
      handler.reset(new dow_transactions(handler, remember_components));
    else if (by_payee)
      handler.reset(new by_payee_transactions(handler, remember_components));

    // interval_transactions groups transactions together based on a
    // time period, such as weekly or monthly.
    if (! report_period.empty()) {
      handler.reset(new interval_transactions(handler, report_period,
					      remember_components));
      handler.reset(new sort_transactions(handler, "d"));
    }
  }

  // invert_transactions inverts the value of the transactions it
  // receives.
  if (show_inverted)
    handler.reset(new invert_transactions(handler));

  // related_transactions will pass along all transactions related
  // to the transaction received.  If `show_all_related' is true,
  // then all the entry's transactions are passed; meaning that if
  // one transaction of an entry is to be printed, all the
  // transaction for that entry will be printed.
  if (show_related)
    handler.reset(new related_transactions(handler, show_all_related));

  // This filter_transactions will only pass through transactions
  // matching the `predicate'.
  if (! predicate.empty())
    handler.reset(new filter_transactions(handler, predicate));

#if 0
  // budget_transactions takes a set of transactions from a data
  // file and uses them to generate "budget transactions" which
  // balance against the reported transactions.
  //
  // forecast_transactions is a lot like budget_transactions, except
  // that it adds entries only for the future, and does not balance
  // them against anything but the future balance.

  if (budget_flags) {
    budget_transactions * budget_handler
      = new budget_transactions(handler, budget_flags);
    budget_handler->add_period_entries(journal->period_entries);
    handler.reset(budget_handler;

    // Apply this before the budget handler, so that only matching
    // transactions are calculated toward the budget.  The use of
    // filter_transactions above will further clean the results so
    // that no automated transactions that don't match the filter get
    // reported.
    if (! predicate.empty())
      handler.reset(new filter_transactions(handler, predicate));
  }
  else if (! forecast_limit.empty()) {
    forecast_transactions * forecast_handler
      = new forecast_transactions(handler, forecast_limit);
    forecast_handler->add_period_entries(journal->period_entries);
    handler.reset(forecast_handler;

    // See above, under budget_transactions.
    if (! predicate.empty())
      handler.reset(new filter_transactions(handler, predicate));
  }
#endif

  if (comm_as_payee)
    handler.reset(new set_comm_as_payee(handler));
  else if (code_as_payee)
    handler.reset(new set_code_as_payee(handler));

  return handler;
}

void report_t::transactions_report(xact_handler_ptr handler)
{
  session_transactions_iterator walker(session);
  pass_down_transactions(chain_xact_handlers(handler), walker);
  handler->flush();

  if (DO_VERIFY())
    session.clean_transactions();
}

void report_t::entry_report(xact_handler_ptr handler, entry_t& entry)
{
  entry_transactions_iterator walker(entry);
  pass_down_transactions(chain_xact_handlers(handler), walker);
  handler->flush();

  if (DO_VERIFY())
    session.clean_transactions(entry);
}

void report_t::sum_all_accounts()
{
  session_transactions_iterator walker(session);
  pass_down_transactions
    (chain_xact_handlers(xact_handler_ptr(new set_account_value), false),
     walker);
  // no flush() needed with set_account_value
  sum_accounts(*session.master);
}

void report_t::accounts_report(acct_handler_ptr handler,
			       const bool print_final_total)
{
  sum_all_accounts();

  if (sort_string.empty()) {
    accounts_iterator walker(*session.master);
    pass_down_accounts<accounts_iterator>(handler, walker);
  } else {
    sorted_accounts_iterator walker(*session.master, sort_string);
    pass_down_accounts<sorted_accounts_iterator>(handler, walker);
  }
  handler->flush();
    
  if (print_final_total && account_has_xdata(*session.master)) {
    account_xdata_t& xdata = account_xdata(*session.master);
    if (! show_collapsed && xdata.total) {
#if 0
      *out << "--------------------\n";
      xdata.value = xdata.total;
      handler->format.format(*out, details_t(*journal->master));
#endif
    }
  }

  if (DO_VERIFY()) {
    session.clean_transactions();
    session.clean_accounts();
  }
}

void report_t::commodities_report(const string& format)
{
}

void report_t::entry_report(const entry_t& entry, const string& format)
{
}

value_t report_t::abbrev(expr::call_scope_t& args)
{
  if (args.size() < 2)
    throw_(std::logic_error, "usage: abbrev(STRING, WIDTH [, STYLE, ABBREV_LEN])");

  string str = args[0].as_string();
#if 0
  long	 wid = args[1];

  elision_style_t style = session.elision_style;
  if (args.size() == 3)
    style = static_cast<elision_style_t>(args[2].as_long());
#endif

  long abbrev_len = session.abbrev_length;
  if (args.size() == 4)
    abbrev_len = args[3].as_long();

#if 0
  return value_t(abbreviate(str, wid, style, true,
			    static_cast<int>(abbrev_len)), true);
#else
  return NULL_VALUE;
#endif
}

value_t report_t::ftime(expr::call_scope_t& args)
{
  if (args.size() < 1)
    throw_(std::logic_error, "usage: ftime(DATE [, DATE_FORMAT])");

  datetime_t date = args[0].as_datetime();

  string date_format;
  if (args.size() == 2)
    date_format = args[1].as_string();
#if 0
  // jww (2007-04-18): Need to setup an output facet here
  else
    date_format = moment_t::output_format;

  return value_t(date.as_string(date_format), true);
#else
  return NULL_VALUE;
#endif
}

#if 0
optional<value_t>
report_t::resolve(const string& name, expr::call_scope_t& args)
{
  const char * p = name.c_str();
  switch (*p) {
  case 'a':
    if (name == "abbrev") {
      return abbrev(args);
    }
    break;

  case 'f':
    if (name == "ftime") {
      return ftime(args);
    }
    break;
  }
  return expr::scope_t::resolve(name, args);
}
#endif

expr::ptr_op_t report_t::lookup(const string& name)
{
  const char * p = name.c_str();
  switch (*p) {
  case 'o':
    if (std::strncmp(p, "option_", 7) == 0) {
      p = p + 7;
      switch (*p) {
      case 'a':
#if 0
	if (std::strcmp(p, "accounts") == 0)
	  return MAKE_FUNCTOR(report_t::option_accounts);
	else
#endif
	  if (std::strcmp(p, "amount") == 0)
	    return MAKE_FUNCTOR(report_t::option_amount);
	break;

      case 'b':
	if (std::strcmp(p, "bar") == 0)
	  return MAKE_FUNCTOR(report_t::option_bar);
	break;

#if 0
      case 'c':
	if (std::strcmp(p, "clean") == 0)
	  return MAKE_FUNCTOR(report_t::option_clean);
	else if (std::strcmp(p, "compact") == 0)
	  return MAKE_FUNCTOR(report_t::option_compact);
	break;
#endif

      case 'e':
#if 0
	if (std::strcmp(p, "entries") == 0)
	  return MAKE_FUNCTOR(report_t::option_entries);
	else if (std::strcmp(p, "eval") == 0)
	  return MAKE_FUNCTOR(report_t::option_eval);
	else if (std::strcmp(p, "exclude") == 0)
	  return MAKE_FUNCTOR(report_t::option_remove);
#endif
	break;

      case 'f':
#if 0
	if (std::strcmp(p, "foo") == 0)
	  return MAKE_FUNCTOR(report_t::option_foo);
	else
#endif
	  if (std::strcmp(p, "format") == 0)
	  return MAKE_FUNCTOR(report_t::option_format);
	break;

      case 'i':
#if 0
	if (std::strcmp(p, "include") == 0)
	  return MAKE_FUNCTOR(report_t::option_select);
#endif
	break;

      case 'l':
#if 0
	if (! *(p + 1) || std::strcmp(p, "limit") == 0)
	  return MAKE_FUNCTOR(report_t::option_limit);
#endif
	break;

#if 0
      case 'm':
	if (std::strcmp(p, "merge") == 0)
	  return MAKE_FUNCTOR(report_t::option_merge);
	break;
#endif

      case 'r':
#if 0
	if (std::strcmp(p, "remove") == 0)
	  return MAKE_FUNCTOR(report_t::option_remove);
#endif
	break;

#if 0
      case 's':
	if (std::strcmp(p, "select") == 0)
	  return MAKE_FUNCTOR(report_t::option_select);
	else if (std::strcmp(p, "split") == 0)
	  return MAKE_FUNCTOR(report_t::option_split);
	break;
#endif

      case 't':
	if (! *(p + 1))
	  return MAKE_FUNCTOR(report_t::option_amount);
	else if (std::strcmp(p, "total") == 0)
	  return MAKE_FUNCTOR(report_t::option_total);
	break;

      case 'T':
	if (! *(p + 1))
	  return MAKE_FUNCTOR(report_t::option_total);
	break;
      }
    }
    break;
  }

  return expr::symbol_scope_t::lookup(name);
}

} // namespace ledger
