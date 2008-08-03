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

#if 0
void
report_t::regexps_to_predicate(const std::string& command,
			       std::list<std::string>::const_iterator begin,
			       std::list<std::string>::const_iterator end,
			       const bool account_regexp,
			       const bool add_account_short_masks,
			       const bool logical_and)
{
  std::string regexps[2];

  assert(begin != end);

  // Treat the remaining command-line arguments as regular
  // expressions, used for refining report results.

  for (std::list<std::string>::const_iterator i = begin;
       i != end;
       i++)
    if ((*i)[0] == '-') {
      if (! regexps[1].empty())
	regexps[1] += "|";
      regexps[1] += (*i).substr(1);
    }
    else if ((*i)[0] == '+') {
      if (! regexps[0].empty())
	regexps[0] += "|";
      regexps[0] += (*i).substr(1);
    }
    else {
      if (! regexps[0].empty())
	regexps[0] += "|";
      regexps[0] += *i;
    }

  for (int i = 0; i < 2; i++) {
    if (regexps[i].empty())
      continue;

    if (! predicate.empty())
      predicate += logical_and ? "&" : "|";

    int add_predicate = 0;	// 1 adds /.../, 2 adds ///.../
    if (i == 1) {
      predicate += "!";
    }
    else if (add_account_short_masks) {
      if (regexps[i].find(':') != std::string::npos ||
	  regexps[i].find('.') != std::string::npos ||
	  regexps[i].find('*') != std::string::npos ||
	  regexps[i].find('+') != std::string::npos ||
	  regexps[i].find('[') != std::string::npos ||
	  regexps[i].find('(') != std::string::npos) {
	show_subtotal = true;
	add_predicate = 1;
      } else {
	add_predicate = 2;
      }
    }
    else {
      add_predicate = 1;
    }

    if (i != 1 && command == "b" && account_regexp) {
      if (! show_related && ! show_all_related) {
	if (! display_predicate.empty())
	  display_predicate += "&";
	if (! show_empty)
	  display_predicate += "T&";

	if (add_predicate == 2)
	  display_predicate += "//";
	display_predicate += "/(?:";
	display_predicate += regexps[i];
	display_predicate += ")/";
      }
      else if (! show_empty) {
	if (! display_predicate.empty())
	  display_predicate += "&";
	display_predicate += "T";
      }
    }

    if (! account_regexp)
      predicate += "/";
    predicate += "/(?:";
    predicate += regexps[i];
    predicate += ")/";
  }
}

void report_t::process_options(const std::string&     command,
			       strings_list::iterator arg,
			       strings_list::iterator args_end)
{
  // Configure some other options depending on report type

  if (command == "p" || command == "e" || command == "w") {
    show_related     =
    show_all_related = true;
  }
  else if (command == "E") {
    show_subtotal = true;
  }
  else if (show_related) {
    if (command == "r") {
      show_inverted = true;
    } else {
      show_subtotal    = true;
      show_all_related = true;
    }
  }

  if (command != "b" && command != "r")
    amount_t::keep_base = true;

  // Process remaining command-line arguments

  if (command != "e") {
    // Treat the remaining command-line arguments as regular
    // expressions, used for refining report results.

    std::list<std::string>::iterator i = arg;
    for (; i != args_end; i++)
      if (*i == "--")
	break;

    if (i != arg)
      regexps_to_predicate(command, arg, i, true,
			   (command == "b" && ! show_subtotal &&
			    display_predicate.empty()));
    if (i != args_end && ++i != args_end)
      regexps_to_predicate(command, i, args_end);
  }

  // Setup the default value for the display predicate

  if (display_predicate.empty()) {
    if (command == "b") {
      if (! show_empty)
	display_predicate = "T";
      if (! show_subtotal) {
	if (! display_predicate.empty())
	  display_predicate += "&";
	display_predicate += "l<=1";
      }
    }
    else if (command == "E") {
      display_predicate = "t";
    }
    else if (command == "r" && ! show_empty) {
      display_predicate = "a";
    }
  }

  DEBUG_PRINT("ledger.config.predicates", "Predicate: " << predicate);
  DEBUG_PRINT("ledger.config.predicates", "Display P: " << display_predicate);

  // Setup the values of %t and %T, used in format strings

  if (! amount_expr.empty())
    ledger::amount_expr = amount_expr;
  if (! total_expr.empty())
    ledger::total_expr  = total_expr;

  // Now setup the various formatting strings

  if (! date_output_format.empty())
    date_t::output_format = date_output_format;

  amount_t::keep_price = keep_price;
  amount_t::keep_date  = keep_date;
  amount_t::keep_tag   = keep_tag;

  if (! report_period.empty() && ! sort_all)
    entry_sort = true;
}
#endif

xact_handler_ptr
report_t::chain_xact_handlers(xact_handler_ptr base_handler,
			      const bool handle_individual_xacts)
{
  bool remember_components = false;

  xact_handler_ptr handler(base_handler);

  // format_xacts write each xact received to the
  // output stream.
  if (handle_individual_xacts) {
    // truncate_entries cuts off a certain number of _entries_ from
    // being displayed.  It does not affect calculation.
    if (head_entries || tail_entries)
      handler.reset(new truncate_entries(handler, head_entries, tail_entries));

    // filter_xacts will only pass through xacts
    // matching the `display_predicate'.
    if (! display_predicate.empty())
      handler.reset(new filter_xacts(handler, display_predicate));

    // calc_xacts computes the running total.  When this
    // appears will determine, for example, whether filtered
    // xacts are included or excluded from the running total.
    handler.reset(new calc_xacts(handler));

    // component_xacts looks for reported xact that
    // match the given `descend_expr', and then reports the
    // xacts which made up the total for that reported
    // xact.
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
	handler.reset(new component_xacts(handler, *i));

      remember_components = true;
    }

    // reconcile_xacts will pass through only those
    // xacts which can be reconciled to a given balance
    // (calculated against the xacts which it receives).
    if (! reconcile_balance.empty()) {
      date_t cutoff = current_date;
      if (! reconcile_date.empty())
	cutoff = parse_date(reconcile_date);
      handler.reset(new reconcile_xacts
		    (handler, value_t(reconcile_balance), cutoff));
    }

    // filter_xacts will only pass through xacts
    // matching the `secondary_predicate'.
    if (! secondary_predicate.empty())
      handler.reset(new filter_xacts(handler, secondary_predicate));

    // sort_xacts will sort all the xacts it sees, based
    // on the `sort_order' value expression.
    if (! sort_string.empty()) {
      if (entry_sort)
	handler.reset(new sort_entries(handler, sort_string));
      else
	handler.reset(new sort_xacts(handler, sort_string));
    }

    // changed_value_xacts adds virtual xacts to the
    // list to account for changes in market value of commodities,
    // which otherwise would affect the running total unpredictably.
    if (show_revalued)
      handler.reset(new changed_value_xacts(handler, show_revalued_only));

    // collapse_xacts causes entries with multiple xacts
    // to appear as entries with a subtotaled xact for each
    // commodity used.
    if (show_collapsed)
      handler.reset(new collapse_xacts(handler));

    // subtotal_xacts combines all the xacts it receives
    // into one subtotal entry, which has one xact for each
    // commodity in each account.
    //
    // period_xacts is like subtotal_xacts, but it
    // subtotals according to time periods rather than totalling
    // everything.
    //
    // dow_xacts is like period_xacts, except that it
    // reports all the xacts that fall on each subsequent day
    // of the week.
    if (show_subtotal)
      handler.reset(new subtotal_xacts(handler, remember_components));

    if (days_of_the_week)
      handler.reset(new dow_xacts(handler, remember_components));
    else if (by_payee)
      handler.reset(new by_payee_xacts(handler, remember_components));

    // interval_xacts groups xacts together based on a
    // time period, such as weekly or monthly.
    if (! report_period.empty()) {
      handler.reset(new interval_xacts(handler, report_period,
					      remember_components));
      handler.reset(new sort_xacts(handler, "d"));
    }
  }

  // invert_xacts inverts the value of the xacts it
  // receives.
  if (show_inverted)
    handler.reset(new invert_xacts(handler));

  // related_xacts will pass along all xacts related
  // to the xact received.  If `show_all_related' is true,
  // then all the entry's xacts are passed; meaning that if
  // one xact of an entry is to be printed, all the
  // xact for that entry will be printed.
  if (show_related)
    handler.reset(new related_xacts(handler, show_all_related));

  // This filter_xacts will only pass through xacts
  // matching the `predicate'.
  if (! predicate.empty())
    handler.reset(new filter_xacts(handler, predicate));

#if 0
  // budget_xacts takes a set of xacts from a data
  // file and uses them to generate "budget xacts" which
  // balance against the reported xacts.
  //
  // forecast_xacts is a lot like budget_xacts, except
  // that it adds entries only for the future, and does not balance
  // them against anything but the future balance.

  if (budget_flags) {
    budget_xacts * budget_handler
      = new budget_xacts(handler, budget_flags);
    budget_handler->add_period_entries(journal->period_entries);
    handler.reset(budget_handler);

    // Apply this before the budget handler, so that only matching
    // xacts are calculated toward the budget.  The use of
    // filter_xacts above will further clean the results so
    // that no automated xacts that don't match the filter get
    // reported.
    if (! predicate.empty())
      handler.reset(new filter_xacts(handler, predicate));
  }
  else if (! forecast_limit.empty()) {
    forecast_xacts * forecast_handler
      = new forecast_xacts(handler, forecast_limit);
    forecast_handler->add_period_entries(journal->period_entries);
    handler.reset(forecast_handler;

    // See above, under budget_xacts.
    if (! predicate.empty())
      handler.reset(new filter_xacts(handler, predicate));
  }
#endif

  if (comm_as_payee)
    handler.reset(new set_comm_as_payee(handler));
  else if (code_as_payee)
    handler.reset(new set_code_as_payee(handler));

  return handler;
}

void report_t::xacts_report(xact_handler_ptr handler)
{
  session_xacts_iterator walker(session);
  pass_down_xacts(chain_xact_handlers(handler), walker);
  handler->flush();

  if (DO_VERIFY())
    session.clean_xacts();
}

void report_t::entry_report(xact_handler_ptr handler, entry_t& entry)
{
  entry_xacts_iterator walker(entry);
  pass_down_xacts(chain_xact_handlers(handler), walker);
  handler->flush();

  if (DO_VERIFY())
    session.clean_xacts(entry);
}

void report_t::sum_all_accounts()
{
  session_xacts_iterator walker(session);
  pass_down_xacts
    (chain_xact_handlers(xact_handler_ptr(new set_account_value), false),
     walker);
  // no flush() needed with set_account_value
  sum_accounts(*session.master);
}

void report_t::accounts_report(acct_handler_ptr	       handler,
			       const bool	       print_final_total,
			       optional<std::ostream&> ostream)
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
    
#if 0
  // jww (2008-08-02): I need access to the output formatter before this is
  // going to work.
  if (print_final_total) {
    assert(ostream);
    assert(account_has_xdata(*session.master));

    account_xdata_t& xdata(account_xdata(*session.master));
    if (! show_collapsed && xdata.total) {
      *ostream << "--------------------\n";
      xdata.value = xdata.total;
      handler->format.format(*ostream, *session.master);
    }
  }
#endif

  if (DO_VERIFY()) {
    session.clean_xacts();
    session.clean_accounts();
  }
}

void report_t::commodities_report(const string& format)
{
}

void report_t::entry_report(const entry_t& entry, const string& format)
{
}

#if 0
value_t report_t::abbrev(call_scope_t& args)
{
  if (args.size() < 2)
    throw_(usage_error, "usage: abbrev(STRING, WIDTH [, STYLE, ABBREV_LEN])");

  const var_t<string> str(args, 0);
  const var_t<long>   wid(args, 1);
  const var_t<long>   style(args, 2);
  const var_t<long>   abbrev_len(args, 3);

  return value_t(abbreviate(*str, *wid,
			    (style ?
			     static_cast<format_t::elision_style_t>(*style) :
			     session.elision_style),
			    true,
			    abbrev_len ? *abbrev_len : session.abbrev_len),
		 true);
}

value_t report_t::ftime(call_scope_t& args)
{
  if (args.size() < 1)
    throw_(std::logic_error, "usage: ftime(DATE [, DATE_FORMAT])");

  date_t date = args[0].as_date();

  string date_format;
  if (args.size() == 2)
    date_format = args[1].as_string();
  else
    date_format = moment_t::output_format;

  return string_value(date.as_string(date_format));
}
#endif

expr_t::ptr_op_t report_t::lookup(const string& name)
{
  const char * p = name.c_str();
  switch (*p) {
  case 'o':
    if (std::strncmp(p, "opt_", 4) == 0) {
      p = p + 4;
      switch (*p) {
      case 'a':
	if (std::strcmp(p, "amount") == 0)
	  return MAKE_FUNCTOR(report_t::option_amount);
	break;

      case 'b':
	if (std::strcmp(p, "bar") == 0)
	  return MAKE_FUNCTOR(report_t::option_bar);
	break;

      case 'f':
	if (std::strcmp(p, "format") == 0)
	  return MAKE_FUNCTOR(report_t::option_format);
	else if (name.find("fmt_") == 0) {
	  switch (name[4]) {
	  case 't':
	    return MAKE_FUNCTOR(report_t::get_amount_expr);
#if 0
	  case 'T':
	    return MAKE_FUNCTOR(report_t::get_total_expr);
#endif
	  }
	}
	break;

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

  return session.lookup(name);
}

// jww (2008-08-01): Find a home for this code

format_xacts::format_xacts(std::ostream& _output_stream,
					 const string& format)
  : output_stream(_output_stream), last_entry(NULL), last_xact(NULL)
{
  TRACE_CTOR(format_xacts, "std::ostream&, const string&");

  const char * f = format.c_str();
  if (const char * p = std::strstr(f, "%/")) {
    first_line_format.parse(string(f, 0, p - f));
    next_lines_format.parse(string(p + 2));
  } else {
    first_line_format.parse(format);
    next_lines_format.parse(format);
  }
}

void format_xacts::operator()(xact_t& xact)
{
  if (! xact.has_xdata() ||
      ! xact.xdata().has_flags(XACT_EXT_DISPLAYED)) {
    if (last_entry != xact.entry) {
      first_line_format.format(output_stream, xact);
      last_entry = xact.entry;
    }
    else if (last_xact && last_xact->date() != xact.date()) {
      first_line_format.format(output_stream, xact);
    }
    else {
      next_lines_format.format(output_stream, xact);
    }

    xact.xdata().add_flags(XACT_EXT_DISPLAYED);
    last_xact = &xact;
  }
}

void format_entries::format_last_entry()
{
  bool first = true;

  foreach (xact_t * xact, last_entry->xacts) {
    if (xact->has_xdata() &&
	xact->xdata().has_flags(XACT_EXT_TO_DISPLAY)) {
      if (first) {
	first_line_format.format(output_stream, *xact);
	first = false;
      } else {
	next_lines_format.format(output_stream, *xact);
      }
      xact->xdata().add_flags(XACT_EXT_DISPLAYED);
    }
  }
}

void format_entries::operator()(xact_t& xact)
{
  xact.xdata().add_flags(XACT_EXT_TO_DISPLAY);

  if (last_entry && xact.entry != last_entry)
    format_last_entry();

  last_entry = xact.entry;
}

void print_entry(std::ostream& out, const entry_base_t& entry_base,
		 const string& prefix)
{
  string print_format;

  if (dynamic_cast<const entry_t *>(&entry_base)) {
    print_format = (prefix + "%D %X%C%P\n" +
		    prefix + "    %-34A  %12o\n%/" +
		    prefix + "    %-34A  %12o\n");
  }
  else if (const auto_entry_t * entry =
	   dynamic_cast<const auto_entry_t *>(&entry_base)) {
    out << "= " << entry->predicate.predicate.text() << '\n';
    print_format = prefix + "    %-34A  %12o\n";
  }
  else if (const period_entry_t * entry =
	   dynamic_cast<const period_entry_t *>(&entry_base)) {
    out << "~ " << entry->period_string << '\n';
    print_format = prefix + "    %-34A  %12o\n";
  }
  else {
    assert(false);
  }

#if 0
  format_entries formatter(out, print_format);
  walk_xacts(const_cast<xacts_list&>(entry_base.xacts), formatter);
  formatter.flush();

  clear_xact_xdata cleaner;
  walk_xacts(const_cast<xacts_list&>(entry_base.xacts), cleaner);
#endif
}

bool disp_subaccounts_p(account_t&		   account,
			item_predicate<account_t>& disp_pred,
			account_t *&		   to_show)
{
  bool	       display  = false;
  unsigned int counted  = 0;
  bool         matches  = disp_pred(account);
  bool         computed = false;
  value_t      acct_total;
  value_t      result;

  to_show = NULL;

  foreach (accounts_map::value_type pair, account.accounts) {
    if (! disp_pred(*pair.second))
      continue;

#if 0
    compute_total(result, *pair.second);
#endif
    if (! computed) {
#if 0
      compute_total(acct_total, account);
#endif
      computed = true;
    }

    if ((result != acct_total) || counted > 0) {
      display = matches;
      break;
    }
    to_show = pair.second;
    counted++;
  }

  return display;
}

bool display_account(account_t& account, item_predicate<account_t>& disp_pred)
{
  // Never display an account that has already been displayed.
  if (account_has_xdata(account) &&
      account_xdata_(account).dflags & ACCOUNT_DISPLAYED)
    return false;

  // At this point, one of two possibilities exists: the account is a
  // leaf which matches the predicate restrictions; or it is a parent
  // and two or more children must be subtotaled; or it is a parent
  // and its child has been hidden by the predicate.  So first,
  // determine if it is a parent that must be displayed regardless of
  // the predicate.

  account_t * account_to_show = NULL;
  if (disp_subaccounts_p(account, disp_pred, account_to_show))
    return true;

  return ! account_to_show && disp_pred(account);
}

void format_accounts::operator()(account_t& account)
{
  if (display_account(account, disp_pred)) {
    if (! account.parent) {
      account_xdata(account).dflags |= ACCOUNT_TO_DISPLAY;
    } else {
      format.format(output_stream, account);
      account_xdata(account).dflags |= ACCOUNT_DISPLAYED;
    }
  }
}

format_equity::format_equity(std::ostream& _output_stream,
			     const string& _format,
			     const string& display_predicate)
  : output_stream(_output_stream), disp_pred(display_predicate)
{
  const char * f = _format.c_str();
  if (const char * p = std::strstr(f, "%/")) {
    first_line_format.parse(string(f, 0, p - f));
    next_lines_format.parse(string(p + 2));
  } else {
    first_line_format.parse(_format);
    next_lines_format.parse(_format);
  }

  entry_t header_entry;
  header_entry.payee = "Opening Balances";
  header_entry._date = current_date;
  first_line_format.format(output_stream, header_entry);
}

void format_equity::flush()
{
  account_xdata_t xdata;
  xdata.value = total;
  xdata.value.negate();
  account_t summary(NULL, "Equity:Opening Balances");
  summary.data = &xdata;

  if (total.type() >= value_t::BALANCE) {
    const balance_t * bal;
    if (total.is_type(value_t::BALANCE))
      bal = &(total.as_balance());
    else if (total.is_type(value_t::BALANCE_PAIR))
      bal = &(total.as_balance_pair().quantity());
    else
      assert(false);

    foreach (balance_t::amounts_map::value_type pair, bal->amounts) {
      xdata.value = pair.second;
      xdata.value.negate();
      next_lines_format.format(output_stream, summary);
    }
  } else {
    next_lines_format.format(output_stream, summary);
  }
  output_stream.flush();
}

void format_equity::operator()(account_t& account)
{
  if (display_account(account, disp_pred)) {
    if (account_has_xdata(account)) {
      value_t val = account_xdata_(account).value;

      if (val.type() >= value_t::BALANCE) {
	const balance_t * bal;
	if (val.is_type(value_t::BALANCE))
	  bal = &(val.as_balance());
	else if (val.is_type(value_t::BALANCE_PAIR))
	  bal = &(val.as_balance_pair().quantity());
	else
	  assert(false);

	foreach (balance_t::amounts_map::value_type pair, bal->amounts) {
	  account_xdata_(account).value = pair.second;
	  next_lines_format.format(output_stream, account);
	}
	account_xdata_(account).value = val;
      } else {
	next_lines_format.format(output_stream, account);
      }
      total += val;
    }
    account_xdata(account).dflags |= ACCOUNT_DISPLAYED;
  }
}

} // namespace ledger
