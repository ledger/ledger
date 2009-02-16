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

#include "output.h"

namespace ledger {

format_xacts::format_xacts(report_t& _report, const string& format)
  : report(_report), last_entry(NULL), last_xact(NULL)
{
  TRACE_CTOR(format_xacts, "report&, const string&");

  const char * f = format.c_str();

  if (const char * p = std::strstr(f, "%/")) {
    first_line_format.parse(string(f, 0, p - f));
    const char * n = p + 2;
    if (const char * p = std::strstr(n, "%/")) {
      next_lines_format.parse(string(n, 0, p - n));
      between_format.parse(string(p + 2));
    } else {
      next_lines_format.parse(n);
    }
  } else {
    first_line_format.parse(format);
    next_lines_format.parse(format);
  }
}

void format_xacts::operator()(xact_t& xact)
{
  std::ostream& out(report.output_stream);

  if (! xact.has_xdata() ||
      ! xact.xdata().has_flags(XACT_EXT_DISPLAYED)) {
    if (last_entry != xact.entry) {
      if (last_entry) {
	bind_scope_t bound_scope(report, *last_entry);
	between_format.format(out, bound_scope);
      }
      bind_scope_t bound_scope(report, xact);
      first_line_format.format(out, bound_scope);
      last_entry = xact.entry;
    }
    else if (last_xact && last_xact->date() != xact.date()) {
      bind_scope_t bound_scope(report, xact);
      first_line_format.format(out, bound_scope);
    }
    else {
      bind_scope_t bound_scope(report, xact);
      next_lines_format.format(out, bound_scope);
    }

    xact.xdata().add_flags(XACT_EXT_DISPLAYED);
    last_xact = &xact;
  }
}

void gather_statistics::flush()
{
  std::ostream& out(report.output_stream);

  out << "Statistics gathered for this report:" << std::endl;

  out << std::endl << "  Time period: "
      << statistics.earliest_xact << " to "
      << statistics.latest_xact << std::endl << std::endl;

  out << "  Files these transactions came from:" << std::endl;

  bool first = true;
  foreach (const path& pathname, statistics.filenames)
    out << "    " << pathname.string() << std::endl;
  out << std::endl;

  out << "  Unique payees:          ";
  out.width(8);
  out << std::right << statistics.payees_referenced.size() << std::endl;

  out << "  Unique accounts:        ";
  out.width(8);
  out << std::right << statistics.accounts_referenced.size() << std::endl;

  out << "  Number of entries:      " ;
  out.width(8);
  out << std::right << statistics.total_entries << std::endl;

  out << "  Number of transactions: ";
  out.width(8);
  out << std::right << statistics.total_xacts;

  out << " (";
  out.precision(2);
  out << (double(statistics.unique_dates.size()) /
	  double(statistics.total_xacts)) << " per day)" << std::endl;

  out << "  Days since last xact:   ";
  out.width(8);
  out << std::right << (CURRENT_DATE() - statistics.latest_xact).days()
      << std::endl;

  out << "  Xacts in last 7 days:   ";
  out.width(8);
  out << std::right << statistics.total_last_7_days << std::endl;
  out << "  Xacts in last 30 days:  ";
  out.width(8);
  out << std::right << statistics.total_last_30_days << std::endl;
  out << "  Xacts seen this month:  ";
  out.width(8);
  out << std::right << statistics.total_this_month << std::endl;

  out << "  Uncleared transactions: ";
  out.width(8);
  out << std::right << statistics.total_uncleared_xacts << std::endl;

  out.flush();
}

void gather_statistics::operator()(xact_t& xact)
{
  if (last_entry != xact.entry) {
    statistics.total_entries++;
    last_entry = xact.entry;
  }
  if (last_xact != &xact) {
    statistics.total_xacts++;
    last_xact = &xact;

    statistics.filenames.insert(xact.pathname);

    date_t date = xact.reported_date();

    statistics.unique_dates.insert(date);

    if (date.year() == CURRENT_DATE().year() &&
	date.month() == CURRENT_DATE().month())
      statistics.total_this_month++;

    if ((CURRENT_DATE() - date).days() <= 30)
      statistics.total_last_30_days++;
    if ((CURRENT_DATE() - date).days() <= 7)
      statistics.total_last_7_days++;

    if (xact.state() != item_t::CLEARED)
      statistics.total_uncleared_xacts++;

    if (! is_valid(statistics.earliest_xact) ||
	xact.reported_date() < statistics.earliest_xact)
      statistics.earliest_xact = xact.reported_date();
    if (! is_valid(statistics.latest_xact) ||
	xact.reported_date() > statistics.latest_xact)
      statistics.latest_xact = xact.reported_date();

    statistics.accounts_referenced.insert(xact.account->fullname());
    statistics.payees_referenced.insert(xact.entry->payee);
  }
}

void format_entries::format_last_entry()
{
  bool		first = true;
  std::ostream& out(report.output_stream);

  foreach (xact_t * xact, last_entry->xacts) {
    if (xact->has_xdata() &&
	xact->xdata().has_flags(XACT_EXT_TO_DISPLAY)) {
      if (first) {
	bind_scope_t bound_scope(report, *xact);
	first_line_format.format(out, bound_scope);
	first = false;
      } else {
	bind_scope_t bound_scope(report, *xact);
	next_lines_format.format(out, bound_scope);
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

void format_accounts::flush()
{
  std::ostream& out(report.output_stream);

  if (print_final_total) {
    assert(out);
    assert(report.session.master->has_xdata());

    account_t::xdata_t& xdata(report.session.master->xdata());

    if (! report.HANDLED(collapse) && xdata.total) {
      out << "--------------------\n";
      xdata.value = xdata.total;
      bind_scope_t bound_scope(report, *report.session.master);
      format.format(out, bound_scope);
    }
  }

  out.flush();
}

void format_accounts::operator()(account_t& account)
{
  if (display_account(account)) {
    if (! account.parent) {
      account.xdata().add_flags(ACCOUNT_EXT_TO_DISPLAY);
    } else {
      bind_scope_t bound_scope(report, account);
      format.format(report.output_stream, bound_scope);
      account.xdata().add_flags(ACCOUNT_EXT_DISPLAYED);
    }
  }
}

bool format_accounts::disp_subaccounts_p(account_t&   account,
					 account_t *& to_show)
{
  bool	      display  = false;
  std::size_t counted  = 0;
  bool        matches  = should_display(account);
  bool        computed = false;
  value_t     acct_total;
  value_t     result;

  to_show = NULL;

  foreach (accounts_map::value_type pair, account.accounts) {
    if (! should_display(*pair.second))
      continue;

    bind_scope_t bound_scope(report, *pair.second);
    call_scope_t args(bound_scope);
    result = report.fn_total_expr(args);
    if (! computed) {
      bind_scope_t account_scope(report, account);
      call_scope_t args(account_scope);
      acct_total = report.fn_total_expr(args);
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

bool format_accounts::display_account(account_t& account)
{
  // Never display an account that has already been displayed.
  if (account.has_xdata() &&
      account.xdata().has_flags(ACCOUNT_EXT_DISPLAYED))
    return false;

  // At this point, one of two possibilities exists: the account is a
  // leaf which matches the predicate restrictions; or it is a parent
  // and two or more children must be subtotaled; or it is a parent
  // and its child has been hidden by the predicate.  So first,
  // determine if it is a parent that must be displayed regardless of
  // the predicate.

  account_t * account_to_show = NULL;
  if (disp_subaccounts_p(account, account_to_show))
    return true;

  return ! account_to_show && should_display(account);
}

format_equity::format_equity(report_t& _report, const string& _format)
  : format_accounts(_report)
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
  header_entry._date = CURRENT_DATE();
  bind_scope_t bound_scope(report, header_entry);
  first_line_format.format(report.output_stream, bound_scope);
}

void format_equity::flush()
{
  account_t summary(NULL, "Equity:Opening Balances");

  account_t::xdata_t& xdata(summary.xdata());
  std::ostream&	      out(report.output_stream);
  
  xdata.value = total.negate();

  if (total.type() >= value_t::BALANCE) {
    const balance_t * bal;
    if (total.is_type(value_t::BALANCE))
      bal = &(total.as_balance());
    else
      assert(false);

    foreach (balance_t::amounts_map::value_type pair, bal->amounts) {
      xdata.value = pair.second;
      xdata.value.negate();
      bind_scope_t bound_scope(report, summary);
      next_lines_format.format(out, bound_scope);
    }
  } else {
    bind_scope_t bound_scope(report, summary);
    next_lines_format.format(out, bound_scope);
  }
  out.flush();
}

void format_equity::operator()(account_t& account)
{
  std::ostream& out(report.output_stream);

  if (display_account(account)) {
    if (account.has_xdata()) {
      value_t val = account.xdata().value;

      if (val.type() >= value_t::BALANCE) {
	const balance_t * bal;
	if (val.is_type(value_t::BALANCE))
	  bal = &(val.as_balance());
	else
	  assert(false);

	foreach (balance_t::amounts_map::value_type pair, bal->amounts) {
	  account.xdata().value = pair.second;
	  bind_scope_t bound_scope(report, account);
	  next_lines_format.format(out, bound_scope);
	}
	account.xdata().value = val;
      } else {
	bind_scope_t bound_scope(report, account);
	next_lines_format.format(out, bound_scope);
      }
      total += val;
    }
    account.xdata().add_flags(ACCOUNT_EXT_DISPLAYED);
  }
}

} // namespace ledger
