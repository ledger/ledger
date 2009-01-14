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
  std::ostream& out(*report.output_stream);

  if (! xact.has_xdata() ||
      ! xact.xdata().has_flags(XACT_EXT_DISPLAYED)) {
    if (last_entry != xact.entry) {
      if (last_entry)
	between_format.format(out, *last_entry);
      first_line_format.format(out, xact);
      last_entry = xact.entry;
    }
    else if (last_xact && last_xact->date() != xact.date()) {
      first_line_format.format(out, xact);
    }
    else {
      next_lines_format.format(out, xact);
    }

    xact.xdata().add_flags(XACT_EXT_DISPLAYED);
    last_xact = &xact;
  }
}

void format_entries::format_last_entry()
{
  bool		first = true;
  std::ostream& out(*report.output_stream);

  foreach (xact_t * xact, last_entry->xacts) {
    if (xact->has_xdata() &&
	xact->xdata().has_flags(XACT_EXT_TO_DISPLAY)) {
      if (first) {
	first_line_format.format(out, *xact);
	first = false;
      } else {
	next_lines_format.format(out, *xact);
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

void format_accounts::flush()
{
  std::ostream& out(*report.output_stream);

  if (print_final_total) {
    assert(out);
    assert(report.session.master->has_xdata());

    account_t::xdata_t& xdata(report.session.master->xdata());

    if (! report.show_collapsed && xdata.total) {
      out << "--------------------\n";
      xdata.value = xdata.total;
      format.format(out, *report.session.master);
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
      format.format(*report.output_stream, account);
      account.xdata().add_flags(ACCOUNT_EXT_DISPLAYED);
    }
  }
}

bool format_accounts::disp_subaccounts_p(account_t&   account,
					 account_t *& to_show)
{
  bool	       display  = false;
  unsigned int counted  = 0;
  bool         matches  = should_display(account);
  bool         computed = false;
  value_t      acct_total;
  value_t      result;

  to_show = NULL;

  foreach (accounts_map::value_type pair, account.accounts) {
    if (! should_display(*pair.second))
      continue;

    call_scope_t args(*pair.second);
    result = report.get_total_expr(args);
    if (! computed) {
      call_scope_t args(account);
      acct_total = report.get_total_expr(args);
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
  header_entry._date = current_date;
  first_line_format.format(*report.output_stream, header_entry);
}

void format_equity::flush()
{
  account_t summary(NULL, "Equity:Opening Balances");

  account_t::xdata_t& xdata(summary.xdata());
  std::ostream&	      out(*report.output_stream);
  
  xdata.value = total.negate();

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
      next_lines_format.format(out, summary);
    }
  } else {
    next_lines_format.format(out, summary);
  }
  out.flush();
}

void format_equity::operator()(account_t& account)
{
  std::ostream& out(*report.output_stream);

  if (display_account(account)) {
    if (account.has_xdata()) {
      value_t val = account.xdata().value;

      if (val.type() >= value_t::BALANCE) {
	const balance_t * bal;
	if (val.is_type(value_t::BALANCE))
	  bal = &(val.as_balance());
	else if (val.is_type(value_t::BALANCE_PAIR))
	  bal = &(val.as_balance_pair().quantity());
	else
	  assert(false);

	foreach (balance_t::amounts_map::value_type pair, bal->amounts) {
	  account.xdata().value = pair.second;
	  next_lines_format.format(out, account);
	}
	account.xdata().value = val;
      } else {
	next_lines_format.format(out, account);
      }
      total += val;
    }
    account.xdata().add_flags(ACCOUNT_EXT_DISPLAYED);
  }
}

} // namespace ledger
