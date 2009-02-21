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

format_xacts::format_xacts(report_t&	 _report,
			   const string& format,
			   bool		 _print_raw)
  : report(_report), last_entry(NULL), last_xact(NULL),
    print_raw(_print_raw)
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

  if (print_raw) {
    if (! xact.has_xdata() ||
	! xact.xdata().has_flags(XACT_EXT_DISPLAYED)) {
      if (last_entry != xact.entry) {
	if (last_entry) {
	  bind_scope_t entry_scope(report, *last_entry);
	  between_format.format(out, entry_scope);
	}
	print_item(out, *xact.entry);
	out << '\n';
	last_entry = xact.entry;
      }
      xact.xdata().add_flags(XACT_EXT_DISPLAYED);
      last_xact = &xact;
    }
  }
  else if (! xact.has_xdata() ||
	   ! xact.xdata().has_flags(XACT_EXT_DISPLAYED)) {
    bind_scope_t bound_scope(report, xact);
    if (last_entry != xact.entry) {
      if (last_entry) {
	bind_scope_t entry_scope(report, *last_entry);
	between_format.format(out, entry_scope);
      }
      first_line_format.format(out, bound_scope);
      last_entry = xact.entry;
    }
    else if (last_xact && last_xact->date() != xact.date()) {
      first_line_format.format(out, bound_scope);
    }
    else {
      next_lines_format.format(out, bound_scope);
    }

    xact.xdata().add_flags(XACT_EXT_DISPLAYED);
    last_xact = &xact;
  }
}

void gather_statistics::flush()
{
  std::ostream& out(report.output_stream);

  out << "Time period: " << statistics.earliest_xact << " to "
      << statistics.latest_xact << std::endl << std::endl;

  out << "  Files these transactions came from:" << std::endl;

  foreach (const path& pathname, statistics.filenames)
    if (! pathname.empty())
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
  out << (double((statistics.latest_xact - statistics.earliest_xact).days()) /
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

void format_accounts::post_account(account_t& account)
{
  bind_scope_t bound_scope(report, account);
  bool	       format_account = false;

  DEBUG("account.display", "Should we display " << account.fullname());

  if (account.has_flags(ACCOUNT_EXT_MATCHING) ||
      (! report.HANDLED(flat) &&
       account.children_with_flags(ACCOUNT_EXT_MATCHING) > 1)) {
    DEBUG("account.display", "  Yes, because it matched");
    format_account = true;
  }
  else if (! report.HANDLED(flat) &&
	   account.children_with_flags(ACCOUNT_EXT_VISITED) &&
	   ! account.children_with_flags(ACCOUNT_EXT_MATCHING)) {
    DEBUG("account.display",
	  "  Maybe, because it has visited, but no matching, children");
    if (disp_pred(bound_scope)) {
      DEBUG("account.display",
	    "    And yes, because it matches the display predicate");
      format_account = true;
    } else {
      DEBUG("account.display",
	    "    And no, because it didn't match the display predicate");
    }
  }
  else {
    DEBUG("account.display",
	  "  No, neither it nor its children were eligible for display");
  }

  if (format_account) {
    account.xdata().add_flags(ACCOUNT_EXT_DISPLAYED);
    format.format(report.output_stream, bound_scope);
  }
}

void format_accounts::flush()
{
  std::ostream& out(report.output_stream);

  std::size_t top_displayed = 0;

  foreach (account_t * account, posted_accounts) {
    post_account(*account);

    if (report.HANDLED(flat) && account->has_flags(ACCOUNT_EXT_DISPLAYED))
      top_displayed++;
  }

  if (! report.HANDLED(flat)) {
    foreach (accounts_map::value_type pair, report.session.master->accounts) {
      if (pair.second->has_flags(ACCOUNT_EXT_DISPLAYED) ||
	  pair.second->children_with_flags(ACCOUNT_EXT_DISPLAYED))
	top_displayed++;
    }
  }

  assert(report.session.master->has_xdata());
  account_t::xdata_t& xdata(report.session.master->xdata());

  if (! report.HANDLED(no_total) && top_displayed > 1 && xdata.total) {
    out << "--------------------\n";
    xdata.value = xdata.total;
    bind_scope_t bound_scope(report, *report.session.master);
    format.format(out, bound_scope);
  }

  out.flush();
}

void format_accounts::operator()(account_t& account)
{
  DEBUG("account.display",
	"Proposing to format account: " << account.fullname());

  if (account.has_flags(ACCOUNT_EXT_VISITED)) {
    DEBUG("account.display",
	  "  Account or its children visited by sum_all_accounts");

    bind_scope_t bound_scope(report, account);
    if (disp_pred(bound_scope)) {
      DEBUG("account.display",
	    "  And the account matched the display predicate");
      account.xdata().add_flags(ACCOUNT_EXT_MATCHING);
    } else {
      DEBUG("account.display",
	    "  But it did not match the display predicate");
    }
  }
  posted_accounts.push_back(&account);
}

} // namespace ledger
