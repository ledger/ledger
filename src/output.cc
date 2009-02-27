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

format_posts::format_posts(report_t&	 _report,
			   const string& format,
			   bool		 _print_raw)
  : report(_report), last_xact(NULL), last_post(NULL),
    print_raw(_print_raw)
{
  TRACE_CTOR(format_posts, "report&, const string&");

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

void format_posts::operator()(post_t& post)
{
  std::ostream& out(report.output_stream);

  if (print_raw) {
    if (! post.has_xdata() ||
	! post.xdata().has_flags(POST_EXT_DISPLAYED)) {
      if (last_xact != post.xact) {
	if (last_xact) {
	  bind_scope_t xact_scope(report, *last_xact);
	  between_format.format(out, xact_scope);
	}
	print_item(out, *post.xact);
	out << '\n';
	last_xact = post.xact;
      }
      post.xdata().add_flags(POST_EXT_DISPLAYED);
      last_post = &post;
    }
  }
  else if (! post.has_xdata() ||
	   ! post.xdata().has_flags(POST_EXT_DISPLAYED)) {
    bind_scope_t bound_scope(report, post);
    if (last_xact != post.xact) {
      if (last_xact) {
	bind_scope_t xact_scope(report, *last_xact);
	between_format.format(out, xact_scope);
      }
      first_line_format.format(out, bound_scope);
      last_xact = post.xact;
    }
    else if (last_post && last_post->date() != post.date()) {
      first_line_format.format(out, bound_scope);
    }
    else {
      next_lines_format.format(out, bound_scope);
    }

    post.xdata().add_flags(POST_EXT_DISPLAYED);
    last_post = &post;
  }
}

void gather_statistics::flush()
{
  std::ostream& out(report.output_stream);

  {
    straccstream accum;
    out << ACCUM(accum << "Time period: %1 to %2" << statistics.earliest_post
		 << statistics.latest_post) << std::endl << std::endl;
  }

  out << _("  Files these postings came from:") << std::endl;

  foreach (const path& pathname, statistics.filenames)
    if (! pathname.empty())
      out << "    " << pathname.string() << std::endl;
  out << std::endl;

  out << _("  Unique payees:          ");
  out.width(8);
  out << std::right << statistics.payees_referenced.size() << std::endl;

  out << _("  Unique accounts:        ");
  out.width(8);
  out << std::right << statistics.accounts_referenced.size() << std::endl;

  out << _("  Number of transactions: ") ;
  out.width(8);
  out << std::right << statistics.total_xacts << std::endl;

  out << _("  Number of postings:     ");
  out.width(8);
  out << std::right << statistics.total_posts;

  out << " (";
  out.precision(2);
  out << (double((statistics.latest_post - statistics.earliest_post).days()) /
	  double(statistics.total_posts)) << _(" per day)") << std::endl;

  out << _("  Days since last post:   ");
  out.width(8);
  out << std::right << (CURRENT_DATE() - statistics.latest_post).days()
      << std::endl;

  out << _("  Posts in last 7 days:   ");
  out.width(8);
  out << std::right << statistics.total_last_7_days << std::endl;
  out << _("  Posts in last 30 days:  ");
  out.width(8);
  out << std::right << statistics.total_last_30_days << std::endl;
  out << _("  Posts seen this month:  ");
  out.width(8);
  out << std::right << statistics.total_this_month << std::endl;

  out << _("  Uncleared postings:     ");
  out.width(8);
  out << std::right << statistics.total_uncleared_posts << std::endl;

  out.flush();
}

void gather_statistics::operator()(post_t& post)
{
  if (last_xact != post.xact) {
    statistics.total_xacts++;
    last_xact = post.xact;
  }
  if (last_post != &post) {
    statistics.total_posts++;
    last_post = &post;

    statistics.filenames.insert(post.pathname);

    date_t date = post.date();

    if (date.year() == CURRENT_DATE().year() &&
	date.month() == CURRENT_DATE().month())
      statistics.total_this_month++;

    if ((CURRENT_DATE() - date).days() <= 30)
      statistics.total_last_30_days++;
    if ((CURRENT_DATE() - date).days() <= 7)
      statistics.total_last_7_days++;

    if (post.state() != item_t::CLEARED)
      statistics.total_uncleared_posts++;

    if (! is_valid(statistics.earliest_post) ||
	post.date() < statistics.earliest_post)
      statistics.earliest_post = post.date();
    if (! is_valid(statistics.latest_post) ||
	post.date() > statistics.latest_post)
      statistics.latest_post = post.date();

    statistics.accounts_referenced.insert(post.account->fullname());
    statistics.payees_referenced.insert(post.xact->payee);
  }
}

format_accounts::format_accounts(report_t&     _report,
				 const string& format)
  : report(_report), disp_pred()
{
  TRACE_CTOR(format_accounts, "report&, const string&");

  if (report.HANDLED(display_)) {
    DEBUG("account.display",
	  "Account display predicate: " << report.HANDLER(display_).str());
    disp_pred.predicate.parse(report.HANDLER(display_).str());
  }

  const char * f = format.c_str();

  if (const char * p = std::strstr(f, "%/")) {
    account_line_format.parse(string(f, 0, p - f));
    const char * n = p + 2;
    if (const char * p = std::strstr(n, "%/")) {
      total_line_format.parse(string(n, 0, p - n));
      separator_format.parse(string(p + 2));
    } else {
      total_line_format.parse(n);
    }
  } else {
    account_line_format.parse(format);
    total_line_format.parse(format);
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
    account_line_format.format(report.output_stream, bound_scope);
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
    xdata.value = xdata.total;
    bind_scope_t bound_scope(report, *report.session.master);
    separator_format.format(out, bound_scope);
    total_line_format.format(out, bound_scope);
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
