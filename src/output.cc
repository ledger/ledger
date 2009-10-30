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

#include <system.hh>

#include "output.h"
#include "xact.h"
#include "post.h"
#include "account.h"
#include "session.h"
#include "report.h"

namespace ledger {

format_posts::format_posts(report_t&	 _report,
			   const string& format,
			   bool		 _print_raw)
  : report(_report), last_xact(NULL), last_post(NULL),
    print_raw(_print_raw)
{
  TRACE_CTOR(format_posts, "report&, const string&, bool");

  const char * f = format.c_str();

  if (const char * p = std::strstr(f, "%/")) {
    first_line_format.parse(string(f, 0, p - f));
    const char * n = p + 2;
    if (const char * p = std::strstr(n, "%/")) {
      next_lines_format.parse(string(n, 0, p - n), first_line_format);
      between_format.parse(string(p + 2), first_line_format);
    } else {
      next_lines_format.parse(n, first_line_format);
    }
  } else {
    first_line_format.parse(format);
    next_lines_format.parse(format);
  }
}

void format_posts::flush()
{
  report.output_stream.flush();
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

format_accounts::format_accounts(report_t&     _report,
				 const string& format)
  : report(_report), disp_pred()
{
  TRACE_CTOR(format_accounts, "report&, const string&");

  const char * f = format.c_str();

  if (const char * p = std::strstr(f, "%/")) {
    account_line_format.parse(string(f, 0, p - f));
    const char * n = p + 2;
    if (const char * p = std::strstr(n, "%/")) {
      total_line_format.parse(string(n, 0, p - n), account_line_format);
      separator_format.parse(string(p + 2), account_line_format);
    } else {
      total_line_format.parse(n, account_line_format);
    }
  } else {
    account_line_format.parse(format);
    total_line_format.parse(format, account_line_format);
  }
}

std::size_t format_accounts::post_account(account_t& account, const bool flat)
{
  if (account.xdata().has_flags(ACCOUNT_EXT_TO_DISPLAY) &&
      ! account.xdata().has_flags(ACCOUNT_EXT_DISPLAYED)) {
    if (! flat && account.parent &&
	account.parent->xdata().has_flags(ACCOUNT_EXT_TO_DISPLAY) &&
	! account.parent->xdata().has_flags(ACCOUNT_EXT_DISPLAYED))
      post_account(*account.parent, flat);

    account.xdata().add_flags(ACCOUNT_EXT_DISPLAYED);

    bind_scope_t bound_scope(report, account);
    account_line_format.format(report.output_stream, bound_scope);

    return 1;
  }
  return 0;
}

std::pair<std::size_t, std::size_t>
format_accounts::mark_accounts(account_t& account, const bool flat)
{
  std::size_t visited	 = 0;
  std::size_t to_display = 0;

  foreach (accounts_map::value_type& pair, account.accounts) {
    std::pair<std::size_t, std::size_t> i = mark_accounts(*pair.second, flat);
    visited    += i.first;
    to_display += i.second;
  }

#if defined(DEBUG_ON)
  DEBUG("account.display", "Considering account: " << account.fullname());
  if (account.has_xflags(ACCOUNT_EXT_VISITED))
    DEBUG("account.display", "  it was visited itself");
  DEBUG("account.display", "  it has " << visited << " visited children");
  DEBUG("account.display",
	"  it has " << to_display << " children to display");
#endif

  if (account.parent &&
      (account.has_xflags(ACCOUNT_EXT_VISITED) || (! flat && visited > 0))) {
    bind_scope_t bound_scope(report, account);
    if ((! flat && to_display > 1) ||
	((flat || to_display != 1 ||
	  account.has_xflags(ACCOUNT_EXT_VISITED)) &&
	 disp_pred(bound_scope))) {
      account.xdata().add_flags(ACCOUNT_EXT_TO_DISPLAY);
      DEBUG("account.display", "Marking account as TO_DISPLAY");
      to_display = 1;
    }
    visited = 1;
  }

  return std::pair<std::size_t, std::size_t>(visited, to_display);
}

void format_accounts::flush()
{
  std::ostream& out(report.output_stream);

  if (report.HANDLED(display_)) {
    DEBUG("account.display",
	  "Account display predicate: " << report.HANDLER(display_).str());
    disp_pred.predicate.parse(report.HANDLER(display_).str());
  }

  mark_accounts(*report.session.journal->master, report.HANDLED(flat));

  std::size_t displayed = 0;

  foreach (account_t * account, posted_accounts)
    displayed += post_account(*account, report.HANDLED(flat));

  if (displayed > 1 &&
      ! report.HANDLED(no_total) && ! report.HANDLED(percent)) {
    bind_scope_t bound_scope(report, *report.session.journal->master);
    separator_format.format(out, bound_scope);
    total_line_format.format(out, bound_scope);
  }

  out.flush();
}

void format_accounts::operator()(account_t& account)
{
  DEBUG("account.display", "Posting account: " << account.fullname());
  posted_accounts.push_back(&account);
}

} // namespace ledger
