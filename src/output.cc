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

#include "output.h"
#include "xact.h"
#include "post.h"
#include "account.h"
#include "session.h"
#include "report.h"

namespace ledger {

format_posts::format_posts(report_t&               _report,
                           const string&           format,
                           const optional<string>& _prepend_format,
                           std::size_t             _prepend_width)
  : report(_report), prepend_width(_prepend_width),
    last_xact(NULL), last_post(NULL), first_report_title(true)
{
  const char * f = format.c_str();

  if (const char * p = std::strstr(f, "%/")) {
    first_line_format.parse_format
      (string(f, 0, static_cast<std::string::size_type>(p - f)));
    const char * n = p + 2;
    if (const char * pp = std::strstr(n, "%/")) {
      next_lines_format.parse_format
        (string(n, 0, static_cast<std::string::size_type>(pp - n)),
         first_line_format);
      between_format.parse_format(string(pp + 2), first_line_format);
    } else {
      next_lines_format.parse_format(string(n), first_line_format);
    }
  } else {
    first_line_format.parse_format(format);
    next_lines_format.parse_format(format);
  }

  if (_prepend_format)
    prepend_format.parse_format(*_prepend_format);

  TRACE_CTOR(format_posts, "report&, const string&, bool");
}

void format_posts::flush()
{
  report.output_stream.flush();
}

void format_posts::operator()(post_t& post)
{
  if (! post.has_xdata() ||
      ! post.xdata().has_flags(POST_EXT_DISPLAYED)) {
    std::ostream& out(report.output_stream);

    bind_scope_t bound_scope(report, post);

    if (! report_title.empty()) {
      if (first_report_title)
        first_report_title = false;
      else
        out << '\n';

      value_scope_t val_scope(bound_scope, string_value(report_title));
      format_t group_title_format(report.HANDLER(group_title_format_).str());

      out << group_title_format(val_scope);

      report_title = "";
    }

    if (prepend_format) {
      out.width(static_cast<std::streamsize>(prepend_width));
      out << prepend_format(bound_scope);
    }

    if (last_xact != post.xact) {
      if (last_xact) {
        bind_scope_t xact_scope(report, *last_xact);
        out << between_format(xact_scope);
      }
      out << first_line_format(bound_scope);
      last_xact = post.xact;
    }
    else if (last_post && last_post->date() != post.date()) {
      out << first_line_format(bound_scope);
    }
    else {
      out << next_lines_format(bound_scope);
    }

    post.xdata().add_flags(POST_EXT_DISPLAYED);
    last_post = &post;
  }
}

format_accounts::format_accounts(report_t&               _report,
                                 const string&           format,
                                 const optional<string>& _prepend_format,
                                 std::size_t             _prepend_width)
  : report(_report), prepend_width(_prepend_width), disp_pred(),
    first_report_title(true)
{
  const char * f = format.c_str();

  if (const char * p = std::strstr(f, "%/")) {
    account_line_format.parse_format
      (string(f, 0, static_cast<std::string::size_type>(p - f)));
    const char * n = p + 2;
    if (const char * pp = std::strstr(n, "%/")) {
      total_line_format.parse_format
        (string(n, 0, static_cast<std::string::size_type>(pp - n)),
         account_line_format);
      separator_format.parse_format(string(pp + 2), account_line_format);
    } else {
      total_line_format.parse_format(n, account_line_format);
    }
  } else {
    account_line_format.parse_format(format);
    total_line_format.parse_format(format, account_line_format);
  }

  if (_prepend_format)
    prepend_format.parse_format(*_prepend_format);

  TRACE_CTOR(format_accounts, "report&, const string&");
}

std::size_t format_accounts::post_account(account_t& account, const bool flat)
{
  if (! flat && account.parent)
    post_account(*account.parent, flat);

  if (account.xdata().has_flags(ACCOUNT_EXT_TO_DISPLAY) &&
      ! account.xdata().has_flags(ACCOUNT_EXT_DISPLAYED)) {
    std::ostream& out(report.output_stream);

    DEBUG("account.display", "Displaying account: " << account.fullname());
    account.xdata().add_flags(ACCOUNT_EXT_DISPLAYED);

    bind_scope_t bound_scope(report, account);

    if (! report_title.empty()) {
      if (first_report_title)
        first_report_title = false;
      else
        out << '\n';

      value_scope_t val_scope(bound_scope, string_value(report_title));
      format_t group_title_format(report.HANDLER(group_title_format_).str());

      out << group_title_format(val_scope);

      report_title = "";
    }

    if (prepend_format) {
      out.width(static_cast<std::streamsize>(prepend_width));
      out << prepend_format(bound_scope);
    }

    out << account_line_format(bound_scope);

    return 1;
  }
  return 0;
}

std::pair<std::size_t, std::size_t>
format_accounts::mark_accounts(account_t& account, const bool flat)
{
  std::size_t visited    = 0;
  std::size_t to_display = 0;

  foreach (accounts_map::value_type& pair, account.accounts) {
    std::pair<std::size_t, std::size_t> i = mark_accounts(*pair.second, flat);
    visited    += i.first;
    to_display += i.second;
  }

#if DEBUG_ON
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
    call_scope_t call_scope(bound_scope);
    if ((! flat && to_display > 1) ||
        ((flat || to_display != 1 ||
          account.has_xflags(ACCOUNT_EXT_VISITED)) &&
         (report.HANDLED(empty) ||
          report.display_value(report.fn_display_total(call_scope))) &&
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
    disp_pred.parse(report.HANDLER(display_).str());
  }

  mark_accounts(*report.session.journal->master, report.HANDLED(flat));

  std::size_t displayed = 0;

  foreach (account_t * account, posted_accounts)
    displayed += post_account(*account, report.HANDLED(flat));

  if (displayed > 1 &&
      ! report.HANDLED(no_total) && ! report.HANDLED(percent)) {
    bind_scope_t bound_scope(report, *report.session.journal->master);
    out << separator_format(bound_scope);

    if (prepend_format) {
      static_cast<std::ostream&>(report.output_stream)
        .width(static_cast<std::streamsize>(prepend_width));
      static_cast<std::ostream&>(report.output_stream)
        << prepend_format(bound_scope);
    }

    out << total_line_format(bound_scope);
  }

  out.flush();
}

void format_accounts::operator()(account_t& account)
{
  DEBUG("account.display", "Posting account: " << account.fullname());
  posted_accounts.push_back(&account);
}

void report_accounts::flush()
{
  std::ostream& out(report.output_stream);

  foreach (accounts_pair& entry, accounts) {
    if (report.HANDLED(count))
      out << entry.second << ' ';
    out << *entry.first << '\n';
  }
}

void report_accounts::operator()(post_t& post)
{
  std::map<account_t *, std::size_t>::iterator i = accounts.find(post.account);
  if (i == accounts.end())
    accounts.insert(accounts_pair(post.account, 1));
  else
    (*i).second++;
}

void report_payees::flush()
{
  std::ostream& out(report.output_stream);

  foreach (payees_pair& entry, payees) {
    if (report.HANDLED(count))
      out << entry.second << ' ';
    out << entry.first << '\n';
  }
}

void report_payees::operator()(post_t& post)
{
  std::map<string, std::size_t>::iterator i = payees.find(post.payee());
  if (i == payees.end())
    payees.insert(payees_pair(post.payee(), 1));
  else
    (*i).second++;
}

void report_tags::flush()
{
  std::ostream& out(report.output_stream);

  foreach (tags_pair& entry, tags) {
    if (report.HANDLED(count))
      out << entry.second << ' ';
    out << entry.first << '\n';
  }
}

void report_tags::operator()(post_t& post)
{
  if (post.metadata) {
    foreach (const item_t::string_map::value_type& data, *post.metadata) {
      string tag=data.first;
      if (report.HANDLED(values) && (data.second).first) {
	tag+=": "+ (data.second).first.get().to_string();
      }
      std::map<string, std::size_t>::iterator i = tags.find(tag);
      if (i == tags.end())
	tags.insert(tags_pair(tag, 1));
      else
	(*i).second++;
    }
  }
}

void report_commodities::flush()
{
  std::ostream& out(report.output_stream);

  foreach (commodities_pair& entry, commodities) {
    if (report.HANDLED(count))
      out << entry.second << ' ';
    out << *entry.first << '\n';
  }
}

void report_commodities::operator()(post_t& post)
{
  amount_t temp(post.amount.strip_annotations(report.what_to_keep()));
  commodity_t& comm(temp.commodity());

  std::map<commodity_t *, std::size_t>::iterator i = commodities.find(&comm);
  if (i == commodities.end())
    commodities.insert(commodities_pair(&comm, 1));
  else
    (*i).second++;

  if (comm.has_annotation()) {
    annotated_commodity_t& ann_comm(as_annotated_commodity(comm));
    if (ann_comm.details.price) {
      std::map<commodity_t *, std::size_t>::iterator ii =
        commodities.find(&ann_comm.details.price->commodity());
      if (ii == commodities.end())
        commodities.insert
          (commodities_pair(&ann_comm.details.price->commodity(), 1));
      else
        (*ii).second++;
    }
  }

  if (post.cost) {
    amount_t temp_cost(post.cost->strip_annotations(report.what_to_keep()));
    i = commodities.find(&temp_cost.commodity());
    if (i == commodities.end())
      commodities.insert(commodities_pair(&temp_cost.commodity(), 1));
    else
      (*i).second++;
  }
}

} // namespace ledger
