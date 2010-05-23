/*
 * Copyright (c) 2003-2010, John Wiegley.  All rights reserved.
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

#include "print.h"
#include "xact.h"
#include "post.h"
#include "account.h"
#include "session.h"
#include "report.h"

namespace ledger {

print_xacts::print_xacts(report_t& _report,
			 bool      _print_raw)
  : report(_report), print_raw(_print_raw)
{
  TRACE_CTOR(print_xacts, "report&, bool");
}

namespace {
  void print_note(std::ostream& out, const string& note)
  {
    if (note.length() > 15)
      out << "\n    ;";
    else
      out << "  ;";

    bool need_separator = false;
    for (const char * p = note.c_str(); *p; p++) {
      if (*p == '\n') {
	need_separator = true;
      } else {
	if (need_separator) {
	  out << "\n    ;";
	  need_separator = false;
	}
	out << *p;
      }
    }
  }

  void print_xact(report_t& report, std::ostream& out, xact_t& xact)
  {
    format_type_t	   format_type = FMT_WRITTEN;
    optional<const char *> format;

    if (report.HANDLED(date_format_)) {
      format_type = FMT_CUSTOM;
      format      = report.HANDLER(date_format_).str().c_str();
    }

    out << format_date(item_t::use_effective_date ?
		       xact.date() : xact.actual_date(), format_type, format);
    if (! item_t::use_effective_date && xact.effective_date())
      out << '=' << format_date(*xact.effective_date(), format_type, format);
    out << ' ';

    out << (xact.state() == item_t::CLEARED ? "* " :
	    (xact.state() == item_t::PENDING ? "! " : ""));

    if (xact.code)
      out << '(' << *xact.code << ") ";

    out << xact.payee;

    if (xact.note)
      print_note(out, *xact.note);
    out << '\n';

    if (xact.metadata) {
      foreach (const item_t::string_map::value_type& data, *xact.metadata) {
	if (! data.second.second) {
	  out << "    ; ";
	  if (data.second.first)
	    out << data.first << ": " << *data.second.first;
	  else
	    out << ':' << data.first << ":";
	  out << '\n';
	}
      }
    }

    foreach (post_t * post, xact.posts) {
      if (post->has_flags(ITEM_TEMP | ITEM_GENERATED) &&
	  ! report.HANDLED(print_virtual))
	continue;

      out << "    ";

      std::ostringstream buf;

      if (xact.state() == item_t::UNCLEARED)
	buf << (post->state() == item_t::CLEARED ? "* " :
		(post->state() == item_t::PENDING ? "! " : ""));

      if (post->has_flags(POST_VIRTUAL)) {
	if (post->has_flags(POST_MUST_BALANCE))
	  buf << '[';
	else
	  buf << '(';
      }

      buf << post->account->fullname();

      if (post->has_flags(POST_VIRTUAL)) {
	if (post->has_flags(POST_MUST_BALANCE))
	  buf << ']';
	else
	  buf << ')';
      }

      if (! post->has_flags(POST_CALCULATED) || report.HANDLED(print_virtual)) {
	unistring name(buf.str());

	out << name.extract();
	int slip = 36 - static_cast<int>(name.length());
	if (slip > 0)
	  out << string(slip, ' ');

	string amt;
	if (post->amount_expr) {
	  amt = post->amount_expr->text();
	} else {
	  std::ostringstream amt_str;
	  report.scrub(post->amount).print(amt_str, 12, -1, true);
	  amt = amt_str.str();
	}

	string trimmed_amt(amt);
	trim_left(trimmed_amt);
	int amt_slip = (static_cast<int>(amt.length()) -
			static_cast<int>(trimmed_amt.length()));
	if (slip + amt_slip < 2)
	  out << string(2 - (slip + amt_slip), ' ');
	out << amt;

	if (post->cost && ! post->has_flags(POST_CALCULATED)) {
	  if (post->has_flags(POST_COST_IN_FULL))
	    out << " @@ " << report.scrub(post->cost->abs());
	  else
	    out << " @ " << report.scrub((*post->cost / post->amount).abs());
	}

	if (post->assigned_amount)
	  out << " = " << report.scrub(*post->assigned_amount);
      } else {
	out << buf.str();
      }

      if (post->note)
	print_note(out, *post->note);
      out << '\n';
    }
  }
}

void print_xacts::flush()
{
  std::ostream& out(report.output_stream);

  bool first = true;
  foreach (xact_t * xact, xacts) {
    if (first)
      first = false;
    else
      out << '\n';

    if (print_raw) {
      print_item(out, *xact);
      out << '\n';
    } else {
      print_xact(report, out, *xact);
    }
  }

  out.flush();
}

void print_xacts::operator()(post_t& post)
{
  if (! post.has_xdata() ||
      ! post.xdata().has_flags(POST_EXT_DISPLAYED)) {
    if (xacts_present.find(post.xact) == xacts_present.end()) {
      xacts_present.insert(xacts_present_map::value_type(post.xact, true));
      xacts.push_back(post.xact);
    }
    post.xdata().add_flags(POST_EXT_DISPLAYED);
  }
}

} // namespace ledger
