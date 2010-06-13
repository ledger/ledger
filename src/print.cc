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

namespace {
  void print_note(std::ostream&     out,
                  const string&     note,
                  const std::size_t columns,
                  const std::size_t prior_width)
  {
    // The 4 is for four leading spaces at the beginning of the posting, and
    // the 3 is for two spaces and a semi-colon before the note.
    if (columns > 0 && note.length() > columns - (prior_width + 3))
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
    format_type_t          format_type = FMT_WRITTEN;
    optional<const char *> format;

    if (report.HANDLED(date_format_)) {
      format_type = FMT_CUSTOM;
      format      = report.HANDLER(date_format_).str().c_str();
    }

    std::ostringstream buf;

    buf << format_date(item_t::use_effective_date ?
                       xact.date() : xact.actual_date(),
                       format_type, format);
    if (! item_t::use_effective_date && xact.effective_date())
      buf << '=' << format_date(*xact.effective_date(),
                                format_type, format);
    buf << ' ';

    buf << (xact.state() == item_t::CLEARED ? "* " :
            (xact.state() == item_t::PENDING ? "! " : ""));

    if (xact.code)
      buf << '(' << *xact.code << ") ";

    buf << xact.payee;

    string leader = buf.str();
    out << leader;

    std::size_t columns = (report.HANDLED(columns_) ?
                           report.HANDLER(columns_).value.to_long() : 80);

    if (xact.note)
      print_note(out, *xact.note, columns, unistring(leader).length());
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
      if (! report.HANDLED(generated) &&
          (post->has_flags(ITEM_TEMP | ITEM_GENERATED) &&
           ! post->has_flags(POST_ANONYMIZED)))
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

      unistring name(buf.str());

      std::size_t account_width =
        (report.HANDLER(account_width_).specified ?
         report.HANDLER(account_width_).value.to_long() : 36);

      if (account_width < name.length())
        account_width = name.length();

      if (! post->has_flags(POST_CALCULATED) || report.HANDLED(generated)) {
        out << name.extract();
        int slip = (static_cast<int>(account_width) -
                    static_cast<int>(name.length()));
        if (slip > 0) {
          out.width(slip);
          out << ' ';
        }

        std::ostringstream amtbuf;

        string amt;
        if (post->amount_expr) {
          amt = post->amount_expr->text();
        } else {
          int amount_width =
            (report.HANDLER(amount_width_).specified ?
             report.HANDLER(amount_width_).value.to_int() : 12);

          std::ostringstream amt_str;
          value_t(post->amount).print(amt_str, amount_width, -1,
                                      AMOUNT_PRINT_RIGHT_JUSTIFY |
                                      AMOUNT_PRINT_NO_COMPUTED_ANNOTATIONS);
          amt = amt_str.str();
        }

        string trimmed_amt(amt);
        trim_left(trimmed_amt);
        int amt_slip = (static_cast<int>(amt.length()) -
                        static_cast<int>(trimmed_amt.length()));
        if (slip + amt_slip < 2)
          amtbuf << string(2 - (slip + amt_slip), ' ');
        amtbuf << amt;

        if (post->cost &&
            ! post->has_flags(POST_CALCULATED | POST_COST_CALCULATED)) {
          if (post->has_flags(POST_COST_IN_FULL))
            amtbuf << " @@ " << post->cost->abs();
          else
            amtbuf << " @ "
                   << (*post->cost / post->amount).abs();
        }

        if (post->assigned_amount)
          amtbuf << " = " << post->assigned_amount;

        string trailer = amtbuf.str();
        out << trailer;

        account_width += unistring(trailer).length();
      } else {
        out << buf.str();
      }

      if (post->note)
        print_note(out, *post->note, columns, 4 + account_width);
      out << '\n';
    }
  }
}

void print_xacts::title(const string&)
{
  if (first_title) {
    first_title = false;
  } else {
    std::ostream& out(report.output_stream);
    out << '\n';
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
