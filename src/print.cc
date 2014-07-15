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

#include "print.h"
#include "xact.h"
#include "post.h"
#include "account.h"
#include "session.h"
#include "report.h"

namespace ledger {

namespace {
  bool post_has_simple_amount(const post_t& post)
  {
    // Is the amount the result of a computation, i.e., it wasn't
    // explicit specified by the user?
    if (post.has_flags(POST_CALCULATED))
      return false;

    // Is the amount still empty?  This shouldn't be true by this point,
    // but we check anyway for safety.
    if (post.amount.is_null())
      return false;

    // Is the amount a complex expression.  If so, the first 'if' should
    // have triggered.
    if (post.amount_expr)
      return false;

    // Is there a balance assignment?  If so, don't elide the amount as
    // that can change the semantics.
    if (post.assigned_amount)
      return false;

    // Does it have an explicitly specified cost (i.e., one that wasn't
    // calculated for the user)?  If so, don't elide the amount!
    if (post.cost && ! post.has_flags(POST_COST_CALCULATED))
      return false;

    return true;
  }

  void print_note(std::ostream&     out,
                  const string&     note,
                  const bool        note_on_next_line,
                  const std::size_t columns,
                  const std::size_t prior_width)
  {
    // The 3 is for two spaces and a semi-colon before the note.
    if (note_on_next_line ||
        (columns > 0 &&
         (columns <= prior_width + 3 ||
          note.length() > columns - (prior_width + 3))))
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

    buf << format_date(item_t::use_aux_date ?
                       xact.date() : xact.primary_date(),
                       format_type, format);
    if (! item_t::use_aux_date && xact.aux_date())
      buf << '=' << format_date(*xact.aux_date(),
                                format_type, format);
    buf << ' ';

    buf << (xact.state() == item_t::CLEARED ? "* " :
            (xact.state() == item_t::PENDING ? "! " : ""));

    if (xact.code)
      buf << '(' << *xact.code << ") ";

    buf << xact.payee;

    string leader = buf.str();
    out << leader;

    std::size_t columns =
      (report.HANDLED(columns_) ?
       lexical_cast<std::size_t>(report.HANDLER(columns_).str()) : 80);

    if (xact.note)
      print_note(out, *xact.note, xact.has_flags(ITEM_NOTE_ON_NEXT_LINE),
                 columns, unistring(leader).length());
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

    std::size_t count = xact.posts.size();
    std::size_t index = 0;

    foreach (post_t * post, xact.posts) {
      index++;

      if (! report.HANDLED(generated) &&
          (post->has_flags(ITEM_TEMP | ITEM_GENERATED) &&
           ! post->has_flags(POST_ANONYMIZED)))
        continue;

      out << "    ";

      std::ostringstream pbuf;

      if (xact.state() == item_t::UNCLEARED)
        pbuf << (post->state() == item_t::CLEARED ? "* " :
                (post->state() == item_t::PENDING ? "! " : ""));

      if (post->has_flags(POST_VIRTUAL)) {
        if (post->has_flags(POST_MUST_BALANCE))
          pbuf << '[';
        else
          pbuf << '(';
      }

      pbuf << post->account->fullname();

      if (post->has_flags(POST_VIRTUAL)) {
        if (post->has_flags(POST_MUST_BALANCE))
          pbuf << ']';
        else
          pbuf << ')';
      }

      unistring name(pbuf.str());

      std::size_t account_width =
        (report.HANDLED(account_width_) ?
         lexical_cast<std::size_t>(report.HANDLER(account_width_).str()) : 36);

      if (account_width < name.length())
        account_width = name.length();

      if (! post->has_flags(POST_CALCULATED) || report.HANDLED(generated)) {
        out << name.extract();
        std::string::size_type slip =
          (static_cast<std::string::size_type>(account_width) -
           static_cast<std::string::size_type>(name.length()));

        std::size_t amount_width =
          (report.HANDLED(amount_width_) ?
           lexical_cast<std::size_t>(report.HANDLER(amount_width_).str()) :
           12);
        string amt;
        if (post->amount_expr) {
          std::ostringstream amt_str;
          justify(amt_str, post->amount_expr->text(), (int)amount_width, true);
          amt = amt_str.str();
        }
        else if (count == 2 && index == 2 &&
                 post_has_simple_amount(*post) &&
                 post_has_simple_amount(*(*xact.posts.begin())) &&
                 ((*xact.posts.begin())->amount.commodity() ==
                  post->amount.commodity())) {
          // If there are two postings and they both simple amount, and
          // they are both of the same commodity, don't bother printing
          // the second amount as it's always just an inverse of the
          // first.
        }
        else {
          std::ostringstream amt_str;
          value_t(post->amount).print(amt_str, static_cast<int>(amount_width),
                                      -1, AMOUNT_PRINT_RIGHT_JUSTIFY |
                                      (report.HANDLED(generated) ? 0 :
                                       AMOUNT_PRINT_NO_COMPUTED_ANNOTATIONS));
          amt = amt_str.str();
        }

        string trimmed_amt(amt);
        trim_left(trimmed_amt);
        std::string::size_type amt_slip =
          (static_cast<std::string::size_type>(amt.length()) -
           static_cast<std::string::size_type>(trimmed_amt.length()));

        std::ostringstream amtbuf;
        if (slip + amt_slip < 2)
          amtbuf << string(2 - (slip + amt_slip), ' ');
        amtbuf << amt;

        if (post->given_cost &&
            ! post->has_flags(POST_CALCULATED | POST_COST_CALCULATED)) {
          std::string cost_op;
          if (post->has_flags(POST_COST_IN_FULL))
            cost_op = "@@";
          else
            cost_op = "@";
          if (post->has_flags(POST_COST_VIRTUAL))
            cost_op = "(" + cost_op + ")";

          if (post->has_flags(POST_COST_IN_FULL))
            amtbuf << " " << cost_op << " " << post->given_cost->abs();
          else
            amtbuf << " " << cost_op << " "
                   << (*post->given_cost / post->amount).abs();
        }

        if (post->assigned_amount)
          amtbuf << " = " << *post->assigned_amount;

        string trailer = amtbuf.str();
        if (! trailer.empty()) {
          if (slip > 0) {
            out.width(static_cast<std::streamsize>(slip));
            out << ' ';
          }
          out << trailer;

          account_width += unistring(trailer).length();
        }
      } else {
        out << pbuf.str();
      }

      if (post->note)
        print_note(out, *post->note, post->has_flags(ITEM_NOTE_ON_NEXT_LINE),
                   columns, 4 + account_width);
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
