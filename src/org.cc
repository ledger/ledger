/*
 * Copyright (c) 2003-2018, John Wiegley.  All rights reserved.
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

#include "org.h"
#include "xact.h"
#include "post.h"
#include "account.h"
#include "journal.h"
#include "pool.h"
#include "report.h"

namespace ledger {

posts_to_org_table::posts_to_org_table(report_t&               _report,
                                       const optional<string>& _prepend_format)
  : report(_report), last_xact(NULL), last_post(NULL),
    header_printed(false), first_report_title(true)
{
  first_line_format.parse_format
    ("|%(format_date(date))"
     "|%(code)"
     "|%(payee)"
     "|%(cleared ? \"*\" : (pending ? \"!\" : \"\"))"
     "|%(display_account)"
     "|%(scrub(top_amount(display_amount)))"
     "|%(scrub(top_amount(display_total)))"
     "|%(join(note | xact.note))\n");

  next_lines_format.parse_format
    ("|"
     "|"
     "|%(has_tag(\"Payee\") ? payee : \"\")"
     "|%(cleared ? \"*\" : (pending ? \"!\" : \"\"))"
     "|%(display_account)"
     "|%(scrub(top_amount(display_amount)))"
     "|%(scrub(top_amount(display_total)))"
     "|%(join(note | xact.note))\n");

  amount_lines_format.parse_format
    ("|"
     "|"
     "|"
     "|"
     "|"
     "|%(scrub(next_amount))"
     "|%(scrub(next_total))"
     "|\n");

  if (_prepend_format)
    prepend_format.parse_format(*_prepend_format);

  TRACE_CTOR(posts_to_org_table, "report&, optional<string>");
}

void posts_to_org_table::flush()
{
  report.output_stream.flush();
}

void posts_to_org_table::operator()(post_t& post)
{
  if (! post.has_xdata() ||
      ! post.xdata().has_flags(POST_EXT_DISPLAYED)) {
    std::ostream& out(report.output_stream);

    bind_scope_t bound_scope(report, post);

    if (! header_printed) {
      out << "|Date|Code|Payee|X|Account|Amount|Total|Note|\n"
          << "|-|\n"
          << "|||<20>|||<r>|<r>|<20>|\n";
      header_printed = true;
    }

    if (! report_title.empty()) {
      if (first_report_title)
        first_report_title = false;
      else
        out << '\n';

      value_scope_t val_scope(bound_scope, string_value(report_title));
      format_t group_title_format(report.HANDLER(group_title_format_).str());

      out << "|-|\n";
      out << '|' << group_title_format(val_scope);
      out << "|-|\n";

      report_title = "";
    }

    if (prepend_format)
      out << '|' << prepend_format(bound_scope);

    if (last_xact != post.xact) {
      out << first_line_format(bound_scope);
      last_xact = post.xact;
    }
    else if (last_post && last_post->date() != post.date()) {
      out << first_line_format(bound_scope);
    }
    else {
      out << next_lines_format(bound_scope);
    }

    value_t amt = expr_t("display_amount").calc(bound_scope).simplified();
    value_t tot = expr_t("display_total").calc(bound_scope).simplified();

    if (amt.type() == value_t::BALANCE || tot.type() == value_t::BALANCE) {
      balance_t amt_bal(amt.to_balance());
      balance_t tot_bal(tot.to_balance());
      balance_t::amounts_map::const_iterator i = amt_bal.amounts.begin();
      balance_t::amounts_map::const_iterator j = tot_bal.amounts.begin();
      bool first = true;
      while (i != amt_bal.amounts.end() || j != tot_bal.amounts.end()) {
        if (first) {
          first = false;
          if (i != amt_bal.amounts.end()) ++i;
          if (j != tot_bal.amounts.end()) ++j;
        } else {
          symbol_scope_t call_scope(bound_scope);
          bool           assigned = false;

          if (i != amt_bal.amounts.end()) {
            if ((*i).second) {
              DEBUG("org.next_amount", "next_amount = " << (*i).second);
              call_scope.define(symbol_t::FUNCTION, "next_amount",
                                expr_t::op_t::wrap_value((*i++).second));
              assigned = true;
            } else {
              call_scope.define(symbol_t::FUNCTION, "next_amount",
                                expr_t::op_t::wrap_value(string_value("")));
              ++i;
            }
          } else {
            call_scope.define(symbol_t::FUNCTION, "next_amount",
                              expr_t::op_t::wrap_value(string_value("")));
          }

          if (j != tot_bal.amounts.end()) {
            if ((*j).second) {
              DEBUG("org.next_total", "next_total = " << (*j).second);
              call_scope.define(symbol_t::FUNCTION, "next_total",
                                expr_t::op_t::wrap_value((*j++).second));
              DEBUG("org.next_total", "2.next_total = " <<
                    call_scope.lookup(symbol_t::FUNCTION,
                                      "next_total")->as_value());
              assigned = true;
            } else {
              call_scope.define(symbol_t::FUNCTION, "next_total",
                                expr_t::op_t::wrap_value(string_value("")));
              ++j;
            }
          } else {
            call_scope.define(symbol_t::FUNCTION, "next_total",
                              expr_t::op_t::wrap_value(string_value("")));
          }

          if (assigned) {
            amount_lines_format.mark_uncompiled();
            out << amount_lines_format(call_scope);
          }
        }
      }
    }

    post.xdata().add_flags(POST_EXT_DISPLAYED);
    last_post = &post;
  }
}

} // namespace ledger
