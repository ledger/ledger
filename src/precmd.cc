/*
 * Copyright (c) 2003-2025, John Wiegley.  All rights reserved.
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

#include "precmd.h"
#include "xact.h"
#include "post.h"
#include "account.h"
#include "query.h"
#include "session.h"
#include "report.h"
#include "format.h"

namespace ledger {

namespace {
post_t* get_sample_xact(report_t& report) {
  {
    string str;
    {
      std::ostringstream buf;

      buf << "2004/05/27 Book Store\n"
          << "    ; This note applies to all postings. :SecondTag:\n"
          << "    Expenses:Books                 20 BOOK @ $10\n"
          << "    ; Metadata: Some Value\n"
          << "    ; Typed:: $100 + $200\n"
          << "    ; :ExampleTag:\n"
          << "    ; Here follows a note describing the posting.\n"
          << "    Liabilities:MasterCard        $-200.00\n";

      str = buf.str();
    }

    std::ostream& out(report.output_stream);

    out << _("--- Context is first posting of the following transaction ---") << '\n'
        << str << '\n';
    {
      std::shared_ptr<std::istringstream> in(new std::istringstream(str));

      parse_context_stack_t parsing_context;
      parsing_context.push(in);
      parsing_context.get_current().journal = report.session.journal.get();
      parsing_context.get_current().scope = &report.session;

      report.session.journal->read(parsing_context, NO_HASHES);
      report.session.journal->clear_xdata();
    }
  }
  xact_t* first = report.session.journal->xacts.front();
  return first->posts.front();
}
} // namespace

value_t parse_command(call_scope_t& args) {
  string arg = join_args(args);
  if (arg.empty())
    throw std::logic_error(_("Usage: parse TEXT"));

  report_t& report(find_scope<report_t>(args));
  std::ostream& out(report.output_stream);

  post_t* post = get_sample_xact(report);

  out << _("--- Input expression ---") << '\n';
  out << arg << '\n';

  out << '\n' << _("--- Text as parsed ---") << '\n';
  expr_t expr(arg);
  expr.print(out);
  out << '\n';

  out << '\n' << _("--- Expression tree ---") << '\n';
  expr.dump(out);

  bind_scope_t bound_scope(args, *post);
  expr.compile(bound_scope);
  out << '\n' << _("--- Compiled tree ---") << '\n';
  expr.dump(out);

  out << '\n' << _("--- Calculated value ---") << '\n';
  value_t result(expr.calc());
  result.strip_annotations(report.what_to_keep()).dump(out);
  out << '\n';

  return NULL_VALUE;
}

value_t eval_command(call_scope_t& args) {
  report_t& report(find_scope<report_t>(args));
  expr_t expr(join_args(args));
  value_t result(expr.calc(args).strip_annotations(report.what_to_keep()));

  if (!result.is_null())
    report.output_stream << result << '\n';

  return NULL_VALUE;
}

value_t format_command(call_scope_t& args) {
  string arg = join_args(args);
  if (arg.empty())
    throw std::logic_error(_("Usage: format TEXT"));

  report_t& report(find_scope<report_t>(args));
  std::ostream& out(report.output_stream);

  post_t* post = get_sample_xact(report);

  out << _("--- Input format string ---") << '\n';
  out << arg << '\n' << '\n';

  out << _("--- Format elements ---") << '\n';
  format_t fmt(arg);
  fmt.dump(out);

  out << '\n' << _("--- Formatted string ---") << '\n';
  bind_scope_t bound_scope(args, *post);
  out << '"';
  out << fmt(bound_scope);
  out << "\"\n";

  return NULL_VALUE;
}

value_t period_command(call_scope_t& args) {
  string arg = join_args(args);
  if (arg.empty())
    throw std::logic_error(_("Usage: period TEXT"));

  report_t& report(find_scope<report_t>(args));
  std::ostream& out(report.output_stream);

  show_period_tokens(out, arg);
  out << '\n';

  date_interval_t interval(arg);
  interval.dump(out);

  return NULL_VALUE;
}

value_t query_command(call_scope_t& args) {
  report_t& report(find_scope<report_t>(args));
  std::ostream& out(report.output_stream);

  out << _("--- Input arguments ---") << '\n';
  args.value().dump(out);
  out << '\n' << '\n';

  query_t query(args.value(), report.what_to_keep(), !report.HANDLED(collapse));
  if (query.has_query(query_t::QUERY_LIMIT)) {
    call_scope_t sub_args(static_cast<scope_t&>(args));
    sub_args.push_back(string_value(query.get_query(query_t::QUERY_LIMIT)));

    parse_command(sub_args);
  }

  if (query.has_query(query_t::QUERY_SHOW)) {
    out << '\n' << _("====== Display predicate ======") << '\n' << '\n';

    call_scope_t disp_sub_args(static_cast<scope_t&>(args));
    disp_sub_args.push_back(string_value(query.get_query(query_t::QUERY_SHOW)));

    parse_command(disp_sub_args);
  }

  return NULL_VALUE;
}

} // namespace ledger
