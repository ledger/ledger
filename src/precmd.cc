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

#include "precmd.h"
#include "xact.h"
#include "post.h"
#include "account.h"
#include "session.h"
#include "report.h"
#include "format.h"

namespace ledger {

namespace {
  post_t * get_sample_xact(report_t& report)
  {
    {
      string str;
      {
	std::ostringstream buf;

	buf << "2004/05/27 Book Store\n"
	    << "    ; This note applies to all postings. :SecondTag:\n"
	    << "    Expenses:Books                 20 BOOK @ $10\n"
	    << "    ; Metadata: Some Value\n"
	    << "    ; :ExampleTag:\n"
	    << "    ; Here follows a note describing the posting.\n"
	    << "    Liabilities:MasterCard        $-200.00\n";

	str = buf.str();
      }

      std::ostream& out(report.output_stream);

      out << _("--- Context is first posting of the following transaction ---")
	  << std::endl << str << std::endl;
      {
	std::istringstream in(str);
	report.session.journal->parse(in, report.session);
	report.session.journal->clear_xdata();
      }
    }
    xact_t * first = report.session.journal->xacts.front();
    return first->posts.front();
  }
}

value_t parse_command(call_scope_t& args)
{
  string arg = join_args(args);
  if (arg.empty()) {
    throw std::logic_error(_("Usage: parse TEXT"));
    return 1L;
  }

  report_t& report(find_scope<report_t>(args));
  std::ostream& out(report.output_stream);

  post_t * post = get_sample_xact(report);

  out << _("--- Input expression ---") << std::endl;
  out << arg << std::endl;

  out << std::endl << _("--- Text as parsed ---") << std::endl;
  expr_t expr(arg);
  expr.print(out);
  out << std::endl;

  out << std::endl << _("--- Expression tree ---") << std::endl;
  expr.dump(out);

  bind_scope_t bound_scope(args, *post);
  expr.compile(bound_scope);
  out << std::endl << _("--- Compiled tree ---") << std::endl;
  expr.dump(out);

  out << std::endl << _("--- Calculated value ---") << std::endl;
  value_t result(expr.calc());
  result.strip_annotations(report.what_to_keep()).dump(out);
  out << std::endl;

  return NULL_VALUE;
}

value_t eval_command(call_scope_t& args)
{
  report_t& report(find_scope<report_t>(args));
  expr_t    expr(join_args(args));
  value_t   result(expr.calc(args).strip_annotations(report.what_to_keep()));

  if (! result.is_null())
    report.output_stream << result << std::endl;

  return NULL_VALUE;
}

value_t format_command(call_scope_t& args)
{
  string arg = join_args(args);
  if (arg.empty()) {
    throw std::logic_error(_("Usage: format TEXT"));
    return 1L;
  }

  report_t& report(find_scope<report_t>(args));
  std::ostream& out(report.output_stream);

  post_t * post = get_sample_xact(report);

  out << _("--- Input format string ---") << std::endl;
  out << arg << std::endl << std::endl;

  out << _("--- Format elements ---") << std::endl;
  format_t fmt(arg);
  fmt.dump(out);

  out << std::endl << _("--- Formatted string ---") << std::endl;
  bind_scope_t bound_scope(args, *post);
  out << '"';
  fmt.format(out, bound_scope);
  out << "\"\n";

  return NULL_VALUE;
}

value_t period_command(call_scope_t& args)
{
  string arg = join_args(args);
  if (arg.empty()) {
    throw std::logic_error(_("Usage: period TEXT"));
    return 1L;
  }

  report_t& report(find_scope<report_t>(args));
  std::ostream& out(report.output_stream);

  date_interval_t interval(arg);

  out << _("global details => ") << std::endl << std::endl;

  if (interval.start)
    out << _("   start: ") << format_date(*interval.start) << std::endl;
  else
    out << _("   start: TODAY: ") << format_date(CURRENT_DATE()) << std::endl;
  if (interval.end)
    out << _("     end: ") << format_date(*interval.end) << std::endl;

  if (interval.skip_duration)
    out << _("    skip: ") << *interval.skip_duration << std::endl;
  if (interval.factor)
    out << _("  factor: ") << interval.factor << std::endl;
  if (interval.duration)
    out << _("duration: ") << *interval.duration << std::endl;

  if (interval.find_period(interval.start ?
			   *interval.start : CURRENT_DATE())) {
    out << std::endl
	<< _("after finding first period => ") << std::endl
	<< std::endl;

    if (interval.start)
      out << _("   start: ") << format_date(*interval.start) << std::endl;
    if (interval.end)
      out << _("     end: ") << format_date(*interval.end) << std::endl;

    if (interval.skip_duration)
      out << _("    skip: ") << *interval.skip_duration << std::endl;
    if (interval.factor)
      out << _("  factor: ") << interval.factor << std::endl;
    if (interval.duration)
      out << _("duration: ") << *interval.duration << std::endl;

    out << std::endl;

    for (int i = 0; i < 20 && interval; i++, ++interval) {
      out << std::right;
      out.width(2);

      out << i << "): " << format_date(*interval.start);
      if (interval.end_of_duration)
	out << " -- " << format_date(*interval.inclusive_end());
      out << std::endl;

      if (! interval.skip_duration)
	break;
    }
  }
  return NULL_VALUE;
}

value_t args_command(call_scope_t& args)
{
  report_t& report(find_scope<report_t>(args));
  std::ostream& out(report.output_stream);

  value_t::sequence_t::const_iterator begin = args.value().begin();
  value_t::sequence_t::const_iterator end   = args.value().end();

  out << _("--- Input arguments ---") << std::endl;
  args.value().dump(out);
  out << std::endl << std::endl;

  std::pair<expr_t, query_parser_t> info = args_to_predicate(begin, end);
  if (! info.first)
    throw_(std::runtime_error,
	   _("Invalid query predicate: %1") << join_args(args));

  call_scope_t sub_args(static_cast<scope_t&>(args));
  sub_args.push_back(string_value(info.first.text()));

  parse_command(sub_args);

  if (info.second.tokens_remaining()) {
    out << std::endl << _("====== Display predicate ======")
	<< std::endl << std::endl;

    call_scope_t disp_sub_args(static_cast<scope_t&>(args));
    info = args_to_predicate(info.second);
    if (! info.first)
      throw_(std::runtime_error,
	     _("Invalid display predicate: %1") << join_args(args));

    disp_sub_args.push_back(string_value(info.first.text()));

    parse_command(disp_sub_args);
  }
  return NULL_VALUE;
}

} // namespace ledger
