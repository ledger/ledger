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

#include "precmd.h"
#include "report.h"

namespace ledger {

value_t parse_command(call_scope_t& args)
{
  var_t<string> arg(args, 0);

  if (! arg) {
    throw std::logic_error("Usage: parse TEXT");
    return 1L;
  }

  report_t& report(find_scope<report_t>(args));
  std::ostream& out(report.output_stream);

  out << "--- Input text ---" << std::endl;
  out << *arg << std::endl;

  out << std::endl << "--- Text as parsed ---" << std::endl;
  expr_t expr(*arg);
  expr.print(out);
  out << std::endl;

  out << std::endl << "--- Expression tree ---" << std::endl;
  expr.dump(out);

  expr.compile(args);
  out << std::endl << "--- Compiled tree ---" << std::endl;
  expr.dump(out);

  out << std::endl << "--- Calculated value ---" << std::endl;
  value_t result(expr.calc(args));
  result.print(out);
  out << std::endl;

  return 0L;
}

value_t eval_command(call_scope_t& args)
{
  var_t<string> arg(args, 0);

  if (! arg) {
    throw std::logic_error("Usage: eval TEXT");
    return 1L;
  }

  report_t& report(find_scope<report_t>(args));
  std::ostream& out(report.output_stream);

  expr_t expr(*arg);
  out << expr.calc(args).strip_annotations() << std::endl;
  return 0L;
}

value_t format_command(call_scope_t& args)
{
  var_t<string> arg(args, 0);

  if (! arg) {
    throw std::logic_error("Usage: format TEXT");
    return 1L;
  }

  report_t& report(find_scope<report_t>(args));
  std::ostream& out(report.output_stream);

  format_t fmt(*arg);
  fmt.dump(out);

  return 0L;
}

value_t period_command(call_scope_t& args)
{
  var_t<string> arg(args, 0);

  if (! arg) {
    throw std::logic_error("Usage: period TEXT");
    return 1L;
  }

  report_t& report(find_scope<report_t>(args));
  std::ostream& out(report.output_stream);

  interval_t interval(*arg);

  if (! is_valid(interval.begin)) {
    out << "Time period has no beginning." << std::endl;
  } else {
    out << "begin: " << format_date(interval.begin) << std::endl;
    out << "  end: " << format_date(interval.end) << std::endl;
    out << std::endl;

    date_t date = interval.first();

    for (int i = 0; i < 20; i++) {
      out << std::right;
      out.width(2);

      out << i << ": " << format_date(date) << std::endl;

      date = interval.increment(date);
      if (is_valid(interval.end) && date >= interval.end)
	break;
    }
  }
  return 0L;
}

value_t args_command(call_scope_t& args)
{
  report_t& report(find_scope<report_t>(args));
  std::ostream& out(report.output_stream);

  value_t::sequence_t::const_iterator begin = args.value().begin();
  value_t::sequence_t::const_iterator end   = args.value().end();

  out << "--- Input arguments ---" << std::endl;
  args.value().print(out);
  out << std::endl << std::endl;

  string predicate = args_to_predicate_expr(begin, end);

  call_scope_t sub_args(static_cast<scope_t&>(args));
  sub_args.push_back(string_value(predicate));

  return parse_command(sub_args);
}

} // namespace ledger
