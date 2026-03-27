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

/**
 * @file   precmd.cc
 * @author John Wiegley
 *
 * @ingroup report
 *
 * @brief Diagnostic pre-commands: parse, eval, format, period, query.
 *
 * All pre-commands operate without loading journal data.  For commands
 * that need a posting context (parse, format), a hardcoded sample
 * transaction is created via get_sample_xact() so that expressions can
 * be compiled and evaluated in a realistic binding environment.
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

/*--- Sample Transaction ---*/

namespace {

/**
 * @brief Create a sample transaction for use by diagnostic pre-commands.
 *
 * The sample transaction is a hardcoded "Book Store" purchase that
 * exercises many ledger features: metadata, typed metadata, tags, notes,
 * lot prices, and multiple postings.  It is parsed into the session's
 * journal so that the resulting posting can serve as a realistic binding
 * context for compiling and evaluating expressions or format strings.
 *
 * @return A pointer to the first posting of the sample transaction.
 */
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

      // Temporarily disable decimal-comma mode while parsing the
      // hardcoded sample journal, which uses period-decimal amounts
      // (e.g. $-200.00).  See issue #594.
      bool saved_decimal_comma = commodity_t::decimal_comma_by_default;
      commodity_t::decimal_comma_by_default = false;

      report.session.journal->read(parsing_context, NO_HASHES);

      commodity_t::decimal_comma_by_default = saved_decimal_comma;

      report.session.journal->clear_xdata();
    }
  }
  xact_t* first = report.session.journal->xacts.front();
  return first->posts.front();
}
} // namespace

/*--- parse Command ---*/

/**
 * @brief The "parse" pre-command: display all stages of expression processing.
 *
 * Given a value expression string, this command shows:
 *   1. The raw input expression
 *   2. The expression as re-serialized after parsing (round-trip test)
 *   3. The expression AST before compilation
 *   4. The expression AST after compilation (with optimizations)
 *   5. The calculated result value
 *
 * This is invaluable for debugging why an expression does not produce
 * the expected result.
 */
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

/*--- eval Command ---*/

/// Evaluate a value expression in the current scope and print the result.
/// Unlike "parse", this shows only the final result, not intermediate stages.
value_t eval_command(call_scope_t& args) {
  report_t& report(find_scope<report_t>(args));
  expr_t expr(join_args(args));
  value_t result(expr.calc(args).strip_annotations(report.what_to_keep()));

  if (!result.is_null())
    report.output_stream << result << '\n';

  return NULL_VALUE;
}

/*--- format Command ---*/

/// Parse a format string, display its internal element structure, and
/// render it against a sample posting.  Shows both the parsed elements
/// and the final formatted output (quoted to reveal whitespace).
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

/*--- period Command ---*/

/// Tokenize a period expression and display the resulting date_interval_t
/// structure, showing how Ledger interprets period strings like
/// "monthly from 2024/01 to 2024/06".
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

/*--- query Command ---*/

/// Parse a query expression (the same syntax used by "bal", "reg", etc.)
/// and display both the limit predicate (which transactions to include)
/// and the display predicate (which to show), each run through
/// parse_command for full AST inspection.
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
