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

#include "select.h"
#include "journal.h"
#include "account.h"
#include "report.h"
#include "output.h"
#include "print.h"
#include "chain.h"
#include "filters.h"
#include "scope.h"
#include "op.h"

namespace ledger {

namespace {
  bool get_principal_identifiers(expr_t::ptr_op_t expr, string& ident,
                                 bool do_transforms = false)
  {
    bool result = true;

    if (expr->is_ident()) {
      string name(expr->as_ident());
      if (name == "date" || name == "aux_date" || name == "payee") {
        if (! ident.empty() &&
            ! (name == "date" || name == "aux_date" || name == "payee"))
          result = false;
        ident = name;
      }
      else if (name == "account") {
        if (! ident.empty() && ! (name == "account"))
          result = false;
        ident = name;
        if (do_transforms)
          expr->set_ident("display_account");
      }
      else if (name == "amount") {
        if (! ident.empty() && ! (name == "amount"))
          result = false;
        ident = name;
        if (do_transforms)
          expr->set_ident("display_amount");
      }
      else if (name == "total") {
        if (! ident.empty() && ! (name == "total"))
          result = false;
        ident = name;
        if (do_transforms)
          expr->set_ident("display_total");
      }
    }

    if (expr->kind > expr_t::op_t::TERMINALS || expr->is_scope()) {
      if (expr->left()) {
        if (! get_principal_identifiers(expr->left(), ident, do_transforms))
          result = false;
        if (expr->kind > expr_t::op_t::UNARY_OPERATORS && expr->has_right())
          if (! get_principal_identifiers(expr->right(), ident, do_transforms))
            result = false;
      }
    }

    return result;
  }
}

value_t select_command(call_scope_t& args)
{
  string text = "select " + join_args(args);
  if (text.empty())
    throw std::logic_error(_("Usage: select TEXT"));

  report_t& report(find_scope<report_t>(args));

  // Our first step is to divide the select statement into its principal
  // parts:
  //
  //   SELECT <VALEXPR-LIST>
  //   FROM <NAME>
  //   WHERE <VALEXPR>
  //   DISPLAY <VALEXPR>
  //   COLLECT <VALEXPR>
  //   GROUP BY <VALEXPR>
  //   STYLE <NAME>

  boost::regex select_re
    ("(select|from|where|display|collect|group\\s+by|style)\\s+"
     "(.+?)"
     "(?=(\\s+(from|where|display|collect|group\\s+by|style)\\s+|$))",
     boost::regex::perl | boost::regex::icase);

  boost::regex from_accounts_re("from\\s+accounts\\>");
  bool accounts_report = boost::regex_search(text, from_accounts_re);

  boost::sregex_iterator m1(text.begin(), text.end(), select_re);
  boost::sregex_iterator m2;

  expr_t::ptr_op_t report_functor;
  std::ostringstream formatter;

  while (m1 != m2) {
    const boost::match_results<string::const_iterator>& match(*m1);

    string keyword(match[1]);
    string arg(match[2]);

    DEBUG("select.parse", "keyword: " << keyword);
    DEBUG("select.parse", "arg: " << arg);

    if (keyword == "select") {
      expr_t  args_expr(arg);
      value_t columns(split_cons_expr(args_expr.get_op()));
      bool    first = true;
      string  thus_far = "";

      std::size_t cols = 0;
      if (report.HANDLED(columns_))
        cols = lexical_cast<std::size_t>(report.HANDLER(columns_).value);
      else if (const char * columns_env = std::getenv("COLUMNS"))
        cols = lexical_cast<std::size_t>(columns_env);
      else
        cols = 80;

      std::size_t date_width =
        (report.HANDLED(date_width_) ?
         lexical_cast<std::size_t>(report.HANDLER(date_width_).str()) :
         static_cast<std::size_t>
         (format_date(CURRENT_DATE(),FMT_PRINTED).length()));
      std::size_t payee_width =
        (report.HANDLED(payee_width_) ?
         lexical_cast<std::size_t>(report.HANDLER(payee_width_).str()) :
         std::size_t(double(cols) * 0.263157));
      std::size_t account_width =
        (report.HANDLED(account_width_) ?
         lexical_cast<std::size_t>(report.HANDLER(account_width_).str()) :
         std::size_t(double(cols) * 0.302631));
      std::size_t amount_width =
        (report.HANDLED(amount_width_) ?
         lexical_cast<std::size_t>(report.HANDLER(amount_width_).str()) :
         std::size_t(double(cols) * 0.157894));
      std::size_t total_width =
        (report.HANDLED(total_width_) ?
         lexical_cast<std::size_t>(report.HANDLER(total_width_).str()) :
         amount_width);
      std::size_t meta_width =
        (report.HANDLED(meta_width_) ?
         lexical_cast<std::size_t>(report.HANDLER(meta_width_).str()) :
         10);

      bool saw_payee   = false;
      bool saw_account = false;

      std::size_t cols_needed = 0;
      foreach (const value_t& column, columns.to_sequence()) {
        string ident;
        if (get_principal_identifiers(as_expr(column), ident)) {
          if (ident == "date" || ident == "aux_date") {
            cols_needed += date_width + 1;
          }
          else if (ident == "payee") {
            cols_needed += payee_width + 1;
            saw_payee = true;
          }
          else if (ident == "account") {
            cols_needed += account_width + 1;
            saw_account = true;
          }
          else if (ident == "amount") {
            cols_needed += amount_width + 1;
          }
          else if (ident == "total") {
            cols_needed += total_width + 1;
          }
          else {
            cols_needed += meta_width + 1;
          }
        }
      }

      while ((saw_account || saw_payee) && cols_needed < cols) {
        if (saw_account && cols_needed < cols) {
          ++account_width;
          ++cols_needed;
          if (cols_needed < cols) {
            ++account_width;
            ++cols_needed;
          }
        }
        if (saw_payee && cols_needed < cols) {
          ++payee_width;
          ++cols_needed;
        }
      }

      while ((saw_account || saw_payee) && cols_needed > cols &&
             account_width > 5 && payee_width > 5) {
        DEBUG("auto.columns", "adjusting account down");
        if (saw_account && cols_needed > cols) {
          --account_width;
          --cols_needed;
          if (cols_needed > cols) {
            --account_width;
            --cols_needed;
          }
        }
        if (saw_payee && cols_needed > cols) {
          --payee_width;
          --cols_needed;
        }
        DEBUG("auto.columns", "account_width now = " << account_width);
      }

      if (! report.HANDLED(date_width_))
        report.HANDLER(date_width_).value    = to_string(date_width);
      if (! report.HANDLED(payee_width_))
        report.HANDLER(payee_width_).value   = to_string(payee_width);
      if (! report.HANDLED(account_width_))
        report.HANDLER(account_width_).value = to_string(account_width);
      if (! report.HANDLED(amount_width_))
        report.HANDLER(amount_width_).value  = to_string(amount_width);
      if (! report.HANDLED(total_width_))
        report.HANDLER(total_width_).value   = to_string(total_width);

      foreach (const value_t& column, columns.to_sequence()) {
        if (first)
          first = false;
        else
          formatter << ' ';

        formatter << "%(";

        string ident;
        if (get_principal_identifiers(as_expr(column), ident, true)) {
          if (ident == "date" || ident == "aux_date") {
            formatter << "ansify_if("
                      << "ansify_if(justify(format_date(";

            as_expr(column)->print(formatter);

            formatter << "), int(date_width)),";
            formatter << "green if color and date > today),"
                      << "bold if should_bold)";

            if (! thus_far.empty())
              thus_far += " + ";
            thus_far += "int(date_width) + 1";
          }
          else if (ident == "payee") {
            formatter << "ansify_if("
                      << "ansify_if(justify(truncated(";

            as_expr(column)->print(formatter);

            formatter << ", int(payee_width)), int(payee_width)),";
            formatter << "bold if color and !cleared and actual),"
                      << "bold if should_bold)";

            if (! thus_far.empty())
              thus_far += " + ";
            thus_far += "int(payee_width) + 1";
          }
          else if (ident == "account") {
            formatter << "ansify_if(";

            if (accounts_report) {
              formatter << "ansify_if(";
              formatter << "partial_account(options.flat), blue if color),";
            } else {
              formatter << "justify(truncated(";
              as_expr(column)->print(formatter);
              formatter << ", int(account_width), int(abbrev_len)),"
                        << "int(account_width), -1, ";
              formatter << "false, color),";

              if (! thus_far.empty())
                thus_far += " + ";
              thus_far += "int(account_width) + 1";
            }

            formatter << " bold if should_bold)";
          }
          else if (ident == "amount" || ident == "total") {
            formatter << "ansify_if("
                      << "justify(scrub(";

            as_expr(column)->print(formatter);

            formatter << "), ";

            if (ident == "amount")
              formatter << "int(amount_width),";
            else
              formatter << "int(total_width),";

            if (! thus_far.empty())
              thus_far += " + ";

            if (ident == "amount")
              thus_far += "int(amount_width)";
            else
              thus_far += "int(total_width)";

            if (thus_far.empty())
              formatter << "-1";
            else
              formatter << thus_far;

            formatter << ", true, color),"
                      << " bold if should_bold)";

            thus_far += " + 1";
          }
          else {
            formatter << "ansify_if("
                      << "justify(truncated(";

            as_expr(column)->print(formatter);

            formatter << ", int(meta_width or 10)), int(meta_width) or 10),";
            formatter << "bold if should_bold)";

            if (! thus_far.empty())
              thus_far += " + ";
            thus_far += "(int(meta_width) or 10) + 1";
          }
        }
        formatter << ")";
      }
      formatter << "\\n";
      DEBUG("select.parse", "formatter: " << formatter.str());
    }
    else if (keyword == "from") {
      if (arg == "xacts" || arg == "txns" || arg == "transactions") {
        report_functor = expr_t::op_t::wrap_functor
          (reporter<>(post_handler_ptr(new print_xacts(report,
                                                       report.HANDLED(raw))),
                      report, string("#select")));
      }
      else if (arg == "posts" || arg == "postings") {
        report_functor = expr_t::op_t::wrap_functor
          (reporter<>(post_handler_ptr(new format_posts(report, formatter.str())),
                      report, string("#select")));
      }
      else if (arg == "accounts") {
        report_functor = expr_t::op_t::wrap_functor
          (reporter<account_t, acct_handler_ptr, &report_t::accounts_report>
           (acct_handler_ptr(new format_accounts(report, formatter.str())),
            report, string("#select")));
      }
      else if (arg == "commodities") {
        report_functor = expr_t::op_t::wrap_functor
          (reporter<post_t, post_handler_ptr, &report_t::commodities_report>
           (post_handler_ptr(new format_posts(report, formatter.str())),
            report, string("#select")));
      }
    }
    else if (keyword == "where") {
#if 0
      query_t          query;
      keep_details_t   keeper(true, true, true);
      expr_t::ptr_op_t expr = 
        query.parse_args(string_value(arg).to_sequence(), keeper, false, true);
      report.HANDLER(limit_).on("#select", query.get_query(query_t::QUERY_LIMIT));
#else
      report.HANDLER(limit_).on("#select", arg);
#endif
    }
    else if (keyword == "display") {
      report.HANDLER(display_).on("#select", arg);
    }
    else if (keyword == "collect") {
      report.HANDLER(amount_).on("#select", arg);
    }
    else if (keyword == "group by") {
      report.HANDLER(group_by_).on("#select", arg);
    }
    else if (keyword == "style") {
      if (arg == "csv") {
      }
      else if (arg == "xml") {
      }
      else if (arg == "json") {
      }
      else if (arg == "emacs") {
      }
      else if (arg == "org") {
      }
    }

    ++m1;
  }

  if (! report_functor) {
    report_functor = expr_t::op_t::wrap_functor
      (reporter<>(post_handler_ptr(new format_posts(report, formatter.str())),
                  report, string("#select")));
  }

  call_scope_t call_args(report);
  return report_functor->as_function()(call_args);
}

} // namespace ledger
