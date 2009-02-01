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

#include "report.h"
#include "iterators.h"
#include "filters.h"
#include "chain.h"
#include "output.h"

namespace ledger {

void report_t::xacts_report(xact_handler_ptr handler)
{
  session_xacts_iterator walker(session);
  pass_down_xacts(chain_xact_handlers(*this, handler), walker);

  if (DO_VERIFY())
    session.clean_xacts();
}

void report_t::entry_report(xact_handler_ptr handler, entry_t& entry)
{
  entry_xacts_iterator walker(entry);
  pass_down_xacts(chain_xact_handlers(*this, handler), walker);

  if (DO_VERIFY())
    session.clean_xacts(entry);
}

void report_t::sum_all_accounts()
{
  session_xacts_iterator walker(session);
  pass_down_xacts
    (chain_xact_handlers(*this, xact_handler_ptr(new set_account_value), false),
     walker);
  session.master->calculate_sums();
}

void report_t::accounts_report(acct_handler_ptr handler)
{
  sum_all_accounts();

  if (sort_string.empty()) {
    basic_accounts_iterator walker(*session.master);
    pass_down_accounts(handler, walker, expr_t("total"));
  } else {
    sorted_accounts_iterator walker(*session.master, sort_string);
    pass_down_accounts(handler, walker, expr_t("total"));
  }
    
  if (DO_VERIFY()) {
    session.clean_xacts();
    session.clean_accounts();
  }
}

void report_t::commodities_report(const string& format)
{
}

value_t report_t::get_amount_expr(call_scope_t& scope)
{
  return amount_expr.calc(scope);
}

value_t report_t::get_total_expr(call_scope_t& scope)
{
  return total_expr.calc(scope);
}

value_t report_t::get_display_total(call_scope_t& scope)
{
  return display_total.calc(scope);
}

value_t report_t::f_market_value(call_scope_t& args)
{
  var_t<datetime_t> date(args, 1);
  var_t<string>	    in_terms_of(args, 2);

  commodity_t * commodity = NULL;
  if (in_terms_of)
    commodity = amount_t::current_pool->find_or_create(*in_terms_of);

  DEBUG("report.market", "getting market value of: " << args[0]);

  value_t result =
    args[0].value(date ? optional<datetime_t>(*date) : optional<datetime_t>(),
		  commodity ? optional<commodity_t&>(*commodity) :
		  optional<commodity_t&>());

  DEBUG("report.market", "result is: " << result);
  return result;
}

namespace {
  value_t print_balance(call_scope_t& args)
  {
    var_t<long>	first_width(args, 1);
    var_t<long>	latter_width(args, 2);
#if 0
    var_t<bool>	bold_negative(args, 3);
#endif

    std::ostringstream out;
    args[0].strip_annotations().dump(out, *first_width, *latter_width);
    return string_value(out.str());
  }

  string args_to_predicate(value_t::sequence_t::const_iterator begin,
			   value_t::sequence_t::const_iterator end)
  {
    std::ostringstream expr;
    bool	       append_and = false;
    bool	       only_parenthesis;

    while (begin != end) {
      const string& arg((*begin).as_string());
      const char *  p = arg.c_str();

      bool parse_argument = true;

      if (arg == "not" || arg == "NOT") {
	expr << " ! ";
	parse_argument = false;
	append_and = false;
      }
      else if (arg == "and" || arg == "AND") {
	expr << " & ";
	parse_argument = false;
	append_and = false;
      }
      else if (arg == "or" || arg == "OR") {
	expr << " | ";
	parse_argument = false;
	append_and = false;
      }
      else if (append_and) {
	if (! only_parenthesis)
	  expr << " & ";
      }
      else {
	append_and = true;
      }

      if (parse_argument) {
	bool in_prefix	   = true;
	bool in_suffix	   = false;
	bool found_specifier = false;
	bool saw_tag_char    = false;

	only_parenthesis = true;

	for (const char * c = p; *c != '\0'; c++) {
	  bool consumed = false;

	  if (*c != '(' && *c != ')')
	    only_parenthesis = false;

	  if (in_prefix) {
	    switch (*c) {
	    case '(':
	      break;
	    case '@':
	      expr << "(payee =~ /";
	      found_specifier = true;
	      consumed = true;
	      break;
	    case '=':
	      expr << "(note =~ /";
	      found_specifier = true;
	      consumed = true;
	      break;
	    case '%':
	      expr << "(note =~ /:";
	      found_specifier = true;
	      saw_tag_char = true;
	      consumed = true;
	      break;
	    case '/':
	    case '_':
	    default:
	      if (! found_specifier) {
		expr << "(account =~ /";
		found_specifier = true;
	      }
	      in_prefix = false;
	      break;
	    }
	  } else {
	    switch (*c) {
	    case ')':
	      if (! in_suffix) {
		if (found_specifier) {
		  if (saw_tag_char)
		    expr << ':';
		  expr << "/)";
		}
		in_suffix = true;
	      }
	      break;
	    default:
	      if (in_suffix)
		throw_(parse_error, "Invalid text in specification argument");
	      break;
	    }
	  }

	  if (! consumed)
	    expr << *c;
	}

	if (! in_suffix) {
	  if (found_specifier) {
	    if (saw_tag_char)
	      expr << ':';
	    expr << "/)";
	  }
	}
      }

      begin++;
    }

    DEBUG("report.predicate", "Regexp predicate expression = " << expr.str());

    return expr.str();
  }

  template <class Type        = xact_t,
	    class handler_ptr = xact_handler_ptr,
	    void (report_t::*report_method)(handler_ptr) =
	      &report_t::xacts_report>
  class reporter
  {
    shared_ptr<item_handler<Type> > handler;

  public:
    reporter(item_handler<Type> * _handler)
      : handler(_handler) {}

    value_t operator()(call_scope_t& args)
    {
      report_t& report(find_scope<report_t>(args));

      if (args.value().size() > 0)
	report.append_predicate
	  (args_to_predicate(args.value().as_sequence().begin(),
			     args.value().as_sequence().end()));

      (report.*report_method)(handler_ptr(handler));

      return true;
    }
  };

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

    out << std::endl << "--- Calculated value as XML ---" << std::endl;
    result.write_xml(out);

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
}

expr_t::ptr_op_t report_t::lookup(const string& name)
{
  const char * p = name.c_str();
  switch (*p) {
  case 'a':
    if (std::strcmp(p, "amount_expr") == 0)
      return MAKE_FUNCTOR(report_t::get_amount_expr);
    break;

  case 'd':
    if (std::strcmp(p, "display_total") == 0)
	return MAKE_FUNCTOR(report_t::get_display_total);
    break;

  case 'l':
    if (std::strncmp(p, "ledger_cmd_", 11) == 0) {

#define FORMAT(str) \
    (format_string.empty() ? session. str : format_string)

#if 0
      // Commands yet to implement:
      //
      // entry
      // dump
      // output
      // prices
      // pricesdb
      // csv
      // emacs | lisp
      // xml
#endif

      p = p + 11;
      switch (*p) {
      case 'b':
	if (*(p + 1) == '\0' ||
	    std::strcmp(p, "bal") == 0 ||
	    std::strcmp(p, "balance") == 0)
	  return expr_t::op_t::wrap_functor
	    (reporter<account_t, acct_handler_ptr, &report_t::accounts_report>
	     (new format_accounts(*this, FORMAT(balance_format))));
	break;

      case 'e':
	if (std::strcmp(p, "equity") == 0)
	  return expr_t::op_t::wrap_functor
	    (reporter<account_t, acct_handler_ptr, &report_t::accounts_report>
	     (new format_equity(*this, FORMAT(print_format))));
	break;

      case 'p':
	if (*(p + 1) == '\0' ||
	    std::strcmp(p, "print") == 0)
	  return WRAP_FUNCTOR
	    (reporter<>(new format_xacts(*this, FORMAT(print_format))));
	break;

      case 'r':
	if (*(p + 1) == '\0' ||
	    std::strcmp(p, "reg") == 0 ||
	    std::strcmp(p, "register") == 0)
	  return WRAP_FUNCTOR
	    (reporter<>(new format_xacts(*this, FORMAT(register_format))));
	break;
      }
    }
    else if (std::strncmp(p, "ledger_precmd_", 14) == 0) {
      p = p + 14;
      switch (*p) {
      case 'p':
	if (std::strcmp(p, "parse") == 0)
	  return WRAP_FUNCTOR(parse_command);
	else if (std::strcmp(p, "period") == 0)
	  return WRAP_FUNCTOR(period_command);
	break;

      case 'e':
	if (std::strcmp(p, "eval") == 0)
	  return WRAP_FUNCTOR(eval_command);
	break;

      case 'f':
	if (std::strcmp(p, "format") == 0)
	  return WRAP_FUNCTOR(format_command);
	break;
      }
    }
    break;

  case 'm':
    if (std::strcmp(p, "market_value") == 0)
      return MAKE_FUNCTOR(report_t::f_market_value);
    break;

  case 'o':
    if (std::strncmp(p, "opt_", 4) == 0) {
      p = p + 4;
      switch (*p) {
      case 'a':
	if (std::strcmp(p, "amount_") == 0)
	  return MAKE_FUNCTOR(report_t::option_amount_);
	else if (std::strcmp(p, "ansi") == 0)
	  return MAKE_FUNCTOR(report_t::option_ansi);
	else if (std::strcmp(p, "ansi-invert") == 0)
	  return MAKE_FUNCTOR(report_t::option_ansi_invert);
	else if (std::strcmp(p, "anon") == 0)
	  return MAKE_FUNCTOR(report_t::option_anon);
	break;

      case 'b':
	if (std::strcmp(p, "b_") == 0 ||
	    std::strcmp(p, "begin_") == 0)
	  return MAKE_FUNCTOR(report_t::option_begin_);
	else if (std::strcmp(p, "base") == 0)
	  return MAKE_FUNCTOR(report_t::option_base);
	else if (std::strcmp(p, "by-payee") == 0)
	  return MAKE_FUNCTOR(report_t::option_by_payee);
	break;

      case 'c':
	if (! *(p + 1) || std::strcmp(p, "current") == 0)
	  return MAKE_FUNCTOR(report_t::option_current);
	else if (std::strcmp(p, "collapse") == 0)
	  return MAKE_FUNCTOR(report_t::option_collapse);
	else if (std::strcmp(p, "cleared") == 0)
	  return MAKE_FUNCTOR(report_t::option_cleared);
	else if (std::strcmp(p, "cost") == 0)
	  return MAKE_FUNCTOR(report_t::option_cost);
	else if (std::strcmp(p, "comm-as-payee") == 0)
	  return MAKE_FUNCTOR(report_t::option_comm_as_payee);
	else if (std::strcmp(p, "code-as-payee") == 0)
	  return MAKE_FUNCTOR(report_t::option_code_as_payee);
	break;

      case 'd':
	if (std::strcmp(p, "daily") == 0)
	  return MAKE_FUNCTOR(report_t::option_daily);
	else if (std::strcmp(p, "dow") == 0)
	  return MAKE_FUNCTOR(report_t::option_dow);
	else if (std::strcmp(p, "date-format_") == 0)
	  return MAKE_FUNCTOR(report_t::option_date_format_);
	break;

      case 'e':
	if (std::strcmp(p, "e_") == 0 ||
	    std::strcmp(p, "end_") == 0)
	  return MAKE_FUNCTOR(report_t::option_end_);
	else if (std::strcmp(p, "empty") == 0)
	  return MAKE_FUNCTOR(report_t::option_empty);
	break;

      case 'f':
	if (std::strcmp(p, "format_") == 0)
	  return MAKE_FUNCTOR(report_t::option_format_);
	break;

      case 'h':
	if (std::strcmp(p, "head_") == 0)
	  return MAKE_FUNCTOR(report_t::option_head_);
	break;

      case 'i':
	if (std::strcmp(p, "input-date-format_") == 0)
	  return MAKE_FUNCTOR(report_t::option_input_date_format_);
	break;

      case 'j':
	if (! *(p + 1))
	  return MAKE_FUNCTOR(report_t::option_amount_data);
	break;

      case 'l':
	if (std::strcmp(p, "l_") == 0
	    || std::strcmp(p, "limit_") == 0)
	  return MAKE_FUNCTOR(report_t::option_limit_);
	break;

      case 'm':
	if (std::strcmp(p, "monthly") == 0)
	  return MAKE_FUNCTOR(report_t::option_monthly);
	else if (std::strcmp(p, "market") == 0)
	  return MAKE_FUNCTOR(report_t::option_market);
	break;

      case 'n':
	if (std::strcmp(p, "n") == 0)
	  return MAKE_FUNCTOR(report_t::option_collapse);
	break;

      case 'p':
	if (std::strcmp(p, "p_") == 0 ||
	    std::strcmp(p, "period_") == 0)
	  return MAKE_FUNCTOR(report_t::option_period_);
	else if (std::strcmp(p, "period_sort_") == 0)
	  return MAKE_FUNCTOR(report_t::option_period_sort_);
	else if (std::strcmp(p, "price") == 0)
	  return MAKE_FUNCTOR(report_t::option_price);
	else if (std::strcmp(p, "price_db_") == 0)
	  return MAKE_FUNCTOR(report_t::option_price_db_);
	else if (std::strcmp(p, "pager_") == 0)
	  return MAKE_FUNCTOR(report_t::option_pager_);
	break;

      case 'q':
	if (std::strcmp(p, "quarterly") == 0)
	  return MAKE_FUNCTOR(report_t::option_quarterly);
	else if (std::strcmp(p, "quantity") == 0)
	  return MAKE_FUNCTOR(report_t::option_quantity);
	break;

      case 'r':
	if (std::strcmp(p, "r") == 0 ||
	    std::strcmp(p, "related") == 0)
	  return MAKE_FUNCTOR(report_t::option_related);
	break;

      case 's':
	if (std::strcmp(p, "s") == 0 ||
	    std::strcmp(p, "subtotal") == 0)
	  return MAKE_FUNCTOR(report_t::option_subtotal);
	else if (std::strcmp(p, "sort_") == 0)
	  return MAKE_FUNCTOR(report_t::option_sort_);
	else if (std::strcmp(p, "sort_entries_") == 0)
	  return MAKE_FUNCTOR(report_t::option_sort_entries_);
	else if (std::strcmp(p, "sort_all_") == 0)
	  return MAKE_FUNCTOR(report_t::option_sort_all_);
	break;

      case 't':
	if (std::strcmp(p, "t_") == 0)
	  return MAKE_FUNCTOR(report_t::option_amount_);
	else if (std::strcmp(p, "total_") == 0)
	  return MAKE_FUNCTOR(report_t::option_total_);
	else if (std::strcmp(p, "totals") == 0)
	  return MAKE_FUNCTOR(report_t::option_totals);
	else if (std::strcmp(p, "tail_") == 0)
	  return MAKE_FUNCTOR(report_t::option_tail_);
	break;

      case 'u':
	if (std::strcmp(p, "uncleared") == 0)
	  return MAKE_FUNCTOR(report_t::option_uncleared);
	break;

      case 'w':
	if (std::strcmp(p, "weekly") == 0)
	  return MAKE_FUNCTOR(report_t::option_weekly);
	break;

      case 'x':
	if (std::strcmp(p, "x"))
	  return MAKE_FUNCTOR(report_t::option_comm_as_payee);
	break;

      case 'y':
	if (std::strcmp(p, "yearly") == 0)
	  return MAKE_FUNCTOR(report_t::option_yearly);
	else if (std::strcmp(p, "y_") == 0)
	  return MAKE_FUNCTOR(report_t::option_date_format_);
	break;

      case 'B':
	if (! *(p + 1))
	  return MAKE_FUNCTOR(report_t::option_cost);
	break;

      case 'C':
	if (! *(p + 1))
	  return MAKE_FUNCTOR(report_t::option_cleared);
	break;

      case 'E':
	if (! *(p + 1))
	  return MAKE_FUNCTOR(report_t::option_empty);
	break;

      case 'F':
	if (std::strcmp(p, "F_") == 0)
	  return MAKE_FUNCTOR(report_t::option_format_);
	break;

      case 'I':
	if (! *(p + 1))
	  return MAKE_FUNCTOR(report_t::option_price);
	break;

      case 'J':
	if (! *(p + 1))
	  return MAKE_FUNCTOR(report_t::option_total_data);
	break;

      case 'M':
	if (! *(p + 1))
	  return MAKE_FUNCTOR(report_t::option_monthly);
	break;

      case 'O':
	if (! *(p + 1))
	  return MAKE_FUNCTOR(report_t::option_quantity);
	break;

      case 'P':
	if (! *(p + 1))
	  return MAKE_FUNCTOR(report_t::option_by_payee);
	break;

      case 'S':
	if (std::strcmp(p, "S_") == 0)
	  return MAKE_FUNCTOR(report_t::option_sort_);
	break;

      case 'T':
	if (std::strcmp(p, "T_") == 0)
	  return MAKE_FUNCTOR(report_t::option_total_);
	break;

      case 'U':
	if (! *(p + 1))
	  return MAKE_FUNCTOR(report_t::option_uncleared);
	break;

      case 'V':
	if (! *(p + 1))
	  return MAKE_FUNCTOR(report_t::option_market);
	break;

      case 'W':
	if (! *(p + 1))
	  return MAKE_FUNCTOR(report_t::option_weekly);
	break;

      case 'Y':
	if (! *(p + 1))
	  return MAKE_FUNCTOR(report_t::option_yearly);
	break;
      }
    }
    break;

  case 'p':
    if (std::strcmp(p, "print_balance") == 0)
      return WRAP_FUNCTOR(print_balance);
    break;

  case 't':
    if (std::strcmp(p, "total_expr") == 0)
	return MAKE_FUNCTOR(report_t::get_total_expr);
    else if (std::strcmp(p, "truncate") == 0)
	return MAKE_FUNCTOR(report_t::get_total_expr);
    break;
  }

  return session.lookup(name);
}

} // namespace ledger
