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

#include "report.h"
#include "session.h"
#include "unistring.h"
#include "format.h"
#include "output.h"
#include "iterators.h"
#include "filters.h"
#include "precmd.h"
#include "stats.h"
#include "generate.h"
#include "derive.h"
#include "emacs.h"

namespace ledger {

void report_t::posts_report(post_handler_ptr handler)
{
  journal_posts_iterator walker(*session.journal.get());
  pass_down_posts(chain_post_handlers(*this, handler), walker);
  session.journal->clear_xdata();
}

void report_t::generate_report(post_handler_ptr handler)
{
  HANDLER(limit_).on(string("#generate"), "actual");

  generate_posts_iterator walker
    (session, HANDLED(seed_) ?
     static_cast<unsigned int>(HANDLER(seed_).value.to_long()) : 0,
     HANDLED(head_) ?
     static_cast<unsigned int>(HANDLER(head_).value.to_long()) : 50);

  pass_down_posts(chain_post_handlers(*this, handler), walker);
}

void report_t::xact_report(post_handler_ptr handler, xact_t& xact)
{
  xact_posts_iterator walker(xact);
  pass_down_posts(chain_post_handlers(*this, handler), walker);
  xact.clear_xdata();
}

void report_t::accounts_report(acct_handler_ptr handler)
{
  journal_posts_iterator walker(*session.journal.get());

  // The lifetime of the chain object controls the lifetime of all temporary
  // objects created within it during the call to pass_down_posts, which will
  // be needed later by the pass_down_accounts.
  post_handler_ptr chain =
    chain_post_handlers(*this, post_handler_ptr(new ignore_posts), true);
  pass_down_posts(chain, walker);

  scoped_ptr<accounts_iterator> iter;
  if (! HANDLED(sort_)) {
    iter.reset(new basic_accounts_iterator(*session.journal->master));
  } else {
    expr_t sort_expr(HANDLER(sort_).str());
    sort_expr.set_context(this);
    iter.reset(new sorted_accounts_iterator(*session.journal->master,
					    sort_expr, HANDLED(flat)));
  }

  if (HANDLED(display_))
    pass_down_accounts(handler, *iter.get(),
		       item_predicate(HANDLER(display_).str(), what_to_keep()),
		       *this);
  else
    pass_down_accounts(handler, *iter.get());

  session.journal->clear_xdata();
}

void report_t::commodities_report(post_handler_ptr handler)
{
  posts_commodities_iterator walker(*session.journal.get());
  pass_down_posts(chain_post_handlers(*this, handler), walker);
  session.journal->clear_xdata();
}

value_t report_t::fn_amount_expr(call_scope_t& scope)
{
  return HANDLER(amount_).expr.calc(scope);
}

value_t report_t::fn_total_expr(call_scope_t& scope)
{
  return HANDLER(total_).expr.calc(scope);
}

value_t report_t::fn_display_amount(call_scope_t& scope)
{
  return HANDLER(display_amount_).expr.calc(scope);
}

value_t report_t::fn_display_total(call_scope_t& scope)
{
  return HANDLER(display_total_).expr.calc(scope);
}

value_t report_t::fn_market(call_scope_t& scope)
{
  interactive_t args(scope, "a&ts");

  value_t              result;
  optional<datetime_t> moment = (args.has(1) ?
				 args.get<datetime_t>(1) :
				 optional<datetime_t>());
  if (args.has(2))
    result = args.value_at(0).exchange_commodities(args.get<string>(2),
						   /* add_prices= */ false,
						   moment);
  else
    result = args.value_at(0).value(true, moment);

  if (! result.is_null())
    return result;

  return args.value_at(0);
}

value_t report_t::fn_get_at(call_scope_t& scope)
{
  interactive_t args(scope, "Sl");

  DEBUG("report.get_at", "get_at[0] = " << args.value_at(0));
  DEBUG("report.get_at", "get_at[1] = " << args.value_at(1));

  if (args.get<long>(1) == 0) {
    if (! args.value_at(0).is_sequence())
      return args.value_at(0);
  } else {
    if (! args.value_at(0).is_sequence())
      throw_(std::runtime_error,
	     _("Attempting to get argument at index %1 from %2")
	     << args.get<long>(1) << args.value_at(0).label());
  }
  return args.get<const value_t::sequence_t&>(0)[args.get<long>(1)];
}

value_t report_t::fn_is_seq(call_scope_t& scope)
{
  return scope.value().is_sequence();
}

value_t report_t::fn_strip(call_scope_t& args)
{
  return args.value().strip_annotations(what_to_keep());
}

value_t report_t::fn_scrub(call_scope_t& args)
{
  value_t temp(args.value().strip_annotations(what_to_keep()));
  if (HANDLED(base))
    return temp;
  else
    return temp.unreduced();
}

value_t report_t::fn_rounded(call_scope_t& args)
{
  return args.value().rounded();
}

value_t report_t::fn_unrounded(call_scope_t& args)
{
  return args.value().unrounded();
}

value_t report_t::fn_quantity(call_scope_t& scope)
{
  interactive_t args(scope, "a");
  return args.get<amount_t>(0).number();
}

value_t report_t::fn_abs(call_scope_t& scope)
{
  interactive_t args(scope, "v");
  return args.value_at(0).abs();
}

value_t report_t::fn_truncated(call_scope_t& scope)
{
  interactive_t args(scope, "v&ll");
  return string_value(format_t::truncate
		      (args.get<string>(0),
		       args.has(1) && args.get<int>(1) > 0 ? args.get<int>(1) : 0,
		       args.has(2) ? args.get<int>(2) : 0));
}

value_t report_t::fn_justify(call_scope_t& scope)
{
  interactive_t args(scope, "vl&lbb");
  std::ostringstream out;
  args.value_at(0)
    .print(out, args.get<int>(1),
	   args.has(2) ? args.get<int>(2) : -1,
	   args.has(3) ? args.get<bool>(3) : false,
	   args.has(4) ? args.get<bool>(4) : false);
  return string_value(out.str());
}

value_t report_t::fn_quoted(call_scope_t& scope)
{
  interactive_t	     args(scope, "s");
  std::ostringstream out;

  out << '"';
  foreach (const char ch, args.get<string>(0)) {
    if (ch == '"')
      out << "\\\"";
    else
      out << ch;
  }
  out << '"';

  return string_value(out.str());
}

value_t report_t::fn_join(call_scope_t& scope)
{
  interactive_t	     args(scope, "s");
  std::ostringstream out;

  foreach (const char ch, args.get<string>(0)) {
    if (ch != '\n')
      out << ch;
    else
      out << "\\n";
  }
  return string_value(out.str());
}

value_t report_t::fn_format_date(call_scope_t& scope)
{
  interactive_t args(scope, "d&s");
  if (args.has(1))
    return string_value(format_date(args.get<date_t>(0), FMT_CUSTOM,
				    args.get<string>(1).c_str()));
  else
    return string_value(format_date(args.get<date_t>(0), FMT_PRINTED));
}

value_t report_t::fn_ansify_if(call_scope_t& scope)
{
  interactive_t args(scope, "v&s");

  if (args.has(1)) {
    string color = args.get<string>(1);
    std::ostringstream buf;
    if (color == "black")	   buf << "\033[30m";
    else if (color == "red")	   buf << "\033[31m";
    else if (color == "green")	   buf << "\033[32m";
    else if (color == "yellow")	   buf << "\033[33m";
    else if (color == "blue")	   buf << "\033[34m";
    else if (color == "magenta")   buf << "\033[35m";
    else if (color == "cyan")	   buf << "\033[36m";
    else if (color == "white")	   buf << "\033[37m";
    else if (color == "bold")	   buf << "\033[1m";
    else if (color == "underline") buf << "\033[4m";
    else if (color == "blink")	   buf << "\033[5m";
    buf << args.value_at(0);
    buf << "\033[0m";
    return string_value(buf.str());
  } else {
    return args.value_at(0);
  }
}

value_t report_t::fn_percent(call_scope_t& scope)
{
  interactive_t args(scope, "aa");
  return (amount_t("100.00%") *
	  (args.get<amount_t>(0) / args.get<amount_t>(1)).number());
}

value_t report_t::fn_price(call_scope_t& scope)
{
  interactive_t args(scope, "v");
  return args.value_at(0).price();
}

value_t report_t::fn_account_total(call_scope_t& args)
{
  account_t * acct = NULL;
  string name;
  if (args[0].is_string()) {
    name = args[0].as_string();
    acct = session.journal->find_account(name, false);
  }
  else if (args[0].is_mask()) {
    name = args[0].as_mask().expr.str();
    acct = session.journal->find_account_re(name);
  }
  else {
    throw_(std::runtime_error,
	   _("Expected string or mask for argument 1, but received %1")
	   << args[0].label());
  }

  if (! acct)
    throw_(std::runtime_error,
	   _("Could not find an account matching ") << name);

  return acct->amount();
}

value_t report_t::fn_lot_date(call_scope_t& scope)
{
  interactive_t args(scope, "v");
  if (args.value_at(0).is_annotated()) {
    const annotation_t& details(args.value_at(0).annotation());
    if (details.date)
      return *details.date;
  }
  return NULL_VALUE;
}

value_t report_t::fn_lot_price(call_scope_t& scope)
{
  interactive_t args(scope, "v");
  if (args.value_at(0).is_annotated()) {
    const annotation_t& details(args.value_at(0).annotation());
    if (details.price)
      return *details.price;
  }
  return NULL_VALUE;
}

value_t report_t::fn_lot_tag(call_scope_t& scope)
{
  interactive_t args(scope, "v");
  if (args.value_at(0).is_annotated()) {
    const annotation_t& details(args.value_at(0).annotation());
    if (details.tag)
      return string_value(*details.tag);
  }
  return NULL_VALUE;
}

namespace {
  value_t fn_black(call_scope_t&) {
    return string_value("black");
  }
  value_t fn_blink(call_scope_t&) {
    return string_value("blink");
  }
  value_t fn_blue(call_scope_t&) {
    return string_value("blue");
  }
  value_t fn_bold(call_scope_t&) {
    return string_value("bold");
  }
  value_t fn_cyan(call_scope_t&) {
    return string_value("cyan");
  }
  value_t fn_green(call_scope_t&) {
    return string_value("green");
  }
  value_t fn_magenta(call_scope_t&) {
    return string_value("magenta");
  }
  value_t fn_red(call_scope_t&) {
    return string_value("red");
  }
  value_t fn_underline(call_scope_t&) {
    return string_value("underline");
  }
  value_t fn_white(call_scope_t&) {
    return string_value("white");
  }
  value_t fn_yellow(call_scope_t&) {
    return string_value("yellow");
  }
  value_t fn_false(call_scope_t&) {
    return false;
  }
  value_t fn_null(call_scope_t&) {
    return NULL_VALUE;
  }

  template <class Type        = post_t,
	    class handler_ptr = post_handler_ptr,
	    void (report_t::*report_method)(handler_ptr) =
	      &report_t::posts_report>
  class reporter
  {
    shared_ptr<item_handler<Type> > handler;

    report_t& report;
    string    whence;

  public:
    reporter(item_handler<Type> * _handler, report_t& _report,
	     const string& _whence)
      : handler(_handler), report(_report), whence(_whence) {}

    value_t operator()(call_scope_t& args)
    {
      if (args.size() > 0) {
	value_t::sequence_t::const_iterator begin =
	  args.value().as_sequence().begin();
	value_t::sequence_t::const_iterator end   =
	  args.value().as_sequence().end();

	std::pair<expr_t, query_parser_t> info = args_to_predicate(begin, end);
	if (! info.first)
	  throw_(std::runtime_error,
		 _("Invalid query predicate: %1") << join_args(args));

	string limit = info.first.text();
	if (! limit.empty())
	  report.HANDLER(limit_).on(whence, limit);

	DEBUG("report.predicate",
	      "Predicate = " << report.HANDLER(limit_).str());

	if (info.second.tokens_remaining()) {
	  info = args_to_predicate(info.second);
	  if (! info.first)
	    throw_(std::runtime_error,
		   _("Invalid display predicate: %1") << join_args(args));

	  string display = info.first.text();

	  if (! display.empty())
	    report.HANDLER(display_).on(whence, display);

	  DEBUG("report.predicate",
		"Display predicate = " << report.HANDLER(display_).str());
	}
      }

      (report.*report_method)(handler_ptr(handler));

      return true;
    }
  };
}

value_t report_t::reload_command(call_scope_t&)
{
  session.close_journal_files();
  session.read_journal_files();
  return true;
}

value_t report_t::echo_command(call_scope_t& scope)
{
  interactive_t args(scope, "s");
  std::ostream& out(output_stream);
  out << args.get<string>(0) << std::endl;
  return true;
}

bool report_t::maybe_import(const string& module)
{
  if (lookup(symbol_t::OPTION, "import_")) {
    expr_t(string("import_(\"") + module + "\")").calc(*this);
    return true;
  }
  return false;
}

option_t<report_t> * report_t::lookup_option(const char * p)
{
  switch (*p) {
  case '%':
    OPT_CH(percent);
    break;
  case 'A':
    OPT_CH(average);
    break;
  case 'B':
    OPT_CH(basis);
    break;
  case 'C':
    OPT_CH(cleared);
    break;
  case 'D':
    OPT_CH(daily);
    break;
  case 'E':
    OPT_CH(empty);
    break;
  case 'F':
    OPT_CH(format_);
    break;
  case 'G':
    OPT_CH(gain);
    break;
  case 'I':
    OPT_CH(price);
    break;
  case 'J':
    OPT_CH(total_data);
    break;
  case 'L':
    OPT_CH(actual);
    break;
  case 'M':
    OPT_CH(monthly);
    break;
  case 'O':
    OPT_CH(quantity);
    break;
  case 'P':
    OPT_CH(by_payee);
    break;
  case 'R':
    OPT_CH(real);
    break;
  case 'S':
    OPT_CH(sort_);
    break;
  case 'T':
    OPT_CH(total_);
    break;
  case 'U':
    OPT_CH(uncleared);
    break;
  case 'V':
    OPT_CH(market);
    break;
  case 'W':
    OPT_CH(weekly);
    break;
  case 'X':
    OPT_CH(exchange_);
    break;
  case 'Y':
    OPT_CH(yearly);
    break;
  case 'a':
    OPT(abbrev_len_);
    else OPT_(account_);
    else OPT(actual);
    else OPT(actual_dates);
    else OPT(add_budget);
    else OPT(amount_);
    else OPT(amount_data);
    else OPT(anon);
    else OPT_ALT(color, ansi);
    else OPT(average);
    else OPT(account_width_);
    else OPT(amount_width_);
    break;
  case 'b':
    OPT(balance_format_);
    else OPT(base);
    else OPT_ALT(basis, cost);
    else OPT_(begin_);
    else OPT(budget);
    else OPT(by_payee);
    break;
  case 'c':
    OPT(csv_format_);
    else OPT(cleared);
    else OPT(collapse);
    else OPT(collapse_if_zero);
    else OPT(color);
    else OPT(columns_);
    else OPT_ALT(basis, cost);
    else OPT_(current);
    break;
  case 'd':
    OPT(daily);
    else OPT(date_format_);
    else OPT(datetime_format_);
    else OPT(depth_);
    else OPT(deviation);
    else OPT_(display_);
    else OPT(display_amount_);
    else OPT(display_total_);
    else OPT_ALT(dow, days-of-week);
    else OPT(date_width_);
    break;
  case 'e':
    OPT(effective);
    else OPT(empty);
    else OPT_(end_);
    else OPT(equity);
    else OPT(exact);
    else OPT(exchange_);
    break;
  case 'f':
    OPT(flat);
    else OPT_ALT(forecast_while_, forecast_);
    else OPT(forecast_years_);
    else OPT(format_);
    else OPT(force_color);
    else OPT(force_pager);
    else OPT_ALT(head_, first_);
    break;
  case 'g':
    OPT(gain);
    break;
  case 'h':
    OPT(head_);
    break;
  case 'i':
    OPT(invert);
    break;
  case 'j':
    OPT_CH(amount_data);
    break;
  case 'l':
    OPT_(limit_);
    else OPT(lot_dates);
    else OPT(lot_prices);
    else OPT(lot_tags);
    else OPT(lots);
    else OPT(lots_actual);
    else OPT_ALT(tail_, last_);
    break;
  case 'm':
    OPT(market);
    else OPT(monthly);
    break;
  case 'n':
    OPT_CH(collapse);
    else OPT(no_color);
    else OPT(no_total);
    else OPT(now_);
    break;
  case 'o':
    OPT(only_);
    else OPT_(output_);
    break;
  case 'p':
    OPT(pager_);
    else OPT(payee_);
    else OPT(pending);
    else OPT(percent);
    else OPT_(period_);
    else OPT_ALT(sort_xacts_, period_sort_);
    else OPT(plot_amount_format_);
    else OPT(plot_total_format_);
    else OPT(price);
    else OPT(prices_format_);
    else OPT(pricesdb_format_);
    else OPT(print_format_);
    else OPT(payee_width_);
    break;
  case 'q':
    OPT(quantity);
    else OPT(quarterly);
    break;
  case 'r':
    OPT(raw);
    else OPT(real);
    else OPT(register_format_);
    else OPT_(related);
    else OPT(related_all);
    else OPT(revalued);
    else OPT(revalued_only);
    else OPT(revalued_total_);
    break;
  case 's':
    OPT(sort_);
    else OPT(sort_all_);
    else OPT(sort_xacts_);
    else OPT_(subtotal);
    else OPT(start_of_week_);
    else OPT(seed_);
    break;
  case 't':
    OPT_CH(amount_);
    else OPT(tail_);
    else OPT(total_);
    else OPT(total_data);
    else OPT(truncate_);
    else OPT(total_width_);
    break;
  case 'u':
    OPT(unbudgeted);
    else OPT(uncleared);
    else OPT(unround);
    else OPT(unsorted);
    break;
  case 'w':
    OPT(weekly);
    else OPT_(wide);
    break;
  case 'y':
    OPT_CH(date_format_);
    else OPT(yearly);
    break;
  }
  return NULL;
}

void report_t::define(const symbol_t::kind_t kind, const string& name,
		      expr_t::ptr_op_t def)
{
  session.define(kind, name, def);
}

expr_t::ptr_op_t report_t::lookup(const symbol_t::kind_t kind,
				  const string& name)
{
  if (expr_t::ptr_op_t def = session.lookup(kind, name))
    return def;

  const char * p = name.c_str();

  switch (kind) {
  case symbol_t::FUNCTION:
    switch (*p) {
    case 'a':
      if (is_eq(p, "amount_expr"))
	return MAKE_FUNCTOR(report_t::fn_amount_expr);
      else if (is_eq(p, "ansify_if"))
	return MAKE_FUNCTOR(report_t::fn_ansify_if);
      else if (is_eq(p, "abs"))
	return MAKE_FUNCTOR(report_t::fn_abs);
      else if (is_eq(p, "account_total"))
	return MAKE_FUNCTOR(report_t::fn_account_total);
      break;

    case 'b':
      if (is_eq(p, "black"))
	return WRAP_FUNCTOR(fn_black);
      else if (is_eq(p, "blink"))
	return WRAP_FUNCTOR(fn_blink);
      else if (is_eq(p, "blue"))
	return WRAP_FUNCTOR(fn_blue);
      else if (is_eq(p, "bold"))
	return WRAP_FUNCTOR(fn_bold);
      break;

    case 'c':
      if (is_eq(p, "cyan"))
	return WRAP_FUNCTOR(fn_cyan);
      break;

    case 'd':
      if (is_eq(p, "display_amount"))
	return MAKE_FUNCTOR(report_t::fn_display_amount);
      else if (is_eq(p, "display_total"))
	return MAKE_FUNCTOR(report_t::fn_display_total);
      else if (is_eq(p, "date"))
	return MAKE_FUNCTOR(report_t::fn_now);
      break;

    case 'f':
      if (is_eq(p, "format_date"))
	return MAKE_FUNCTOR(report_t::fn_format_date);
      break;

    case 'g':
      if (is_eq(p, "get_at"))
	return MAKE_FUNCTOR(report_t::fn_get_at);
      else if (is_eq(p, "green"))
	return WRAP_FUNCTOR(fn_green);
      break;

    case 'i':
      if (is_eq(p, "is_seq"))
	return MAKE_FUNCTOR(report_t::fn_is_seq);
      break;

    case 'j':
      if (is_eq(p, "justify"))
	return MAKE_FUNCTOR(report_t::fn_justify);
      else if (is_eq(p, "join"))
	return MAKE_FUNCTOR(report_t::fn_join);
      break;

    case 'm':
      if (is_eq(p, "market"))
	return MAKE_FUNCTOR(report_t::fn_market);
      else if (is_eq(p, "magenta"))
	return WRAP_FUNCTOR(fn_magenta);
      break;

    case 'n':
      if (is_eq(p, "null"))
	return WRAP_FUNCTOR(fn_null);
      else if (is_eq(p, "now"))
	return MAKE_FUNCTOR(report_t::fn_now);
      break;

    case 'o':
      if (is_eq(p, "options"))
	return MAKE_FUNCTOR(report_t::fn_options);
      break;

    case 'p':
      if (is_eq(p, "post"))
	return WRAP_FUNCTOR(fn_false);
      else if (is_eq(p, "percent"))
	return MAKE_FUNCTOR(report_t::fn_percent);
      else if (is_eq(p, "price"))
	return MAKE_FUNCTOR(report_t::fn_price);
      break;

    case 'q':
      if (is_eq(p, "quoted"))
	return MAKE_FUNCTOR(report_t::fn_quoted);
      else if (is_eq(p, "quantity"))
	return MAKE_FUNCTOR(report_t::fn_quantity);
      break;

    case 'r':
      if (is_eq(p, "rounded"))
	return MAKE_FUNCTOR(report_t::fn_rounded);
      else if (is_eq(p, "red"))
	return WRAP_FUNCTOR(fn_red);
      break;

    case 's':
      if (is_eq(p, "scrub"))
	return MAKE_FUNCTOR(report_t::fn_scrub);
      else if (is_eq(p, "strip"))
	return MAKE_FUNCTOR(report_t::fn_strip);
      break;

    case 't':
      if (is_eq(p, "truncated"))
	return MAKE_FUNCTOR(report_t::fn_truncated);
      else if (is_eq(p, "total_expr"))
	return MAKE_FUNCTOR(report_t::fn_total_expr);
      else if (is_eq(p, "today"))
	return MAKE_FUNCTOR(report_t::fn_today);
      else if (is_eq(p, "t"))
	return MAKE_FUNCTOR(report_t::fn_display_amount);
      break;

    case 'T':
      if (is_eq(p, "T"))
	return MAKE_FUNCTOR(report_t::fn_display_total);
      break;

    case 'u':
      if (is_eq(p, "underline"))
	return WRAP_FUNCTOR(fn_underline);
      else if (is_eq(p, "unrounded"))
	return MAKE_FUNCTOR(report_t::fn_unrounded);
      break;

    case 'w':
      if (is_eq(p, "white"))
	return WRAP_FUNCTOR(fn_white);
      break;

    case 'y':
      if (is_eq(p, "yellow"))
	return WRAP_FUNCTOR(fn_yellow);
      break;
    }

    // Check if they are trying to access an option's setting or value.
    if (option_t<report_t> * handler = lookup_option(p))
      return MAKE_OPT_FUNCTOR(report_t, handler);
    break;

  case symbol_t::OPTION:
    if (option_t<report_t> * handler = lookup_option(p))
      return MAKE_OPT_HANDLER(report_t, handler);
    break;

  case symbol_t::COMMAND:
    switch (*p) {
    case 'b':
      if (*(p + 1) == '\0' || is_eq(p, "bal") || is_eq(p, "balance")) {
	return expr_t::op_t::wrap_functor
	  (reporter<account_t, acct_handler_ptr, &report_t::accounts_report>
	   (new format_accounts(*this, report_format(HANDLER(balance_format_))),
	    *this, "#balance"));
      }
      else if (is_eq(p, "budget")) {
	HANDLER(amount_).set_expr(string("#budget"), "(amount, 0)");

	budget_flags |= BUDGET_WRAP_VALUES;
	if (! (budget_flags & ~BUDGET_WRAP_VALUES))
	  budget_flags |= BUDGET_BUDGETED;

	return expr_t::op_t::wrap_functor
	  (reporter<account_t, acct_handler_ptr, &report_t::accounts_report>
	   (new format_accounts(*this, report_format(HANDLER(budget_format_))),
	    *this, "#budget"));
      }
      break;

    case 'c':
      if (is_eq(p, "csv")) {
	return WRAP_FUNCTOR
	  (reporter<>
	   (new format_posts(*this, report_format(HANDLER(csv_format_))),
	    *this, "#csv"));
      }
      else if (is_eq(p, "cleared")) {
	HANDLER(amount_).set_expr(string("#cleared"),
				  "(amount, cleared ? amount : 0)");

	return expr_t::op_t::wrap_functor
	  (reporter<account_t, acct_handler_ptr, &report_t::accounts_report>
	   (new format_accounts(*this, report_format(HANDLER(cleared_format_))),
	    *this, "#cleared"));
      }
      break;

    case 'e':
      if (is_eq(p, "equity"))
	return WRAP_FUNCTOR
	  (reporter<>
	   (new format_posts(*this, report_format(HANDLER(print_format_))),
	    *this, "#equity"));
      else if (is_eq(p, "entry"))
	return WRAP_FUNCTOR(xact_command);
      else if (is_eq(p, "emacs"))
	return WRAP_FUNCTOR
	  (reporter<>(new format_emacs_posts(output_stream), *this, "#emacs"));
      else if (is_eq(p, "echo"))
	return MAKE_FUNCTOR(report_t::echo_command);
      break;

    case 'p':
      if (*(p + 1) == '\0' || is_eq(p, "print"))
	return WRAP_FUNCTOR
	  (reporter<>
	   (new format_posts(*this, report_format(HANDLER(print_format_)),
			     HANDLED(raw)), *this, "#print"));
      else if (is_eq(p, "prices"))
	return expr_t::op_t::wrap_functor
	  (reporter<post_t, post_handler_ptr, &report_t::commodities_report>
	   (new format_posts(*this, report_format(HANDLER(prices_format_))),
	    *this, "#prices"));
      else if (is_eq(p, "pricesdb"))
	return expr_t::op_t::wrap_functor
	  (reporter<post_t, post_handler_ptr, &report_t::commodities_report>
	   (new format_posts(*this, report_format(HANDLER(pricesdb_format_))),
	    *this, "#pricesdb"));
      else if (is_eq(p, "python") && maybe_import("ledger.interp"))
	return session.lookup(symbol_t::COMMAND, "python");
      break;

    case 'r':
      if (*(p + 1) == '\0' || is_eq(p, "reg") || is_eq(p, "register"))
	return WRAP_FUNCTOR
	  (reporter<>
	   (new format_posts(*this, report_format(HANDLER(register_format_))),
	    *this, "#register"));
      else if (is_eq(p, "reload"))
	return MAKE_FUNCTOR(report_t::reload_command);
      break;

    case 's':
      if (is_eq(p, "stats") || is_eq(p, "stat"))
	return WRAP_FUNCTOR(report_statistics);
      else
	if (is_eq(p, "server") && maybe_import("ledger.server"))
	  return session.lookup(symbol_t::COMMAND, "server");
      break;

    case 'x':
      if (is_eq(p, "xact"))
	return WRAP_FUNCTOR(xact_command);
      break;
    }
    break;

  case symbol_t::PRECOMMAND:
    switch (*p) {
    case 'a':
      if (is_eq(p, "args"))
	return WRAP_FUNCTOR(args_command);
      break;
    case 'e':
      if (is_eq(p, "eval"))
	return WRAP_FUNCTOR(eval_command);
      break;
    case 'f':
      if (is_eq(p, "format"))
	return WRAP_FUNCTOR(format_command);
      break;
    case 'g':
      if (is_eq(p, "generate"))
	return expr_t::op_t::wrap_functor
	  (reporter<post_t, post_handler_ptr, &report_t::generate_report>
	   (new format_posts(*this, report_format(HANDLER(print_format_)),
			     false), *this, "#generate"));
    case 'h':
      if (is_eq(p, "hello") && maybe_import("ledger.hello"))
	return session.lookup(symbol_t::PRECOMMAND, "hello");
      break;
    case 'p':
      if (is_eq(p, "parse"))
	return WRAP_FUNCTOR(parse_command);
      else if (is_eq(p, "period"))
	return WRAP_FUNCTOR(period_command);
      break;
    case 't':
      if (is_eq(p, "template"))
	return WRAP_FUNCTOR(template_command);
      break;
    }
    break;

  default:
    break;
  }

  return NULL;
}

} // namespace ledger
