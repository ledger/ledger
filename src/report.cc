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
#include "pool.h"
#include "format.h"
#include "query.h"
#include "output.h"
#include "iterators.h"
#include "filters.h"
#include "precmd.h"
#include "stats.h"
#include "generate.h"
#include "draft.h"
#include "xml.h"
#include "emacs.h"

namespace ledger {

void report_t::normalize_options(const string& verb)
{
  // Patch up some of the reporting options based on what kind of
  // command it was.

#ifdef HAVE_ISATTY
  if (! HANDLED(force_color)) {
    if (! HANDLED(no_color) && isatty(STDOUT_FILENO))
      HANDLER(color).on_only(string("?normalize"));
    if (HANDLED(color) && ! isatty(STDOUT_FILENO))
      HANDLER(color).off();
  }
  if (! HANDLED(force_pager)) {
    if (HANDLED(pager_) && ! isatty(STDOUT_FILENO))
      HANDLER(pager_).off();
  }
#endif

  item_t::use_effective_date = (HANDLED(effective) &&
				! HANDLED(actual_dates));

  session.journal->commodity_pool->keep_base  = HANDLED(base);
  session.journal->commodity_pool->get_quotes = session.HANDLED(download);

  if (session.HANDLED(price_exp_))
    session.journal->commodity_pool->quote_leeway =
      session.HANDLER(price_exp_).value.as_long();

  if (session.HANDLED(price_db_))
    session.journal->commodity_pool->price_db =
      session.HANDLER(price_db_).str();
  else
    session.journal->commodity_pool->price_db = none;

  if (HANDLED(date_format_))
    set_date_format(HANDLER(date_format_).str().c_str());
  if (HANDLED(datetime_format_))
    set_datetime_format(HANDLER(datetime_format_).str().c_str());
  if (HANDLED(start_of_week_)) {
    if (optional<date_time::weekdays> weekday =
	string_to_day_of_week(HANDLER(start_of_week_).str()))
      start_of_week = *weekday;
  }

  if (verb == "print" || verb == "xact" || verb == "dump") {
    HANDLER(related).on_only(string("?normalize"));
    HANDLER(related_all).on_only(string("?normalize"));
  }
  else if (verb == "equity") {
    HANDLER(equity).on_only(string("?normalize"));
  }

  if (verb == "print")
    HANDLER(limit_).on(string("?normalize"), "actual");

  if (! HANDLED(empty))
    HANDLER(display_).on(string("?normalize"), "amount|(!post&total)");

  if (verb[0] != 'b' && verb[0] != 'r')
    HANDLER(base).on_only(string("?normalize"));

  // If a time period was specified with -p, check whether it also gave a
  // begin and/or end to the report period (though these can be overridden
  // using -b or -e).  Then, if no _duration_ was specified (such as monthly),
  // then ignore the period since the begin/end are the only interesting
  // details.
  if (HANDLED(period_)) {
    if (! HANDLED(sort_all_))
      HANDLER(sort_xacts_).on_only(string("?normalize"));

    date_interval_t interval(HANDLER(period_).str());

    if (! HANDLED(begin_) && interval.start) {
      string predicate =
	"date>=[" + to_iso_extended_string(*interval.start) + "]";
      HANDLER(limit_).on(string("?normalize"), predicate);
    }
    if (! HANDLED(end_) && interval.finish) {
      string predicate =
	"date<[" + to_iso_extended_string(*interval.finish) + "]";
      HANDLER(limit_).on(string("?normalize"), predicate);
    }

    if (! interval.duration)
      HANDLER(period_).off();
  }

  // If -j or -J were specified, set the appropriate format string now so as
  // to avoid option ordering issues were we to have done it during the
  // initial parsing of the options.
  if (HANDLED(amount_data)) {
    HANDLER(format_)
      .on_with(string("?normalize"), HANDLER(plot_amount_format_).value);
  }
  else if (HANDLED(total_data)) {
    HANDLER(format_)
      .on_with(string("?normalize"), HANDLER(plot_total_format_).value);
  }

  // If the --exchange (-X) option was used, parse out any final price
  // settings that may be there.
  if (HANDLED(exchange_) &&
      HANDLER(exchange_).str().find('=') != string::npos) {
    value_t(0L).exchange_commodities(HANDLER(exchange_).str(), true,
				     terminus);
  }

  long cols = 0;
  if (HANDLED(columns_))
    cols = HANDLER(columns_).value.to_long();
  else if (const char * columns = std::getenv("COLUMNS"))
    cols = lexical_cast<long>(columns);
  else
    cols = 80L;

  if (cols > 0) {
    DEBUG("auto.columns", "cols = " << cols);

    if (! HANDLER(date_width_).specified)
      HANDLER(date_width_)
	.on_with(none, static_cast<long>(format_date(CURRENT_DATE(),
						     FMT_PRINTED).length()));

    long date_width    = HANDLER(date_width_).value.to_long();
    long payee_width   = (HANDLER(payee_width_).specified ?
			  HANDLER(payee_width_).value.to_long() :
			  int(double(cols) * 0.263157));
    long account_width = (HANDLER(account_width_).specified ?
			  HANDLER(account_width_).value.to_long() :
			  int(double(cols) * 0.302631));
    long amount_width  = (HANDLER(amount_width_).specified ?
			  HANDLER(amount_width_).value.to_long() :
			  int(double(cols) * 0.157894));
    long total_width   = (HANDLER(total_width_).specified ?
			  HANDLER(total_width_).value.to_long() :
			  amount_width);

    DEBUG("auto.columns", "date_width	 = " << date_width);
    DEBUG("auto.columns", "payee_width	 = " << payee_width);
    DEBUG("auto.columns", "account_width = " << account_width);
    DEBUG("auto.columns", "amount_width	 = " << amount_width);
    DEBUG("auto.columns", "total_width	 = " << total_width);

    if (! HANDLER(date_width_).specified &&
	! HANDLER(payee_width_).specified &&
	! HANDLER(account_width_).specified &&
	! HANDLER(amount_width_).specified &&
	! HANDLER(total_width_).specified) {
      long total = (4 /* the spaces between */ + date_width + payee_width +
		    account_width + amount_width + total_width);
      if (total > cols) {
	DEBUG("auto.columns", "adjusting account down");
	account_width -= total - cols;
	DEBUG("auto.columns", "account_width now = " << account_width);
      }
    }

    if (! HANDLER(date_width_).specified)
      HANDLER(date_width_).on_with(string("?normalize"), date_width);
    if (! HANDLER(payee_width_).specified)
      HANDLER(payee_width_).on_with(string("?normalize"), payee_width);
    if (! HANDLER(account_width_).specified)
      HANDLER(account_width_).on_with(string("?normalize"), account_width);
    if (! HANDLER(amount_width_).specified)
      HANDLER(amount_width_).on_with(string("?normalize"), amount_width);
    if (! HANDLER(total_width_).specified)
      HANDLER(total_width_).on_with(string("?normalize"), total_width);
  }
}

void report_t::parse_query_args(const value_t& args, const string& whence)
{
  query_t query(args, what_to_keep());
  if (! query)
    throw_(std::runtime_error,
	   _("Invalid query predicate: %1") << query.text());

  HANDLER(limit_).on(whence, query.text());

  DEBUG("report.predicate",
	"Predicate = " << HANDLER(limit_).str());

  if (query.tokens_remaining()) {
    query.parse_again();
    if (! query)
      throw_(std::runtime_error,
	     _("Invalid display predicate: %1") << query.text());

    HANDLER(display_).on(whence, query.text());

    DEBUG("report.predicate",
	  "Display predicate = " << HANDLER(display_).str());
  }
}  

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
		       predicate_t(HANDLER(display_).str(), what_to_keep()),
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

value_t report_t::fn_floor(call_scope_t& scope)
{
  interactive_t args(scope, "v");
  return args.value_at(0).floored();
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

value_t report_t::fn_to_boolean(call_scope_t& scope)
{
  interactive_t args(scope, "v");
  args.value_at(0).in_place_cast(value_t::BOOLEAN);
  return args.value_at(0);
}

value_t report_t::fn_to_int(call_scope_t& scope)
{
  interactive_t args(scope, "v");
  args.value_at(0).in_place_cast(value_t::INTEGER);
  return args.value_at(0);
}

value_t report_t::fn_to_datetime(call_scope_t& scope)
{
  interactive_t args(scope, "v");
  args.value_at(0).in_place_cast(value_t::DATETIME);
  return args.value_at(0);
}

value_t report_t::fn_to_date(call_scope_t& scope)
{
  interactive_t args(scope, "v");
  args.value_at(0).in_place_cast(value_t::DATE);
  return args.value_at(0);
}

value_t report_t::fn_to_amount(call_scope_t& scope)
{
  interactive_t args(scope, "v");
  args.value_at(0).in_place_cast(value_t::AMOUNT);
  return args.value_at(0);
}

value_t report_t::fn_to_balance(call_scope_t& scope)
{
  interactive_t args(scope, "v");
  args.value_at(0).in_place_cast(value_t::BALANCE);
  return args.value_at(0);
}

value_t report_t::fn_to_string(call_scope_t& scope)
{
  interactive_t args(scope, "v");
  args.value_at(0).in_place_cast(value_t::STRING);
  return args.value_at(0);
}

value_t report_t::fn_to_mask(call_scope_t& scope)
{
  interactive_t args(scope, "v");
  args.value_at(0).in_place_cast(value_t::MASK);
  return args.value_at(0);
}

value_t report_t::fn_to_sequence(call_scope_t& scope)
{
  interactive_t args(scope, "v");
  args.value_at(0).in_place_cast(value_t::SEQUENCE);
  return args.value_at(0);
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
    else OPT(date_);
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
    else OPT(pricedb_format_);
    else OPT(print_format_);
    else OPT(payee_width_);
    else OPT(prepend_format_);
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
    // Support 2.x's single-letter value expression names.
    if (*(p + 1) == '\0') {
      switch (*p) {
      case 'd':
      case 'm':
	return MAKE_FUNCTOR(report_t::fn_now);
      case 'P':
	return MAKE_FUNCTOR(report_t::fn_market);
      case 't':
	return MAKE_FUNCTOR(report_t::fn_display_amount);
      case 'T':
	return MAKE_FUNCTOR(report_t::fn_display_total);
      case 'U':
	return MAKE_FUNCTOR(report_t::fn_abs);
      case 'S':
	return MAKE_FUNCTOR(report_t::fn_strip);
      case 'i':
	throw_(std::runtime_error,
	       _("The i value expression variable is no longer supported"));
      case 'A':
	throw_(std::runtime_error,
	       _("The A value expression variable is no longer supported"));
      case 'v':
      case 'V':
	throw_(std::runtime_error,
	       _("The V and v value expression variables are no longer supported"));
      case 'I':
      case 'B':
	throw_(std::runtime_error,
	       _("The I and B value expression variables are no longer supported"));
      case 'g':
      case 'G':
	throw_(std::runtime_error,
	       _("The G and g value expression variables are no longer supported"));
      default:
	return NULL;
      }
    }

    switch (*p) {
    case 'a':
      if (is_eq(p, "amount_expr"))
	return MAKE_FUNCTOR(report_t::fn_amount_expr);
      else if (is_eq(p, "ansify_if"))
	return MAKE_FUNCTOR(report_t::fn_ansify_if);
      else if (is_eq(p, "abs"))
	return MAKE_FUNCTOR(report_t::fn_abs);
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
      else if (is_eq(p, "floor"))
	return MAKE_FUNCTOR(report_t::fn_floor);
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
      else if (is_eq(p, "to_boolean"))
	return MAKE_FUNCTOR(report_t::fn_to_boolean);
      else if (is_eq(p, "to_int"))
	return MAKE_FUNCTOR(report_t::fn_to_int);
      else if (is_eq(p, "to_datetime"))
	return MAKE_FUNCTOR(report_t::fn_to_datetime);
      else if (is_eq(p, "to_date"))
	return MAKE_FUNCTOR(report_t::fn_to_date);
      else if (is_eq(p, "to_amount"))
	return MAKE_FUNCTOR(report_t::fn_to_amount);
      else if (is_eq(p, "to_balance"))
	return MAKE_FUNCTOR(report_t::fn_to_balance);
      else if (is_eq(p, "to_string"))
	return MAKE_FUNCTOR(report_t::fn_to_string);
      else if (is_eq(p, "to_mask"))
	return MAKE_FUNCTOR(report_t::fn_to_mask);
      else if (is_eq(p, "to_sequence"))
	return MAKE_FUNCTOR(report_t::fn_to_sequence);
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
	   (new format_accounts(*this, report_format(HANDLER(balance_format_)),
				maybe_format(HANDLER(prepend_format_))),
	    *this, "#balance"));
      }
      else if (is_eq(p, "budget")) {
	HANDLER(amount_).set_expr(string("#budget"), "(amount, 0)");

	budget_flags |= BUDGET_WRAP_VALUES;
	if (! (budget_flags & ~BUDGET_WRAP_VALUES))
	  budget_flags |= BUDGET_BUDGETED;

	return expr_t::op_t::wrap_functor
	  (reporter<account_t, acct_handler_ptr, &report_t::accounts_report>
	   (new format_accounts(*this, report_format(HANDLER(budget_format_)),
				maybe_format(HANDLER(prepend_format_))),
	    *this, "#budget"));
      }
      break;

    case 'c':
      if (is_eq(p, "csv")) {
	return WRAP_FUNCTOR
	  (reporter<>
	   (new format_posts(*this, report_format(HANDLER(csv_format_)),
			     maybe_format(HANDLER(prepend_format_))),
	    *this, "#csv"));
      }
      else if (is_eq(p, "cleared")) {
	HANDLER(amount_).set_expr(string("#cleared"),
				  "(amount, cleared ? amount : 0)");

	return expr_t::op_t::wrap_functor
	  (reporter<account_t, acct_handler_ptr, &report_t::accounts_report>
	   (new format_accounts(*this, report_format(HANDLER(cleared_format_)),
				maybe_format(HANDLER(prepend_format_))),
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
	   (new format_posts(*this, report_format(HANDLER(prices_format_)),
			     maybe_format(HANDLER(prepend_format_))),
	    *this, "#prices"));
      else if (is_eq(p, "pricedb"))
	return expr_t::op_t::wrap_functor
	  (reporter<post_t, post_handler_ptr, &report_t::commodities_report>
	   (new format_posts(*this, report_format(HANDLER(pricedb_format_)),
			     maybe_format(HANDLER(prepend_format_))),
	    *this, "#pricedb"));
      break;

    case 'r':
      if (*(p + 1) == '\0' || is_eq(p, "reg") || is_eq(p, "register"))
	return WRAP_FUNCTOR
	  (reporter<>
	   (new format_posts(*this, report_format(HANDLER(register_format_)),
			     false, maybe_format(HANDLER(prepend_format_))),
	    *this, "#register"));
      else if (is_eq(p, "reload"))
	return MAKE_FUNCTOR(report_t::reload_command);
      break;

    case 's':
      if (is_eq(p, "stats") || is_eq(p, "stat"))
	return WRAP_FUNCTOR(report_statistics);
      break;

    case 'x':
      if (is_eq(p, "xact"))
	return WRAP_FUNCTOR(xact_command);
      else if (is_eq(p, "xml"))
	return WRAP_FUNCTOR(reporter<>(new format_xml(*this), *this, "#xml"));
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
