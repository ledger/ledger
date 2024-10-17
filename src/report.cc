/*
 * Copyright (c) 2003-2023, John Wiegley.  All rights reserved.
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
#include "print.h"
#include "iterators.h"
#include "filters.h"
#include "precmd.h"
#include "select.h"
#include "stats.h"
#include "generate.h"
#include "draft.h"
#include "convert.h"
#include "ptree.h"
#include "emacs.h"

namespace ledger {

void report_t::normalize_options(const string& verb)
{
  // Patch up some of the reporting options based on what kind of
  // command it was.

#if HAVE_ISATTY
  if (! HANDLED(force_color)) {
    if (! HANDLED(no_color) && isatty(STDOUT_FILENO))
      HANDLER(color).on("?normalize");
    if (HANDLED(color) && ! isatty(STDOUT_FILENO))
      HANDLER(color).off();
  }
  else {
    HANDLER(color).on("?normalize");
  }
  if (! HANDLED(force_pager)) {
    if (HANDLED(pager_) && ! isatty(STDOUT_FILENO))
      HANDLER(pager_).off();
  }
#endif

  if (HANDLED(output_)) {
    if (HANDLED(color) && ! HANDLED(force_color))
      HANDLER(color).off();
    if (HANDLED(pager_) && ! HANDLED(force_pager))
      HANDLER(pager_).off();
  }

  item_t::use_aux_date = (HANDLED(aux_date) && ! HANDLED(primary_date));

  commodity_pool_t::current_pool->keep_base  = HANDLED(base);
  commodity_pool_t::current_pool->get_quotes = session.HANDLED(download);

  if (session.HANDLED(price_exp_))
    commodity_pool_t::current_pool->quote_leeway =
      lexical_cast<long>(session.HANDLER(price_exp_).value) * 3600L;

  if (session.HANDLED(price_db_))
    commodity_pool_t::current_pool->price_db = session.HANDLER(price_db_).str();
  else
    commodity_pool_t::current_pool->price_db = none;

  if (HANDLED(date_format_))
    set_date_format(HANDLER(date_format_).str().c_str());
  if (HANDLED(datetime_format_))
    set_datetime_format(HANDLER(datetime_format_).str().c_str());
  if (HANDLED(start_of_week_)) {
    if (optional<date_time::weekdays> weekday =
        string_to_day_of_week(HANDLER(start_of_week_).str()))
      start_of_week = *weekday;
  }

  long meta_width = -1;

  if (! HANDLED(prepend_format_) && HANDLED(meta_)) {
    if (! HANDLED(meta_width_)) {
      string::size_type i = HANDLER(meta_).str().find(':');
      if (i != string::npos) {
        HANDLED(meta_width_).on("?normalize",
                                string(HANDLER(meta_).str(), i + 1));
        HANDLED(meta_).on("?normalize",
                          string(HANDLER(meta_).str(), 0, i));
      }
    }
    if (HANDLED(meta_width_)) {
      HANDLER(prepend_format_)
        .on("?normalize", string("%(justify(truncated(tag(\"") +
            HANDLER(meta_).str() + "\"), " +
            HANDLED(meta_width_).value + " - 1), " +
            HANDLED(meta_width_).value + "))");
      meta_width = lexical_cast<long>(HANDLED(meta_width_).value);
    } else {
      HANDLER(prepend_format_)
        .on("?normalize", string("%(tag(\"") + HANDLER(meta_).str() + "\"))");
    }
  }

  if (verb == "print" || verb == "xact" || verb == "dump") {
    HANDLER(related_all).parent = this;
    HANDLER(related_all).on("?normalize");
  }
  else if (verb == "equity") {
    HANDLER(equity).on("?normalize");
  }

  if (verb[0] != 'b' && verb[0] != 'r')
    HANDLER(base).on("?normalize");

  // If a time period was specified with -p, check whether it also gave a
  // begin and/or end to the report period (though these can be overridden
  // using -b or -e).  Then, if no _duration_ was specified (such as monthly),
  // then ignore the period since the begin/end are the only interesting
  // details.
  if (HANDLED(period_))
    normalize_period();

  // If -j or -J were specified, set the appropriate format string now so as
  // to avoid option ordering issues were we to have done it during the
  // initial parsing of the options.
  if (HANDLED(amount_data)) {
    HANDLER(format_).on("?normalize", HANDLER(plot_amount_format_).value);
  }
  else if (HANDLED(total_data)) {
    HANDLER(format_).on("?normalize", HANDLER(plot_total_format_).value);
  }

  // If the --exchange (-X) option was used, parse out any final price
  // settings that may be there.
  if (HANDLED(exchange_) &&
      HANDLER(exchange_).str().find('=') != string::npos) {
    value_t(0L).exchange_commodities(HANDLER(exchange_).str(), true,
                                     terminus);
  }

  if (HANDLED(percent)) {
    commodity_t::decimal_comma_by_default = false;
    if (HANDLED(market)) {
      HANDLER(total_)
        .on("?normalize",
            "(__tmp = market(parent.total, value_date, exchange);"
            " ((is_account & parent & __tmp) ?"
            "   percent(scrub(market(total, value_date, exchange)), "
            "           scrub(__tmp)) : 0))");
    }
  }

  if (HANDLED(immediate) && HANDLED(market)) {
    HANDLER(amount_)
      .on("?normalize", "market(amount_expr, value_date, exchange)");
  }

  long cols = 0;
#if HAVE_IOCTL
  struct winsize ws;
#endif
  if (HANDLED(columns_))
    cols = lexical_cast<long>(HANDLER(columns_).value);
  else if (const char * columns = std::getenv("COLUMNS"))
    cols = lexical_cast<long>(columns);
#if HAVE_IOCTL
  else if (ioctl(STDIN_FILENO, TIOCGWINSZ, &ws) != -1)
      cols = ws.ws_col;
#endif
  else
    cols = 80L;

  if (meta_width > 0)
    cols -= meta_width;

  if (cols > 0) {
    DEBUG("auto.columns", "cols = " << cols);

    long date_width    = (HANDLED(date_width_) ?
                          lexical_cast<long>(HANDLER(date_width_).str()) :
                          static_cast<long>
                          (format_date(CURRENT_DATE(),FMT_PRINTED).length()));
    long payee_width   = (HANDLED(payee_width_) ?
                          lexical_cast<long>(HANDLER(payee_width_).str()) :
                          long(double(cols) * 0.263157));
    long account_width = (HANDLED(account_width_) ?
                          lexical_cast<long>(HANDLER(account_width_).str()) :
                          long(double(cols) * 0.302631));
    long amount_width  = (HANDLED(amount_width_) ?
                          lexical_cast<long>(HANDLER(amount_width_).str()) :
                          long(double(cols) * 0.157894));
    long total_width   = (HANDLED(total_width_) ?
                          lexical_cast<long>(HANDLER(total_width_).str()) :
                          amount_width);

    DEBUG("auto.columns", "date_width    = " << date_width);
    DEBUG("auto.columns", "payee_width   = " << payee_width);
    DEBUG("auto.columns", "account_width = " << account_width);
    DEBUG("auto.columns", "amount_width  = " << amount_width);
    DEBUG("auto.columns", "total_width   = " << total_width);

    if (! HANDLED(date_width_) &&
        ! HANDLED(payee_width_) &&
        ! HANDLED(account_width_) &&
        ! HANDLED(amount_width_) &&
        ! HANDLED(total_width_)) {
      long total = (4 /* the spaces between */ + date_width + payee_width +
                    account_width + amount_width + total_width +
                    (HANDLED(dc) ? 1 + amount_width : 0));
      while (total > cols && account_width > 5 && payee_width > 5) {
        DEBUG("auto.columns", "adjusting account down");
        if (total > cols) {
          --account_width;
          --total;
          if (total > cols) {
            --account_width;
            --total;
          }
        }
        if (total > cols) {
          --payee_width;
          --total;
        }
        DEBUG("auto.columns", "account_width now = " << account_width);
      }
    }

    if (! HANDLED(meta_width_))
      HANDLER(meta_width_).value    = "0";
    if (! HANDLED(prepend_width_))
      HANDLER(prepend_width_).value = "0";
    if (! HANDLED(date_width_))
      HANDLER(date_width_).value    = to_string(date_width);
    if (! HANDLED(payee_width_))
      HANDLER(payee_width_).value   = to_string(payee_width);
    if (! HANDLED(account_width_))
      HANDLER(account_width_).value = to_string(account_width);
    if (! HANDLED(amount_width_))
      HANDLER(amount_width_).value  = to_string(amount_width);
    if (! HANDLED(total_width_))
      HANDLER(total_width_).value   = to_string(total_width);
  }
}

void report_t::normalize_period()
{
  date_interval_t interval(HANDLER(period_).str());

  optional<date_t> begin = interval.begin();
  optional<date_t> end   = interval.end();

  if (! HANDLED(begin_) && begin) {
    string predicate = "date>=[" + to_iso_extended_string(*begin) + "]";
    HANDLER(limit_).on(string("?normalize"), predicate);
  }
  if (! HANDLED(end_) && end) {
    string predicate = "date<[" + to_iso_extended_string(*end) + "]";
    HANDLER(limit_).on(string("?normalize"), predicate);
  }

  if (! interval.duration)
    HANDLER(period_).off();
  else if (! HANDLED(sort_all_))
    HANDLER(sort_xacts_).on("?normalize");
}

void report_t::parse_query_args(const value_t& args, const string& whence)
{
  query_t query(args, what_to_keep());

  if (query.has_query(query_t::QUERY_LIMIT)) {
    HANDLER(limit_).on(whence, query.get_query(query_t::QUERY_LIMIT));
    DEBUG("report.predicate", "Limit predicate   = " << HANDLER(limit_).str());
  }

  if (query.has_query(query_t::QUERY_ONLY)) {
    HANDLER(only_).on(whence, query.get_query(query_t::QUERY_ONLY));
    DEBUG("report.predicate", "Only predicate    = " << HANDLER(only_).str());
  }

  if (query.has_query(query_t::QUERY_SHOW)) {
    HANDLER(display_).on(whence, query.get_query(query_t::QUERY_SHOW));
    DEBUG("report.predicate", "Display predicate = " << HANDLER(display_).str());
  }

  if (query.has_query(query_t::QUERY_BOLD)) {
    HANDLER(bold_if_).on(whence, query.get_query(query_t::QUERY_BOLD));
    DEBUG("report.predicate", "Bolding predicate = " << HANDLER(bold_if_).str());
  }

  if (query.has_query(query_t::QUERY_FOR)) {
    HANDLER(period_).on(whence, query.get_query(query_t::QUERY_FOR));
    DEBUG("report.predicate", "Report period     = " << HANDLER(period_).str());

    normalize_period();         // it needs normalization
  }
}

namespace {
  struct posts_flusher
  {
    post_handler_ptr handler;
    report_t&        report;

    posts_flusher(post_handler_ptr _handler, report_t& _report)
      : handler(_handler), report(_report) {
      TRACE_CTOR(posts_flusher, "post_handler_ptr, report_t&");
    }
    ~posts_flusher() throw() {
      TRACE_DTOR(posts_flusher);
    }

    void operator()(const value_t&) {
      report.session.journal->clear_xdata();
    }
  };
}

void report_t::posts_report(post_handler_ptr handler)
{
  handler = chain_post_handlers(handler, *this);
  if (HANDLED(group_by_)) {
    unique_ptr<post_splitter>
      splitter(new post_splitter(handler, *this, HANDLER(group_by_).expr));
    splitter->set_postflush_func(posts_flusher(handler, *this));
    handler = post_handler_ptr(splitter.release());
  }
  handler = chain_pre_post_handlers(handler, *this);

  journal_posts_iterator walker(*session.journal.get());
  pass_down_posts<journal_posts_iterator>(handler, walker);

  if (! HANDLED(group_by_))
    posts_flusher(handler, *this)(value_t());
}

void report_t::generate_report(post_handler_ptr handler)
{
  handler = chain_handlers(handler, *this);

  generate_posts_iterator walker
    (session, HANDLED(seed_) ?
     lexical_cast<unsigned int>(HANDLER(seed_).str()) : 0,
     HANDLED(head_) ?
     lexical_cast<unsigned int>(HANDLER(head_).str()) : 50);

  pass_down_posts<generate_posts_iterator>(handler, walker);
}

void report_t::xact_report(post_handler_ptr handler, xact_t& xact)
{
  handler = chain_handlers(handler, *this);

  xact_posts_iterator walker(xact);
  pass_down_posts<xact_posts_iterator>(handler, walker);

  xact.clear_xdata();
}

namespace {
  struct accounts_title_printer
  {
    acct_handler_ptr handler;
    report_t&        report;

    accounts_title_printer(acct_handler_ptr _handler, report_t& _report)
      : handler(_handler), report(_report) {}

    void operator()(const value_t& val)
    {
      if (! report.HANDLED(no_titles)) {
        std::ostringstream buf;
        val.print(buf);
        handler->title(buf.str());
      }
    }
  };

  struct accounts_flusher
  {
    acct_handler_ptr handler;
    report_t&        report;

    accounts_flusher(acct_handler_ptr _handler, report_t& _report)
      : handler(_handler), report(_report) {}

    void operator()(const value_t&)
    {
      report.HANDLER(amount_).expr.mark_uncompiled();
      report.HANDLER(total_).expr.mark_uncompiled();
      report.HANDLER(display_amount_).expr.mark_uncompiled();
      report.HANDLER(display_total_).expr.mark_uncompiled();
      report.HANDLER(revalued_total_).expr.mark_uncompiled();

      if (report.HANDLED(display_)) {
        DEBUG("report.predicate",
              "Display predicate = " << report.HANDLER(display_).str());
        if (! report.HANDLED(sort_)) {
          basic_accounts_iterator iter(*report.session.journal->master);
          pass_down_accounts<basic_accounts_iterator>
            (handler, iter, predicate_t(report.HANDLER(display_).str(),
                                        report.what_to_keep()), report);
        } else {
          expr_t sort_expr(report.HANDLER(sort_).str());
          sort_expr.set_context(&report);
          sorted_accounts_iterator iter(
            *report.session.journal->master, sort_expr, report,
            report.HANDLED(flat));
          pass_down_accounts<sorted_accounts_iterator>
            (handler, iter, predicate_t(report.HANDLER(display_).str(),
                                        report.what_to_keep()), report);
        }
      } else {
        if (! report.HANDLED(sort_)) {
          basic_accounts_iterator iter(*report.session.journal->master);
          pass_down_accounts<basic_accounts_iterator>(handler, iter);
        } else {
          expr_t sort_expr(report.HANDLER(sort_).str());
          sort_expr.set_context(&report);
          sorted_accounts_iterator iter(
            *report.session.journal->master, sort_expr, report,
            report.HANDLED(flat));
          pass_down_accounts<sorted_accounts_iterator>(handler, iter);
        }
      }

      report.session.journal->clear_xdata();
    }
  };
}

void report_t::accounts_report(acct_handler_ptr handler)
{
  post_handler_ptr chain =
    chain_post_handlers(post_handler_ptr(new ignore_posts), *this,
                        /* for_accounts_report= */ true);
  if (HANDLED(group_by_)) {
    unique_ptr<post_splitter>
      splitter(new post_splitter(chain, *this, HANDLER(group_by_).expr));

    splitter->set_preflush_func(accounts_title_printer(handler, *this));
    splitter->set_postflush_func(accounts_flusher(handler, *this));

    chain = post_handler_ptr(splitter.release());
  }
  chain = chain_pre_post_handlers(chain, *this);

  // The lifetime of the chain object controls the lifetime of all temporary
  // objects created within it during the call to pass_down_posts, which will
  // be needed later by the pass_down_accounts.
  journal_posts_iterator walker(*session.journal.get());
  pass_down_posts<journal_posts_iterator>(chain, walker);

  if (! HANDLED(group_by_))
    accounts_flusher(handler, *this)(value_t());
}

void report_t::commodities_report(post_handler_ptr handler)
{
  handler = chain_handlers(handler, *this);

  posts_commodities_iterator * walker(new posts_commodities_iterator(*session.journal.get()));
  try {
    pass_down_posts<posts_commodities_iterator>(handler, *walker);
  }
  catch (...) {
    IF_VERIFY() {
      // If --verify was used, clean up the posts_commodities_iterator.
      // Otherwise, just leak like a sieve.
      checked_delete(walker);
    }
    throw;
  }

  session.journal->clear_xdata();
}

value_t report_t::display_value(const value_t& val)
{
  value_t temp(val.strip_annotations(what_to_keep()));
  if (HANDLED(base))
    return temp;
  else
    return temp.unreduced();
}

namespace {
  value_t top_amount(const value_t& val)
  {
    switch (val.type()) {
    case value_t::BALANCE:
      return (*val.as_balance().amounts.begin()).second;

    case value_t::SEQUENCE: {
      return top_amount(*val.as_sequence().begin());
    }

    default:
      return val;
    }
  }
}

value_t report_t::fn_top_amount(call_scope_t& args)
{
  return top_amount(args[0]);
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

value_t report_t::fn_should_bold(call_scope_t& scope)
{
  if (HANDLED(bold_if_))
    return HANDLER(bold_if_).expr.calc(scope);
  else
    return false;
}

value_t report_t::fn_averaged_lots(call_scope_t& args)
{
  if (args.has<balance_t>(0))
    return average_lot_prices(args.get<balance_t>(0));
  else
    return args[0];
}

value_t report_t::fn_market(call_scope_t& args)
{
  value_t result;
  value_t arg0 = args[0];

  datetime_t moment;
  if (args.has<datetime_t>(1))
    moment = args.get<datetime_t>(1);

  if (arg0.is_string()) {
    amount_t tmp(1L);
    commodity_t * commodity =
      commodity_pool_t::current_pool->find_or_create(arg0.as_string());
    tmp.set_commodity(*commodity);
    arg0 = tmp;
  }

  string target_commodity;
  if (args.has<string>(2))
    target_commodity = args.get<string>(2);

  if (! target_commodity.empty())
    result = arg0.exchange_commodities(target_commodity,
                                       /* add_prices= */ false, moment);
  else
    result = arg0.value(moment);

  return ! result.is_null() ? result : arg0;
}

value_t report_t::fn_get_at(call_scope_t& args)
{
  std::size_t index = static_cast<std::size_t>(args.get<long>(1));
  if (index == 0) {
    if (! args[0].is_sequence())
      return args[0];
  }
  else if (! args[0].is_sequence()) {
    throw_(std::runtime_error,
           _f("Attempting to get argument at index %1% from %2%")
           % index % args[0].label());
  }

  value_t::sequence_t& seq(args[0].as_sequence_lval());
  if (index >= seq.size())
    throw_(std::runtime_error,
           _f("Attempting to get index %1% from %2% with %3% elements")
           % index % args[0].label() % seq.size());

  return seq[index];
}

value_t report_t::fn_is_seq(call_scope_t& scope)
{
  return scope.value().is_sequence();
}

value_t report_t::fn_strip(call_scope_t& args)
{
  return args.value().strip_annotations(what_to_keep());
}

value_t report_t::fn_trim(call_scope_t& args)
{
  string             temp(args.value().to_string());
  scoped_array<char> buf(new char[temp.length() + 1]);
  std::strcpy(buf.get(), temp.c_str());

  const char * p = buf.get();
  const char * e = buf.get() + temp.length() - 1;

  while (p <= e && std::isspace(static_cast<unsigned char>(*p)))
    p++;

  while (e > p && std::isspace(static_cast<unsigned char>(*e)))
    e--;

  if (p > e) {
    return string_value(empty_string);
  }
  else {
    return string_value(string(p, static_cast<std::string::size_type>(e - p + 1)));
  }
}

value_t report_t::fn_format(call_scope_t& args)
{
  format_t format(args.get<string>(0));
  std::ostringstream out;
  out << format(args);
  return string_value(out.str());
}

value_t report_t::fn_print(call_scope_t& args)
{
  for (std::size_t i = 0; i < args.size(); i++)
    args[i].print(output_stream);
  static_cast<std::ostream&>(output_stream) << std::endl;
  return true;
}

value_t report_t::fn_scrub(call_scope_t& args)
{
  return display_value(args.value());
}

value_t report_t::fn_rounded(call_scope_t& args)
{
  return args.value().rounded();
}

value_t report_t::fn_unrounded(call_scope_t& args)
{
  return args.value().unrounded();
}

value_t report_t::fn_quantity(call_scope_t& args)
{
  return args.get<amount_t>(0).number();
}

value_t report_t::fn_floor(call_scope_t& args)
{
  return args[0].floored();
}

value_t report_t::fn_ceiling(call_scope_t& args)
{
  return args[0].ceilinged();
}

value_t report_t::fn_round(call_scope_t& args)
{
  return args[0].rounded();
}

value_t report_t::fn_roundto(call_scope_t& args)
{
  return args[0].roundto(args.get<int>(1));
}

value_t report_t::fn_unround(call_scope_t& args)
{
  return args[0].unrounded();
}

value_t report_t::fn_abs(call_scope_t& args)
{
  return args[0].abs();
}

value_t report_t::fn_truncated(call_scope_t& args)
{
  return string_value(format_t::truncate
                      (args.get<string>(0),
                       (args.has<int>(1) && args.get<int>(1) > 0) ?
                       static_cast<std::size_t>(args.get<int>(1)) : 0,
                       args.has<int>(2) ?
                       static_cast<std::size_t>(args.get<int>(2)) : 0));
}

value_t report_t::fn_justify(call_scope_t& args)
{
  uint_least8_t flags(AMOUNT_PRINT_ELIDE_COMMODITY_QUOTES);

  if (args.has<bool>(3) && args.get<bool>(3))
    flags |= AMOUNT_PRINT_RIGHT_JUSTIFY;
  if (args.has<bool>(4) && args.get<bool>(4))
    flags |= AMOUNT_PRINT_COLORIZE;

  std::ostringstream out;
  args[0].print(out, args.get<int>(1),
                args.has<int>(2) ? args.get<int>(2) : -1, flags);

  return string_value(out.str());
}

value_t report_t::fn_quoted(call_scope_t& args)
{
  std::ostringstream out;

  out << '"';
  string arg(args.get<string>(0));
  foreach (const char ch, arg) {
    if (ch == '"')
      out << "\\\"";
    else
      out << ch;
  }
  out << '"';

  return string_value(out.str());
}

value_t report_t::fn_quoted_rfc(call_scope_t& args)
{
  std::ostringstream out;

  out << '"';
  string arg(args.get<string>(0));
  foreach (const char ch, arg) {
    if (ch == '"')
      out << '"' << '"';
    else
      out << ch;
  }
  out << '"';

  return string_value(out.str());
}

value_t report_t::fn_join(call_scope_t& args)
{
  std::ostringstream out;

  string arg(args.get<string>(0));
  foreach (const char ch, arg) {
    if (ch != '\n')
      out << ch;
    else
      out << "\\n";
  }
  return string_value(out.str());
}

value_t report_t::fn_format_date(call_scope_t& args)
{
  if (args.has<string>(1))
    return string_value(format_date(args.get<date_t>(0), FMT_CUSTOM,
                                    args.get<string>(1).c_str()));
  else
    return string_value(format_date(args.get<date_t>(0), FMT_PRINTED));
}

value_t report_t::fn_format_datetime(call_scope_t& args)
{
  if (args.has<string>(1))
    return string_value(format_datetime(args.get<datetime_t>(0), FMT_CUSTOM,
                                        args.get<string>(1).c_str()));
  else
    return string_value(format_datetime(args.get<datetime_t>(0), FMT_PRINTED));
}

value_t report_t::fn_ansify_if(call_scope_t& args)
{
  if (args.has<string>(1)) {
    string color = args.get<string>(1);
    std::ostringstream buf;
    if (color == "black")          buf << "\033[30m";
    else if (color == "red")       buf << "\033[31m";
    else if (color == "green")     buf << "\033[32m";
    else if (color == "yellow")    buf << "\033[33m";
    else if (color == "blue")      buf << "\033[34m";
    else if (color == "magenta")   buf << "\033[35m";
    else if (color == "cyan")      buf << "\033[36m";
    else if (color == "white")     buf << "\033[37m";
    else if (color == "bold")      buf << "\033[1m";
    else if (color == "underline") buf << "\033[4m";
    else if (color == "blink")     buf << "\033[5m";
    buf << args[0];
    buf << "\033[0m";
    return string_value(buf.str());
  }
  return args[0];
}

value_t report_t::fn_percent(call_scope_t& args)
{
  return (amount_t("100.00%") *
          (args.get<amount_t>(0) / args.get<amount_t>(1)).number());
}

value_t report_t::fn_commodity(call_scope_t& args)
{
  return string_value(args.get<amount_t>(0).commodity().symbol());
}

value_t report_t::fn_commodity_price(call_scope_t& args)
{
  optional<price_point_t> price_point
    = commodity_pool_t::current_pool->commodity_price_history.find_price
        (args.get<amount_t>(0).commodity(), args.get<datetime_t>(1));
  if (price_point) {
    return price_point->price;
  } else {
    return amount_t();
  }
}

value_t report_t::fn_set_commodity_price(call_scope_t& args)
{
  args.get<amount_t>(0).commodity().add_price(
    args.get<datetime_t>(1), args.get<amount_t>(2), true);
  return NULL_VALUE;
}

value_t report_t::fn_clear_commodity(call_scope_t& args)
{
  amount_t amt(args.get<amount_t>(0));
  amt.clear_commodity();
  return amt;
}

value_t report_t::fn_nail_down(call_scope_t& args)
{
  value_t arg0(args[0]);
  value_t arg1(args[1]);

  switch (arg0.type()) {
  case value_t::AMOUNT: {
    amount_t tmp(arg0.as_amount());
    if (tmp.has_commodity() && ! tmp.is_null() && ! tmp.is_realzero()) {
      arg1 = arg1.strip_annotations(keep_details_t()).to_amount();
      expr_t value_expr(is_expr(arg1) ?
                        as_expr(arg1) :
                        expr_t::op_t::wrap_value(arg1.unrounded() /
                                                 arg0.number()));
      std::ostringstream buf;
      value_expr.print(buf);
      value_expr.set_text(buf.str());

      tmp.set_commodity(tmp.commodity().nail_down(value_expr));
    }
    return tmp;
  }

  case value_t::BALANCE: {
    balance_t tmp;
    foreach (const balance_t::amounts_map::value_type& pair,
             arg0.as_balance_lval().amounts) {
      call_scope_t inner_args(*args.parent);
      inner_args.push_back(pair.second);
      inner_args.push_back(arg1);
      tmp += fn_nail_down(inner_args).as_amount();
    }
    return tmp;
  }

  case value_t::SEQUENCE: {
    value_t tmp;
    foreach (value_t& value, arg0.as_sequence_lval()) {
      call_scope_t inner_args(*args.parent);
      inner_args.push_back(value);
      inner_args.push_back(arg1);
      tmp.push_back(fn_nail_down(inner_args));
    }
    return tmp;
  }

  default:
    throw_(std::runtime_error, _f("Attempting to nail down %1%")
           % args[0].label());
  }
  return arg0;
}

value_t report_t::fn_lot_date(call_scope_t& args)
{
  if (args[0].has_annotation()) {
    const annotation_t& details(args[0].annotation());
    if (details.date)
      return *details.date;
  }
  return NULL_VALUE;
}

value_t report_t::fn_lot_price(call_scope_t& args)
{
  if (args[0].has_annotation()) {
    const annotation_t& details(args[0].annotation());
    if (details.price)
      return *details.price;
  }
  return NULL_VALUE;
}

value_t report_t::fn_lot_tag(call_scope_t& args)
{
  if (args[0].has_annotation()) {
    const annotation_t& details(args[0].annotation());
    if (details.tag)
      return string_value(*details.tag);
  }
  return NULL_VALUE;
}

value_t report_t::fn_to_boolean(call_scope_t& args)
{
  return args.get<bool>(0);
}

value_t report_t::fn_to_int(call_scope_t& args)
{
  // This method is not called fn_to_long, because that would be
  // confusing to users who don't care about the distinction between
  // integer and long.
  return args.get<long>(0);
}

value_t report_t::fn_to_datetime(call_scope_t& args)
{
  return args.get<datetime_t>(0);
}

value_t report_t::fn_to_date(call_scope_t& args)
{
  return args.get<date_t>(0);
}

value_t report_t::fn_to_amount(call_scope_t& args)
{
  return args.get<amount_t>(0);
}

value_t report_t::fn_to_balance(call_scope_t& args)
{
  return args.get<balance_t>(0);
}

value_t report_t::fn_to_string(call_scope_t& args)
{
  return string_value(args.get<string>(0));
}

value_t report_t::fn_to_mask(call_scope_t& args)
{
  return args.get<mask_t>(0);
}

value_t report_t::fn_to_sequence(call_scope_t& args)
{
  return args[0].to_sequence();
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

value_t report_t::echo_command(call_scope_t& args)
{
  std::ostream& out(output_stream);
  out << args.get<string>(0) << std::endl;
  return true;
}

value_t report_t::pricemap_command(call_scope_t& args)
{
  std::ostream& out(output_stream);
  commodity_pool_t::current_pool->commodity_price_history.print_map
    (out, args.has<string>(0) ?
     datetime_t(parse_date(args.get<string>(0))) : datetime_t());
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
  case 'H':
    OPT_CH(historical);
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
    else OPT(add_budget);
    else OPT(amount_);
    else OPT(amount_data);
    else OPT_ALT(primary_date, actual_dates);
    else OPT(align_intervals);
    else OPT(anon);
    else OPT_ALT(color, ansi);
    else OPT(auto_match);
    else OPT(aux_date);
    else OPT(average);
    else OPT(account_width_);
    else OPT(amount_width_);
    else OPT(average_lot_prices);
    break;
  case 'b':
    OPT(balance_format_);
    else OPT(base);
    else OPT(basis);
    else OPT_(begin_);
    else OPT(bold_if_);
    else OPT(budget);
    else OPT(budget_format_);
    else OPT(by_payee);
    break;
  case 'c':
    OPT(csv_format_);
    else OPT_ALT(gain, change);
    else OPT(cleared);
    else OPT(cleared_format_);
    else OPT(collapse);
    else OPT(collapse_if_zero);
    else OPT(color);
    else OPT(columns_);
    else OPT_ALT(basis, cost);
    else OPT_(current);
    else OPT(count);
    break;
  case 'd':
    OPT(daily);
    else OPT(date_);
    else OPT(date_format_);
    else OPT(datetime_format_);
    else OPT(dc);
    else OPT(depth_);
    else OPT(deviation);
    else OPT_ALT(rich_data, detail);
    else OPT_(display_);
    else OPT(display_amount_);
    else OPT(display_total_);
    else OPT_ALT(dow, days_of_week);
    else OPT(date_width_);
    break;
  case 'e':
    OPT(empty);
    else OPT_(end_);
    else OPT(equity);
    else OPT(exact);
    else OPT(exchange_);
    else OPT_ALT(aux_date, effective);
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
    else OPT(group_by_);
    else OPT(group_title_format_);
    else OPT(generated);
    break;
  case 'h':
    OPT(head_);
    else OPT(historical);
    break;
  case 'i':
    OPT(invert);
    else OPT(inject_);
    else OPT(immediate);
    break;
  case 'j':
    OPT_CH(amount_data);
    break;
  case 'l':
    OPT_(limit_);
    else OPT(lot_dates);
    else OPT(lot_prices);
    else OPT_ALT(lot_notes, lot_tags);
    else OPT(lots);
    else OPT(lots_actual);
    else OPT_ALT(tail_, last_);
    break;
  case 'm':
    OPT(market);
    else OPT(monthly);
    else OPT(meta_);
    else OPT(meta_width_);
    break;
  case 'n':
    OPT_CH(collapse);
    else OPT(no_color);
    else OPT(no_pager);
    else OPT(no_revalued);
    else OPT(no_rounding);
    else OPT(no_titles);
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
    else OPT(pivot_);
    else OPT(plot_amount_format_);
    else OPT(plot_total_format_);
    else OPT(price);
    else OPT(prices_format_);
    else OPT(pricedb_format_);
    else OPT(primary_date);
    else OPT(payee_width_);
    else OPT(prepend_format_);
    else OPT(prepend_width_);
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
    else OPT(rich_data);
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
    else OPT(time_report);
    break;
  case 'u':
    OPT(unbudgeted);
    else OPT(uncleared);
    else OPT(unrealized);
    else OPT(unrealized_gains_);
    else OPT(unrealized_losses_);
    else OPT(unround);
    break;
  case 'v':
    OPT_ALT(market, value);
    else OPT(values);
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
      else if (is_eq(p, "averaged_lots"))
        return MAKE_FUNCTOR(report_t::fn_averaged_lots);
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
      else if (is_eq(p, "commodity"))
        return MAKE_FUNCTOR(report_t::fn_commodity);
      else if (is_eq(p, "commodity_price"))
        return MAKE_FUNCTOR(report_t::fn_commodity_price);
      else if (is_eq(p, "ceiling"))
        return MAKE_FUNCTOR(report_t::fn_ceiling);
      else if (is_eq(p, "clear_commodity"))
        return MAKE_FUNCTOR(report_t::fn_clear_commodity);
      break;

    case 'd':
      if (is_eq(p, "display_amount"))
        return MAKE_FUNCTOR(report_t::fn_display_amount);
      else if (is_eq(p, "display_total"))
        return MAKE_FUNCTOR(report_t::fn_display_total);
      else if (is_eq(p, "date"))
        return MAKE_FUNCTOR(report_t::fn_today);
      break;

    case 'f':
      if (is_eq(p, "format_date"))
        return MAKE_FUNCTOR(report_t::fn_format_date);
      else if (is_eq(p, "format_datetime"))
        return MAKE_FUNCTOR(report_t::fn_format_datetime);
      else if (is_eq(p, "format"))
        return MAKE_FUNCTOR(report_t::fn_format);
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
      else if (is_eq(p, "nail_down"))
        return MAKE_FUNCTOR(report_t::fn_nail_down);
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
      else if (is_eq(p, "print"))
        return MAKE_FUNCTOR(report_t::fn_print);
      break;

    case 'q':
      if (is_eq(p, "quoted"))
        return MAKE_FUNCTOR(report_t::fn_quoted);
      else if (is_eq(p, "quoted_rfc"))
        return MAKE_FUNCTOR(report_t::fn_quoted_rfc);
      else if (is_eq(p, "quantity"))
        return MAKE_FUNCTOR(report_t::fn_quantity);
      break;

    case 'r':
      if (is_eq(p, "rounded"))
        return MAKE_FUNCTOR(report_t::fn_rounded);
      else if (is_eq(p, "red"))
        return WRAP_FUNCTOR(fn_red);
      else if (is_eq(p, "round"))
        return MAKE_FUNCTOR(report_t::fn_round);
      else if (is_eq(p, "roundto"))
        return MAKE_FUNCTOR(report_t::fn_roundto);
      break;

    case 's':
      if (is_eq(p, "scrub"))
        return MAKE_FUNCTOR(report_t::fn_scrub);
      else if (is_eq(p, "strip"))
        return MAKE_FUNCTOR(report_t::fn_strip);
      else if (is_eq(p, "should_bold"))
        return MAKE_FUNCTOR(report_t::fn_should_bold);
      else if (is_eq(p, "set_commodity_price"))
        return MAKE_FUNCTOR(report_t::fn_set_commodity_price);
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
      else if (is_eq(p, "trim"))
        return MAKE_FUNCTOR(report_t::fn_trim);
      else if (is_eq(p, "top_amount"))
        return MAKE_FUNCTOR(report_t::fn_top_amount);
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
      else if (is_eq(p, "unround"))
        return MAKE_FUNCTOR(report_t::fn_unround);
      else if (is_eq(p, "unrounded"))
        return MAKE_FUNCTOR(report_t::fn_unrounded);
      break;

    case 'v':
      if (is_eq(p, "value_date"))
        return MAKE_FUNCTOR(report_t::fn_now);
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

#define POSTS_REPORTER(formatter)                               \
    WRAP_FUNCTOR(reporter<>(post_handler_ptr(formatter), *this, \
                            string("#") + p))

    // Can't use WRAP_FUNCTOR here because the template arguments
    // confuse the parser
#define POSTS_REPORTER_(method, formatter)                      \
    expr_t::op_t::wrap_functor                                  \
    (reporter<post_t, post_handler_ptr, method>                 \
      (post_handler_ptr(formatter), *this, string("#") + p))

#define FORMATTED_POSTS_REPORTER(format)                                \
    POSTS_REPORTER                                                      \
      (new format_posts                                                 \
       (*this, report_format(HANDLER(format)),                          \
        maybe_format(HANDLER(prepend_format_)),                         \
        HANDLED(prepend_width_) ?                                       \
        lexical_cast<std::size_t>(HANDLER(prepend_width_).str()) : 0))

#define FORMATTED_COMMODITIES_REPORTER(format)                          \
    POSTS_REPORTER_                                                     \
      (&report_t::commodities_report,                                   \
       new format_posts                                                 \
       (*this, report_format(HANDLER(format)),                          \
        maybe_format(HANDLER(prepend_format_)),                         \
        HANDLED(prepend_width_) ?                                       \
        lexical_cast<std::size_t>(HANDLER(prepend_width_).str()) : 0))

#define ACCOUNTS_REPORTER(formatter)                                    \
    expr_t::op_t::wrap_functor(reporter<account_t, acct_handler_ptr,    \
                               &report_t::accounts_report>              \
                               (acct_handler_ptr(formatter), *this,     \
                                string("#") + p))

#define FORMATTED_ACCOUNTS_REPORTER(format)                             \
    ACCOUNTS_REPORTER                                                   \
      (new format_accounts                                              \
       (*this, report_format(HANDLER(format)),                          \
        maybe_format(HANDLER(prepend_format_)),                         \
        HANDLED(prepend_width_) ?                                       \
        lexical_cast<std::size_t>(HANDLER(prepend_width_).str()) : 0))

  case symbol_t::COMMAND:
    switch (*p) {
    case 'a':
      if (is_eq(p, "accounts")) {
        return POSTS_REPORTER(new report_accounts(*this));
      }
      break;

    case 'b':
      if (*(p + 1) == '\0' || is_eq(p, "bal") || is_eq(p, "balance")) {
        // jww (2023-01-27): This next 'if' statement is a hack for historical
        // purposes. Until this date, the balance report always used an amount
        // width of 20. If the user has set the amount width, this should be
        // used instead; but if they haven't, we need to use the old default
        // in order for the tests to pass.
        if (! HANDLED(amount_width_))
          HANDLER(amount_width_).value = "20";
        return FORMATTED_ACCOUNTS_REPORTER(balance_format_);
      }
      else if (is_eq(p, "budget")) {
        HANDLER(amount_).on(string("#budget"), "(amount, 0)");

        budget_flags |= BUDGET_WRAP_VALUES;
        if (! (budget_flags & ~BUDGET_WRAP_VALUES))
          budget_flags |= BUDGET_BUDGETED;

        return FORMATTED_ACCOUNTS_REPORTER(budget_format_);
      }
      break;

    case 'c':
      if (is_eq(p, "csv")) {
        return FORMATTED_POSTS_REPORTER(csv_format_);
      }
      else if (is_eq(p, "cleared")) {
        HANDLER(amount_).on(string("#cleared"),
                            "(amount, cleared ? amount : 0)");
        return FORMATTED_ACCOUNTS_REPORTER(cleared_format_);
      }
      else if (is_eq(p, "convert")) {
        return WRAP_FUNCTOR(convert_command);
      }
      else if (is_eq(p, "commodities")) {
        return POSTS_REPORTER(new report_commodities(*this));
      }
      break;
    case 'd':
      if (is_eq(p, "draft")) {
        return WRAP_FUNCTOR(xact_command);
      }
      break;
    case 'e':
      if (is_eq(p, "equity")) {
        HANDLER(generated).on("#equity");
        return POSTS_REPORTER(new print_xacts(*this));
      }
      else if (is_eq(p, "entry")) {
        return WRAP_FUNCTOR(xact_command);
      }
      else if (is_eq(p, "emacs")) {
        return POSTS_REPORTER(new format_emacs_posts(output_stream));
      }
      else if (is_eq(p, "echo")) {
        return MAKE_FUNCTOR(report_t::echo_command);
      }
      break;

    case 'l':
      if (is_eq(p, "lisp"))
        return POSTS_REPORTER(new format_emacs_posts(output_stream));
      break;

    case 'p':
      if (*(p + 1) == '\0' || is_eq(p, "print")) {
        return POSTS_REPORTER(new print_xacts(*this, HANDLED(raw)));
      }
      else if (is_eq(p, "prices")) {
        return FORMATTED_COMMODITIES_REPORTER(prices_format_);
      }
      else if (is_eq(p, "pricedb") || is_eq(p, "pricesdb")) {
        return FORMATTED_COMMODITIES_REPORTER(pricedb_format_);
      }
      else if (is_eq(p, "pricemap")) {
        return MAKE_FUNCTOR(report_t::pricemap_command);
      }
      else if (is_eq(p, "payees")) {
        return POSTS_REPORTER(new report_payees(*this));
      }
      break;

    case 'r':
      if (*(p + 1) == '\0' || is_eq(p, "reg") || is_eq(p, "register")) {
        return FORMATTED_POSTS_REPORTER(register_format_);
      }
      else if (is_eq(p, "reload")) {
        return MAKE_FUNCTOR(report_t::reload_command);
      }
      break;

    case 's':
      if (is_eq(p, "stats") || is_eq(p, "stat"))
        return WRAP_FUNCTOR(report_statistics);
      else if (is_eq(p, "source"))
        return WRAP_FUNCTOR(source_command);
      else if (is_eq(p, "select"))
        return WRAP_FUNCTOR(select_command);
      break;
    case 't':
      if (is_eq(p, "tags")) {
        return POSTS_REPORTER(new report_tags(*this));
      }
      break;
    case 'x':
      if (is_eq(p, "xact"))
        return WRAP_FUNCTOR(xact_command);
      else if (is_eq(p, "xml"))
        return POSTS_REPORTER(new format_ptree(*this,
                                               format_ptree::FORMAT_XML));
      break;
    }
    break;

  case symbol_t::PRECOMMAND:
    switch (*p) {
    case 'a':
      if (is_eq(p, "args"))
        return WRAP_FUNCTOR(query_command);
      break;
    case 'e':
      if (is_eq(p, "eval"))
        return WRAP_FUNCTOR(eval_command);
      else if (is_eq(p, "expr"))
        return WRAP_FUNCTOR(parse_command);
      break;
    case 'f':
      if (is_eq(p, "format"))
        return WRAP_FUNCTOR(format_command);
      break;
    case 'g':
      if (is_eq(p, "generate"))
        return POSTS_REPORTER_(&report_t::generate_report,
                               new print_xacts(*this));
      break;
    case 'p':
      if (is_eq(p, "parse"))
        return WRAP_FUNCTOR(parse_command);
      else if (is_eq(p, "period"))
        return WRAP_FUNCTOR(period_command);
      break;
    case 'q':
      if (is_eq(p, "query"))
        return WRAP_FUNCTOR(query_command);
      break;
    case 's':
      if (is_eq(p, "script"))
        return WRAP_FUNCTOR(source_command);
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
