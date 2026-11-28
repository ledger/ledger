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
 * @file   report.cc
 * @author John Wiegley
 *
 * @ingroup report
 *
 * @brief Implementation of report_t: option normalization, report drivers,
 *        built-in value expression functions, option lookup, and command dispatch.
 *
 * This file is organized into the following sections:
 *
 * 1. **Option Normalization** -- normalize_options() and normalize_period()
 *    reconcile option interactions after parsing (e.g., color auto-detection,
 *    column width calculation, period-to-limit propagation).
 *
 * 2. **Query Argument Parsing** -- parse_query_args() converts positional
 *    arguments into --limit, --only, --display, --bold-if, and --period.
 *
 * 3. **Report Drivers** -- posts_report(), accounts_report(), etc. assemble
 *    the filter chain and iterate journal data through the handler pipeline.
 *
 * 4. **Built-in Functions** -- fn_market(), fn_scrub(), fn_justify(), etc.
 *    implement the functions callable from value expressions and format strings.
 *
 * 5. **Option Lookup** -- lookup_option() uses a first-character switch table
 *    to resolve option names to handler instances.
 *
 * 6. **Symbol Lookup and Command Dispatch** -- lookup() resolves functions,
 *    options, commands, and precommands, connecting command names to report
 *    driver + formatter combinations.
 */

#include <system.hh>
#include <utility>

#include "report.h"
#include "session.h"
#include "pool.h"
#include "op.h"
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

namespace {

/// Parse a --price-exp / --leeway duration string into seconds.
///
/// Accepts an integer optionally followed by a single-character unit suffix
/// (case-insensitive): @c s (seconds), @c m (minutes), @c h (hours),
/// @c d (days), or @c w (weeks).  A bare integer with no suffix is interpreted
/// as hours for backward compatibility (issue #607).  Whitespace around the
/// value and between the number and its suffix is tolerated.
long parse_leeway_seconds(const string& raw) {
  string s = raw;
  boost::algorithm::trim(s);
  if (s.empty())
    throw_(option_error, _f("Missing argument for --price-exp"));

  long multiplier = 3600L; // default: hours
  char suffix = s[s.size() - 1];
  if (!std::isdigit(static_cast<unsigned char>(suffix))) {
    switch (std::tolower(static_cast<unsigned char>(suffix))) {
    case 's':
      multiplier = 1L;
      break;
    case 'm':
      multiplier = 60L;
      break;
    case 'h':
      multiplier = 3600L;
      break;
    case 'd':
      multiplier = 86400L;
      break;
    case 'w':
      multiplier = 604800L;
      break;
    default:
      throw_(option_error,
             _f("Invalid unit '%1%' for --price-exp; expected s, m, h, d, or w") % suffix);
    }
    s.erase(s.size() - 1);
    boost::algorithm::trim_right(s);
  }

  long value;
  try {
    value = lexical_cast<long>(s);
  } catch (const bad_lexical_cast&) {
    throw_(option_error, _f("Invalid value for --price-exp: %1%") % raw);
  }

  if (value < 0)
    throw_(option_error, _f("--price-exp value must be non-negative: %1%") % raw);

  if (value > std::numeric_limits<long>::max() / multiplier)
    throw_(option_error, _f("--price-exp value too large: %1%") % raw);

  return value * multiplier;
}

} // anonymous namespace

temporaries_t& report_t::temp_accounts() {
  accounts_pool = session.journal->report_accounts_pool();
  return *accounts_pool;
}

/*--- Option Normalization ---*/

void report_t::normalize_options(const string& verb) {
  // Patch up some of the reporting options based on what kind of
  // command it was.

#if HAVE_ISATTY
  if (!HANDLED(force_color)) { // NOLINT(bugprone-branch-clone)
    if (!HANDLED(no_color) && isatty(STDOUT_FILENO))
      HANDLER(color).on("?normalize");
    if (HANDLED(color) && !isatty(STDOUT_FILENO))
      HANDLER(color).off();
  } else {
    HANDLER(color).on("?normalize");
  }
  if (!HANDLED(force_pager)) {
    if (HANDLED(pager_) && !isatty(STDOUT_FILENO))
      HANDLER(pager_).off();
  }
#endif

  if (HANDLED(output_)) {
    if (HANDLED(color) && !HANDLED(force_color))
      HANDLER(color).off();
    if (HANDLED(pager_) && !HANDLED(force_pager))
      HANDLER(pager_).off();
  }

#if defined(_WIN32) && !defined(__CYGWIN__)
  // On Windows, ANSI escape sequences are not processed by CMD or PowerShell
  // unless virtual terminal processing is explicitly enabled.  Enable it now
  // if we have determined that color output will be used.
  if (HANDLED(color)) {
    HANDLE hOut = GetStdHandle(STD_OUTPUT_HANDLE);
    if (hOut != INVALID_HANDLE_VALUE) {
      DWORD dwMode = 0;
      if (GetConsoleMode(hOut, &dwMode))
        SetConsoleMode(hOut, dwMode | ENABLE_VIRTUAL_TERMINAL_PROCESSING);
    }
  }
#endif

  item_t::use_aux_date = wants_aux_date();

  commodity_pool_t::current_pool->keep_base = HANDLED(base);
  commodity_pool_t::current_pool->get_quotes = session.HANDLED(download);

  if (session.HANDLED(getquote_))
    commodity_pool_t::current_pool->getquote = session.HANDLER(getquote_).str();
  else
    commodity_pool_t::current_pool->getquote = "getquote";

  if (session.HANDLED(price_exp_))
    commodity_pool_t::current_pool->quote_leeway =
        parse_leeway_seconds(session.HANDLER(price_exp_).value);

  if (session.HANDLED(price_db_))
    commodity_pool_t::current_pool->price_db = session.HANDLER(price_db_).str();
  else
    commodity_pool_t::current_pool->price_db = std::nullopt;

  if (HANDLED(date_format_))
    set_date_format(HANDLER(date_format_).str().c_str());
  if (HANDLED(datetime_format_))
    set_datetime_format(HANDLER(datetime_format_).str().c_str());

  apply_option_globals();

  long meta_width = -1;

  if (!HANDLED(prepend_format_) && HANDLED(meta_)) {
    if (!HANDLED(meta_width_)) {
      string::size_type i = HANDLER(meta_).str().find(':');
      if (i != string::npos) {
        HANDLED(meta_width_).on("?normalize", string(HANDLER(meta_).str(), i + 1));
        HANDLED(meta_).on("?normalize", string(HANDLER(meta_).str(), 0, i));
      }
    }
    if (HANDLED(meta_width_)) { // NOLINT(bugprone-branch-clone)
      meta_width = HANDLER(meta_width_).as_int();
      // Paste the parsed width, not the argument text: "+5" is a valid
      // --meta-width but not a valid term of the format expression.
      const string width = std::to_string(meta_width);
      HANDLER(prepend_format_)
          .on("?normalize", string("%(justify(truncated(tag(\"") + HANDLER(meta_).str() + "\"), " +
                                width + " - 1), " + width + "))");
    } else {
      HANDLER(prepend_format_).on("?normalize", string("%(tag(\"") + HANDLER(meta_).str() + "\"))");
    }
  }

  if (verb == "print" || verb == "xact") { // NOLINT(bugprone-branch-clone)
    HANDLER(related_all).parent = this;
    HANDLER(related_all).on("?normalize");
  } else if (verb == "equity") {
    HANDLER(equity).on("?normalize");
  }

  // Balance and register display rounded running totals; every other
  // command reports in base units.  Only their text output, not -j/-J
  // plot data, reconciles display rounding with <Adjustment> rows.
  bool totals_display = verb[0] == 'b' || verb[0] == 'r';
  if (!totals_display)
    HANDLER(base).on("?normalize");
  show_rounding_adjustments = totals_display && !HANDLED(amount_data) && !HANDLED(total_data);

  // If a time period was specified with -p, check whether it also gave a
  // begin and/or end to the report period (though these can be overridden
  // using -b or -e).  Then, if no _duration_ was specified (such as monthly),
  // then ignore the period since the begin/end are the only interesting
  // details.
  if (HANDLED(period_))
    normalize_period();

  // If -j or -J were specified, set the appropriate format string now so as
  // to avoid option ordering issues were we to have done it during the
  // initial parsing of the options.  Also force --base so that quantity()
  // always returns values in a consistent base unit (e.g. seconds for time),
  // since the output is intended for plotting tools that need comparable
  // numeric values (#620).
  if (HANDLED(amount_data)) { // NOLINT(bugprone-branch-clone)
    HANDLER(format_).on("?normalize", HANDLER(plot_amount_format_).value);
    HANDLER(base).on("?normalize");
  } else if (HANDLED(total_data)) {
    HANDLER(format_).on("?normalize", HANDLER(plot_total_format_).value);
    HANDLER(base).on("?normalize");
  }

  // If the --exchange (-X) option was used, parse out any final price
  // settings that may be there.
  if (HANDLED(exchange_) && HANDLER(exchange_).str().find('=') != string::npos) {
    value_t(0L).exchange_commodities(HANDLER(exchange_).str(), true, terminus);
  }

  // --no-revalued must win regardless of option order: -V/-X (and other
  // options that imply --revalued, like --basis or --gain) call
  // OTHER(revalued).on(whence) inside their handlers, so a --no-revalued
  // appearing earlier on the command line is silently re-enabled.  After
  // all options have been parsed, force --revalued back off if the user
  // explicitly requested --no-revalued.
  if (HANDLED(no_revalued))
    HANDLER(revalued).off();

  if (HANDLED(percent)) {
    commodity_t::decimal_comma_by_default = false;
    if (HANDLED(market)) {
      HANDLER(total_).on("?normalize",
                         "(__tmp = market(display_parent.total, value_date, exchange);"
                         " ((is_account & display_parent & __tmp) ?"
                         "   percent(scrub(market(total, value_date, exchange)), "
                         "           scrub(__tmp)) : 0))");
    }
  }

  if (HANDLED(immediate) && HANDLED(market)) {
    HANDLER(amount_).on("?normalize", "market(amount_expr, value_date, exchange)");
  }

  if (HANDLED(round)) {
    if (HANDLED(market)) {
      if (!HANDLED(immediate)) {
        // Force per-posting market conversion so rounding happens on individual
        // market values rather than the accumulated total.  Use 'today' rather
        // than 'value_date' because in the per-posting expression context
        // 'value_date' resolves to the posting's own date, which may predate the
        // price history entries and thus fall back to the lot cost.
        HANDLER(amount_).on("?normalize", "market(amount_expr, today, exchange)");
      }
      HANDLER(amount_).on("?normalize_round", "roundto_commodity(amount_expr)");
    } else {
      HANDLER(amount_).on("?normalize_round", "roundto_commodity(amount_expr)");
      HANDLER(total_).on("?normalize_round", "roundto_commodity(total_expr)");
    }
  }

  if (HANDLED(gain) && HANDLED(gain_since_)) {
    // When --gain-since DATE is used with --gain, compute gain relative to
    // the market value at DATE rather than from the original cost basis.
    // For postings dated before DATE, use market(amount, DATE) as the cost
    // baseline instead of the original cost.
    HANDLER(amount_).expr.set_base_expr(
        "(amount, date < [" + to_iso_extended_string(gain_from->date()) + "] ? market(amount, [" +
        to_iso_extended_string(gain_from->date()) + "], exchange) : cost)");
  }

  long cols = terminal_columns();

  if (meta_width > 0)
    cols -= meta_width;

  if (cols > 0) {
    DEBUG("auto.columns", "cols = " << cols);

    const column_widths_t widths = default_column_widths(cols);
    long date_width = widths.date;
    long payee_width = widths.payee;
    long account_width = widths.account;
    long amount_width = widths.amount;
    long total_width = widths.total;

    DEBUG("auto.columns", "date_width    = " << date_width);
    DEBUG("auto.columns", "payee_width   = " << payee_width);
    DEBUG("auto.columns", "account_width = " << account_width);
    DEBUG("auto.columns", "amount_width  = " << amount_width);
    DEBUG("auto.columns", "total_width   = " << total_width);

    if (!HANDLED(date_width_) && !HANDLED(payee_width_) && !HANDLED(account_width_) &&
        !HANDLED(amount_width_) && !HANDLED(total_width_)) {
      long total = (4 /* the spaces between */ + date_width + payee_width + account_width +
                    amount_width + total_width + (HANDLED(dc) ? 1 + amount_width : 0));
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

    if (!HANDLED(meta_width_))
      HANDLER(meta_width_).value = "0";
    if (!HANDLED(prepend_width_))
      HANDLER(prepend_width_).value = "0";
    if (!HANDLED(date_width_))
      HANDLER(date_width_).value = std::to_string(date_width);
    if (!HANDLED(payee_width_))
      HANDLER(payee_width_).value = std::to_string(payee_width);
    if (!HANDLED(account_width_))
      HANDLER(account_width_).value = std::to_string(account_width);
    if (!HANDLED(amount_width_))
      HANDLER(amount_width_).value = std::to_string(amount_width);
    if (!HANDLED(total_width_))
      HANDLER(total_width_).value = std::to_string(total_width);
  }
}

void report_t::apply_option_globals() {
  if (HANDLED(start_of_week_)) {
    const string value = HANDLER(start_of_week_).str();
    if (optional<date_time::weekdays> weekday = string_to_day_of_week(value))
      start_of_week = *weekday;
    else
      throw_(option_error, _f("Invalid value for --start-of-week: %1%; expected a day name "
                              "like Monday or Mon, or 0-6 with 0 for Sunday") %
                               value);
  } else {
    start_of_week = gregorian::Sunday;
  }

  day_of_period = HANDLED(period_shift_) ? HANDLER(period_shift_).as_int() : 0;

  ledger::ignore_diacritics = HANDLED(ignore_diacritics);

  // A report that kept a --now (push --now DATE, options before --script)
  // runs its commands at that instant.  Not the handler: on a report copy it
  // has a null parent (option_t's copy constructor) and would overwrite a
  // terminus that --end moved.
  if (HANDLED(now_) && now_epoch)
    ledger::epoch = *now_epoch;

  // Re-running the handler re-applies this report's --truncate style (its
  // value was checked when the option was given, so it cannot throw here).
  if (HANDLED(truncate_)) {
    HANDLER(truncate_).handler_thunk(std::nullopt, HANDLER(truncate_).value);
  } else {
    format_t::default_style = format_t::TRUNCATE_TRAILING;
    format_t::default_style_changed = false;
  }

  commodity_pool_t::current_pool->uncap_precision();
  if (HANDLED(decimal_places_))
    commodity_pool_t::current_pool->cap_precision(
        static_cast<amount_t::precision_t>(HANDLER(decimal_places_).as_int()));
}

long report_t::terminal_columns() const {
  long cols = 0;
#if HAVE_IOCTL
  struct winsize ws;
#endif
  if (HANDLED(columns_))
    cols = HANDLER(columns_).as_int();
  else if (const char* columns = std::getenv("COLUMNS"))
    cols = lexical_cast<long>(columns);
#if HAVE_IOCTL
  else if (ioctl(STDIN_FILENO, TIOCGWINSZ, &ws) != -1 && ws.ws_col > 0)
    cols = ws.ws_col;
#endif
  else
    cols = 80L;
  return cols;
}

report_t::column_widths_t report_t::default_column_widths(long cols) const {
  long date_width =
      (HANDLED(date_width_) ? HANDLER(date_width_).as_int()
                            : static_cast<long>(format_date(CURRENT_DATE(), FMT_PRINTED).length()));
  // In interval reports (e.g. --daily, --monthly), the payee column shows
  // the period range "- <end-date>", which is exactly date_width + 2 wide.
  // Default payee_width to that size + 1 so the column ends with a trailing
  // space, giving two visual spaces of separation from the account column
  // (the format string adds one more between fields).  This avoids the
  // usual cols * 0.263 default, which leaves the column much wider than
  // needed and wastes space that would be better given to the account
  // column (#3216).
  long payee_width = (HANDLED(payee_width_) ? HANDLER(payee_width_).as_int()
                      : HANDLED(period_)    ? date_width + 3
                                            : long(double(cols) * 0.263157));
  long account_width =
      (HANDLED(account_width_) ? HANDLER(account_width_).as_int() : long(double(cols) * 0.302631));
  long amount_width =
      (HANDLED(amount_width_) ? HANDLER(amount_width_).as_int() : long(double(cols) * 0.157894));
  long total_width = (HANDLED(total_width_) ? HANDLER(total_width_).as_int() : amount_width);
  return {date_width, payee_width, account_width, amount_width, total_width};
}

void report_t::set_gain_display_exprs(const optional<string>& whence, const char* when) {
  HANDLER(amount_).expr.set_base_expr("(amount, cost)");

  // Since we are displaying the amounts of revalued postings, they
  // will end up being composite totals, and hence a pair of pairs.
  // When --subtotal collapses compound postings the amount expression
  // evaluates to a nested pair ((amount, cost), (amount, cost)).  We
  // distinguish two sub-cases by looking at the inner cost element:
  //
  //  inner[1] != 0  →  subtotaled post: compute market(inner[0]) - inner[1]
  //  inner[1] == 0  →  revalued post from changed_value_posts: the delta
  //                    market(repriced) - market(last) is already computed as
  //                    inner[0], so return it unchanged (issue #965).
  HANDLER(display_amount_)
      .on(whence, "use_direct_amount ? amount :"
                  " (is_seq(get_at(amount_expr, 0)) ?"
                  "  (get_at(get_at(amount_expr, 0), 1) ?"
                  "    market(get_at(get_at(amount_expr, 0), 0), " +
                      string(when) +
                      ", exchange)"
                      "    - get_at(get_at(amount_expr, 0), 1) :"
                      "    get_at(get_at(amount_expr, 0), 0)) :"
                      "  market(get_at(amount_expr, 0), " +
                      string(when) +
                      ", exchange)"
                      "  - get_at(amount_expr, 1))");
  HANDLER(display_total_)
      .on(whence, "use_direct_amount ? total_expr :"
                  " market(get_at(total_expr, 0), " +
                      string(when) +
                      ", exchange)"
                      " - get_at(total_expr, 1)");
}

void report_t::normalize_period() {
  date_interval_t interval(HANDLER(period_).str());

  optional<date_t> begin = interval.begin();
  optional<date_t> end = interval.end();

  if (!HANDLED(begin_) && begin) {
    HANDLER(limit_).on(string("?normalize"), "date>=[" + to_iso_extended_string(*begin) + "]");
    if (!origin || *begin < *origin)
      origin = begin;
  }
  if (!HANDLED(end_) && end)
    HANDLER(limit_).on(string("?normalize"), "date<[" + to_iso_extended_string(*end) + "]");

  // Assigned on every run: a report copied by push_report()/push_command()
  // keeps the value, and the copy's own --sort-all or --sort-xacts must
  // still win when its options are normalized.
  sort_within_periods =
      interval.duration.has_value() && !HANDLED(sort_all_) && !HANDLED(sort_xacts_);
  if (!interval.duration)
    HANDLER(period_).off();
}

/*--- Query Argument Parsing ---*/

namespace {

/// @brief Build O_CALL("any", arg) for a given argument subtree.
expr_t::ptr_op_t make_any_call(const expr_t::ptr_op_t& arg) {
  using op_t = expr_t::op_t;
  expr_t::ptr_op_t ident = new op_t(op_t::IDENT);
  ident->set_ident("any");
  return op_t::new_node(op_t::O_CALL, ident, arg);
}

/// @brief Wrap the operand of every negation reached through and/or/not
/// in any(); see wrap_xact_predicate().
expr_t::ptr_op_t wrap_negations(const expr_t::ptr_op_t& op) {
  using op_t = expr_t::op_t;
  switch (op->kind) {
  case op_t::O_NOT:
    return op_t::new_node(op_t::O_NOT, make_any_call(wrap_negations(op->left())));
  case op_t::O_AND:
  case op_t::O_OR:
    return op_t::new_node(op->kind, wrap_negations(op->left()), wrap_negations(op->right()));
  default:
    return op;
  }
}

/// @brief Wrap a query AST for transaction-level (xact) commands.
///
/// For print, xact and --related-all (report.cc
/// `if (HANDLED(related_all))`), a `not` means "no posting of the
/// transaction matches" (#1227) at any depth: every O_NOT(t) reached
/// through and/or/not becomes O_NOT(any(t)), and a root that is not a
/// negation is wrapped in any(), e.g.
///   not food and cash  ->  any(!any(account =~ /food/) & account =~ /cash/)
/// Positive terms joined by and/or keep their single-posting meaning
/// (`food and cash` asks for one posting matching both).
///
/// The walk does not tell the query's own and/or/not from the same
/// operators inside an `expr` term: `expr 'amount != $10'` is
/// O_NOT(O_EQ) and is rewritten wherever it sits, as it already is when
/// it is the root.  The walk stops at every other node (a function call
/// such as any(...), a comparison, a match), so a negation inside a
/// call's argument keeps its meaning.  A root `not` yields !any(...),
/// and a root without any `not` yields any(root).
expr_t::ptr_op_t wrap_xact_predicate(const expr_t::ptr_op_t& op) {
  expr_t::ptr_op_t wrapped = wrap_negations(op);
  return wrapped->kind == expr_t::op_t::O_NOT ? wrapped : make_any_call(wrapped);
}

} // namespace

void report_t::parse_query_args(const value_t& args, const string& whence) {
  if (args.empty())
    return;

  query_t query;
  expr_t::ptr_op_t limit_op =
      query.parse_args(args, what_to_keep(), true, false, session.journal.get());

  if (limit_op) {
    string pred;

    // For commands that display whole transactions (print, xact),
    // wrap the predicate in any() at the AST level so that the filter
    // operates on the transaction rather than individual postings.
    if (HANDLED(related_all)) {
      expr_t::ptr_op_t wrapped = wrap_xact_predicate(limit_op);
      pred = predicate_t(wrapped, what_to_keep()).print_to_str();
      DEBUG("report.predicate", "Xact-level limit predicate = " << pred);
    } else {
      pred = query.get_query(query_t::QUERY_LIMIT);
    }

    HANDLER(limit_).on(whence, pred);
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

    normalize_period(); // it needs normalization
  }
}

/*--- Report Drivers ---*/

post_handler_ptr report_t::walk_posts(post_handler_ptr chain,
                                      const function<void(const value_t&)>& postflush,
                                      const function<void(const value_t&)>& preflush) {
  if (HANDLED(group_by_)) {
    auto splitter = std::make_shared<post_splitter>(chain, *this, HANDLER(group_by_).expr);
    if (preflush)
      splitter->set_preflush_func(preflush);
    // Runs before the splitter clears the group's chain, so an accounts
    // flusher still sees the group's temporary postings (#3207).
    splitter->set_postflush_func([this, postflush](const value_t& group) {
      if (postflush)
        postflush(group);
      // Between cumulative groups calc_posts carries the running total
      // from the postings' xdata, so only the accounts' is cleared, and
      // only when no postflush reads their carried totals (see report.h).
      if (!HANDLED(group_by_cumulative))
        session.journal->clear_xdata();
      else if (!postflush)
        session.journal->master->clear_xdata();
    });
    chain = std::move(splitter);
  }
  chain = chain_pre_post_handlers(chain, *this);

  journal_posts_iterator walker(*session.journal.get());
  pass_down_posts<journal_posts_iterator>(chain, walker); // NOLINT(bugprone-unused-raii)

  if (!HANDLED(group_by_) && postflush)
    postflush(value_t());
  session.journal->clear_xdata();
  return chain;
}

void report_t::posts_report(post_handler_ptr handler) {
  walk_posts(chain_post_handlers(std::move(handler), *this));
}

void report_t::posts_report_quick(post_handler_ptr handler) {
  predicate_t display_predicate;
  predicate_t only_predicate;
  handler = chain_selection_handlers(handler, *this, /*has_collapse_stage=*/false,
                                     display_predicate, only_predicate);
  handler = chain_transfer_handlers(handler, *this);

  walk_posts(std::move(handler));
}

void report_t::generate_report(post_handler_ptr handler) {
  handler = chain_handlers(handler, *this);

  generate_posts_iterator walker(session, HANDLED(seed_) ? HANDLER(seed_).as_int() : 0,
                                 HANDLED(head_) ? static_cast<unsigned int>(HANDLER(head_).as_int())
                                                : 50);

  pass_down_posts<generate_posts_iterator>(handler, walker); // NOLINT(bugprone-unused-raii)
}

void report_t::xact_report(post_handler_ptr handler, xact_t& xact) {
  handler = chain_handlers(handler, *this);

  xact_posts_iterator walker(xact);
  pass_down_posts<xact_posts_iterator>(handler, walker); // NOLINT(bugprone-unused-raii)

  xact.clear_xdata();
}

namespace {
/// @brief Post-flush functor for account reports: walks the account tree and passes
/// accounts to the handler, applying display predicates and sort expressions.
struct accounts_flusher {
  acct_handler_ptr handler;
  report_t& report;

  accounts_flusher(acct_handler_ptr _handler, report_t& _report)
      : handler(std::move(_handler)), report(_report) {}

  void operator()(const value_t&) {
    report.HANDLER(amount_).expr.mark_uncompiled();
    report.HANDLER(total_).expr.mark_uncompiled();
    report.HANDLER(display_amount_).expr.mark_uncompiled();
    report.HANDLER(display_total_).expr.mark_uncompiled();
    report.HANDLER(revalued_total_).expr.mark_uncompiled();

    optional<predicate_t> pred;
    if (report.HANDLED(display_)) {
      DEBUG("report.predicate", "Display predicate = " << report.HANDLER(display_).str());
      pred = predicate_t(report.HANDLER(display_).str(), report.what_to_keep());
    }

    // pass_down_accounts reads the context only when a predicate is set.
    if (!report.HANDLED(sort_)) {
      basic_accounts_iterator iter(*report.session.journal->master);
      pass_down_accounts<basic_accounts_iterator>(handler, iter, pred, &report);
    } else {
      expr_t sort_expr(report.HANDLER(sort_).str());
      sort_expr.set_context(&report);
      sorted_accounts_iterator iter(*report.session.journal->master, sort_expr, report,
                                    report.HANDLED(flat));
      pass_down_accounts<sorted_accounts_iterator>(handler, iter, pred, &report);
    }

    // The next cumulative group adds to these totals; walk_posts clears xdata otherwise.
    if (report.HANDLED(group_by_cumulative))
      report.session.journal->master->clear_display_state();
  }
};
} // namespace

void report_t::accounts_report(
    acct_handler_ptr handler) { // NOLINT(performance-unnecessary-value-param)
  // With --flat every account has a line of its own, so an account whose
  // sub-accounts are listed as well shows only its own amount rather than
  // the family total (#1987); one whose sub-accounts are all hidden, e.g. by
  // --depth, still shows the family total (#1020), as does the grand total
  // line.  This is only the default total of an account report, so it is
  // installed here, after every option has been parsed, as the base of
  // the --total chain: --round and --unround wrap it, while an explicit
  // --total or --percent replaces it whatever the order of the options.
  // Posting reports never see it, and is_account protects the postings
  // that account reports evaluate along the way (#3282).
  if (HANDLED(flat) && HANDLER(total_).expr.base_expr == "total")
    HANDLER(total_).expr.set_base_expr(
        "is_account & parent & has_children_to_display ? amount : total");

  // walk_posts keeps the chain, and with it every temporary posting the
  // walk created, alive through the flush that walks the account tree.
  walk_posts(chain_post_handlers(post_handler_ptr(new ignore_posts), *this,
                                 /* for_accounts_report= */ true),
             accounts_flusher(handler, *this), [handler, this](const value_t& val) {
               if (!HANDLED(no_titles)) {
                 std::ostringstream buf;
                 val.print(buf);
                 handler->title(buf.str());
               }
             });
}

namespace {

/// @brief Whether @p op is a term that a transaction-state option appends
/// to --limit: -C "cleared", --pending "pending", -L "actual", -R "real",
/// or -U "uncleared|pending".  These literals mirror the OTHER(limit_).on
/// calls in the option handlers in report.h (actual, cleared, pending,
/// real, uncleared); a new state option must be added here as well.
bool is_state_conjunct(const expr_t::ptr_op_t& op) {
  using op_t = expr_t::op_t;
  if (op->is_ident()) {
    const string& name(op->as_ident());
    return name == "cleared" || name == "pending" || name == "actual" || name == "real";
  }
  return op->kind == op_t::O_OR && op->left()->is_ident() &&
         op->left()->as_ident() == "uncleared" && op->right()->is_ident() &&
         op->right()->as_ident() == "pending";
}

/// @brief Remove the transaction-state conjuncts from a --limit AST.
///
/// Returns @p op itself when it holds none, a null pointer when nothing
/// but state terms remain, and otherwise a rebuilt O_AND tree sharing the
/// untouched subtrees.  Only operands of the top-level conjunction are
/// examined; a state term under O_OR or O_NOT is the user's and stays.
expr_t::ptr_op_t strip_state_conjuncts(const expr_t::ptr_op_t& op) {
  using op_t = expr_t::op_t;
  if (is_state_conjunct(op))
    return nullptr;
  if (op->kind != op_t::O_AND)
    return op;
  expr_t::ptr_op_t left = strip_state_conjuncts(op->left());
  expr_t::ptr_op_t right = strip_state_conjuncts(op->right());
  if (!left)
    return right;
  if (!right)
    return left;
  if (left == op->left() && right == op->right())
    return op;
  return op_t::new_node(op_t::O_AND, left, right);
}

} // namespace

void report_t::commodities_report(post_handler_ptr handler) {
  // Transaction state predicates (--cleared, --pending, --uncleared,
  // --actual, --real) don't apply to commodity prices.  Drop them from
  // the parsed limit, as wrap_xact_predicate rewrites it for print,
  // before building the handler chain.
  string saved_limit;
  bool had_limit = HANDLED(limit_);
  if (had_limit) {
    saved_limit = HANDLER(limit_).str();
    expr_t limit_expr(saved_limit);
    expr_t::ptr_op_t limit_op = limit_expr.get_op();
    expr_t::ptr_op_t stripped = limit_op ? strip_state_conjuncts(limit_op) : limit_op;
    if (stripped != limit_op) {
      HANDLER(limit_).off();
      if (stripped)
        HANDLER(limit_).on(std::nullopt, predicate_t(stripped, what_to_keep()).print_to_str());
    }
  }

  // The walker owns the synthetic xacts and posts that it passes down the
  // chain, and display_filter_posts links its rounding posts to those
  // xacts, so the chain has to be destroyed first.  Declaring the walker
  // ahead of the chain makes that so on every path out of this function.
  unique_ptr<posts_commodities_iterator> walker;
  post_handler_ptr chain = chain_handlers(std::move(handler), *this);

  // Restore the original limit.
  if (had_limit) {
    HANDLER(limit_).off();
    HANDLER(limit_).on(std::nullopt, saved_limit);
  }

  optional<date_interval_t> price_interval;
  if (HANDLED(period_)) {
    date_interval_t ival(HANDLER(period_).str());
    if (ival.duration)
      price_interval = ival;
  }

  // Destroying the walker removes its posts from the accounts it filed
  // them under, one per priced commodity.  Leaking it instead, as this
  // once did, left them there for the next report in a REPL or embedding
  // session to add into those accounts' totals (#3293).
  walker.reset(new posts_commodities_iterator(*session.journal.get(), HANDLED(latest),
                                              price_interval, HANDLED(both_directions)));
  pass_down_posts<posts_commodities_iterator>(chain, *walker); // NOLINT(bugprone-unused-raii)

  session.journal->clear_xdata();
}

/*--- Built-in Value Expression Functions ---*/

value_t report_t::display_value(const value_t& val) {
  value_t temp(val.strip_annotations(what_to_keep()));
  if (HANDLED(base) || HANDLED(exchange_))
    return temp;
  else
    return temp.unreduced();
}

namespace {
value_t top_amount(const value_t& val) {
  switch (val.type()) {
  case value_t::BALANCE: {
    // The component map is unordered; the "top" amount is the first one in
    // the display order of balance_t::print() (counting a zero component,
    // which print() would skip), found without sorting the whole balance.
    if (const amount_t* top = val.as_balance().first_sorted_amount())
      return *top;
    return value_t();
  }

  case value_t::SEQUENCE: {
    return top_amount(*val.as_sequence().begin());
  }

  default:
    return val;
  }
}
} // namespace

value_t report_t::fn_top_amount(call_scope_t& args) {
  return top_amount(args[0]);
}

value_t report_t::fn_amount_expr(call_scope_t& scope) {
  return HANDLER(amount_).expr.calc(scope);
}

value_t report_t::fn_total_expr(call_scope_t& scope) {
  return HANDLER(total_).expr.calc(scope);
}

value_t report_t::fn_display_amount(call_scope_t& scope) {
  return HANDLER(display_amount_).expr.calc(scope);
}

value_t report_t::fn_display_total(call_scope_t& scope) {
  return HANDLER(display_total_).expr.calc(scope);
}

/// scrub(display_total), reading the register's cached stripped total when
/// the posting has one.  calc_posts (or display_filter_posts) caches the
/// annotation-stripped running total in xdata.display_total; display_value
/// strips it again through balance_t's no-op fast path for a value that
/// carries no annotations, which is exactly what scrub(display_total) did
/// with the cache.
value_t report_t::fn_scrubbed_display_total(call_scope_t& scope) {
  if (post_t* post = search_scope<post_t>(&scope)) {
    if (post->has_xdata() && post->xdata().has_flags(POST_EXT_DISPLAY_TOTAL_CACHED))
      return display_value(post->xdata().display_total);
  }
  return display_value(HANDLER(display_total_).expr.calc(scope));
}

value_t report_t::fn_display_account(call_scope_t& scope) {
  value_t acct = HANDLER(display_account_).expr.calc(scope);
  if (post_t* post = search_scope<post_t>(&scope))
    return post->decorate_account_name(acct);
  return acct;
}

value_t report_t::fn_should_bold(call_scope_t& scope) {
  if (HANDLED(bold_if_))
    return HANDLER(bold_if_).expr.calc(scope);
  else
    return false;
}

value_t report_t::fn_averaged_lots(call_scope_t& args) {
  if (args.has<balance_t>(0))
    return average_lot_prices(args.get<balance_t>(0));
  else
    return args[0];
}

value_t report_t::fn_fifo_lots(call_scope_t& args) {
  if (args.has<balance_t>(0))
    return fifo_lot_prices(args.get<balance_t>(0));
  else
    return args[0];
}

value_t report_t::fn_lifo_lots(call_scope_t& args) {
  if (args.has<balance_t>(0))
    return lifo_lot_prices(args.get<balance_t>(0));
  else
    return args[0];
}

value_t report_t::fn_market(call_scope_t& args) {
  value_t result;
  value_t arg0 = args[0];

  datetime_t moment;
  if (args.has<datetime_t>(1))
    moment = args.get<datetime_t>(1);

  if (arg0.is_string()) {
    amount_t tmp(1L);
    commodity_t* commodity = commodity_pool_t::current_pool->find_or_create(arg0.as_string());
    tmp.set_commodity(*commodity);
    arg0 = tmp;
  }

  string target_commodity;
  if (args.has<string>(2))
    target_commodity = args.get<string>(2);

  // When arg0 is a sequence (e.g., from the cleared or budget command's
  // amount tuple), apply market conversion to each element individually.
  // A sequence returned by value_t::value() is never is_null() even when all
  // its elements are NULL_VALUE, so the fallback below would not trigger and
  // the display total would become a sequence of nulls, causing no accounts
  // to be marked for display.
  if (arg0.is_sequence()) {
    value_t temp;
    for (value_t elem : arg0.as_sequence()) {
      value_t elem_result;
      if (!target_commodity.empty())
        elem_result = elem.exchange_commodities(target_commodity,
                                                /* add_prices= */ false, moment);
      else
        elem_result = elem.value(moment);
      // Only fall back to the original element if it carries a value.
      // Integer zeros used as placeholders in budget/cleared tuples should
      // remain null so they display as blank rather than "0".
      if (elem_result.is_null() && !elem.is_long())
        temp.push_back(elem);
      else
        temp.push_back(elem_result);
    }
    return temp;
  }

  if (!target_commodity.empty())
    result = arg0.exchange_commodities(target_commodity,
                                       /* add_prices= */ false, moment);
  else
    result = arg0.value(moment);

  // If the exchange failed and the input is an annotated amount whose lot
  // price is denominated in the target commodity, use the lot annotation
  // price as a fallback.  This handles postings that predate all price
  // history entries when -H is used together with -X or -V.
  if (result.is_null() && !target_commodity.empty() && arg0.is_amount()) {
    const amount_t& amt = arg0.as_amount();
    if (amt.has_annotation()) {
      const annotation_t& ann = amt.annotation();
      if (ann.price) {
        commodity_t* target = commodity_pool_t::current_pool->find(target_commodity);
        if (target && &ann.price->commodity().referent() == &target->referent()) {
          // Compute total value: lot_price_per_unit * quantity
          amount_t stripped = amt.strip_annotations(keep_details_t());
          result = *ann.price * stripped.number();
        }
      }
    }
  }

  return !result.is_null() ? result : arg0;
}

value_t report_t::fn_get_at(call_scope_t& args) {
  std::size_t index = static_cast<std::size_t>(args.get<long>(1));
  if (index == 0) {
    if (!args[0].is_sequence())
      return args[0];
  } else if (!args[0].is_sequence()) {
    // Return void for non-sequence values at index > 0 so that
    // callers using fallback expressions (e.g. "get_at(x, 1) || 0"
    // in --dc format strings) work instead of crashing (issue #697).
    return value_t();
  }

  value_t::sequence_t& seq(args[0].as_sequence_lval());
  // An index past the end also yields void, for the same reason (#697).
  if (index >= seq.size())
    return value_t();

  return seq[index];
}

value_t report_t::fn_is_seq(call_scope_t& scope) {
  return scope.value().is_sequence();
}

value_t report_t::fn_strip(call_scope_t& args) {
  return args.value().strip_annotations(what_to_keep());
}

value_t report_t::fn_trim(call_scope_t& args) {
  return string_value(trim_copy(args.value().to_string()));
}

value_t report_t::fn_format(call_scope_t& args) {
  format_t format(args.get<string>(0));
  std::ostringstream out;
  out << format(args);
  return string_value(out.str());
}

value_t report_t::fn_print(call_scope_t& args) {
  for (std::size_t i = 0; i < args.size(); i++)
    args[i].print(output_stream);
  static_cast<std::ostream&>(output_stream) << '\n';
  return true;
}

value_t report_t::fn_scrub(call_scope_t& args) {
  return display_value(args.value());
}

value_t report_t::fn_rounded(call_scope_t& args) {
  value_t val(args.value());
  // #3187: when a cost basis is in a commodity that has no declared display
  // precision -- a dollar that appears only inside `@`/`@@` price annotations,
  // never as a posting amount -- there is no precision to round to, and
  // rounding would discard the value's fractional content (rendering a basis
  // like 20 ETH * `@ $5.025` = $100.500 as $100).  Leave such an amount as is.
  if (val.is_amount()) {
    const amount_t& amt(val.as_amount());
    if (amt.has_commodity() && amt.commodity().precision() == 0 && amt.precision() > 0)
      return val;
  }
  // Display-only rounding (#3220): clear keep_precision so the value prints at
  // commodity precision, but leave the stored value exact so a balance
  // accumulates full precision and rounds only once at display time.  This
  // backs the --basis/--cost amount expression rounded(cost).  (Per-posting
  // physical rounding for --round goes through fn_roundto_commodity below.)
  return val.rounded();
}

value_t report_t::fn_roundto_commodity(call_scope_t& args) {
  // Physically round to commodity precision before aggregation, for the
  // --round option (#781): this mutates the stored value so each posting is
  // rounded before it is summed, matching how a broker statement totals
  // positions that have each already been rounded to the cent.
  return args.value().rounded_to_commodity_precision();
}

value_t report_t::fn_unrounded(call_scope_t& args) {
  return args.value().unrounded();
}

value_t report_t::fn_quantity(call_scope_t& args) {
  value_t val(args[0]);
  if (val.is_balance()) {
    const balance_t& bal(val.as_balance());
    if (bal.single_amount())
      return bal.amounts.begin()->second.number();
    // A balance of several commodities has no single quantity; yield 0.
    return 0L;
  }
  return val.to_amount().number();
}

value_t report_t::fn_truncate(call_scope_t& args) {
  return args[0].truncated();
}

value_t report_t::fn_floor(call_scope_t& args) {
  return args[0].floored();
}

value_t report_t::fn_ceiling(call_scope_t& args) {
  return args[0].ceilinged();
}

value_t report_t::fn_round(call_scope_t& args) {
  return args.has<int>(1) ? fn_roundto(args) : args[0].rounded();
}

value_t report_t::fn_roundto(call_scope_t& args) {
  return args[0].roundto(args.get<int>(1));
}

value_t report_t::fn_abs(call_scope_t& args) {
  return args[0].abs();
}

value_t report_t::fn_truncated(call_scope_t& args) {
  value_t& val = args[0];
  // If no width argument is provided, do numeric truncation for amounts and
  // balances (used in automated transaction expressions like truncated(amount)).
  // If a width argument is provided, do string display truncation (used by the
  // select command for column width formatting).
  if (!args.has(1)) {
    if (val.is_amount() || val.is_balance() || val.is_long()) {
      return val.truncated();
    }
  }
  return string_value(format_t::truncate(
      val.to_string(),
      (args.has<int>(1) && args.get<int>(1) > 0) ? static_cast<std::size_t>(args.get<int>(1)) : 0,
      (args.has<int>(2) && args.get<int>(2) > 0) ? static_cast<std::size_t>(args.get<int>(2)) : 0));
}

value_t report_t::fn_justify(call_scope_t& args) {
  uint_least8_t flags(AMOUNT_PRINT_ELIDE_COMMODITY_QUOTES);

  if (args.has<bool>(3) && args.get<bool>(3))
    flags |= AMOUNT_PRINT_RIGHT_JUSTIFY;
  if (args.has<bool>(4) && args.get<bool>(4))
    flags |= AMOUNT_PRINT_COLORIZE;
  // Optional 6th argument: widen the first/latter widths uniformly to fit
  // the widest commodity in a multi-commodity balance (used by the balance
  // report to align amounts whose rendered form overflows the nominal
  // column width; see issue #1795).
  if (args.has<bool>(5) && args.get<bool>(5))
    flags |= AMOUNT_PRINT_FIT_TO_WIDEST;

  // Colorized negative amounts are red, or whatever color --colors makes red.
  std::ostringstream out;
  args[0].print(out, args.get<int>(1), args.has<int>(2) ? args.get<int>(2) : -1, flags,
                (flags & AMOUNT_PRINT_COLORIZE) ? color_escape("red") : string());

  return string_value(out.str());
}

value_t report_t::fn_quoted(call_scope_t& args) {
  std::ostringstream out;

  out << '"';
  string arg(args.get<string>(0));
  for (const char ch : arg) {
    if (ch == '"')
      out << "\\\"";
    else
      out << ch;
  }
  out << '"';

  return string_value(out.str());
}

value_t report_t::fn_quoted_rfc(call_scope_t& args) {
  std::ostringstream out;

  out << '"';
  string arg(args.get<string>(0));
  for (const char ch : arg) {
    if (ch == '"')
      out << '"' << '"';
    else
      out << ch;
  }
  out << '"';

  return string_value(out.str());
}

value_t report_t::fn_join(call_scope_t& args) {
  std::ostringstream out;

  string arg(args.get<string>(0));
  for (const char ch : arg) {
    if (ch != '\n')
      out << ch;
    else
      out << "\\n";
  }
  return string_value(out.str());
}

namespace {
/// Keep (keep) or drop (!keep) the first (from_front) or last N ':'-separated
/// components of the account name in args[0]; N is args[1], clamped to
/// [0, depth].
string account_slice(call_scope_t& args, bool from_front, bool keep) {
  const string name(args.get<string>(0));
  const long count = args.get<long>(1);
  std::vector<string> parts;
  boost::split(parts, name, boost::is_any_of(":"));
  const std::size_t n = std::min(parts.size(), count < 0 ? std::size_t(0) : std::size_t(count));
  const auto cut = parts.begin() + static_cast<std::ptrdiff_t>(from_front ? n : parts.size() - n);
  return keep == from_front ? boost::join(boost::make_iterator_range(parts.begin(), cut), ":")
                            : boost::join(boost::make_iterator_range(cut, parts.end()), ":");
}
} // namespace

value_t report_t::fn_account_prefix(call_scope_t& args) {
  return string_value(account_slice(args, true, true));
}

value_t report_t::fn_account_suffix(call_scope_t& args) {
  return string_value(account_slice(args, false, true));
}

value_t report_t::fn_account_skip_prefix(call_scope_t& args) {
  return string_value(account_slice(args, true, false));
}

value_t report_t::fn_account_skip_suffix(call_scope_t& args) {
  return string_value(account_slice(args, false, false));
}

value_t report_t::fn_format_date(call_scope_t& args) {
  if (args.has<string>(1))
    return string_value(format_date(args.get<date_t>(0), FMT_CUSTOM, args.get<string>(1).c_str()));
  else
    return string_value(format_date(args.get<date_t>(0), FMT_PRINTED));
}

value_t report_t::fn_format_datetime(call_scope_t& args) {
  if (args.has<string>(1))
    return string_value(
        format_datetime(args.get<datetime_t>(0), FMT_CUSTOM, args.get<string>(1).c_str()));
  else
    return string_value(format_datetime(args.get<datetime_t>(0), FMT_PRINTED));
}

/*--- Terminal Colors ---*/

namespace {

/// The colors that formats name, such as blue for account names and red for
/// negative amounts, with the parameters of the ANSI "Select Graphic
/// Rendition" escape sequence (ESC [ parameters m) that renders each.
/// --colors can make any of these names stand for another color.
const std::pair<const char*, const char*> standard_colors[] = {
    {"black", "30"}, {"red", "31"},      {"green", "32"}, {"yellow", "33"},
    {"blue", "34"},  {"magenta", "35"},  {"cyan", "36"},  {"white", "37"},
    {"bold", "1"},   {"underline", "4"}, {"blink", "5"}};

/// Return the SGR parameters of the standard color @p name, or nullptr if
/// there is no such color.
const char* standard_color(const string& name) {
  for (const auto& [color_name, parameters] : standard_colors)
    if (name == color_name)
      return parameters;
  return nullptr;
}

/// Whether @p word is a 256-color index, a number from 0 to 255.
bool is_color_index(const string& word) {
  return !word.empty() && word.size() <= 3 &&
         std::all_of(word.begin(), word.end(), [](unsigned char c) { return std::isdigit(c); }) &&
         std::stoi(word) <= 255;
}

/// Whether @p word is a 24-bit color written #RRGGBB.
bool is_rgb_color(const string& word) {
  return word.size() == 7 && word[0] == '#' &&
         std::all_of(word.begin() + 1, word.end(),
                     [](unsigned char c) { return std::isxdigit(c); });
}

/// Translate @p color into SGR parameters.  Each of its words is a color
/// name, which means what @p palette assigns it or else the standard color;
/// a 256-color index from 0 to 255; or a 24-bit color written #RRGGBB.  So
/// "bold #5f87ff" becomes "1;38;2;95;135;255", and an empty color nothing.
string sgr_parameters(const string& color, const std::map<string, string>& palette) {
  string parameters;
  string::size_type end = 0;
  for (string::size_type begin = color.find_first_not_of(" \t", end); begin != string::npos;
       begin = color.find_first_not_of(" \t", end)) {
    end = color.find_first_of(" \t", begin);
    const string word = color.substr(begin, end - begin);

    string parameter;
    if (auto assigned = palette.find(word); assigned != palette.end()) {
      parameter = assigned->second;
    } else if (const char* standard = standard_color(word)) {
      parameter = standard;
    } else if (is_color_index(word)) {
      parameter = "38;5;" + std::to_string(std::stoi(word));
    } else if (is_rgb_color(word)) {
      parameter = "38;2;" + std::to_string(std::stoi(word.substr(1, 2), nullptr, 16)) + ";" +
                  std::to_string(std::stoi(word.substr(3, 2), nullptr, 16)) + ";" +
                  std::to_string(std::stoi(word.substr(5, 2), nullptr, 16));
    } else {
      throw_(std::runtime_error, _f("Invalid color '%1%'; expected a color name, a number from 0 "
                                    "to 255, or #RRGGBB") %
                                     word);
    }

    if (!parameter.empty()) {
      if (!parameters.empty())
        parameters += ';';
      parameters += parameter;
    }
  }
  return parameters;
}

} // namespace

string report_t::color_escape(const string& color) {
  const string parameters = sgr_parameters(color, HANDLER(colors_).palette);
  return parameters.empty() ? string() : "\033[" + parameters + "m";
}

void report_t::assign_colors(const string& assignments, std::map<string, string>& palette) {
  // Assign to a copy, so that a mistake leaves the palette as it was.
  std::map<string, string> assigned(palette);

  std::istringstream in(assignments);
  string assignment;
  while (std::getline(in, assignment, ',')) {
    trim(assignment);
    if (assignment.empty())
      continue;

    const string::size_type equals = assignment.find('=');
    if (equals == string::npos)
      throw_(option_error, _f("Invalid color assignment '%1%'; expected NAME=COLOR") % assignment);

    const string name = trim_copy(assignment.substr(0, equals));
    if (!standard_color(name))
      throw_(option_error, _f("Invalid color name '%1%'; expected black, red, green, yellow, "
                              "blue, magenta, cyan, white, bold, underline or blink") %
                               name);

    // The names in the color itself mean the standard colors, so that
    // "red=green,green=red" swaps the two rather than making both green.
    assigned[name] = sgr_parameters(assignment.substr(equals + 1), {});
  }

  palette.swap(assigned);
}

value_t report_t::fn_ansify_if(call_scope_t& args) {
  // A null color, as "blue if color" yields when color is off, leaves the
  // value alone, as does a boolean, which is a condition and not a color.
  if (args.has<string>(1) && !args[1].is_boolean()) {
    const string escape = color_escape(args.get<string>(1));
    if (!escape.empty()) {
      std::ostringstream buf;
      buf << escape << args[0] << "\033[0m";
      return string_value(buf.str());
    }
  }
  return args[0];
}

value_t report_t::fn_percent(call_scope_t& args) {
  return (amount_t("100.00%") *
          (args.get<amount_t>(0).reduced() / args.get<amount_t>(1).reduced()).number());
}

value_t report_t::fn_commodity(call_scope_t& args) {
  value_t val(args[0]);
  if (val.is_balance()) {
    const balance_t& bal(val.as_balance());
    if (bal.single_amount())
      return bal.amounts.begin()->second.commodity().strip_annotations(keep_details_t{});
    return string_value(empty_string);
  }
  return val.to_amount().commodity().strip_annotations(keep_details_t{});
}

value_t report_t::fn_commodity_price(call_scope_t& args) {
  std::optional<price_point_t> price_point =
      commodity_pool_t::current_pool->commodity_price_history.find_price(
          args.get<amount_t>(0).commodity(), args.get<datetime_t>(1));
  if (price_point) {
    return price_point->price;
  } else {
    return amount_t();
  }
}

value_t report_t::fn_set_commodity_price(call_scope_t& args) {
  args.get<amount_t>(0).commodity().add_price(args.get<datetime_t>(1), args.get<amount_t>(2), true);
  return NULL_VALUE;
}

value_t report_t::fn_clear_commodity(call_scope_t& args) {
  amount_t amt(args.get<amount_t>(0));
  amt.clear_commodity();
  return amt;
}

value_t report_t::fn_nail_down(call_scope_t& args) {
  value_t arg0(args[0]);
  value_t arg1(args[1]);

  switch (arg0.type()) {
  case value_t::AMOUNT: {
    amount_t tmp(arg0.as_amount());
    if (tmp.has_commodity() && !tmp.is_null() && !tmp.is_realzero()) {
      value_t stripped = arg1.strip_annotations(keep_details_t());

      // When the market value is a balance with multiple commodities
      // (e.g., because not all commodities could be converted to the
      // target), extract just the component matching this amount's
      // commodity for the per-unit cost calculation.
      if (stripped.is_balance()) {
        const balance_t& bal = stripped.as_balance();
        if (optional<amount_t> comm_amt = bal.commodity_amount(&tmp.commodity())) {
          arg1 = *comm_amt;
        } else if (bal.amounts.size() == 1) {
          arg1 = bal.amounts.begin()->second;
        } else {
          return tmp;
        }
      } else {
        arg1 = stripped.to_amount();
      }
      expr_t value_expr(is_expr(arg1) ? as_expr(arg1)
                                      : expr_t::op_t::wrap_value(arg1.unrounded() / arg0.number()));
      std::ostringstream buf;
      value_expr.print(buf);
      value_expr.set_text(buf.str());

      tmp.set_commodity(tmp.commodity().nail_down(value_expr));
    }
    return tmp;
  }

  case value_t::BALANCE: {
    balance_t tmp;
    for (const balance_t::amounts_map::value_type& pair : arg0.as_balance_lval().amounts) {
      call_scope_t inner_args(*args.parent);
      inner_args.push_back(pair.second);
      inner_args.push_back(arg1);
      tmp += fn_nail_down(inner_args).as_amount();
    }
    return tmp;
  }

  case value_t::SEQUENCE: {
    // A market sequence of the same length (e.g. the (amount, cost) pair
    // of -G or --plopen under -H) pairs each element with its own value.
    const bool paired = arg1.is_sequence() && arg1.size() == arg0.size();
    value_t tmp;
    std::size_t i = 0;
    for (value_t& value : arg0.as_sequence_lval()) {
      call_scope_t inner_args(*args.parent);
      inner_args.push_back(value);
      inner_args.push_back(paired ? arg1[i] : arg1);
      tmp.push_back(fn_nail_down(inner_args));
      ++i;
    }
    return tmp;
  }

  default:
    throw_(std::runtime_error, _f("Attempting to nail down %1%") % args[0].label());
  }
  return arg0;
}

value_t report_t::fn_to_boolean(call_scope_t& args) {
  return args.get<bool>(0);
}

value_t report_t::fn_to_int(call_scope_t& args) {
  // This method is not called fn_to_long, because that would be
  // confusing to users who don't care about the distinction between
  // integer and long.
  return args.get<long>(0);
}

value_t report_t::fn_to_datetime(call_scope_t& args) {
  return args.get<datetime_t>(0);
}

value_t report_t::fn_to_date(call_scope_t& args) {
  return args.get<date_t>(0);
}

value_t report_t::fn_to_amount(call_scope_t& args) {
  return args.get<amount_t>(0);
}

value_t report_t::fn_to_balance(call_scope_t& args) {
  return args.get<balance_t>(0);
}

value_t report_t::fn_to_string(call_scope_t& args) {
  return string_value(args.get<string>(0));
}

value_t report_t::fn_to_mask(call_scope_t& args) {
  return args.get<mask_t>(0);
}

value_t report_t::fn_to_sequence(call_scope_t& args) {
  return args[0].to_sequence();
}

/*--- Sentinel Functions ---*/

namespace {
value_t fn_false(call_scope_t&) {
  return false;
}
value_t fn_null(call_scope_t&) {
  return NULL_VALUE;
}
} // namespace

/*--- Interactive Commands ---*/

value_t report_t::reload_command(call_scope_t&) {
  session.close_journal_files();
  session.read_journal_files();
  return true;
}

value_t report_t::source_command(call_scope_t& args) {
  if (args.has(0))
    session.read_journal(path(args.get<string>(0)));
  else
    session.read_journal_files();
  return true;
}

value_t report_t::echo_command(call_scope_t& args) {
  std::ostream& out(output_stream);
  out << args.get<string>(0) << '\n';
  return true;
}

value_t report_t::pricemap_command(call_scope_t& args) {
  std::ostream& out(output_stream);
  commodity_pool_t::current_pool->commodity_price_history.print_map(
      out, args.has<string>(0) ? datetime_t(parse_date(args.get<string>(0))) : datetime_t());
  return true;
}

/*--- Option Lookup (first-character dispatched switch table) ---*/

option_t<report_t>* report_t::lookup_option(const char* p) {
  switch (*p) {
  case '%': // NOLINT(bugprone-branch-clone)
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
    else OPT(account_rewrite_);
    else OPT(actual);
    else OPT(add_budget);
    else OPT(amount_);
    else OPT(amount_data);
    else OPT_ALT(primary_date, actual_dates);
    else OPT(align_intervals);
    else OPT(anon);
    else OPT(append_format_);
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
    else OPT(both_directions);
    else OPT(budget);
    else OPT(budget_format_);
    else OPT(by_payee);
    break;
  case 'c':
    OPT(csv_format_);
    else OPT(csv_separator_);
    else OPT_ALT(gain, change);
    else OPT(cleared);
    else OPT(cleared_format_);
    else OPT(collapse);
    else OPT(collapse_if_);
    else OPT(collapse_if_zero);
    else OPT(color);
    else OPT(colors_);
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
    else OPT(decimal_places_);
    else OPT(depth_);
    else OPT(deviation);
    else OPT_ALT(rich_data, detail);
    else OPT_(display_);
    else OPT(display_account_);
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
    else OPT(gain_since_);
    else OPT(group_by_);
    else OPT(group_by_cumulative);
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
    else OPT(ignore_diacritics);
    else OPT(immediate);
    break;
  case 'j':
    OPT_CH(amount_data);
    break;
  case 'l':
    OPT_(limit_);
    else OPT(latest);
    else OPT(lisp_date_format_);
    else OPT(lot_dates);
    else OPT(lot_prices);
    else OPT_ALT(lot_notes, lot_tags);
    else OPT(lots);
    else OPT(lots_actual);
    else OPT(lots_fifo);
    else OPT(lots_lifo);
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
    else OPT(no_group_by);
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
    else OPT(payee_rewrite_);
    else OPT(pending);
    else OPT(percent);
    else OPT_(period_);
    else OPT_ALT(sort_xacts_, period_sort_);
    else OPT(pivot_);
    else OPT(pivot_only_);
    else OPT(plopen);
    else OPT(plot_amount_format_);
    else OPT(plot_total_format_);
    else OPT(price);
    else OPT(prices_format_);
    else OPT(pricedb_format_);
    else OPT(primary_date);
    else OPT(payee_width_);
    else OPT(period_shift_);
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
    else OPT(round);
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
  default:
    break;
  }
  return nullptr;
}

/*--- Symbol Lookup and Command Dispatch ---*/

void report_t::define(const symbol_t::kind_t kind, const string& name,
                      const expr_t::ptr_op_t& def) {
  session.define(kind, name, def);
}

namespace {
using report_fn_t = value_t (report_t::*)(call_scope_t&);

/// The functions report_t binds by name (phase 2 of report_t::lookup).
constexpr symbol_entry_t<report_fn_t> report_functions[] = {
    {"abs", &report_t::fn_abs},
    {"account_prefix", &report_t::fn_account_prefix},
    {"account_skip_prefix", &report_t::fn_account_skip_prefix},
    {"account_skip_suffix", &report_t::fn_account_skip_suffix},
    {"account_suffix", &report_t::fn_account_suffix},
    {"amount_expr", &report_t::fn_amount_expr},
    {"ansify_if", &report_t::fn_ansify_if},
    {"averaged_lots", &report_t::fn_averaged_lots},
    {"ceiling", &report_t::fn_ceiling},
    {"clear_commodity", &report_t::fn_clear_commodity},
    {"commodity", &report_t::fn_commodity},
    {"commodity_price", &report_t::fn_commodity_price},
    {"date", &report_t::fn_today},
    {"display_account", &report_t::fn_display_account},
    {"display_amount", &report_t::fn_display_amount},
    {"display_total", &report_t::fn_display_total},
    {"fifo_lots", &report_t::fn_fifo_lots},
    {"floor", &report_t::fn_floor},
    {"format", &report_t::fn_format},
    {"format_date", &report_t::fn_format_date},
    {"format_datetime", &report_t::fn_format_datetime},
    {"get_at", &report_t::fn_get_at},
    {"is_seq", &report_t::fn_is_seq},
    {"join", &report_t::fn_join},
    {"justify", &report_t::fn_justify},
    {"lifo_lots", &report_t::fn_lifo_lots},
    {"market", &report_t::fn_market},
    {"nail_down", &report_t::fn_nail_down},
    {"now", &report_t::fn_now},
    {"options", &report_t::fn_options},
    {"percent", &report_t::fn_percent},
    {"print", &report_t::fn_print},
    {"quantity", &report_t::fn_quantity},
    {"quoted", &report_t::fn_quoted},
    {"quoted_rfc", &report_t::fn_quoted_rfc},
    {"round", &report_t::fn_round},
    {"rounded", &report_t::fn_rounded},
    {"roundto", &report_t::fn_roundto},
    {"roundto_commodity", &report_t::fn_roundto_commodity},
    {"scrub", &report_t::fn_scrub},
    {"scrubbed_display_total", &report_t::fn_scrubbed_display_total},
    {"set_commodity_price", &report_t::fn_set_commodity_price},
    {"should_bold", &report_t::fn_should_bold},
    {"strip", &report_t::fn_strip},
    {"to_amount", &report_t::fn_to_amount},
    {"to_balance", &report_t::fn_to_balance},
    {"to_boolean", &report_t::fn_to_boolean},
    {"to_date", &report_t::fn_to_date},
    {"to_datetime", &report_t::fn_to_datetime},
    {"to_int", &report_t::fn_to_int},
    {"to_mask", &report_t::fn_to_mask},
    {"to_sequence", &report_t::fn_to_sequence},
    {"to_string", &report_t::fn_to_string},
    {"today", &report_t::fn_today},
    {"top_amount", &report_t::fn_top_amount},
    {"total_expr", &report_t::fn_total_expr},
    {"trim", &report_t::fn_trim},
    {"truncate", &report_t::fn_truncate},
    {"truncated", &report_t::fn_truncated},
    {"unround", &report_t::fn_unrounded},
    {"unrounded", &report_t::fn_unrounded},
    {"value_date", &report_t::fn_now},
};
static_assert(symbol_table_sorted(report_functions), "report_functions must stay sorted");
} // namespace

expr_t::ptr_op_t report_t::lookup(const symbol_t::kind_t kind, const string& name) {
  if (expr_t::ptr_op_t def = session.lookup(kind, name))
    return def;

  const char* p = name.c_str();

  switch (kind) {
  case symbol_t::FUNCTION:
    // Phase 1: Support 2.x's single-letter value expression names.
    // These are legacy compatibility aliases (d/m -> now, P -> market, etc.).
    if (*(p + 1) == '\0') {
      switch (*p) {
      case 'd': // NOLINT(bugprone-branch-clone)
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
        throw_(std::runtime_error, _("The i value expression variable is no longer supported"));
      case 'A':
        throw_(std::runtime_error, _("The A value expression variable is no longer supported"));
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
        return nullptr;
      }
    }

    // Phase 2: Multi-character function names, from report_functions; the
    // color names, from standard_colors; and two constant sentinels.
    if (const report_fn_t* fn = find_symbol(report_functions, name))
      return expr_t::op_t::wrap_functor(
          [this, member = *fn](call_scope_t& args) { return (this->*member)(args); });
    if (standard_color(name))
      return expr_t::op_t::wrap_functor([name](call_scope_t&) { return string_value(name); });
    if (name == "null")
      return WRAP_FUNCTOR(fn_null);
    if (name == "post")
      return WRAP_FUNCTOR(fn_false);

    // Phase 3: Check if they are trying to access an option's setting or value.
    // Options can be read from expressions (e.g., "options.flat" in format strings).
    if (option_t<report_t>* handler = lookup_option(p))
      return MAKE_OPT_FUNCTOR(report_t, handler);
    break;

  case symbol_t::OPTION:
    // Option lookup: invoked by process_arguments() and process_option()
    // when the user passes --option-name on the command line.
    if (option_t<report_t>* handler = lookup_option(p))
      return MAKE_OPT_HANDLER(report_t, handler);
    break;

    // Command dispatch macros: these create reporter<> functors that bind a
    // formatter to a report driver method.  When the command is invoked, the
    // reporter parses query arguments and calls the report method.

#define POSTS_REPORTER(formatter)                                                                  \
  WRAP_FUNCTOR(reporter<>(post_handler_ptr(formatter), *this, string("#") + p))

    // Can't use WRAP_FUNCTOR here because the template arguments
    // confuse the parser
#define POSTS_REPORTER_(method, formatter)                                                         \
  expr_t::op_t::wrap_functor(reporter<post_t, post_handler_ptr, method>(                           \
      post_handler_ptr(formatter), *this, string("#") + p))

#define FORMATTED_POSTS_REPORTER(format)                                                           \
  POSTS_REPORTER(new format_posts(                                                                 \
      *this, report_format(HANDLER(format)), maybe_format(HANDLER(prepend_format_)),               \
      HANDLED(prepend_width_) ? static_cast<std::size_t>(HANDLER(prepend_width_).as_int()) : 0,    \
      maybe_format(HANDLER(append_format_))))

#define FORMATTED_COMMODITIES_REPORTER(format)                                                     \
  POSTS_REPORTER_(                                                                                 \
      &report_t::commodities_report,                                                               \
      new format_posts(                                                                            \
          *this, report_format(HANDLER(format)), maybe_format(HANDLER(prepend_format_)),           \
          HANDLED(prepend_width_) ? static_cast<std::size_t>(HANDLER(prepend_width_).as_int())     \
                                  : 0,                                                             \
          maybe_format(HANDLER(append_format_))))

#define ACCOUNTS_REPORTER(formatter)                                                               \
  expr_t::op_t::wrap_functor(reporter<account_t, acct_handler_ptr, &report_t::accounts_report>(    \
      acct_handler_ptr(formatter), *this, string("#") + p))

#define FORMATTED_ACCOUNTS_REPORTER(format)                                                        \
  ACCOUNTS_REPORTER(new format_accounts(                                                           \
      *this, report_format(HANDLER(format)), maybe_format(HANDLER(prepend_format_)),               \
      HANDLED(prepend_width_) ? static_cast<std::size_t>(HANDLER(prepend_width_).as_int()) : 0,    \
      maybe_format(HANDLER(append_format_))))

  case symbol_t::COMMAND:
    // Command dispatch: maps user-facing command names (balance, register, print, etc.)
    // to reporter<> functors that bind a formatter to the appropriate report driver.
    switch (*p) { // NOLINT(bugprone-branch-clone)
    case 'a':
      if (is_eq(p, "accounts")) {
        return POSTS_REPORTER_(&report_t::posts_report_quick, new report_accounts(*this));
      }
      break;

    case 'b':
      if (*(p + 1) == '\0' || is_eq(p, "bal") || is_eq(p, "balance")) {
        // jww (2023-01-27): This next 'if' statement is a hack for historical
        // purposes. Until this date, the balance report always used an amount
        // width of 20. If the user has set the amount width, this should be
        // used instead; but if they haven't, we need to use the old default
        // in order for the tests to pass.
        if (!HANDLED(amount_width_))
          HANDLER(amount_width_).value = "20";
        return FORMATTED_ACCOUNTS_REPORTER(balance_format_);
      } else if (is_eq(p, "budget")) {
        HANDLER(amount_).on(string("#budget"), "(amount, 0)");

        budget_flags |= BUDGET_WRAP_VALUES;
        if (!(budget_flags & ~BUDGET_WRAP_VALUES))
          budget_flags |= BUDGET_BUDGETED;

        return FORMATTED_ACCOUNTS_REPORTER(budget_format_);
      }
      break;

    case 'c':
      if (is_eq(p, "csv")) { // NOLINT(bugprone-branch-clone)
        return FORMATTED_POSTS_REPORTER(csv_format_);
      } else if (is_eq(p, "cleared")) {
        HANDLER(amount_).on(string("#cleared"), "(amount, cleared ? amount : 0)");
        return FORMATTED_ACCOUNTS_REPORTER(cleared_format_);
      } else if (is_eq(p, "convert")) {
        return WRAP_FUNCTOR(convert_command);
      } else if (is_eq(p, "commodities")) {
        return POSTS_REPORTER_(&report_t::posts_report_quick, new report_commodities(*this));
      }
      break;
    case 'd':
      if (is_eq(p, "draft")) {
        return WRAP_FUNCTOR(xact_command);
      }
      break;
    case 'e':
      if (is_eq(p, "equity")) { // NOLINT(bugprone-branch-clone)
        HANDLER(generated).on("#equity");
        return POSTS_REPORTER(new print_xacts(*this));
      } else if (is_eq(p, "entry")) {
        return WRAP_FUNCTOR(xact_command);
      } else if (is_eq(p, "emacs")) {
        return POSTS_REPORTER(new format_emacs_posts(*this, output_stream));
      } else if (is_eq(p, "echo")) {
        return MAKE_FUNCTOR(report_t::echo_command);
      }
      break;

    case 'l':
      if (is_eq(p, "lisp"))
        return POSTS_REPORTER(new format_emacs_posts(*this, output_stream));
      break;

    case 'p':
      if (*(p + 1) == '\0' || is_eq(p, "print")) { // NOLINT(bugprone-branch-clone)
        return POSTS_REPORTER(new print_xacts(*this, HANDLED(raw)));
      } else if (is_eq(p, "prices")) {
        return FORMATTED_COMMODITIES_REPORTER(prices_format_);
      } else if (is_eq(p, "pricedb") || is_eq(p, "pricesdb")) {
        return FORMATTED_COMMODITIES_REPORTER(pricedb_format_);
      } else if (is_eq(p, "pricemap")) {
        return MAKE_FUNCTOR(report_t::pricemap_command);
      } else if (is_eq(p, "payees")) {
        return POSTS_REPORTER_(&report_t::posts_report_quick, new report_payees(*this));
      }
      break;

    case 'r':
      if (*(p + 1) == '\0' || is_eq(p, "reg") ||
          is_eq(p, "register")) { // NOLINT(bugprone-branch-clone)
        return FORMATTED_POSTS_REPORTER(register_format_);
      } else if (is_eq(p, "reload")) {
        return MAKE_FUNCTOR(report_t::reload_command);
      }
      break;

    case 's':
      if (is_eq(p, "stats") || is_eq(p, "stat"))
        return POSTS_REPORTER_(&report_t::posts_report_quick, new report_statistics(*this));
      else if (is_eq(p, "select"))
        return WRAP_FUNCTOR(select_command);
      break;
    case 't':
      if (is_eq(p, "tags")) {
        return POSTS_REPORTER_(&report_t::posts_report_quick, new report_tags(*this));
      }
      break;
    case 'x':
      if (is_eq(p, "xact"))
        return WRAP_FUNCTOR(xact_command);
      else if (is_eq(p, "xml"))
        return POSTS_REPORTER(new format_ptree(*this));
      break;
    default:
      break;
    }
    break;

  case symbol_t::PRECOMMAND:
    // Precommands run before the journal is fully read; they are debugging
    // and utility commands (eval, parse, query, generate, etc.).
    switch (*p) {
    case 'a': // NOLINT(bugprone-branch-clone)
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
        return POSTS_REPORTER_(&report_t::generate_report, new print_xacts(*this));
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
        return WRAP_FUNCTOR(script_command);
      else if (is_eq(p, "source"))
        return MAKE_FUNCTOR(report_t::source_command);
      break;
    case 't':
      if (is_eq(p, "template"))
        return WRAP_FUNCTOR(template_command);
      break;
    default:
      break;
    }
    break;

  default:
    break;
  }

  return nullptr;
}

} // namespace ledger
