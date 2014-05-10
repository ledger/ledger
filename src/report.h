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

/**
 * @addtogroup report
 */

/**
 * @file   report.h
 * @author John Wiegley
 *
 * @ingroup report
 */
#ifndef _REPORT_H
#define _REPORT_H

#include "expr.h"
#include "query.h"
#include "chain.h"
#include "stream.h"
#include "option.h"
#include "commodity.h"
#include "annotate.h"
#include "session.h"
#include "format.h"

namespace ledger {

class session_t;
class xact_t;

// These are the elements of any report:
//
// 1. Formatting string used for outputting the underlying ReportedType.
//
// 2. Handler object for the ReportedType.  This is constructed using #1, or
//    else #1 is ignored completely.  This handler object is also constructed
//    with the output stream that will be used during formatting.
//
// --- The details of #1 and #2 together represent the ItemHandler.
//
// 3. Mode of the report.  Currently there are four modes:
//
//    a. Posting or commodity iteration.  In this mode, all the journal's
//       xacts, the postings of a specific xact, or all the journal's
//       commodities are walked.  In the first two cases, it's the underlying
//       postings which are passed to #2; in the second case, each
//       commodity is passed to #2.
//
//    b. Account iteration.  This employs step 'a', but add a prologue and
//       epilogue to it.  In the prologue it "sums" all account totals and
//       subtotals; in the epilogue it calls yet another handler whose job is
//       reporting (the handler used in 'a' is only for calculation).
//
//       There is one variation on 'b' in which a "totals" line is also
//       displayed.
//
//    c. Write journal.  In this mode, a single function is called that output
//       the journal object as a textual file.  #2 is used to print out each
//       posting in the journal.
//
//    d. Dump binary file.  This is just like 'c', except that it dumps out a
//       binary file and #2 is completely ignored.
//
// 4. For 'a' and 'b' in #3, there is a different iteration function called,
//    depending on whether we're iterating:
//
//    a. The postings of an xact: walk_postings.
//    b. The xacts of a journal: walk_xacts.
//    c. The commodities of a journal: walk_commodities.
//
// 5. Finally, for the 'a' and 'b' reporting modes, there is a variant which
//    says that the formatter should be "flushed" after the entities are
//    iterated.  This does not happen for the commodities iteration, however.

class report_t : public scope_t
{
  report_t();

public:
  session_t&      session;
  output_stream_t output_stream;

#define BUDGET_NO_BUDGET   0x00
#define BUDGET_BUDGETED    0x01
#define BUDGET_UNBUDGETED  0x02
#define BUDGET_WRAP_VALUES 0x04

  datetime_t    terminus;
  uint_least8_t budget_flags;

  explicit report_t(session_t& _session)
    : session(_session), terminus(CURRENT_TIME()),
      budget_flags(BUDGET_NO_BUDGET) {
    TRACE_CTOR(report_t, "session_t&");
  }
  report_t(const report_t& report)
    : scope_t(report), session(report.session),
      output_stream(report.output_stream),
      terminus(report.terminus),
      budget_flags(report.budget_flags) {
    TRACE_CTOR(report_t, "copy");
  }

  virtual ~report_t() {
    TRACE_DTOR(report_t);
    output_stream.close();
  }

  void quick_close() {
    output_stream.close();
  }

  virtual string description() {
    return _("current report");
  }

  void normalize_options(const string& verb);
  void normalize_period();
  void parse_query_args(const value_t& args, const string& whence);

  void posts_report(post_handler_ptr handler);
  void generate_report(post_handler_ptr handler);
  void xact_report(post_handler_ptr handler, xact_t& xact);
  void accounts_report(acct_handler_ptr handler);
  void commodities_report(post_handler_ptr handler);

  value_t display_value(const value_t& val);

  value_t fn_amount_expr(call_scope_t& scope);
  value_t fn_total_expr(call_scope_t& scope);
  value_t fn_display_amount(call_scope_t& scope);
  value_t fn_display_total(call_scope_t& scope);
  value_t fn_top_amount(call_scope_t& val);
  value_t fn_should_bold(call_scope_t& scope);
  value_t fn_market(call_scope_t& scope);
  value_t fn_get_at(call_scope_t& scope);
  value_t fn_is_seq(call_scope_t& scope);
  value_t fn_strip(call_scope_t& scope);
  value_t fn_trim(call_scope_t& scope);
  value_t fn_format(call_scope_t& scope);
  value_t fn_print(call_scope_t& scope);
  value_t fn_scrub(call_scope_t& scope);
  value_t fn_quantity(call_scope_t& scope);
  value_t fn_rounded(call_scope_t& scope);
  value_t fn_unrounded(call_scope_t& scope);
  value_t fn_truncated(call_scope_t& scope);
  value_t fn_floor(call_scope_t& scope);
  value_t fn_ceiling(call_scope_t& scope);
  value_t fn_round(call_scope_t& scope);
  value_t fn_roundto(call_scope_t& scope);
  value_t fn_unround(call_scope_t& scope);
  value_t fn_abs(call_scope_t& scope);
  value_t fn_justify(call_scope_t& scope);
  value_t fn_quoted(call_scope_t& scope);
  value_t fn_join(call_scope_t& scope);
  value_t fn_format_date(call_scope_t& scope);
  value_t fn_format_datetime(call_scope_t& scope);
  value_t fn_ansify_if(call_scope_t& scope);
  value_t fn_percent(call_scope_t& scope);
  value_t fn_commodity(call_scope_t& scope);
  value_t fn_nail_down(call_scope_t& scope);
  value_t fn_lot_date(call_scope_t& scope);
  value_t fn_lot_price(call_scope_t& scope);
  value_t fn_lot_tag(call_scope_t& scope);
  value_t fn_to_boolean(call_scope_t& scope);
  value_t fn_to_int(call_scope_t& scope);
  value_t fn_to_datetime(call_scope_t& scope);
  value_t fn_to_date(call_scope_t& scope);
  value_t fn_to_amount(call_scope_t& scope);
  value_t fn_to_balance(call_scope_t& scope);
  value_t fn_to_string(call_scope_t& scope);
  value_t fn_to_mask(call_scope_t& scope);
  value_t fn_to_sequence(call_scope_t& scope);

  value_t fn_now(call_scope_t&) {
    return terminus;
  }
  value_t fn_today(call_scope_t&) {
    return terminus.date();
  }

  value_t fn_options(call_scope_t&) {
    return scope_value(this);
  }

  string report_format(option_t<report_t>& option) {
    if (HANDLED(format_))
      return HANDLER(format_).str();
    return option.str();
  }

  optional<string> maybe_format(option_t<report_t>& option) {
    if (option)
      return option.str();
    return none;
  }

  value_t reload_command(call_scope_t&);
  value_t echo_command(call_scope_t& scope);
  value_t pricemap_command(call_scope_t& scope);

  keep_details_t what_to_keep() {
    bool lots = HANDLED(lots) || HANDLED(lots_actual);
    return keep_details_t(lots || HANDLED(lot_prices),
                          lots || HANDLED(lot_dates),
                          lots || HANDLED(lot_notes),
                          HANDLED(lots_actual));
  }

  void report_options(std::ostream& out)
  {
    HANDLER(abbrev_len_).report(out);
    HANDLER(account_).report(out);
    HANDLER(actual).report(out);
    HANDLER(add_budget).report(out);
    HANDLER(amount_).report(out);
    HANDLER(amount_data).report(out);
    HANDLER(anon).report(out);
    HANDLER(auto_match).report(out);
    HANDLER(aux_date).report(out);
    HANDLER(average).report(out);
    HANDLER(balance_format_).report(out);
    HANDLER(base).report(out);
    HANDLER(basis).report(out);
    HANDLER(begin_).report(out);
    HANDLER(budget).report(out);
    HANDLER(budget_format_).report(out);
    HANDLER(by_payee).report(out);
    HANDLER(cleared).report(out);
    HANDLER(cleared_format_).report(out);
    HANDLER(color).report(out);
    HANDLER(collapse).report(out);
    HANDLER(collapse_if_zero).report(out);
    HANDLER(columns_).report(out);
    HANDLER(csv_format_).report(out);
    HANDLER(current).report(out);
    HANDLER(daily).report(out);
    HANDLER(date_).report(out);
    HANDLER(date_format_).report(out);
    HANDLER(datetime_format_).report(out);
    HANDLER(dc).report(out);
    HANDLER(depth_).report(out);
    HANDLER(deviation).report(out);
    HANDLER(display_).report(out);
    HANDLER(display_amount_).report(out);
    HANDLER(display_total_).report(out);
    HANDLER(dow).report(out);
    HANDLER(empty).report(out);
    HANDLER(end_).report(out);
    HANDLER(equity).report(out);
    HANDLER(exact).report(out);
    HANDLER(exchange_).report(out);
    HANDLER(flat).report(out);
    HANDLER(force_color).report(out);
    HANDLER(force_pager).report(out);
    HANDLER(forecast_while_).report(out);
    HANDLER(forecast_years_).report(out);
    HANDLER(format_).report(out);
    HANDLER(gain).report(out);
    HANDLER(generated).report(out);
    HANDLER(group_by_).report(out);
    HANDLER(group_title_format_).report(out);
    HANDLER(head_).report(out);
    HANDLER(immediate).report(out);
    HANDLER(inject_).report(out);
    HANDLER(invert).report(out);
    HANDLER(limit_).report(out);
    HANDLER(lot_dates).report(out);
    HANDLER(lot_prices).report(out);
    HANDLER(lot_notes).report(out);
    HANDLER(lots).report(out);
    HANDLER(lots_actual).report(out);
    HANDLER(market).report(out);
    HANDLER(meta_).report(out);
    HANDLER(monthly).report(out);
    HANDLER(no_pager).report(out);
    HANDLER(no_rounding).report(out);
    HANDLER(no_titles).report(out);
    HANDLER(no_total).report(out);
    HANDLER(now_).report(out);
    HANDLER(only_).report(out);
    HANDLER(output_).report(out);
    HANDLER(pager_).report(out);
    HANDLER(payee_).report(out);
    HANDLER(pending).report(out);
    HANDLER(percent).report(out);
    HANDLER(period_).report(out);
    HANDLER(pivot_).report(out);
    HANDLER(plot_amount_format_).report(out);
    HANDLER(plot_total_format_).report(out);
    HANDLER(prepend_format_).report(out);
    HANDLER(prepend_width_).report(out);
    HANDLER(price).report(out);
    HANDLER(prices_format_).report(out);
    HANDLER(pricedb_format_).report(out);
    HANDLER(primary_date).report(out);
    HANDLER(quantity).report(out);
    HANDLER(quarterly).report(out);
    HANDLER(raw).report(out);
    HANDLER(real).report(out);
    HANDLER(register_format_).report(out);
    HANDLER(related).report(out);
    HANDLER(related_all).report(out);
    HANDLER(revalued).report(out);
    HANDLER(revalued_only).report(out);
    HANDLER(revalued_total_).report(out);
    HANDLER(rich_data).report(out);
    HANDLER(seed_).report(out);
    HANDLER(sort_).report(out);
    HANDLER(sort_all_).report(out);
    HANDLER(sort_xacts_).report(out);
    HANDLER(start_of_week_).report(out);
    HANDLER(subtotal).report(out);
    HANDLER(tail_).report(out);
    HANDLER(time_report).report(out);
    HANDLER(total_).report(out);
    HANDLER(total_data).report(out);
    HANDLER(truncate_).report(out);
    HANDLER(unbudgeted).report(out);
    HANDLER(uncleared).report(out);
    HANDLER(unrealized).report(out);
    HANDLER(unrealized_gains_).report(out);
    HANDLER(unrealized_losses_).report(out);
    HANDLER(unround).report(out);
    HANDLER(weekly).report(out);
    HANDLER(wide).report(out);
    HANDLER(yearly).report(out);
    HANDLER(meta_width_).report(out);
    HANDLER(date_width_).report(out);
    HANDLER(payee_width_).report(out);
    HANDLER(account_width_).report(out);
    HANDLER(amount_width_).report(out);
    HANDLER(total_width_).report(out);
    HANDLER(values).report(out);
  }

  option_t<report_t> * lookup_option(const char * p);

  virtual void define(const symbol_t::kind_t kind, const string& name,
                      expr_t::ptr_op_t def);

  virtual expr_t::ptr_op_t lookup(const symbol_t::kind_t kind,
                                  const string& name);

  /**
   * Option handlers
   */

  OPTION__
  (report_t, abbrev_len_,
   CTOR(report_t, abbrev_len_) {
    on(none, "2");
  });

  OPTION(report_t, account_);

  OPTION_(report_t, actual, DO() { // -L
      OTHER(limit_).on(whence, "actual");
    });

  OPTION_(report_t, add_budget, DO() {
      parent->budget_flags |= BUDGET_BUDGETED | BUDGET_UNBUDGETED;
    });

  OPTION__
  (report_t, amount_, // -t
   DECL1(report_t, amount_, merged_expr_t, expr, ("amount_expr", "amount")) {}
   DO_(str) {
     expr.append(str);
   });

  OPTION(report_t, amount_data); // -j
  OPTION(report_t, anon);
  OPTION(report_t, auto_match);

  OPTION_(report_t, average, DO() { // -A
      OTHER(display_total_)
        .on(whence, "count>0?(display_total/count):0");
    });

  OPTION__
  (report_t, balance_format_,
   CTOR(report_t, balance_format_) {
    on(none,
       "%(ansify_if("
       "  justify(scrub(display_total), 20,"
       "          20 + int(prepend_width), true, color),"
       "            bold if should_bold))"
       "  %(!options.flat ? depth_spacer : \"\")"
       "%-(ansify_if("
       "   ansify_if(partial_account(options.flat), blue if color),"
       "             bold if should_bold))\n%/"
       "%$1\n%/"
       "%(prepend_width ? \" \" * int(prepend_width) : \"\")"
       "--------------------\n");
  });

  OPTION(report_t, base);

  OPTION_(report_t, basis, DO() { // -B
      OTHER(revalued).on(whence);
      OTHER(amount_).expr.set_base_expr("rounded(cost)");
    });

  OPTION_(report_t, begin_, DO_(str) { // -b
      date_interval_t interval(str);
      if (optional<date_t> begin = interval.begin()) {
        string predicate = "date>=[" + to_iso_extended_string(*begin) + "]";
        OTHER(limit_).on(whence, predicate);
      } else {
        throw_(std::invalid_argument,
               _f("Could not determine beginning of period '%1%'") % str);
      }
    });

  OPTION_
  (report_t, bold_if_,
   expr_t expr;
   DO_(str) {
     expr = str;
   });

  OPTION_(report_t, budget, DO() {
      parent->budget_flags |= BUDGET_BUDGETED;
    });

  OPTION__
  (report_t, budget_format_,
   CTOR(report_t, budget_format_) {
    on(none,
       "%(justify(scrub(get_at(display_total, 0)), 12, -1, true, color))"
       " %(justify(-scrub(get_at(display_total, 1)), 12, "
       "           12 + 1 + 12, true, color))"
       " %(justify(scrub(get_at(display_total, 1) + "
       "                 get_at(display_total, 0)), 12, "
       "           12 + 1 + 12 + 1 + 12, true, color))"
       " %(ansify_if("
       "   justify((get_at(display_total, 1) ? "
       "            (100% * scrub(get_at(display_total, 0))) / "
       "             -scrub(get_at(display_total, 1)) : 0), "
       "           5, -1, true, false),"
       "   magenta if (color and get_at(display_total, 1) and "
       "               (abs(quantity(scrub(get_at(display_total, 0))) / "
       "                    quantity(scrub(get_at(display_total, 1)))) >= 1))))"
       "  %(!options.flat ? depth_spacer : \"\")"
       "%-(ansify_if(partial_account(options.flat), blue if color))\n"
       "%/%$1 %$2 %$3 %$4\n%/"
       "%(prepend_width ? \" \" * int(prepend_width) : \"\")"
       "------------ ------------ ------------ -----\n");
  });

  OPTION(report_t, by_payee); // -P

  OPTION_(report_t, cleared, DO() { // -C
      OTHER(limit_).on(whence, "cleared");
    });

  OPTION__
  (report_t, cleared_format_,
   CTOR(report_t, cleared_format_) {
    on(none,
       "%(justify(scrub(get_at(display_total, 0)), 16, 16 + int(prepend_width), "
       " true, color))  %(justify(scrub(get_at(display_total, 1)), 18, "
       " 36 + int(prepend_width), true, color))"
       "    %(latest_cleared ? format_date(latest_cleared) : \"         \")"
       "    %(!options.flat ? depth_spacer : \"\")"
       "%-(ansify_if(partial_account(options.flat), blue if color))\n%/"
       "%$1  %$2    %$3\n%/"
       "%(prepend_width ? \" \" * int(prepend_width) : \"\")"
       "----------------  ----------------    ---------\n");
  });

  OPTION(report_t, color);

  OPTION_(report_t, collapse, DO() { // -n
      // Make sure that balance reports are collapsed too, but only apply it
      // to account xacts
      OTHER(display_).on(whence, "post|depth<=1");
    });

  OPTION_(report_t, collapse_if_zero, DO() {
      OTHER(collapse).on(whence);
    });

  OPTION(report_t, columns_);
  OPTION(report_t, count);

  OPTION__
  (report_t, csv_format_,
   CTOR(report_t, csv_format_) {
    on(none,
       "%(quoted(date)),"
       "%(quoted(code)),"
       "%(quoted(payee)),"
       "%(quoted(display_account)),"
       "%(quoted(commodity(scrub(display_amount)))),"
       "%(quoted(quantity(scrub(display_amount)))),"
       "%(quoted(cleared ? \"*\" : (pending ? \"!\" : \"\"))),"
       "%(quoted(join(note | xact.note)))\n");
  });

  OPTION_(report_t, current, DO() { // -c
      OTHER(limit_).on(whence, "date<=today");
    });

  OPTION_(report_t, daily, DO() { // -D
      OTHER(period_).on(whence, "daily");
    });

  OPTION(report_t, date_);
  OPTION(report_t, date_format_);
  OPTION(report_t, datetime_format_);

  OPTION_(report_t, dc, DO() {
      OTHER(amount_).expr.set_base_expr
        ("(amount > 0 ? amount : 0, amount < 0 ? amount : 0)");

      OTHER(register_format_)
        .on(none,
            "%(ansify_if("
            "  ansify_if(justify(format_date(date), int(date_width)),"
            "            green if color and date > today),"
            "            bold if should_bold))"
            " %(ansify_if("
            "   ansify_if(justify(truncated(payee, int(payee_width)), int(payee_width)), "
            "             bold if color and !cleared and actual),"
            "             bold if should_bold))"
            " %(ansify_if("
            "   ansify_if(justify(truncated(display_account, int(account_width), "
            "                               int(abbrev_len)), int(account_width)),"
            "             blue if color),"
            "             bold if should_bold))"
            " %(ansify_if("
            "   justify(scrub(abs(get_at(display_amount, 0))), int(amount_width), "
            "           3 + int(meta_width) + int(date_width) + int(payee_width)"
            "             + int(account_width) + int(amount_width) + int(prepend_width),"
            "           true, color),"
            "           bold if should_bold))"
            " %(ansify_if("
            "   justify(scrub(abs(get_at(display_amount, 1))), int(amount_width), "
            "           4 + int(meta_width) + int(date_width) + int(payee_width)"
            "             + int(account_width) + int(amount_width) + int(amount_width) + int(prepend_width),"
            "           true, color),"
            "           bold if should_bold))"
            " %(ansify_if("
            "   justify(scrub(get_at(display_total, 0) + get_at(display_total, 1)), int(total_width), "
            "           5 + int(meta_width) + int(date_width) + int(payee_width)"
            "             + int(account_width) + int(amount_width) + int(amount_width) + int(total_width)"
            "             + int(prepend_width), true, color),"
            "           bold if should_bold))\n%/"
            "%(justify(\" \", int(date_width)))"
            " %(ansify_if("
            "   justify(truncated(has_tag(\"Payee\") ? payee : \" \", "
            "                     int(payee_width)), int(payee_width)),"
            "             bold if should_bold))"
            " %$3 %$4 %$5 %$6\n");

      OTHER(balance_format_)
        .on(none,
            "%(ansify_if("
            "  justify(scrub(abs(get_at(display_total, 0))), 14,"
            "          14 + int(prepend_width), true, color),"
            "            bold if should_bold)) "
            "%(ansify_if("
            "  justify(scrub(abs(get_at(display_total, 1))), 14,"
            "          14 + 1 + int(prepend_width) + int(total_width), true, color),"
            "            bold if should_bold)) "
            "%(ansify_if("
            "  justify(scrub(get_at(display_total, 0) + get_at(display_total, 1)), 14,"
            "          14 + 2 + int(prepend_width) + int(total_width) + int(total_width), true, color),"
            "            bold if should_bold))"
            "  %(!options.flat ? depth_spacer : \"\")"
            "%-(ansify_if("
            "   ansify_if(partial_account(options.flat), blue if color),"
            "             bold if should_bold))\n%/"
            "%$1 %$2 %$3\n%/"
            "%(prepend_width ? \" \" * int(prepend_width) : \"\")"
            "--------------------------------------------\n");
  });

  OPTION_(report_t, depth_, DO_(str) {
      OTHER(display_).on(whence, string("depth<=") + str);
    });

  OPTION_(report_t, deviation, DO() {
      OTHER(display_total_)
        .on(whence, "display_amount-display_total");
    });

  OPTION_
  (report_t, display_,
   DO_(str) { // -d
     if (handled)
       value = string("(") + value + ")&(" + str + ")";
   });

  OPTION__
  (report_t, display_amount_,
   DECL1(report_t, display_amount_, merged_expr_t, expr,
         ("display_amount", "amount_expr")) {}
   DO_(str) {
     expr.append(str);
   });

  OPTION__
  (report_t, display_total_,
   DECL1(report_t, display_total_, merged_expr_t, expr,
         ("display_total", "total_expr")) {}
   DO_(str) {
     expr.append(str);
   });

  OPTION(report_t, dow);
  OPTION(report_t, aux_date);
  OPTION(report_t, empty); // -E

  OPTION_(report_t, end_, DO_(str) { // -e
      // Use begin() here so that if the user says --end=2008, we end on
      // 2008/01/01 instead of 2009/01/01 (which is what end() would
      // return).
      date_interval_t interval(str);
      if (optional<date_t> end = interval.begin()) {
        string predicate = "date<[" + to_iso_extended_string(*end) + "]";
        OTHER(limit_).on(whence, predicate);

        parent->terminus = datetime_t(*end);
      } else {
        throw_(std::invalid_argument,
               _f("Could not determine end of period '%1%'")
               % str);
      }
    });

  OPTION(report_t, equity);
  OPTION(report_t, exact);

  OPTION_(report_t, exchange_, DO_(str) { // -X
      // Using -X implies -V.  The main difference is that now
      // HANDLER(exchange_) contains the name of a commodity, which
      // is accessed via the "exchange" value expression function.
      OTHER(market).on(whence);
    });

  OPTION(report_t, flat);
  OPTION(report_t, force_color);
  OPTION(report_t, force_pager);
  OPTION(report_t, forecast_while_);
  OPTION(report_t, forecast_years_);
  OPTION(report_t, format_); // -F

  OPTION_(report_t, gain, DO() { // -G
      OTHER(revalued).on(whence);
      OTHER(amount_).expr.set_base_expr("(amount, cost)");

      // Since we are displaying the amounts of revalued postings, they
      // will end up being composite totals, and hence a pair of pairs.
      OTHER(display_amount_)
        .on(whence,
            "use_direct_amount ? amount :"
            " (is_seq(get_at(amount_expr, 0)) ?"
            "  get_at(get_at(amount_expr, 0), 0) :"
            "  market(get_at(amount_expr, 0), value_date, exchange)"
            "  - get_at(amount_expr, 1))");
      OTHER(revalued_total_)
        .on(whence,
            "(market(get_at(total_expr, 0), value_date, exchange), "
            "get_at(total_expr, 1))");
      OTHER(display_total_)
        .on(whence,
            "use_direct_amount ? total_expr :"
            " market(get_at(total_expr, 0), value_date, exchange)"
            " - get_at(total_expr, 1)");
    });

  OPTION(report_t, generated);

  OPTION_
  (report_t, group_by_,
   expr_t expr;
   DO_(str) {
     expr = str;
   });

  OPTION__
  (report_t, group_title_format_,
   CTOR(report_t, group_title_format_) {
    on(none, "%(value)\n");
  });

  OPTION(report_t, head_);

  OPTION_(report_t, historical, DO() { // -H
      OTHER(market).on(whence);
      OTHER(amount_)
        .on(whence, "nail_down(amount_expr, "
            "market(amount_expr, value_date, exchange))");
    });

  OPTION(report_t, immediate);
  OPTION(report_t, inject_);

  OPTION_(report_t, invert, DO() {
      OTHER(amount_).on(whence, "-amount_expr");
    });

  OPTION_
  (report_t, limit_,
   DO_(str) { // -l
     if (handled)
       value = string("(") + value + ")&(" + str + ")";
   });

  OPTION(report_t, lot_dates);
  OPTION(report_t, lot_prices);
  OPTION(report_t, lot_notes);
  OPTION(report_t, lots);
  OPTION(report_t, lots_actual);

  OPTION_(report_t, market, DO() { // -V
      OTHER(revalued).on(whence);

      OTHER(display_amount_)
        .on(whence, "market(display_amount, value_date, exchange)");
      OTHER(display_total_)
        .on(whence, "market(display_total, value_date, exchange)");
    });

  OPTION(report_t, meta_);

  OPTION_(report_t, monthly, DO() { // -M
      OTHER(period_).on(whence, "monthly");
    });

  OPTION_(report_t, no_color, DO() {
      OTHER(color).off();
    });

  OPTION(report_t, no_rounding);
  OPTION(report_t, no_titles);
  OPTION(report_t, no_total);

  OPTION_(report_t, now_, DO_(str) {
      date_interval_t interval(str);
      if (optional<date_t> begin = interval.begin()) {
        ledger::epoch = parent->terminus = datetime_t(*begin);
      } else {
        throw_(std::invalid_argument,
               _f("Could not determine beginning of period '%1%'")
               % str);
      }
    });

  OPTION_
  (report_t, only_,
   DO_(str) {
     if (handled)
       value = string("(") + value + ")&(" + str + ")";
   });

  OPTION(report_t, output_); // -o

#if HAVE_ISATTY
  OPTION__
  (report_t, pager_,
   CTOR(report_t, pager_) {
     if (! std::getenv("PAGER") && isatty(STDOUT_FILENO)) {
       bool have_less = false;
       if (exists(path("/opt/local/bin/less")) ||
           exists(path("/usr/local/bin/less")) ||
           exists(path("/usr/bin/less")))
         have_less = true;

       if (have_less) {
         on(none, "less");
         setenv("LESS", "-FRSX", 0); // don't overwrite
       }
     }
   });
#else // HAVE_ISATTY
  OPTION(report_t, pager_);
#endif // HAVE_ISATTY

  OPTION_(report_t, no_pager, DO() {
      OTHER(pager_).off();
    });

  OPTION(report_t, payee_);

  OPTION_(report_t, pending, DO() { // -C
      OTHER(limit_).on(whence, "pending");
    });

  OPTION_(report_t, percent, DO() { // -%
      OTHER(total_)
        .on(whence,
            "((is_account&parent&parent.total)?"
            "  percent(scrub(total), scrub(parent.total)):0)");
    });

  OPTION_
  (report_t, period_,
   DO_(str) { // -p
     if (handled)
       value += string(" ") + str;
   });

  OPTION(report_t, pivot_);

  OPTION__
  (report_t, plot_amount_format_,
   CTOR(report_t, plot_amount_format_) {
    on(none,
       "%(format_date(date, \"%Y-%m-%d\")) %(quantity(scrub(display_amount)))\n");
  });

  OPTION__
  (report_t, plot_total_format_,
   CTOR(report_t, plot_total_format_) {
    on(none,
       "%(format_date(date, \"%Y-%m-%d\")) %(quantity(scrub(display_total)))\n");
  });

  OPTION(report_t, prepend_format_);
  OPTION(report_t, prepend_width_);

  OPTION_(report_t, price, DO() { // -I
      OTHER(amount_).expr.set_base_expr("price");
    });

  OPTION__
  (report_t, prices_format_,
   CTOR(report_t, prices_format_) {
    on(none,
       "%(date) %-8(display_account) %(justify(scrub(display_amount), 12, "
       "    2 + 9 + 8 + 12, true, color))\n");
  });

  OPTION__
  (report_t, pricedb_format_,
   CTOR(report_t, pricedb_format_) {
    on(none,
       "P %(datetime) %(display_account) %(scrub(display_amount))\n");
  });

  OPTION(report_t, primary_date);

  OPTION_(report_t, quantity, DO() { // -O
      OTHER(revalued).off();

      OTHER(amount_).expr.set_base_expr("amount");
      OTHER(total_).expr.set_base_expr("total");
    });

  OPTION_(report_t, quarterly, DO() {
      OTHER(period_).on(whence, "quarterly");
    });

  OPTION(report_t, raw);

  OPTION_(report_t, real, DO() { // -R
      OTHER(limit_).on(whence, "real");
    });

  OPTION__
  (report_t, register_format_,
   CTOR(report_t, register_format_) {
    on(none,
       "%(ansify_if("
       "  ansify_if(justify(format_date(date), int(date_width)),"
       "            green if color and date > today),"
       "            bold if should_bold))"
       " %(ansify_if("
       "   ansify_if(justify(truncated(payee, int(payee_width)), int(payee_width)), "
       "             bold if color and !cleared and actual),"
       "             bold if should_bold))"
       " %(ansify_if("
       "   ansify_if(justify(truncated(display_account, int(account_width), "
       "                               int(abbrev_len)), int(account_width)),"
       "             blue if color),"
       "             bold if should_bold))"
       " %(ansify_if("
       "   justify(scrub(display_amount), int(amount_width), "
       "           3 + int(meta_width) + int(date_width) + int(payee_width)"
       "             + int(account_width) + int(amount_width) + int(prepend_width),"
       "           true, color),"
       "           bold if should_bold))"
       " %(ansify_if("
       "   justify(scrub(display_total), int(total_width), "
       "           4 + int(meta_width) + int(date_width) + int(payee_width)"
       "             + int(account_width) + int(amount_width) + int(total_width)"
       "             + int(prepend_width), true, color),"
       "           bold if should_bold))\n%/"
       "%(justify(\" \", int(date_width)))"
       " %(ansify_if("
       "   justify(truncated(has_tag(\"Payee\") ? payee : \" \", "
       "                     int(payee_width)), int(payee_width)),"
       "             bold if should_bold))"
       " %$3 %$4 %$5\n");
  });

  OPTION(report_t, related); // -r

  OPTION_(report_t, related_all, DO() {
      OTHER(related).on(whence);
    });

  OPTION(report_t, revalued);
  OPTION(report_t, revalued_only);

  OPTION_
  (report_t, revalued_total_,
   expr_t expr;
   DO_(str) {
     expr = str;
   });

  OPTION(report_t, rich_data);

  OPTION(report_t, seed_);

  OPTION_(report_t, sort_, DO_(str) { // -S
      OTHER(sort_xacts_).off();
      OTHER(sort_all_).off();
    });

  OPTION_(report_t, sort_all_, DO_(str) {
      OTHER(sort_).on(whence, str);
      OTHER(sort_xacts_).off();
    });

  OPTION_(report_t, sort_xacts_, DO_(str) {
      OTHER(sort_).on(whence, str);
      OTHER(sort_all_).off();
    });

  OPTION(report_t, start_of_week_);
  OPTION(report_t, subtotal); // -s
  OPTION(report_t, tail_);

  OPTION_(report_t, time_report, DO() {
      OTHER(balance_format_)
        .on(none,
            "%(ansify_if(justify(earliest_checkin ? "
            "     format_datetime(earliest_checkin) : \"\", 19, -1, true),"
            "     bold if latest_checkout_cleared))  "
            "%(ansify_if(justify(latest_checkout ? "
            "     format_datetime(latest_checkout) : \"\", 19, -1, true), "
            "     bold if latest_checkout_cleared)) "
            "%(latest_checkout_cleared ? \"*\" : \" \")  "
            "%(ansify_if("
            "  justify(scrub(display_total), 8,"
            "          8 + 4 + 19 * 2, true, color), bold if should_bold))"
            "  %(!options.flat ? depth_spacer : \"\")"
            "%-(ansify_if("
            "   ansify_if(partial_account(options.flat), blue if color),"
            "             bold if should_bold))\n%/"
            "%$1  %$2  %$3\n%/"
            "%(prepend_width ? \" \" * int(prepend_width) : \"\")"
            "--------------------------------------------------\n");
    });

  OPTION__
  (report_t, total_, // -T
   DECL1(report_t, total_, merged_expr_t, expr, ("total_expr", "total")) {}
   DO_(str) {
     expr.append(str);
   });

  OPTION(report_t, total_data); // -J

  OPTION_(report_t, truncate_, DO_(style) {
      if (style == "leading")
        format_t::default_style = format_t::TRUNCATE_LEADING;
      else if (style == "middle")
        format_t::default_style = format_t::TRUNCATE_MIDDLE;
      else if (style == "trailing")
        format_t::default_style = format_t::TRUNCATE_TRAILING;
      else
        throw_(std::invalid_argument,
               _f("Unrecognized truncation style: '%1%'") % style);
      format_t::default_style_changed = true;
    });

  OPTION_(report_t, unbudgeted, DO() {
      parent->budget_flags |= BUDGET_UNBUDGETED;
    });

  OPTION_(report_t, uncleared, DO() { // -U
      OTHER(limit_).on(whence, "uncleared|pending");
    });

  OPTION(report_t, unrealized);

  OPTION(report_t, unrealized_gains_);
  OPTION(report_t, unrealized_losses_);

  OPTION_(report_t, unround, DO() {
      OTHER(amount_).on(whence, "unrounded(amount_expr)");
      OTHER(total_).on(whence, "unrounded(total_expr)");
    });

  OPTION_(report_t, weekly, DO() { // -W
      OTHER(period_).on(whence, "weekly");
    });

  OPTION_(report_t, wide, DO() { // -w
      OTHER(columns_).on(whence, "132");
    });

  OPTION_(report_t, yearly, DO() { // -Y
      OTHER(period_).on(whence, "yearly");
    });

  OPTION(report_t, meta_width_);
  OPTION(report_t, date_width_);
  OPTION(report_t, payee_width_);
  OPTION(report_t, account_width_);
  OPTION(report_t, amount_width_);
  OPTION(report_t, total_width_);
  OPTION(report_t, values);
};

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
  reporter(shared_ptr<item_handler<Type> > _handler,
           report_t& _report, const string& _whence)
    : handler(_handler), report(_report), whence(_whence) {
    TRACE_CTOR(reporter, "item_handler<Type>, report_t&, string");
  }
  reporter(const reporter& other)
    : handler(other.handler), report(other.report), whence(other.whence) {
    TRACE_CTOR(reporter, "copy");
  }
  ~reporter() throw() {
    TRACE_DTOR(reporter);
  }

  value_t operator()(call_scope_t& args)
  {
    if (args.size() > 0)
      report.parse_query_args(args.value(), whence);

    (report.*report_method)(handler_ptr(handler));

    return true;
  }
};

} // namespace ledger

#endif // _REPORT_H
