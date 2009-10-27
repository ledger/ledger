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

/**
 * @addtogroup report
 */

/**
 * @file   report.h
 * @author John Wiegley
 *
 * @ingroup report
 *
 * @brief Brief
 *
 * Long.
 */
#ifndef _REPORT_H
#define _REPORT_H

#include "interactive.h"
#include "expr.h"
#include "chain.h"
#include "stream.h"
#include "option.h"
#include "commodity.h"
#include "annotate.h"
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

/**
 * @brief Brief
 *
 * Long.
 */
class report_t : public scope_t
{
  report_t();

public:
  session_t&	  session;
  output_stream_t output_stream;

#define BUDGET_NO_BUDGET   0x00
#define BUDGET_BUDGETED	   0x01
#define BUDGET_UNBUDGETED  0x02
#define BUDGET_WRAP_VALUES 0x04

  datetime_t    terminus;
  uint_least8_t budget_flags;

  explicit report_t(session_t& _session)
    : session(_session), terminus(CURRENT_TIME()),
      budget_flags(BUDGET_NO_BUDGET) {}

  virtual ~report_t() {
    output_stream.close();
  }

  void posts_report(post_handler_ptr handler);
  void generate_report(post_handler_ptr handler);
  void xact_report(post_handler_ptr handler, xact_t& xact);
  void accounts_report(acct_handler_ptr handler);
  void commodities_report(post_handler_ptr handler);

  value_t fn_amount_expr(call_scope_t& scope);
  value_t fn_total_expr(call_scope_t& scope);
  value_t fn_display_amount(call_scope_t& scope);
  value_t fn_display_total(call_scope_t& scope);
  value_t fn_market(call_scope_t& scope);
  value_t fn_get_at(call_scope_t& scope);
  value_t fn_is_seq(call_scope_t& scope);
  value_t fn_strip(call_scope_t& scope);
  value_t fn_scrub(call_scope_t& scope);
  value_t fn_quantity(call_scope_t& scope);
  value_t fn_rounded(call_scope_t& scope);
  value_t fn_unrounded(call_scope_t& scope);
  value_t fn_truncated(call_scope_t& scope);
  value_t fn_abs(call_scope_t& scope);
  value_t fn_justify(call_scope_t& scope);
  value_t fn_quoted(call_scope_t& scope);
  value_t fn_join(call_scope_t& scope);
  value_t fn_format_date(call_scope_t& scope);
  value_t fn_ansify_if(call_scope_t& scope);
  value_t fn_percent(call_scope_t& scope);
  value_t fn_price(call_scope_t& scope);

  value_t fn_now(call_scope_t&) {
    return terminus;
  }
  value_t fn_today(call_scope_t&) {
    return terminus.date();
  }

  value_t fn_options(call_scope_t&) {
    return value_t(static_cast<scope_t *>(this));
  }

  string report_format(option_t<report_t>& option) {
    if (HANDLED(format_))
      return HANDLER(format_).str();
    return option.str();
  }

  value_t reload_command(call_scope_t&);
  value_t echo_command(call_scope_t& scope);

  keep_details_t what_to_keep() {
    bool lots = HANDLED(lots) || HANDLED(lots_actual);
    return keep_details_t(lots || HANDLED(lot_prices),
			  lots || HANDLED(lot_dates),
			  lots || HANDLED(lot_tags),
			  HANDLED(lots_actual));
  }

  bool maybe_import(const string& module);

  void report_options(std::ostream& out)
  {
    HANDLER(abbrev_len_).report(out);
    HANDLER(account_).report(out);
    HANDLER(actual).report(out);
    HANDLER(add_budget).report(out);
    HANDLER(amount_).report(out);
    HANDLER(amount_data).report(out);
    HANDLER(anon).report(out);
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
    HANDLER(code_as_payee).report(out);
    HANDLER(comm_as_payee).report(out);
    HANDLER(code_as_account).report(out);
    HANDLER(comm_as_account).report(out);
    HANDLER(color).report(out);
    HANDLER(collapse).report(out);
    HANDLER(collapse_if_zero).report(out);
    HANDLER(columns_).report(out);
    HANDLER(csv_format_).report(out);
    HANDLER(current).report(out);
    HANDLER(daily).report(out);
    HANDLER(date_format_).report(out);
    HANDLER(datetime_format_).report(out);
    HANDLER(depth_).report(out);
    HANDLER(deviation).report(out);
    HANDLER(display_).report(out);
    HANDLER(display_amount_).report(out);
    HANDLER(display_total_).report(out);
    HANDLER(dow).report(out);
    HANDLER(effective).report(out);
    HANDLER(empty).report(out);
    HANDLER(end_).report(out);
    HANDLER(equity).report(out);
    HANDLER(exact).report(out);
    HANDLER(exchange_).report(out);
    HANDLER(flat).report(out);
    HANDLER(forecast_while_).report(out);
    HANDLER(format_).report(out);
    HANDLER(gain).report(out);
    HANDLER(head_).report(out);
    HANDLER(invert).report(out);
    HANDLER(limit_).report(out);
    HANDLER(lot_dates).report(out);
    HANDLER(lot_prices).report(out);
    HANDLER(lot_tags).report(out);
    HANDLER(lots).report(out);
    HANDLER(lots_actual).report(out);
    HANDLER(market).report(out);
    HANDLER(monthly).report(out);
    HANDLER(no_total).report(out);
    HANDLER(only_).report(out);
    HANDLER(output_).report(out);
    HANDLER(pager_).report(out);
    HANDLER(payee_as_account).report(out);
    HANDLER(pending).report(out);
    HANDLER(percent).report(out);
    HANDLER(period_).report(out);
    HANDLER(plot_amount_format_).report(out);
    HANDLER(plot_total_format_).report(out);
    HANDLER(price).report(out);
    HANDLER(prices_format_).report(out);
    HANDLER(pricesdb_format_).report(out);
    HANDLER(print_format_).report(out);
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
    HANDLER(seed_).report(out);
    HANDLER(set_account_).report(out);
    HANDLER(set_payee_).report(out);
    HANDLER(sort_).report(out);
    HANDLER(sort_all_).report(out);
    HANDLER(sort_xacts_).report(out);
    HANDLER(start_of_week_).report(out);
    HANDLER(subtotal).report(out);
    HANDLER(tail_).report(out);
    HANDLER(total_).report(out);
    HANDLER(total_data).report(out);
    HANDLER(truncate_).report(out);
    HANDLER(unbudgeted).report(out);
    HANDLER(uncleared).report(out);
    HANDLER(unround).report(out);
    HANDLER(weekly).report(out);
    HANDLER(wide).report(out);
    HANDLER(yearly).report(out);
    HANDLER(date_width_).report(out);
    HANDLER(payee_width_).report(out);
    HANDLER(account_width_).report(out);
    HANDLER(amount_width_).report(out);
    HANDLER(total_width_).report(out);
  }

  option_t<report_t> * lookup_option(const char * p);

  virtual void define(const string& name, expr_t::ptr_op_t def);

  virtual expr_t::ptr_op_t lookup(const string& name);

  /**
   * Option handlers
   */

  OPTION__(report_t, abbrev_len_,
	   CTOR(report_t, abbrev_len_) { on_with(none, 2L); });
  OPTION(report_t, account_);

  OPTION_(report_t, actual, DO() { // -L
      parent->HANDLER(limit_).on(string("--actual"), "actual");
    });

  OPTION_(report_t, add_budget, DO() {
      parent->budget_flags |= BUDGET_BUDGETED | BUDGET_UNBUDGETED;
    });

  OPTION__
  (report_t, amount_, // -t
   expr_t expr;
   CTOR(report_t, amount_) {
     set_expr(none, "amount");
   }
   void set_expr(const optional<string>& whence, const string& str) {
     expr = str;
     on(whence, str);
   }
   DO_(args) {
     set_expr(args[0].to_string(), args[1].to_string());
   });

  OPTION(report_t, amount_data);
  OPTION(report_t, anon);

  OPTION_(report_t, average, DO() { // -A
      parent->HANDLER(display_total_)
	.set_expr(string("--average"), "total_expr/count");
    });

  OPTION__(report_t, balance_format_, CTOR(report_t, balance_format_) {
      on(none,
	 "%(justify(scrub(display_total), 20, -1, true, color))"
	 "  %(!options.flat ? depth_spacer : \"\")"
	 "%-(ansify_if(partial_account(options.flat), blue if color))\n%/"
	 "%(justify(scrub(display_total), 20, -1, true, color))\n%/"
	 "--------------------\n");
    });

  OPTION(report_t, base);

  OPTION_(report_t, basis, DO() { // -B
      parent->HANDLER(revalued).on_only(string("--basis"));
      parent->HANDLER(amount_).set_expr(string("--basis"), "rounded(cost)");
    });

  OPTION_(report_t, begin_, DO_(args) { // -b
      date_interval_t interval(args[1].to_string());
      if (! interval.start)
	throw_(std::invalid_argument,
	       _("Could not determine beginning of period '%1'")
	       << args[1].to_string());

      string predicate =
	"date>=[" + to_iso_extended_string(*interval.start) + "]";
      parent->HANDLER(limit_).on(string("--begin"), predicate);
    });

  OPTION_(report_t, budget, DO() {
      parent->budget_flags |= BUDGET_BUDGETED;
    });

  OPTION__(report_t, budget_format_, CTOR(report_t, budget_format_) {
      on(none,
	 "%(justify(scrub(get_at(total_expr, 0)), 12, -1, true, color))"
	 " %(justify(scrub(- get_at(total_expr, 1)), 12, -1, true, color))"
	 " %(justify(scrub(get_at(total_expr, 1) + "
	 "                 get_at(total_expr, 0)), 12, -1, true, color))"
	 " %(ansify_if("
	 "   justify((get_at(total_expr, 1) ? "
	 "            scrub((100% * get_at(total_expr, 0)) / "
	 "                  - get_at(total_expr, 1)) : 0), "
	 "           5, -1, true, false),"
	 "   magenta if (color and get_at(total_expr, 1) and "
	 "               (abs(quantity(get_at(total_expr, 0)) / "
	 "                    quantity(get_at(total_expr, 1))) >= 1))))"
	 "  %(!options.flat ? depth_spacer : \"\")"
	 "%-(ansify_if(partial_account(options.flat), blue if color))\n"
	 "%/"
	 "%(justify(scrub(get_at(total_expr, 0)), 12, -1, true, color))"
	 " %(justify(scrub(- get_at(total_expr, 1)), 12, -1, true, color))"
	 " %(justify(scrub(get_at(total_expr, 1) + "
	 "                 get_at(total_expr, 0)), 12, -1, true, color))"
	 " %(ansify_if("
	 "   justify((get_at(total_expr, 1) ? "
	 "            scrub((100% * get_at(total_expr, 0)) / "
	 "                  - get_at(total_expr, 1)) : 0), "
	 "           5, -1, true, false),"
	 "   magenta if (color and get_at(total_expr, 1) and "
	 "               (abs(quantity(get_at(total_expr, 0)) / "
	 "                    quantity(get_at(total_expr, 1))) >= 1))))\n%/"
	 "------------ ------------ ------------ -----\n");
    });

  OPTION(report_t, by_payee); // -P

  OPTION_(report_t, cleared, DO() { // -C
      parent->HANDLER(limit_).on(string("--cleared"), "cleared");
    });

  OPTION__(report_t, cleared_format_, CTOR(report_t, cleared_format_) {
      on(none,
	 "%(justify(scrub(get_at(total_expr, 0)), 16, -1, true, color))"
	 "  %(justify(scrub(get_at(total_expr, 1)), 16, -1, true, color))"
	 "    %(latest_cleared ? format_date(latest_cleared) : \"         \")"
	 "    %(!options.flat ? depth_spacer : \"\")"
	 "%-(ansify_if(partial_account(options.flat), blue if color))\n%/"
	 "%(justify(scrub(get_at(total_expr, 0)), 16, -1, true, color))"
	 "    %(justify(scrub(get_at(total_expr, 1)), 16, -1, true, color))"
	 "    %(latest_cleared ? format_date(latest_cleared) : \"         \")\n%/"
	 "----------------  ----------------    ---------\n");
    });

  OPTION(report_t, code_as_payee);
  OPTION(report_t, comm_as_payee);
  OPTION(report_t, code_as_account);
  OPTION(report_t, comm_as_account);
  OPTION(report_t, color);

  OPTION_(report_t, collapse, DO() { // -n
      // Make sure that balance reports are collapsed too, but only apply it
      // to account xacts
      parent->HANDLER(display_).on(string("--collapse"), "post|depth<=1");
    });

  OPTION_(report_t, collapse_if_zero, DO() {
      parent->HANDLER(collapse).on_only(string("--collapse-if-zero"));
    });

  OPTION(report_t, columns_);

  OPTION__(report_t, csv_format_, CTOR(report_t, csv_format_) {
      on(none,
	 "%(quoted(date)),"
	 "%(quoted(payee)),"
	 "%(quoted(account)),"
	 "%(quoted(scrub(display_amount))),"
	 "%(quoted(cleared ? \"*\" : (pending ? \"!\" : \"\"))),"
	 "%(quoted(code)),"
	 "%(quoted(join(note | xact.note)))\n");
    });

  OPTION_(report_t, current, DO() { // -c
      parent->HANDLER(limit_).on(string("--current"), "date<=today");
    });

  OPTION_(report_t, daily, DO() {
      parent->HANDLER(period_).on(string("--daily"), "daily");
    });

  OPTION(report_t, date_format_);
  OPTION(report_t, datetime_format_);

  OPTION_(report_t, depth_, DO_(scope) {
      interactive_t args(scope, "sl");
      parent->HANDLER(display_).on(string("--depth"),
				   string("depth<=") + args.get<string>(1));
    });

  OPTION_(report_t, deviation, DO() { // -D
      parent->HANDLER(display_total_)
	.set_expr(string("--deviation"), "amount_expr-total_expr/count");
    });

  OPTION__
  (report_t, display_, // -d
   CTOR(report_t, display_) {}
   virtual void on_with(const optional<string>& whence, const value_t& text) {
     if (! handled)
       option_t<report_t>::on_with(whence, text);
     else
       option_t<report_t>::on_with(whence,
				   string_value(string("(") + str() + ")&(" +
						text.as_string() + ")"));
   });

  OPTION__
  (report_t, display_amount_,
   expr_t expr;
   CTOR(report_t, display_amount_) {
     set_expr(none, "amount_expr");
   }
   void set_expr(const optional<string>& whence, const string& str) {
     expr = str;
     on(whence, str);
   }
   DO_(args) {
     set_expr(args[0].to_string(), args[1].to_string());
   });

  OPTION__
  (report_t, display_total_,
   expr_t expr;
   CTOR(report_t, display_total_) {
     set_expr(none, "total_expr");
   }
   void set_expr(const optional<string>& whence, const string& str) {
     expr = str;
     on(whence, str);
   }
   DO_(args) {
     set_expr(args[0].to_string(), args[1].to_string());
   });

  OPTION(report_t, dow);
  OPTION(report_t, effective);
  OPTION(report_t, empty); // -E

  OPTION_(report_t, end_, DO_(args) { // -e
      date_interval_t interval(args[1].to_string());
      if (! interval.start)
	throw_(std::invalid_argument,
	       _("Could not determine end of period '%1'")
	       << args[1].to_string());

      string predicate =
	"date<[" + to_iso_extended_string(*interval.start) + "]";
      parent->HANDLER(limit_).on(string("--end"), predicate);

      parent->terminus = datetime_t(*interval.start);
    });

  OPTION(report_t, equity);
  OPTION(report_t, exact);

  OPTION_(report_t, exchange_, DO_(args) { // -X
      on_with(args[0].as_string(), args[1]);
      call_scope_t no_args(*parent);
      no_args.push_back(args[0]);
      parent->HANDLER(market).parent = parent;
      parent->HANDLER(market).handler(no_args);
    });

  OPTION(report_t, flat);
  OPTION(report_t, forecast_while_);
  OPTION(report_t, format_); // -F

  OPTION_(report_t, gain, DO() { // -G
      parent->HANDLER(revalued).on_only(string("--gain"));
      parent->HANDLER(amount_).set_expr(string("--gain"), "(amount, cost)");
      // Since we are displaying the amounts of revalued postings, they
      // will end up being composite totals, and hence a pair of pairs.
      parent->HANDLER(display_amount_)
	.set_expr(string("--gain"),
		  "use_direct_amount ? amount :"
		  " (is_seq(get_at(amount_expr, 0)) ?"
		  "  get_at(get_at(amount_expr, 0), 0) :"
		  "  market(get_at(amount_expr, 0), date, exchange)"
		  "  - get_at(amount_expr, 1))");
      parent->HANDLER(revalued_total_)
	.set_expr(string("--gain"),
		  "(market(get_at(total_expr, 0), date, exchange), "
		  "get_at(total_expr, 1))");
      parent->HANDLER(display_total_)
	.set_expr(string("--gain"),
		  "use_direct_amount ? total_expr :"
		  " market(get_at(total_expr, 0), date, exchange)"
		  " - get_at(total_expr, 1)");
    });

  OPTION(report_t, head_);

  OPTION_(report_t, invert, DO() {
      parent->HANDLER(amount_).set_expr(string("--invert"), "-amount");
    });

  OPTION__
  (report_t, limit_, // -l
   CTOR(report_t, limit_) {}
   virtual void on_with(const optional<string>& whence, const value_t& text) {
     if (! handled)
       option_t<report_t>::on_with(whence, text);
     else
       option_t<report_t>::on_with(whence,
				   string_value(string("(") + str() + ")&(" +
						text.as_string() + ")"));
   });

  OPTION(report_t, lot_dates);
  OPTION(report_t, lot_prices);
  OPTION(report_t, lot_tags);
  OPTION(report_t, lots);
  OPTION(report_t, lots_actual);

  OPTION_(report_t, market, DO() { // -V
      parent->HANDLER(revalued).on_only(string("--market"));
      parent->HANDLER(display_amount_)
	.set_expr(string("--market"), "market(amount_expr, date, exchange)");
      parent->HANDLER(display_total_)
	.set_expr(string("--market"), "market(total_expr, date, exchange)");
    });

  OPTION_(report_t, monthly, DO() { // -M
      parent->HANDLER(period_).on(string("--monthly"), "monthly");
    });

  OPTION_(report_t, no_color, DO() {
      parent->HANDLER(color).off();
    });

  OPTION(report_t, no_total);

  OPTION__
  (report_t, only_,
   CTOR(report_t, only_) {}
   virtual void on_with(const optional<string>& whence, const value_t& text) {
     if (! handled)
       option_t<report_t>::on_with(whence, text);
     else
       option_t<report_t>::on_with(whence,
				   string_value(string("(") + str() + ")&(" +
						text.as_string() + ")"));
   });

  OPTION(report_t, output_); // -o
  OPTION(report_t, pager_);
  OPTION(report_t, payee_as_account);

  OPTION_(report_t, pending, DO() { // -C
      parent->HANDLER(limit_).on(string("--pending"), "pending");
    });

  OPTION_(report_t, percent, DO() { // -%
      parent->HANDLER(total_)
	.set_expr(string("--percent"),
		  "is_account&parent&parent.total&percent(total, parent.total)");
    });

  OPTION__
  (report_t, period_, // -p
   CTOR(report_t, period_) {}
   virtual void on_with(const optional<string>& whence, const value_t& text) {
     if (! handled)
       option_t<report_t>::on_with(whence, text);
     else
       option_t<report_t>::on_with(whence,
				   string_value(text.as_string() + " " + str()));
   });

  OPTION__(report_t, plot_amount_format_, CTOR(report_t, plot_amount_format_) {
      on(none,
	 "%(format_date(date, \"%Y-%m-%d\")) %(quantity(scrub(display_amount)))\n");
    });

  OPTION__(report_t, plot_total_format_, CTOR(report_t, plot_total_format_) {
      on(none,
	 "%(format_date(date, \"%Y-%m-%d\")) %(quantity(scrub(display_total)))\n");
    });

  OPTION_(report_t, price, DO() { // -I
      parent->HANDLER(display_amount_)
	.set_expr(string("--price"), "price(amount_expr)");
      parent->HANDLER(display_total_)
	.set_expr(string("--price"), "price(total_expr)");
    });

  OPTION__(report_t, prices_format_, CTOR(report_t, prices_format_) {
      on(none,
	 "%-.9(date) %-8(account) %(justify(scrub(display_amount), 12, "
	 "    2 + 9 + 8 + 12, true, color))\n");
    });

  OPTION__(report_t, pricesdb_format_, CTOR(report_t, pricesdb_format_) {
      on(none,
	 "P %(datetime) %(account) %(scrub(display_amount))\n");
    });

  OPTION__(report_t, print_format_, CTOR(report_t, print_format_) {
      on(none,
	 "%(xact.date)"
	 "%(!effective & xact.effective_date ?"
	 " \"=\" + xact.effective_date : \"\")"
	 "%(xact.cleared ? \" *\" : (xact.pending ? \" !\" : \"\"))"
	 "%(code ? \" (\" + code + \")\" :"
	 " \"\") %(payee)%(xact.comment)\n"
	 "    %(xact.uncleared ?"
	 " (cleared ? \"* \" : (pending ? \"! \" : \"\")) : \"\")"
	 "%-34(account)"
	 "  %12(calculated ? \"\" : justify(scrub(amount), 12, -1, true))"
	 "%(has_cost & !cost_calculated ?"
	 " \" @ \" + justify(scrub(abs(cost / amount)), 0) : \"\")"
	 "%(comment)\n%/"
	 "    %(xact.uncleared ?"
	 " (cleared ? \"* \" : (pending ? \"! \" : \"\")) : \"\")"
	 "%-34(account)"
	 "  %12(calculated ? \"\" : justify(scrub(amount), 12, -1, true))"
	 "%(has_cost & !cost_calculated ?"
	 " \" @ \" + justify(scrub(abs(cost / amount)), 0) : \"\")"
	 "%(comment)\n%/\n");
    });

  OPTION_(report_t, quantity, DO() { // -O
      parent->HANDLER(revalued).off();
      parent->HANDLER(amount_).set_expr(string("--quantity"), "amount");
      parent->HANDLER(total_).set_expr(string("--quantity"), "total");
    });

  OPTION_(report_t, quarterly, DO() {
      parent->HANDLER(period_).on(string("--quarterly"), "quarterly");
    });

  OPTION(report_t, raw);

  OPTION_(report_t, real, DO() { // -R
      parent->HANDLER(limit_).on(string("--real"), "real");
    });

  OPTION__(report_t, register_format_, CTOR(report_t, register_format_) {
      on(none,
	 "%(ansify_if(justify(format_date(date), date_width), green "
	 "    if color & date > today))"
	 " %(ansify_if(justify(truncated(payee, payee_width), payee_width), "
	 "    bold if color & !cleared))"
	 " %(ansify_if(justify(truncated(account, account_width, abbrev_len), "
	 "    account_width), blue if color))"
	 " %(justify(scrub(display_amount), amount_width, "
	 "    3 + date_width + payee_width + account_width + amount_width, "
	 "    true, color))"
	 " %(justify(scrub(display_total), total_width, "
	 "    4 + date_width + payee_width + account_width + amount_width "
	 "    + total_width, true, color))\n%/"
	 "%(justify(\" \", 2 + date_width + payee_width))"
	 "%(ansify_if(justify(truncated(account, account_width, abbrev_len), "
	 "    account_width), blue if color))"
	 " %(justify(scrub(display_amount), amount_width, "
	 "    3 + date_width + payee_width + account_width + amount_width, "
	 "    true, color))"
	 " %(justify(scrub(display_total), total_width, "
	 "    4 + date_width + payee_width + account_width + amount_width "
	 "    + total_width, true, color))\n");
    });

  OPTION(report_t, related); // -r

  OPTION_(report_t, related_all, DO() {
      parent->HANDLER(related).on_only(string("--related-all"));
    });

  OPTION(report_t, revalued);
  OPTION(report_t, revalued_only);

  OPTION__
  (report_t, revalued_total_,
   expr_t expr;
   CTOR(report_t, revalued_total_) {}
   void set_expr(const optional<string>& whence, const string& str) {
     expr = str;
     on(whence, str);
   }
   DO_(args) {
     set_expr(args[0].to_string(), args[1].to_string());
   });

  OPTION(report_t, seed_);
  OPTION(report_t, set_account_);
  OPTION(report_t, set_payee_);

  OPTION_(report_t, sort_, DO_(args) { // -S
      on_with(args[0].as_string(), args[1]);
      parent->HANDLER(sort_xacts_).off();
      parent->HANDLER(sort_all_).off();
    });

  OPTION_(report_t, sort_all_, DO_(args) {
      parent->HANDLER(sort_).on_with(string("--sort-all"), args[1]);
      parent->HANDLER(sort_xacts_).off();
    });

  OPTION_(report_t, sort_xacts_, DO_(args) {
      parent->HANDLER(sort_).on_with(string("--sort-xacts"), args[1]);
      parent->HANDLER(sort_all_).off();
    });

  OPTION(report_t, start_of_week_);
  OPTION(report_t, subtotal); // -s
  OPTION(report_t, tail_);

  OPTION__
  (report_t, total_, // -T
   expr_t expr;
   CTOR(report_t, total_) {
     set_expr(none, "total");
   }
   void set_expr(const optional<string>& whence, const string& str) {
     expr = str;
     on(whence, str);
   }
   DO_(args) {
     set_expr(args[0].to_string(), args[1].to_string());
   });

  OPTION(report_t, total_data);

  OPTION_(report_t, truncate_, DO_(args) {
      string style(args[1].to_string());
      if (style == "leading")
	format_t::default_style = format_t::TRUNCATE_LEADING;
      else if (style == "middle")
	format_t::default_style = format_t::TRUNCATE_MIDDLE;
      else if (style == "trailing")
	format_t::default_style = format_t::TRUNCATE_TRAILING;
      else
	throw_(std::invalid_argument,
	       _("Unrecognized truncation style: '%1'") << style);
      format_t::default_style_changed = true;
    });

  OPTION_(report_t, unbudgeted, DO() {
      parent->budget_flags |= BUDGET_UNBUDGETED;
    });

  OPTION_(report_t, uncleared, DO() { // -U
      parent->HANDLER(limit_).on(string("--uncleared"), "uncleared|pending");
    });

  OPTION_(report_t, unround, DO() {
      parent->HANDLER(display_amount_)
	.set_expr(string("--unround"), "unrounded(amount_expr)");
      parent->HANDLER(display_total_)
	.set_expr(string("--unround"), "unrounded(total_expr)");
    });

  OPTION_(report_t, weekly, DO() { // -W
      parent->HANDLER(period_).on(string("--weekly"), "weekly");
    });

  OPTION_(report_t, wide, DO() { // -w
      parent->HANDLER(date_width_).on_with(string("--wide"), 9L);
      parent->HANDLER(date_width_).specified = true;
      parent->HANDLER(payee_width_).on_with(string("--wide"), 35L);
      parent->HANDLER(payee_width_).specified = true;
      parent->HANDLER(account_width_).on_with(string("--wide"), 39L);
      parent->HANDLER(account_width_).specified = true;
      parent->HANDLER(amount_width_).on_with(string("--wide"), 22L);
      parent->HANDLER(amount_width_).specified = true;
      parent->HANDLER(total_width_).on_with(string("--wide"), 22L);
      parent->HANDLER(total_width_).specified = true;
    });

  OPTION_(report_t, yearly, DO() { // -Y
      parent->HANDLER(period_).on(string("--yearly"), "yearly");
    });

  OPTION__(report_t, date_width_,
	   bool specified;
	   CTOR(report_t, date_width_) {
	     on_with(none, 9L);
	     specified = false;
	   }
	   DO_(args) { value = args[1].to_long(); specified = true; });
  OPTION__(report_t, payee_width_,
	   bool specified;
	   CTOR(report_t, payee_width_) {
	     on_with(none, 20L);
	     specified = false;
	   }
	   DO_(args) { value = args[1].to_long(); specified = true; });
  OPTION__(report_t, account_width_,
	   bool specified;
	   CTOR(report_t, account_width_) {
	     on_with(none, 23L);
	     specified = false;
	   }
	   DO_(args) { value = args[1].to_long(); specified = true; });
  OPTION__(report_t, amount_width_,
	   bool specified;
	   CTOR(report_t, amount_width_) {
	     on_with(none, 12L);
	     specified = false;
	   }
	   DO_(args) { value = args[1].to_long(); specified = true; });
  OPTION__(report_t, total_width_,
	   bool specified;
	   CTOR(report_t, total_width_) {
	     on_with(none, 12L);
	     specified = false;
	   }
	   DO_(args) { value = args[1].to_long(); specified = true; });
};

} // namespace ledger

#endif // _REPORT_H
