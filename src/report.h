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

#include "session.h"
#include "chain.h"

namespace ledger {

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
//    a. Transaction or commodity iteration.  In this mode, all the journal's
//       entries, the transactions of a specific entry, or all the journal's
//       commodities are walked.  In the first two cases, it's the underlying
//       transactions which are passed to #2; in the second case, each
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
//       transaction in the journal.
// 
//    d. Dump binary file.  This is just like 'c', except that it dumps out a
//       binary file and #2 is completely ignored.
//
// 4. For 'a' and 'b' in #3, there is a different iteration function called,
//    depending on whether we're iterating:
//
//    a. The transactions of an entry: walk_transactions.
//    b. The entries of a journal: walk_entries.
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

#define BUDGET_NO_BUDGET  0x00
#define BUDGET_BUDGETED   0x01
#define BUDGET_UNBUDGETED 0x02

  uint_least8_t budget_flags;

  explicit report_t(session_t& _session)
    : session(_session), budget_flags(BUDGET_NO_BUDGET) {}

  virtual ~report_t() {
    output_stream.close();
  }

  void xacts_report(xact_handler_ptr handler);
  void entry_report(xact_handler_ptr handler, entry_t& entry);
  void accounts_report(acct_handler_ptr handler);
  void commodities_report(xact_handler_ptr handler);

  void sum_all_accounts();

  value_t fn_amount_expr(call_scope_t& scope);
  value_t fn_total_expr(call_scope_t& scope);
  value_t fn_display_amount(call_scope_t& scope);
  value_t fn_display_total(call_scope_t& scope);
  value_t fn_market_value(call_scope_t& scope);
  value_t fn_strip(call_scope_t& scope);
  value_t fn_scrub(call_scope_t& scope);
  value_t fn_quantity(call_scope_t& scope);
  value_t fn_rounded(call_scope_t& scope);
  value_t fn_truncate(call_scope_t& scope);
  value_t fn_justify(call_scope_t& scope);
  value_t fn_quoted(call_scope_t& scope);
  value_t fn_join(call_scope_t& scope);
  value_t fn_format_date(call_scope_t& scope);
  value_t fn_ansify_if(call_scope_t& scope);
  value_t fn_false(call_scope_t&) {
    return false;
  }

  value_t fn_options(call_scope_t&) {
    return value_t(static_cast<scope_t *>(this));
  }

  string report_format(option_t<report_t>& option) {
    if (HANDLED(format_))
      return HANDLER(format_).str();
    return option.str();
  }

  value_t reload_command(call_scope_t&) {
    session.close_journal_files();
    session.read_journal_files();
    return true;
  }

  keep_details_t what_to_keep() {
    return keep_details_t(HANDLED(lots) || HANDLED(lot_prices),
			  HANDLED(lots) || HANDLED(lot_dates),
			  HANDLED(lots) || HANDLED(lot_tags),
			  HANDLED(base));
  }

  option_t<report_t> * lookup_option(const char * p);

  virtual void define(const string& name, expr_t::ptr_op_t def) {
    session.define(name, def);
  }

  virtual expr_t::ptr_op_t lookup(const string& name);

  /**
   * Option handlers
   */

  OPTION__(report_t, abbrev_len_,
	   CTOR(report_t, abbrev_len_) { on_with(2L); });
  OPTION(report_t, account_);

  OPTION__
  (report_t, account_amount_,
   expr_t expr;
   CTOR(report_t, account_amount_) {
     set_expr("amount");
   }
   void set_expr(const string& str) {
     expr = str;
     on(str);
   }
   DO_(args) {
     set_expr(args[0].to_string());
   });

  OPTION_(report_t, actual, DO() { // -L
      parent->HANDLER(limit_).on("actual");
    });

  OPTION_(report_t, add_budget, DO() {
      parent->budget_flags = BUDGET_BUDGETED | BUDGET_UNBUDGETED;
    });

  OPTION__
  (report_t, amount_, // -t
   expr_t expr;
   CTOR(report_t, amount_) {
     set_expr("amount");
   }
   void set_expr(const string& str) {
     expr = str;
     on(str);
   }
   DO_(args) {
     set_expr(args[0].to_string());
   });

  OPTION_(report_t, amount_data, DO() { // -j
      parent->HANDLER(format_).on_with(parent->HANDLER(plot_amount_format_).value);
    });

  OPTION(report_t, anon);

  OPTION_(report_t, average, DO() { // -A
      parent->HANDLER(display_total_).set_expr("total_expr/count");
    });

  OPTION__(report_t, balance_format_, CTOR(report_t, balance_format_) {
      on("%(ansify_if(justify(scrub(display_total), 20, -1, true), \"red\", "
	 "    color & scrub(display_total) < 0))"
	 "  %(!options.flat ? depth_spacer : \"\")"
	 "%-(ansify_if(partial_account(options.flat), \"blue\", color))\n");
    });

  OPTION(report_t, base);

  OPTION_(report_t, basis, DO() { // -B
      parent->HANDLER(revalued).off();
      parent->HANDLER(amount_).set_expr("rounded(cost)");
    });

  OPTION_(report_t, begin_, DO_(args) { // -b
      interval_t interval(args[0].to_string());
      if (! is_valid(interval.begin))
	throw_(std::invalid_argument,
	       "Could not determine beginning of period '"
	       << args[0].to_string() << "'");

      string predicate =
	"date>=[" + to_iso_extended_string(interval.begin) + "]";
      parent->HANDLER(limit_).on(predicate);
    });

  OPTION_(report_t, budget, DO() {
      parent->budget_flags = BUDGET_BUDGETED;
    });

  OPTION(report_t, by_payee); // -P

  OPTION_(report_t, cleared, DO() { // -C
      parent->HANDLER(limit_).on("cleared");
    });

  OPTION(report_t, code_as_payee);
  OPTION(report_t, comm_as_payee);
  OPTION(report_t, code_as_account);
  OPTION(report_t, comm_as_account);
  OPTION(report_t, color);

  OPTION_(report_t, collapse, DO() { // -n
      // Make sure that balance reports are collapsed too, but only apply it
      // to account entries
      parent->HANDLER(display_).on("xact|depth<=1");
    });

  OPTION_(report_t, collapse_if_zero, DO() {
      parent->HANDLER(collapse).on_only();
    });

  OPTION(report_t, columns_);

  OPTION__(report_t, csv_format_, CTOR(report_t, csv_format_) {
      on("%(quoted(date)),"
	 "%(quoted(payee)),"
	 "%(quoted(account)),"
	 "%(quoted(scrub(display_amount))),"
	 "%(quoted((cleared or entry.cleared) ?"
	 " \"*\" : ((pending or entry.pending) ? \"!\" : \"\"))),"
	 "%(quoted(code)),"
	 "%(quoted(join(note)))\n");
    });

  OPTION_(report_t, current, DO() { // -c
      parent->HANDLER(limit_).on("date<=today");
    });

  OPTION_(report_t, daily, DO() {
      parent->HANDLER(period_).on("daily");
    });

  OPTION__(report_t, date_format_, // -y
	   CTOR(report_t, date_format_) {
	     on("%y-%b-%d");
	   });

  OPTION_(report_t, deviation, DO() { // -D
      parent->HANDLER(display_total_).set_expr("amount_expr-total_expr/count");
    });

  OPTION__
  (report_t, display_, // -d
   CTOR(report_t, display_) {}
   virtual void on_with(const value_t& text) {
     if (! handled)
       option_t<report_t>::on_with(text);
     else
       option_t<report_t>::on_with(string_value(string("(") + str() + ")&(" +
						text.as_string() + ")"));
   });

  OPTION__
  (report_t, display_amount_,
   expr_t expr;
   CTOR(report_t, display_amount_) {
     set_expr("amount_expr");
   }
   void set_expr(const string& str) {
     expr = str;
     on(str);
   }
   DO_(args) {
     set_expr(args[0].to_string());
   });

  OPTION__
  (report_t, display_total_,
   expr_t expr;
   CTOR(report_t, display_total_) {
     set_expr("total_expr");
   }
   void set_expr(const string& str) {
     expr = str;
     on(str);
   }
   DO_(args) {
     set_expr(args[0].to_string());
   });

  OPTION(report_t, dow);
  OPTION(report_t, effective);
  OPTION(report_t, empty); // -E

  OPTION_(report_t, end_, DO_(args) { // -e
      interval_t interval(args[0].to_string());
      if (! is_valid(interval.begin))
	throw_(std::invalid_argument,
	       "Could not determine end of period '"
	       << args[0].to_string() << "'");

      string predicate =
	"date<[" + to_iso_extended_string(interval.begin) + "]";
      parent->HANDLER(limit_).on(predicate);
#if 0
      terminus = interval.begin;
#endif
    });

  OPTION(report_t, equity);
  OPTION(report_t, exact);

  OPTION_(report_t, exchange_, DO_(args) { // -x
      on_with(args[0]);
      call_scope_t no_args(*parent);
      parent->HANDLER(market).parent = parent;
      parent->HANDLER(market).handler(no_args);
    });

  OPTION(report_t, flat);
  OPTION(report_t, forecast_while_);
  OPTION(report_t, format_); // -F
  OPTION(report_t, gain); // -G
  OPTION(report_t, head_);

  OPTION_(report_t, invert, DO() {
      parent->HANDLER(amount_).set_expr("-amount");
    });

  OPTION__
  (report_t, limit_, // -l
   CTOR(report_t, limit_) {}
   virtual void on_with(const value_t& text) {
     if (! handled)
       option_t<report_t>::on_with(text);
     else
       option_t<report_t>::on_with(string_value(string("(") + str() + ")&(" +
						text.as_string() + ")"));
   });

  OPTION(report_t, lot_dates);
  OPTION(report_t, lot_prices);
  OPTION(report_t, lot_tags);
  OPTION(report_t, lots);

  OPTION_(report_t, market, DO() { // -V
      parent->HANDLER(revalued).on_only();
      parent->HANDLER(display_amount_)
	.set_expr("market(amount_expr, now, exchange)");
      parent->HANDLER(display_total_)
	.set_expr("market(total_expr, now, exchange)");
    });

  OPTION_(report_t, monthly, DO() { // -M
      parent->HANDLER(period_).on("monthly");
    });

  OPTION(report_t, no_total);

  OPTION__
  (report_t, only_,
   CTOR(report_t, only_) {}
   virtual void on_with(const value_t& text) {
     if (! handled)
       option_t<report_t>::on_with(text);
     else
       option_t<report_t>::on_with(string_value(string("(") + str() + ")&(" +
						text.as_string() + ")"));
   });

  OPTION(report_t, output_); // -o
  OPTION(report_t, pager_);
  OPTION(report_t, payee_as_account);

  OPTION_(report_t, pending, DO() { // -C
      parent->HANDLER(limit_).on("pending");
    });

  OPTION(report_t, percentage); // -%
  OPTION(report_t, performance); // -g

  OPTION__
  (report_t, period_, // -p
   CTOR(report_t, period_) {}
   virtual void on_with(const value_t& text) {
     if (! handled)
       option_t<report_t>::on_with(text);
     else
       option_t<report_t>::on_with(string_value(text.as_string() + " " + str()));
   });

  OPTION(report_t, period_sort_);

  OPTION__(report_t, plot_amount_format_, CTOR(report_t, plot_amount_format_) {
      on("%(format_date(date, \"%Y-%m-%d\")) %(quantity(scrub(display_amount)))\n");
    });

  OPTION__(report_t, plot_total_format_, CTOR(report_t, plot_total_format_) {
      on("%(format_date(date, \"%Y-%m-%d\")) %(quantity(scrub(display_total)))\n");
    });

  OPTION_(report_t, price, DO() { // -I
      parent->HANDLER(revalued).off();
      parent->HANDLER(amount_).set_expr("price");
    });

  OPTION(report_t, price_exp_); // -Z

  OPTION__(report_t, prices_format_, CTOR(report_t, prices_format_) {
      on("%-.9(date) %-8(account) %12(scrub(display_amount))\n");
    });

  OPTION__(report_t, pricesdb_format_, CTOR(report_t, pricesdb_format_) {
      on("P %[%Y/%m/%d %H:%M:%S] %A %t\n");
    });

  OPTION__(report_t, print_format_, CTOR(report_t, print_format_) {
      on("%(format_date(entry.date, \"%Y/%m/%d\"))"
	 "%(entry.cleared ? \" *\" : (entry.pending ? \" !\" : \"\"))"
	 "%(code ? \" (\" + code + \")\" : \"\") %(payee)%(entry.comment | \"\")\n"
	 "    %(entry.uncleared ? (cleared ? \"* \" : (pending ? \"! \" : \"\")) : \"\")"
	 "%-34(account)"
	 "  %12(calculated ? \"\" : justify(scrub(amount), 12, -1, true))"
	 "%(comment | \"\")\n%/"
	 "    %(entry.uncleared ? (cleared ? \"* \" : (pending ? \"! \" : \"\")) : \"\")"
	 "%-34(account)"
	 "  %12(calculated ? \"\" : justify(scrub(amount), 12, -1, true))"
	 "%(comment | \"\")\n%/\n");
    });

  OPTION_(report_t, quantity, DO() { // -O
      parent->HANDLER(revalued).off();
      parent->HANDLER(amount_).set_expr("amount");
      parent->HANDLER(total_).set_expr("total");
    });

  OPTION_(report_t, quarterly, DO() {
      parent->HANDLER(period_).on("quarterly");
    });

  OPTION(report_t, raw);

  OPTION_(report_t, real, DO() { // -R
      parent->HANDLER(limit_).on("real");
    });

  OPTION__(report_t, register_format_, CTOR(report_t, register_format_) {
      on("%(ansify_if(justify(date, date_width), \"green\", color & date > today))"
	 " %(ansify_if(justify(truncate(payee, payee_width), payee_width), "
	 "    \"bold\", color & !cleared))"
	 " %(ansify_if(justify(truncate(account, account_width, abbrev_len), "
	 "    account_width), \"blue\", color))"
	 " %(ansify_if(justify(scrub(display_amount), amount_width, "
	 "    3 + date_width + payee_width + account_width + amount_width, true), "
	 "    \"red\", color & scrub(display_amount) < 0))"
	 " %(ansify_if(justify(scrub(display_total), total_width, "
	 "    4 + date_width + payee_width + account_width + amount_width "
	 "    + total_width, true), \"red\", color & scrub(display_amount) < 0))\n%/"
	 "%(justify(\" \", 2 + date_width + payee_width))"
	 "%(ansify_if(justify(truncate(account, account_width, abbrev_len), "
	 "    account_width), \"blue\", color))"
	 " %(ansify_if(justify(scrub(display_amount), amount_width, "
	 "    3 + date_width + payee_width + account_width + amount_width, true), "
	 "    \"red\", color & scrub(display_amount) < 0))"
	 " %(ansify_if(justify(scrub(display_total), total_width, "
	 "    4 + date_width + payee_width + account_width + amount_width "
	 "    + total_width, true), \"red\", color & scrub(display_amount) < 0))\n");
    });

  OPTION(report_t, related); // -r
  OPTION(report_t, related_all);
  OPTION(report_t, revalued);
  OPTION(report_t, revalued_only);
  OPTION(report_t, set_account_);
  OPTION(report_t, set_payee_);
  OPTION(report_t, set_price_);

  OPTION_(report_t, sort_, DO_(args) { // -S
      on_with(args[0]);
      parent->HANDLER(sort_entries_).off();
      parent->HANDLER(sort_all_).off();
    });

  OPTION_(report_t, sort_all_, DO_(args) {
      parent->HANDLER(sort_).on_with(args[0]);
      parent->HANDLER(sort_entries_).off();
    });

  OPTION_(report_t, sort_entries_, DO_(args) {
      parent->HANDLER(sort_).on_with(args[0]);
      parent->HANDLER(sort_all_).off();
    });

  OPTION(report_t, start_of_week_);
  OPTION(report_t, subtotal); // -s
  OPTION(report_t, tail_);

  OPTION__
  (report_t, total_, // -T
   expr_t expr;
   CTOR(report_t, total_) {
     set_expr("total");
   }
   void set_expr(const string& str) {
     expr = str;
     on(str);
   }
   DO_(args) {
     set_expr(args[0].to_string());
   });

  OPTION_(report_t, total_data, DO() { // -J
      parent->HANDLER(format_).on_with(parent->HANDLER(plot_total_format_).value);
    });

  OPTION_(report_t, truncate_, DO() {
#if 0
      string style(args[0].to_string());
      if (style == "leading")
	format_t::elision_style = format_t::TRUNCATE_LEADING;
      else if (style == "middle")
	format_t::elision_style = format_t::TRUNCATE_MIDDLE;
      else if (style == "trailing")
	format_t::elision_style = format_t::TRUNCATE_TRAILING;
      else if (style == "abbrev")
	format_t::elision_style = format_t::ABBREVIATE;
#endif
    });

  OPTION_(report_t, unbudgeted, DO() {
      parent->budget_flags = BUDGET_UNBUDGETED;
    });

  OPTION_(report_t, uncleared, DO() { // -U
      parent->HANDLER(limit_).on("uncleared|pending");
    });

  OPTION_(report_t, weekly, DO() { // -W
      parent->HANDLER(period_).on("weekly");
    });

  OPTION_(report_t, wide, DO() { // -w
      parent->HANDLER(date_width_).on_with(9L);
      parent->HANDLER(date_width_).specified = true;
      parent->HANDLER(payee_width_).on_with(35L);
      parent->HANDLER(payee_width_).specified = true;
      parent->HANDLER(account_width_).on_with(39L);
      parent->HANDLER(account_width_).specified = true;
      parent->HANDLER(amount_width_).on_with(22L);
      parent->HANDLER(amount_width_).specified = true;
      parent->HANDLER(total_width_).on_with(22L);
      parent->HANDLER(total_width_).specified = true;
    });

  OPTION_(report_t, yearly, DO() { // -Y
      parent->HANDLER(period_).on("yearly");
    });

  OPTION__(report_t, date_width_,
	   bool specified;
	   CTOR(report_t, date_width_) { on_with(9L); specified = false; }
	   DO_(args) { value = args[0].to_long(); specified = true; });
  OPTION__(report_t, payee_width_,
	   bool specified;
	   CTOR(report_t, payee_width_) { on_with(20L); specified = false; }
	   DO_(args) { value = args[0].to_long(); specified = true; });
  OPTION__(report_t, account_width_,
	   bool specified;
	   CTOR(report_t, account_width_) { on_with(23L); specified = false; }
	   DO_(args) { value = args[0].to_long(); specified = true; });
  OPTION__(report_t, amount_width_,
	   bool specified;
	   CTOR(report_t, amount_width_) { on_with(12L); specified = false; }
	   DO_(args) { value = args[0].to_long(); specified = true; });
  OPTION__(report_t, total_width_,
	   bool specified;
	   CTOR(report_t, total_width_) { on_with(12L); specified = false; }
	   DO_(args) { value = args[0].to_long(); specified = true; });
};

} // namespace ledger

#endif // _REPORT_H
