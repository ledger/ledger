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

  explicit report_t(session_t& _session);

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
  value_t fn_print_balance(call_scope_t& scope);
  value_t fn_strip(call_scope_t& scope);
  value_t fn_truncate(call_scope_t& scope);
  value_t fn_quoted(call_scope_t& scope);
  value_t fn_join(call_scope_t& scope);
  value_t fn_format_date(call_scope_t& scope);
  value_t fn_false(call_scope_t&) {
    return false;
  }

  value_t fn_options(call_scope_t& scope) {
    return value_t(static_cast<scope_t *>(this));
  }

  string report_format(option_t<report_t>& option) {
    if (HANDLED(format_))
      return HANDLER(format_).str();
    return option.str();
  }

  value_t reload_command(call_scope_t& scope) {
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

  OPTION(report_t, abbrev_len_);
  OPTION(report_t, account_);

  OPTION_(report_t, actual, DO() { // -L
      parent->HANDLER(limit_).append("actual");
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

  OPTION_(report_t, amount_data, DO() {
#if 0
      format_string = session.plot_amount_format;
#endif
    }); // -j

  OPTION(report_t, anon);
  OPTION(report_t, ansi);
  OPTION(report_t, ansi_invert);

  OPTION_(report_t, average, DO() { // -A
      parent->HANDLER(display_total_).set_expr("total_expr/count");
    });

  OPTION(report_t, balance_format_);
  OPTION(report_t, base);

  OPTION_(report_t, basis, DO() { // -B
      parent->HANDLER(revalued).off();
      parent->HANDLER(amount_).set_expr("cost");
    });

  OPTION_(report_t, begin_, DO_(args) { // -b
      interval_t interval(args[0].to_string());
      if (! is_valid(interval.begin))
	throw_(std::invalid_argument,
	       "Could not determine beginning of period '"
	       << args[0].to_string() << "'");

      string predicate =
	"date>=[" + to_iso_extended_string(interval.begin) + "]";
      parent->HANDLER(limit_).append(predicate);
    });

  OPTION_(report_t, budget, DO() {
      parent->budget_flags = BUDGET_BUDGETED;
    });

  OPTION(report_t, by_payee); // -P
  OPTION(report_t, cache_);

  OPTION_(report_t, cleared, DO() { // -C
      parent->HANDLER(limit_).append("cleared");
    });

  OPTION(report_t, code_as_payee);

  OPTION_(report_t, collapse, DO() { // -n
      // Make sure that balance reports are collapsed too, but only apply it
      // to account entries
      parent->HANDLER(display_).append("xact|depth<=1");
    });

  OPTION_(report_t, collapse_if_zero, DO() {
      parent->HANDLER(collapse).on();
    });

  OPTION(report_t, comm_as_payee); // -x
  OPTION(report_t, cost);
  OPTION(report_t, csv_format_);
  OPTION(report_t, current); // -c

  OPTION_(report_t, daily, DO() {
      parent->HANDLER(period_).prepend("daily");
    });

  OPTION(report_t, date_format_); // -y
  OPTION(report_t, deviation); // -D

  OPTION__
  (report_t, display_, // -d
   CTOR(report_t, display_) {}
   void append(const string& text) {
     if (! handled)
       on(text);
     else
       on(string("(") + str() + ")&(" + text + ")");
   }
   DO_(args) {
     append(args[0].to_string());
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
      parent->HANDLER(limit_).append(predicate);
#if 0
      terminus = interval.begin;
#endif
    });

  OPTION(report_t, equity_format_);
  OPTION(report_t, forecast_);
  OPTION(report_t, format_); // -F
  OPTION(report_t, gain); // -G
  OPTION(report_t, head_);
  OPTION(report_t, invert);

  OPTION__
  (report_t, limit_, // -l
   CTOR(report_t, limit_) {}
   void append(const string& text) {
     if (! handled)
       on(text);
     else
       on(string("(") + str() + ")&(" + text + ")");
   }
   DO_(args) {
     append(args[0].to_string());
   });

  OPTION(report_t, lot_dates);
  OPTION(report_t, lot_prices);
  OPTION(report_t, lot_tags);
  OPTION(report_t, lots);

  OPTION_(report_t, market, DO() { // -V
      parent->HANDLER(revalued).on();
      parent->HANDLER(display_amount_).set_expr("market_value(amount_expr)");
      parent->HANDLER(display_total_).set_expr("market_value(total_expr)");
    });

  OPTION_(report_t, monthly, DO() { // -M
      parent->HANDLER(period_).prepend("monthly");
    });

  OPTION__
  (report_t, only_,
   CTOR(report_t, only_) {}
   void append(const string& text) {
     if (! handled)
       on(text);
     else
       on(string("(") + str() + ")&(" + text + ")");
   }
   DO_(args) {
     append(args[0].to_string());
   });

  OPTION(report_t, output_); // -o
  OPTION(report_t, pager_);

  OPTION_(report_t, pending, DO() { // -C
      parent->HANDLER(limit_).append("pending");
    });

  OPTION(report_t, percentage); // -%
  OPTION(report_t, performance); // -g

  OPTION__
  (report_t, period_, // -p
   CTOR(report_t, period_) {}
   void prepend(const string& text) {
     if (! handled)
       on(text);
     else
       on(text + " " + str());
   }
   DO_(args) {
     prepend(args[0].to_string());
   });

  OPTION(report_t, period_sort_);
  OPTION(report_t, plot_amount_format_);
  OPTION(report_t, plot_total_format_);

  OPTION_(report_t, price, DO() { // -I
      parent->HANDLER(revalued).off();
      parent->HANDLER(amount_).set_expr("price");
    });

  OPTION(report_t, price_exp_); // -Z
  OPTION(report_t, prices_format_);
  OPTION(report_t, pricesdb_format_);
  OPTION(report_t, print_format_);

  OPTION_(report_t, quantity, DO() { // -O
      parent->HANDLER(revalued).off();
      parent->HANDLER(amount_).set_expr("amount");
      parent->HANDLER(total_).set_expr("total");
    });

  OPTION_(report_t, quarterly, DO() {
      parent->HANDLER(period_).prepend("quarterly");
    });

  OPTION_(report_t, real, DO() { // -R
      parent->HANDLER(limit_).append("real");
    });

  OPTION(report_t, register_format_);
  OPTION(report_t, related); // -r
  OPTION(report_t, related_all);
  OPTION(report_t, revalued);
  OPTION(report_t, revalued_only);
  OPTION(report_t, set_price_);

  OPTION_(report_t, sort_, DO_(args) { // -S
      on(args[0].to_string());
      parent->HANDLER(sort_entries_).off();
      parent->HANDLER(sort_all_).off();
    });

  OPTION_(report_t, sort_all_, DO_(args) {
      parent->HANDLER(sort_).on(args[0].to_string());
      parent->HANDLER(sort_entries_).off();
    });

  OPTION_(report_t, sort_entries_, DO_(args) {
      parent->HANDLER(sort_).on(args[0].to_string());
      parent->HANDLER(sort_all_).off();
    });

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

  OPTION_(report_t, total_data, DO() {
#if 0
      format_string = session.plot_total_format;
#endif
    }); // -J

  OPTION(report_t, totals);

  OPTION_(report_t, truncate_, DO_(args) {
#if 0
      std::string style(optarg);
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
      parent->HANDLER(limit_).append("uncleared|pending");
    });

  OPTION_(report_t, weekly, DO() { // -W
      parent->HANDLER(period_).prepend("weekly");
    });

  OPTION(report_t, wide); // -w
  OPTION(report_t, wide_register_format_);

  OPTION_(report_t, yearly, DO() { // -Y
      parent->HANDLER(period_).prepend("yearly");
    });
};

// jww (2009-02-10): These should perhaps live elsewhere
value_t entry_command(call_scope_t& args);
value_t template_command(call_scope_t& args);

entry_t * derive_new_entry(report_t& report,
			   value_t::sequence_t::const_iterator i,
			   value_t::sequence_t::const_iterator end);

string join_args(call_scope_t& args);


} // namespace ledger

#endif // _REPORT_H
