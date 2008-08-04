/*
 * Copyright (c) 2003-2008, John Wiegley.  All rights reserved.
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

#ifndef _REPORT_H
#define _REPORT_H

#include "session.h"
#include "handler.h"

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

class report_t : public noncopyable, public scope_t
{
  report_t();

public:
  optional<path> output_file;
  std::ostream * output_stream;

  string	 format_string;
  string	 date_output_format;
  string	 predicate;
  string	 secondary_predicate;
  string	 display_predicate;
  string	 report_period;
  string	 report_period_sort;
  string	 sort_string;
  string	 descend_expr;
  string	 forecast_limit;
  string	 reconcile_balance;
  string	 reconcile_date;

  expr_t	 amount_expr;
  expr_t	 total_expr;

  unsigned long  budget_flags;

  int		 head_entries;
  int		 tail_entries;

  bool		 show_collapsed;
  bool		 show_subtotal;
  bool		 show_totals;
  bool		 show_related;
  bool		 show_all_related;
  bool		 show_inverted;
  bool		 show_empty;
  bool		 days_of_the_week;
  bool		 by_payee;
  bool		 comm_as_payee;
  bool		 code_as_payee;
  bool		 show_revalued;
  bool		 show_revalued_only;
  bool		 keep_price;
  bool		 keep_date;
  bool		 keep_tag;
  bool		 entry_sort;
  bool		 sort_all;

  string	 account;
  optional<path> pager;

  bool           raw_mode;

  session_t&	 session;

  explicit report_t(session_t& _session)
    : amount_expr("amount"),
      total_expr("total"),

      head_entries(0),
      tail_entries(0),

      show_collapsed(false),
      show_subtotal(false),
      show_totals(false),
      show_related(false),
      show_all_related(false),
      show_inverted(false),
      show_empty(false),
      days_of_the_week(false),
      by_payee(false),
      comm_as_payee(false),
      code_as_payee(false),
      show_revalued(false),
      show_revalued_only(false),
      keep_price(false),
      keep_date(false),
      keep_tag(false),
      entry_sort(false),
      sort_all(false),

      raw_mode(false),

      session(_session)
  {
    TRACE_CTOR(report_t, "session_t&");
  }

  virtual ~report_t() {
    TRACE_DTOR(report_t);
  }

  //
  // Actual report generation; this is why we're here...
  //

  xact_handler_ptr
  chain_xact_handlers(xact_handler_ptr handler,
		      const bool handle_individual_transactions = true);

  void xacts_report(xact_handler_ptr handler);

  void entry_report(xact_handler_ptr handler, entry_t& entry);

  void sum_all_accounts();

  void accounts_report(acct_handler_ptr handler);

  void commodities_report(const string& format);

  void entry_report(const entry_t& entry, const string& format);

  //
  // Config options
  //

#if 0
  void eval(const string& expr) {
    expr_t(expr).calc(*this);
  }
  value_t option_eval(call_scope_t& args) {
    eval(args[0].as_string());
    return NULL_VALUE;
  }

  value_t option_amount_(call_scope_t& args) {
    eval(string("t=") + args[0].as_string());
    return NULL_VALUE;
  }
  value_t option_total_(call_scope_t& args) {
    eval(string("T()=") + args[0].as_string());
    return NULL_VALUE;
  }

  value_t option_raw(call_scope_t&) {
    raw_mode = true;
    return NULL_VALUE;
  }
#endif

  value_t option_format_(call_scope_t& args) {
    format_string = args[0].as_string();
    return NULL_VALUE;
  }

  value_t option_foo(call_scope_t&) {
    std::cout << "This is foo" << std::endl;
    return NULL_VALUE;
  }
  value_t option_bar_(call_scope_t& args) {
    std::cout << "This is bar: " << args[0] << std::endl;
    return args[0];
  }

  value_t option_limit_(call_scope_t& args) {
    if (! predicate.empty())
      predicate += "&";
    predicate += args[0].as_string();
    return true;
  }

  //
  // Formatting functions
  //

  value_t get_amount_expr(call_scope_t& scope);
  value_t get_total_expr(call_scope_t& scope);

  //
  // Scope members
  //

  virtual expr_t::ptr_op_t lookup(const string& name);
};

} // namespace ledger

#endif // _REPORT_H
