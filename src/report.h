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

  void xacts_report(xact_handler_ptr handler);
  void entry_report(xact_handler_ptr handler, entry_t& entry);
  void sum_all_accounts();
  void accounts_report(acct_handler_ptr handler);
  void commodities_report(const string& format);

  xact_handler_ptr
  chain_xact_handlers(xact_handler_ptr handler,
		      const bool handle_individual_transactions = true);

#if 0
  //////////////////////////////////////////////////////////////////////
  //
  // Basic options

  value_t option_full_help(call_scope_t& args) { // H
    option_full_help(std::cout);
    throw 0;
  }

  value_t option_help(call_scope_t& args) { // h
    option_help(std::cout);
    throw 0;
  }

  value_t option_help_calc(call_scope_t& args) {
    option_calc_help(std::cout);
    throw 0;
  }

  value_t option_help_disp(call_scope_t& args) {
    option_disp_help(std::cout);
    throw 0;
  }

  value_t option_help_comm(call_scope_t& args) {
    option_comm_help(std::cout);
    throw 0;
  }

  value_t option_version(call_scope_t& args) { // v
    show_version(std::cout);
    throw 0;
  }

  value_t option_init_file(call_scope_t& args) { // i:
    std::string path = resolve_path(optarg);
    if (access(path.c_str(), R_OK) != -1)
      config->init_file = path;
    else
      throw_(std::invalid_argument,
	     "The init file '" << path << "' does not exist or is not readable");
  }

  value_t option_file(call_scope_t& args) { // f:
    if (std::string(optarg) == "-") {
      config->data_file = optarg;
    } else {
      std::string path = resolve_path(optarg);
      if (access(path.c_str(), R_OK) != -1)
	config->data_file = path;
      else
	throw_(std::invalid_argument,
	       "The ledger file '" << path << "' does not exist or is not readable");
    }
  }

  value_t option_cache(call_scope_t& args) { // :
    config->cache_file = resolve_path(optarg);
  }

  value_t option_no_cache(call_scope_t& args) {
    config->cache_file = "<none>";
  }

  value_t option_output(call_scope_t& args) { // o:
    if (std::string(optarg) != "-") {
      std::string path = resolve_path(optarg);
      report->output_file = path;
    }
  }

  value_t option_account(call_scope_t& args) { // a:
    config->account = optarg;
  }

  value_t option_debug(call_scope_t& args) { // :
    config->debug_mode = true;
    ::setenv("DEBUG_CLASS", optarg, 1);
  }

  value_t option_verbose(call_scope_t& args) {
    config->verbose_mode = true;
  }

  value_t option_trace(call_scope_t& args) {
    config->trace_mode = true;
  }

  //////////////////////////////////////////////////////////////////////
  //
  // Report filtering

  value_t option_effective(call_scope_t& args) {
    xact_t::use_effective_date = true;
  }

  value_t option_begin(call_scope_t& args) { // b:
    char buf[128];
    interval_t interval(optarg);
    if (! interval.begin)
      throw_(std::invalid_argument,
	     "Could not determine beginning of period '" << optarg << "'");

    if (! report->predicate.empty())
      report->predicate += "&";
    report->predicate += "d>=[";
    report->predicate += interval.begin.to_string();
    report->predicate += "]";
  }

  value_t option_end(call_scope_t& args) { // e:
    char buf[128];
    interval_t interval(optarg);
    if (! interval.begin)
      throw_(std::invalid_argument,
	     "Could not determine end of period '" << optarg << "'");

    if (! report->predicate.empty())
      report->predicate += "&";
    report->predicate += "d<[";
    report->predicate += interval.begin.to_string();
    report->predicate += "]";

    terminus = interval.begin;
  }

  value_t option_current(call_scope_t& args) { // c
    if (! report->predicate.empty())
      report->predicate += "&";
    report->predicate += "d<=m";
  }

  value_t option_cleared(call_scope_t& args) { // C
    if (! report->predicate.empty())
      report->predicate += "&";
    report->predicate += "X";
  }

  value_t option_uncleared(call_scope_t& args) { // U
    if (! report->predicate.empty())
      report->predicate += "&";
    report->predicate += "!X";
  }

  value_t option_real(call_scope_t& args) { // R
    if (! report->predicate.empty())
      report->predicate += "&";
    report->predicate += "R";
  }

  value_t option_actual(call_scope_t& args) { // L
    if (! report->predicate.empty())
      report->predicate += "&";
    report->predicate += "L";
  }

  value_t option_lots(call_scope_t& args) {
    report->keep_price =
      report->keep_date  =
      report->keep_tag   = true;
  }

  value_t option_lot_prices(call_scope_t& args) {
    report->keep_price = true;
  }

  value_t option_lot_dates(call_scope_t& args) {
    report->keep_date = true;
  }

  value_t option_lot_tags(call_scope_t& args) {
    report->keep_tag = true;
  }
#endif

  //////////////////////////////////////////////////////////////////////
  //
  // Output customization

  value_t option_format_(call_scope_t& args) { // F:
    format_string = args[0].as_string();
    return true;
  }

#if 0
  value_t option_date_format(call_scope_t& args) { // y:
    report->date_output_format = optarg;
  }

  value_t option_input_date_format(call_scope_t& args) { // :
    config->date_input_format = optarg;
  }

  value_t option_balance_format(call_scope_t& args) { // :
    config->balance_format = optarg;
  }

  value_t option_register_format(call_scope_t& args) { // :
    config->register_format = optarg;
  }

  value_t option_wide_register_format(call_scope_t& args) { // :
    config->wide_register_format = optarg;
  }

  value_t option_plot_amount_format(call_scope_t& args) { // :
    config->plot_amount_format = optarg;
  }

  value_t option_plot_total_format(call_scope_t& args) { // :
    config->plot_total_format = optarg;
  }

  value_t option_print_format(call_scope_t& args) { // :
    config->print_format = optarg;
  }

  value_t option_write_hdr_format(call_scope_t& args) { // :
    config->write_hdr_format = optarg;
  }

  value_t option_write_xact_format(call_scope_t& args) { // :
    config->write_xact_format = optarg;
  }

  value_t option_equity_format(call_scope_t& args) { // :
    config->equity_format = optarg;
  }

  value_t option_prices_format(call_scope_t& args) { // :
    config->prices_format = optarg;
  }

  value_t option_wide(call_scope_t& args) { // w
    config->register_format = config->wide_register_format;
  }

  value_t option_head(call_scope_t& args) { // :
    report->head_entries = std::atoi(optarg);
  }

  value_t option_tail(call_scope_t& args) { // :
    report->tail_entries = std::atoi(optarg);
  }

  value_t option_pager(call_scope_t& args) { // :
    config->pager = optarg;
  }

  value_t option_truncate(call_scope_t& args) { // :
    std::string style(optarg);
    if (style == "leading")
      format_t::elision_style = format_t::TRUNCATE_LEADING;
    else if (style == "middle")
      format_t::elision_style = format_t::TRUNCATE_MIDDLE;
    else if (style == "trailing")
      format_t::elision_style = format_t::TRUNCATE_TRAILING;
    else if (style == "abbrev")
      format_t::elision_style = format_t::ABBREVIATE;
  }

  value_t option_abbrev_len(call_scope_t& args) { // :
    format_t::abbrev_length = std::atoi(optarg);
  }

  value_t option_empty(call_scope_t& args) { // E
    report->show_empty = true;
  }

  value_t option_collapse(call_scope_t& args) { // n
    report->show_collapsed = true;
  }

  value_t option_subtotal(call_scope_t& args) { // s
    report->show_subtotal = true;
  }

  value_t option_totals(call_scope_t& args) {
    report->show_totals = true;
  }

  value_t option_sort(call_scope_t& args) { // S:
    report->sort_string = optarg;
  }

  value_t option_sort_entries(call_scope_t& args) {
    report->sort_string = optarg;
    report->entry_sort = true;
  }

  value_t option_sort_all(call_scope_t& args) {
    report->sort_string = optarg;
    report->entry_sort = false;
    report->sort_all   = true;
  }

  value_t option_period_sort(call_scope_t& args) { // :
    report->sort_string = optarg;
    report->entry_sort = true;
  }

  value_t option_related(call_scope_t& args) { // r
    report->show_related = true;
  }

  value_t option_descend(call_scope_t& args) {
    std::string arg(optarg);
    std::string::size_type beg = 0;
    report->descend_expr = "";
    for (std::string::size_type pos = arg.find(';');
	 pos != std::string::npos;
	 beg = pos + 1, pos = arg.find(';', beg))
      report->descend_expr += (std::string("t=={") +
			       std::string(arg, beg, pos - beg) + "};");
    report->descend_expr += (std::string("t=={") +
			     std::string(arg, beg) + "}");
  }

  value_t option_descend_if(call_scope_t& args) {
    report->descend_expr = optarg;
  }

  value_t option_period(call_scope_t& args) { // p:
    if (report->report_period.empty()) {
      report->report_period = optarg;
    } else {
      report->report_period += " ";
      report->report_period += optarg;
    }

    // If the period gives a beginning and/or ending date, make sure to
    // modify the calculation predicate (via the --begin and --end
    // options) to take this into account.

    interval_t interval(report->report_period);

    if (interval.begin) {
      if (! report->predicate.empty())
	report->predicate += "&";
      report->predicate += "d>=[";
      report->predicate += interval.begin.to_string();
      report->predicate += "]";
    }

    if (interval.end) {
      if (! report->predicate.empty())
	report->predicate += "&";
      report->predicate += "d<[";
      report->predicate += interval.end.to_string();
      report->predicate += "]";

      terminus = interval.end;
    }
  }

  value_t option_daily(call_scope_t& args) {
    if (report->report_period.empty())
      report->report_period = "daily";
    else
      report->report_period = std::string("daily ") + report->report_period;
  }

  value_t option_weekly(call_scope_t& args) { // W
    if (report->report_period.empty())
      report->report_period = "weekly";
    else
      report->report_period = std::string("weekly ") + report->report_period;
  }

  value_t option_monthly(call_scope_t& args) { // M
    if (report->report_period.empty())
      report->report_period = "monthly";
    else
      report->report_period = std::string("monthly ") + report->report_period;
  }

  value_t option_quarterly(call_scope_t& args) {
    if (report->report_period.empty())
      report->report_period = "quarterly";
    else
      report->report_period = std::string("quarterly ") + report->report_period;
  }

  value_t option_yearly(call_scope_t& args) { // Y
    if (report->report_period.empty())
      report->report_period = "yearly";
    else
      report->report_period = std::string("yearly ") + report->report_period;
  }

  value_t option_dow(call_scope_t& args) {
    report->days_of_the_week = true;
  }

  value_t option_by_payee(call_scope_t& args) { // P
    report->by_payee = true;
  }

  value_t option_comm_as_payee(call_scope_t& args) { // x
    report->comm_as_payee = true;
  }

  value_t option_code_as_payee(call_scope_t& args) {
    report->code_as_payee = true;
  }

  value_t option_budget(call_scope_t& args) {
    report->budget_flags = BUDGET_BUDGETED;
  }

  value_t option_add_budget(call_scope_t& args) {
    report->budget_flags = BUDGET_BUDGETED | BUDGET_UNBUDGETED;
  }

  value_t option_unbudgeted(call_scope_t& args) {
    report->budget_flags = BUDGET_UNBUDGETED;
  }

  value_t option_forecast(call_scope_t& args) { // :
    report->forecast_limit = optarg;
  }

  value_t option_reconcile(call_scope_t& args) { // :
    report->reconcile_balance = optarg;
  }

  value_t option_reconcile_date(call_scope_t& args) { // :
    report->reconcile_date = optarg;
  }
#endif

  void append_predicate(const string& str) {
    if (! predicate.empty())
      predicate = string("(") + predicate + ")&";
    predicate += str;
  }

  value_t option_limit_(call_scope_t& args) { // l:
    append_predicate(args[0].as_string());
    return true;
  }

#if 0
  value_t option_only(call_scope_t& args) { // :
    if (! report->secondary_predicate.empty())
      report->secondary_predicate += "&";
    report->secondary_predicate += "(";
    report->secondary_predicate += optarg;
    report->secondary_predicate += ")";
  }

  value_t option_display(call_scope_t& args) { // d:
    if (! report->display_predicate.empty())
      report->display_predicate += "&";
    report->display_predicate += "(";
    report->display_predicate += optarg;
    report->display_predicate += ")";
  }
#endif

  value_t option_amount_(call_scope_t& args) { // t:
    amount_expr = args[0].as_string();
    return true;
  }

  value_t option_total_(call_scope_t& args) { // T:
    total_expr = args[0].as_string();
    return true;
  }

  value_t get_amount_expr(call_scope_t& scope);
  value_t get_total_expr(call_scope_t& scope);

  value_t option_amount_data(call_scope_t&) { // j
    format_string = session.plot_amount_format;
    return true;
  }

  value_t option_total_data(call_scope_t&) { // J
    format_string = session.plot_total_format;
    return true;
  }

#if 0
  value_t option_ansi(call_scope_t& args) {
    format_t::ansi_codes  = true;
    format_t::ansi_invert = false;
  }

  value_t option_ansi_invert(call_scope_t& args) {
    format_t::ansi_codes  =
    format_t::ansi_invert = true;
  }

  //////////////////////////////////////////////////////////////////////
  //
  // Commodity reporting

  value_t option_base(call_scope_t& args) { // :
    amount_t::keep_base = true;
  }

  value_t option_price_db(call_scope_t& args) { // :
    config->price_db = optarg;
  }

  value_t option_price_exp(call_scope_t& args) { // Z:
    config->pricing_leeway = std::atol(optarg) * 60;
  }

  value_t option_download(call_scope_t& args) { // Q
    config->download_quotes = true;
  }

  value_t option_quantity(call_scope_t& args) { // O
    ledger::amount_expr = "amount";
    ledger::total_expr  = "total";
  }

  value_t option_basis(call_scope_t& args) { // B
    ledger::amount_expr = "b";
    ledger::total_expr  = "B";
  }

  value_t option_price(call_scope_t& args) { // I
    ledger::amount_expr = "i";
    ledger::total_expr  = "I";
  }

  value_t option_market(call_scope_t& args) { // V
    report->show_revalued = true;

    ledger::amount_expr = "v";
    ledger::total_expr  = "V";
  }

#if 0
  namespace {
    void parse_price_setting(const char * optarg)
    {
      char * equals = std::strchr(optarg, '=');
      if (! equals)
	return;

      while (std::isspace(*optarg))
	optarg++;
      while (equals > optarg && std::isspace(*(equals - 1)))
	equals--;

      std::string symbol(optarg, 0, equals - optarg);
      amount_t price(equals + 1);

      if (commodity_t * commodity = commodity_t::find_or_create(symbol)) {
	commodity->add_price(datetime_t::now, price);
	commodity->history()->bogus_time = datetime_t::now;
      }
    }
  }
#endif
#endif

  //
  // Scope members
  //

  virtual expr_t::ptr_op_t lookup(const string& name);
};

} // namespace ledger

#endif // _REPORT_H
