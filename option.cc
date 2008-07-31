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

#include "option.h"

namespace ledger {

namespace {
  typedef tuple<expr_t::ptr_op_t, bool> op_bool_tuple;

  op_bool_tuple find_option(scope_t& scope, const string& name)
  {
    char buf[128];
    std::strcpy(buf, "option_");
    char * p = &buf[7];
    foreach (char ch, name) {
      if (ch == '-')
	*p++ = '_';
      else
	*p++ = ch;
    }
    *p = '\0';

    expr_t::ptr_op_t op = scope.lookup(buf);
    if (op)
      return op_bool_tuple(op, false);

    *p++ = '_';
    *p = '\0';

    return op_bool_tuple(scope.lookup(buf), true);
  }

  op_bool_tuple find_option(scope_t& scope, const char letter)
  {
    char buf[10];
    std::strcpy(buf, "option_");
    buf[7] = letter;
    buf[8] = '\0';

    expr_t::ptr_op_t op = scope.lookup(buf);
    if (op)
      return op_bool_tuple(op, false);

    buf[8] = '_';
    buf[9] = '\0';

    return op_bool_tuple(scope.lookup(buf), true);
  }

  void process_option(const function_t& opt, scope_t& scope,
		      const char * arg)
  {
    try {
      call_scope_t args(scope);
      if (arg)
	args.push_back(string_value(arg));

      opt(args);
    }
    catch (const std::exception& err) {
#if 0
      add_error_context("While parsing option '--" << opt->long_opt
			<< "'" << (opt->short_opt != '\0' ?
				   (string(" (-") + opt->short_opt + "):") :
				   ": "));
#endif
      throw err;
    }
  }
}

void process_option(const string& name, scope_t& scope,
		    const char * arg)
{
  op_bool_tuple opt(find_option(scope, name));
  if (opt.get<0>())
    process_option(opt.get<0>()->as_function(), scope, arg);
}

void process_environment(const char ** envp, const string& tag,
			 scope_t& scope)
{
  const char * tag_p   = tag.c_str();
  unsigned int tag_len = tag.length();

  for (const char ** p = envp; *p; p++)
    if (! tag_p || std::strncmp(*p, tag_p, tag_len) == 0) {
      char   buf[128];
      char * r = buf;
      const char * q;
      for (q = *p + tag_len;
	   *q && *q != '=' && r - buf < 128;
	   q++)
	if (*q == '_')
	  *r++ = '-';
	else
	  *r++ = std::tolower(*q);
      *r = '\0';

      if (*q == '=') {
	try {
	  process_option(string(buf), scope, q + 1);
	}
	catch (const std::exception& err) {
	  add_error_context("While parsing environment variable option '"
			    << *p << "':");
	  throw err;
	}
      }
    }
}

void process_arguments(int, char ** argv, const bool anywhere,
		       scope_t& scope, std::list<string>& args)
{
  for (char ** i = argv; *i; i++) {
    if ((*i)[0] != '-') {
      if (anywhere) {
	args.push_back(*i);
	continue;
      } else {
	for (; *i; i++)
	  args.push_back(*i);
	break;
      }
    }

    // --long-option or -s
    if ((*i)[1] == '-') {
      if ((*i)[2] == '\0')
	break;

      char * name  = *i + 2;
      char * value = NULL;
      if (char * p = std::strchr(name, '=')) {
	*p++ = '\0';
	value = p;
      }

      op_bool_tuple opt(find_option(scope, name));
      if (! opt.get<0>())
	throw_(option_error, "illegal option --" << name);

      if (opt.get<1>() && value == NULL) {
	value = *++i;
	if (value == NULL)
	  throw_(option_error, "missing option argument for --" << name);
      }
      process_option(opt.get<0>()->as_function(), scope, value);
    }
    else if ((*i)[1] == '\0') {
      throw_(option_error, "illegal option -");
    }
    else {
      typedef tuple<expr_t::ptr_op_t, bool, char> op_bool_char_tuple;

      std::list<op_bool_char_tuple> option_queue;

      int x = 1;
      for (char c = (*i)[x]; c != '\0'; x++, c = (*i)[x]) {
	op_bool_tuple opt(find_option(scope, c));
	if (! opt.get<0>())
	  throw_(option_error, "illegal option -" << c);

	option_queue.push_back
	  (op_bool_char_tuple(opt.get<0>(), opt.get<1>(), c));
      }

      foreach (op_bool_char_tuple& o, option_queue) {
	char * value = NULL;
	if (o.get<1>()) {
	  value = *++i;
	  if (value == NULL)
	    throw_(option_error,
		   "missing option argument for -" << o.get<2>());
	}
	process_option(o.get<0>()->as_function(), scope, value);
      }
    }
  }
}

} // namespace ledger

#if 0
void option_full_help(std::ostream& out)
{
  out << "usage: ledger [options] COMMAND [ACCT REGEX]... [-- [PAYEE REGEX]...]\n\n\
Basic options:\n\
  -H, --full-help        display this help text\n\
  -h, --help             display summarized help text\n\
  -v, --version          show version information\n\
  -f, --file FILE        read ledger data from FILE\n\
  -o, --output FILE      write output to FILE\n\
  -i, --init-file FILE   initialize ledger using FILE (default: ~/.ledgerrc)\n\
      --cache FILE       use FILE as a binary cache when --file is not used\n\
      --no-cache         don't use a cache, even if it would be appropriate\n\
  -a, --account NAME     use NAME for the default account (useful with QIF)\n\n\
Report filtering:\n\
  -c, --current          show only current and past entries (not future)\n\
  -b, --begin DATE       set report begin date\n\
  -e, --end DATE         set report end date\n\
  -p, --period STR       report using the given period\n\
      --period-sort EXPR sort each report period's entries by EXPR\n\
  -C, --cleared          consider only cleared transactions\n\
  -U, --uncleared        consider only uncleared transactions\n\
  -R, --real             consider only real (non-virtual) transactions\n\
  -L, --actual           consider only actual (non-automated) transactions\n\
  -r, --related          calculate report using related transactions\n\
      --budget           generate budget entries based on periodic entries\n\
      --add-budget       show all transactions plus the budget\n\
      --unbudgeted       show only unbudgeted transactions\n\
      --forecast EXPR    generate forecast entries while EXPR is true\n\
  -l, --limit EXPR       calculate only transactions matching EXPR\n\
  -t, --amount EXPR      use EXPR to calculate the displayed amount\n\
  -T, --total EXPR       use EXPR to calculate the displayed total\n\n\
Output customization:\n\
  -n, --collapse         register: collapse entries; balance: no grand total\n\
  -s, --subtotal         balance: show sub-accounts; other: show subtotals\n\
  -P, --by-payee         show summarized totals by payee\n\
  -x, --comm-as-payee    set commodity name as the payee, for reporting\n\
  -E, --empty            balance: show accounts with zero balance\n\
  -W, --weekly           show weekly sub-totals\n\
  -M, --monthly          show monthly sub-totals\n\
  -Y, --yearly           show yearly sub-totals\n\
      --dow              show a days-of-the-week report\n\
  -S, --sort EXPR        sort report according to the value expression EXPR\n\
  -w, --wide             for the default register report, use 132 columns\n\
      --head COUNT       show only the first COUNT entries (negative inverts)\n\
      --tail COUNT       show only the last COUNT entries (negative inverts)\n\
      --pager PAGER      send all output through the given PAGER program\n\
  -A, --average          report average transaction amount\n\
  -D, --deviation        report deviation from the average\n\
  -%, --percentage       report balance totals as a percentile of the parent\n\
      --totals           in the \"xml\" report, include running total\n\
  -j, --amount-data      print only raw amount data (useful for scripting)\n\
  -J, --total-data       print only raw total data\n\
  -d, --display EXPR     display only transactions matching EXPR\n\
  -y, --date-format STR  use STR as the date format (default: %Y/%m/%d)\n\
  -F, --format STR       use STR as the format; for each report type, use:\n\
      --balance-format      --register-format       --print-format\n\
      --plot-amount-format  --plot-total-format     --equity-format\n\
      --prices-format       --wide-register-format\n\n\
Commodity reporting:\n\
      --price-db FILE    sets the price database to FILE (def: ~/.pricedb)\n\
  -L, --price-exp MINS   download quotes only if newer than MINS (def: 1440)\n\
  -Q, --download         download price information when needed\n\
  -O, --quantity         report commodity totals (this is the default)\n\
  -B, --basis            report cost basis of commodities\n\
  -V, --market           report last known market value\n\
  -g, --performance      report gain/loss for each displayed transaction\n\
  -G, --gain             report net gain/loss\n\n\
Commands:\n\
  balance  [REGEXP]...   show balance totals for matching accounts\n\
  register [REGEXP]...   show register of matching transactions\n\
  print    [REGEXP]...   print all matching entries\n\
  xml      [REGEXP]...   print matching entries in XML format\n\
  equity   [REGEXP]...   output equity entries for matching accounts\n\
  prices   [REGEXP]...   display price history for matching commodities\n\
  entry DATE PAYEE AMT   output a derived entry, based on the arguments\n";
}

void option_help(std::ostream& out)
{
  out << "usage: ledger [options] COMMAND [ACCT REGEX]... [-- [PAYEE REGEX]...]\n\n\
Use -H to see all the help text on one page, or:\n\
      --help-calc        calculation options\n\
      --help-disp        display options\n\
      --help-comm        commodity options\n\n\
Basic options:\n\
  -h, --help             display this help text\n\
  -v, --version          show version information\n\
  -f, --file FILE        read ledger data from FILE\n\
  -o, --output FILE      write output to FILE\n\
  -i, --init-file FILE   initialize ledger using FILE (default: ~/.ledgerrc)\n\
      --cache FILE       use FILE as a binary cache when --file is not used\n\
      --no-cache         don't use a cache, even if it would be appropriate\n\
  -a, --account NAME     use NAME for the default account (useful with QIF)\n\n\
Commands:\n\
  balance  [REGEXP]...   show balance totals for matching accounts\n\
  register [REGEXP]...   show register of matching transactions\n\
  print    [REGEXP]...   print all matching entries\n\
  xml      [REGEXP]...   print matching entries in XML format\n\
  equity   [REGEXP]...   output equity entries for matching accounts\n\
  prices   [REGEXP]...   display price history for matching commodities\n\
  entry DATE PAYEE AMT   output a derived entry, based on the arguments\n";
}

void option_calc_help(std::ostream& out)
{
  out << "Options to control how a report is calculated:\n\
  -c, --current          show only current and past entries (not future)\n\
  -b, --begin DATE       set report begin date\n\
  -e, --end DATE         set report end date\n\
  -p, --period STR       report using the given period\n\
      --period-sort EXPR sort each report period's entries by EXPR\n\
  -C, --cleared          consider only cleared transactions\n\
  -U, --uncleared        consider only uncleared transactions\n\
  -R, --real             consider only real (non-virtual) transactions\n\
  -L, --actual           consider only actual (non-automated) transactions\n\
  -r, --related          calculate report using related transactions\n\
      --budget           generate budget entries based on periodic entries\n\
      --add-budget       show all transactions plus the budget\n\
      --unbudgeted       show only unbudgeted transactions\n\
      --forecast EXPR    generate forecast entries while EXPR is true\n\
  -l, --limit EXPR       calculate only transactions matching EXPR\n\
  -t, --amount EXPR      use EXPR to calculate the displayed amount\n\
  -T, --total EXPR       use EXPR to calculate the displayed total\n";
}

void option_disp_help(std::ostream& out)
{
  out << "Output to control how report results are displayed:\n\
  -n, --collapse         register: collapse entries; balance: no grand total\n\
  -s, --subtotal         balance: show sub-accounts; other: show subtotals\n\
  -P, --by-payee         show summarized totals by payee\n\
  -x, --comm-as-payee    set commodity name as the payee, for reporting\n\
  -E, --empty            balance: show accounts with zero balance\n\
  -W, --weekly           show weekly sub-totals\n\
  -M, --monthly          show monthly sub-totals\n\
  -Y, --yearly           show yearly sub-totals\n\
      --dow              show a days-of-the-week report\n\
  -S, --sort EXPR        sort report according to the value expression EXPR\n\
  -w, --wide             for the default register report, use 132 columns\n\
      --head COUNT       show only the first COUNT entries (negative inverts)\n\
      --tail COUNT       show only the last COUNT entries (negative inverts)\n\
      --pager PAGER      send all output through the given PAGER program\n\
  -A, --average          report average transaction amount\n\
  -D, --deviation        report deviation from the average\n\
  -%, --percentage       report balance totals as a percentile of the parent\n\
      --totals           in the \"xml\" report, include running total\n\
  -j, --amount-data      print only raw amount data (useful for scripting)\n\
  -J, --total-data       print only raw total data\n\
  -d, --display EXPR     display only transactions matching EXPR\n\
  -y, --date-format STR  use STR as the date format (default: %Y/%m/%d)\n\
  -F, --format STR       use STR as the format; for each report type, use:\n\
      --balance-format      --register-format       --print-format\n\
      --plot-amount-format  --plot-total-format     --equity-format\n\
      --prices-format       --wide-register-format\n";
}

void option_comm_help(std::ostream& out)
{
  out << "Options to control how commodity values are determined:\n\
      --price-db FILE    sets the price database to FILE (def: ~/.pricedb)\n\
  -Z, --price-exp MINS   download quotes only if newer than MINS (def: 1440)\n\
  -Q, --download         download price information when needed\n\
  -O, --quantity         report commodity totals (this is the default)\n\
  -B, --basis            report cost basis of commodities\n\
  -V, --market           report last known market value\n\
  -g, --performance      report gain/loss for each displayed transaction\n\
  -G, --gain             report net gain/loss\n";
}

//////////////////////////////////////////////////////////////////////
//
// Basic options

OPT_BEGIN(full_help, "H") {
  option_full_help(std::cout);
  throw 0;
} OPT_END(full_help);

OPT_BEGIN(help, "h") {
  option_help(std::cout);
  throw 0;
} OPT_END(help);

OPT_BEGIN(help_calc, "") {
  option_calc_help(std::cout);
  throw 0;
} OPT_END(help_calc);

OPT_BEGIN(help_disp, "") {
  option_disp_help(std::cout);
  throw 0;
} OPT_END(help_disp);

OPT_BEGIN(help_comm, "") {
  option_comm_help(std::cout);
  throw 0;
} OPT_END(help_comm);

OPT_BEGIN(version, "v") {
  show_version(std::cout);
  throw 0;
} OPT_END(version);

OPT_BEGIN(init_file, "i:") {
  std::string path = resolve_path(optarg);
  if (access(path.c_str(), R_OK) != -1)
    config->init_file = path;
  else
    throw_(std::invalid_argument,
	   "The init file '" << path << "' does not exist or is not readable");
} OPT_END(init_file);

OPT_BEGIN(file, "f:") {
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
} OPT_END(file);

OPT_BEGIN(cache, ":") {
  config->cache_file = resolve_path(optarg);
} OPT_END(cache);

OPT_BEGIN(no_cache, "") {
  config->cache_file = "<none>";
} OPT_END(no_cache);

OPT_BEGIN(output, "o:") {
  if (std::string(optarg) != "-") {
    std::string path = resolve_path(optarg);
    report->output_file = path;
  }
} OPT_END(output);

OPT_BEGIN(account, "a:") {
  config->account = optarg;
} OPT_END(account);

OPT_BEGIN(debug, ":") {
  config->debug_mode = true;
  ::setenv("DEBUG_CLASS", optarg, 1);
} OPT_END(debug);

OPT_BEGIN(verbose, "") {
  config->verbose_mode = true;
} OPT_END(verbose);

OPT_BEGIN(trace, "") {
  config->trace_mode = true;
} OPT_END(trace);

//////////////////////////////////////////////////////////////////////
//
// Report filtering

OPT_BEGIN(effective, "") {
  xact_t::use_effective_date = true;
} OPT_END(effective);

OPT_BEGIN(begin, "b:") {
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
} OPT_END(begin);

OPT_BEGIN(end, "e:") {
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
} OPT_END(end);

OPT_BEGIN(current, "c") {
  if (! report->predicate.empty())
    report->predicate += "&";
  report->predicate += "d<=m";
} OPT_END(current);

OPT_BEGIN(cleared, "C") {
  if (! report->predicate.empty())
    report->predicate += "&";
  report->predicate += "X";
} OPT_END(cleared);

OPT_BEGIN(uncleared, "U") {
  if (! report->predicate.empty())
    report->predicate += "&";
  report->predicate += "!X";
} OPT_END(uncleared);

OPT_BEGIN(real, "R") {
  if (! report->predicate.empty())
    report->predicate += "&";
  report->predicate += "R";
} OPT_END(real);

OPT_BEGIN(actual, "L") {
  if (! report->predicate.empty())
    report->predicate += "&";
  report->predicate += "L";
} OPT_END(actual);

OPT_BEGIN(lots, "") {
  report->keep_price =
  report->keep_date  =
  report->keep_tag   = true;
} OPT_END(lots);

OPT_BEGIN(lot_prices, "") {
  report->keep_price = true;
} OPT_END(lots_prices);

OPT_BEGIN(lot_dates, "") {
  report->keep_date = true;
} OPT_END(lots_dates);

OPT_BEGIN(lot_tags, "") {
  report->keep_tag = true;
} OPT_END(lots_tags);

//////////////////////////////////////////////////////////////////////
//
// Output customization

OPT_BEGIN(format, "F:") {
  report->format_string = optarg;
} OPT_END(format);

OPT_BEGIN(date_format, "y:") {
  report->date_output_format = optarg;
} OPT_END(date_format);

OPT_BEGIN(input_date_format, ":") {
  config->date_input_format = optarg;
} OPT_END(input_date_format);

OPT_BEGIN(balance_format, ":") {
  config->balance_format = optarg;
} OPT_END(balance_format);

OPT_BEGIN(register_format, ":") {
  config->register_format = optarg;
} OPT_END(register_format);

OPT_BEGIN(wide_register_format, ":") {
  config->wide_register_format = optarg;
} OPT_END(wide_register_format);

OPT_BEGIN(plot_amount_format, ":") {
  config->plot_amount_format = optarg;
} OPT_END(plot_amount_format);

OPT_BEGIN(plot_total_format, ":") {
  config->plot_total_format = optarg;
} OPT_END(plot_total_format);

OPT_BEGIN(print_format, ":") {
  config->print_format = optarg;
} OPT_END(print_format);

OPT_BEGIN(write_hdr_format, ":") {
  config->write_hdr_format = optarg;
} OPT_END(write_hdr_format);

OPT_BEGIN(write_xact_format, ":") {
  config->write_xact_format = optarg;
} OPT_END(write_xact_format);

OPT_BEGIN(equity_format, ":") {
  config->equity_format = optarg;
} OPT_END(equity_format);

OPT_BEGIN(prices_format, ":") {
  config->prices_format = optarg;
} OPT_END(prices_format);

OPT_BEGIN(wide, "w") {
  config->register_format = config->wide_register_format;
} OPT_END(wide);

OPT_BEGIN(head, ":") {
  report->head_entries = std::atoi(optarg);
} OPT_END(head);

OPT_BEGIN(tail, ":") {
  report->tail_entries = std::atoi(optarg);
} OPT_END(tail);

OPT_BEGIN(pager, ":") {
  config->pager = optarg;
} OPT_END(pager);

OPT_BEGIN(truncate, ":") {
  std::string style(optarg);
  if (style == "leading")
    format_t::elision_style = format_t::TRUNCATE_LEADING;
  else if (style == "middle")
    format_t::elision_style = format_t::TRUNCATE_MIDDLE;
  else if (style == "trailing")
    format_t::elision_style = format_t::TRUNCATE_TRAILING;
  else if (style == "abbrev")
    format_t::elision_style = format_t::ABBREVIATE;
} OPT_END(truncate);

OPT_BEGIN(abbrev_len, ":") {
  format_t::abbrev_length = std::atoi(optarg);
} OPT_END(abbrev_len);

OPT_BEGIN(empty, "E") {
  report->show_empty = true;
} OPT_END(empty);

OPT_BEGIN(collapse, "n") {
  report->show_collapsed = true;
} OPT_END(collapse);

OPT_BEGIN(subtotal, "s") {
  report->show_subtotal = true;
} OPT_END(subtotal);

OPT_BEGIN(totals, "") {
  report->show_totals = true;
} OPT_END(totals);

OPT_BEGIN(sort, "S:") {
  report->sort_string = optarg;
} OPT_END(sort);

OPT_BEGIN(sort_entries, "") {
  report->sort_string = optarg;
  report->entry_sort = true;
} OPT_END(sort_entries);

OPT_BEGIN(sort_all, "") {
  report->sort_string = optarg;
  report->entry_sort = false;
  report->sort_all   = true;
} OPT_END(sort_all);

OPT_BEGIN(period_sort, ":") {
  report->sort_string = optarg;
  report->entry_sort = true;
} OPT_END(period_sort);

OPT_BEGIN(related, "r") {
  report->show_related = true;
} OPT_END(related);

OPT_BEGIN(descend, "") {
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
} OPT_END(descend);

OPT_BEGIN(descend_if, "") {
  report->descend_expr = optarg;
} OPT_END(descend_if);

OPT_BEGIN(period, "p:") {
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
} OPT_END(period);

OPT_BEGIN(daily, "") {
  if (report->report_period.empty())
    report->report_period = "daily";
  else
    report->report_period = std::string("daily ") + report->report_period;
} OPT_END(daily);

OPT_BEGIN(weekly, "W") {
  if (report->report_period.empty())
    report->report_period = "weekly";
  else
    report->report_period = std::string("weekly ") + report->report_period;
} OPT_END(weekly);

OPT_BEGIN(monthly, "M") {
  if (report->report_period.empty())
    report->report_period = "monthly";
  else
    report->report_period = std::string("monthly ") + report->report_period;
} OPT_END(monthly);

OPT_BEGIN(quarterly, "") {
  if (report->report_period.empty())
    report->report_period = "quarterly";
  else
    report->report_period = std::string("quarterly ") + report->report_period;
} OPT_END(quarterly);

OPT_BEGIN(yearly, "Y") {
  if (report->report_period.empty())
    report->report_period = "yearly";
  else
    report->report_period = std::string("yearly ") + report->report_period;
} OPT_END(yearly);

OPT_BEGIN(dow, "") {
  report->days_of_the_week = true;
} OPT_END(dow);

OPT_BEGIN(by_payee, "P") {
  report->by_payee = true;
} OPT_END(by_payee);

OPT_BEGIN(comm_as_payee, "x") {
  report->comm_as_payee = true;
} OPT_END(comm_as_payee);

OPT_BEGIN(code_as_payee, "") {
  report->code_as_payee = true;
} OPT_END(code_as_payee);

OPT_BEGIN(budget, "") {
  report->budget_flags = BUDGET_BUDGETED;
} OPT_END(budget);

OPT_BEGIN(add_budget, "") {
  report->budget_flags = BUDGET_BUDGETED | BUDGET_UNBUDGETED;
} OPT_END(add_budget);

OPT_BEGIN(unbudgeted, "") {
  report->budget_flags = BUDGET_UNBUDGETED;
} OPT_END(unbudgeted);

OPT_BEGIN(forecast, ":") {
  report->forecast_limit = optarg;
} OPT_END(forecast);

OPT_BEGIN(reconcile, ":") {
  report->reconcile_balance = optarg;
} OPT_END(reconcile);

OPT_BEGIN(reconcile_date, ":") {
  report->reconcile_date = optarg;
} OPT_END(reconcile_date);

OPT_BEGIN(limit, "l:") {
  if (! report->predicate.empty())
    report->predicate += "&";
  report->predicate += "(";
  report->predicate += optarg;
  report->predicate += ")";
} OPT_END(limit);

OPT_BEGIN(only, ":") {
  if (! report->secondary_predicate.empty())
    report->secondary_predicate += "&";
  report->secondary_predicate += "(";
  report->secondary_predicate += optarg;
  report->secondary_predicate += ")";
} OPT_END(only);

OPT_BEGIN(display, "d:") {
  if (! report->display_predicate.empty())
    report->display_predicate += "&";
  report->display_predicate += "(";
  report->display_predicate += optarg;
  report->display_predicate += ")";
} OPT_END(display);

OPT_BEGIN(amount, "t:") {
  ledger::amount_expr = optarg;
} OPT_END(amount);

OPT_BEGIN(total, "T:") {
  ledger::total_expr = optarg;
} OPT_END(total);

OPT_BEGIN(amount_data, "j") {
  report->format_string = config->plot_amount_format;
} OPT_END(amount_data);

OPT_BEGIN(total_data, "J") {
  report->format_string = config->plot_total_format;
} OPT_END(total_data);

OPT_BEGIN(ansi, "") {
  format_t::ansi_codes  = true;
  format_t::ansi_invert = false;
} OPT_END(ansi);

OPT_BEGIN(ansi_invert, "") {
  format_t::ansi_codes  =
  format_t::ansi_invert = true;
} OPT_END(ansi);

//////////////////////////////////////////////////////////////////////
//
// Commodity reporting

OPT_BEGIN(base, ":") {
  amount_t::keep_base = true;
} OPT_END(base);

OPT_BEGIN(price_db, ":") {
  config->price_db = optarg;
} OPT_END(price_db);

OPT_BEGIN(price_exp, "Z:") {
  config->pricing_leeway = std::atol(optarg) * 60;
} OPT_END(price_exp);

OPT_BEGIN(download, "Q") {
  config->download_quotes = true;
} OPT_END(download);

OPT_BEGIN(quantity, "O") {
  ledger::amount_expr = "a";
  ledger::total_expr  = "O";
} OPT_END(quantity);

OPT_BEGIN(basis, "B") {
  ledger::amount_expr = "b";
  ledger::total_expr  = "B";
} OPT_END(basis);

OPT_BEGIN(price, "I") {
  ledger::amount_expr = "i";
  ledger::total_expr  = "I";
} OPT_END(price);

OPT_BEGIN(market, "V") {
  report->show_revalued = true;

  ledger::amount_expr = "v";
  ledger::total_expr  = "V";
} OPT_END(market);

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
