#include "option.h"
#include "config.h"
#include "report.h"
#include "debug.h"
#include "error.h"

#include <iostream>
#include <cstdarg>
#include <cstdlib>
#include <unistd.h>

#include "util.h"

namespace {
  inline void process_option(option_t * opt, const char * arg = NULL) {
    try {
      opt->handler(arg);
    }
    catch (error * err) {
      err->context.push_back
	(new error_context
	 (std::string("While parsing option '--") + opt->long_opt +
	  "'" + (opt->short_opt != '\0' ?
		 (std::string(" (-") + opt->short_opt + "):") : ":")));
      throw err;
    }
  }

  option_t * search_options(option_t * array, const char * name)
  {
    int first = 0;
    int last  = CONFIG_OPTIONS_SIZE;
    while (first <= last) {
      int mid = (first + last) / 2; // compute mid point.

      int result;
      if ((result = (int)name[0] - (int)array[mid].long_opt[0]) == 0)
	result = std::strcmp(name, array[mid].long_opt);

      if (result > 0)
	first = mid + 1;		// repeat search in top half.
      else if (result < 0)
	last = mid - 1;		// repeat search in bottom half.
      else
	return &array[mid];
    }
    return NULL;
  }

  option_t * search_options(option_t * array, const char letter)
  {
    for (int i = 0; i < CONFIG_OPTIONS_SIZE; i++)
      if (letter == array[i].short_opt)
	return &array[i];
    return NULL;
  }
}

bool process_option(option_t * options, const std::string& name,
		    const char * arg)
{
  option_t * opt = search_options(options, name.c_str());
  if (opt) {
    process_option(opt, arg);
    return true;
  }
  return false;
}

void process_arguments(option_t * options, int argc, char ** argv,
		       const bool anywhere, std::list<std::string>& args)
{
  int index = 0;
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
   again:
    if ((*i)[1] == '-') {
      if ((*i)[2] == '\0')
	break;

      char * name  = *i + 2;
      char * value = NULL;
      if (char * p = std::strchr(name, '=')) {
	*p++ = '\0';
	value = p;
      }

      option_t * opt = search_options(options, name);
      if (! opt)
	throw new option_error(std::string("illegal option --") + name);

      if (opt->wants_arg && value == NULL) {
	value = *++i;
	if (value == NULL)
	  throw new option_error(std::string("missing option argument for --") +
				 name);
      }
      process_option(opt, value);
    }
    else if ((*i)[1] == '\0') {
      throw new option_error(std::string("illegal option -"));
    }
    else {
      std::list<option_t *> opt_queue;

      int x = 1;
      for (char c = (*i)[x]; c != '\0'; x++, c = (*i)[x]) {
	option_t * opt = search_options(options, c);
	if (! opt)
	  throw new option_error(std::string("illegal option -") + c);
	opt_queue.push_back(opt);
      }

      for (std::list<option_t *>::iterator o = opt_queue.begin();
	   o != opt_queue.end(); o++) {
	char * value = NULL;
	if ((*o)->wants_arg) {
	  value = *++i;
	  if (value == NULL)
	    throw new option_error(std::string("missing option argument for -") +
				   (*o)->short_opt);
	}
	process_option(*o, value);
      }
    }

   next:
    ;
  }
}

void process_environment(option_t * options, const char ** envp,
			 const std::string& tag)
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
	  process_option(options, buf, q + 1);
	}
	catch (error * err) {
	  err->context.pop_back();
	  err->context.push_back
	    (new error_context
	     (std::string("While parsing environment variable option '") +
	      *p + "':"));
	  throw err;
	}
      }
    }
}

//////////////////////////////////////////////////////////////////////

namespace ledger {

config_t * config = NULL;
report_t * report = NULL;

static void show_version(std::ostream& out)
{
  out << "Ledger " << ledger::version << ", the command-line accounting tool";
  out << "\n\nCopyright (c) 2003-2009, John Wiegley.  All rights reserved.\n\n\
This program is made available under the terms of the BSD Public License.\n\
See LICENSE file included with the distribution for details and disclaimer.\n";
  out << "\n(modules: gmp, pcre";
#if defined(HAVE_EXPAT) || defined(HAVE_XMLPARSE)
  out << ", xml";
#endif
#ifdef HAVE_LIBOFX
  out << ", ofx";
#endif
  out << ")\n";
}

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
    throw new error(std::string("The init file '") + path +
		    "' does not exist or is not readable");
} OPT_END(init_file);

OPT_BEGIN(file, "f:") {
  if (std::string(optarg) == "-") {
    config->data_file = optarg;
  } else {
    std::string path = resolve_path(optarg);
    if (access(path.c_str(), R_OK) != -1)
      config->data_file = path;
    else
      throw new error(std::string("The ledger file '") + path +
		      "' does not exist or is not readable");
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
  transaction_t::use_effective_date = true;
} OPT_END(effective);

OPT_BEGIN(begin, "b:") {
  char buf[128];
  interval_t interval(optarg);
  if (! interval.begin)
    throw new error(std::string("Could not determine beginning of period '") +
		    optarg + "'");

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
    throw new error(std::string("Could not determine end of period '") +
		    optarg + "'");

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
  report->show_revalued = false;
  ledger::amount_expr = "@a";
  ledger::total_expr  = "@O";
} OPT_END(quantity);

OPT_BEGIN(basis, "B") {
  report->show_revalued = false;
  ledger::amount_expr = "@b";
  ledger::total_expr  = "@B";
} OPT_END(basis);

OPT_BEGIN(price, "I") {
  report->show_revalued = false;
  ledger::amount_expr = "@i";
  ledger::total_expr  = "@I";
} OPT_END(price);

OPT_BEGIN(market, "V") {
  report->show_revalued = true;
  ledger::amount_expr = "@v";
  ledger::total_expr  = "@V";
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

OPT_BEGIN(set_price, ":") {
  std::string arg(optarg);
  std::string::size_type beg = 0;
  for (std::string::size_type pos = arg.find(';');
       pos != std::string::npos;
       beg = pos + 1, pos = arg.find(';', beg))
    parse_price_setting(std::string(arg, beg, pos - beg).c_str());
  parse_price_setting(std::string(arg, beg).c_str());
} OPT_END(set_price);

OPT_BEGIN(performance, "g") {
  ledger::amount_expr = "@P(@a,@m)-@b";
  ledger::total_expr  = "@P(@O,@m)-@B";
} OPT_END(performance);

OPT_BEGIN(gain, "G") {
  report->show_revalued      =
  report->show_revalued_only = true;

  ledger::amount_expr = "@a";
  ledger::total_expr  = "@G";
} OPT_END(gain);

static std::string expand_value_expr(const std::string& tmpl,
				     const std::string& expr)
{
  std::string xp = tmpl;
  for (std::string::size_type i = xp.find('#');
       i != std::string::npos;
       i = xp.find('#'))
    xp = (std::string(xp, 0, i) + "(" + expr + ")" +
	  std::string(xp, i + 1));
  return xp;
}

OPT_BEGIN(average, "A") {
  ledger::total_expr = expand_value_expr("@A(#)", ledger::total_expr.expr);
} OPT_END(average);

OPT_BEGIN(deviation, "D") {
  ledger::total_expr = expand_value_expr("@t-@A(#)", ledger::total_expr.expr);
} OPT_END(deviation);

OPT_BEGIN(percentage, "%") {
  ledger::total_expr = expand_value_expr("^#&{100.0%}*(#/^#)",
					 ledger::total_expr.expr);
} OPT_END(percentage);

//////////////////////////////////////////////////////////////////////

option_t config_options[CONFIG_OPTIONS_SIZE] = {
  { "abbrev-len",	    '\0', true,	 opt_abbrev_len },
  { "account",		    'a',  true,	 opt_account },
  { "actual",		    'L',  false, opt_actual },
  { "add-budget",	    '\0', false, opt_add_budget },
  { "amount",		    't',  true,	 opt_amount },
  { "amount-data",	    'j',  false, opt_amount_data },
  { "ansi",		    '\0', false, opt_ansi },
  { "ansi-invert",	    '\0', false, opt_ansi_invert },
  { "average",		    'A',  false, opt_average },
  { "balance-format",	    '\0', true,	 opt_balance_format },
  { "base",		    '\0', false, opt_base },
  { "basis",		    'B',  false, opt_basis },
  { "begin",		    'b',  true,	 opt_begin },
  { "budget",		    '\0', false, opt_budget },
  { "by-payee",		    'P',  false, opt_by_payee },
  { "cache",		    '\0', true,	 opt_cache },
  { "cleared",		    'C',  false, opt_cleared },
  { "code-as-payee",	    '\0', false, opt_code_as_payee },
  { "collapse",		    'n',  false, opt_collapse },
  { "comm-as-payee",	    'x',  false, opt_comm_as_payee },
  { "cost",		    '\0', false, opt_basis },
  { "current",		    'c',  false, opt_current },
  { "daily",		    '\0', false, opt_daily },
  { "date-format",	    'y',  true,	 opt_date_format },
  { "debug",		    '\0', true,	 opt_debug },
  { "descend",		    '\0', true,	 opt_descend },
  { "descend-if",	    '\0', true,	 opt_descend_if },
  { "deviation",	    'D',  false, opt_deviation },
  { "display",		    'd',  true,	 opt_display },
  { "dow",		    '\0', false, opt_dow },
  { "download",		    'Q',  false, opt_download },
  { "effective",	    '\0', false, opt_effective },
  { "empty",		    'E',  false, opt_empty },
  { "end",		    'e',  true,	 opt_end },
  { "equity-format",	    '\0', true,	 opt_equity_format },
  { "file",		    'f',  true,	 opt_file },
  { "forecast",		    '\0', true,	 opt_forecast },
  { "format",		    'F',  true,	 opt_format },
  { "full-help",	    'H',  false, opt_full_help },
  { "gain",		    'G',  false, opt_gain },
  { "head",		    '\0', true,	 opt_head },
  { "help",		    'h',  false, opt_help },
  { "help-calc",	    '\0', false, opt_help_calc },
  { "help-comm",	    '\0', false, opt_help_comm },
  { "help-disp",	    '\0', false, opt_help_disp },
  { "init-file",	    'i',  true,	 opt_init_file },
  { "input-date-format",    '\0', true,	 opt_input_date_format },
  { "limit",		    'l',  true,	 opt_limit },
  { "lot-dates",	    '\0', false, opt_lot_dates },
  { "lot-prices",	    '\0', false, opt_lot_prices },
  { "lot-tags",		    '\0', false, opt_lot_tags },
  { "lots",		    '\0', false, opt_lots },
  { "market",		    'V',  false, opt_market },
  { "monthly",		    'M',  false, opt_monthly },
  { "no-cache",		    '\0', false, opt_no_cache },
  { "only",		    '\0', true,	 opt_only },
  { "output",		    'o',  true,	 opt_output },
  { "pager",		    '\0', true,	 opt_pager },
  { "percentage",	    '%',  false, opt_percentage },
  { "performance",	    'g',  false, opt_performance },
  { "period",		    'p',  true,	 opt_period },
  { "period-sort",	    '\0', true,	 opt_period_sort },
  { "plot-amount-format",   '\0', true,	 opt_plot_amount_format },
  { "plot-total-format",    '\0', true,	 opt_plot_total_format },
  { "price",		    'I',  false, opt_price },
  { "price-db",		    '\0', true,	 opt_price_db },
  { "price-exp",	    'Z',  true,	 opt_price_exp },
  { "prices-format",	    '\0', true,	 opt_prices_format },
  { "print-format",	    '\0', true,	 opt_print_format },
  { "quantity",		    'O',  false, opt_quantity },
  { "quarterly",	    '\0', false, opt_quarterly },
  { "real",		    'R',  false, opt_real },
  { "reconcile",	    '\0', true,	 opt_reconcile },
  { "reconcile-date",	    '\0', true,	 opt_reconcile_date },
  { "register-format",	    '\0', true,	 opt_register_format },
  { "related",		    'r',  false, opt_related },
  { "set-price",	    '\0', true,	 opt_set_price },
  { "sort",		    'S',  true,	 opt_sort },
  { "sort-all",		    '\0', true,	 opt_sort_all },
  { "sort-entries",	    '\0', true,	 opt_sort_entries },
  { "subtotal",		    's',  false, opt_subtotal },
  { "tail",		    '\0', true,	 opt_tail },
  { "total",		    'T',  true,	 opt_total },
  { "total-data",	    'J',  false, opt_total_data },
  { "totals",		    '\0', false, opt_totals },
  { "trace",		    '\0', false, opt_trace },
  { "truncate",		    '\0', true,	 opt_truncate },
  { "unbudgeted",	    '\0', false, opt_unbudgeted },
  { "uncleared",	    'U',  false, opt_uncleared },
  { "verbose",		    '\0', false, opt_verbose },
  { "version",		    'v',  false, opt_version },
  { "weekly",		    'W',  false, opt_weekly },
  { "wide",		    'w',  false, opt_wide },
  { "wide-register-format", '\0', true,	 opt_wide_register_format },
  { "write-hdr-format",	    '\0', true,	 opt_write_hdr_format },
  { "write-xact-format",    '\0', true,	 opt_write_xact_format },
  { "yearly",		    'Y',  false, opt_yearly },
};

} // namespace ledger
