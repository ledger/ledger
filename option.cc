#include "option.h"
#include "config.h"
#include "report.h"
#include "debug.h"
#include "error.h"
#ifdef USE_BOOST_PYTHON
#include "py_eval.h"
#endif

#include <iostream>
#include <cstdarg>

#include "util.h"

#ifdef USE_BOOST_PYTHON
option_handler_t * find_option_handler(const std::string& name);
#endif

namespace {
  void process_option(option_handler_t * opt,
		      option_handler_t::option_source_t source,
		      const char * arg = NULL)
  {
    if (opt->check(source)) {
      try {
	opt->run(arg);
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
  }

  option_handler_t * search_options(static_option_t * array, const char * name)
  {
    int first = 0;
    int last  = OPTIONS_SIZE;
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
	return array[mid].handler;
    }

#ifdef USE_BOOST_PYTHON
    return find_option_handler(name);
#else
    return NULL;
#endif
  }

  option_handler_t * search_options(static_option_t * array, const char letter)
  {
    for (int i = 0; i < OPTIONS_SIZE; i++)
      if (letter == array[i].short_opt)
	return array[i].handler;
    return NULL;
  }
}

bool option_handler_t::check(option_source_t source)
{
  if (! handled) {
    handled |= (unsigned short)source;
    return true;
  }
  return false;
}

bool process_option(static_option_t * options,
		    option_handler_t::option_source_t source,
		    const std::string& name, const char * arg)
{
  if (option_handler_t * opt = search_options(options, name.c_str())) {
    process_option(opt, source, arg);
    return true;
  }
  return false;
}

void process_arguments(static_option_t * options, int argc, char ** argv,
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

      option_handler_t * opt = search_options(options, name);
      if (! opt)
	throw new option_error(std::string("illegal option --") + name);

      if (opt->wants_arg && value == NULL) {
	value = *++i;
	if (value == NULL)
	  throw new option_error(std::string("missing option argument for --") +
				 name);
      }
      process_option(opt, option_handler_t::COMMAND_LINE, value);
    }
    else if ((*i)[1] == '\0') {
      throw new option_error(std::string("illegal option -"));
    }
    else {
      std::list<option_handler_t *> opt_queue;

      int x = 1;
      for (char c = (*i)[x]; c != '\0'; x++, c = (*i)[x]) {
	option_handler_t * opt = search_options(options, c);
	if (! opt)
	  throw new option_error(std::string("illegal option -") + c);
	opt_queue.push_back(opt);
      }

      for (std::list<option_handler_t *>::iterator o = opt_queue.begin();
	   o != opt_queue.end();
	   o++) {
	char * value = NULL;
	if ((*o)->wants_arg) {
	  value = *++i;
	  if (value == NULL)
	    throw new option_error(std::string("missing option argument for -") +
				   (*o)->short_opt);
	}
	process_option(*o, option_handler_t::COMMAND_LINE, value);
      }
    }

   next:
    ;
  }
}

void process_environment(static_option_t * options, const char ** envp,
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
	  process_option(options, option_handler_t::ENVIRONMENT, buf, q + 1);
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
  out << "\n\nCopyright (c) 2003-2006, John Wiegley.  All rights reserved.\n\n\
This program is made available under the terms of the BSD Public License.\n\
See LICENSE file included with the distribution for details and disclaimer.\n";
  out << "\n(modules: gmp, pcre";
#ifdef USE_BOOST_PYTHON
  out << ", python";
#endif
#if defined(HAVE_EXPAT) || defined(HAVE_XMLPARSE)
  out << ", xml";
#endif
#ifdef HAVE_LIBOFX
  out << ", ofx";
#endif
  out << ")\n";
}

void full_help(std::ostream& out)
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
      --budget           generate budget entries based on FILE\n\
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

void help(std::ostream& out)
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

void calc_help(std::ostream& out)
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
      --budget           generate budget entries based on FILE\n\
      --add-budget       show all transactions plus the budget\n\
      --unbudgeted       show only unbudgeted transactions\n\
      --forecast EXPR    generate forecast entries while EXPR is true\n\
  -l, --limit EXPR       calculate only transactions matching EXPR\n\
  -t, --amount EXPR      use EXPR to calculate the displayed amount\n\
  -T, --total EXPR       use EXPR to calculate the displayed total\n";
}

void disp_help(std::ostream& out)
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

void comm_help(std::ostream& out)
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
  full_help(std::cout);
  throw 0;
} OPT_END(full_help);

OPT_BEGIN(help, "h") {
  help(std::cout);
  throw 0;
} OPT_END(help);

OPT_BEGIN(help_calc, "") {
  calc_help(std::cout);
  throw 0;
} OPT_END(help_calc);

OPT_BEGIN(help_disp, "") {
  disp_help(std::cout);
  throw 0;
} OPT_END(help_disp);

OPT_BEGIN(help_comm, "") {
  comm_help(std::cout);
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
  if (! interval.end)
    throw new error(std::string("Could not determine end of period '") +
		    optarg + "'");

  if (! report->predicate.empty())
    report->predicate += "&";
  report->predicate += "d<[";
  report->predicate += interval.end.to_string();
  report->predicate += "]";

  terminus = interval.end;
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
  ledger::amount_expr = "@a";
  ledger::total_expr  = "@O";
} OPT_END(quantity);

OPT_BEGIN(basis, "B") {
  ledger::amount_expr = "@b";
  ledger::total_expr  = "@B";
} OPT_END(basis);

OPT_BEGIN(price, "I") {
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
//
// Transforms

DEFR_OPT(pre_split, "pre-split")
  pre_transform_queue.push_back(new split_transform); END_DEFR()
DEFR_OPT(split, "split")
  transform_queue.push_back(new split_transform); END_DEFR()
DEFR_OPT(post_split, "post-split")
  post_transform_queue.push_back(new split_transform); END_DEFR()

//////////////////////////////////////////////////////////////////////
//
// Python support

#ifdef USE_BOOST_PYTHON

DEF_OPT_(import, "import")
  virtual bool check(option_source_t source) {
    // Allow any number of modules to be imported, from any source
    return true;
  }
  virtual void run(const char * optarg) {
    python_import(optarg);
  }
END_DEF()

DEFR_OPT(import_stdin, "import-stdin")
  python_eval(std::cin, PY_EVAL_MULTI);
END_DEFR()

#endif

//////////////////////////////////////////////////////////////////////

static_option_t options[OPTIONS_SIZE] = {
  { "abbrev-len", '\0',
    new option_abbrev_len("abbrev-len", '\0', true) },
  { "account", 'a',
    new option_account("account", 'a', true) },
  { "actual", 'L',
    new option_actual("actual", 'L', false) },
  { "add-budget", '\0',
    new option_add_budget("add-budget", '\0', false) },
  { "amount", 't',
    new option_amount("amount", 't', true) },
  { "amount-data", 'j',
    new option_amount_data("amount-data", 'j', false) },
  { "ansi", '\0',
    new option_ansi("ansi", '\0', false) },
  { "ansi-invert", '\0',
    new option_ansi_invert("ansi-invert", '\0', false) },
  { "average", 'A',
    new option_average("average", 'A', false) },
  { "balance-format", '\0',
    new option_balance_format("balance-format", '\0', true) },
  { "base", '\0',
    new option_base("base", '\0', false) },
  { "basis", 'B',
    new option_basis("basis", 'B', false) },
  { "begin", 'b',
    new option_begin("begin", 'b', true) },
  { "budget", '\0',
    new option_budget("budget", '\0', false) },
  { "by-payee", 'P',
    new option_by_payee("by-payee", 'P', false) },
  { "cache", '\0',
    new option_cache("cache", '\0', true) },
  { "cleared", 'C',
    new option_cleared("cleared", 'C', false) },
  { "code-as-payee", '\0',
    new option_code_as_payee("code-as-payee", '\0', false) },
  { "collapse", 'n',
    new option_collapse("collapse", 'n', false) },
  { "comm-as-payee", 'x',
    new option_comm_as_payee("comm-as-payee", 'x', false) },
  { "cost", '\0',
    new option_basis("cost", '\0', false) },
  { "current", 'c',
    new option_current("current", 'c', false) },
  { "daily", '\0',
    new option_daily("daily", '\0', false) },
  { "date-format", 'y',
    new option_date_format("date-format", 'y', true) },
  { "debug", '\0',
    new option_debug("debug", '\0', true) },
  { "descend", '\0',
    new option_descend("descend", '\0', true) },
  { "descend-if", '\0',
    new option_descend_if("descend-if", '\0', true) },
  { "deviation", 'D',
    new option_deviation("deviation", 'D', false) },
  { "display", 'd',
    new option_display("display", 'd', true) },
  { "dow", '\0',
    new option_dow("dow", '\0', false) },
  { "download", 'Q',
    new option_download("download", 'Q', false) },
  { "effective", '\0',
    new option_effective("effective", '\0', false) },
  { "empty", 'E',
    new option_empty("empty", 'E', false) },
  { "end", 'e',
    new option_end("end", 'e', true) },
  { "equity-format", '\0',
    new option_equity_format("equity-format", '\0', true) },
  { "file", 'f',
    new option_file("file", 'f', true) },
  { "forecast", '\0',
    new option_forecast("forecast", '\0', true) },
  { "format", 'F',
    new option_format("format", 'F', true) },
  { "full-help", 'H',
    new option_full_help("full-help", 'H', false) },
  { "gain", 'G',
    new option_gain("gain", 'G', false) },
  { "head", '\0',
    new option_head("head", '\0', true) },
  { "help", 'h',
    new option_help("help", 'h', false) },
  { "help-calc", '\0',
    new option_help_calc("help-calc", '\0', false) },
  { "help-comm", '\0',
    new option_help_comm("help-comm", '\0', false) },
  { "help-disp", '\0',
    new option_help_disp("help-disp", '\0', false) },
  { "import",       '\0', new option_import() },
  { "import-stdin", '\0', new option_import_stdin() },
  { "init-file", 'i',
    new option_init_file("init-file", 'i', true) },
  { "input-date-format", '\0',
    new option_input_date_format("input-date-format", '\0', true) },
  { "limit", 'l',
    new option_limit("limit", 'l', true) },
  { "lot-dates", '\0',
    new option_lot_dates("lot-dates", '\0', false) },
  { "lot-prices", '\0',
    new option_lot_prices("lot-prices", '\0', false) },
  { "lot-tags", '\0',
    new option_lot_tags("lot-tags", '\0', false) },
  { "lots", '\0',
    new option_lots("lots", '\0', false) },
  { "market", 'V',
    new option_market("market", 'V', false) },
  { "monthly", 'M',
    new option_monthly("monthly", 'M', false) },
  { "no-cache", '\0',
    new option_no_cache("no-cache", '\0', false) },
  { "only", '\0',
    new option_only("only", '\0', true) },
  { "output", 'o',
    new option_output("output", 'o', true) },
  { "pager", '\0',
    new option_pager("pager", '\0', true) },
  { "percentage", '%',
    new option_percentage("percentage", '%', false) },
  { "performance", 'g',
    new option_performance("performance", 'g', false) },
  { "period", 'p',
    new option_period("period", 'p', true) },
  { "period-sort", '\0',
    new option_period_sort("period-sort", '\0', true) },
  { "plot-amount-format", '\0',
    new option_plot_amount_format("plot-amount-format", '\0', true) },
  { "plot-total-format", '\0',
    new option_plot_total_format("plot-total-format", '\0', true) },
  { "post-split", '\0', new option_post_split },
  { "pre-split", '\0', new option_pre_split },
  { "price", 'I',
    new option_price("price", 'I', false) },
  { "price-db", '\0',
    new option_price_db("price-db", '\0', true) },
  { "price-exp", 'Z',
    new option_price_exp("price-exp", 'Z', true) },
  { "prices-format", '\0',
    new option_prices_format("prices-format", '\0', true) },
  { "print-format", '\0',
    new option_print_format("print-format", '\0', true) },
  { "quantity", 'O',
    new option_quantity("quantity", 'O', false) },
  { "quarterly", '\0',
    new option_quarterly("quarterly", '\0', false) },
  { "real", 'R',
    new option_real("real", 'R', false) },
  { "reconcile", '\0',
    new option_reconcile("reconcile", '\0', true) },
  { "reconcile-date", '\0',
    new option_reconcile_date("reconcile-date", '\0', true) },
  { "register-format", '\0',
    new option_register_format("register-format", '\0', true) },
  { "related", 'r',
    new option_related("related", 'r', false) },
  { "set-price", '\0',
    new option_set_price("set-price", '\0', true) },
  { "sort", 'S',
    new option_sort("sort", 'S', true) },
  { "sort-all", '\0',
    new option_sort_all("sort-all", '\0', true) },
  { "sort-entries", '\0',
    new option_sort_entries("sort-entries", '\0', true) },
  { "split", '\0', new option_split },
  { "subtotal", 's',
    new option_subtotal("subtotal", 's', false) },
  { "tail", '\0',
    new option_tail("tail", '\0', true) },
  { "total", 'T',
    new option_total("total", 'T', true) },
  { "total-data", 'J',
    new option_total_data("total-data", 'J', false) },
  { "totals", '\0',
    new option_totals("totals", '\0', false) },
  { "trace", '\0',
    new option_trace("trace", '\0', false) },
  { "truncate", '\0',
    new option_truncate("truncate", '\0', true) },
  { "unbudgeted", '\0',
    new option_unbudgeted("unbudgeted", '\0', false) },
  { "uncleared", 'U',
    new option_uncleared("uncleared", 'U', false) },
  { "verbose", '\0',
    new option_verbose("verbose", '\0', false) },
  { "version", 'v',
    new option_version("version", 'v', false) },
  { "weekly", 'W',
    new option_weekly("weekly", 'W', false) },
  { "wide", 'w',
    new option_wide("wide", 'w', false) },
  { "wide-register-format", '\0',
    new option_wide_register_format("wide-register-format", '\0', true) },
  { "write-hdr-format", '\0',
    new option_write_hdr_format("write-hdr-format", '\0', true) },
  { "write-xact-format", '\0',
    new option_write_xact_format("write-xact-format", '\0', true) },
  { "yearly", 'Y',
    new option_yearly("yearly", 'Y', false) },
};

} // namespace ledger

#ifdef USE_BOOST_PYTHON

#include <boost/python.hpp>
#include <boost/python/detail/api_placeholder.hpp>
#include <boost/python/suite/indexing/map_indexing_suite.hpp>

using namespace boost::python;

struct py_option_handler_t : public option_handler_t
{
  PyObject * self;
  py_option_handler_t(PyObject * self_,
		      const std::string& long_opt,
		      const bool wants_arg)
    : self(self_), option_handler_t(long_opt, wants_arg) {}
  virtual ~py_option_handler_t() {}

  virtual bool check(option_source_t source) {
    return call_method<bool>(self, "check", source);
  }
  virtual void run(const char * optarg = NULL) {
    if (optarg)
      return call_method<void>(self, "run", optarg);
    else
      return call_method<void>(self, "run");
  }
};

BOOST_PYTHON_MEMBER_FUNCTION_OVERLOADS(option_handler_run_overloads,
				       py_option_handler_t::run, 0, 1)

typedef std::map<const std::string, object>  option_handlers_map;
typedef std::pair<const std::string, object> option_handlers_pair;

option_handlers_map option_handlers;

option_handler_t * find_option_handler(const std::string& name)
{
  option_handlers_map::const_iterator i =
    option_handlers.find(name);
  if (i != option_handlers.end())
    return extract<py_option_handler_t *>((*i).second.ptr());
  return NULL;
}

void shutdown_option()
{
  option_handlers.clear();
}

void export_option()
{
  class_< option_handler_t, py_option_handler_t, boost::noncopyable >
    ("OptionHandler", init<const std::string&, bool>())
    .def("check", &py_option_handler_t::check)
    .def("run", &py_option_handler_t::run, option_handler_run_overloads())
    ;

  enum_< option_handler_t::option_source_t > ("OptionSource")
    .value("InitFile",    option_handler_t::INIT_FILE)
    .value("Environment", option_handler_t::ENVIRONMENT)
    .value("DataFile",    option_handler_t::DATA_FILE)
    .value("CommandLine", option_handler_t::COMMAND_LINE)
    ;

  class_< option_handlers_map > ("OptionHandlersMap")
    .def(map_indexing_suite<option_handlers_map>())
    ;

  scope().attr("options") = ptr(&option_handlers);
}

#endif // USE_BOOST_PYTHON
