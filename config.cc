#include "config.h"
#include "report.h"
#include "transform.h"
#include "journal.h"
#include "format.h"
#ifdef USE_BOOST_PYTHON
#include "py_eval.h"
#endif

#include <iostream>

namespace ledger {

//////////////////////////////////////////////////////////////////////
//
// Help options
//

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
//

DEFR_OPTS(full_help, "full-help", 'H')
  full_help(std::cout);
  throw 0;
END_DEFR()

DEFR_OPTS(help, "help", 'h')
  help(std::cout);
  throw 0;
END_DEFR()

DEFR_OPT(help_calc, "help-calc")
  calc_help(std::cout);
  throw 0;
END_DEFR()

DEFR_OPT(help_disp, "help-disp")
  disp_help(std::cout);
  throw 0;
END_DEFR()

DEFR_OPT(help_comm, "help-comm")
  comm_help(std::cout);
  throw 0;
END_DEFR()

DEFR_OPTS(version, "version", 'v')
  show_version(std::cout);
  throw 0;
END_DEFR()

DEFR_OPTS_(init_file, "init-file", 'i')
  std::string path = resolve_path(optarg);
  if (access(path.c_str(), R_OK) != -1)
    report->session->init_file = path;
  else
    throw new error(std::string("The init file '") + path +
		    "' does not exist or is not readable");
END_DEFR()

DEFR_OPTS_(file, "file", 'f')
  if (std::string(optarg) == "-") {
    report->session->data_file = optarg;
  } else {
    std::string path = resolve_path(optarg);
    if (access(path.c_str(), R_OK) != -1)
      report->session->data_file = path;
    else
      throw new error(std::string("The ledger file '") + path +
		      "' does not exist or is not readable");
  }
END_DEFR()

DEFR_OPT_(cache, "cache")
  report->session->cache_file = resolve_path(optarg);
END_DEFR()

DEFR_OPT(no_cache, "no-cache")
  report->session->cache_file = "<none>";
END_DEFR()

DEFR_OPTS_(output, "output", 'o')
  if (std::string(optarg) != "-") {
    std::string path = resolve_path(optarg);
    report->output_file = path;
  }
END_DEFR()

DEFR_OPTS_(account, "account", 'a')
  report->account = optarg;
END_DEFR()

DEFR_OPT_(debug, "debug")
  report->session->debug_mode = true;
  std::string dbgcls = std::string("ledger.") + optarg;
  ::setenv("DEBUG_CLASS", dbgcls.c_str(), 1);
END_DEFR()

DEFR_OPT(verbose, "verbose")
  report->session->verbose_mode = true;
END_DEFR()

DEFR_OPT(trace, "trace")
  report->session->trace_mode = true;
END_DEFR()

//////////////////////////////////////////////////////////////////////
//
// Report filtering
//

DEFR_OPT(effective, "effective")
  transaction_t::use_effective_date = true;
END_DEFR()

DEFR_OPTS_(begin, "begin", 'b')
  char buf[128];
  interval_t interval(optarg);
  if (! interval.begin)
    throw new error(std::string("Could not determine beginning of period '") +
		    optarg + "'");

#if 0
  if (! report->predicate.empty())
    report->predicate += "&";
  report->predicate += "d>=[";
  report->predicate += interval.begin.to_string();
  report->predicate += "]";
#endif
END_DEFR()

DEFR_OPTS_(end, "end", 'e')
  char buf[128];
  interval_t interval(optarg);
  if (! interval.end)
    throw new error(std::string("Could not determine end of period '") +
		    optarg + "'");

#if 0
  if (! report->predicate.empty())
    report->predicate += "&";
  report->predicate += "d<[";
  report->predicate += interval.end.to_string();
  report->predicate += "]";
#endif

  report->session->terminus = interval.end;
END_DEFR()

DEFR_OPTS(current, "current", 'c')
#if 0
  if (! report->predicate.empty())
    report->predicate += "&";
  report->predicate += "d<=m";
#endif
END_DEFR()

DEFR_OPTS(cleared, "cleared", 'C')
#if 0
  if (! report->predicate.empty())
    report->predicate += "&";
  report->predicate += "X";
#endif
END_DEFR()

DEFR_OPTS(uncleared, "uncleared", 'U')
#if 0
  if (! report->predicate.empty())
    report->predicate += "&";
  report->predicate += "!X";
#endif
END_DEFR()

DEFR_OPTS(real, "real", 'R')
#if 0
  if (! report->predicate.empty())
    report->predicate += "&";
  report->predicate += "R";
#endif
END_DEFR()

DEFR_OPTS(actual, "actual", 'L')
#if 0
  if (! report->predicate.empty())
    report->predicate += "&";
  report->predicate += "L";
#endif
END_DEFR()

DEFR_OPT(lots, "lots")
  report->keep_price =
  report->keep_date  =
  report->keep_tag   = true;
END_DEFR()

DEFR_OPT(lot_prices, "lot-prices")
  report->keep_price = true;
END_DEFR()

DEFR_OPT(lot_dates, "lot-dates")
  report->keep_date = true;
END_DEFR()

DEFR_OPT(lot_tags, "lot-tags")
  report->keep_tag = true;
END_DEFR()

//////////////////////////////////////////////////////////////////////
//
// Output customization
//

DEFR_OPTS_(format, "format", 'F')
  report->format_string = optarg;
END_DEFR()

DEFR_OPTS_(date_format, "date-format", 'y')
  report->date_output_format = optarg;
END_DEFR()

DEFR_OPT_(input_date_format, "input-date-format")
  report->session->date_input_format = optarg;
END_DEFR()

DEFR_OPT_(balance_format, "balance-format")
  report->session->balance_format = optarg;
END_DEFR()

DEFR_OPT_(register_format, "register-format")
  report->session->register_format = optarg;
END_DEFR()

DEFR_OPT_(wide_register_format, "wide-register-format")
  report->session->wide_register_format = optarg;
END_DEFR()

DEFR_OPT_(plot_amount_format, "plot-amount-format")
  report->session->plot_amount_format = optarg;
END_DEFR()

DEFR_OPT_(plot_total_format, "plot-total-format")
  report->session->plot_total_format = optarg;
END_DEFR()

DEFR_OPT_(print_format, "print-format")
  report->session->print_format = optarg;
END_DEFR()

DEFR_OPT_(write_hdr_format, "write-hdr-format")
  report->session->write_hdr_format = optarg;
END_DEFR()

DEFR_OPT_(write_xact_format, "write-xact-format")
  report->session->write_xact_format = optarg;
END_DEFR()

DEFR_OPT_(equity_format, "equity-format")
  report->session->equity_format = optarg;
END_DEFR()

DEFR_OPT_(prices_format, "prices-format")
  report->session->prices_format = optarg;
END_DEFR()

DEFR_OPTS(wide, "wide", 'w')
  report->session->register_format = report->session->wide_register_format;
END_DEFR()

DEFR_OPT_(head, "head")
#if 0
  report->head_entries = std::atoi(optarg);
#endif
END_DEFR()

DEFR_OPT_(tail, "tail")
#if 0
  report->tail_entries = std::atoi(optarg);
#endif
END_DEFR()

DEFR_OPT_(pager, "pager")
  report->pager = optarg;
END_DEFR()

DEFR_OPT_(truncate, "truncate")
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
END_DEFR()

DEFR_OPT_(abbrev_len, "abbrev-len")
#if 0
  format_t::abbrev_length = std::atoi(optarg);
#endif
END_DEFR()

DEFR_OPTS(empty, "empty", 'E')
#if 0
  report->show_empty = true;
#endif
END_DEFR()

DEFR_OPTS(collapse, "collapse", 'n')
#if 0
  report->show_collapsed = true;
#endif
END_DEFR()

DEFR_OPTS(subtotal, "subtotal", 's')
#if 0
  report->show_subtotal = true;
#endif
END_DEFR()

DEFR_OPT(totals, "totals")
  report->show_totals = true;
END_DEFR()

DEFR_OPTS_(sort, "sort", 'S')
#if 0
  report->sort_string = optarg;
#endif
END_DEFR()

DEFR_OPT(sort_entries, "sort-entries")
#if 0
  report->sort_string = optarg;
  report->entry_sort = true;
#endif
END_DEFR()

DEFR_OPT(sort_all, "sort-all")
#if 0
  report->sort_string = optarg;
  report->entry_sort = false;
  report->sort_all   = true;
#endif
END_DEFR()

DEFR_OPT_(period_sort, "period-sort")
#if 0
  report->sort_string = optarg;
  report->entry_sort = true;
#endif
END_DEFR()

DEFR_OPTS(related, "related", 'r')
#if 0
  report->show_related = true;
#endif
END_DEFR()

DEFR_OPT(descend, "descend")
#if 0
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
#endif
END_DEFR()

DEFR_OPT(descend_if, "descend-if")
#if 0
  report->descend_expr = optarg;
#endif
END_DEFR()

DEFR_OPTS_(period, "period", 'p')
#if 0
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

    report->session->terminus = interval.end;
  }
#endif
END_DEFR()

DEFR_OPT(daily, "daily")
#if 0
  if (report->report_period.empty())
    report->report_period = "daily";
  else
    report->report_period = std::string("daily ") + report->report_period;
#endif
END_DEFR()

DEFR_OPTS(weekly, "weekly", 'W')
#if 0
  if (report->report_period.empty())
    report->report_period = "weekly";
  else
    report->report_period = std::string("weekly ") + report->report_period;
#endif
END_DEFR()

DEFR_OPTS(monthly, "monthly", 'M')
#if 0
  if (report->report_period.empty())
    report->report_period = "monthly";
  else
    report->report_period = std::string("monthly ") + report->report_period;
#endif
END_DEFR()

DEFR_OPT(quarterly, "quarterly")
#if 0
  if (report->report_period.empty())
    report->report_period = "quarterly";
  else
    report->report_period = std::string("quarterly ") + report->report_period;
#endif
END_DEFR()

DEFR_OPTS(yearly, "yearly", 'Y')
#if 0
  if (report->report_period.empty())
    report->report_period = "yearly";
  else
    report->report_period = std::string("yearly ") + report->report_period;
#endif
END_DEFR()

DEFR_OPT(dow, "dow")
#if 0
  report->days_of_the_week = true;
#endif
END_DEFR()

DEFR_OPTS(by_payee, "by-payee", 'P')
#if 0
  report->by_payee = true;
#endif
END_DEFR()

DEFR_OPTS(comm_as_payee, "comm-as-payee", 'x')
#if 0
  report->comm_as_payee = true;
#endif
END_DEFR()

DEFR_OPT(code_as_payee, "code-as-payee")
#if 0
  report->code_as_payee = true;
#endif
END_DEFR()

DEFR_OPT(budget, "budget")
#if 0
  report->budget_flags = BUDGET_BUDGETED;
#endif
END_DEFR()

DEFR_OPT(add_budget, "add-budget")
#if 0
  report->budget_flags = BUDGET_BUDGETED | BUDGET_UNBUDGETED;
#endif
END_DEFR()

DEFR_OPT(unbudgeted, "unbudgeted")
#if 0
  report->budget_flags = BUDGET_UNBUDGETED;
#endif
END_DEFR()

DEFR_OPT_(forecast, "forecast")
#if 0
  report->forecast_limit = optarg;
#endif
END_DEFR()

DEFR_OPT_(reconcile, "reconcile")
#if 0
  report->reconcile_balance = optarg;
#endif
END_DEFR()

DEFR_OPT_(reconcile_date, "reconcile-date")
#if 0
  report->reconcile_date = optarg;
#endif
END_DEFR()

DEFR_OPTS_(limit, "limit", 'l')
#if 0
  if (! report->predicate.empty())
    report->predicate += "&";
  report->predicate += "(";
  report->predicate += optarg;
  report->predicate += ")";
#endif
END_DEFR()

DEFR_OPT_(only, "only")
#if 0
  if (! report->secondary_predicate.empty())
    report->secondary_predicate += "&";
  report->secondary_predicate += "(";
  report->secondary_predicate += optarg;
  report->secondary_predicate += ")";
#endif
END_DEFR()

DEFR_OPTS_(display, "display", 'd')
#if 0
  if (! report->display_predicate.empty())
    report->display_predicate += "&";
  report->display_predicate += "(";
  report->display_predicate += optarg;
  report->display_predicate += ")";
#endif
END_DEFR()

DEFR_OPTS_(amount, "amount", 't')
END_DEFR()

DEFR_OPTS_(total, "total", 'T')
END_DEFR()

DEFR_OPTS(amount_data, "amount-data", 'j')
  report->format_string = report->session->plot_amount_format;
END_DEFR()

DEFR_OPTS(total_data, "total-data", 'J')
  report->format_string = report->session->plot_total_format;
END_DEFR()

DEFR_OPT(ansi, "ansi")
#if 0
  format_t::ansi_codes  = true;
  format_t::ansi_invert = false;
#endif
END_DEFR()

DEFR_OPT(ansi_invert, "ansi-invert")
#if 0
  format_t::ansi_codes  =
  format_t::ansi_invert = true;
#endif
END_DEFR()

//////////////////////////////////////////////////////////////////////
//
// Commodity reporting
//

DEFR_OPT_(base, "base")
  amount_t::keep_base = true;
END_DEFR()

DEFR_OPT_(price_db, "price-db")
  report->session->price_db = optarg;
END_DEFR()

DEFR_OPTS_(price_exp, "price-exp", 'Z')
  report->session->pricing_leeway = std::atol(optarg) * 60;
END_DEFR()

DEFR_OPTS(download, "download", 'Q')
  report->session->download_quotes = true;
END_DEFR()

DEFR_OPTS(quantity, "quantity", 'O')
#if 0
  ledger::amount_expr = "@a";
  ledger::total_expr  = "@O";
#endif
END_DEFR()

DEFR_OPTS(basis, "basis", 'B')
#if 0
  ledger::amount_expr = "@b";
  ledger::total_expr  = "@B";
#endif
END_DEFR()

DEFR_OPTS(price, "price", 'I')
#if 0
  ledger::amount_expr = "@i";
  ledger::total_expr  = "@I";
#endif
END_DEFR()

DEFR_OPTS(market, "market", 'V')
#if 0
  report->show_revalued = true;

  ledger::amount_expr = "@v";
  ledger::total_expr  = "@V";
#endif
END_DEFR()

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

DEFR_OPT_(set_price, "set-price")
  std::string arg(optarg);
  std::string::size_type beg = 0;
  for (std::string::size_type pos = arg.find(';');
       pos != std::string::npos;
       beg = pos + 1, pos = arg.find(';', beg))
    parse_price_setting(std::string(arg, beg, pos - beg).c_str());
  parse_price_setting(std::string(arg, beg).c_str());
END_DEFR()

DEFR_OPTS(performance, "performance", 'g')
#if 0
  ledger::amount_expr = "@P(@a,@m)-@b";
  ledger::total_expr  = "@P(@O,@m)-@B";
#endif
END_DEFR()

DEFR_OPTS(gain, "gain", 'G')
#if 0
  report->show_revalued      =
  report->show_revalued_only = true;

  ledger::amount_expr = "@a";
  ledger::total_expr  = "@G";
#endif
END_DEFR()

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

DEFR_OPTS(average, "average", 'A')
#if 0
  ledger::total_expr = expand_value_expr("@A(#)", ledger::total_expr.expr);
#endif
END_DEFR()

DEFR_OPTS(deviation, "deviation", 'D')
#if 0
  ledger::total_expr = expand_value_expr("@t-@A(#)", ledger::total_expr.expr);
#endif
END_DEFR()

DEFR_OPTS(percentage, "percentage", '%')
#if 0
  ledger::total_expr = expand_value_expr("^#&{100.0%}*(#/^#)",
					 ledger::total_expr.expr);
#endif
END_DEFR()

//////////////////////////////////////////////////////////////////////
//
// Transforms
//

DEFR_OPT(split, "split")
  report->transforms.push_back(new split_transform);
END_DEFR()

} // namespace ledger
