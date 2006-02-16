#include "config.h"
#include "acconf.h"
#include "option.h"
#include "datetime.h"
#include "quotes.h"
#include "valexpr.h"
#include "walk.h"

#include <fstream>
#include <ctime>
#include <cstdlib>
#ifdef WIN32
#include <io.h>
#else
#include <unistd.h>
#endif

namespace ledger {

std::list<option_t> config_options;

static config_t * config = NULL;

void config_t::reset()
{
  amount_expr	     = "a";
  total_expr	     = "O";
  total_expr_template = "#";
  pricing_leeway     = 24 * 3600;
  budget_flags       = BUDGET_NO_BUDGET;
  balance_format     = "%20T  %2_%-a\n";
  register_format    = ("%D %-.20P %-.22A %12.67t %12.80T\n%/"
			"%32|%-.22A %12.67t %12.80T\n");
  wide_register_format = ("%D  %-.35P %-.38A %22.108t %22.132T\n%/"
			  "%48|%-.38A %22.108t %22.132T\n");
  csv_register_format = "\"%D\",\"%P\",\"%A\",\"%t\",\"%T\"\n";
  plot_amount_format = "%D %(St)\n";
  plot_total_format  = "%D %(ST)\n";
  print_format       = "\n%d %Y%C%P\n    %-34W  %12o%n\n%/    %-34W  %12o%n\n";
  write_hdr_format   = "%d %Y%C%P\n";
  write_xact_format  = "    %-34W  %12o%n\n";
  equity_format      = "\n%D %Y%C%P\n%/    %-34W  %12t\n";
  prices_format      = "%[%Y/%m/%d %H:%M:%S %Z]   %-10A %12t %12T\n";
  pricesdb_format    = "P %[%Y/%m/%d %H:%M:%S] %A %t\n";

  predicate	    = "";
  display_predicate = "";

  head_entries = 0;
  tail_entries = 0;

  show_collapsed     = false;
  show_subtotal      = false;
  show_totals        = false;
  show_related       = false;
  show_all_related   = false;
  show_inverted      = false;
  show_empty	     = false;
  days_of_the_week   = false;
  by_payee           = false;
  comm_as_payee      = false;
  show_revalued      = false;
  show_revalued_only = false;
  download_quotes    = false;

  use_cache	     = false;
  cache_dirty        = false;
}

void
config_t::regexps_to_predicate(const std::string& command,
			       std::list<std::string>::const_iterator begin,
			       std::list<std::string>::const_iterator end,
			       const bool account_regexp,
			       const bool add_account_short_masks,
			       const bool logical_and)
{
  std::string regexps[2];

  assert(begin != end);

  // Treat the remaining command-line arguments as regular
  // expressions, used for refining report results.

  for (std::list<std::string>::const_iterator i = begin;
       i != end;
       i++)
    if ((*i)[0] == '-') {
      if (! regexps[1].empty())
	regexps[1] += "|";
      regexps[1] += (*i).substr(1);
    }
    else if ((*i)[0] == '+') {
      if (! regexps[0].empty())
	regexps[0] += "|";
      regexps[0] += (*i).substr(1);
    }
    else {
      if (! regexps[0].empty())
	regexps[0] += "|";
      regexps[0] += *i;
    }

  for (int i = 0; i < 2; i++) {
    if (regexps[i].empty())
      continue;

    if (! predicate.empty())
      predicate += logical_and ? "&" : "|";

    int add_predicate = 0;	// 1 adds /.../, 2 adds ///.../
    if (i == 1) {
      predicate += "!";
    }
    else if (add_account_short_masks) {
      if (regexps[i].find(':') != std::string::npos ||
	  regexps[i].find('.') != std::string::npos ||
	  regexps[i].find('*') != std::string::npos ||
	  regexps[i].find('+') != std::string::npos ||
	  regexps[i].find('[') != std::string::npos ||
	  regexps[i].find('(') != std::string::npos) {
	show_subtotal = true;
	add_predicate = 1;
      } else {
	add_predicate = 2;
      }
    }
    else {
      add_predicate = 1;
    }

    if (i != 1 && command == "b" && account_regexp) {
      if (! display_predicate.empty())
	display_predicate += "&";
      else if (! show_empty)
	display_predicate += "T&";

      if (add_predicate == 2)
	display_predicate += "//";
      display_predicate += "/(?:";
      display_predicate += regexps[i];
      display_predicate += ")/";
    }

    if (! account_regexp)
      predicate += "/";
    predicate += "/(?:";
    predicate += regexps[i];
    predicate += ")/";
  }
}

bool config_t::process_option(const std::string& opt, const char * arg)
{
  config = this;
  bool result = ::process_option(config_options, opt, arg);
  config = NULL;
  return result;
}

void config_t::process_arguments(int argc, char ** argv, const bool anywhere,
				 std::list<std::string>& args)
{
  config = this;
  ::process_arguments(config_options, argc, argv, anywhere, args);
  config = NULL;
}

void config_t::process_environment(char ** envp, const std::string& tag)
{
  config = this;
  ::process_environment(config_options, envp, tag);
  config = NULL;
}

void config_t::process_options(const std::string&     command,
			       strings_list::iterator arg,
			       strings_list::iterator args_end)
{
  // Configure some other options depending on report type

  if (command == "p" || command == "e" || command == "w") {
    show_related     =
    show_all_related = true;
  }
  else if (command == "E") {
    show_subtotal = true;
  }
  else if (show_related) {
    if (command == "r") {
      show_inverted = true;
    } else {
      show_subtotal    = true;
      show_all_related = true;
    }
  }

  // Process remaining command-line arguments

  if (command != "e") {
    // Treat the remaining command-line arguments as regular
    // expressions, used for refining report results.

    std::list<std::string>::iterator i = arg;
    for (; i != args_end; i++)
      if (*i == "--")
	break;

    if (i != arg)
      regexps_to_predicate(command, arg, i, true,
			   (command == "b" && ! show_subtotal &&
			    display_predicate.empty()));
    if (i != args_end && ++i != args_end)
      regexps_to_predicate(command, i, args_end);
  }

  // Setup the default value for the display predicate

  if (display_predicate.empty()) {
    if (command == "b") {
      if (! show_empty)
	display_predicate = "T";
      if (! show_subtotal) {
	if (! display_predicate.empty())
	  display_predicate += "&";
	display_predicate += "l<=1";
      }
    }
    else if (command == "E") {
      display_predicate = "t";
    }
    else if (command == "r" && ! show_empty) {
      display_predicate = "a";
    }
  }

  DEBUG_PRINT("ledger.config.predicates", "Predicate: " << predicate);
  DEBUG_PRINT("ledger.config.predicates", "Display P: " << display_predicate);

  // Setup the values of %t and %T, used in format strings

  try {
    ledger::amount_expr.reset(parse_value_expr(amount_expr));
  }
  catch (const value_expr_error& err) {
    throw error(std::string("In amount expression '") + amount_expr +
		"': " + err.what());
  }

  std::string expr = total_expr_template;
  for (std::string::size_type i = expr.find('#');
       i != std::string::npos;
       i = expr.find('#'))
    expr = (std::string(expr, 0, i) + "(" + total_expr + ")" +
	    std::string(expr, i + 1));

  DEBUG_PRINT("ledger.config.total_expr",
	      "Total expression template = " << total_expr_template);
  DEBUG_PRINT("ledger.config.total_expr",
	      "Total expression is now   = " << expr);
  try {
    ledger::total_expr.reset(parse_value_expr(expr));
  }
  catch (const value_expr_error& err) {
    throw error(std::string("In total expression '") + expr + "': " +
		err.what());
  }

  // If downloading is to be supported, configure the updater

  if (! commodity_t::updater && download_quotes)
    commodity_t::updater = new quotes_by_script(price_db, pricing_leeway,
						cache_dirty);

  if (! date_format.empty())
    format_t::date_format = date_format;
}

item_handler<transaction_t> *
config_t::chain_xact_handlers(const std::string& command,
			      item_handler<transaction_t> * base_formatter,
			      journal_t * journal,
			      account_t * master,
			      std::list<item_handler<transaction_t> *>& ptrs)
{
  item_handler<transaction_t> * formatter = NULL;

  ptrs.push_back(formatter = base_formatter);

  // format_transactions write each transaction received to the
  // output stream.
  if (! (command == "b" || command == "E")) {
    // truncate_entries cuts off a certain number of _entries_ from
    // being displayed.  It does not affect calculation.
    if (head_entries || tail_entries)
      ptrs.push_back(formatter =
		     new truncate_entries(formatter,
					  head_entries, tail_entries));

    // filter_transactions will only pass through transactions
    // matching the `display_predicate'.
    if (! display_predicate.empty())
      ptrs.push_back(formatter =
		     new filter_transactions(formatter,
					     display_predicate));

    // calc_transactions computes the running total.  When this
    // appears will determine, for example, whether filtered
    // transactions are included or excluded from the running total.
    ptrs.push_back(formatter = new calc_transactions(formatter));

    // reconcile_transactions will pass through only those
    // transactions which can be reconciled to a given balance
    // (calculated against the transactions which it receives).
    if (! reconcile_balance.empty()) {
      value_t target_balance(reconcile_balance);
      time_t  cutoff = now;
      if (! reconcile_date.empty())
	parse_date(reconcile_date.c_str(), &cutoff);
      ptrs.push_back(formatter =
		     new reconcile_transactions(formatter, target_balance,
						cutoff));
    }

    // sort_transactions will sort all the transactions it sees, based
    // on the `sort_order' value expression.
    if (! sort_string.empty())
      ptrs.push_back(formatter =
		     new sort_transactions(formatter, sort_string));

    // changed_value_transactions adds virtual transactions to the
    // list to account for changes in market value of commodities,
    // which otherwise would affect the running total unpredictably.
    if (show_revalued)
      ptrs.push_back(formatter =
		     new changed_value_transactions(formatter,
						    show_revalued_only));

    // collapse_transactions causes entries with multiple transactions
    // to appear as entries with a subtotaled transaction for each
    // commodity used.
    if (show_collapsed)
      ptrs.push_back(formatter = new collapse_transactions(formatter));
  }

  // subtotal_transactions combines all the transactions it receives
  // into one subtotal entry, which has one transaction for each
  // commodity in each account.
  //
  // period_transactions is like subtotal_transactions, but it
  // subtotals according to time periods rather than totalling
  // everything.
  //
  // dow_transactions is like period_transactions, except that it
  // reports all the transactions that fall on each subsequent day
  // of the week.
  if (show_subtotal && ! (command == "b" || command == "E"))
    ptrs.push_back(formatter = new subtotal_transactions(formatter));

  if (days_of_the_week)
    ptrs.push_back(formatter = new dow_transactions(formatter));
  else if (by_payee)
    ptrs.push_back(formatter = new by_payee_transactions(formatter));

  if (! report_period.empty()) {
    ptrs.push_back(formatter =
		   new interval_transactions(formatter,
					     report_period,
					     report_period_sort));
    ptrs.push_back(formatter = new sort_transactions(formatter, "d"));
  }

  // invert_transactions inverts the value of the transactions it
  // receives.
  if (show_inverted)
    ptrs.push_back(formatter = new invert_transactions(formatter));

  // related_transactions will pass along all transactions related
  // to the transaction received.  If `show_all_related' is true,
  // then all the entry's transactions are passed; meaning that if
  // one transaction of an entry is to be printed, all the
  // transaction for that entry will be printed.
  if (show_related)
    ptrs.push_back(formatter =
		   new related_transactions(formatter,
					    show_all_related));

  // This filter_transactions will only pass through transactions
  // matching the `predicate'.
  if (! predicate.empty())
    ptrs.push_back(formatter = new filter_transactions(formatter, predicate));

  // budget_transactions takes a set of transactions from a data
  // file and uses them to generate "budget transactions" which
  // balance against the reported transactions.
  //
  // forecast_transactions is a lot like budget_transactions, except
  // that it adds entries only for the future, and does not balance
  // them against anything but the future balance.

  if (budget_flags) {
    budget_transactions * handler
      = new budget_transactions(formatter, budget_flags);
    handler->add_period_entries(journal->period_entries);
    ptrs.push_back(formatter = handler);

    // Apply this before the budget handler, so that only matching
    // transactions are calculated toward the budget.  The use of
    // filter_transactions above will further clean the results so
    // that no automated transactions that don't match the filter get
    // reported.
    if (! predicate.empty())
      ptrs.push_back(formatter = new filter_transactions(formatter, predicate));
  }
  else if (! forecast_limit.empty()) {
    forecast_transactions * handler
      = new forecast_transactions(formatter, forecast_limit);
    handler->add_period_entries(journal->period_entries);
    ptrs.push_back(formatter = handler);

    // See above, under budget_transactions.
    if (! predicate.empty())
      ptrs.push_back(formatter = new filter_transactions(formatter, predicate));
  }

  if (comm_as_payee)
    ptrs.push_back(formatter = new set_comm_as_payee(formatter));

  return formatter;
}

static void show_version(std::ostream& out)
{
  out << "Ledger " << ledger::version << ", the command-line accounting tool";
  out << "\n\nCopyright (c) 2003-2005, John Wiegley.  All rights reserved.\n\n\
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
  -G, --gain             report net gain/loss\n\n";
  out
    << "Commands:\n\
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
      --help-comm        commodity options\n";
  out << "\nBasic options:\n\
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
      --budget           generate budget entries based on FILE\n\
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
  config->init_file = optarg;
} OPT_END(init_file);

OPT_BEGIN(file, "f:") {
  if (std::string(optarg) == "-" || access(optarg, R_OK) != -1)
    config->data_file = optarg;
  else
    throw error(std::string("The ledger file '") + optarg +
		"' does not exist or is not readable");
} OPT_END(file);

OPT_BEGIN(cache, ":") {
  config->cache_file = optarg;
} OPT_END(cache);

OPT_BEGIN(no_cache, "") {
  config->cache_file = "<none>";
} OPT_END(no_cache);

OPT_BEGIN(output, "o:") {
  if (std::string(optarg) != "-")
    config->output_file = optarg;
} OPT_END(output);

OPT_BEGIN(account, "a:") {
  config->account = optarg;
} OPT_END(account);

//////////////////////////////////////////////////////////////////////
//
// Report filtering

OPT_BEGIN(effective, "") {
  transaction_t::use_effective_date = true;
} OPT_END(effective);

OPT_BEGIN(begin, "b:") {
  char buf[128];
  interval_t interval(optarg);
  if (interval.begin)
    std::strftime(buf, 127, formats[0], std::localtime(&interval.begin));
  else
    throw error(std::string("Could not determine beginning of period '") +
		optarg + "'");

  if (! config->predicate.empty())
    config->predicate += "&";
  config->predicate += "d>=[";
  config->predicate += buf;
  config->predicate += "]";
} OPT_END(begin);

OPT_BEGIN(end, "e:") {
  char buf[128];
  interval_t interval(optarg);
  if (interval.end)
    std::strftime(buf, 127, formats[0], std::localtime(&interval.end));
  else
    throw error(std::string("Could not determine end of period '") +
		optarg + "'");

  if (! config->predicate.empty())
    config->predicate += "&";
  config->predicate += "d<[";
  config->predicate += buf;
  config->predicate += "]";

  terminus = interval.end;
} OPT_END(end);

OPT_BEGIN(current, "c") {
  if (! config->predicate.empty())
    config->predicate += "&";
  config->predicate += "d<=m";
} OPT_END(current);

OPT_BEGIN(cleared, "C") {
  if (! config->predicate.empty())
    config->predicate += "&";
  config->predicate += "X";
} OPT_END(cleared);

OPT_BEGIN(uncleared, "U") {
  if (! config->predicate.empty())
    config->predicate += "&";
  config->predicate += "!X";
} OPT_END(uncleared);

OPT_BEGIN(real, "R") {
  if (! config->predicate.empty())
    config->predicate += "&";
  config->predicate += "R";
} OPT_END(real);

OPT_BEGIN(actual, "L") {
  if (! config->predicate.empty())
    config->predicate += "&";
  config->predicate += "L";
} OPT_END(actual);

//////////////////////////////////////////////////////////////////////
//
// Output customization

OPT_BEGIN(format, "F:") {
  config->format_string = optarg;
} OPT_END(format);

OPT_BEGIN(date_format, "y:") {
  config->date_format = optarg;
} OPT_END(date_format);

OPT_BEGIN(input_date_format, ":") {
  std::strcpy(input_format, optarg);
  formats[0] = input_format;
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

OPT_BEGIN(csv_register_format, ":") {
  config->csv_register_format = optarg;
} OPT_END(csv_register_format);

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
  config->head_entries = std::atoi(optarg);
} OPT_END(head);

OPT_BEGIN(tail, ":") {
  config->tail_entries = std::atoi(optarg);
} OPT_END(tail);

OPT_BEGIN(pager, ":") {
  config->pager = optarg;
} OPT_END(pager);

OPT_BEGIN(empty, "E") {
  config->show_empty = true;
} OPT_END(empty);

OPT_BEGIN(collapse, "n") {
  config->show_collapsed = true;
} OPT_END(collapse);

OPT_BEGIN(subtotal, "s") {
  config->show_subtotal = true;
} OPT_END(subtotal);

OPT_BEGIN(totals, "") {
  config->show_totals = true;
} OPT_END(totals);

OPT_BEGIN(sort, "S:") {
  config->sort_string = optarg;
} OPT_END(sort);

OPT_BEGIN(related, "r") {
  config->show_related = true;
} OPT_END(related);

OPT_BEGIN(period, "p:") {
  if (config->report_period.empty()) {
    config->report_period = optarg;
  } else {
    config->report_period += " ";
    config->report_period += optarg;
  }

  // If the period gives a beginning and/or ending date, make sure to
  // modify the calculation predicate (via the --begin and --end
  // options) to take this into account.

  char buf[128];
  interval_t interval(config->report_period);

  if (interval.begin) {
    std::strftime(buf, 127, formats[0], std::localtime(&interval.begin));

    if (! config->predicate.empty())
      config->predicate += "&";
    config->predicate += "d>=[";
    config->predicate += buf;
    config->predicate += "]";
  }

  if (interval.end) {
    std::strftime(buf, 127, formats[0], std::localtime(&interval.end));

    if (! config->predicate.empty())
      config->predicate += "&";
    config->predicate += "d<[";
    config->predicate += buf;
    config->predicate += "]";

    terminus = interval.end;
  }
} OPT_END(period);

OPT_BEGIN(period_sort, ":") {
  config->report_period_sort = optarg;
} OPT_END(period_sort);

OPT_BEGIN(weekly, "W") {
  if (config->report_period.empty())
    config->report_period = "weekly";
  else
    config->report_period = std::string("weekly ") + config->report_period;
} OPT_END(weekly);

OPT_BEGIN(monthly, "M") {
  if (config->report_period.empty())
    config->report_period = "monthly";
  else
    config->report_period = std::string("monthly ") + config->report_period;
} OPT_END(monthly);

OPT_BEGIN(yearly, "Y") {
  if (config->report_period.empty())
    config->report_period = "yearly";
  else
    config->report_period = std::string("yearly ") + config->report_period;
} OPT_END(yearly);

OPT_BEGIN(dow, "") {
  config->days_of_the_week = true;
} OPT_END(dow);

OPT_BEGIN(by_payee, "P") {
  config->by_payee = true;
} OPT_END(by_payee);

OPT_BEGIN(comm_as_payee, "x") {
  config->comm_as_payee = true;
} OPT_END(comm_as_payee);

OPT_BEGIN(budget, "") {
  config->budget_flags = BUDGET_BUDGETED;
} OPT_END(budget);

OPT_BEGIN(add_budget, "") {
  config->budget_flags = BUDGET_BUDGETED | BUDGET_UNBUDGETED;
} OPT_END(add_budget);

OPT_BEGIN(unbudgeted, "") {
  config->budget_flags = BUDGET_UNBUDGETED;
} OPT_END(unbudgeted);

OPT_BEGIN(forecast, ":") {
  config->forecast_limit = optarg;
} OPT_END(forecast);

OPT_BEGIN(reconcile, ":") {
  config->reconcile_balance = optarg;
} OPT_END(reconcile);

OPT_BEGIN(reconcile_date, ":") {
  config->reconcile_date = optarg;
} OPT_END(reconcile_date);

OPT_BEGIN(limit, "l:") {
  if (! config->predicate.empty())
    config->predicate += "&";
  config->predicate += "(";
  config->predicate += optarg;
  config->predicate += ")";
} OPT_END(limit);

OPT_BEGIN(display, "d:") {
  if (! config->display_predicate.empty())
    config->display_predicate += "&";
  config->display_predicate += "(";
  config->display_predicate += optarg;
  config->display_predicate += ")";
} OPT_END(display);

OPT_BEGIN(amount, "t:") {
  config->amount_expr = optarg;
} OPT_END(amount);

OPT_BEGIN(total, "T:") {
  config->total_expr = optarg;
} OPT_END(total);

OPT_BEGIN(amount_data, "j") {
  config->format_string = config->plot_amount_format;
} OPT_END(amount_data);

OPT_BEGIN(total_data, "J") {
  config->format_string = config->plot_total_format;
} OPT_END(total_data);

//////////////////////////////////////////////////////////////////////
//
// Commodity reporting

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
  config->amount_expr = "a";
  config->total_expr  = "O";
} OPT_END(quantity);

OPT_BEGIN(basis, "B") {
  config->amount_expr = "b";
  config->total_expr  = "B";
} OPT_END(basis);

OPT_BEGIN(market, "V") {
  config->show_revalued = true;

  config->amount_expr = "v";
  config->total_expr  = "V";
} OPT_END(market);

OPT_BEGIN(performance, "g") {
  config->amount_expr = "P(a,m)-b"; // same as 'g', but priced now
  config->total_expr  = "P(O,m)-B";
} OPT_END(performance);

OPT_BEGIN(gain, "G") {
  config->show_revalued      =
  config->show_revalued_only = true;

  config->amount_expr = "a";
  config->total_expr  = "G";
} OPT_END(gain);

OPT_BEGIN(average, "A") {
  config->total_expr_template = "A#";
} OPT_END(average);

OPT_BEGIN(deviation, "D") {
  config->total_expr_template = "t-A#";
} OPT_END(deviation);

OPT_BEGIN(percentage, "%") {
  config->total_expr_template = "^#&{100.0%}*(#/^#)";
} OPT_END(percentage);

} // namespace ledger
