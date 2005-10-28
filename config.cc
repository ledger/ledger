#include "config.h"
#include "acconf.h"
#include "option.h"
#include "datetime.h"
#include "quotes.h"
#include "valexpr.h"
#include "walk.h"
#ifdef USE_BOOST_PYTHON
#include "py_eval.h"
#endif

#include <fstream>
#include <ctime>
#include <cstdlib>
#ifdef WIN32
#include <io.h>
#else
#include <unistd.h>
#endif

namespace ledger {

config_t	    config;
std::list<option_t> config_options;

config_t::config_t()
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
  plot_amount_format = "%D %(St)\n";
  plot_total_format  = "%D %(ST)\n";
  print_format       = "\n%d %Y%C%P\n    %-34W  %12o%n\n%/    %-34W  %12o%n\n";
  write_hdr_format   = "%d %Y%C%P\n";
  write_xact_format  = "    %-34W  %12o%n\n";
  equity_format      = "\n%D %Y%C%P\n%/    %-34W  %12t\n";
#ifndef USE_BOOST_PYTHON
  prices_format      = "%[%Y/%m/%d %H:%M:%S %Z]   %-10A %12t %12T\n";
#else
  prices_format      = ("%[%Y/%m/%d %H:%M:%S %Z]   %-8A "
			"%10t %10(@vmin(t)) %10(@vmax(t)) %12T\n");
#endif
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

static void
regexps_to_predicate(config_t& config, const std::string& command,
		     std::list<std::string>::const_iterator begin,
		     std::list<std::string>::const_iterator end,
		     const bool account_regexp		= false,
		     const bool add_account_short_masks = false)
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

    if (! config.predicate.empty())
      config.predicate += "&";

    int add_predicate = 0;	// 1 adds /.../, 2 adds ///.../
    if (i == 1) {
      config.predicate += "!";
    }
    else if (add_account_short_masks) {
      if (regexps[i].find(':') != std::string::npos ||
	  regexps[i].find('.') != std::string::npos ||
	  regexps[i].find('*') != std::string::npos ||
	  regexps[i].find('+') != std::string::npos ||
	  regexps[i].find('[') != std::string::npos ||
	  regexps[i].find('(') != std::string::npos) {
	config.show_subtotal = true;
	add_predicate = 1;
      } else {
	add_predicate = 2;
      }
    }
    else {
      add_predicate = 1;
    }

    if (i != 1 && command == "b" && account_regexp) {
      if (! config.display_predicate.empty())
	config.display_predicate += "&";
      else if (! config.show_empty)
	config.display_predicate += "T&";

      if (add_predicate == 2)
	config.display_predicate += "//";
      config.display_predicate += "/(?:";
      config.display_predicate += regexps[i];
      config.display_predicate += ")/";
    }

    if (! account_regexp)
      config.predicate += "/";
    config.predicate += "/(?:";
    config.predicate += regexps[i];
    config.predicate += ")/";
  }
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
      regexps_to_predicate(*this, command, arg, i, true,
			   (command == "b" && ! show_subtotal &&
			    display_predicate.empty()));
    if (i != args_end && ++i != args_end)
      regexps_to_predicate(*this, command, i, args_end);
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

void parse_ledger_data(journal_t * journal, parser_t * cache_parser,
		       parser_t * text_parser, parser_t * xml_parser)
{
  int entry_count = 0;

  DEBUG_PRINT("ledger.config.cache", "3. use_cache = " << config.use_cache);

  if (! config.init_file.empty() &&
      access(config.init_file.c_str(), R_OK) != -1) {
    if (parse_journal_file(config.init_file, journal) ||
	journal->auto_entries.size() > 0 ||
	journal->period_entries.size() > 0)
      throw error(std::string("Entries found in initialization file '") +
		  config.init_file + "'");

    journal->sources.pop_front(); // remove init file
  }

  if (cache_parser && config.use_cache &&
      ! config.cache_file.empty() &&
      ! config.data_file.empty()) {
    DEBUG_PRINT("ledger.config.cache", "using_cache " << config.cache_file);
    config.cache_dirty = true;
    if (access(config.cache_file.c_str(), R_OK) != -1) {
      std::ifstream stream(config.cache_file.c_str());
      if (cache_parser->test(stream)) {
	std::string price_db_orig = journal->price_db;
	journal->price_db = config.price_db;
	entry_count += cache_parser->parse(stream, journal, NULL,
					   &config.data_file);
	if (entry_count > 0)
	  config.cache_dirty = false;
	else
	  journal->price_db = price_db_orig;
      }
    }
  }

  if (entry_count == 0 && ! config.data_file.empty()) {
    account_t * account = NULL;
    if (! config.account.empty())
      account = journal->find_account(config.account);

    journal->price_db = config.price_db;
    if (! journal->price_db.empty() &&
	access(journal->price_db.c_str(), R_OK) != -1) {
      if (parse_journal_file(journal->price_db, journal)) {
	throw error("Entries not allowed in price history file");
      } else {
	DEBUG_PRINT("ledger.config.cache",
		    "read price database " << journal->price_db);
	journal->sources.pop_back();
      }
    }

    DEBUG_PRINT("ledger.config.cache",
		"rejected cache, parsing " << config.data_file);
    if (config.data_file == "-") {
      config.use_cache = false;
      journal->sources.push_back("<stdin>");
      if (xml_parser && std::cin.peek() == '<')
	entry_count += xml_parser->parse(std::cin, journal, account);
      else
	entry_count += text_parser->parse(std::cin, journal, account);
    }
    else if (access(config.data_file.c_str(), R_OK) != -1) {
      entry_count += parse_journal_file(config.data_file, journal, account);
      if (! journal->price_db.empty())
	journal->sources.push_back(journal->price_db);
    }
  }

  if (entry_count == 0)
    throw error("Please specify ledger file using -f"
		" or LEDGER_FILE environment variable.");

  VALIDATE(journal->valid());
}

static void show_version(std::ostream& out)
{
  out << "Ledger " << ledger::version << ", the command-line accounting tool";
  out << "\n\nCopyright (c) 2003-2005, John Wiegley.  All rights reserved.\n\n\
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
      --prices-format       --wide-register-format\n\
Commodity reporting:\n\
      --price-db FILE    sets the price database to FILE (def: ~/.pricedb)\n\
  -L, --price-exp MINS   download quotes only if newer than MINS (def: 1440)\n\
  -Q, --download         download price information when needed\n\
  -O, --quantity         report commodity totals (this is the default)\n\
  -B, --basis            report cost basis of commodities\n\
  -V, --market           report last known market value\n\
  -g, --performance      report gain/loss for each displayed transaction\n\
  -G, --gain             report net gain/loss\n\n";
#ifdef USE_BOOST_PYTHON
  out
    << "Python support:\n\
      --import MODULE    on startup, import the given Python MODULE\n\
      --import-stdin     on start, read Python code from standard input\n\n";
#endif
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
#ifdef USE_BOOST_PYTHON
  out << "      --help-python      Python options\n";
#endif
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

#ifdef USE_BOOST_PYTHON
void option_python_help(std::ostream& out)
{
  out << "Python support options:\n\
      --import MODULE    on startup, import the given Python MODULE\n\
      --import-stdin     on start, read Python code from standard input\n\n";
}
#endif

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

#ifdef USE_BOOST_PYTHON
OPT_BEGIN(help_python, "") {
  option_python_help(std::cout);
  throw 0;
} OPT_END(help_python);
#endif

OPT_BEGIN(version, "v") {
  show_version(std::cout);
  throw 0;
} OPT_END(version);

OPT_BEGIN(init_file, "i:") {
  config.init_file = optarg;
} OPT_END(init_file);

OPT_BEGIN(file, "f:") {
  if (std::string(optarg) == "-" || access(optarg, R_OK) != -1)
    config.data_file = optarg;
  else
    throw error(std::string("The ledger file '") + optarg +
		"' does not exist or is not readable");
} OPT_END(file);

OPT_BEGIN(cache, ":") {
  config.cache_file = optarg;
} OPT_END(cache);

OPT_BEGIN(no_cache, "") {
  config.cache_file = "<none>";
} OPT_END(no_cache);

OPT_BEGIN(output, "o:") {
  if (std::string(optarg) != "-")
    config.output_file = optarg;
} OPT_END(output);

OPT_BEGIN(account, "a:") {
  config.account = optarg;
} OPT_END(account);

//////////////////////////////////////////////////////////////////////
//
// Report filtering

OPT_BEGIN(begin, "b:") {
  char buf[128];
  interval_t interval(optarg);
  if (interval.begin)
    std::strftime(buf, 127, formats[0], std::localtime(&interval.begin));
  else
    throw error(std::string("Could not determine beginning of period '") +
		optarg + "'");

  if (! config.predicate.empty())
    config.predicate += "&";
  config.predicate += "d>=[";
  config.predicate += buf;
  config.predicate += "]";
} OPT_END(begin);

OPT_BEGIN(end, "e:") {
  char buf[128];
  interval_t interval(optarg);
  if (interval.end)
    std::strftime(buf, 127, formats[0], std::localtime(&interval.end));
  else
    throw error(std::string("Could not determine end of period '") +
		optarg + "'");

  if (! config.predicate.empty())
    config.predicate += "&";
  config.predicate += "d<[";
  config.predicate += buf;
  config.predicate += "]";

  terminus = interval.end;
} OPT_END(end);

OPT_BEGIN(current, "c") {
  if (! config.predicate.empty())
    config.predicate += "&";
  config.predicate += "d<=m";
} OPT_END(current);

OPT_BEGIN(cleared, "C") {
  if (! config.predicate.empty())
    config.predicate += "&";
  config.predicate += "X";
} OPT_END(cleared);

OPT_BEGIN(uncleared, "U") {
  if (! config.predicate.empty())
    config.predicate += "&";
  config.predicate += "!X";
} OPT_END(uncleared);

OPT_BEGIN(real, "R") {
  if (! config.predicate.empty())
    config.predicate += "&";
  config.predicate += "R";
} OPT_END(real);

OPT_BEGIN(actual, "L") {
  if (! config.predicate.empty())
    config.predicate += "&";
  config.predicate += "L";
} OPT_END(actual);

//////////////////////////////////////////////////////////////////////
//
// Output customization

OPT_BEGIN(format, "F:") {
  config.format_string = optarg;
} OPT_END(format);

OPT_BEGIN(date_format, "y:") {
  config.date_format = optarg;
} OPT_END(date_format);

OPT_BEGIN(input_date_format, ":") {
  std::strcpy(input_format, optarg);
  formats[0] = input_format;
} OPT_END(input_date_format);

OPT_BEGIN(balance_format, ":") {
  config.balance_format = optarg;
} OPT_END(balance_format);

OPT_BEGIN(register_format, ":") {
  config.register_format = optarg;
} OPT_END(register_format);

OPT_BEGIN(wide_register_format, ":") {
  config.wide_register_format = optarg;
} OPT_END(wide_register_format);

OPT_BEGIN(plot_amount_format, ":") {
  config.plot_amount_format = optarg;
} OPT_END(plot_amount_format);

OPT_BEGIN(plot_total_format, ":") {
  config.plot_total_format = optarg;

OPT_BEGIN(effective, "") {
  transaction_t::use_effective_date = true;
} OPT_END(effective);
} OPT_END(plot_total_format);

OPT_BEGIN(print_format, ":") {
  config.print_format = optarg;
} OPT_END(print_format);

OPT_BEGIN(write_hdr_format, ":") {
  config.write_hdr_format = optarg;
} OPT_END(write_hdr_format);

OPT_BEGIN(write_xact_format, ":") {
  config.write_xact_format = optarg;
} OPT_END(write_xact_format);

OPT_BEGIN(equity_format, ":") {
  config.equity_format = optarg;
} OPT_END(equity_format);

OPT_BEGIN(prices_format, ":") {
  config.prices_format = optarg;
} OPT_END(prices_format);

OPT_BEGIN(wide, "w") {
  config.register_format = config.wide_register_format;
} OPT_END(wide);

OPT_BEGIN(head, ":") {
  config.head_entries = std::atoi(optarg);
} OPT_END(head);

OPT_BEGIN(tail, ":") {
  config.tail_entries = std::atoi(optarg);
} OPT_END(tail);

OPT_BEGIN(pager, ":") {
  config.pager = optarg;
} OPT_END(pager);

OPT_BEGIN(empty, "E") {
  config.show_empty = true;
} OPT_END(empty);

OPT_BEGIN(collapse, "n") {
  config.show_collapsed = true;
} OPT_END(collapse);

OPT_BEGIN(subtotal, "s") {
  config.show_subtotal = true;
} OPT_END(subtotal);

OPT_BEGIN(totals, "") {
  config.show_totals = true;
} OPT_END(totals);

OPT_BEGIN(sort, "S:") {
  config.sort_string = optarg;
} OPT_END(sort);

OPT_BEGIN(related, "r") {
  config.show_related = true;
} OPT_END(related);

OPT_BEGIN(period, "p:") {
  if (config.report_period.empty()) {
    config.report_period = optarg;
  } else {
    config.report_period += " ";
    config.report_period += optarg;
  }

  // If the period gives a beginning and/or ending date, make sure to
  // modify the calculation predicate (via the --begin and --end
  // options) to take this into account.

  char buf[128];
  interval_t interval(config.report_period);
  if (interval.begin) {
    std::strftime(buf, 127, formats[0], std::localtime(&interval.begin));

    if (! config.predicate.empty())
      config.predicate += "&";
    config.predicate += "d>=[";
    config.predicate += buf;
    config.predicate += "]";
  }
  if (interval.end) {
    std::strftime(buf, 127, formats[0], std::localtime(&interval.end));

    if (! config.predicate.empty())
      config.predicate += "&";
    config.predicate += "d<[";
    config.predicate += buf;
    config.predicate += "]";

    terminus = interval.end;
  }
} OPT_END(period);

OPT_BEGIN(period_sort, ":") {
  config.report_period_sort = optarg;
} OPT_END(period_sort);

OPT_BEGIN(weekly, "W") {
  if (config.report_period.empty())
    config.report_period = "weekly";
  else
    config.report_period = std::string("weekly ") + config.report_period;
} OPT_END(weekly);

OPT_BEGIN(monthly, "M") {
  if (config.report_period.empty())
    config.report_period = "monthly";
  else
    config.report_period = std::string("monthly ") + config.report_period;
} OPT_END(monthly);

OPT_BEGIN(yearly, "Y") {
  if (config.report_period.empty())
    config.report_period = "yearly";
  else
    config.report_period = std::string("yearly ") + config.report_period;
} OPT_END(yearly);

OPT_BEGIN(dow, "") {
  config.days_of_the_week = true;
} OPT_END(dow);

OPT_BEGIN(by_payee, "P") {
  config.by_payee = true;
} OPT_END(by_payee);

OPT_BEGIN(comm_as_payee, "x") {
  config.comm_as_payee = true;
} OPT_END(comm_as_payee);

OPT_BEGIN(budget, "") {
  config.budget_flags = BUDGET_BUDGETED;
} OPT_END(budget);

OPT_BEGIN(add_budget, "") {
  config.budget_flags = BUDGET_BUDGETED | BUDGET_UNBUDGETED;
} OPT_END(add_budget);

OPT_BEGIN(unbudgeted, "") {
  config.budget_flags = BUDGET_UNBUDGETED;
} OPT_END(unbudgeted);

OPT_BEGIN(forecast, ":") {
  config.forecast_limit = optarg;
} OPT_END(forecast);

OPT_BEGIN(reconcile, ":") {
  config.reconcile_balance = optarg;
} OPT_END(reconcile);

OPT_BEGIN(reconcile_date, ":") {
  config.reconcile_date = optarg;
} OPT_END(reconcile_date);

OPT_BEGIN(limit, "l:") {
  if (! config.predicate.empty())
    config.predicate += "&";
  config.predicate += "(";
  config.predicate += optarg;
  config.predicate += ")";
} OPT_END(limit);

OPT_BEGIN(display, "d:") {
  if (! config.display_predicate.empty())
    config.display_predicate += "&";
  config.display_predicate += "(";
  config.display_predicate += optarg;
  config.display_predicate += ")";
} OPT_END(display);

OPT_BEGIN(amount, "t:") {
  config.amount_expr = optarg;
} OPT_END(amount);

OPT_BEGIN(total, "T:") {
  config.total_expr = optarg;
} OPT_END(total);

OPT_BEGIN(amount_data, "j") {
  config.format_string = config.plot_amount_format;
} OPT_END(amount_data);


OPT_BEGIN(total_data, "J") {
  config.format_string = config.plot_total_format;
} OPT_END(total_data);


//////////////////////////////////////////////////////////////////////
//
// Commodity reporting

OPT_BEGIN(price_db, ":") {
  config.price_db = optarg;
} OPT_END(price_db);

OPT_BEGIN(price_exp, "Z:") {
  config.pricing_leeway = std::atol(optarg) * 60;
} OPT_END(price_exp);

OPT_BEGIN(download, "Q") {
  config.download_quotes = true;
} OPT_END(download);

OPT_BEGIN(quantity, "O") {
  config.amount_expr = "a";
  config.total_expr  = "O";
} OPT_END(quantity);

OPT_BEGIN(basis, "B") {
  config.amount_expr = "b";
  config.total_expr  = "B";
} OPT_END(basis);

OPT_BEGIN(market, "V") {
  config.show_revalued = true;

  config.amount_expr = "v";
  config.total_expr  = "V";
} OPT_END(market);

OPT_BEGIN(performance, "g") {
  config.amount_expr = "P(a,m)-b"; // same as 'g', but priced now
  config.total_expr  = "P(O,m)-B";
} OPT_END(performance);

OPT_BEGIN(gain, "G") {
  config.show_revalued      =
  config.show_revalued_only = true;

  config.amount_expr = "a";
  config.total_expr  = "G";
} OPT_END(gain);

OPT_BEGIN(average, "A") {
  config.total_expr_template = "A#";
} OPT_END(average);

OPT_BEGIN(deviation, "D") {
  config.total_expr_template = "t-A#";
} OPT_END(deviation);

OPT_BEGIN(percentage, "%") {
  config.total_expr_template = "^#&{100.0%}*(#/^#)";
} OPT_END(percentage);

#ifdef USE_BOOST_PYTHON

//////////////////////////////////////////////////////////////////////
//
// Python support

OPT_BEGIN(import, ":") {
  python_eval(std::string("import ") + optarg, PY_EVAL_STMT);
} OPT_END(import);

OPT_BEGIN(import_stdin, "") {
  python_eval(std::cin, PY_EVAL_MULTI);
} OPT_END(import_stdin);

#endif // USE_BOOST_PYTHON

} // namespace ledger

#ifdef USE_BOOST_PYTHON

#include <boost/python.hpp>
#include <boost/python/detail/api_placeholder.hpp>

using namespace boost::python;
using namespace ledger;

void py_process_options(config_t& config, const std::string& command,
			list args)
{
  strings_list strs;

  int l = len(args);
  for (int i = 0; i < l; i++)
    strs.push_back(std::string(extract<char *>(args[i])));

  config.process_options(command, strs.begin(), strs.end());
}

void add_other_option_handlers(const std::list<option_t>& other);

void py_add_config_option_handlers()
{
  add_other_option_handlers(config_options);
}

BOOST_PYTHON_FUNCTION_OVERLOADS(parse_ledger_data_overloads,
				parse_ledger_data, 1, 2)

void py_option_help()
{
  option_help(std::cout);
}

void export_config()
{
  class_< config_t > ("Config")
    .def_readwrite("init_file", &config_t::init_file)
    .def_readwrite("data_file", &config_t::data_file)
    .def_readwrite("cache_file", &config_t::cache_file)
    .def_readwrite("price_db", &config_t::price_db)
    .def_readwrite("output_file", &config_t::output_file)
    .def_readwrite("account", &config_t::account)
    .def_readwrite("predicate", &config_t::predicate)
    .def_readwrite("display_predicate", &config_t::display_predicate)
    .def_readwrite("report_period", &config_t::report_period)
    .def_readwrite("report_period_sort", &config_t::report_period_sort)
    .def_readwrite("format_string", &config_t::format_string)
    .def_readwrite("balance_format", &config_t::balance_format)
    .def_readwrite("register_format", &config_t::register_format)
    .def_readwrite("wide_register_format", &config_t::wide_register_format)
    .def_readwrite("plot_amount_format", &config_t::plot_amount_format)
    .def_readwrite("plot_total_format", &config_t::plot_total_format)
    .def_readwrite("print_format", &config_t::print_format)
    .def_readwrite("write_hdr_format", &config_t::write_hdr_format)
    .def_readwrite("write_xact_format", &config_t::write_xact_format)
    .def_readwrite("equity_format", &config_t::equity_format)
    .def_readwrite("prices_format", &config_t::prices_format)
    .def_readwrite("pricesdb_format", &config_t::pricesdb_format)
    .def_readwrite("date_format", &config_t::date_format)
    .def_readwrite("sort_string", &config_t::sort_string)
    .def_readwrite("amount_expr", &config_t::amount_expr)
    .def_readwrite("total_expr", &config_t::total_expr)
    .def_readwrite("total_expr_template", &config_t::total_expr_template)
    .def_readwrite("forecast_limit", &config_t::forecast_limit)
    .def_readwrite("reconcile_balance", &config_t::reconcile_balance)
    .def_readwrite("reconcile_date", &config_t::reconcile_date)
    .def_readwrite("budget_flags", &config_t::budget_flags)
    .def_readwrite("pricing_leeway", &config_t::pricing_leeway)
    .def_readwrite("show_collapsed", &config_t::show_collapsed)
    .def_readwrite("show_subtotal", &config_t::show_subtotal)
    .def_readwrite("show_totals", &config_t::show_totals)
    .def_readwrite("show_related", &config_t::show_related)
    .def_readwrite("show_all_related", &config_t::show_all_related)
    .def_readwrite("show_inverted", &config_t::show_inverted)
    .def_readwrite("show_empty", &config_t::show_empty)
    .def_readwrite("head_entries", &config_t::head_entries)
    .def_readwrite("tail_entries", &config_t::tail_entries)
    .def_readwrite("pager", &config_t::pager)
    .def_readwrite("days_of_the_week", &config_t::days_of_the_week)
    .def_readwrite("by_payee", &config_t::by_payee)
    .def_readwrite("comm_as_payee", &config_t::comm_as_payee)
    .def_readwrite("show_revalued", &config_t::show_revalued)
    .def_readwrite("show_revalued_only", &config_t::show_revalued_only)
    .def_readwrite("download_quotes", &config_t::download_quotes)
    .def_readwrite("use_cache", &config_t::use_cache)
    .def_readwrite("cache_dirty", &config_t::cache_dirty)

    .def("process_options", py_process_options)
    ;

  scope().attr("config") = ptr(&config);

  def("option_help", py_option_help);
  def("parse_ledger_data", parse_ledger_data, parse_ledger_data_overloads());
  def("add_config_option_handlers", py_add_config_option_handlers);
}

#endif // USE_BOOST_PYTHON
