#include "config.h"
#include "option.h"
#include "quotes.h"

#include <fstream>
#include <stdlib.h>

namespace ledger {

config_t	    config;
std::list<option_t> config_options;

config_t::config_t()
{
  if (const char * p = std::getenv("HOME"))
    init_file = cache_file = price_db = p;

  init_file  += "/.ledgerrc";
  cache_file += "/.ledger";
  price_db   += "/.pricedb";

  amount_expr	     = "a";
  total_expr	     = "O";
  pricing_leeway     = 24 * 3600;
  balance_format     = "%20T  %2_%-n\n";
  register_format    = ("%D %-.20P %-.22N %12.66t %12.80T\n%/"
			"%32|%-.22N %12.66t %12.80T\n");
  plot_amount_format = "%D %t\n";
  plot_total_format  = "%D %T\n";
  print_format       = "\n%D %X%C%P\n    %-34N  %12o\n%/    %-34N  %12o\n";
  equity_format      = "\n%D %X%C%P\n%/    %-34N  %12t\n";
  prices_format      = "%[%Y/%m/%d %H:%M:%S %Z]   %-10N %12t %12T\n";

  show_collapsed     = false;
  show_subtotal      = false;
  show_related       = false;
  show_all_related   = false;
  show_inverted      = false;
  show_empty	     = false;
  days_of_the_week   = false;
  show_revalued      = false;
  show_revalued_only = false;
  download_quotes    = false;

  use_cache	     = false;
  cache_dirty        = false;
}

static void
regexps_to_predicate(config_t& config,
		     std::list<std::string>::const_iterator begin,
		     std::list<std::string>::const_iterator end,
		     const bool account_regexp		= false,
		     const bool add_account_short_masks = false)
{
  std::string regexps[2];

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

    if (i == 1) {
      config.predicate += "!";
    }
    else if (add_account_short_masks) {
      if (regexps[i].find(':') != std::string::npos) {
	config.show_subtotal = true;
      } else {
	if (! config.display_predicate.empty())
	  config.display_predicate += "&";
	else if (! config.show_empty)
	  config.display_predicate += "T&";

	config.display_predicate += "///(?:";
	config.display_predicate += regexps[i];
	config.display_predicate += ")/";
      }
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

  if (command == "p" || command == "e") {
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

    regexps_to_predicate(*this, arg, i, true,
			 (command == "b" && ! show_subtotal &&
			  display_predicate.empty()));
    if (i != args_end)
      regexps_to_predicate(*this, i, args_end);
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
  }

  // Setup the values of %t and %T, used in format strings

  try {
    if (! format_t::amount_expr)
      format_t::amount_expr = parse_value_expr(amount_expr);
  }
  catch (const value_expr_error& err) {
    throw error(std::string("In amount expression (-t): ") + err.what());
  }

  try {
    if (! format_t::total_expr)
      format_t::total_expr = parse_value_expr(total_expr);
  }
  catch (const value_expr_error& err) {
    throw error(std::string("In total expression (-T): ") + err.what());
  }

  // If downloading is to be supported, configure the updater

  if (! commodity_t::updater && download_quotes)
    commodity_t::updater = new quotes_by_script(price_db, pricing_leeway,
						cache_dirty);

  if (! date_format.empty())
    format_t::date_format = date_format;
}

void parse_ledger_data(journal_t * journal,
		       parser_t *  text_parser,
		       parser_t *  cache_parser)
{
  int entry_count = 0;

  if (! config.init_file.empty() &&
      access(config.init_file.c_str(), R_OK) != -1) {
    if (parse_journal_file(config.init_file, journal))
      throw error("Entries not allowed in initialization file");
    journal->sources.pop_front(); // remove init file
  }

  if (cache_parser && config.use_cache &&
      ! config.cache_file.empty() &&
      ! config.data_file.empty()) {
    config.cache_dirty = true;
    if (access(config.cache_file.c_str(), R_OK) != -1) {
      std::ifstream stream(config.cache_file.c_str());
      if (cache_parser->test(stream)) {
	entry_count += cache_parser->parse(stream, journal, NULL,
					   &config.data_file);
	if (entry_count > 0)
	  config.cache_dirty = false;
      }
    }
  }

  if (entry_count == 0 && ! config.data_file.empty()) {
    account_t * account = NULL;
    if (! config.account.empty())
      account = journal->find_account(config.account);

    if (config.data_file == "-") {
      config.use_cache = false;
      entry_count += parse_journal(std::cin, journal, account);
    } else {
      entry_count += parse_journal_file(config.data_file, journal, account);
    }

    if (! config.price_db.empty() &&
	access(config.price_db.c_str(), R_OK) != -1)
      if (parse_journal_file(config.price_db, journal))
	throw error("Entries not allowed in price history file");
  }

  for (strings_list::iterator i = config.price_settings.begin();
       i != config.price_settings.end();
       i++) {
    std::string conversion = "C ";
    conversion += *i;
    int i = conversion.find('=');
    if (i != -1) {
      conversion[i] = ' ';
      std::istringstream stream(conversion);
      text_parser->parse(stream, journal, journal->master);
    }
  }

  if (entry_count == 0)
    throw error("Please specify ledger file using -f,"
		" or LEDGER_FILE environment variable.");

  VALIDATE(journal->valid());
}

static void show_version(std::ostream& out)
{
  out
    << "Ledger " << ledger::version << ", the command-line accounting tool\n\n"
    << "Copyright (c) 2003-2004, New Artisans LLC. All rights reserved.\n\n"
    << "This program is made available under the terms of the BSD Public\n"
    << "License.  See the LICENSE file included with the distribution for\n"
    << "details and disclaimer.\n";
}

void option_help(std::ostream& out)
{
  out
    << "usage: ledger [options] COMMAND [ACCT REGEX]... [-- [PAYEE REGEX]...]\n\n\
Basic options:\n\
  -h, --help             display this help text\n\
  -v, --version          show version information\n\
  -i, --init FILE        initialize ledger by loading FILE (def: ~/.ledgerrc)\n\
      --cache FILE       use FILE as a binary cache when --file is not used\n\
  -f, --file FILE        read ledger data from FILE\n\
  -o, --output FILE      write output to FILE\n\
  -z, --set-price CONV   specify a commodity conversion: \"COMM=AMOUNT\"\n\
  -a, --account NAME     use NAME as default account (useful with QIF files)\n\n\
Report filtering:\n\
  -b, --begin-date DATE  set report begin date\n\
  -e, --end-date DATE    set report end date\n\
  -c, --current          show only current and past entries (not future)\n\
  -C, --cleared          consider only cleared transactions\n\
  -U, --uncleared        consider only uncleared transactions\n\
  -R, --real             consider only non-virtual transactions\n\
  -r, --related          calculate report using related transactions\n\n\
Output customization:\n\
  -F, --format STR       use STR as the format; for each report type, use:\n\
			   --balance-format   --equity-format\n\
			   --register-format  --plot-amount-format\n\
			   --print-format     --plot-total-format\n\
			   --prices-format\n\
  -y, --date-format STR  use STR as the date format (def: %Y/%m/%d)\n\
  -E, --empty            balance: show accounts with zero balance\n\
  -n, --collapse         register: collapse entries with multiple transactions\n\
  -s, --subtotal         balance: show sub-accounts; register: show subtotals\n\
  -S, --sort EXPR        sort report according to the value expression EXPR\n\
  -p, --interval STR     report by interval (period), based on STR\n\
      --dow              show a days-of-the-week report\n\
  -W, --weekly           show weekly sub-totals\n\
  -M, --monthly          show monthly sub-totals\n\
  -Y, --yearly           show yearly sub-totals\n\
  -l, --limit EXPR       calculate only transactions matching EXPR\n\
  -d, --display EXPR     display only transactions matching EXPR\n\
  -t, --amount-expr EXPR set the amount expression for all report types\n\
  -T, --total-expr EXPR  set the total expression for all report types\n\
  -j, --amount-data      print only raw amount data (useful for scripting)\n\
  -J, --total-data       print only raw total data\n\n\
Commodity reporting:\n\
  -P, --price-db FILE    sets the price database to FILE (def: ~/.pricedb)\n\
  -L, --price-exp MINS   download quotes only if newer than MINS (def: 1440)\n\
  -Q, --download         download price information when needed\n\
  -O, --quantity         report commodity totals (this is the default)\n\
  -B, --basis            report commodity cost basis\n\
  -V, --market           report commodity market value\n\
  -G, --gain             report commodity gain/loss\n\
  -A, --average          report average transaction amount\n\
  -D, --deviation        report deviation from the average\n\
  -X, --trend            report average deviation from the average\n\
  -Z, --weighted-trend   same as trend, but older values are less significant\n\
			 (-D, -X and -Z make little sense in balance reports)\n\
Commands:\n\
  balance [REGEXP]...    show balance totals for matching accounts\n\
  register [REGEXP]...   show register of matching transactions\n\
  print [REGEXP]...      print all matching entries\n\
  equity [REGEXP]...     output equity entries for matching accounts\n\
  prices [REGEXP]...     display price history for matching commodities\n\
  entry DATE PAYEE AMT   output a derived entry, based on the arguments\n";
}

//////////////////////////////////////////////////////////////////////
//
// Basic options

OPT_BEGIN(help, "h") {
  option_help(std::cout);
  throw 0;
} OPT_END(help);

OPT_BEGIN(version, "v") {
  show_version(std::cout);
  throw 0;
} OPT_END(version);

OPT_BEGIN(init, "i:") {
  config.init_file = optarg;
} OPT_END(init);

OPT_BEGIN(file, "f:") {
  if (access(optarg, R_OK) != -1)
    config.data_file = optarg;
  else
    throw error(std::string("The ledger file '") + optarg +
		"' does not exist or is not readable");
} OPT_END(file);

OPT_BEGIN(cache, ":") {
  config.cache_file = optarg;
} OPT_END(cache);

OPT_BEGIN(output, "o:") {
  if (std::string(optarg) != "-")
    config.output_file = optarg;
} OPT_END(output);

OPT_BEGIN(set_price, "z:") {
  if (std::strchr(optarg, '='))
    config.price_settings.push_back(optarg);
  else
    throw error(std::string("Invalid price setting: ") + optarg);
} OPT_END(set_price);

OPT_BEGIN(account, "a:") {
  config.account = optarg;
} OPT_END(account);

//////////////////////////////////////////////////////////////////////
//
// Report filtering

OPT_BEGIN(begin_date, "b:") {
  if (! config.predicate.empty())
    config.predicate += "&";
  config.predicate += "d>=[";
  config.predicate += optarg;
  config.predicate += "]";
} OPT_END(begin_date);

OPT_BEGIN(end_date, "e:") {
  if (! config.predicate.empty())
    config.predicate += "&";
  config.predicate += "d<[";
  config.predicate += optarg;
  config.predicate += "]";
} OPT_END(end_date);

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

//////////////////////////////////////////////////////////////////////
//
// Output customization

OPT_BEGIN(format, "F:") {
  config.format_string = optarg;
} OPT_END(format);

OPT_BEGIN(date_format, "y:") {
  config.date_format = optarg;
} OPT_END(date_format);

OPT_BEGIN(balance_format, ":") {
  config.balance_format = optarg;
} OPT_END(balance_format);

OPT_BEGIN(register_format, ":") {
  config.register_format = optarg;
} OPT_END(register_format);

OPT_BEGIN(plot_amount_format, ":") {
  config.plot_amount_format = optarg;
} OPT_END(plot_amount_format);

OPT_BEGIN(plot_total_format, ":") {
  config.plot_total_format = optarg;
} OPT_END(plot_total_format);

OPT_BEGIN(print_format, ":") {
  config.print_format = optarg;
} OPT_END(print_format);

OPT_BEGIN(equity_format, ":") {
  config.equity_format = optarg;
} OPT_END(equity_format);

OPT_BEGIN(prices_format, ":") {
  config.prices_format = optarg;
} OPT_END(prices_format);

OPT_BEGIN(empty, "E") {
  config.show_empty = true;
} OPT_END(empty);

OPT_BEGIN(collapse, "n") {
  config.show_collapsed = true;
} OPT_END(collapse);

OPT_BEGIN(subtotal, "s") {
  config.show_subtotal = true;
} OPT_END(subtotal);

OPT_BEGIN(sort, "S:") {
  config.sort_string = optarg;
} OPT_END(sort);

OPT_BEGIN(related, "r") {
  config.show_related = true;
} OPT_END(related);

OPT_BEGIN(interval, "p:") {
  config.report_interval = optarg;
} OPT_END(interval);

OPT_BEGIN(weekly, "W") {
  config.report_interval = "weekly";
} OPT_END(weekly);

OPT_BEGIN(dow, "") {
  config.days_of_the_week = true;
} OPT_END(dow);

OPT_BEGIN(monthly, "M") {
  config.report_interval = "monthly";
} OPT_END(monthly);

OPT_BEGIN(yearly, "Y") {
  config.report_interval = "yearly";
} OPT_END(yearly);

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

OPT_BEGIN(amount_expr, "t:") {
  config.amount_expr = optarg;
} OPT_END(amount_expr);

OPT_BEGIN(total_expr, "T:") {
  config.total_expr = optarg;
} OPT_END(total_expr);

OPT_BEGIN(amount_data, "j") {
  config.amount_expr   = "S" + config.amount_expr;
  config.format_string = config.plot_amount_format;
} OPT_END(amount_data);

OPT_BEGIN(total_data, "J") {
  config.total_expr    = "S" + config.total_expr;
  config.format_string = config.plot_total_format;
} OPT_END(total_data);

//////////////////////////////////////////////////////////////////////
//
// Commodity reporting

OPT_BEGIN(price_db, "P:") {
  config.price_db = optarg;
} OPT_END(price_db);

OPT_BEGIN(price_exp, "L:") {
  config.pricing_leeway = std::atol(optarg) * 60;
} OPT_END(price_exp);

OPT_BEGIN(download, "Q") {
  config.download_quotes = true;
} OPT_END(download);

OPT_BEGIN(quantity, "O") {
  config.amount_expr = "a";
  config.total_expr = "O";
} OPT_END(quantity);

OPT_BEGIN(basis, "B") {
  config.amount_expr = "c";
  config.total_expr = "C";
} OPT_END(basis);

OPT_BEGIN(market, "V") {
  config.show_revalued = true;

  config.amount_expr = "v";
  config.total_expr = "V";
} OPT_END(market);

OPT_BEGIN(gain, "G") {
  config.show_revalued      =
  config.show_revalued_only = true;

  config.amount_expr = "a";
  config.total_expr = "G";
} OPT_END(gain);

OPT_BEGIN(average, "A") {
  config.total_expr = std::string("M") + config.total_expr;
} OPT_END(average);

OPT_BEGIN(deviation, "D") {
  config.total_expr = std::string("DM") + config.total_expr;
} OPT_END(deviation);

OPT_BEGIN(trend, "X") {
  config.total_expr = std::string("MDM") + config.total_expr;
} OPT_END(trend);

OPT_BEGIN(weighted_trend, "Z") {
  config.total_expr = (std::string("MD(M(") + config.total_expr +
		       ")/(1+(((m-d)/(30*86400))<0?0:((m-d)/(30*86400)))))");
} OPT_END(weighted_trend);

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
				parse_ledger_data, 2, 3)

void py_option_help()
{
  option_help(std::cout);
}

void export_config()
{
  class_< config_t > ("Config")
    .def_readwrite("price_settings", &config_t::price_settings)
    .def_readwrite("init_file", &config_t::init_file)
    .def_readwrite("data_file", &config_t::data_file)
    .def_readwrite("cache_file", &config_t::cache_file)
    .def_readwrite("price_db", &config_t::price_db)
    .def_readwrite("output_file", &config_t::output_file)
    .def_readwrite("account", &config_t::account)
    .def_readwrite("predicate", &config_t::predicate)
    .def_readwrite("display_predicate", &config_t::display_predicate)
    .def_readwrite("report_interval", &config_t::report_interval)
    .def_readwrite("format_string", &config_t::format_string)
    .def_readwrite("balance_format", &config_t::balance_format)
    .def_readwrite("register_format", &config_t::register_format)
    .def_readwrite("plot_amount_format", &config_t::plot_amount_format)
    .def_readwrite("plot_total_format", &config_t::plot_total_format)
    .def_readwrite("print_format", &config_t::print_format)
    .def_readwrite("equity_format", &config_t::equity_format)
    .def_readwrite("prices_format", &config_t::prices_format)
    .def_readwrite("date_format", &config_t::date_format)
    .def_readwrite("sort_string", &config_t::sort_string)
    .def_readwrite("amount_expr", &config_t::amount_expr)
    .def_readwrite("total_expr", &config_t::total_expr)
    .def_readwrite("pricing_leeway", &config_t::pricing_leeway)
    .def_readwrite("show_collapsed", &config_t::show_collapsed)
    .def_readwrite("show_subtotal", &config_t::show_subtotal)
    .def_readwrite("show_related", &config_t::show_related)
    .def_readwrite("show_all_related", &config_t::show_all_related)
    .def_readwrite("show_inverted", &config_t::show_inverted)
    .def_readwrite("show_empty", &config_t::show_empty)
    .def_readwrite("days_of_the_week", &config_t::days_of_the_week)
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
