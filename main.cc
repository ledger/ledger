#include "ledger.h"
#include "error.h"
#include "valexpr.h"
#include "format.h"
#include "walk.h"
#include "option.h"
#include "timing.h"

#include <fstream>
#include <cstring>
#include <unistd.h>
#include <ctime>

namespace {

using namespace ledger;

const std::string bal_fmt = "%20T  %2_%-n\n";
const std::string reg_fmt
  = "%D %-.20P %-.22N %12.66t %12.80T\n\
%/                                %-.22N %12.66t %12.80T\n";
const std::string plot_value_fmt = "%D %t\n";
const std::string plot_total_fmt = "%D %T\n";
const std::string print_fmt
  = "\n%D %X%C%P\n    %-34N  %12o\n%/    %-34N  %12o\n";
const std::string equity_fmt
  = "\n%D %X%C%P\n%/    %-34N  %12t\n";

std::auto_ptr<journal_t>    journal(new journal_t);
std::list<std::string>      files;
std::auto_ptr<value_expr_t> sort_order;
std::auto_ptr<std::ostream> output_stream;
std::auto_ptr<interval_t>   report_interval;

#define OUT() (output_stream.get() ? *output_stream.get() : std::cout)

std::string   init_file;
std::string   cache_file;
std::string   price_db;
std::string   predicate;
std::string   display_predicate;
std::string   format_string;
std::string   sort_string;
std::string   value_expr     = "a";
std::string   total_expr     = "T";
std::time_t   interval_begin = 0;
unsigned long pricing_leeway = 24 * 3600;

bool cache_dirty        = true;
bool show_subtotals	= true;
bool show_expanded	= false;
bool show_related	= false;
bool show_all_related   = false;
bool show_inverted	= false;
bool show_empty		= false;
bool days_of_the_week   = false;
bool show_revalued      = false;
bool show_revalued_only = false;

void download_price_quote(commodity_t *	    commodity,
			  const std::time_t age,
			  const amount_t&   price,
			  const std::time_t moment)
{
  std::time_t now = std::time(NULL); // the time of the query

  if (! (commodity->flags & COMMODITY_STYLE_CONSULTED) &&
      std::difftime(now, moment) < pricing_leeway &&
      (! price || std::difftime(moment, age) > pricing_leeway)) {
    using namespace std;

    // Only consult the Internet once for any commodity
    commodity->flags |= COMMODITY_STYLE_CONSULTED;
    cache_dirty = true;

    char buf[256];
    buf[0] = '\0';

    if (FILE * fp = popen((string("getquote ") +
			   commodity->symbol).c_str(), "r")) {
      if (feof(fp) || ! fgets(buf, 255, fp)) {
	fclose(fp);
	return;
      }
      fclose(fp);
    }

    if (buf[0]) {
      char * p = strchr(buf, '\n');
      if (p) *p = '\0';

      amount_t current;
      current.parse(buf);

      commodity->add_price(now, current);

      if (! price_db.empty()) {
	char buf[128];
	strftime(buf, 127, "%Y/%m/%d %H:%M:%S", localtime(&now));
	ofstream database(price_db.c_str(), ios_base::out | ios_base::app);
	database << "P " << buf << " " << commodity->symbol << " "
		 << current << endl;
      }
    }
  }
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

static void show_help(std::ostream& out)
{
  out
    << "usage: ledger [options] COMMAND [options] [REGEXPS]\n\n\
Basic options:\n\
  -h, --help            display this help text\n\
  -v, --version         display version information\n\
  -i, --init FILE       initialize ledger by loading FILE\n\
  -f, --file FILE       specify pathname of ledger data file\n\
  -o, --output FILE     write all output to FILE\n\
  -p, --set-price CONV  specifies commodity conversion: COMM=AMOUNT\n\n\
Report filtering:\n\
  -b, --begin-date DATE specify a beginning date\n\
  -e, --end-date DATE   specify an ending date\n\
  -c, --current         do not show future entries (same as -e TODAY)\n\
  -C, --cleared         show only cleared transactions and balances\n\
  -U, --uncleared       show only uncleared transactions and balances\n\
  -R, --real            do not consider virtual transactions: real only\n\n\
Output customization:\n\
  -F, --format STR      \n\
  -y, --date-format STR \n\
  -E, --empty           balance: also show accounts that total to zero\n\
  -n, --collapse        balance: no parent account totals; register: collapse\n\
  -s, --show-all        balance: show sub-accounts; register: show subtotals\n\
  -S, --sort EXPR       sort report according to value EXPR\n\
  -r, --related         \n\
  -z, --interval EXPR   \n\
  -w, --dow             print register using day of week sub-totals\n\
  -W, --weekly                 \"   \"         weekly sub-totals\n\
  -M, --monthly                \"   \"         monthly sub-totals\n\
  -Y, --yearly                 \"   \"         yearly sub-totals\n\
  -l, --limit EXPR      don't calculate entries for which EXPR yields 0\n\
  -d, --display EXPR    don't print entries for which EXPR yields 0\n\
  -t, --value EXPR      \n\
  -T, --total EXPR      \n\
  -j, --value-data      \n\
  -J, --total-data      \n\n\
Commodity reporting:\n\
  -P, --price-db FILE   sets the price database\n\
  -L, --price-exp MINS  with -Q, fetch quotes only if data is older than MINS\n\
  -Q, --download        download price information from the Internet\n\
			(works by running \"getquote SYMBOL\")\n\
  -O, --quantity        \n\
  -B, --basis           report cost basis of commodities\n\
  -V, --market          report the market value of commodities\n\
  -G, --gain            \n\
  -A, --average         \n\
  -D, --deviation       \n\
  -X, --trend           \n\
  -Z, --weighted-trend  \n\n\
Commands:\n\
  balance       show balance totals\n\
  register      display a register for ACCOUNT\n\
  print         print all ledger entries\n\
  entry         output a newly formed entry, based on arguments\n\
  equity        output equity entries for specified accounts\n";
}


//////////////////////////////////////////////////////////////////////
//
// Basic options

DEF_OPT_HANDLERS();

OPT_BEGIN(help, "h", false) {
  show_help(std::cout);
  std::exit(0);
} OPT_END(help);

OPT_BEGIN(version, "v", false) {
  show_version(std::cout);
  std::exit(0);
} OPT_END(version);

OPT_BEGIN(init, "i:", true) {
  init_file = optarg;
} OPT_END(init);

OPT_BEGIN(file, "f:", true) {
  char * buf = new char[std::strlen(optarg) + 1];
  std::strcpy(buf, optarg);
  for (char * p = std::strtok(buf, ":");
       p;
       p = std::strtok(NULL, ":"))
    files.push_back(p);
  delete[] buf;
} OPT_END(file);

OPT_BEGIN(cache, ":", false) {
  cache_file = optarg;
} OPT_END(cache);

OPT_BEGIN(output, "o:", false) {
  if (std::string(optarg) != "-")
    output_stream.reset(new std::ofstream(optarg));
} OPT_END(output);

OPT_BEGIN(set_price, "p:", true) {
  // jww (2004-08-14): fix this relative to the other file settings
  if (char * p = std::strchr(optarg, '=')) {
    *p = ' ';
    std::string conversion = "C ";
    conversion += p;
    std::istringstream stream(conversion);
    parse_textual_journal(stream, journal.get(), journal->master);
  } else {
    std::cerr << "Error: Invalid price setting: " << optarg
	      << std::endl;
    std::exit(1);
  }
} OPT_END(set_price);

//////////////////////////////////////////////////////////////////////
//
// Report filtering

OPT_BEGIN(begin_date, "b:", false) {
  if (! predicate.empty())
    predicate += "&";
  predicate += "(d>=[";
  predicate += optarg;
  predicate += "])";
} OPT_END(begin_date);

OPT_BEGIN(end_date, "e:", false) {
  if (! predicate.empty())
    predicate += "&";
  predicate += "(d<[";
  predicate += optarg;
  predicate += "])";
} OPT_END(end_date);

OPT_BEGIN(current, "c", false) {
  if (! predicate.empty())
    predicate += "&";
  predicate += "(d<t)";
} OPT_END(current);

OPT_BEGIN(cleared, "C", false) {
  if (! predicate.empty())
    predicate += "&";
  predicate += "X";
} OPT_END(cleared);

OPT_BEGIN(uncleared, "U", false) {
  if (! predicate.empty())
    predicate += "&";
  predicate += "!X";
} OPT_END(uncleared);

OPT_BEGIN(real, "R", false) {
  if (! predicate.empty())
    predicate += "&";
  predicate += "R";
} OPT_END(real);

//////////////////////////////////////////////////////////////////////
//
// Output customization

OPT_BEGIN(format, "F:", false) {
  format_string = optarg;
} OPT_END(format);

OPT_BEGIN(date_format, "y:", false) {
  format_t::date_format = optarg;
} OPT_END(date_format);

OPT_BEGIN(empty, "E", false) {
  show_empty = true;
} OPT_END(empty);

OPT_BEGIN(collapse, "n", false) {
  show_subtotals = false;
} OPT_END(collapse);

OPT_BEGIN(show_all, "s", false) {
  show_expanded = true;
} OPT_END(show_all);

OPT_BEGIN(sort, "S:", false) {
  sort_string = optarg;
} OPT_END(sort);

OPT_BEGIN(related, "r", false) {
  show_related = true;
} OPT_END(related);

OPT_BEGIN(interval, "z:", false) {
  std::string str(optarg);
  std::istringstream stream(str);
  report_interval.reset(interval_t::parse(stream));

  if (! stream.eof()) {
    std::string word;
    stream >> word;
    if (word == "from") {
      stream >> word;
      if (! parse_date(word.c_str(), &interval_begin))
	throw interval_expr_error("Could not parse 'from' date");
    }
  }
} OPT_END(interval);

OPT_BEGIN(weekly, "W", false) {
  report_interval.reset(new interval_t(604800, 0, 0));
} OPT_END(weekly);

OPT_BEGIN(dow, "w", false) {
  days_of_the_week = true;
} OPT_END(dow);

OPT_BEGIN(monthly, "M", false) {
  report_interval.reset(new interval_t(0, 1, 0));
} OPT_END(monthly);

OPT_BEGIN(yearly, "Y", false) {
  report_interval.reset(new interval_t(0, 0, 1));
} OPT_END(yearly);

OPT_BEGIN(limit, "l:", false) {
  if (! predicate.empty())
    predicate += "&";
  predicate += "(";
  predicate += optarg;
  predicate += ")";
} OPT_END(limit);

OPT_BEGIN(display, "d:", false) {
  if (! display_predicate.empty())
    display_predicate += "&";
  display_predicate += "(";
  display_predicate += optarg;
  display_predicate += ")";
} OPT_END(display);

OPT_BEGIN(value, "t:", false) {
  value_expr = optarg;
} OPT_END(value);

OPT_BEGIN(total, "T:", false) {
  total_expr = optarg;
} OPT_END(total);

OPT_BEGIN(value_data, "j", false) {
  value_expr    = "S" + value_expr;
  format_string = plot_value_fmt;
} OPT_END(value_data);

OPT_BEGIN(total_data, "J", false) {
  total_expr    = "S" + total_expr;
  format_string = plot_total_fmt;
} OPT_END(total_data);

//////////////////////////////////////////////////////////////////////
//
// Commodity reporting

OPT_BEGIN(price_db, "P:", false) {
  price_db = optarg;
} OPT_END(price_db);

OPT_BEGIN(price_exp, "L:", false) {
  pricing_leeway = std::atol(optarg) * 60;
} OPT_END(price_exp);

OPT_BEGIN(download, "Q", false) {
  commodity_t::updater = download_price_quote;
} OPT_END(download);

OPT_BEGIN(quantity, "O", false) {
  value_expr = "a";
  total_expr = "T";
} OPT_END(quantity);

OPT_BEGIN(basis, "B", false) {
  value_expr = "c";
  total_expr = "C";
} OPT_END(basis);

OPT_BEGIN(market, "V", false) {
  show_revalued = true;

  value_expr = "v";
  total_expr = "V";
} OPT_END(market);

OPT_BEGIN(gain, "G", false) {
  show_revalued      =
  show_revalued_only = true;

  value_expr = "a";
  total_expr = "G";
} OPT_END(gain);

OPT_BEGIN(average, "A", false) {
  value_expr = "a";
  total_expr = "MT";
} OPT_END(average);

OPT_BEGIN(deviation, "D", false) {
  value_expr = "a";
  total_expr = "DMT";
} OPT_END(deviation);

OPT_BEGIN(trend, "X", false) {
  value_expr = "a";
  total_expr = "MDMT";
} OPT_END(trend);

OPT_BEGIN(weighted_trend, "Z", false) {
  value_expr = "a";
  total_expr = "MD(MT/(1+(((t-d)/(30*86400))<0?0:((t-d)/(30*86400)))))";
} OPT_END(weighted_trend);


TIMER_DEF(write_cache,	  "writing cache file");
TIMER_DEF(report_gen,	  "generation of final report");
TIMER_DEF(handle_options, "configuring based on options");
TIMER_DEF(parse_files,	  "parsing ledger files");
TIMER_DEF(process_env,	  "processing environment");
TIMER_DEF(process_args,   "processing command-line arguments");
TIMER_DEF(read_cache,	  "reading cache file");

} // namespace

int main(int argc, char * argv[], char * envp[])
{
#ifdef DEBUG_ENABLED
  // If debugging is enabled, and DEBUG_FILE is set, then all
  // debugging output goes to that file.

  if (const char * p = std::getenv("DEBUG_FILE")) {
    debug_stream      = new std::ofstream(p);
    free_debug_stream = true;
  }
#endif

  // Initialize default paths

  if (const char * p = std::getenv("HOME"))
    init_file = cache_file = price_db = p;

  init_file  += "/.ledgerrc";
  cache_file += "/.ledger";
  price_db   += "/.pricedb";

  // Parse command-line arguments

  TIMER_START(process_args);

  std::list<std::string> args;

  process_arguments(argc, argv, false, args);

  if (args.empty()) {
    show_help(std::cerr);
    return 1;
  }
  std::list<std::string>::iterator arg = args.begin();

  TIMER_STOP(process_args);

  const bool use_cache = files.empty();

  // Process options from the environment

  TIMER_START(process_env);

  process_environment(envp, "LEDGER_");

  if (const char * p = std::getenv("LEDGER"))
    process_option("file", p);
  if (const char * p = std::getenv("PRICE_HIST"))
    process_option("price-db", p);
  if (const char * p = std::getenv("PRICE_EXP"))
    process_option("price-exp", p);

  TIMER_STOP(process_env);

  // Parse ledger files

  TIMER_START(parse_files);

  int entry_count = 0;

  try {
    if (! init_file.empty())
      if (parse_journal_file(init_file, journal.get()))
	throw error("Entries not allowed in initialization file");

    if (use_cache && ! cache_file.empty()) {
      journal->sources.clear();		// remove init_file
      entry_count += parse_journal_file(cache_file, journal.get());
      journal->sources.pop_front();	// remove cache_file

      std::list<std::string> exceptions;
      std::set_difference(journal->sources.begin(), journal->sources.end(),
			  files.begin(), files.end(), exceptions.begin());

      if (entry_count == 0 || exceptions.size() > 0) {
	journal.reset(new journal_t);
	entry_count = 0;
      } else {
	cache_dirty = false;
      }
    }

    if (entry_count == 0)
      for (std::list<std::string>::iterator i = files.begin();
	   i != files.end(); i++)
	entry_count += parse_journal_file(*i, journal.get());

    if (! price_db.empty())
      if (parse_journal_file(price_db, journal.get()))
	throw error("Entries not allowed in price history file");
  }
  catch (error& err) {
    std::cerr << "Fatal: " << err.what() << std::endl;
    return 1;
  }

  if (entry_count == 0) {
    std::cerr << ("Please specify ledger file(s) using -f option "
		  "or LEDGER environment variable.") << std::endl;
    return 1;
  }

  TIMER_STOP(parse_files);

  // Read the command word, and then check and simplify it

  std::string command = *arg++;

  TIMER_START(handle_options);

  if (command == "balance" || command == "bal" || command == "b")
    command = "b";
  else if (command == "register" || command == "reg" || command == "r")
    command = "r";
  else if (command == "print" || command == "p")
    command = "p";
  else if (command == "entry")
    command = "e";
  else if (command == "equity")
    command = "E";
  else {
    std::cerr << "Error: Unrecognized command '" << command << "'."
	      << std::endl;
    return 1;
  }

  // Process the remaining command-line arguments

  std::auto_ptr<entry_t> new_entry;
  if (command == "e") {
    new_entry.reset(journal->derive_entry(arg, args.end()));
  } else {
    // Treat the remaining command-line arguments as regular
    // expressions, used for refining report results.

    std::list<std::string>::iterator i = args.begin();
    for (; i != args.end(); i++)
      if (*i == "--")
	break;

    std::string pred = regexps_to_predicate(arg, i);
    if (! pred.empty()) {
      if (! predicate.empty())
	predicate += "&";
      predicate += pred;
    }

    if (i != args.end()) {
      std::string pred = regexps_to_predicate(i, args.end(), false);
      if (! pred.empty()) {
	if (! predicate.empty())
	  predicate += "&";
	predicate += pred;
      }
    }
  }

  // Compile the predicates

  if (display_predicate.empty()) {
    if (command == "b") {
      if (! show_empty)
	display_predicate = "T";

      if (! show_expanded && predicate.empty()) {
	if (! display_predicate.empty())
	  display_predicate += "&";
	display_predicate += "!n";
      }
    }
    else if (command == "E") {
      display_predicate = "a";
    }
  }

  // Compile the sorting criteria

  if (! sort_string.empty()) {
    try {
      std::istringstream stream(sort_string);
      sort_order.reset(parse_value_expr(stream));
      if (stream.peek() != -1) {
	std::ostringstream err;
	err << "Unexpected character '" << char(stream.peek()) << "'";
	throw value_expr_error(err.str());
      }
      else if (! sort_order.get()) {
	std::cerr << "Failed to parse sort criteria!" << std::endl;
	return 1;
      }
    }
    catch (const value_expr_error& err) {
      std::cerr << "Error in sort criteria: " << err.what() << std::endl;
      return 1;
    }
  }

  // Setup the meaning of %t and %T, used in format strings

  try {
    format_t::value_expr.reset(parse_value_expr(value_expr));
  }
  catch (const value_expr_error& err) {
    std::cerr << "Error in amount (-t) specifier: " << err.what()
	      << std::endl;
    return 1;
  }

  try {
    format_t::total_expr.reset(parse_value_expr(total_expr));
  }
  catch (const value_expr_error& err) {
    std::cerr << "Error in total (-T) specifier: " << err.what()
	      << std::endl;
    return 1;
  }

  // Configure some option depending on the report type

  if (command == "p" || command == "e") {
    show_related = show_all_related = true;
    show_expanded = true;
  }
  else if (command == "E") {
    show_expanded = true;
  }
  else if (show_related) {
    if (command == "r")
      show_inverted = true;
    else
      show_all_related = true;
  }

  // Compile the format strings

  const char * f;
  if (! format_string.empty())
    f = format_string.c_str();
  else if (command == "b")
    f = bal_fmt.c_str();
  else if (command == "r")
    f = reg_fmt.c_str();
  else if (command == "E")
    f = equity_fmt.c_str();
  else
    f = print_fmt.c_str();

  std::string first_line_format;
  std::string next_lines_format;

  if (const char * p = std::strstr(f, "%/")) {
    first_line_format = std::string(f, 0, p - f);
    next_lines_format = std::string(p + 2);
  } else {
    first_line_format = next_lines_format = f;
  }

  format_t format(first_line_format);
  format_t nformat(next_lines_format);

  TIMER_STOP(handle_options);

  // Walk the entries based on the report type and the options

  TIMER_START(report_gen);

  if (command == "b") {
    std::auto_ptr<item_handler<transaction_t> > formatter;
    formatter.reset(new add_to_account_value);
    if (show_related)
      formatter.reset(new related_transactions(formatter.release(),
					       show_all_related));
    formatter.reset(new filter_transactions(formatter.release(), predicate));
    walk_entries(journal->entries, *formatter.get());

    format_account acct_formatter(OUT(), format, display_predicate);
    if (show_subtotals)
      sum_accounts(journal->master);
    walk_accounts(journal->master, acct_formatter, sort_order.get());

    if (format_account::disp_subaccounts_p(journal->master)) {
      std::string end_format = "--------------------\n";
      format.reset(end_format + f);
      format.format_elements(OUT(), details_t(journal->master));
    }
  }
  else if (command == "E") {
    std::auto_ptr<item_handler<transaction_t> > formatter;
    formatter.reset(new add_to_account_value);
    formatter.reset(new filter_transactions(formatter.release(), predicate));
    walk_entries(journal->entries, *formatter.get());

    format_equity acct_formatter(OUT(), format, nformat, display_predicate);
    sum_accounts(journal->master);
    walk_accounts(journal->master, acct_formatter, sort_order.get());
  }
  else if (command == "e") {
    format_transactions formatter(OUT(), format, nformat);
    walk_transactions(new_entry->transactions, formatter);
  }
  else {
    std::auto_ptr<item_handler<transaction_t> > formatter;

    // Stack up all the formatter needed to fulfills the user's
    // requests.  Some of these are order dependent, in terms of
    // whether calc_transactions occurs before or after them.

    // format_transactions write each transaction received to the
    // output stream.
    formatter.reset(new format_transactions(OUT(), format, nformat));

    // sort_transactions will sort all the transactions it sees, based
    // on the `sort_order' value expression.
    if (sort_order.get())
      formatter.reset(new sort_transactions(formatter.release(),
					    sort_order.get()));

    // filter_transactions will only pass through transactions
    // matching the `display_predicate'.
    formatter.reset(new filter_transactions(formatter.release(),
					    display_predicate));

    // calc_transactions computes the running total.  When this
    // appears will determine, for example, whether filtered
    // transactions are included or excluded from the running total.
    formatter.reset(new calc_transactions(formatter.release(), show_inverted));

    // changed_value_transactions adds virtual transactions to the
    // list to account for changes in market value of commodities,
    // which otherwise would affect the running total unpredictably.
    if (show_revalued)
      formatter.reset(new changed_value_transactions(formatter.release(),
						     show_revalued_only));

    // collapse_transactions causes entries with multiple transactions
    // to appear as entries with a subtotaled transaction for each
    // commodity used.
    if (! show_subtotals)
      formatter.reset(new collapse_transactions(formatter.release()));

    // subtotal_transactions combines all the transactions it receives
    // into one subtotal entry, which has one transaction for each
    // commodity in each account.
    //
    // interval_transactions is like subtotal_transactions, but it
    // subtotals according to time intervals rather than totalling
    // everything.
    //
    // dow_transactions is like interval_transactions, except that it
    // reports all the transactions that fall on each subsequent day
    // of the week.
    if (show_expanded)
      formatter.reset(new subtotal_transactions(formatter.release()));
    else if (report_interval.get())
      formatter.reset(new interval_transactions(formatter.release(),
						*report_interval.get(),
						interval_begin));
    else if (days_of_the_week)
      formatter.reset(new dow_transactions(formatter.release()));

    // related_transactions will pass along all transactions related
    // to the transaction received.  If `show_all_related' is true,
    // then all the entry's transactions are passed; meaning that if
    // one transaction of an entry is to be printed, all the
    // transaction for that entry will be printed.
    if (show_related)
      formatter.reset(new related_transactions(formatter.release(),
					       show_all_related));

    // This filter_transactions will only pass through transactions
    // matching the `predicate'.
    formatter.reset(new filter_transactions(formatter.release(), predicate));

    // Once the filters are chained, walk `journal's entries and start
    // feeding each transaction that matches `predicate' to the chain.
    walk_entries(journal->entries, *formatter.get());

#ifdef DEBUG_ENABLED
    // The transaction display flags (dflags) are not recorded in the
    // binary cache, and only need to be cleared if the transactions
    // are to be displayed a second time.
    clear_display_flags cleanup;
    walk_entries(journal->entries, cleanup);
#endif
  }

  TIMER_STOP(report_gen);

  // Save the cache, if need be

  TIMER_START(write_cache);

  if (use_cache && cache_dirty && ! cache_file.empty()) {
    std::ofstream stream(cache_file.c_str());
    write_binary_journal(stream, journal.get(), &journal->sources);
  }

  TIMER_STOP(write_cache);

  return 0;
}

// main.cc ends here.
