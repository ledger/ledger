#include "ledger.h"
#include "error.h"
#include "valexpr.h"
#include "format.h"
#include "walk.h"

#include <fstream>
#include <cstring>
#include <unistd.h>
#include <ctime>

namespace ledger {

static const std::string bal_fmt = "%20T  %2_%-n\n";

static const std::string reg_fmt
  = "%10d %-.20p %-.22N %12.66t %12.80T\n\
%/                                %-.22N %12.66t %12.80T\n";

static const std::string print_fmt
  = "\n%10d %X%C%p\n    %-34N  %12o\n%/    %-34N  %12o\n";

static const std::string equity_fmt
  = "\n%10d %X%C%p\n%/    %-34N  %12t\n";


void set_price_conversion(const std::string& setting)
{
  char buf[128];
  std::strcpy(buf, setting.c_str());

  assert(setting.length() < 128);

  char * c = buf;
  char * p = std::strchr(buf, '=');
  if (! p) {
    std::cerr << "Warning: Invalid price setting: " << setting << std::endl;
  } else {
    *p++ = '\0';

    amount_t price;
    price.parse(p);

    commodity_t * commodity = commodity_t::find_commodity(c, true);
    commodity->set_conversion(price);
  }
}


static long	   pricing_leeway = 24 * 3600;
static std::string price_db;
static bool        cache_dirty    = false;

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

} // namespace ledger


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
    << "usage: ledger [options] COMMAND [options] [REGEXPS]\n\n"
    << "Basic options:\n"
    << "  -h        display this help text\n"
    << "  -v        display version information\n"
    << "  -f FILE   specify pathname of ledger data file\n\n"
    << "Report filtering:\n"
    << "  -a REGEX  specify an account regex for \"print\"\n"
    << "  -b DATE   specify a beginning date\n"
    << "  -e DATE   specify an ending date\n"
    << "  -c        do not show future entries (same as -e TODAY)\n"
    << "  -d DATE   specify a date mask ('-d mon', for all mondays)\n"
    << "  -C        show only cleared transactions and balances\n"
    << "  -U        show only uncleared transactions and balances\n"
    << "  -R        do not consider virtual transactions: real only\n"
    << "  -l EXPR   don't print entries for which EXPR yields 0\n\n"
    << "Customizing output:\n"
    << "  -n        do not calculate parent account totals\n"
    << "  -s        show sub-accounts in balance, and splits in register\n"
    << "  -M        print register using monthly sub-totals\n"
    << "  -E        show accounts that total to zero\n"
    << "  -S EXPR   sort entry output based on EXPR\n\n"
    << "Commodity prices:\n"
    << "  -T        report commodity totals, not their market value\n"
    << "  -B        report cost basis of commodities\n"
    << "  -V        report the market value of commodities\n"
    << "  -P FILE   sets the price database, for reading/writing price info\n"
    << "  -Q        download price information from the Internet\n"
    << "            (works by running \"getquote SYMBOL\")\n"
    << "  -L MINS   with -Q, fetch quotes only if data is older than MINS\n"
    << "  -p STR    specifies a direct commodity conversion: COMM=AMOUNT\n\n"
    << "Commands:\n"
    << "  balance   show balance totals\n"
    << "  register  display a register for ACCOUNT\n"
    << "  print     print all ledger entries\n"
    << "  entry     output a newly formed entry, based on arguments\n"
    << "  equity    output equity entries for specified accounts\n";
}

int main(int argc, char * argv[])
{
  using namespace ledger;

  std::auto_ptr<journal_t> journal(new journal_t);
  std::list<std::string>   files;
  std::auto_ptr<value_expr_t>    sort_order;

  std::string predicate;
  std::string display_predicate;
  std::string format_string;
  std::string sort_string;
  std::string value_expr = "a";
  std::string total_expr = "T";

  bool show_subtotals = true;
  bool show_expanded  = false;
  bool show_related   = false;
  bool show_inverted  = false;
  bool show_empty     = false;

  bool show_revalued      = false;
  bool show_revalued_only = false;

#ifdef DEBUG_ENABLED
  if (char * p = std::getenv("DEBUG_FILE")) {
    debug_stream = new std::ofstream(p);
    free_debug_stream = true;
  }
#endif

  // Initialize some variables based on environment variable settings

  if (char * p = std::getenv("PRICE_HIST"))
    price_db = p;

  if (char * p = std::getenv("PRICE_EXP"))
    pricing_leeway = std::atol(p) * 60;

  // A ledger data file must be specified

  bool use_cache = std::getenv("LEDGER") != NULL;

  if (use_cache) {
    for (int i = 0; i < argc; i++)
      if (std::strcmp(argv[i], "-f") == 0) {
	use_cache = false;
	break;
      }

    cache_dirty = true;

    if (use_cache)
      if (const char * p = std::getenv("LEDGER_CACHE"))
	if (access(p, R_OK) != -1) {
	  std::ifstream instr(p);
	  if (! read_binary_journal(instr, std::getenv("LEDGER"),
				   journal.get())) {
	    // Throw away what's been read, and create a new journal
	    journal.reset(new journal_t);
	  } else {
	    cache_dirty = false;
	  }
	}
  }

  // Parse the command-line options

  int c, index;
  while (-1 !=
	 (c = getopt(argc, argv,
		     "+ABb:Ccd:DEe:F:f:Ghi:L:l:MnoOP:p:QRS:st:T:UVvWXZ"))) {
    switch (char(c)) {
    // Basic options
    case 'h':
      show_help(std::cout);
      break;

    case 'v':
      show_version(std::cout);
      return 0;

    case 'f':
      files.push_back(optarg);
      use_cache = false;
      break;

    case 'p':
      set_price_conversion(optarg);
      break;

    case 'b':
      if (! predicate.empty())
	predicate += "&";
      predicate += "(d>=[";
      predicate += optarg;
      predicate += "])";
      break;

    case 'e':
      if (! predicate.empty())
	predicate += "&";
      predicate += "(d<[";
      predicate += optarg;
      predicate += "])";
      break;

    case 'c': {
      if (! predicate.empty())
	predicate += "&";
      predicate += "(d<";
      std::ostringstream now;
      now << std::time(NULL);
      predicate += now.str();
      predicate += ")";
      break;
    }

    case 'C':
      if (! predicate.empty())
	predicate += "&";
      predicate += "X";
      break;

    case 'U':
      if (! predicate.empty())
	predicate += "&";
      predicate += "!X";
      break;

    case 'R':
      if (! predicate.empty())
	predicate += "&";
      predicate += "R";
      break;

    // Customizing output
    case 'F':
      format_string = optarg;
      break;

    case 'E':
      show_empty = true;
      break;

    case 'n':
      show_subtotals = false;
      break;

    case 's':
      show_expanded = true;
      break;

    case 'S':
      sort_string = optarg;
      break;

    case 'o':
      show_related = true;
      break;

    case 'l':
      if (! predicate.empty())
	predicate += "&";
      predicate += "(";
      predicate += optarg;
      predicate += ")";
      break;

    case 'd':
      if (! display_predicate.empty())
	display_predicate += "&";
      display_predicate += "(";
      display_predicate += optarg;
      display_predicate += ")";
      break;

    // Commodity reporting
    case 'P':
      price_db = optarg;
      break;

    case 'L':
      pricing_leeway = std::atol(optarg) * 60;
      break;

    case 'Q':
      commodity_t::updater = download_price_quote;
      break;

    case 't':
      value_expr = optarg;
      break;

    case 'T':
      total_expr = optarg;
      break;

    case 'O':
      value_expr = "a";
      total_expr = "T";
      break;

    case 'B':
      value_expr = "c";
      total_expr = "C";
      break;

    case 'V':
      show_revalued = true;

      value_expr = "v";
      total_expr = "V";
      break;

    case 'G':
      show_revalued      =
      show_revalued_only = true;

      value_expr = "c";
      total_expr = "G";
      break;

    case 'A':
      value_expr = "a";
      total_expr = "MT";
      break;

    case 'D':
      value_expr = "a";
      total_expr = "DMT";
      break;

    case 'Z':
      value_expr = "a";
      total_expr = "MDMT";
      break;

#if 0
    case 'W':
      value_expr = "a";
      total_expr = "MD(MT*(d-b/e-b))";
      break;

    case 'X':
      value_expr = "a";
      total_expr = "a+MD(MT*(d-b/e-b))";
      break;
#endif

    default:
      assert(0);
      break;
    }
  }

  if (optind == argc) {
    show_help(std::cerr);
    return 1;
  }

  index = optind;

  // Read the ledger file, unless we already read it from the cache

  if (! use_cache || cache_dirty) {
    int entry_count = 0;

    try {
      if (files.empty()) {
	if (char * p = std::getenv("LEDGER"))
	  for (p = std::strtok(p, ":"); p; p = std::strtok(NULL, ":"))
	    entry_count += parse_journal_file(p, journal.get());
      } else {
	for (std::list<std::string>::iterator i = files.begin();
	     i != files.end(); i++) {
	  char buf[4096];
	  char * p = buf;
	  std::strcpy(p, (*i).c_str());
	  entry_count += parse_journal_file(p, journal.get());
	}
      }

      // Read prices from their own ledger file, after all others have
      // been read.

      if (! price_db.empty()) {
	const char * path = price_db.c_str();
	std::ifstream db(path);
	journal->sources.push_back(path);
	entry_count += parse_textual_journal(db, journal.get(),
						    journal->master);
      }
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
  }

  // Read the command word, and then check and simplify it

  std::string command = argv[index++];

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
    new_entry.reset(journal->derive_entry(argc - index, &argv[index]));
  } else {
    // Treat the remaining command-line arguments as regular
    // expressions, used for refining report results.

    int start = index;
    for (; index < argc; index++)
      if (std::strcmp(argv[index], "--") == 0) {
	index++;
	break;
      }

    if (start < index) {
      std::list<std::string> regexps(&argv[start], &argv[index]);
      std::string pred = regexps_to_predicate(regexps.begin(), regexps.end());
      if (! pred.empty()) {
	if (! predicate.empty())
	  predicate += "&";
	predicate += pred;
      }
    }

    if (index < argc) {
      std::list<std::string> regexps(&argv[index], &argv[argc]);
      std::string pred = regexps_to_predicate(regexps.begin(), regexps.end(),
					      false);
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

  // Compile the sorting string

  if (! sort_string.empty())
    sort_order.reset(parse_value_expr(sort_string));

  // Setup the meaning of %t and %T, used in format strings

  format_t::value_expr.reset(parse_value_expr(value_expr));
  format_t::total_expr.reset(parse_value_expr(total_expr));

  // Now handle the command that was identified above.

  bool show_all_related = false;

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

  if (command == "b") {
    std::auto_ptr<item_handler<transaction_t> > formatter;
    formatter.reset(new add_to_account_value);
    if (show_related)
      formatter.reset(new related_transactions(formatter.release(),
					       show_all_related));
    formatter.reset(new filter_transactions(formatter.release(), predicate));
    walk_entries(journal->entries, *formatter.get());

    format_account acct_formatter(std::cout, format, display_predicate);
    if (show_subtotals)
      sum_accounts(journal->master);
    walk_accounts(journal->master, acct_formatter, sort_order.get());

    if (format_account::disp_subaccounts_p(journal->master)) {
      std::string end_format = "--------------------\n";
      format.reset(end_format + f);
      format.format_elements(std::cout, details_t(journal->master));
    }
  }
  else if (command == "E") {
    std::auto_ptr<item_handler<transaction_t> > formatter;
    formatter.reset(new add_to_account_value);
    formatter.reset(new filter_transactions(formatter.release(), predicate));
    walk_entries(journal->entries, *formatter.get());

    format_equity acct_formatter(std::cout, format, nformat, display_predicate);
    sum_accounts(journal->master);
    walk_accounts(journal->master, acct_formatter, sort_order.get());
  }
  else if (command == "e") {
    format_transactions formatter(std::cout, format, nformat);
    walk_transactions(new_entry->transactions, formatter);
  }
  else {
    std::auto_ptr<item_handler<transaction_t> > formatter;

    // Stack up all the formatter needed to fulfills the user's
    // requests.  Some of these are order dependent, in terms of
    // whether calc_transactions occurs before or after them.

    // format_transactions write each transaction received to the
    // output stream.
    formatter.reset(new format_transactions(std::cout, format, nformat));

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
      formatter.reset(new changed_value_transactions(formatter.release() /*,
						   show_revalued_only*/));

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
    if (show_expanded)
      formatter.reset(new subtotal_transactions(formatter.release()));
    else if (0)
      formatter.reset(new interval_transactions(formatter.release(), 0,
						interval_t(9676800, 0, 0)));

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

  // Save the cache, if need be

  if (use_cache && cache_dirty)
    if (const char * p = std::getenv("LEDGER_CACHE")) {
      std::ofstream outstr(p);
      assert(std::getenv("LEDGER"));
      write_binary_journal(outstr, journal.get(), std::getenv("LEDGER"));
    }

  return 0;
}

// main.cc ends here.
