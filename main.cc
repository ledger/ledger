#include "ledger.h"
#include "balance.h"
#include "error.h"
#include "textual.h"
#include "binary.h"
#include "constraint.h"
#include "item.h"
#include "expr.h"
#include "format.h"

#include <fstream>
#include <cstring>
#include <unistd.h>
#include <ctime>

namespace ledger {


//////////////////////////////////////////////////////////////////////
//
// The command-line balance report
//

static const std::string bal_fmt = "%20T%2_%-n\n";

unsigned int show_balances(std::ostream&	out,
			   items_deque&		items,
			   const constraints_t& constraints,
			   const node_t *       sort_order,
			   const format_t&	format,
			   const item_t *	displayed_parent)
{
  unsigned int headlines = 0;

  for (items_deque::const_iterator i = items.begin();
       i != items.end();
       i++) {
    const item_t * parent = displayed_parent;

    if (constraints(*i) &&
	((*i)->subitems.size() != 1 ||
	 (*i)->total != (*i)->subitems[0]->total)) {
      format.format_elements(out, *i, parent);
      parent = *i;

      if (! displayed_parent->parent)
	headlines++;
    }

    if (sort_order)
      (*i)->sort(sort_order);

    if (constraints.show_expanded)
      headlines += show_balances(out, (*i)->subitems, constraints,
				 sort_order, format, parent);
  }

  return headlines;
}

void balance_report(std::ostream&	 out,
		    item_t *		 top,
		    const constraints_t& constraints,
		    const node_t *       sort_order,
		    const format_t&	 format)
{
  if (sort_order)
    top->sort(sort_order);

  unsigned int headlines = show_balances(out, top->subitems, constraints,
					 sort_order, format, top);

  if (constraints.show_subtotals && headlines > 1 && top->total) {
    std::cout << "--------------------\n";
    format.format_elements(std::cout, top);
  }
}


//////////////////////////////////////////////////////////////////////
//
// The command-line register report
//

static const std::string reg_fmt
  = "%10d %-.20p %-.22N %12.66t %12.80T\n%/%22_ %-.22N %12.66t %12.80T\n";

static bool show_commodities_revalued      = false;
static bool show_commodities_revalued_only = false;

static void report_value_change(std::ostream&         out,
				const std::time_t     date,
				const balance_pair_t& balance,
				const balance_pair_t& prev_balance,
				const constraints_t&  constraints,
				const format_t&       first_line_format,
				const format_t&       next_lines_format)
{
  static std::time_t prev_date = -1;
  if (prev_date == -1) {
    prev_date = date;
    return;
  }

  item_t temp;
  temp.date  = prev_date;
  temp.total = prev_balance;
  balance_t prev_bal = format_t::compute_total(&temp);

  temp.date  = date;
  temp.total = balance;
  balance_t cur_bal = format_t::compute_total(&temp);

  if (balance_t diff = cur_bal - prev_bal) {
    temp.value = diff;
    temp.total = balance;
    temp.payee = "Commodities revalued";

    if (constraints(&temp)) {
      first_line_format.format_elements(out, &temp);
      next_lines_format.format_elements(out, &temp);
    }
  }

  prev_date = date;
}

void register_report(std::ostream&	  out,
		     item_t *		  top,
		     const constraints_t& constraints,
		     const node_t *       sort_order,
		     const format_t&	  first_line_format,
		     const format_t&	  next_lines_format)
{
  if (sort_order)
    top->sort(sort_order);

  balance_pair_t balance;
  balance_pair_t last_reported;
  account_t      splits(NULL, "<Total>");

  for (items_deque::const_iterator i = top->subitems.begin();
       i != top->subitems.end();
       i++) {
    bool first = true;

    if ((*i)->subitems.size() > 1 && ! constraints.show_expanded) {
      item_t summary;
      summary.date    = (*i)->date;
      summary.parent  = *i;
      summary.account = &splits;

      for (items_deque::const_iterator j = (*i)->subitems.begin();
	   j != (*i)->subitems.end();
	   j++)
	summary.value += (*j)->value;
      summary.total = balance + summary.value;

      bool show = constraints(&summary);
      if (show && show_commodities_revalued)
	report_value_change(out, summary.date, balance, last_reported,
			    constraints, first_line_format, next_lines_format);

      balance += summary.value;

      if (show) {
	if (! show_commodities_revalued_only)
	  first_line_format.format_elements(out, *i, top);

	if (show_commodities_revalued)
	  last_reported = balance;
      }
    } else {
      for (items_deque::const_iterator j = (*i)->subitems.begin();
	   j != (*i)->subitems.end();
	   j++) {
	(*j)->total = balance + (*j)->value;

	bool show = constraints(*j);
	if (show && first && show_commodities_revalued) {
	  report_value_change(out, (*i)->date, balance, last_reported,
			      constraints, first_line_format,
			      next_lines_format);
	  if (show_commodities_revalued_only)
	    first = false;
	}

	balance += (*j)->value;

	if (show) {
	  if (! show_commodities_revalued_only) {
	    if (first) {
	      first = false;
	      first_line_format.format_elements(out, *j, *i);
	    } else {
	      next_lines_format.format_elements(out, *j, *i);
	    }
	  }

	  if (show_commodities_revalued)
	    last_reported = balance;
	}
      }
    }
  }

  if (show_commodities_revalued)
    report_value_change(out, -1, balance, last_reported, constraints,
			first_line_format, next_lines_format);
}


bool add_new_entry(int index, int argc, char **argv, ledger_t * ledger)
{
  masks_list	regexps;
  entry_t       added;
  entry_t *     matching = NULL;

  added.state = entry_t::UNCLEARED;

  assert(index < argc);

  if (! parse_date(argv[index++], &added.date)) {
    std::cerr << "Error: Bad entry date: " << argv[index - 1]
	      << std::endl;
    return false;
  }

  if (index == argc) {
    std::cerr << "Error: Too few arguments to 'entry'." << std::endl;
    return false;
  }

  regexps.push_back(mask_t(argv[index++]));

  for (entries_list::reverse_iterator i = ledger->entries.rbegin();
       i != ledger->entries.rend();
       i++)
    if (matches(regexps, (*i)->payee)) {
      matching = *i;
      break;
    }

  added.payee = matching ? matching->payee : regexps.front().pattern;

  if (index == argc) {
    std::cerr << "Error: Too few arguments to 'entry'." << std::endl;
    return false;
  }

  if (argv[index][0] == '-' || std::isdigit(argv[index][0])) {
    if (! matching) {
      std::cerr << "Error: Missing account name for non-matching entry."
		<< std::endl;
      return false;
    }

    transaction_t * m_xact, * xact, * first;
    m_xact = matching->transactions.front();

    amount_t amt(argv[index++]);
    first = xact = new transaction_t(&added, m_xact->account, amt, amt);

    if (xact->amount.commodity->symbol.empty()) {
      xact->amount.commodity = m_xact->amount.commodity;
      xact->cost.commodity   = m_xact->amount.commodity;
    }
    added.add_transaction(xact);

    m_xact = matching->transactions.back();

    xact = new transaction_t(&added, m_xact->account,
			     - first->amount, - first->amount);
    added.add_transaction(xact);

    if ((index + 1) < argc && std::string(argv[index]) == "-from")
      if (account_t * acct = ledger->find_account(argv[++index]))
	added.transactions.back()->account = acct;
  } else {
    while (index < argc && std::string(argv[index]) != "-from") {
      mask_t acct_regex(argv[index++]);

      account_t *   acct  = NULL;
      commodity_t * cmdty = NULL;

      if (matching) {
	for (transactions_list::iterator x
	       = matching->transactions.begin();
	     x != matching->transactions.end();
	     x++) {
	  if (acct_regex.match((*x)->account->fullname())) {
	    acct  = (*x)->account;
	    cmdty = (*x)->amount.commodity;
	    break;
	  }
	}
      }

      if (! acct)
	acct = ledger->find_account(acct_regex.pattern);

      if (! acct) {
	std::cerr << "Error: Could not find account name '"
		  << acct_regex.pattern << "'." << std::endl;
	return false;
      }

      if (index == argc) {
	std::cerr << "Error: Too few arguments to 'entry'." << std::endl;
	return false;
      }

      amount_t amt(argv[index]++);
      transaction_t * xact = new transaction_t(&added, acct, amt, amt);

      if (! xact->amount.commodity)
	xact->amount.commodity = cmdty;

      added.add_transaction(xact);
    }

    if ((index + 1) < argc && std::string(argv[index]) == "-from") {
      if (account_t * acct = ledger->find_account(argv[++index])) {
	transaction_t * xact = new transaction_t(NULL, acct);
	added.add_transaction(xact);
      }
    } else {
      if (! matching) {
	std::cerr << "Error: Could not figure out the account to draw from."
		  << std::endl;
	std::exit(1);
      }
      transaction_t * xact
	= new transaction_t(&added, matching->transactions.back()->account);
      added.add_transaction(xact);
    }
  }

  //  if (added.finalize())
  print_textual_entry(std::cout, &added);

  return true;
}


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


static void show_help(std::ostream& out)
{
  std::cerr
    << "usage: ledger [options] COMMAND [options] [REGEXPS]" << std::endl
    << std::endl
    << "Basic options:" << std::endl
    << "  -h        display this help text" << std::endl
    << "  -v        display version information" << std::endl
    << "  -f FILE   specify pathname of ledger data file" << std::endl
    << std::endl
    << "Report filtering:" << std::endl
    << "  -a REGEX  specify an account regex for \"print\"" << std::endl
    << "  -b DATE   specify a beginning date" << std::endl
    << "  -e DATE   specify an ending date" << std::endl
    << "  -c        do not show future entries (same as -e TODAY)" << std::endl
    << "  -d DATE   specify a date mask ('-d mon', for all mondays)" << std::endl
    << "  -C        show only cleared transactions and balances" << std::endl
    << "  -U        show only uncleared transactions and balances" << std::endl
    << "  -R        do not consider virtual transactions: real only" << std::endl
    << "  -l EXPR   don't print entries for which EXPR yields 0" << std::endl
    << std::endl
    << "Customizing output:" << std::endl
    << "  -n        do not calculate parent account totals" << std::endl
    << "  -s        show sub-accounts in balance, and splits in register" << std::endl
    << "  -M        print register using monthly sub-totals" << std::endl
    << "  -E        show accounts that total to zero" << std::endl
    << "  -S EXPR   sort entry output based on EXPR" << std::endl
    << std::endl
    << "Commodity prices:" << std::endl
    << "  -T        report commodity totals, not their market value" << std::endl
    << "  -B        report cost basis of commodities" << std::endl
    << "  -V        report the market value of commodities" << std::endl
    << "  -P FILE   sets the price database, for reading/writing price info" << std::endl
    << "  -Q        download new price information (when needed) from the Internet" << std::endl
    << "            (works by running \"getquote SYMBOL\")" << std::endl
    << "  -L MINS   with -Q, fetch quotes only if data is older than MINS" << std::endl
    << "  -p STR    specifies a direct commodity conversion: COMM=AMOUNT" << std::endl
    << std::endl
    << "commands:" << std::endl
    << "  balance   show balance totals" << std::endl
    << "  register  display a register for ACCOUNT" << std::endl
    << "  print     print all ledger entries" << std::endl
    << "  equity    generate equity ledger for all entries" << std::endl
    << "  entry     output a newly formed entry, based on arguments" << std::endl
    << "  price     show the last known price for matching commodities" << std::endl;
}

int main(int argc, char * argv[])
{
  std::list<std::string> files;
  ledger::ledger_t *	 journal = new ledger::ledger_t;
  ledger::constraints_t  constraints;
  std::string            predicate;
  std::string		 format_string;
  std::string		 sort_str;
  ledger::node_t *	 sort_order = NULL;
  std::string		 value_expr = "a";
  std::string		 total_expr = "T";

#ifdef DEBUG
  bool debug = false;
#endif

  // Initialize some variables based on environment variable settings

  if (char * p = std::getenv("PRICE_HIST"))
    ledger::price_db = p;

  if (char * p = std::getenv("PRICE_EXP"))
    ledger::pricing_leeway = std::atol(p) * 60;

  // A ledger data file must be specified

  bool use_cache = std::getenv("LEDGER") != NULL;

  if (use_cache) {
    for (int i = 0; i < argc; i++)
      if (std::strcmp(argv[i], "-f") == 0) {
	use_cache = false;
	break;
      }

    ledger::cache_dirty = true;

    if (use_cache)
      if (const char * p = std::getenv("LEDGER_CACHE"))
	if (access(p, R_OK) != -1) {
	  std::ifstream instr(p);
	  if (! ledger::read_binary_ledger(instr, std::getenv("LEDGER"),
					   journal)) {
	    // We need to throw away what we've read, and create a new
	    // ledger
	    delete journal;
	    journal = new ledger::ledger_t;
	  } else {
	    ledger::cache_dirty = false;
	  }
	}
  }

  // Parse the command-line options

  int c, index;
  while (-1 !=
	 (c = getopt(argc, argv,
		     "+ABb:Ccd:DEe:F:f:Ghi:L:l:MnoOP:p:QRS:st:T:UVvWXZz"))) {
    switch (char(c)) {
    // Basic options
    case 'h':
      show_help(std::cout);
      break;

#ifdef DEBUG
    case 'z':
      debug = 1;
      break;
#endif

    case 'v':
      std::cout
	<< "Ledger " << ledger::version
	<< ", the command-line accounting tool" << std::endl
	<< "  Copyright (c) 2003-2004, New Artisans LLC. All rights reserved."
	<< std::endl << std::endl
	<< "This program is made available under the terms of the BSD Public"
	<< std::endl
	<< "License.  See the LICENSE file included with the distribution for"
	<< std::endl
	<< "details and disclaimer." << std::endl;
      return 0;

    case 'f':
      files.push_back(optarg);
      use_cache = false;
      break;

    case 'p':
      ledger::set_price_conversion(optarg);
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
      constraints.show_empty = true;
      break;

    case 'n':
      constraints.show_subtotals = false;
      break;

    case 's':
      constraints.show_expanded = true;
      break;

    case 'S':
      sort_str = optarg;
      break;

    case 'o':
      constraints.show_related = true;
      break;

    case 'l':
      if (! predicate.empty())
	predicate += "&";
      predicate += "(";
      predicate += optarg;
      predicate += ")";
      break;

    // Commodity reporting
    case 'P':
      ledger::price_db = optarg;
      break;

    case 'L':
      ledger::pricing_leeway = std::atol(optarg) * 60;
      break;

    case 'Q':
      ledger::commodity_t::updater = ledger::download_price_quote;
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
      ledger::show_commodities_revalued = true;

      value_expr = "v";
      total_expr = "V";
      break;

    case 'G':
      ledger::show_commodities_revalued      =
      ledger::show_commodities_revalued_only = true;

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

    case 'W':
      value_expr = "a";
      total_expr = "MD(MT*(d-b/e-b))";
      break;

    case 'X':
      value_expr = "a";
      total_expr = "a+MD(MT*(d-b/e-b))";
      break;
    }
  }

  if (optind == argc) {
    show_help(std::cout);
    return 1;
  }

  index = optind;

  // Read the ledger file, unless we already read it from the cache

  if (! use_cache || ledger::cache_dirty) {
    int entry_count = 0;

    try {
      if (files.empty()) {
	if (char * p = std::getenv("LEDGER"))
	  for (p = std::strtok(p, ":"); p; p = std::strtok(NULL, ":"))
	    entry_count += parse_ledger_file(p, journal);
      } else {
	for (std::list<std::string>::iterator i = files.begin();
	     i != files.end(); i++) {
	  char buf[4096];
	  char * p = buf;
	  std::strcpy(p, (*i).c_str());
	  entry_count += parse_ledger_file(p, journal);
	}
      }

      // Read prices from their own ledger file, after all others have
      // been read.

      if (! ledger::price_db.empty()) {
	const char * path = ledger::price_db.c_str();
	std::ifstream db(path);
	journal->sources.push_back(path);
	entry_count += ledger::parse_textual_ledger(db, journal, journal->master);
      }
    }
    catch (ledger::error& err) {
      std::cerr << "Fatal: " << err.what() << std::endl;
      return 1;
    }

    if (entry_count == 0) {
      std::cerr << ("Please specify ledger file(s) using -f option "
		    "or LEDGER environment variable.") << std::endl;
      return 1;
    }
  }

  // Read the command word, and handle the "entry" command specially,
  // without any other processing.

  const std::string command = argv[index++];

  if (command == "entry")
    return add_new_entry(index, argc, argv, journal) ? 0 : 1;

  // Interpret the remaining arguments as regular expressions, used
  // for refining report results.

  for (; index < argc; index++) {
    if (std::strcmp(argv[index], "--") == 0) {
      index++;
      break;
    }

    constraints.show_expanded = true;

    if (! predicate.empty())
      predicate += "&";

    if (argv[index][0] == '-') {
      predicate += "(!/";
      predicate += argv[index] + 1;
    } else {
      predicate += "(/";
      predicate += argv[index];
    }
    predicate += "/)";
  }

  for (; index < argc; index++) {
    constraints.show_expanded = true;

    if (! predicate.empty())
      predicate += "&";

    if (argv[index][0] == '-') {
      predicate += "(!//";
      predicate += argv[index] + 1;
    } else {
      predicate += "(//";
      predicate += argv[index];
    }
    predicate += "/)";
  }

  // Copy the constraints to the format object, and compile the value
  // and total style strings

  if (! predicate.empty()) {
#ifdef DEBUG
    if (debug)
      std::cerr << "predicate = " << predicate << std::endl;
#endif
    constraints.predicate = ledger::parse_expr(predicate);
  }

  if (! sort_str.empty())
    sort_order = ledger::parse_expr(sort_str);

  // Setup the meaning of %t and %T encountered in format strings

  ledger::format_t::value_expr = ledger::parse_expr(value_expr);
  ledger::format_t::total_expr = ledger::parse_expr(total_expr);

  // Now handle the command that was identified above.

  if (command == "print") {
#if 0
    if (ledger::item_t * top
	  = ledger::walk_entries(journal->entries.begin(),
				 journal->entries.end(), constraints)) {
      ledger::format_t * format = new ledger::format_t(format_string);
      ledger::entry_report(std::cout, top, *format);
#ifdef DEBUG
      delete top;
      delete format;
#endif
    }
#endif
  }
  else if (command == "equity") {
#if 0
    if (ledger::item_t * top
	  = ledger::walk_accounts(journal->master, constraints)) {
      ledger::format_t * format = new ledger::format_t(format_string);
      ledger::entry_report(std::cout, top, constraints, *format);
#ifdef DEBUG
      delete top;
      delete format;
#endif
    }
#endif
  }
  else if (! sort_order && ! constraints.show_related &&
	   (command == "balance"  || command == "bal")) {
    if (ledger::item_t * top
	  = ledger::walk_accounts(journal->master, constraints)) {
      ledger::format_t * format
	= new ledger::format_t(format_string.empty() ?
			       ledger::bal_fmt : format_string);
      ledger::balance_report(std::cout, top, constraints, sort_order, *format);
#ifdef DEBUG
      delete format;
      delete top;
#endif
    }
  }
  else if (command == "balance"  || command == "bal") {
    if (ledger::item_t * list
	  = ledger::walk_entries(journal->entries.begin(),
				 journal->entries.end(), constraints))
      if (ledger::item_t * top
	    = ledger::walk_items(list, journal->master, constraints)) {
	ledger::format_t * format
	  = new ledger::format_t(format_string.empty() ?
				 ledger::bal_fmt : format_string);
	ledger::balance_report(std::cout, top, constraints, sort_order,
			       *format);
#ifdef DEBUG
	delete format;
	delete top;
	delete list;
#endif
      }
  }
  else if (command == "register"  || command == "reg") {
    if (constraints.show_related)
      constraints.show_inverted = true;

    if (ledger::item_t * top
	  = ledger::walk_entries(journal->entries.begin(),
				 journal->entries.end(), constraints)) {
      std::string first_line_format;
      std::string next_lines_format;

      const char * f = (format_string.empty() ?
			ledger::reg_fmt.c_str() : format_string.c_str());
      if (const char * p = std::strstr(f, "%/")) {
	first_line_format = std::string(f, 0, p - f);
	next_lines_format = std::string(p + 2);
      } else {
	first_line_format = format_string;
	next_lines_format = format_string;
      }

      ledger::format_t * format  = new ledger::format_t(first_line_format);
      ledger::format_t * nformat = new ledger::format_t(next_lines_format);
      ledger::register_report(std::cout, top, constraints, sort_order,
			      *format, *nformat);
#ifdef DEBUG
      delete format;
      delete top;
#endif
    }
  }
  else {
    std::cerr << "Error: Unrecognized command '" << command << "'."
	      << std::endl;
    return 1;
  }

  // Save the cache, if need be

  if (use_cache && ledger::cache_dirty)
    if (const char * p = std::getenv("LEDGER_CACHE")) {
      std::ofstream outstr(p);
      assert(std::getenv("LEDGER"));
      ledger::write_binary_ledger(outstr, journal, std::getenv("LEDGER"));
    }

#ifdef DEBUG
  delete journal;

  if (sort_order)
    delete sort_order;

  if (ledger::format_t::value_expr)
    delete ledger::format_t::value_expr;
  if (ledger::format_t::total_expr)
    delete ledger::format_t::total_expr;

  // jww (2004-07-30): This should be moved into some kind of
  // "ledger::shutdown" function.
  for (ledger::commodities_map::iterator i
	 = ledger::commodity_t::commodities.begin();
       i != ledger::commodity_t::commodities.end();
       i++)
    delete (*i).second;
#endif

  return 0;
}

// main.cc ends here.
