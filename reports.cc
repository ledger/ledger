#include "ledger.h"

#define LEDGER_VERSION "1.1"

#include <fstream>
#include <unistd.h>

namespace ledger {

extern book * parse_ledger(std::istream& in, regexps_map& regexps,
			   bool compute_balances);
#ifdef READ_GNUCASH
extern book * parse_gnucash(std::istream& in, bool compute_balances);
#endif

extern bool parse_date(const char * date_str, std::time_t * result,
		       const int year = -1);
extern void parse_price_setting(const std::string& setting);


static bool        show_cleared;
static bool        show_virtual;
static bool        get_quotes;
static bool        show_children;
static bool        show_sorted;
static bool        show_empty;
static bool        show_subtotals;
static bool        full_names;

static std::time_t begin_date;
static bool        have_beginning;
static std::time_t end_date;
static bool        have_ending;


//////////////////////////////////////////////////////////////////////
//
// Balance reporting code
//

static void display_total(std::ostream& out, totals& balance,
			  account * acct, bool top_level,
			  int * headlines)
{
  bool displayed = false;

  if (acct->checked == 1 &&
      (show_empty || ! acct->balance.is_zero())) {
    displayed = true;

    acct->balance.print(out, 20);
    if (show_subtotals && top_level)
      balance.credit(acct->balance);

    if (acct->parent && ! full_names && ! top_level) {
      for (const account * a = acct; a; a = a->parent)
	out << "  ";
      out << acct->name << std::endl;
    } else {
      out << "  " << acct->as_str() << std::endl;
      (*headlines)++;
    }
  }

  // Display balances for all child accounts

  for (accounts_map_iterator i = acct->children.begin();
       i != acct->children.end();
       i++)
    display_total(out, balance, (*i).second, ! displayed, headlines);
}

void report_balances(std::ostream& out, regexps_map& regexps)
{
  // Walk through all of the ledger entries, computing the account
  // totals

  for (entries_list_iterator i = main_ledger->entries.begin();
       i != main_ledger->entries.end();
       i++) {
    if ((have_beginning && difftime((*i)->date, begin_date) < 0) ||
	(have_ending && difftime((*i)->date, end_date) >= 0) ||
	(show_cleared && ! (*i)->cleared))
      continue;

    for (std::list<transaction *>::iterator x = (*i)->xacts.begin();
	 x != (*i)->xacts.end();
	 x++) {
      if (! show_virtual && (*x)->is_virtual)
	continue;

      for (account * acct = (*x)->acct;
	   acct;
	   acct = show_subtotals ? acct->parent : NULL) {
	if (acct->checked == 0) {
	  if (regexps.empty()) {
	    if (! (show_children || ! acct->parent))
	      acct->checked = 2;
	    else
	      acct->checked = 1;
	  }
	  else {
	    bool by_exclusion = false;
	    bool match = matches(regexps, acct->as_str(),
				 &by_exclusion);
	    if (! match) {
	      acct->checked = 2;
	    }
	    else if (by_exclusion) {
	      if (! (show_children || ! acct->parent))
		acct->checked = 2;
	      else
		acct->checked = 1;
	    }
	    else {
	      acct->checked = 1;
	    }
	  }
	}

	if (acct->checked == 1) {
	  amount * street = (*x)->cost->street(get_quotes);
	  acct->balance.credit(street);
	  delete street;
	}
      }
    }
  }

  // Walk through all the top-level accounts, giving the balance
  // report for each, and then for each of their children.

  totals balance;
  int    headlines = 0;

  for (accounts_map_iterator i = main_ledger->accounts.begin();
       i != main_ledger->accounts.end();
       i++)
    display_total(out, balance, (*i).second, true, &headlines);

  // Print the total of all the balances shown

  if (show_subtotals && headlines > 1) {
    out << "--------------------" << std::endl;
    balance.print(out, 20);
    out << std::endl;
  }
}

//////////////////////////////////////////////////////////////////////
//
// Register printing code
//

static std::string truncated(const std::string& str, int width)
{
  char buf[256];
  memset(buf, '\0', 255);
  std::strncpy(buf, str.c_str(), width);
  if (buf[width - 1])
    std::strcpy(&buf[width - 3], "...");
  else
    buf[width] = '\0';
  return buf;
}

void print_register(const std::string& acct_name, std::ostream& out,
		    regexps_map& regexps)
{
  mask acct_regex(acct_name);

  // Walk through all of the ledger entries, printing their register
  // formatted equivalent

  totals balance;

  for (entries_list_iterator i = main_ledger->entries.begin();
       i != main_ledger->entries.end();
       i++) {
    if (! (*i)->matches(regexps))
      continue;

    for (std::list<transaction *>::iterator x = (*i)->xacts.begin();
	 x != (*i)->xacts.end();
	 x++) {
      if (! acct_regex.match((*x)->acct->as_str()) ||
	  ! show_cleared && (*i)->cleared)
	continue;

      char buf[32];
      std::strftime(buf, 31, "%m.%d ", std::localtime(&(*i)->date));
      out << buf;

#if 0
      if ((*i)->cleared)
	out << "* ";
      else
	out << "  ";

      out.width(4);
      if ((*i)->code.empty())
	out << " ";
      else
	out << std::left << (*i)->code;
#endif
      out << " ";

      out.width(30);
      if ((*i)->desc.empty())
	out << " ";
      else
	out << std::left << truncated((*i)->desc, 30);
      out << " ";

      // Always display the street value, if prices have been
      // specified

      amount * street = (*x)->cost->street(get_quotes);
      balance.credit(street);

      // If there are two transactions, use the one which does not
      // refer to this account.  If there are more than two, we will
      // just have to print all of the splits, like gnucash does.

      transaction * xact;
      if ((*i)->xacts.size() == 2) {
	if (*x == (*i)->xacts.front())
	  xact = (*i)->xacts.back();
	else
	  xact = (*i)->xacts.front();
      } else {
	xact = *x;
      }
      std::string xact_str = xact->acct_as_str();

      if (xact == *x && ! show_subtotals)
	xact_str = "(Splits...)";

      out.width(22);
      out << std::left << truncated(xact_str(), 22) << " ";

      out.width(12);
      out << std::right << street->as_str(true);
      delete street;

      balance.print(out, 12);

      out << std::endl;

      if (! show_subtotals || xact != *x)
	continue;

      for (std::list<transaction *>::iterator y = (*i)->xacts.begin();
	   y != (*i)->xacts.end();
	   y++) {
	if (*x == *y)
	  continue;

	out << "                                      ";

	out.width(22);
	out << std::left << truncated((*y)->acct_as_str(), 22) << " ";

	out.width(12);
	street = (*y)->cost->street(get_quotes);
	out << std::right << street->as_str(true) << std::endl;
	delete street;
      }
    }
  }
}

//////////////////////////////////////////////////////////////////////
//
// Create an Equity file based on a ledger.  This is used for
// archiving past years, and starting out a new year with compiled
// balances.
//

static void equity_entry(account * acct, regexps_map& regexps,
			 std::ostream& out)
{
  if (! acct->balance.is_zero() &&
      (regexps.empty() || matches(regexps, acct->as_str()))) {
    entry opening;

    opening.date    = std::time(NULL);
    opening.cleared = true;
    opening.desc    = "Opening Balance";

    transaction * xact;
    for (totals::const_iterator i = acct->balance.amounts.begin();
	 i != acct->balance.amounts.end();
	 i++) {
      // Skip it, if there is a zero balance for the commodity
      if ((*i).second->is_zero())
	continue;

      xact = new transaction();
      xact->acct = const_cast<account *>(acct);
      xact->cost = (*i).second->street(get_quotes);
      opening.xacts.push_back(xact);

      xact = new transaction();
      xact->acct = main_ledger->find_account("Equity:Opening Balances");
      xact->cost = (*i).second->street(get_quotes);
      xact->cost->negate();
      opening.xacts.push_back(xact);
    }

    opening.print(out);
  }

  // Display balances for all child accounts

  for (accounts_map_iterator i = acct->children.begin();
       i != acct->children.end();
       i++)
    equity_entry((*i).second, regexps, out);
}

void equity_ledger(std::ostream& out, regexps_map& regexps)
{
  // The account have their current totals already generated as a
  // result of parsing.  We just have to output those values.
  // totals

  for (accounts_map_iterator i = main_ledger->accounts.begin();
       i != main_ledger->accounts.end();
       i++)
    equity_entry((*i).second, regexps, out);
}

} // namespace ledger

using namespace ledger;

static void show_help(std::ostream& out)
{
  std::cerr
    << "usage: ledger [options] COMMAND [options] [REGEXPS]" << std::endl
    << std::endl
    << "ledger options:" << std::endl
    << "  -C        also show cleared transactions" << std::endl
    << "  -d DATE   specify an implicit date range (e.g., -d april)"
    << std::endl
    << "  -b DATE   specify a beginning date" << std::endl
    << "  -e DATE   specify an ending date" << std::endl
    << "  -c        do not show future entries (same as -e TODAY)" << std::endl
    << "  -f FILE   specify pathname of ledger data file" << std::endl
    << "  -h        display this help text" << std::endl
    << "  -R        do not factor any virtual transactions" << std::endl
    << "  -V FILE   use virtual mappings listed in FILE" << std::endl
    << "  -i FILE   read the list of inclusion regexps from FILE" << std::endl
    << "  -p FILE   read the list of prices from FILE" << std::endl
    << "  -P        download price quotes from the Internet" << std::endl
    << "            (works by running the command \"getquote SYMBOL\")"
    << std::endl
    << "  -v        display version information" << std::endl
    << "  -w        print out warnings where applicable" << std::endl
    << std::endl
    << "commands:" << std::endl
    << "  balance   show balance totals" << std::endl
    << "  register  display a register for ACCOUNT" << std::endl
    << "  print     print all ledger entries" << std::endl
    << "  equity    generate equity ledger for all entries" << std::endl
    << std::endl
    << "`balance' options:" << std::endl
    << "  -F        print each account's full name" << std::endl
    << "  -n        do not generate totals for parent accounts" << std::endl
    << "  -s        show sub-accounts in balance totals" << std::endl
    << "  -S        show empty accounts in balance totals" << std::endl;
}

//////////////////////////////////////////////////////////////////////
//
// Command-line parser and top-level logic.
//

int main(int argc, char * argv[])
{
  std::istream * file = NULL;
  std::string    prices;
  regexps_map    regexps;
  int            index;

  have_beginning = false;
  have_ending    = false;
  show_cleared   = false;
  show_virtual   = true;
  show_children  = false;
  show_sorted    = false;
  show_empty     = false;
  show_subtotals = true;
  full_names     = false;

  // Parse the command-line options

  int c;
  while (-1 != (c = getopt(argc, argv, "+b:e:d:cChRV:f:i:p:PvsSEnF"))) {
    switch (char(c)) {
    case 'b':
    case 'e': {
      std::time_t when;
      if (! parse_date(optarg, &when)) {
	std::cerr << "Error: Bad date string: " << optarg << std::endl;
	return 1;
      }

      if (c == 'b') {
	begin_date     = when;
	have_beginning = true;
      } else {
	end_date       = when;
	have_ending    = true;
      }
      break;
    }

#if 0
    case 'd': {
      if (! parse_date(optarg, &begin_date)) {
	std::cerr << "Error: Bad date string: " << optarg << std::endl;
	return 1;
      }
      have_beginning = true;

      struct std::tm when, then;
      std::memset(&then, 0, sizeof(struct std::tm));

      std::time_t now = std::time(NULL);
      struct std::tm * now_tm = std::localtime(&now);

      for (const char ** f = formats; *f; f++) {
	memset(&when, INT_MAX, sizeof(struct std::tm));
	if (strptime(optarg, *f, &when)) {
	  then.tm_hour = 0;
	  then.tm_min  = 0;
	  then.tm_sec  = 0;

	  if (when.tm_year != -1)
	    then.tm_year = when.tm_year + 1;
	  else
	    then.tm_year = now_tm->tm_year;

	  if (std::strcmp(*f, "%Y") == 0) {
	    then.tm_mon  = 0;
	    then.tm_mday = 1;
	  } else {
	    if (when.tm_mon != -1)
	      then.tm_mon  = when.tm_mon + 1;
	    else
	      then.tm_mon = now_tm->tm_mon;

	    if (when.tm_mday != -1)
	      then.tm_mday = when.tm_mday + 1;
	    else
	      then.tm_mday = now_tm->tm_mday;
	  }

	  end_date = std::mktime(&then);
	  have_ending = true;
	  break;
	}
      }
      break;
    }
#endif

    case 'c':
      end_date = std::time(NULL);
      have_ending = true;
      break;

    case 'h': show_help(std::cout); break;
    case 'f': file = new std::ifstream(optarg); break;

    case 'C': show_cleared   = true;  break;
    case 'R': show_virtual   = false; break;
    case 's': show_children  = true;  break;
    case 'S': show_sorted    = true;  break;
    case 'E': show_empty     = true;  break;
    case 'n': show_subtotals = false; break;
    case 'F': full_names     = true;  break;

    // -i path-to-file-of-regexps
    case 'i':
      read_regexps(optarg, regexps);
      break;

    // -p "COMMODITY=PRICE"
    // -p path-to-price-database
    case 'p':
      prices = optarg;
      break;

    case 'P':
      get_quotes = true;
      break;

    case 'v':
      std::cout
	<< "Ledger Accouting Tool " LEDGER_VERSION << std::endl
	<< "    Copyright (c) 2003 John Wiegley <johnw@newartisans.com>"
	<< std::endl << std::endl
	<< "This program is made available under the terms of the BSD"
	<< std::endl
	<< "Public License.  See the LICENSE file included with the"
	<< std::endl
	<< "distribution for details and disclaimer." << std::endl;
      return 0;
    }
  }

  if (optind == argc) {
    show_help(std::cout);
    return 1;
  }

  index = optind;

#if 0
  if (have_beginning || have_ending) {
    std::cout << "Reporting";

    if (have_beginning) {
      char buf[32];
      std::strftime(buf, 31, "%Y.%m.%d", std::localtime(&begin_date));
      std::cout << " from " << buf;
    }

    if (have_ending) {
      char buf[32];
      std::strftime(buf, 31, "%Y.%m.%d", std::localtime(&end_date));
      std::cout << " until " << buf;
    }

    std::cout << std::endl;
  }
#endif

  // A ledger data file must be specified

  if (! file) {
    const char * p = std::getenv("LEDGER");
    if (p)
      file = new std::ifstream(p);

    if (! file || ! *file) {
      std::cerr << ("Please specify ledger file using -f option "
		    "or LEDGER environment variable.")
		<< std::endl;
      return 1;
    }
  }

  // Read the command word

  const std::string command = argv[index++];

  int name_index = index;
  if (command == "register") {
    if (optind == argc) {
      std::cerr << ("Error: Must specify an account name "
		    "after the 'register' command.") << std::endl;
      return 1;
    }
    index++;
  }

  // Compile the list of specified regular expressions, which can be
  // specified after the command, or using the '-i FILE' option

  for (; index < argc; index++)
    regexps.push_back(mask(argv[index]));

  // Parse the ledger

#ifdef READ_GNUCASH
  char buf[32];
  file->get(buf, 31);
  file->seekg(0);

  if (std::strncmp(buf, "<?xml version=\"1.0\"?>", 21) == 0)
    main_ledger = parse_gnucash(*file, command == "equity");
  else
#endif
    main_ledger = parse_ledger(*file, regexps, command == "equity");

  delete file;

  if (! main_ledger)
    std::exit(1);

  // Record any prices specified by the user

  if (! prices.empty()) {
    if (access(prices.c_str(), R_OK) != -1) {
      std::ifstream pricedb(prices.c_str());
      while (! pricedb.eof()) {
	char buf[80];
	pricedb.getline(buf, 79);
	if (*buf && ! std::isspace(*buf))
	  parse_price_setting(buf);
      }
    } else {
      parse_price_setting(prices);
    }
  }

  // Process the command

  if (command == "balance") {
    report_balances(std::cout, regexps);
  }
  else if (command == "register") {
    if (show_sorted)
      main_ledger->sort(cmp_entry_date());
    print_register(argv[name_index], std::cout, regexps);
  }
  else if (command == "print") {
    if (show_sorted)
      main_ledger->sort(cmp_entry_date());
    main_ledger->print(std::cout, regexps, true);
  }
  else if (command == "equity") {
    equity_ledger(std::cout, regexps);
  }

#ifdef DEBUG
  // Ordinarily, deleting the main ledger isn't necessary, since the
  // process is about to give back its heap to the OS.

  delete main_ledger;
#endif
}

// reports.cc ends here.
