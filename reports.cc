#include "ledger.h"

#define LEDGER_VERSION "1.7"

#include <cstring>
#include <unistd.h>

namespace ledger {

static bool cleared_only   = false;
static bool uncleared_only = false;
static bool cost_basis     = false;
static bool show_virtual   = true;
static bool show_children  = false;
static bool show_sorted    = false;
static bool show_empty     = false;
static bool show_subtotals = true;
static bool full_names     = false;
static bool print_monthly  = false;
static bool gnuplot_safe   = false;

static bool get_quotes = false;
       long pricing_leeway = 24 * 3600;
       std::string price_db;

static amount * lower_limit = NULL;

static mask * negonly_regexp = NULL;

static std::time_t begin_date;
static bool have_beginning = false;

static std::time_t end_date;
static bool have_ending = false;

static struct std::tm date_mask;
static bool have_date_mask = false;

static bool matches_date_range(entry * ent)
{
  if (have_beginning && difftime(ent->date, begin_date) < 0)
    return false;

  if (have_ending && difftime(ent->date, end_date) >= 0)
    return false;

  if (have_date_mask) {
    struct std::tm * then = std::localtime(&ent->date);

    if (date_mask.tm_mon != -1 &&
	date_mask.tm_mon != then->tm_mon)
      return false;

    if (date_mask.tm_mday != -1 &&
	date_mask.tm_mday != then->tm_mday)
      return false;

#if 0
    // jww (2003-10-10): This causes only certain days of the week to
    // print, even when it was not included in the mask.
    if (date_mask.tm_wday != -1 &&
	date_mask.tm_wday != then->tm_wday)
      return false;
#endif

    if (date_mask.tm_year != -1 &&
	date_mask.tm_year != then->tm_year)
      return false;
  }

  return true;
}

//////////////////////////////////////////////////////////////////////
//
// Balance reporting code
//

static bool satisfies_limit(totals& balance)
{
  bool satisfies = true;
  bool invert    = false;

  assert(lower_limit);

  if (balance.is_negative())
    invert = true;
  else
    lower_limit->negate();

  balance.credit(lower_limit);
  if (balance.is_negative())
    satisfies = invert;
  else
    satisfies = ! invert;

  lower_limit->negate();
  balance.credit(lower_limit);

  if (invert)
    lower_limit->negate();

  return satisfies;
}

static bool satisfies_limit(amount * balance)
{
  bool satisfies = true;
  bool invert    = false;

  assert(lower_limit);

  if (balance->is_negative())
    invert = true;
  else
    lower_limit->negate();

  balance->credit(lower_limit);
  if (balance->is_negative())
    satisfies = invert;
  else
    satisfies = ! invert;

  lower_limit->negate();
  balance->credit(lower_limit);

  if (invert)
    lower_limit->negate();

  return satisfies;
}

static void adjust_total(account * acct)
{
  for (accounts_map_iterator i = acct->children.begin();
       i != acct->children.end();
       i++)
    adjust_total((*i).second);

  if (acct->checked == 1) {
    if (! show_empty && acct->balance.is_zero())
      acct->checked = 2;
    else if (lower_limit && ! satisfies_limit(acct->balance))
      acct->checked = 2;
    else if (negonly_regexp && negonly_regexp->match(acct->as_str()) &&
	     ! acct->balance.is_negative())
      acct->checked = 2;

    if (acct->checked == 2) {
      acct->balance.negate();
      for (account * a = acct->parent; a; a = a->parent)
	a->balance.credit(acct->balance);
    }
  }
}

static int acct_visible_children(account * acct)
{
  int count = 0;
  for (accounts_map_iterator i = acct->children.begin();
       i != acct->children.end();
       i++) {
    if ((*i).second->checked == 1) {
      if ((*i).second->children.size() == 0)
	count++;
      else
	count += acct_visible_children((*i).second);
    }
  }
  return count;
}

static void display_total(std::ostream& out, totals& balance,
			  account * acct, int level, int * headlines)
{
  // If the number of visible children is exactly one, do not print
  // the parent account, but just the one child (whose name will
  // output with sufficiently qualification).
  
  if (acct->checked == 1 && acct_visible_children(acct) != 1) {
    if (acct->balance.is_zero()) {
      out.width(20);
      out << " ";
    } else {
      acct->balance.print(out, 20);
    }

    if (level == 0 || full_names || ! show_subtotals) {
      if (show_subtotals) {
	balance.credit(acct->balance);
	(*headlines)++;
      }

      out << " " << acct->as_str() << std::endl;
    } else {
      out << " ";
      for (int i = 0; i < level; i++)
	out << "  ";

      assert(acct->parent);
      if (acct_visible_children(acct->parent) == 1) {
	/* If the account has no other siblings, instead of printing:
	     Parent
	       Child
	   print:
	     Parent:Child */
	const account * parent;
	for (parent = acct->parent;
	     parent->parent && acct_visible_children(parent->parent) == 1;
	     parent = parent->parent) {}

	out << acct->as_str(parent) << std::endl;
      } else {
	out << acct->name << std::endl;
      }
    }

    level++;
  }

  // Display balances for all child accounts

  for (accounts_map_iterator i = acct->children.begin();
       i != acct->children.end();
       i++)
    display_total(out, balance, (*i).second, level, headlines);
}

void report_balances(std::ostream& out, regexps_list& regexps)
{
  // Walk through all of the ledger entries, computing the account
  // totals

  for (entries_list_iterator i = main_ledger->entries.begin();
       i != main_ledger->entries.end();
       i++) {
    if ((cleared_only && ! (*i)->cleared) ||
	(uncleared_only && (*i)->cleared) || ! matches_date_range(*i))
      continue;

    for (std::list<transaction *>::iterator x = (*i)->xacts.begin();
	 x != (*i)->xacts.end();
	 x++) {
      if (! show_virtual && (*x)->is_virtual)
	continue;

      for (account * acct = (*x)->acct;
	   acct;
	   acct = show_subtotals ? acct->parent : NULL) {
	bool by_exclusion = false;
	bool match        = false;

	if (acct->checked == 0) {
	  if (regexps.empty()) {
	    if (! (show_children || ! acct->parent))
	      acct->checked = 2;
	    else
	      acct->checked = 1;
	  } else {
	    match = matches(regexps, acct->as_str(), &by_exclusion);
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
	  amount * street = (*x)->cost->street(have_ending ? &end_date : NULL,
					       cost_basis || get_quotes,
					       get_quotes);
	  if (cost_basis &&
	      street->commdty() == (*x)->cost->commdty() &&
	      (*x)->cost->has_price()) {
	    street = (*x)->cost->value();
	  }
	  acct->balance.credit(street);
	  delete street;
	}
	else if (show_subtotals) {
	  if (! regexps.empty() && ! match) {
	    for (account * a = acct->parent; a; a = a->parent) {
	      if (matches(regexps, a->as_str(), &by_exclusion) &&
		  ! by_exclusion) {
		match = true;
		break;
	      }
	    }
	    if (! match) break;
	  }
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
       i++) {
    adjust_total((*i).second);
    display_total(out, balance, (*i).second, 0, &headlines);
  }

  // Print the total of all the balances shown

  if (show_subtotals && headlines > 1 && ! balance.is_zero()) {
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

enum periodicity_t {
  PERIOD_NONE,
  PERIOD_MONTHLY,
  PERIOD_WEEKLY_SUN,
  PERIOD_WEEKLY_MON
};

void print_register_transaction(std::ostream& out, entry *ent,
				transaction *xact, totals& balance)
{
  char buf[32];
  std::strftime(buf, 31, "%m/%d ", std::localtime(&ent->date));
  out << buf;

  out.width(25);
  if (ent->desc.empty())
    out << " ";
  else
    out << std::left << truncated(ent->desc, 25);
  out << " ";

  // Always display the street value, if prices have been
  // specified

  amount * street = xact->cost->street(&ent->date, cost_basis || get_quotes,
				       get_quotes);
  balance.credit(street);

  // If there are two transactions, use the one which does not
  // refer to this account.  If there are more than two, print
  // "<Splits...>", unless the -s option is being used (show
  // children), in which case print all of the splits, like
  // gnucash does.

  transaction * xp;
  if (ent->xacts.size() == 2) {
    if (xact == ent->xacts.front())
      xp = ent->xacts.back();
    else
      xp = ent->xacts.front();
  } else {
    xp = xact;
  }
  std::string xact_str = xp->acct_as_str();

  if (xp == xact && ! show_subtotals)
    xact_str = "<Splits...>";

  out.width(22);
  out << std::left << truncated(xact_str, 22) << " ";

  out.width(12);
  out << std::right << street->as_str(true);
  delete street;

  balance.print(out, 12);

  out << std::endl;

  if (! show_children || xp != xact)
    return;

  for (std::list<transaction *>::iterator y = ent->xacts.begin();
       y != ent->xacts.end();
       y++) {
    if (xact == *y)
      continue;

    out << "                                ";

    out.width(22);
    out << std::left << truncated((*y)->acct_as_str(), 22) << " ";

    out.width(12);
    street = (*y)->cost->street(&ent->date, cost_basis || get_quotes,
				get_quotes);
    out << std::right << street->as_str(true) << std::endl;
    delete street;
  }
}

void print_register_period(std::ostream& out, std::time_t date,
			   account *acct, amount& sum, totals& balance)
{
  char buf[32];
  std::strftime(buf, 31, "%Y/%m/%d ", std::localtime(&date));
  out << buf;

  if (! gnuplot_safe) {
    out.width(20);
    std::strftime(buf, 31, "%B", std::localtime(&date));
    out << std::left << truncated(buf, 20);
    out << " ";

    out.width(22);
    out << std::left << truncated(acct->as_str(), 22) << " ";
  } else {
    commodity * cmdty = sum.commdty();
    cmdty->symbol    = "";
    cmdty->separate  = false;
    cmdty->thousands = false;
    cmdty->european  = false;
  }

  out.width(12);
  out << std::right << sum.as_str();

  if (! gnuplot_safe)
    balance.print(out, 12);

  out << std::endl;
}

void print_register(std::ostream& out, const std::string& acct_name,
		    regexps_list& regexps, periodicity_t period = PERIOD_NONE)
{
  mask acct_regex(acct_name);

  // Walk through all of the ledger entries, printing their register
  // formatted equivalent

  totals      balance;
  amount *    period_sum = NULL; // jww (2004-04-27): should be 'totals' type
  std::time_t last_date;
  account *   last_acct;
  int         last_mon = -1;

  for (entries_list_iterator i = main_ledger->entries.begin();
       i != main_ledger->entries.end();
       i++) {
    if ((cleared_only && ! (*i)->cleared) ||
	(uncleared_only && (*i)->cleared) ||
	! matches_date_range(*i) || ! (*i)->matches(regexps))
      continue;

    int entry_mon = std::localtime(&(*i)->date)->tm_mon;

    if (period_sum && period == PERIOD_MONTHLY &&
	last_mon != -1 && entry_mon != last_mon) {
      assert(last_acct);
      print_register_period(out, last_date, last_acct,
			    *period_sum, balance);
      delete period_sum;
      period_sum = NULL;
    }

    for (std::list<transaction *>::iterator x = (*i)->xacts.begin();
	 x != (*i)->xacts.end();
	 x++) {
      if (! acct_regex.match((*x)->acct->as_str()) ||
	  (lower_limit && ! satisfies_limit((*x)->cost)))
	continue;

      if (period == PERIOD_NONE) {
	print_register_transaction(out, *i, *x, balance);
      } else {
	amount * street = (*x)->cost->street(&(*i)->date,
					     cost_basis || get_quotes,
					     get_quotes);
	balance.credit(street);

	if (period_sum) {
	  period_sum->credit(street);
	  delete street;
	} else {
	  period_sum = street;
	}

	last_acct = (*x)->acct;
	last_date = (*i)->date;
	last_mon  = entry_mon;
      }
    }
  }

  if (period_sum) {
    if (last_acct)
      print_register_period(out, last_date, last_acct,
			    *period_sum, balance);
    delete period_sum;
  }
}

//////////////////////////////////////////////////////////////////////
//
// Create an Equity file based on a ledger.  This is used for
// archiving past years, and starting out a new year with compiled
// balances.
//

static void equity_entry(account * acct, regexps_list& regexps,
			 std::ostream& out)
{
  if (! acct->balance.is_zero() &&
      (regexps.empty() || matches(regexps, acct->as_str()))) {
    entry opening(main_ledger);

    opening.date    = have_ending ? end_date : std::time(NULL);
    opening.cleared = true;
    opening.desc    = "Opening Balance";

    for (totals::const_iterator i = acct->balance.amounts.begin();
	 i != acct->balance.amounts.end();
	 i++) {
      // Skip it, if there is a zero balance for the commodity
      if ((*i).second->is_zero())
	continue;

      transaction * xact = new transaction();
      xact->acct = const_cast<account *>(acct);
      xact->cost = (*i).second->street(have_ending ? &end_date : NULL,
				       cost_basis || get_quotes,
				       get_quotes);
      opening.xacts.push_back(xact);

      xact = new transaction();
      xact->acct = main_ledger->find_account("Equity:Opening Balances");
      xact->cost = (*i).second->street(have_ending ? &end_date : NULL,
				       cost_basis || get_quotes,
				       get_quotes);
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

void equity_ledger(std::ostream& out, regexps_list& regexps)
{
  // The account have their current totals already generated as a
  // result of parsing.  We just have to output those values.
  // totals

  for (accounts_map_iterator i = main_ledger->accounts.begin();
       i != main_ledger->accounts.end();
       i++)
    equity_entry((*i).second, regexps, out);
}

//////////////////////////////////////////////////////////////////////
//
// Report on the price of any commodities matching REGEXPS.  This can
// be used to see what something was worth at a specific time.
//

void price_report(std::ostream& out, regexps_list& regexps)
{
  if (! have_ending) {
    end_date = std::time(NULL);
    have_ending = true;
  }
  
  for (commodities_map_iterator i = main_ledger->commodities.begin();
       i != main_ledger->commodities.end();
       i++)
    if (regexps.empty() || matches(regexps, (*i).first)) {
      amount * price = (*i).second->price(have_ending ? &end_date : NULL,
					  cost_basis || get_quotes,
					  get_quotes);
      if (price && ! price->is_zero()) {
	out.width(20);
	out << std::right << price->as_str() << " " << (*i).first
	    << std::endl;
      }
    }
}

//////////////////////////////////////////////////////////////////////
//
// Add a new entry, using hueristic logic to simplify the entry
// requirements
//

void add_new_entry(int index, int argc, char **argv)
{
  regexps_list 	regexps;
  entry       	added(main_ledger);
  entry *     	matching = NULL;

  assert(index < argc);

  if (! parse_date(argv[index++], &added.date)) {
    std::cerr << "Error: Bad entry date: " << argv[index - 1]
              << std::endl;
    std::exit(1);
  }

  added.cleared = cleared_only;

  if (index == argc) {
    std::cerr << "Error: Too few arguments to 'entry'." << std::endl;
    std::exit(1);
  }

  regexps.clear();
  regexps.push_back(mask(argv[index++]));

  for (entries_list_reverse_iterator i = main_ledger->entries.rbegin();
       i != main_ledger->entries.rend();
       i++) {
    if ((*i)->matches(regexps)) {
      matching = *i;
      break;
    }
  }

  added.desc = matching ? matching->desc : regexps.front().pattern;

  if (index == argc) {
    std::cerr << "Error: Too few arguments to 'entry'." << std::endl;
    std::exit(1);
  }

  if (argv[index][0] == '-' || std::isdigit(argv[index][0])) {
    if (! matching) {
      std::cerr << "Error: Missing account name for non-matching entry."
                << std::endl;
      std::exit(1);
    }

    transaction * m_xact, * xact, * first;

    m_xact = matching->xacts.front();
      
    first = xact = new transaction();
    xact->acct = m_xact->acct;
    xact->cost = create_amount(argv[index++]);
    xact->cost->set_commdty(m_xact->cost->commdty());

    added.xacts.push_back(xact);

    m_xact = matching->xacts.back();
      
    xact = new transaction();
    xact->acct = m_xact->acct;
    xact->cost = first->cost->copy();
    xact->cost->negate();

    added.xacts.push_back(xact);

    if ((index + 1) < argc && std::string(argv[index]) == "-from")
      if (account * acct = main_ledger->re_find_account(argv[++index]))
        added.xacts.back()->acct = acct;
  } else {
    while (index < argc && std::string(argv[index]) != "-from") {
      transaction * xact = new transaction();

      mask acct_regex(argv[index++]);

      account * acct = NULL;
      commodity * cmdty = NULL;

      if (matching) {
	for (std::list<transaction *>::iterator x = matching->xacts.begin();
	     x != matching->xacts.end();
	     x++) {
	  if (acct_regex.match((*x)->acct->as_str())) {
	    acct = (*x)->acct;
	    cmdty = (*x)->cost->commdty();
	    break;
	  }
	}
      }

      if (acct)
        xact->acct = acct;
      else
        xact->acct = main_ledger->re_find_account(acct_regex.pattern);

      if (! xact->acct) {
        std::cerr << "Error: Could not find account name '"
                  << acct_regex.pattern << "'." << std::endl;
        std::exit(1);
      }

      if (index == argc) {
        std::cerr << "Error: Too few arguments to 'entry'." << std::endl;
        std::exit(1);
      }

      xact->cost = create_amount(argv[index++]);
      if (! xact->cost->commdty())
	xact->cost->set_commdty(cmdty);

      added.xacts.push_back(xact);
    }

    if ((index + 1) < argc && std::string(argv[index]) == "-from") {
      if (account * acct = main_ledger->re_find_account(argv[++index])) {
        transaction * xact = new transaction();
        xact->acct = acct;
        xact->cost = NULL;

        added.xacts.push_back(xact);
      }
    } else {
        transaction * xact = new transaction();
	if (! matching) {
	  std::cerr << "Error: Could not figure out the account to draw from."
		    << std::endl;
	  std::exit(1);
	}
	xact->acct = matching->xacts.back()->acct;
        xact->cost = NULL;
        added.xacts.push_back(xact);
    }
  }

  if (added.finalize())
    added.print(std::cout);
}

// Print out the entire ledger that was read in.  This can be used to
// "wash" ugly ledger files.  It's written here, instead of ledger.cc,
// in order to access the static globals above.

void book::print(std::ostream& out, regexps_list& regexps,
		 bool shortcut) const
{
  for (entries_list_const_iterator i = entries.begin();
       i != entries.end();
       i++) {
    if (! matches_date_range(*i) || ! (*i)->matches(regexps))
      continue;

    (*i)->print(out, shortcut);
  }
}

} // namespace ledger

using namespace ledger;

static void show_help(std::ostream& out)
{
  std::cerr
    << "usage: ledger [options] COMMAND [options] [REGEXPS]" << std::endl
    << std::endl
    << "ledger options:" << std::endl
    << "  -b DATE   specify a beginning date" << std::endl
    << "  -e DATE   specify an ending date" << std::endl
    << "  -c        do not show future entries (same as -e TODAY)" << std::endl
    << "  -C        show only cleared transactions and balances" << std::endl
    << "  -d DATE   specify a date mask ('-d mon', for all mondays)" << std::endl
    << "  -E        also show accounts with zero totals" << std::endl
    << "  -f FILE   specify pathname of ledger data file" << std::endl
    << "  -F        print each account's full name" << std::endl
    << "  -h        display this help text" << std::endl
    << "  -i FILE   read the list of inclusion regexps from FILE" << std::endl
    << "  -l AMT    don't print balance totals whose abs value is <AMT" << std::endl
    << "  -M        print register using monthly sub-totals" << std::endl
    << "  -G        use with -M to produce gnuplot-friendly output" << std::endl
    << "  -n        do not calculate parent account totals" << std::endl
    << "  -N REGEX  accounts matching REGEXP only display if negative" << std::endl
    << "  -p ARG    set a price, or read prices from a file" << std::endl
    << "  -P        download price quotes from the Internet" << std::endl
    << "            (works by running the command \"getquote SYMBOL\")" << std::endl
    << "  -R        do not factor in virtual transactions" << std::endl
    << "  -s        show sub-accounts in balance totals" << std::endl
    << "  -S        sort the output of \"print\" by date" << std::endl
    << "  -U        show only uncleared transactions and balances" << std::endl
    << "  -v        display version information" << std::endl << std::endl
    << "commands:" << std::endl
    << "  balance   show balance totals" << std::endl
    << "  register  display a register for ACCOUNT" << std::endl
    << "  print     print all ledger entries" << std::endl
    << "  equity    generate equity ledger for all entries" << std::endl
    << "  entry     output a newly formed entry, based on arguments"
    << std::endl;
}

//////////////////////////////////////////////////////////////////////
//
// Command-line parser and top-level logic.
//

int main(int argc, char * argv[])
{
  int           index;
  std::string   prices;
  std::string   limit;
  regexps_list  regexps;
  bool          no_history = false;

  std::vector<std::string> files;

  main_ledger = new book;

  // Parse the command-line options

  int c;
  while (-1 != (c = getopt(argc, argv,
			   "+b:e:d:cCUhBRV:f:i:p:PL:Q:TvsSEnFMGl:N:"))) {
    switch (char(c)) {
    case 'b':
      have_beginning = true;
      if (! parse_date(optarg, &begin_date)) {
	std::cerr << "Error: Bad begin date: " << optarg << std::endl;
	return 1;
      }
      break;

    case 'e':
      have_ending = true;
      if (! parse_date(optarg, &end_date)) {
	std::cerr << "Error: Bad end date: " << optarg << std::endl;
	return 1;
      }
      break;

    case 'c':
      end_date = std::time(NULL);
      have_ending = true;
      break;

    case 'd':
      have_date_mask = true;
      if (! parse_date_mask(optarg, &date_mask)) {
	std::cerr << "Error: Bad date mask: " << optarg << std::endl;
	return 1;
      }
      break;

    case 'h': show_help(std::cout); break;
    case 'f': files.push_back(optarg); break;
    case 'C': cleared_only   = true;  break;
    case 'U': uncleared_only = true;  break;
    case 'R': show_virtual   = false; break;
    case 's': show_children  = true;  break;
    case 'S': show_sorted    = true;  break;
    case 'E': show_empty     = true;  break;
    case 'n': show_subtotals = false; break;
    case 'F': full_names     = true;  break;
    case 'M': print_monthly  = true;  break;
    case 'G': gnuplot_safe   = true;  break;

    case 'N':
      negonly_regexp = new mask(optarg);
      break;

    // -i path-to-file-of-regexps
    case 'i':
      if (access(optarg, R_OK) != -1)
	read_regexps(optarg, regexps);
      break;

    // -p "COMMODITY=PRICE"
    case 'p':
      parse_price_setting(optarg);
      break;

    case 'P':
      get_quotes = true;
      break;

    case 'L':
      pricing_leeway = std::atol(optarg) * 60;
      break;

    case 'Q':
      get_quotes = true;
      price_db = optarg;
      break;

    case 'B':
      cost_basis = true;
      // fall through...
    case 'T':
      no_history = true;
      get_quotes = false;
      break;

    case 'l':
      lower_limit = create_amount(optarg);
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

  // Read the command word

  const std::string command = argv[index++];

  int name_index = index;
  if (command == "register" || command == "reg") {
    if (name_index == argc) {
      std::cerr << ("Error: Must specify an account name "
		    "after the 'register' command.") << std::endl;
      return 1;
    }
    index++;
  }

  // Compile the list of specified regular expressions, which can be
  // specified after the command, or using the '-i FILE' option

  if (command != "entry")
    for (; index < argc; index++)
      regexps.push_back(mask(argv[index]));

  // If a price history file is specified with the environment
  // variable PRICE_HIST, add it to the list of ledger files to read.

  if (! no_history) {
    if (price_db.empty())
      if (char * p = std::getenv("PRICE_HIST")) {
	get_quotes = true;
	price_db = p;
      }

    if (char * p = std::getenv("PRICE_EXP"))
      pricing_leeway = std::atol(p) * 60;
  }

  // A ledger data file must be specified

  int entry_count = 0;
  
  if (files.empty()) {
    if (char * p = std::getenv("LEDGER")) {
      for (p = std::strtok(p, ":"); p; p = std::strtok(NULL, ":")) {
	char * sep = std::strrchr(p, '=');
	if (sep) *sep++ = '\0';
	entry_count += parse_ledger_file(main_ledger, std::string(p),
					 regexps, command == "equity", sep);
      }
    }
  } else {
    for (std::vector<std::string>::iterator i = files.begin();
	 i != files.end(); i++) {
      char buf[4096];
      char * p = buf;
      std::strcpy(p, (*i).c_str());
      char * sep = std::strrchr(p, '=');
      if (sep) *sep++ = '\0';
      entry_count += parse_ledger_file(main_ledger, std::string(p),
				       regexps, command == "equity", sep);
    }
  }

  if (! no_history && ! price_db.empty())
    entry_count += parse_ledger_file(main_ledger, price_db,
				     regexps, command == "equity");

  if (entry_count == 0) {
    std::cerr << ("Please specify ledger file(s) using -f option "
		  "or LEDGER environment variable.") << std::endl;
    return 1;
  }

  // Process the command

  if (command == "balance" || command == "bal") {
    report_balances(std::cout, regexps);
  }
  else if (command == "register" || command == "reg") {
    if (show_sorted || print_monthly)
      main_ledger->sort(cmp_entry_date());
    print_register(std::cout, argv[name_index], regexps, 
		   print_monthly ? PERIOD_MONTHLY : PERIOD_NONE);
  }
  else if (command == "print") {
    if (show_sorted)
      main_ledger->sort(cmp_entry_date());
    main_ledger->print(std::cout, regexps, ! full_names);
  }
  else if (command == "equity") {
    equity_ledger(std::cout, regexps);
  }
  else if (command == "price" || command == "prices") {
    price_report(std::cout, regexps);
  }
  else if (command == "entry") {
    add_new_entry(index, argc, argv);
  }
  else {
    std::cerr << "Error: Unrecognized command '" << command << "'."
	      << std::endl;
    return 1;
  }

#ifdef DEBUG
  // Ordinarily, deleting the main ledger isn't necessary, since the
  // process is about to give back its heap to the OS.

  delete main_ledger;

  if (lower_limit)
    delete lower_limit;

  if (negonly_regexp)
    delete negonly_regexp;
#endif

  return 0;
}

// reports.cc ends here.
