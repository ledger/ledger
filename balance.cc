#include "ledger.h"

#include <unistd.h>

namespace ledger {

extern bool show_cleared;

extern std::time_t begin_date;
extern bool have_beginning;
extern std::time_t end_date;
extern bool have_ending;

static bool show_children;
static bool show_empty;
static bool no_subtotals;
static bool full_names;

static bool account_matches(const account * acct,
			    const std::list<mask>& regexps,
			    bool * true_match)
{
  bool match = true;

  if (show_children) {
    match = false;
    for (const account * a = acct; a; a = a->parent) {
      bool exclude = false;
      if (matches(regexps, a->name, &exclude)) {
	match = true;
	*true_match = a == acct;
	break;
      }
      if (exclude)
	break;
    }
  } else {
    match = matches(regexps, acct->as_str());
    *true_match = matches(regexps, acct->name);
  }
  return match;
}

static void display_total(std::ostream& out, totals& total_balance,
			  const account * acct, bool top_level,
			  const std::map<account *, totals *>& balances,
			  const std::list<mask>& regexps)
{
  bool displayed = false;

  std::map<account *, totals *>::const_iterator b =
    balances.find(const_cast<account *>(acct));
  if (b != balances.end()) {
    totals * balance = (*b).second;
    if (balance && (show_empty || *balance)) {
      displayed = true;

      out << *balance;
      if (top_level)
	total_balance.credit(*balance);

      if (acct->parent && ! no_subtotals && ! full_names) {
	for (const account * a = acct; a; a = a->parent)
	  out << "  ";
	out << acct->name << std::endl;
      } else {
	out << "  " << *acct << std::endl;
      }
    }
  }

  // Display balances for all child accounts

  for (account::const_iterator i = acct->children.begin();
       i != acct->children.end();
       i++)
    display_total(out, total_balance, (*i).second, ! displayed,
		  balances, regexps);
}

//////////////////////////////////////////////////////////////////////
//
// Balance reporting code
//

void report_balances(int argc, char **argv, std::ostream& out)
{
  show_children = false;
  show_empty    = false;
  no_subtotals  = false;
  full_names    = false;

  optind = 1;

  int c;
  while (-1 != (c = getopt(argc, argv, "sSnFG:"))) {
    switch (char(c)) {
    case 's': show_children = true; break;
    case 'S': show_empty    = true; break;
    case 'n': no_subtotals  = true; break;
    case 'F': full_names    = true; break;

#ifdef HUQUQULLAH
    case 'G': {
      double gold = std::atof(optarg);
      gold = 1 / gold;
      char buf[256];
      std::sprintf(buf, DEFAULT_COMMODITY "=%f troy", gold);
      main_ledger.record_price(buf);
      break;
    }
#endif
    }
  }

  // Compile the list of specified regular expressions, which can be
  // specified on the command line, or using an include/exclude file

  for (; optind < argc; optind++)
    record_regexp(argv[optind], regexps);

  // Walk through all of the ledger entries, computing the account
  // totals

  std::map<account *, totals *> balances;

  for (entries_iterator i = main_ledger.entries.begin();
       i != main_ledger.entries.end();
       i++) {
    for (std::list<transaction *>::iterator x = (*i)->xacts.begin();
	 x != (*i)->xacts.end();
	 x++) {
      account * acct = (*x)->acct;

      for (; acct; acct = no_subtotals ? NULL : acct->parent) {
	bool true_match = false;
	if (! (regexps.empty() ||
	       account_matches(acct, regexps, &true_match)))
	  break;
	else if (! (true_match || show_children || ! acct->parent))
	  continue;

	totals * balance = NULL;

	std::map<account *, totals *>::iterator t = balances.find(acct);
	if (t == balances.end()) {
	  balance = new totals;
	  balances.insert(std::pair<account *, totals *>(acct, balance));
	} else {
	  balance = (*t).second;
	}

	bool do_credit = true;
	if (have_beginning && difftime((*i)->date, begin_date) < 0)
	  do_credit = false;
	else if (have_ending && difftime((*i)->date, end_date) > 0)
	  do_credit = false;
	else if (show_cleared && ! (*i)->cleared)
	  do_credit = false;
	if (! do_credit)
	  continue;

	balance->credit((*x)->cost->street());
      }
    }
  }

  // Walk through all the top-level accounts, giving the balance
  // report for each, and then for each of their children.

  totals total_balance;

  for (accounts_iterator i = main_ledger.accounts.begin();
       i != main_ledger.accounts.end();
       i++)
    display_total(out, total_balance, (*i).second, true, balances, regexps);

  // Print the total of all the balances shown

  if (! no_subtotals)
    out << "--------------------" << std::endl
	<< total_balance << std::endl;

  // Free up temporary variables created on the heap

  for (std::map<account *, totals *>::iterator i = balances.begin();
       i != balances.end();
       i++)
    delete (*i).second;
}

} // namespace ledger
