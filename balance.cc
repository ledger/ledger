#include "ledger.h"

#include <unistd.h>

namespace ledger {

extern bool        show_cleared;

extern std::time_t begin_date;
extern bool        have_beginning;
extern std::time_t end_date;
extern bool        have_ending;

static bool        show_children;
static bool        show_empty;
static bool        no_subtotals;
static bool        full_names;

static bool account_matches(const account * acct,
			    const std::list<mask>& regexps,
			    bool * true_match)
{
  bool match = false;
  *true_match = false;

  if (show_children) {
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
    if (match)
      *true_match = matches(regexps, acct->name);
  }
  return match;
}

static void display_total(std::ostream& out, totals& balance,
			  account * acct, bool top_level,
			  const std::list<mask>& regexps)
{
  bool displayed = false;

  if (acct->checked == 1 && (show_empty || acct->balance)) {
    displayed = true;

    out << acct->balance;
    if (! no_subtotals && top_level)
      balance.credit(acct->balance);

    if (acct->parent && ! no_subtotals && ! full_names) {
      for (const account * a = acct; a; a = a->parent)
	out << "  ";
      out << acct->name << std::endl;
    } else {
      out << "  " << *acct << std::endl;
    }
  }

  // Display balances for all child accounts

  for (accounts_iterator i = acct->children.begin();
       i != acct->children.end();
       i++)
    display_total(out, balance, (*i).second, ! displayed, regexps);
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
  while (-1 != (c = getopt(argc, argv, "sSnF"))) {
    switch (char(c)) {
    case 's': show_children = true; break;
    case 'S': show_empty    = true; break;
    case 'n': no_subtotals  = true; break;
    case 'F': full_names    = true; break;
    }
  }

  // Compile the list of specified regular expressions, which can be
  // specified on the command line, or using an include/exclude file

  for (; optind < argc; optind++)
    record_regexp(argv[optind], regexps);

  // Walk through all of the ledger entries, computing the account
  // totals

  for (entries_iterator i = main_ledger.entries.begin();
       i != main_ledger.entries.end();
       i++) {
    if ((have_beginning && difftime((*i)->date, begin_date) < 0) ||
	(have_ending && difftime((*i)->date, end_date) >= 0) ||
	(show_cleared && ! (*i)->cleared))
      continue;

    for (std::list<transaction *>::iterator x = (*i)->xacts.begin();
	 x != (*i)->xacts.end();
	 x++) {
      for (account * acct = (*x)->acct;
	   acct;
	   acct = no_subtotals ? NULL : acct->parent) {
	if (acct->checked == 0) {
	  bool true_match = false;
	  if (! (regexps.empty() ||
		 account_matches(acct, regexps, &true_match)))
	    acct->checked = 2;
	  else if (! (true_match || show_children || ! acct->parent))
	    acct->checked = 3;
	  else
	    acct->checked = 1;
	}

	if (acct->checked == 2)
	  break;
	else if (acct->checked == 3)
	  continue;

	acct->balance.credit((*x)->cost->street());
      }
    }
  }

  // Walk through all the top-level accounts, giving the balance
  // report for each, and then for each of their children.

  totals balance;

  for (accounts_iterator i = main_ledger.accounts.begin();
       i != main_ledger.accounts.end();
       i++)
    display_total(out, balance, (*i).second, true, regexps);

  // Print the total of all the balances shown

  if (! no_subtotals && balance)
    out << "--------------------" << std::endl
	<< balance << std::endl;
}

} // namespace ledger
