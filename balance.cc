#include "ledger.h"

#include <unistd.h>

namespace ledger {

extern bool        show_cleared;
extern bool        show_virtual;
extern bool        show_children;
extern bool        show_empty;
extern bool        show_subtotals;
extern bool        full_names;

extern std::time_t begin_date;
extern bool        have_beginning;
extern std::time_t end_date;
extern bool        have_ending;

static void display_total(std::ostream& out, totals& balance,
			  account * acct, bool top_level)
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
    }
  }

  // Display balances for all child accounts

  for (accounts_map_iterator i = acct->children.begin();
       i != acct->children.end();
       i++)
    display_total(out, balance, (*i).second, ! displayed);
}

//////////////////////////////////////////////////////////////////////
//
// Balance reporting code
//

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
	    bool by_exclusion;
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

	if (acct->checked == 1)
	  acct->balance.credit((*x)->cost->street());
      }
    }
  }

  // Walk through all the top-level accounts, giving the balance
  // report for each, and then for each of their children.

  totals balance;

  for (accounts_map_iterator i = main_ledger->accounts.begin();
       i != main_ledger->accounts.end();
       i++)
    display_total(out, balance, (*i).second, true);

  // Print the total of all the balances shown

  if (show_subtotals && ! balance.is_zero()) {
    out << "--------------------" << std::endl;
    balance.print(out, 20);
    out << std::endl;
  }
}

} // namespace ledger
