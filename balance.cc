#include "ledger.h"

#include <pcre.h>               // Perl regular expression library

namespace ledger {

//////////////////////////////////////////////////////////////////////
//
// Balance report.
//

static inline bool matches(const std::list<pcre *>& regexps,
			   const std::string& str) {
  for (std::list<pcre *>::const_iterator r = regexps.begin();
       r != regexps.end();
       r++) {
    int ovec[3];
    if (pcre_exec(*r, NULL, str.c_str(), str.length(), 0, 0, ovec, 3) >= 0)
      return true;
  }

  return false;
}

void report_balances(int argc, char *argv[], std::ostream& out)
{
  bool show_current  = false;
  bool show_cleared  = false;
  bool show_children = false;
  bool show_empty    = false;
  bool no_subtotals  = false;

  int c;
  while (-1 != (c = getopt(argc, argv, "cCsSn"))) {
    switch (char(c)) {
    case 'c': show_current  = true; break;
    case 'C': show_cleared  = true; break;
    case 's': show_children = true; break;
    case 'S': show_empty    = true; break;
    case 'n': no_subtotals  = true; break;
    }
  }

  // Compile the list of specified regular expressions, which can be
  // specified on the command line, or using an include/exclude file.

  std::list<pcre *> regexps;

  for (; optind < argc; optind++) {
    const char *error;
    int erroffset;
    pcre * re = pcre_compile(argv[optind], PCRE_CASELESS,
			     &error, &erroffset, NULL);
    if (! re)
      std::cerr << "Warning: Failed to compile regexp: " << argv[optind]
		<< std::endl;
    else
      regexps.push_back(re);
  }

  // Walk through all of the ledger entries, computing the account
  // totals

  std::map<account *, totals *> balances;

  std::time_t now = std::time(NULL);

  for (ledger_iterator i = ledger.begin(); i != ledger.end(); i++) {
    for (std::list<transaction *>::iterator x = (*i)->xacts.begin();
	 x != (*i)->xacts.end();
	 x++) {
      account * acct = (*x)->acct;
      if (! regexps.empty() && ! matches(regexps, acct->name))
	continue;

      while (acct) {
	totals * balance = NULL;

	std::map<account *, totals *>::iterator t = balances.find(acct);
	if (t == balances.end()) {
	  balance = new totals;
	  balances.insert(std::pair<account *, totals *>(acct, balance));
	} else {
	  balance = (*t).second;
	}

	if (show_current) {
	  if (difftime((*i)->date, now) < 0)
	    balance->credit((*x)->cost);
	}
	else if (show_cleared) {
	  if ((*i)->cleared)
	    balance->credit((*x)->cost);
	}
	else {
	  balance->credit((*x)->cost);
	}

	if (no_subtotals)
	  acct = NULL;
	else
	  acct = acct->parent;
      }
    }
  }

  // Print out the balance report header

  std::string which = "Future";
  if (show_current)
    which = "Current";
  else if (show_cleared)
    which = "Cleared";

  std::cout.width(10);
  std::cout << std::right << which << std::endl;

  // Walk through the accounts, given the balance report for each

  totals total_balance;

  for (accounts_iterator i = accounts.begin(); i != accounts.end(); i++) {
    account * acct = (*i).second;

    if (! regexps.empty() && ! matches(regexps, acct->name))
      continue;

    int depth = 0;
    for (account * a = acct; a; a = a->parent)
      depth++;

    if (! show_children && depth)
      continue;

    totals * balance = balances[acct];

    if (! show_empty && ! *balance)
      continue;

    std::cout.width(10);
    std::cout << *balance << "  ";

    total_balance.credit(*balance);

    if (depth) {
      while (--depth >= 0)
	std::cout << "  ";
      std::cout << acct->name << std::endl;
    } else {
      std::cout << *acct << std::endl;
    }
  }

  // Print the total of all the balances shown

  std::cout.width(10);
  std::cout << std::right << total_balance << std::endl;

  // Free up temporary variables created on the heap

  for (std::map<account *, totals *>::iterator i = balances.begin();
       i != balances.end();
       i++) {
    delete (*i).second;
  }
}

} // namespace ledger
