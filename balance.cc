#include "ledger.h"

#include <pcre.h>               // Perl regular expression library

namespace ledger {

//////////////////////////////////////////////////////////////////////
//
// Balance report.
//

static bool show_current  = false;
static bool show_cleared  = false;
static bool show_children = false;
static bool show_empty    = false;
static bool no_subtotals  = false;

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

void display_total(std::ostream& out, account * acct,
		   const std::map<account *, totals *>& balances)
{
  std::map<account *, totals *>::const_iterator b = balances.find(acct);
  if (b != balances.end()) {
    totals * balance = (*b).second;

    if (balance && (show_empty || *balance)) {
      out << *balance;

      if (acct->parent) {
	for (account * a = acct; a; a = a->parent)
	  out << "  ";
	out << acct->name << std::endl;
      } else {
	out << "  " << *acct << std::endl;
      }
    }
  }

  for (account::iterator i = acct->children.begin();
       i != acct->children.end();
       i++) {
    display_total(out, (*i).second, balances);
  }
}

void report_balances(int argc, char **argv, std::ostream& out)
{
  int c;
  optind = 1;
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
  totals total_balance;

  for (ledger_iterator i = ledger.begin(); i != ledger.end(); i++) {
    for (std::list<transaction *>::iterator x = (*i)->xacts.begin();
	 x != (*i)->xacts.end();
	 x++) {
      account * acct = (*x)->acct;

      if (! regexps.empty()) {
	if (show_children) {
	  bool match = false;
	  for (account * a = acct; a; a = a->parent) {
	    if (matches(regexps, a->name)) {
	      match = true;
	      break;
	    }
	  }
	  if (! match)
	    continue;
	}
	else if (! matches(regexps, acct->name)) {
	  continue;
	}
      }
      else if (! show_children && acct->parent) {
	continue;
      }

      while (acct) {
	totals * balance = NULL;

	std::map<account *, totals *>::iterator t = balances.find(acct);
	if (t == balances.end()) {
	  balance = new totals;
	  balances.insert(std::pair<account *, totals *>(acct, balance));
	} else {
	  balance = (*t).second;
	}

	bool do_credit = false;

	if (show_current) {
	  if (difftime((*i)->date, now) < 0)
	    do_credit = true;
	}
	else if (show_cleared) {
	  if ((*i)->cleared)
	    do_credit = true;
	}
	else {
	  do_credit = true;
	}

	if (do_credit) {
	  balance->credit((*x)->cost);

	  // If this is a top-level account, then update the total
	  // running balance as well.
	  if (! acct->parent)
	    total_balance.credit((*x)->cost);
	}

	if (no_subtotals)
	  acct = NULL;
	else
	  acct = acct->parent;
      }
    }
  }

#if 0
  // Print out the balance report header

  std::string which = "Future";
  if (show_current)
    which = "Current";
  else if (show_cleared)
    which = "Cleared";

  out.width(20);
  out << std::right << which << std::endl
      << "--------------------" << std::endl;
#endif

  // Walk through all the top-level accounts, given the balance
  // report for each, and then for each of their children.

  for (accounts_iterator i = accounts.begin(); i != accounts.end(); i++)
    if (! (*i).second->parent)
      display_total(out, (*i).second, balances);

  // Print the total of all the balances shown

  if (! no_subtotals)
    out << "--------------------" << std::endl
	<< total_balance << std::endl;

  // Free up temporary variables created on the heap

  for (std::map<account *, totals *>::iterator i = balances.begin();
       i != balances.end();
       i++) {
    delete (*i).second;
  }
}

} // namespace ledger
