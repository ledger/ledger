#include <iostream>
#include <vector>

#include <pcre.h>               // Perl regular expression library

#include "ledger.h"

namespace ledger {

//////////////////////////////////////////////////////////////////////
//
// Balance report.
//

void report_balances(std::ostream& out, std::vector<entry *>& ledger,
		     bool show_children, bool show_empty)
{
#if 0
  // Compile the list of specified regular expressions, which can be
  // specified on the command line, or using an include/exclude file.

  std::list<pcre *> regexps;

  for (; optind < argc; optind++) {
    const char *error;
    int erroffset;
    pcre * re = pcre_compile(argv[optind], PCRE_CASELESS,
			     &error, &erroffset, NULL);
    assert(re);
    regexps.push_back(re);
  }
#endif

  // The balance of all accounts must equal zero
  totals future_balance;
  totals current_balance;
  totals cleared_balance;

  std::cout.width(10);
  std::cout << std::right << "Future" << "  ";
  std::cout.width(10);
  std::cout << std::right << "Current" << "  ";
  std::cout.width(10);
  std::cout << std::right << "Cleared" << std::endl;

  for (accounts_iterator i = accounts.begin();
       i != accounts.end();
       i++) {
    if (! show_empty && ! (*i).second->future)
      continue;

    int depth = 0;
    account * acct = (*i).second;
    while (acct->parent) {
      depth++;
      acct = acct->parent;
    }

#if 0
    if (! regexps.empty()) {
      bool matches = false;
      for (std::list<pcre *>::iterator r = regexps.begin();
	   r != regexps.end();
	   r++) {
	int ovector[30];
	if (pcre_exec(*r, NULL, (*i).first.c_str(), (*i).first.length(),
		      0, 0, ovector, 30) >= 0) {
	  matches = true;
	  break;
	}
      }

      if (! matches)
	continue;
    }
    else
#endif
      if (! show_children && depth) {
      continue;
    }

    std::cout.width(10);
    std::cout << (*i).second->future << "  ";
    std::cout.width(10);
    std::cout << (*i).second->current << "  ";
    std::cout.width(10);
    std::cout << (*i).second->cleared << "  ";

    if (depth) {
      while (--depth >= 0)
	std::cout << "  ";
      std::cout << (*i).second->name << std::endl;
    } else {
      std::cout << (*i).first << std::endl;

#if 0
      if (regexps.empty()) {
#endif
	future_balance.credit((*i).second->future);
	current_balance.credit((*i).second->current);
	cleared_balance.credit((*i).second->cleared);
#if 0
      }
#endif
    }
  }

#if 0
  if (regexps.empty()) {
#endif
    std::cout.width(10);
    std::cout << std::right << future_balance << "  ";
    std::cout.width(10);
    std::cout << std::right << current_balance << "  ";
    std::cout.width(10);
    std::cout << std::right << cleared_balance << std::endl;
#if 0
  }
#endif
}

} // namespace ledger
