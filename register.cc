#include "ledger.h"

#include <unistd.h>

namespace ledger {

extern bool show_cleared;

extern std::time_t begin_date;
extern bool have_beginning;
extern std::time_t end_date;
extern bool have_ending;

//////////////////////////////////////////////////////////////////////
//
// Register printing code
//

void print_register(int argc, char **argv, std::ostream& out)
{
  optind = 1;
#if 0
  int c;
  while (-1 != (c = getopt(argc, argv, ""))) {
    switch (char(c)) {
    }
  }
#endif

  // Find out which account this register is to be printed for

  account * acct = main_ledger.find_account(argv[optind++], false);
  if (! acct) {
    std::cerr << "Error: Unknown account name: " << argv[optind - 1]
	      << std::endl;
    return;
  }

  // Compile the list of specified regular expressions, which can be
  // specified on the command line, or using an include/exclude file

  for (; optind < argc; optind++)
    record_regexp(argv[optind], regexps);

  // Walk through all of the ledger entries, printing their register
  // formatted equivalent

  totals balance;

  for (entries_iterator i = main_ledger.entries.begin();
       i != main_ledger.entries.end();
       i++) {
    bool applies = false;
    for (std::list<transaction *>::iterator x = (*i)->xacts.begin();
	 x != (*i)->xacts.end();
	 x++) {
      if ((*x)->acct == acct) {
	applies = true;
	break;
      }
    }

    if (! applies || ! (*i)->matches(regexps))
      continue;

    for (std::list<transaction *>::iterator x = (*i)->xacts.begin();
	 x != (*i)->xacts.end();
	 x++) {
      if ((*x)->acct == acct || ! show_cleared && (*i)->cleared)
	continue;

      char buf[32];
      std::strftime(buf, 31, "%Y.%m.%d ", std::localtime(&(*i)->date));
      out << buf;

      if ((*i)->cleared)
	out << "* ";
      else
	out << "  ";

      out.width(4);
      if ((*i)->code.empty())
	out << " ";
      else
	out << std::left << (*i)->code;
      out << " ";

      out.width(20);
      if ((*i)->desc.empty())
	out << " ";
      else
	out << std::left << (*i)->desc;
      out << " ";

      out.width(18);
      out << std::left << (*x)->acct->as_str() << " ";

      (*x)->cost->negate();

      out.width(12);
      out << std::right << (*x)->cost->as_str(true);

      balance.credit((*x)->cost);
      balance.print(out, 12);

      out << std::endl;
    }
  }
}

} // namespace ledger
