#include "ledger.h"

#include <unistd.h>

namespace ledger {

extern bool show_cleared;

extern std::time_t begin_date;
extern bool have_beginning;
extern std::time_t end_date;
extern bool have_ending;

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

//////////////////////////////////////////////////////////////////////
//
// Register printing code
//

void print_register(const std::string& acct_name, std::ostream& out,
		    regexps_map& regexps)
{
  account * acct = main_ledger->find_account(acct_name, false);
  if (! acct) {
    std::cerr << "Error: Unknown account name: " << acct_name
	      << std::endl;
    return;
  }

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
      if ((*x)->acct != acct || ! show_cleared && (*i)->cleared)
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

      amount * street = (*x)->cost->street();
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

      out.width(22);
      out << std::left << truncated(xact->acct_as_str(), 22) << " ";

      out.width(12);
      out << std::right << street->as_str(true);
      delete street;

      balance.print(out, 12);

      out << std::endl;

      if (xact != *x)
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
	street = (*y)->cost->street();
	out << std::right << street->as_str(true) << std::endl;
	delete street;
      }
    }
  }
}

} // namespace ledger
