#include <vector>

#include "ledger.h"

namespace ledger {

commodities_t commodities;
commodity *   commodity_usd;

accounts_t accounts;

void entry::print(std::ostream& out) const
{
  char buf[32];
  std::strftime(buf, 31, "%Y.%m.%d ", std::localtime(&date));
  out << buf;

  if (cleared)
    out << "* ";
  if (! code.empty())
    out << '(' << code << ") ";
  if (! desc.empty())
    out << " " << desc;

  out << std::endl;

  for (std::list<transaction *>::const_iterator i = xacts.begin();
       i != xacts.end();
       i++) {
    out << "    ";

    std::string acct_name;
    for (account * acct = (*i)->acct;
	 acct;
	 acct = acct->parent) {
      if (acct_name.empty())
	acct_name = acct->name;
      else
	acct_name = acct->name + ":" + acct_name;
    }

    out.width(30);
    out << std::left << acct_name << "  ";

    out.width(10);
    out << std::right << *((*i)->cost);

    if (! (*i)->note.empty())
      out << " ; " << (*i)->note;

    out << std::endl;
  }
}

bool entry::validate() const
{
  totals balance;

  for (std::list<transaction *>::const_iterator i = xacts.begin();
       i != xacts.end();
       i++) {
    balance.credit((*i)->cost->value());
  }

  if (balance) {
    std::cout << "Totals are:" << std::endl;
    balance.print(std::cout);
  }
  return ! balance;             // must balance to 0.0
}

void totals::credit(const totals& other)
{
  for (const_iterator_t i = other.amounts.begin();
       i != other.amounts.end();
       i++) {
    credit((*i).second);
  }
}

totals::operator bool() const
{
  for (const_iterator_t i = amounts.begin(); i != amounts.end(); i++)
    if (*((*i).second))
      return true;
  return false;
}

void totals::print(std::ostream& out) const
{
 for (const_iterator_t i = amounts.begin(); i != amounts.end(); i++)
   std::cout << (*i).first << " = " << *((*i).second) << std::endl;
}

amount * totals::value(const std::string& commodity)
{
  // Render all of the amounts into the given commodity.  This
  // requires known prices for each commodity.

  amount * total = create_amount((commodity + " 0.00").c_str());

  for (iterator_t i = amounts.begin(); i != amounts.end(); i++)
    *total += *((*i).second);

  return total;
}

// Print out the entire ledger that was read in, but now sorted.
// This can be used to "wash" ugly ledger files.

void print_ledger(std::ostream& out, std::vector<entry *>& ledger)
{
  // Sort the list of entries by date, then print them in order.

  std::sort(ledger.begin(), ledger.end(), cmp_entry_date());

  for (std::vector<entry *>::const_iterator i = ledger.begin();
       i != ledger.end();
       i++) {
    (*i)->print(out);
  }
}

} // namespace ledger
