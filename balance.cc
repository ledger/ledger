#include "ledger.h"
#include "balance.h"

namespace ledger {

amount_t balance_t::amount(const commodity_t * commodity) const
{
  if (! commodity) {
    if (amounts.size() == 1) {
      amounts_map::const_iterator i = amounts.begin();
      return (*i).second;
    }
  }
  else if (amounts.size() > 0) {
    amounts_map::const_iterator i = amounts.find(commodity);
    if (i != amounts.end())
      return (*i).second;
  }
  return amount_t();
}

#if 0
balance_t balance_t::round() const
{
  balance_t temp;

  for (amounts_map::const_iterator i = amounts.begin();
       i != amounts.end();
       i++)
    temp += (*i).second.round();

  return temp;
}
#endif

balance_t balance_t::value(const std::time_t moment) const
{
  balance_t temp;

  for (amounts_map::const_iterator i = amounts.begin();
       i != amounts.end();
       i++)
    temp += (*i).second.value(moment);

#if 1
  return temp;
#else
  return temp.round();
#endif
}

void balance_t::write(std::ostream& out,
		      const int     first_width,
		      const int     latter_width) const
{
  bool first  = true;
  int  lwidth = latter_width;

  if (lwidth == -1)
    lwidth = first_width;

  for (amounts_map::const_iterator i = amounts.begin();
       i != amounts.end();
       i++) {
    if ((*i).second) {
      int width;

      if (! first) {
	out << std::endl;
	width = lwidth;
      } else {
	first = false;
	width = first_width;
      }

      out.width(width);
      out.fill(' ');
      out << std::right << std::string((*i).second);
    }
  }

  if (first) {
    out.width(first_width);
    out.fill(' ');
    out << std::right << "0";
  }
}


balance_pair_t::balance_pair_t(const transaction_t& xact)
  : quantity(xact.amount), cost(xact.cost) {}

balance_pair_t& balance_pair_t::operator+=(const transaction_t& xact)
{
  quantity += xact.amount;
  cost     += xact.cost;
  return *this;
}

balance_pair_t& balance_pair_t::operator-=(const transaction_t& xact)
{
  quantity -= xact.amount;
  cost     -= xact.cost;
  return *this;
}

} // namespace ledger
