#include "balance.h"
#include "util.h"

#include <deque>
#include <algorithm>

namespace ledger {

amount_t balance_t::amount(const commodity_t& commodity) const
{
  if (! commodity) {
    if (amounts.size() == 1) {
      amounts_map::const_iterator i = amounts.begin();
      return (*i).second;
    }
  }
  else if (amounts.size() > 0) {
    amounts_map::const_iterator i = amounts.find(&commodity);
    if (i != amounts.end())
      return (*i).second;
  }
  return amount_t();
}

balance_t balance_t::value(const std::time_t moment) const
{
  balance_t temp;

  for (amounts_map::const_iterator i = amounts.begin();
       i != amounts.end();
       i++)
    temp += (*i).second.value(moment);

  return temp;
}

balance_t balance_t::price() const
{
  balance_t temp;

  for (amounts_map::const_iterator i = amounts.begin();
       i != amounts.end();
       i++)
    temp += (*i).second.price();

  return temp;
}

struct compare_amount_commodities {
  bool operator()(const amount_t * left, const amount_t * right) const {
    return left->commodity().symbol < right->commodity().symbol;
  }
};

void balance_t::write(std::ostream& out,
		      const int     first_width,
		      const int     latter_width) const
{
  bool first  = true;
  int  lwidth = latter_width;

  if (lwidth == -1)
    lwidth = first_width;

  typedef std::deque<const amount_t *> amounts_deque;
  amounts_deque sorted;

  for (amounts_map::const_iterator i = amounts.begin();
       i != amounts.end();
       i++)
    if ((*i).second)
      sorted.push_back(&(*i).second);

  std::stable_sort(sorted.begin(), sorted.end(), compare_amount_commodities());

  for (amounts_deque::const_iterator i = sorted.begin();
       i != sorted.end();
       i++) {
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
    out << std::right << **i;
  }

  if (first) {
    out.width(first_width);
    out.fill(' ');
    out << std::right << "0";
  }
}

balance_t& balance_t::operator*=(const balance_t& bal)
{
  if (! *this || ! bal) {
    return (*this = 0L);
  }
  else if (amounts.size() == 1 && bal.amounts.size() == 1) {
    return *this *= (*bal.amounts.begin()).second;
  }
  else {
    std::string msg;
    std::ostringstream errmsg(msg);
    errmsg << "It makes no sense to multiply two balances: "
	   << *this << " * " << bal;
    throw amount_error(errmsg.str());
  }
}

balance_t& balance_t::operator/=(const balance_t& bal)
{
  if (! *this) {
    return (*this = 0L);
  }
  else if (! bal) {
    std::string msg;
    std::ostringstream errmsg(msg);
    errmsg << "Attempt to divide by zero: " << *this << " / " << bal;
    throw amount_error(errmsg.str());
  }
  else if (amounts.size() == 1 && bal.amounts.size() == 1) {
    return *this /= (*bal.amounts.begin()).second;
  }
  else if (*this == bal) {
    return (*this = 1L);
  }
  else {
    std::string msg;
    std::ostringstream errmsg(msg);
    errmsg << "It makes no sense to divide two balances: "
	   << *this << " / " << bal;
    throw amount_error(errmsg.str());
  }
}

balance_t::operator amount_t() const
{
  if (amounts.size() == 1) {
    return (*amounts.begin()).second;
  }
  else if (amounts.size() == 0) {
    return amount_t();
  }
  else {
    std::string msg;
    std::ostringstream errmsg(msg);
    errmsg << "Cannot convert a balance with "
	   << "multiple commodities to an amount: " << *this;
    throw amount_error(errmsg.str());
  }
}

balance_pair_t& balance_pair_t::operator/=(const balance_pair_t& bal_pair)
{
  if (bal_pair.cost && ! cost)
    cost = new balance_t(quantity);
  quantity /= bal_pair.quantity;
  if (cost)
    *cost /= bal_pair.cost ? *bal_pair.cost : bal_pair.quantity;

  if (bal_pair.price && *bal_pair.price && price)
    *price /= *bal_pair.price;
  return *this;
}

balance_pair_t& balance_pair_t::add(const amount_t&  amount,
				    const amount_t * a_price,
				    const amount_t * a_cost)
{
  if (a_cost && ! cost)
    cost = new balance_t(quantity);
  quantity += amount;
  if (cost)
    *cost += a_cost ? *a_cost : amount;

  if (a_price) {
    if (! price)
      price = new balance_t(*a_price);
    else
      *price += *a_price;
  }
  return *this;
}

} // namespace ledger
