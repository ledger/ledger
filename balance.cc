#include "balance.h"
#include "ledger.h"

#include <deque>

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

balance_t balance_t::value(const std::time_t moment) const
{
  balance_t temp;

  for (amounts_map::const_iterator i = amounts.begin();
       i != amounts.end();
       i++)
    temp += (*i).second.value(moment);

  return temp;
}

struct compare_amount_commodities {
  bool operator()(const amount_t * left, const amount_t * right) const {
    return left->commodity->symbol < right->commodity->symbol;
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

  std::stable_sort(sorted.begin(), sorted.end(),
		   compare_amount_commodities());

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

balance_pair_t::balance_pair_t(const transaction_t& xact)
  : quantity(xact.amount), cost(NULL)
{
  DEBUG_PRINT("ledger.memory.ctors", "ctor balance_pair_t");
  if (xact.cost)
    cost = new balance_t(*xact.cost);
}

balance_pair_t& balance_pair_t::operator+=(const transaction_t& xact)
{
  if (xact.cost && ! cost)
    cost = new balance_t(quantity);

  quantity += xact.amount;

  if (cost)
    *cost += xact.cost ? *xact.cost : xact.amount;

  return *this;
}

balance_pair_t& balance_pair_t::operator-=(const transaction_t& xact)
{
  if (xact.cost && ! cost)
    cost = new balance_t(quantity);

  quantity -= xact.amount;

  if (cost)
    *cost -= xact.cost ? *xact.cost : xact.amount;

  return *this;
}

} // namespace ledger

#ifdef USE_BOOST_PYTHON

#include <boost/python.hpp>

using namespace boost::python;
using namespace ledger;

void export_balance()
{
  class_< balance_t > ("Balance")
    .def(init<balance_t>())
    .def(init<amount_t>())
    .def(init<int>())
    .def(init<unsigned int>())
    .def(init<double>())

    .def(self <  other<unsigned int>())
    .def(self <= other<unsigned int>())
    .def(self >  other<unsigned int>())
    .def(self >= other<unsigned int>())
    .def(self == other<unsigned int>())
    .def(self != other<unsigned int>())

    .def(self += self)
    .def(self += other<amount_t>())
    .def(self +  self)
    .def(self +  other<amount_t>())
    .def(self -= self)
    .def(self -= other<amount_t>())
    .def(self -  self)
    .def(self -  other<amount_t>())
    .def(self *= self)
    .def(self *= other<amount_t>())
    .def(self *  self)
    .def(self *  other<amount_t>())
    .def(self /= self)
    .def(self /= other<amount_t>())
    .def(self /  self)
    .def(self /  other<amount_t>())
    .def(- self)

    .def(self <  self)
    .def(self <  other<amount_t>())
    .def(self <= self)
    .def(self <= other<amount_t>())
    .def(self >  self)
    .def(self >  other<amount_t>())
    .def(self >= self)
    .def(self >= other<amount_t>())
    .def(self == self)
    .def(self == other<amount_t>())
    .def(self != self)
    .def(self != other<amount_t>())
    .def(! self)

    .def(abs(self))
    //.def(str(self))

    .def("negate", &balance_t::negate,
	 return_value_policy<reference_existing_object>())
    .def("amount", &balance_t::amount)
    .def("value",  &balance_t::value)
    .def("write",  &balance_t::write)
    .def("valid", &balance_t::valid)
    ;

  class_< balance_pair_t > ("BalancePair")
    ;
}

#endif // USE_BOOST_PYTHON
