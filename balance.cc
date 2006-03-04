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

std::time_t balance_t::date() const
{
  std::time_t temp = 0;

  for (amounts_map::const_iterator i = amounts.begin();
       i != amounts.end();
       i++) {
    std::time_t date = (*i).second.date();
    if (temp == 0 && date != 0)
      temp = date;
    else if (temp != date)
      return 0;
  }

  return temp;
}

balance_t balance_t::reduce(const bool keep_price, const bool keep_date,
			    const bool keep_tag) const
{
  balance_t temp;

  for (amounts_map::const_iterator i = amounts.begin();
       i != amounts.end();
       i++)
    temp += (*i).second.reduce_commodity(keep_price, keep_date, keep_tag);

  return temp;
}

struct compare_amount_commodities {
  bool operator()(const amount_t * left, const amount_t * right) const {
    commodity_t& leftcomm(left->commodity());
    commodity_t& rightcomm(right->commodity());

    int cmp = leftcomm.symbol().compare(rightcomm.symbol());
    if (cmp != 0)
      return cmp < 0;

    if (! leftcomm.annotated) {
      assert(rightcomm.annotated);
      return true;
    }
    else if (! rightcomm.annotated) {
      assert(leftcomm.annotated);
      return false;
    }
    else {
      annotated_commodity_t& aleftcomm(static_cast<annotated_commodity_t&>(leftcomm));
      annotated_commodity_t& arightcomm(static_cast<annotated_commodity_t&>(rightcomm));

      amount_t val = aleftcomm.price - arightcomm.price;
      if (val)
	return val < 0;

      int diff = aleftcomm.date - arightcomm.date;
      if (diff)
	return diff < 0;

      return aleftcomm.tag < arightcomm.tag;
    }
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

  if (commodity_t::commodities_sorted) {
    for (amounts_map::const_iterator i = amounts.begin();
	 i != amounts.end();
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
      out << std::right << (*i).second;
    }
  } else {
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
    std::ostringstream errmsg;
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
    std::ostringstream errmsg;
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
    std::ostringstream errmsg;
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
    std::ostringstream errmsg;
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
  return *this;
}

balance_pair_t& balance_pair_t::add(const amount_t&  amount,
				    const amount_t * a_cost)
{
  if (a_cost && ! cost)
    cost = new balance_t(quantity);
  quantity += amount;
  if (cost)
    *cost += a_cost ? *a_cost : amount;
  return *this;
}

} // namespace ledger

#ifdef USE_BOOST_PYTHON

#include <boost/python.hpp>

using namespace boost::python;
using namespace ledger;

unsigned int balance_len(balance_t& bal)
{
  return bal.amounts.size();
}

amount_t balance_getitem(balance_t& bal, int i)
{
  std::size_t len = bal.amounts.size();

  if (abs(i) >= len) {
    PyErr_SetString(PyExc_IndexError, "Index out of range");
    throw_error_already_set();
  }

  int x = i < 0 ? len + i : i;
  amounts_map::iterator elem = bal.amounts.begin();
  while (--x >= 0)
    elem++;

  return (*elem).second;
}

unsigned int balance_pair_len(balance_pair_t& bal_pair)
{
  return balance_len(bal_pair.quantity);
}

amount_t balance_pair_getitem(balance_pair_t& bal_pair, int i)
{
  return balance_getitem(bal_pair.quantity, i);
}

void export_balance()
{
  class_< balance_t > ("Balance")
    .def(init<balance_t>())
    .def(init<amount_t>())
    .def(init<long>())
    .def(init<unsigned long>())
    .def(init<double>())

    .def(self += self)
    .def(self += other<amount_t>())
    .def(self += long())
    .def(self +  self)
    .def(self +  other<amount_t>())
    .def(self +  long())
    .def(self -= self)
    .def(self -= other<amount_t>())
    .def(self -= long())
    .def(self -  self)
    .def(self -  other<amount_t>())
    .def(self -  long())
    .def(self *= self)
    .def(self *= other<amount_t>())
    .def(self *= long())
    .def(self *  self)
    .def(self *  other<amount_t>())
    .def(self *  long())
    .def(self /= self)
    .def(self /= other<amount_t>())
    .def(self /= long())
    .def(self /  self)
    .def(self /  other<amount_t>())
    .def(self /  long())
    .def(- self)

    .def(self <  self)
    .def(self <  other<amount_t>())
    .def(self <  long())
    .def(self <= self)
    .def(self <= other<amount_t>())
    .def(self <= long())
    .def(self >  self)
    .def(self >  other<amount_t>())
    .def(self >  long())
    .def(self >= self)
    .def(self >= other<amount_t>())
    .def(self >= long())
    .def(self == self)
    .def(self == other<amount_t>())
    .def(self == long())
    .def(self != self)
    .def(self != other<amount_t>())
    .def(self != long())
    .def(! self)

    .def(abs(self))
    .def(self_ns::str(self))

    .def("__len__", balance_len)
    .def("__getitem__", balance_getitem)

    .def("negate", &balance_t::negate)
    .def("amount", &balance_t::amount)
    .def("value",  &balance_t::value)
    .def("price",  &balance_t::price)
    .def("reduce", &balance_t::reduce)
    .def("write",  &balance_t::write)
    .def("valid",  &balance_t::valid)
    ;

  class_< balance_pair_t > ("BalancePair")
    .def(init<balance_pair_t>())
    .def(init<balance_t>())
    .def(init<amount_t>())
    .def(init<long>())
    .def(init<unsigned long>())
    .def(init<double>())

    .def(self += self)
    .def(self += other<balance_t>())
    .def(self += other<amount_t>())
    .def(self += long())
    .def(self +  self)
    .def(self +  other<balance_t>())
    .def(self +  other<amount_t>())
    .def(self +  long())
    .def(self -= self)
    .def(self -= other<balance_t>())
    .def(self -= other<amount_t>())
    .def(self -= long())
    .def(self -  self)
    .def(self -  other<balance_t>())
    .def(self -  other<amount_t>())
    .def(self -  long())
    .def(self *= self)
    .def(self *= other<balance_t>())
    .def(self *= other<amount_t>())
    .def(self *= long())
    .def(self *  self)
    .def(self *  other<balance_t>())
    .def(self *  other<amount_t>())
    .def(self *  long())
    .def(self /= self)
    .def(self /= other<balance_t>())
    .def(self /= other<amount_t>())
    .def(self /= long())
    .def(self /  self)
    .def(self /  other<balance_t>())
    .def(self /  other<amount_t>())
    .def(self /  long())
    .def(- self)

    .def(self <  self)
    .def(self <  other<balance_t>())
    .def(self <  other<amount_t>())
    .def(self <  long())
    .def(self <= self)
    .def(self <= other<balance_t>())
    .def(self <= other<amount_t>())
    .def(self <= long())
    .def(self >  self)
    .def(self >  other<balance_t>())
    .def(self >  other<amount_t>())
    .def(self >  long())
    .def(self >= self)
    .def(self >= other<balance_t>())
    .def(self >= other<amount_t>())
    .def(self >= long())
    .def(self == self)
    .def(self == other<balance_t>())
    .def(self == other<amount_t>())
    .def(self == long())
    .def(self != self)
    .def(self != other<balance_t>())
    .def(self != other<amount_t>())
    .def(self != long())
    .def(! self)

    .def(abs(self))
    .def(self_ns::str(self))

    .def("__len__", balance_pair_len)
    .def("__getitem__", balance_pair_getitem)

    .add_property("cost",
		  make_getter(&balance_pair_t::cost,
			      return_value_policy<reference_existing_object>()))

    .def("negate", &balance_pair_t::negate)
    .def("amount", &balance_pair_t::amount)
    .def("value",  &balance_pair_t::value)
    .def("write",  &balance_pair_t::write)

    .def("valid", &balance_pair_t::valid)
    ;
}

#endif // USE_BOOST_PYTHON
