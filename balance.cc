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
    else if (amounts.size() > 1) {
      // Try stripping annotations before giving an error.
      balance_t temp(strip_annotations());
      if (temp.amounts.size() == 1)
	return temp.amount(commodity);

      std::ostringstream errmsg;
      errmsg << "Requested amount of a balance with multiple commodities: "
	     << temp;
      throw new amount_error(errmsg.str());
    }
  }
  else if (amounts.size() > 0) {
    amounts_map::const_iterator i = amounts.find(&commodity);
    if (i != amounts.end())
      return (*i).second;
  }
  return amount_t();
}

balance_t balance_t::value(const datetime_t& moment) const
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

datetime_t balance_t::date() const
{
  datetime_t temp;

  for (amounts_map::const_iterator i = amounts.begin();
       i != amounts.end();
       i++) {
    datetime_t date = (*i).second.date();
    if (! temp && date)
      temp = date;
    else if (temp != date)
      return datetime_t();
  }

  return temp;
}

balance_t balance_t::strip_annotations(const bool keep_price,
				       const bool keep_date,
				       const bool keep_tag) const
{
  balance_t temp;

  for (amounts_map::const_iterator i = amounts.begin();
       i != amounts.end();
       i++)
    temp += (*i).second.strip_annotations(keep_price, keep_date, keep_tag);

  return temp;
}

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
  if (realzero() || bal.realzero()) {
    return *this = 0L;
  }
  else if (bal.amounts.size() == 1) {
    return *this *= (*bal.amounts.begin()).second;
  }
  else if (amounts.size() == 1) {
    return *this = bal * *this;
  }
  else {
    // Since we would fail with an error at this point otherwise, try
    // stripping annotations to see if we can come up with a
    // reasonable result.  The user will not notice any annotations
    // missing (since they are viewing a stripped report anyway), only
    // that some of their value expression may not see any pricing or
    // date data because of this operation.

    balance_t temp(bal.strip_annotations());
    if (temp.amounts.size() == 1)
      return *this *= temp;
    temp = strip_annotations();
    if (temp.amounts.size() == 1)
      return *this = bal * temp;

    std::ostringstream errmsg;
    errmsg << "Cannot multiply two balances: " << temp << " * " << bal;
    throw new amount_error(errmsg.str());
  }
}

balance_t& balance_t::operator*=(const amount_t& amt)
{
  if (realzero() || amt.realzero()) {
    return *this = 0L;
  }
  else if (! amt.commodity()) {
    // Multiplying by the null commodity causes all amounts to be
    // increased by the same factor.
    for (amounts_map::iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      (*i).second *= amt;
  }
  else if (amounts.size() == 1) {
    *this = (*amounts.begin()).second * amt;
  }
  else {
    amounts_map::iterator i = amounts.find(&amt.commodity());
    if (i != amounts.end()) {
      (*i).second *= amt;
    } else {
      // Try stripping annotations before giving an error.
      balance_t temp(strip_annotations());
      if (temp.amounts.size() == 1) {
	return *this = (*temp.amounts.begin()).second * amt;
      } else {
	i = temp.amounts.find(&amt.commodity());
	if (i != temp.amounts.end())
	  return *this = temp * amt;
      }

      std::ostringstream errmsg;
      errmsg << "Attempt to multiply balance by a commodity"
	     << " not found in that balance: "
	     << temp << " * " << amt;
      throw new amount_error(errmsg.str());
    }
  }
  return *this;
}

balance_t& balance_t::operator/=(const balance_t& bal)
{
  if (bal.realzero()) {
    std::ostringstream errmsg;
    errmsg << "Attempt to divide by zero: " << *this << " / " << bal;
    throw new amount_error(errmsg.str());
  }
  else if (realzero()) {
    return *this = 0L;
  }
  else if (bal.amounts.size() == 1) {
    return *this /= (*bal.amounts.begin()).second;
  }
  else if (*this == bal) {
    return *this = 1L;
  }
  else {
    // Try stripping annotations before giving an error.
    balance_t temp(bal.strip_annotations());
    if (temp.amounts.size() == 1)
      return *this /= temp;

    std::ostringstream errmsg;
    errmsg << "Cannot divide between two balances: " << temp << " / " << bal;
    throw new amount_error(errmsg.str());
  }
}

balance_t& balance_t::operator/=(const amount_t& amt)
{
  if (amt.realzero()) {
    std::ostringstream errmsg;
    errmsg << "Attempt to divide by zero: " << *this << " / " << amt;
    throw new amount_error(errmsg.str());
  }
  else if (realzero()) {
    return *this = 0L;
  }
  else if (! amt.commodity()) {
    // Dividing by the null commodity causes all amounts to be
    // decreased by the same factor.
    for (amounts_map::iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      (*i).second /= amt;
  }
  else if (amounts.size() == 1 &&
	   (*amounts.begin()).first == &amt.commodity()) {
    (*amounts.begin()).second /= amt;
  }
  else {
    amounts_map::iterator i = amounts.find(&amt.commodity());
    if (i != amounts.end()) {
      (*i).second /= amt;
    } else {
      // Try stripping annotations before giving an error.
      balance_t temp(strip_annotations());
      if (temp.amounts.size() == 1 &&
	  (*temp.amounts.begin()).first == &amt.commodity())
	return *this = temp / amt;

      std::ostringstream errmsg;
      errmsg << "Attempt to divide balance by a commodity"
	     << " not found in that balance: "
	     << temp << " * " << amt;
      throw new amount_error(errmsg.str());
    }
  }
  return *this;
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
    // Try stripping annotations before giving an error.
    balance_t temp(strip_annotations());
    if (temp.amounts.size() == 1)
      return (*temp.amounts.begin()).second;

    std::ostringstream errmsg;
    errmsg << "Cannot convert a balance with "
	   << "multiple commodities to an amount: " << temp;
    throw new amount_error(errmsg.str());
  }
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

    .def("valid",  &balance_t::valid)

    .def("realzero", &balance_t::realzero)
    .def("amount", &balance_t::amount)
    .def("value",  &balance_t::value)
    .def("price",  &balance_t::price)
    .def("date",  &balance_t::date)
    .def("strip_annotations", &balance_t::strip_annotations)
    .def("write",  &balance_t::write)
    .def("round",  &balance_t::round)
    .def("negate", &balance_t::negate)
    .def("negated", &balance_t::negated)
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

    .def("valid",  &balance_pair_t::valid)

    .def("realzero", &balance_pair_t::realzero)
    .def("amount", &balance_pair_t::amount)
    .def("value",  &balance_pair_t::value)
    .def("price",  &balance_pair_t::price)
    .def("date",  &balance_pair_t::date)
    .def("strip_annotations", &balance_pair_t::strip_annotations)
    .def("write",  &balance_pair_t::write)
    .def("round",  &balance_pair_t::round)
    .def("negate", &balance_pair_t::negate)
    .def("negated", &balance_pair_t::negated)

    .add_property("cost",
		  make_getter(&balance_pair_t::cost,
			      return_value_policy<reference_existing_object>()))
    ;
}

#endif // USE_BOOST_PYTHON
