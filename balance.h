#ifndef _BALANCE_H
#define _BALANCE_H

#include <map>
#include <ctime>
#include <iostream>

#include "amount.h"

namespace ledger {

typedef std::map<const commodity_t *, amount_t>  amounts_map;
typedef std::pair<const commodity_t *, amount_t> amounts_pair;

class balance_t
{
 public:
  amounts_map amounts;

  bool valid() const {
    for (amounts_map::const_iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      if (! (*i).second.valid())
	return false;
    return true;
  }

  // constructors
  balance_t() {}
  balance_t(const balance_t& bal) {
    for (amounts_map::const_iterator i = bal.amounts.begin();
	 i != bal.amounts.end();
	 i++)
      *this += (*i).second;
  }
  balance_t(const amount_t& amt) {
    *this += amt;
  }
  balance_t(const int value) {
    *this += amount_t(value);
  }
  balance_t(const unsigned int value) {
    *this += amount_t(value);
  }
  balance_t(const double value) {
    *this += amount_t(value);
  }

  // destructor
  ~balance_t() {}

  // assignment operator
  balance_t& operator=(const balance_t& bal) {
    amounts.clear();
    for (amounts_map::const_iterator i = bal.amounts.begin();
	 i != bal.amounts.end();
	 i++)
      *this += (*i).second;
    return *this;
  }
  balance_t& operator=(const amount_t& amt) {
    amounts.clear();
    *this += amt;
    return *this;
  }
  balance_t& operator=(const int value) {
    amounts.clear();
    *this += amount_t(value);
    return *this;
  }
  balance_t& operator=(const unsigned int value) {
    amounts.clear();
    *this += amount_t(value);
    return *this;
  }
  balance_t& operator=(const double value) {
    amounts.clear();
    *this += amount_t(value);
    return *this;
  }

  // in-place arithmetic
  balance_t& operator+=(const balance_t& bal) {
    for (amounts_map::const_iterator i = bal.amounts.begin();
	 i != bal.amounts.end();
	 i++)
      *this += (*i).second;
    return *this;
  }
  balance_t& operator+=(const amount_t& amt) {
    amounts_map::iterator i = amounts.find(amt.commodity);
    if (i != amounts.end())
      (*i).second += amt;
    else if (amt)
      amounts.insert(amounts_pair(amt.commodity, amt));
    return *this;
  }
  balance_t& operator-=(const balance_t& bal) {
    for (amounts_map::const_iterator i = bal.amounts.begin();
	 i != bal.amounts.end();
	 i++)
      *this -= (*i).second;
    return *this;
  }
  balance_t& operator-=(const amount_t& amt) {
    amounts_map::iterator i = amounts.find(amt.commodity);
    if (i != amounts.end())
      (*i).second -= amt;
    else if (amt)
      amounts.insert(amounts_pair(amt.commodity, amt));
    return *this;
  }

  // simple arithmetic
  balance_t operator+(const balance_t& bal) const {
    balance_t temp = *this;
    temp += bal;
    return temp;
  }
  balance_t operator+(const amount_t& amt) const {
    balance_t temp = *this;
    temp += amt;
    return temp;
  }
  balance_t operator-(const balance_t& bal) const {
    balance_t temp = *this;
    temp -= bal;
    return temp;
  }
  balance_t operator-(const amount_t& amt) const {
    balance_t temp = *this;
    temp -= amt;
    return temp;
  }

  // multiplication and divide
  balance_t& operator*=(const balance_t& bal) {
    for (amounts_map::const_iterator i = bal.amounts.begin();
	 i != bal.amounts.end();
	 i++)
      *this *= (*i).second;
    return *this;
  }
  balance_t& operator*=(const amount_t& amt) {
    // Multiplying by the null commodity causes all amounts to be
    // increased by the same factor.
    if (amt.commodity->symbol.empty()) {
      for (amounts_map::iterator i = amounts.begin();
	   i != amounts.end();
	   i++)
	(*i).second *= amt;
    } else {
      amounts_map::iterator i = amounts.find(amt.commodity);
      if (i != amounts.end())
	(*i).second *= amt;
    }
    return *this;
  }

  balance_t& operator/=(const balance_t& bal) {
    for (amounts_map::const_iterator i = bal.amounts.begin();
	 i != bal.amounts.end();
	 i++)
      *this /= (*i).second;
    return *this;
  }
  balance_t& operator/=(const amount_t& amt) {
    // Dividing by the null commodity causes all amounts to be
    // increased by the same factor.
    if (amt.commodity->symbol.empty()) {
      for (amounts_map::iterator i = amounts.begin();
	   i != amounts.end();
	   i++)
	(*i).second /= amt;
    } else {
      amounts_map::iterator i = amounts.find(amt.commodity);
      if (i != amounts.end())
	(*i).second /= amt;
    }
    return *this;
  }

  // multiplication and divide
  balance_t operator*(const balance_t& bal) const {
    balance_t temp = *this;
    temp *= bal;
    return temp;
  }
  balance_t operator*(const amount_t& amt) const {
    balance_t temp = *this;
    temp *= amt;
    return temp;
  }
  balance_t operator/(const balance_t& bal) const {
    balance_t temp = *this;
    temp /= bal;
    return temp;
  }
  balance_t operator/(const amount_t& amt) const {
    balance_t temp = *this;
    temp /= amt;
    return temp;
  }

  // comparison
  bool operator<(const balance_t& bal) const {
    for (amounts_map::const_iterator i = bal.amounts.begin();
	 i != bal.amounts.end();
	 i++)
      if (! (amount((*i).first) < (*i).second))
	return false;

    for (amounts_map::const_iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      if (! ((*i).second < bal.amount((*i).first)))
	return false;

    if (bal.amounts.size() == 0 && amounts.size() == 0)
      return false;

    return true;
  }
  bool operator<=(const balance_t& bal) const {
    for (amounts_map::const_iterator i = bal.amounts.begin();
	 i != bal.amounts.end();
	 i++)
      if (! (amount((*i).first) <= (*i).second))
	return false;

    for (amounts_map::const_iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      if (! ((*i).second <= bal.amount((*i).first)))
	return false;

    return true;
  }
  bool operator<(const amount_t& amt) const {
    return amount(amt.commodity) < amt;
  }
  bool operator<=(const amount_t& amt) const {
    return amount(amt.commodity) <= amt;
  }

  bool operator>(const balance_t& bal) const {
    for (amounts_map::const_iterator i = bal.amounts.begin();
	 i != bal.amounts.end();
	 i++)
      if (! (amount((*i).first) > (*i).second))
	return false;

    for (amounts_map::const_iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      if (! ((*i).second > bal.amount((*i).first)))
	return false;

    if (bal.amounts.size() == 0 && amounts.size() == 0)
      return false;

    return true;
  }
  bool operator>=(const balance_t& bal) const {
    for (amounts_map::const_iterator i = bal.amounts.begin();
	 i != bal.amounts.end();
	 i++)
      if (! (amount((*i).first) >= (*i).second))
	return false;

    for (amounts_map::const_iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      if (! ((*i).second >= bal.amount((*i).first)))
	return false;

    return true;
  }
  bool operator>(const amount_t& amt) const {
    return amount(amt.commodity) > amt;
  }
  bool operator>=(const amount_t& amt) const {
    return amount(amt.commodity) >= amt;
  }

  bool operator==(const balance_t& bal) const {
    amounts_map::const_iterator i, j;
    for (i = amounts.begin(), j = bal.amounts.begin();
	 i != amounts.end() && j != bal.amounts.end();
	 i++, j++) {
      if (! ((*i).first  == (*j).first &&
	     (*i).second == (*j).second))
	return false;
    }
    return i == amounts.end() && j == bal.amounts.end();
  }
  bool operator==(const amount_t& amt) const {
    return amounts.size() == 1 && (*amounts.begin()).second == amt;
  }
  bool operator!=(const balance_t& bal) const {
    return ! (*this == bal);
  }
  bool operator!=(const amount_t& amt) const {
    return ! (*this == amt);
  }

  // unary negation
  balance_t& negate() {
    for (amounts_map::iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      (*i).second.negate();
    return *this;
  }
  balance_t negated() const {
    balance_t temp = *this;
    temp.negate();
    return temp;
  }
  balance_t operator-() const {
    return negated();
  }

  // test for non-zero (use ! for zero)
  operator bool() const {
    for (amounts_map::const_iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      if ((*i).second)
	return true;
    return false;
  }

  amount_t  amount(const commodity_t * commodity = NULL) const;
  balance_t value(const std::time_t moment) const;

  void      write(std::ostream& out,
		  const int	first_width,
		  const int	latter_width = -1) const;
};

inline balance_t abs(const balance_t& bal) {
  balance_t temp;
  for (amounts_map::const_iterator i = bal.amounts.begin();
       i != bal.amounts.end();
       i++)
    temp += abs((*i).second);
  return temp;
}

#ifdef DEBUG_ENABLED
inline std::ostream& operator<<(std::ostream& out, const balance_t& bal) {
  bal.write(out, 12);
  return out;
}
#endif

class transaction_t;

class balance_pair_t
{
 public:
  balance_t quantity;
  balance_t cost;

  bool valid() const {
    return quantity.valid() && cost.valid();
  }

  // constructors
  balance_pair_t() {}
  balance_pair_t(const balance_pair_t& bal_pair)
    : quantity(bal_pair.quantity), cost(bal_pair.cost) {}
  balance_pair_t(const balance_t& _quantity, const balance_t& _cost)
    : quantity(_quantity), cost(_cost) {}
  balance_pair_t(const balance_t& _quantity)
    : quantity(_quantity), cost(_quantity) {}
  balance_pair_t(const amount_t& _quantity, const amount_t& _cost)
    : quantity(_quantity), cost(_cost) {}
  balance_pair_t(const amount_t& _quantity)
    : quantity(_quantity), cost(_quantity) {}
  balance_pair_t(const int value)
    : quantity(value), cost(value) {}
  balance_pair_t(const unsigned int value)
    : quantity(value), cost(value) {}
  balance_pair_t(const double value)
    : quantity(value), cost(value) {}
  balance_pair_t(const transaction_t& xact);

  // destructor
  ~balance_pair_t() {}

  // assignment operator
  balance_pair_t& operator=(const balance_pair_t& bal_pair) {
    quantity = bal_pair.quantity;
    cost     = bal_pair.cost;
    return *this;
  }
  balance_pair_t& operator=(const balance_t& bal) {
    quantity = cost = bal;
    return *this;
  }
  balance_pair_t& operator=(const amount_t& amt) {
    quantity = cost = amt;
    return *this;
  }
  balance_pair_t& operator=(const int value) {
    quantity = cost = amount_t(value);
    return *this;
  }
  balance_pair_t& operator=(const unsigned int value) {
    quantity = cost = amount_t(value);
    return *this;
  }
  balance_pair_t& operator=(const double value) {
    quantity = cost = amount_t(value);
    return *this;
  }

  // in-place arithmetic
  balance_pair_t& operator+=(const balance_pair_t& bal_pair) {
    quantity += bal_pair.quantity;
    cost     += bal_pair.cost;
    return *this;
  }
  balance_pair_t& operator+=(const balance_t& bal) {
    quantity += bal;
    cost     += bal;
    return *this;
  }
  balance_pair_t& operator+=(const amount_t& amt) {
    quantity += amt;
    cost     += amt;
    return *this;
  }
  balance_pair_t& operator+=(const transaction_t& xact);

  balance_pair_t& operator-=(const balance_pair_t& bal_pair) {
    quantity -= bal_pair.quantity;
    cost     -= bal_pair.cost;
    return *this;
  }
  balance_pair_t& operator-=(const balance_t& bal) {
    quantity -= bal;
    cost     -= bal;
    return *this;
  }
  balance_pair_t& operator-=(const amount_t& amt) {
    quantity -= amt;
    cost     -= amt;
    return *this;
  }
  balance_pair_t& operator-=(const transaction_t& xact);

  // simple arithmetic
  balance_pair_t operator+(const balance_pair_t& bal_pair) const {
    balance_pair_t temp = *this;
    temp += bal_pair;
    return temp;
  }
  balance_pair_t operator+(const balance_t& bal) const {
    balance_pair_t temp = *this;
    temp += bal;
    return temp;
  }
  balance_pair_t operator+(const amount_t& amt) const {
    balance_pair_t temp = *this;
    temp += amt;
    return temp;
  }

  balance_pair_t operator-(const balance_pair_t& bal_pair) const {
    balance_pair_t temp = *this;
    temp -= bal_pair;
    return temp;
  }
  balance_pair_t operator-(const balance_t& bal) const {
    balance_pair_t temp = *this;
    temp -= bal;
    return temp;
  }
  balance_pair_t operator-(const amount_t& amt) const {
    balance_pair_t temp = *this;
    temp -= amt;
    return temp;
  }

  // multiplication and division
  balance_pair_t& operator*=(const balance_pair_t& bal_pair) {
    quantity *= bal_pair.quantity;
    cost     *= bal_pair.quantity;
    return *this;
  }
  balance_pair_t& operator*=(const balance_t& bal) {
    quantity *= bal;
    cost     *= bal;
    return *this;
  }
  balance_pair_t& operator*=(const amount_t& amt) {
    quantity *= amt;
    cost     *= amt;
    return *this;
  }

  balance_pair_t& operator/=(const balance_pair_t& bal_pair) {
    quantity /= bal_pair.quantity;
    cost     /= bal_pair.quantity;
    return *this;
  }
  balance_pair_t& operator/=(const balance_t& bal) {
    quantity /= bal;
    cost     /= bal;
    return *this;
  }
  balance_pair_t& operator/=(const amount_t& amt) {
    quantity /= amt;
    cost     /= amt;
    return *this;
  }

  balance_pair_t operator*(const balance_pair_t& bal_pair) const {
    balance_pair_t temp = *this;
    temp *= bal_pair;
    return temp;
  }
  balance_pair_t operator*(const balance_t& bal) const {
    balance_pair_t temp = *this;
    temp *= bal;
    return temp;
  }
  balance_pair_t operator*(const amount_t& amt) const {
    balance_pair_t temp = *this;
    temp *= amt;
    return temp;
  }

  balance_pair_t operator/(const balance_pair_t& bal_pair) const {
    balance_pair_t temp = *this;
    temp /= bal_pair;
    return temp;
  }
  balance_pair_t operator/(const balance_t& bal) const {
    balance_pair_t temp = *this;
    temp /= bal;
    return temp;
  }
  balance_pair_t operator/(const amount_t& amt) const {
    balance_pair_t temp = *this;
    temp /= amt;
    return temp;
  }

  // comparison
  bool operator<(const balance_pair_t& bal_pair) const {
    return quantity < bal_pair.quantity;
  }
  bool operator<(const balance_t& bal) const {
    return quantity < bal;
  }
  bool operator<(const amount_t& amt) const {
    return quantity < amt;
  }
  bool operator<=(const balance_pair_t& bal_pair) const {
    return quantity <= bal_pair.quantity;
  }
  bool operator<=(const balance_t& bal) const {
    return quantity <= bal;
  }
  bool operator<=(const amount_t& amt) const {
    return quantity <= amt;
  }

  bool operator>(const balance_pair_t& bal_pair) const {
    return quantity > bal_pair.quantity;
  }
  bool operator>(const balance_t& bal) const {
    return quantity > bal;
  }
  bool operator>(const amount_t& amt) const {
    return quantity > amt;
  }
  bool operator>=(const balance_pair_t& bal_pair) const {
    return quantity >= bal_pair.quantity;
  }
  bool operator>=(const balance_t& bal) const {
    return quantity >= bal;
  }
  bool operator>=(const amount_t& amt) const {
    return quantity >= amt;
  }

  bool operator==(const balance_pair_t& bal_pair) const {
    return quantity == bal_pair.quantity;
  }
  bool operator==(const balance_t& bal) const {
    return quantity == bal;
  }
  bool operator==(const amount_t& amt) const {
    return quantity == amt;
  }
  bool operator!=(const balance_pair_t& bal_pair) const {
    return ! (*this == bal_pair);
  }
  bool operator!=(const balance_t& bal) const {
    return ! (*this == bal);
  }
  bool operator!=(const amount_t& amt) const {
    return ! (*this == amt);
  }

  // unary negation
  balance_pair_t& negate() {
    quantity.negate();
    cost.negate();
    return *this;
  }
  balance_pair_t negated() const {
    balance_pair_t temp = *this;
    temp.negate();
    return temp;
  }
  balance_pair_t operator-() const {
    return negated();
  }

  // test for non-zero (use ! for zero)
  operator bool() const {
    return quantity;
  }
};

inline balance_pair_t abs(const balance_pair_t& bal_pair) {
  balance_pair_t temp;
  temp.quantity = abs(bal_pair.quantity);
  temp.cost     = abs(bal_pair.cost);
  return temp;
}

} // namespace ledger

#endif // _BALANCE_H
