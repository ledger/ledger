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
  balance_t() {
    DEBUG_PRINT("ledger.memory.ctors", "ctor balance_t");
  }
  balance_t(const balance_t& bal) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor balance_t");
    for (amounts_map::const_iterator i = bal.amounts.begin();
	 i != bal.amounts.end();
	 i++)
      *this += (*i).second;
  }
  balance_t(const amount_t& amt) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor balance_t");
    if (amt)
      amounts.insert(amounts_pair(amt.commodity, amt));
  }
  balance_t(const int value) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor balance_t");
    amount_t amt(value);
    if (amt)
      amounts.insert(amounts_pair(amt.commodity, amt));
  }
  balance_t(const unsigned int value) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor balance_t");
    amount_t amt(value);
    if (amt)
      amounts.insert(amounts_pair(amt.commodity, amt));
  }
  balance_t(const double value) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor balance_t");
    amount_t amt(value);
    if (amt)
      amounts.insert(amounts_pair(amt.commodity, amt));
  }

  // destructor
#ifdef DEBUG_ENABLED
  ~balance_t() {
    DEBUG_PRINT("ledger.memory.dtors", "dtor balance_t");
  }
#endif

  // assignment operator
  balance_t& operator=(const balance_t& bal) {
    if (this != &bal) {
      amounts.clear();
      for (amounts_map::const_iterator i = bal.amounts.begin();
	   i != bal.amounts.end();
	   i++)
	*this += (*i).second;
    }
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
  bool operator<(const unsigned int val) const {
    return amount() < val;
  }
  bool operator<=(const unsigned int val) const {
    return amount() <= val;
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
  bool operator>(const unsigned int val) const {
    return amount() > val;
  }
  bool operator>=(const unsigned int val) const {
    return amount() >= val;
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
  bool operator==(const unsigned int val) const {
    return amount() == val;
  }
  bool operator!=(const balance_t& bal) const {
    return ! (*this == bal);
  }
  bool operator!=(const amount_t& amt) const {
    return ! (*this == amt);
  }
  bool operator!=(const unsigned int val) const {
    return ! (*this == val);
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

  // conversion operators
  operator amount_t() const {
    return amount();
  }
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

  void abs() {
    for (amounts_map::iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      (*i).second.abs();
  }
};

inline balance_t abs(const balance_t& bal) {
  balance_t temp = bal;
  temp.abs();
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
  balance_t * cost;

  bool valid() const {
    return quantity.valid() && (! cost || cost->valid());
  }

  // constructors
  balance_pair_t() : cost(NULL) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor balance_pair_t");
  }
  balance_pair_t(const balance_pair_t& bal_pair)
    : quantity(bal_pair.quantity), cost(NULL) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor balance_pair_t");
    if (bal_pair.cost)
      cost = new balance_t(*bal_pair.cost);
  }
  balance_pair_t(const balance_t& _quantity)
    : quantity(_quantity), cost(NULL) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor balance_pair_t");
  }
  balance_pair_t(const amount_t& _quantity)
    : quantity(_quantity), cost(NULL) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor balance_pair_t");
  }
  balance_pair_t(const int value)
    : quantity(value), cost(NULL) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor balance_pair_t");
  }
  balance_pair_t(const unsigned int value)
    : quantity(value), cost(NULL) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor balance_pair_t");
  }
  balance_pair_t(const double value)
    : quantity(value), cost(NULL) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor balance_pair_t");
  }
  balance_pair_t(const transaction_t& xact);

  // destructor
  ~balance_pair_t() {
    DEBUG_PRINT("ledger.memory.dtors", "dtor balance_pair_t");
    if (cost)
      delete cost;
  }

  // assignment operator
  balance_pair_t& operator=(const balance_pair_t& bal_pair) {
    if (this != &bal_pair) {
      if (cost) {
	delete cost;
	cost = NULL;
      }

      quantity = bal_pair.quantity;
      if (bal_pair.cost)
	cost = new balance_t(*bal_pair.cost);
    }
    return *this;
  }
  balance_pair_t& operator=(const balance_t& bal) {
    if (cost) {
      delete cost;
      cost = NULL;
    }
    quantity = bal;
    return *this;
  }
  balance_pair_t& operator=(const amount_t& amt) {
    if (cost) {
      delete cost;
      cost = NULL;
    }
    quantity = amt;
    return *this;
  }
  balance_pair_t& operator=(const int value) {
    if (cost) {
      delete cost;
      cost = NULL;
    }
    quantity = value;
    return *this;
  }
  balance_pair_t& operator=(const unsigned int value) {
    if (cost) {
      delete cost;
      cost = NULL;
    }
    quantity = value;
    return *this;
  }
  balance_pair_t& operator=(const double value) {
    if (cost) {
      delete cost;
      cost = NULL;
    }
    quantity = value;
    return *this;
  }

  // in-place arithmetic
  balance_pair_t& operator+=(const balance_pair_t& bal_pair) {
    if (bal_pair.cost && ! cost)
      cost = new balance_t(quantity);

    quantity += bal_pair.quantity;

    if (cost)
      *cost += bal_pair.cost ? *bal_pair.cost : bal_pair.quantity;

    return *this;
  }
  balance_pair_t& operator+=(const balance_t& bal) {
    quantity += bal;
    if (cost)
      *cost += bal;
    return *this;
  }
  balance_pair_t& operator+=(const amount_t& amt) {
    quantity += amt;
    if (cost)
      *cost += amt;
    return *this;
  }
  balance_pair_t& operator+=(const transaction_t& xact);

  balance_pair_t& operator-=(const balance_pair_t& bal_pair) {
    if (bal_pair.cost && ! cost)
      cost = new balance_t(quantity);

    quantity -= bal_pair.quantity;

    if (cost)
      *cost -= bal_pair.cost ? *bal_pair.cost : bal_pair.quantity;

    return *this;
  }
  balance_pair_t& operator-=(const balance_t& bal) {
    quantity -= bal;
    if (cost)
      *cost -= bal;
    return *this;
  }
  balance_pair_t& operator-=(const amount_t& amt) {
    quantity -= amt;
    if (cost)
      *cost -= amt;
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
    if (bal_pair.cost && ! cost)
      cost = new balance_t(quantity);

    quantity *= bal_pair.quantity;

    if (cost)
      *cost *= bal_pair.cost ? *bal_pair.cost : bal_pair.quantity;

    return *this;
  }
  balance_pair_t& operator*=(const balance_t& bal) {
    quantity *= bal;
    if (cost)
      *cost *= bal;
    return *this;
  }
  balance_pair_t& operator*=(const amount_t& amt) {
    quantity *= amt;
    if (cost)
      *cost *= amt;
    return *this;
  }

  balance_pair_t& operator/=(const balance_pair_t& bal_pair) {
    if (bal_pair.cost && ! cost)
      cost = new balance_t(quantity);

    quantity /= bal_pair.quantity;

    if (cost)
      *cost /= bal_pair.cost ? *bal_pair.cost : bal_pair.quantity;

    return *this;
  }
  balance_pair_t& operator/=(const balance_t& bal) {
    quantity /= bal;
    if (cost)
      *cost /= bal;
    return *this;
  }
  balance_pair_t& operator/=(const amount_t& amt) {
    quantity /= amt;
    if (cost)
      *cost /= amt;
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
    if (cost)
      cost->negate();
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
  operator balance_t() const {
    return quantity;
  }
  operator amount_t() const {
    assert(0);
    return quantity.amount();
  }

  void abs() {
    quantity.abs();
    if (cost)
      cost->abs();
  }
};

inline balance_pair_t abs(const balance_pair_t& bal_pair) {
  balance_pair_t temp;
  temp.abs();
  return temp;
}

} // namespace ledger

#endif // _BALANCE_H
