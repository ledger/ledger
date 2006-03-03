#ifndef _BALANCE_H
#define _BALANCE_H

#include "amount.h"

#include <map>
#include <ctime>
#include <iostream>

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
    if (amt)
      amounts.insert(amounts_pair(&amt.commodity(), amt));
  }
  template <typename T>
  balance_t(T value) {
    amount_t amt(value);
    if (amt)
      amounts.insert(amounts_pair(&amt.commodity(), amt));
  }

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
  template <typename T>
  balance_t& operator=(T value) {
    amounts.clear();
    *this += value;
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
    amounts_map::iterator i = amounts.find(&amt.commodity());
    if (i != amounts.end())
      (*i).second += amt;
    else if (amt)
      amounts.insert(amounts_pair(&amt.commodity(), amt));
    return *this;
  }
  template <typename T>
  balance_t& operator+=(T val) {
    return *this += amount_t(val);
  }
  balance_t& operator-=(const balance_t& bal) {
    for (amounts_map::const_iterator i = bal.amounts.begin();
	 i != bal.amounts.end();
	 i++)
      *this -= (*i).second;
    return *this;
  }
  balance_t& operator-=(const amount_t& amt) {
    amounts_map::iterator i = amounts.find(&amt.commodity());
    if (i != amounts.end())
      (*i).second -= amt;
    else if (amt)
      amounts.insert(amounts_pair(&amt.commodity(), amt));
    return *this;
  }
  template <typename T>
  balance_t& operator-=(T val) {
    return *this -= amount_t(val);
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
  template <typename T>
  balance_t operator+(T val) const {
    balance_t temp = *this;
    temp += val;
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
  template <typename T>
  balance_t operator-(T val) const {
    balance_t temp = *this;
    temp -= val;
    return temp;
  }

  // multiplication and divide
  balance_t& operator*=(const balance_t& bal);
  balance_t& operator*=(const amount_t& amt) {
    // Multiplying by the null commodity causes all amounts to be
    // increased by the same factor.
    if (! amt.commodity()) {
      for (amounts_map::iterator i = amounts.begin();
	   i != amounts.end();
	   i++)
	(*i).second *= amt;
    }
    else if (amounts.size() == 1) {
      (*amounts.begin()).second *= amt;
    }
    else {
      amounts_map::iterator i = amounts.find(&amt.commodity());
      if (i != amounts.end())
	(*i).second *= amt;
    }
    return *this;
  }
  template <typename T>
  balance_t& operator*=(T val) {
    return *this *= amount_t(val);
  }

  balance_t& operator/=(const balance_t& bal);
  balance_t& operator/=(const amount_t& amt) {
    // Dividing by the null commodity causes all amounts to be
    // increased by the same factor.
    if (! amt.commodity()) {
      for (amounts_map::iterator i = amounts.begin();
	   i != amounts.end();
	   i++)
	(*i).second /= amt;
    }
    else if (amounts.size() == 1) {
      (*amounts.begin()).second /= amt;
    }
    else {
      amounts_map::iterator i = amounts.find(&amt.commodity());
      if (i != amounts.end())
	(*i).second /= amt;
    }
    return *this;
  }
  template <typename T>
  balance_t& operator/=(T val) {
    return *this /= amount_t(val);
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
  template <typename T>
  balance_t operator*(T val) const {
    balance_t temp = *this;
    temp *= val;
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
  template <typename T>
  balance_t operator/(T val) const {
    balance_t temp = *this;
    temp /= val;
    return temp;
  }

  // comparison
  bool operator<(const balance_t& bal) const {
    for (amounts_map::const_iterator i = bal.amounts.begin();
	 i != bal.amounts.end();
	 i++)
      if (! (amount(*(*i).first) < (*i).second))
	return false;

    for (amounts_map::const_iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      if (! ((*i).second < bal.amount(*(*i).first)))
	return false;

    if (bal.amounts.size() == 0 && amounts.size() == 0)
      return false;

    return true;
  }
  bool operator<(const amount_t& amt) const {
    return amount(amt.commodity()) < amt;
  }
  template <typename T>
  bool operator<(T val) const {
    for (amounts_map::const_iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      if ((*i).second < val)
	return true;
    return false;
  }

  bool operator<=(const balance_t& bal) const {
    for (amounts_map::const_iterator i = bal.amounts.begin();
	 i != bal.amounts.end();
	 i++)
      if (! (amount(*(*i).first) <= (*i).second))
	return false;

    for (amounts_map::const_iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      if (! ((*i).second <= bal.amount(*(*i).first)))
	return false;

    return true;
  }
  bool operator<=(const amount_t& amt) const {
    return amount(amt.commodity()) <= amt;
  }
  template <typename T>
  bool operator<=(T val) const {
    for (amounts_map::const_iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      if ((*i).second <= val)
	return true;
    return false;
  }

  bool operator>(const balance_t& bal) const {
    for (amounts_map::const_iterator i = bal.amounts.begin();
	 i != bal.amounts.end();
	 i++)
      if (! (amount(*(*i).first) > (*i).second))
	return false;

    for (amounts_map::const_iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      if (! ((*i).second > bal.amount(*(*i).first)))
	return false;

    if (bal.amounts.size() == 0 && amounts.size() == 0)
      return false;

    return true;
  }
  bool operator>(const amount_t& amt) const {
    return amount(amt.commodity()) > amt;
  }
  template <typename T>
  bool operator>(T val) const {
    for (amounts_map::const_iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      if ((*i).second > val)
	return true;
    return false;
  }

  bool operator>=(const balance_t& bal) const {
    for (amounts_map::const_iterator i = bal.amounts.begin();
	 i != bal.amounts.end();
	 i++)
      if (! (amount(*(*i).first) >= (*i).second))
	return false;

    for (amounts_map::const_iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      if (! ((*i).second >= bal.amount(*(*i).first)))
	return false;

    return true;
  }
  bool operator>=(const amount_t& amt) const {
    return amount(amt.commodity()) >= amt;
  }
  template <typename T>
  bool operator>=(T val) const {
    for (amounts_map::const_iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      if ((*i).second >= val)
	return true;
    return false;
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
  template <typename T>
  bool operator==(T val) const {
    for (amounts_map::const_iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      if ((*i).second == val)
	return true;
    return false;
  }

  bool operator!=(const balance_t& bal) const {
    return ! (*this == bal);
  }
  bool operator!=(const amount_t& amt) const {
    return ! (*this == amt);
  }
  template <typename T>
  bool operator!=(T val) const {
    return ! (*this == val);
  }

  // unary negation
  void negate() {
    for (amounts_map::iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      (*i).second.negate();
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
  operator amount_t() const;
  operator bool() const {
    for (amounts_map::const_iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      if ((*i).second)
	return true;
    return false;
  }

  amount_t  amount(const commodity_t& commodity) const;
  balance_t value(const std::time_t moment) const;
  balance_t price() const;
  balance_t reduce(const bool keep_price = false,
		   const bool keep_date  = false,
		   const bool keep_tag   = false) const;

  void write(std::ostream& out, const int first_width,
	     const int latter_width = -1) const;

  void abs() {
    for (amounts_map::iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      (*i).second.abs();
  }

  void round() {
    for (amounts_map::iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      if ((*i).second.commodity())
	(*i).second = (*i).second.round((*i).second.commodity().precision());
  }
};

inline balance_t abs(const balance_t& bal) {
  balance_t temp = bal;
  temp.abs();
  return temp;
}

inline std::ostream& operator<<(std::ostream& out, const balance_t& bal) {
  bal.write(out, 12);
  return out;
}

class balance_pair_t
{
 public:
  balance_t   quantity;
  balance_t * cost;

  // constructors
  balance_pair_t() : cost(NULL) {}
  balance_pair_t(const balance_pair_t& bal_pair)
    : quantity(bal_pair.quantity), cost(NULL) {
    if (bal_pair.cost)
      cost = new balance_t(*bal_pair.cost);
  }
  balance_pair_t(const balance_t& _quantity)
    : quantity(_quantity), cost(NULL) {}
  balance_pair_t(const amount_t& _quantity)
    : quantity(_quantity), cost(NULL) {}
  template <typename T>
  balance_pair_t(T value) : quantity(value), cost(NULL) {}

  // destructor
  ~balance_pair_t() {
    if (cost) delete cost;
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
  template <typename T>
  balance_pair_t& operator=(T value) {
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
  template <typename T>
  balance_pair_t& operator+=(T val) {
    return *this += amount_t(val);
  }

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
  template <typename T>
  balance_pair_t& operator-=(T val) {
    return *this -= amount_t(val);
  }

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
  template <typename T>
  balance_pair_t operator+(T val) const {
    balance_pair_t temp = *this;
    temp += val;
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
  template <typename T>
  balance_pair_t operator-(T val) const {
    balance_pair_t temp = *this;
    temp -= val;
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
  template <typename T>
  balance_pair_t& operator*=(T val) {
    return *this *= amount_t(val);
  }

  balance_pair_t& operator/=(const balance_pair_t& bal_pair);
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
  template <typename T>
  balance_pair_t& operator/=(T val) {
    return *this /= amount_t(val);
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
  template <typename T>
  balance_pair_t operator*(T val) const {
    balance_pair_t temp = *this;
    temp *= val;
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
  template <typename T>
  balance_pair_t operator/(T val) const {
    balance_pair_t temp = *this;
    temp /= val;
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
  template <typename T>
  bool operator<(T val) const {
    return quantity < val;
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
  template <typename T>
  bool operator<=(T val) const {
    return quantity <= val;
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
  template <typename T>
  bool operator>(T val) const {
    return quantity > val;
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
  template <typename T>
  bool operator>=(T val) const {
    return quantity >= val;
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
  template <typename T>
  bool operator==(T val) const {
    return quantity == val;
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
  template <typename T>
  bool operator!=(T val) const {
    return ! (*this == val);
  }

  // unary negation
  void negate() {
    quantity.negate();
    if (cost) cost->negate();
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
    return quantity;
  }

  void abs() {
    quantity.abs();
    if (cost) cost->abs();
  }

  amount_t  amount(const commodity_t& commodity) const {
    return quantity.amount(commodity);
  }
  balance_t value(const std::time_t moment) const {
    return quantity.value(moment);
  }
  void write(std::ostream& out, const int first_width,
	     const int latter_width = -1) const {
    quantity.write(out, first_width, latter_width);
  }

  balance_pair_t& add(const amount_t&  amount,
		      const amount_t * a_cost = NULL);

  bool valid() {
    return quantity.valid() && (! cost || cost->valid());
  }

  void round() {
    quantity.round();
  }
};

inline balance_pair_t abs(const balance_pair_t& bal_pair) {
  balance_pair_t temp;
  temp.abs();
  return temp;
}

inline std::ostream& operator<<(std::ostream& out,
				const balance_pair_t& bal_pair) {
  bal_pair.quantity.write(out, 12);
  return out;
}

} // namespace ledger

#endif // _BALANCE_H
