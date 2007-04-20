#ifndef _BALANCE_H
#define _BALANCE_H

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
    TRACE_CTOR("balance_t()");
  }
  balance_t(const balance_t& bal) {
    TRACE_CTOR("balance_t(copy)");
    for (amounts_map::const_iterator i = bal.amounts.begin();
	 i != bal.amounts.end();
	 i++)
      *this += (*i).second;
  }
  balance_t(const amount_t& amt) {
    TRACE_CTOR("balance_t(const amount_t&)");
    if (! amt.realzero())
      amounts.insert(amounts_pair(&amt.commodity(), amt));
  }
  template <typename T>
  balance_t(T val) {
    TRACE_CTOR("balance_t(T)");
    amount_t amt(val);
    if (! amt.realzero())
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
  balance_t& operator=(T val) {
    amounts.clear();
    *this += val;
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
    else if (! amt.realzero())
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
    if (i != amounts.end()) {
      (*i).second -= amt;
      if ((*i).second.realzero())
	amounts.erase(i);
    }
    else if (! amt.realzero()) {
      amounts.insert(amounts_pair(&amt.commodity(), - amt));
    }
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
  balance_t& operator*=(const amount_t& amt);
  template <typename T>
  balance_t& operator*=(T val) {
    return *this *= amount_t(val);
  }

  balance_t& operator/=(const balance_t& bal);
  balance_t& operator/=(const amount_t& amt);
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
    if (amt.commodity())
      return amount(amt.commodity()) < amt;

    for (amounts_map::const_iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      if ((*i).second < amt)
	return true;
    return false;
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
    if (amt.commodity())
      return amount(amt.commodity()) <= amt;

    for (amounts_map::const_iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      if ((*i).second <= amt)
	return true;
    return false;
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
    if (amt.commodity())
      return amount(amt.commodity()) > amt;

    for (amounts_map::const_iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      if ((*i).second > amt)
	return true;
    return false;
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
    if (amt.commodity())
      return amount(amt.commodity()) >= amt;

    for (amounts_map::const_iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      if ((*i).second >= amt)
	return true;
    return false;
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
    if (amt.commodity())
      return amounts.size() == 1 && (*amounts.begin()).second == amt;

    for (amounts_map::const_iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      if ((*i).second == amt)
	return true;
    return false;
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
  void in_place_negate() {
    for (amounts_map::iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      (*i).second = (*i).second.negate();
  }
  balance_t negate() const {
    balance_t temp = *this;
    temp.in_place_negate();
    return temp;
  }
  balance_t operator-() const {
    return negate();
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

  bool realzero() const {
    if (amounts.size() == 0)
      return true;
    for (amounts_map::const_iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      if (! (*i).second.realzero())
	return false;
    return true;
  }

  amount_t  amount(const commodity_t& commodity = 
		   *commodity_t::null_commodity) const;
  balance_t value(const moment_t& moment = now) const;
  balance_t price() const;
  moment_t     date() const;

  balance_t
  strip_annotations(const bool keep_price = amount_t::keep_price,
		    const bool keep_date  = amount_t::keep_date,
		    const bool keep_tag   = amount_t::keep_tag) const;

  void write(std::ostream& out, const int first_width,
	     const int latter_width = -1) const;

  void in_place_abs() {
    for (amounts_map::iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      (*i).second = (*i).second.abs();
  }
  balance_t abs() const {
    balance_t temp = *this;
    temp.in_place_abs();
    return temp;
  }

  void in_place_reduce() {
    for (amounts_map::iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      (*i).second.in_place_reduce();
  }
  balance_t reduce() const {
    balance_t temp(*this);
    temp.in_place_reduce();
    return temp;
  }

  void in_place_round() {
    for (amounts_map::iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      (*i).second = (*i).second.round();
  }
  balance_t round() const {
    balance_t temp(*this);
    temp.in_place_round();
    return temp;
  }

  balance_t unround() const {
    balance_t temp;
    for (amounts_map::const_iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      if ((*i).second.commodity())
	temp += (*i).second.unround();
    return temp;
  }
};

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
  balance_pair_t() : cost(NULL) {
    TRACE_CTOR("balance_pair_t()");
  }
  balance_pair_t(const balance_pair_t& bal_pair)
    : quantity(bal_pair.quantity), cost(NULL) {
    TRACE_CTOR("balance_pair_t(copy)");
    if (bal_pair.cost)
      cost = new balance_t(*bal_pair.cost);
  }
  balance_pair_t(const balance_t& _quantity)
    : quantity(_quantity), cost(NULL) {
    TRACE_CTOR("balance_pair_t(const balance_t&)");
  }
  balance_pair_t(const amount_t& _quantity)
    : quantity(_quantity), cost(NULL) {
    TRACE_CTOR("balance_pair_t(const amount_t&)");
  }
  template <typename T>
  balance_pair_t(T val) : quantity(val), cost(NULL) {
    TRACE_CTOR("balance_pair_t(T)");
  }

  // destructor
  ~balance_pair_t() {
    TRACE_DTOR("balance_pair_t");
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
  balance_pair_t& operator=(T val) {
    if (cost) {
      delete cost;
      cost = NULL;
    }
    quantity = val;
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
  void in_place_negate() {
    quantity = quantity.negate();
    if (cost)
      *cost = cost->negate();
  }
  balance_pair_t negate() const {
    balance_pair_t temp = *this;
    temp.in_place_negate();
    return temp;
  }
  balance_pair_t operator-() const {
    return negate();
  }

  // test for non-zero (use ! for zero)
  operator balance_t() const {
    return quantity;
  }
  operator amount_t() const {
    return quantity;
  }
  operator bool() const {
    return quantity;
  }

  bool realzero() const {
    return ((! cost || cost->realzero()) && quantity.realzero());
  }

  balance_pair_t in_place_abs() {
    quantity = quantity.abs();
    if (cost)
      *cost = cost->abs();
  }
  balance_pair_t abs() const {
    balance_pair_t temp = *this;
    temp.in_place_abs();
    return temp;
  }

  amount_t  amount(const commodity_t& commodity =
		   *commodity_t::null_commodity) const {
    return quantity.amount(commodity);
  }
  balance_t value(const moment_t& moment = now) const {
    return quantity.value(moment);
  }
  balance_t price() const {
    return quantity.price();
  }
  moment_t     date() const {
    return quantity.date();
  }

  balance_t
  strip_annotations(const bool keep_price = amount_t::keep_price,
		    const bool keep_date  = amount_t::keep_date,
		    const bool keep_tag   = amount_t::keep_tag) const {
    return quantity.strip_annotations(keep_price, keep_date, keep_tag);
  }

  void write(std::ostream& out, const int first_width,
	     const int latter_width = -1) const {
    quantity.write(out, first_width, latter_width);
  }

  balance_pair_t& add(const amount_t&  amt,
		      const amount_t * a_cost = NULL) {
    if (a_cost && ! cost)
      cost = new balance_t(quantity);
    quantity += amt;
    if (cost)
      *cost += a_cost ? *a_cost : amt;
    return *this;
  }

  bool valid() {
    return quantity.valid() && (! cost || cost->valid());
  }

  void in_place_reduce() {
    quantity.in_place_reduce();
    if (cost) cost->in_place_reduce();
  }
  balance_pair_t reduce() const {
    balance_pair_t temp(*this);
    temp.in_place_reduce();
    return temp;
  }

  void in_place_round() {
    quantity = quantity.round();
    if (cost)
      *cost = cost->round();
  }
  balance_pair_t round() const {
    balance_pair_t temp(*this);
    temp.in_place_round();
    return temp;
  }

  balance_pair_t unround() {
    balance_pair_t temp(quantity.unround());
    if (cost)
      temp.cost = new balance_t(cost->unround());
    return temp;
  }
};

inline std::ostream& operator<<(std::ostream& out,
				const balance_pair_t& bal_pair) {
  bal_pair.quantity.write(out, 12);
  return out;
}

} // namespace ledger

#endif // _BALANCE_H
