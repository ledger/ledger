#ifndef _VALUE_H
#define _VALUE_H

#include "amount.h"
#include "balance.h"
#include "error.h"

#include <exception>

namespace ledger {

// The following type is a polymorphous value type used solely for
// performance reasons.  The alternative is to compute value
// expressions (valexpr.cc) in terms of the largest data type,
// balance_t. This was found to be prohibitively expensive, especially
// when large logic chains were involved, since many temporary
// allocations would occur for every operator.  With value_t, and the
// fact that logic chains only need boolean values to continue, no
// memory allocations need to take place at all.

class value_t
{
 public:
  char data[sizeof(balance_pair_t)];

  enum type_t {
    BOOLEAN,
    INTEGER,
    DATETIME,
    AMOUNT,
    BALANCE,
    BALANCE_PAIR
  } type;

  value_t() {
    *((long *) data) = 0;
    type = INTEGER;
  }

  value_t(const value_t& value) : type(INTEGER) {
    *this = value;
  }
  value_t(const bool value) {
    *((bool *) data) = value;
    type = BOOLEAN;
  }
  value_t(const long value) {
    *((long *) data) = value;
    type = INTEGER;
  }
  value_t(const datetime_t value) {
    *((datetime_t *) data) = value;
    type = DATETIME;
  }
  value_t(const unsigned long value) {
    new((amount_t *) data) amount_t(value);
    type = AMOUNT;
  }
  value_t(const double value) {
    new((amount_t *) data) amount_t(value);
    type = AMOUNT;
  }
  value_t(const std::string& value) {
    new((amount_t *) data) amount_t(value);
    type = AMOUNT;
  }
  value_t(const char * value) {
    new((amount_t *) data) amount_t(value);
    type = AMOUNT;
  }
  value_t(const amount_t& value) {
    new((amount_t *)data) amount_t(value);
    type = AMOUNT;
  }
  value_t(const balance_t& value) : type(INTEGER) {
    *this = value;
  }
  value_t(const balance_pair_t& value) : type(INTEGER) {
    *this = value;
  }

  ~value_t() {
    destroy();
  }

  void destroy();
  void simplify();

  value_t& operator=(const value_t& value);
  value_t& operator=(const bool value) {
    if ((bool *) data != &value) {
      destroy();
      *((bool *) data) = value;
      type = BOOLEAN;
    }
    return *this;
  }
  value_t& operator=(const long value) {
    if ((long *) data != &value) {
      destroy();
      *((long *) data) = value;
      type = INTEGER;
    }
    return *this;
  }
  value_t& operator=(const datetime_t value) {
    if ((datetime_t *) data != &value) {
      destroy();
      *((datetime_t *) data) = value;
      type = DATETIME;
    }
    return *this;
  }
  value_t& operator=(const unsigned long value) {
    return *this = amount_t(value);
  }
  value_t& operator=(const double value) {
    return *this = amount_t(value);
  }
  value_t& operator=(const std::string& value) {
    return *this = amount_t(value);
  }
  value_t& operator=(const char * value) {
    return *this = amount_t(value);
  }
  value_t& operator=(const amount_t& value) {
    if (type == AMOUNT &&
	(amount_t *) data == &value)
      return *this;
    
    if (value.realzero()) {
      return *this = 0L;
    } else {
      destroy();
      new((amount_t *)data) amount_t(value);
      type = AMOUNT;
    }
    return *this;
  }
  value_t& operator=(const balance_t& value) {
    if (type == BALANCE &&
	(balance_t *) data == &value)
      return *this;
    
    if (value.realzero()) {
      return *this = 0L;
    }
    else if (value.amounts.size() == 1) {
      return *this = (*value.amounts.begin()).second;
    }
    else {
      destroy();
      new((balance_t *)data) balance_t(value);
      type = BALANCE;
      return *this;
    }
  }
  value_t& operator=(const balance_pair_t& value) {
    if (type == BALANCE_PAIR &&
	(balance_pair_t *) data == &value)
      return *this;
    
    if (value.realzero()) {
      return *this = 0L;
    }
    else if (! value.cost) {
      return *this = value.quantity;
    }
    else {
      destroy();
      new((balance_pair_t *)data) balance_pair_t(value);
      type = BALANCE_PAIR;
      return *this;
    }
  }

  value_t& operator+=(const value_t& value);
  value_t& operator-=(const value_t& value);
  value_t& operator*=(const value_t& value);
  value_t& operator/=(const value_t& value);

  template <typename T>
  value_t& operator+=(const T& value) {
    return *this += value_t(value);
  }
  template <typename T>
  value_t& operator-=(const T& value) {
    return *this -= value_t(value);
  }
  template <typename T>
  value_t& operator*=(const T& value) {
    return *this *= value_t(value);
  }
  template <typename T>
  value_t& operator/=(const T& value) {
    return *this /= value_t(value);
  }

  value_t operator+(const value_t& value) {
    value_t temp(*this);
    temp += value;
    return temp;
  }
  value_t operator-(const value_t& value) {
    value_t temp(*this);
    temp -= value;
    return temp;
  }
  value_t operator*(const value_t& value) {
    value_t temp(*this);
    temp *= value;
    return temp;
  }
  value_t operator/(const value_t& value) {
    value_t temp(*this);
    temp /= value;
    return temp;
  }

  template <typename T>
  value_t operator+(const T& value) {
    return *this + value_t(value);
  }
  template <typename T>
  value_t operator-(const T& value) {
    return *this - value_t(value);
  }
  template <typename T>
  value_t operator*(const T& value) {
    return *this * value_t(value);
  }
  template <typename T>
  value_t operator/(const T& value) {
    return *this / value_t(value);
  }

  bool operator<(const value_t& value);
  bool operator<=(const value_t& value);
  bool operator>(const value_t& value);
  bool operator>=(const value_t& value);
  bool operator==(const value_t& value);
  bool operator!=(const value_t& value) {
    return ! (*this == value);
  }

  template <typename T>
  bool operator<(const T& value) {
    return *this < value_t(value);
  }
  template <typename T>
  bool operator<=(const T& value) {
    return *this <= value_t(value);
  }
  template <typename T>
  bool operator>(const T& value) {
    return *this > value_t(value);
  }
  template <typename T>
  bool operator>=(const T& value) {
    return *this >= value_t(value);
  }
  template <typename T>
  bool operator==(const T& value) {
    return *this == value_t(value);
  }
  template <typename T>
  bool operator!=(const T& value) {
    return ! (*this == value);
  }

  template <typename T>
  operator T() const;

  void negate();
  value_t negated() const {
    value_t temp = *this;
    temp.negate();
    return temp;
  }
  value_t operator-() const {
    return negated();
  }

  bool realzero() const {
    switch (type) {
    case BOOLEAN:
      return ! *((bool *) data);
    case INTEGER:
      return *((long *) data) == 0;
    case DATETIME:
      return ! *((datetime_t *) data);
    case AMOUNT:
      return ((amount_t *) data)->realzero();
    case BALANCE:
      return ((balance_t *) data)->realzero();
    case BALANCE_PAIR:
      return ((balance_pair_t *) data)->realzero();

    default:
      assert(0);
      break;
    }
    assert(0);
    return 0;
  }

  void    abs();
  void    cast(type_t cast_type);
  value_t cost() const;
  value_t price() const;
  value_t date() const;

  value_t strip_annotations(const bool keep_price = amount_t::keep_price,
			    const bool keep_date  = amount_t::keep_date,
			    const bool keep_tag   = amount_t::keep_tag) const;

  value_t& add(const amount_t&  amount, const amount_t * cost = NULL);
  value_t  value(const datetime_t& moment) const;
  void     reduce();

  value_t reduced() const {
    value_t temp(*this);
    temp.reduce();
    return temp;
  }

  void     round();
  value_t  unround() const;
};

#define DEF_VALUE_AUX_OP(OP)					\
  inline value_t operator OP(const balance_pair_t& value,	\
			     const value_t& obj) {		\
    return value_t(value) OP obj;				\
  }								\
  inline value_t operator OP(const balance_t& value,		\
			     const value_t& obj) {		\
    return value_t(value) OP obj;				\
  }								\
  inline value_t operator OP(const amount_t& value,		\
			     const value_t& obj) {		\
    return value_t(value) OP obj;				\
  }								\
  template <typename T>						\
  inline value_t operator OP(T value, const value_t& obj) {	\
    return value_t(value) OP obj;				\
  }

DEF_VALUE_AUX_OP(+)
DEF_VALUE_AUX_OP(-)
DEF_VALUE_AUX_OP(*)
DEF_VALUE_AUX_OP(/)

DEF_VALUE_AUX_OP(<)
DEF_VALUE_AUX_OP(<=)
DEF_VALUE_AUX_OP(>)
DEF_VALUE_AUX_OP(>=)
DEF_VALUE_AUX_OP(==)
DEF_VALUE_AUX_OP(!=)

template <typename T>
value_t::operator T() const
{
  switch (type) {
  case BOOLEAN:
    return *((bool *) data);
  case INTEGER:
    return *((long *) data);
  case DATETIME:
    return *((datetime_t *) data);
  case AMOUNT:
    return *((amount_t *) data);
  case BALANCE:
    return *((balance_t *) data);
  case BALANCE_PAIR:
    return *((balance_pair_t *) data);

  default:
    assert(0);
    break;
  }
  assert(0);
  return 0;
}

template <> value_t::operator long() const;
template <> value_t::operator datetime_t() const;
template <> value_t::operator double() const;

inline value_t abs(const value_t& value) {
  value_t temp(value);
  temp.abs();
  return temp;
}

inline std::ostream& operator<<(std::ostream& out, const value_t& value) {
  switch (value.type) {
  case value_t::BOOLEAN:
    out << (*((bool *) value.data) ? "true" : "false");
    break;
  case value_t::INTEGER:
    out << *((long *) value.data);
    break;
  case value_t::DATETIME:
    out << *((datetime_t *) value.data);
    break;
  case value_t::AMOUNT:
    out << *((amount_t *) value.data);
    break;
  case value_t::BALANCE:
    out << *((balance_t *) value.data);
    break;
  case value_t::BALANCE_PAIR:
    out << *((balance_pair_t *) value.data);
    break;

  default:
    assert(0);
    break;
  }
  return out;
}

class value_context : public error_context
{
  value_t * bal;
 public:
  value_context(const value_t& _bal,
		const std::string& desc = "") throw();
  virtual ~value_context() throw();

  virtual void describe(std::ostream& out) const throw();
};

class value_error : public error {
 public:
  value_error(const std::string& reason, error_context * ctxt = NULL) throw()
    : error(reason, ctxt) {}
  virtual ~value_error() throw() {}
};

} // namespace ledger

#endif // _VALUE_H
