#ifndef _VALUE_H
#define _VALUE_H

#include "amount.h"
#include "balance.h"
#include "debug.h"

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
  value_t(const value_t& copy);

 public:
  char data[sizeof(balance_t)];

  enum type_t {
    BOOLEAN,
    INTEGER,
    AMOUNT,
    BALANCE,
    ANY
  } type;

  value_t() {
    *((unsigned int *) data) = 0;
    type = INTEGER;
  }

  value_t(const bool value) {
    *((bool *) data) = value;
    type = BOOLEAN;
  }
  value_t(const unsigned int value) {
    *((unsigned int *) data) = value;
    type = INTEGER;
  }
  value_t(const amount_t& value) {
    new((amount_t *)data) amount_t(value);
    type = AMOUNT;
  }
  value_t(const balance_t& value) {
    new((balance_t *)data) balance_t(value);
    type = BALANCE;
  }

  ~value_t() {
    destroy();
  }

  void destroy();

  value_t& operator=(const bool value) {
    destroy();
    *((bool *) data) = value;
    type = BOOLEAN;
    return *this;
  }
  value_t& operator=(const unsigned int value) {
    destroy();
    *((unsigned int *) data) = value;
    type = INTEGER;
    return *this;
  }
  value_t& operator=(const amount_t& value) {
    destroy();
    new((amount_t *)data) amount_t(value);
    type = AMOUNT;
    return *this;
  }
  value_t& operator=(const balance_t& value) {
    destroy();
    new((balance_t *)data) balance_t(value);
    type = BALANCE;
    return *this;
  }

  value_t& operator+=(const value_t& value);
  value_t& operator-=(const value_t& value);
  value_t& operator*=(const value_t& value);
  value_t& operator/=(const value_t& value);

  bool operator==(const value_t& value);
  bool operator!=(const value_t& value) {
    return ! (*this == value);
  }

  bool operator<(const value_t& value);
  bool operator<=(const value_t& value);
  bool operator>(const value_t& value);
  bool operator>=(const value_t& value);

  template <typename T>
  operator T() const;

  void cast(type_t cast_type);
  void negate();
  void abs();
};

template <typename T>
value_t::operator T() const
{
  switch (type) {
  case BOOLEAN:
    return *((bool *) data);
  case INTEGER:
    return *((unsigned int *) data);
  case AMOUNT:
    return *((amount_t *) data);
  case BALANCE:
    return *((balance_t *) data);

  default:
    assert(0);
    break;
  }
  assert(0);
  return 0;
 }

} // namespace ledger

#endif // _VALUE_H
