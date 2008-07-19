#include "value.h"
#include "debug.h"
#include "error.h"

namespace ledger {

void value_t::destroy()
{
  switch (type) {
  case AMOUNT:
    ((amount_t *)data)->~amount_t();
    break;
  case BALANCE:
    ((balance_t *)data)->~balance_t();
    break;
  case BALANCE_PAIR:
    ((balance_pair_t *)data)->~balance_pair_t();
    break;
  default:
    break;
  }
}

void value_t::simplify()
{
  if (realzero()) {
    DEBUG_PRINT("amounts.values.simplify", "Zeroing type " << type);
    *this = 0L;
    return;
  }

  if (type == BALANCE_PAIR &&
      (! ((balance_pair_t *) data)->cost ||
       ((balance_pair_t *) data)->cost->realzero())) {
    DEBUG_PRINT("amounts.values.simplify", "Reducing balance pair to balance");
    cast(BALANCE);
  }

  if (type == BALANCE &&
      ((balance_t *) data)->amounts.size() == 1) {
    DEBUG_PRINT("amounts.values.simplify", "Reducing balance to amount");
    cast(AMOUNT);
  }

  if (type == AMOUNT &&
      ! ((amount_t *) data)->commodity()) {
    DEBUG_PRINT("amounts.values.simplify", "Reducing amount to integer");
    cast(INTEGER);
  }
}

value_t& value_t::operator=(const value_t& value)
{
  if (this == &value)
    return *this;

  destroy();

  switch (value.type) {
  case BOOLEAN:
    *((bool *) data) = *((bool *) value.data);
    break;

  case INTEGER:
    *((long *) data) = *((long *) value.data);
    break;

  case DATETIME:
    *((datetime_t *) data) = *((datetime_t *) value.data);
    break;

  case AMOUNT:
    new((amount_t *)data) amount_t(*((amount_t *) value.data));
    break;

  case BALANCE:
    new((balance_t *)data) balance_t(*((balance_t *) value.data));
    break;

  case BALANCE_PAIR:
    new((balance_pair_t *)data) balance_pair_t(*((balance_pair_t *) value.data));
    break;

  default:
    assert(0);
    break;
  }

  type = value.type;

  return *this;
}

value_t& value_t::operator+=(const value_t& value)
{
  if (value.type == BOOLEAN)
    throw new value_error("Cannot add a boolean to a value");
  else if (value.type == DATETIME)
    throw new value_error("Cannot add a date/time to a value");

  switch (type) {
  case BOOLEAN:
    throw new value_error("Cannot add a value to a boolean");

  case INTEGER:
    switch (value.type) {
    case INTEGER:
      *((long *) data) += *((long *) value.data);
      break;
    case AMOUNT:
      cast(AMOUNT);
      *((amount_t *) data) += *((amount_t *) value.data);
      break;
    case BALANCE:
      cast(BALANCE);
      *((balance_t *) data) += *((balance_t *) value.data);
      break;
    case BALANCE_PAIR:
      cast(BALANCE_PAIR);
      *((balance_pair_t *) data) += *((balance_pair_t *) value.data);
      break;
    default:
      assert(0);
      break;
    }
    break;

  case DATETIME:
    switch (value.type) {
    case INTEGER:
      *((datetime_t *) data) += *((long *) value.data);
      break;
    case AMOUNT:
      *((datetime_t *) data) += long(*((amount_t *) value.data));
      break;
    case BALANCE:
      *((datetime_t *) data) += long(*((balance_t *) value.data));
      break;
    case BALANCE_PAIR:
      *((datetime_t *) data) += long(*((balance_pair_t *) value.data));
      break;
    default:
      assert(0);
      break;
    }
    break;

  case AMOUNT:
    switch (value.type) {
    case INTEGER:
      if (*((long *) value.data) &&
	  ((amount_t *) data)->commodity()) {
	cast(BALANCE);
	return *this += value;
      }
      *((amount_t *) data) += *((long *) value.data);
      break;

    case AMOUNT:
      if (((amount_t *) data)->commodity() !=
	  ((amount_t *) value.data)->commodity()) {
	cast(BALANCE);
	return *this += value;
      }
      *((amount_t *) data) += *((amount_t *) value.data);
      break;

    case BALANCE:
      cast(BALANCE);
      *((balance_t *) data) += *((balance_t *) value.data);
      break;

    case BALANCE_PAIR:
      cast(BALANCE_PAIR);
      *((balance_pair_t *) data) += *((balance_pair_t *) value.data);
      break;

    default:
      assert(0);
      break;
    }
    break;

  case BALANCE:
    switch (value.type) {
    case INTEGER:
      *((balance_t *) data) += *((long *) value.data);
      break;
    case AMOUNT:
      *((balance_t *) data) += *((amount_t *) value.data);
      break;
    case BALANCE:
      *((balance_t *) data) += *((balance_t *) value.data);
      break;
    case BALANCE_PAIR:
      cast(BALANCE_PAIR);
      *((balance_pair_t *) data) += *((balance_pair_t *) value.data);
      break;
    default:
      assert(0);
      break;
    }
    break;

  case BALANCE_PAIR:
    switch (value.type) {
    case INTEGER:
      *((balance_pair_t *) data) += *((long *) value.data);
      break;
    case AMOUNT:
      *((balance_pair_t *) data) += *((amount_t *) value.data);
      break;
    case BALANCE:
      *((balance_pair_t *) data) += *((balance_t *) value.data);
      break;
    case BALANCE_PAIR:
      *((balance_pair_t *) data) += *((balance_pair_t *) value.data);
      break;
    default:
      assert(0);
      break;
    }
    break;

  default:
    assert(0);
    break;
  }
  return *this;
}

value_t& value_t::operator-=(const value_t& value)
{
  if (value.type == BOOLEAN)
    throw new value_error("Cannot subtract a boolean from a value");
  else if (value.type == DATETIME && type != DATETIME)
    throw new value_error("Cannot subtract a date/time from a value");

  switch (type) {
  case BOOLEAN:
    throw new value_error("Cannot subtract a value from a boolean");

  case INTEGER:
    switch (value.type) {
    case INTEGER:
      *((long *) data) -= *((long *) value.data);
      break;
    case AMOUNT:
      cast(AMOUNT);
      *((amount_t *) data) -= *((amount_t *) value.data);
      break;
    case BALANCE:
      cast(BALANCE);
      *((balance_t *) data) -= *((balance_t *) value.data);
      break;
    case BALANCE_PAIR:
      cast(BALANCE_PAIR);
      *((balance_pair_t *) data) -= *((balance_pair_t *) value.data);
      break;
    default:
      assert(0);
      break;
    }
    break;

  case DATETIME:
    switch (value.type) {
    case INTEGER:
      *((datetime_t *) data) -= *((long *) value.data);
      break;
    case DATETIME: {
      long val = *((datetime_t *) data) - *((datetime_t *) value.data);
      cast(INTEGER);
      *((long *) data) = val;
      break;
    }
    case AMOUNT:
      *((datetime_t *) data) -= long(*((amount_t *) value.data));
      break;
    case BALANCE:
      *((datetime_t *) data) -= long(*((balance_t *) value.data));
      break;
    case BALANCE_PAIR:
      *((datetime_t *) data) -= long(*((balance_pair_t *) value.data));
      break;
    default:
      assert(0);
      break;
    }
    break;

  case AMOUNT:
    switch (value.type) {
    case INTEGER:
      if (*((long *) value.data) &&
	  ((amount_t *) data)->commodity()) {
	cast(BALANCE);
	return *this -= value;
      }
      *((amount_t *) data) -= *((long *) value.data);
      break;

    case AMOUNT:
      if (((amount_t *) data)->commodity() !=
	  ((amount_t *) value.data)->commodity()) {
	cast(BALANCE);
	return *this -= value;
      }
      *((amount_t *) data) -= *((amount_t *) value.data);
      break;

    case BALANCE:
      cast(BALANCE);
      *((balance_t *) data) -= *((balance_t *) value.data);
      break;

    case BALANCE_PAIR:
      cast(BALANCE_PAIR);
      *((balance_pair_t *) data) -= *((balance_pair_t *) value.data);
      break;

    default:
      assert(0);
      break;
    }
    break;

  case BALANCE:
    switch (value.type) {
    case INTEGER:
      *((balance_t *) data) -= *((long *) value.data);
      break;
    case AMOUNT:
      *((balance_t *) data) -= *((amount_t *) value.data);
      break;
    case BALANCE:
      *((balance_t *) data) -= *((balance_t *) value.data);
      break;
    case BALANCE_PAIR:
      cast(BALANCE_PAIR);
      *((balance_pair_t *) data) -= *((balance_pair_t *) value.data);
      break;
    default:
      assert(0);
      break;
    }
    break;

  case BALANCE_PAIR:
    switch (value.type) {
    case INTEGER:
      *((balance_pair_t *) data) -= *((long *) value.data);
      break;
    case AMOUNT:
      *((balance_pair_t *) data) -= *((amount_t *) value.data);
      break;
    case BALANCE:
      *((balance_pair_t *) data) -= *((balance_t *) value.data);
      break;
    case BALANCE_PAIR:
      *((balance_pair_t *) data) -= *((balance_pair_t *) value.data);
      break;
    default:
      assert(0);
      break;
    }
    break;

  default:
    assert(0);
    break;
  }

  simplify();

  return *this;
}

value_t& value_t::operator*=(const value_t& value)
{
  if (value.type == BOOLEAN)
    throw new value_error("Cannot multiply a boolean by a value");
  else if (value.type == DATETIME)
    throw new value_error("Cannot multiply a date/time by a value");

  if (value.realzero()) {
    *this = 0L;
    return *this;
  }

  switch (type) {
  case BOOLEAN:
    throw new value_error("Cannot multiply a value by a boolean");

  case INTEGER:
    switch (value.type) {
    case INTEGER:
      *((long *) data) *= *((long *) value.data);
      break;
    case AMOUNT:
      cast(AMOUNT);
      *((amount_t *) data) *= *((amount_t *) value.data);
      break;
    case BALANCE:
      cast(BALANCE);
      *((balance_t *) data) *= *((balance_t *) value.data);
      break;
    case BALANCE_PAIR:
      cast(BALANCE_PAIR);
      *((balance_pair_t *) data) *= *((balance_pair_t *) value.data);
      break;
    default:
      assert(0);
      break;
    }
    break;

  case AMOUNT:
    switch (value.type) {
    case INTEGER:
      *((amount_t *) data) *= *((long *) value.data);
      break;
    case AMOUNT:
      *((amount_t *) data) *= *((amount_t *) value.data);
      break;
    case BALANCE:
      cast(BALANCE);
      *((balance_t *) data) *= *((balance_t *) value.data);
      break;
    case BALANCE_PAIR:
      cast(BALANCE_PAIR);
      *((balance_pair_t *) data) *= *((balance_pair_t *) value.data);
      break;
    default:
      assert(0);
      break;
    }
    break;

  case BALANCE:
    switch (value.type) {
    case INTEGER:
      *((balance_t *) data) *= *((long *) value.data);
      break;
    case AMOUNT:
      *((balance_t *) data) *= *((amount_t *) value.data);
      break;
    case BALANCE:
      *((balance_t *) data) *= *((balance_t *) value.data);
      break;
    case BALANCE_PAIR:
      cast(BALANCE_PAIR);
      *((balance_pair_t *) data) *= *((balance_pair_t *) value.data);
      break;
    default:
      assert(0);
      break;
    }
    break;

  case BALANCE_PAIR:
    switch (value.type) {
    case INTEGER:
      *((balance_pair_t *) data) *= *((long *) value.data);
      break;
    case AMOUNT:
      *((balance_pair_t *) data) *= *((amount_t *) value.data);
      break;
    case BALANCE:
      *((balance_pair_t *) data) *= *((balance_t *) value.data);
      break;
    case BALANCE_PAIR:
      *((balance_pair_t *) data) *= *((balance_pair_t *) value.data);
      break;
    default:
      assert(0);
      break;
    }
    break;

  default:
    assert(0);
    break;
  }
  return *this;
}

value_t& value_t::operator/=(const value_t& value)
{
  if (value.type == BOOLEAN)
    throw new value_error("Cannot divide a boolean by a value");
  else if (value.type == DATETIME)
    throw new value_error("Cannot divide a date/time by a value");

  switch (type) {
  case BOOLEAN:
    throw new value_error("Cannot divide a value by a boolean");

  case INTEGER:
    switch (value.type) {
    case INTEGER:
      *((long *) data) /= *((long *) value.data);
      break;
    case AMOUNT:
      cast(AMOUNT);
      *((amount_t *) data) /= *((amount_t *) value.data);
      break;
    case BALANCE:
      cast(BALANCE);
      *((balance_t *) data) /= *((balance_t *) value.data);
      break;
    case BALANCE_PAIR:
      cast(BALANCE_PAIR);
      *((balance_pair_t *) data) /= *((balance_pair_t *) value.data);
      break;
    default:
      assert(0);
      break;
    }
    break;

  case AMOUNT:
    switch (value.type) {
    case INTEGER:
      *((amount_t *) data) /= *((long *) value.data);
      break;
    case AMOUNT:
      *((amount_t *) data) /= *((amount_t *) value.data);
      break;
    case BALANCE:
      cast(BALANCE);
      *((balance_t *) data) /= *((balance_t *) value.data);
      break;
    case BALANCE_PAIR:
      cast(BALANCE_PAIR);
      *((balance_pair_t *) data) /= *((balance_pair_t *) value.data);
      break;
    default:
      assert(0);
      break;
    }
    break;

  case BALANCE:
    switch (value.type) {
    case INTEGER:
      *((balance_t *) data) /= *((long *) value.data);
      break;
    case AMOUNT:
      *((balance_t *) data) /= *((amount_t *) value.data);
      break;
    case BALANCE:
      *((balance_t *) data) /= *((balance_t *) value.data);
      break;
    case BALANCE_PAIR:
      cast(BALANCE_PAIR);
      *((balance_pair_t *) data) /= *((balance_pair_t *) value.data);
      break;
    default:
      assert(0);
      break;
    }
    break;

  case BALANCE_PAIR:
    switch (value.type) {
    case INTEGER:
      *((balance_pair_t *) data) /= *((long *) value.data);
      break;
    case AMOUNT:
      *((balance_pair_t *) data) /= *((amount_t *) value.data);
      break;
    case BALANCE:
      *((balance_pair_t *) data) /= *((balance_t *) value.data);
      break;
    case BALANCE_PAIR:
      *((balance_pair_t *) data) /= *((balance_pair_t *) value.data);
      break;
    default:
      assert(0);
      break;
    }
    break;

  default:
    assert(0);
    break;
  }
  return *this;
}

#define DEF_VALUE_CMP_OP(OP)						\
bool value_t::operator OP(const value_t& value)				\
{									\
  switch (type) {							\
  case BOOLEAN:								\
    switch (value.type) {						\
    case BOOLEAN:							\
      return *((bool *) data) OP *((bool *) value.data);		\
									\
    case INTEGER:							\
      return *((bool *) data) OP bool(*((long *) value.data));		\
									\
    case DATETIME:							\
      return *((bool *) data) OP bool(*((datetime_t *) value.data));	\
									\
    case AMOUNT:							\
      return *((bool *) data) OP bool(*((amount_t *) value.data));	\
									\
    case BALANCE:							\
      return *((bool *) data) OP bool(*((balance_t *) value.data));	\
									\
    case BALANCE_PAIR:							\
      return *((bool *) data) OP bool(*((balance_pair_t *) value.data)); \
									\
    default:								\
      assert(0);							\
      break;								\
    }									\
    break;								\
									\
  case INTEGER:								\
    switch (value.type) {						\
    case BOOLEAN:							\
      return (*((long *) data) OP					\
	      ((long) *((bool *) value.data)));				\
									\
    case INTEGER:							\
      return (*((long *) data) OP *((long *) value.data));		\
									\
    case DATETIME:							\
      return (*((long *) data) OP					\
	      ((long) ((datetime_t *) value.data)->when));		\
									\
    case AMOUNT:							\
      return (amount_t(*((long *) data)) OP				\
	      *((amount_t *) value.data));				\
									\
    case BALANCE:							\
      return (balance_t(*((long *) data)) OP				\
	      *((balance_t *) value.data));				\
									\
    case BALANCE_PAIR:							\
      return (balance_pair_t(*((long *) data)) OP			\
	      *((balance_pair_t *) value.data));			\
									\
    default:								\
      assert(0);							\
      break;								\
    }									\
    break;								\
									\
  case DATETIME:							\
    switch (value.type) {						\
    case BOOLEAN:							\
      throw new value_error("Cannot compare a date/time to a boolean");	\
									\
    case INTEGER:							\
      return (*((datetime_t *) data) OP					\
	      datetime_t(std::time_t(*((long *) value.data))));		\
									\
    case DATETIME:							\
      return (*((datetime_t *) data) OP					\
	      *((datetime_t *) value.data));				\
									\
    case AMOUNT:							\
      throw new value_error("Cannot compare a date/time to an amount");	\
									\
    case BALANCE:							\
      throw new value_error("Cannot compare a date/time to a balance");	\
									\
    case BALANCE_PAIR:							\
      throw new value_error("Cannot compare a date/time to a balance pair"); \
									\
    default:								\
      assert(0);							\
      break;								\
    }									\
    break;								\
									\
  case AMOUNT:								\
    switch (value.type) {						\
    case BOOLEAN:							\
      throw new value_error("Cannot compare an amount to a boolean");	\
									\
    case INTEGER:							\
      return (*((amount_t *) data) OP					\
	      amount_t(*((long *) value.data)));			\
									\
    case DATETIME:							\
      throw new value_error("Cannot compare an amount to a date/time");	\
									\
    case AMOUNT:							\
      return *((amount_t *) data) OP *((amount_t *) value.data);	\
									\
    case BALANCE:							\
      return (balance_t(*((amount_t *) data)) OP			\
	      *((balance_t *) value.data));				\
									\
    case BALANCE_PAIR:							\
      return (balance_t(*((amount_t *) data)) OP			\
	      *((balance_pair_t *) value.data));			\
									\
    default:								\
      assert(0);							\
      break;								\
    }									\
    break;								\
									\
  case BALANCE:								\
    switch (value.type) {						\
    case BOOLEAN:							\
      throw new value_error("Cannot compare a balance to a boolean");	\
									\
    case INTEGER:							\
      return *((balance_t *) data) OP *((long *) value.data);		\
									\
    case DATETIME:							\
      throw new value_error("Cannot compare a balance to a date/time");	\
									\
    case AMOUNT:							\
      return *((balance_t *) data) OP *((amount_t *) value.data);	\
									\
    case BALANCE:							\
      return *((balance_t *) data) OP *((balance_t *) value.data);	\
									\
    case BALANCE_PAIR:							\
      return (*((balance_t *) data) OP					\
	      ((balance_pair_t *) value.data)->quantity);		\
									\
    default:								\
      assert(0);							\
      break;								\
    }									\
    break;								\
									\
  case BALANCE_PAIR:							\
    switch (value.type) {						\
    case BOOLEAN:							\
      throw new value_error("Cannot compare a balance pair to a boolean");	\
									\
    case INTEGER:							\
      return (((balance_pair_t *) data)->quantity OP			\
	      *((long *) value.data));					\
									\
    case DATETIME:							\
      throw new value_error("Cannot compare a balance pair to a date/time"); \
									\
    case AMOUNT:							\
      return (((balance_pair_t *) data)->quantity OP			\
	      *((amount_t *) value.data));				\
									\
    case BALANCE:							\
      return (((balance_pair_t *) data)->quantity OP			\
	      *((balance_t *) value.data));				\
									\
    case BALANCE_PAIR:							\
      return (*((balance_pair_t *) data) OP				\
	      *((balance_pair_t *) value.data));			\
									\
    default:								\
      assert(0);							\
      break;								\
    }									\
    break;								\
									\
  default:								\
    assert(0);								\
    break;								\
  }									\
  return *this;								\
}

DEF_VALUE_CMP_OP(==)
DEF_VALUE_CMP_OP(<)
DEF_VALUE_CMP_OP(<=)
DEF_VALUE_CMP_OP(>)
DEF_VALUE_CMP_OP(>=)

template <>
value_t::operator long() const
{
  switch (type) {
  case BOOLEAN:
    throw new value_error("Cannot convert a boolean to an integer");
  case INTEGER:
    return *((long *) data);
  case DATETIME:
    return ((datetime_t *) data)->when;
  case AMOUNT:
    return *((amount_t *) data);
  case BALANCE:
    throw new value_error("Cannot convert a balance to an integer");
  case BALANCE_PAIR:
    throw new value_error("Cannot convert a balance pair to an integer");

  default:
    assert(0);
    break;
  }
  assert(0);
  return 0;
}

template <>
value_t::operator datetime_t() const
{
  switch (type) {
  case BOOLEAN:
    throw new value_error("Cannot convert a boolean to a date/time");
  case INTEGER:
    return *((long *) data);
  case DATETIME:
    return *((datetime_t *) data);
  case AMOUNT:
    throw new value_error("Cannot convert an amount to a date/time");
  case BALANCE:
    throw new value_error("Cannot convert a balance to a date/time");
  case BALANCE_PAIR:
    throw new value_error("Cannot convert a balance pair to a date/time");

  default:
    assert(0);
    break;
  }
  assert(0);
  return 0;
}

template <>
value_t::operator double() const
{
  switch (type) {
  case BOOLEAN:
    throw new value_error("Cannot convert a boolean to a double");
  case INTEGER:
    return *((long *) data);
  case DATETIME:
    throw new value_error("Cannot convert a date/time to a double");
  case AMOUNT:
    return *((amount_t *) data);
  case BALANCE:
    throw new value_error("Cannot convert a balance to a double");
  case BALANCE_PAIR:
    throw new value_error("Cannot convert a balance pair to a double");

  default:
    assert(0);
    break;
  }
  assert(0);
  return 0;
}

void value_t::cast(type_t cast_type)
{
  switch (type) {
  case BOOLEAN:
    switch (cast_type) {
    case BOOLEAN:
      break;
    case INTEGER:
      throw new value_error("Cannot convert a boolean to an integer");
    case DATETIME:
      throw new value_error("Cannot convert a boolean to a date/time");
    case AMOUNT:
      throw new value_error("Cannot convert a boolean to an amount");
    case BALANCE:
      throw new value_error("Cannot convert a boolean to a balance");
    case BALANCE_PAIR:
      throw new value_error("Cannot convert a boolean to a balance pair");

    default:
      assert(0);
      break;
    }
    break;

  case INTEGER:
    switch (cast_type) {
    case BOOLEAN:
      *((bool *) data) = *((long *) data);
      break;
    case INTEGER:
      break;
    case DATETIME:
      *((datetime_t *) data) = datetime_t(std::time_t(*((long *) data)));
      break;
    case AMOUNT:
      new((amount_t *)data) amount_t(*((long *) data));
      break;
    case BALANCE:
      new((balance_t *)data) balance_t(amount_t(*((long *) data)));
      break;
    case BALANCE_PAIR:
      new((balance_pair_t *)data) balance_pair_t(amount_t(*((long *) data)));
      break;

    default:
      assert(0);
      break;
    }
    break;

  case DATETIME:
    switch (cast_type) {
    case BOOLEAN:
      *((bool *) data) = *((datetime_t *) data);
      break;
    case INTEGER:
      *((long *) data) = ((datetime_t *) data)->when;
      break;
    case DATETIME:
      break;
    case AMOUNT:
      throw new value_error("Cannot convert a date/time to an amount");
    case BALANCE:
      throw new value_error("Cannot convert a date/time to a balance");
    case BALANCE_PAIR:
      throw new value_error("Cannot convert a date/time to a balance pair");

    default:
      assert(0);
      break;
    }
    break;

  case AMOUNT:
    switch (cast_type) {
    case BOOLEAN: {
      bool temp = *((amount_t *) data);
      destroy();
      *((bool *)data) = temp;
      break;
    }
    case INTEGER: {
      long temp = *((amount_t *) data);
      destroy();
      *((long *)data) = temp;
      break;
    }
    case DATETIME:
      throw new value_error("Cannot convert an amount to a date/time");
    case AMOUNT:
      break;
    case BALANCE: {
      amount_t temp = *((amount_t *) data);
      destroy();
      new((balance_t *)data) balance_t(temp);
      break;
    }
    case BALANCE_PAIR: {
      amount_t temp = *((amount_t *) data);
      destroy();
      new((balance_pair_t *)data) balance_pair_t(temp);
      break;
    }

    default:
      assert(0);
      break;
    }
    break;

  case BALANCE:
    switch (cast_type) {
    case BOOLEAN: {
      bool temp = *((balance_t *) data);
      destroy();
      *((bool *)data) = temp;
      break;
    }
    case INTEGER:
      throw new value_error("Cannot convert a balance to an integer");
    case DATETIME:
      throw new value_error("Cannot convert a balance to a date/time");

    case AMOUNT: {
      balance_t * temp = (balance_t *) data;
      if (temp->amounts.size() == 1) {
	amount_t amt = (*temp->amounts.begin()).second;
	destroy();
	new((amount_t *)data) amount_t(amt);
      }
      else if (temp->amounts.size() == 0) {
	new((amount_t *)data) amount_t();
      }
      else {
	throw new value_error("Cannot convert a balance with "
			  "multiple commodities to an amount");
      }
      break;
    }
    case BALANCE:
      break;
    case BALANCE_PAIR: {
      balance_t temp = *((balance_t *) data);
      destroy();
      new((balance_pair_t *)data) balance_pair_t(temp);
      break;
    }

    default:
      assert(0);
      break;
    }
    break;

  case BALANCE_PAIR:
    switch (cast_type) {
    case BOOLEAN: {
      bool temp = *((balance_pair_t *) data);
      destroy();
      *((bool *)data) = temp;
      break;
    }
    case INTEGER:
      throw new value_error("Cannot convert a balance pair to an integer");
    case DATETIME:
      throw new value_error("Cannot convert a balance pair to a date/time");

    case AMOUNT: {
      balance_t * temp = &((balance_pair_t *) data)->quantity;
      if (temp->amounts.size() == 1) {
	amount_t amt = (*temp->amounts.begin()).second;
	destroy();
	new((amount_t *)data) amount_t(amt);
      }
      else if (temp->amounts.size() == 0) {
	new((amount_t *)data) amount_t();
      }
      else {
	throw new value_error("Cannot convert a balance pair with "
			  "multiple commodities to an amount");
      }
      break;
    }
    case BALANCE: {
      balance_t temp = ((balance_pair_t *) data)->quantity;
      destroy();
      new((balance_t *)data) balance_t(temp);
      break;
    }
    case BALANCE_PAIR:
      break;

    default:
      assert(0);
      break;
    }
    break;

  default:
    assert(0);
    break;
  }
  type = cast_type;
}

void value_t::negate()
{
  switch (type) {
  case BOOLEAN:
    *((bool *) data) = ! *((bool *) data);
    break;
  case INTEGER:
    *((long *) data) = - *((long *) data);
    break;
  case DATETIME:
    cast(INTEGER);
    negate();
    break;
  case AMOUNT:
    ((amount_t *) data)->negate();
    break;
  case BALANCE:
    ((balance_t *) data)->negate();
    break;
  case BALANCE_PAIR:
    ((balance_pair_t *) data)->negate();
    break;

  default:
    assert(0);
    break;
  }
}

void value_t::abs()
{
  switch (type) {
  case BOOLEAN:
    break;
  case INTEGER:
    if (*((long *) data) < 0)
      *((long *) data) = - *((long *) data);
    break;
  case DATETIME:
    break;
  case AMOUNT:
    ((amount_t *) data)->abs();
    break;
  case BALANCE:
    ((balance_t *) data)->abs();
    break;
  case BALANCE_PAIR:
    ((balance_pair_t *) data)->abs();
    break;

  default:
    assert(0);
    break;
  }
}

value_t value_t::value(const datetime_t& moment) const
{
  switch (type) {
  case BOOLEAN:
    throw new value_error("Cannot find the value of a boolean");
  case DATETIME:
    throw new value_error("Cannot find the value of a date/time");
  case INTEGER:
    return *this;
  case AMOUNT:
    return ((amount_t *) data)->value(moment);
  case BALANCE:
    return ((balance_t *) data)->value(moment);
  case BALANCE_PAIR:
    return ((balance_pair_t *) data)->quantity.value(moment);
  }
}

void value_t::reduce()
{
  switch (type) {
  case BOOLEAN:
  case DATETIME:
  case INTEGER:
    break;
  case AMOUNT:
    ((amount_t *) data)->reduce();
    break;
  case BALANCE:
    ((balance_t *) data)->reduce();
    break;
  case BALANCE_PAIR:
    ((balance_pair_t *) data)->reduce();
    break;
  }
}

void value_t::round()
{
  switch (type) {
  case BOOLEAN:
    throw new value_error("Cannot round a boolean");
  case DATETIME:
    throw new value_error("Cannot round a date/time");
  case INTEGER:
    break;
  case AMOUNT:
    *((amount_t *) data) = ((amount_t *) data)->round();
    break;
  case BALANCE:
    ((balance_t *) data)->round();
    break;
  case BALANCE_PAIR:
    ((balance_pair_t *) data)->round();
    break;
  }
}

value_t value_t::unround() const
{
  value_t temp;
  switch (type) {
  case BOOLEAN:
    throw new value_error("Cannot un-round a boolean");
  case DATETIME:
    throw new value_error("Cannot un-round a date/time");
  case INTEGER:
    break;
  case AMOUNT:
    temp = ((amount_t *) data)->unround();
    break;
  case BALANCE:
    temp = ((balance_t *) data)->unround();
    break;
  case BALANCE_PAIR:
    temp = ((balance_pair_t *) data)->unround();
    break;
  }
  return temp;
}

value_t value_t::price() const
{
  switch (type) {
  case BOOLEAN:
    throw new value_error("Cannot find the price of a boolean");
  case INTEGER:
    return *this;
  case DATETIME:
    throw new value_error("Cannot find the price of a date/time");

  case AMOUNT:
    return ((amount_t *) data)->price();

  case BALANCE:
    return ((balance_t *) data)->price();

  case BALANCE_PAIR:
    return ((balance_pair_t *) data)->quantity.price();

  default:
    assert(0);
    break;
  }
  assert(0);
  return value_t();
}

value_t value_t::date() const
{
  switch (type) {
  case BOOLEAN:
    throw new value_error("Cannot find the date of a boolean");
  case INTEGER:
    return datetime_t();
  case DATETIME:
    return *this;

  case AMOUNT:
    return datetime_t(((amount_t *) data)->date());

  case BALANCE:
    return datetime_t(((balance_t *) data)->date());

  case BALANCE_PAIR:
    return datetime_t(((balance_pair_t *) data)->quantity.date());

  default:
    assert(0);
    break;
  }
  assert(0);
  return value_t();
}

value_t value_t::strip_annotations(const bool keep_price,
				   const bool keep_date,
				   const bool keep_tag) const
{
  switch (type) {
  case BOOLEAN:
  case INTEGER:
  case DATETIME:
    return *this;

  case AMOUNT:
    return ((amount_t *) data)->strip_annotations
      (keep_price, keep_date, keep_tag);
  case BALANCE:
    return ((balance_t *) data)->strip_annotations
      (keep_price, keep_date, keep_tag);
  case BALANCE_PAIR:
    return ((balance_pair_t *) data)->quantity.strip_annotations
      (keep_price, keep_date, keep_tag);

  default:
    assert(0);
    break;
  }
  assert(0);
  return value_t();
}

value_t value_t::cost() const
{
  switch (type) {
  case BOOLEAN:
    throw new value_error("Cannot find the cost of a boolean");
  case INTEGER:
  case AMOUNT:
  case BALANCE:
    return *this;
  case DATETIME:
    throw new value_error("Cannot find the cost of a date/time");

  case BALANCE_PAIR:
    assert(((balance_pair_t *) data)->cost);
    if (((balance_pair_t *) data)->cost)
      return *(((balance_pair_t *) data)->cost);
    else
      return ((balance_pair_t *) data)->quantity;

  default:
    assert(0);
    break;
  }
  assert(0);
  return value_t();
}

value_t& value_t::add(const amount_t& amount, const amount_t * cost)
{
  switch (type) {
  case BOOLEAN:
    throw new value_error("Cannot add an amount to a boolean");
  case DATETIME:
    throw new value_error("Cannot add an amount to a date/time");
  case INTEGER:
  case AMOUNT:
    if (cost) {
      cast(BALANCE_PAIR);
      return add(amount, cost);
    }
    else if ((type == AMOUNT &&
	      ((amount_t *) data)->commodity() != amount.commodity()) ||
	     (type != AMOUNT && amount.commodity())) {
      cast(BALANCE);
      return add(amount, cost);
    }
    else if (type != AMOUNT) {
      cast(AMOUNT);
    }
    *((amount_t *) data) += amount;
    break;

  case BALANCE:
    if (cost) {
      cast(BALANCE_PAIR);
      return add(amount, cost);
    }
    *((balance_t *) data) += amount;
    break;

  case BALANCE_PAIR:
    ((balance_pair_t *) data)->add(amount, cost);
    break;

  default:
    assert(0);
    break;
  }

  return *this;
}

value_context::value_context(const value_t& _bal,
			     const std::string& desc) throw()
  : bal(new value_t(_bal)), error_context(desc) {}

value_context::~value_context() throw()
{
  delete bal;
}

void value_context::describe(std::ostream& out) const throw()
{
  if (! desc.empty())
    out << desc << std::endl;

  ledger::balance_t * ptr = NULL;

  out << std::right;
  out.width(20);

  switch (bal->type) {
  case ledger::value_t::BOOLEAN:
    out << (*((bool *) bal->data) ? "true" : "false");
    break;
  case ledger::value_t::INTEGER:
    out << *((long *) bal->data);
    break;
  case ledger::value_t::DATETIME:
    out << *((datetime_t *) bal->data);
    break;
  case ledger::value_t::AMOUNT:
    out << *((ledger::amount_t *) bal->data);
    break;
  case ledger::value_t::BALANCE:
    ptr = (ledger::balance_t *) bal->data;
    // fall through...

  case ledger::value_t::BALANCE_PAIR:
    if (! ptr)
      ptr = &((ledger::balance_pair_t *) bal->data)->quantity;

    ptr->write(out, 20);
    break;
  default:
    assert(0);
    break;
  }
  out << std::endl;
}

} // namespace ledger

#ifdef USE_BOOST_PYTHON

#include <boost/python.hpp>

using namespace boost::python;
using namespace ledger;

long	 balance_len(balance_t& bal);
amount_t balance_getitem(balance_t& bal, int i);
long	 balance_pair_len(balance_pair_t& bal_pair);
amount_t balance_pair_getitem(balance_pair_t& bal_pair, int i);

long value_len(value_t& value)
{
  switch (value.type) {
  case value_t::BOOLEAN:
  case value_t::INTEGER:
  case value_t::DATETIME:
  case value_t::AMOUNT:
    return 1;

  case value_t::BALANCE:
    return balance_len(*((balance_t *) value.data));

  case value_t::BALANCE_PAIR:
    return balance_pair_len(*((balance_pair_t *) value.data));

  default:
    assert(0);
    break;
  }
  assert(0);
  return 0;
}

amount_t value_getitem(value_t& value, int i)
{
  std::size_t len = value_len(value);

  if (abs(i) >= len) {
    PyErr_SetString(PyExc_IndexError, "Index out of range");
    throw_error_already_set();
  }

  switch (value.type) {
  case value_t::BOOLEAN:
    throw new value_error("Cannot cast a boolean to an amount");

  case value_t::INTEGER:
    return long(value);

  case value_t::DATETIME:
    throw new value_error("Cannot cast a date/time to an amount");

  case value_t::AMOUNT:
    return *((amount_t *) value.data);

  case value_t::BALANCE:
    return balance_getitem(*((balance_t *) value.data), i);

  case value_t::BALANCE_PAIR:
    return balance_pair_getitem(*((balance_pair_t *) value.data), i);

  default:
    assert(0);
    break;
  }
  assert(0);
  return 0L;
}

double py_to_float(value_t& value)
{
  return double(value);
}

void export_value()
{
  scope in_value = class_< value_t > ("Value")
    .def(init<value_t>())
    .def(init<balance_pair_t>())
    .def(init<balance_t>())
    .def(init<amount_t>())
    .def(init<std::string>())
    .def(init<double>())
    .def(init<long>())
    .def(init<datetime_t>())

    .def(self + self)
    .def(self + other<balance_pair_t>())
    .def(self + other<balance_t>())
    .def(self + other<amount_t>())
    .def(self + long())
    .def(self + double())

    .def(other<balance_pair_t>() + self)
    .def(other<balance_t>() + self)
    .def(other<amount_t>() + self)
    .def(long() + self)
    .def(double() + self)

    .def(self - self)
    .def(self - other<balance_pair_t>())
    .def(self - other<balance_t>())
    .def(self - other<amount_t>())
    .def(self - long())
    .def(self - double())

    .def(other<balance_pair_t>() - self)
    .def(other<balance_t>() - self)
    .def(other<amount_t>() - self)
    .def(long() - self)
    .def(double() - self)

    .def(self * self)
    .def(self * other<balance_pair_t>())
    .def(self * other<balance_t>())
    .def(self * other<amount_t>())
    .def(self * long())
    .def(self * double())

    .def(other<balance_pair_t>() * self)
    .def(other<balance_t>() * self)
    .def(other<amount_t>() * self)
    .def(long() * self)
    .def(double() * self)

    .def(self / self)
    .def(self / other<balance_pair_t>())
    .def(self / other<balance_t>())
    .def(self / other<amount_t>())
    .def(self / long())
    .def(self / double())

    .def(other<balance_pair_t>() / self)
    .def(other<balance_t>() / self)
    .def(other<amount_t>() / self)
    .def(long() / self)
    .def(double() / self)

    .def(- self)

    .def(self += self)
    .def(self += other<balance_pair_t>())
    .def(self += other<balance_t>())
    .def(self += other<amount_t>())
    .def(self += long())
    .def(self += double())

    .def(self -= self)
    .def(self -= other<balance_pair_t>())
    .def(self -= other<balance_t>())
    .def(self -= other<amount_t>())
    .def(self -= long())
    .def(self -= double())

    .def(self *= self)
    .def(self *= other<balance_pair_t>())
    .def(self *= other<balance_t>())
    .def(self *= other<amount_t>())
    .def(self *= long())
    .def(self *= double())

    .def(self /= self)
    .def(self /= other<balance_pair_t>())
    .def(self /= other<balance_t>())
    .def(self /= other<amount_t>())
    .def(self /= long())
    .def(self /= double())

    .def(self <  self)
    .def(self < other<balance_pair_t>())
    .def(self < other<balance_t>())
    .def(self < other<amount_t>())
    .def(self < long())
    .def(self < other<datetime_t>())
    .def(self < double())

    .def(other<balance_pair_t>() < self)
    .def(other<balance_t>() < self)
    .def(other<amount_t>() < self)
    .def(long() < self)
    .def(other<datetime_t>() < self)
    .def(double() < self)

    .def(self <= self)
    .def(self <= other<balance_pair_t>())
    .def(self <= other<balance_t>())
    .def(self <= other<amount_t>())
    .def(self <= long())
    .def(self <= other<datetime_t>())
    .def(self <= double())

    .def(other<balance_pair_t>() <= self)
    .def(other<balance_t>() <= self)
    .def(other<amount_t>() <= self)
    .def(long() <= self)
    .def(other<datetime_t>() <= self)
    .def(double() <= self)

    .def(self >  self)
    .def(self > other<balance_pair_t>())
    .def(self > other<balance_t>())
    .def(self > other<amount_t>())
    .def(self > long())
    .def(self > other<datetime_t>())
    .def(self > double())

    .def(other<balance_pair_t>() > self)
    .def(other<balance_t>() > self)
    .def(other<amount_t>() > self)
    .def(long() > self)
    .def(other<datetime_t>() > self)
    .def(double() > self)

    .def(self >= self)
    .def(self >= other<balance_pair_t>())
    .def(self >= other<balance_t>())
    .def(self >= other<amount_t>())
    .def(self >= long())
    .def(self >= other<datetime_t>())
    .def(self >= double())

    .def(other<balance_pair_t>() >= self)
    .def(other<balance_t>() >= self)
    .def(other<amount_t>() >= self)
    .def(long() >= self)
    .def(other<datetime_t>() >= self)
    .def(double() >= self)

    .def(self == self)
    .def(self == other<balance_pair_t>())
    .def(self == other<balance_t>())
    .def(self == other<amount_t>())
    .def(self == long())
    .def(self == other<datetime_t>())
    .def(self == double())

    .def(other<balance_pair_t>() == self)
    .def(other<balance_t>() == self)
    .def(other<amount_t>() == self)
    .def(long() == self)
    .def(other<datetime_t>() == self)
    .def(double() == self)

    .def(self != self)
    .def(self != other<balance_pair_t>())
    .def(self != other<balance_t>())
    .def(self != other<amount_t>())
    .def(self != long())
    .def(self != other<datetime_t>())
    .def(self != double())

    .def(other<balance_pair_t>() != self)
    .def(other<balance_t>() != self)
    .def(other<amount_t>() != self)
    .def(long() != self)
    .def(other<datetime_t>() != self)
    .def(double() != self)

    .def(! self)

    .def(self_ns::int_(self))
    .def(self_ns::float_(self))
    .def(self_ns::str(self))
    .def(abs(self))

    .def_readonly("type", &value_t::type)

    .def("__len__", value_len)
    .def("__getitem__", value_getitem)

    .def("cast", &value_t::cast)
    .def("cost", &value_t::cost)
    .def("price", &value_t::price)
    .def("date", &value_t::date)
    .def("strip_annotations", &value_t::strip_annotations)
    .def("add", &value_t::add, return_internal_reference<>())
    .def("value", &value_t::value)
    .def("round", &value_t::round)
    .def("negate", &value_t::negate)
    .def("negated", &value_t::negated)
    ;

  enum_< value_t::type_t > ("ValueType")
    .value("BOOLEAN", value_t::BOOLEAN)
    .value("INTEGER", value_t::INTEGER)
    .value("DATETIME", value_t::DATETIME)
    .value("AMOUNT", value_t::AMOUNT)
    .value("BALANCE", value_t::BALANCE)
    .value("BALANCE_PAIR", value_t::BALANCE_PAIR)
    ;
}

#endif // USE_BOOST_PYTHON
