#include "value.h"
#include "ledger.h"

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
    *((unsigned int *) data) = *((unsigned int *) value.data);
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

#define DEF_VALUE_OP(OP)						\
value_t& value_t::operator OP(const value_t& value)			\
{									\
  switch (value.type) {							\
  case BOOLEAN:								\
  case INTEGER:								\
    switch (type) {							\
    case BOOLEAN:							\
      cast(INTEGER);							\
									\
    case INTEGER:							\
      *((unsigned int *) data) OP *((unsigned int *) value.data);	\
      break;								\
									\
    case AMOUNT:							\
      *((amount_t *) data) OP *((unsigned int *) value.data);		\
      break;								\
									\
    case BALANCE:							\
      *((balance_t *) data) OP amount_t(*((unsigned int *) value.data)); \
      break;								\
									\
    case BALANCE_PAIR:							\
      *((balance_pair_t *) data) OP amount_t(*((unsigned int *) value.data)); \
      break;								\
									\
    default:								\
      assert(0);							\
      break;								\
    }									\
    break;								\
									\
  case AMOUNT:								\
    switch (type) {							\
    case BOOLEAN:							\
    case INTEGER:							\
      cast(AMOUNT);							\
									\
    case AMOUNT:							\
      if (((amount_t *) data)->commodity &&				\
	  ((amount_t *) data)->commodity !=				\
	  ((amount_t *) value.data)->commodity) {			\
	cast(BALANCE);							\
	return *this OP value;						\
      }									\
      *((amount_t *) data) OP *((amount_t *) value.data);		\
      break;								\
									\
    case BALANCE:							\
      *((balance_t *) data) OP *((amount_t *) value.data);		\
      break;								\
									\
    case BALANCE_PAIR:							\
      *((balance_pair_t *) data) OP *((amount_t *) value.data);		\
      break;								\
									\
    default:								\
      assert(0);							\
      break;								\
    }									\
    break;								\
									\
  case BALANCE:								\
    switch (type) {							\
    case BOOLEAN:							\
    case INTEGER:							\
    case AMOUNT:							\
      cast(BALANCE);							\
									\
    case BALANCE:							\
      *((balance_t *) data) OP *((balance_t *) value.data);		\
      break;								\
									\
    case BALANCE_PAIR:							\
      *((balance_pair_t *) data) OP *((balance_t *) value.data);	\
      break;								\
									\
    default:								\
      assert(0);							\
      break;								\
    }									\
    break;								\
									\
  case BALANCE_PAIR:							\
    switch (type) {							\
    case BOOLEAN:							\
    case INTEGER:							\
    case AMOUNT:							\
    case BALANCE:							\
      cast(BALANCE_PAIR);						\
									\
    case BALANCE_PAIR:							\
      *((balance_pair_t *) data) OP *((balance_pair_t *) value.data);	\
      break;								\
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

DEF_VALUE_OP(+=)
DEF_VALUE_OP(-=)
DEF_VALUE_OP(*=)
DEF_VALUE_OP(/=)

value_t& value_t::operator +=(const transaction_t& xact)
{
  switch (type) {
  case BOOLEAN:
  case INTEGER:
    cast(AMOUNT);

  case AMOUNT:
    if (xact.cost) {
      cast(BALANCE_PAIR);
      return *this += xact;
    }
    else if (((amount_t *) data)->commodity &&
	     ((amount_t *) data)->commodity != xact.amount.commodity) {
      cast(BALANCE);
      return *this += xact;
    }
    *((amount_t *) data) += xact.amount;
    break;

  case BALANCE:
    if (xact.cost) {
      cast(BALANCE_PAIR);
      return *this += xact;
    }
    *((balance_t *) data) += xact.amount;
    break;

  case BALANCE_PAIR:
    *((balance_pair_t *) data) += xact;
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
  switch (value.type) {							\
  case BOOLEAN:								\
    switch (type) {							\
    case BOOLEAN:							\
      return *((bool *) data) OP *((bool *) value.data);		\
									\
    case INTEGER:							\
      return bool(*((unsigned int *) data)) OP *((bool *) value.data);	\
									\
    case AMOUNT:							\
      return bool(*((amount_t *) data)) OP *((bool *) value.data);	\
									\
    case BALANCE:							\
      return bool(*((balance_t *) data)) OP *((bool *) value.data);	\
									\
    case BALANCE_PAIR:							\
      return bool(*((balance_pair_t *) data)) OP *((bool *) value.data); \
									\
    default:								\
      assert(0);							\
      break;								\
    }									\
    break;								\
									\
  case INTEGER:								\
    switch (type) {							\
    case BOOLEAN:							\
      return ((unsigned int) *((bool *) data)) OP *((unsigned int *) value.data); \
									\
    case INTEGER:							\
      return *((unsigned int *) data) OP *((unsigned int *) value.data); \
									\
    case AMOUNT:							\
      return ((unsigned int) *((amount_t *) data)) OP *((unsigned int *) value.data); \
									\
    case BALANCE:							\
      return ((unsigned int) *((balance_t *) data)) OP *((unsigned int *) value.data); \
									\
    case BALANCE_PAIR:							\
      return ((unsigned int) *((balance_pair_t *) data)) OP *((unsigned int *) value.data); \
									\
    default:								\
      assert(0);							\
      break;								\
    }									\
    break;								\
									\
  case AMOUNT:								\
    switch (type) {							\
    case BOOLEAN:							\
      return amount_t(*((bool *) data)) OP *((amount_t *) value.data);	\
									\
    case INTEGER:							\
      return amount_t(*((unsigned int *) data)) OP *((amount_t *) value.data); \
									\
    case AMOUNT:							\
      return *((amount_t *) data) OP *((amount_t *) value.data);	\
									\
    case BALANCE:							\
      return ((balance_t *) data)->amount(((amount_t *) value.data)->commodity) OP *((amount_t *) value.data); \
									\
    case BALANCE_PAIR:							\
      return ((balance_pair_t *) data)->quantity.amount(((amount_t *) value.data)->commodity) OP *((amount_t *) value.data); \
									\
    default:								\
      assert(0);							\
      break;								\
    }									\
    break;								\
									\
  case BALANCE:								\
    switch (type) {							\
    case BOOLEAN:							\
      return balance_t(*((bool *) data)) OP *((balance_t *) value.data); \
									\
    case INTEGER:							\
      return balance_t(*((unsigned int *) data)) OP *((balance_t *) value.data); \
									\
    case AMOUNT:							\
      return balance_t(*((amount_t *) data)) OP *((balance_t *) value.data); \
									\
    case BALANCE:							\
      return *((balance_t *) data) OP *((balance_t *) value.data);	\
									\
    case BALANCE_PAIR:							\
      return *((balance_pair_t *) data) OP *((balance_t *) value.data);	\
									\
    default:								\
      assert(0);							\
      break;								\
    }									\
    break;								\
									\
  case BALANCE_PAIR:							\
    switch (type) {							\
    case BOOLEAN:							\
      return balance_pair_t(*((bool *) data)) OP *((balance_pair_t *) value.data); \
									\
    case INTEGER:							\
      return balance_pair_t(*((unsigned int *) data)) OP *((balance_pair_t *) value.data); \
									\
    case AMOUNT:							\
      return balance_pair_t(*((amount_t *) data)) OP *((balance_pair_t *) value.data); \
									\
    case BALANCE:							\
      return balance_pair_t(*((balance_t *) data)) OP *((balance_pair_t *) value.data);	\
									\
    case BALANCE_PAIR:							\
      return *((balance_pair_t *) data) OP *((balance_pair_t *) value.data); \
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

void value_t::cast(type_t cast_type)
{
  switch (type) {
  case BOOLEAN:
    switch (cast_type) {
    case BOOLEAN:
      break;
    case INTEGER:
      *((unsigned int *) data) = *((bool *) data);
      break;
    case AMOUNT:
      new((amount_t *)data) amount_t(*((bool *) data));
      break;
    case BALANCE:
      new((balance_t *)data) balance_t(*((bool *) data));
      break;
    case BALANCE_PAIR:
      new((balance_pair_t *)data) balance_pair_t(*((bool *) data));
      break;

    default:
      assert(0);
      break;
    }
    break;

  case INTEGER:
    switch (cast_type) {
    case BOOLEAN:
      *((bool *) data) = *((unsigned int *) data);
      break;
    case INTEGER:
      break;
    case AMOUNT:
      new((amount_t *)data) amount_t(*((unsigned int *) data));
      break;
    case BALANCE:
      new((balance_t *)data) balance_t(*((unsigned int *) data));
      break;
    case BALANCE_PAIR:
      new((balance_pair_t *)data) balance_pair_t(*((unsigned int *) data));
      break;

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
      unsigned int temp = *((amount_t *) data);
      destroy();
      *((unsigned int *)data) = temp;
      break;
    }
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
    case INTEGER: {
      unsigned int temp = ((balance_t *) data)->amount();
      destroy();
      *((unsigned int *)data) = temp;
      break;
    }
    case AMOUNT: {
      amount_t temp = ((balance_t *) data)->amount();
      destroy();
      new((amount_t *)data) amount_t(temp);
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
    case INTEGER: {
      unsigned int temp = ((balance_pair_t *) data)->quantity.amount();
      destroy();
      *((unsigned int *)data) = temp;
      break;
    }
    case AMOUNT: {
      amount_t temp = ((balance_pair_t *) data)->quantity.amount();
      destroy();
      new((amount_t *)data) amount_t(temp);
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
    *((unsigned int *) data) = - *((unsigned int *) data);
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
    if (*((unsigned int *) data) < 0)
      *((unsigned int *) data) = - *((unsigned int *) data);
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

value_t value_t::cost() const
{
  switch (type) {
  case BOOLEAN:
  case INTEGER:
  case AMOUNT:
  case BALANCE:
    return *this;

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

} // namespace ledger

#ifdef USE_BOOST_PYTHON

#include <boost/python.hpp>

using namespace boost::python;
using namespace ledger;

void export_value()
{
  class_< value_t > ("Value")
    ;
}

#endif // USE_BOOST_PYTHON
