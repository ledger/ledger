#include "value.h"

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
  default:
    break;
  }
}

value_t& value_t::operator+=(const value_t& value)
{
  switch (value.type) {
  case BOOLEAN:
  case INTEGER:
    switch (type) {
    case BOOLEAN:
      cast(INTEGER);
    case INTEGER:
      *((unsigned int *) data) += *((unsigned int *) value.data);
      break;

    case AMOUNT:
      *((amount_t *) data) += *((unsigned int *) value.data);
      break;

    case BALANCE:
      *((balance_t *) data) +=  amount_t(*((unsigned int *) value.data));
      break;

    default:
      assert(0);
      break;
    }
    break;

  case AMOUNT:
    switch (type) {
    case BOOLEAN:
    case INTEGER:
      cast(AMOUNT);
    case AMOUNT:
      *((amount_t *) data) += *((amount_t *) value.data);
      break;

    case BALANCE:
      *((balance_t *) data) += *((amount_t *) value.data);
      break;

    default:
      assert(0);
      break;
    }
    break;

  case BALANCE:
    switch (type) {
    case BOOLEAN:
    case INTEGER:
    case AMOUNT:
      cast(BALANCE);
    case BALANCE:
      *((balance_t *) data) += *((balance_t *) value.data);
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
  switch (value.type) {
  case BOOLEAN:
  case INTEGER:
    switch (type) {
    case BOOLEAN:
      cast(INTEGER);
    case INTEGER:
      *((unsigned int *) data) -= *((unsigned int *) value.data);
      break;

    case AMOUNT:
      *((amount_t *) data) -= *((unsigned int *) value.data);
      break;

    case BALANCE:
      *((balance_t *) data) -= amount_t(*((unsigned int *) value.data));
      break;

    default:
      assert(0);
      break;
    }
    break;

  case AMOUNT:
    switch (type) {
    case BOOLEAN:
    case INTEGER:
      cast(AMOUNT);
    case AMOUNT:
      *((amount_t *) data) -= *((amount_t *) value.data);
      break;

    case BALANCE:
      *((balance_t *) data) -= *((amount_t *) value.data);
      break;

    default:
      assert(0);
      break;
    }
    break;

  case BALANCE:
    switch (type) {
    case BOOLEAN:
    case INTEGER:
    case AMOUNT:
      cast(BALANCE);
    case BALANCE:
      *((balance_t *) data) -= *((balance_t *) value.data);
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

value_t& value_t::operator*=(const value_t& value)
{
  switch (value.type) {
  case BOOLEAN:
  case INTEGER:
    switch (type) {
    case BOOLEAN:
      cast(INTEGER);
    case INTEGER:
      *((unsigned int *) data) *= *((unsigned int *) value.data);
      break;

    case AMOUNT:
      *((amount_t *) data) *= *((unsigned int *) value.data);
      break;

    case BALANCE:
      *((balance_t *) data) *= amount_t(*((unsigned int *) value.data));
      break;

    default:
      assert(0);
      break;
    }
    break;

  case AMOUNT:
    switch (type) {
    case BOOLEAN:
    case INTEGER:
      cast(AMOUNT);
    case AMOUNT:
      *((amount_t *) data) *= *((amount_t *) value.data);
      break;

    case BALANCE:
      *((balance_t *) data) *= *((amount_t *) value.data);
      break;

    default:
      assert(0);
      break;
    }
    break;

  case BALANCE:
    switch (type) {
    case BOOLEAN:
    case INTEGER:
    case AMOUNT:
      cast(BALANCE);
    case BALANCE:
      *((balance_t *) data) *= *((balance_t *) value.data);
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
  switch (value.type) {
  case BOOLEAN:
  case INTEGER:
    switch (type) {
    case BOOLEAN:
      cast(INTEGER);
    case INTEGER:
      *((unsigned int *) data) /= *((unsigned int *) value.data);
      break;

    case AMOUNT:
      *((amount_t *) data) /= *((unsigned int *) value.data);
      break;

    case BALANCE:
      *((balance_t *) data) /= amount_t(*((unsigned int *) value.data));
      break;

    default:
      assert(0);
      break;
    }
    break;

  case AMOUNT:
    switch (type) {
    case BOOLEAN:
    case INTEGER:
      cast(AMOUNT);
    case AMOUNT:
      *((amount_t *) data) /= *((amount_t *) value.data);
      break;

    case BALANCE:
      *((balance_t *) data) /= *((amount_t *) value.data);
      break;

    default:
      assert(0);
      break;
    }
    break;

  case BALANCE:
    switch (type) {
    case BOOLEAN:
    case INTEGER:
    case AMOUNT:
      cast(BALANCE);
    case BALANCE:
      *((balance_t *) data) /= *((balance_t *) value.data);
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

bool value_t::operator==(const value_t& value)
{
  switch (value.type) {
  case BOOLEAN:
    switch (type) {
    case BOOLEAN:
      return *((bool *) data) == *((bool *) value.data);

    case INTEGER:
      return bool(*((unsigned int *) data)) == *((bool *) value.data);

    case AMOUNT:
      return bool(*((amount_t *) data)) == *((bool *) value.data);

    case BALANCE:
      return bool(*((balance_t *) data)) == *((bool *) value.data);

    default:
      assert(0);
      break;
    }
    break;

  case INTEGER:
    switch (type) {
    case BOOLEAN:
      return ((unsigned int) *((bool *) data)) == *((unsigned int *) value.data);

    case INTEGER:
      return *((unsigned int *) data) == *((unsigned int *) value.data);

    case AMOUNT:
      return ((unsigned int) *((amount_t *) data)) == *((unsigned int *) value.data);

    case BALANCE:
      return ((unsigned int) *((balance_t *) data)) == *((unsigned int *) value.data);

    default:
      assert(0);
      break;
    }
    break;

  case AMOUNT:
    switch (type) {
    case BOOLEAN:
      return amount_t(*((bool *) data)) == *((amount_t *) value.data);

    case INTEGER:
      return amount_t(*((unsigned int *) data)) == *((amount_t *) value.data);

    case AMOUNT:
      return *((amount_t *) data) == *((amount_t *) value.data);

    case BALANCE:
      return ((balance_t *) data)->amount() == *((amount_t *) value.data);

    default:
      assert(0);
      break;
    }
    break;

  case BALANCE:
    switch (type) {
    case BOOLEAN:
      return balance_t(*((bool *) data)) == *((balance_t *) value.data);

    case INTEGER:
      return balance_t(*((unsigned int *) data)) == *((balance_t *) value.data);

    case AMOUNT:
      return balance_t(*((amount_t *) data)) == *((balance_t *) value.data);

    case BALANCE:
      return *((balance_t *) data) == *((balance_t *) value.data);

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

bool value_t::operator<(const value_t& value)
{
  switch (value.type) {
  case BOOLEAN:
    switch (type) {
    case BOOLEAN:
      return *((bool *) data) < *((bool *) value.data);

    case INTEGER:
      return bool(*((unsigned int *) data)) < *((bool *) value.data);

    case AMOUNT:
      return bool(*((amount_t *) data)) < *((bool *) value.data);

    case BALANCE:
      return bool(*((balance_t *) data)) < *((bool *) value.data);

    default:
      assert(0);
      break;
    }
    break;

  case INTEGER:
    switch (type) {
    case BOOLEAN:
      return ((unsigned int) *((bool *) data)) < *((unsigned int *) value.data);

    case INTEGER:
      return *((unsigned int *) data) < *((unsigned int *) value.data);

    case AMOUNT:
      return *((amount_t *) data) < *((unsigned int *) value.data);

    case BALANCE:
      return *((balance_t *) data) < *((unsigned int *) value.data);

    default:
      assert(0);
      break;
    }
    break;

  case AMOUNT:
    switch (type) {
    case BOOLEAN:
      return amount_t(*((bool *) data)) < *((amount_t *) value.data);

    case INTEGER:
      return amount_t(*((unsigned int *) data)) < *((amount_t *) value.data);

    case AMOUNT:
      return *((amount_t *) data) < *((amount_t *) value.data);

    case BALANCE:
      return ((balance_t *) data)->amount(((amount_t *) value.data)->commodity) < *((amount_t *) value.data);

    default:
      assert(0);
      break;
    }
    break;

  case BALANCE:
    switch (type) {
    case BOOLEAN:
      return balance_t(*((bool *) data)) < *((balance_t *) value.data);

    case INTEGER:
      return balance_t(*((unsigned int *) data)) < *((balance_t *) value.data);

    case AMOUNT:
      return balance_t(*((amount_t *) data)) < *((balance_t *) value.data);

    case BALANCE:
      return *((balance_t *) data) < *((balance_t *) value.data);

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

bool value_t::operator<=(const value_t& value)
{
  switch (value.type) {
  case BOOLEAN:
    switch (type) {
    case BOOLEAN:
      return *((bool *) data) <= *((bool *) value.data);

    case INTEGER:
      return bool(*((unsigned int *) data)) <= *((bool *) value.data);

    case AMOUNT:
      return bool(*((amount_t *) data)) <= *((bool *) value.data);

    case BALANCE:
      return bool(*((balance_t *) data)) <= *((bool *) value.data);

    default:
      assert(0);
      break;
    }
    break;

  case INTEGER:
    switch (type) {
    case BOOLEAN:
      return ((unsigned int) *((bool *) data)) <= *((unsigned int *) value.data);

    case INTEGER:
      return *((unsigned int *) data) <= *((unsigned int *) value.data);

    case AMOUNT:
      return ((unsigned int) *((amount_t *) data)) <= *((unsigned int *) value.data);

    case BALANCE:
      return ((unsigned int) *((balance_t *) data)) <= *((unsigned int *) value.data);

    default:
      assert(0);
      break;
    }
    break;

  case AMOUNT:
    switch (type) {
    case BOOLEAN:
      return amount_t(*((bool *) data)) <= *((amount_t *) value.data);

    case INTEGER:
      return amount_t(*((unsigned int *) data)) <= *((amount_t *) value.data);

    case AMOUNT:
      return *((amount_t *) data) <= *((amount_t *) value.data);

    case BALANCE:
      return ((balance_t *) data)->amount() <= *((amount_t *) value.data);

    default:
      assert(0);
      break;
    }
    break;

  case BALANCE:
    switch (type) {
    case BOOLEAN:
      return balance_t(*((bool *) data)) <= *((balance_t *) value.data);

    case INTEGER:
      return balance_t(*((unsigned int *) data)) <= *((balance_t *) value.data);

    case AMOUNT:
      return balance_t(*((amount_t *) data)) <= *((balance_t *) value.data);

    case BALANCE:
      return *((balance_t *) data) <= *((balance_t *) value.data);

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

bool value_t::operator>(const value_t& value)
{
  switch (value.type) {
  case BOOLEAN:
    switch (type) {
    case BOOLEAN:
      return *((bool *) data) > *((bool *) value.data);

    case INTEGER:
      return bool(*((unsigned int *) data)) > *((bool *) value.data);

    case AMOUNT:
      return bool(*((amount_t *) data)) > *((bool *) value.data);

    case BALANCE:
      return bool(*((balance_t *) data)) > *((bool *) value.data);

    default:
      assert(0);
      break;
    }
    break;

  case INTEGER:
    switch (type) {
    case BOOLEAN:
      return ((unsigned int) *((bool *) data)) > *((unsigned int *) value.data);

    case INTEGER:
      return *((unsigned int *) data) > *((unsigned int *) value.data);

    case AMOUNT:
      return ((unsigned int) *((amount_t *) data)) > *((unsigned int *) value.data);

    case BALANCE:
      return ((unsigned int) *((balance_t *) data)) > *((unsigned int *) value.data);

    default:
      assert(0);
      break;
    }
    break;

  case AMOUNT:
    switch (type) {
    case BOOLEAN:
      return amount_t(*((bool *) data)) > *((amount_t *) value.data);

    case INTEGER:
      return amount_t(*((unsigned int *) data)) > *((amount_t *) value.data);

    case AMOUNT:
      return *((amount_t *) data) > *((amount_t *) value.data);

    case BALANCE:
      return ((balance_t *) data)->amount(((amount_t *) value.data)->commodity) > *((amount_t *) value.data);

    default:
      assert(0);
      break;
    }
    break;

  case BALANCE:
    switch (type) {
    case BOOLEAN:
      return balance_t(*((bool *) data)) > *((balance_t *) value.data);

    case INTEGER:
      return balance_t(*((unsigned int *) data)) > *((balance_t *) value.data);

    case AMOUNT:
      return balance_t(*((amount_t *) data)) > *((balance_t *) value.data);

    case BALANCE:
      return *((balance_t *) data) > *((balance_t *) value.data);

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

bool value_t::operator>=(const value_t& value)
{
  switch (value.type) {
  case BOOLEAN:
    switch (type) {
    case BOOLEAN:
      return *((bool *) data) >= *((bool *) value.data);

    case INTEGER:
      return bool(*((unsigned int *) data)) >= *((bool *) value.data);

    case AMOUNT:
      return bool(*((amount_t *) data)) >= *((bool *) value.data);

    case BALANCE:
      return bool(*((balance_t *) data)) >= *((bool *) value.data);

    default:
      assert(0);
      break;
    }
    break;

  case INTEGER:
    switch (type) {
    case BOOLEAN:
      return ((unsigned int) *((bool *) data)) >= *((unsigned int *) value.data);

    case INTEGER:
      return *((unsigned int *) data) >= *((unsigned int *) value.data);

    case AMOUNT:
      return ((unsigned int) *((amount_t *) data)) >= *((unsigned int *) value.data);

    case BALANCE:
      return ((unsigned int) *((balance_t *) data)) >= *((unsigned int *) value.data);

    default:
      assert(0);
      break;
    }
    break;

  case AMOUNT:
    switch (type) {
    case BOOLEAN:
      return amount_t(*((bool *) data)) >= *((amount_t *) value.data);

    case INTEGER:
      return amount_t(*((unsigned int *) data)) >= *((amount_t *) value.data);

    case AMOUNT:
      return *((amount_t *) data) >= *((amount_t *) value.data);

    case BALANCE:
      return ((balance_t *) data)->amount() >= *((amount_t *) value.data);

    default:
      assert(0);
      break;
    }
    break;

  case BALANCE:
    switch (type) {
    case BOOLEAN:
      return balance_t(*((bool *) data)) >= *((balance_t *) value.data);

    case INTEGER:
      return balance_t(*((unsigned int *) data)) >= *((balance_t *) value.data);

    case AMOUNT:
      return balance_t(*((amount_t *) data)) >= *((balance_t *) value.data);

    case BALANCE:
      return *((balance_t *) data) >= *((balance_t *) value.data);

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

  default:
    assert(0);
    break;
  }
}

} // namespace ledger
