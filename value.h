#ifndef _VALUE_H
#define _VALUE_H

#include "amount.h"
#include "balance.h"

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
  bool constructed;

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

  value_t() : constructed(false) {
    *this = 0U;
  }

  value_t(const bool value) : constructed(false) {
    *((bool *) data) = value;
    type = BOOLEAN;
  }
  value_t(const unsigned int value) : constructed(false) {
    *((unsigned int *) data) = value;
    type = INTEGER;
  }
  value_t(const amount_t& value) : constructed(true) {
    new((amount_t *)data) amount_t(value);
    type = AMOUNT;
  }
  value_t(const balance_t& value) : constructed(true) {
    new((balance_t *)data) balance_t(value);
    type = BALANCE;
  }

  ~value_t() {
    destroy();
  }

  void destroy() {
    if (constructed) {
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
      constructed = false;
    }
  }

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

  value_t& operator+=(const value_t& value) {
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

  value_t& operator-=(const value_t& value) {
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

  value_t& operator*=(const value_t& value) {
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

  value_t& operator/=(const value_t& value) {
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

  bool operator==(const value_t& value) {
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
  bool operator!=(const value_t& value) {
    return ! (*this == value);
  }

  bool operator<(const value_t& value) {
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

  bool operator<=(const value_t& value) {
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

  bool operator>(const value_t& value) {
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

  bool operator>=(const value_t& value) {
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

  template <typename T>
  operator T() const {
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

  void cast(type_t cast_type) {
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
	constructed = true;
	break;
      case BALANCE:
	new((balance_t *)data) balance_t(*((bool *) data));
	constructed = true;
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
	constructed = true;
	break;
      case BALANCE:
	new((balance_t *)data) balance_t(*((unsigned int *) data));
	constructed = true;
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
	constructed = true;
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
	constructed = true;
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

  void negate() {
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

  void abs() {
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
};

} // namespace ledger

#endif // _VALUE_H
