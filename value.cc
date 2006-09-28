#ifdef USE_PCH
#include "pch.h"
#else
#include "value.h"
#include "xml.h"
#include "debug.h"
#include "error.h"
#endif

bool value_t::to_boolean() const
{
  if (type == BOOLEAN) {
    return *(bool *) data;
  } else {
    value_t temp(*this);
    temp.cast(BOOLEAN);
    return *(bool *) temp.data;
  }
}

long value_t::to_integer() const
{
  if (type == INTEGER) {
    return *(long *) data;
  } else {
    value_t temp(*this);
    temp.cast(INTEGER);
    return *(long *) temp.data;
  }
}

datetime_t value_t::to_datetime() const
{
  if (type == DATETIME) {
    return *(datetime_t *) data;
  } else {
    value_t temp(*this);
    temp.cast(DATETIME);
    return *(datetime_t *) temp.data;
  }
}

amount_t value_t::to_amount() const
{
  if (type == AMOUNT) {
    return *(amount_t *) data;
  } else {
    value_t temp(*this);
    temp.cast(AMOUNT);
    return *(amount_t *) temp.data;
  }
}

balance_t value_t::to_balance() const
{
  if (type == BALANCE) {
    return *(balance_t *) data;
  } else {
    value_t temp(*this);
    temp.cast(BALANCE);
    return *(balance_t *) temp.data;
  }
}

balance_pair_t value_t::to_balance_pair() const
{
  if (type == BALANCE_PAIR) {
    return *(balance_pair_t *) data;
  } else {
    value_t temp(*this);
    temp.cast(BALANCE_PAIR);
    return *(balance_pair_t *) temp.data;
  }
}

std::string value_t::to_string() const
{
  if (type == STRING) {
    return **(std::string **) data;
  } else {
    std::ostringstream out;
    out << *this;
    return out.str();
  }
}

xml::node_t * value_t::to_xml_node() const
{
  if (type == XML_NODE)
    return *(xml::node_t **) data;
  else
    throw new value_error("Value is not an XML node");
}

void * value_t::to_pointer() const
{
  if (type == POINTER)
    return *(void **) data;
  else
    throw new value_error("Value is not a pointer");
}

value_t::sequence_t * value_t::to_sequence() const
{
  if (type == SEQUENCE)
    return *(sequence_t **) data;
  else
    throw new value_error("Value is not a sequence");
}

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
  case STRING:
    delete *(std::string **) data;
    break;
  case SEQUENCE:
    delete *(sequence_t **) data;
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

  if (type == BOOLEAN && value.type == BOOLEAN) {
    *((bool *) data) = *((bool *) value.data);
    return *this;
  }
  else if (type == INTEGER && value.type == INTEGER) {
    *((long *) data) = *((long *) value.data);
    return *this;
  }
  else if (type == DATETIME && value.type == DATETIME) {
    *((datetime_t *) data) = *((datetime_t *) value.data);
    return *this;
  }
  else if (type == AMOUNT && value.type == AMOUNT) {
    *(amount_t *) data = *(amount_t *) value.data;
    return *this;
  }
  else if (type == BALANCE && value.type == BALANCE) {
    *(balance_t *) data = *(balance_t *) value.data;
    return *this;
  }
  else if (type == BALANCE_PAIR && value.type == BALANCE_PAIR) {
    *(balance_pair_t *) data = *(balance_pair_t *) value.data;
    return *this;
  }
  else if (type == STRING && value.type == STRING) {
    **(std::string **) data = **(std::string **) value.data;
    return *this;
  }
  else if (type == SEQUENCE && value.type == SEQUENCE) {
    **(sequence_t **) data = **(sequence_t **) value.data;
    return *this;
  }

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

  case STRING:
    *(std::string **) data = new std::string(**(std::string **) value.data);
    break;

  case XML_NODE:
    *(xml::node_t **) data = *(xml::node_t **) value.data;
    break;

  case POINTER:
    *(void **) data = *(void **) value.data;
    break;

  case SEQUENCE:
    *(sequence_t **) data = new sequence_t(**(sequence_t **) value.data);
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
  else if (value.type == XML_NODE)
    throw new value_error("Cannot add an XML node to a value");
  else if (value.type == POINTER)
    throw new value_error("Cannot add a pointer to a value");
  else if (value.type == SEQUENCE)
    throw new value_error("Cannot add a sequence to a value");

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
    case STRING:
      throw new value_error("Cannot add a string to an integer");
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
    case STRING:
      throw new value_error("Cannot add a string to an date/time");
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

    case STRING:
      throw new value_error("Cannot add a string to an amount");

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
    case STRING:
      throw new value_error("Cannot add a string to an balance");
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
    case STRING:
      throw new value_error("Cannot add a string to an balance pair");
    default:
      assert(0);
      break;
    }
    break;

  case STRING:
    switch (value.type) {
    case INTEGER:
      throw new value_error("Cannot add an integer to a string");
    case AMOUNT:
      throw new value_error("Cannot add an amount to a string");
    case BALANCE:
      throw new value_error("Cannot add a balance to a string");
    case BALANCE_PAIR:
      throw new value_error("Cannot add a balance pair to a string");
    case STRING:
      **(std::string **) data += **(std::string **) value.data;
      break;
    default:
      assert(0);
      break;
    }
    break;

  case XML_NODE:
    throw new value_error("Cannot add a value to an XML node");

  case POINTER:
    throw new value_error("Cannot add a value to a pointer");

  case SEQUENCE:
    throw new value_error("Cannot add a value to a sequence");

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
  else if (value.type == STRING)
    throw new value_error("Cannot subtract a string from a value");
  else if (value.type == XML_NODE)
    throw new value_error("Cannot subtract an XML node from a value");
  else if (value.type == POINTER)
    throw new value_error("Cannot subtract a pointer from a value");
  else if (value.type == SEQUENCE)
    throw new value_error("Cannot subtract a sequence from a value");

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

  case STRING:
    throw new value_error("Cannot subtract a value from a string");
  case XML_NODE:
    throw new value_error("Cannot subtract a value from an XML node");
  case POINTER:
    throw new value_error("Cannot subtract a value from a pointer");
  case SEQUENCE:
    throw new value_error("Cannot subtract a value from a sequence");

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
    throw new value_error("Cannot multiply a value by a boolean");
  else if (value.type == DATETIME)
    throw new value_error("Cannot multiply a value by a date/time");
  else if (value.type == STRING)
    throw new value_error("Cannot multiply a value by a string");
  else if (value.type == XML_NODE)
    throw new value_error("Cannot multiply a value by an XML node");
  else if (value.type == POINTER)
    throw new value_error("Cannot multiply a value by a pointer");
  else if (value.type == SEQUENCE)
    throw new value_error("Cannot multiply a value by a sequence");

  if (value.realzero() && type != STRING) {
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

  case STRING:
    switch (value.type) {
    case INTEGER: {
      std::string temp;
      for (long i = 0; i < *(long *) value.data; i++)
	temp += **(std::string **) data;
      **(std::string **) data = temp;
      break;
    }
    case AMOUNT: {
      std::string temp;
      value_t num(value);
      num.cast(INTEGER);
      for (long i = 0; i < *(long *) num.data; i++)
	temp += **(std::string **) data;
      **(std::string **) data = temp;
      break;
    }
    case BALANCE:
      throw new value_error("Cannot multiply a string by a balance");
    case BALANCE_PAIR:
      throw new value_error("Cannot multiply a string by a balance pair");
    default:
      assert(0);
      break;
    }
    break;

  case XML_NODE:
    throw new value_error("Cannot multiply an XML node by a value");
  case POINTER:
    throw new value_error("Cannot multiply a pointer by a value");
  case SEQUENCE:
    throw new value_error("Cannot multiply a sequence by a value");

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
  else if (value.type == STRING)
    throw new value_error("Cannot divide a string by a value");
  else if (value.type == XML_NODE)
    throw new value_error("Cannot divide a value by an XML node");
  else if (value.type == POINTER)
    throw new value_error("Cannot divide a pointer by a value");
  else if (value.type == SEQUENCE)
    throw new value_error("Cannot divide a value by a sequence");

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

  case STRING:
    throw new value_error("Cannot divide a value from a string");
  case XML_NODE:
    throw new value_error("Cannot divide a value from an XML node");
  case POINTER:
    throw new value_error("Cannot divide a value from a pointer");
  case SEQUENCE:
    throw new value_error("Cannot divide a value from a sequence");

  default:
    assert(0);
    break;
  }
  return *this;
}

template <>
value_t::operator bool() const
{
  switch (type) {
  case BOOLEAN:
    return *(bool *) data;
  case INTEGER:
    return *(long *) data;
  case DATETIME:
    return *(datetime_t *) data;
  case AMOUNT:
    return *(amount_t *) data;
  case BALANCE:
    return *(balance_t *) data;
  case BALANCE_PAIR:
    return *(balance_pair_t *) data;
  case STRING:
    return ! (**((std::string **) data)).empty();
  case XML_NODE:
    return *(xml::node_t **) data != NULL;
  case POINTER:
    return *(void **) data != NULL;
  case SEQUENCE:
    return (*(sequence_t **) data != NULL &&
	    ! (*(sequence_t **) data)->empty());

  default:
    assert(0);
    break;
  }
  assert(0);
  return 0;
}

template <>
value_t::operator long() const
{
  switch (type) {
  case BOOLEAN:
    throw new value_error("Cannot convert a boolean to an integer");
  case INTEGER:
    return *((long *) data);
  case DATETIME:
    return *((datetime_t *) data);
  case AMOUNT:
    return *((amount_t *) data);
  case BALANCE:
    throw new value_error("Cannot convert a balance to an integer");
  case BALANCE_PAIR:
    throw new value_error("Cannot convert a balance pair to an integer");
  case STRING:
    throw new value_error("Cannot convert a string to an integer");
  case XML_NODE:
    throw new value_error("Cannot convert an XML node to an integer");
  case POINTER:
    throw new value_error("Cannot convert a pointer to an integer");
  case SEQUENCE:
    throw new value_error("Cannot convert a sequence to an integer");

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
  case STRING:
    throw new value_error("Cannot convert a string to a date/time");
  case XML_NODE:
    throw new value_error("Cannot convert an XML node to a date/time");
  case POINTER:
    throw new value_error("Cannot convert a pointer to a date/time");
  case SEQUENCE:
    throw new value_error("Cannot convert a sequence to a date/time");

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
  case STRING:
    throw new value_error("Cannot convert a string to a double");
  case XML_NODE:
    throw new value_error("Cannot convert an XML node to a double");
  case POINTER:
    throw new value_error("Cannot convert a pointer to a double");
  case SEQUENCE:
    throw new value_error("Cannot convert a sequence to a double");

  default:
    assert(0);
    break;
  }
  assert(0);
  return 0;
}

template <>
value_t::operator std::string() const
{
  switch (type) {
  case BOOLEAN:
  case INTEGER:
  case DATETIME:
  case AMOUNT:
  case BALANCE:
  case BALANCE_PAIR: {
    value_t temp(*this);
    temp.cast(STRING);
    return temp;
  }

  case STRING:
    return **(std::string **) data;

  case XML_NODE:
    return (*(xml::node_t **) data)->text();
  case POINTER:
    throw new value_error("Cannot convert a pointer to a string");
  case SEQUENCE:
    throw new value_error("Cannot convert a sequence to a string");

  default:
    assert(0);
    break;
  }
  assert(0);
  return 0;
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
    case STRING:							\
      throw new value_error("Cannot compare a boolean to a string");	\
    case XML_NODE:							\
      throw new value_error("Cannot compare a boolean to an XML node"); \
    case POINTER:							\
      throw new value_error("Cannot compare a boolean to a pointer"); \
    case SEQUENCE:							\
      throw new value_error("Cannot compare a boolean to a sequence"); \
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
	      ((long) *((datetime_t *) value.data)));			\
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
    case STRING:							\
      throw new value_error("Cannot compare an integer to a string");	\
    case XML_NODE:							\
      throw new value_error("Cannot compare an integer to an XML node"); \
    case POINTER:							\
      throw new value_error("Cannot compare an integer to a pointer"); \
    case SEQUENCE:							\
      throw new value_error("Cannot compare an integer to a sequence"); \
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
	      datetime_t(*((long *) value.data)));			\
									\
    case DATETIME:							\
      return (*((datetime_t *) data) OP					\
	      *((datetime_t *) value.data));				\
									\
    case AMOUNT:							\
      throw new value_error("Cannot compare a date/time to an amount");	\
    case BALANCE:							\
      throw new value_error("Cannot compare a date/time to a balance");	\
    case BALANCE_PAIR:							\
      throw new value_error("Cannot compare a date/time to a balance pair"); \
    case STRING:							\
      throw new value_error("Cannot compare a date/time to a string");	\
    case XML_NODE:							\
      throw new value_error("Cannot compare a date/time to an XML node"); \
    case POINTER:							\
      throw new value_error("Cannot compare a date/time to a pointer"); \
    case SEQUENCE:							\
      throw new value_error("Cannot compare a date/time to a sequence"); \
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
    case STRING:							\
      throw new value_error("Cannot compare an amount to a string");	\
    case XML_NODE:							\
      throw new value_error("Cannot compare an amount to an XML node"); \
    case POINTER:							\
      throw new value_error("Cannot compare an amount to a pointer"); \
    case SEQUENCE:							\
      throw new value_error("Cannot compare an amount to a sequence"); \
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
    case STRING:							\
      throw new value_error("Cannot compare a balance to a string");	\
    case XML_NODE:							\
      throw new value_error("Cannot compare a balance to an XML node"); \
    case POINTER:							\
      throw new value_error("Cannot compare a balance to a pointer"); \
    case SEQUENCE:							\
      throw new value_error("Cannot compare a balance to a sequence"); \
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
    case STRING:							\
      throw new value_error("Cannot compare a balance pair to a string"); \
    case XML_NODE:							\
      throw new value_error("Cannot compare a balance pair to an XML node"); \
    case POINTER:							\
      throw new value_error("Cannot compare a balance pair to a pointer"); \
    case SEQUENCE:							\
      throw new value_error("Cannot compare a balance pair to a sequence"); \
									\
    default:								\
      assert(0);							\
      break;								\
    }									\
    break;								\
									\
  case STRING:								\
    switch (value.type) {						\
    case BOOLEAN:							\
      throw new value_error("Cannot compare a string to a boolean");	\
    case INTEGER:							\
      throw new value_error("Cannot compare a string to an integer");	\
    case DATETIME:							\
      throw new value_error("Cannot compare a string to a date/time");	\
    case AMOUNT:							\
      throw new value_error("Cannot compare a string to an amount");	\
    case BALANCE:							\
      throw new value_error("Cannot compare a string to a balance");	\
    case BALANCE_PAIR:							\
      throw new value_error("Cannot compare a string to a balance pair"); \
									\
    case STRING:							\
      return (**((std::string **) data) OP				\
	      **((std::string **) value.data));				\
									\
    case XML_NODE:							\
      return (**((std::string **) data) OP				\
	      (*(xml::node_t **) value.data)->text());			\
									\
    case POINTER:							\
      throw new value_error("Cannot compare a string to a pointer"); \
    case SEQUENCE:							\
      throw new value_error("Cannot compare a string to a sequence"); \
									\
    default:								\
      assert(0);							\
      break;								\
    }									\
    break;								\
									\
  case XML_NODE:							\
    switch (value.type) {						\
    case BOOLEAN:							\
      throw new value_error("Cannot compare an XML node to a boolean");	\
    case INTEGER:							\
      throw new value_error("Cannot compare an XML node to an integer");	\
    case DATETIME:							\
      throw new value_error("Cannot compare an XML node to a date/time");	\
    case AMOUNT:							\
      throw new value_error("Cannot compare an XML node to an amount");	\
    case BALANCE:							\
      throw new value_error("Cannot compare an XML node to a balance");	\
    case BALANCE_PAIR:							\
      throw new value_error("Cannot compare an XML node to a balance pair"); \
									\
    case STRING:							\
      return ((*(xml::node_t **) data)->text() OP			\
	      **((std::string **) value.data));				\
									\
    case XML_NODE:							\
      return (*((xml::node_t **) data) OP				\
	      *((xml::node_t **) value.data));				\
									\
    case POINTER:							\
      throw new value_error("Cannot compare an XML node to a pointer"); \
    case SEQUENCE:							\
      throw new value_error("Cannot compare an XML node to a sequence"); \
									\
    default:								\
      assert(0);							\
      break;								\
    }									\
    break;								\
									\
  case POINTER:								\
    switch (value.type) {						\
    case BOOLEAN:							\
      throw new value_error("Cannot compare a pointer to a boolean");	\
    case INTEGER:							\
      throw new value_error("Cannot compare a pointer to an integer");	\
    case DATETIME:							\
      throw new value_error("Cannot compare a pointer to a date/time");	\
    case AMOUNT:							\
      throw new value_error("Cannot compare a pointer to an amount");	\
    case BALANCE:							\
      throw new value_error("Cannot compare a pointer to a balance");	\
    case BALANCE_PAIR:							\
      throw new value_error("Cannot compare a pointer to a balance pair"); \
    case STRING:							\
      throw new value_error("Cannot compare a pointer to a string node"); \
    case XML_NODE:							\
      throw new value_error("Cannot compare a pointer to an XML node"); \
    case POINTER:							\
      return (*((void **) data) OP *((void **) value.data));		\
    case SEQUENCE:							\
      throw new value_error("Cannot compare a pointer to a sequence"); \
									\
    default:								\
      assert(0);							\
      break;								\
    }									\
    break;								\
									\
  case SEQUENCE:							\
    throw new value_error("Cannot compare a value to a sequence");	\
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
      throw new value_error("Cannot convert a boolean to an integer");
    case DATETIME:
      throw new value_error("Cannot convert a boolean to a date/time");
    case AMOUNT:
      throw new value_error("Cannot convert a boolean to an amount");
    case BALANCE:
      throw new value_error("Cannot convert a boolean to a balance");
    case BALANCE_PAIR:
      throw new value_error("Cannot convert a boolean to a balance pair");
    case STRING:
      *(std::string **) data = new std::string(*((bool *) data) ? "true" : "false");
      break;
    case XML_NODE:
      throw new value_error("Cannot convert a boolean to an XML node");
    case POINTER:
      throw new value_error("Cannot convert a boolean to a pointer");
    case SEQUENCE:
      throw new value_error("Cannot convert a boolean to a sequence");

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
      *((datetime_t *) data) = datetime_t(*((long *) data));
      break;
    case AMOUNT:
      new((amount_t *)data) amount_t(*((long *) data));
      break;
    case BALANCE:
      new((balance_t *)data) balance_t(*((long *) data));
      break;
    case BALANCE_PAIR:
      new((balance_pair_t *)data) balance_pair_t(*((long *) data));
      break;
    case STRING: {
      char buf[32];
      std::sprintf(buf, "%ld", *(long *) data);
      *(std::string **) data = new std::string(buf);
      break;
    }
    case XML_NODE:
      throw new value_error("Cannot convert an integer to an XML node");
    case POINTER:
      throw new value_error("Cannot convert an integer to a pointer");
    case SEQUENCE:
      throw new value_error("Cannot convert an integer to a sequence");

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
      *((long *) data) = *((datetime_t *) data);
      break;
    case DATETIME:
      break;
    case AMOUNT:
      throw new value_error("Cannot convert a date/time to an amount");
    case BALANCE:
      throw new value_error("Cannot convert a date/time to a balance");
    case BALANCE_PAIR:
      throw new value_error("Cannot convert a date/time to a balance pair");
    case STRING:
      throw new value_error("Cannot convert a date/time to a string");
    case XML_NODE:
      throw new value_error("Cannot convert a date/time to an XML node");
    case POINTER:
      throw new value_error("Cannot convert a date/time to a pointer");
    case SEQUENCE:
      throw new value_error("Cannot convert a date/time to a sequence");

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
    case STRING: {
      std::ostringstream out;
      out << *(amount_t *) data;
      destroy();
      *(std::string **) data = new std::string(out.str());
      break;
    }
    case XML_NODE:
      throw new value_error("Cannot convert an amount to an XML node");
    case POINTER:
      throw new value_error("Cannot convert an amount to a pointer");
    case SEQUENCE:
      throw new value_error("Cannot convert an amount to a sequence");

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
    case STRING:
      throw new value_error("Cannot convert a balance to a string");
    case XML_NODE:
      throw new value_error("Cannot convert a balance to an XML node");
    case POINTER:
      throw new value_error("Cannot convert a balance to a pointer");
    case SEQUENCE:
      throw new value_error("Cannot convert a balance to a sequence");

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
    case STRING:
      throw new value_error("Cannot convert a balance pair to a string");
    case XML_NODE:
      throw new value_error("Cannot convert a balance pair to an XML node");
    case POINTER:
      throw new value_error("Cannot convert a balance pair to a pointer");
    case SEQUENCE:
      throw new value_error("Cannot convert a balance pair to a sequence");

    default:
      assert(0);
      break;
    }
    break;

  case STRING:
    switch (cast_type) {
    case BOOLEAN: {
      if (**(std::string **) data == "true") {
	destroy();
	*(bool *) data = true;
      }
      else if (**(std::string **) data == "false") {
	destroy();
	*(bool *) data = false;
      }
      else {
	throw new value_error("Cannot convert string to an boolean");
      }
      break;
    }
    case INTEGER: {
      int l = (*(std::string **) data)->length();
      const char * p = (*(std::string **) data)->c_str();
      bool alldigits = true;
      for (int i = 0; i < l; i++)
	if (! std::isdigit(p[i])) {
	  alldigits = false;
	  break;
	}
      if (alldigits) {
	long temp = std::atol((*(std::string **) data)->c_str());
	destroy();
	*(long *) data = temp;
      } else {
	throw new value_error("Cannot convert string to an integer");
      }
      break;
    }

    case DATETIME:
      throw new value_error("Cannot convert a string to a date/time");

    case AMOUNT: {
      amount_t temp = **(std::string **) data;
      destroy();
      new((amount_t *)data) amount_t(temp);
      break;
    }
    case BALANCE:
      throw new value_error("Cannot convert a string to a balance");
    case BALANCE_PAIR:
      throw new value_error("Cannot convert a string to a balance pair");
    case STRING:
      break;
    case XML_NODE:
      throw new value_error("Cannot convert a string to an XML node");
    case POINTER:
      throw new value_error("Cannot convert a string to a pointer");
    case SEQUENCE:
      throw new value_error("Cannot convert a string to a sequence");

    default:
      assert(0);
      break;
    }
    break;

  case XML_NODE:
    switch (cast_type) {
    case BOOLEAN:
      throw new value_error("Cannot convert an XML node to a boolean");
    case INTEGER:
      throw new value_error("Cannot convert an XML node to an integer");
    case DATETIME:
      throw new value_error("Cannot convert an XML node to a date/time");
    case AMOUNT:
      throw new value_error("Cannot convert an XML node to an amount");
    case BALANCE:
      throw new value_error("Cannot convert an XML node to a balance");
    case BALANCE_PAIR:
      throw new value_error("Cannot convert an XML node to a balance pair");
    case STRING:
      throw new value_error("Cannot convert an XML node to a string");
    case XML_NODE:
      break;
    case POINTER:
      throw new value_error("Cannot convert an XML node to a pointer");
    case SEQUENCE:
      throw new value_error("Cannot convert an XML node to a sequence");

    default:
      assert(0);
      break;
    }
    break;

  case POINTER:
    switch (cast_type) {
    case BOOLEAN:
      throw new value_error("Cannot convert a pointer to a boolean");
    case INTEGER:
      throw new value_error("Cannot convert a pointer to an integer");
    case DATETIME:
      throw new value_error("Cannot convert a pointer to a date/time");
    case AMOUNT:
      throw new value_error("Cannot convert a pointer to an amount");
    case BALANCE:
      throw new value_error("Cannot convert a pointer to a balance");
    case BALANCE_PAIR:
      throw new value_error("Cannot convert a pointer to a balance pair");
    case STRING:
      throw new value_error("Cannot convert a pointer to a string");
    case XML_NODE:
      throw new value_error("Cannot convert a pointer to an XML node");
    case POINTER:
      break;
    case SEQUENCE:
      throw new value_error("Cannot convert a pointer to a sequence");

    default:
      assert(0);
      break;
    }
    break;

  case SEQUENCE:
    switch (cast_type) {
    case BOOLEAN:
      throw new value_error("Cannot convert a sequence to a boolean");
    case INTEGER:
      throw new value_error("Cannot convert a sequence to an integer");
    case DATETIME:
      throw new value_error("Cannot convert a sequence to a date/time");
    case AMOUNT:
      throw new value_error("Cannot convert a sequence to an amount");
    case BALANCE:
      throw new value_error("Cannot convert a sequence to a balance");
    case BALANCE_PAIR:
      throw new value_error("Cannot convert a sequence to a balance pair");
    case STRING:
      throw new value_error("Cannot convert a sequence to a string");
    case XML_NODE:							\
      throw new value_error("Cannot compare a sequence to an XML node"); \
    case POINTER:
      throw new value_error("Cannot convert a sequence to a pointer");
    case SEQUENCE:
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
    throw new value_error("Cannot negate a date/time");
  case AMOUNT:
    ((amount_t *) data)->negate();
    break;
  case BALANCE:
    ((balance_t *) data)->negate();
    break;
  case BALANCE_PAIR:
    ((balance_pair_t *) data)->negate();
    break;
  case STRING:
    throw new value_error("Cannot negate a string");
  case XML_NODE:
    throw new value_error("Cannot negate an XML node");
  case POINTER:
    throw new value_error("Cannot negate a pointer");
  case SEQUENCE:
    throw new value_error("Cannot negate a sequence");

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
  case STRING:
    throw new value_error("Cannot take the absolute value of a string");
  case XML_NODE:
    throw new value_error("Cannot take the absolute value of an XML node");
  case POINTER:
    throw new value_error("Cannot take the absolute value of a pointer");
  case SEQUENCE:
    throw new value_error("Cannot take the absolute value of a sequence");

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
  case STRING:
    throw new value_error("Cannot find the value of a string");
  case XML_NODE:
    throw new value_error("Cannot find the value of an XML node");
  case POINTER:
    throw new value_error("Cannot find the value of a pointer");
  case SEQUENCE:
    throw new value_error("Cannot find the value of a sequence");
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
  case STRING:
    throw new value_error("Cannot reduce a string");
  case XML_NODE:
    throw new value_error("Cannot reduce an XML node");
  case POINTER:
    throw new value_error("Cannot reduce a pointer");
  case SEQUENCE:
    throw new value_error("Cannot reduce a sequence");
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
  case STRING:
    throw new value_error("Cannot round a string");
  case XML_NODE:
    throw new value_error("Cannot round an XML node");
  case POINTER:
    throw new value_error("Cannot round a pointer");
  case SEQUENCE:
    throw new value_error("Cannot round a sequence");
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
  case STRING:
    throw new value_error("Cannot un-round a string");
  case XML_NODE:
    throw new value_error("Cannot un-round an XML node");
  case POINTER:
    throw new value_error("Cannot un-round a pointer");
  case SEQUENCE:
    throw new value_error("Cannot un-round a sequence");
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

  case STRING:
    throw new value_error("Cannot find the price of a string");
  case XML_NODE:
    throw new value_error("Cannot find the price of an XML node");
  case POINTER:
    throw new value_error("Cannot find the price of a pointer");
  case SEQUENCE:
    throw new value_error("Cannot find the price of a sequence");

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

  case STRING:
    throw new value_error("Cannot find the date of a string");
  case XML_NODE:
    throw new value_error("Cannot find the date of an XML node");
  case POINTER:
    throw new value_error("Cannot find the date of a pointer");
  case SEQUENCE:
    throw new value_error("Cannot find the date of a sequence");

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
  case STRING:
  case XML_NODE:
  case POINTER:
    return *this;

  case SEQUENCE:
    assert(0);			// jww (2006-09-28): strip them all!
    break;

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

  case STRING:
    throw new value_error("Cannot find the cost of a string");
  case XML_NODE:
    throw new value_error("Cannot find the cost of an XML node");
  case POINTER:
    throw new value_error("Cannot find the cost of a pointer");
  case SEQUENCE:
    throw new value_error("Cannot find the cost of a sequence");

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

  case STRING:
    throw new value_error("Cannot add an amount to a string");
  case XML_NODE:
    throw new value_error("Cannot add an amount to an XML node");
  case POINTER:
    throw new value_error("Cannot add an amount to a pointer");
  case SEQUENCE:
    throw new value_error("Cannot add an amount to a sequence");

  default:
    assert(0);
    break;
  }

  return *this;
}

void value_t::write(std::ostream& out, const int first_width,
		    const int latter_width) const
{
  switch (type) {
  case BOOLEAN:
  case DATETIME:
  case INTEGER:
  case AMOUNT:
  case STRING:
  case POINTER:
    out << *this;
    break;

  case XML_NODE:
    (*(xml::node_t **) data)->write(out);
    break;

  case SEQUENCE:
    assert(0);			// jww (2006-09-28): write them all out!
    throw new value_error("Cannot write out a sequence");

  case BALANCE:
    ((balance_t *) data)->write(out, first_width, latter_width);
    break;
  case BALANCE_PAIR:
    ((balance_pair_t *) data)->write(out, first_width, latter_width);
    break;
  }
}

std::ostream& operator<<(std::ostream& out, const value_t& value)
{
  switch (value.type) {
  case value_t::BOOLEAN:
    out << (*((bool *) value.data) ? "true" : "false");
    break;
  case value_t::INTEGER:
    out << *(long *) value.data;
    break;
  case value_t::DATETIME:
    out << *(datetime_t *) value.data;
    break;
  case value_t::AMOUNT:
    out << *(amount_t *) value.data;
    break;
  case value_t::BALANCE:
    out << *(balance_t *) value.data;
    break;
  case value_t::BALANCE_PAIR:
    out << *(balance_pair_t *) value.data;
    break;
  case value_t::STRING:
    out << **(std::string **) value.data;
    break;
  case value_t::XML_NODE:
#if 1
    out << (*(xml::node_t **) value.data)->text();
#else
    (*(xml::node_t **) value.data)->write(out);
#endif
    break;

  case value_t::POINTER:
    throw new value_error("Cannot output a pointer value");

  case value_t::SEQUENCE: {
    out << '(';
    bool first = true;
    for (value_t::sequence_t::iterator
	   i = (*(value_t::sequence_t **) value.data)->begin();
	 i != (*(value_t::sequence_t **) value.data)->end();
	 i++) {
      if (first)
	first = false;
      else
	out << ", ";
      out << *i;
    }
    out << ')';
    break;
  }

  default:
    assert(0);
    break;
  }
  return out;
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

  balance_t * ptr = NULL;

  out << std::right;
  out.width(20);

  switch (bal->type) {
  case value_t::BOOLEAN:
    out << (*((bool *) bal->data) ? "true" : "false");
    break;
  case value_t::INTEGER:
    out << *((long *) bal->data);
    break;
  case value_t::DATETIME:
    out << *((datetime_t *) bal->data);
    break;
  case value_t::AMOUNT:
    out << *((amount_t *) bal->data);
    break;
  case value_t::BALANCE:
    ptr = (balance_t *) bal->data;
    // fall through...

  case value_t::BALANCE_PAIR:
    if (! ptr)
      ptr = &((balance_pair_t *) bal->data)->quantity;

    ptr->write(out, 20);
    break;
  default:
    assert(0);
    break;
  }
  out << std::endl;
}

#ifdef USE_BOOST_PYTHON

#ifndef USE_PCH
#include <boost/python.hpp>
#endif

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

  case value_t::STRING:
  case value_t::XML_NODE:
  case value_t::POINTER:
    return 1;

  case value_t::SEQUENCE:
    return (*(value_t::sequence_t **) value.data)->size();

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

  case value_t::STRING:
    throw new value_error("Cannot cast a string to an amount");

  case value_t::XML_NODE:
    throw new value_error("Cannot cast an XML node to an amount");

  case value_t::POINTER:
    throw new value_error("Cannot cast a pointer to an amount");

  case value_t::SEQUENCE:
    return (*(value_t::sequence_t **) value.data)[i];

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
  class_< value_t > ("Value")
    .def(init<value_t>())
    .def(init<balance_pair_t>())
    .def(init<balance_t>())
    .def(init<amount_t>())
    .def(init<std::string>())
    .def(init<double>())
    .def(init<long>())
    .def(init<datetime_t>())

    .def(self + self)
    .def(self + other<std::string>())
    .def(self + other<balance_pair_t>())
    .def(self + other<balance_t>())
    .def(self + other<amount_t>())
    .def(self + long())
    .def(self + double())

    .def(other<std::string>() + self)
    .def(other<balance_pair_t>() + self)
    .def(other<balance_t>() + self)
    .def(other<amount_t>() + self)
    .def(long() + self)
    .def(double() + self)

    .def(self - self)
    .def(self - other<std::string>())
    .def(self - other<balance_pair_t>())
    .def(self - other<balance_t>())
    .def(self - other<amount_t>())
    .def(self - long())
    .def(self - double())

    .def(other<std::string>() - self)
    .def(other<balance_pair_t>() - self)
    .def(other<balance_t>() - self)
    .def(other<amount_t>() - self)
    .def(long() - self)
    .def(double() - self)

    .def(self * self)
    .def(self * other<std::string>())
    .def(self * other<balance_pair_t>())
    .def(self * other<balance_t>())
    .def(self * other<amount_t>())
    .def(self * long())
    .def(self * double())

    .def(other<std::string>() * self)
    .def(other<balance_pair_t>() * self)
    .def(other<balance_t>() * self)
    .def(other<amount_t>() * self)
    .def(long() * self)
    .def(double() * self)

    .def(self / self)
    .def(self / other<std::string>())
    .def(self / other<balance_pair_t>())
    .def(self / other<balance_t>())
    .def(self / other<amount_t>())
    .def(self / long())
    .def(self / double())

    .def(other<std::string>() / self)
    .def(other<balance_pair_t>() / self)
    .def(other<balance_t>() / self)
    .def(other<amount_t>() / self)
    .def(long() / self)
    .def(double() / self)

    .def(- self)

    .def(self += self)
    .def(self += other<std::string>())
    .def(self += other<balance_pair_t>())
    .def(self += other<balance_t>())
    .def(self += other<amount_t>())
    .def(self += long())
    .def(self += double())

    .def(self -= self)
    .def(self -= other<std::string>())
    .def(self -= other<balance_pair_t>())
    .def(self -= other<balance_t>())
    .def(self -= other<amount_t>())
    .def(self -= long())
    .def(self -= double())

    .def(self *= self)
    .def(self *= other<std::string>())
    .def(self *= other<balance_pair_t>())
    .def(self *= other<balance_t>())
    .def(self *= other<amount_t>())
    .def(self *= long())
    .def(self *= double())

    .def(self /= self)
    .def(self /= other<std::string>())
    .def(self /= other<balance_pair_t>())
    .def(self /= other<balance_t>())
    .def(self /= other<amount_t>())
    .def(self /= long())
    .def(self /= double())

    .def(self <  self)
    .def(self < other<std::string>())
    .def(self < other<balance_pair_t>())
    .def(self < other<balance_t>())
    .def(self < other<amount_t>())
    .def(self < long())
    .def(self < other<datetime_t>())
    .def(self < double())

    .def(other<std::string>() < self)
    .def(other<balance_pair_t>() < self)
    .def(other<balance_t>() < self)
    .def(other<amount_t>() < self)
    .def(long() < self)
    .def(other<datetime_t>() < self)
    .def(double() < self)

    .def(self <= self)
    .def(self <= other<std::string>())
    .def(self <= other<balance_pair_t>())
    .def(self <= other<balance_t>())
    .def(self <= other<amount_t>())
    .def(self <= long())
    .def(self <= other<datetime_t>())
    .def(self <= double())

    .def(other<std::string>() <= self)
    .def(other<balance_pair_t>() <= self)
    .def(other<balance_t>() <= self)
    .def(other<amount_t>() <= self)
    .def(long() <= self)
    .def(other<datetime_t>() <= self)
    .def(double() <= self)

    .def(self > self)
    .def(self > other<std::string>())
    .def(self > other<balance_pair_t>())
    .def(self > other<balance_t>())
    .def(self > other<amount_t>())
    .def(self > long())
    .def(self > other<datetime_t>())
    .def(self > double())

    .def(other<std::string>() > self)
    .def(other<balance_pair_t>() > self)
    .def(other<balance_t>() > self)
    .def(other<amount_t>() > self)
    .def(long() > self)
    .def(other<datetime_t>() > self)
    .def(double() > self)

    .def(self >= self)
    .def(self >= other<std::string>())
    .def(self >= other<balance_pair_t>())
    .def(self >= other<balance_t>())
    .def(self >= other<amount_t>())
    .def(self >= long())
    .def(self >= other<datetime_t>())
    .def(self >= double())

    .def(other<std::string>() >= self)
    .def(other<balance_pair_t>() >= self)
    .def(other<balance_t>() >= self)
    .def(other<amount_t>() >= self)
    .def(long() >= self)
    .def(other<datetime_t>() >= self)
    .def(double() >= self)

    .def(self == self)
    .def(self == other<std::string>())
    .def(self == other<balance_pair_t>())
    .def(self == other<balance_t>())
    .def(self == other<amount_t>())
    .def(self == long())
    .def(self == other<datetime_t>())
    .def(self == double())

    .def(other<std::string>() == self)
    .def(other<balance_pair_t>() == self)
    .def(other<balance_t>() == self)
    .def(other<amount_t>() == self)
    .def(long() == self)
    .def(other<datetime_t>() == self)
    .def(double() == self)

    .def(self != self)
    .def(self != other<std::string>())
    .def(self != other<balance_pair_t>())
    .def(self != other<balance_t>())
    .def(self != other<amount_t>())
    .def(self != long())
    .def(self != other<datetime_t>())
    .def(self != double())

    .def(other<std::string>() != self)
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
    .def("write", &value_t::write)
    ;

  enum_< value_t::type_t > ("ValueType")
    .value("Boolean", value_t::BOOLEAN)
    .value("Integer", value_t::INTEGER)
    .value("DateTime", value_t::DATETIME)
    .value("Amount", value_t::AMOUNT)
    .value("Balance", value_t::BALANCE)
    .value("BalancePair", value_t::BALANCE_PAIR)
    .value("String", value_t::STRING)
    .value("XmlNode", value_t::XML_NODE)
    .value("Pointer", value_t::POINTER)
    .value("Sequence", value_t::SEQUENCE)
    ;
}

#endif // USE_BOOST_PYTHON
