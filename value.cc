#include "value.h"
#include "xml.h"

namespace ledger {

bool value_t::to_boolean() const
{
  if (type == BOOLEAN) {
    return *(bool *) data;
  } else {
    value_t temp(*this);
    temp.in_place_cast(BOOLEAN);
    return *(bool *) temp.data;
  }
}

long value_t::to_integer() const
{
  if (type == INTEGER) {
    return *(long *) data;
  } else {
    value_t temp(*this);
    temp.in_place_cast(INTEGER);
    return *(long *) temp.data;
  }
}

moment_t value_t::to_datetime() const
{
  if (type == DATETIME) {
    return *(moment_t *) data;
  } else {
    value_t temp(*this);
    temp.in_place_cast(DATETIME);
    return *(moment_t *) temp.data;
  }
}

amount_t value_t::to_amount() const
{
  if (type == AMOUNT) {
    return *(amount_t *) data;
  } else {
    value_t temp(*this);
    temp.in_place_cast(AMOUNT);
    return *(amount_t *) temp.data;
  }
}

balance_t value_t::to_balance() const
{
  if (type == BALANCE) {
    return *(balance_t *) data;
  } else {
    value_t temp(*this);
    temp.in_place_cast(BALANCE);
    return *(balance_t *) temp.data;
  }
}

balance_pair_t value_t::to_balance_pair() const
{
  if (type == BALANCE_PAIR) {
    return *(balance_pair_t *) data;
  } else {
    value_t temp(*this);
    temp.in_place_cast(BALANCE_PAIR);
    return *(balance_pair_t *) temp.data;
  }
}

string value_t::to_string() const
{
  if (type == STRING) {
    return **(string **) data;
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
    throw_(value_exception, "Value is not an XML node");
}

void * value_t::to_pointer() const
{
  if (type == POINTER)
    return *(void **) data;
  else
    throw_(value_exception, "Value is not a pointer");
}

value_t::sequence_t * value_t::to_sequence() const
{
  if (type == SEQUENCE)
    return *(sequence_t **) data;
  else
    throw_(value_exception, "Value is not a sequence");
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
    delete *(string **) data;
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
    DEBUG_("amounts.values.simplify", "Zeroing type " << type);
    *this = 0L;
    return;
  }

  if (type == BALANCE_PAIR &&
      (! ((balance_pair_t *) data)->cost ||
       ((balance_pair_t *) data)->cost->realzero())) {
    DEBUG_("amounts.values.simplify", "Reducing balance pair to balance");
    in_place_cast(BALANCE);
  }

  if (type == BALANCE &&
      ((balance_t *) data)->amounts.size() == 1) {
    DEBUG_("amounts.values.simplify", "Reducing balance to amount");
    in_place_cast(AMOUNT);
  }

  if (type == AMOUNT &&
      ! ((amount_t *) data)->commodity()) {
    DEBUG_("amounts.values.simplify", "Reducing amount to integer");
    in_place_cast(INTEGER);
  }
}

value_t& value_t::operator=(const value_t& val)
{
  if (this == &val)
    return *this;

  if (type == BOOLEAN && val.type == BOOLEAN) {
    *((bool *) data) = *((bool *) val.data);
    return *this;
  }
  else if (type == INTEGER && val.type == INTEGER) {
    *((long *) data) = *((long *) val.data);
    return *this;
  }
  else if (type == DATETIME && val.type == DATETIME) {
    *((moment_t *) data) = *((moment_t *) val.data);
    return *this;
  }
  else if (type == AMOUNT && val.type == AMOUNT) {
    *(amount_t *) data = *(amount_t *) val.data;
    return *this;
  }
  else if (type == BALANCE && val.type == BALANCE) {
    *(balance_t *) data = *(balance_t *) val.data;
    return *this;
  }
  else if (type == BALANCE_PAIR && val.type == BALANCE_PAIR) {
    *(balance_pair_t *) data = *(balance_pair_t *) val.data;
    return *this;
  }
  else if (type == STRING && val.type == STRING) {
    **(string **) data = **(string **) val.data;
    return *this;
  }
  else if (type == SEQUENCE && val.type == SEQUENCE) {
    **(sequence_t **) data = **(sequence_t **) val.data;
    return *this;
  }

  destroy();

  switch (val.type) {
  case BOOLEAN:
    *((bool *) data) = *((bool *) val.data);
    break;

  case INTEGER:
    *((long *) data) = *((long *) val.data);
    break;

  case DATETIME:
    *((moment_t *) data) = *((moment_t *) val.data);
    break;

  case AMOUNT:
    new((amount_t *)data) amount_t(*((amount_t *) val.data));
    break;

  case BALANCE:
    new((balance_t *)data) balance_t(*((balance_t *) val.data));
    break;

  case BALANCE_PAIR:
    new((balance_pair_t *)data) balance_pair_t(*((balance_pair_t *) val.data));
    break;

  case STRING:
    *(string **) data = new string(**(string **) val.data);
    break;

  case XML_NODE:
    *(xml::node_t **) data = *(xml::node_t **) val.data;
    break;

  case POINTER:
    *(void **) data = *(void **) val.data;
    break;

  case SEQUENCE:
    *(sequence_t **) data = new sequence_t(**(sequence_t **) val.data);
    break;

  default:
    assert(0);
    break;
  }

  type = val.type;

  return *this;
}

value_t& value_t::operator+=(const value_t& val)
{
  if (val.type == BOOLEAN)
    throw_(value_exception, "Cannot add a boolean to a value");
  else if (val.type == DATETIME)
    throw_(value_exception, "Cannot add a date/time to a value");
  else if (val.type == POINTER)
    throw_(value_exception, "Cannot add a pointer to a value");
  else if (val.type == SEQUENCE)
    throw_(value_exception, "Cannot add a sequence to a value");
  else if (val.type == XML_NODE) // recurse
    return *this += (*(xml::node_t **) val.data)->to_value();

  switch (type) {
  case BOOLEAN:
    throw_(value_exception, "Cannot add a value to a boolean");

  case INTEGER:
    switch (val.type) {
    case INTEGER:
      *((long *) data) += *((long *) val.data);
      break;
    case AMOUNT:
      in_place_cast(AMOUNT);
      *((amount_t *) data) += *((amount_t *) val.data);
      break;
    case BALANCE:
      in_place_cast(BALANCE);
      *((balance_t *) data) += *((balance_t *) val.data);
      break;
    case BALANCE_PAIR:
      in_place_cast(BALANCE_PAIR);
      *((balance_pair_t *) data) += *((balance_pair_t *) val.data);
      break;
    case STRING:
      throw_(value_exception, "Cannot add a string to an integer");
    default:
      assert(0);
      break;
    }
    break;

  case DATETIME:
    switch (val.type) {
    case INTEGER:
      *((moment_t *) data) += date_duration(*((long *) val.data));
      break;
    case AMOUNT:
      *((moment_t *) data) += date_duration(long(*((amount_t *) val.data)));
      break;
    case BALANCE:
      *((moment_t *) data) += date_duration(long(*((balance_t *) val.data)));
      break;
    case BALANCE_PAIR:
      *((moment_t *) data) += date_duration(long(*((balance_pair_t *) val.data)));
      break;
    case STRING:
      throw_(value_exception, "Cannot add a string to an date/time");
    default:
      assert(0);
      break;
    }
    break;

  case AMOUNT:
    switch (val.type) {
    case INTEGER:
      if (*((long *) val.data) &&
	  ((amount_t *) data)->commodity()) {
	in_place_cast(BALANCE);
	return *this += val;
      }
      *((amount_t *) data) += *((long *) val.data);
      break;

    case AMOUNT:
      if (((amount_t *) data)->commodity() !=
	  ((amount_t *) val.data)->commodity()) {
	in_place_cast(BALANCE);
	return *this += val;
      }
      *((amount_t *) data) += *((amount_t *) val.data);
      break;

    case BALANCE:
      in_place_cast(BALANCE);
      *((balance_t *) data) += *((balance_t *) val.data);
      break;

    case BALANCE_PAIR:
      in_place_cast(BALANCE_PAIR);
      *((balance_pair_t *) data) += *((balance_pair_t *) val.data);
      break;

    case STRING:
      throw_(value_exception, "Cannot add a string to an amount");

    default:
      assert(0);
      break;
    }
    break;

  case BALANCE:
    switch (val.type) {
    case INTEGER:
      *((balance_t *) data) += *((long *) val.data);
      break;
    case AMOUNT:
      *((balance_t *) data) += *((amount_t *) val.data);
      break;
    case BALANCE:
      *((balance_t *) data) += *((balance_t *) val.data);
      break;
    case BALANCE_PAIR:
      in_place_cast(BALANCE_PAIR);
      *((balance_pair_t *) data) += *((balance_pair_t *) val.data);
      break;
    case STRING:
      throw_(value_exception, "Cannot add a string to an balance");
    default:
      assert(0);
      break;
    }
    break;

  case BALANCE_PAIR:
    switch (val.type) {
    case INTEGER:
      *((balance_pair_t *) data) += *((long *) val.data);
      break;
    case AMOUNT:
      *((balance_pair_t *) data) += *((amount_t *) val.data);
      break;
    case BALANCE:
      *((balance_pair_t *) data) += *((balance_t *) val.data);
      break;
    case BALANCE_PAIR:
      *((balance_pair_t *) data) += *((balance_pair_t *) val.data);
      break;
    case STRING:
      throw_(value_exception, "Cannot add a string to an balance pair");
    default:
      assert(0);
      break;
    }
    break;

  case STRING:
    switch (val.type) {
    case INTEGER:
      throw_(value_exception, "Cannot add an integer to a string");
    case AMOUNT:
      throw_(value_exception, "Cannot add an amount to a string");
    case BALANCE:
      throw_(value_exception, "Cannot add a balance to a string");
    case BALANCE_PAIR:
      throw_(value_exception, "Cannot add a balance pair to a string");
    case STRING:
      **(string **) data += **(string **) val.data;
      break;
    default:
      assert(0);
      break;
    }
    break;

  case XML_NODE:
    throw_(value_exception, "Cannot add a value to an XML node");

  case POINTER:
    throw_(value_exception, "Cannot add a value to a pointer");

  case SEQUENCE:
    throw_(value_exception, "Cannot add a value to a sequence");

  default:
    assert(0);
    break;
  }
  return *this;
}

value_t& value_t::operator-=(const value_t& val)
{
  if (val.type == BOOLEAN)
    throw_(value_exception, "Cannot subtract a boolean from a value");
  else if (val.type == DATETIME && type != DATETIME)
    throw_(value_exception, "Cannot subtract a date/time from a value");
  else if (val.type == STRING)
    throw_(value_exception, "Cannot subtract a string from a value");
  else if (val.type == POINTER)
    throw_(value_exception, "Cannot subtract a pointer from a value");
  else if (val.type == SEQUENCE)
    throw_(value_exception, "Cannot subtract a sequence from a value");
  else if (val.type == XML_NODE) // recurse
    return *this -= (*(xml::node_t **) val.data)->to_value();

  switch (type) {
  case BOOLEAN:
    throw_(value_exception, "Cannot subtract a value from a boolean");

  case INTEGER:
    switch (val.type) {
    case INTEGER:
      *((long *) data) -= *((long *) val.data);
      break;
    case AMOUNT:
      in_place_cast(AMOUNT);
      *((amount_t *) data) -= *((amount_t *) val.data);
      break;
    case BALANCE:
      in_place_cast(BALANCE);
      *((balance_t *) data) -= *((balance_t *) val.data);
      break;
    case BALANCE_PAIR:
      in_place_cast(BALANCE_PAIR);
      *((balance_pair_t *) data) -= *((balance_pair_t *) val.data);
      break;
    default:
      assert(0);
      break;
    }
    break;

  case DATETIME:
    switch (val.type) {
    case INTEGER:
      *((moment_t *) data) -= date_duration(*((long *) val.data));
      break;
    case DATETIME: {
      duration_t tval = ((moment_t *) data)->operator-(*((moment_t *) val.data));
      in_place_cast(INTEGER);
      *((long *) data) = tval.total_seconds() / 86400L;
      break;
    }
    case AMOUNT:
      *((moment_t *) data) -= date_duration(long(*((amount_t *) val.data)));
      break;
    case BALANCE:
      *((moment_t *) data) -= date_duration(long(*((balance_t *) val.data)));
      break;
    case BALANCE_PAIR:
      *((moment_t *) data) -= date_duration(long(*((balance_pair_t *) val.data)));
      break;
    default:
      assert(0);
      break;
    }
    break;

  case AMOUNT:
    switch (val.type) {
    case INTEGER:
      if (*((long *) val.data) &&
	  ((amount_t *) data)->commodity()) {
	in_place_cast(BALANCE);
	return *this -= val;
      }
      *((amount_t *) data) -= *((long *) val.data);
      break;

    case AMOUNT:
      if (((amount_t *) data)->commodity() !=
	  ((amount_t *) val.data)->commodity()) {
	in_place_cast(BALANCE);
	return *this -= val;
      }
      *((amount_t *) data) -= *((amount_t *) val.data);
      break;

    case BALANCE:
      in_place_cast(BALANCE);
      *((balance_t *) data) -= *((balance_t *) val.data);
      break;

    case BALANCE_PAIR:
      in_place_cast(BALANCE_PAIR);
      *((balance_pair_t *) data) -= *((balance_pair_t *) val.data);
      break;

    default:
      assert(0);
      break;
    }
    break;

  case BALANCE:
    switch (val.type) {
    case INTEGER:
      *((balance_t *) data) -= *((long *) val.data);
      break;
    case AMOUNT:
      *((balance_t *) data) -= *((amount_t *) val.data);
      break;
    case BALANCE:
      *((balance_t *) data) -= *((balance_t *) val.data);
      break;
    case BALANCE_PAIR:
      in_place_cast(BALANCE_PAIR);
      *((balance_pair_t *) data) -= *((balance_pair_t *) val.data);
      break;
    default:
      assert(0);
      break;
    }
    break;

  case BALANCE_PAIR:
    switch (val.type) {
    case INTEGER:
      *((balance_pair_t *) data) -= *((long *) val.data);
      break;
    case AMOUNT:
      *((balance_pair_t *) data) -= *((amount_t *) val.data);
      break;
    case BALANCE:
      *((balance_pair_t *) data) -= *((balance_t *) val.data);
      break;
    case BALANCE_PAIR:
      *((balance_pair_t *) data) -= *((balance_pair_t *) val.data);
      break;
    default:
      assert(0);
      break;
    }
    break;

  case STRING:
    throw_(value_exception, "Cannot subtract a value from a string");
  case XML_NODE:
    throw_(value_exception, "Cannot subtract a value from an XML node");
  case POINTER:
    throw_(value_exception, "Cannot subtract a value from a pointer");
  case SEQUENCE:
    throw_(value_exception, "Cannot subtract a value from a sequence");

  default:
    assert(0);
    break;
  }

  simplify();

  return *this;
}

value_t& value_t::operator*=(const value_t& val)
{
  if (val.type == BOOLEAN)
    throw_(value_exception, "Cannot multiply a value by a boolean");
  else if (val.type == DATETIME)
    throw_(value_exception, "Cannot multiply a value by a date/time");
  else if (val.type == STRING)
    throw_(value_exception, "Cannot multiply a value by a string");
  else if (val.type == POINTER)
    throw_(value_exception, "Cannot multiply a value by a pointer");
  else if (val.type == SEQUENCE)
    throw_(value_exception, "Cannot multiply a value by a sequence");
  else if (val.type == XML_NODE) // recurse
    return *this *= (*(xml::node_t **) val.data)->to_value();

  if (val.realzero() && type != STRING) {
    *this = 0L;
    return *this;
  }

  switch (type) {
  case BOOLEAN:
    throw_(value_exception, "Cannot multiply a value by a boolean");

  case INTEGER:
    switch (val.type) {
    case INTEGER:
      *((long *) data) *= *((long *) val.data);
      break;
    case AMOUNT:
      in_place_cast(AMOUNT);
      *((amount_t *) data) *= *((amount_t *) val.data);
      break;
    case BALANCE:
      in_place_cast(BALANCE);
      *((balance_t *) data) *= *((balance_t *) val.data);
      break;
    case BALANCE_PAIR:
      in_place_cast(BALANCE_PAIR);
      *((balance_pair_t *) data) *= *((balance_pair_t *) val.data);
      break;
    default:
      assert(0);
      break;
    }
    break;

  case AMOUNT:
    switch (val.type) {
    case INTEGER:
      *((amount_t *) data) *= *((long *) val.data);
      break;
    case AMOUNT:
      *((amount_t *) data) *= *((amount_t *) val.data);
      break;
    case BALANCE:
      in_place_cast(BALANCE);
      *((balance_t *) data) *= *((balance_t *) val.data);
      break;
    case BALANCE_PAIR:
      in_place_cast(BALANCE_PAIR);
      *((balance_pair_t *) data) *= *((balance_pair_t *) val.data);
      break;
    default:
      assert(0);
      break;
    }
    break;

  case BALANCE:
    switch (val.type) {
    case INTEGER:
      *((balance_t *) data) *= *((long *) val.data);
      break;
    case AMOUNT:
      *((balance_t *) data) *= *((amount_t *) val.data);
      break;
    case BALANCE:
      *((balance_t *) data) *= *((balance_t *) val.data);
      break;
    case BALANCE_PAIR:
      in_place_cast(BALANCE_PAIR);
      *((balance_pair_t *) data) *= *((balance_pair_t *) val.data);
      break;
    default:
      assert(0);
      break;
    }
    break;

  case BALANCE_PAIR:
    switch (val.type) {
    case INTEGER:
      *((balance_pair_t *) data) *= *((long *) val.data);
      break;
    case AMOUNT:
      *((balance_pair_t *) data) *= *((amount_t *) val.data);
      break;
    case BALANCE:
      *((balance_pair_t *) data) *= *((balance_t *) val.data);
      break;
    case BALANCE_PAIR:
      *((balance_pair_t *) data) *= *((balance_pair_t *) val.data);
      break;
    default:
      assert(0);
      break;
    }
    break;

  case STRING:
    switch (val.type) {
    case INTEGER: {
      string temp;
      for (long i = 0; i < *(long *) val.data; i++)
	temp += **(string **) data;
      **(string **) data = temp;
      break;
    }
    case AMOUNT: {
      string temp;
      value_t num(val);
      num.in_place_cast(INTEGER);
      for (long i = 0; i < *(long *) num.data; i++)
	temp += **(string **) data;
      **(string **) data = temp;
      break;
    }
    case BALANCE:
      throw_(value_exception, "Cannot multiply a string by a balance");
    case BALANCE_PAIR:
      throw_(value_exception, "Cannot multiply a string by a balance pair");
    default:
      assert(0);
      break;
    }
    break;

  case XML_NODE:
    throw_(value_exception, "Cannot multiply an XML node by a value");
  case POINTER:
    throw_(value_exception, "Cannot multiply a pointer by a value");
  case SEQUENCE:
    throw_(value_exception, "Cannot multiply a sequence by a value");

  default:
    assert(0);
    break;
  }
  return *this;
}

value_t& value_t::operator/=(const value_t& val)
{
  if (val.type == BOOLEAN)
    throw_(value_exception, "Cannot divide a boolean by a value");
  else if (val.type == DATETIME)
    throw_(value_exception, "Cannot divide a date/time by a value");
  else if (val.type == STRING)
    throw_(value_exception, "Cannot divide a string by a value");
  else if (val.type == POINTER)
    throw_(value_exception, "Cannot divide a pointer by a value");
  else if (val.type == SEQUENCE)
    throw_(value_exception, "Cannot divide a value by a sequence");
  else if (val.type == XML_NODE) // recurse
    return *this /= (*(xml::node_t **) val.data)->to_value();

  switch (type) {
  case BOOLEAN:
    throw_(value_exception, "Cannot divide a value by a boolean");

  case INTEGER:
    switch (val.type) {
    case INTEGER:
      *((long *) data) /= *((long *) val.data);
      break;
    case AMOUNT:
      in_place_cast(AMOUNT);
      *((amount_t *) data) /= *((amount_t *) val.data);
      break;
    case BALANCE:
      in_place_cast(BALANCE);
      *((balance_t *) data) /= *((balance_t *) val.data);
      break;
    case BALANCE_PAIR:
      in_place_cast(BALANCE_PAIR);
      *((balance_pair_t *) data) /= *((balance_pair_t *) val.data);
      break;
    default:
      assert(0);
      break;
    }
    break;

  case AMOUNT:
    switch (val.type) {
    case INTEGER:
      *((amount_t *) data) /= *((long *) val.data);
      break;
    case AMOUNT:
      *((amount_t *) data) /= *((amount_t *) val.data);
      break;
    case BALANCE:
      in_place_cast(BALANCE);
      *((balance_t *) data) /= *((balance_t *) val.data);
      break;
    case BALANCE_PAIR:
      in_place_cast(BALANCE_PAIR);
      *((balance_pair_t *) data) /= *((balance_pair_t *) val.data);
      break;
    default:
      assert(0);
      break;
    }
    break;

  case BALANCE:
    switch (val.type) {
    case INTEGER:
      *((balance_t *) data) /= *((long *) val.data);
      break;
    case AMOUNT:
      *((balance_t *) data) /= *((amount_t *) val.data);
      break;
    case BALANCE:
      *((balance_t *) data) /= *((balance_t *) val.data);
      break;
    case BALANCE_PAIR:
      in_place_cast(BALANCE_PAIR);
      *((balance_pair_t *) data) /= *((balance_pair_t *) val.data);
      break;
    default:
      assert(0);
      break;
    }
    break;

  case BALANCE_PAIR:
    switch (val.type) {
    case INTEGER:
      *((balance_pair_t *) data) /= *((long *) val.data);
      break;
    case AMOUNT:
      *((balance_pair_t *) data) /= *((amount_t *) val.data);
      break;
    case BALANCE:
      *((balance_pair_t *) data) /= *((balance_t *) val.data);
      break;
    case BALANCE_PAIR:
      *((balance_pair_t *) data) /= *((balance_pair_t *) val.data);
      break;
    default:
      assert(0);
      break;
    }
    break;

  case STRING:
    throw_(value_exception, "Cannot divide a value from a string");
  case XML_NODE:
    throw_(value_exception, "Cannot divide a value from an XML node");
  case POINTER:
    throw_(value_exception, "Cannot divide a value from a pointer");
  case SEQUENCE:
    throw_(value_exception, "Cannot divide a value from a sequence");

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
    return is_valid_moment(*((moment_t *) data));
  case AMOUNT:
    return *(amount_t *) data;
  case BALANCE:
    return *(balance_t *) data;
  case BALANCE_PAIR:
    return *(balance_pair_t *) data;
  case STRING:
    return ! (**((string **) data)).empty();
  case XML_NODE:
    return (*(xml::node_t **) data)->to_value().to_boolean();
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
    throw_(value_exception, "Cannot convert a boolean to an integer");
  case INTEGER:
    return *((long *) data);
  case DATETIME:
    throw_(value_exception, "Cannot convert a date/time to an integer");
  case AMOUNT:
    return *((amount_t *) data);
  case BALANCE:
    throw_(value_exception, "Cannot convert a balance to an integer");
  case BALANCE_PAIR:
    throw_(value_exception, "Cannot convert a balance pair to an integer");
  case STRING:
    throw_(value_exception, "Cannot convert a string to an integer");
  case XML_NODE:
    return (*(xml::node_t **) data)->to_value().to_integer();
  case POINTER:
    throw_(value_exception, "Cannot convert a pointer to an integer");
  case SEQUENCE:
    throw_(value_exception, "Cannot convert a sequence to an integer");

  default:
    assert(0);
    break;
  }
  assert(0);
  return 0;
}

template <>
value_t::operator moment_t() const
{
  switch (type) {
  case BOOLEAN:
    throw_(value_exception, "Cannot convert a boolean to a date/time");
  case INTEGER:
    throw_(value_exception, "Cannot convert an integer to a date/time");
  case DATETIME:
    return *((moment_t *) data);
  case AMOUNT:
    throw_(value_exception, "Cannot convert an amount to a date/time");
  case BALANCE:
    throw_(value_exception, "Cannot convert a balance to a date/time");
  case BALANCE_PAIR:
    throw_(value_exception, "Cannot convert a balance pair to a date/time");
  case STRING:
    throw_(value_exception, "Cannot convert a string to a date/time");
  case XML_NODE:
    return (*(xml::node_t **) data)->to_value().to_datetime();
  case POINTER:
    throw_(value_exception, "Cannot convert a pointer to a date/time");
  case SEQUENCE:
    throw_(value_exception, "Cannot convert a sequence to a date/time");

  default:
    assert(0);
    break;
  }
  assert(0);
  return moment_t();
}

template <>
value_t::operator double() const
{
  switch (type) {
  case BOOLEAN:
    throw_(value_exception, "Cannot convert a boolean to a double");
  case INTEGER:
    return *((long *) data);
  case DATETIME:
    throw_(value_exception, "Cannot convert a date/time to a double");
  case AMOUNT:
    return *((amount_t *) data);
  case BALANCE:
    throw_(value_exception, "Cannot convert a balance to a double");
  case BALANCE_PAIR:
    throw_(value_exception, "Cannot convert a balance pair to a double");
  case STRING:
    throw_(value_exception, "Cannot convert a string to a double");
  case XML_NODE:
    return (*(xml::node_t **) data)->to_value().to_amount().number();
  case POINTER:
    throw_(value_exception, "Cannot convert a pointer to a double");
  case SEQUENCE:
    throw_(value_exception, "Cannot convert a sequence to a double");

  default:
    assert(0);
    break;
  }
  assert(0);
  return 0;
}

template <>
value_t::operator string() const
{
  switch (type) {
  case BOOLEAN:
  case INTEGER:
  case DATETIME:
  case AMOUNT:
  case BALANCE:
  case BALANCE_PAIR: {
    value_t temp(*this);
    temp.in_place_cast(STRING);
    return temp;
  }
  case STRING:
    return **(string **) data;
  case XML_NODE:
    return (*(xml::node_t **) data)->to_value().to_string();

  case POINTER:
    throw_(value_exception, "Cannot convert a pointer to a string");
  case SEQUENCE:
    throw_(value_exception, "Cannot convert a sequence to a string");

  default:
    assert(0);
    break;
  }
  assert(0);
  return 0;
}

#define DEF_VALUE_CMP_OP(OP)						\
bool value_t::operator OP(const value_t& val)				\
{									\
  switch (type) {							\
  case BOOLEAN:								\
    switch (val.type) {							\
    case BOOLEAN:							\
      return *((bool *) data) OP *((bool *) val.data);			\
									\
    case INTEGER:							\
      return *((bool *) data) OP bool(*((long *) val.data));		\
									\
    case DATETIME:							\
      throw_(value_exception, "Cannot compare a boolean to a date/time");	\
									\
    case AMOUNT:							\
      return *((bool *) data) OP bool(*((amount_t *) val.data));	\
									\
    case BALANCE:							\
      return *((bool *) data) OP bool(*((balance_t *) val.data));	\
									\
    case BALANCE_PAIR:							\
      return *((bool *) data) OP bool(*((balance_pair_t *) val.data));	\
									\
    case STRING:							\
      throw_(value_exception, "Cannot compare a boolean to a string");	\
									\
    case XML_NODE:							\
      return *this OP (*(xml::node_t **) data)->to_value();		\
									\
    case POINTER:							\
      throw_(value_exception, "Cannot compare a boolean to a pointer");	\
    case SEQUENCE:							\
      throw_(value_exception, "Cannot compare a boolean to a sequence");	\
									\
    default:								\
      assert(0);							\
      break;								\
    }									\
    break;								\
									\
  case INTEGER:								\
    switch (val.type) {							\
    case BOOLEAN:							\
      return (*((long *) data) OP					\
	      ((long) *((bool *) val.data)));				\
									\
    case INTEGER:							\
      return (*((long *) data) OP *((long *) val.data));		\
									\
    case DATETIME:							\
      throw_(value_exception, "Cannot compare an integer to a date/time"); \
									\
    case AMOUNT:							\
      return (amount_t(*((long *) data)) OP				\
	      *((amount_t *) val.data));				\
									\
    case BALANCE:							\
      return (balance_t(*((long *) data)) OP				\
	      *((balance_t *) val.data));				\
									\
    case BALANCE_PAIR:							\
      return (balance_pair_t(*((long *) data)) OP			\
	      *((balance_pair_t *) val.data));				\
									\
    case STRING:							\
      throw_(value_exception, "Cannot compare an integer to a string");	\
									\
    case XML_NODE:							\
      return *this OP (*(xml::node_t **) data)->to_value();		\
									\
    case POINTER:							\
      throw_(value_exception, "Cannot compare an integer to a pointer");	\
    case SEQUENCE:							\
      throw_(value_exception, "Cannot compare an integer to a sequence");	\
									\
    default:								\
      assert(0);							\
      break;								\
    }									\
    break;								\
									\
  case DATETIME:							\
    switch (val.type) {							\
    case BOOLEAN:							\
      throw_(value_exception, "Cannot compare a date/time to a boolean");	\
									\
    case INTEGER:							\
      throw_(value_exception, "Cannot compare a date/time to an integer"); \
									\
    case DATETIME:							\
      return *((moment_t *) data) OP *((moment_t *) val.data);		\
									\
    case AMOUNT:							\
      throw_(value_exception, "Cannot compare a date/time to an amount");	\
    case BALANCE:							\
      throw_(value_exception, "Cannot compare a date/time to a balance");	\
    case BALANCE_PAIR:							\
      throw_(value_exception, "Cannot compare a date/time to a balance pair"); \
    case STRING:							\
      throw_(value_exception, "Cannot compare a date/time to a string");	\
									\
    case XML_NODE:							\
      return *this OP (*(xml::node_t **) data)->to_value();		\
									\
    case POINTER:							\
      throw_(value_exception, "Cannot compare a date/time to a pointer");	\
    case SEQUENCE:							\
      throw_(value_exception, "Cannot compare a date/time to a sequence"); \
									\
    default:								\
      assert(0);							\
      break;								\
    }									\
    break;								\
									\
  case AMOUNT:								\
    switch (val.type) {							\
    case BOOLEAN:							\
      throw_(value_exception, "Cannot compare an amount to a boolean");	\
									\
    case INTEGER:							\
      return (*((amount_t *) data) OP					\
	      amount_t(*((long *) val.data)));				\
									\
    case DATETIME:							\
      throw_(value_exception, "Cannot compare an amount to a date/time");	\
									\
    case AMOUNT:							\
      return *((amount_t *) data) OP *((amount_t *) val.data);		\
									\
    case BALANCE:							\
      return (balance_t(*((amount_t *) data)) OP			\
	      *((balance_t *) val.data));				\
									\
    case BALANCE_PAIR:							\
      return (balance_t(*((amount_t *) data)) OP			\
	      *((balance_pair_t *) val.data));				\
									\
    case STRING:							\
      throw_(value_exception, "Cannot compare an amount to a string");	\
									\
    case XML_NODE:							\
      return *this OP (*(xml::node_t **) data)->to_value();		\
									\
    case POINTER:							\
      throw_(value_exception, "Cannot compare an amount to a pointer");	\
    case SEQUENCE:							\
      throw_(value_exception, "Cannot compare an amount to a sequence");	\
									\
    default:								\
      assert(0);							\
      break;								\
    }									\
    break;								\
									\
  case BALANCE:								\
    switch (val.type) {							\
    case BOOLEAN:							\
      throw_(value_exception, "Cannot compare a balance to a boolean");	\
									\
    case INTEGER:							\
      return *((balance_t *) data) OP *((long *) val.data);		\
									\
    case DATETIME:							\
      throw_(value_exception, "Cannot compare a balance to a date/time");	\
									\
    case AMOUNT:							\
      return *((balance_t *) data) OP *((amount_t *) val.data);		\
									\
    case BALANCE:							\
      return *((balance_t *) data) OP *((balance_t *) val.data);	\
									\
    case BALANCE_PAIR:							\
      return (*((balance_t *) data) OP					\
	      ((balance_pair_t *) val.data)->quantity);			\
									\
    case STRING:							\
      throw_(value_exception, "Cannot compare a balance to a string");	\
									\
    case XML_NODE:							\
      return *this OP (*(xml::node_t **) data)->to_value();		\
									\
    case POINTER:							\
      throw_(value_exception, "Cannot compare a balance to a pointer");	\
    case SEQUENCE:							\
      throw_(value_exception, "Cannot compare a balance to a sequence");	\
									\
    default:								\
      assert(0);							\
      break;								\
    }									\
    break;								\
									\
  case BALANCE_PAIR:							\
    switch (val.type) {							\
    case BOOLEAN:							\
      throw_(value_exception, "Cannot compare a balance pair to a boolean"); \
									\
    case INTEGER:							\
      return (((balance_pair_t *) data)->quantity OP			\
	      *((long *) val.data));					\
									\
    case DATETIME:							\
      throw_(value_exception, "Cannot compare a balance pair to a date/time"); \
									\
    case AMOUNT:							\
      return (((balance_pair_t *) data)->quantity OP			\
	      *((amount_t *) val.data));				\
									\
    case BALANCE:							\
      return (((balance_pair_t *) data)->quantity OP			\
	      *((balance_t *) val.data));				\
									\
    case BALANCE_PAIR:							\
      return (*((balance_pair_t *) data) OP				\
	      *((balance_pair_t *) val.data));				\
									\
    case STRING:							\
      throw_(value_exception, "Cannot compare a balance pair to a string"); \
									\
    case XML_NODE:							\
      return *this OP (*(xml::node_t **) data)->to_value();		\
									\
    case POINTER:							\
      throw_(value_exception, "Cannot compare a balance pair to a pointer"); \
    case SEQUENCE:							\
      throw_(value_exception, "Cannot compare a balance pair to a sequence"); \
									\
    default:								\
      assert(0);							\
      break;								\
    }									\
    break;								\
									\
  case STRING:								\
    switch (val.type) {							\
    case BOOLEAN:							\
      throw_(value_exception, "Cannot compare a string to a boolean");	\
    case INTEGER:							\
      throw_(value_exception, "Cannot compare a string to an integer");	\
    case DATETIME:							\
      throw_(value_exception, "Cannot compare a string to a date/time");	\
    case AMOUNT:							\
      throw_(value_exception, "Cannot compare a string to an amount");	\
    case BALANCE:							\
      throw_(value_exception, "Cannot compare a string to a balance");	\
    case BALANCE_PAIR:							\
      throw_(value_exception, "Cannot compare a string to a balance pair"); \
									\
    case STRING:							\
      return (**((string **) data) OP				\
	      **((string **) val.data));				\
									\
    case XML_NODE:							\
      return *this OP (*(xml::node_t **) data)->to_value();		\
									\
    case POINTER:							\
      throw_(value_exception, "Cannot compare a string to a pointer");	\
    case SEQUENCE:							\
      throw_(value_exception, "Cannot compare a string to a sequence");	\
									\
    default:								\
      assert(0);							\
      break;								\
    }									\
    break;								\
									\
  case XML_NODE:							\
    switch (val.type) {							\
    case BOOLEAN:							\
      return (*(xml::node_t **) data)->to_value() OP *this;		\
    case INTEGER:							\
      return (*(xml::node_t **) data)->to_value() OP *this;		\
    case DATETIME:							\
      return (*(xml::node_t **) data)->to_value() OP *this;		\
    case AMOUNT:							\
      return (*(xml::node_t **) data)->to_value() OP *this;		\
    case BALANCE:							\
      return (*(xml::node_t **) data)->to_value() OP *this;		\
    case BALANCE_PAIR:							\
      return (*(xml::node_t **) data)->to_value() OP *this;		\
    case STRING:							\
      return (*(xml::node_t **) data)->to_value() OP *this;		\
									\
    case XML_NODE:							\
      return ((*(xml::node_t **) data)->to_value() OP			\
	      (*(xml::node_t **) val.data)->to_value());		\
									\
    case POINTER:							\
      throw_(value_exception, "Cannot compare an XML node to a pointer");	\
    case SEQUENCE:							\
      throw_(value_exception, "Cannot compare an XML node to a sequence"); \
									\
    default:								\
      assert(0);							\
      break;								\
    }									\
    break;								\
									\
  case POINTER:								\
    switch (val.type) {							\
    case BOOLEAN:							\
      throw_(value_exception, "Cannot compare a pointer to a boolean");	\
    case INTEGER:							\
      throw_(value_exception, "Cannot compare a pointer to an integer");	\
    case DATETIME:							\
      throw_(value_exception, "Cannot compare a pointer to a date/time");	\
    case AMOUNT:							\
      throw_(value_exception, "Cannot compare a pointer to an amount");	\
    case BALANCE:							\
      throw_(value_exception, "Cannot compare a pointer to a balance");	\
    case BALANCE_PAIR:							\
      throw_(value_exception, "Cannot compare a pointer to a balance pair"); \
    case STRING:							\
      throw_(value_exception, "Cannot compare a pointer to a string node"); \
    case XML_NODE:							\
      throw_(value_exception, "Cannot compare a pointer to an XML node");	\
    case POINTER:							\
      return (*((void **) data) OP *((void **) val.data));		\
    case SEQUENCE:							\
      throw_(value_exception, "Cannot compare a pointer to a sequence");	\
									\
    default:								\
      assert(0);							\
      break;								\
    }									\
    break;								\
									\
  case SEQUENCE:							\
    throw_(value_exception, "Cannot compare a value to a sequence");	\
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

void value_t::in_place_cast(type_t cast_type)
{
  switch (type) {
  case BOOLEAN:
    switch (cast_type) {
    case BOOLEAN:
      break;
    case INTEGER:
      throw_(value_exception, "Cannot convert a boolean to an integer");
    case DATETIME:
      throw_(value_exception, "Cannot convert a boolean to a date/time");
    case AMOUNT:
      throw_(value_exception, "Cannot convert a boolean to an amount");
    case BALANCE:
      throw_(value_exception, "Cannot convert a boolean to a balance");
    case BALANCE_PAIR:
      throw_(value_exception, "Cannot convert a boolean to a balance pair");
    case STRING:
      *(string **) data = new string(*((bool *) data) ? "true" : "false");
      break;
    case XML_NODE:
      throw_(value_exception, "Cannot convert a boolean to an XML node");
    case POINTER:
      throw_(value_exception, "Cannot convert a boolean to a pointer");
    case SEQUENCE:
      throw_(value_exception, "Cannot convert a boolean to a sequence");

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
      throw_(value_exception, "Cannot convert an integer to a date/time");

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
      *(string **) data = new string(buf);
      break;
    }
    case XML_NODE:
      throw_(value_exception, "Cannot convert an integer to an XML node");
    case POINTER:
      throw_(value_exception, "Cannot convert an integer to a pointer");
    case SEQUENCE:
      throw_(value_exception, "Cannot convert an integer to a sequence");

    default:
      assert(0);
      break;
    }
    break;

  case DATETIME:
    switch (cast_type) {
    case BOOLEAN:
      *((bool *) data) = is_valid_moment(*((moment_t *) data));
      break;
    case INTEGER:
      throw_(value_exception, "Cannot convert a date/time to an integer");
    case DATETIME:
      break;
    case AMOUNT:
      throw_(value_exception, "Cannot convert a date/time to an amount");
    case BALANCE:
      throw_(value_exception, "Cannot convert a date/time to a balance");
    case BALANCE_PAIR:
      throw_(value_exception, "Cannot convert a date/time to a balance pair");
    case STRING:
      throw_(value_exception, "Cannot convert a date/time to a string");
    case XML_NODE:
      throw_(value_exception, "Cannot convert a date/time to an XML node");
    case POINTER:
      throw_(value_exception, "Cannot convert a date/time to a pointer");
    case SEQUENCE:
      throw_(value_exception, "Cannot convert a date/time to a sequence");

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
      throw_(value_exception, "Cannot convert an amount to a date/time");
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
      *(string **) data = new string(out.str());
      break;
    }
    case XML_NODE:
      throw_(value_exception, "Cannot convert an amount to an XML node");
    case POINTER:
      throw_(value_exception, "Cannot convert an amount to a pointer");
    case SEQUENCE:
      throw_(value_exception, "Cannot convert an amount to a sequence");

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
      throw_(value_exception, "Cannot convert a balance to an integer");
    case DATETIME:
      throw_(value_exception, "Cannot convert a balance to a date/time");

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
	throw_(value_exception, "Cannot convert a balance with "
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
      throw_(value_exception, "Cannot convert a balance to a string");
    case XML_NODE:
      throw_(value_exception, "Cannot convert a balance to an XML node");
    case POINTER:
      throw_(value_exception, "Cannot convert a balance to a pointer");
    case SEQUENCE:
      throw_(value_exception, "Cannot convert a balance to a sequence");

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
      throw_(value_exception, "Cannot convert a balance pair to an integer");
    case DATETIME:
      throw_(value_exception, "Cannot convert a balance pair to a date/time");

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
	throw_(value_exception, "Cannot convert a balance pair with "
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
      throw_(value_exception, "Cannot convert a balance pair to a string");
    case XML_NODE:
      throw_(value_exception, "Cannot convert a balance pair to an XML node");
    case POINTER:
      throw_(value_exception, "Cannot convert a balance pair to a pointer");
    case SEQUENCE:
      throw_(value_exception, "Cannot convert a balance pair to a sequence");

    default:
      assert(0);
      break;
    }
    break;

  case STRING:
    switch (cast_type) {
    case BOOLEAN: {
      if (**(string **) data == "true") {
	destroy();
	*(bool *) data = true;
      }
      else if (**(string **) data == "false") {
	destroy();
	*(bool *) data = false;
      }
      else {
	throw_(value_exception, "Cannot convert string to an boolean");
      }
      break;
    }
    case INTEGER: {
      int l = (*(string **) data)->length();
      const char * p = (*(string **) data)->c_str();
      bool alldigits = true;
      for (int i = 0; i < l; i++)
	if (! std::isdigit(p[i])) {
	  alldigits = false;
	  break;
	}
      if (alldigits) {
	long temp = std::atol((*(string **) data)->c_str());
	destroy();
	*(long *) data = temp;
      } else {
	throw_(value_exception, "Cannot convert string to an integer");
      }
      break;
    }

    case DATETIME:
      throw_(value_exception, "Cannot convert a string to a date/time");

    case AMOUNT: {
      amount_t temp = **(string **) data;
      destroy();
      new((amount_t *)data) amount_t(temp);
      break;
    }
    case BALANCE:
      throw_(value_exception, "Cannot convert a string to a balance");
    case BALANCE_PAIR:
      throw_(value_exception, "Cannot convert a string to a balance pair");
    case STRING:
      break;
    case XML_NODE:
      throw_(value_exception, "Cannot convert a string to an XML node");
    case POINTER:
      throw_(value_exception, "Cannot convert a string to a pointer");
    case SEQUENCE:
      throw_(value_exception, "Cannot convert a string to a sequence");

    default:
      assert(0);
      break;
    }
    break;

  case XML_NODE:
    switch (cast_type) {
    case BOOLEAN:
    case INTEGER:
    case DATETIME:
    case AMOUNT:
    case BALANCE:
    case BALANCE_PAIR:
    case STRING:
      *this = (*(xml::node_t **) data)->to_value();
      break;
    case XML_NODE:
      break;
    case POINTER:
      throw_(value_exception, "Cannot convert an XML node to a pointer");
    case SEQUENCE:
      throw_(value_exception, "Cannot convert an XML node to a sequence");

    default:
      assert(0);
      break;
    }
    break;

  case POINTER:
    switch (cast_type) {
    case BOOLEAN:
      throw_(value_exception, "Cannot convert a pointer to a boolean");
    case INTEGER:
      throw_(value_exception, "Cannot convert a pointer to an integer");
    case DATETIME:
      throw_(value_exception, "Cannot convert a pointer to a date/time");
    case AMOUNT:
      throw_(value_exception, "Cannot convert a pointer to an amount");
    case BALANCE:
      throw_(value_exception, "Cannot convert a pointer to a balance");
    case BALANCE_PAIR:
      throw_(value_exception, "Cannot convert a pointer to a balance pair");
    case STRING:
      throw_(value_exception, "Cannot convert a pointer to a string");
    case XML_NODE:
      throw_(value_exception, "Cannot convert a pointer to an XML node");
    case POINTER:
      break;
    case SEQUENCE:
      throw_(value_exception, "Cannot convert a pointer to a sequence");

    default:
      assert(0);
      break;
    }
    break;

  case SEQUENCE:
    switch (cast_type) {
    case BOOLEAN:
      throw_(value_exception, "Cannot convert a sequence to a boolean");
    case INTEGER:
      throw_(value_exception, "Cannot convert a sequence to an integer");
    case DATETIME:
      throw_(value_exception, "Cannot convert a sequence to a date/time");
    case AMOUNT:
      throw_(value_exception, "Cannot convert a sequence to an amount");
    case BALANCE:
      throw_(value_exception, "Cannot convert a sequence to a balance");
    case BALANCE_PAIR:
      throw_(value_exception, "Cannot convert a sequence to a balance pair");
    case STRING:
      throw_(value_exception, "Cannot convert a sequence to a string");
    case XML_NODE:
      throw_(value_exception, "Cannot compare a sequence to an XML node");
    case POINTER:
      throw_(value_exception, "Cannot convert a sequence to a pointer");
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

void value_t::in_place_negate()
{
  switch (type) {
  case BOOLEAN:
    *((bool *) data) = ! *((bool *) data);
    break;
  case INTEGER:
    *((long *) data) = - *((long *) data);
    break;
  case DATETIME:
    throw_(value_exception, "Cannot negate a date/time");
  case AMOUNT:
    ((amount_t *) data)->in_place_negate();
    break;
  case BALANCE:
    ((balance_t *) data)->in_place_negate();
    break;
  case BALANCE_PAIR:
    ((balance_pair_t *) data)->in_place_negate();
    break;
  case STRING:
    throw_(value_exception, "Cannot negate a string");
  case XML_NODE:
    *this = (*(xml::node_t **) data)->to_value();
    in_place_negate();
    break;
  case POINTER:
    throw_(value_exception, "Cannot negate a pointer");
  case SEQUENCE:
    throw_(value_exception, "Cannot negate a sequence");

  default:
    assert(0);
    break;
  }
}

void value_t::in_place_abs()
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
    throw_(value_exception, "Cannot take the absolute value of a string");
  case XML_NODE:
    *this = (*(xml::node_t **) data)->to_value();
    in_place_abs();
    break;
  case POINTER:
    throw_(value_exception, "Cannot take the absolute value of a pointer");
  case SEQUENCE:
    throw_(value_exception, "Cannot take the absolute value of a sequence");

  default:
    assert(0);
    break;
  }
}

value_t value_t::value(const moment_t& moment) const
{
  switch (type) {
  case BOOLEAN:
    throw_(value_exception, "Cannot find the value of a boolean");
  case DATETIME:
    throw_(value_exception, "Cannot find the value of a date/time");
  case INTEGER:
    return *this;
  case AMOUNT:
    return ((amount_t *) data)->value(moment);
  case BALANCE:
    return ((balance_t *) data)->value(moment);
  case BALANCE_PAIR:
    return ((balance_pair_t *) data)->quantity.value(moment);
  case STRING:
    throw_(value_exception, "Cannot find the value of a string");
  case XML_NODE:
    return (*(xml::node_t **) data)->to_value().value(moment);
  case POINTER:
    throw_(value_exception, "Cannot find the value of a pointer");
  case SEQUENCE:
    throw_(value_exception, "Cannot find the value of a sequence");
  default:
    assert(0);
    return value_t();
  }
}

void value_t::in_place_reduce()
{
  switch (type) {
  case BOOLEAN:
  case DATETIME:
  case INTEGER:
    break;
  case AMOUNT:
    ((amount_t *) data)->in_place_reduce();
    break;
  case BALANCE:
    ((balance_t *) data)->in_place_reduce();
    break;
  case BALANCE_PAIR:
    ((balance_pair_t *) data)->in_place_reduce();
    break;
  case STRING:
    throw_(value_exception, "Cannot reduce a string");
  case XML_NODE:
    *this = (*(xml::node_t **) data)->to_value();
    in_place_reduce();		// recurse
    break;
  case POINTER:
    throw_(value_exception, "Cannot reduce a pointer");
  case SEQUENCE:
    throw_(value_exception, "Cannot reduce a sequence");
  }
}

value_t value_t::round() const
{
  switch (type) {
  case BOOLEAN:
    throw_(value_exception, "Cannot round a boolean");
  case DATETIME:
    throw_(value_exception, "Cannot round a date/time");
  case INTEGER:
    return *this;
  case AMOUNT:
    return ((amount_t *) data)->round();
  case BALANCE:
    return ((balance_t *) data)->round();
  case BALANCE_PAIR:
    return ((balance_pair_t *) data)->round();
  case STRING:
    throw_(value_exception, "Cannot round a string");
  case XML_NODE:
    return (*(xml::node_t **) data)->to_value().round();
  case POINTER:
    throw_(value_exception, "Cannot round a pointer");
  case SEQUENCE:
    throw_(value_exception, "Cannot round a sequence");
  }
  assert(0);
  return value_t();
}

value_t value_t::unround() const
{
  switch (type) {
  case BOOLEAN:
    throw_(value_exception, "Cannot un-round a boolean");
  case DATETIME:
    throw_(value_exception, "Cannot un-round a date/time");
  case INTEGER:
    return *this;
  case AMOUNT:
    return ((amount_t *) data)->unround();
  case BALANCE:
    return ((balance_t *) data)->unround();
  case BALANCE_PAIR:
    return ((balance_pair_t *) data)->unround();
  case STRING:
    throw_(value_exception, "Cannot un-round a string");
  case XML_NODE:
    return (*(xml::node_t **) data)->to_value().unround();
  case POINTER:
    throw_(value_exception, "Cannot un-round a pointer");
  case SEQUENCE:
    throw_(value_exception, "Cannot un-round a sequence");
  }
  assert(0);
  return value_t();
}

value_t value_t::price() const
{
  switch (type) {
  case BOOLEAN:
    throw_(value_exception, "Cannot find the price of a boolean");
  case INTEGER:
    return *this;
  case DATETIME:
    throw_(value_exception, "Cannot find the price of a date/time");

  case AMOUNT:
    return ((amount_t *) data)->price();
  case BALANCE:
    return ((balance_t *) data)->price();
  case BALANCE_PAIR:
    return ((balance_pair_t *) data)->quantity.price();

  case STRING:
    throw_(value_exception, "Cannot find the price of a string");

  case XML_NODE:
    return (*(xml::node_t **) data)->to_value().price();

  case POINTER:
    throw_(value_exception, "Cannot find the price of a pointer");
  case SEQUENCE:
    throw_(value_exception, "Cannot find the price of a sequence");

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
    throw_(value_exception, "Cannot find the date of a boolean");
  case INTEGER:
    throw_(value_exception, "Cannot find the date of an integer");

  case DATETIME:
    return *this;

  case AMOUNT:
    return ((amount_t *) data)->date();
  case BALANCE:
    return ((balance_t *) data)->date();
  case BALANCE_PAIR:
    return ((balance_pair_t *) data)->quantity.date();

  case STRING:
    throw_(value_exception, "Cannot find the date of a string");

  case XML_NODE:
    return (*(xml::node_t **) data)->to_value().date();

  case POINTER:
    throw_(value_exception, "Cannot find the date of a pointer");
  case SEQUENCE:
    throw_(value_exception, "Cannot find the date of a sequence");

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
    throw_(value_exception, "Cannot find the cost of a boolean");
  case INTEGER:
  case AMOUNT:
  case BALANCE:
    return *this;
  case DATETIME:
    throw_(value_exception, "Cannot find the cost of a date/time");

  case BALANCE_PAIR:
    assert(((balance_pair_t *) data)->cost);
    if (((balance_pair_t *) data)->cost)
      return *(((balance_pair_t *) data)->cost);
    else
      return ((balance_pair_t *) data)->quantity;

  case STRING:
    throw_(value_exception, "Cannot find the cost of a string");
  case XML_NODE:
    return (*(xml::node_t **) data)->to_value().cost();
  case POINTER:
    throw_(value_exception, "Cannot find the cost of a pointer");
  case SEQUENCE:
    throw_(value_exception, "Cannot find the cost of a sequence");

  default:
    assert(0);
    break;
  }
  assert(0);
  return value_t();
}

value_t& value_t::add(const amount_t& amount, const amount_t * tcost)
{
  switch (type) {
  case BOOLEAN:
    throw_(value_exception, "Cannot add an amount to a boolean");
  case DATETIME:
    throw_(value_exception, "Cannot add an amount to a date/time");
  case INTEGER:
  case AMOUNT:
    if (tcost) {
      in_place_cast(BALANCE_PAIR);
      return add(amount, tcost);
    }
    else if ((type == AMOUNT &&
	      ((amount_t *) data)->commodity() != amount.commodity()) ||
	     (type != AMOUNT && amount.commodity())) {
      in_place_cast(BALANCE);
      return add(amount, tcost);
    }
    else if (type != AMOUNT) {
      in_place_cast(AMOUNT);
    }
    *((amount_t *) data) += amount;
    break;

  case BALANCE:
    if (tcost) {
      in_place_cast(BALANCE_PAIR);
      return add(amount, tcost);
    }
    *((balance_t *) data) += amount;
    break;

  case BALANCE_PAIR:
    ((balance_pair_t *) data)->add(amount, tcost);
    break;

  case STRING:
    throw_(value_exception, "Cannot add an amount to a string");
  case XML_NODE:
    throw_(value_exception, "Cannot add an amount to an XML node");
  case POINTER:
    throw_(value_exception, "Cannot add an amount to a pointer");
  case SEQUENCE:
    throw_(value_exception, "Cannot add an amount to a sequence");

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
    throw_(value_exception, "Cannot write out a sequence");

  case BALANCE:
    ((balance_t *) data)->write(out, first_width, latter_width);
    break;
  case BALANCE_PAIR:
    ((balance_pair_t *) data)->write(out, first_width, latter_width);
    break;
  }
}

std::ostream& operator<<(std::ostream& out, const value_t& val)
{
  switch (val.type) {
  case value_t::BOOLEAN:
    out << (*((bool *) val.data) ? "true" : "false");
    break;
  case value_t::INTEGER:
    out << *(long *) val.data;
    break;
  case value_t::DATETIME:
    out << *(moment_t *) val.data;
    break;
  case value_t::AMOUNT:
    out << *(amount_t *) val.data;
    break;
  case value_t::BALANCE:
    out << *(balance_t *) val.data;
    break;
  case value_t::BALANCE_PAIR:
    out << *(balance_pair_t *) val.data;
    break;
  case value_t::STRING:
    out << **(string **) val.data;
    break;
  case value_t::XML_NODE:
    if ((*(xml::node_t **) val.data)->flags & XML_NODE_IS_PARENT)
      out << '<' << (*(xml::node_t **) val.data)->name() << '>';
    else
      out << (*(xml::node_t **) val.data)->text();
    break;

  case value_t::POINTER:
    throw_(value_exception, "Cannot output a pointer value");

  case value_t::SEQUENCE: {
    out << '(';
    bool first = true;
    for (value_t::sequence_t::iterator
	   i = (*(value_t::sequence_t **) val.data)->begin();
	 i != (*(value_t::sequence_t **) val.data)->end();
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

#if 0
value_context::value_context(const value_t& _bal,
			     const string& _desc) throw()
  : error_context(_desc), bal(new value_t(_bal)) {}

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
    out << *((moment_t *) bal->data);
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
#endif

} // namespace ledger
