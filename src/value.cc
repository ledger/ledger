/*
 * Copyright (c) 2003-2009, John Wiegley.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 *
 * - Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * - Neither the name of New Artisans LLC nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "value.h"
#include "unistring.h"

namespace ledger {

intrusive_ptr<value_t::storage_t> value_t::true_value;
intrusive_ptr<value_t::storage_t> value_t::false_value;

value_t::storage_t& value_t::storage_t::operator=(const value_t::storage_t& rhs)
{
  type = rhs.type;

  switch (type) {
  case BALANCE:
    data = new balance_t(*boost::get<balance_t *>(rhs.data));
    break;
  case SEQUENCE:
    data = new sequence_t(*boost::get<sequence_t *>(rhs.data));
    break;

  default:
    data = rhs.data;
    break;
  }

  return *this;
}

void value_t::initialize()
{
  true_value	    = new storage_t;
  true_value->type  = BOOLEAN;
  true_value->data  = true;

  false_value	    = new storage_t;
  false_value->type = BOOLEAN;
  false_value->data = false;
}

void value_t::shutdown()
{
  true_value  = intrusive_ptr<storage_t>();
  false_value = intrusive_ptr<storage_t>();
}

value_t::operator bool() const
{
  switch (type()) {
  case VOID:
    return false;
  case BOOLEAN:
    return as_boolean();
  case DATETIME:
    return is_valid(as_datetime());
  case DATE:
    return is_valid(as_date());
  case INTEGER:
    return as_long();
  case AMOUNT:
    return as_amount();
  case BALANCE:
    return as_balance();
  case STRING:
    return ! as_string().empty();
  case SEQUENCE:
    if (! as_sequence().empty()) {
      foreach (const value_t& value, as_sequence()) {
	if (value)
	  return true;
      }
    }
    return false;
  case POINTER:
    return ! as_any_pointer().empty();
  default:
    break;
  }

  throw_(value_error, "Cannot determine truth of " << label());
  return false;
}

void value_t::set_type(type_t new_type)
{
  assert(new_type >= VOID && new_type <= POINTER);
  if (new_type == VOID) {
#if BOOST_VERSION >= 103700
    storage.reset();
#else
    storage = intrusive_ptr<storage_t>();
#endif
    assert(is_null());
  } else {
    if (! storage || storage->refc > 1)
      storage = new storage_t;
    else
      storage->destroy();
    storage->type = new_type;
    assert(is_type(new_type));
  }
}

bool value_t::to_boolean() const
{
  if (is_boolean()) {
    return as_boolean();
  } else {
    value_t temp(*this);
    temp.in_place_cast(BOOLEAN);
    return temp.as_boolean();
  }
}

datetime_t value_t::to_datetime() const
{
  if (is_datetime()) {
    return as_datetime();
  } else {
    value_t temp(*this);
    temp.in_place_cast(DATETIME);
    return temp.as_datetime();
  }
}

date_t value_t::to_date() const
{
  if (is_date()) {
    return as_date();
  } else {
    value_t temp(*this);
    temp.in_place_cast(DATE);
    return temp.as_date();
  }
}

long value_t::to_long() const
{
  if (is_long()) {
    return as_long();
  } else {
    value_t temp(*this);
    temp.in_place_cast(INTEGER);
    return temp.as_long();
  }
}

amount_t value_t::to_amount() const
{
  if (is_amount()) {
    return as_amount();
  } else {
    value_t temp(*this);
    temp.in_place_cast(AMOUNT);
    return temp.as_amount();
  }
}

balance_t value_t::to_balance() const
{
  if (is_balance()) {
    return as_balance();
  } else {
    value_t temp(*this);
    temp.in_place_cast(BALANCE);
    return temp.as_balance();
  }
}

string value_t::to_string() const
{
  if (is_string()) {
    return as_string();
  } else {
    value_t temp(*this);
    temp.in_place_cast(STRING);
    return temp.as_string();
  }
}

mask_t value_t::to_mask() const
{
  if (is_mask()) {
    return as_mask();
  } else {
    value_t temp(*this);
    temp.in_place_cast(MASK);
    return temp.as_mask();
  }
}

value_t::sequence_t value_t::to_sequence() const
{
  if (is_sequence()) {
    return as_sequence();
  } else {
    value_t temp(*this);
    temp.in_place_cast(SEQUENCE);
    return temp.as_sequence();
  }
}


void value_t::in_place_simplify()
{
#if defined(DEBUG_ON)
  LOGGER("amounts.values.simplify");
#endif

  if (is_realzero()) {
    DEBUG_("Zeroing type " << static_cast<int>(type()));
    set_long(0L);
    return;
  }

  if (is_balance() && as_balance().amounts.size() == 1) {
    DEBUG_("Reducing balance to amount");
    DEBUG("ledger.value.reduce", "as a balance it looks like: " << *this);
    in_place_cast(AMOUNT);
    DEBUG("ledger.value.reduce", "as an amount it looks like: " << *this);
  }

#ifdef REDUCE_TO_INTEGER	// this is off by default
  if (is_amount() && ! as_amount().has_commodity() &&
      as_amount().fits_in_long()) {
    DEBUG_("Reducing amount to integer");
    in_place_cast(INTEGER);
  }
#endif
}

value_t& value_t::operator+=(const value_t& val)
{
  if (is_string()) {
    if (val.is_string())
      as_string_lval() += val.as_string();
    else
      as_string_lval() += val.to_string();
    return *this;
  }
  else if (is_sequence()) {
    if (val.is_sequence()) {
      if (size() == val.size()) {
	sequence_t::iterator	   i = begin();
	sequence_t::const_iterator j = val.begin();

	for (; i != end(); i++, j++)
	  *i += *j;
      } else {
	throw_(value_error, "Cannot add sequences of different lengths");
      }
    } else {
      as_sequence_lval().push_back(val);
    }
    return *this;
  }

  switch (type()) {
  case DATETIME:
    switch (val.type()) {
    case INTEGER:
      as_datetime_lval() += date_duration(val.as_long());
      return *this;
    case AMOUNT:
      as_datetime_lval() += date_duration(val.as_amount().to_long());
      return *this;
    default:
      break;
    }
    break;

  case DATE:
    switch (val.type()) {
    case INTEGER:
      as_date_lval() += date_duration_t(val.as_long());
      return *this;
    case AMOUNT:
      as_date_lval() += date_duration_t(val.as_amount().to_long());
      return *this;
    default:
      break;
    }
    break;

  case INTEGER:
    switch (val.type()) {
    case INTEGER:
      as_long_lval() += val.as_long();
      return *this;
    case AMOUNT:
      if (val.as_amount().has_commodity()) {
	in_place_cast(BALANCE);
	return *this += val;
      }
      in_place_cast(AMOUNT);
      as_amount_lval() += val.as_amount();
      return *this;
    case BALANCE:
      in_place_cast(BALANCE);
      as_balance_lval() += val.as_balance();
      return *this;
    default:
      break;
    }
    break;

  case AMOUNT:
    switch (val.type()) {
    case INTEGER:
      if (as_amount().has_commodity()) {
	in_place_cast(BALANCE);
	return *this += val;
      } else {
	as_amount_lval() += val.as_long();
	return *this;
      }
      break;

    case AMOUNT:
      if (as_amount().commodity() != val.as_amount().commodity()) {
	in_place_cast(BALANCE);
	return *this += val;
      } else {
	as_amount_lval() += val.as_amount();
	return *this;
      }
      break;

    case BALANCE:
      in_place_cast(BALANCE);
      as_balance_lval() += val.as_balance();
      return *this;

    default:
      break;
    }
    break;

  case BALANCE:
    switch (val.type()) {
    case INTEGER:
      as_balance_lval() += val.to_amount();
      return *this;
    case AMOUNT:
      as_balance_lval() += val.as_amount();
      return *this;
    case BALANCE:
      as_balance_lval() += val.as_balance();
      return *this;
    default:
      break;
    }
    break;

  default:
    break;
  }

  throw_(value_error, "Cannot add " << val.label() << " to " << label());
  return *this;
}

value_t& value_t::operator-=(const value_t& val)
{
  if (is_sequence()) {
    sequence_t& seq(as_sequence_lval());

    if (val.is_sequence()) {
      if (size() == val.size()) {
	sequence_t::iterator	   i = begin();
	sequence_t::const_iterator j = val.begin();

	for (; i != end(); i++, j++)
	  *i -= *j;
      } else {
	throw_(value_error, "Cannot subtract sequences of different lengths");
      }
    } else {
      sequence_t::iterator i = std::find(seq.begin(), seq.end(), val);
      if (i != seq.end())
	seq.erase(i);
    }
    return *this;
  }

  switch (type()) {
  case DATETIME:
    switch (val.type()) {
    case INTEGER:
      as_datetime_lval() -= date_duration(val.as_long());
      return *this;
    case AMOUNT:
      as_datetime_lval() -= date_duration(val.as_amount().to_long());
      return *this;
    default:
      break;
    }
    break;

  case DATE:
    switch (val.type()) {
    case INTEGER:
      as_date_lval() -= date_duration_t(val.as_long());
      return *this;
    case AMOUNT:
      as_date_lval() -= date_duration_t(val.as_amount().to_long());
      return *this;
    default:
      break;
    }
    break;

  case INTEGER:
    switch (val.type()) {
    case INTEGER:
      as_long_lval() -= val.as_long();
      return *this;
    case AMOUNT:
      in_place_cast(AMOUNT);
      as_amount_lval() -= val.as_amount();
      in_place_simplify();
      return *this;
    case BALANCE:
      in_place_cast(BALANCE);
      as_balance_lval() -= val.as_balance();
      in_place_simplify();
      return *this;
    default:
      break;
    }
    break;

  case AMOUNT:
    switch (val.type()) {
    case INTEGER:
      if (as_amount().has_commodity()) {
	in_place_cast(BALANCE);
	*this -= val;
	in_place_simplify();
	return *this;
      } else {
	as_amount_lval() -= val.as_long();
	in_place_simplify();
	return *this;
      }
      break;

    case AMOUNT:
      if (as_amount().commodity() != val.as_amount().commodity()) {
	in_place_cast(BALANCE);
	*this -= val;
	in_place_simplify();
	return *this;
      } else {
	as_amount_lval() -= val.as_amount();
	in_place_simplify();
	return *this;
      }
      break;

    case BALANCE:
      in_place_cast(BALANCE);
      as_balance_lval() -= val.as_balance();
      in_place_simplify();
      return *this;

    default:
      break;
    }
    break;

  case BALANCE:
    switch (val.type()) {
    case INTEGER:
      as_balance_lval() -= val.to_amount();
      in_place_simplify();
      return *this;
    case AMOUNT:
      as_balance_lval() -= val.as_amount();
      in_place_simplify();
      return *this;
    case BALANCE:
      as_balance_lval() -= val.as_balance();
      in_place_simplify();
      return *this;
    default:
      break;
    }
    break;

  default:
    break;
  }

  throw_(value_error, "Cannot subtract " << val.label() << " from " << label());

  return *this;
}

value_t& value_t::operator*=(const value_t& val)
{
  if (is_string()) {
    string temp;
    long count = val.to_long();
    for (long i = 0; i < count; i++)
      temp += as_string();
    set_string(temp);
    return *this;
  }
  else if (is_sequence()) {
    value_t temp;
    long count = val.to_long();
    for (long i = 0; i < count; i++)
      temp += as_sequence();
    return *this = temp;
  }

  switch (type()) {
  case INTEGER:
    switch (val.type()) {
    case INTEGER:
      as_long_lval() *= val.as_long();
      return *this;
    case AMOUNT:
      set_amount(val.as_amount() * as_long());
      return *this;
    default:
      break;
    }
    break;

  case AMOUNT:
    switch (val.type()) {
    case INTEGER:
      as_amount_lval() *= val.as_long();
      return *this;
    case AMOUNT:
      if (as_amount().commodity() == val.as_amount().commodity() ||
	  ! val.as_amount().has_commodity()) {
	as_amount_lval() *= val.as_amount();
	return *this;
      }
      break;
    default:
      break;
    }
    break;

  case BALANCE:
    switch (val.type()) {
    case INTEGER:
      as_balance_lval() *= val.as_long();
      return *this;
    case AMOUNT:
      if (! val.as_amount().has_commodity()) {
	as_balance_lval() *= val.as_amount();
	return *this;
      }
      break;
    default:
      break;
    }
    break;

  default:
    break;
  }

  throw_(value_error, "Cannot multiply " << label() << " with " << val.label());

  return *this;
}

value_t& value_t::operator/=(const value_t& val)
{
  switch (type()) {
  case INTEGER:
    switch (val.type()) {
    case INTEGER:
      as_long_lval() /= val.as_long();
      return *this;
    case AMOUNT:
      set_amount(val.as_amount() / as_long());
      return *this;
    default:
      break;
    }
    break;

  case AMOUNT:
    switch (val.type()) {
    case INTEGER:
      as_amount_lval() /= val.as_long();
      return *this;

    case AMOUNT:
      as_amount_lval() /= val.as_amount();
      return *this;
    default:
      break;
    }
    break;

  case BALANCE:
    switch (val.type()) {
    case INTEGER:
      as_balance_lval() /= val.as_long();
      return *this;
    case AMOUNT:
      if (! val.as_amount().has_commodity()) {
	as_balance_lval() /= val.as_amount();
	return *this;
      }
      break;
    default:
      break;
    }
    break;

  default:
    break;
  }

  throw_(value_error, "Cannot divide " << label() << " by " << val.label());

  return *this;
}


bool value_t::is_equal_to(const value_t& val) const
{
  switch (type()) {
  case VOID:
    return val.type() == VOID;
    
  case BOOLEAN:
    if (val.is_boolean())
      return as_boolean() == val.as_boolean();
    break;

  case DATETIME:
    if (val.is_datetime())
      return as_datetime() == val.as_datetime();
    break;

  case DATE:
    if (val.is_date())
      return as_date() == val.as_date();
    break;

  case INTEGER:
    switch (val.type()) {
    case INTEGER:
      return as_long() == val.as_long();
    case AMOUNT:
      return val.as_amount() == to_amount();
    case BALANCE:
      return val.as_balance() == to_amount();
    default:
      break;
    }
    break;

  case AMOUNT:
    switch (val.type()) {
    case INTEGER:
      return as_amount() == val.as_long();
    case AMOUNT:
      return as_amount() == val.as_amount();
    case BALANCE:
      return val.as_balance() == as_amount();
    default:
      break;
    }
    break;

  case BALANCE:
    switch (val.type()) {
    case INTEGER:
      return as_balance() == val.to_amount();
    case AMOUNT:
      return as_balance() == val.as_amount();
    case BALANCE:
      return as_balance() == val.as_balance();
    default:
      break;
    }
    break;

  case STRING:
    if (val.is_string())
      return as_string() == val.as_string();
    break;

  case MASK:
    if (val.is_mask())
      return as_mask() == val.as_mask();
    break;

  case SEQUENCE:
    if (val.is_sequence())
      return as_sequence() == val.as_sequence();
    break;

  default:
    break;
  }

  throw_(value_error, "Cannot compare " << label() << " to " << val.label());

  return *this;
}

bool value_t::is_less_than(const value_t& val) const
{
  switch (type()) {
  case DATETIME:
    if (val.is_datetime())
      return as_datetime() < val.as_datetime();
    break;

  case DATE:
    if (val.is_date())
      return as_date() < val.as_date();
    break;

  case INTEGER:
    switch (val.type()) {
    case INTEGER:
      return as_long() < val.as_long();
    case AMOUNT:
      return val.as_amount() >= as_long();
    default:
      break;
    }
    break;

  case AMOUNT:
    switch (val.type()) {
    case INTEGER:
      return as_amount() < val.as_long();
    case AMOUNT:
      try {
	return as_amount() < val.as_amount();
      }
      catch (const amount_error&) {
	return compare_amount_commodities()(&as_amount(), &val.as_amount());
      }
    default:
      break;
    }
    break;

  case BALANCE:
    switch (val.type()) {
    case INTEGER:
    case AMOUNT: {
      if (val.is_nonzero())
	break;

      bool no_amounts = true;
      foreach (const balance_t::amounts_map::value_type& pair,
	       as_balance().amounts) {
	if (pair.second >= 0L)
	  return false;
	no_amounts = false;
      }
      return ! no_amounts;
    }
    default:
      break;
    }
    break;

  case STRING:
    if (val.is_string())
      return as_string() < val.as_string();
    break;

  case SEQUENCE:
    switch (val.type()) {
    case INTEGER:
    case AMOUNT: {
      if (val.is_nonzero())
	break;

      bool no_amounts = true;
      foreach (const value_t& value, as_sequence()) {
	if (value >= 0L)
	  return false;
	no_amounts = false;
      }
      return ! no_amounts;
    }
    default:
      break;
    }
    break;

  default:
    break;
  }

  throw_(value_error, "Cannot compare " << label() << " to " << val.label());

  return *this;
}

bool value_t::is_greater_than(const value_t& val) const
{
  switch (type()) {
  case DATETIME:
    if (val.is_datetime())
      return as_datetime() > val.as_datetime();
    break;

  case DATE:
    if (val.is_date())
      return as_date() > val.as_date();
    break;

  case INTEGER:
    switch (val.type()) {
    case INTEGER:
      return as_long() > val.as_long();
    case AMOUNT:
      return val.as_amount() > as_long();
    default:
      break;
    }
    break;

  case AMOUNT:
    switch (val.type()) {
    case INTEGER:
      return as_amount() > val.as_long();
    case AMOUNT:
      return as_amount() > val.as_amount();
    default:
      break;
    }
    break;

  case BALANCE:
    switch (val.type()) {
    case INTEGER:
    case AMOUNT: {
      if (val.is_nonzero())
	break;

      bool no_amounts = true;
      foreach (const balance_t::amounts_map::value_type& pair,
	       as_balance().amounts) {
	if (pair.second <= 0L)
	  return false;
	no_amounts = false;
      }
      return ! no_amounts;
    }
    default:
      break;
    }
    break;

  case STRING:
    if (val.is_string())
      return as_string() > val.as_string();
    break;

  case SEQUENCE:
    switch (val.type()) {
    case INTEGER:
    case AMOUNT: {
      if (val.is_nonzero())
	break;

      bool no_amounts = true;
      foreach (const value_t& value, as_sequence()) {
	if (value <= 0L)
	  return false;
	no_amounts = false;
      }
      return ! no_amounts;
    }
    default:
      break;
    }
    break;

  default:
    break;
  }

  throw_(value_error, "Cannot compare " << label() << " to " << val.label());

  return *this;
}

void value_t::in_place_cast(type_t cast_type)
{
  if (type() == cast_type)
    return;

  if (cast_type == BOOLEAN) {
    set_boolean(bool(*this));
    return;
  }
  else if (cast_type == SEQUENCE) {
    sequence_t temp;
    if (! is_null())
      temp.push_back(*this);
    set_sequence(temp);
    return;
  }

  switch (type()) {
  case BOOLEAN:
    switch (cast_type) {
    case AMOUNT:
      set_amount(as_boolean() ? 1L : 0L);
      return;
    case STRING:
      set_string(as_boolean() ? "true" : "false");
      return;
    default:
      break;
    }
    break;

  case DATE:
    switch (cast_type) {
    case DATETIME:
      set_datetime(datetime_t(as_date(), time_duration(0, 0, 0, 0)));
      return;
    case STRING:
      set_string(format_date(as_date(), string("%Y-%m-%d")));
      return;
    default:
      break;
    }
    break;
  case DATETIME:
    switch (cast_type) {
    case DATE:
      set_date(as_datetime().date());
      return;
    case STRING:
      set_string(format_datetime(as_datetime(), string("%Y-%m-%d %H:%M:%S")));
      return;
    default:
      break;
    }
    break;

  case INTEGER:
    switch (cast_type) {
    case AMOUNT:
      set_amount(as_long());
      return;
    case BALANCE:
      set_balance(to_amount());
      return;
    case STRING:
      set_string(lexical_cast<string>(as_long()));
      return;
    default:
      break;
    }
    break;

  case AMOUNT: {
    const amount_t& amt(as_amount());
    switch (cast_type) {
    case INTEGER:
      if (amt.is_null())
	set_long(0L);
      else
	set_long(as_amount().to_long());
      return;
    case BALANCE:
      if (amt.is_null())
	set_balance(balance_t());
      else
	set_balance(as_amount());
      return;
    case STRING:
      if (amt.is_null())
	set_string("");
      else
	set_string(as_amount().to_string());
      return;
    default:
      break;
    }
    break;
  }

  case BALANCE:
    switch (cast_type) {
    case AMOUNT: {
      const balance_t& temp(as_balance());
      if (temp.amounts.size() == 1) {
	// Because we are changing the current balance value to an amount
	// value, and because set_amount takes a reference (and that memory is
	// about to be repurposed), we must pass in a copy.
	set_amount(amount_t((*temp.amounts.begin()).second));
	return;
      }
      else if (temp.amounts.size() == 0) {
	set_amount(0L);
	return;
      }
      else {
	throw_(value_error, "Cannot convert " << label() <<
	       " with multiple commodities to " << label(cast_type));
      }
      break;
    }
    default:
      break;
    }
    break;

  case STRING:
    switch (cast_type) {
    case INTEGER: {
      if (all(as_string(), is_digit())) {
	set_long(lexical_cast<long>(as_string()));
	return;
      } else {
	throw_(value_error,
	       "Cannot convert string '" << *this << "' to an integer");
      }
      break;
    }
    case AMOUNT:
      set_amount(amount_t(as_string()));
      return;
    case MASK:
      set_mask(as_string());
      return;
    default:
      break;
    }
    break;

  default:
    break;
  }

  throw_(value_error,
	 "Cannot convert " << label() << " to " << label(cast_type));
}

void value_t::in_place_negate()
{
  switch (type()) {
  case BOOLEAN:
    set_boolean(! as_boolean());
    return;
  case INTEGER:
  case DATETIME:
    set_long(- as_long());
    return;
  case DATE:
    set_long(- as_long());
    return;
  case AMOUNT:
    as_amount_lval().in_place_negate();
    return;
  case BALANCE:
    as_balance_lval().in_place_negate();
    return;
  default:
    break;
  }

  throw_(value_error, "Cannot negate " << label());
}

void value_t::in_place_not()
{
  switch (type()) {
  case BOOLEAN:
    set_boolean(! as_boolean());
    return;
  case INTEGER:
  case DATETIME:
    set_boolean(! as_long());
    return;
  case DATE:
    set_boolean(! as_long());
    return;
  case AMOUNT:
    set_boolean(! as_amount());
    return;
  case BALANCE:
    set_boolean(! as_balance());
    return;
  case STRING:
    set_boolean(as_string().empty());
    return;
  default:
    break;
  }

  throw_(value_error, "Cannot not " << label());
}

bool value_t::is_realzero() const
{
  switch (type()) {
  case BOOLEAN:
    return ! as_boolean();
  case INTEGER:
    return as_long() == 0;
  case DATETIME:
    return ! is_valid(as_datetime());
  case DATE:
    return ! is_valid(as_date());
  case AMOUNT:
    return as_amount().is_realzero();
  case BALANCE:
    return as_balance().is_realzero();
  case STRING:
    return as_string().empty();
  case SEQUENCE:
    return as_sequence().empty();

  case POINTER:
    return as_any_pointer().empty();

  default:
    throw_(value_error, "Cannot determine if " << label() << " is really zero");
  }
  return false;
}

bool value_t::is_zero() const
{
  switch (type()) {
  case BOOLEAN:
    return ! as_boolean();
  case INTEGER:
    return as_long() == 0;
  case DATETIME:
    return ! is_valid(as_datetime());
  case DATE:
    return ! is_valid(as_date());
  case AMOUNT:
    return as_amount().is_zero();
  case BALANCE:
    return as_balance().is_zero();
  case STRING:
    return as_string().empty();
  case SEQUENCE:
    return as_sequence().empty();

  case POINTER:
    return as_any_pointer().empty();

  default:
    throw_(value_error, "Cannot determine if " << label() << " is zero");
  }
  return false;
}

value_t value_t::value(const bool		     primary_only,
		       const optional<datetime_t>&   moment,
		       const optional<commodity_t&>& in_terms_of) const
{
  switch (type()) {
  case INTEGER:
    return NULL_VALUE;

  case AMOUNT:
    if (optional<amount_t> val =
	as_amount().value(primary_only, moment, in_terms_of))
      return *val;
    return NULL_VALUE;

  case BALANCE:
    if (optional<balance_t> bal =
	as_balance().value(primary_only, moment, in_terms_of))
      return *bal;
    return NULL_VALUE;

  default:
    break;
  }

  throw_(value_error, "Cannot find the value of " << label());
  return NULL_VALUE;
}

void value_t::in_place_reduce()
{
  switch (type()) {
  case AMOUNT:
    as_amount_lval().in_place_reduce();
    return;
  case BALANCE:
    as_balance_lval().in_place_reduce();
    return;
  default:
    return;
  }

  //throw_(value_error, "Cannot reduce " << label());
}

void value_t::in_place_unreduce()
{
  switch (type()) {
  case AMOUNT:
    as_amount_lval().in_place_unreduce();
    return;
  case BALANCE:
    as_balance_lval().in_place_unreduce();
    return;
  default:
    return;
  }

  //throw_(value_error, "Cannot reduce " << label());
}

value_t value_t::abs() const
{
  switch (type()) {
  case INTEGER: {
    long val = as_long();
    if (val < 0)
      return - val;
    return val;
  }
  case AMOUNT:
    return as_amount().abs();
  case BALANCE:
    return as_balance().abs();
  default:
    break;
  }

  throw_(value_error, "Cannot abs " << label());
  return NULL_VALUE;
}

value_t value_t::rounded() const
{
  switch (type()) {
  case INTEGER:
    return *this;
  case AMOUNT:
    return as_amount().rounded();
  case BALANCE:
    return as_balance().rounded();
  default:
    break;
  }

  throw_(value_error, "Cannot set rounding for " << label());
  return NULL_VALUE;
}

value_t value_t::unrounded() const
{
  switch (type()) {
  case INTEGER:
    return *this;
  case AMOUNT:
    return as_amount().unrounded();
  case BALANCE:
    return as_balance().unrounded();
  default:
    break;
  }

  throw_(value_error, "Cannot unround " << label());
  return NULL_VALUE;
}

void value_t::annotate(const annotation_t& details)
{
  if (is_amount())
    as_amount_lval().annotate(details);
  else
    throw_(value_error, "Cannot annotate " << label());
}

bool value_t::is_annotated() const
{
  if (is_amount())
    return as_amount().is_annotated();
  else
    throw_(value_error,
	   "Cannot determine whether " << label() << " is annotated");
  return false;
}

annotation_t& value_t::annotation()
{
  if (is_amount())
    return as_amount_lval().annotation();
  else {
    throw_(value_error, "Cannot request annotation of " << label());
    return as_amount_lval().annotation(); // quiet g++ warning
  }
}

value_t value_t::strip_annotations(const keep_details_t& what_to_keep) const
{
  if (what_to_keep.keep_all())
    return *this;

  switch (type()) {
  case VOID:
  case BOOLEAN:
  case INTEGER:
  case DATETIME:
  case DATE:
  case STRING:
  case MASK:
  case POINTER:
    return *this;

  case SEQUENCE: {
    sequence_t temp;
    foreach (const value_t& value, as_sequence())
      temp.push_back(value.strip_annotations(what_to_keep));
    return temp;
  }

  case AMOUNT:
    return as_amount().strip_annotations(what_to_keep);
  case BALANCE:
    return as_balance().strip_annotations(what_to_keep);

  default:
    assert(false);
    break;
  }
  assert(false);
  return NULL_VALUE;
}

void value_t::print(std::ostream&           out,
		    const int	            first_width,
		    const int               latter_width,
		    const bool              right_justify,
		    const optional<string>& date_format) const
{
  if (first_width > 0 &&
      ! is_amount() && ! is_balance() && ! is_string()) {
    out.width(first_width);

    if (right_justify)
      out << std::right;
    else
      out << std::left;
  }

  switch (type()) {
  case VOID:
    out << "";
    break;

  case BOOLEAN:
    out << (as_boolean() ? "1" : "0");
    break;

  case DATETIME:
    if (date_format)
      out << format_datetime(as_datetime(), *date_format);
    else
      out << format_datetime(as_datetime());
    break;

  case DATE:
    if (date_format)
      out << format_date(as_date(), *date_format);
    else
      out << format_date(as_date());
    break;

  case INTEGER:
    out << std::right << as_long();
    break;

  case AMOUNT: {
    if (as_amount().is_zero()) {
      out.width(first_width);
      out << (right_justify ? std::right : std::left) << 0;
    } else {
      std::ostringstream buf;
      buf << as_amount();
      justify(out, buf.str(), first_width, right_justify);
    }
    break;
  }

  case STRING:
    justify(out, as_string(), first_width, right_justify);
    break;

  case MASK:
    out << '/' << as_mask() << '/';
    break;

  case SEQUENCE: {
    out << '(';
    bool first = true;
    foreach (const value_t& value, as_sequence()) {
      if (first)
	first = false;
      else
	out << ", ";

      value.print(out, first_width, latter_width, right_justify,
		  date_format);
    }
    out << ')';
    break;
  }

  case BALANCE:
    as_balance().print(out, first_width, latter_width, right_justify);
    break;

  case POINTER:
    out << "<POINTER>";
    break;

  default:
    throw_(value_error, "Cannot print " << label());
  }
}

void value_t::dump(std::ostream& out, const bool relaxed) const
{
  switch (type()) {
  case VOID:
    out << "<null>";
    break;

  case BOOLEAN:
    if (as_boolean())
      out << "true";
    else
      out << "false";
    break;

  case DATETIME:
    out << '[' << format_datetime(as_datetime()) << ']';
    break;
  case DATE:
    out << '[' << format_date(as_date()) << ']';
    break;

  case INTEGER:
    out << as_long();
    break;

  case AMOUNT:
    if (! relaxed)
      out << '{';
    out << as_amount();
    if (! relaxed)
      out << '}';
    break;

  case BALANCE:
    out << as_balance();
    break;

  case STRING:
    out << '"' << as_string() << '"';
    break;

  case MASK:
    out << '/' << as_mask() << '/';
    break;

  case POINTER:
    out << boost::unsafe_any_cast<const void *>(&as_any_pointer());
    break;

  case SEQUENCE: {
    out << '(';
    bool first = true;
    foreach (const value_t& value, as_sequence()) {
      if (first)
	first = false;
      else
	out << ", ";

      value.dump(out, relaxed);
    }
    out << ')';
    break;
  }

  default:
    assert(false);
    break;
  }
}

bool value_t::valid() const
{
  switch (type()) {
  case AMOUNT:
    return as_amount().valid();
  case BALANCE:
    return as_balance().valid();
  default:
    break;
  }
  return true;
}

bool sort_value_is_less_than(const std::list<sort_value_t>& left_values,
			     const std::list<sort_value_t>& right_values)
{
  std::list<sort_value_t>::const_iterator left_iter  = left_values.begin();
  std::list<sort_value_t>::const_iterator right_iter = right_values.begin();

  while (left_iter != left_values.end() && right_iter != right_values.end()) {
    // Don't even try to sort balance values
    if (! (*left_iter).value.is_balance() &&
	! (*right_iter).value.is_balance()) {
      DEBUG("value.sort",
	    " Comparing " << (*left_iter).value << " < " << (*right_iter).value);
      if ((*left_iter).value < (*right_iter).value) {
	DEBUG("value.sort", "  is less");
	return ! (*left_iter).inverted;
      }
      else if ((*left_iter).value > (*right_iter).value) {
	DEBUG("value.sort", "  is greater");
	return (*left_iter).inverted;
      }
    }
    left_iter++; right_iter++;
  }

  assert(left_iter == left_values.end());
  assert(right_iter == right_values.end());

  return false;
}

} // namespace ledger
