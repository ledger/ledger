/*
 * Copyright (c) 2003-2008, John Wiegley.  All rights reserved.
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
#include "binary.h"

namespace ledger {

intrusive_ptr<value_t::storage_t> value_t::true_value;
intrusive_ptr<value_t::storage_t> value_t::false_value;

value_t::storage_t& value_t::storage_t::operator=(const value_t::storage_t& rhs)
{
  type = rhs.type;

  switch (type) {
  case DATETIME:
    new(reinterpret_cast<datetime_t *>(data))
      datetime_t(*reinterpret_cast<datetime_t *>
		 (const_cast<char *>(rhs.data)));
    break;

  case DATE:
    new(reinterpret_cast<date_t *>(data))
      date_t(*reinterpret_cast<date_t *>(const_cast<char *>(rhs.data)));
    break;

  case AMOUNT:
    new(reinterpret_cast<amount_t *>(data))
      amount_t(*reinterpret_cast<amount_t *>
	       (const_cast<char *>(rhs.data)));
    break;

  case BALANCE:
    *reinterpret_cast<balance_t **>(data) =
      new balance_t(**reinterpret_cast<balance_t **>
		    (const_cast<char *>(rhs.data)));
    break;

  case BALANCE_PAIR:
    *reinterpret_cast<balance_pair_t **>(data) =
      new balance_pair_t(**reinterpret_cast<balance_pair_t **>
			 (const_cast<char *>(rhs.data)));
    break;

  case STRING:
    new(reinterpret_cast<string *>(data))
      string(*reinterpret_cast<string *>(const_cast<char *>(rhs.data)));
    break;

  case SEQUENCE:
    *reinterpret_cast<sequence_t **>(data) =
      new sequence_t(**reinterpret_cast<sequence_t **>
		     (const_cast<char *>(rhs.data)));
    break;

  default:
    // The rest are fundamental types, which can be copied using
    // std::memcpy
    std::memcpy(data, rhs.data, sizeof(data));
    break;
  }

  return *this;
}

void value_t::storage_t::destroy()
{
  switch (type) {
  case AMOUNT:
    reinterpret_cast<amount_t *>(data)->~amount_t();
    break;
  case BALANCE:
    checked_delete(*reinterpret_cast<balance_t **>(data));
    break;
  case BALANCE_PAIR:
    checked_delete(*reinterpret_cast<balance_pair_t **>(data));
    break;
  case STRING:
    reinterpret_cast<string *>(data)->~string();
    break;
  case SEQUENCE:
    checked_delete(*reinterpret_cast<sequence_t **>(data));
    break;
  case POINTER:
    reinterpret_cast<boost::any *>(data)->~any();
    break;

  default:
    break;
  }
  type = VOID;
}

void value_t::initialize()
{
#if defined(DEBUG_ON)
  LOGGER("value.initialize");
#endif

  true_value = new storage_t;
  true_value->type = BOOLEAN;
  *reinterpret_cast<bool *>(true_value->data) = true;

  false_value = new storage_t;
  false_value->type = BOOLEAN;
  *reinterpret_cast<bool *>(false_value->data) = false;

#if 0
  BOOST_STATIC_ASSERT(sizeof(amount_t) >= sizeof(bool));
  BOOST_STATIC_ASSERT(sizeof(amount_t) >= sizeof(datetime_t));
  BOOST_STATIC_ASSERT(sizeof(amount_t) >= sizeof(date_t));
  BOOST_STATIC_ASSERT(sizeof(amount_t) >= sizeof(long));
  BOOST_STATIC_ASSERT(sizeof(amount_t) >= sizeof(amount_t));
  BOOST_STATIC_ASSERT(sizeof(amount_t) >= sizeof(balance_t *));
  BOOST_STATIC_ASSERT(sizeof(amount_t) >= sizeof(balance_pair_t *));
  BOOST_STATIC_ASSERT(sizeof(amount_t) >= sizeof(string));
  BOOST_STATIC_ASSERT(sizeof(amount_t) >= sizeof(sequence_t *));
  BOOST_STATIC_ASSERT(sizeof(amount_t) >= sizeof(boost::any));
#endif

  DEBUG_(std::setw(3) << std::right << sizeof(bool)
	 << "  sizeof(bool)");
  DEBUG_(std::setw(3) << std::right << sizeof(datetime_t)
	 << "  sizeof(datetime_t)");
  DEBUG_(std::setw(3) << std::right << sizeof(date_t)
	 << "  sizeof(date_t)");
  DEBUG_(std::setw(3) << std::right << sizeof(long)
	 << "  sizeof(long)");
  DEBUG_(std::setw(3) << std::right << sizeof(amount_t)
	 << "  sizeof(amount_t)");
  DEBUG_(std::setw(3) << std::right << sizeof(balance_t *)
	 << "  sizeof(balance_t *)");
  DEBUG_(std::setw(3) << std::right << sizeof(balance_pair_t *)
	 << "  sizeof(balance_pair_t *)");
  DEBUG_(std::setw(3) << std::right << sizeof(string)
	 << "  sizeof(string)");
  DEBUG_(std::setw(3) << std::right << sizeof(sequence_t *)
	 << "  sizeof(sequence_t *)");
  DEBUG_(std::setw(3) << std::right << sizeof(boost::any)
	 << "  sizeof(boost::any)");
}

void value_t::shutdown()
{
  true_value  = intrusive_ptr<storage_t>();
  false_value = intrusive_ptr<storage_t>();
}

void value_t::_dup()
{
  assert(storage);
  if (storage->refc > 1)
    storage = new storage_t(*storage.get());
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
  case BALANCE_PAIR:
    return as_balance_pair();
  case STRING:
    return ! as_string().empty();
  case SEQUENCE:
    return ! as_sequence().empty();
  case POINTER:
    return ! as_any_pointer().empty();
  default:
    assert(false);
    break;
  }
  assert(false);
  return 0;
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

balance_pair_t value_t::to_balance_pair() const
{
  if (is_balance_pair()) {
    return as_balance_pair();
  } else {
    value_t temp(*this);
    temp.in_place_cast(BALANCE_PAIR);
    return temp.as_balance_pair();
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

  if (is_balance_pair() &&
      (! as_balance_pair().cost || as_balance_pair().cost->is_realzero())) {
    DEBUG_("Reducing balance pair to balance");
    in_place_cast(BALANCE);
  }

  if (is_balance() && as_balance().amounts.size() == 1) {
    DEBUG_("Reducing balance to amount");
    DEBUG("ledger.value.reduce", "as a balance it looks like: " << *this);
    in_place_cast(AMOUNT);
    DEBUG("ledger.value.reduce", "as an amount it looks like: " << *this);
  }

#if 0
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
      sequence_t& seq(as_sequence_lval());
      seq.insert(seq.end(), val.as_sequence().begin(),
		 val.as_sequence().end());
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
      in_place_cast(AMOUNT);
      as_amount_lval() += val.as_amount();
      return *this;
    case BALANCE:
      in_place_cast(BALANCE);
      as_balance_lval() += val.as_balance();
      return *this;
    case BALANCE_PAIR:
      in_place_cast(BALANCE_PAIR);
      as_balance_pair_lval() += val.as_balance_pair();
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

    case BALANCE_PAIR:
      in_place_cast(BALANCE_PAIR);
      as_balance_pair_lval() += val.as_balance_pair();
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
    case BALANCE_PAIR:
      in_place_cast(BALANCE_PAIR);
      as_balance_pair_lval() += val.as_balance_pair();
      return *this;
    default:
      break;
    }
    break;

  case BALANCE_PAIR:
    switch (val.type()) {
    case INTEGER:
      as_balance_pair_lval() += val.to_amount();
      return *this;
    case AMOUNT:
      as_balance_pair_lval() += val.as_amount();
      return *this;
    case BALANCE:
      as_balance_pair_lval() += val.as_balance();
      return *this;
    case BALANCE_PAIR:
      as_balance_pair_lval() += val.as_balance_pair();
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
      foreach (const value_t& v, val.as_sequence()) {
	sequence_t::iterator j = std::find(seq.begin(), seq.end(), v);
	if (j != seq.end())
	  seq.erase(j);
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
    case BALANCE_PAIR:
      in_place_cast(BALANCE_PAIR);
      as_balance_pair_lval() -= val.as_balance_pair();
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

    case BALANCE_PAIR:
      in_place_cast(BALANCE_PAIR);
      as_balance_pair_lval() -= val.as_balance_pair();
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
    case BALANCE_PAIR:
      in_place_cast(BALANCE_PAIR);
      as_balance_pair_lval() -= val.as_balance_pair();
      in_place_simplify();
      return *this;
    default:
      break;
    }
    break;

  case BALANCE_PAIR:
    switch (val.type()) {
    case INTEGER:
      as_balance_pair_lval() -= val.to_amount();
      in_place_simplify();
      return *this;
    case AMOUNT:
      as_balance_pair_lval() -= val.as_amount();
      in_place_simplify();
      return *this;
    case BALANCE:
      as_balance_pair_lval() -= val.as_balance();
      in_place_simplify();
      return *this;
    case BALANCE_PAIR:
      as_balance_pair_lval() -= val.as_balance_pair();
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

  case BALANCE_PAIR:
    switch (val.type()) {
    case INTEGER:
      as_balance_pair_lval() *= val.as_long();
      return *this;
    case AMOUNT:
      if (! val.as_amount().has_commodity()) {
	as_balance_pair_lval() *= val.as_amount();
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
      if (as_amount().commodity() == val.as_amount().commodity() ||
	  ! val.as_amount().has_commodity()) {
	as_amount_lval() /= val.as_amount();
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

  case BALANCE_PAIR:
    switch (val.type()) {
    case INTEGER:
      as_balance_pair_lval() /= val.as_long();
      return *this;
    case AMOUNT:
      if (! val.as_amount().has_commodity()) {
	as_balance_pair_lval() /= val.as_amount();
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


bool value_t::operator==(const value_t& val) const
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
    case BALANCE_PAIR:
      return val.as_balance_pair() == to_amount();
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
    case BALANCE_PAIR:
      return val.as_balance_pair() == as_amount();
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
    case BALANCE_PAIR:
      return val.as_balance_pair() == as_balance();
    default:
      break;
    }
    break;

  case BALANCE_PAIR:
    switch (val.type()) {
    case INTEGER:
      return as_balance_pair() == val.to_amount();
    case AMOUNT:
      return as_balance_pair() == val.as_amount();
    case BALANCE:
      return as_balance_pair() == val.as_balance();
    case BALANCE_PAIR:
      return as_balance_pair() == val.as_balance_pair();
    default:
      break;
    }
    break;

  case STRING:
    if (val.is_string())
      return as_string() == val.as_string();
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

bool value_t::operator<(const value_t& val) const
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
      return val.as_amount() < as_long();
    default:
      break;
    }
    break;

  case AMOUNT:
    switch (val.type()) {
    case INTEGER:
      return as_amount() < val.as_long();
    case AMOUNT:
      return as_amount() < val.as_amount();
    default:
      break;
    }
    break;

  case STRING:
    if (val.is_string())
      return as_string() < val.as_string();
    break;

  default:
    break;
  }

  throw_(value_error, "Cannot compare " << label() << " to " << val.label());

  return *this;
}

#if 0
bool value_t::operator>(const value_t& val) const
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

  case STRING:
    if (val.is_string())
      return as_string() > val.as_string();
    break;

  default:
    break;
  }

  throw_(value_error, "Cannot compare " << label() << " to " << val.label());

  return *this;
}
#endif

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
    case STRING:
      set_string(as_boolean() ? "true" : "false");
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
    case BALANCE_PAIR:
      set_balance_pair(to_amount());
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
	set_balance(as_amount()); // creates temporary
      return;
    case BALANCE_PAIR:
      if (amt.is_null())
	set_balance_pair(balance_pair_t());
      else
	set_balance_pair(as_amount()); // creates temporary
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
    case BALANCE_PAIR:
      set_balance_pair(as_balance());
      return;
    default:
      break;
    }
    break;

  case BALANCE_PAIR:
    switch (cast_type) {
    case AMOUNT: {
      const balance_t& temp(as_balance_pair().quantity());
      if (temp.amounts.size() == 1) {
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
    case BALANCE:
      // A temporary is required, becaues set_balance is going to wipe us out
      // before assigned the value passed in.
      set_balance(balance_t(as_balance_pair().quantity()));
      return;
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
  case BALANCE_PAIR:
    as_balance_pair_lval().in_place_negate();
    return;
  default:
    break;
  }

  throw_(value_error, "Cannot negate " << label());
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
  case BALANCE_PAIR:
    return as_balance_pair().is_realzero();
  case STRING:
    return as_string().empty();
  case SEQUENCE:
    return as_sequence().empty();

  case POINTER:
    return as_any_pointer().empty();

  default:
    assert(false);
    break;
  }
  assert(false);
  return true;
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
  case BALANCE_PAIR:
    return as_balance_pair().is_zero();
  case STRING:
    return as_string().empty();
  case SEQUENCE:
    return as_sequence().empty();

  case POINTER:
    return as_any_pointer().empty();

  default:
    assert(false);
    break;
  }
  assert(false);
  return true;
}

value_t value_t::value(const optional<datetime_t>& moment) const
{
  switch (type()) {
  case INTEGER:
    return *this;

  case AMOUNT: {
    if (optional<amount_t> val = as_amount().value(moment))
      return *val;
    return false;
  }
  case BALANCE: {
    if (optional<balance_t> bal = as_balance().value(moment))
      return *bal;
    return false;
  }
  case BALANCE_PAIR: {
    if (optional<balance_t> bal_pair =
	as_balance_pair().quantity().value(moment))
      return *bal_pair;
    return false;
  }

  default:
    break;
  }

  throw_(value_error, "Cannot find the value of " << label());
  return NULL_VALUE;
}

void value_t::in_place_reduce()
{
  switch (type()) {
  case INTEGER:
    return;
  case AMOUNT:
    as_amount_lval().in_place_reduce();
    return;
  case BALANCE:
    as_balance_lval().in_place_reduce();
    return;
  case BALANCE_PAIR:
    as_balance_pair_lval().in_place_reduce();
    return;
  default:
    break;
  }

  throw_(value_error, "Cannot reduce " << label());
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
  case BALANCE_PAIR:
    return as_balance_pair().abs();
  default:
    break;
  }

  throw_(value_error, "Cannot abs " << label());
  return NULL_VALUE;
}

value_t value_t::round() const
{
  switch (type()) {
  case INTEGER:
    return *this;
  case AMOUNT:
    return as_amount().round();
  case BALANCE:
    return as_balance().round();
  case BALANCE_PAIR:
    return as_balance_pair().round();
  default:
    break;
  }

  throw_(value_error, "Cannot round " << label());
  return NULL_VALUE;
}

value_t value_t::unround() const
{
  switch (type()) {
  case INTEGER:
    return *this;
  case AMOUNT:
    return as_amount().unround();
  default:
    break;
  }

  throw_(value_error, "Cannot unround " << label());
  return NULL_VALUE;
}

#if 0
value_t value_t::annotated_price() const
{
  switch (type()) {
  case AMOUNT: {
    optional<amount_t> temp = as_amount().annotation_details().price;
    if (! temp)
      return false;
    return *temp;
  }

  default:
    break;
  }

  throw_(value_error, "Cannot find the annotated price of " << label());
  return NULL_VALUE;
}

value_t value_t::annotated_date() const
{
  switch (type()) {
  case DATETIME:
    return *this;
  case DATE:
    return *this;

  case AMOUNT: {
    optional<datetime_t> temp = as_amount().annotation_details().date;
    if (! temp)
      return false;
    return *temp;
  }

  default:
    break;
  }

  throw_(value_error, "Cannot find the annotated date of " << label());
  return NULL_VALUE;
}

value_t value_t::annotated_tag() const
{
  switch (type()) {
  case AMOUNT: {
    optional<string> temp = as_amount().annotation_details().tag;
    if (! temp)
      return false;
    return string_value(*temp);
  }

  case STRING:
    return *this;

  default:
    break;
  }

  throw_(value_error, "Cannot find the annotated tag of " << label());
  return NULL_VALUE;
}
#endif

value_t value_t::strip_annotations(const bool keep_price,
				   const bool keep_date,
				   const bool keep_tag) const
{
  switch (type()) {
  case VOID:
  case BOOLEAN:
  case INTEGER:
  case DATETIME:
  case DATE:
  case STRING:
  case POINTER:
    return *this;

  case SEQUENCE: {
    sequence_t temp;
    foreach (const value_t& value, as_sequence())
      temp.push_back(value.strip_annotations(keep_price, keep_date, keep_tag));
    return temp;
  }

  case AMOUNT:
    return as_amount().strip_annotations(keep_price, keep_date, keep_tag);
  case BALANCE:
    return as_balance().strip_annotations(keep_price, keep_date, keep_tag);
  case BALANCE_PAIR:
    return as_balance_pair().quantity().strip_annotations(keep_price, keep_date,
							  keep_tag);

  default:
    assert(false);
    break;
  }
  assert(false);
  return NULL_VALUE;
}

value_t value_t::cost() const
{
  switch (type()) {
  case INTEGER:
  case AMOUNT:
  case BALANCE:
    return *this;

  case BALANCE_PAIR:
    assert(as_balance_pair().cost);
    if (as_balance_pair().cost)
      return *(as_balance_pair().cost);
    else
      return as_balance_pair().quantity();

  default:
    break;
  }

  throw_(value_error, "Cannot find the cost of " << label());
  return NULL_VALUE;
}

value_t& value_t::add(const amount_t& amount, const optional<amount_t>& tcost)
{
  switch (type()) {
  case INTEGER:
  case AMOUNT:
    if (tcost) {
      in_place_cast(BALANCE_PAIR);
      return add(amount, tcost);
    }
    else if ((is_amount() &&
	      as_amount().commodity() != amount.commodity()) ||
	     (! is_amount() && amount.commodity())) {
      in_place_cast(BALANCE);
      return add(amount, tcost);
    }
    else if (! is_amount()) {
      in_place_cast(AMOUNT);
    }
    return *this += amount;

  case BALANCE:
    if (tcost) {
      in_place_cast(BALANCE_PAIR);
      return add(amount, tcost);
    }
    return *this += amount;

  case BALANCE_PAIR:
    as_balance_pair_lval().add(amount, tcost);
    return *this;

  default:
    break;
  }

  throw_(value_error, "Cannot add an amount to " << label());
  return *this;
}

void value_t::dump(std::ostream& out, const int first_width,
		   const int latter_width) const
{
  switch (type()) {
  case VOID:
    out << "VOID";
    break;

  case BOOLEAN:
    out << as_boolean();
    break;

  case DATETIME:
    out << format_datetime(as_datetime());
    break;

  case DATE:
    out << format_date(as_date());
    break;

  case INTEGER:
    out << as_long();
    break;

  case AMOUNT:
    out << as_amount();
    break;

  case STRING:
    out << as_string();
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

      value.dump(out, first_width, latter_width);
    }
    out << ')';
    break;
  }

  case BALANCE:
    as_balance().print(out, first_width, latter_width);
    break;
  case BALANCE_PAIR:
    as_balance_pair().print(out, first_width, latter_width);
    break;
  default:
    assert(false);
    break;
  }
}

void value_t::print(std::ostream& out, const bool relaxed) const
{
  switch (type()) {
  case VOID:
    out << "";
    break;

  case BOOLEAN:
    if (as_boolean())
      out << "true";
    else
      out << "false";
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
  case BALANCE_PAIR:
    assert(false);
    break;

  case DATETIME:
    assert(false);
    break;
  case DATE:
    out << '[' << format_date(as_date()) << ']';
    break;

  case STRING:
    out << '"' << as_string() << '"';
    break;

  case POINTER:
    assert(false);
    break;

  case SEQUENCE: {
    out << '(';
    bool first = true;
    foreach (const value_t& value, as_sequence()) {
      if (first)
	first = false;
      else
	out << ", ";

      value.print(out, relaxed);
    }
    out << ')';
    break;
  }
  }
}

void value_t::read(const char *& data)
{
  switch (static_cast<value_t::type_t>(binary::read_long<int>(data))) {
  case BOOLEAN:
    set_boolean(binary::read_bool(data));
    break;
  case INTEGER:
    set_long(binary::read_long<unsigned long>(data));
    break;
  case DATETIME:
#if 0
    // jww (2008-04-22): I need to record and read a datetime_t directly
    set_datetime(read_long<unsigned long>(data));
#endif
    break;
  case DATE:
#if 0
    // jww (2008-04-22): I need to record and read a date_t directly
    set_date(read_long<unsigned long>(data));
#endif
    break;
  case AMOUNT: {
    amount_t temp;
    temp.read(data);
    set_amount(temp);
    break;
  }
  default:
    break;
  }

  throw_(value_error, "Cannot read " << label() << " from a stream");
}

void value_t::write(std::ostream& out) const
{
  binary::write_long(out, static_cast<int>(type()));

  switch (type()) {
  case BOOLEAN:
    binary::write_bool(out, as_boolean());
    break;
  case INTEGER:
    binary::write_long(out, as_long());
    break;
  case DATETIME:
#if 0
    binary::write_number(out, as_datetime());
#endif
    break;
  case DATE:
#if 0
    binary::write_number(out, as_date());
#endif
    break;
  case AMOUNT:
    as_amount().write(out);
    break;
  default:
    break;
  }

  throw_(value_error, "Cannot read " << label() << " to a stream");
}

bool value_t::valid() const
{
  switch (type()) {
  case AMOUNT:
    return as_amount().valid();
  case BALANCE:
    return as_balance().valid();
  case BALANCE_PAIR:
    return as_balance_pair().valid();
  default:
    break;
  }
  return true;
}

} // namespace ledger
