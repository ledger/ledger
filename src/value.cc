/*
 * Copyright (c) 2003-2023, John Wiegley.  All rights reserved.
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

#include <system.hh>

#include "value.h"
#include "commodity.h"
#include "annotate.h"
#include "pool.h"
#include "unistring.h"          // for justify()
#include "op.h"

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
  true_value        = new storage_t;
  true_value->type  = BOOLEAN;
  true_value->data  = true;

  false_value       = new storage_t;
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
  case MASK: {
    std::ostringstream out;
    out << *this;
    throw_(value_error,
           _f("Cannot determine truth of %1% (did you mean 'account =~ %2%'?)")
           % label() % out.str());
  }
  case SEQUENCE:
    if (! as_sequence().empty()) {
      foreach (const value_t& value, as_sequence()) {
        if (value)
          return true;
      }
    }
    return false;
  case SCOPE:
    return as_scope() != NULL;
  case ANY:
    return ! as_any().empty();
  }

  add_error_context(_f("While taking boolean value of %1%:") % *this);
  throw_(value_error, _f("Cannot determine truth of %1%") % label());

  return false;
}

void value_t::set_type(type_t new_type)
{
  if (new_type == VOID) {
    storage.reset();
  } else {
    if (! storage || storage->refc > 1)
      storage = new storage_t;
    else
      storage->destroy();
    storage->type = new_type;
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

int value_t::to_int() const
{
  if (is_long()) {
    return static_cast<int>(as_long());
  } else {
    value_t temp(*this);
    temp.in_place_cast(INTEGER);
    return static_cast<int>(temp.as_long());
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
#if DEBUG_ON
  LOGGER("value.simplify");
#endif

  if (is_realzero()) {
    DEBUG_("Zeroing type " << static_cast<int>(type()));
    set_long(0L);
    return;
  }

  if (is_balance() && as_balance().single_amount()) {
    DEBUG_("Reducing balance to amount");
    DEBUG_("as a balance it looks like: " << *this);
    in_place_cast(AMOUNT);
    DEBUG_("as an amount it looks like: " << *this);
  }
}

value_t value_t::number() const
{
  switch (type()) {
  case VOID:
    return 0L;
  case BOOLEAN:
    return as_boolean() ? 1L : 0L;
  case INTEGER:
    return as_long();
  case AMOUNT:
    return as_amount().number();
  case BALANCE:
    return as_balance().number();
  case SEQUENCE:
    if (! as_sequence().empty()) {
      value_t temp;
      foreach (const value_t& value, as_sequence())
        temp += value.number();
      return temp;
    }
    break;
  default:
    break;
  }

  add_error_context(_f("While calling number() on %1%:") % *this);
  throw_(value_error, _f("Cannot determine numeric value of %1%") % label());

  return false;
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
        sequence_t::iterator       i = begin();
        sequence_t::const_iterator j = val.begin();

        for (; i != end(); i++, j++)
          *i += *j;
      } else {
        add_error_context(_f("While adding %1% to %2%:") % val % *this);
        throw_(value_error, _("Cannot add sequences of different lengths"));
      }
    } else {
      as_sequence_lval().push_back(new value_t(val));
    }
    return *this;
  }

  switch (type()) {
  case VOID:
    *this = value_t(val);
    return *this;

  case DATETIME:
    switch (val.type()) {
    case INTEGER:
      as_datetime_lval() +=
        time_duration_t(0, 0, static_cast<time_duration_t::sec_type>(val.as_long()));
      return *this;
    case AMOUNT:
      as_datetime_lval() +=
        time_duration_t(0, 0, static_cast<time_duration_t::sec_type>
                        (val.as_amount().to_long()));
      return *this;
    default:
      break;
    }
    break;

  case DATE:
    switch (val.type()) {
    case INTEGER:
      as_date_lval() += gregorian::date_duration(val.as_long());
      return *this;
    case AMOUNT:
      as_date_lval() += gregorian::date_duration(val.as_amount().to_long());
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

    case AMOUNT:
      if (as_amount().commodity() != val.as_amount().commodity()) {
        in_place_cast(BALANCE);
        return *this += val;
      } else {
        as_amount_lval() += val.as_amount();
        return *this;
      }

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

  add_error_context(_f("While adding %1% to %2%:") % val % *this);
  throw_(value_error, _f("Cannot add %1% to %2%") % val.label() % label());

  return *this;
}

value_t& value_t::operator-=(const value_t& val)
{
  if (is_sequence()) {
    sequence_t& seq(as_sequence_lval());

    if (val.is_sequence()) {
      if (size() == val.size()) {
        sequence_t::iterator       i = begin();
        sequence_t::const_iterator j = val.begin();

        for (; i != end(); i++, j++)
          *i -= *j;
      } else {
        add_error_context(_f("While subtracting %1% from %2%:") % val % *this);
        throw_(value_error, _("Cannot subtract sequences of different lengths"));
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
      as_datetime_lval() -=
        time_duration_t(0, 0, static_cast<time_duration_t::sec_type>(val.as_long()));
      return *this;
    case AMOUNT:
      as_datetime_lval() -=
        time_duration_t(0, 0, static_cast<time_duration_t::sec_type>
                        (val.as_amount().to_long()));
      return *this;
    default:
      break;
    }
    break;

  case DATE:
    switch (val.type()) {
    case INTEGER:
      as_date_lval() -= gregorian::date_duration(val.as_long());
      return *this;
    case AMOUNT:
      as_date_lval() -= gregorian::date_duration(val.as_amount().to_long());
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

  add_error_context(_f("While subtracting %1% from %2%:") % val % *this);
  throw_(value_error, _f("Cannot subtract %1% from %2%") % val.label() % label());

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
      as_amount_lval() *= val.as_amount();
      return *this;
    case BALANCE:
      if (val.as_balance().single_amount()) {
        as_amount_lval() *= val.simplified().as_amount();
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
      if (as_balance().single_amount()) {
        in_place_simplify();
        as_amount_lval() *= val.as_amount();
        return *this;
      }
      else if (! val.as_amount().has_commodity()) {
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

  add_error_context(_f("While multiplying %1% with %2%:") % val % *this);
  throw_(value_error, _f("Cannot multiply %1% with %2%") % label() % val.label());

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
    case BALANCE:
      if (val.as_balance().single_amount()) {
        value_t simpler(val.simplified());
        switch (simpler.type()) {
        case INTEGER:
          as_amount_lval() /= simpler.as_long();
          break;
        case AMOUNT:
          as_amount_lval() /= simpler.as_amount();
          break;
        default:
          assert(false);
          break;
        }
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
      if (as_balance().single_amount()) {
        in_place_cast(AMOUNT);
        as_amount_lval() /= val.as_amount();
        return *this;
      }
      else if (! val.as_amount().has_commodity()) {
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

  add_error_context(_f("While dividing %1% by %2%:") % *this % val);
  throw_(value_error, _f("Cannot divide %1% by %2%") % label() % val.label());

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

  add_error_context(_f("While comparing equality of %1% and %2%:") % *this % val);
  throw_(value_error, _f("Cannot compare %1% to %2%") % label() % val.label());

  return *this;
}

bool value_t::is_less_than(const value_t& val) const
{
  switch (type()) {
  case BOOLEAN:
    if (val.is_boolean()) {
      if (as_boolean()) {
        if (! val.as_boolean())
          return false;
        else
          return false;
      }
      else if (! as_boolean()) {
        if (! val.as_boolean())
          return false;
        else
          return true;
      }
    }
    break;

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
      return val.as_amount() > as_long();
    case BALANCE:
      return val.to_amount() > as_long();
    default:
      break;
    }
    break;

  case AMOUNT:
    switch (val.type()) {
    case INTEGER:
      return as_amount() < val.as_long();
    case AMOUNT:
      if (as_amount().commodity() == val.as_amount().commodity() ||
          ! as_amount().has_commodity() ||
          ! val.as_amount().has_commodity())
        return as_amount() < val.as_amount();
      else
        return commodity_t::compare_by_commodity()(&as_amount(), &val.as_amount()) < 0;
    case BALANCE:
      return val.to_amount() > as_amount();
    default:
      break;
    }
    break;

  case BALANCE:
    switch (val.type()) {
    case INTEGER:
    case AMOUNT: {
      bool no_amounts = true;
      foreach (const balance_t::amounts_map::value_type& pair,
               as_balance().amounts) {
        if (pair.second >= val)
          return false;
        no_amounts = false;
      }
      return ! no_amounts;
    }
    case BALANCE:
      return val.to_amount() > to_amount();
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
      bool no_amounts = true;
      foreach (const value_t& value, as_sequence()) {
        if (value >= val)
          return false;
        no_amounts = false;
      }
      return ! no_amounts;
    }
    case SEQUENCE: {
      sequence_t::const_iterator i = as_sequence().begin();
      sequence_t::const_iterator j = val.as_sequence().begin();
      for (; (i != as_sequence().end() &&
              j != val.as_sequence().end()); i++, j++) {
        if (! ((*i) < (*j)))
          return false;
      }
      if (i == as_sequence().end())
        return true;
      else
        return false;
    }
    default:
      break;
    }
    break;

  default:
    break;
  }

  add_error_context(_f("While comparing if %1% is less than %2%:") % *this % val);
  throw_(value_error, _f("Cannot compare %1% to %2%") % label() % val.label());

  return *this;
}

bool value_t::is_greater_than(const value_t& val) const
{
  switch (type()) {
  case BOOLEAN:
    if (val.is_boolean()) {
      if (as_boolean()) {
        if (! val.as_boolean())
          return true;
        else
          return false;
      }
      else if (! as_boolean()) {
        if (! val.as_boolean())
          return false;
        else
          return false;
      }
    }
    break;

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
      return val.as_amount() < as_long();
    case BALANCE:
      return val.to_amount() < as_long();
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
    case BALANCE:
      return val.to_amount() < as_amount();
    default:
      break;
    }
    break;

  case BALANCE:
    switch (val.type()) {
    case INTEGER:
    case AMOUNT: {
      bool no_amounts = true;
      foreach (const balance_t::amounts_map::value_type& pair,
               as_balance().amounts) {
        if (pair.second <= val)
          return false;
        no_amounts = false;
      }
      return ! no_amounts;
    }
    case BALANCE:
      return val.to_amount() < to_amount();
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
      bool no_amounts = true;
      foreach (const value_t& value, as_sequence()) {
        if (value <= val)
          return false;
        no_amounts = false;
      }
      return ! no_amounts;
    }
    case SEQUENCE: {
      sequence_t::const_iterator i = as_sequence().begin();
      sequence_t::const_iterator j = val.as_sequence().begin();
      for (; (i != as_sequence().end() &&
              j != val.as_sequence().end()); i++, j++) {
        if (! ((*i) > (*j)))
          return false;
      }
      if (i == as_sequence().end())
        return false;
      else
        return true;
    }
    default:
      break;
    }
    break;

  default:
    break;
  }

  add_error_context(_f("While comparing if %1% is greater than %2%:") % *this % val);
  throw_(value_error, _f("Cannot compare %1% to %2%") % label() % val.label());

  return *this;
}

void value_t::in_place_cast(type_t cast_type)
{
  if (type() == cast_type)
    return;

  _dup();

  if (cast_type == BOOLEAN) {
    set_boolean(bool(*this));
    return;
  }
  else if (cast_type == SEQUENCE) {
    sequence_t temp;
    if (! is_null())
      temp.push_back(new value_t(*this));
    set_sequence(temp);
    return;
  }

  switch (type()) {
  case VOID:
    switch (cast_type) {
    case INTEGER:
      set_long(0L);
      return;
    case AMOUNT:
      set_amount(0L);
      return;
    case STRING:
      set_string("");
      return;
    default:
      break;
    }
    break;

  case BOOLEAN:
    switch (cast_type) {
    case INTEGER:
      set_long(as_boolean() ? 1L : 0L);
      return;
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
      set_string(format_date(as_date(), FMT_WRITTEN));
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
      set_string(format_datetime(as_datetime(), FMT_WRITTEN));
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

  case BALANCE: {
    const balance_t& bal(as_balance());
    switch (cast_type) {
    case AMOUNT: {
      if (bal.amounts.size() == 1) {
        // Because we are changing the current balance value to an amount
        // value, and because set_amount takes a reference (and that memory is
        // about to be repurposed), we must pass in a copy.
        set_amount(amount_t((*bal.amounts.begin()).second));
        return;
      }
      else if (bal.amounts.size() == 0) {
        set_amount(0L);
        return;
      }
      else {
        add_error_context(_f("While converting %1% to an amount:") % *this);
        throw_(value_error, _f("Cannot convert %1% with multiple commodities to %2%")
               % label() % label(cast_type));
      }
      break;
    }
    case STRING:
      if (bal.is_empty())
        set_string("");
      else
        set_string(as_balance().to_string());
      return;
    default:
      break;
    }
    break;
  }

  case STRING:
    switch (cast_type) {
    case INTEGER: {
      if (all(as_string(), is_any_of("-0123456789"))) {
        set_long(lexical_cast<long>(as_string()));
        return;
      }
      break;
    }
    case AMOUNT:
      set_amount(amount_t(as_string()));
      return;
    case DATE:
      set_date(parse_date(as_string()));
      return;
    case DATETIME:
      set_datetime(parse_datetime(as_string()));
      return;
    case MASK:
      set_mask(as_string());
      return;
    default:
      break;
    }
    break;

  case MASK:
    switch (cast_type) {
    case STRING:
      set_string(as_mask().str());
      return;
    default:
      break;
    }
    break;

  default:
    break;
  }

  add_error_context(_f("While converting %1%:") % *this);
  throw_(value_error,
         _f("Cannot convert %1% to %2%") % label() % label(cast_type));
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
  case SEQUENCE:
    foreach (value_t& value, as_sequence_lval())
      value.in_place_negate();
    return;
  default:
    break;
  }

  add_error_context(_f("While negating %1%:") % *this);
  throw_(value_error, _f("Cannot negate %1%") % label());
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
  case SEQUENCE:
    foreach (value_t& value, as_sequence_lval())
      value.in_place_not();
    return;
  default:
    break;
  }

  add_error_context(_f("While applying not to %1%:") % *this);
  throw_(value_error, _f("Cannot 'not' %1%") % label());
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

  case SCOPE:
    return as_scope() == NULL;
  case ANY:
    return as_any().empty();

  default:
    add_error_context(_f("While applying is_realzero to %1%:") % *this);
    throw_(value_error, _f("Cannot determine if %1% is really zero") % label());
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

  case SCOPE:
    return as_scope() == NULL;
  case ANY:
    return as_any().empty();

  default:
    add_error_context(_f("While applying is_zero to %1%:") % *this);
    throw_(value_error, _f("Cannot determine if %1% is zero") % label());
  }
  return false;
}

value_t value_t::value(const datetime_t&   moment,
                       const commodity_t * in_terms_of) const
{
  switch (type()) {
  case INTEGER:
    return NULL_VALUE;

  case AMOUNT:
    if (optional<amount_t> val = as_amount().value(moment, in_terms_of))
      return *val;
    return NULL_VALUE;

  case BALANCE:
    if (optional<balance_t> bal = as_balance().value(moment, in_terms_of))
      return *bal;
    return NULL_VALUE;

  case SEQUENCE: {
    value_t temp;
    foreach (const value_t& value, as_sequence())
      temp.push_back(value.value(moment, in_terms_of));
    return temp;
  }

  default:
    break;
  }

  add_error_context(_f("While finding valuation of %1%:") % *this);
  throw_(value_error, _f("Cannot find the value of %1%") % label());
  return NULL_VALUE;
}

value_t value_t::exchange_commodities(const std::string& commodities,
                                      const bool         add_prices,
                                      const datetime_t&  moment)
{
  if (type() == SEQUENCE) {
    value_t temp;
    foreach (value_t& value, as_sequence_lval())
      temp.push_back(value.exchange_commodities(commodities, add_prices, moment));
    return temp;
  }

  // If we are repricing to just a single commodity, with no price
  // expression, skip the expensive logic below.
  if (commodities.find(',') == string::npos &&
      commodities.find('=') == string::npos)
    return value(moment, commodity_pool_t::current_pool->find_or_create(commodities));

  std::vector<commodity_t *> comms;
  std::vector<bool>          force;

  typedef tokenizer<char_separator<char> > tokenizer;
  tokenizer tokens(commodities, char_separator<char>(","));

  foreach (const string& name, tokens) {
    string::size_type name_len = name.length();

    if (commodity_t * commodity = commodity_pool_t::current_pool
        ->parse_price_expression(name[name_len - 1] == '!' ?
                                 string(name, 0, name_len - 1) :
                                 name, add_prices, moment)) {
      DEBUG("commodity.exchange", "Pricing for commodity: " << commodity->symbol());
      comms.push_back(&commodity->referent());
      force.push_back(name[name_len - 1] == '!');
    }
  }

  std::size_t index = 0;
  foreach (commodity_t * comm, comms) {
    switch (type()) {
    case AMOUNT:
      DEBUG("commodity.exchange", "We have an amount: " << as_amount_lval());
      if (! force[index] &&
          std::find(comms.begin(), comms.end(),
                    &as_amount_lval().commodity().referent()) != comms.end())
        break;

      DEBUG("commodity.exchange", "Referent doesn't match, pricing...");
      if (optional<amount_t> val = as_amount_lval().value(moment, comm)) {
        DEBUG("commodity.exchange", "Re-priced amount is: " << *val);
        return *val;
      }
      DEBUG("commodity.exchange", "Was unable to find a price");
      break;

    case BALANCE: {
      balance_t temp;
      bool repriced = false;

      DEBUG("commodity.exchange", "We have a balance: " << as_balance_lval());
      foreach (const balance_t::amounts_map::value_type& pair,
               as_balance_lval().amounts) {
        DEBUG("commodity.exchange", "We have a balance amount of commodity: "
              << pair.first->symbol() << " == "
              << pair.second.commodity().symbol());
        if (! force[index] &&
            std::find(comms.begin(), comms.end(),
                      &pair.first->referent()) != comms.end()) {
          temp += pair.second;
        } else {
          DEBUG("commodity.exchange", "Referent doesn't match, pricing...");
          if (optional<amount_t> val = pair.second.value(moment, comm)) {
            DEBUG("commodity.exchange", "Re-priced member amount is: " << *val);
            temp += *val;
            repriced = true;
          } else {
            DEBUG("commodity.exchange", "Was unable to find price");
            temp += pair.second;
          }
        }
      }

      if (repriced) {
        DEBUG("commodity.exchange", "Re-priced balance is: " << temp);
        return temp;
      }
    }

    default:
      break;
    }

    ++index;
  }

  return *this;
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
  case SEQUENCE:
    foreach (value_t& value, as_sequence_lval())
      value.in_place_reduce();
    return;
  default:
    return;
  }

  //throw_(value_error, _f("Cannot reduce %1%") % label());
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
  case SEQUENCE:
    foreach (value_t& value, as_sequence_lval())
      value.in_place_unreduce();
    return;
  default:
    return;
  }

  //throw_(value_error, _f("Cannot reduce %1%") % label());
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

  add_error_context(_f("While taking abs of %1%:") % *this);
  throw_(value_error, _f("Cannot abs %1%") % label());
  return NULL_VALUE;
}

void value_t::in_place_round()
{
  switch (type()) {
  case INTEGER:
    return;
  case AMOUNT:
    as_amount_lval().in_place_round();
    return;
  case BALANCE:
    as_balance_lval().in_place_round();
    return;
  case SEQUENCE:
    foreach (value_t& value, as_sequence_lval())
      value.in_place_round();
    return;
  default:
    break;
  }

  add_error_context(_f("While rounding %1%:") % *this);
  throw_(value_error, _f("Cannot set rounding for %1%") % label());
}

void value_t::in_place_roundto(int places)
{
  DEBUG("amount.roundto", "=====> roundto places " << places);
  switch (type()) {
  case INTEGER:
    return;
  case AMOUNT:
    as_amount_lval().in_place_roundto(places);
    return;
  case BALANCE:
    as_balance_lval().in_place_roundto(places);
    return;
  case SEQUENCE:
    foreach (value_t& value, as_sequence_lval())
      value.in_place_roundto(places);
    return;
  default:
    break;
  }
}

void value_t::in_place_truncate()
{
  switch (type()) {
  case INTEGER:
    return;
  case AMOUNT:
    as_amount_lval().in_place_truncate();
    return;
  case BALANCE:
    as_balance_lval().in_place_truncate();
    return;
  case SEQUENCE:
    foreach (value_t& value, as_sequence_lval())
      value.in_place_truncate();
    return;
  default:
    break;
  }

  add_error_context(_f("While truncating %1%:") % *this);
  throw_(value_error, _f("Cannot truncate %1%") % label());
}

void value_t::in_place_floor()
{
  switch (type()) {
  case INTEGER:
    return;
  case AMOUNT:
    as_amount_lval().in_place_floor();
    return;
  case BALANCE:
    as_balance_lval().in_place_floor();
    return;
  case SEQUENCE:
    foreach (value_t& value, as_sequence_lval())
      value.in_place_floor();
    return;
  default:
    break;
  }

  add_error_context(_f("While flooring %1%:") % *this);
  throw_(value_error, _f("Cannot floor %1%") % label());
}

void value_t::in_place_ceiling()
{
  switch (type()) {
  case INTEGER:
    return;
  case AMOUNT:
    as_amount_lval().in_place_ceiling();
    return;
  case BALANCE:
    as_balance_lval().in_place_ceiling();
    return;
  case SEQUENCE:
    foreach (value_t& value, as_sequence_lval())
      value.in_place_ceiling();
    return;
  default:
    break;
  }

  add_error_context(_f("While ceiling %1%:") % *this);
  throw_(value_error, _f("Cannot ceiling %1%") % label());
}

void value_t::in_place_unround()
{
  switch (type()) {
  case INTEGER:
    return;
  case AMOUNT:
    as_amount_lval().in_place_unround();
    return;
  case BALANCE:
    as_balance_lval().in_place_unround();
    return;
  case SEQUENCE:
    foreach (value_t& value, as_sequence_lval())
      value.in_place_unround();
    return;
  default:
    break;
  }

  add_error_context(_f("While unrounding %1%:") % *this);
  throw_(value_error, _f("Cannot unround %1%") % label());
}

void value_t::annotate(const annotation_t& details)
{
  if (is_amount()) {
    as_amount_lval().annotate(details);
  } else {
    add_error_context(_f("While attempting to annotate %1%:") % *this);
    throw_(value_error, _f("Cannot annotate %1%") % label());
  }
}

bool value_t::has_annotation() const
{
  if (is_amount()) {
    return as_amount().has_annotation();
  } else {
    add_error_context(_f("While checking if %1% has annotations:") % *this);
    throw_(value_error,
           _f("Cannot determine whether %1% is annotated") % label());
  }
  return false;
}

annotation_t& value_t::annotation()
{
  if (is_amount()) {
    return as_amount_lval().annotation();
  } else {
    add_error_context(_f("While requesting the annotations of %1%:") % *this);
    throw_(value_error, _f("Cannot request annotation of %1%") % label());
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
  case SCOPE:
  case ANY:
    return *this;

  case SEQUENCE: {
    sequence_t temp;
    foreach (const value_t& value, as_sequence())
      temp.push_back(new value_t(value.strip_annotations(what_to_keep)));
    return temp;
  }

  case AMOUNT:
    return as_amount().strip_annotations(what_to_keep);
  case BALANCE:
    return as_balance().strip_annotations(what_to_keep);
  }

  assert(false);
  return NULL_VALUE;
}

string value_t::label(optional<type_t> the_type) const
{
  switch (the_type ? *the_type : type()) {
  case VOID:
    return _("an uninitialized value");
  case BOOLEAN:
    return _("a boolean");
  case DATETIME:
    return _("a date/time");
  case DATE:
    return _("a date");
  case INTEGER:
    return _("an integer");
  case AMOUNT:
    return _("an amount");
  case BALANCE:
    return _("a balance");
  case STRING:
    return _("a string");
  case MASK:
    return _("a regexp");
  case SEQUENCE:
    return _("a sequence");
  case SCOPE:
    return _("a scope");
  case ANY:
    if (as_any().type() == typeid(expr_t::ptr_op_t))
      return _("an expr");
    else
      return _("an object");
  }
  assert(false);
  return _("<invalid>");
}

void value_t::print(std::ostream&       _out,
                    const int           first_width,
                    const int           latter_width,
                    const uint_least8_t flags) const
{
  std::ostringstream out;

  if (first_width > 0 &&
      (! is_amount() || as_amount().is_zero()) &&
      ! is_balance() && ! is_string()) {
    out.width(first_width);

    if (flags & AMOUNT_PRINT_RIGHT_JUSTIFY)
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
    out << format_datetime(as_datetime(), FMT_WRITTEN);
    break;

  case DATE:
    out << format_date(as_date(), FMT_WRITTEN);
    break;

  case INTEGER:
    if (flags & AMOUNT_PRINT_COLORIZE && as_long() < 0)
      justify(out, to_string(), first_width,
              flags & AMOUNT_PRINT_RIGHT_JUSTIFY, true);
    else
      out << as_long();
    break;

  case AMOUNT: {
    if (as_amount().is_zero()) {
      out << 0;
    } else {
      std::ostringstream buf;
      as_amount().print(buf, flags);
      justify(out, buf.str(), first_width, flags & AMOUNT_PRINT_RIGHT_JUSTIFY,
              flags & AMOUNT_PRINT_COLORIZE && as_amount().sign() < 0);
    }
    break;
  }

  case BALANCE:
    as_balance().print(out, first_width, latter_width, flags);
    break;

  case STRING:
    if (first_width > 0)
      justify(out, as_string(), first_width, flags & AMOUNT_PRINT_RIGHT_JUSTIFY);
    else
      out << as_string();
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

      value.print(out, first_width, latter_width, flags);
    }
    out << ')';
    break;
  }

  case SCOPE:
    out << "<#SCOPE>";
    break;
  case ANY:
    if (as_any().type() == typeid(expr_t::ptr_op_t)) {
      out << "<#EXPR ";
      as_any<expr_t::ptr_op_t>()->print(out);
      out << ">";
    } else {
      out << "<#OBJECT>";
    }
    break;
  }

  _out << out.str();
}

void value_t::dump(std::ostream& out, const bool relaxed) const
{
  switch (type()) {
  case VOID:
    out << "null";
    break;

  case BOOLEAN:
    if (as_boolean())
      out << "true";
    else
      out << "false";
    break;

  case DATETIME:
    out << '[' << format_datetime(as_datetime(), FMT_WRITTEN) << ']';
    break;
  case DATE:
    out << '[' << format_date(as_date(), FMT_WRITTEN) << ']';
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
    out << '"';
    foreach (const char& ch, as_string()) {
      switch (ch) {
      case '"':
        out << "\\\"";
        break;
      case '\\':
        out << "\\\\";
        break;
      default:
        out << ch;
        break;
      }
    }
    out << '"';
    break;

  case MASK:
    out << '/' << as_mask() << '/';
    break;

  case SCOPE:
    out << as_scope();
    break;
  case ANY:
    if (as_any().type() == typeid(expr_t::ptr_op_t))
      as_any<expr_t::ptr_op_t>()->dump(out);
    else
      out << boost::unsafe_any_cast<const void *>(&as_any());
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

void put_value(property_tree::ptree& pt, const value_t& value)
{
  switch (value.type()) {
  case value_t::VOID:
    pt.add("void", "");
    break;
  case value_t::BOOLEAN:
    pt.add("bool", value.as_boolean() ? "true" : "false");
    break;
  case value_t::INTEGER:
    pt.add("int", value.to_string());
    break;
  case value_t::AMOUNT:
    put_amount(pt.add("amount", ""), value.as_amount());
    break;
  case value_t::BALANCE:
    put_balance(pt.add("balance", ""), value.as_balance());
    break;
  case value_t::DATETIME:
    put_datetime(pt.add("datetime", ""), value.as_datetime());
    break;
  case value_t::DATE:
    put_date(pt.add("date", ""), value.as_date());
    break;
  case value_t::STRING:
    pt.add("string", value.as_string());
    break;
  case value_t::MASK:
    put_mask(pt.add("mask", ""), value.as_mask());
    break;

  case value_t::SEQUENCE: {
    property_tree::ptree& st(pt.add("sequence", ""));
    foreach (const value_t& member, value.as_sequence())
      put_value(st, member);
    break;
  }

  case value_t::SCOPE:
  case value_t::ANY:
    assert(false);
    break;
  }
}

} // namespace ledger
