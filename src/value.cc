/*
 * Copyright (c) 2003-2025, John Wiegley.  All rights reserved.
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

/**
 * @file   value.cc
 * @author John Wiegley
 *
 * @ingroup math
 *
 * @brief  Implementation of the polymorphic value_t type
 *
 * This file implements value_t, the dynamic type that flows through every
 * stage of Ledger's expression engine, filter pipeline, and report
 * formatter.  The central design goal is to avoid expensive balance_t
 * heap allocations for the overwhelmingly common cases where a simple
 * boolean, integer, or single-commodity amount suffices.
 *
 * value_t achieves this through a type promotion strategy: arithmetic
 * and comparison operators inspect the types of both operands and choose
 * the smallest result type that preserves full precision.  The promotion
 * hierarchy, from least to most general, is:
 *
 *   VOID < BOOLEAN < DATETIME < DATE < INTEGER < AMOUNT < BALANCE
 *        < STRING < MASK < SEQUENCE < SCOPE < ANY
 *
 * For example, INTEGER + INTEGER stays INTEGER, but INTEGER + AMOUNT
 * (with a commodity) promotes to BALANCE so the commodity is not lost.
 * Subtraction may simplify back down: if a BALANCE ends up with a
 * single commodity after subtraction, in_place_simplify() demotes it
 * to AMOUNT or even INTEGER.
 *
 * The file is organized into the following logical sections:
 *
 *   1. Storage management     -- storage_t copy, initialize/shutdown
 *   2. Truth testing          -- operator bool, is_zero, is_realzero
 *   3. Type conversion        -- to_*() helpers, in_place_cast, simplify
 *   4. Arithmetic operators   -- +=, -=, *=, /= with type promotion
 *   5. Comparison operators   -- is_equal_to, is_less_than, is_greater_than
 *   6. Unary operations       -- negate, not, abs, rounding family
 *   7. Commodity operations   -- value, exchange, annotate, strip
 *   8. Printing and debugging -- print, dump, label, valid
 *   9. Sorting and XML output -- sort_value_is_less_than, put_value
 */

#include <system.hh>

#include "value.h"
#include "commodity.h"
#include "annotate.h"
#include "pool.h"
#include "unistring.h" // for justify()
#include "op.h"

namespace ledger {

intrusive_ptr<value_t::storage_t> value_t::true_value;
intrusive_ptr<value_t::storage_t> value_t::false_value;

// ----------------------------------------------------------------------
// Section 1: Storage management
//
// value_t uses intrusive reference-counted storage with copy-on-write
// semantics.  BALANCE and SEQUENCE own heap-allocated objects and must
// be deep-copied; all other types are stored inline in the variant and
// can be copied by value.
// ----------------------------------------------------------------------

/// @brief Deep-copy assignment for storage objects.
///
/// BALANCE and SEQUENCE contain raw pointers to heap-allocated objects,
/// so they must be cloned rather than pointer-copied.  All other variant
/// alternatives are trivially copyable or have proper copy constructors
/// within std::variant and are handled by the default branch.
value_t::storage_t& value_t::storage_t::operator=(const value_t::storage_t& rhs) {
  if (this == &rhs)
    return *this;

  type = rhs.type;

  switch (type) {
  case BALANCE: // NOLINT(bugprone-branch-clone)
    data = new balance_t(*std::get<balance_t*>(rhs.data));
    break;
  case SEQUENCE:
    data = new sequence_t(*std::get<sequence_t*>(rhs.data));
    break;

  default:
    data = rhs.data;
    break;
  }

  return *this;
}

/// @brief Create the shared true/false singleton storage objects.
///
/// Because boolean values are extremely common in expression evaluation
/// (every filter predicate produces one), two static storage_t objects
/// are pre-allocated and shared by all boolean value_t instances.  This
/// eliminates per-value heap allocations for the most frequent case.
void value_t::initialize() {
  true_value = new storage_t;
  true_value->type = BOOLEAN;
  true_value->data = true;

  false_value = new storage_t;
  false_value->type = BOOLEAN;
  false_value->data = false;
}

/// @brief Release the shared boolean singletons at program shutdown.
void value_t::shutdown() {
  true_value = intrusive_ptr<storage_t>();
  false_value = intrusive_ptr<storage_t>();
}

// ----------------------------------------------------------------------
// Section 2: Truth testing
//
// Truth testing is used pervasively by the expression engine to evaluate
// filter predicates (`--limit`, `--display`, etc.).  Each type defines
// its own truthiness: integers are true if non-zero, amounts delegate
// to amount_t::operator bool, strings are true if non-empty, and
// sequences are true if any element is true.  MASK values cannot be
// truth-tested (they are patterns, not values) and throw an error
// suggesting the `=~` operator instead.
// ----------------------------------------------------------------------

/// @brief Determine the truth value of any value_t.
///
/// This is the core predicate used by the expression engine when
/// evaluating `if`, `and`, `or`, and filter expressions.  The rules
/// for each type mirror common programming conventions:
///   - VOID is always false
///   - BOOLEAN returns its stored value
///   - DATETIME/DATE are true if the stored date is valid
///   - INTEGER is true if non-zero
///   - AMOUNT/BALANCE delegate to their own operator bool
///   - STRING is true if non-empty
///   - SEQUENCE is true if any element is true
///   - SCOPE is true if the pointer is non-null
///   - MASK always throws (use `=~` instead)
value_t::operator bool() const {
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
  case COMMODITY:
    return as_commodity();
  case STRING:
    return !as_string().empty();
  case MASK: {
    std::ostringstream out;
    out << *this;
    throw_(value_error, _f("Cannot determine truth of %1% (did you mean 'account =~ %2%'?)") %
                            label() % out.str());
  }
  case SEQUENCE:
    if (!as_sequence().empty()) {
      for (const value_t& value : as_sequence()) {
        if (value)
          return true;
      }
    }
    return false;
  case SCOPE:
    return as_scope() != nullptr;
  case ANY:
    return as_any().has_value();
  }

  add_error_context(_f("While taking boolean value of %1%:") % *this);
  throw_(value_error, _f("Cannot determine truth of %1%") % label());

  return false;
}

/// @brief Prepare storage for a new type, reusing the object if possible.
///
/// If the value is VOID, storage is simply released.  Otherwise, if the
/// current storage has a reference count of 1 (we are the sole owner),
/// the existing storage is destroyed and reused -- avoiding a heap
/// allocation.  If the storage is shared, a fresh storage_t is allocated.
void value_t::set_type(type_t new_type) {
  if (new_type == VOID) { // NOLINT(bugprone-branch-clone)
    storage.reset();
  } else {
    if (!storage || storage->refc > 1)
      storage = new storage_t;
    else
      storage->destroy();
    storage->type = new_type;
  }
}

void value_t::set_commodity(const commodity_t& val) {
  VERIFY(val.valid());
  set_type(COMMODITY);
  storage->data = &val;
}

// ----------------------------------------------------------------------
// Section 3: Type conversion (to_*() helpers, in_place_cast, simplify)
//
// The to_*() methods provide convenient type-safe extraction.  If the
// value already holds the requested type, the stored data is returned
// directly.  Otherwise, in_place_cast is used to attempt a conversion
// on a temporary copy.
//
// in_place_cast() is the general-purpose type coercion engine.  It
// handles all valid conversions between value types and throws
// value_error for unsupported ones.  The valid conversions include:
//   - Any type -> BOOLEAN (via truth testing)
//   - Any type -> SEQUENCE (wraps in a single-element sequence)
//   - VOID -> INTEGER (0), AMOUNT (0), STRING ("")
//   - BOOLEAN -> INTEGER (0/1), AMOUNT (0/1), STRING ("true"/"false")
//   - DATE <-> DATETIME
//   - DATE/DATETIME -> STRING
//   - INTEGER -> AMOUNT, BALANCE, STRING
//   - AMOUNT -> INTEGER (truncates), BALANCE, STRING
//   - BALANCE -> AMOUNT (only if single commodity), STRING
//   - STRING -> INTEGER, AMOUNT, COMMODITY, DATE, DATETIME, MASK
//   - COMMODITY -> STRING
//   - MASK -> STRING
//
// in_place_simplify() reduces a value to the most compact representation
// that preserves its meaning: a zero value of any type becomes INTEGER 0,
// and a BALANCE with a single commodity becomes an AMOUNT.
// ----------------------------------------------------------------------

bool value_t::to_boolean() const {
  if (is_boolean()) {
    return as_boolean();
  } else {
    value_t temp(*this);
    temp.in_place_cast(BOOLEAN);
    return temp.as_boolean();
  }
}

datetime_t value_t::to_datetime() const {
  if (is_datetime()) {
    return as_datetime();
  } else {
    value_t temp(*this);
    temp.in_place_cast(DATETIME);
    return temp.as_datetime();
  }
}

date_t value_t::to_date() const {
  if (is_date()) {
    return as_date();
  } else {
    value_t temp(*this);
    temp.in_place_cast(DATE);
    return temp.as_date();
  }
}

int value_t::to_int() const {
  if (is_long()) {
    return boost::numeric_cast<int>(as_long());
  } else {
    value_t temp(*this);
    temp.in_place_cast(INTEGER);
    return boost::numeric_cast<int>(temp.as_long());
  }
}

long value_t::to_long() const {
  if (is_long()) {
    return as_long();
  } else {
    value_t temp(*this);
    temp.in_place_cast(INTEGER);
    return temp.as_long();
  }
}

amount_t value_t::to_amount() const {
  if (is_amount()) {
    return as_amount();
  } else {
    value_t temp(*this);
    temp.in_place_cast(AMOUNT);
    return temp.as_amount();
  }
}

balance_t value_t::to_balance() const {
  if (is_balance()) {
    return as_balance();
  } else {
    value_t temp(*this);
    temp.in_place_cast(BALANCE);
    return temp.as_balance();
  }
}

const commodity_t& value_t::to_commodity() const {
  if (is_commodity()) {
    return as_commodity();
  } else {
    value_t temp(*this);
    temp.in_place_cast(COMMODITY);
    return temp.as_commodity();
  }
}

string value_t::to_string() const {
  if (is_string()) {
    return as_string();
  } else if (is_commodity()) {
    return as_commodity().base_symbol();
  } else {
    value_t temp(*this);
    temp.in_place_cast(STRING);
    return temp.as_string();
  }
}

mask_t value_t::to_mask() const {
  if (is_mask()) {
    return as_mask();
  } else {
    value_t temp(*this);
    temp.in_place_cast(MASK);
    return temp.as_mask();
  }
}

value_t::sequence_t value_t::to_sequence() const {
  if (is_sequence()) {
    return as_sequence();
  } else {
    value_t temp(*this);
    temp.in_place_cast(SEQUENCE);
    return temp.as_sequence();
  }
}

/// @brief Reduce this value to the simplest type that preserves its meaning.
///
/// This is called after subtraction and other operations that may leave
/// a value in a more complex type than necessary.  Two simplifications
/// are performed:
///   1. Any value that is "really zero" (is_realzero()) is replaced by
///      INTEGER 0, avoiding the overhead of carrying an empty balance or
///      zero amount through subsequent calculations.
///   2. A BALANCE containing exactly one commodity is demoted to AMOUNT,
///      since there is no need for the multi-commodity map.
void value_t::in_place_simplify() {
#if DEBUG_ON
  LOGGER("value.simplify");
#endif

  if (is_null())
    return;

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

/// @brief Extract the numeric magnitude, stripping any commodity.
///
/// Returns the pure numeric value without commodity information.  For
/// AMOUNT, this calls amount_t::number() which strips the commodity.
/// For BALANCE, it calls balance_t::number().  For SEQUENCE, the
/// numbers of all elements are summed.  BOOLEAN is converted to 0 or 1.
///
/// This is used by expressions that need a raw number for arithmetic,
/// such as percentage calculations where the commodity is irrelevant.
value_t value_t::number() const {
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
    if (!as_sequence().empty()) {
      value_t temp;
      for (const value_t& value : as_sequence())
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

// ----------------------------------------------------------------------
// Section 4: Arithmetic operators (+=, -=, *=, /=)
//
// These are the largest and most complex methods in value.cc because
// each must handle every valid combination of left-hand and right-hand
// types, applying the correct type promotion rules.
//
// Type promotion rules for addition and subtraction:
//   INTEGER + INTEGER       -> INTEGER
//   INTEGER + AMOUNT(bare)  -> AMOUNT   (promote left to AMOUNT)
//   INTEGER + AMOUNT(comm)  -> BALANCE  (commodity requires balance)
//   INTEGER + BALANCE       -> BALANCE
//   AMOUNT  + INTEGER       -> BALANCE if amount has commodity, else AMOUNT
//   AMOUNT  + AMOUNT(same)  -> AMOUNT
//   AMOUNT  + AMOUNT(diff)  -> BALANCE  (different commodities)
//   AMOUNT  + BALANCE       -> BALANCE
//   BALANCE + INTEGER/AMOUNT/BALANCE -> BALANCE
//   DATETIME/DATE + INTEGER/AMOUNT   -> add seconds/days
//   STRING  + anything      -> string concatenation
//   SEQUENCE + SEQUENCE     -> element-wise (must be same length)
//   SEQUENCE + scalar       -> append to sequence
//   VOID + anything         -> copy the right-hand value
//
// Subtraction follows similar rules but calls in_place_simplify()
// after each operation, since a BALANCE minus an AMOUNT of the same
// commodity may reduce to a single-commodity result.
//
// Multiplication and division are more restrictive:
//   - Only numeric types (INTEGER, AMOUNT, BALANCE) are supported.
//   - BALANCE * BALANCE is not defined (what would it mean in accounting?).
//   - STRING * INTEGER repeats the string.
//   - BALANCE can only be multiplied/divided by a scalar (INTEGER or
//     uncommoditized AMOUNT), distributing the operation across all
//     commodity components.
// ----------------------------------------------------------------------

/// @brief Add a value to this one, promoting types as necessary.
///
/// The addition logic is organized in three phases:
///   1. Special-case handling for STRING (concatenation), COMMODITY
///      (cast to string first), and SEQUENCE (element-wise or append).
///   2. A switch over the left-hand type for numeric and temporal types.
///   3. Within each left-hand case, a switch over the right-hand type
///      determines the promotion path.
///
/// The key accounting insight is that adding amounts of different
/// commodities must produce a BALANCE (multi-commodity map), since
/// "$100 + 50 EUR" cannot be reduced to a single number without a
/// price lookup.
value_t& value_t::operator+=(const value_t& val) {
  if (val.is_null())
    return *this;

  if (is_string()) {
    if (val.is_string())
      as_string_lval() += val.as_string();
    else
      as_string_lval() += val.to_string();
    return *this;
  } else if (is_commodity()) {
    in_place_cast(STRING);
    return *this += val;
  } else if (is_sequence()) {
    if (val.is_sequence()) {
      if (size() == val.size()) {
        sequence_t::iterator i = begin();
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
          time_duration_t(0, 0, static_cast<time_duration_t::sec_type>(val.as_amount().to_long()));
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

/// @brief Subtract a value, with post-operation simplification.
///
/// Subtraction mirrors addition's type promotion rules but adds an
/// important difference: after every numeric subtraction, in_place_simplify()
/// is called.  This is because subtraction frequently cancels out
/// commodity components (e.g., a BALANCE of "$100, 50 EUR" minus "$100"
/// yields a single-commodity balance that can be simplified to AMOUNT).
///
/// For VOID, subtraction assigns the negated right-hand value, matching
/// the mathematical identity 0 - x = -x.
///
/// For SEQUENCE, subtraction has two modes:
///   - SEQUENCE - SEQUENCE: element-wise subtraction (lengths must match)
///   - SEQUENCE - scalar: removes the first matching element
value_t& value_t::operator-=(const value_t& val) {
  if (val.is_null())
    return *this;

  if (is_sequence()) {
    sequence_t& seq(as_sequence_lval());

    if (val.is_sequence()) {
      if (size() == val.size()) {
        sequence_t::iterator i = begin();
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
  case VOID:
    *this = val;
    in_place_negate();
    return *this;

  case DATETIME:
    switch (val.type()) {
    case INTEGER:
      as_datetime_lval() -=
          time_duration_t(0, 0, static_cast<time_duration_t::sec_type>(val.as_long()));
      return *this;
    case AMOUNT:
      as_datetime_lval() -=
          time_duration_t(0, 0, static_cast<time_duration_t::sec_type>(val.as_amount().to_long()));
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

/// @brief Multiply this value by another.
///
/// Multiplication is more restrictive than addition because multiplying
/// two commoditized amounts is generally undefined in accounting (what
/// is "$10 * 5 EUR"?).  The supported combinations are:
///   - INTEGER * INTEGER -> INTEGER
///   - INTEGER * AMOUNT  -> AMOUNT (the amount's commodity is preserved)
///   - AMOUNT  * INTEGER -> AMOUNT
///   - AMOUNT  * AMOUNT  -> AMOUNT (used for percentage calculations, etc.)
///   - BALANCE * INTEGER -> BALANCE (each component scaled)
///   - BALANCE * AMOUNT(bare) -> BALANCE (scale by uncommoditized factor)
///   - STRING  * INTEGER -> string repetition ("ab" * 3 = "ababab")
///   - SEQUENCE * INTEGER -> sequence repetition
///
/// BALANCE * AMOUNT(commoditized) is only allowed when the balance has
/// a single commodity (it is simplified to AMOUNT first).
value_t& value_t::operator*=(const value_t& val) {
  if (is_string()) {
    const string& s = as_string();
    long count = val.to_long();
    string temp;
    temp.reserve(s.size() * static_cast<std::size_t>(count));
    for (long i = 0; i < count; i++)
      temp += s;
    set_string(temp);
    return *this;
  } else if (is_commodity()) {
    in_place_cast(STRING);
    return *this *= val;
  } else if (is_sequence()) {
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
      } else if (!val.as_amount().has_commodity()) {
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

/// @brief Divide this value by another.
///
/// Division follows the same restrictions as multiplication: only
/// numeric types are supported, and BALANCE can only be divided by
/// scalars.  The supported combinations mirror operator*= with
/// division substituted.
///
/// Note: INTEGER / AMOUNT produces AMOUNT (not INTEGER), preserving
/// the precision of the amount's decimal representation.
value_t& value_t::operator/=(const value_t& val) {
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
      } else if (!val.as_amount().has_commodity()) {
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

// ----------------------------------------------------------------------
// Section 5: Comparison operators
//
// Comparison is used by the expression engine for sorting (`--sort`),
// filtering (`--limit "amount > 100"`), and conditional formatting.
// Like arithmetic, comparisons handle cross-type operands by promoting
// to a common type:
//   - INTEGER can be compared to AMOUNT and BALANCE (via to_amount())
//   - AMOUNT can be compared to INTEGER and BALANCE
//   - BALANCE values are compared via to_amount() (single-commodity only)
//   - COMMODITY and STRING can be compared to each other (by symbol name)
//   - Same-type comparisons use the type's native operator
//
// Comparing incompatible types (e.g., DATE to AMOUNT) throws value_error.
// ----------------------------------------------------------------------

/// @brief Test equality between two values, handling cross-type comparisons.
///
/// VOID is only equal to VOID.  For numeric types (INTEGER, AMOUNT,
/// BALANCE), cross-type equality is supported by promoting the simpler
/// operand.  COMMODITY can be compared to STRING by looking up the
/// string in the commodity pool.
bool value_t::is_equal_to(const value_t& val) const {
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

  case COMMODITY:
    switch (val.type()) {
    case COMMODITY:
      return as_commodity() == val.as_commodity();
    case STRING: {
      if (const auto* otherCommodity = as_commodity().pool().find(val.as_string()))
        return as_commodity() == *otherCommodity;
      return false;
    }
    default:
      break;
    }
    break;

  case STRING:
    switch (val.type()) {
    case COMMODITY:
      return val.is_equal_to(*this);
    case STRING:
      return as_string() == val.as_string();
    default:
      break;
    }
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

/// @brief Test whether this value is strictly less than another.
///
/// For VOID, null is considered less than any non-null value (establishing
/// a total order where VOID sorts first).  For AMOUNT values of different
/// commodities, lexicographic commodity comparison is used as a tiebreaker
/// so that sort output is deterministic.
///
/// BALANCE comparison against scalars checks whether ALL component amounts
/// are less than the scalar.  SEQUENCE comparison is lexicographic.
bool value_t::is_less_than(const value_t& val) const {
  if (val.is_null())
    return false; // nothing is less than null (null sorts first)

  switch (type()) {
  case VOID:
    return true; // null is less than any non-null value

  case BOOLEAN:
    if (val.is_boolean()) {
      if (as_boolean()) {
        return false;
      } else if (!as_boolean()) {
        if (!val.as_boolean())
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
    case SEQUENCE:
      return val.is_greater_than(*this);
    default:
      break;
    }
    break;

  case AMOUNT:
    switch (val.type()) {
    case INTEGER:
      return as_amount() < val.as_long();
    case AMOUNT:
      if (as_amount().commodity() == val.as_amount().commodity() || !as_amount().has_commodity() ||
          !val.as_amount().has_commodity())
        return as_amount() < val.as_amount();
      else
        return commodity_t::compare_by_commodity()(&as_amount(), &val.as_amount()) < 0;
    case BALANCE:
      return val.to_amount() > as_amount();
    case SEQUENCE:
      return val.is_greater_than(*this);
    default:
      break;
    }
    break;

  case BALANCE:
    switch (val.type()) {
    case INTEGER:
    case AMOUNT: {
      bool no_amounts = true;
      for (const balance_t::amounts_map::value_type& pair : as_balance().amounts) {
        if (pair.second >= val)
          return false;
        no_amounts = false;
      }
      return !no_amounts;
    }
    case BALANCE:
      return val.to_amount() > to_amount();
    default:
      break;
    }
    break;

  case COMMODITY:
    switch (val.type()) {
    case COMMODITY:
      return to_string() < val.to_string();
    case STRING: {
      if (const auto* otherCommodity = as_commodity().pool().find(val.as_string()))
        return to_string() < otherCommodity->symbol();
      return to_string() < val.as_string();
    }
    default:
      break;
    }
    break;

  case STRING:
    switch (val.type()) {
    case COMMODITY:
      return val.is_greater_than(*this);
    case STRING:
      return as_string() < val.as_string();
    default:
      break;
    }
    break;

  case SEQUENCE:
    switch (val.type()) {
    case INTEGER:
    case AMOUNT: {
      bool no_amounts = true;
      for (const value_t& value : as_sequence()) {
        if (value >= val)
          return false;
        no_amounts = false;
      }
      return !no_amounts;
    }
    case SEQUENCE: {
      sequence_t::const_iterator i = as_sequence().begin();
      sequence_t::const_iterator j = val.as_sequence().begin();
      for (; (i != as_sequence().end() && j != val.as_sequence().end()); i++, j++) {
        if (!((*i) < (*j)))
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

/// @brief Test whether this value is strictly greater than another.
///
/// Mirrors is_less_than() with reversed comparison directions.  VOID is
/// never greater than anything.  BALANCE > scalar requires ALL components
/// to be greater.
bool value_t::is_greater_than(const value_t& val) const {
  if (val.is_null())
    return !is_null(); // non-null is greater than null

  switch (type()) {
  case VOID:
    return false; // null is not greater than anything

  case BOOLEAN:
    if (val.is_boolean()) {
      if (as_boolean()) {
        if (!val.as_boolean())
          return true;
        else
          return false;
      } else if (!as_boolean()) {
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
    case SEQUENCE:
      return val.is_less_than(*this);
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
    case SEQUENCE:
      return val.is_less_than(*this);
    default:
      break;
    }
    break;

  case BALANCE:
    switch (val.type()) {
    case INTEGER:
    case AMOUNT: {
      bool no_amounts = true;
      for (const balance_t::amounts_map::value_type& pair : as_balance().amounts) {
        if (pair.second <= val)
          return false;
        no_amounts = false;
      }
      return !no_amounts;
    }
    case BALANCE:
      return val.to_amount() < to_amount();
    default:
      break;
    }
    break;

  case COMMODITY:
    switch (val.type()) {
    case COMMODITY:
      return to_string() > val.to_string();
    case STRING: {
      if (const auto* otherCommodity = as_commodity().pool().find(val.as_string()))
        return to_string() > otherCommodity->symbol();
      return to_string() > val.as_string();
    }
    default:
      break;
    }
    break;

  case STRING:
    switch (val.type()) {
    case COMMODITY:
      return val.is_less_than(*this);
    case STRING:
      return as_string() > val.as_string();
    default:
      break;
    }
    break;

  case SEQUENCE:
    switch (val.type()) {
    case INTEGER:
    case AMOUNT: {
      bool no_amounts = true;
      for (const value_t& value : as_sequence()) {
        if (value <= val)
          return false;
        no_amounts = false;
      }
      return !no_amounts;
    }
    case SEQUENCE: {
      sequence_t::const_iterator i = as_sequence().begin();
      sequence_t::const_iterator j = val.as_sequence().begin();
      for (; (i != as_sequence().end() && j != val.as_sequence().end()); i++, j++) {
        if (!((*i) > (*j)))
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

/// @brief Convert this value to the specified type in place.
///
/// This is the general-purpose type coercion engine.  Two target types
/// receive universal handling before the main switch:
///   - BOOLEAN: any type can be cast to bool via operator bool()
///   - SEQUENCE: any type is wrapped in a single-element sequence
///
/// All other conversions are handled by a two-level switch on
/// (source type, target type).  If a conversion path does not exist,
/// a value_error is thrown.  See the Section 3 comment above for the
/// complete table of valid conversions.
///
/// Notable behaviors:
///   - BALANCE -> AMOUNT only succeeds if the balance contains exactly
///     one commodity (or is empty, yielding amount 0).  Multi-commodity
///     balances cannot be losslessly represented as a single amount.
///   - STRING -> INTEGER only succeeds if the string contains only
///     digits and an optional leading minus sign.
///   - STRING -> AMOUNT parses the string as a commoditized amount
///     (e.g., "$100.00" or "50 EUR").
///   - STRING -> MASK compiles the string as a regular expression.
void value_t::in_place_cast(type_t cast_type) {
  if (type() == cast_type)
    return;

  _dup();

  if (cast_type == BOOLEAN) {
    set_boolean(bool(*this));
    return;
  } else if (cast_type == SEQUENCE) {
    sequence_t temp;
    if (!is_null())
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
      set_string(format_date(as_date(), date_format_is_set() ? FMT_PRINTED : FMT_WRITTEN));
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
      set_string(
          format_datetime(as_datetime(), datetime_format_is_set() ? FMT_PRINTED : FMT_WRITTEN));
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
      } else if (bal.amounts.size() == 0) {
        set_amount(0L);
        return;
      } else {
        add_error_context(_f("While converting %1% to an amount:") % *this);
        throw_(value_error, _f("Cannot convert %1% with multiple commodities to %2%") % label() %
                                label(cast_type));
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

  case COMMODITY:
    switch (cast_type) {
    case STRING:
      set_string(to_string());
      return;
    default:
      break;
    }
    break;

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
    case COMMODITY:
      if (const auto* commodity = commodity_pool_t::current_pool->find(as_string())) {
        set_commodity(*commodity);
        return;
      }
      break;
    case DATE:
      set_date(parse_date(as_string()));
      return;
    case DATETIME:
      set_datetime(parse_datetime(as_string()));
      return;
    case MASK: {
      // Make a copy since set_mask calls set_type which may reallocate storage,
      // invalidating the reference returned by as_string()
      string str = as_string(); // NOLINT(bugprone-unused-local-non-trivial-variable)
      set_mask(str);
      return;
    }
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
  throw_(value_error, _f("Cannot convert %1% to %2%") % label() % label(cast_type));
}

// ----------------------------------------------------------------------
// Section 6: Unary operations (negate, not, abs, rounding family)
//
// These operations transform a single value in place.  They share a
// common structure: a switch over the value's type delegates to the
// corresponding operation on amount_t or balance_t, while INTEGER is
// handled directly and SEQUENCE applies the operation element-wise.
//
// The rounding family deserves special attention because it determines
// how amounts appear in reports:
//   - in_place_round()       -- round to the commodity's internal precision
//   - in_place_roundto(n)    -- round to exactly n decimal places
//   - in_place_truncate()    -- truncate (round toward zero)
//   - in_place_floor()       -- round toward negative infinity
//   - in_place_ceiling()     -- round toward positive infinity
//   - in_place_display_round() -- round to the commodity's display precision
//   - in_place_round_to_commodity_precision() -- round to commodity precision
//   - in_place_unround()     -- restore full internal precision
//
// These correspond to the expression functions round(), roundto(),
// truncate(), floor(), ceiling(), and unround() available in format
// strings and value expressions.
// ----------------------------------------------------------------------

/// @brief Negate this value in place (unary minus).
///
/// For BOOLEAN, negation flips the value (equivalent to logical not).
/// For INTEGER, DATETIME, and DATE, the stored long is negated.
/// For AMOUNT and BALANCE, the corresponding in_place_negate() is called.
/// For SEQUENCE, each element is negated recursively.
void value_t::in_place_negate() {
  switch (type()) {
  case VOID:
    return;
  case BOOLEAN:
    set_boolean(!as_boolean());
    return;
  case INTEGER:
  case DATETIME:
  case DATE:
    set_long(-as_long());
    return;
  case AMOUNT:
    as_amount_lval().in_place_negate();
    return;
  case BALANCE:
    as_balance_lval().in_place_negate();
    return;
  case SEQUENCE:
    for (value_t& value : as_sequence_lval())
      value.in_place_negate();
    return;
  default:
    break;
  }

  add_error_context(_f("While negating %1%:") % *this);
  throw_(value_error, _f("Cannot negate %1%") % label());
}

/// @brief Apply logical NOT in place, converting the value to BOOLEAN.
///
/// Unlike in_place_negate() which preserves the value's type,
/// in_place_not() always converts the result to BOOLEAN.  This is used
/// by the expression engine's `not` operator and `!` syntax.
void value_t::in_place_not() {
  switch (type()) {
  case BOOLEAN:
    set_boolean(!as_boolean());
    return;
  case INTEGER:
  case DATETIME:
  case DATE:
    set_boolean(!as_long());
    return;
  case AMOUNT:
    set_boolean(!as_amount());
    return;
  case BALANCE:
    set_boolean(!as_balance());
    return;
  case COMMODITY:
    set_boolean(!as_commodity());
    return;
  case STRING:
    set_boolean(as_string().empty());
    return;
  case SEQUENCE:
    for (value_t& value : as_sequence_lval())
      value.in_place_not();
    return;
  default:
    break;
  }

  add_error_context(_f("While applying not to %1%:") % *this);
  throw_(value_error, _f("Cannot 'not' %1%") % label());
}

/// @brief Test whether this value is truly zero, ignoring display rounding.
///
/// is_realzero() differs from is_zero() for AMOUNT values: is_zero()
/// considers an amount zero if it rounds to zero at the commodity's
/// display precision, while is_realzero() checks whether the underlying
/// value is exactly zero at full internal precision.  This distinction
/// matters for in_place_simplify(), which should only collapse a value
/// to INTEGER 0 if it is genuinely zero, not merely display-zero.
bool value_t::is_realzero() const {
  switch (type()) {
  case BOOLEAN:
    return !as_boolean();
  case INTEGER:
    return as_long() == 0;
  case DATETIME:
    return !is_valid(as_datetime());
  case DATE:
    return !is_valid(as_date());
  case AMOUNT:
    return as_amount().is_realzero();
  case BALANCE:
    return as_balance().is_realzero();
  case COMMODITY:
    return !as_commodity();
  case STRING:
    return as_string().empty();
  case SEQUENCE:
    return as_sequence().empty();

  case SCOPE:
    return as_scope() == nullptr;
  case ANY:
    return !as_any().has_value();

  default:
    add_error_context(_f("While applying is_realzero to %1%:") % *this);
    throw_(value_error, _f("Cannot determine if %1% is really zero") % label());
  }
  return false;
}

/// @brief Test whether this value is zero, respecting display precision.
///
/// For AMOUNT values, this uses amount_t::is_zero(), which considers an
/// amount zero if it rounds to zero at the commodity's display precision.
/// For example, $0.001 is considered zero if the commodity "$" displays
/// only two decimal places.  Use is_realzero() to check exact zero.
bool value_t::is_zero() const {
  switch (type()) {
  case BOOLEAN:
    return !as_boolean();
  case INTEGER:
    return as_long() == 0;
  case DATETIME:
    return !is_valid(as_datetime());
  case DATE:
    return !is_valid(as_date());
  case AMOUNT:
    return as_amount().is_zero();
  case BALANCE:
    return as_balance().is_zero();
  case COMMODITY:
    return !as_commodity();
  case STRING:
    return as_string().empty();
  case SEQUENCE:
    return as_sequence().empty();

  case SCOPE:
    return as_scope() == nullptr;
  case ANY:
    return !as_any().has_value();

  default:
    add_error_context(_f("While applying is_zero to %1%:") % *this);
    throw_(value_error, _f("Cannot determine if %1% is zero") % label());
  }
  return false;
}

// ----------------------------------------------------------------------
// Section 7: Commodity operations (value, exchange, annotate, strip)
//
// These methods connect value_t to Ledger's commodity and pricing
// system.  They implement the `--market`, `--exchange`, and annotation
// stripping behaviors that transform multi-commodity balances into
// user-friendly report output.
// ----------------------------------------------------------------------

/// @brief Return the market value of this value at the given moment.
///
/// This powers the `--market` and `--exchange COMM` report options.
/// For AMOUNT, it delegates to amount_t::value() which looks up the
/// most recent price in the commodity's price history.  For BALANCE,
/// each component amount is individually repriced.  INTEGER and VOID
/// have no associated commodity and thus no market price, so they
/// return NULL_VALUE (which the caller typically treats as "no change").
///
/// An empty BALANCE is returned as-is rather than NULL_VALUE so that
/// it renders as zero instead of blank in report output.
value_t value_t::value(const datetime_t& moment, const commodity_t* in_terms_of) const {
  switch (type()) {
  case VOID:
  case INTEGER:
    return NULL_VALUE;

  case AMOUNT:
    if (auto val = as_amount().value(moment, in_terms_of))
      return *val;
    return NULL_VALUE;

  case BALANCE:
    // An empty balance is zero in any commodity, so return it as-is rather
    // than converting to NULL_VALUE which would display as blank
    if (as_balance().is_empty())
      return *this;
    if (optional<balance_t> bal = as_balance().value(moment, in_terms_of))
      return *bal;
    return NULL_VALUE;

  case SEQUENCE: {
    value_t temp;
    for (const value_t& value : as_sequence())
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

/// @brief Convert commodities according to a comma-separated exchange spec.
///
/// This implements the `--exchange COMM1,COMM2` command-line option.  The
/// @p commodities string is parsed as a comma-separated list where each
/// entry may be:
///   - "USD"     -- convert everything to USD
///   - "EUR:USD" -- convert only EUR to USD, leave other commodities alone
///   - "USD!"    -- force conversion even if the value is already in USD
///   - "=expr"   -- use a price expression instead of a commodity name
///
/// For a simple single-commodity target with no special syntax (no comma,
/// colon, or equals sign), this method shortcuts to value() for efficiency.
///
/// For BALANCE values, each component amount is examined individually:
/// amounts matching a source specifier (or not matching any target
/// commodity) are repriced; others are kept as-is.
value_t value_t::exchange_commodities(const std::string& commodities, const bool add_prices,
                                      const datetime_t& moment) {
  if (type() == SEQUENCE) {
    value_t temp;
    for (value_t& value : as_sequence_lval())
      temp.push_back(value.exchange_commodities(commodities, add_prices, moment));
    return temp;
  }

  // If we are repricing to just a single commodity, with no price
  // expression, skip the expensive logic below.
  if (commodities.find(',') == string::npos && commodities.find('=') == string::npos &&
      commodities.find(':') == string::npos)
    return value(moment, commodity_pool_t::current_pool->find_or_create(commodities));

  // Phase 1: Parse the comma-separated commodity spec into parallel vectors
  // of target commodities, optional source commodities, and force flags.
  std::vector<commodity_t*> comms;
  std::vector<commodity_t*> sources;
  std::vector<bool> force;

  std::vector<string> tokens;
  boost::split(tokens, commodities, boost::is_any_of(","));

  for (const string& name : tokens) {
    string target_name = name;
    string source_name;

    // Handle A:B syntax for selective conversion (convert A to B only)
    string::size_type colon_pos = name.find(':');
    if (colon_pos != string::npos) {
      source_name = string(name, 0, colon_pos);
      target_name = string(name, colon_pos + 1);
    }

    string::size_type name_len = target_name.length();
    bool is_forced = name_len > 0 && target_name[name_len - 1] == '!';
    string expr_name = is_forced ? string(target_name, 0, name_len - 1) : target_name;

    if (commodity_t* commodity =
            commodity_pool_t::current_pool->parse_price_expression(expr_name, add_prices, moment)) {
      DEBUG("commodity.exchange", "Pricing for commodity: " << commodity->symbol());
      comms.push_back(&commodity->referent());
      force.push_back(is_forced);

      if (!source_name.empty()) {
        if (commodity_t* src = commodity_pool_t::current_pool->find(source_name))
          sources.push_back(&src->referent());
        else
          sources.push_back(nullptr);
      } else {
        sources.push_back(nullptr);
      }
    }
  }

  // Phase 2: Apply each target commodity to this value.  For AMOUNT, we
  // reprice directly.  For BALANCE, we iterate over each component amount
  // and selectively reprice based on the source/force constraints.
  std::size_t index = 0;
  for (commodity_t* comm : comms) {
    switch (type()) {
    case AMOUNT:
      DEBUG("commodity.exchange", "We have an amount: " << as_amount_lval());
      if (sources[index]) {
        // A:B syntax: only convert if the amount is commodity A
        if (&as_amount_lval().commodity().referent() != sources[index])
          break;
      } else if (!force[index] &&
                 std::find(comms.begin(), comms.end(), &as_amount_lval().commodity().referent()) !=
                     comms.end()) {
        break;
      }

      DEBUG("commodity.exchange", "Referent doesn't match, pricing...");
      if (auto val = as_amount_lval().value(moment, comm)) {
        DEBUG("commodity.exchange", "Re-priced amount is: " << *val);
        return *val;
      }
      DEBUG("commodity.exchange", "Was unable to find a price");
      break;

    case BALANCE: {
      balance_t temp;
      bool repriced = false;

      DEBUG("commodity.exchange", "We have a balance: " << as_balance_lval());
      for (const balance_t::amounts_map::value_type& pair : as_balance_lval().amounts) {
        DEBUG("commodity.exchange", "We have a balance amount of commodity: "
                                        << pair.first->symbol()
                                        << " == " << pair.second.commodity().symbol());
        bool should_convert;
        if (sources[index]) {
          // A:B syntax: only convert if the amount is commodity A
          should_convert = (&pair.first->referent() == sources[index]);
        } else {
          should_convert = force[index] || std::find(comms.begin(), comms.end(),
                                                     &pair.first->referent()) == comms.end();
        }

        if (!should_convert) {
          temp += pair.second;
        } else {
          DEBUG("commodity.exchange", "Referent doesn't match, pricing...");
          if (auto val = pair.second.value(moment, comm)) {
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

/// @brief Reduce commodity expressions to their base commodities.
///
/// For example, if "min" is defined as "60s", an amount of "5 min" would
/// be reduced to "300s".  This delegates to amount_t::in_place_reduce()
/// or balance_t::in_place_reduce() which walk the commodity equivalence
/// chain.  Non-numeric types are silently ignored.
void value_t::in_place_reduce() {
  switch (type()) {
  case AMOUNT:
    as_amount_lval().in_place_reduce();
    return;
  case BALANCE:
    as_balance_lval().in_place_reduce();
    return;
  case SEQUENCE:
    for (value_t& value : as_sequence_lval())
      value.in_place_reduce();
    return;
  default:
    return;
  }

  // throw_(value_error, _f("Cannot reduce %1%") % label());
}

/// @brief Expand base commodities back to their higher-level equivalents.
///
/// The inverse of in_place_reduce(): "300s" would become "5 min" if such
/// an equivalence is defined.
void value_t::in_place_unreduce() {
  switch (type()) {
  case AMOUNT:
    as_amount_lval().in_place_unreduce();
    return;
  case BALANCE:
    as_balance_lval().in_place_unreduce();
    return;
  case SEQUENCE:
    for (value_t& value : as_sequence_lval())
      value.in_place_unreduce();
    return;
  default:
    return;
  }

  // throw_(value_error, _f("Cannot reduce %1%") % label());
}

/// @brief Return the absolute value.
///
/// Supported for INTEGER, AMOUNT, and BALANCE.  VOID is returned as-is
/// (it has no sign).  Delegates to amount_t::abs() or balance_t::abs()
/// for the commodity-aware types.
value_t value_t::abs() const {
  switch (type()) {
  case VOID:
    return *this;
  case INTEGER: {
    long val = as_long();
    if (val < 0)
      return -val;
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

/// @brief Round to the commodity's internal precision.
///
/// For AMOUNT, this rounds to the number of decimal places defined by the
/// commodity (e.g., 2 for USD).  For BALANCE, each component amount is
/// rounded independently.  INTEGER values are already integral and need
/// no rounding.  For SEQUENCE, rounding is applied element-wise.
void value_t::in_place_round() {
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
    for (value_t& value : as_sequence_lval())
      value.in_place_round();
    return;
  default:
    break;
  }

  add_error_context(_f("While rounding %1%:") % *this);
  throw_(value_error, _f("Cannot set rounding for %1%") % label());
}

/// @brief Round to a specific number of decimal places.
/// @param places  The number of decimal places to retain.
void value_t::in_place_roundto(int places) {
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
    for (value_t& value : as_sequence_lval())
      value.in_place_roundto(places);
    return;
  default:
    break;
  }
}

/// @brief Truncate toward zero (remove fractional part without rounding).
void value_t::in_place_truncate() {
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
    for (value_t& value : as_sequence_lval())
      value.in_place_truncate();
    return;
  default:
    break;
  }

  add_error_context(_f("While truncating %1%:") % *this);
  throw_(value_error, _f("Cannot truncate %1%") % label());
}

/// @brief Round to the commodity's display precision.
///
/// Display precision may differ from internal precision.  For example,
/// a commodity might track 6 decimal places internally but display only 2.
/// This method rounds to the display precision, which is used when
/// formatting output for the user.
void value_t::in_place_display_round() {
  switch (type()) {
  case INTEGER:
    return;
  case AMOUNT:
    as_amount_lval().in_place_roundto(as_amount_lval().display_precision());
    return;
  case BALANCE:
    as_balance_lval().in_place_display_round();
    return;
  case SEQUENCE:
    for (value_t& value : as_sequence_lval())
      value.in_place_display_round();
    return;
  default:
    break;
  }

  add_error_context(_f("While display-rounding %1%:") % *this);
  throw_(value_error, _f("Cannot display-round %1%") % label());
}

/// @brief Round to the precision defined by the commodity's usage history.
///
/// This differs from in_place_round() in that it uses the precision
/// observed from actual transactions involving the commodity, rather than
/// a fixed internal precision.
void value_t::in_place_round_to_commodity_precision() {
  switch (type()) {
  case INTEGER:
    return;
  case AMOUNT:
    as_amount_lval().in_place_round_to_commodity_precision();
    return;
  case BALANCE:
    as_balance_lval().in_place_round_to_commodity_precision();
    return;
  case SEQUENCE:
    for (value_t& value : as_sequence_lval())
      value.in_place_round_to_commodity_precision();
    return;
  default:
    break;
  }

  add_error_context(_f("While rounding %1% to commodity precision:") % *this);
  throw_(value_error, _f("Cannot round %1% to commodity precision") % label());
}

/// @brief Round toward negative infinity.
void value_t::in_place_floor() {
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
    for (value_t& value : as_sequence_lval())
      value.in_place_floor();
    return;
  default:
    break;
  }

  add_error_context(_f("While flooring %1%:") % *this);
  throw_(value_error, _f("Cannot floor %1%") % label());
}

/// @brief Round toward positive infinity.
void value_t::in_place_ceiling() {
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
    for (value_t& value : as_sequence_lval())
      value.in_place_ceiling();
    return;
  default:
    break;
  }

  add_error_context(_f("While ceiling %1%:") % *this);
  throw_(value_error, _f("Cannot ceiling %1%") % label());
}

/// @brief Restore the value to its full internal precision, undoing rounding.
void value_t::in_place_unround() {
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
    for (value_t& value : as_sequence_lval())
      value.in_place_unround();
    return;
  default:
    break;
  }

  add_error_context(_f("While unrounding %1%:") % *this);
  throw_(value_error, _f("Cannot unround %1%") % label());
}

/// @brief Attach price/date/tag annotations to this value's commodity.
///
/// Annotations record metadata about how an amount was acquired, such as
/// the per-unit cost (`{$50}`), the acquisition date (`[2024-01-15]`),
/// or a note tag (`(lot1)`).  Only AMOUNT values can be annotated; other
/// types throw value_error.
///
/// @param details  The annotation containing price, date, and/or tag info.
void value_t::annotate(const annotation_t& details) {
  if (is_amount()) {
    as_amount_lval().annotate(details);
  } else {
    add_error_context(_f("While attempting to annotate %1%:") % *this);
    throw_(value_error, _f("Cannot annotate %1%") % label());
  }
}

/// @brief Check whether this value's commodity carries an annotation.
///
/// Only AMOUNT values can carry annotations (price, date, or tag metadata
/// attached to a commodity).  Calling this on any other type throws
/// value_error.
bool value_t::has_annotation() const {
  if (is_amount()) {
    return as_amount().has_annotation();
  } else {
    add_error_context(_f("While checking if %1% has annotations:") % *this);
    throw_(value_error, _f("Cannot determine whether %1% is annotated") % label());
  }
  return false;
}

/// @brief Return a mutable reference to this value's annotation.
///
/// The caller can modify the returned annotation_t to change the price,
/// date, or tag associated with this amount's commodity.  Only AMOUNT
/// values support this; other types throw value_error.
annotation_t& value_t::annotation() {
  if (is_amount()) {
    return as_amount_lval().annotation();
  } else {
    add_error_context(_f("While requesting the annotations of %1%:") % *this);
    throw_(value_error, _f("Cannot request annotation of %1%") % label());
    return as_amount_lval().annotation(); // quiet g++ warning
  }
}

/// @brief Return a copy of this value with annotations selectively removed.
///
/// The @p what_to_keep parameter controls which annotation components
/// (price, date, tag) are preserved.  This is used by report options like
/// `--lot-prices`, `--lot-dates`, and `--lot-tags` to control how much
/// lot detail appears in output.  If what_to_keep.keep_all() is true,
/// the value is returned unchanged for efficiency.
///
/// Non-annotatable types (BOOLEAN, INTEGER, STRING, etc.) are returned
/// as-is.  SEQUENCE values have each element stripped recursively.
value_t value_t::strip_annotations(const keep_details_t& what_to_keep) const {
  if (what_to_keep.keep_all())
    return *this;

  switch (type()) {
  case VOID:
  case BOOLEAN:
  case INTEGER:
  case DATETIME:
  case DATE:
  case COMMODITY:
  case STRING:
  case MASK:
  case SCOPE:
  case ANY:
    return *this;

  case SEQUENCE: {
    sequence_t temp;
    for (const value_t& value : as_sequence())
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

// ----------------------------------------------------------------------
// Section 8: Printing and debugging (print, dump, label, valid)
//
// print() produces user-facing report output with optional column
// justification and colorization.  dump() produces a debug-oriented
// representation that can be round-tripped through the expression
// parser (amounts are wrapped in braces, strings in quotes, etc.).
// label() returns a human-readable type name for error messages.
// valid() performs internal consistency checks.
// ----------------------------------------------------------------------

/// @brief Return a human-readable label for this value's type.
///
/// Used in error messages to describe what kind of value caused a
/// problem, e.g., "Cannot add a string to an integer".
string value_t::label(optional<type_t> the_type) const {
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
  case COMMODITY:
    return _("a commodity");
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

/// @brief Format this value for user-facing report output.
///
/// This is the primary output method used by the report formatters.
/// It handles column width justification for tabular output (balance
/// reports, register reports) and optional ANSI color for negative values.
///
/// @param _out          The output stream to write to.
/// @param first_width   Column width for the first line of output.
///                      A value of -1 disables width formatting.
/// @param latter_width  Column width for subsequent lines (used by
///                      BALANCE which may span multiple lines).
/// @param flags         Bitmask of AMOUNT_PRINT_* flags controlling
///                      right-justification, colorization, etc.
void value_t::print(std::ostream& _out, const int first_width, const int latter_width,
                    const uint_least8_t flags) const {
  // We buffer into a local ostringstream so that width and justification
  // settings do not leak into the caller's stream.
  std::ostringstream out;

  // Set column width for simple scalar types.  AMOUNT, BALANCE, and STRING
  // handle their own justification via the justify() helper so they are
  // excluded here.
  if (first_width > 0 && (!is_amount() || as_amount().is_zero()) && !is_balance() && !is_string()) {
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

  case DATETIME: // NOLINT(bugprone-branch-clone)
    out << format_datetime(as_datetime(), datetime_format_is_set() ? FMT_PRINTED : FMT_WRITTEN);
    break;

  case DATE:
    out << format_date(as_date(), date_format_is_set() ? FMT_PRINTED : FMT_WRITTEN);
    break;

  case INTEGER:
    if (flags & AMOUNT_PRINT_COLORIZE && as_long() < 0)
      justify(out, to_string(), first_width, flags & AMOUNT_PRINT_RIGHT_JUSTIFY, true);
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

  case COMMODITY:
    justify(out, to_string(), first_width, flags & AMOUNT_PRINT_RIGHT_JUSTIFY);
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
    for (const value_t& value : as_sequence()) {
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

/// @brief Write a debug-oriented representation of this value.
///
/// Unlike print(), dump() produces output that is closer to the internal
/// representation and can often be parsed back by the expression engine.
/// Amounts are wrapped in braces (e.g., `{$100.00}`), strings in double
/// quotes with escape sequences, dates in brackets, and booleans as
/// `true`/`false`.
///
/// @param out      The output stream to write to.
/// @param relaxed  If true, amounts are printed without braces (more
///                 readable but not round-trippable).
void value_t::dump(std::ostream& out, const bool relaxed) const {
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

  case DATETIME: // NOLINT(bugprone-branch-clone)
    out << '[' << format_datetime(as_datetime(), FMT_WRITTEN) << ']';
    break;
  case DATE:
    out << '[' << format_date(as_date(), FMT_WRITTEN) << ']';
    break;

  case INTEGER:
    out << as_long();
    break;

  case AMOUNT:
    if (!relaxed)
      out << '{';
    out << as_amount();
    if (!relaxed)
      out << '}';
    break;

  case BALANCE:
    out << as_balance();
    break;

  case COMMODITY:
    out << as_commodity();
    break;

  case STRING:
    out << '"';
    for (const char& ch : as_string()) {
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

  case MASK: {
    out << '/';
    for (char ch : as_mask().str()) {
      switch (ch) {
      case '/':
        out << "\\/";
        break;
      case '\\':
        out << "\\\\";
        break;
      default:
        out << ch;
        break;
      }
    }
    out << '/';
    break;
  }

  case SCOPE:
    out << as_scope();
    break;
  case ANY:
    if (as_any().type() == typeid(expr_t::ptr_op_t))
      as_any<expr_t::ptr_op_t>()->dump(out);
    else
      out << as_any().type().name();
    break;

  case SEQUENCE: {
    out << '(';
    bool first = true;
    for (const value_t& value : as_sequence()) {
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

/// @brief Validate internal consistency of this value.
///
/// Delegates to the valid() method of AMOUNT, BALANCE, or COMMODITY
/// if the value holds one of those types.  Other types are always
/// considered valid.  Used by VERIFY assertions throughout the codebase.
bool value_t::valid() const {
  switch (type()) {
  case AMOUNT:
    return as_amount().valid();
  case BALANCE:
    return as_balance().valid();
  case COMMODITY:
    return as_commodity().valid();
  default:
    break;
  }
  return true;
}

// ----------------------------------------------------------------------
// Section 9: Sorting and XML output
//
// sort_value_is_less_than() provides the comparator used by the `--sort`
// option to order transactions and postings.  It walks a list of sort
// keys in priority order, respecting per-key inversion for descending
// sorts, and skips BALANCE values which lack a natural total ordering.
//
// put_value() serializes a value_t into a Boost.PropertyTree node,
// enabling the `xml` report format to emit structured output that
// external tools can parse.
// ----------------------------------------------------------------------

/// @brief Compare two multi-key sort value lists for ordering.
///
/// This implements the `--sort` option's comparison logic.  Each
/// sort_value_t in the list represents one sort key (e.g., `--sort date`
/// or `--sort "amount, account"`).  Keys are compared left to right;
/// the first non-equal key determines the result.  Each key may be
/// individually inverted (descending order).
///
/// BALANCE values are skipped during comparison since they have no
/// natural total ordering (a multi-commodity balance cannot be
/// meaningfully compared as less-than or greater-than).
bool sort_value_is_less_than(const std::list<sort_value_t>& left_values,
                             const std::list<sort_value_t>& right_values) {
  std::list<sort_value_t>::const_iterator left_iter = left_values.begin();
  std::list<sort_value_t>::const_iterator right_iter = right_values.begin();

  while (left_iter != left_values.end() && right_iter != right_values.end()) {
    // Don't even try to sort balance values
    if (!(*left_iter).value.is_balance() && !(*right_iter).value.is_balance()) {
      DEBUG("value.sort", " Comparing " << (*left_iter).value << " < " << (*right_iter).value);
      if ((*left_iter).value < (*right_iter).value) {
        DEBUG("value.sort", "  is less");
        return !(*left_iter).inverted;
      } else if ((*left_iter).value > (*right_iter).value) {
        DEBUG("value.sort", "  is greater");
        return (*left_iter).inverted;
      }
    }
    left_iter++;
    right_iter++;
  }

  assert(left_iter == left_values.end());
  assert(right_iter == right_values.end());

  return false;
}

/// @brief Serialize a value_t into a Boost property tree for XML output.
///
/// This is used by the `xml` report format to produce structured output.
/// Each value type maps to a specific XML element name (e.g., "amount",
/// "balance", "int", "bool").  SCOPE and ANY values cannot be serialized
/// and will trigger an assertion failure.
void put_value(property_tree::ptree& pt, const value_t& value) {
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
  case value_t::COMMODITY:
    put_commodity(pt.add("commodity", ""), value.as_commodity());
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
    for (const value_t& member : value.as_sequence())
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
