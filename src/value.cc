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
#include <charconv>

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
// semantics.  SEQUENCE owns a heap-allocated object and must be
// deep-copied; all other types, BALANCE included, are stored inline in
// the variant and can be copied by value.
// ----------------------------------------------------------------------

/// @brief Deep-copy assignment for storage objects.
///
/// Only SEQUENCE holds an owned pointer to a heap-allocated object, so it
/// must be cloned rather than pointer-copied.  Every other variant
/// alternative, BALANCE included, is copied by std::variant's own copy
/// and is handled by the default branch.
value_t::storage_t& value_t::storage_t::operator=(const value_t::storage_t& rhs) {
  if (this == &rhs)
    return *this;

  type = rhs.type;

  switch (type) {
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
//   - Numeric types (INTEGER, AMOUNT, BALANCE), plus STRING and
//     SEQUENCE multiplied by an integer.
//   - BALANCE * BALANCE is not defined (what would it mean in accounting?).
//   - STRING * INTEGER repeats the string; SEQUENCE * INTEGER scales
//     each element (repeated element-wise addition).
//   - BALANCE can only be multiplied/divided by a scalar (INTEGER or
//     uncommoditized AMOUNT), distributing the operation across all
//     commodity components, unless it comes to one amount
//     (balance_t::sole_amount()), which then stands in for it.
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
      return *this -= val; // as the same bare amount does, keeping val's commodity
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
///   - SEQUENCE * INTEGER -> each element scaled (repeated element-wise addition)
///   - INTEGER/AMOUNT(bare) * BALANCE -> BALANCE (as BALANCE * the scalar)
///
/// A BALANCE that comes to one amount (balance_t::sole_amount(), which skips a
/// component that addition cancelled) acts as that amount, so it can also be
/// multiplied by a commoditized amount.
value_t& value_t::operator*=(const value_t& val) {
  if (is_string()) {
    const string& s = as_string();
    long count = val.to_long();
    string temp;
    if (count > 0)
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
    case BALANCE:
      in_place_cast(AMOUNT);
      return *this *= val; // as the same bare amount does
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
      // A bare factor scales a balance of several components, as
      // BALANCE * AMOUNT does; otherwise the balance stands for the one
      // amount it comes to, a zero one included.
      if (!val.as_balance().single_amount() && !as_amount().has_commodity()) {
        set_balance(val.as_balance() * as_amount());
        return *this;
      } else if (const amount_t* sole = val.as_balance().sole_amount()) {
        as_amount_lval() *= *sole;
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
      if (!as_balance().single_amount() && !val.as_amount().has_commodity()) {
        as_balance_lval() *= val.as_amount();
        return *this;
      } else if (as_balance().sole_amount()) {
        in_place_cast(AMOUNT); // the one amount it comes to
        as_amount_lval() *= val.as_amount();
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
/// division substituted, except that a scalar can be divided only by a
/// BALANCE that comes to one amount.
///
/// Note: INTEGER / AMOUNT produces AMOUNT (not INTEGER), preserving
/// the precision of the amount's decimal representation.
///   - INTEGER / BALANCE -> as the bare amount divided by the balance's one amount
value_t& value_t::operator/=(const value_t& val) {
  switch (type()) {
  case INTEGER:
    switch (val.type()) {
    case INTEGER:
      as_long_lval() /= val.as_long();
      return *this;
    case AMOUNT:
    case BALANCE:
      in_place_cast(AMOUNT);
      return *this /= val; // as the same bare amount does
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
      if (const amount_t* sole = val.as_balance().sole_amount()) {
        as_amount_lval() /= *sole;
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
      if (!as_balance().single_amount() && !val.as_amount().has_commodity()) {
        as_balance_lval() /= val.as_amount();
        return *this;
      } else if (as_balance().sole_amount()) {
        in_place_cast(AMOUNT); // the one amount it comes to
        as_amount_lval() /= val.as_amount();
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
// is_less_than() and is_greater_than() share one type matrix,
// value_ordered(), which takes the direction as a sign; a value
// expression evaluates `a > b` as `b < a`.  Across types:
//   - INTEGER and AMOUNT compare by quantity; an AMOUNT with no commodity
//     compares with one in any commodity.  AMOUNTs in two commodities
//     order by commodity (compare_by_commodity), so sorts are
//     deterministic.
//   - BALANCE vs INTEGER or AMOUNT, on either side, compares every
//     component: a live component in another commodity throws
//     amount_error, and one that netted to zero in another commodity is
//     left out.
//   - BALANCE vs BALANCE compares the one amount each side comes to
//     (to_amount(), by balance_t::sole_amount()): components that are
//     exactly zero are ignored, a balance of nothing but zeros is zero,
//     and a balance with two live commodities throws value_error.
//   - SEQUENCE vs INTEGER or AMOUNT compares every element; SEQUENCE vs
//     SEQUENCE compares pair by pair, not lexicographically.
//   - COMMODITY orders by bare symbol, against a STRING as against
//     another COMMODITY, so it neither precedes nor follows a string
//     that names it.
//   - Same-type comparisons use the type's native operator.
//
// Comparing incompatible types (e.g., DATE to AMOUNT) throws value_error.
// ----------------------------------------------------------------------

namespace {

/// @brief The symbol a string spells as a journal writes one.
///
/// A string in double quotes loses the two quotes and nothing else
/// (#3241).  A string that does not begin with a double quote and holds a
/// backslash is read by commodity_t::parse_symbol, the journal's reader of
/// bare symbols, where a backslash makes the next character part of the
/// symbol (#3313, #3321); if it reads as exactly one symbol, that symbol
/// is returned.  Any other string is returned unchanged.
string journal_symbol(const string& name) {
  if (name.length() >= 2 && name.front() == '"' && name.back() == '"')
    return string(name, 1, name.length() - 2);
  if (name.empty() || name.front() == '"' || name.find('\\') == string::npos)
    return name;
  string text(name);
  char* p = text.data();
  string symbol;
  try {
    commodity_t::parse_symbol(p, symbol);
  } catch (const amount_error&) {
    return name;
  }
  return *p == '\0' ? symbol : name;
}

/// @brief Find the commodity that a string names in a value expression.
///
/// The string may give the commodity's symbol or one of its aliases, or
/// either of those in the double quotes that a journal writes around a
/// symbol containing spaces, digits, or other reserved characters (issue
/// #3241), or with backslash escapes, as a bare journal symbol.  Returns
/// nullptr if the string names no commodity.
commodity_t* find_named_commodity(commodity_pool_t& pool, const string& name) {
  if (commodity_t* commodity = pool.find(name))
    return commodity;
  string symbol = journal_symbol(name);
  return symbol != name ? pool.find(symbol) : nullptr;
}

/// Whether @p bal comes to exactly @p amt.  A component that
/// balance_t::operator+= kept after cancelling it has no value, so it
/// is left out, as is_less_than leaves it out.
bool balance_equals(const balance_t& bal, const amount_t& amt) {
  if (amt.is_null())
    throw_(balance_error, _("Cannot compare a balance to an uninitialized amount"));
  if (amt.is_realzero())
    return bal.is_realzero();
  const amount_t* sole = bal.sole_amount();
  return sole && *sole == amt;
}

/// Whether two balances hold the same components, apart from exact zeros.
bool balances_equal(const balance_t& lhs, const balance_t& rhs) {
  return lhs == rhs || (lhs - rhs).is_realzero();
}

} // namespace

/// @brief Test equality between two values, handling cross-type comparisons.
///
/// A null value is equal only to a null value, on either side.  For
/// numeric types (INTEGER, AMOUNT, BALANCE), cross-type equality is
/// supported by promoting the simpler operand.  A BALANCE compares as its
/// components that are not exactly zero.  COMMODITY equals a STRING that
/// names it, by symbol, alias, or quoted symbol (see find_named_commodity()).
bool value_t::is_equal_to(const value_t& val) const {
  if (val.is_null())
    return is_null();

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
      return balance_equals(val.as_balance(), to_amount());
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
      return balance_equals(val.as_balance(), as_amount());
    default:
      break;
    }
    break;

  case BALANCE:
    switch (val.type()) {
    case INTEGER:
      return balance_equals(as_balance(), val.to_amount());
    case AMOUNT:
      return balance_equals(as_balance(), val.as_amount());
    case BALANCE:
      return balances_equal(as_balance(), val.as_balance());
    default:
      break;
    }
    break;

  case COMMODITY:
    switch (val.type()) {
    case COMMODITY:
      return as_commodity() == val.as_commodity();
    case STRING: {
      if (const auto* otherCommodity = find_named_commodity(as_commodity().pool(), val.as_string()))
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

namespace {
/// For comparing a balance with @p val: the first component in display
/// order whose commodity is not the amount's, or null when every component
/// is in it.  A plain number compares with any amount, as it always has,
/// and a component that netted to zero in another commodity is left out
/// of the comparison, so neither counts as another commodity.
const amount_t* foreign_component(const balance_t& bal, const value_t& val) {
  if (!val.is_amount() || !val.as_amount().has_commodity())
    return nullptr;
  const commodity_t& comm(val.as_amount().commodity());
  auto foreign = [&comm](const amount_t& amount) {
    return !amount.is_realzero() && amount.has_commodity() && amount.commodity() != comm;
  };
  for (const balance_t::amounts_map::value_type& pair : bal.amounts)
    if (foreign(pair.second)) {
      // Name the component the display order lists first: the map's own
      // order changes from one run to the next.
      balance_t::amounts_array sorted;
      bal.sorted_amounts(sorted);
      for (const amount_t* amount : sorted)
        if (foreign(*amount))
          return amount;
    }
  return nullptr;
}

/// Whether the three-way result @p cmp points the way @p sign asks: below
/// zero for -1 (less), above zero for +1 (greater).
bool has_sign(int cmp, int sign) {
  return sign < 0 ? cmp < 0 : cmp > 0;
}

/// Whether @p left is less (@p sign -1) or greater (+1) than @p right, by
/// the type's own operator<.  Greater asks `right < left`, as `left >
/// right` does under Boost's operators, so for amounts the same operand
/// is the object of amount_t::compare() in either direction.
template <typename T>
bool ordered_pair(const T& left, const T& right, int sign) {
  return sign < 0 ? left < right : right < left;
}

/// @brief The one type matrix behind value_t::is_less_than() (@p sign -1)
/// and value_t::is_greater_than() (@p sign +1).
///
/// A pair that only the other operand's type handles is asked of it with
/// the direction reversed.  The elements of a SEQUENCE compare through
/// is_less_than() in the order a value expression gives them, which runs
/// `a > b` as `b < a`; so an element comparison, and the error it may
/// raise, is the one the expression engine would make.
bool value_ordered(const value_t& left, const value_t& right, int sign) {
  // Null sorts first: nothing is less than it, and every other value is
  // greater.
  if (right.is_null())
    return sign > 0 && !left.is_null();

  auto precedes = [sign](const value_t& first, const value_t& second) {
    return sign < 0 ? first.is_less_than(second) : second.is_less_than(first);
  };

  switch (left.type()) {
  case value_t::VOID:
    return sign < 0;

  case value_t::BOOLEAN:
    if (right.is_boolean())
      return ordered_pair(left.as_boolean(), right.as_boolean(), sign);
    break;

  case value_t::DATETIME:
    if (right.is_datetime())
      return ordered_pair(left.as_datetime(), right.as_datetime(), sign);
    break;

  case value_t::DATE:
    if (right.is_date())
      return ordered_pair(left.as_date(), right.as_date(), sign);
    break;

  case value_t::INTEGER:
    switch (right.type()) {
    case value_t::INTEGER:
      return ordered_pair(left.as_long(), right.as_long(), sign);
    case value_t::AMOUNT:
    case value_t::BALANCE:
    case value_t::SEQUENCE:
      return value_ordered(right, left, -sign);
    default:
      break;
    }
    break;

  case value_t::AMOUNT:
    switch (right.type()) {
    case value_t::INTEGER:
      return has_sign(left.as_amount().compare(amount_t(right.as_long())), sign);
    case value_t::AMOUNT: {
      // Amounts in two commodities order by commodity, so that sort output
      // is deterministic; an amount with no commodity compares with any.
      const amount_t& lhs(left.as_amount());
      const amount_t& rhs(right.as_amount());
      if (lhs.has_commodity() && rhs.has_commodity() && lhs.commodity() != rhs.commodity())
        return sign < 0 ? commodity_t::compare_by_commodity()(&lhs, &rhs) < 0
                        : commodity_t::compare_by_commodity()(&rhs, &lhs) < 0;
      return ordered_pair(lhs, rhs, sign);
    }
    case value_t::BALANCE:
    case value_t::SEQUENCE:
      return value_ordered(right, left, -sign);
    default:
      break;
    }
    break;

  case value_t::BALANCE:
    switch (right.type()) {
    case value_t::INTEGER:
    case value_t::AMOUNT: {
      // A component in another commodity makes the comparison an error
      // whichever component is met first; otherwise every component
      // decides, in any order, except a component that netted to zero in
      // another commodity, which is no holding in the amount's and is left
      // out.  A balance of nothing else compares as zero; an empty one
      // compares false, as it always has.
      const balance_t& bal(left.as_balance());
      if (const amount_t* foreign = foreign_component(bal, right))
        throw_(amount_error,
               _f("Cannot compare amounts with different commodities: '%1%' and '%2%'") %
                   foreign->commodity() % right.as_amount().commodity());
      const amount_t rhs(right.is_amount() ? right.as_amount() : amount_t(right.as_long()));
      bool any = false;
      bool compared = false;
      for (const balance_t::amounts_map::value_type& pair : bal.amounts) {
        const amount_t& component(pair.second);
        any = true;
        if (component.is_realzero() && component.has_commodity() && rhs.has_commodity() &&
            component.commodity() != rhs.commodity())
          continue;
        compared = true;
        if (!has_sign(component.compare(rhs), sign))
          return false;
      }
      return compared || (any && has_sign(rhs.sign(), -sign));
    }
    case value_t::BALANCE:
      return ordered_pair(left.to_amount(), right.to_amount(), sign);
    default:
      break;
    }
    break;

  case value_t::COMMODITY:
    switch (right.type()) {
    case value_t::COMMODITY:
      return ordered_pair(left.to_string(), right.to_string(), sign);
    case value_t::STRING: {
      const auto* named = find_named_commodity(left.as_commodity().pool(), right.as_string());
      return ordered_pair(left.to_string(),
                          named ? named->base_symbol() : journal_symbol(right.as_string()), sign);
    }
    default:
      break;
    }
    break;

  case value_t::STRING:
    switch (right.type()) {
    case value_t::COMMODITY:
      return value_ordered(right, left, -sign);
    case value_t::STRING:
      return ordered_pair(left.as_string(), right.as_string(), sign);
    default:
      break;
    }
    break;

  case value_t::SEQUENCE:
    switch (right.type()) {
    case value_t::INTEGER:
    case value_t::AMOUNT: {
      // Every element decides; an empty sequence compares false.
      for (const value_t& value : left.as_sequence())
        if (!precedes(value, right))
          return false;
      return !left.as_sequence().empty();
    }
    case value_t::SEQUENCE: {
      // Pair by pair, not lexicographically.  The end is not antisymmetric:
      // less holds when this sequence ran out first or with the other,
      // greater only when this one is longer.
      value_t::sequence_t::const_iterator i = left.as_sequence().begin();
      value_t::sequence_t::const_iterator j = right.as_sequence().begin();
      for (; i != left.as_sequence().end() && j != right.as_sequence().end(); ++i, ++j)
        if (!precedes(*i, *j))
          return false;
      const bool longer = i != left.as_sequence().end();
      return sign < 0 ? !longer : longer;
    }
    default:
      break;
    }
    break;

  default:
    break;
  }

  if (sign < 0)
    add_error_context(_f("While comparing if %1% is less than %2%:") % left % right);
  else
    add_error_context(_f("While comparing if %1% is greater than %2%:") % left % right);
  throw_(value_error, _f("Cannot compare %1% to %2%") % left.label() % right.label());
}
} // namespace

/// @brief Test whether this value is strictly less than another.
///
/// Null is less than any non-null value and nothing is less than null, so
/// VOID sorts first.  The type matrix is value_ordered()'s; the Section 5
/// header lists it.  SEQUENCE < SEQUENCE holds only when every paired
/// element is less and this sequence is not longer; it is not
/// lexicographic.  Sorting by several keys uses sort_value_is_less_than(),
/// which is.
bool value_t::is_less_than(const value_t& val) const {
  return value_ordered(*this, val, -1);
}

/// @brief Test whether this value is strictly greater than another.
///
/// The mirror of is_less_than(), from value_ordered() with the direction
/// reversed: null is greater than nothing, and every non-null value is
/// greater than null.  SEQUENCE > SEQUENCE holds only when every paired
/// element is greater and this sequence is longer, so (10, 20) > (1, 2)
/// is false although (1, 2) < (10, 20) is true.
bool value_t::is_greater_than(const value_t& val) const {
  return value_ordered(*this, val, 1);
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
      if (const amount_t* sole = bal.sole_amount()) {
        // Because we are changing the current balance value to an amount
        // value, and because set_amount takes a reference (and that memory is
        // about to be repurposed), we must pass in a copy.
        set_amount(amount_t(*sole));
        return;
      } else if (bal.is_realzero()) {
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
      const string& str(as_string());
      const char* const end = str.data() + str.size();
      long result = 0;
      const auto [ptr, ec] = std::from_chars(str.data(), end, result);
      if (ec == std::errc() && ptr == end) {
        set_long(result);
        return;
      }
      break;
    }
    case AMOUNT:
      set_amount(amount_t(as_string()));
      return;
    case COMMODITY:
      if (const auto* commodity =
              find_named_commodity(*commodity_pool_t::current_pool, as_string())) {
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

  case SEQUENCE:
    switch (cast_type) {
    case STRING: {
      // Render the sequence using the same "(a, b, c)" form as print()
      // so that to_string() is consistent with streaming output.
      std::ostringstream buf;
      print(buf);
      set_string(buf.str());
      return;
    }
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
/// For INTEGER, the stored long is negated.
/// For AMOUNT and BALANCE, the corresponding in_place_negate() is called.
/// For SEQUENCE, each element is negated recursively.
/// COMMODITY, DATE, DATETIME, STRING, MASK, SCOPE and ANY throw value_error.
void value_t::in_place_negate() {
  switch (type()) {
  case VOID:
    return;
  case BOOLEAN:
    set_boolean(!as_boolean());
    return;
  case INTEGER:
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
/// in_place_not() turns a scalar into a BOOLEAN; a SEQUENCE stays a
/// SEQUENCE whose elements are each not-ed.  DATE and DATETIME follow
/// operator bool (a valid date is true).  The parser uses this to fold
/// a constant `not`/`!`; the evaluator's O_NOT uses operator bool.
void value_t::in_place_not() {
  switch (type()) {
  case BOOLEAN:
    set_boolean(!as_boolean());
    return;
  case INTEGER:
    set_boolean(!as_long());
    return;
  case DATETIME:
  case DATE:
    set_boolean(!bool(*this));
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

namespace {

/// One entry of an `--exchange` specification.
struct exchange_entry_t {
  const commodity_t* target; ///< The referent of the entry's target commodity B.
  bool selective;            ///< True when the entry was written `A:B` with a non-empty A.
  const commodity_t* source; ///< The referent of A; nullptr (matching nothing) when unknown.
  bool forced;               ///< True when the target carried a trailing `!`.
};

/// Parse a comma-separated exchange spec into its entries.
/// parse_price_expression() creates a target the pool does not hold yet.
std::vector<exchange_entry_t> parse_exchange_spec(const string& commodities, const bool add_prices,
                                                  const datetime_t& moment) {
  std::vector<exchange_entry_t> entries;

  std::vector<string> tokens;
  boost::split(tokens, commodities, boost::is_any_of(","));

  for (const string& name : tokens) {
    string target_name = name;
    string source_name;

    // Handle A:B syntax for selective conversion (convert A to B only)
    string::size_type colon_pos = name.find(':');
    if (colon_pos != string::npos) {
      source_name = boost::trim_copy(string(name, 0, colon_pos));
      target_name = string(name, colon_pos + 1);
    }
    bool selective = !source_name.empty();

    string::size_type name_len = target_name.length();
    bool is_forced = name_len > 0 && target_name[name_len - 1] == '!';
    string expr_name = is_forced ? string(target_name, 0, name_len - 1) : target_name;

    commodity_t* commodity =
        commodity_pool_t::current_pool->parse_price_expression(expr_name, add_prices, moment);
    DEBUG("commodity.exchange", "Pricing for commodity: " << commodity->symbol());
    commodity_t* src = selective ? commodity_pool_t::current_pool->find(source_name) : nullptr;
    entries.push_back(
        {&commodity->referent(), selective, src ? &src->referent() : nullptr, is_forced});
  }

  return entries;
}

/// Price @p amount with the first entry that matches it and can price it.
/// An `A:B` entry matches only amounts whose referent is A; any other entry
/// matches an amount not already in a listed target, or any amount when
/// forced.
std::optional<amount_t> exchange_amount(const amount_t& amount,
                                        const std::vector<exchange_entry_t>& entries,
                                        const datetime_t& moment) {
  const commodity_t* referent = &amount.commodity().referent();
  bool in_a_target = std::any_of(entries.begin(), entries.end(),
                                 [&](const exchange_entry_t& e) { return e.target == referent; });
  for (const exchange_entry_t& entry : entries) {
    bool matches = entry.selective ? entry.source == referent : entry.forced || !in_a_target;
    if (!matches)
      continue;
    if (auto val = amount.value(moment, entry.target))
      return val;
  }
  return std::nullopt;
}

} // namespace

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
/// colon, or equals sign), this method drops a trailing `!` (forcing a lone
/// target changes nothing) and shortcuts to value() for efficiency.
///
/// Each AMOUNT, and each component of a BALANCE, is converted by the first
/// entry that matches it and can price it.  An `A:B` entry matches only
/// amounts in commodity A, and matches nothing when the commodity pool does
/// not know A; any other entry matches an amount that is not already in one
/// of the listed target commodities, or any amount when it is forced.
/// Components no entry converts are kept as they are.
value_t value_t::exchange_commodities(const std::string& commodities, const bool add_prices,
                                      const datetime_t& moment) const {
  if (type() == SEQUENCE) {
    value_t temp;
    for (const value_t& value : as_sequence())
      temp.push_back(value.exchange_commodities(commodities, add_prices, moment));
    return temp;
  }

  // If we are repricing to just a single commodity, with no price
  // expression, skip the expensive logic below.  With one target, forcing
  // changes nothing: an amount already in the target converts to itself.
  if (commodities.find_first_of(",=:") == string::npos) {
    string target(commodities);
    if (!target.empty() && target.back() == '!')
      target.pop_back();
    return value(moment, commodity_pool_t::current_pool->find_or_create(target));
  }

  // Parse whatever the value's type: a caller may convert a plain integer
  // only to record the spec's `=price` entries.
  std::vector<exchange_entry_t> entries = parse_exchange_spec(commodities, add_prices, moment);

  switch (type()) {
  case AMOUNT:
    DEBUG("commodity.exchange", "We have an amount: " << as_amount());
    if (auto val = exchange_amount(as_amount(), entries, moment)) {
      DEBUG("commodity.exchange", "Re-priced amount is: " << *val);
      return *val;
    }
    break;

  case BALANCE: {
    balance_t temp;
    bool repriced = false;

    DEBUG("commodity.exchange", "We have a balance: " << as_balance());
    // Components priced into one commodity collapse: merge() keeps the
    // result independent of the map's order.
    auto reprice = [&](const amount_t& amount) {
      if (auto val = exchange_amount(amount, entries, moment)) {
        DEBUG("commodity.exchange", "Re-priced member amount is: " << *val);
        temp.merge(*val);
        repriced = true;
      } else {
        temp.merge(amount);
      }
    };
    // The map's order is a hash order: visit_components() makes a
    // valuation error name the same component on every run.
    as_balance().visit_components(reprice);

    if (repriced) {
      DEBUG("commodity.exchange", "Re-priced balance is: " << temp);
      return temp;
    }
    break;
  }

  default:
    break;
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
    if (is_any<expr_t::ptr_op_t>())
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
/// @param negative_color  The ANSI escape sequence with which
///                      AMOUNT_PRINT_COLORIZE renders negative values;
///                      empty renders them without color.
void value_t::print(std::ostream& _out, const int first_width, const int latter_width,
                    const uint_least8_t flags, const string& negative_color) const {
  // A balance lays out its own columns through justify(), which leaves the
  // stream's adjustment flags alone but writes piecewise, so a width the
  // caller set would pad only the first piece: hence the direct path only
  // when no width is set, and the buffered one below otherwise.
  if (is_balance() && _out.width() == 0) {
    as_balance().print(_out, first_width, latter_width, flags, negative_color);
    return;
  }

  // We buffer into a local ostringstream so that width and justification
  // settings do not leak into the caller's stream.
  std::ostringstream out;

  // Set column width for simple scalar types.  AMOUNT, BALANCE, COMMODITY
  // and STRING handle their own justification via the justify() helper so
  // they are excluded here.
  if (first_width > 0 && (!is_amount() || as_amount().is_zero()) && !is_balance() &&
      !is_commodity() && !is_string()) {
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
    out << format_datetime(as_datetime(), FMT_WRITTEN);
    break;

  case DATE:
    out << format_date(as_date(), FMT_WRITTEN);
    break;

  case INTEGER:
    if (flags & AMOUNT_PRINT_COLORIZE && as_long() < 0) {
      // justify() pads the number itself, so drop the width set above.
      out.width(0);
      justify(out, to_string(), first_width, flags & AMOUNT_PRINT_RIGHT_JUSTIFY, negative_color);
    } else {
      out << as_long();
    }
    break;

  case AMOUNT: {
    if (as_amount().is_zero()) {
      out << 0;
    } else {
      std::ostringstream buf;
      as_amount().print(buf, flags);
      justify(out, buf.str(), first_width, flags & AMOUNT_PRINT_RIGHT_JUSTIFY,
              (flags & AMOUNT_PRINT_COLORIZE && as_amount().sign() < 0) ? negative_color
                                                                        : empty_string);
    }
    break;
  }

  case BALANCE:
    as_balance().print(out, first_width, latter_width, flags, negative_color);
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

      value.print(out, first_width, latter_width, flags, negative_color);
    }
    out << ')';
    break;
  }

  case SCOPE:
    out << "<#SCOPE>";
    break;
  case ANY:
    if (is_any<expr_t::ptr_op_t>()) {
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
    if (is_any<expr_t::ptr_op_t>())
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
