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
 * @file   amount.cc
 * @author John Wiegley
 *
 * @ingroup math
 *
 * @brief Implements the infinite-precision commoditized amount_t type.
 *
 * This file provides the full implementation of amount_t, Ledger's
 * fundamental numeric type.  Every monetary value, share count, or time
 * quantity that flows through the system is represented as an amount_t.
 *
 * Internally, each amount stores its numeric value as a GMP rational
 * number (mpq_t).  Rational arithmetic guarantees that addition,
 * subtraction, multiplication, and division never introduce rounding
 * error -- a critical property for double-entry accounting where the
 * sum of all postings in a transaction must balance to exactly zero.
 *
 * To avoid the cost of deep-copying large GMP integers on every
 * assignment, amounts use a copy-on-write (COW) strategy backed by a
 * reference-counted @c bigint_t structure.  Multiple amount_t objects
 * may share the same bigint_t; a private copy is made only when a
 * mutating operation (arithmetic, rounding, etc.) is about to modify
 * the value.
 *
 * The file is organized into the following logical sections:
 *
 *   - Internal storage: bigint_t, GMP temporaries, initialization
 *   - Copy-on-write helpers: _copy, _dup, _clear, _release
 *   - Construction from primitive types (double, long, unsigned long)
 *   - Comparison operators (compare, operator==)
 *   - Arithmetic operators (+, -, *, /)
 *   - Precision management and display precision
 *   - Rounding family: round, roundto, truncate, floor, ceiling, unround
 *   - Unit reduction/unreduction (e.g. seconds to minutes)
 *   - Commodity operations: value, price, annotation
 *   - Parsing: parse from stream, parse_conversion
 *   - Printing: print with full commodity formatting, stream_out_mpq
 *   - Validation
 */

#include <system.hh>
#include <math.h>

#include "amount.h"
#include "commodity.h"
#include "annotate.h"
#include "pool.h"

namespace ledger {

bool amount_t::stream_fullstrings = false;

/*----------------------------------------------------------------------*/
/*  Internal storage: GMP temporaries and copy-on-write bigint_t        */
/*----------------------------------------------------------------------*/

// These global temporaries are pre-initialized for the sake of
// efficiency, and are reused over and over again.
// TODO: These globals are not thread-safe. Use thread_local when
// per-thread initialization can be guaranteed.
static mpz_t temp;
static mpq_t tempq;
static mpfr_t tempf;

/**
 * @brief Copy-on-write storage wrapper around a GMP rational (mpq_t).
 *
 * Every amount_t stores its numeric value indirectly through a pointer
 * to a bigint_t.  The struct holds:
 *
 *   - @c val   -- the GMP rational number (numerator / denominator).
 *   - @c prec  -- the number of decimal digits of precision that were
 *                  present when the value was parsed or computed.  This
 *                  is *not* used by GMP itself (which always stores the
 *                  exact rational), but drives display formatting and
 *                  commodity-precision tracking.
 *   - @c refc  -- a manual reference count.  When greater than one,
 *                  the bigint_t is shared among multiple amount_t
 *                  objects and must be duplicated before mutation
 *                  (see @c amount_t::_dup).
 *
 * Two flags control special behavior:
 *   - @c BIGINT_KEEP_PREC  -- the stored precision should be used for
 *     display rather than the commodity's default precision.  This is
 *     set when an amount is parsed with PARSE_NO_MIGRATE or after an
 *     explicit @c unround() call.
 *   - @c BIGINT_COST_PREC  -- set by set_cost_precision() on an amount
 *     inferred from a cost (xact.cc).  When the commodity's display
 *     precision would show the amount as zero, or the commodity has no
 *     precision yet, is_zero() and print() use the stored precision
 *     instead (#1692).  operator+= and operator-= clear it on their
 *     result (#717).
 */
struct amount_t::bigint_t : public flags::supports_flags<> {
#define BIGINT_KEEP_PREC 0x02
#define BIGINT_COST_PREC 0x04

  mpq_t val;
  precision_t prec;
  uint_least32_t refc;

  /** @brief Convenience macro: access the mpq_t value from a bigint_t pointer. */
#define MP(bigint) ((bigint)->val)

  bigint_t() : prec(0), refc(1) {
    mpq_init(val);
    TRACE_CTOR(bigint_t, "");
  }
  bigint_t(const bigint_t& other) : supports_flags<>(other.flags()), prec(other.prec), refc(1) {
    mpq_init(val);
    mpq_set(val, other.val);
    TRACE_CTOR(bigint_t, "copy");
  }
  ~bigint_t() {
    TRACE_DTOR(bigint_t);
    assert(refc == 0);
    mpq_clear(val);
  }

  /** @brief Sanity-check that precision and flags are within expected bounds. */
  bool valid() const {
    if (prec > 1024) {
      DEBUG("ledger.validate", "amount_t::bigint_t: prec > 1024");
      return false;
    }
    if (flags() & ~(BIGINT_KEEP_PREC | BIGINT_COST_PREC)) {
      DEBUG("ledger.validate", "amount_t::bigint_t: flags() & ~(KEEP_PREC | COST_PREC)");
      return false;
    }
    return true;
  }
};

bool amount_t::is_initialized = false;

/*----------------------------------------------------------------------*/
/*  Printing: stream_out_mpq -- rational-to-decimal string conversion   */
/*----------------------------------------------------------------------*/

namespace {

/// Scratch integers for format_mpq_exact(), within_half_unit() and the
/// time-colon minutes of stream_out_mpq(), initialized by
/// amount_t::initialize() alongside the other GMP temporaries.  None of
/// these users calls another while holding a value in them.
mpz_t out_scale;
mpz_t out_quot;
mpz_t out_rem;

/**
 * @brief Set @p quot to @p num / @p den rounded to the nearest integer,
 * ties away from zero: the single tie rule of Ledger.
 *
 * @p rem is scratch.  @p den must be positive.  @p quot may alias @p num,
 * because the sign is taken from the remainder (with truncating division
 * the remainder carries the dividend's sign whenever it is nonzero);
 * neither may alias @p den.  Used by both format_mpq_exact() and, through
 * round_quantity(), amount_t::in_place_roundto(), so a displayed figure
 * always equals its roundto().
 */
void round_quotient_half_away(mpz_ptr quot, mpz_ptr rem, mpz_srcptr num, mpz_srcptr den) {
  mpz_tdiv_qr(quot, rem, num, den);
  const int sign = mpz_sgn(rem);
  if (sign == 0)
    return;
  mpz_abs(rem, rem);
  mpz_mul_2exp(rem, rem, 1);
  if (mpz_cmp(rem, den) >= 0) { // half a unit or more: away from zero
    if (sign < 0)
      mpz_sub_ui(quot, quot, 1);
    else
      mpz_add_ui(quot, quot, 1);
  }
}

/**
 * @brief Whether @p quant lies within half a unit of zero at @p places
 * decimals, an exact half included: the tolerance a transaction may leave
 * over and still balance (regress/1765.test).
 */
bool within_half_unit(mpq_srcptr quant, amount_t::precision_t places) {
  mpz_ui_pow_ui(out_scale, 10, places);
  mpz_mul(out_quot, mpq_numref(quant), out_scale);
  mpz_abs(out_quot, out_quot);
  mpz_mul_2exp(out_quot, out_quot, 1);
  return mpz_cmp(out_quot, mpq_denref(quant)) <= 0;
}

/// How round_quantity() settles the digits it drops.
enum class rounding_t { toward_zero, half_away_from_zero };

/**
 * @brief Round @p quant in place to @p places decimals, toward zero or
 * half away from zero as @p mode says.
 *
 * Zero @p places rounds to an integer; negative @p places rounds to
 * tens, hundreds, and so on.  The integer division is mpz_tdiv_q() or
 * round_quotient_half_away(), the one tie rule Ledger's printer also
 * uses.  The file-scope temp holds 10^|places| and tempq's numerator and
 * denominator are the quotient and remainder scratch, so @p quant must
 * not be tempq.
 */
void round_quantity(mpq_ptr quant, int places, rounding_t mode) {
  mpz_t& scale(temp);
  if (places)
    mpz_ui_pow_ui(scale, 10, labs(places));

  if (places > 0) {
    mpz_mul(mpq_numref(quant), mpq_numref(quant), scale);
  } else if (places < 0) {
    mpz_mul(mpq_denref(quant), mpq_denref(quant), scale);
  }

  auto whole(mpq_numref(tempq));
  if (mode == rounding_t::toward_zero)
    mpz_tdiv_q(whole, mpq_numref(quant), mpq_denref(quant));
  else
    round_quotient_half_away(whole, mpq_denref(tempq), mpq_numref(quant), mpq_denref(quant));

  if (places > 0) {
    mpq_set_num(quant, whole);
    mpq_set_den(quant, scale);
    mpq_canonicalize(quant);
  } else if (places == 0) {
    mpq_set_z(quant, whole);
  } else {
    mpq_set_ui(quant, 0, 1);
    mpz_mul(mpq_numref(quant), whole, scale);
  }
}

/**
 * @brief Render @p quant with exactly @p precision decimals into
 * @p digits, rounding the exact rational to nearest with ties away from
 * zero (round_quotient_half_away()), the rule roundto() and --round use.
 *
 * A negative value that rounds to zero keeps its sign ("-0.00").
 */
void format_mpq_exact(std::string& digits, mpq_t quant, amount_t::precision_t precision) {
  mpz_ui_pow_ui(out_scale, 10, precision);
  mpz_mul(out_quot, mpq_numref(quant), out_scale);
  round_quotient_half_away(out_quot, out_rem, out_quot, mpq_denref(quant));

  // mpz_get_str wants room for the digits, a sign and the terminator.
  char stack_buf[64];
  std::string heap_buf;
  char* buf = stack_buf;
  const std::size_t needed = mpz_sizeinbase(out_quot, 10) + 2;
  if (needed > sizeof(stack_buf)) {
    heap_buf.resize(needed);
    buf = &heap_buf[0];
  }
  mpz_get_str(buf, 10, out_quot);

  const char* p = buf;
  if (*p == '-')
    ++p; // the sign is restored from the numerator below
  const std::size_t len = std::strlen(p);

  digits.clear();
  if (mpz_sgn(mpq_numref(quant)) < 0)
    digits += '-';
  if (precision == 0) {
    digits.append(p, len);
  } else if (len <= precision) {
    digits += "0.";
    digits.append(precision - len, '0');
    digits.append(p, len);
  } else {
    digits.append(p, len - precision);
    digits += '.';
    digits.append(p + (len - precision), precision);
  }
}

/**
 * @brief Convert a GMP rational to a human-readable decimal string.
 *
 * This is the core output routine used by amount_t::print().  It
 * converts the exact rational @a quant to a decimal representation
 * at the requested @a precision and writes the result to @a out,
 * respecting the commodity's display conventions:
 *
 *   - Thousands separators (commas, periods, or apostrophes depending
 *     on the commodity style).
 *   - Decimal comma vs. decimal point.
 *   - Time-colon notation for "h" and "m" commodities (e.g. 1:30h
 *     instead of 1.50h).
 *
 * The digits come from format_mpq_exact(), exact integer arithmetic on
 * the rational rounded half away from zero; the separators
 * and the time-colon conversion are then applied to that string.
 *
 * @param out        Output stream to write the formatted number to.
 * @param quant      The GMP rational value to format.
 * @param precision  Number of decimal places to emit.
 * @param zeros_prec If non-negative, trailing zeros beyond this many
 *                   decimal places are stripped.  A value of -1 (the
 *                   default) preserves all trailing zeros up to
 *                   @a precision.
 * @param comm       Optional commodity reference whose display flags
 *                   control thousands separators and decimal style.
 */
void stream_out_mpq(std::ostream& out, mpq_t quant, amount_t::precision_t precision,
                    int zeros_prec = -1, const commodity_t* comm = nullptr) {
#if DEBUG_ON
  IF_DEBUG("amount.convert") {
    char* tbuf = mpq_get_str(nullptr, 10, quant);
    DEBUG("amount.convert", "Rational to convert = " << tbuf);
    std::free(tbuf);
  }
#endif

  std::string digits;
  format_mpq_exact(digits, quant, precision);

  DEBUG("amount.convert", "rendered = " << digits << " (precision " << precision << ", zeros_prec "
                                        << zeros_prec << ")");

  // Trim fractional zeros beyond zeros_prec digits, and the decimal point
  // itself when nothing is left after it.
  if (zeros_prec >= 0) {
    const std::string::size_type point = digits.find('.');
    if (point != std::string::npos && point > 0) {
      std::ptrdiff_t index = static_cast<std::ptrdiff_t>(digits.size());
      const std::ptrdiff_t limit = static_cast<std::ptrdiff_t>(point) + 1 + zeros_prec;
      while (--index >= limit && digits[static_cast<std::size_t>(index)] == '0') {}
      digits.resize(static_cast<std::size_t>(index + 1));
      if (index >= static_cast<std::ptrdiff_t>(point) + zeros_prec &&
          digits[static_cast<std::size_t>(index)] == '.')
        digits.resize(static_cast<std::size_t>(index));
    }
  }

  if (!comm) {
    out << digits;
    return;
  }

  const bool decimal_comma =
      commodity_t::decimal_comma_by_default || comm->has_flags(COMMODITY_STYLE_DECIMAL_COMMA);
  const bool time_colon =
      (commodity_t::time_colon_by_default || comm->has_flags(COMMODITY_STYLE_TIME_COLON)) &&
      (comm->symbol() == "h" || comm->symbol() == "m");

  // Under time-colon notation the fraction becomes the next smaller time
  // unit (0.50h -> 0:30h, meaning 30 min), rounded half up and padded with
  // zeros to the width of the fraction.  This is done in GMP: --unround can
  // show dozens of digits, which used to overflow a machine word.  The
  // scratch integers of format_mpq_exact() are free again by now.  A
  // fraction that rounds to 60 carries into the integer digits, which must
  // happen before they are counted for digit grouping.
  std::string minutes;
  if (time_colon) {
    const std::string::size_type point = digits.find('.');
    if (point != std::string::npos) {
      const std::string frac(digits, point + 1);
      if (frac.empty())
        mpz_set_ui(out_quot, 0);
      else
        mpz_set_str(out_quot, frac.c_str(), 10);
      mpz_mul_ui(out_quot, out_quot, 60);
      mpz_ui_pow_ui(out_scale, 10, frac.size());
      mpz_fdiv_q_ui(out_rem, out_scale, 2);
      mpz_add(out_quot, out_quot, out_rem);
      mpz_fdiv_q(out_quot, out_quot, out_scale);
      if (mpz_cmp_ui(out_quot, 60) == 0) {
        mpz_set_ui(out_quot, 0);
        std::string::size_type i = point;
        while (i > 0 && digits[i - 1] == '9')
          digits[--i] = '0';
        if (i == 0 || digits[i - 1] == '-')
          digits.insert(i, 1, '1');
        else
          ++digits[i - 1];
      }
      char* minutes_buf = mpz_get_str(nullptr, 10, out_quot);
      minutes = minutes_buf;
      std::free(minutes_buf);
      if (minutes.size() < frac.size())
        minutes.insert(0, frac.size() - minutes.size(), '0');
    }
  }

  int integer_digits = 0;
  if (comm->has_flags(COMMODITY_STYLE_THOUSANDS)) {
    // Count the number of integer digits
    for (const char c : digits) {
      if (c == '.')
        break;
      else if (c != '-')
        integer_digits++;
    }
  }

  std::string result;
  result.reserve(digits.size() + 8);

  for (const char* p = digits.c_str(); *p; p++) {
    if (*p == '.') {
      if (time_colon) {
        result += ':';
        result += minutes;
        break; // fractional part fully handled
      } else if (decimal_comma) {
        result += ',';
      } else {
        result += *p;
      }
      assert(integer_digits <= 3);
    } else if (*p == '-') {
      result += *p;
    } else {
      result += *p;

      if (integer_digits > 3 && --integer_digits % 3 == 0) {
        if (time_colon)
          result += ':';
        else if (comm->has_flags(COMMODITY_STYLE_THOUSANDS_APOSTROPHE))
          result += '\'';
        else if (decimal_comma)
          result += '.';
        else
          result += ',';
      }
    }
  }
  out << result;
}
} // namespace

/*----------------------------------------------------------------------*/
/*  Initialization and shutdown                                         */
/*----------------------------------------------------------------------*/

/**
 * @brief Initialize the GMP/MPFR temporary variables and commodity pool.
 *
 * Must be called once at program startup before any amount_t objects
 * are created.  Sets up the reusable GMP/MPFR temporaries and creates
 * the global commodity pool with built-in commodities ("s" for seconds,
 * "%" for percentiles).
 */
void amount_t::initialize() {
  if (!is_initialized) {
    mpz_init(temp);
    mpq_init(tempq);
    mpz_init(out_scale);
    mpz_init(out_quot);
    mpz_init(out_rem);
    mpfr_init(tempf);

    commodity_pool_t::current_pool.reset(new commodity_pool_t);

    // Add time commodity conversions, so that timelogs may be parsed
    // in terms of seconds, but reported as minutes or hours.
    if (commodity_t* commodity = commodity_pool_t::current_pool->create("s"))
      commodity->add_flags(COMMODITY_BUILTIN | COMMODITY_NOMARKET);
    else
      assert(false);

    // Add a "percentile" commodity
    if (commodity_t* commodity = commodity_pool_t::current_pool->create("%")) {
      commodity->add_flags(COMMODITY_BUILTIN | COMMODITY_NOMARKET);
      commodity_pool_t::current_pool->percent_commodity = commodity;
    } else {
      assert(false);
    }

    is_initialized = true;
  }
}

/**
 * @brief Release all GMP/MPFR temporaries and destroy the commodity pool.
 *
 * Called once at program exit.  After this call, no amount_t operations
 * are valid.
 */
void amount_t::shutdown() {
  if (is_initialized) {
    mpz_clear(temp);
    mpq_clear(tempq);
    mpz_clear(out_scale);
    mpz_clear(out_quot);
    mpz_clear(out_rem);
    mpfr_clear(tempf);

    commodity_pool_t::current_pool.reset();

    is_initialized = false;
  }
}

/*----------------------------------------------------------------------*/
/*  Copy-on-write helpers                                               */
/*----------------------------------------------------------------------*/

/**
 * @brief Share another amount's bigint_t.
 *
 * Releases this amount's own bigint_t, then points at @p amt's and
 * increments its reference count; the first mutation of either amount
 * makes a private copy (see _dup).
 */
void amount_t::_copy(const amount_t& amt) {
  VERIFY(amt.valid());

  if (quantity != amt.quantity) {
    if (quantity)
      _release();

    quantity = amt.quantity;
    DEBUG("amount.refs", quantity << " refc++, now " << (quantity->refc + 1));
    quantity->refc++;
  }
  commodity_ = amt.commodity_;

  VERIFY(valid());
}

/**
 * @brief Ensure this amount has an exclusive (unshared) bigint_t.
 *
 * If the current bigint_t has a reference count greater than one, a
 * private copy is created so that subsequent mutations do not affect
 * other amount_t objects that share the same storage.  This is the
 * "copy" half of copy-on-write.
 */
void amount_t::_dup() {
  VERIFY(valid());

  if (quantity->refc > 1) {
    bigint_t* q = new bigint_t(*quantity);
    _release();
    quantity = q;
  }

  VERIFY(valid());
}

/**
 * @brief Reset this amount to the uninitialized (null) state.
 *
 * Releases the reference to the current bigint_t and sets both the
 * quantity pointer and commodity pointer to nullptr.
 */
void amount_t::_clear() {
  if (quantity) {
    _release();
    quantity = nullptr;
    commodity_ = nullptr;
  } else {
    assert(!commodity_);
  }
}

/**
 * @brief Decrement the bigint_t reference count, freeing if it reaches zero.
 *
 * The last release deletes it.
 */
void amount_t::_release() {
  VERIFY(valid());

  DEBUG("amount.refs", quantity << " refc--, now " << (quantity->refc - 1));

  if (--quantity->refc == 0) {
    checked_delete(quantity);
    quantity = nullptr;
    commodity_ = nullptr;
  }

  VERIFY(valid());
}

/*----------------------------------------------------------------------*/
/*  Construction from primitive types                                    */
/*----------------------------------------------------------------------*/

/**
 * @brief Construct an uncommoditized amount from a double.
 *
 * The double is converted to a GMP rational via mpq_set_d.  Because
 * IEEE 754 doubles cannot represent most decimals exactly, the
 * precision is set to @c extend_by_digits as a reasonable
 * approximation of the significant digits.
 */
amount_t::amount_t(const double val) : commodity_(nullptr) {
  quantity = new bigint_t;
  mpq_set_d(MP(quantity), val);
  quantity->prec = extend_by_digits; // an approximation
  TRACE_CTOR(amount_t, "const double");
}

/** @brief Construct an uncommoditized amount from an unsigned long. */
amount_t::amount_t(const unsigned long val) : commodity_(nullptr) {
  quantity = new bigint_t;
  mpq_set_ui(MP(quantity), val, 1);
  TRACE_CTOR(amount_t, "const unsigned long");
}

/** @brief Construct an uncommoditized amount from a signed long. */
amount_t::amount_t(const long val) : commodity_(nullptr) {
  quantity = new bigint_t;
  mpq_set_si(MP(quantity), val, 1);
  TRACE_CTOR(amount_t, "const long");
}

amount_t& amount_t::operator=(const amount_t& amt) {
  if (this != &amt) {
    if (amt.quantity)
      _copy(amt);
    else if (quantity)
      _clear();
  }
  return *this;
}

/*----------------------------------------------------------------------*/
/*  Comparison operators                                                */
/*----------------------------------------------------------------------*/

/**
 * @brief Three-way comparison of two amounts.
 *
 * Returns a negative value if *this < @a amt, zero if equal, and a
 * positive value if *this > @a amt.  Both amounts must be initialized
 * and share the same commodity (or both be uncommoditized); otherwise
 * an exception is thrown.
 *
 * The comparison is performed on the exact GMP rationals, so no
 * precision is lost regardless of display formatting.
 */
int amount_t::compare(const amount_t& amt) const {
  VERIFY(amt.valid());

  if (!quantity || !amt.quantity) {
    if (quantity)
      throw_(amount_error, _("Cannot compare an amount to an uninitialized amount"));
    else if (amt.quantity)
      throw_(amount_error, _("Cannot compare an uninitialized amount to an amount"));
    else
      throw_(amount_error, _("Cannot compare two uninitialized amounts"));
  }

  if (has_commodity() && amt.has_commodity() && commodity() != amt.commodity()) {
    throw_(amount_error, _f("Cannot compare amounts with different commodities: '%1%' and '%2%'") %
                             commodity() % amt.commodity());
  }

  return mpq_cmp(MP(quantity), MP(amt.quantity));
}

/**
 * @brief Test two amounts for exact equality.
 *
 * Unlike compare(), equality testing does *not* throw when the
 * commodities differ -- it simply returns false, since "$10" and
 * "10 EUR" are never equal regardless of exchange rates.  Two
 * uninitialized (null) amounts are considered equal.
 */
bool amount_t::operator==(const amount_t& amt) const {
  if ((quantity && !amt.quantity) || (!quantity && amt.quantity))
    return false; // NOLINT(bugprone-branch-clone)
  else if (!quantity && !amt.quantity)
    return true;
  else if (commodity() != amt.commodity())
    return false;

  return mpq_equal(MP(quantity), MP(amt.quantity));
}

/*----------------------------------------------------------------------*/
/*  Arithmetic operators                                                */
/*----------------------------------------------------------------------*/

/**
 * @brief The shared body of operator+= and operator-=: add @p amt to
 * this amount, or subtract it when @p subtract is true, in place.
 */
amount_t& amount_t::add_or_subtract(const amount_t& amt, bool subtract) {
  VERIFY(amt.valid());

  if (!quantity || !amt.quantity) {
    if (quantity)
      throw_(amount_error, subtract ? _("Cannot subtract an uninitialized amount from an amount")
                                    : _("Cannot add an uninitialized amount to an amount"));
    else if (amt.quantity)
      throw_(amount_error, subtract ? _("Cannot subtract an amount from an uninitialized amount")
                                    : _("Cannot add an amount to an uninitialized amount"));
    else
      throw_(amount_error, subtract ? _("Cannot subtract two uninitialized amounts")
                                    : _("Cannot add two uninitialized amounts"));
  }

  if (has_commodity() && amt.has_commodity() && commodity() != amt.commodity())
    throw_(amount_error,
           boost::format(subtract
                             ? _("Subtracting amounts with different commodities: '%1%' != '%2%'")
                             : _("Adding amounts with different commodities: '%1%' != '%2%'")) %
               commodity() % amt.commodity());

  _dup();

  (subtract ? mpq_sub : mpq_add)(MP(quantity), MP(quantity), MP(amt.quantity));

  if (has_commodity() == amt.has_commodity())
    if (quantity->prec < amt.quantity->prec)
      quantity->prec = amt.quantity->prec;

  // Clear BIGINT_COST_PREC on the result: this flag marks amounts that
  // were produced by set_cost_precision() on an auto-inferred null
  // posting (issue #1692), and it changes how is_zero() and print()
  // treat the amount against a zero-precision commodity.  Letting the
  // flag propagate through sums would cause derived running totals and
  // rounding residuals to inherit it spuriously, emitting ghost
  // adjustments (issue #717).
  if (quantity->has_flags(BIGINT_COST_PREC))
    quantity->drop_flags(BIGINT_COST_PREC);

  return *this;
}

/**
 * @brief Add another amount to this one in place.
 *
 * Both amounts must be initialized and share the same commodity.
 * The result's precision is the maximum of the two operands'
 * precisions, so that no decimal information is lost in the sum.
 *
 * Triggers copy-on-write via _dup() before mutating the GMP value.
 */
amount_t& amount_t::operator+=(const amount_t& amt) {
  return add_or_subtract(amt, false);
}

/**
 * @brief Subtract another amount from this one in place.
 *
 * Same commodity and precision rules as operator+=.
 */
amount_t& amount_t::operator-=(const amount_t& amt) {
  return add_or_subtract(amt, true);
}

/**
 * @brief Multiply this amount by another, with commodity propagation.
 *
 * Precision of the result is the sum of the two operands' precisions,
 * reflecting that multiplying a number with P1 decimal places by one
 * with P2 decimal places can produce up to P1+P2 decimal places.
 *
 * If the result would exceed the commodity's precision plus
 * @c extend_by_digits, it is clamped to avoid unbounded growth in
 * long chains of multiplications.
 *
 * The "%" commodity has no special meaning here: a price or a quantity
 * in "%" multiplies at face value.  Value expressions read "%" as a
 * fraction (expr_t::op_t::calc).
 *
 * @param amt              The multiplier amount.
 * @param ignore_commodity If true, do not propagate the multiplier's
 *                         commodity to the result.  Used internally
 *                         by value() when computing market prices.
 */
amount_t& amount_t::multiply(const amount_t& amt, bool ignore_commodity) {
  VERIFY(amt.valid());

  if (!quantity || !amt.quantity) {
    if (quantity)
      throw_(amount_error, _("Cannot multiply an amount by an uninitialized amount"));
    else if (amt.quantity)
      throw_(amount_error, _("Cannot multiply an uninitialized amount by an amount"));
    else
      throw_(amount_error, _("Cannot multiply two uninitialized amounts"));
  }

  _dup();

  mpq_mul(MP(quantity), MP(quantity), MP(amt.quantity));
  quantity->prec = static_cast<precision_t>(quantity->prec + amt.quantity->prec);

  if (!has_commodity() && !ignore_commodity)
    commodity_ = amt.commodity_;

  if (has_commodity() && !keep_precision()) {
    precision_t comm_prec = commodity().precision();
    if (quantity->prec > comm_prec + extend_by_digits)
      quantity->prec = static_cast<precision_t>(comm_prec + extend_by_digits);
  }

  return *this;
}

/**
 * @brief Divide this amount by another in place.
 *
 * Division extends precision by @c extend_by_digits (default 6) beyond
 * the sum of the two operands' precisions.  This extra headroom
 * captures fractional digits that arise from non-terminating decimals
 * (e.g. 1/3) while keeping the rational exact under the hood.
 *
 * The "%" commodity has no special meaning here: a price or a quantity
 * in "%" divides at face value.  Value expressions read "%" as a
 * fraction (expr_t::op_t::calc).
 *
 * As with multiplication, the result precision is clamped to
 * commodity precision + extend_by_digits when a commodity is present
 * and BIGINT_KEEP_PREC is not set.
 */
amount_t& amount_t::operator/=(const amount_t& amt) {
  VERIFY(amt.valid());

  if (!quantity || !amt.quantity) {
    if (quantity)
      throw_(amount_error, _("Cannot divide an amount by an uninitialized amount"));
    else if (amt.quantity)
      throw_(amount_error, _("Cannot divide an uninitialized amount by an amount"));
    else
      throw_(amount_error, _("Cannot divide two uninitialized amounts"));
  }

  // Use is_realzero() rather than operator bool() (== is_nonzero() == !is_zero()):
  // the latter treats an amount as zero when its exact value rounds to zero at
  // the commodity's display precision, but division is a mathematical operation
  // on the exact rational value.  A divide-by-zero happens only when the actual
  // value is zero, not when it merely displays as zero (issue #1789).
  if (amt.is_realzero())
    throw_(amount_error, _("Divide by zero"));

  _dup();

  // Increase the value's precision, to capture fractional parts after
  // the divide.  Round up in the last position.

  mpq_div(MP(quantity), MP(quantity), MP(amt.quantity));
  quantity->prec = static_cast<precision_t>(quantity->prec + amt.quantity->prec + extend_by_digits);

  if (!has_commodity())
    commodity_ = amt.commodity_;

  // If this amount has a commodity, and we're not dealing with plain
  // numbers, or internal numbers (which keep full precision at all
  // times), then round the number to within the commodity's precision
  // plus six places.

  if (has_commodity() && !keep_precision()) {
    precision_t comm_prec = commodity().precision();
    if (quantity->prec > comm_prec + extend_by_digits)
      quantity->prec = static_cast<precision_t>(comm_prec + extend_by_digits);
  }

  return *this;
}

/**
 * @brief The fraction a percentage denotes: 19% is 0.19.
 *
 * Returns the quantity divided by 100, without a commodity, at two more
 * decimal places.  Value expressions use it to read "%" as a fraction
 * (expr_t::op_t::calc).
 */
amount_t amount_t::percent_fraction() const {
  if (!quantity)
    throw_(amount_error, _("Cannot take the fraction of an uninitialized amount"));

  amount_t fraction(number());
  fraction._dup();
  mpz_mul_ui(mpq_denref(MP(fraction.quantity)), mpq_denref(MP(fraction.quantity)), 100);
  mpq_canonicalize(MP(fraction.quantity));
  fraction.quantity->prec = static_cast<precision_t>(fraction.quantity->prec + 2);
  return fraction;
}

/*----------------------------------------------------------------------*/
/*  Precision management                                                */
/*----------------------------------------------------------------------*/

/** @brief Return the stored decimal precision of this amount. */
amount_t::precision_t amount_t::precision() const {
  if (!quantity)
    throw_(amount_error, _("Cannot determine precision of an uninitialized amount"));

  return quantity->prec;
}

/**
 * @brief Check whether this amount retains its full internal precision
 *        for display, ignoring commodity display precision.
 */
bool amount_t::keep_precision() const {
  if (!quantity)
    throw_(amount_error, _("Cannot determine if precision of an uninitialized amount is kept"));

  return quantity->has_flags(BIGINT_KEEP_PREC);
}

void amount_t::set_keep_precision(const bool keep) const {
  if (!quantity)
    throw_(amount_error, _("Cannot set whether to keep the precision of an uninitialized amount"));

  if (keep)
    quantity->add_flags(BIGINT_KEEP_PREC);
  else
    quantity->drop_flags(BIGINT_KEEP_PREC);
}

void amount_t::set_cost_precision(precision_t prec) {
  if (!quantity)
    throw_(amount_error, _("Cannot set cost precision of an uninitialized amount"));
  _dup();
  in_place_roundto(prec);
  quantity->prec = prec;
  quantity->add_flags(BIGINT_COST_PREC);
}

amount_t::precision_t amount_t::cost_display_precision() const {
  const precision_t comm_prec = commodity().precision();
  if (!quantity->has_flags(BIGINT_COST_PREC) || quantity->prec <= comm_prec)
    return comm_prec;
  if (comm_prec == 0)
    return quantity->prec;
  amount_t at_comm_prec(*this);
  at_comm_prec.in_place_roundto(comm_prec);
  return at_comm_prec.is_realzero() ? quantity->prec : comm_prec;
}

bool amount_t::set_precision_if_exact(precision_t prec) {
  if (!quantity)
    throw_(amount_error, _("Cannot set precision of an uninitialized amount"));

  // The canonical value is exact at prec decimals when its denominator
  // divides 10^prec.
  mpz_ui_pow_ui(temp, 10, prec);
  if (!mpz_divisible_p(temp, mpq_denref(MP(quantity))))
    return false;

  _dup();
  quantity->prec = prec;
  return true;
}

/**
 * @brief Return the precision used for display output.
 *
 * If the amount has a commodity and BIGINT_KEEP_PREC is not set, the
 * commodity's display precision is used.  Otherwise the internal
 * precision is used, taking the maximum of the stored precision and
 * the commodity precision when both are available.
 */
amount_t::precision_t amount_t::display_precision() const {
  if (!quantity)
    throw_(amount_error, _("Cannot determine display precision of an uninitialized amount"));

  commodity_t& comm(commodity());

  if (comm && !keep_precision())
    return comm.precision();
  else
    return comm ? std::max(quantity->prec, comm.precision()) : quantity->prec;
}

/*----------------------------------------------------------------------*/
/*  Negation, inversion, and the rounding family                        */
/*----------------------------------------------------------------------*/

/** @brief Negate this amount in place (multiply by -1). */
void amount_t::in_place_negate() {
  if (quantity) {
    _dup();
    mpq_neg(MP(quantity), MP(quantity));
  } else {
    throw_(amount_error, _("Cannot negate an uninitialized amount"));
  }
}

/** @brief Replace this amount with its reciprocal (1/x). */
void amount_t::in_place_invert() {
  if (!quantity)
    throw_(amount_error, _("Cannot invert an uninitialized amount"));

  _dup();

  if (sign() != 0)
    mpq_inv(MP(quantity), MP(quantity));
}

/**
 * @brief Round this amount to its commodity's display precision.
 *
 * Simply clears the BIGINT_KEEP_PREC flag so that subsequent display
 * operations use the commodity's precision rather than the full
 * internal precision.  The underlying GMP rational is unchanged --
 * rounding only affects how the number is printed.
 */
void amount_t::in_place_round() {
  if (!quantity)
    throw_(amount_error, _("Cannot set rounding for an uninitialized amount"));
  else if (!keep_precision())
    return;

  _dup();
  set_keep_precision(false);
}

/**
 * @brief Round to commodity precision, also modifying the GMP value.
 *
 * Unlike in_place_round() which only affects display, this function
 * actually rounds the underlying rational to the commodity's decimal
 * precision (half away from zero) when that precision is nonzero and
 * the stored precision exceeds it.  It backs the roundto_commodity()
 * value expression, which --round applies to each posting so that a sum
 * matches a statement whose lines were each rounded to the cent.
 */
void amount_t::in_place_round_to_commodity_precision() {
  if (!quantity)
    throw_(amount_error, _("Cannot set rounding for an uninitialized amount"));

  if (has_commodity()) {
    const precision_t comm_prec = commodity().precision();
    if (comm_prec > 0 && quantity->prec > comm_prec) {
      in_place_roundto(comm_prec);
      // in_place_roundto already called _dup(), so set_keep_precision is safe
      if (keep_precision())
        set_keep_precision(false);
      return;
    }
    // A cleared keep_precision flag is a post-condition that we rounded to
    // a meaningful commodity precision (the #1765 invariant: a rounded
    // value must not drift if the commodity widens later).  When the
    // commodity's precision is 0 (as for a $ seen only inside `@` price
    // annotations) nothing was rounded, so clearing the flag is unearned:
    // it would hide the value's fractional content, and --round --basis
    // would show a cost of 20 * `@ $5.025` = $100.500 as $101.  Preserve
    // the flag in that case (#3187).  This tests the stored precision
    // alone.  The display-only rounded() behind --basis has a guard of its
    // own in report_t::fn_rounded; the two sit on different paths.
    if (comm_prec == 0 && keep_precision() && quantity->prec > 0)
      return;
  }

  if (keep_precision()) {
    _dup();
    set_keep_precision(false);
  }
}

/**
 * @brief Truncate toward zero to the display precision.
 *
 * Discards fractional digits beyond the display precision without
 * rounding.  For positive numbers this is equivalent to floor; for
 * negative numbers it rounds toward zero (unlike floor which rounds
 * toward negative infinity).
 *
 * Rounds toward zero at display_precision() through round_quantity().
 */
void amount_t::in_place_truncate() {
  if (!quantity)
    throw_(amount_error, _("Cannot truncate an uninitialized amount"));

  DEBUG("amount.truncate", "Truncating " << *this << " to precision " << display_precision());

  _dup();

  round_quantity(MP(quantity), display_precision(), rounding_t::toward_zero);

  DEBUG("amount.truncate", "Truncated = " << *this);
}

/**
 * @brief Round down to the nearest integer (toward negative infinity).
 *
 * Uses GMP's mpz_fdiv_q ("floor division quotient") to compute the
 * largest integer not exceeding the rational value.
 */
void amount_t::in_place_floor() {
  if (!quantity)
    throw_(amount_error, _("Cannot compute floor on an uninitialized amount"));

  _dup();

  mpz_fdiv_q(temp, mpq_numref(MP(quantity)), mpq_denref(MP(quantity)));
  mpq_set_z(MP(quantity), temp);
}

/**
 * @brief Round up to the nearest integer (toward positive infinity).
 *
 * Uses GMP's mpz_cdiv_q ("ceiling division quotient") to compute the
 * smallest integer not less than the rational value.
 */
void amount_t::in_place_ceiling() {
  if (!quantity)
    throw_(amount_error, _("Cannot compute ceiling on an uninitialized amount"));

  _dup();

  mpz_cdiv_q(temp, mpq_numref(MP(quantity)), mpq_denref(MP(quantity)));
  mpq_set_z(MP(quantity), temp);
}

/**
 * @brief Round to exactly @a places decimal digits, modifying the GMP value.
 *
 * Rounds half away from zero (not banker's rounding) through
 * round_quantity(), whose division is round_quotient_half_away(), the
 * rule the printer uses too.
 *
 * @param places Number of decimal places to round to.  May be zero
 *               (round to integer) or negative (round to tens,
 *               hundreds, etc.).
 */
void amount_t::in_place_roundto(int places) {
  if (!quantity)
    throw_(amount_error, _("Cannot round an uninitialized amount"));

  _dup();

  round_quantity(MP(quantity), places, rounding_t::half_away_from_zero);
}

bool amount_t::in_place_round_unit_price(precision_t divisor_prec) {
  if (!has_commodity() || !keep_precision())
    return false;
  amount_t exact(*this);
  in_place_roundto(
      boost::numeric_cast<int>(divisor_prec + commodity().precision() + extend_by_digits));
  return *this != exact;
}

/**
 * @brief Mark this amount to display its full internal precision.
 *
 * Sets the BIGINT_KEEP_PREC flag so that printing reveals all
 * computed decimal places rather than truncating to the commodity's
 * display precision.  This is the inverse of in_place_round().
 */
void amount_t::in_place_unround() {
  if (!quantity)
    throw_(amount_error, _("Cannot unround an uninitialized amount"));
  else if (keep_precision())
    return;

  _dup();

  DEBUG("amount.unround", "Unrounding " << *this);
  set_keep_precision(true);
  DEBUG("amount.unround", "Unrounded = " << *this);
}

/*----------------------------------------------------------------------*/
/*  Unit reduction and unreduction                                      */
/*----------------------------------------------------------------------*/

/**
 * @brief Convert to the smallest equivalent commodity unit.
 *
 * Walks the commodity's "smaller" chain, multiplying by the
 * conversion factor at each step.  For example, if "1h = 60m" and
 * "1m = 60s" are defined, reducing 1.5h yields 5400s.  This is used
 * during parsing so that all time amounts are stored uniformly in the
 * smallest defined unit.
 *
 * When the amount has a lot-price annotation (from an @ or @@ cost),
 * the per-unit price is divided by the same factor that the amount is
 * multiplied by, so that the total cost remains invariant.  Without
 * this, a C directive linking two units would produce lots whose
 * annotations differ from lots acquired directly in the smaller unit,
 * preventing them from combining in a balance (fixes #512).
 */
void amount_t::in_place_reduce() {
  if (!quantity)
    throw_(amount_error, _("Cannot reduce an uninitialized amount"));

  int depth = 0;
  commodity_t* original = commodity_;
  amount_t scale_factor(1L);
  bool scaled = false;
  while (commodity_ && commodity().smaller()) {
    if (++depth > 100) // safety net: break out of cyclic smaller chains
      break;
    amount_t step = commodity().smaller()->number();
    *this *= step;
    scale_factor *= step;
    scaled = true;
    commodity_ = commodity().smaller()->commodity_;
  }
  if (original && original != commodity_ && commodity_ && original->has_annotation()) {
    annotation_t details(as_annotated_commodity(*original).details);
    if (scaled && details.price)
      *details.price /= scale_factor;
    commodity_ = commodity_pool_t::current_pool->find_or_create(*commodity_, details);
  }
}

/**
 * @brief Convert to the largest unit where the value is at least 1.
 *
 * The inverse of in_place_reduce(): walks the "larger" chain,
 * dividing by conversion factors, stopping when the result would
 * drop below 1 in absolute value.  For example, unreducing 90s when
 * "1m = 60s" is defined yields 1.5m (not 0.025h).
 *
 * When the amount has a lot-price annotation, the per-unit price is
 * multiplied by the same factor that the amount is divided by, so
 * that the total cost remains invariant.  This is the inverse of the
 * price adjustment in in_place_reduce() (fixes #512).
 */
void amount_t::in_place_unreduce() {
  if (!quantity)
    throw_(amount_error, _("Cannot unreduce an uninitialized amount"));

  amount_t tmp = *this;
  commodity_t* comm = commodity_;
  bool shifted = false;
  amount_t scale_factor(1L);

  int depth = 0;
  while (comm && comm->larger()) {
    if (++depth > 100) // safety net: break out of cyclic larger chains
      break;
    amount_t step = comm->larger()->number();
    amount_t next_temp = tmp / step;
    if (next_temp.abs() < amount_t(1L))
      break;
    tmp = next_temp;
    scale_factor *= step;
    comm = comm->larger()->commodity_;
    shifted = true;
  }

  if (shifted) {
    *this = tmp;
    if (commodity_ && commodity_->has_annotation()) {
      annotation_t details(as_annotated_commodity(*commodity_).details);
      if (details.price)
        *details.price *= scale_factor;
      commodity_ = commodity_pool_t::current_pool->find_or_create(*comm, details);
    } else {
      commodity_ = comm;
    }
  }
}

/*----------------------------------------------------------------------*/
/*  Commodity operations: value, price, annotation                      */
/*----------------------------------------------------------------------*/

/**
 * @brief Compute the market value of this amount at a given moment.
 *
 * Looks up the current or historical price of this amount's commodity
 * and returns the equivalent value in another commodity.  The price
 * lookup proceeds through several strategies:
 *
 *   1. If the amount has a fixated price annotation, use it directly.
 *   2. Query the commodity's price history for the best price at
 *      @a moment, optionally expressed @a in_terms_of a target
 *      commodity.
 *   3. If no direct price exists, walk the "larger" unit chain
 *      (e.g. seconds -> minutes -> hours) to find an intermediate
 *      unit that does have a price defined.
 *
 * @param moment       The point in time for the price lookup.  If
 *                     not_a_date_time, the latest available price is used.
 * @param in_terms_of  Optional target commodity.  If nullptr, the
 *                     price annotation's commodity is used as the target.
 * @return The market value as an amount in the target commodity, or
 *         std::nullopt if no price could be determined.
 */
std::optional<amount_t> amount_t::value(const datetime_t& moment,
                                        const commodity_t* in_terms_of) const {
  if (quantity) {
#if DEBUG_ON
    DEBUG("commodity.price.find", "amount_t::value of " << commodity().symbol());
    if (!moment.is_not_a_date_time())
      DEBUG("commodity.price.find", "amount_t::value: moment = " << moment);
    if (in_terms_of)
      DEBUG("commodity.price.find", "amount_t::value: in_terms_of = " << in_terms_of->symbol());
#endif
    if (has_commodity() && (in_terms_of || (!commodity().has_flags(COMMODITY_PRIMARY) &&
                                            !(commodity().has_flags(COMMODITY_NOMARKET) &&
                                              !commodity().has_flags(COMMODITY_BUILTIN))))) {
      const commodity_t* comm(in_terms_of);
      const bool fixated = has_annotation() && annotation().price &&
                           annotation().has_flags(ANNOTATION_PRICE_FIXATED);
      if (!comm && !fixated && has_annotation() && annotation().price)
        comm = annotation().price->commodity_ptr();

      if (comm && commodity().referent() == comm->referent())
        return with_commodity(comm->referent());

      // annotated_commodity_t::find_price returns a fixated lot price as
      // is; it establishes no market value, so it is never re-downloaded.
      std::optional<price_point_t> point = commodity().find_price(comm, moment);
      if (!fixated) {
        // Whether a price was found or not, check whether we should attempt
        // to download a price from the Internet.  This is done if (a) no
        // price was found, or (b) the price is "stale" according to the
        // setting of --price-exp.
        point = commodity().check_for_updated_price(point, moment, comm);
      }

      // If no price was found directly, walk the larger unit chain to find
      // a price for an equivalent larger unit (issue #2017).  This handles
      // chains such as A -> B -> C -> D when amounts are stored in the
      // smallest unit (D) but a price is only defined for an intermediate
      // unit such as B.  We scale the amount up to the larger unit before
      // computing the price, then return the scaled price directly.
      //
      // Also handles the case where the target commodity is reachable via
      // the larger() chain directly (e.g. -X h forces seconds to hours,
      // even when the result is a fraction like 0.50h for 1800s).
      if (!point && commodity().larger()) {
        amount_t scaled(*this);
        int depth = 0;
        while (!point && scaled.commodity_ && scaled.commodity().larger()) {
          if (++depth > 100) // safety net: break out of cyclic larger chains
            break;
          scaled /= scaled.commodity().larger()->number();
          scaled.commodity_ = scaled.commodity().larger()->commodity_;
          if (comm && scaled.commodity().referent() == comm->referent())
            return scaled;
          point = scaled.commodity().find_price(comm, moment);
          point = scaled.commodity().check_for_updated_price(point, moment, comm);
        }
        if (point) {
          amount_t price(point->price);
          price.multiply(scaled, true);
          price.in_place_round();
          return price;
        }
      }

      if (point) {
        amount_t price(point->price);
        price.multiply(*this, true);
        price.in_place_round();
        return price;
      }
    }
  } else {
    throw_(amount_error, _("Cannot determine value of an uninitialized amount"));
  }
  return std::nullopt;
}

/**
 * @brief Return the total cost implied by this amount's price annotation.
 *
 * If this amount is annotated with a per-unit price (e.g. 100 AAPL @
 * $150), returns the total cost (100 * $150 = $15000), in the price's
 * commodity: a price with none, such as 1.50 in C$100 {1.50}, gives a total
 * with none (150).  Returns std::nullopt if no price annotation is present.
 */
std::optional<amount_t> amount_t::price() const {
  if (has_annotation() && annotation().price) {
    amount_t tmp(*annotation().price);
    tmp.multiply(*this, true);
    DEBUG("amount.price", "Returning price of " << *this << " = " << tmp);
    return tmp;
  }
  return std::nullopt;
}

/** @brief Return -1, 0, or 1 indicating the sign of this amount. */
int amount_t::sign() const {
  if (!quantity)
    throw_(amount_error, _("Cannot determine sign of an uninitialized amount"));

  return mpq_sgn(MP(quantity));
}

/**
 * @brief Test whether this amount is zero at its display precision:
 * within half a unit of zero there, an exact half included
 * (within_half_unit()), so a transaction may leave that much over and
 * still balance.
 *
 * For example, with a 2-digit commodity $0.001 and $0.005 are zero and
 * $0.006 is not.  An exact half unit therefore counts as zero yet prints
 * rounded away from zero when the value is not first simplified to zero:
 * is_zero() is true for $0.005, which prints "$0.01".
 *
 * The check is optimized: if the magnitude is at least one
 * the amount is obviously non-zero; otherwise it is decided in exact
 * rational arithmetic, independent of how the amount prints.
 */
bool amount_t::is_zero() const {
  if (!quantity)
    throw_(amount_error, _("Cannot determine if an uninitialized amount is zero"));

  if (has_commodity()) {
    // print() shows the amount at this precision; testing at it keeps a
    // shown amount from counting as zero (#1692).
    const precision_t check_prec = cost_display_precision();

    if (keep_precision() || quantity->prec <= check_prec) {
      return is_realzero();
    } else if (is_realzero()) {
      return true;
    } else if (mpz_cmpabs(mpq_numref(MP(quantity)), mpq_denref(MP(quantity))) >= 0) {
      DEBUG("amount.is_zero", "Magnitude is at least one");
      return false;
    } else {
      return within_half_unit(MP(quantity), check_prec);
    }
  }
  return is_realzero();
}

/** @brief Convert to a double-precision floating point (lossy). */
double amount_t::to_double() const {
  if (!quantity)
    throw_(amount_error, _("Cannot convert an uninitialized amount to a double"));

  mpfr_set_q(tempf, MP(quantity), GMP_RNDN);
  return mpfr_get_d(tempf, GMP_RNDN);
}

/** @brief Convert to a long integer, rounding to nearest. */
long amount_t::to_long() const {
  if (!quantity)
    throw_(amount_error, _("Cannot convert an uninitialized amount to a long"));

  mpfr_set_q(tempf, MP(quantity), GMP_RNDN);
  return mpfr_get_si(tempf, GMP_RNDN);
}

bool amount_t::fits_in_long() const {
  if (!quantity)
    throw_(amount_error, _("Cannot determine if an uninitialized amount fits in a long"));

  mpfr_set_q(tempf, MP(quantity), GMP_RNDN);
  return mpfr_fits_slong_p(tempf, GMP_RNDN);
}

commodity_t* amount_t::commodity_ptr() const {
  return (commodity_ ? commodity_ : commodity_pool_t::current_pool->null_commodity);
}

bool amount_t::has_commodity() const {
  return commodity_ && commodity_ != commodity_->pool().null_commodity;
}

/**
 * @brief Attach price/date/tag annotation metadata to this amount's commodity.
 *
 * Creates (or finds) an annotated commodity that wraps the current
 * commodity with the given annotation details, then rebinds this
 * amount to that annotated commodity.  Annotations are how Ledger
 * tracks per-lot purchase prices and dates for investment accounting.
 */
void amount_t::annotate(const annotation_t& details) {
  commodity_t* this_base;
  annotated_commodity_t* this_ann = nullptr;

  if (!quantity)
    throw_(amount_error, _("Cannot annotate the commodity of an uninitialized amount"));
  else if (!has_commodity())
    return; // ignore attempt to annotate a "bare commodity

  if (commodity().has_annotation()) {
    this_ann = &as_annotated_commodity(commodity());
    this_base = &this_ann->referent();
  } else {
    this_base = &commodity();
  }
  assert(this_base);

  DEBUG("amount.commodities", "Annotating commodity for amount " << *this << '\n' << details);

  if (commodity_t* ann_comm = this_base->pool().find_or_create(*this_base, details))
    set_commodity(*ann_comm);
  else
    assert(false);

  DEBUG("amount.commodities", "Annotated amount is " << *this);
}

bool amount_t::has_annotation() const {
  if (!quantity)
    throw_(amount_error, _("Cannot determine if an uninitialized amount's commodity is annotated"));

  assert(!has_commodity() || !commodity().has_annotation() ||
         as_annotated_commodity(commodity()).details);
  return has_commodity() && commodity().has_annotation();
}

annotation_t& amount_t::annotation() {
  if (!quantity)
    throw_(amount_error,
           _("Cannot return commodity annotation details of an uninitialized amount"));

  if (!commodity().has_annotation())
    throw_(amount_error, _("Request for annotation details from an unannotated amount"));

  annotated_commodity_t& ann_comm(as_annotated_commodity(commodity()));
  return ann_comm.details;
}

amount_t amount_t::strip_annotations(const keep_details_t& what_to_keep) const {
  if (!quantity)
    throw_(amount_error, _("Cannot strip commodity annotations from an uninitialized amount"));

  if (!what_to_keep.keep_all(commodity())) {
    amount_t t(*this);
    t.set_commodity(commodity().strip_annotations(what_to_keep));
    return t;
  }
  return *this;
}

/*----------------------------------------------------------------------*/
/*  Parsing: amount from stream or string                               */
/*----------------------------------------------------------------------*/

namespace {

/**
 * @brief Read a raw numeric string from the input stream.
 *
 * Consumes digits, periods, commas, and apostrophes (all of which may
 * appear as decimal marks or thousands separators depending on locale
 * conventions).  Trailing non-digit punctuation is stripped and pushed
 * back onto the stream, with special handling for a trailing period
 * (which may indicate zero decimal places in hledger-style input).
 * Under PARSE_IN_EXPR, a comma or plus sign where the number would begin
 * is left in the stream and the quantity is empty.
 *
 * @param[in]  in     Input stream positioned at the start of a number.
 * @param[out] value  The raw numeric string, including punctuation but
 *                    excluding any commodity symbol.
 * @param[in]  flags  The amount's parsing flags.
 */
void parse_quantity(std::istream& in, string& value, const parse_flags_t& flags) {
  value.clear();
  int c = peek_next_nonws(in);
  if ((c == ',' || c == '+') && flags.has_flags(PARSE_IN_EXPR))
    return;
  if (c == '-') {
    value.push_back(static_cast<char>(c));
    in.get();
  } else if (c == '+') {
    in.get(); // consume unary '+'; value is positive by default
  }
  read_into(in, value, [](int ch) {
    return std::isdigit(static_cast<unsigned char>(ch)) || ch == '.' || ch == ',' || ch == '\'';
  });

  while (!value.empty() && !std::isdigit(static_cast<unsigned char>(value.back()))) {
    value.pop_back();
    in.unget();
  }

  // hledger uses a trailing decimal point with no following digits to
  // disambiguate the decimal mark from the digit group mark (e.g. "1,234."
  // means 1234 with `.` as decimal mark).  If the strip loop just ungot
  // a trailing `.`, restore it: the read consumed every digit/`.`/`,`/`'`
  // greedily, so anything still in the stream after the stripped `.` is by
  // definition non-numeric and the `.` was the true trailing mark.  Commas
  // are not re-read because they serve as argument separators in value
  // expressions (e.g. "justify(..., 12, 2, ...)").
  if (!value.empty() && std::isdigit(static_cast<unsigned char>(value.back())) &&
      in.peek() == '.') {
    in.get();
    value.push_back('.');
  }
}
} // namespace

/**
 * @brief Parse an amount from an input stream.
 *
 * Recognizes both prefix and suffix commodity styles:
 *   - @c [-]NUM[ ]SYM -- e.g. "10.00 USD", "-$42.50"
 *   - @c SYM[ ][-]NUM -- e.g. "$10.00", "EUR -5.00"
 *
 * The parser automatically detects the decimal mark convention
 * (period vs. comma vs. apostrophe for thousands grouping) and
 * updates the commodity's display style flags accordingly, unless
 * PARSE_NO_MIGRATE is set.
 *
 * After parsing, the raw decimal string is converted to a GMP
 * rational, unit reduction is applied (unless PARSE_NO_REDUCE), and
 * any price/date annotations are attached.
 *
 * @param in    Input stream to read from.
 * @param flags Parsing control flags (see parse_flags_enum_t).
 * @return true if an amount was successfully parsed, false only if
 *         PARSE_SOFT_FAIL is set and no quantity was found.
 */
bool amount_t::parse(std::istream& in, const parse_flags_t& flags,
                     std::optional<amount_t>* given_lot_total) {
  // The possible syntax for an amount is:
  //
  //   [-]NUM[ ]SYM [@ AMOUNT]
  //   SYM[ ][-]NUM [@ AMOUNT]

  string symbol;
  string quant;
  annotation_t details;
  bool negative = false;

  commodity_t::flags_t comm_flags = COMMODITY_STYLE_DEFAULTS;

  int c = peek_next_nonws(in);
  if (c == '-') {
    negative = true;
    in.get();
    c = peek_next_nonws(in);
  } else if (c == '+') {
    in.get(); // consume unary '+'; value is positive by default
    c = peek_next_nonws(in);
  }

  char n;
  if (std::isdigit(static_cast<unsigned char>(c)) || c == '.') {
    parse_quantity(in, quant, flags);

    n = in.eof() ? '\n' : static_cast<char>(in.peek());
    if (!in.eof() && n != '\n') {
      if (std::isspace(static_cast<unsigned char>(n)))
        comm_flags |= COMMODITY_STYLE_SEPARATED;

      commodity_t::parse_symbol(in, symbol);

      if (!symbol.empty())
        comm_flags |= COMMODITY_STYLE_SUFFIXED;

      n = in.eof() ? '\n' : static_cast<char>(in.peek());
      if (!flags.has_flags(PARSE_NO_ANNOT) && !in.eof() && n != '\n')
        details.parse(in);
    }
  } else {
    commodity_t::parse_symbol(in, symbol);

    n = in.eof() ? '\n' : static_cast<char>(in.peek());
    if (!in.eof() && n != '\n') {
      if (std::isspace(static_cast<unsigned char>(in.peek())))
        comm_flags |= COMMODITY_STYLE_SEPARATED;

      parse_quantity(in, quant, flags);

      n = in.eof() ? '\n' : static_cast<char>(in.peek());
      if (!flags.has_flags(PARSE_NO_ANNOT) && !quant.empty() && !in.eof() && n != '\n')
        details.parse(in);
    }
  }

  if (quant.empty()) {
    if (flags.has_flags(PARSE_SOFT_FAIL))
      return false;
    else
      throw_(amount_error, _("No quantity specified for amount"));
  }

  // Allocate memory for the amount's quantity value.  We have to
  // monitor the allocation in a unique_ptr because this function gets
  // called sometimes from amount_t's constructor; and if there is an
  // exception thrown by any of the function calls after this point,
  // the destructor will never be called and the memory never freed.

  unique_ptr<bigint_t> new_quantity;

  if (quantity) {
    if (quantity->refc > 1)
      _release();
    else
      new_quantity.reset(quantity);
    quantity = nullptr;
  }

  if (!new_quantity.get())
    new_quantity.reset(new bigint_t);

  // No one is holding a reference to this now.
  new_quantity->refc--;

  // Create the commodity if has not already been seen, and update the
  // precision if something greater was used for the quantity.

  if (symbol.empty()) {
    commodity_ = nullptr;
  } else {
    commodity_ = commodity_pool_t::current_pool->find(symbol);
    if (!commodity_)
      commodity_ = commodity_pool_t::current_pool->create(symbol);
    assert(commodity_);
  }

  // Quickly scan through and verify the correctness of the amount's use of
  // punctuation.

  precision_t decimal_offset = 0;
  string::size_type string_index = quant.length();
  string::size_type last_comma = string::npos;
  string::size_type last_period = string::npos;
  string::size_type last_apostrophe = string::npos;

  bool no_more_commas = false;
  bool no_more_periods = false;
  bool no_migrate_style = commodity().has_flags(COMMODITY_STYLE_NO_MIGRATE);
  bool decimal_comma_style = (commodity_t::decimal_comma_by_default ||
                              commodity().has_flags(COMMODITY_STYLE_DECIMAL_COMMA));
  // Set when the decimal comma came only from the comma heuristics below,
  // not from the commodity, --decimal-comma, or a period.
  bool decimal_comma_guessed = false;

  new_quantity->prec = 0;

  for (auto it = quant.rbegin(); it != quant.rend(); ++it) {
    const char& ch = *it;
    string_index--;

    if (ch == '.') {
      if (no_more_periods)
        throw_(amount_error, _("Too many periods in amount"));

      if (decimal_comma_style) {
        if (decimal_offset % 3 != 0)
          throw_(amount_error, _("Incorrect use of thousand-mark period"));
        comm_flags |= COMMODITY_STYLE_THOUSANDS;
        no_more_commas = true;
      } else {
        if (last_comma != string::npos) {
          // A period appearing after a comma that was tentatively treated as
          // a thousands separator means the comma was actually the decimal
          // separator (European style). Retroactively switch to decimal_comma.
          decimal_comma_style = true;
          new_quantity->prec = static_cast<precision_t>(quant.length() - 1 - last_comma);
          if (decimal_offset % 3 != 0)
            throw_(amount_error, _("Incorrect use of thousand-mark period"));
          comm_flags |= COMMODITY_STYLE_THOUSANDS;
          no_more_commas = true;
        } else {
          no_more_periods = true;
          new_quantity->prec = decimal_offset;
          decimal_offset = 0;
        }
      }

      if (last_period == string::npos)
        last_period = string_index;
    } else if (ch == ',') {
      if (no_more_commas)
        throw_(amount_error, _("Too many commas in amount"));

      if (decimal_comma_style) {
        if (last_period != string::npos) {
          throw_(amount_error, _("Incorrect use of decimal comma"));
        } else {
          no_more_commas = true;
          new_quantity->prec = decimal_offset;
          decimal_offset = 0;
        }
      } else {
        // Check if the integer part is zero (e.g., "0,075" should be 0.075, not 75)
        // This handles European-style decimal notation where comma is the decimal
        // separator. A comma followed by exactly 3 digits is ambiguous: it could
        // be a thousands separator (1,000) or a decimal separator (0,075 = 0.075).
        // We treat it as a decimal separator when the integer part is 0.
        bool integer_part_is_zero = false;
        if (last_comma == string::npos && string_index > 0) {
          // Check if all characters before this comma are '0'
          bool all_zeros = true;
          for (std::size_t i = 0; i < string_index; i++) {
            if (quant[i] != '0' && !(i == 0 && quant[i] == '-')) {
              all_zeros = false;
              break;
            }
          }
          integer_part_is_zero = all_zeros;
        }

        if (decimal_offset % 3 != 0 ||
            // A single comma with more than 3 digits after it cannot
            // be a thousands separator (which requires commas every 3
            // digits). Treat it as a decimal comma instead.
            (last_comma == string::npos && decimal_offset > 3) ||
            // A comma after a zero integer part is a decimal separator
            // (e.g., "0,075" = 0.075, not 75)
            integer_part_is_zero) {
          if (last_comma != string::npos || last_period != string::npos) {
            throw_(amount_error, _("Incorrect use of thousand-mark comma"));
          } else {
            decimal_comma_style = true;
            decimal_comma_guessed = true;
            no_more_commas = true;
            new_quantity->prec = decimal_offset;
            decimal_offset = 0;
          }
        } else {
          comm_flags |= COMMODITY_STYLE_THOUSANDS;
          // Only block periods after the second comma confirms thousands
          // style. The first comma with exactly 3 trailing digits is
          // ambiguous and a later period can disambiguate it.
          if (last_comma != string::npos)
            no_more_periods = true;
        }
      }

      if (last_comma == string::npos)
        last_comma = string_index;
    } else if (ch == '\'') {
      // Apostrophe is always a thousands separator (e.g., CHF 1'234'567.89)
      if (decimal_offset % 3 != 0)
        throw_(amount_error, _("Incorrect use of thousand-mark apostrophe"));
      comm_flags |= COMMODITY_STYLE_THOUSANDS;
      comm_flags |= COMMODITY_STYLE_THOUSANDS_APOSTROPHE;
      if (last_apostrophe == string::npos)
        last_apostrophe = string_index;
    } else {
      decimal_offset++;
    }
  }

  if (decimal_comma_style)
    comm_flags |= COMMODITY_STYLE_DECIMAL_COMMA;

  if (flags.has_flags(PARSE_NO_MIGRATE)) {
    // Can't call set_keep_precision here, because it assumes that `quantity'
    // is non-NULL.
    new_quantity->add_flags(BIGINT_KEEP_PREC);
  } else if (commodity_ && !no_migrate_style) {
    // A decimal comma guessed from the digits alone must not overturn the
    // decimal period this commodity has already been written with, unless
    // the amount declares the style; a precision learned only from a price
    // directive establishes nothing.
    if (decimal_comma_guessed && last_period == string::npos &&
        !flags.has_flags(PARSE_DECLARES_STYLE) && commodity().precision() > 0 &&
        !commodity().has_flags(COMMODITY_PRECISION_FROM_PRICE))
      throw_(amount_error, _f("Ambiguous decimal comma: commodity %1% uses a decimal period") %
                               commodity().symbol());

    commodity().add_flags(comm_flags);

    if (commodity().has_flags(COMMODITY_PRECISION_FROM_PRICE) && new_quantity->prec > 0) {
      // Precision was set tentatively from a price record; a real transaction
      // amount that explicitly specifies decimal places (non-zero precision)
      // overrides the tentative precision and locks it in.  Integer amounts
      // (precision == 0) do not override, preserving the price-derived
      // precision for display.
      commodity().set_precision(new_quantity->prec);
      commodity().drop_flags(COMMODITY_PRECISION_FROM_PRICE);
    } else if (new_quantity->prec > commodity().precision()) {
      commodity().set_precision(new_quantity->prec);
    }
  } else if (!commodity_ && !no_migrate_style && flags.has_flags(PARSE_APPLY_STYLE_TO_BARE)) {
    // For bare amounts parsed from a D directive, apply only the
    // numeric-formatting style flags (thousands separator and
    // decimal-comma choice) to null_commodity so that subsequent bare
    // amounts display with the same separators.  SEPARATED/SUFFIXED
    // are deliberately excluded since there is no commodity symbol.
    // The D directive locks the style afterwards by setting
    // COMMODITY_STYLE_NO_MIGRATE on null_commodity, preventing further
    // bare-amount parses from mutating it (#761).
    const commodity_t::flags_t numeric_flags = COMMODITY_STYLE_THOUSANDS |
                                               COMMODITY_STYLE_DECIMAL_COMMA |
                                               COMMODITY_STYLE_THOUSANDS_APOSTROPHE;
    commodity().add_flags(comm_flags & numeric_flags);
    if (new_quantity->prec > commodity().precision())
      commodity().set_precision(new_quantity->prec);
  }

  // Now we have the final number.  Remove commas and periods, if necessary.

  if (last_comma != string::npos || last_period != string::npos ||
      last_apostrophe != string::npos) {
    string::size_type len = quant.length();
    scoped_array<char> buf(new char[len + 1]);
    const char* p = quant.c_str();
    char* t = buf.get();

    while (*p) {
      if (*p == ',' || *p == '.' || *p == '\'') {
        ++p;
        continue; // skip separator; do NOT copy the character that follows
      }
      *t++ = *p++;
    }
    *t = '\0';

    mpq_set_str(MP(new_quantity.get()), buf.get(), 10);
    mpz_ui_pow_ui(temp, 10, new_quantity->prec);
    mpq_set_z(tempq, temp);
    mpq_div(MP(new_quantity.get()), MP(new_quantity.get()), tempq);

    IF_DEBUG("amount.parse") {
      char* amt_buf = mpq_get_str(nullptr, 10, MP(new_quantity.get()));
      DEBUG("amount.parse", "Rational parsed = " << amt_buf);
      std::free(amt_buf);
    }
  } else {
    mpq_set_str(MP(new_quantity.get()), quant.c_str(), 10);
  }

  if (negative)
    mpq_neg(MP(new_quantity.get()), MP(new_quantity.get()));

  new_quantity->refc++;
  quantity = new_quantity.release();

  if (!flags.has_flags(PARSE_NO_REDUCE))
    in_place_reduce(); // will not throw an exception

  if (commodity_ && details) {
    if (details.has_flags(ANNOTATION_PRICE_NOT_PER_UNIT)) {
      assert(details.price);
      // `details` is this parse's own annotation, not the pooled lot.
      if (given_lot_total)
        *given_lot_total = *details.price;
      *details.price /= this->abs();
      // Same rounding as an @@ cost in commodity_pool_t::exchange().
      if (details.price->in_place_round_unit_price(this->precision()))
        details.add_flags(ANNOTATION_PRICE_ROUNDED);
    }
    set_commodity(*commodity_pool_t::current_pool->find_or_create(*commodity_, details));
  }

  VERIFY(valid());

  return true;
}

/**
 * @brief Register a unit conversion between two commodity amounts.
 *
 * Establishes a bidirectional conversion link (larger/smaller) between
 * two commodity units.  For example, parse_conversion("1.0h", "60m")
 * tells Ledger that one hour equals sixty minutes, enabling automatic
 * unit reduction and unreduction.
 *
 * @param larger_str   String representation of the larger unit (e.g. "1.0h").
 * @param smaller_str  String representation of the equivalent smaller
 *                     unit (e.g. "60m").
 */
void amount_t::parse_conversion(const string& larger_str, const string& smaller_str) {
  amount_t larger, smaller;

  (void)larger.parse(larger_str, PARSE_NO_REDUCE);
  (void)smaller.parse(smaller_str, PARSE_NO_REDUCE);

  // Normalize so the stored smaller/larger links represent a per-unit
  // conversion factor (issue #1793).  For "C 100 apple = 48 kcal" the
  // user means 1 apple = 0.48 kcal, not 1 apple = 48 kcal; without this
  // step in_place_reduce() would multiply 225 apple by 48 instead of by
  // 0.48.  For the builtin time directives ("C 1.00h = 60m") larger's
  // number is already 1 so this is a no-op.
  if (!larger.number().is_realzero()) {
    smaller /= larger.number();
    larger /= larger.number();
  }

  larger *= smaller.number();

  // Detect cycles before setting the smaller/larger links (bug #1065).
  // The C directive (e.g. "C 3600s = 1h") can create cycles when there
  // is already a chain connecting the two commodities.  Walk both chains
  // to verify that the new link would not close a loop.
  if (larger.commodity_ && smaller.commodity_) {
    // Would setting larger_commodity.smaller = smaller create a cycle?
    // Walk the existing smaller chain from smaller's commodity.
    for (commodity_t* c = smaller.commodity_; c && c->smaller(); c = c->smaller()->commodity_) {
      if (c->smaller()->commodity_ == larger.commodity_)
        throw parse_error(_("Commodity conversion would create a cycle"));
    }
    // Would setting smaller_commodity.larger = larger create a cycle?
    // Walk the existing larger chain from larger's commodity.
    for (commodity_t* c = larger.commodity_; c && c->larger(); c = c->larger()->commodity_) {
      if (c->larger()->commodity_ == smaller.commodity_)
        throw parse_error(_("Commodity conversion would create a cycle"));
    }
  }

  if (larger.commodity()) {
    larger.commodity().set_smaller(smaller);
    larger.commodity().add_flags(smaller.commodity().flags() | COMMODITY_NOMARKET);
  }
  if (smaller.commodity())
    smaller.commodity().set_larger(larger);
}

/*----------------------------------------------------------------------*/
/*  Printing: amount_t::print                                           */
/*----------------------------------------------------------------------*/

/**
 * @brief Format this amount to an output stream with full commodity styling.
 *
 * Assembles the complete display representation: commodity symbol
 * (prefix or suffix, with optional separator), the numeric value
 * formatted by stream_out_mpq with appropriate thousands separators
 * and decimal style, and any commodity annotations (price, date, tag).
 *
 * When @a _out has a width set, the output is first assembled in a
 * temporary ostringstream so that the width applies to the entire
 * formatted string rather than just its first piece; otherwise
 * print_unpadded() writes straight to the stream.
 *
 * @param _out  Destination output stream.
 * @param flags AMOUNT_PRINT_* flags controlling elision of quotes,
 *              right-justification, colorization, etc.
 */
void amount_t::print(std::ostream& _out, const uint_least8_t flags) const {
  VERIFY(valid());

  if (!quantity) {
    _out << "<null>";
    return;
  }

  // When the caller has set a width or fill on the stream it must apply
  // to the entire amount string, not just its first part, so the amount
  // is rendered into a string first.  Without a width -- the common case
  // when formatting reports -- write straight through.
  if (_out.width() == 0) {
    print_unpadded(_out, flags);
  } else {
    std::ostringstream out;
    print_unpadded(out, flags);
    _out << out.str();
  }
}

void amount_t::print_unpadded(std::ostream& out, const uint_least8_t flags,
                              const precision_t min_decimals) const {
  commodity_t& comm(commodity());

  if (!comm.has_flags(COMMODITY_STYLE_SUFFIXED)) {
    comm.print(out, flags & AMOUNT_PRINT_ELIDE_COMMODITY_QUOTES);
    if (comm.has_flags(COMMODITY_STYLE_SEPARATED))
      out << " ";
  }

  precision_t disp_prec = display_precision();
  int zeros_prec = 0;
  if (comm) {
    // An amount inferred from a cost may show more digits than its
    // commodity's precision (see cost_display_precision, #1692).
    zeros_prec = cost_display_precision();
    if (zeros_prec > commodity().precision())
      disp_prec = static_cast<precision_t>(zeros_prec);
  }
  zeros_prec = std::max(zeros_prec, static_cast<int>(min_decimals));
  stream_out_mpq(out, MP(quantity), disp_prec, zeros_prec, &comm);

  if (comm.has_flags(COMMODITY_STYLE_SUFFIXED)) {
    if (comm.has_flags(COMMODITY_STYLE_SEPARATED))
      out << " ";
    comm.print(out, flags & AMOUNT_PRINT_ELIDE_COMMODITY_QUOTES);
  }

  // If there are any annotations associated with this commodity, output them
  // now.
  comm.write_annotations(out, flags & AMOUNT_PRINT_NO_COMPUTED_ANNOTATIONS);
}

/*----------------------------------------------------------------------*/
/*  Validation                                                          */
/*----------------------------------------------------------------------*/

/**
 * @brief Check internal consistency of this amount.
 *
 * Validates that: (1) if a quantity exists, the bigint_t passes its
 * own validity check and the reference count is positive; and (2) a
 * null quantity does not have a dangling commodity pointer.  Used by
 * the VERIFY() macro throughout the codebase for defensive debugging.
 */
bool amount_t::valid() const {
  if (quantity) {
    if (!quantity->valid()) {
      DEBUG("ledger.validate", "amount_t: ! quantity->valid()");
      return false;
    }

    if (quantity->refc == 0) {
      DEBUG("ledger.validate", "amount_t: quantity->refc == 0");
      return false;
    }
  } else if (commodity_) {
    DEBUG("ledger.validate", "amount_t: commodity_ != NULL");
    return false;
  }
  return true;
}

/** @brief Serialize an amount into a Boost property tree (XML/JSON output). */
void put_amount(property_tree::ptree& st, const amount_t& amt, bool commodity_details) {
  if (amt.has_commodity())
    put_commodity(st.put("commodity", ""), amt.commodity(), commodity_details);

  st.put("quantity", amt.quantity_string());
}

} // namespace ledger
