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

#include "amount.h"
#include "binary.h"

namespace ledger {

commodity_pool_t * amount_t::current_pool = NULL;

bool amount_t::keep_base = false;

bool amount_t::keep_price = false;
bool amount_t::keep_date  = false;
bool amount_t::keep_tag	  = false;

bool amount_t::stream_fullstrings = false;

#if !defined(THREADSAFE)
// These global temporaries are pre-initialized for the sake of
// efficiency, and are reused over and over again.
static mpz_t temp;
static mpfr_t tempf;
#ifdef INTEGER_MATH
static mpz_t divisor;
#else
static mpq_t tempq;
#endif
#endif

struct amount_t::bigint_t : public supports_flags<>
{
#define BIGINT_BULK_ALLOC 0x01
#define BIGINT_KEEP_PREC  0x02

#ifdef INTEGER_MATH
  mpz_t		 val;
#else
  mpq_t		 val;
#endif
  precision_t	 prec;		// this is only an estimate
  uint_least16_t ref;
  uint_fast32_t	 index;

#define MP(bigint) ((bigint)->val)

  bigint_t() : prec(0), ref(1), index(0) {
    TRACE_CTOR(bigint_t, "");
#ifdef INTEGER_MATH
    mpz_init(val);
#else
    mpq_init(val);
#endif
  }
#ifdef INTEGER_MATH
  bigint_t(mpz_t _val) : prec(0), ref(1), index(0) {
    TRACE_CTOR(bigint_t, "mpz_t");
    mpz_init_set(val, _val);
  }
#else
#if 0
  bigint_t(mpq_t _val) : prec(0), ref(1), index(0) {
    TRACE_CTOR(bigint_t, "mpq_t");
    mpq_init(val, _val);
    mpq_set(val, _val);
  }
#endif
#endif
  bigint_t(const bigint_t& other)
    : supports_flags<>(other.flags() & ~BIGINT_BULK_ALLOC),
      prec(other.prec), ref(1), index(0) {
    TRACE_CTOR(bigint_t, "copy");
#ifdef INTEGER_MATH
    mpz_init_set(val, other.val);
#else
    mpq_init(val);
    mpq_set(val, other.val);
#endif
  }
  ~bigint_t() {
    TRACE_DTOR(bigint_t);
    assert(ref == 0);
#ifdef INTEGER_MATH
    mpz_clear(val);
#else
    mpq_clear(val);
#endif
  }

  bool valid() const {
    if (prec > 128) {
      DEBUG("ledger.validate", "amount_t::bigint_t: prec > 128");
      return false;
    }
    if (ref > 16535) {
      DEBUG("ledger.validate", "amount_t::bigint_t: ref > 16535");
      return false;
    }
    return true;
  }
};

uint_fast32_t amount_t::sizeof_bigint_t()
{
  return sizeof(bigint_t);
}

amount_t * one = NULL;

void amount_t::initialize()
{
  mpz_init(temp);
  mpfr_init(tempf);
#ifdef INTEGER_MATH
  mpz_init(divisor);
#else
  mpq_init(tempq);
#endif

  one = new amount_t(amount_t(1L).unround());

  if (! current_pool)
    current_pool = new commodity_pool_t;

  // Add time commodity conversions, so that timelog's may be parsed
  // in terms of seconds, but reported as minutes or hours.
  if (commodity_t * commodity = current_pool->create("s")) {
    commodity->add_flags(COMMODITY_BUILTIN | COMMODITY_NOMARKET);

    parse_conversion("1.0m", "60s");
    parse_conversion("1.0h", "60m");
  } else {
    assert(false);
  }
}

void amount_t::shutdown()
{
  mpz_clear(temp);
  mpfr_clear(tempf);
#ifdef INTEGER_MATH
  mpz_clear(divisor);
#else
  mpq_clear(tempq);
#endif

  checked_delete(one);

  if (current_pool) {
    checked_delete(current_pool);
    current_pool = NULL;
  }
}

void amount_t::_copy(const amount_t& amt)
{
  assert(amt.valid());

  if (quantity != amt.quantity) {
    if (quantity)
      _release();

    // Never maintain a pointer into a bulk allocation pool; such
    // pointers are not guaranteed to remain.
    if (amt.quantity->has_flags(BIGINT_BULK_ALLOC)) {
      quantity = new bigint_t(*amt.quantity);
    } else {
      quantity = amt.quantity;
      DEBUG("amounts.refs",
	     quantity << " ref++, now " << (quantity->ref + 1));
      quantity->ref++;
    }
  }
  commodity_ = amt.commodity_;

  assert(valid());
}

void amount_t::_dup()
{
  assert(valid());

  if (quantity->ref > 1) {
    bigint_t * q = new bigint_t(*quantity);
    _release();
    quantity = q;
  }

  assert(valid());
}

#ifdef INTEGER_MATH

void amount_t::_resize(precision_t prec)
{
  assert(prec < 256);

  if (! quantity || prec == quantity->prec)
    return;

  _dup();

  assert(prec > quantity->prec);
  mpz_ui_pow_ui(divisor, 10, prec - quantity->prec);
  mpz_mul(MP(quantity), MP(quantity), divisor);

  quantity->prec = prec;

  assert(valid());
}

#endif // INTEGER_MATH

void amount_t::_clear()
{
  if (quantity) {
    _release();
    quantity   = NULL;
    commodity_ = NULL;
  } else {
    assert(! commodity_);
  }
}

void amount_t::_release()
{
  assert(valid());

  DEBUG("amounts.refs", quantity << " ref--, now " << (quantity->ref - 1));

  if (--quantity->ref == 0) {
    if (quantity->has_flags(BIGINT_BULK_ALLOC))
      quantity->~bigint_t();
    else
      checked_delete(quantity);
    quantity   = NULL;
    commodity_ = NULL;
  }

  assert(valid());
}


amount_t::amount_t(const double val) : commodity_(NULL)
{
  TRACE_CTOR(amount_t, "const double");
  quantity = new bigint_t;
#ifdef INTEGER_MATH
  mpfr_set_d(tempf, val, GMP_RNDN);
#else
  mpq_set_d(MP(quantity), val);
#endif
  quantity->prec = extend_by_digits; // an approximation
}

amount_t::amount_t(const unsigned long val) : commodity_(NULL)
{
  TRACE_CTOR(amount_t, "const unsigned long");
  quantity = new bigint_t;
#ifdef INTEGER_MATH
  mpz_set_ui(MP(quantity), val);
#else
  mpq_set_ui(MP(quantity), val, 1);
#endif
}

amount_t::amount_t(const long val) : commodity_(NULL)
{
  TRACE_CTOR(amount_t, "const long");
  quantity = new bigint_t;
#ifdef INTEGER_MATH
  mpz_set_si(MP(quantity), val);
#else
  mpq_set_si(MP(quantity), val, 1);
#endif
}


amount_t& amount_t::operator=(const amount_t& amt)
{
  if (this != &amt) {
    if (amt.quantity)
      _copy(amt);
    else if (quantity)
      _clear();
  }
  return *this;
}


int amount_t::compare(const amount_t& amt) const
{
  assert(amt.valid());

  if (! quantity || ! amt.quantity) {
    if (quantity)
      throw_(amount_error, "Cannot compare an amount to an uninitialized amount");
    else if (amt.quantity)
      throw_(amount_error, "Cannot compare an uninitialized amount to an amount");
    else
      throw_(amount_error, "Cannot compare two uninitialized amounts");
  }

  if (has_commodity() && amt.has_commodity() &&
      commodity() != amt.commodity())
    throw_(amount_error,
	   "Cannot compare amounts with different commodities: " <<
	   commodity().symbol() << " and " << amt.commodity().symbol());

#ifdef INTEGER_MATH
  if (quantity->prec == amt.quantity->prec) {
    return mpz_cmp(MP(quantity), MP(amt.quantity));
  }
  else if (quantity->prec < amt.quantity->prec) {
    amount_t t(*this);
    t._resize(amt.quantity->prec);
    return mpz_cmp(MP(t.quantity), MP(amt.quantity));
  }
  else {
    amount_t t = amt;
    t._resize(quantity->prec);
    return mpz_cmp(MP(quantity), MP(t.quantity));
  }
#else
  return mpq_cmp(MP(quantity), MP(amt.quantity));
#endif
}


amount_t& amount_t::operator+=(const amount_t& amt)
{
  assert(amt.valid());

  if (! quantity || ! amt.quantity) {
    if (quantity)
      throw_(amount_error, "Cannot add an amount to an uninitialized amount");
    else if (amt.quantity)
      throw_(amount_error, "Cannot add an uninitialized amount to an amount");
    else
      throw_(amount_error, "Cannot add two uninitialized amounts");
  }

  if (commodity() != amt.commodity())
    throw_(amount_error,
	   "Adding amounts with different commodities: " <<
	   (has_commodity() ? commodity().symbol() : "NONE") <<
	   " != " <<
	   (amt.has_commodity() ? amt.commodity().symbol() : "NONE"));

  _dup();

#ifdef INTEGER_MATH
  if (quantity->prec == amt.quantity->prec) {
    mpz_add(MP(quantity), MP(quantity), MP(amt.quantity));
  }
  else if (quantity->prec < amt.quantity->prec) {
    _resize(amt.quantity->prec);
    mpz_add(MP(quantity), MP(quantity), MP(amt.quantity));
  }
  else {
    amount_t t = amt;
    t._resize(quantity->prec);
    mpz_add(MP(quantity), MP(quantity), MP(t.quantity));
  }
#else
  mpq_add(MP(quantity), MP(quantity), MP(amt.quantity));

  if (quantity->prec < amt.quantity->prec)
    quantity->prec = amt.quantity->prec;
#endif

  return *this;
}

amount_t& amount_t::operator-=(const amount_t& amt)
{
  assert(amt.valid());

  if (! quantity || ! amt.quantity) {
    if (quantity)
      throw_(amount_error, "Cannot subtract an amount from an uninitialized amount");
    else if (amt.quantity)
      throw_(amount_error, "Cannot subtract an uninitialized amount from an amount");
    else
      throw_(amount_error, "Cannot subtract two uninitialized amounts");
  }

  if (commodity() != amt.commodity())
    throw_(amount_error,
	   "Subtracting amounts with different commodities: " <<
	   (has_commodity() ? commodity().symbol() : "NONE") <<
	   " != " <<
	   (amt.has_commodity() ? amt.commodity().symbol() : "NONE"));

  _dup();

#ifdef INTEGER_MATH
  if (quantity->prec == amt.quantity->prec) {
    mpz_sub(MP(quantity), MP(quantity), MP(amt.quantity));
  }
  else if (quantity->prec < amt.quantity->prec) {
    _resize(amt.quantity->prec);
    mpz_sub(MP(quantity), MP(quantity), MP(amt.quantity));
  }
  else {
    amount_t t = amt;
    t._resize(quantity->prec);
    mpz_sub(MP(quantity), MP(quantity), MP(t.quantity));
  }
#else
  mpq_sub(MP(quantity), MP(quantity), MP(amt.quantity));

  if (quantity->prec < amt.quantity->prec)
    quantity->prec = amt.quantity->prec;
#endif

  return *this;
}

#ifdef INTEGER_MATH

namespace {
  void mpz_round(mpz_t out, mpz_t value, int value_prec, int round_prec)
  {
    // Round `value', with an encoding precision of `value_prec', to a
    // rounded value with precision `round_prec'.  Result is stored in
    // `out'.

    assert(value_prec > round_prec);

    mpz_t quotient;
    mpz_t remainder;

    mpz_init(quotient);
    mpz_init(remainder);

    mpz_ui_pow_ui(divisor, 10, value_prec - round_prec);
    mpz_tdiv_qr(quotient, remainder, value, divisor);
    mpz_divexact_ui(divisor, divisor, 10);
    mpz_mul_ui(divisor, divisor, 5);

    if (mpz_sgn(remainder) < 0) {
      mpz_neg(divisor, divisor);
      if (mpz_cmp(remainder, divisor) < 0) {
	mpz_ui_pow_ui(divisor, 10, value_prec - round_prec);
	mpz_add(remainder, divisor, remainder);
	mpz_ui_sub(remainder, 0, remainder);
	mpz_add(out, value, remainder);
      } else {
	mpz_sub(out, value, remainder);
      }
    } else {
      if (mpz_cmp(remainder, divisor) >= 0) {
	mpz_ui_pow_ui(divisor, 10, value_prec - round_prec);
	mpz_sub(remainder, divisor, remainder);
	mpz_add(out, value, remainder);
      } else {
	mpz_sub(out, value, remainder);
      }
    }
    mpz_clear(quotient);
    mpz_clear(remainder);

    // chop off the rounded bits
    mpz_ui_pow_ui(divisor, 10, value_prec - round_prec);
    mpz_tdiv_q(out, out, divisor);
  }
}

#endif // INTEGER_MATH

amount_t& amount_t::operator*=(const amount_t& amt)
{
  assert(amt.valid());

  if (! quantity || ! amt.quantity) {
    if (quantity)
      throw_(amount_error, "Cannot multiply an amount by an uninitialized amount");
    else if (amt.quantity)
      throw_(amount_error, "Cannot multiply an uninitialized amount by an amount");
    else
      throw_(amount_error, "Cannot multiply two uninitialized amounts");
  }

  _dup();

#ifdef INTEGER_MATH
  mpz_mul(MP(quantity), MP(quantity), MP(amt.quantity));
#else
  mpq_mul(MP(quantity), MP(quantity), MP(amt.quantity));
#endif
  quantity->prec += amt.quantity->prec;

  if (! has_commodity())
    commodity_ = amt.commodity_;

  if (has_commodity() && ! keep_precision()) {
    precision_t comm_prec = commodity().precision();
    if (quantity->prec > comm_prec + extend_by_digits) {
#ifdef INTEGER_MATH
      mpz_round(MP(quantity), MP(quantity), quantity->prec,
		comm_prec + extend_by_digits);
#endif // INTEGER_MATH
      quantity->prec = comm_prec + extend_by_digits;
    }
  }

  return *this;
}

amount_t& amount_t::operator/=(const amount_t& amt)
{
  assert(amt.valid());

  if (! quantity || ! amt.quantity) {
    if (quantity)
      throw_(amount_error, "Cannot divide an amount by an uninitialized amount");
    else if (amt.quantity)
      throw_(amount_error, "Cannot divide an uninitialized amount by an amount");
    else
      throw_(amount_error, "Cannot divide two uninitialized amounts");
  }

  if (! amt)
    throw_(amount_error, "Divide by zero");

  _dup();

  // Increase the value's precision, to capture fractional parts after
  // the divide.  Round up in the last position.

#ifdef INTEGER_MATH
  mpz_ui_pow_ui(divisor, 10, (2 * amt.quantity->prec) + quantity->prec +
		extend_by_digits + 1U);
  mpz_mul(MP(quantity), MP(quantity), divisor);
  mpz_tdiv_q(MP(quantity), MP(quantity), MP(amt.quantity));
  quantity->prec += amt.quantity->prec + quantity->prec + extend_by_digits + 1U;

  mpz_round(MP(quantity), MP(quantity), quantity->prec, quantity->prec - 1);
  quantity->prec -= 1;
#else
  mpq_div(MP(quantity), MP(quantity), MP(amt.quantity));
  quantity->prec += amt.quantity->prec + quantity->prec + extend_by_digits;
#endif

  if (! has_commodity())
    commodity_ = amt.commodity_;

  // If this amount has a commodity, and we're not dealing with plain
  // numbers, or internal numbers (which keep full precision at all
  // times), then round the number to within the commodity's precision
  // plus six places.

  if (has_commodity() && ! keep_precision()) {
    precision_t comm_prec = commodity().precision();
    if (quantity->prec > comm_prec + extend_by_digits) {
#ifdef INTEGER_MATH
      mpz_round(MP(quantity), MP(quantity), quantity->prec,
		comm_prec + extend_by_digits);
#endif // INTEGER_MATH
      quantity->prec = comm_prec + extend_by_digits;
    }
  }

  return *this;
}


amount_t::precision_t amount_t::precision() const
{
  if (! quantity)
    throw_(amount_error,
	   "Cannot determine precision of an uninitialized amount");

  return quantity->prec;
}

bool amount_t::keep_precision() const
{
  if (! quantity)
    throw_(amount_error,
	   "Cannot determine if precision of an uninitialized amount is kept");

  return quantity->has_flags(BIGINT_KEEP_PREC);
}

void amount_t::set_keep_precision(const bool keep) const
{
  if (! quantity)
    throw_(amount_error,
	   "Cannot set whether to keep the precision of an uninitialized amount");

  if (keep)
    quantity->add_flags(BIGINT_KEEP_PREC);
  else
    quantity->drop_flags(BIGINT_KEEP_PREC);
}

amount_t::precision_t amount_t::display_precision(const bool full_precision) const
{
  if (! quantity)
    throw_(amount_error,
	   "Cannot determine display precision of an uninitialized amount");

  commodity_t& comm(commodity());

  if (! comm || full_precision || keep_precision())
    return quantity->prec;
  else if (comm.precision() != quantity->prec)
    return comm.precision();
  else if (quantity->prec)
    return quantity->prec;

  return 0;
}

amount_t& amount_t::in_place_negate()
{
  if (quantity) {
    _dup();
#ifdef INTEGER_MATH
    mpz_neg(MP(quantity), MP(quantity));
#else
    mpq_neg(MP(quantity), MP(quantity));
#endif
  } else {
    throw_(amount_error, "Cannot negate an uninitialized amount");
  }
  return *this;
}

#ifdef INTEGER_MATH

amount_t& amount_t::in_place_round()
{
  if (! quantity)
    throw_(amount_error, "Cannot round an uninitialized amount");

  if (has_commodity())
    in_place_round(commodity().precision());

  return *this;
}

amount_t& amount_t::in_place_round(precision_t prec)
{
  if (! quantity)
    throw_(amount_error, "Cannot round an uninitialized amount");

  if (quantity && quantity->prec <= prec) {
    if (keep_precision()) {
      _dup();
      set_keep_precision(false);
    }
    return *this;
  }

  DEBUG("amount.round", "Rounding " << *this << " to precision " << prec);

  _dup();
  mpz_round(MP(quantity), MP(quantity), quantity->prec, prec);

  quantity->prec = prec;
  set_keep_precision(false);

  DEBUG("amount.round", "  result = " << *this);

  return *this;
}

#endif // INTEGER_MATH

amount_t amount_t::unround() const
{
  if (! quantity)
    throw_(amount_error, "Cannot unround an uninitialized amount");
  else if (keep_precision())
    return *this;

  amount_t t(*this);
  t._dup();
  t.set_keep_precision(true);

  return t;
}

amount_t& amount_t::in_place_reduce()
{
  if (! quantity)
    throw_(amount_error, "Cannot reduce an uninitialized amount");

  while (commodity_ && commodity().smaller()) {
    *this *= commodity().smaller()->number();
    commodity_ = commodity().smaller()->commodity_;
  }
  return *this;
}

amount_t& amount_t::in_place_unreduce()
{
  if (! quantity)
    throw_(amount_error, "Cannot unreduce an uninitialized amount");

  while (commodity_ && commodity().larger()) {
    *this /= commodity().larger()->number();
    commodity_ = commodity().larger()->commodity_;
    if (abs() < amount_t(1L))
      break;
  }
  return *this;
}

optional<amount_t> amount_t::value(const optional<datetime_t>&   moment,
				   const optional<commodity_t&>& in_terms_of) const
{
  if (quantity) {
    optional<price_point_t> point(commodity().find_price(in_terms_of, moment));
    if (point)
#ifdef INTEGER_MATH
      return (point->price * number()).round();
#else
      return point->price * number();
#endif
  } else {
    throw_(amount_error, "Cannot determine value of an uninitialized amount");
  }
  return none;
}


int amount_t::sign() const
{
  if (! quantity)
    throw_(amount_error, "Cannot determine sign of an uninitialized amount");

#ifdef INTEGER_MATH
  return mpz_sgn(MP(quantity));
#else
  return mpq_sgn(MP(quantity));
#endif
}

bool amount_t::is_zero() const
{
  if (! quantity)
    throw_(amount_error, "Cannot determine if an uninitialized amount is zero");

  if (has_commodity()) {
    if (quantity->prec <= commodity().precision() || keep_precision()) {
      return is_realzero();
    } else {
#ifdef INTEGER_MATH
      return round(commodity().precision()).sign() == 0;
#else
      char * buf;

      mpfr_set_q(tempf, MP(quantity), GMP_RNDN);
      mpfr_asprintf(&buf, "%.*RNf", commodity().precision(), tempf);

      bool all_zeroes = true;
      for (const char * p = buf; *p; p++) {
	if (*p != '0' || *p != '.') {
	  all_zeroes = false;
	  break;
	}
      }

      mpfr_free_str(buf);
      return all_zeroes;
#endif
    }
  }
  return is_realzero();
}


double amount_t::to_double(bool no_check) const
{
  if (! quantity)
    throw_(amount_error, "Cannot convert an uninitialized amount to a double");

#ifdef INTEGER_MATH
  mpz_t remainder;
  mpz_init(remainder);

  mpz_set(temp, MP(quantity));
  mpz_ui_pow_ui(divisor, 10, quantity->prec);
  mpz_tdiv_qr(temp, remainder, temp, divisor);

  char * quotient_s  = mpz_get_str(NULL, 10, temp);
  char * remainder_s = mpz_get_str(NULL, 10, remainder);

  std::ostringstream num;
  num << quotient_s << '.' << remainder_s;

  std::free(quotient_s);
  std::free(remainder_s);

  mpz_clear(remainder);

  double value = lexical_cast<double>(num.str());

  if (! no_check && *this != value)
    throw_(amount_error, "Conversion of amount to_double loses precision");

  return value;
#else
  mpfr_set_q(tempf, MP(quantity), GMP_RNDN);
  return mpfr_get_d(tempf, GMP_RNDN);
#endif
}

long amount_t::to_long(bool no_check) const
{
  if (! quantity)
    throw_(amount_error, "Cannot convert an uninitialized amount to a long");

#ifdef INTEGER_MATH
  mpz_set(temp, MP(quantity));
  mpz_ui_pow_ui(divisor, 10, quantity->prec);
  mpz_tdiv_q(temp, temp, divisor);
#else
  mpfr_set_q(tempf, MP(quantity), GMP_RNDN);
  mpfr_get_z(temp, tempf, GMP_RNDN);
#endif
  long value = mpz_get_si(temp);

  if (! no_check && *this != value)
    throw_(amount_error, "Conversion of amount to_long loses precision");

  return value;
}

bool amount_t::fits_in_double() const
{
  double value = to_double(true);
  return *this == amount_t(value);
}

bool amount_t::fits_in_long() const
{
  long value = to_long(true);
  return *this == amount_t(value);
}


void amount_t::annotate(const annotation_t& details)
{
  commodity_t *		  this_base;
  annotated_commodity_t * this_ann = NULL;

  if (! quantity)
    throw_(amount_error, "Cannot annotate the commodity of an uninitialized amount");
  else if (! has_commodity())
    throw_(amount_error, "Cannot annotate an amount with no commodity");

  if (commodity().annotated) {
    this_ann  = &as_annotated_commodity(commodity());
    this_base = &this_ann->referent();
  } else {
    this_base = &commodity();
  }
  assert(this_base);

  DEBUG("amounts.commodities", "Annotating commodity for amount "
	<< *this << std::endl << details);

  if (commodity_t * ann_comm =
      this_base->parent().find_or_create(*this_base, details))
    set_commodity(*ann_comm);
#ifdef ASSERTS_ON
  else
    assert(false);
#endif

  DEBUG("amounts.commodities", "  Annotated amount is " << *this);
}

bool amount_t::is_annotated() const
{
  if (! quantity)
    throw_(amount_error,
	   "Cannot determine if an uninitialized amount's commodity is annotated");

  assert(! commodity().annotated || as_annotated_commodity(commodity()).details);
  return commodity().annotated;
}

annotation_t& amount_t::annotation()
{
  if (! quantity)
    throw_(amount_error,
	   "Cannot return commodity annotation details of an uninitialized amount");

  if (! commodity().is_annotated())
    throw_(amount_error,
	   "Request for annotation details from an unannotated amount");

  annotated_commodity_t& ann_comm(as_annotated_commodity(commodity()));
  return ann_comm.details;
}

amount_t amount_t::strip_annotations(const bool _keep_price,
				     const bool _keep_date,
				     const bool _keep_tag) const
{
  if (! quantity)
    throw_(amount_error,
	   "Cannot strip commodity annotations from an uninitialized amount");

  if (! commodity().annotated ||
      (_keep_price && _keep_date && _keep_tag))
    return *this;

  amount_t t(*this);
  t.set_commodity(as_annotated_commodity(commodity()).
		  strip_annotations(_keep_price, _keep_date, _keep_tag));
  return t;
}


namespace {
  void parse_quantity(std::istream& in, string& value)
  {
    char buf[256];
    char c = peek_next_nonws(in);
    READ_INTO(in, buf, 255, c,
	      std::isdigit(c) || c == '-' || c == '.' || c == ',');

    int len = std::strlen(buf);
    while (len > 0 && ! std::isdigit(buf[len - 1])) {
      buf[--len] = '\0';
      in.unget();
    }

    value = buf;
  }
}

bool amount_t::parse(std::istream& in, const parse_flags_t& flags)
{
  // The possible syntax for an amount is:
  //
  //   [-]NUM[ ]SYM [@ AMOUNT]
  //   SYM[ ][-]NUM [@ AMOUNT]

  string       symbol;
  string       quant;
  annotation_t details;
  bool	       negative	  = false;

  commodity_t::flags_t comm_flags = COMMODITY_STYLE_DEFAULTS;

  char c = peek_next_nonws(in);
  if (c == '-') {
    negative = true;
    in.get(c);
    c = peek_next_nonws(in);
  }

  char n;
  if (std::isdigit(c)) {
    parse_quantity(in, quant);

    if (! in.eof() && ((n = in.peek()) != '\n')) {
      if (std::isspace(n))
	comm_flags |= COMMODITY_STYLE_SEPARATED;

      commodity_t::parse_symbol(in, symbol);

      if (! symbol.empty())
	comm_flags |= COMMODITY_STYLE_SUFFIXED;

      if (! in.eof() && ((n = in.peek()) != '\n'))
	details.parse(in);
    }
  } else {
    commodity_t::parse_symbol(in, symbol);

    if (! in.eof() && ((n = in.peek()) != '\n')) {
      if (std::isspace(in.peek()))
	comm_flags |= COMMODITY_STYLE_SEPARATED;

      parse_quantity(in, quant);

      if (! quant.empty() && ! in.eof() && ((n = in.peek()) != '\n'))
	details.parse(in);
    }
  }

  if (quant.empty()) {
    if (flags.has_flags(PARSE_SOFT_FAIL))
      return false;
    else
      throw_(amount_error, "No quantity specified for amount");
  }

  // Allocate memory for the amount's quantity value.  We have to
  // monitor the allocation in an auto_ptr because this function gets
  // called sometimes from amount_t's constructor; and if there is an
  // exeception thrown by any of the function calls after this point,
  // the destructor will never be called and the memory never freed.

  std::auto_ptr<bigint_t> safe_holder;

  if (! quantity) {
    quantity = new bigint_t;
    safe_holder.reset(quantity);
  }
  else if (quantity->ref > 1) {
    _release();
    quantity = new bigint_t;
    safe_holder.reset(quantity);
  }

  // Create the commodity if has not already been seen, and update the
  // precision if something greater was used for the quantity.

  bool newly_created = false;

  if (symbol.empty()) {
    commodity_ = NULL;
  } else {
    commodity_ = current_pool->find(symbol);
    if (! commodity_) {
      commodity_ = current_pool->create(symbol);
      newly_created = true;
    }
    assert(commodity_);

    if (details)
      commodity_ = current_pool->find_or_create(*commodity_, details);
  }

  // Determine the precision of the amount, based on the usage of
  // comma or period.

  string::size_type last_comma  = quant.rfind(',');
  string::size_type last_period = quant.rfind('.');

  if (last_comma != string::npos && last_period != string::npos) {
    comm_flags |= COMMODITY_STYLE_THOUSANDS;
    if (last_comma > last_period) {
      comm_flags |= COMMODITY_STYLE_EUROPEAN;
      quantity->prec = quant.length() - last_comma - 1;
    } else {
      quantity->prec = quant.length() - last_period - 1;
    }
  }
  else if (last_comma != string::npos &&
	   commodity().has_flags(COMMODITY_STYLE_EUROPEAN)) {
    comm_flags |= COMMODITY_STYLE_EUROPEAN;
    quantity->prec = quant.length() - last_comma - 1;
  }
  else if (last_period != string::npos &&
	   ! (commodity().has_flags(COMMODITY_STYLE_EUROPEAN))) {
    quantity->prec = quant.length() - last_period - 1;
  }
  else {
    quantity->prec = 0;
  }

  // Set the commodity's flags and precision accordingly

  if (commodity_ && ! flags.has_flags(PARSE_NO_MIGRATE)) {
    commodity().add_flags(comm_flags);

    if (quantity->prec > commodity().precision())
      commodity().set_precision(quantity->prec);
  }

  // Setup the amount's own flags

  if (flags.has_flags(PARSE_NO_MIGRATE))
    set_keep_precision(true);

  // Now we have the final number.  Remove commas and periods, if
  // necessary.

  if (last_comma != string::npos || last_period != string::npos) {
    int		       len = quant.length();
    scoped_array<char> buf(new char[len + 1]);
    const char *       p   = quant.c_str();
    char *	       t   = buf.get();

    while (*p) {
      if (*p == ',' || *p == '.')
	p++;
      *t++ = *p++;
    }
    *t = '\0';

#ifdef INTEGER_MATH
    mpz_set_str(MP(quantity), buf.get(), 10);
#else
    mpq_set_str(MP(quantity), buf.get(), 10);
    mpz_ui_pow_ui(temp, 10, quantity->prec);
    mpq_set_z(tempq, temp);
    mpq_div(MP(quantity), MP(quantity), tempq);
#endif
  } else {
#ifdef INTEGER_MATH
    mpz_set_str(MP(quantity), quant.c_str(), 10);
#else
    mpq_set_str(MP(quantity), quant.c_str(), 10);
#endif
  }

  if (negative)
    in_place_negate();

  if (! flags.has_flags(PARSE_NO_REDUCE))
    in_place_reduce();

  safe_holder.release();	// `this->quantity' owns the pointer

  assert(valid());

  return true;
}

void amount_t::parse_conversion(const string& larger_str,
				const string& smaller_str)
{
  amount_t larger, smaller;

  larger.parse(larger_str, PARSE_NO_REDUCE);
  smaller.parse(smaller_str, PARSE_NO_REDUCE);

  larger *= smaller.number();

  if (larger.commodity()) {
    larger.commodity().set_smaller(smaller);
    larger.commodity().add_flags(smaller.commodity().flags() |
				 COMMODITY_NOMARKET);
  }
  if (smaller.commodity())
    smaller.commodity().set_larger(larger);
}

void amount_t::print(std::ostream& _out, bool omit_commodity,
		     bool full_precision) const
{
  assert(valid());

  if (! quantity) {
    _out << "<null>";
    return;
  }

  amount_t base(*this);
  if (! amount_t::keep_base)
    base.in_place_unreduce();

  std::ostringstream out;

  commodity_t& comm(base.commodity());
  precision_t  precision = 0;

#ifdef INTEGER_MATH

  mpz_t quotient;
  mpz_t rquotient;
  mpz_t remainder;

  mpz_init(quotient);
  mpz_init(rquotient);
  mpz_init(remainder);

  bool negative = false;

  // Ensure the value is rounded to the commodity's precision before
  // outputting it.  NOTE: `rquotient' is used here as a temp variable!

  if (quantity) {
    if (! comm || full_precision || base.keep_precision()) {
      mpz_ui_pow_ui(divisor, 10, base.quantity->prec);
      mpz_tdiv_qr(quotient, remainder, MP(base.quantity), divisor);
      precision = base.quantity->prec;
    }
    else if (comm.precision() < base.quantity->prec) {
      mpz_round(rquotient, MP(base.quantity), base.quantity->prec,
		comm.precision());
      mpz_ui_pow_ui(divisor, 10, comm.precision());
      mpz_tdiv_qr(quotient, remainder, rquotient, divisor);
      precision = comm.precision();
    }
    else if (comm.precision() > base.quantity->prec) {
      mpz_ui_pow_ui(divisor, 10, comm.precision() - base.quantity->prec);
      mpz_mul(rquotient, MP(base.quantity), divisor);
      mpz_ui_pow_ui(divisor, 10, comm.precision());
      mpz_tdiv_qr(quotient, remainder, rquotient, divisor);
      precision = comm.precision();
    }
    else if (base.quantity->prec) {
      mpz_ui_pow_ui(divisor, 10, base.quantity->prec);
      mpz_tdiv_qr(quotient, remainder, MP(base.quantity), divisor);
      precision = base.quantity->prec;
    }
    else {
      mpz_set(quotient, MP(base.quantity));
      mpz_set_ui(remainder, 0);
      precision = 0;
    }

    if (mpz_sgn(quotient) < 0 || mpz_sgn(remainder) < 0) {
      negative = true;

      mpz_abs(quotient, quotient);
      mpz_abs(remainder, remainder);
    }
    mpz_set(rquotient, remainder);
  }

  if (! omit_commodity && ! comm.has_flags(COMMODITY_STYLE_SUFFIXED)) {
    comm.print(out);
    if (comm.has_flags(COMMODITY_STYLE_SEPARATED))
      out << " ";
  }

  if (negative)
    out << "-";

  if (! quantity || mpz_sgn(quotient) == 0) {
    out << '0';
  }
  else if (omit_commodity || ! comm.has_flags(COMMODITY_STYLE_THOUSANDS)) {
    char * p = mpz_get_str(NULL, 10, quotient);
    out << p;
    std::free(p);
  }
  else {
    std::list<string> strs;
    char buf[4];

    for (int powers = 0; true; powers += 3) {
      if (powers > 0) {
	mpz_ui_pow_ui(divisor, 10, powers);
	mpz_tdiv_q(temp, quotient, divisor);
	if (mpz_sgn(temp) == 0)
	  break;
	mpz_tdiv_r_ui(temp, temp, 1000);
      } else {
	mpz_tdiv_r_ui(temp, quotient, 1000);
      }
      mpz_get_str(buf, 10, temp);
      strs.push_back(buf);
    }

    bool printed = false;

    for (std::list<string>::reverse_iterator i = strs.rbegin();
	 i != strs.rend();
	 i++) {
      if (printed) {
	out << (comm.has_flags(COMMODITY_STYLE_EUROPEAN) ? '.' : ',');
	out.width(3);
	out.fill('0');
      }
      out << *i;

      printed = true;
    }
  }

  if (quantity && precision) {
    std::ostringstream final;
    final.width(precision);
    final.fill('0');
    char * p = mpz_get_str(NULL, 10, rquotient);
    final << p;
    std::free(p);

    const string& str(final.str());
    int i, len = str.length();
    const char * q = str.c_str();
    for (i = len; i > 0; i--)
      if (q[i - 1] != '0')
	break;

    string ender;
    if (i == len)
      ender = str;
    else if (i < comm.precision())
      ender = string(str, 0, comm.precision());
    else
      ender = string(str, 0, i);

    if (! ender.empty()) {
      if (omit_commodity)
	out << '.';
      else
	out << (comm.has_flags(COMMODITY_STYLE_EUROPEAN) ? ',' : '.');
      out << ender;
    }
  }

  if (! omit_commodity && comm.has_flags(COMMODITY_STYLE_SUFFIXED)) {
    if (comm.has_flags(COMMODITY_STYLE_SEPARATED))
      out << " ";
    comm.print(out);
  }

  mpz_clear(quotient);
  mpz_clear(rquotient);
  mpz_clear(remainder);

#else // INTEGER_MATH

  char * buf;
  mpfr_set_q(tempf, MP(quantity), GMP_RNDN);
  mpfr_asprintf(&buf, "%.*RNf", base.display_precision(full_precision), tempf);
  DEBUG("amount.print", "mpfr_print = " << buf);

  try {
    if (! omit_commodity && ! comm.has_flags(COMMODITY_STYLE_SUFFIXED)) {
      comm.print(out);
      if (comm.has_flags(COMMODITY_STYLE_SEPARATED))
	out << " ";
    }

    if (omit_commodity || ! comm.has_flags(COMMODITY_STYLE_THOUSANDS)) {
      if (! comm.has_flags(COMMODITY_STYLE_EUROPEAN))
	out << buf;
      else
	for (const char * p = buf; *p; p++)
	  if (*p == '.')
	    out << ',';
	  else
	    out << *p;
    } else {
      // Count the number of integer digits
      int integer_digits = 0;
      for (const char * p = buf; *p; p++) {
	if (*p == '.')
	  break;
	else if (std::isdigit(*p))
	  integer_digits++;
      }

      for (const char * p = buf; *p; p++) {
	if (*p == '.' && comm.has_flags(COMMODITY_STYLE_EUROPEAN))
	  out << ',';
	else
	  out << *p;

	if (std::isdigit(*p) && integer_digits > 3 &&
	    --integer_digits % 3 == 0)
	  out << ',';
      }
    }
  }
  catch (...) {
    mpfr_free_str(buf);
    throw;
  }
  mpfr_free_str(buf);

  if (! omit_commodity && comm.has_flags(COMMODITY_STYLE_SUFFIXED)) {
    if (comm.has_flags(COMMODITY_STYLE_SEPARATED))
      out << " ";
    comm.print(out);
  }

#endif // INTEGER_MATH

  // If there are any annotations associated with this commodity,
  // output them now.

  if (! omit_commodity && comm.annotated) {
    annotated_commodity_t& ann(static_cast<annotated_commodity_t&>(comm));
    assert(&*ann.details.price != this);
    ann.write_annotations(out);
  }

  // Things are output to a string first, so that if anyone has
  // specified a width or fill for _out, it will be applied to the
  // entire amount string, and not just the first part.

  _out << out.str();
}

void amount_t::read(std::istream& in)
{
  using namespace ledger::binary;

  // Read in the commodity for this amount

  commodity_t::ident_t ident;
  read_long(in, ident);
  if (ident == 0xffffffff)
    commodity_ = NULL;
  else if (ident == 0)
    commodity_ = current_pool->null_commodity;
  else {
    commodity_ = current_pool->find(ident);
    assert(commodity_);
  }

  // Read in the quantity

  char byte;
  in.read(&byte, sizeof(byte));

  if (byte < 3) {
    quantity = new bigint_t;

#ifndef INTEGER_MATH
    mpz_t numerator;
    mpz_t denominator;
#endif

    unsigned short len;
    in.read(reinterpret_cast<char *>(&len), sizeof(len));
    assert(len < 4096);
    static char buf[4096];
    in.read(buf, len);
#ifdef INTEGER_MATH
    mpz_import(MP(quantity), len / sizeof(short), 1, sizeof(short),
	       0, 0, buf);
#else
    mpz_init(numerator);
    mpz_import(numerator, len / sizeof(short), 1, sizeof(short),
	       0, 0, buf);

    in.read(reinterpret_cast<char *>(&len), sizeof(len));
    assert(len < 4096);
    in.read(buf, len);
    mpz_init(denominator);
    mpz_import(denominator, len / sizeof(short), 1, sizeof(short),
	       0, 0, buf);

    mpq_set_num(MP(quantity), numerator);
    mpq_set_den(MP(quantity), denominator);
#endif

    char negative;
    in.read(&negative, sizeof(negative));
    if (negative)
#ifdef INTEGER_MATH
      mpz_neg(MP(quantity), MP(quantity));
#else
      mpq_neg(MP(quantity), MP(quantity));
#endif

    in.read(reinterpret_cast<char *>(&quantity->prec), sizeof(quantity->prec));

    bigint_t::flags_t tflags;
    in.read(reinterpret_cast<char *>(&tflags), sizeof(tflags));
    quantity->set_flags(tflags);
  }
  else {
    assert(false);
  }
}

void amount_t::read(const char *& data,
		    char **	  pool,
		    char **       pool_next)
{
  using namespace ledger::binary;

  // Read in the commodity for this amount

  commodity_t::ident_t ident;
  read_long(data, ident);
  if (ident == 0xffffffff)
    commodity_ = NULL;
  else if (ident == 0)
    commodity_ = current_pool->null_commodity;
  else {
    commodity_ = current_pool->find(ident);
    assert(commodity_);
  }

  // Read in the quantity

  char byte = *data++;;

  if (byte < 3) {
    if (byte == 2) {
      quantity = new(reinterpret_cast<bigint_t *>(*pool_next)) bigint_t;
      *pool_next += sizeof(bigint_t);
    } else {
      quantity = new bigint_t;
    }

#ifndef INTEGER_MATH
    mpz_t numerator;
    mpz_t denominator;
#endif

    unsigned short len =
      *reinterpret_cast<unsigned short *>(const_cast<char *>(data));
    data += sizeof(unsigned short);
#ifdef INTEGER_MATH
    mpz_import(MP(quantity), len / sizeof(short), 1, sizeof(short),
	       0, 0, data);
#else
    mpz_init(numerator);
    mpz_import(numerator, len / sizeof(short), 1, sizeof(short),
	       0, 0, data);

    len = *reinterpret_cast<unsigned short *>(const_cast<char *>(data));
    data += sizeof(unsigned short);
    mpz_init(denominator);
    mpz_import(denominator, len / sizeof(short), 1, sizeof(short),
	       0, 0, data);

    mpq_set_num(MP(quantity), numerator);
    mpq_set_den(MP(quantity), denominator);
#endif
    data += len;

    char negative = *data++;
    if (negative)
#ifdef INTEGER_MATH
      mpz_neg(MP(quantity), MP(quantity));
#else
      mpq_neg(MP(quantity), MP(quantity));
#endif

    quantity->prec = *reinterpret_cast<precision_t *>(const_cast<char *>(data));
    data += sizeof(precision_t);
    quantity->set_flags(*reinterpret_cast<bigint_t::flags_t *>(const_cast<char *>(data)));
    data += sizeof(bigint_t::flags_t);

    if (byte == 2)
      quantity->add_flags(BIGINT_BULK_ALLOC);
  } else {
    uint_fast32_t index = *reinterpret_cast<uint_fast32_t *>(const_cast<char *>(data));
    data += sizeof(uint_fast32_t);

    quantity = reinterpret_cast<bigint_t *>(*pool + (index - 1) * sizeof(bigint_t));

    DEBUG("amounts.refs",
	   quantity << " ref++, now " << (quantity->ref + 1));
    quantity->ref++;
  }
}

void amount_t::write(std::ostream& out, std::size_t index) const
{
  using namespace ledger::binary;

  // Write out the commodity for this amount

  if (! quantity)
    throw_(amount_error, "Cannot serialize an uninitialized amount");

  if (commodity_)
    write_long(out, commodity_->ident);
  else
    write_long<commodity_t::ident_t>(out, 0xffffffff);

  // Write out the quantity

  char byte;

  if (index == 0 || quantity->index == 0) {
    if (index != 0) {
      quantity->index = index;	// if !optimized, this is garbage
      byte = 2;
    } else {
      byte = 1;
    }
    out.write(&byte, sizeof(byte));

    std::size_t size;
    static char buf[4096];
#ifdef INTEGER_MATH
    mpz_export(buf, &size, 1, sizeof(short), 0, 0, MP(quantity));
#else
    mpz_t numerator;
    mpz_t denominator;

    mpz_init(numerator);
    mpq_get_num(numerator, MP(quantity));
    mpz_export(buf, &size, 1, sizeof(short), 0, 0, numerator);

    mpz_init(denominator);
    mpq_get_den(denominator, MP(quantity));
    mpz_export(buf, &size, 1, sizeof(short), 0, 0, denominator);
#endif
    unsigned short len = size * sizeof(short);
    out.write(reinterpret_cast<char *>(&len), sizeof(len));
    if (len) {
      assert(len < 4096);
      out.write(buf, len);
    }

#ifdef INTEGER_MATH
    byte = mpz_sgn(MP(quantity)) < 0 ? 1 : 0;
#else
    byte = mpq_sgn(MP(quantity)) < 0 ? 1 : 0;
#endif
    out.write(&byte, sizeof(byte));

    out.write(reinterpret_cast<char *>(&quantity->prec), sizeof(quantity->prec));
    bigint_t::flags_t tflags = quantity->flags() & ~BIGINT_BULK_ALLOC;
    assert(sizeof(tflags) == sizeof(bigint_t::flags_t));
    out.write(reinterpret_cast<char *>(&tflags), sizeof(tflags));
  } else {
    assert(quantity->ref > 1);

    // Since this value has already been written, we simply write
    // out a reference to which one it was.
    byte = 3;
    out.write(&byte, sizeof(byte));
    out.write(reinterpret_cast<char *>(&quantity->index), sizeof(quantity->index));
  }
}

void amount_t::read_xml(std::istream& in)
{
}

void amount_t::write_xml(std::ostream& out, const int depth) const
{
  xml_print(out, "<amount>\n", depth);

  commodity().write_xml(out, depth + 1);

  xml_print(out, "<quantity>", depth + 1);
  out << quantity_string() << "</quantity>\n";

  xml_print(out, "</amount>\n", depth);
}

bool amount_t::valid() const
{
  if (quantity) {
    if (! quantity->valid())
      return false;

    if (quantity->ref == 0) {
      DEBUG("ledger.validate", "amount_t: quantity->ref == 0");
      return false;
    }
  }
  else if (commodity_) {
    DEBUG("ledger.validate", "amount_t: commodity_ != NULL");
    return false;
  }
  return true;
}

} // namespace ledger
