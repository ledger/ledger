/**
 * @file   amount.cc
 * @author John Wiegley
 * @date   Thu Apr 26 15:19:46 2007
 * 
 * @brief  Types for handling commoditized math.
 * 
 * This file defines member functions for amount_t and the various
 * flavors of commodity_t.
 */

/*
 * Copyright (c) 2003-2007, John Wiegley.  All rights reserved.
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

bool do_cleanup = true;

bool amount_t::keep_price   = false;
bool amount_t::keep_date    = false;
bool amount_t::keep_tag	    = false;
bool amount_t::keep_base    = false;
bool amount_t::full_strings = false;

#define BIGINT_BULK_ALLOC 0x0001
#define BIGINT_KEEP_PREC  0x0002

class amount_t::bigint_t
{
 public:
  mpz_t		val;
  unsigned char prec;
  unsigned char flags;
  unsigned int	ref;
  unsigned int	index;

  bigint_t() : prec(0), flags(0), ref(1), index(0) {
    TRACE_CTOR(bigint_t, "");
    mpz_init(val);
  }
  bigint_t(mpz_t _val) : prec(0), flags(0), ref(1), index(0) {
    TRACE_CTOR(bigint_t, "mpz_t");
    mpz_init_set(val, _val);
  }
  bigint_t(const bigint_t& other)
    : prec(other.prec), flags(other.flags & BIGINT_KEEP_PREC),
      ref(1), index(0) {
    TRACE_CTOR(bigint_t, "copy");
    mpz_init_set(val, other.val);
  }
  ~bigint_t();
};

unsigned int sizeof_bigint_t() {
  return sizeof(amount_t::bigint_t);
}

#define MPZ(x) ((x)->val)

#ifndef THREADSAFE
static mpz_t temp;		// these are the global temp variables
static mpz_t divisor;
#endif

static amount_t::bigint_t * true_value = NULL;

inline amount_t::bigint_t::~bigint_t() {
  TRACE_DTOR(bigint_t);
  assert(ref == 0 || (! do_cleanup && this == true_value));
  mpz_clear(val);
}

#ifndef THREADSAFE
base_commodities_map commodity_base_t::commodities;

commodity_base_t::updater_t * commodity_base_t::updater = NULL;

commodities_map	    commodity_t::commodities;
commodities_array * commodity_t::commodities_by_ident;
bool		    commodity_t::commodities_sorted = false;
commodity_t *	    commodity_t::null_commodity;
commodity_t *	    commodity_t::default_commodity  = NULL;
#endif

void amount_t::initialize()
{
  mpz_init(temp);
  mpz_init(divisor);

  true_value = new amount_t::bigint_t;
  mpz_set_ui(true_value->val, 1);

  commodity_base_t::updater = NULL;

  commodity_t::commodities_by_ident = new commodities_array;

  commodity_t::default_commodity = NULL;
  commodity_t::null_commodity    = commodity_t::create("");
  commodity_t::null_commodity->add_flags(COMMODITY_STYLE_NOMARKET |
					 COMMODITY_STYLE_BUILTIN);

  // Add time commodity conversions, so that timelog's may be parsed
  // in terms of seconds, but reported as minutes or hours.
  commodity_t * commodity = commodity_t::create("s");
  commodity->add_flags(COMMODITY_STYLE_NOMARKET | COMMODITY_STYLE_BUILTIN);

  parse_conversion("1.0m", "60s");
  parse_conversion("1.0h", "60m");
}

void amount_t::shutdown()
{
  mpz_clear(temp);
  mpz_clear(divisor);

  if (commodity_base_t::updater) {
    delete commodity_base_t::updater;
    commodity_base_t::updater = NULL;
  }

  for (base_commodities_map::iterator i = commodity_base_t::commodities.begin();
       i != commodity_base_t::commodities.end();
       i++)
    delete (*i).second;

  for (commodities_map::iterator i = commodity_t::commodities.begin();
       i != commodity_t::commodities.end();
       i++)
    delete (*i).second;

  commodity_base_t::commodities.clear();
  commodity_t::commodities.clear();

  delete commodity_t::commodities_by_ident;
  commodity_t::commodities_by_ident = NULL;

  commodity_t::null_commodity    = NULL;
  commodity_t::default_commodity = NULL;

  true_value->ref--;
  assert(true_value->ref == 0);
  delete true_value;
  true_value = NULL;
}

static void mpz_round(mpz_t out, mpz_t value, int value_prec, int round_prec)
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

amount_t::amount_t(const long val)
{
  TRACE_CTOR(amount_t, "const long");
  if (val != 0) {
    quantity = new bigint_t;
    mpz_set_si(MPZ(quantity), val);
  } else {
    quantity = NULL;
  }
  commodity_ = NULL;
}

amount_t::amount_t(const unsigned long val)
{
  TRACE_CTOR(amount_t, "const unsigned long");
  if (val != 0) {
    quantity = new bigint_t;
    mpz_set_ui(MPZ(quantity), val);
  } else {
    quantity = NULL;
  }
  commodity_ = NULL;
}

namespace {
  unsigned char convert_double(mpz_t dest, double val)
  {
#ifndef HAVE_GDTOA
    // This code is far too imprecise to be worthwhile.

    mpf_t temp;
    mpf_init_set_d(temp, val);

    mp_exp_t exp;
    char * buf = mpf_get_str(NULL, &exp, 10, 1000, temp);

    int len = std::strlen(buf);
    if (len > 0 && buf[0] == '-')
      exp++;

    if (exp <= len) {
      exp = len - exp;
    } else {
      // There were trailing zeros, which we have to put back on in
      // order to convert this buffer into an integer.

      int zeroes = exp - len;

      char * newbuf = (char *)std::malloc(len + zeroes);
      std::strcpy(newbuf, buf);

      int i;
      for (i = 0; i < zeroes; i++)
	newbuf[len + i] = '0';
      newbuf[len + i] = '\0';

      free(buf);
      buf = newbuf;

      exp = (len - exp) + zeroes;
    }

    mpz_set_str(dest, buf, 10);
    free(buf);

    return (unsigned char)exp;
#else
    int decpt, sign;
    char * buf = dtoa(val, 0, 0, &decpt, &sign, NULL);
    char * result;
    int len = std::strlen(buf);

    if (decpt <= len) {
      decpt  = len - decpt;
      result = NULL;
    } else {
      // There were trailing zeros, which we have to put back on in
      // order to convert this buffer into an integer.

      int zeroes = decpt - len;
      result = new char[len + zeroes];

      std::strcpy(result, buf);
      int i;
      for (i = 0; i < zeroes; i++)
	result[len + i] = '0';
      result[len + i] = '\0';

      decpt = (len - decpt) + zeroes;
    }

    if (sign) {
      char * newbuf = new char[std::strlen(result ? result : buf) + 1];
      newbuf[0] = '-';
      std::strcpy(&newbuf[1], result ? result : buf);
      mpz_set_str(dest, newbuf, 10);
      delete[] newbuf;
    } else {
      mpz_set_str(dest, result ? result : buf, 10);
    }

    if (result)
      delete[] result;
    freedtoa(buf);

    return decpt;
#endif
  }
}

amount_t::amount_t(const double val)
{
  TRACE_CTOR(amount_t, "const double");
  quantity = new bigint_t;
  quantity->prec = convert_double(MPZ(quantity), val);
  commodity_ = NULL;
}

void amount_t::_release()
{
  DEBUG("amounts.refs", quantity << " ref--, now " << (quantity->ref - 1));

  if (--quantity->ref == 0) {
    if (! (quantity->flags & BIGINT_BULK_ALLOC))
      delete quantity;
    else
      quantity->~bigint_t();
  }
}

void amount_t::_init()
{
  if (! quantity) {
    quantity = new bigint_t;
  }
  else if (quantity->ref > 1) {
    _release();
    quantity = new bigint_t;
  }
}

void amount_t::_dup()
{
  if (quantity->ref > 1) {
    bigint_t * q = new bigint_t(*quantity);
    _release();
    quantity = q;
  }
}

void amount_t::_copy(const amount_t& amt)
{
  if (quantity != amt.quantity) {
    if (quantity)
      _release();

    // Never maintain a pointer into a bulk allocation pool; such
    // pointers are not guaranteed to remain.
    if (amt.quantity->flags & BIGINT_BULK_ALLOC) {
      quantity = new bigint_t(*amt.quantity);
    } else {
      quantity = amt.quantity;
      DEBUG("amounts.refs",
	     quantity << " ref++, now " << (quantity->ref + 1));
      quantity->ref++;
    }
  }
  commodity_ = amt.commodity_;
}

amount_t& amount_t::operator=(const string& val)
{
  std::istringstream str(val);
  parse(str);
  return *this;
}

amount_t& amount_t::operator=(const char * val)
{
  string valstr(val);
  std::istringstream str(valstr);
  parse(str);
  return *this;
}

// assignment operator
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

amount_t& amount_t::operator=(const long val)
{
  if (val == 0) {
    if (quantity)
      _clear();
  } else {
    commodity_ = NULL;
    _init();
    mpz_set_si(MPZ(quantity), val);
  }
  return *this;
}

amount_t& amount_t::operator=(const unsigned long val)
{
  if (val == 0) {
    if (quantity)
      _clear();
  } else {
    commodity_ = NULL;
    _init();
    mpz_set_ui(MPZ(quantity), val);
  }
  return *this;
}

amount_t& amount_t::operator=(const double val)
{
  commodity_ = NULL;
  _init();
  quantity->prec = convert_double(MPZ(quantity), val);
  return *this;
}


void amount_t::_resize(unsigned int prec)
{
  assert(prec < 256);

  if (! quantity || prec == quantity->prec)
    return;

  _dup();

  if (prec < quantity->prec) {
    mpz_ui_pow_ui(divisor, 10, quantity->prec - prec);
    mpz_tdiv_q(MPZ(quantity), MPZ(quantity), divisor);
  } else {
    mpz_ui_pow_ui(divisor, 10, prec - quantity->prec);
    mpz_mul(MPZ(quantity), MPZ(quantity), divisor);
  }

  quantity->prec = prec;
}


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


amount_t& amount_t::operator+=(const amount_t& amt)
{
  if (commodity() != amt.commodity()) {
    throw amount_exception
      (string("Adding amounts with different commodities: ") +
       (has_commodity() ? commodity_->qualified_symbol : "NONE") + " != " +
       (amt.has_commodity() ? amt.commodity_->qualified_symbol : "NONE"),
       context());
  }

  if (! amt.quantity)
    return *this;

  if (! quantity) {
    _copy(amt);
    return *this;
  }

  _dup();

  if (quantity->prec == amt.quantity->prec) {
    mpz_add(MPZ(quantity), MPZ(quantity), MPZ(amt.quantity));
  }
  else if (quantity->prec < amt.quantity->prec) {
    _resize(amt.quantity->prec);
    mpz_add(MPZ(quantity), MPZ(quantity), MPZ(amt.quantity));
  }
  else {
    amount_t t = amt;
    t._resize(quantity->prec);
    mpz_add(MPZ(quantity), MPZ(quantity), MPZ(t.quantity));
  }

  return *this;
}

amount_t& amount_t::operator-=(const amount_t& amt)
{
  if (commodity() != amt.commodity())
    throw amount_exception
      (string("Subtracting amounts with different commodities: ") +
       (has_commodity() ? commodity_->qualified_symbol : "NONE") + " != " +
       (amt.has_commodity() ? amt.commodity_->qualified_symbol : "NONE"),
       context());

  if (! amt.quantity)
    return *this;

  if (! quantity) {
    quantity  = new bigint_t(*amt.quantity);
    commodity_ = amt.commodity_;
    mpz_neg(MPZ(quantity), MPZ(quantity));
    return *this;
  }

  _dup();

  if (quantity->prec == amt.quantity->prec) {
    mpz_sub(MPZ(quantity), MPZ(quantity), MPZ(amt.quantity));
  }
  else if (quantity->prec < amt.quantity->prec) {
    _resize(amt.quantity->prec);
    mpz_sub(MPZ(quantity), MPZ(quantity), MPZ(amt.quantity));
  }
  else {
    amount_t t = amt;
    t._resize(quantity->prec);
    mpz_sub(MPZ(quantity), MPZ(quantity), MPZ(t.quantity));
  }

  return *this;
}

amount_t& amount_t::operator*=(const amount_t& amt)
{
  if (has_commodity() && amt.has_commodity() &&
      commodity() != amt.commodity()) {
    throw amount_exception
      (string("Multiplying amounts with different commodities: ") +
       (has_commodity() ? commodity_->qualified_symbol : "NONE") + " != " +
       (amt.has_commodity() ? amt.commodity_->qualified_symbol : "NONE"),
       context());
  }

  if (! amt.quantity) {
    *this = *this - *this;	// preserve our commodity
    goto finish;
  }
  else if (! quantity) {
    *this = amt;
    *this = *this - *this;	// preserve the foreign commodity
    goto finish;
  }

  _dup();

  mpz_mul(MPZ(quantity), MPZ(quantity), MPZ(amt.quantity));
  quantity->prec += amt.quantity->prec;

 finish:
  if (! has_commodity())
    commodity_ = amt.commodity_;

  if (has_commodity() && ! (quantity->flags & BIGINT_KEEP_PREC)) {
    unsigned int comm_prec = commodity().precision();
    if (quantity->prec > comm_prec + 6U) {
      mpz_round(MPZ(quantity), MPZ(quantity), quantity->prec, comm_prec + 6U);
      quantity->prec = comm_prec + 6U;
    }
  }

  return *this;
}

amount_t& amount_t::operator/=(const amount_t& amt)
{
  if (has_commodity() && amt.has_commodity() &&
      commodity() != amt.commodity()) {
    throw amount_exception
      (string("Dividing amounts with different commodities: ") +
       (has_commodity() ? commodity_->qualified_symbol : "NONE") + " != " +
       (amt.has_commodity() ? amt.commodity_->qualified_symbol : "NONE"),
       context());
  }

  if (! amt.quantity || ! amt) {
    throw amount_exception("Divide by zero", context());
  }
  else if (! quantity) {
    *this = amt;
    *this = *this - *this;	// preserve the foreign commodity
    goto finish;
  }

  _dup();

  // Increase the value's precision, to capture fractional parts after
  // the divide.  Round up in the last position.

  mpz_ui_pow_ui(divisor, 10, (2 * amt.quantity->prec) + quantity->prec + 7U);
  mpz_mul(MPZ(quantity), MPZ(quantity), divisor);
  mpz_tdiv_q(MPZ(quantity), MPZ(quantity), MPZ(amt.quantity));
  quantity->prec += amt.quantity->prec + quantity->prec + 7U;

  mpz_round(MPZ(quantity), MPZ(quantity), quantity->prec, quantity->prec - 1);
  quantity->prec -= 1;

 finish:
  if (! has_commodity())
    commodity_ = amt.commodity_;

  // If this amount has a commodity, and we're not dealing with plain
  // numbers, or internal numbers (which keep full precision at all
  // times), then round the number to within the commodity's precision
  // plus six places.

  if (has_commodity() && ! (quantity->flags & BIGINT_KEEP_PREC)) {
    unsigned int comm_prec = commodity().precision();
    if (quantity->prec > comm_prec + 6U) {
      mpz_round(MPZ(quantity), MPZ(quantity), quantity->prec, comm_prec + 6U);
      quantity->prec = comm_prec + 6U;
    }
  }

  return *this;
}

// unary negation
void amount_t::in_place_negate()
{
  if (quantity) {
    _dup();
    mpz_neg(MPZ(quantity), MPZ(quantity));
  }
}

int amount_t::sign() const
{
  return quantity ? mpz_sgn(MPZ(quantity)) : 0;
}

int amount_t::compare(const amount_t& amt) const
{
  if (! quantity) {
    if (! amt.quantity)
      return 0;
    return - amt.sign();
  }
  if (! amt.quantity)
    return sign();

  if (has_commodity() && amt.commodity() && commodity() != amt.commodity())
    throw amount_exception
      (string("Cannot compare amounts with different commodities: ") +
       commodity().symbol() + " and " + amt.commodity().symbol(),
       context());

  if (quantity->prec == amt.quantity->prec) {
    return mpz_cmp(MPZ(quantity), MPZ(amt.quantity));
  }
  else if (quantity->prec < amt.quantity->prec) {
    amount_t t = *this;
    t._resize(amt.quantity->prec);
    return mpz_cmp(MPZ(t.quantity), MPZ(amt.quantity));
  }
  else {
    amount_t t = amt;
    t._resize(quantity->prec);
    return mpz_cmp(MPZ(quantity), MPZ(t.quantity));
  }
}

bool amount_t::operator==(const amount_t& amt) const
{
  if (commodity() != amt.commodity())
    return false;
  return compare(amt) == 0;
}

bool amount_t::operator!=(const amount_t& amt) const
{
  if (commodity() != amt.commodity())
    return true;
  return compare(amt) != 0;
}

bool amount_t::zero() const
{
  if (! quantity)
    return true;

  if (has_commodity()) {
    if (quantity->prec <= commodity().precision())
      return realzero();
    else
      return round(commodity().precision()).sign() == 0;
  }
  return realzero();
}

amount_t::operator long() const
{
  if (! quantity)
    return 0;

  mpz_set(temp, MPZ(quantity));
  mpz_ui_pow_ui(divisor, 10, quantity->prec);
  mpz_tdiv_q(temp, temp, divisor);
  return mpz_get_si(temp);
}

amount_t::operator double() const
{
  if (! quantity)
    return 0.0;

  mpz_t remainder;
  mpz_init(remainder);

  mpz_set(temp, MPZ(quantity));
  mpz_ui_pow_ui(divisor, 10, quantity->prec);
  mpz_tdiv_qr(temp, remainder, temp, divisor);

  char * quotient_s  = mpz_get_str(NULL, 10, temp);
  char * remainder_s = mpz_get_str(NULL, 10, remainder);

  std::ostringstream num;
  num << quotient_s << '.' << remainder_s;

  std::free(quotient_s);
  std::free(remainder_s);

  mpz_clear(remainder);

  return std::atof(num.str().c_str());
}

amount_t amount_t::value(const moment_t& moment) const
{
  if (quantity) {
    amount_t amt(commodity().value(moment));
    if (! amt.realzero())
      return (amt * number()).round();
  }
  return *this;
}

amount_t amount_t::round(unsigned int prec) const
{
  amount_t t = *this;

  if (! quantity || quantity->prec <= prec) {
    if (quantity && quantity->flags & BIGINT_KEEP_PREC) {
      t._dup();
      t.quantity->flags &= ~BIGINT_KEEP_PREC;
    }
    return t;
  }

  t._dup();

  mpz_round(MPZ(t.quantity), MPZ(t.quantity), t.quantity->prec, prec);

  t.quantity->prec = prec;
  t.quantity->flags &= ~BIGINT_KEEP_PREC;

  return t;
}

amount_t amount_t::unround() const
{
  if (! quantity) {
    amount_t t(0L);
    assert(t.quantity);
    t.quantity->flags |= BIGINT_KEEP_PREC;
    return t;
  }
  else if (quantity->flags & BIGINT_KEEP_PREC) {
    return *this;
  }

  amount_t t = *this;
  t._dup();
  t.quantity->flags |= BIGINT_KEEP_PREC;

  return t;
}

void amount_t::print_quantity(std::ostream& out) const
{
  if (! quantity) {
    out << "0";
    return;
  }

  mpz_t quotient;
  mpz_t rquotient;
  mpz_t remainder;

  mpz_init(quotient);
  mpz_init(rquotient);
  mpz_init(remainder);

  bool negative = false;

  // Ensure the value is rounded to the commodity's precision before
  // outputting it.  NOTE: `rquotient' is used here as a temp variable!

  commodity_t&  comm(commodity());
  unsigned char precision;

  if (! comm || quantity->flags & BIGINT_KEEP_PREC) {
    mpz_ui_pow_ui(divisor, 10, quantity->prec);
    mpz_tdiv_qr(quotient, remainder, MPZ(quantity), divisor);
    precision = quantity->prec;
  }
  else if (comm.precision() < quantity->prec) {
    mpz_round(rquotient, MPZ(quantity), quantity->prec, comm.precision());
    mpz_ui_pow_ui(divisor, 10, comm.precision());
    mpz_tdiv_qr(quotient, remainder, rquotient, divisor);
    precision = comm.precision();
  }
  else if (comm.precision() > quantity->prec) {
    mpz_ui_pow_ui(divisor, 10, comm.precision() - quantity->prec);
    mpz_mul(rquotient, MPZ(quantity), divisor);
    mpz_ui_pow_ui(divisor, 10, comm.precision());
    mpz_tdiv_qr(quotient, remainder, rquotient, divisor);
    precision = comm.precision();
  }
  else if (quantity->prec) {
    mpz_ui_pow_ui(divisor, 10, quantity->prec);
    mpz_tdiv_qr(quotient, remainder, MPZ(quantity), divisor);
    precision = quantity->prec;
  }
  else {
    mpz_set(quotient, MPZ(quantity));
    mpz_set_ui(remainder, 0);
    precision = 0;
  }

  if (mpz_sgn(quotient) < 0 || mpz_sgn(remainder) < 0) {
    negative = true;

    mpz_abs(quotient, quotient);
    mpz_abs(remainder, remainder);
  }
  mpz_set(rquotient, remainder);

  if (mpz_sgn(quotient) == 0 && mpz_sgn(rquotient) == 0) {
    out << "0";
    return;
  }

  if (negative)
    out << "-";

  if (mpz_sgn(quotient) == 0) {
    out << '0';
  } else {
    char * p = mpz_get_str(NULL, 10, quotient);
    out << p;
    std::free(p);
  }

  if (precision) {
    out << '.';

    out.width(precision);
    out.fill('0');

    char * p = mpz_get_str(NULL, 10, rquotient);
    out << p;
    std::free(p);
  }

  mpz_clear(quotient);
  mpz_clear(rquotient);
  mpz_clear(remainder);
}

void amount_t::print(std::ostream& _out, bool omit_commodity,
		     bool full_precision) const
{
  amount_t base(*this);
  if (! amount_t::keep_base && commodity().larger()) {
    amount_t last(*this);
    while (last.commodity().larger()) {
      last /= last.commodity().larger()->number();
      last.commodity_ = last.commodity().larger()->commodity_;
      if (last.abs() < 1)
	break;
      base = last.round();
    }
  }

  std::ostringstream out;

  mpz_t quotient;
  mpz_t rquotient;
  mpz_t remainder;

  mpz_init(quotient);
  mpz_init(rquotient);
  mpz_init(remainder);

  bool negative = false;

  // Ensure the value is rounded to the commodity's precision before
  // outputting it.  NOTE: `rquotient' is used here as a temp variable!

  commodity_t&	comm(base.commodity());
  unsigned char precision = 0;

  if (quantity) {
    if (! comm || full_precision || base.quantity->flags & BIGINT_KEEP_PREC) {
      mpz_ui_pow_ui(divisor, 10, base.quantity->prec);
      mpz_tdiv_qr(quotient, remainder, MPZ(base.quantity), divisor);
      precision = base.quantity->prec;
    }
    else if (comm.precision() < base.quantity->prec) {
      mpz_round(rquotient, MPZ(base.quantity), base.quantity->prec,
		comm.precision());
      mpz_ui_pow_ui(divisor, 10, comm.precision());
      mpz_tdiv_qr(quotient, remainder, rquotient, divisor);
      precision = comm.precision();
    }
    else if (comm.precision() > base.quantity->prec) {
      mpz_ui_pow_ui(divisor, 10, comm.precision() - base.quantity->prec);
      mpz_mul(rquotient, MPZ(base.quantity), divisor);
      mpz_ui_pow_ui(divisor, 10, comm.precision());
      mpz_tdiv_qr(quotient, remainder, rquotient, divisor);
      precision = comm.precision();
    }
    else if (base.quantity->prec) {
      mpz_ui_pow_ui(divisor, 10, base.quantity->prec);
      mpz_tdiv_qr(quotient, remainder, MPZ(base.quantity), divisor);
      precision = base.quantity->prec;
    }
    else {
      mpz_set(quotient, MPZ(base.quantity));
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

  if (! omit_commodity && ! (comm.flags() & COMMODITY_STYLE_SUFFIXED)) {
    comm.write(out);

    if (comm.flags() & COMMODITY_STYLE_SEPARATED)
      out << " ";
  }

  if (negative)
    out << "-";

  if (! quantity || mpz_sgn(quotient) == 0) {
    out << '0';
  }
  else if (! (comm.flags() & COMMODITY_STYLE_THOUSANDS)) {
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
	out << (comm.flags() & COMMODITY_STYLE_EUROPEAN ? '.' : ',');
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
      out << ((comm.flags() & COMMODITY_STYLE_EUROPEAN) ? ',' : '.');
      out << ender;
    }
  }

  if (! omit_commodity && comm.flags() & COMMODITY_STYLE_SUFFIXED) {
    if (comm.flags() & COMMODITY_STYLE_SEPARATED)
      out << " ";

    comm.write(out);
  }

  mpz_clear(quotient);
  mpz_clear(rquotient);
  mpz_clear(remainder);

  // If there are any annotations associated with this commodity,
  // output them now.

  if (! omit_commodity && comm.annotated) {
    annotated_commodity_t& ann(static_cast<annotated_commodity_t&>(comm));
    assert(&ann.price != this);
    ann.write_annotations(out);
  }

  // Things are output to a string first, so that if anyone has
  // specified a width or fill for _out, it will be applied to the
  // entire amount string, and not just the first part.

  _out << out.str();

  return;
}

static void parse_quantity(std::istream& in, string& value)
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

// Invalid commodity characters:
//   SPACE, TAB, NEWLINE, RETURN
//   0-9 . , ; - + * / ^ ? : & | ! =
//   < > { } [ ] ( ) @

int invalid_chars[256] = {
      /* 0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f */
/* 00 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0,
/* 10 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* 20 */ 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1,
/* 30 */ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
/* 40 */ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* 50 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0,
/* 60 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* 70 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0,
/* 80 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* 90 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* a0 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* b0 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* c0 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* d0 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* e0 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
/* f0 */ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
};

static void parse_commodity(std::istream& in, string& symbol)
{
  char buf[256];
  char c = peek_next_nonws(in);
  if (c == '"') {
    in.get(c);
    READ_INTO(in, buf, 255, c, c != '"');
    if (c == '"')
      in.get(c);
    else
      throw amount_exception("Quoted commodity symbol lacks closing quote",
			     context());
  } else {
    READ_INTO(in, buf, 255, c, ! invalid_chars[(unsigned char)c]);
  }
  symbol = buf;
}

bool parse_annotations(std::istream& in, amount_t& price,
		       moment_t& date, string& tag)
{
  bool has_date = false;
  
  do {
    char buf[256];
    char c = peek_next_nonws(in);
    if (c == '{') {
      if (price)
	throw amount_exception("Commodity specifies more than one price",
			       context());

      in.get(c);
      READ_INTO(in, buf, 255, c, c != '}');
      if (c == '}')
	in.get(c);
      else
	throw amount_exception("Commodity price lacks closing brace", context());

      price.parse(buf, AMOUNT_PARSE_NO_MIGRATE);
      price.in_place_reduce();

      // Since this price will maintain its own precision, make sure
      // it is at least as large as the base commodity, since the user
      // may have only specified {$1} or something similar.

      if (price.has_commodity() &&
	  price.quantity->prec < price.commodity().precision())
	price = price.round();	// no need to retain individual precision
    }
    else if (c == '[') {
      if (is_valid_moment(date))
	throw amount_exception("Commodity specifies more than one date",
			       context());

      in.get(c);
      READ_INTO(in, buf, 255, c, c != ']');
      if (c == ']')
	in.get(c);
      else
	throw amount_exception("Commodity date lacks closing bracket",
			       context());

      date = parse_datetime(buf);
      has_date = true;
    }
    else if (c == '(') {
      if (! tag.empty())
	throw amount_exception("Commodity specifies more than one tag",
			       context());

      in.get(c);
      READ_INTO(in, buf, 255, c, c != ')');
      if (c == ')')
	in.get(c);
      else
	throw amount_exception("Commodity tag lacks closing parenthesis",
			       context());

      tag = buf;
    }
    else {
      break;
    }
  } while (true);

  DEBUG("amounts.commodities",
	 "Parsed commodity annotations: "
	 << "  price " << price << " "
	 << "  date " << date << " "
	 << "  tag " << tag);

  return has_date;
}

void amount_t::parse(std::istream& in, unsigned char flags)
{
  // The possible syntax for an amount is:
  //
  //   [-]NUM[ ]SYM [@ AMOUNT]
  //   SYM[ ][-]NUM [@ AMOUNT]

  string  symbol;
  string  quant;
  amount_t     tprice;
  moment_t   tdate;
  bool         had_date = false;
  string  tag;
  unsigned int comm_flags = COMMODITY_STYLE_DEFAULTS;
  bool         negative = false;

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

      parse_commodity(in, symbol);

      if (! symbol.empty())
	comm_flags |= COMMODITY_STYLE_SUFFIXED;

      if (! in.eof() && ((n = in.peek()) != '\n'))
	had_date = parse_annotations(in, tprice, tdate, tag);
    }
  } else {
    parse_commodity(in, symbol);

    if (! in.eof() && ((n = in.peek()) != '\n')) {
      if (std::isspace(in.peek()))
	comm_flags |= COMMODITY_STYLE_SEPARATED;

      parse_quantity(in, quant);

      if (! quant.empty() && ! in.eof() && ((n = in.peek()) != '\n'))
	had_date = parse_annotations(in, tprice, tdate, tag);
    }
  }

  if (quant.empty())
    throw amount_exception("No quantity specified for amount",
			   context());

  _init();

  // Create the commodity if has not already been seen, and update the
  // precision if something greater was used for the quantity.

  bool newly_created = false;

  if (symbol.empty()) {
    commodity_ = NULL;
  } else {
    commodity_ = commodity_t::find(symbol);
    if (! commodity_) {
      commodity_ = commodity_t::create(symbol);
      newly_created = true;
    }
    assert(commodity_);

    if (! tprice.realzero() || had_date || ! tag.empty())
      commodity_ =
	annotated_commodity_t::find_or_create(*commodity_, tprice, tdate, tag);
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
	   commodity().flags() & COMMODITY_STYLE_EUROPEAN) {
    quantity->prec = quant.length() - last_comma - 1;
  }
  else if (last_period != string::npos &&
	   ! (commodity().flags() & COMMODITY_STYLE_EUROPEAN)) {
    quantity->prec = quant.length() - last_period - 1;
  }
  else {
    quantity->prec = 0;
  }

  // Set the commodity's flags and precision accordingly

  if (commodity_ && (newly_created || ! (flags & AMOUNT_PARSE_NO_MIGRATE))) {
    commodity().add_flags(comm_flags);
    if (quantity->prec > commodity().precision())
      commodity().set_precision(quantity->prec);
  }

  if (flags & AMOUNT_PARSE_NO_MIGRATE)
    quantity->flags |= BIGINT_KEEP_PREC;

  // Now we have the final number.  Remove commas and periods, if
  // necessary.

  if (last_comma != string::npos || last_period != string::npos) {
    int		 len = quant.length();
    char *	 buf = new char[len + 1];
    const char * p   = quant.c_str();
    char *	 t   = buf;

    while (*p) {
      if (*p == ',' || *p == '.')
	p++;
      *t++ = *p++;
    }
    *t = '\0';

    mpz_set_str(MPZ(quantity), buf, 10);
    delete[] buf;
  } else {
    mpz_set_str(MPZ(quantity), quant.c_str(), 10);
  }

  if (negative)
    in_place_negate();

  if (! (flags & AMOUNT_PARSE_NO_REDUCE))
    in_place_reduce();
}

void amount_t::in_place_reduce()
{
  while (commodity_ && commodity().smaller()) {
    *this *= commodity().smaller()->number();
    commodity_ = commodity().smaller()->commodity_;
  }
}

void parse_conversion(const string& larger_str,
		      const string& smaller_str)
{
  amount_t larger, smaller;

  larger.parse(larger_str.c_str(), AMOUNT_PARSE_NO_REDUCE);
  smaller.parse(smaller_str.c_str(), AMOUNT_PARSE_NO_REDUCE);

  larger *= smaller.number();

  if (larger.commodity()) {
    larger.commodity().set_smaller(smaller);
    larger.commodity().add_flags(smaller.commodity().flags() |
				 COMMODITY_STYLE_NOMARKET);
  }
  if (smaller.commodity())
    smaller.commodity().set_larger(larger);
}

void amount_t::read(std::istream& in)
{
  commodity_t::ident_t ident;
  read_binary_long(in, ident);
  if (ident == 0xffffffff)
    commodity_ = NULL;
  else if (ident == 0)
    commodity_ = commodity_t::null_commodity;
  else
    commodity_ = (*commodity_t::commodities_by_ident)[ident - 1];

  read_quantity(in);
}

void amount_t::read(char *& data)
{
  commodity_t::ident_t ident;
  read_binary_long(data, ident);
  if (ident == 0xffffffff)
    commodity_ = NULL;
  else if (ident == 0)
    commodity_ = commodity_t::null_commodity;
  else
    commodity_ = (*commodity_t::commodities_by_ident)[ident - 1];

  read_quantity(data);
}

void amount_t::write(std::ostream& out) const
{
  if (commodity_)
    write_binary_long(out, commodity_->ident);
  else
    write_binary_long<commodity_t::ident_t>(out, 0xffffffff);

  write_quantity(out);
}


#ifndef THREADSAFE
static char *	    bigints;
static char *	    bigints_next;
static unsigned int bigints_index;
static unsigned int bigints_count;
#endif

void amount_t::read_quantity(char *& data)
{
  char byte = *data++;;

  if (byte == 0) {
    quantity = NULL;
  }
  else if (byte == 1) {
    quantity = new((bigint_t *)bigints_next) bigint_t;
    bigints_next += sizeof(bigint_t);

    unsigned short len = *((unsigned short *) data);
    data += sizeof(unsigned short);
    mpz_import(MPZ(quantity), len / sizeof(short), 1, sizeof(short),
	       0, 0, data);
    data += len;

    char negative = *data++;
    if (negative)
      mpz_neg(MPZ(quantity), MPZ(quantity));

    quantity->prec = *((unsigned char *) data);
    data += sizeof(unsigned char);
    quantity->flags = *((unsigned char *) data);
    data += sizeof(unsigned char);
    quantity->flags |= BIGINT_BULK_ALLOC;
  } else {
    unsigned int index = *((unsigned int *) data);
    data += sizeof(unsigned int);

    quantity = (bigint_t *) (bigints + (index - 1) * sizeof(bigint_t));
    DEBUG("amounts.refs",
	   quantity << " ref++, now " << (quantity->ref + 1));
    quantity->ref++;
  }
}

#ifndef THREADSAFE
static char buf[4096];
#endif

void amount_t::read_quantity(std::istream& in)
{
  char byte;
  in.read(&byte, sizeof(byte));

  if (byte == 0) {
    quantity = NULL;
  }
  else if (byte == 1) {
    quantity = new bigint_t;

    unsigned short len;
    in.read((char *)&len, sizeof(len));
    assert(len < 4096);
    in.read(buf, len);
    mpz_import(MPZ(quantity), len / sizeof(short), 1, sizeof(short),
	       0, 0, buf);

    char negative;
    in.read(&negative, sizeof(negative));
    if (negative)
      mpz_neg(MPZ(quantity), MPZ(quantity));

    in.read((char *)&quantity->prec, sizeof(quantity->prec));
    in.read((char *)&quantity->flags, sizeof(quantity->flags));
  }
  else {
    assert(0);
  }
}

void amount_t::write_quantity(std::ostream& out) const
{
  char byte;

  if (! quantity) {
    byte = 0;
    out.write(&byte, sizeof(byte));
    return;
  }

  if (quantity->index == 0) {
    quantity->index = ++bigints_index;
    bigints_count++;

    byte = 1;
    out.write(&byte, sizeof(byte));

    std::size_t size;
    mpz_export(buf, &size, 1, sizeof(short), 0, 0, MPZ(quantity));
    unsigned short len = size * sizeof(short);
    out.write((char *)&len, sizeof(len));
    if (len) {
      assert(len < 4096);
      out.write(buf, len);
    }

    byte = mpz_sgn(MPZ(quantity)) < 0 ? 1 : 0;
    out.write(&byte, sizeof(byte));

    out.write((char *)&quantity->prec, sizeof(quantity->prec));
    unsigned char flags = quantity->flags & ~BIGINT_BULK_ALLOC;
    assert(sizeof(flags) == sizeof(quantity->flags));
    out.write((char *)&flags, sizeof(flags));
  } else {
    assert(quantity->ref > 1);

    // Since this value has already been written, we simply write
    // out a reference to which one it was.
    byte = 2;
    out.write(&byte, sizeof(byte));
    out.write((char *)&quantity->index, sizeof(quantity->index));
  }
}

bool amount_t::valid() const
{
  if (quantity) {
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

void amount_t::annotate_commodity(const amount_t&    tprice,
				  const moment_t&  tdate,
				  const string& tag)
{
  const commodity_t *	  this_base;
  annotated_commodity_t * this_ann = NULL;

  if (commodity().annotated) {
    this_ann = &static_cast<annotated_commodity_t&>(commodity());
    this_base = this_ann->ptr;
  } else {
    this_base = &commodity();
  }
  assert(this_base);

  DEBUG("amounts.commodities", "Annotating commodity for amount "
	 << *this << std::endl
	 << "  price " << tprice << " "
	 << "  date " << tdate << " "
	 << "  tag " << tag);

  commodity_t * ann_comm =
    annotated_commodity_t::find_or_create
      (*this_base, ! tprice && this_ann ? this_ann->price : tprice,
       ! is_valid_moment(tdate) && this_ann ? this_ann->date : tdate,
       tag.empty() && this_ann ? this_ann->tag : tag);
  if (ann_comm)
    set_commodity(*ann_comm);

  DEBUG("amounts.commodities", "  Annotated amount is " << *this);
}

amount_t amount_t::strip_annotations(const bool _keep_price,
				     const bool _keep_date,
				     const bool _keep_tag) const
{
  if (! commodity().annotated ||
      (_keep_price && _keep_date && _keep_tag))
    return *this;

  DEBUG("amounts.commodities", "Reducing commodity for amount "
	 << *this << std::endl
	 << "  keep price " << _keep_price << " "
	 << "  keep date " << _keep_date << " "
	 << "  keep tag " << _keep_tag);

  annotated_commodity_t&
    ann_comm(static_cast<annotated_commodity_t&>(commodity()));
  assert(ann_comm.base);

  commodity_t * new_comm;

  if ((_keep_price && ann_comm.price) ||
      (_keep_date  && is_valid_moment(ann_comm.date)) ||
      (_keep_tag   && ! ann_comm.tag.empty()))
  {
    new_comm = annotated_commodity_t::find_or_create
      (*ann_comm.ptr, _keep_price ? ann_comm.price : amount_t(),
       _keep_date ? ann_comm.date : moment_t(),
       _keep_tag ? ann_comm.tag : "");
  } else {
    new_comm = commodity_t::find_or_create(ann_comm.base_symbol());
  }
  assert(new_comm);

  amount_t t(*this);
  t.set_commodity(*new_comm);
  DEBUG("amounts.commodities", "  Reduced amount is " << t);

  return t;
}

amount_t amount_t::price() const
{
  if (commodity_ && commodity_->annotated) {
    amount_t t(((annotated_commodity_t *)commodity_)->price);
    t *= number();
    DEBUG("amounts.commodities",
	   "Returning price of " << *this << " = " << t);
    return t;
  }
  return *this;
}

moment_t amount_t::date() const
{
  if (commodity_ && commodity_->annotated) {
    DEBUG("amounts.commodities",
	   "Returning date of " << *this << " = "
	   << ((annotated_commodity_t *)commodity_)->date);
    return ((annotated_commodity_t *)commodity_)->date;
  }
  return moment_t();
}


void commodity_base_t::add_price(const moment_t& date,
				 const amount_t& price)
{
  if (! history)
    history = new history_t;

  history_map::iterator i = history->prices.find(date);
  if (i != history->prices.end()) {
    (*i).second = price;
  } else {
    std::pair<history_map::iterator, bool> result
      = history->prices.insert(history_pair(date, price));
    assert(result.second);
  }
}

bool commodity_base_t::remove_price(const moment_t& date)
{
  if (history) {
    history_map::size_type n = history->prices.erase(date);
    if (n > 0) {
      if (history->prices.empty())
	history = NULL;
      return true;
    }
  }
  return false;
}

commodity_base_t * commodity_base_t::create(const string& symbol)
{
  commodity_base_t * commodity = new commodity_base_t(symbol);

  DEBUG("amounts.commodities", "Creating base commodity " << symbol);

  std::pair<base_commodities_map::iterator, bool> result
    = commodities.insert(base_commodities_pair(symbol, commodity));
  assert(result.second);

  return commodity;
}

bool commodity_t::needs_quotes(const string& symbol)
{
  for (const char * p = symbol.c_str(); *p; p++)
    if (std::isspace(*p) || std::isdigit(*p) || *p == '-' || *p == '.')
      return true;

  return false;
}

bool commodity_t::valid() const
{
  if (symbol().empty() && this != null_commodity) {
    DEBUG("ledger.validate",
	   "commodity_t: symbol().empty() && this != null_commodity");
    return false;
  }

  if (annotated && ! base) {
    DEBUG("ledger.validate", "commodity_t: annotated && ! base");
    return false;
  }

  if (precision() > 16) {
    DEBUG("ledger.validate", "commodity_t: precision() > 16");
    return false;
  }

  return true;
}

commodity_t * commodity_t::create(const string& symbol)
{
  std::auto_ptr<commodity_t> commodity(new commodity_t);

  commodity->base = commodity_base_t::create(symbol);

  if (needs_quotes(symbol)) {
    commodity->qualified_symbol = "\"";
    commodity->qualified_symbol += symbol;
    commodity->qualified_symbol += "\"";
  } else {
    commodity->qualified_symbol = symbol;
  }

  DEBUG("amounts.commodities",
	 "Creating commodity " << commodity->qualified_symbol);

  std::pair<commodities_map::iterator, bool> result
    = commodities.insert(commodities_pair(symbol, commodity.get()));
  if (! result.second)
    return NULL;

  commodity->ident = commodities_by_ident->size();
  commodities_by_ident->push_back(commodity.get());

  // Start out the new commodity with the default commodity's flags
  // and precision, if one has been defined.
  if (default_commodity)
    commodity->drop_flags(COMMODITY_STYLE_THOUSANDS |
			  COMMODITY_STYLE_NOMARKET);

  return commodity.release();
}

commodity_t * commodity_t::find_or_create(const string& symbol)
{
  DEBUG("amounts.commodities", "Find-or-create commodity " << symbol);

  commodity_t * commodity = find(symbol);
  if (commodity)
    return commodity;
  return create(symbol);
}

commodity_t * commodity_t::find(const string& symbol)
{
  DEBUG("amounts.commodities", "Find commodity " << symbol);

  commodities_map::const_iterator i = commodities.find(symbol);
  if (i != commodities.end())
    return (*i).second;
  return NULL;
}

amount_t commodity_base_t::value(const moment_t& moment)
{
  moment_t age;
  amount_t   price;

  if (history) {
    assert(history->prices.size() > 0);

    if (! is_valid_moment(moment)) {
      history_map::reverse_iterator r = history->prices.rbegin();
      age   = (*r).first;
      price = (*r).second;
    } else {
      history_map::iterator i = history->prices.lower_bound(moment);
      if (i == history->prices.end()) {
	history_map::reverse_iterator r = history->prices.rbegin();
	age   = (*r).first;
	price = (*r).second;
      } else {
	age = (*i).first;
	if (moment != age) {
	  if (i != history->prices.begin()) {
	    --i;
	    age	  = (*i).first;
	    price = (*i).second;
	  } else {
	    age   = moment_t();
	  }
	} else {
	  price = (*i).second;
	}
      }
    }
  }

  if (updater && ! (flags & COMMODITY_STYLE_NOMARKET))
    (*updater)(*this, moment, age,
	       (history && history->prices.size() > 0 ?
		(*history->prices.rbegin()).first : moment_t()), price);

  return price;
}

bool annotated_commodity_t::operator==(const commodity_t& comm) const
{
  // If the base commodities don't match, the game's up.
  if (base != comm.base)
    return false;

  if (price &&
      (! comm.annotated ||
       price != static_cast<const annotated_commodity_t&>(comm).price))
    return false;

  if (is_valid_moment(date) &&
      (! comm.annotated ||
       date != static_cast<const annotated_commodity_t&>(comm).date))
    return false;

  if (! tag.empty() &&
      (! comm.annotated ||
       tag != static_cast<const annotated_commodity_t&>(comm).tag))
    return false;

  return true;
}

void
annotated_commodity_t::write_annotations(std::ostream&      out,
					 const amount_t&    price,
					 const moment_t&  date,
					 const string& tag)
{
  if (price)
    out << " {" << price << '}';

  if (is_valid_moment(date))
    out << " [" << date << ']';

  if (! tag.empty())
    out << " (" << tag << ')';
}

commodity_t *
annotated_commodity_t::create(const commodity_t& comm,
			      const amount_t&    price,
			      const moment_t&  date,
			      const string& tag,
			      const string& mapping_key)
{
  std::auto_ptr<annotated_commodity_t> commodity(new annotated_commodity_t);

  // Set the annotated bits
  commodity->price = price;
  commodity->date  = date;
  commodity->tag   = tag;

  commodity->ptr = &comm;
  assert(commodity->ptr);
  commodity->base = comm.base;
  assert(commodity->base);

  commodity->qualified_symbol = comm.symbol();

  DEBUG("amounts.commodities", "Creating annotated commodity "
	 << "symbol " << commodity->symbol()
	 << " key " << mapping_key << std::endl
	 << "  price " << price << " "
	 << "  date " << date << " "
	 << "  tag " << tag);

  // Add the fully annotated name to the map, so that this symbol may
  // quickly be found again.
  std::pair<commodities_map::iterator, bool> result
    = commodities.insert(commodities_pair(mapping_key, commodity.get()));
  if (! result.second)
    return NULL;

  commodity->ident = commodities_by_ident->size();
  commodities_by_ident->push_back(commodity.get());

  return commodity.release();
}

namespace {
  string make_qualified_name(const commodity_t& comm,
				  const amount_t&    price,
				  const moment_t&  date,
				  const string& tag)
  {
    if (price < 0)
      throw amount_exception("A commodity's price may not be negative",
			     context());

    std::ostringstream name;

    comm.write(name);
    annotated_commodity_t::write_annotations(name, price, date, tag);

    DEBUG("amounts.commodities", "make_qualified_name for "
	   << comm.qualified_symbol << std::endl
	   << "  price " << price << " "
	   << "  date " << date << " "
	   << "  tag " << tag);

    DEBUG("amounts.commodities", "qualified_name is " << name.str());

    return name.str();
  }
}

commodity_t *
annotated_commodity_t::find_or_create(const commodity_t& comm,
				      const amount_t&    price,
				      const moment_t&  date,
				      const string& tag)
{
  string name = make_qualified_name(comm, price, date, tag);

  commodity_t * ann_comm = commodity_t::find(name);
  if (ann_comm) {
    assert(ann_comm->annotated);
    return ann_comm;
  }
  return create(comm, price, date, tag, name);
}

bool compare_amount_commodities::operator()(const amount_t * left,
					    const amount_t * right) const
{
  commodity_t& leftcomm(left->commodity());
  commodity_t& rightcomm(right->commodity());

  int cmp = leftcomm.base_symbol().compare(rightcomm.base_symbol());
  if (cmp != 0)
    return cmp < 0;

  if (! leftcomm.annotated) {
    assert(rightcomm.annotated);
    return true;
  }
  else if (! rightcomm.annotated) {
    assert(leftcomm.annotated);
    return false;
  }
  else {
    annotated_commodity_t& aleftcomm(static_cast<annotated_commodity_t&>(leftcomm));
    annotated_commodity_t& arightcomm(static_cast<annotated_commodity_t&>(rightcomm));

    if (! aleftcomm.price && arightcomm.price)
      return true;
    if (aleftcomm.price && ! arightcomm.price)
      return false;

    if (aleftcomm.price && arightcomm.price) {
      amount_t leftprice(aleftcomm.price);
      leftprice.in_place_reduce();
      amount_t rightprice(arightcomm.price);
      rightprice.in_place_reduce();

      if (leftprice.commodity() == rightprice.commodity()) {
	amount_t val = leftprice - rightprice;
	if (val)
	  return val < 0;
      } else {
	// Since we have two different amounts, there's really no way
	// to establish a true sorting order; we'll just do it based
	// on the numerical values.
	leftprice.clear_commodity();
	rightprice.clear_commodity();

	amount_t val = leftprice - rightprice;
	if (val)
	  return val < 0;
      }
    }

    if (! is_valid_moment(aleftcomm.date) &&
	is_valid_moment(arightcomm.date))
      return true;
    if (is_valid_moment(aleftcomm.date) &&
	! is_valid_moment(arightcomm.date))
      return false;

    if (is_valid_moment(aleftcomm.date) &&
	is_valid_moment(arightcomm.date)) {
      duration_t diff = aleftcomm.date - arightcomm.date;
      return diff.is_negative();
    }

    if (aleftcomm.tag.empty() && ! arightcomm.tag.empty())
      return true;
    if (! aleftcomm.tag.empty() && arightcomm.tag.empty())
      return false;

    if (! aleftcomm.tag.empty() && ! arightcomm.tag.empty())
      return aleftcomm.tag < arightcomm.tag;

    assert(0);
    return true;
  }
}

} // namespace ledger
