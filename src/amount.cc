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
#include <math.h>

#include "amount.h"
#include "commodity.h"
#include "annotate.h"
#include "pool.h"

namespace ledger {

bool amount_t::stream_fullstrings = false;

#if !defined(THREADSAFE)
// These global temporaries are pre-initialized for the sake of
// efficiency, and are reused over and over again.
static mpz_t  temp;
static mpq_t  tempq;
static mpfr_t tempf;
static mpfr_t tempfb;
static mpfr_t tempfnum;
static mpfr_t tempfden;
#endif

struct amount_t::bigint_t : public flags::supports_flags<>
{
#define BIGINT_BULK_ALLOC 0x01
#define BIGINT_KEEP_PREC  0x02

  mpq_t          val;
  precision_t    prec;
  uint_least32_t refc;

#define MP(bigint) ((bigint)->val)

  bigint_t() : prec(0), refc(1) {
    mpq_init(val);
    TRACE_CTOR(bigint_t, "");
  }
  bigint_t(const bigint_t& other)
    : supports_flags<>(static_cast<uint_least8_t>
                       (other.flags() & ~BIGINT_BULK_ALLOC)),
      prec(other.prec), refc(1) {
    mpq_init(val);
    mpq_set(val, other.val);
    TRACE_CTOR(bigint_t, "copy");
  }
  ~bigint_t() {
    TRACE_DTOR(bigint_t);
    assert(refc == 0);
    mpq_clear(val);
  }

  bool valid() const {
    if (prec > 1024) {
      DEBUG("ledger.validate", "amount_t::bigint_t: prec > 1024");
      return false;
    }
    if (flags() & ~(BIGINT_BULK_ALLOC | BIGINT_KEEP_PREC)) {
      DEBUG("ledger.validate",
            "amount_t::bigint_t: flags() & ~(BULK_ALLOC | KEEP_PREC)");
      return false;
    }
    return true;
  }
};

bool amount_t::is_initialized = false;

namespace {
  void stream_out_mpq(std::ostream&                 out,
                      mpq_t                         quant,
                      amount_t::precision_t         precision,
                      int                           zeros_prec = -1,
                      mpfr_rnd_t                    rnd        = GMP_RNDN,
                      const optional<commodity_t&>& comm       = none)
  {
    char * buf = NULL;
    try {
#if DEBUG_ON
      IF_DEBUG("amount.convert") {
        char * tbuf = mpq_get_str(NULL, 10, quant);
        DEBUG("amount.convert", "Rational to convert = " << tbuf);
        std::free(tbuf);
      }
#endif

      // Convert the rational number to a floating-point, extending the
      // floating-point to a large enough size to get a precise answer.

      mp_prec_t num_prec =
        static_cast<mpfr_prec_t>(mpz_sizeinbase(mpq_numref(quant), 2));
      num_prec += amount_t::extend_by_digits*64;
      if (num_prec < MPFR_PREC_MIN)
        num_prec = MPFR_PREC_MIN;
      DEBUG("amount.convert", "num prec = " << num_prec);

      mpfr_set_prec(tempfnum, num_prec);
      mpfr_set_z(tempfnum, mpq_numref(quant), rnd);

      mp_prec_t den_prec =
        static_cast<mpfr_prec_t>(mpz_sizeinbase(mpq_denref(quant), 2));
      den_prec += amount_t::extend_by_digits*64;
      if (den_prec < MPFR_PREC_MIN)
        den_prec = MPFR_PREC_MIN;
      DEBUG("amount.convert", "den prec = " << den_prec);

      mpfr_set_prec(tempfden, den_prec);
      mpfr_set_z(tempfden, mpq_denref(quant), rnd);

      mpfr_set_prec(tempfb, num_prec + den_prec);
      mpfr_div(tempfb, tempfnum, tempfden, rnd);

      if (mpfr_asprintf(&buf, "%.*RNf", precision, tempfb) < 0)
        throw_(amount_error,
               _("Cannot output amount to a floating-point representation"));

      DEBUG("amount.convert", "mpfr_print = " << buf
            << " (precision " << precision
            << ", zeros_prec " << zeros_prec << ")");

      if (zeros_prec >= 0) {
        string::size_type index = std::strlen(buf);
        string::size_type point = 0;
        for (string::size_type i = 0; i < index; i++) {
          if (buf[i] == '.') {
            point = i;
            break;
          }
        }
        if (point > 0) {
          while (--index >= (point + 1 + static_cast<std::size_t>(zeros_prec)) &&
                 buf[index] == '0')
            buf[index] = '\0';
          if (index >= (point + static_cast<std::size_t>(zeros_prec)) &&
              buf[index] == '.')
            buf[index] = '\0';
        }
      }

      if (comm) {
        int integer_digits = 0;
        if (comm && comm->has_flags(COMMODITY_STYLE_THOUSANDS)) {
          // Count the number of integer digits
          for (const char * p = buf; *p; p++) {
            if (*p == '.')
              break;
            else if (*p != '-')
              integer_digits++;
          }
        }

        for (const char * p = buf; *p; p++) {
          if (*p == '.') {
            if (("h" == comm->symbol() || "m" == comm->symbol()) && (commodity_t::time_colon_by_default ||
                (comm && comm->has_flags(COMMODITY_STYLE_TIME_COLON))))
              out << ':';
            else if (commodity_t::decimal_comma_by_default ||
                (comm && comm->has_flags(COMMODITY_STYLE_DECIMAL_COMMA)))
              out << ',';
            else
              out << *p;
            assert(integer_digits <= 3);
          }
          else if (*p == '-') {
            out << *p;
          }
          else {
            out << *p;

            if (integer_digits > 3 && --integer_digits % 3 == 0) {
              if (("h" == comm->symbol() || "m" == comm->symbol()) && (commodity_t::time_colon_by_default ||
                  (comm && comm->has_flags(COMMODITY_STYLE_TIME_COLON))))
                out << ':';
              else if (commodity_t::decimal_comma_by_default ||
                  (comm && comm->has_flags(COMMODITY_STYLE_DECIMAL_COMMA)))
                out << '.';
              else
                out << ',';
            }
          }
        }
      } else {
        out << buf;
      }
    }
    catch (...) {
      if (buf != NULL)
        mpfr_free_str(buf);
      throw;
    }
    if (buf != NULL)
      mpfr_free_str(buf);
  }
}

void amount_t::initialize()
{
  if (! is_initialized) {
    mpz_init(temp);
    mpq_init(tempq);
    mpfr_init(tempf);
    mpfr_init(tempfb);
    mpfr_init(tempfnum);
    mpfr_init(tempfden);

    commodity_pool_t::current_pool.reset(new commodity_pool_t);

    // Add time commodity conversions, so that timelogs may be parsed
    // in terms of seconds, but reported as minutes or hours.
    if (commodity_t * commodity = commodity_pool_t::current_pool->create("s"))
      commodity->add_flags(COMMODITY_BUILTIN | COMMODITY_NOMARKET);
    else
      assert(false);

    // Add a "percentile" commodity
    if (commodity_t * commodity = commodity_pool_t::current_pool->create("%"))
      commodity->add_flags(COMMODITY_BUILTIN | COMMODITY_NOMARKET);
    else
      assert(false);

    is_initialized = true;
  }
}

void amount_t::shutdown()
{
  if (is_initialized) {
    mpz_clear(temp);
    mpq_clear(tempq);
    mpfr_clear(tempf);
    mpfr_clear(tempfb);
    mpfr_clear(tempfnum);
    mpfr_clear(tempfden);

    commodity_pool_t::current_pool.reset();

    is_initialized = false;
  }
}

void amount_t::_copy(const amount_t& amt)
{
  VERIFY(amt.valid());

  if (quantity != amt.quantity) {
    if (quantity)
      _release();

    // Never maintain a pointer into a bulk allocation pool; such
    // pointers are not guaranteed to remain.
    if (amt.quantity->has_flags(BIGINT_BULK_ALLOC)) {
      quantity = new bigint_t(*amt.quantity);
    } else {
      quantity = amt.quantity;
      DEBUG("amount.refs",
             quantity << " refc++, now " << (quantity->refc + 1));
      quantity->refc++;
    }
  }
  commodity_ = amt.commodity_;

  VERIFY(valid());
}

void amount_t::_dup()
{
  VERIFY(valid());

  if (quantity->refc > 1) {
    bigint_t * q = new bigint_t(*quantity);
    _release();
    quantity = q;
  }

  VERIFY(valid());
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

void amount_t::_release()
{
  VERIFY(valid());

  DEBUG("amount.refs", quantity << " refc--, now " << (quantity->refc - 1));

  if (--quantity->refc == 0) {
    if (quantity->has_flags(BIGINT_BULK_ALLOC))
      quantity->~bigint_t();
    else
      checked_delete(quantity);
    quantity   = NULL;
    commodity_ = NULL;
  }

  VERIFY(valid());
}


amount_t::amount_t(const double val) : commodity_(NULL)
{
  quantity = new bigint_t;
  mpq_set_d(MP(quantity), val);
  quantity->prec = extend_by_digits; // an approximation
  TRACE_CTOR(amount_t, "const double");
}

amount_t::amount_t(const unsigned long val) : commodity_(NULL)
{
  quantity = new bigint_t;
  mpq_set_ui(MP(quantity), val, 1);
  TRACE_CTOR(amount_t, "const unsigned long");
}

amount_t::amount_t(const long val) : commodity_(NULL)
{
  quantity = new bigint_t;
  mpq_set_si(MP(quantity), val, 1);
  TRACE_CTOR(amount_t, "const long");
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
  VERIFY(amt.valid());

  if (! quantity || ! amt.quantity) {
    if (quantity)
      throw_(amount_error, _("Cannot compare an amount to an uninitialized amount"));
    else if (amt.quantity)
      throw_(amount_error, _("Cannot compare an uninitialized amount to an amount"));
    else
      throw_(amount_error, _("Cannot compare two uninitialized amounts"));
  }

  if (has_commodity() && amt.has_commodity() && commodity() != amt.commodity()) {
    throw_(amount_error,
           _f("Cannot compare amounts with different commodities: '%1%' and '%2%'")
           % commodity() % amt.commodity());
  }

  return mpq_cmp(MP(quantity), MP(amt.quantity));
}

bool amount_t::operator==(const amount_t& amt) const
{
  if ((quantity && ! amt.quantity) || (! quantity && amt.quantity))
    return false;
  else if (! quantity && ! amt.quantity)
    return true;
  else if (commodity() != amt.commodity())
    return false;

  return mpq_equal(MP(quantity), MP(amt.quantity));
}


amount_t& amount_t::operator+=(const amount_t& amt)
{
  VERIFY(amt.valid());

  if (! quantity || ! amt.quantity) {
    if (quantity)
      throw_(amount_error, _("Cannot add an uninitialized amount to an amount"));
    else if (amt.quantity)
      throw_(amount_error, _("Cannot add an amount to an uninitialized amount"));
    else
      throw_(amount_error, _("Cannot add two uninitialized amounts"));
  }

  if (has_commodity() && amt.has_commodity() && commodity() != amt.commodity()) {
    throw_(amount_error,
           _f("Adding amounts with different commodities: '%1%' != '%2%'")
           % commodity() % amt.commodity());
  }

  _dup();

  mpq_add(MP(quantity), MP(quantity), MP(amt.quantity));

  if (has_commodity() == amt.has_commodity())
    if (quantity->prec < amt.quantity->prec)
      quantity->prec = amt.quantity->prec;

  return *this;
}

amount_t& amount_t::operator-=(const amount_t& amt)
{
  VERIFY(amt.valid());

  if (! quantity || ! amt.quantity) {
    if (quantity)
      throw_(amount_error, _("Cannot subtract an amount from an uninitialized amount"));
    else if (amt.quantity)
      throw_(amount_error, _("Cannot subtract an uninitialized amount from an amount"));
    else
      throw_(amount_error, _("Cannot subtract two uninitialized amounts"));
  }

  if (has_commodity() && amt.has_commodity() && commodity() != amt.commodity()) {
    throw_(amount_error,
           _f("Subtracting amounts with different commodities: '%1%' != '%2%'")
           % commodity() % amt.commodity());
  }

  _dup();

  mpq_sub(MP(quantity), MP(quantity), MP(amt.quantity));

  if (has_commodity() == amt.has_commodity())
    if (quantity->prec < amt.quantity->prec)
      quantity->prec = amt.quantity->prec;

  return *this;
}

amount_t& amount_t::multiply(const amount_t& amt, bool ignore_commodity)
{
  VERIFY(amt.valid());

  if (! quantity || ! amt.quantity) {
    if (quantity)
      throw_(amount_error, _("Cannot multiply an amount by an uninitialized amount"));
    else if (amt.quantity)
      throw_(amount_error, _("Cannot multiply an uninitialized amount by an amount"));
    else
      throw_(amount_error, _("Cannot multiply two uninitialized amounts"));
  }

  _dup();

  mpq_mul(MP(quantity), MP(quantity), MP(amt.quantity));
  quantity->prec =
    static_cast<precision_t>(quantity->prec + amt.quantity->prec);

  if (! has_commodity() && ! ignore_commodity)
    commodity_ = amt.commodity_;

  if (has_commodity() && ! keep_precision()) {
    precision_t comm_prec = commodity().precision();
    if (quantity->prec > comm_prec + extend_by_digits)
      quantity->prec = static_cast<precision_t>(comm_prec + extend_by_digits);
  }

  return *this;
}

amount_t& amount_t::operator/=(const amount_t& amt)
{
  VERIFY(amt.valid());

  if (! quantity || ! amt.quantity) {
    if (quantity)
      throw_(amount_error, _("Cannot divide an amount by an uninitialized amount"));
    else if (amt.quantity)
      throw_(amount_error, _("Cannot divide an uninitialized amount by an amount"));
    else
      throw_(amount_error, _("Cannot divide two uninitialized amounts"));
  }

  if (! amt)
    throw_(amount_error, _("Divide by zero"));

  _dup();

  // Increase the value's precision, to capture fractional parts after
  // the divide.  Round up in the last position.

  mpq_div(MP(quantity), MP(quantity), MP(amt.quantity));
  quantity->prec =
    static_cast<precision_t>(quantity->prec + amt.quantity->prec +
                             extend_by_digits);

  if (! has_commodity())
    commodity_ = amt.commodity_;

  // If this amount has a commodity, and we're not dealing with plain
  // numbers, or internal numbers (which keep full precision at all
  // times), then round the number to within the commodity's precision
  // plus six places.

  if (has_commodity() && ! keep_precision()) {
    precision_t comm_prec = commodity().precision();
    if (quantity->prec > comm_prec + extend_by_digits)
      quantity->prec = static_cast<precision_t>(comm_prec + extend_by_digits);
  }

  return *this;
}

amount_t::precision_t amount_t::precision() const
{
  if (! quantity)
    throw_(amount_error,
           _("Cannot determine precision of an uninitialized amount"));

  return quantity->prec;
}

bool amount_t::keep_precision() const
{
  if (! quantity)
    throw_(amount_error,
           _("Cannot determine if precision of an uninitialized amount is kept"));

  return quantity->has_flags(BIGINT_KEEP_PREC);
}

void amount_t::set_keep_precision(const bool keep) const
{
  if (! quantity)
    throw_(amount_error,
           _("Cannot set whether to keep the precision of an uninitialized amount"));

  if (keep)
    quantity->add_flags(BIGINT_KEEP_PREC);
  else
    quantity->drop_flags(BIGINT_KEEP_PREC);
}

amount_t::precision_t amount_t::display_precision() const
{
  if (! quantity)
    throw_(amount_error,
           _("Cannot determine display precision of an uninitialized amount"));

  commodity_t& comm(commodity());

  if (comm && ! keep_precision())
    return comm.precision();
  else
    return comm ? std::max(quantity->prec, comm.precision()) : quantity->prec;
}

void amount_t::in_place_negate()
{
  if (quantity) {
    _dup();
    mpq_neg(MP(quantity), MP(quantity));
  } else {
    throw_(amount_error, _("Cannot negate an uninitialized amount"));
  }
}

void amount_t::in_place_invert()
{
  if (! quantity)
    throw_(amount_error, _("Cannot invert an uninitialized amount"));

  _dup();

  if (sign() != 0)
    mpq_inv(MP(quantity), MP(quantity));
}

void amount_t::in_place_round()
{
  if (! quantity)
    throw_(amount_error, _("Cannot set rounding for an uninitialized amount"));
  else if (! keep_precision())
    return;

  _dup();
  set_keep_precision(false);
}

void amount_t::in_place_truncate()
{
  if (! quantity)
    throw_(amount_error, _("Cannot truncate an uninitialized amount"));

  DEBUG("amount.truncate",
        "Truncating " << *this << " to precision " << display_precision());

  in_place_roundto(display_precision());

  DEBUG("amount.truncate", "Truncated = " << *this);
}

void amount_t::in_place_floor()
{
  if (! quantity)
    throw_(amount_error, _("Cannot compute floor on an uninitialized amount"));

  _dup();

  mpz_fdiv_q(temp,  mpq_numref(MP(quantity)), mpq_denref(MP(quantity)));
  mpq_set_z(MP(quantity), temp);
}

void amount_t::in_place_ceiling()
{
  if (! quantity)
    throw_(amount_error, _("Cannot compute ceiling on an uninitialized amount"));

  _dup();

  mpz_cdiv_q(temp,  mpq_numref(MP(quantity)), mpq_denref(MP(quantity)));
  mpq_set_z(MP(quantity), temp);
}

void amount_t::in_place_roundto(int places)
{
  if (! quantity)
    throw_(amount_error, _("Cannot round an uninitialized amount"));

  _dup();

  mpz_t& scale(temp);
  if (places)
    mpz_ui_pow_ui(scale, 10, labs(places));

  if (places > 0) {
    mpz_mul(mpq_numref(MP(quantity)), mpq_numref(MP(quantity)), scale);
  } else if (places < 0) {
    mpz_mul(mpq_denref(MP(quantity)), mpq_denref(MP(quantity)), scale);
  }

  auto whole(mpq_numref(tempq));
  auto reminder(mpq_denref(tempq));
  mpz_fdiv_qr(whole, reminder, mpq_numref(MP(quantity)), mpq_denref(MP(quantity)));
  mpz_mul_2exp(reminder, reminder, 1);
  const int rem_denom_cmp = mpz_cmp(reminder, mpq_denref(MP(quantity)));
  if (rem_denom_cmp > 0
      || (rem_denom_cmp == 0 && mpz_odd_p(whole)))
    mpz_add_ui(whole, whole, 1);

  if (places > 0) {
    mpq_set_num(MP(quantity), whole);
    mpq_set_den(MP(quantity), scale);
    mpq_canonicalize(MP(quantity));
  } else if (places == 0)
    mpq_set_z(MP(quantity), whole);
  else {
    mpq_set_ui(MP(quantity), 0, 1);
    mpz_mul(mpq_numref(MP(quantity)), whole, scale);
  }
}

void amount_t::in_place_unround()
{
  if (! quantity)
    throw_(amount_error, _("Cannot unround an uninitialized amount"));
  else if (keep_precision())
    return;

  _dup();

  DEBUG("amount.unround", "Unrounding " << *this);
  set_keep_precision(true);
  DEBUG("amount.unround", "Unrounded = " << *this);
}

void amount_t::in_place_reduce()
{
  if (! quantity)
    throw_(amount_error, _("Cannot reduce an uninitialized amount"));

  while (commodity_ && commodity().smaller()) {
    *this *= commodity().smaller()->number();
    commodity_ = commodity().smaller()->commodity_;
  }
}

void amount_t::in_place_unreduce()
{
  if (! quantity)
    throw_(amount_error, _("Cannot unreduce an uninitialized amount"));

  amount_t      tmp     = *this;
  commodity_t * comm    = commodity_;
  bool          shifted = false;

  while (comm && comm->larger()) {
    amount_t next_temp = tmp / comm->larger()->number();
    if (next_temp.abs() < amount_t(1L))
      break;
    tmp  = next_temp;
    comm = comm->larger()->commodity_;
    shifted = true;
  }

  if (shifted) {
    if (comm && ("h" == comm->symbol() || "m" == comm->symbol())
        && commodity_t::time_colon_by_default) {
      double truncated = trunc(tmp.to_double());
      double precision = tmp.to_double() - truncated;
      tmp = truncated + (precision * (comm->smaller()->number() / 100.0));
    }

    *this      = tmp;
    commodity_ = comm;
  }
}

optional<amount_t>
amount_t::value(const datetime_t&   moment,
                const commodity_t * in_terms_of) const
{
  if (quantity) {
#if DEBUG_ON
    DEBUG("commodity.price.find",
          "amount_t::value of " << commodity().symbol());
    if (! moment.is_not_a_date_time())
      DEBUG("commodity.price.find",
            "amount_t::value: moment = " << moment);
    if (in_terms_of)
      DEBUG("commodity.price.find",
            "amount_t::value: in_terms_of = " << in_terms_of->symbol());
#endif
    if (has_commodity() &&
        (in_terms_of || ! commodity().has_flags(COMMODITY_PRIMARY))) {
      optional<price_point_t> point;
      const commodity_t * comm(in_terms_of);

      if (has_annotation() && annotation().price) {
        if (annotation().has_flags(ANNOTATION_PRICE_FIXATED)) {
          point = price_point_t();
          point->price = *annotation().price;
          DEBUG("commodity.prices.find",
                "amount_t::value: fixated price =  " << point->price);
        }
        else if (! comm) {
          comm = annotation().price->commodity_ptr();
        }
      }

      if (comm && commodity().referent() == comm->referent())
        return with_commodity(comm->referent());

      if (! point) {
        point = commodity().find_price(comm, moment);

        // Whether a price was found or not, check whether we should attempt
        // to download a price from the Internet.  This is done if (a) no
        // price was found, or (b) the price is "stale" according to the
        // setting of --price-exp.
        if (point)
          point = commodity().check_for_updated_price(point, moment, comm);
      }

      if (point) {
        amount_t price(point->price);
        price.multiply(*this, true);
        price.in_place_round();
        return price;
      }
    }
  } else {
    throw_(amount_error,
           _("Cannot determine value of an uninitialized amount"));
  }
  return none;
}

optional<amount_t> amount_t::price() const
{
  if (has_annotation() && annotation().price) {
    amount_t tmp(*annotation().price);
    tmp *= *this;
    DEBUG("amount.price", "Returning price of " << *this << " = " << tmp);
    return tmp;
  }
  return none;
}


int amount_t::sign() const
{
  if (! quantity)
    throw_(amount_error, _("Cannot determine sign of an uninitialized amount"));

  return mpq_sgn(MP(quantity));
}

bool amount_t::is_zero() const
{
  if (! quantity)
    throw_(amount_error, _("Cannot determine if an uninitialized amount is zero"));

  if (has_commodity()) {
    if (keep_precision() || quantity->prec <= commodity().precision()) {
      return is_realzero();
    }
    else if (is_realzero()) {
      return true;
    }
    else if (mpz_cmp(mpq_numref(MP(quantity)),
                     mpq_denref(MP(quantity))) > 0) {
      DEBUG("amount.is_zero", "Numerator is larger than the denominator");
      return false;
    }
    else {
      DEBUG("amount.is_zero", "We have to print the number to check for zero");

      std::ostringstream out;
      stream_out_mpq(out, MP(quantity), commodity().precision());

      string output = out.str();
      if (! output.empty()) {
        for (const char * p = output.c_str(); *p; p++)
          if (*p != '0' && *p != '.' && *p  != '-')
            return false;
      }
      return true;
    }
  }
  return is_realzero();
}


double amount_t::to_double() const
{
  if (! quantity)
    throw_(amount_error, _("Cannot convert an uninitialized amount to a double"));

  mpfr_set_q(tempf, MP(quantity), GMP_RNDN);
  return mpfr_get_d(tempf, GMP_RNDN);
}

long amount_t::to_long() const
{
  if (! quantity)
    throw_(amount_error, _("Cannot convert an uninitialized amount to a long"));

  mpfr_set_q(tempf, MP(quantity), GMP_RNDN);
  return mpfr_get_si(tempf, GMP_RNDN);
}

bool amount_t::fits_in_long() const
{
  mpfr_set_q(tempf, MP(quantity), GMP_RNDN);
  return mpfr_fits_slong_p(tempf, GMP_RNDN);
}

commodity_t * amount_t::commodity_ptr() const
{
  return (commodity_ ?
          commodity_ : commodity_pool_t::current_pool->null_commodity);
}

bool amount_t::has_commodity() const
{
  return commodity_ && commodity_ != commodity_->pool().null_commodity;
}

void amount_t::annotate(const annotation_t& details)
{
  commodity_t *           this_base;
  annotated_commodity_t * this_ann = NULL;

  if (! quantity)
    throw_(amount_error, _("Cannot annotate the commodity of an uninitialized amount"));
  else if (! has_commodity())
    return;                     // ignore attempt to annotate a "bare commodity

  if (commodity().has_annotation()) {
    this_ann  = &as_annotated_commodity(commodity());
    this_base = &this_ann->referent();
  } else {
    this_base = &commodity();
  }
  assert(this_base);

  DEBUG("amount.commodities", "Annotating commodity for amount "
        << *this << std::endl << details);

  if (commodity_t * ann_comm =
      this_base->pool().find_or_create(*this_base, details))
    set_commodity(*ann_comm);
  else
    assert(false);

  DEBUG("amount.commodities", "Annotated amount is " << *this);
}

bool amount_t::has_annotation() const
{
  if (! quantity)
    throw_(amount_error,
           _("Cannot determine if an uninitialized amount's commodity is annotated"));

  assert(! has_commodity() || ! commodity().has_annotation() ||
         as_annotated_commodity(commodity()).details);
  return has_commodity() && commodity().has_annotation();
}

annotation_t& amount_t::annotation()
{
  if (! quantity)
    throw_(amount_error,
           _("Cannot return commodity annotation details of an uninitialized amount"));

  if (! commodity().has_annotation())
    throw_(amount_error,
           _("Request for annotation details from an unannotated amount"));

  annotated_commodity_t& ann_comm(as_annotated_commodity(commodity()));
  return ann_comm.details;
}

amount_t amount_t::strip_annotations(const keep_details_t& what_to_keep) const
{
  if (! quantity)
    throw_(amount_error,
           _("Cannot strip commodity annotations from an uninitialized amount"));

  if (! what_to_keep.keep_all(commodity())) {
    amount_t t(*this);
    t.set_commodity(commodity().strip_annotations(what_to_keep));
    return t;
  }
  return *this;
}


namespace {
  void parse_quantity(std::istream& in, string& value)
  {
    char buf[256];
    int c = peek_next_nonws(in);
    int max = 255;
    char *p = buf;
    if (c == '-') {
      *p++ = c;
      max--;
      in.get();
    }
    READ_INTO(in, p, max, c,
              std::isdigit(c) || c == '.' || c == ',');

    string::size_type len = std::strlen(buf);
    while (len > 0 &&
           ! std::isdigit(static_cast<unsigned char>(buf[len - 1]))) {
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
  bool         negative   = false;

  commodity_t::flags_t comm_flags = COMMODITY_STYLE_DEFAULTS;

  int c = peek_next_nonws(in);
  if (c == '-') {
    negative = true;
    in.get();
    c = peek_next_nonws(in);
  }

  char n;
  if (std::isdigit(c)) {
    parse_quantity(in, quant);

    if (! in.eof() && ((n = static_cast<char>(in.peek())) != '\n')) {
      if (std::isspace(static_cast<unsigned char>(n)))
        comm_flags |= COMMODITY_STYLE_SEPARATED;

      commodity_t::parse_symbol(in, symbol);

      if (! symbol.empty())
        comm_flags |= COMMODITY_STYLE_SUFFIXED;

      if (! flags.has_flags(PARSE_NO_ANNOT) &&
          ! in.eof() && ((n = static_cast<char>(in.peek())) != '\n'))
        details.parse(in);
    }
  } else {
    commodity_t::parse_symbol(in, symbol);

    if (! in.eof() && ((n = static_cast<char>(in.peek())) != '\n')) {
      if (std::isspace(in.peek()))
        comm_flags |= COMMODITY_STYLE_SEPARATED;

      parse_quantity(in, quant);

      if (! flags.has_flags(PARSE_NO_ANNOT) && ! quant.empty() &&
          ! in.eof() && ((n = static_cast<char>(in.peek())) != '\n'))
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
    quantity = NULL;
  }

  if (! new_quantity.get())
    new_quantity.reset(new bigint_t);

  // No one is holding a reference to this now.
  new_quantity->refc--;

  // Create the commodity if has not already been seen, and update the
  // precision if something greater was used for the quantity.

  if (symbol.empty()) {
    commodity_ = NULL;
  } else {
    commodity_ = commodity_pool_t::current_pool->find(symbol);
    if (! commodity_)
      commodity_ = commodity_pool_t::current_pool->create(symbol);
    assert(commodity_);
  }

  // Quickly scan through and verify the correctness of the amount's use of
  // punctuation.

  precision_t       decimal_offset  = 0;
  string::size_type string_index    = quant.length();
  string::size_type last_comma      = string::npos;
  string::size_type last_period     = string::npos;

  bool no_more_commas  = false;
  bool no_more_periods = false;
  bool no_migrate_style
    = commodity().has_flags(COMMODITY_STYLE_NO_MIGRATE);
  bool decimal_comma_style
    = (commodity_t::decimal_comma_by_default ||
       commodity().has_flags(COMMODITY_STYLE_DECIMAL_COMMA));
#if 0
  bool time_colon_style
    = (commodity_t::time_colon_by_default ||
       commodity().has_flags(COMMODITY_STYLE_TIME_COLON));
#endif

  new_quantity->prec = 0;

  BOOST_REVERSE_FOREACH (const char& ch, quant) {
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
          decimal_comma_style = true;
          if (decimal_offset % 3 != 0)
            throw_(amount_error, _("Incorrect use of thousand-mark period"));
        } else {
          no_more_periods    = true;
          new_quantity->prec = decimal_offset;
          decimal_offset     = 0;
        }
      }

      if (last_period == string::npos)
        last_period = string_index;
    }
    else if (ch == ',') {
      if (no_more_commas)
        throw_(amount_error, _("Too many commas in amount"));

      if (decimal_comma_style) {
        if (last_period != string::npos) {
          throw_(amount_error, _("Incorrect use of decimal comma"));
        } else {
          no_more_commas     = true;
          new_quantity->prec = decimal_offset;
          decimal_offset     = 0;
        }
      } else {
        if (decimal_offset % 3 != 0) {
          if (last_comma != string::npos ||
              last_period != string::npos) {
            throw_(amount_error, _("Incorrect use of thousand-mark comma"));
          } else {
            decimal_comma_style = true;
            no_more_commas      = true;
            new_quantity->prec  = decimal_offset;
            decimal_offset      = 0;
          }
        } else {
          comm_flags |= COMMODITY_STYLE_THOUSANDS;
          no_more_periods = true;
        }
      }

      if (last_comma == string::npos)
        last_comma = string_index;
    }
    else {
      decimal_offset++;
    }
  }

  if (decimal_comma_style)
    comm_flags |= COMMODITY_STYLE_DECIMAL_COMMA;

  if (flags.has_flags(PARSE_NO_MIGRATE)) {
    // Can't call set_keep_precision here, because it assumes that `quantity'
    // is non-NULL.
    new_quantity->add_flags(BIGINT_KEEP_PREC);
  }
  else if (commodity_ && ! no_migrate_style) {
    commodity().add_flags(comm_flags);

    if (new_quantity->prec > commodity().precision())
      commodity().set_precision(new_quantity->prec);
  }

  // Now we have the final number.  Remove commas and periods, if necessary.

  if (last_comma != string::npos || last_period != string::npos) {
    string::size_type  len = quant.length();
    scoped_array<char> buf(new char[len + 1]);
    const char *       p   = quant.c_str();
    char *             t   = buf.get();

    while (*p) {
      if (*p == ',' || *p == '.')
        p++;
      *t++ = *p++;
    }
    *t = '\0';

    mpq_set_str(MP(new_quantity.get()), buf.get(), 10);
    mpz_ui_pow_ui(temp, 10, new_quantity->prec);
    mpq_set_z(tempq, temp);
    mpq_div(MP(new_quantity.get()), MP(new_quantity.get()), tempq);

    IF_DEBUG("amount.parse") {
      char * amt_buf = mpq_get_str(NULL, 10, MP(new_quantity.get()));
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

  if (! flags.has_flags(PARSE_NO_REDUCE))
    in_place_reduce();          // will not throw an exception

  if (commodity_ && details) {
    if (details.has_flags(ANNOTATION_PRICE_NOT_PER_UNIT)) {
      assert(details.price);
      *details.price /= this->abs();
    }
    set_commodity(*commodity_pool_t::current_pool->find_or_create(*commodity_, details));
  }

  VERIFY(valid());

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

void amount_t::print(std::ostream& _out, const uint_least8_t flags) const
{
  VERIFY(valid());

  if (! quantity) {
    _out << "<null>";
    return;
  }

  std::ostringstream out;

  commodity_t& comm(commodity());

  if (! comm.has_flags(COMMODITY_STYLE_SUFFIXED)) {
    comm.print(out, flags & AMOUNT_PRINT_ELIDE_COMMODITY_QUOTES);
    if (comm.has_flags(COMMODITY_STYLE_SEPARATED))
      out << " ";
  }

  stream_out_mpq(out, MP(quantity), display_precision(),
                 comm ? commodity().precision() : 0, GMP_RNDN, comm);

  if (comm.has_flags(COMMODITY_STYLE_SUFFIXED)) {
    if (comm.has_flags(COMMODITY_STYLE_SEPARATED))
      out << " ";
    comm.print(out, flags & AMOUNT_PRINT_ELIDE_COMMODITY_QUOTES);
  }

  // If there are any annotations associated with this commodity, output them
  // now.
  comm.write_annotations(out, flags & AMOUNT_PRINT_NO_COMPUTED_ANNOTATIONS);

  // Things are output to a string first, so that if anyone has specified a
  // width or fill for _out, it will be applied to the entire amount string,
  // and not just the first part.
  _out << out.str();
}

bool amount_t::valid() const
{
  if (quantity) {
    if (! quantity->valid()) {
      DEBUG("ledger.validate", "amount_t: ! quantity->valid()");
      return false;
    }

    if (quantity->refc == 0) {
      DEBUG("ledger.validate", "amount_t: quantity->refc == 0");
      return false;
    }
  }
  else if (commodity_) {
    DEBUG("ledger.validate", "amount_t: commodity_ != NULL");
    return false;
  }
  return true;
}

void put_amount(property_tree::ptree& st, const amount_t& amt,
                bool commodity_details)
{
  if (amt.has_commodity())
    put_commodity(st.put("commodity", ""), amt.commodity(), commodity_details);

  st.put("quantity", amt.quantity_string());
}

} // namespace ledger
