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
#include "commodity.h"
#include "unistring.h"

namespace ledger {

bool amount_t::stream_fullstrings = false;

#if !defined(THREADSAFE)
// These global temporaries are pre-initialized for the sake of
// efficiency, and are reused over and over again.
static mpz_t  temp;
static mpq_t  tempq;
static mpfr_t tempf;
static mpfr_t tempfb;
#endif

struct amount_t::bigint_t : public supports_flags<>
{
#define BIGINT_BULK_ALLOC 0x01
#define BIGINT_KEEP_PREC  0x02

  mpq_t		 val;
  precision_t	 prec;
  uint_least16_t ref;

#define MP(bigint) ((bigint)->val)

  bigint_t() : prec(0), ref(1) {
    TRACE_CTOR(bigint_t, "");
    mpq_init(val);
  }
  bigint_t(const bigint_t& other)
    : supports_flags<>(other.flags() & ~BIGINT_BULK_ALLOC),
      prec(other.prec), ref(1) {
    TRACE_CTOR(bigint_t, "copy");
    mpq_init(val);
    mpq_set(val, other.val);
  }
  ~bigint_t() {
    TRACE_DTOR(bigint_t);
    assert(ref == 0);
    mpq_clear(val);
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

shared_ptr<commodity_pool_t> amount_t::current_pool;

bool amount_t::is_initialized = false;

void amount_t::initialize(shared_ptr<commodity_pool_t> pool)
{
  if (! is_initialized) {
    mpz_init(temp);
    mpq_init(tempq);
    mpfr_init(tempf);
    mpfr_init(tempfb);
    is_initialized = true;
  }
  current_pool = pool;
}

void amount_t::initialize()
{
  initialize(shared_ptr<commodity_pool_t>(new commodity_pool_t));
}

void amount_t::shutdown()
{
  current_pool.reset();

  if (is_initialized) {
    mpz_clear(temp);
    mpq_clear(tempq);
    mpfr_clear(tempf);
    mpfr_clear(tempfb);
    is_initialized = false;
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
  mpq_set_d(MP(quantity), val);
  quantity->prec = extend_by_digits; // an approximation
}

amount_t::amount_t(const unsigned long val) : commodity_(NULL)
{
  TRACE_CTOR(amount_t, "const unsigned long");
  quantity = new bigint_t;
  mpq_set_ui(MP(quantity), val, 1);
}

amount_t::amount_t(const long val) : commodity_(NULL)
{
  TRACE_CTOR(amount_t, "const long");
  quantity = new bigint_t;
  mpq_set_si(MP(quantity), val, 1);
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
  assert(amt.valid());

  if (! quantity || ! amt.quantity) {
    if (quantity)
      throw_(amount_error, "Cannot add an uninitialized amount to an amount");
    else if (amt.quantity)
      throw_(amount_error, "Cannot add an amount to an uninitialized amount");
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

  mpq_add(MP(quantity), MP(quantity), MP(amt.quantity));

  if (quantity->prec < amt.quantity->prec)
    quantity->prec = amt.quantity->prec;

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

  mpq_sub(MP(quantity), MP(quantity), MP(amt.quantity));

  if (quantity->prec < amt.quantity->prec)
    quantity->prec = amt.quantity->prec;

  return *this;
}

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

  mpq_mul(MP(quantity), MP(quantity), MP(amt.quantity));
  quantity->prec += amt.quantity->prec;

  if (! has_commodity())
    commodity_ = amt.commodity_;

  if (has_commodity() && ! keep_precision()) {
    precision_t comm_prec = commodity().precision();
    if (quantity->prec > comm_prec + extend_by_digits)
      quantity->prec = comm_prec + extend_by_digits;
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

  mpq_div(MP(quantity), MP(quantity), MP(amt.quantity));
  quantity->prec += amt.quantity->prec + quantity->prec + extend_by_digits;

  if (! has_commodity())
    commodity_ = amt.commodity_;

  // If this amount has a commodity, and we're not dealing with plain
  // numbers, or internal numbers (which keep full precision at all
  // times), then round the number to within the commodity's precision
  // plus six places.

  if (has_commodity() && ! keep_precision()) {
    precision_t comm_prec = commodity().precision();
    if (quantity->prec > comm_prec + extend_by_digits)
      quantity->prec = comm_prec + extend_by_digits;
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

amount_t::precision_t amount_t::display_precision() const
{
  if (! quantity)
    throw_(amount_error,
	   "Cannot determine display precision of an uninitialized amount");

  commodity_t& comm(commodity());

  if (! comm || keep_precision())
    return quantity->prec;
  else if (comm.precision() != quantity->prec)
    return comm.precision();
  else
    return quantity->prec;
}

void amount_t::in_place_negate()
{
  if (quantity) {
    _dup();
    mpq_neg(MP(quantity), MP(quantity));
  } else {
    throw_(amount_error, "Cannot negate an uninitialized amount");
  }
}

amount_t amount_t::inverted() const
{
  if (! quantity)
    throw_(amount_error, "Cannot invert an uninitialized amount");

  amount_t t(*this);
  t._dup();
  mpq_inv(MP(t.quantity), MP(t.quantity));

  return t;
}

amount_t amount_t::rounded() const
{
  if (! quantity)
    throw_(amount_error, "Cannot set rounding for an uninitialized amount");
  else if (! keep_precision())
    return *this;

  amount_t t(*this);
  t._dup();
  t.set_keep_precision(false);

  return t;
}

amount_t amount_t::unrounded() const
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

void amount_t::in_place_reduce()
{
  if (! quantity)
    throw_(amount_error, "Cannot reduce an uninitialized amount");

  while (commodity_ && commodity().smaller()) {
    *this *= commodity().smaller()->number();
    commodity_ = commodity().smaller()->commodity_;
  }
}

void amount_t::in_place_unreduce()
{
  if (! quantity)
    throw_(amount_error, "Cannot unreduce an uninitialized amount");

  amount_t	temp	= *this;
  commodity_t * comm	= commodity_;
  bool		shifted = false;

  while (comm && comm->larger()) {
    temp /= comm->larger()->number();
    if (temp.abs() < amount_t(1L))
      break;
    shifted = true;
    comm = comm->larger()->commodity_;
  }

  if (shifted) {
    *this      = temp;
    commodity_ = comm;
  }
}

optional<amount_t>
amount_t::value(const bool		      primary_only,
		const optional<datetime_t>&   moment,
		const optional<commodity_t&>& in_terms_of) const
{
  if (quantity) {
#if defined(DEBUG_ON)
    DEBUG("commodity.prices.find",
	  "amount_t::value of " << commodity().symbol());
    if (moment)
      DEBUG("commodity.prices.find",
	    "amount_t::value: moment =  " << *moment);
    if (in_terms_of)
      DEBUG("commodity.prices.find",
	    "amount_t::value: in_terms_of = " << in_terms_of->symbol());
#endif
    if (has_commodity() &&
	(! primary_only || ! commodity().has_flags(COMMODITY_PRIMARY))) {
      if (in_terms_of && commodity() == *in_terms_of) {
	return *this;
      }
      else if (optional<price_point_t> point =
	       commodity().find_price(in_terms_of, moment)) {
	return (point->price * number()).rounded();
      }
    }
  } else {
    throw_(amount_error, "Cannot determine value of an uninitialized amount");
  }
  return none;
}


int amount_t::sign() const
{
  if (! quantity)
    throw_(amount_error, "Cannot determine sign of an uninitialized amount");

  return mpq_sgn(MP(quantity));
}

namespace {
  void stream_out_mpq(std::ostream&	            out,
		      mpq_t		            quant,
		      amount_t::precision_t         prec,
		      bool		            no_trailing_zeroes = false,
		      const optional<commodity_t&>& comm               = none)
  {
    char * buf = NULL;
    try {
      IF_DEBUG("amount.convert") {
	char * tbuf = mpq_get_str(NULL, 10, quant);
	DEBUG("amount.convert", "Rational to convert = " << tbuf);
	std::free(tbuf);
      }

      // Convert the rational number to a floating-point, extending the
      // floating-point to a large enough size to get a precise answer.
      const std::size_t bits = (mpz_sizeinbase(mpq_numref(quant), 2) +
				mpz_sizeinbase(mpq_denref(quant), 2));
      mpfr_set_prec(tempfb, bits + amount_t::extend_by_digits*8);
      mpfr_set_q(tempfb, quant, GMP_RNDN);

      mpfr_asprintf(&buf, "%.*Rf", prec, tempfb);
      DEBUG("amount.convert",
	    "mpfr_print = " << buf << " (precision " << prec << ")");

      if (no_trailing_zeroes) {
	int index = std::strlen(buf);
	int point = 0;
	for (int i = 0; i < index; i++) {
	  if (buf[i] == '.') {
	    point = i;
	    break;
	  }
	}
	if (point > 0) {
	  while (--index >= point && buf[index] == '0')
	    buf[index] = '\0';
	  if (index >= point && buf[index] == '.')
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
	    if (comm && comm->has_flags(COMMODITY_STYLE_EUROPEAN))
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
	      if (comm && comm->has_flags(COMMODITY_STYLE_EUROPEAN))
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

bool amount_t::is_zero() const
{
  if (! quantity)
    throw_(amount_error, "Cannot determine if an uninitialized amount is zero");

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
      
      for (const char * p = out.str().c_str(); *p; p++)
	if (*p != '0' && *p != '.' && *p  != '-')
	  return false;
      return true;
    }
  }
  return is_realzero();
}


double amount_t::to_double() const
{
  if (! quantity)
    throw_(amount_error, "Cannot convert an uninitialized amount to a double");

  mpfr_set_q(tempf, MP(quantity), GMP_RNDN);
  return mpfr_get_d(tempf, GMP_RNDN);
}

long amount_t::to_long() const
{
  if (! quantity)
    throw_(amount_error, "Cannot convert an uninitialized amount to a long");

  mpfr_set_q(tempf, MP(quantity), GMP_RNDN);
  return mpfr_get_si(tempf, GMP_RNDN);
}

bool amount_t::fits_in_long() const
{
  mpfr_set_q(tempf, MP(quantity), GMP_RNDN);
  return mpfr_fits_slong_p(tempf, GMP_RNDN);
}

commodity_t& amount_t::commodity() const
{
  return has_commodity() ? *commodity_ : *current_pool->null_commodity;
}

bool amount_t::has_commodity() const
{
  return commodity_ && commodity_ != commodity_->parent().null_commodity;
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

  DEBUG("amounts.commodities", "Annotated amount is " << *this);
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

amount_t amount_t::strip_annotations(const keep_details_t& what_to_keep) const
{
  if (! quantity)
    throw_(amount_error,
	   "Cannot strip commodity annotations from an uninitialized amount");

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

  if (commodity_ && (newly_created || ! flags.has_flags(PARSE_NO_MIGRATE))) {
    commodity().add_flags(comm_flags);

    if (quantity->prec > commodity().precision())
      commodity().set_precision(quantity->prec);
  }
  else if (flags.has_flags(PARSE_NO_MIGRATE)) {
    set_keep_precision(true);
  }

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

    mpq_set_str(MP(quantity), buf.get(), 10);
    mpz_ui_pow_ui(temp, 10, quantity->prec);
    mpq_set_z(tempq, temp);
    mpq_div(MP(quantity), MP(quantity), tempq);

    IF_DEBUG("amount.parse") {
      char * buf = mpq_get_str(NULL, 10, MP(quantity));
      DEBUG("amount.parse", "Rational parsed = " << buf);
      std::free(buf);
    }
  } else {
    mpq_set_str(MP(quantity), quant.c_str(), 10);
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

void amount_t::print(std::ostream& _out) const
{
  assert(valid());

  if (! quantity) {
    _out << "<null>";
    return;
  }

  std::ostringstream out;

  commodity_t& comm(commodity());

  if (! comm.has_flags(COMMODITY_STYLE_SUFFIXED)) {
    comm.print(out);
    if (comm.has_flags(COMMODITY_STYLE_SEPARATED))
      out << " ";
  }

  stream_out_mpq(out, MP(quantity), display_precision(), ! comm, comm);

  if (comm.has_flags(COMMODITY_STYLE_SUFFIXED)) {
    if (comm.has_flags(COMMODITY_STYLE_SEPARATED))
      out << " ";
    comm.print(out);
  }

  // If there are any annotations associated with this commodity, output them
  // now.
  comm.write_annotations(out);

  // Things are output to a string first, so that if anyone has specified a
  // width or fill for _out, it will be applied to the entire amount string,
  // and not just the first part.
  _out << out.str();
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
