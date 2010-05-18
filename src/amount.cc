/*
 * Copyright (c) 2003-2010, John Wiegley.  All rights reserved.
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
#endif

struct amount_t::bigint_t : public supports_flags<>
{
#define BIGINT_BULK_ALLOC 0x01
#define BIGINT_KEEP_PREC  0x02

  mpq_t		 val;
  precision_t	 prec;
  uint_least32_t refc;

#define MP(bigint) ((bigint)->val)

  bigint_t() : prec(0), refc(1) {
    TRACE_CTOR(bigint_t, "");
    mpq_init(val);
  }
  bigint_t(const bigint_t& other)
    : supports_flags<>(static_cast<uint_least8_t>
		       (other.flags() & ~BIGINT_BULK_ALLOC)),
      prec(other.prec), refc(1) {
    TRACE_CTOR(bigint_t, "copy");
    mpq_init(val);
    mpq_set(val, other.val);
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

#if defined(HAVE_BOOST_SERIALIZATION)
private:
  friend class boost::serialization::access;

  template<class Archive>
  void serialize(Archive& ar, const unsigned int /* version */)
  {
    ar & boost::serialization::base_object<supports_flags<> >(*this);
    ar & val;
    ar & prec;
    ar & refc;
  }
#endif // HAVE_BOOST_SERIALIZATION
};

bool amount_t::is_initialized = false;

namespace {
  void stream_out_mpq(std::ostream&	            out,
		      mpq_t		            quant,
		      amount_t::precision_t         prec,
		      int                           zeros_prec = -1,
		      const optional<commodity_t&>& comm       = none)
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
	  while (--index >= (point + 1 + zeros_prec) && buf[index] == '0')
	    buf[index] = '\0';
	  if (index >= (point + zeros_prec) && buf[index] == '.')
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
	    if (commodity_t::european_by_default ||
		(comm && comm->has_flags(COMMODITY_STYLE_EUROPEAN)))
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
	      if (commodity_t::european_by_default ||
		  (comm && comm->has_flags(COMMODITY_STYLE_EUROPEAN)))
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

    commodity_pool_t::current_pool.reset(new commodity_pool_t);

    // Add time commodity conversions, so that timelog's may be parsed
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
      DEBUG("amounts.refs",
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

  DEBUG("amounts.refs", quantity << " refc--, now " << (quantity->refc - 1));

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
  VERIFY(amt.valid());

  if (! quantity || ! amt.quantity) {
    if (quantity)
      throw_(amount_error, _("Cannot compare an amount to an uninitialized amount"));
    else if (amt.quantity)
      throw_(amount_error, _("Cannot compare an uninitialized amount to an amount"));
    else
      throw_(amount_error, _("Cannot compare two uninitialized amounts"));
  }

  if (has_commodity() && amt.has_commodity() &&
      commodity() != amt.commodity())
    throw_(amount_error,
	   _("Cannot compare amounts with different commodities: %1 and %2")
	   << commodity().symbol() << amt.commodity().symbol());

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

  if (has_commodity() && amt.has_commodity() &&
      commodity() != amt.commodity())
    throw_(amount_error,
	   _("Adding amounts with different commodities: %1 != %2")
	   << (has_commodity() ? commodity().symbol() : _("NONE"))
	   << (amt.has_commodity() ? amt.commodity().symbol() : _("NONE")));

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

  if (has_commodity() && amt.has_commodity() &&
      commodity() != amt.commodity())
    throw_(amount_error,
	   _("Subtracting amounts with different commodities: %1 != %2")
	   << (has_commodity() ? commodity().symbol() : _("NONE"))
	   << (amt.has_commodity() ? amt.commodity().symbol() : _("NONE")));

  _dup();

  mpq_sub(MP(quantity), MP(quantity), MP(amt.quantity));

  if (has_commodity() == amt.has_commodity())
    if (quantity->prec < amt.quantity->prec)
      quantity->prec = amt.quantity->prec;

  return *this;
}

amount_t& amount_t::operator*=(const amount_t& amt)
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

  if (! has_commodity())
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
    throw_(amount_error, _("Cannot negate an uninitialized amount"));
  }
}

amount_t amount_t::inverted() const
{
  if (! quantity)
    throw_(amount_error, _("Cannot invert an uninitialized amount"));

  amount_t t(*this);
  t._dup();
  mpq_inv(MP(t.quantity), MP(t.quantity));

  return t;
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

void amount_t::in_place_floor()
{
  if (! quantity)
    throw_(amount_error, _("Cannot floor an uninitialized amount"));

  _dup();

  std::ostringstream out;
  stream_out_mpq(out, MP(quantity), 0);

  mpq_set_str(MP(quantity), out.str().c_str(), 10);
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

  amount_t	temp	= *this;
  commodity_t * comm	= commodity_;
  bool		shifted = false;

  while (comm && comm->larger()) {
    amount_t next_temp = temp / comm->larger()->number();
    if (next_temp.abs() < amount_t(1L))
      break;
    temp = next_temp;
    comm = comm->larger()->commodity_;
    shifted = true;
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
      else if (has_annotation() && annotation().price &&
	       annotation().has_flags(ANNOTATION_PRICE_FIXATED)) {
	return (*annotation().price * number()).rounded();
      }
      else {
	optional<price_point_t> point =
	  commodity().find_price(in_terms_of, moment);

	// Whether a price was found or not, check whether we should attempt
	// to download a price from the Internet.  This is done if (a) no
	// price was found, or (b) the price is "stale" according to the
	// setting of --price-exp.
	point = commodity().check_for_updated_price(point, moment, in_terms_of);
	if (point)
	  return (point->price * number()).rounded();
      }
    }
  } else {
    throw_(amount_error,
	   _("Cannot determine value of an uninitialized amount"));
  }
  return none;
}

amount_t amount_t::price() const
{
  if (has_annotation() && annotation().price) {
    amount_t temp(*annotation().price);
    temp *= *this;
    DEBUG("amount.price", "Returning price of " << *this << " = " << temp);
    return temp;
  }
  return *this;
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

commodity_t& amount_t::commodity() const
{
  return (has_commodity() ?
	  *commodity_ : *commodity_pool_t::current_pool->null_commodity);
}

bool amount_t::has_commodity() const
{
  return commodity_ && commodity_ != commodity_->pool().null_commodity;
}

void amount_t::annotate(const annotation_t& details)
{
  commodity_t *		  this_base;
  annotated_commodity_t * this_ann = NULL;

  if (! quantity)
    throw_(amount_error, _("Cannot annotate the commodity of an uninitialized amount"));
  else if (! has_commodity())
    return;			// ignore attempt to annotate a "bare commodity

  if (commodity().has_annotation()) {
    this_ann  = &as_annotated_commodity(commodity());
    this_base = &this_ann->referent();
  } else {
    this_base = &commodity();
  }
  assert(this_base);

  DEBUG("amounts.commodities", "Annotating commodity for amount "
	<< *this << std::endl << details);

  if (commodity_t * ann_comm =
      this_base->pool().find_or_create(*this_base, details))
    set_commodity(*ann_comm);
#ifdef ASSERTS_ON
  else
    assert(false);
#endif

  DEBUG("amounts.commodities", "Annotated amount is " << *this);
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
    char c = peek_next_nonws(in);
    READ_INTO(in, buf, 255, c,
	      std::isdigit(c) || c == '-' || c == '.' || c == ',');

    string::size_type len = std::strlen(buf);
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

    if (! in.eof() && ((n = static_cast<char>(in.peek())) != '\n')) {
      if (std::isspace(n))
	comm_flags |= COMMODITY_STYLE_SEPARATED;

      commodity_t::parse_symbol(in, symbol);

      if (! symbol.empty())
	comm_flags |= COMMODITY_STYLE_SUFFIXED;

      if (! in.eof() && ((n = static_cast<char>(in.peek())) != '\n'))
	details.parse(in);
    }
  } else {
    commodity_t::parse_symbol(in, symbol);

    if (! in.eof() && ((n = static_cast<char>(in.peek())) != '\n')) {
      if (std::isspace(static_cast<char>(in.peek())))
	comm_flags |= COMMODITY_STYLE_SEPARATED;

      parse_quantity(in, quant);

      if (! quant.empty() && ! in.eof() &&
	  ((n = static_cast<char>(in.peek())) != '\n'))
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
  // monitor the allocation in an auto_ptr because this function gets
  // called sometimes from amount_t's constructor; and if there is an
  // exeception thrown by any of the function calls after this point,
  // the destructor will never be called and the memory never freed.

  std::auto_ptr<bigint_t> new_quantity;

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

  bool newly_created = false;

  if (symbol.empty()) {
    commodity_ = NULL;
  } else {
    commodity_ = commodity_pool_t::current_pool->find(symbol);
    if (! commodity_) {
      commodity_ = commodity_pool_t::current_pool->create(symbol);
      newly_created = true;
    }
    assert(commodity_);

    if (details)
      commodity_ =
	commodity_pool_t::current_pool->find_or_create(*commodity_, details);
  }

  // Quickly scan through and verify the correctness of the amount's use of
  // punctuation.

  precision_t       decimal_offset  = 0;
  string::size_type string_index    = quant.length();
  string::size_type last_comma      = string::npos;
  string::size_type last_period     = string::npos;

  bool no_more_commas  = false;
  bool no_more_periods = false;
  bool european_style  = (commodity_t::european_by_default ||
			  commodity().has_flags(COMMODITY_STYLE_EUROPEAN));

  new_quantity->prec = 0;

  BOOST_REVERSE_FOREACH (const char& ch, quant) {
    string_index--;

    if (ch == '.') {
      if (no_more_periods)
	throw_(amount_error, _("Too many periods in amount"));

      if (european_style) {
	if (decimal_offset % 3 != 0)
	  throw_(amount_error, _("Incorrect use of european-style period"));
	comm_flags |= COMMODITY_STYLE_THOUSANDS;
	no_more_commas = true;
      } else {
	if (last_comma != string::npos) {
	  european_style = true;
	  if (decimal_offset % 3 != 0)
	    throw_(amount_error, _("Incorrect use of european-style period"));
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

      if (european_style) {
	if (last_period != string::npos) {
	  throw_(amount_error, _("Incorrect use of european-style comma"));
	} else {
	  no_more_commas     = true;
	  new_quantity->prec = decimal_offset;
	  decimal_offset     = 0;
	}
      } else {
	if (decimal_offset % 3 != 0) {
	  if (last_comma != string::npos ||
	      last_period != string::npos) {
	    throw_(amount_error, _("Incorrect use of American-style comma"));
	  } else {
	    european_style     = true;
	    no_more_commas     = true;
	    new_quantity->prec = decimal_offset;
	    decimal_offset     = 0;
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

  if (european_style)
    comm_flags |= COMMODITY_STYLE_EUROPEAN;

  if (flags.has_flags(PARSE_NO_MIGRATE)) {
    // Can't call set_keep_precision here, because it assumes that `quantity'
    // is non-NULL.
    new_quantity->add_flags(BIGINT_KEEP_PREC);
  }
  else if (commodity_) {
    commodity().add_flags(comm_flags);

    if (new_quantity->prec > commodity().precision())
      commodity().set_precision(new_quantity->prec);
  }

  // Now we have the final number.  Remove commas and periods, if necessary.

  if (last_comma != string::npos || last_period != string::npos) {
    string::size_type  len = quant.length();
    scoped_array<char> buf(new char[len + 1]);
    const char *       p   = quant.c_str();
    char *	       t   = buf.get();

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
      char * buf = mpq_get_str(NULL, 10, MP(new_quantity.get()));
      DEBUG("amount.parse", "Rational parsed = " << buf);
      std::free(buf);
    }
  } else {
    mpq_set_str(MP(new_quantity.get()), quant.c_str(), 10);
  }

  if (negative)
    mpq_neg(MP(new_quantity.get()), MP(new_quantity.get()));

  new_quantity->refc++;
  quantity = new_quantity.release();

  if (! flags.has_flags(PARSE_NO_REDUCE))
    in_place_reduce();		// will not throw an exception

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

void amount_t::print(std::ostream& _out) const
{
  VERIFY(valid());

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

  stream_out_mpq(out, MP(quantity), display_precision(),
		 comm ? commodity().precision() : 0, comm);

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

void to_xml(std::ostream& out, const amount_t& amt, bool commodity_details)
{
  push_xml x(out, "amount");

  if (amt.has_commodity())
    to_xml(out, amt.commodity(), commodity_details);

  {
    push_xml y(out, "quantity");
    out << y.guard(amt.quantity_string());
  }
}

#if defined(HAVE_BOOST_SERIALIZATION)

template<class Archive>
void amount_t::serialize(Archive& ar, const unsigned int /* version */)
{
  ar & is_initialized;
  ar & quantity;
  ar & commodity_;
}

#endif // HAVE_BOOST_SERIALIZATION

} // namespace ledger

#if defined(HAVE_BOOST_SERIALIZATION)
namespace boost {
namespace serialization {

template <class Archive>
void serialize(Archive& ar, MP_INT& mpz, const unsigned int /* version */)
{
  ar & mpz._mp_alloc;
  ar & mpz._mp_size;
  ar & mpz._mp_d;
}

template <class Archive>
void serialize(Archive& ar, MP_RAT& mpq, const unsigned int /* version */)
{
  ar & mpq._mp_num;
  ar & mpq._mp_den;
}

template <class Archive>
void serialize(Archive& ar, long unsigned int& integer,
	       const unsigned int /* version */)
{
  ar & make_binary_object(&integer, sizeof(long unsigned int));
}

} // namespace serialization
} // namespace boost

BOOST_CLASS_EXPORT(ledger::annotated_commodity_t)

template void boost::serialization::serialize(boost::archive::binary_iarchive&,
					      MP_INT&, const unsigned int);
template void boost::serialization::serialize(boost::archive::binary_oarchive&,
					      MP_INT&, const unsigned int);
template void boost::serialization::serialize(boost::archive::binary_iarchive&,
					      MP_RAT&, const unsigned int);
template void boost::serialization::serialize(boost::archive::binary_oarchive&,
					      MP_RAT&, const unsigned int);
template void boost::serialization::serialize(boost::archive::binary_iarchive&,
					      long unsigned int&,
					      const unsigned int);
template void boost::serialization::serialize(boost::archive::binary_oarchive&,
					      long unsigned int&,
					      const unsigned int);

template void ledger::amount_t::serialize(boost::archive::binary_iarchive&,
					  const unsigned int);
template void ledger::amount_t::serialize(boost::archive::binary_oarchive&,
					  const unsigned int);

#endif // HAVE_BOOST_SERIALIZATION
