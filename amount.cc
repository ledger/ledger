#include <sstream>

#include <gmp.h>                // GNU multi-precision library
#include <pcre.h>               // Perl regular expression library

#include "ledger.h"

namespace ledger {

//////////////////////////////////////////////////////////////////////
//
// The `amount' structure.  Every transaction has an associated amount,
// which is represented by this structure.  `amount' uses the GNU
// multi-precision library, allowing for arbitrarily large amounts.
// Each amount is a quantity of commodity at a certain price; the
// default commodity is the US dollar, with a price of 1.00.
//

#define MAX_PRECISION 10        // must be 2 or higher

class gmp_amount : public amount
{
  bool priced;

  mpz_t price;
  commodity * price_comm;

  mpz_t quantity;
  commodity * quantity_comm;

  gmp_amount(const gmp_amount& other) {}
  gmp_amount& operator=(const gmp_amount& other) { return *this; }

 public:
  gmp_amount() : priced(false), price_comm(NULL), quantity_comm(NULL) {
    mpz_init(price);
    mpz_init(quantity);
  }

  virtual ~gmp_amount() {
    mpz_clear(price);
    mpz_clear(quantity);
  }

  virtual const std::string& comm_symbol() const {
    assert(quantity_comm);
    return quantity_comm->symbol;
  }

  virtual amount * copy() const;
  virtual amount * value() const;

  virtual operator bool() const;

  virtual void credit(const amount * other) {
    *this += *other;
  }
  virtual void negate() {
    mpz_ui_sub(quantity, 0, quantity);
  }
  virtual void operator+=(const amount& other);

  virtual void parse(const char * num) {
    *this = num;
  }
  virtual amount& operator=(const char * num);
  virtual std::string as_str(bool full_prec) const;
  virtual operator std::string() const {
    return as_str(false);
  }

  friend amount * create_amount(const char * value, const amount * price);
};

amount * create_amount(const char * value, const amount * price)
{
  gmp_amount * a = new gmp_amount();
  a->parse(value);

  // If a price was specified, it refers to a total price for the
  // whole `value', meaning we must divide to determine the
  // per-commodity price.

  if (price) {
    assert(! a->priced);        // don't specify price twice!

    const gmp_amount * p = dynamic_cast<const gmp_amount *>(price);
    assert(p);

    // There is no need for per-commodity pricing when the total
    // price is in the same commodity as the quantity!  In that case,
    // the two will always be identical.
    if (a->quantity_comm == p->quantity_comm) {
      assert(mpz_cmp(a->quantity, p->quantity) == 0);
      return a;
    }

    mpz_t quotient;
    mpz_t remainder;
    mpz_t addend;

    mpz_init(quotient);
    mpz_init(remainder);
    mpz_init(addend);

    mpz_ui_pow_ui(addend, 10, MAX_PRECISION);

    mpz_tdiv_qr(quotient, remainder, p->quantity, a->quantity);
    mpz_mul(remainder, remainder, addend);
    mpz_tdiv_q(remainder, remainder, a->quantity);
    mpz_mul(quotient, quotient, addend);
    mpz_add(quotient, quotient, remainder);

    a->priced = true;
    mpz_set(a->price, quotient);
    a->price_comm = p->quantity_comm;

    mpz_clear(quotient);
    mpz_clear(remainder);
    mpz_clear(addend);
  }
  return a;
}

static void round(mpz_t out, const mpz_t val, int prec)
{
  mpz_t divisor;
  mpz_t quotient;
  mpz_t remainder;

  mpz_init(divisor);
  mpz_init(quotient);
  mpz_init(remainder);

  mpz_ui_pow_ui(divisor, 10, MAX_PRECISION - prec);
  mpz_tdiv_qr(quotient, remainder, val, divisor);

  mpz_ui_pow_ui(divisor, 10, MAX_PRECISION - prec - 1);
  mpz_mul_ui(divisor, divisor, 5);
  if (mpz_cmp(remainder, divisor) >= 0) {
    mpz_ui_pow_ui(divisor, 10, MAX_PRECISION - prec);
    mpz_sub(remainder, divisor, remainder);
    mpz_add(out, val, remainder);
  } else {
    mpz_sub(out, val, remainder);
  }

  mpz_clear(divisor);
  mpz_clear(quotient);
  mpz_clear(remainder);
}

static void multiply(mpz_t out, const mpz_t l, const mpz_t r)
{
  mpz_t divisor;

  mpz_init(divisor);

  mpz_mul(out, l, r);

  // The number is at double-precision right now, so rounding at
  // precision 0 effectively means rounding to the ordinary
  // precision.
  round(out, out, 0);

  // after multiplying, truncate to the correct precision
  mpz_ui_pow_ui(divisor, 10, MAX_PRECISION);
  mpz_tdiv_q(out, out, divisor);

  mpz_clear(divisor);
}

amount * gmp_amount::copy() const
{
  gmp_amount * new_amt = new gmp_amount();
#if 0
  // Don't copy the price
  new_amt->priced = priced;
  mpz_set(new_amt->price, price);
  new_amt->price_comm = price_comm;
#endif
  mpz_set(new_amt->quantity, quantity);
  new_amt->quantity_comm = quantity_comm;
  return new_amt;
}

amount * gmp_amount::value() const
{
  if (! priced) {
    return copy();
  } else {
    gmp_amount * new_amt = new gmp_amount();
    new_amt->priced = false;
    multiply(new_amt->quantity, quantity, price);
    new_amt->quantity_comm = price_comm;
    return new_amt;
  }
}

gmp_amount::operator bool() const
{
  mpz_t copy;
  mpz_init_set(copy, quantity);
  assert(quantity_comm);
  if (quantity_comm->precision < MAX_PRECISION)
    round(copy, copy, quantity_comm->precision);
  bool zero = mpz_sgn(copy) == 0;
  mpz_clear(copy);
  return ! zero;
}

static std::string amount_to_str(const commodity * comm, const mpz_t val,
				 bool full_precision)
{
  mpz_t temp;
  mpz_t quotient;
  mpz_t rquotient;
  mpz_t remainder;
  mpz_t divisor;

  bool negative = false;

  mpz_init_set(temp, val);

  mpz_init(quotient);
  mpz_init(rquotient);
  mpz_init(remainder);
  mpz_init(divisor);

  if (! full_precision && comm->precision < MAX_PRECISION)
    round(temp, temp, comm->precision);

  mpz_ui_pow_ui(divisor, 10, MAX_PRECISION);
  mpz_tdiv_qr(quotient, remainder, temp, divisor);

  if (mpz_sgn(quotient) < 0 || mpz_sgn(remainder) < 0)
    negative = true;
  mpz_abs(quotient, quotient);
  mpz_abs(remainder, remainder);

  if (full_precision || comm->precision == MAX_PRECISION) {
    mpz_set(rquotient, remainder);
  } else {
    assert(MAX_PRECISION - comm->precision > 0);
    mpz_ui_pow_ui(divisor, 10, MAX_PRECISION - comm->precision);
    mpz_tdiv_qr(rquotient, remainder, remainder, divisor);
  }

  std::ostringstream s;

  if (comm->prefix) {
    s << comm->symbol;
    if (comm->separate)
      s << " ";
  }

  if (negative)
    s << "-";

  if (comm->thousands) {
    // jww (2003-09-29): use a smarter starting value

    bool printed = false;

    for (int powers = 27; powers >= 0; powers -= 3) {
      mpz_ui_pow_ui(divisor, 10, powers);
      mpz_tdiv_q(temp, quotient, divisor);

      if (mpz_sgn(temp) == 0)
	continue;

      mpz_ui_pow_ui(divisor, 10, 3);
      mpz_tdiv_r(temp, temp, divisor);

      if (printed) {
	s.width(3);
	s.fill('0');
      }
      s << temp;

      if (powers > 0) {
	if (comm->european)
	  s << ".";
	else
	  s << ",";

	printed = true;
      }
    }
  } else {
    s << quotient;
  }

  if (comm->european)
    s << ',';
  else
    s << '.';

  if (full_precision)
    s.width(MAX_PRECISION);
  else
    s.width(comm->precision);

  s.fill('0');
  s << rquotient;

  if (! comm->prefix) {
    if (comm->separate)
      s << " ";
    s << comm->symbol;
  }

  mpz_clear(temp);
  mpz_clear(quotient);
  mpz_clear(rquotient);
  mpz_clear(remainder);
  mpz_clear(divisor);

  return s.str();
}

std::string gmp_amount::as_str(bool full_prec) const
{
  std::ostringstream s;

  assert(quantity_comm);
  s << amount_to_str(quantity_comm, quantity, full_prec);

  if (priced) {
    assert(price_comm);
    s << " @ " << amount_to_str(price_comm, price, full_prec);
  }
  return s.str();
}

static void parse_number(mpz_t out, const char * num, commodity * comm)
{
  if (char * p = std::strchr(num, '/')) {
    mpz_t numer;
    mpz_t val;

    // The number was specified as a numerator over denominator, such
    // as 5250/100.  This gives us the precision, and avoids any
    // nastiness having to do with international numbering formats.

    std::string numer_str(num, p - num);
    mpz_init_set_str(numer, numer_str.c_str(), 10);
    mpz_init(val);

    int missing = MAX_PRECISION - (std::strlen(++p) - 1);
    assert(missing > 0);
    mpz_ui_pow_ui(val, 10, missing);

    mpz_mul(out, numer, val);

    mpz_clear(numer);
    mpz_clear(val);
  }
  else {
    static char buf[256];

    // The number is specified as the user desires, with the
    // commodity telling us how to parse it.

    std::memset(buf, '0', 255);
    std::strncpy(buf, num, std::strlen(num));

    if (comm->thousands)
      while (char * t = std::strchr(buf, comm->european ? '.' : ','))
	do { *t = *(t + 1); } while (*(t++ + 1));

    char * t = std::strchr(buf, comm->european ? ',' : '.');
    if (! t)
      t = buf + std::strlen(num);

    for (int prec = 0; prec < MAX_PRECISION; prec++) {
      *t = *(t + 1);
      t++;
    }
    *t = '\0';

    mpz_set_str(out, buf, 10);
  }
}

static commodity * parse_amount(mpz_t out, const char * num,
				int matched, int * ovector, int base)
{
  static char buf[256];

  bool saw_commodity = false;

  std::string symbol;
  bool prefix, separate, thousands, european;
  int precision, result;

  if (ovector[base * 2] >= 0) {
    // A prefix symbol was found
    saw_commodity = true;
    prefix = true;
    separate = ovector[(base + 2) * 2] != ovector[(base + 2) * 2 + 1];
    result = pcre_copy_substring(num, ovector, matched, base + 1, buf, 255);
    assert(result >= 0);
    symbol = buf;
  }

  // This is the value, and must be present
  assert(ovector[(base + 3) * 2] >= 0);
  result = pcre_copy_substring(num, ovector, matched, base + 3, buf, 255);
  assert(result >= 0);

  // Determine the precision used
  if (char * p = std::strchr(buf, '.'))
    precision = std::strlen(++p);
  else if (char * p = std::strchr(buf, '/'))
    precision = std::strlen(++p) - 1;
  else
    precision = 0;

  // Where "thousands" markers used?  Is it a european number?
  if (char * p = std::strrchr(buf, ',')) {
    if (std::strchr(p, '.'))
      thousands = true;
    else
      european = true;
  }

  // Parse the actual quantity
  std::string value_str = buf;

  if (ovector[(base + 4) * 2] >= 0) {
    // A suffix symbol was found
    saw_commodity = true;
    prefix = false;
    separate = ovector[(base + 5) * 2] != ovector[(base + 5) * 2 + 1];
    result = pcre_copy_substring(num, ovector, matched, base + 6, buf, 255);
    assert(result >= 0);
    symbol = buf;
  }

  commodity * comm = NULL;

  if (! saw_commodity) {
    std::cerr << "Error: No commodity specified: " << value_str
	      << std::endl;
    std::exit(1);
  } else {
    commodities_iterator item = commodities.find(symbol.c_str());
    if (item == commodities.end()) {
      comm = new commodity(symbol, prefix, separate,
			   thousands, european, precision);
    } else {
      comm = (*item).second;
#if 0
      // If a finer precision was used than the commodity allows,
      // increase the precision.
      if (precision > comm->precision)
	comm->precision = precision;
#else
      if (use_warnings && precision > comm->precision)
	std::cerr << "Warning: Use of higher precision than expected: "
		  << value_str << std::endl;
#endif
    }
  }
  assert(comm);

  parse_number(out, value_str.c_str(), comm);

  return comm;
}

amount& gmp_amount::operator=(const char * num)
{
  // Compile the regular expression used for parsing amounts
  static pcre * re = NULL;
  if (! re) {
    const char *error;
    int erroffset;
    static const std::string amount_re =
      "(([^-0-9/.,]+)(\\s*))?([-0-9/.,]+)((\\s*)([^-0-9/.,@]+))?";
    const std::string regexp =
      "^" + amount_re + "(\\s*@\\s*" + amount_re + ")?$";
    re = pcre_compile(regexp.c_str(), 0, &error, &erroffset, NULL);
  }

  int ovector[60];
  int matched;

  matched = pcre_exec(re, NULL, num, std::strlen(num), 0, 0, ovector, 60);
  if (matched > 0) {
    quantity_comm = parse_amount(quantity, num, matched, ovector, 1);

    // If the following succeeded, then we have a price
    if (ovector[8 * 2] >= 0) {
      priced = true;
      price_comm = parse_amount(price, num, matched, ovector, 9);
    }
  } else {
    std::cerr << "Failed to parse amount: " << num << std::endl;
  }
  return *this;
}

void gmp_amount::operator+=(const amount& _other)
{
  const gmp_amount& other = dynamic_cast<const gmp_amount&>(_other);
  assert(quantity_comm == other.quantity_comm);
  mpz_add(quantity, quantity, other.quantity);
}

} // namespace ledger
