#include <sstream>
#include <cassert>

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

  virtual amount * copy() const {
    gmp_amount * new_amt = new gmp_amount();
    new_amt->priced = priced;
    mpz_set(new_amt->price, price);
    new_amt->price_comm = price_comm;
    mpz_set(new_amt->quantity, quantity);
    new_amt->quantity_comm = quantity_comm;
    return new_amt;
  }

  virtual amount * value() const {
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

  virtual operator bool() const;

  virtual void credit(const amount * other) {
    *this += *other;
  }
  virtual void operator+=(const amount& other);

  virtual void parse(const char * num) {
    *this = num;
  }
  virtual amount& operator=(const char * num);
  virtual operator std::string() const;

  static const std::string to_str(const commodity * comm, const mpz_t val);

  static void parse(mpz_t out, char * num);
  static void round(mpz_t out, const mpz_t val, int prec);
  static void multiply(mpz_t out, const mpz_t l, const mpz_t r);

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

gmp_amount::operator bool() const
{
  mpz_t copy;
  mpz_init_set(copy, quantity);
  assert(quantity_comm);
  gmp_amount::round(copy, copy, quantity_comm->precision);
  bool zero = mpz_sgn(copy) == 0;
  mpz_clear(copy);
  return ! zero;
}

const std::string gmp_amount::to_str(const commodity * comm, const mpz_t val)
{
  mpz_t copy;
  mpz_t quotient;
  mpz_t rquotient;
  mpz_t remainder;
  mpz_t divisor;
  bool negative = false;

  mpz_init_set(copy, val);

  mpz_init(quotient);
  mpz_init(rquotient);
  mpz_init(remainder);
  mpz_init(divisor);

  gmp_amount::round(copy, copy, comm->precision);

  mpz_ui_pow_ui(divisor, 10, MAX_PRECISION);
  mpz_tdiv_qr(quotient, remainder, copy, divisor);

  if (mpz_sgn(quotient) < 0 || mpz_sgn(remainder) < 0)
    negative = true;
  mpz_abs(quotient, quotient);
  mpz_abs(remainder, remainder);

  assert(MAX_PRECISION - comm->precision > 0);
  mpz_ui_pow_ui(divisor, 10, MAX_PRECISION - comm->precision);
  mpz_tdiv_qr(rquotient, remainder, remainder, divisor);

  std::ostringstream s;

  if (comm->prefix) {
    s << comm->symbol;
    if (comm->separate)
      s << " ";
  }

  if (negative)
    s << "-";
  s << quotient;
  s << '.';

  s.width(comm->precision);
  s.fill('0');
  s << rquotient;

  if (! comm->prefix) {
    if (comm->separate)
      s << " ";
    s << comm->symbol;
  }

  mpz_clear(copy);
  mpz_clear(quotient);
  mpz_clear(rquotient);
  mpz_clear(remainder);
  mpz_clear(divisor);

  return s.str();
}

gmp_amount::operator std::string() const
{
  std::ostringstream s;

  assert(quantity_comm);
  s << to_str(quantity_comm, quantity);

  if (priced) {
    assert(price_comm);
    s << " @ " << to_str(price_comm, price);
  }
  return s.str();
}

void gmp_amount::parse(mpz_t out, char * num)
{
  if (char * p = std::strchr(num, '/')) {
    mpz_t numer;
    mpz_t val;

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

    // jww (2003-09-28): What if there is no decimal?

    std::memset(buf, '0', 255);
    std::strncpy(buf, num, std::strlen(num));

    char * t = std::strchr(buf, '.');
    for (int prec = 0; prec < MAX_PRECISION; prec++) {
      *t = *(t + 1);
      t++;
    }
    *t = '\0';

    mpz_set_str(out, buf, 10);
  }
}

amount& gmp_amount::operator=(const char * num)
{
  // Compile the regular expression used for parsing amounts
  static pcre * re = NULL;
  if (! re) {
    const char *error;
    int erroffset;
    static const std::string amount_re =
      "(([^-0-9/.]+)(\\s*))?([-0-9/.]+)((\\s*)([^-0-9/.@]+))?";
    const std::string regexp =
      "^" + amount_re + "(\\s*@\\s*" + amount_re + ")?$";
    re = pcre_compile(regexp.c_str(), 0, &error, &erroffset, NULL);
  }

  bool saw_commodity;
  std::string symbol;
  bool prefix;
  bool separate;
  int precision;

  static char buf[256];
  int ovector[60];
  int matched, result;

  matched = pcre_exec(re, NULL, num, std::strlen(num), 0, 0, ovector, 60);
  if (matched > 0) {
    saw_commodity = false;

    if (ovector[1 * 2] >= 0) {
      // A prefix symbol was found
      saw_commodity = true;
      prefix = true;
      separate = ovector[3 * 2] != ovector[3 * 2 + 1];
      result = pcre_copy_substring(num, ovector, matched, 2, buf, 255);
      assert(result >= 0);
      symbol = buf;
    }

    // This is the value, and must be present
    assert(ovector[4 * 2] >= 0);
    result = pcre_copy_substring(num, ovector, matched, 4, buf, 255);
    assert(result >= 0);

    // Determine the precision used
    if (char * p = std::strchr(buf, '.'))
      precision = std::strlen(++p);
    else if (char * p = std::strchr(buf, '/'))
      precision = std::strlen(++p) - 1;
    else
      precision = 0;

    // Parse the actual quantity
    parse(quantity, buf);

    if (ovector[5 * 2] >= 0) {
      // A suffix symbol was found
      saw_commodity = true;
      prefix = false;
      separate = ovector[6 * 2] != ovector[6 * 2 + 1];
      result = pcre_copy_substring(num, ovector, matched, 7, buf, 255);
      assert(result >= 0);
      symbol = buf;
    }

    if (! saw_commodity) {
      quantity_comm = commodity_usd;
    } else {
      commodities_iterator item = commodities.find(symbol.c_str());
      if (item == commodities.end()) {
	quantity_comm = new commodity(symbol, prefix, separate, precision);
	std::pair<commodities_iterator, bool> insert_result =
	  commodities.insert(commodities_entry(symbol, quantity_comm));
	assert(insert_result.second);
      } else {
	quantity_comm = (*item).second;

	// If a finer precision was used than the commodity allows,
	// increase the precision.
	if (precision > quantity_comm->precision)
	  quantity_comm->precision = precision;
      }
    }

    // If the following succeeded, then we have a price
    if (ovector[8 * 2] >= 0) {
      saw_commodity = false;

      if (ovector[9 * 2] >= 0) {
	// A prefix symbol was found
	saw_commodity = true;
	prefix = true;
	separate = ovector[11 * 2] != ovector[11 * 2 + 1];
	result = pcre_copy_substring(num, ovector, matched, 10, buf, 255);
	assert(result >= 0);
	symbol = buf;
      }

      assert(ovector[12 * 2] >= 0);
      result = pcre_copy_substring(num, ovector, matched, 4, buf, 255);
      assert(result >= 0);

      // Determine the precision used
      if (char * p = std::strchr(buf, '.'))
	precision = std::strlen(++p);
      else if (char * p = std::strchr(buf, '/'))
	precision = std::strlen(++p) - 1;
      else
	precision = 0;

      // Parse the actual price
      parse(price, buf);
      priced = true;

      if (ovector[13 * 2] >= 0) {
	// A suffix symbol was found
	saw_commodity = true;
	prefix = false;
	separate = ovector[14 * 2] != ovector[14 * 2 + 1];
	result = pcre_copy_substring(num, ovector, matched, 15, buf, 255);
	assert(result >= 0);
	symbol = buf;
      }

      if (! saw_commodity) {
	price_comm = commodity_usd;
      } else {
	commodities_iterator item = commodities.find(symbol.c_str());
	if (item == commodities.end()) {
	  price_comm = new commodity(symbol, prefix, separate, precision);
	  std::pair<commodities_iterator, bool> insert_result =
	    commodities.insert(commodities_entry(symbol, price_comm));
	  assert(insert_result.second);
	} else {
	  price_comm = (*item).second;

	  // If a finer precision was used than the commodity allows,
	  // increase the precision.
	  if (precision > price_comm->precision)
	    price_comm->precision = precision;
	}
      }
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

void gmp_amount::round(mpz_t out, const mpz_t val, int prec)
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

void gmp_amount::multiply(mpz_t out, const mpz_t l, const mpz_t r)
{
  mpz_t divisor;

  mpz_init(divisor);

  mpz_mul(out, l, r);

  // The number is at double-precision right now, so rounding at
  // precision 0 effectively means rounding to the ordinary
  // precision.
  gmp_amount::round(out, out, 0);

  // after multiplying, truncate to the correct precision
  mpz_ui_pow_ui(divisor, 10, MAX_PRECISION);
  mpz_tdiv_q(out, out, divisor);

  mpz_clear(divisor);
}

} // namespace ledger
