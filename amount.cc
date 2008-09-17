#include "amount.h"
#include "util.h"

#include <list>
#include <sstream>
#include <cstdlib>
#include <memory>

#include <gmp.h>

namespace ledger {

bool do_cleanup = true;

bool amount_t::keep_price = false;
bool amount_t::keep_date  = false;
bool amount_t::keep_tag   = false;
bool amount_t::keep_base  = false;

#define BIGINT_BULK_ALLOC 0x0001
#define BIGINT_KEEP_PREC  0x0002

class amount_t::bigint_t {
 public:
  mpz_t		val;
  unsigned char prec;
  unsigned char flags;
  unsigned int	ref;
  unsigned int	index;

  bigint_t() : prec(0), flags(0), ref(1), index(0) {
    mpz_init(val);
  }
  bigint_t(mpz_t _val) : prec(0), flags(0), ref(1), index(0) {
    mpz_init_set(val, _val);
  }
  bigint_t(const bigint_t& other)
    : prec(other.prec), flags(other.flags & BIGINT_KEEP_PREC),
      ref(1), index(0) {
    mpz_init_set(val, other.val);
  }
  ~bigint_t();
};

unsigned int sizeof_bigint_t() {
  return sizeof(amount_t::bigint_t);
}

#define MPZ(x) ((x)->val)

static mpz_t temp;		// these are the global temp variables
static mpz_t divisor;

static amount_t::bigint_t true_value;

inline amount_t::bigint_t::~bigint_t() {
  assert(ref == 0 || (! do_cleanup && this == &true_value));
  mpz_clear(val);
}

base_commodities_map commodity_base_t::commodities;

commodity_base_t::updater_t * commodity_base_t::updater = NULL;

commodities_map	commodity_t::commodities;
bool		commodity_t::commodities_sorted = false;
commodity_t *   commodity_t::null_commodity;
commodity_t *   commodity_t::default_commodity = NULL;

static struct _init_amounts {
  _init_amounts() {
    mpz_init(temp);
    mpz_init(divisor);

    mpz_set_ui(true_value.val, 1);

    commodity_base_t::updater	   = NULL;
    commodity_t::null_commodity    = commodity_t::create("");
    commodity_t::default_commodity = NULL;

    commodity_t::null_commodity->add_flags(COMMODITY_STYLE_NOMARKET |
					   COMMODITY_STYLE_BUILTIN);

    // Add time commodity conversions, so that timelog's may be parsed
    // in terms of seconds, but reported as minutes or hours.
    commodity_t * commodity;

    commodity = commodity_t::create("s");
    commodity->add_flags(COMMODITY_STYLE_NOMARKET | COMMODITY_STYLE_BUILTIN);

    parse_conversion("1.0m", "60s");
    parse_conversion("1.0h", "60m");

#if 0
    commodity = commodity_t::create("b");
    commodity->add_flags(COMMODITY_STYLE_NOMARKET | COMMODITY_STYLE_BUILTIN);

    parse_conversion("1.00 Kb", "1024 b");
    parse_conversion("1.00 Mb", "1024 Kb");
    parse_conversion("1.00 Gb", "1024 Mb");
    parse_conversion("1.00 Tb", "1024 Gb");
#endif
  }

  ~_init_amounts() {
    if (! do_cleanup)
      return;

    mpz_clear(temp);
    mpz_clear(divisor);

    if (commodity_base_t::updater) {
      delete commodity_base_t::updater;
      commodity_base_t::updater = NULL;
    }

    for (commodities_map::iterator i = commodity_t::commodities.begin();
	 i != commodity_t::commodities.end();
	 i++)
      delete (*i).second;

    commodity_t::commodities.clear();

    true_value.ref--;
  }
} _init_obj;

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

amount_t::amount_t(const bool value)
{
  if (value) {
    quantity = &true_value;
    quantity->ref++;
  } else {
    quantity = NULL;
  }
  commodity_ = NULL;
}

amount_t::amount_t(const long value)
{
  if (value != 0) {
    quantity = new bigint_t;
    mpz_set_si(MPZ(quantity), value);
  } else {
    quantity = NULL;
  }
  commodity_ = NULL;
}

amount_t::amount_t(const unsigned long value)
{
  if (value != 0) {
    quantity = new bigint_t;
    mpz_set_ui(MPZ(quantity), value);
  } else {
    quantity = NULL;
  }
  commodity_ = NULL;
}

amount_t::amount_t(const double value)
{
  if (value != 0.0) {
    quantity = new bigint_t;
    mpz_set_d(MPZ(quantity), value);
  } else {
    quantity = NULL;
  }
  commodity_ = NULL;
}

void amount_t::_release()
{
  DEBUG_PRINT("amounts.refs",
	      quantity << " ref--, now " << (quantity->ref - 1));
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
      DEBUG_PRINT("amounts.refs",
		  quantity << " ref++, now " << (quantity->ref + 1));
      quantity->ref++;
    }
  }
  commodity_ = amt.commodity_;
}

amount_t& amount_t::operator=(const std::string& value)
{
  std::istringstream str(value);
  parse(str);
  return *this;
}

amount_t& amount_t::operator=(const char * value)
{
  std::string valstr(value);
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

amount_t& amount_t::operator=(const bool value)
{
  if (! value) {
    if (quantity)
      _clear();
  } else {
    commodity_ = NULL;
    if (quantity)
      _release();
    quantity = &true_value;
    quantity->ref++;
  }
  return *this;
}

amount_t& amount_t::operator=(const long value)
{
  if (value == 0) {
    if (quantity)
      _clear();
  } else {
    commodity_ = NULL;
    _init();
    mpz_set_si(MPZ(quantity), value);
  }
  return *this;
}

amount_t& amount_t::operator=(const unsigned long value)
{
  if (value == 0) {
    if (quantity)
      _clear();
  } else {
    commodity_ = NULL;
    _init();
    mpz_set_ui(MPZ(quantity), value);
  }
  return *this;
}

amount_t& amount_t::operator=(const double value)
{
  if (value == 0.0) {
    if (quantity)
      _clear();
  } else {
    commodity_ = NULL;
    _init();
    mpz_set_d(MPZ(quantity), value);
  }
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


amount_t& amount_t::operator+=(const amount_t& amt)
{
  if (! amt.quantity)
    return *this;

  if (! quantity) {
    _copy(amt);
    return *this;
  }

  _dup();

  if (commodity() != amt.commodity())
    throw new amount_error
      (std::string("Adding amounts with different commodities: ") +
       commodity_->qualified_symbol + " != " +
       amt.commodity_->qualified_symbol);

  if (quantity->prec == amt.quantity->prec) {
    mpz_add(MPZ(quantity), MPZ(quantity), MPZ(amt.quantity));
  }
  else if (quantity->prec < amt.quantity->prec) {
    _resize(amt.quantity->prec);
    mpz_add(MPZ(quantity), MPZ(quantity), MPZ(amt.quantity));
  }
  else {
    amount_t temp = amt;
    temp._resize(quantity->prec);
    mpz_add(MPZ(quantity), MPZ(quantity), MPZ(temp.quantity));
  }

  return *this;
}

amount_t& amount_t::operator-=(const amount_t& amt)
{
  if (! amt.quantity)
    return *this;

  if (! quantity) {
    quantity  = new bigint_t(*amt.quantity);
    commodity_ = amt.commodity_;
    mpz_neg(MPZ(quantity), MPZ(quantity));
    return *this;
  }

  _dup();

  if (commodity() != amt.commodity())
    throw new amount_error
      (std::string("Subtracting amounts with different commodities: ") +
       commodity_->qualified_symbol + " != " +
       amt.commodity_->qualified_symbol);

  if (quantity->prec == amt.quantity->prec) {
    mpz_sub(MPZ(quantity), MPZ(quantity), MPZ(amt.quantity));
  }
  else if (quantity->prec < amt.quantity->prec) {
    _resize(amt.quantity->prec);
    mpz_sub(MPZ(quantity), MPZ(quantity), MPZ(amt.quantity));
  }
  else {
    amount_t temp = amt;
    temp._resize(quantity->prec);
    mpz_sub(MPZ(quantity), MPZ(quantity), MPZ(temp.quantity));
  }

  return *this;
}

amount_t& amount_t::operator*=(const amount_t& amt)
{
  if (! amt.quantity)
    return (*this = amt);
  else if (! quantity)
    return *this;

  _dup();

  mpz_mul(MPZ(quantity), MPZ(quantity), MPZ(amt.quantity));
  quantity->prec += amt.quantity->prec;

  unsigned int comm_prec = commodity().precision();
  if (quantity->prec > comm_prec + 6U) {
    mpz_round(MPZ(quantity), MPZ(quantity), quantity->prec, comm_prec + 6U);
    quantity->prec = comm_prec + 6U;
  }

  return *this;
}

amount_t& amount_t::operator/=(const amount_t& amt)
{
  if (! amt.quantity || ! amt)
    throw new amount_error("Divide by zero");
  else if (! quantity)
    return *this;

  _dup();

  // Increase the value's precision, to capture fractional parts after
  // the divide.
  mpz_ui_pow_ui(divisor, 10, amt.quantity->prec + 6U);
  mpz_mul(MPZ(quantity), MPZ(quantity), divisor);
  mpz_tdiv_q(MPZ(quantity), MPZ(quantity), MPZ(amt.quantity));
  quantity->prec += 6U;

  unsigned int comm_prec = commodity().precision();
  if (quantity->prec > comm_prec + 6U) {
    mpz_round(MPZ(quantity), MPZ(quantity), quantity->prec, comm_prec + 6U);
    quantity->prec = comm_prec + 6U;
  }

  return *this;
}

// unary negation
void amount_t::negate()
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

  if (commodity() && amt.commodity() && commodity() != amt.commodity())
    throw new amount_error
      (std::string("Cannot compare amounts with different commodities: ") +
       commodity().symbol() + " and " + amt.commodity().symbol());

  if (quantity->prec == amt.quantity->prec) {
    return mpz_cmp(MPZ(quantity), MPZ(amt.quantity));
  }
  else if (quantity->prec < amt.quantity->prec) {
    amount_t temp = *this;
    temp._resize(amt.quantity->prec);
    return mpz_cmp(MPZ(temp.quantity), MPZ(amt.quantity));
  }
  else {
    amount_t temp = amt;
    temp._resize(quantity->prec);
    return mpz_cmp(MPZ(quantity), MPZ(temp.quantity));
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

amount_t::operator bool() const
{
  if (! quantity)
    return false;

  if (quantity->prec <= commodity().precision() ||
      (quantity->flags & BIGINT_KEEP_PREC)) {
    return mpz_sgn(MPZ(quantity)) != 0;
  } else {
    mpz_set(temp, MPZ(quantity));
    mpz_ui_pow_ui(divisor, 10, quantity->prec - commodity().precision());
    mpz_tdiv_q(temp, temp, divisor);
    bool zero = mpz_sgn(temp) == 0;
    return ! zero;
  }
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

bool amount_t::realzero() const
{
  if (! quantity)
    return true;
  return mpz_sgn(MPZ(quantity)) == 0;
}

amount_t amount_t::value(const datetime_t& moment) const
{
  if (quantity) {
    amount_t amt(commodity().value(moment));
    if (! amt.realzero())
      return (amt * *this).round();
  }
  return *this;
}

amount_t amount_t::round(unsigned int prec) const
{
  amount_t temp = *this;

  if (! quantity || quantity->prec <= prec) {
    if (quantity && quantity->flags & BIGINT_KEEP_PREC) {
      temp._dup();
      temp.quantity->flags &= ~BIGINT_KEEP_PREC;
    }
    return temp;
  }

  temp._dup();

  mpz_round(MPZ(temp.quantity), MPZ(temp.quantity), temp.quantity->prec, prec);

  temp.quantity->prec = prec;
  temp.quantity->flags &= ~BIGINT_KEEP_PREC;

  return temp;
}

amount_t amount_t::unround() const
{
  if (! quantity) {
    amount_t temp(0L);
    assert(temp.quantity);
    temp.quantity->flags |= BIGINT_KEEP_PREC;
    return temp;
  }
  else if (quantity->flags & BIGINT_KEEP_PREC) {
    return *this;
  }

  amount_t temp = *this;
  temp._dup();
  temp.quantity->flags |= BIGINT_KEEP_PREC;

  return temp;
}

std::string amount_t::quantity_string() const
{
  if (! quantity)
    return "0";

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

  if (mpz_sgn(quotient) == 0 && mpz_sgn(rquotient) == 0)
    return "0";

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

  return out.str();
}

std::ostream& operator<<(std::ostream& _out, const amount_t& amt)
{
  if (! amt.quantity) {
    _out << "0";
    return _out;
  }

  amount_t base(amt);
  if (! amount_t::keep_base && amt.commodity().larger()) {
    amount_t last(amt);
    while (last.commodity().larger()) {
      last /= *last.commodity().larger();
      last.commodity_ = last.commodity().larger()->commodity_;
      if (ledger::abs(last) < 1)
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
  unsigned char precision;

  if (! comm || base.quantity->flags & BIGINT_KEEP_PREC) {
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

  if (mpz_sgn(quotient) == 0 && mpz_sgn(rquotient) == 0) {
    _out << "0";
    return _out;
  }

  if (! (comm.flags() & COMMODITY_STYLE_SUFFIXED)) {
    comm.write(out);

    if (comm.flags() & COMMODITY_STYLE_SEPARATED)
      out << " ";
  }

  if (negative)
    out << "-";

  if (mpz_sgn(quotient) == 0) {
    out << '0';
  }
  else if (! (comm.flags() & COMMODITY_STYLE_THOUSANDS)) {
    char * p = mpz_get_str(NULL, 10, quotient);
    out << p;
    std::free(p);
  }
  else {
    std::list<std::string> strs;
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

    for (std::list<std::string>::reverse_iterator i = strs.rbegin();
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

  if (precision) {
    std::ostringstream final;
    final.width(precision);
    final.fill('0');
    char * p = mpz_get_str(NULL, 10, rquotient);
    final << p;
    std::free(p);

    const std::string& str(final.str());
    int i, len = str.length();
    const char * q = str.c_str();
    for (i = len; i > 0; i--)
      if (q[i - 1] != '0')
	break;

    std::string ender;
    if (i == len)
      ender = str;
    else if (i < comm.precision())
      ender = std::string(str, 0, comm.precision());
    else
      ender = std::string(str, 0, i);

    if (! ender.empty()) {
      out << ((comm.flags() & COMMODITY_STYLE_EUROPEAN) ? ',' : '.');
      out << ender;
    }
  }

  if (comm.flags() & COMMODITY_STYLE_SUFFIXED) {
    if (comm.flags() & COMMODITY_STYLE_SEPARATED)
      out << " ";

    comm.write(out);
  }

  mpz_clear(quotient);
  mpz_clear(rquotient);
  mpz_clear(remainder);

  // If there are any annotations associated with this commodity,
  // output them now.

  if (comm.annotated) {
    annotated_commodity_t& ann(static_cast<annotated_commodity_t&>(comm));
    assert(&ann.price != &amt);
    ann.write_annotations(out);
  }

  // Things are output to a string first, so that if anyone has
  // specified a width or fill for _out, it will be applied to the
  // entire amount string, and not just the first part.

  _out << out.str();

  return _out;
}

void parse_quantity(std::istream& in, std::string& value)
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

void parse_commodity(std::istream& in, std::string& symbol)
{
  char buf[256];
  char c = peek_next_nonws(in);
  if (c == '"') {
    in.get(c);
    READ_INTO(in, buf, 255, c, c != '"');
    if (c == '"')
      in.get(c);
    else
      throw new amount_error("Quoted commodity symbol lacks closing quote");
  } else {
    READ_INTO(in, buf, 255, c, ! invalid_chars[(unsigned char)c]);
  }
  symbol = buf;
}

void parse_annotations(std::istream& in, amount_t& price,
		       datetime_t& date, std::string& tag)
{
  do {
    char buf[256];
    char c = peek_next_nonws(in);
    if (c == '{') {
      if (price)
	throw new amount_error("Commodity specifies more than one price");

      in.get(c);
      READ_INTO(in, buf, 255, c, c != '}');
      if (c == '}')
	in.get(c);
      else
	throw new amount_error("Commodity price lacks closing brace");

      price.parse(buf, AMOUNT_PARSE_NO_MIGRATE);
      price.reduce();

      // Since this price will maintain its own precision, make sure
      // it is at least as large as the base commodity, since the user
      // may have only specified {$1} or something similar.

      if (price.quantity->prec < price.commodity().precision())
	price = price.round();	// no need to retain individual precision
    }
    else if (c == '[') {
      if (date)
	throw new amount_error("Commodity specifies more than one date");

      in.get(c);
      READ_INTO(in, buf, 255, c, c != ']');
      if (c == ']')
	in.get(c);
      else
	throw new amount_error("Commodity date lacks closing bracket");

      date = buf;
    }
    else if (c == '(') {
      if (! tag.empty())
	throw new amount_error("Commodity specifies more than one tag");

      in.get(c);
      READ_INTO(in, buf, 255, c, c != ')');
      if (c == ')')
	in.get(c);
      else
	throw new amount_error("Commodity tag lacks closing parenthesis");

      tag = buf;
    }
    else {
      break;
    }
  } while (true);

  DEBUG_PRINT("amounts.commodities",
	      "Parsed commodity annotations: "
	      << "  price " << price << " "
	      << "  date " << date << " "
	      << "  tag " << tag);
}

bool amount_t::parse(std::istream& in, unsigned char flags)
{
  // The possible syntax for an amount is:
  //
  //   [-]NUM[ ]SYM [@ AMOUNT]
  //   SYM[ ][-]NUM [@ AMOUNT]

  std::string  symbol;
  std::string  quant;
  amount_t     price;
  datetime_t   date;
  std::string  tag;
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
	parse_annotations(in, price, date, tag);
    }
  } else {
    parse_commodity(in, symbol);

    if (! in.eof() && ((n = in.peek()) != '\n')) {
      if (std::isspace(in.peek()))
	comm_flags |= COMMODITY_STYLE_SEPARATED;

      parse_quantity(in, quant);

      if (! quant.empty() && ! in.eof() && ((n = in.peek()) != '\n'))
	parse_annotations(in, price, date, tag);
    }
  }

  if (quant.empty()) {
    if (flags & AMOUNT_PARSE_SOFT_FAIL)
      return false;
    else
      throw new amount_error("No quantity specified for amount");
  }

  _init();

  // Create the commodity if has not already been seen, and update the
  // precision if something greater was used for the quantity.

  bool newly_created = false;

  if (symbol.empty()) {
    commodity_ = commodity_t::null_commodity;
  } else {
    commodity_ = commodity_t::find(symbol);
    if (! commodity_) {
      commodity_ = commodity_t::create(symbol);
      newly_created = true;
    }
    assert(commodity_);

    if (! price.realzero() || date || ! tag.empty())
      commodity_ =
	annotated_commodity_t::find_or_create(*commodity_, price, date, tag);
  }

  // Determine the precision of the amount, based on the usage of
  // comma or period.

  std::string::size_type last_comma  = quant.rfind(',');
  std::string::size_type last_period = quant.rfind('.');

  if (last_comma != std::string::npos && last_period != std::string::npos) {
    comm_flags |= COMMODITY_STYLE_THOUSANDS;
    if (last_comma > last_period) {
      comm_flags |= COMMODITY_STYLE_EUROPEAN;
      quantity->prec = quant.length() - last_comma - 1;
    } else {
      quantity->prec = quant.length() - last_period - 1;
    }
  }
  else if (last_comma != std::string::npos &&
	   (! commodity_t::default_commodity ||
	    commodity_t::default_commodity->flags() & COMMODITY_STYLE_EUROPEAN)) {
      comm_flags |= COMMODITY_STYLE_EUROPEAN;
    quantity->prec = quant.length() - last_comma - 1;
  }
  else if (last_period != std::string::npos &&
	   ! (commodity().flags() & COMMODITY_STYLE_EUROPEAN)) {
    quantity->prec = quant.length() - last_period - 1;
  }
  else {
    quantity->prec = 0;
  }

  // Set the commodity's flags and precision accordingly

  if (! (flags & AMOUNT_PARSE_NO_MIGRATE)) {
    commodity().add_flags(comm_flags);
    if (quantity->prec > commodity().precision())
      commodity().set_precision(quantity->prec);
  } else {
    quantity->flags |= BIGINT_KEEP_PREC;
  }

  // Now we have the final number.  Remove commas and periods, if
  // necessary.

  if (last_comma != std::string::npos || last_period != std::string::npos) {
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
    negate();

  if (! (flags & AMOUNT_PARSE_NO_REDUCE))
    reduce();

  return true;
}

void amount_t::reduce()
{
  while (commodity_ && commodity().smaller()) {
    *this *= *commodity().smaller();
    commodity_ = commodity().smaller()->commodity_;
  }
}

bool amount_t::parse(const std::string& str, unsigned char flags)
{
  std::istringstream stream(str);
  parse(stream, flags);
}

void parse_conversion(const std::string& larger_str,
		      const std::string& smaller_str)
{
  amount_t larger, smaller;

  larger.parse(larger_str.c_str(), AMOUNT_PARSE_NO_REDUCE);
  smaller.parse(smaller_str.c_str(), AMOUNT_PARSE_NO_REDUCE);

  larger *= smaller;

  if (larger.commodity()) {
    larger.commodity().set_smaller(smaller);
    larger.commodity().add_flags(smaller.commodity().flags() |
				 COMMODITY_STYLE_NOMARKET);
  }
  if (smaller.commodity())
    smaller.commodity().set_larger(larger);
}


char *	     bigints;
char *	     bigints_next;
unsigned int bigints_index;
unsigned int bigints_count;

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
    DEBUG_PRINT("amounts.refs",
		quantity << " ref++, now " << (quantity->ref + 1));
    quantity->ref++;
  }
}

static char buf[4096];

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
      DEBUG_PRINT("ledger.validate", "amount_t: quantity->ref == 0");
      return false;
    }
  }
  else if (commodity_) {
    DEBUG_PRINT("ledger.validate", "amount_t: commodity_ != NULL");
    return false;
  }
  return true;
}

void amount_t::annotate_commodity(const amount_t&    price,
				  const datetime_t&  date,
				  const std::string& tag)
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

  DEBUG_PRINT("amounts.commodities", "Annotating commodity for amount "
	      << *this << std::endl
	      << "  price " << price << " "
	      << "  date " << date << " "
	      << "  tag " << tag);

  commodity_t * ann_comm =
    annotated_commodity_t::find_or_create
      (*this_base, ! price && this_ann ? this_ann->price : price,
       ! date && this_ann ? this_ann->date : date,
       tag.empty() && this_ann ? this_ann->tag : tag);
  if (ann_comm)
    set_commodity(*ann_comm);

  DEBUG_PRINT("amounts.commodities", "  Annotated amount is " << *this);
}

amount_t amount_t::strip_annotations(const bool _keep_price,
				     const bool _keep_date,
				     const bool _keep_tag) const
{
  if (! commodity().annotated ||
      (_keep_price && _keep_date && _keep_tag))
    return *this;

  DEBUG_PRINT("amounts.commodities", "Reducing commodity for amount "
	      << *this << std::endl
	      << "  keep price " << _keep_price << " "
	      << "  keep date " << _keep_date << " "
	      << "  keep tag " << _keep_tag);

  annotated_commodity_t&
    ann_comm(static_cast<annotated_commodity_t&>(commodity()));
  assert(ann_comm.base);

  commodity_t * new_comm;

  if ((_keep_price && ann_comm.price) ||
      (_keep_date && ann_comm.date) ||
      (_keep_tag && ! ann_comm.tag.empty()))
  {
    new_comm = annotated_commodity_t::find_or_create
      (*ann_comm.ptr, _keep_price ? ann_comm.price : amount_t(),
       _keep_date ? ann_comm.date : datetime_t(),
       _keep_tag ? ann_comm.tag : "");
  } else {
    new_comm = commodity_t::find_or_create(ann_comm.base_symbol());
  }
  assert(new_comm);

  amount_t temp(*this);
  temp.set_commodity(*new_comm);

  DEBUG_PRINT("amounts.commodities", "  Reduced amount is " << temp);

  return temp;
}

amount_t amount_t::price() const
{
  if (commodity_ && commodity_->annotated) {
    amount_t temp(((annotated_commodity_t *)commodity_)->price);
    temp *= *this;
    DEBUG_PRINT("amounts.commodities",
		"Returning price of " << *this << " = " << temp);
    return temp;
  }
  return *this;
}

datetime_t amount_t::date() const
{
  if (commodity_ && commodity_->annotated) {
    DEBUG_PRINT("amounts.commodities",
		"Returning date of " << *this << " = "
		<< ((annotated_commodity_t *)commodity_)->date);
    return ((annotated_commodity_t *)commodity_)->date;
  }
  return 0L;
}


void commodity_base_t::add_price(const datetime_t& date,
				 const amount_t&   price)
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

bool commodity_base_t::remove_price(const datetime_t& date)
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

commodity_base_t * commodity_base_t::create(const std::string& symbol)
{
  commodity_base_t * commodity = new commodity_base_t(symbol);

  DEBUG_PRINT("amounts.commodities", "Creating base commodity " << symbol);

  std::pair<base_commodities_map::iterator, bool> result
    = commodities.insert(base_commodities_pair(symbol, commodity));
  assert(result.second);

  return commodity;
}

bool commodity_t::needs_quotes(const std::string& symbol)
{
  for (const char * p = symbol.c_str(); *p; p++)
    if (std::isspace(*p) || std::isdigit(*p) || *p == '-' || *p == '.')
      return true;

  return false;
}

bool commodity_t::valid() const
{
  if (symbol().empty() && this != null_commodity) {
    DEBUG_PRINT("ledger.validate",
		"commodity_t: symbol().empty() && this != null_commodity");
    return false;
  }

  if (annotated && ! base) {
    DEBUG_PRINT("ledger.validate", "commodity_t: annotated && ! base");
    return false;
  }

  if (precision() > 16) {
    DEBUG_PRINT("ledger.validate", "commodity_t: precision() > 16");
    return false;
  }

  return true;
}

commodity_t * commodity_t::create(const std::string& symbol)
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

  DEBUG_PRINT("amounts.commodities",
	      "Creating commodity " << commodity->qualified_symbol);

  std::pair<commodities_map::iterator, bool> result
    = commodities.insert(commodities_pair(symbol, commodity.get()));
  if (! result.second)
    return NULL;

  // Start out the new commodity with the default commodity's flags
  // and precision, if one has been defined.
  if (default_commodity)
    commodity->drop_flags(COMMODITY_STYLE_THOUSANDS |
			  COMMODITY_STYLE_NOMARKET);

  return commodity.release();
}

commodity_t * commodity_t::find_or_create(const std::string& symbol)
{
  DEBUG_PRINT("amounts.commodities", "Find-or-create commodity " << symbol);

  commodity_t * commodity = find(symbol);
  if (commodity)
    return commodity;
  return create(symbol);
}

commodity_t * commodity_t::find(const std::string& symbol)
{
  DEBUG_PRINT("amounts.commodities", "Find commodity " << symbol);

  commodities_map::const_iterator i = commodities.find(symbol);
  if (i != commodities.end())
    return (*i).second;
  return NULL;
}

amount_t commodity_base_t::value(const datetime_t& moment)
{
  datetime_t age;
  amount_t   price;

  if (history) {
    assert(history->prices.size() > 0);

    if (! moment) {
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
	    age = 0;
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
		(*history->prices.rbegin()).first : datetime_t()), price);

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

  if (date &&
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
					 const datetime_t&  date,
					 const std::string& tag)
{
  if (price)
    out << " {" << price << '}';

  if (date)
    out << " [" << date_t(date) << ']';

  if (! tag.empty())
    out << " (" << tag << ')';
}

commodity_t *
annotated_commodity_t::create(const commodity_t& comm,
			      const amount_t&    price,
			      const datetime_t&  date,
			      const std::string& tag,
			      const std::string& mapping_key)
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

  DEBUG_PRINT("amounts.commodities", "Creating annotated commodity "
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

  return commodity.release();
}

namespace {
  std::string make_qualified_name(const commodity_t& comm,
				  const amount_t&    price,
				  const datetime_t&  date,
				  const std::string& tag)
  {
    if (price < 0)
      throw new amount_error("A commodity's price may not be negative");

    std::ostringstream name;

    comm.write(name);
    annotated_commodity_t::write_annotations(name, price, date, tag);

    DEBUG_PRINT("amounts.commodities", "make_qualified_name for "
		<< comm.qualified_symbol << std::endl
		<< "  price " << price << " "
		<< "  date " << date << " "
		<< "  tag " << tag);

    DEBUG_PRINT("amounts.commodities", "qualified_name is " << name.str());

    return name.str();
  }
}

commodity_t *
annotated_commodity_t::find_or_create(const commodity_t& comm,
				      const amount_t&    price,
				      const datetime_t&  date,
				      const std::string& tag)
{
  std::string name = make_qualified_name(comm, price, date, tag);

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
      leftprice.reduce();
      amount_t rightprice(arightcomm.price);
      rightprice.reduce();

      if (leftprice.commodity() == rightprice.commodity()) {
	int diff = leftprice.compare(rightprice);
	if (diff)
	  return diff;
      } else {
	// Since we have two different amounts, there's really no way
	// to establish a true sorting order; we'll just do it based
	// on the numerical values.
	leftprice.clear_commodity();
	rightprice.clear_commodity();

	int diff = leftprice.compare(rightprice);
	if (diff)
	  return diff;
      }
    }

    if (! aleftcomm.date && arightcomm.date)
      return true;
    if (aleftcomm.date && ! arightcomm.date)
      return false;

    if (aleftcomm.date && arightcomm.date) {
      int diff = aleftcomm.date - arightcomm.date;
      if (diff)
	return diff < 0;
    }

    if (aleftcomm.tag.empty() && ! arightcomm.tag.empty())
      return true;
    if (! aleftcomm.tag.empty() && arightcomm.tag.empty())
      return false;

    if (! aleftcomm.tag.empty() && ! arightcomm.tag.empty())
      return aleftcomm.tag < arightcomm.tag;

    // The two annotated commodities don't differ enough to matter.  This
    // should make this identical.
    return true;
  }
}

} // namespace ledger

#ifdef USE_BOOST_PYTHON

#include <boost/python.hpp>
#include <Python.h>

using namespace boost::python;
using namespace ledger;

int py_amount_quantity(amount_t& amount)
{
  std::string quant = amount.quantity_string();
  return std::atol(quant.c_str());
}

void py_parse_1(amount_t& amount, const std::string& str,
		unsigned char flags) {
  amount.parse(str, flags);
}
void py_parse_2(amount_t& amount, const std::string& str) {
  amount.parse(str);
}

struct commodity_updater_wrap : public commodity_base_t::updater_t
{
  PyObject * self;
  commodity_updater_wrap(PyObject * self_) : self(self_) {}

  virtual void operator()(commodity_base_t& commodity,
			  const datetime_t& moment,
			  const datetime_t& date,
			  const datetime_t& last,
			  amount_t&         price) {
    call_method<void>(self, "__call__", commodity, moment, date, last, price);
  }
};

commodity_t * py_find_commodity(const std::string& symbol)
{
  return commodity_t::find(symbol);
}

#define EXC_TRANSLATOR(type)				\
  void exc_translate_ ## type(const type& err) {	\
    PyErr_SetString(PyExc_RuntimeError, err.what());	\
  }

EXC_TRANSLATOR(amount_error)

void export_amount()
{
  scope().attr("AMOUNT_PARSE_NO_MIGRATE") = AMOUNT_PARSE_NO_MIGRATE;
  scope().attr("AMOUNT_PARSE_NO_REDUCE")  = AMOUNT_PARSE_NO_REDUCE;

  class_< amount_t > ("Amount")
    .def(init<amount_t>())
    .def(init<std::string>())
    .def(init<char *>())
    .def(init<bool>())
    .def(init<long>())
    .def(init<unsigned long>())
    .def(init<double>())

    .def(self += self)
    .def(self += long())
    .def(self +  self)
    .def(self +  long())
    .def(self -= self)
    .def(self -= long())
    .def(self -  self)
    .def(self -  long())
    .def(self *= self)
    .def(self *= long())
    .def(self *  self)
    .def(self *  long())
    .def(self /= self)
    .def(self /= long())
    .def(self /  self)
    .def(self /  long())
    .def(- self)

    .def(self <  self)
    .def(self <  long())
    .def(self <= self)
    .def(self <= long())
    .def(self >  self)
    .def(self >  long())
    .def(self >= self)
    .def(self >= long())
    .def(self == self)
    .def(self == long())
    .def(self != self)
    .def(self != long())
    .def(! self)

    .def(self_ns::int_(self))
    .def(self_ns::float_(self))
    .def(self_ns::str(self))
    .def(abs(self))

    .add_property("commodity",
		  make_function(&amount_t::commodity,
				return_value_policy<reference_existing_object>()),
		  make_function(&amount_t::set_commodity,
				with_custodian_and_ward<1, 2>()))

    .def("strip_annotations", &amount_t::strip_annotations)

    .def("negate", &amount_t::negate)
    .def("negated", &amount_t::negated)
    .def("parse", py_parse_1)
    .def("parse", py_parse_2)
    .def("reduce", &amount_t::reduce)

    .def("valid", &amount_t::valid)
    ;

  class_< commodity_base_t::updater_t, commodity_updater_wrap,
	  boost::noncopyable >
    ("Updater")
    ;

  scope().attr("COMMODITY_STYLE_DEFAULTS")  = COMMODITY_STYLE_DEFAULTS;
  scope().attr("COMMODITY_STYLE_SUFFIXED")  = COMMODITY_STYLE_SUFFIXED;
  scope().attr("COMMODITY_STYLE_SEPARATED") = COMMODITY_STYLE_SEPARATED;
  scope().attr("COMMODITY_STYLE_EUROPEAN")  = COMMODITY_STYLE_EUROPEAN;
  scope().attr("COMMODITY_STYLE_THOUSANDS") = COMMODITY_STYLE_THOUSANDS;
  scope().attr("COMMODITY_STYLE_NOMARKET")  = COMMODITY_STYLE_NOMARKET;
  scope().attr("COMMODITY_STYLE_BUILTIN")   = COMMODITY_STYLE_BUILTIN;

  class_< commodity_t > ("Commodity")
    .add_property("symbol", &commodity_t::symbol)

#if 0
    .add_property("name", &commodity_t::name, &commodity_t::set_name)
    .add_property("note", &commodity_t::note, &commodity_t::set_note)
    .add_property("precision", &commodity_t::precision,
		  &commodity_t::set_precision)
    .add_property("flags", &commodity_t::flags, &commodity_t::set_flags)
    .add_property("add_flags", &commodity_t::add_flags)
    .add_property("drop_flags", &commodity_t::drop_flags)
#if 0
    .add_property("updater", &commodity_t::updater)
#endif

    .add_property("smaller",
		  make_getter(&commodity_t::smaller,
			      return_value_policy<reference_existing_object>()),
		  make_setter(&commodity_t::smaller,
			      return_value_policy<reference_existing_object>()))
    .add_property("larger",
		  make_getter(&commodity_t::larger,
			      return_value_policy<reference_existing_object>()),
		  make_setter(&commodity_t::larger,
			      return_value_policy<reference_existing_object>()))

    .def(self_ns::str(self))

    .def("find", py_find_commodity,
	 return_value_policy<reference_existing_object>())
    .staticmethod("find")
#endif

    .def("add_price", &commodity_t::add_price)
    .def("remove_price", &commodity_t::remove_price)
    .def("value", &commodity_t::value)

    .def("valid", &commodity_t::valid)
    ;

#define EXC_TRANSLATE(type)					\
  register_exception_translator<type>(&exc_translate_ ## type);

  EXC_TRANSLATE(amount_error);
}

#endif // USE_BOOST_PYTHON
