#include "ledger.h"
#include "binary.h"
#include "error.h"
#include "util.h"
#include "debug.h"

#include <deque>
#include <sstream>

#include "gmp.h"

namespace ledger {

#define BIGINT_BULK_ALLOC 0x0001

class amount_t::bigint_t {
 public:
  mpz_t		 val;
  unsigned short prec;
  unsigned short flags;
  unsigned int	 ref;
  unsigned int	 index;

  bigint_t() : prec(0), flags(0), ref(1), index(0) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor amount_t::bigint_t");
    mpz_init(val);
  }
  bigint_t(mpz_t _val) : prec(0), flags(0), ref(1), index(0) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor amount_t::bigint_t");
    mpz_init_set(val, _val);
  }
  bigint_t(const bigint_t& other)
    : prec(other.prec), flags(0), ref(1), index(0) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor amount_t::bigint_t");
    mpz_init_set(val, other.val);
  }
  ~bigint_t() {
    DEBUG_PRINT("ledger.memory.dtors", "dtor amount_t::bigint_t");
    assert(ref == 0);
    mpz_clear(val);
  }
};

unsigned int sizeof_bigint_t() {
  return sizeof(amount_t::bigint_t);
}

#define MPZ(x) ((x)->val)

static mpz_t temp;
static mpz_t divisor;
static mpz_t true_value;

commodity_t::updater_t * commodity_t::updater;
commodities_map		 commodity_t::commodities;
commodity_t *            commodity_t::null_commodity;

void initialize_amounts()
{
  mpz_init(temp);
  mpz_init(divisor);
  mpz_init(true_value);
  mpz_set_ui(true_value, 1);

  commodity_t::updater	      = NULL;
  commodity_t::null_commodity = commodity_t::find_commodity("", true);
}

void shutdown_amounts()
{
  mpz_clear(true_value);
  mpz_clear(divisor);
  mpz_clear(temp);

  if (commodity_t::updater)
    delete commodity_t::updater;

  for (commodities_map::iterator i = commodity_t::commodities.begin();
       i != commodity_t::commodities.end();
       i++)
    delete (*i).second;

  commodity_t::commodities.clear();
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

  // chop off the rounded bits
  mpz_ui_pow_ui(divisor, 10, value_prec - round_prec);
  mpz_tdiv_q(out, out, divisor);

  mpz_clear(quotient);
  mpz_clear(remainder);
}

amount_t::amount_t(const bool value)
{
  DEBUG_PRINT("ledger.memory.ctors", "ctor amount_t");
  if (value) {
    quantity  = new bigint_t(true_value);
    commodity = commodity_t::null_commodity;
  } else {
    quantity  = NULL;
    commodity = NULL;
  }
}

amount_t::amount_t(const int value)
{
  DEBUG_PRINT("ledger.memory.ctors", "ctor amount_t");
  if (value != 0) {
    quantity = new bigint_t;
    mpz_set_si(MPZ(quantity), value);
    commodity = commodity_t::null_commodity;
  } else {
    quantity  = NULL;
    commodity = NULL;
  }
}

amount_t::amount_t(const unsigned int value)
{
  DEBUG_PRINT("ledger.memory.ctors", "ctor amount_t");
  if (value != 0) {
    quantity = new bigint_t;
    mpz_set_ui(MPZ(quantity), value);
    commodity = commodity_t::null_commodity;
  } else {
    quantity  = NULL;
    commodity = NULL;
  }
}

amount_t::amount_t(const double value)
{
  DEBUG_PRINT("ledger.memory.ctors", "ctor amount_t");
  if (value != 0.0) {
    quantity = new bigint_t;
    mpz_set_d(MPZ(quantity), value);
    // jww (2004-08-20): How do I calculate this?
    commodity = commodity_t::null_commodity;
  } else {
    quantity  = NULL;
    commodity = NULL;
  }
}

void amount_t::_release()
{
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

    quantity = amt.quantity;
    quantity->ref++;
  }
  commodity = amt.commodity;
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
  if (amt.quantity)
    _copy(amt);
  else if (quantity)
    _clear();

  return *this;
}

amount_t& amount_t::operator=(const bool value)
{
  if (! value) {
    if (quantity)
      _clear();
  } else {
    commodity = commodity_t::null_commodity;
    _init();
    mpz_set(MPZ(quantity), true_value);
  }
  return *this;
}

amount_t& amount_t::operator=(const int value)
{
  if (value == 0) {
    if (quantity)
      _clear();
  } else {
    commodity = commodity_t::null_commodity;
    _init();
    mpz_set_si(MPZ(quantity), value);
  }
  return *this;
}

amount_t& amount_t::operator=(const unsigned int value)
{
  if (value == 0) {
    if (quantity)
      _clear();
  } else {
    commodity = commodity_t::null_commodity;
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
    commodity = commodity_t::null_commodity;
    _init();
    // jww (2004-08-20): How do I calculate precision?
    mpz_set_d(MPZ(quantity), value);
  }
  return *this;
}


void amount_t::_resize(unsigned int prec)
{
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
    quantity  = new bigint_t(*amt.quantity);
    commodity = amt.commodity;
    return *this;
  }

  _dup();

  if (commodity != amt.commodity)
    throw amount_error("Adding amounts with different commodities");

  if (quantity->prec == amt.quantity->prec) {
    mpz_add(MPZ(quantity), MPZ(quantity), MPZ(amt.quantity));
  }
  else if (quantity->prec < amt.quantity->prec) {
    _resize(amt.quantity->prec);
    mpz_add(MPZ(quantity), MPZ(quantity), MPZ(amt.quantity));
  } else {
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
    mpz_neg(MPZ(quantity), MPZ(quantity));
    commodity = amt.commodity;
    return *this;
  }

  _dup();

  if (commodity != amt.commodity)
    throw amount_error("Subtracting amounts with different commodities");

  if (quantity->prec == amt.quantity->prec) {
    mpz_sub(MPZ(quantity), MPZ(quantity), MPZ(amt.quantity));
  }
  else if (quantity->prec < amt.quantity->prec) {
    _resize(amt.quantity->prec);
    mpz_sub(MPZ(quantity), MPZ(quantity), MPZ(amt.quantity));
  } else {
    amount_t temp = amt;
    temp._resize(quantity->prec);
    mpz_sub(MPZ(quantity), MPZ(quantity), MPZ(temp.quantity));
  }

  return *this;
}

// unary negation
amount_t& amount_t::negate()
{
  if (quantity) {
    _dup();
    mpz_ui_sub(MPZ(quantity), 0, MPZ(quantity));
  }
  return *this;
}

// integer comparisons
template <typename T>
static inline void parse_num(amount_t& amt, T num) {
  std::string str;
  { std::ostringstream strstr(str);
    strstr << num;
  }
  { std::istringstream strstr(str);
    amt.parse(strstr);
  }
}

bool amount_t::operator<(const int num) const
{
  if (num == 0) {
    return quantity ? mpz_sgn(MPZ(quantity)) < 0 : false;
  } else {
    amount_t amt;
    parse_num(amt, num);
    return *this < amt;
  }
}

bool amount_t::operator<=(const int num) const
{
  if (num == 0) {
    return quantity ? mpz_sgn(MPZ(quantity)) <= 0 : true;
  } else {
    amount_t amt;
    parse_num(amt, num);
    return *this <= amt;
  }
}

bool amount_t::operator>(const int num) const
{
  if (num == 0) {
    return quantity ? mpz_sgn(MPZ(quantity)) > 0 : false;
  } else {
    amount_t amt;
    parse_num(amt, num);
    return *this > amt;
  }
}

bool amount_t::operator>=(const int num) const
{
  if (num == 0) {
    return quantity ? mpz_sgn(MPZ(quantity)) >= 0 : true;
  } else {
    amount_t amt;
    parse_num(amt, num);
    return *this >= amt;
  }
}

bool amount_t::operator<(const unsigned int num) const
{
  if (num == 0) {
    return quantity ? mpz_sgn(MPZ(quantity)) < 0 : false;
  } else {
    amount_t amt;
    parse_num(amt, num);
    return *this < amt;
  }
}

bool amount_t::operator<=(const unsigned int num) const
{
  if (num == 0) {
    return quantity ? mpz_sgn(MPZ(quantity)) <= 0 : true;
  } else {
    amount_t amt;
    parse_num(amt, num);
    return *this <= amt;
  }
}

bool amount_t::operator>(const unsigned int num) const
{
  if (num == 0) {
    return quantity ? mpz_sgn(MPZ(quantity)) > 0 : false;
  } else {
    amount_t amt;
    parse_num(amt, num);
    return *this > amt;
  }
}

bool amount_t::operator>=(const unsigned int num) const
{
  if (num == 0) {
    return quantity ? mpz_sgn(MPZ(quantity)) >= 0 : true;
  } else {
    amount_t amt;
    parse_num(amt, num);
    return *this >= amt;
  }
}

bool amount_t::operator==(const unsigned int num) const
{
  if (num == 0) {
    return quantity ? mpz_sgn(MPZ(quantity)) == 0 : true;
  } else {
    amount_t amt;
    parse_num(amt, num);
    return *this == amt;
  }
}

// comparisons between amounts
#define DEF_CMP_OPERATOR(OP)						\
bool amount_t::operator OP(const amount_t& amt) const			\
{									\
  if (! quantity)							\
    return amt > 0;							\
  if (! amt.quantity)							\
    return *this < 0;							\
									\
  if (commodity != amt.commodity)					\
    throw amount_error("Comparing amounts with different commodities");	\
									\
  if (quantity->prec == amt.quantity->prec) {				\
    return mpz_cmp(MPZ(quantity), MPZ(amt.quantity)) OP 0;		\
  }									\
  else if (quantity->prec < amt.quantity->prec) {			\
    amount_t temp = *this;						\
    temp._resize(amt.quantity->prec);					\
    return mpz_cmp(MPZ(temp.quantity), MPZ(amt.quantity)) OP 0;		\
  }									\
  else {								\
    amount_t temp = amt;						\
    temp._resize(quantity->prec);					\
    return mpz_cmp(MPZ(quantity), MPZ(temp.quantity)) OP 0;		\
  }									\
}

DEF_CMP_OPERATOR(<)
DEF_CMP_OPERATOR(<=)
DEF_CMP_OPERATOR(>)
DEF_CMP_OPERATOR(>=)
DEF_CMP_OPERATOR(==)

amount_t::operator bool() const
{
  if (quantity) {
    if (quantity->prec <= commodity->precision) {
      return mpz_sgn(MPZ(quantity)) != 0;
    } else {
      assert(commodity);
      mpz_set(temp, MPZ(quantity));
      mpz_ui_pow_ui(divisor, 10, quantity->prec - commodity->precision);
      mpz_tdiv_q(temp, temp, divisor);
      bool zero = mpz_sgn(temp) == 0;
      return ! zero;
    }
  } else {
    return false;
  }
}

amount_t amount_t::value(const std::time_t moment) const
{
  if (quantity && ! (commodity->flags & COMMODITY_STYLE_NOMARKET))
    if (amount_t amt = commodity->value(moment))
      return (amt * *this).round(commodity->precision);

  return *this;
}

amount_t& amount_t::operator*=(const amount_t& amt)
{
  if (! amt.quantity || ! quantity)
    return *this;

  _dup();

  mpz_mul(MPZ(quantity), MPZ(quantity), MPZ(amt.quantity));
  quantity->prec += amt.quantity->prec;

  if (quantity->prec > commodity->precision + 6U) {
    mpz_round(MPZ(quantity), MPZ(quantity),
	      quantity->prec, commodity->precision + 6U);
    quantity->prec = commodity->precision + 6U;
  }

  return *this;
}

amount_t& amount_t::operator/=(const amount_t& amt)
{
  if (! quantity)
    return *this;
  if (! amt.quantity)
    throw amount_error("Divide by zero");

  _dup();

  // Increase the value's precision, to capture fractional parts after
  // the divide.
  mpz_ui_pow_ui(divisor, 10, amt.quantity->prec + 6);
  mpz_mul(MPZ(quantity), MPZ(quantity), divisor);
  mpz_tdiv_q(MPZ(quantity), MPZ(quantity), MPZ(amt.quantity));
  quantity->prec += 6;

  if (quantity->prec > commodity->precision + 6U) {
    mpz_round(MPZ(quantity), MPZ(quantity),
	      quantity->prec, commodity->precision + 6U);
    quantity->prec = commodity->precision + 6U;
  }

  return *this;
}

amount_t amount_t::round(unsigned int prec) const
{
  if (! quantity || quantity->prec <= prec) {
    return *this;
  } else {
    amount_t temp = *this;
    temp._dup();
    mpz_round(MPZ(temp.quantity), MPZ(temp.quantity),
	      temp.quantity->prec, prec);
    temp.quantity->prec = prec;
    return temp;
  }
}

std::ostream& operator<<(std::ostream& _out, const amount_t& amt)
{
  if (! amt.quantity)
    return _out;

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

  if (amt.commodity->precision < amt.quantity->prec) {
    mpz_round(rquotient, MPZ(amt.quantity), amt.quantity->prec,
	      amt.commodity->precision);
    mpz_ui_pow_ui(divisor, 10, amt.commodity->precision);
    mpz_tdiv_qr(quotient, remainder, rquotient, divisor);
  }
  else if (amt.commodity->precision > amt.quantity->prec) {
    mpz_ui_pow_ui(divisor, 10, amt.commodity->precision - amt.quantity->prec);
    mpz_mul(rquotient, MPZ(amt.quantity), divisor);
    mpz_ui_pow_ui(divisor, 10, amt.commodity->precision);
    mpz_tdiv_qr(quotient, remainder, rquotient, divisor);
  }
  else if (amt.quantity->prec) {
    mpz_ui_pow_ui(divisor, 10, amt.quantity->prec);
    mpz_tdiv_qr(quotient, remainder, MPZ(amt.quantity), divisor);
  }
  else {
    mpz_set(quotient, MPZ(amt.quantity));
    mpz_set_ui(remainder, 0);
  }

  if (mpz_sgn(quotient) < 0 || mpz_sgn(remainder) < 0) {
    negative = true;

    mpz_abs(quotient, quotient);
    mpz_abs(remainder, remainder);
  }
  mpz_set(rquotient, remainder);

  if (! (amt.commodity->flags & COMMODITY_STYLE_SUFFIXED)) {
    if (amt.commodity->quote)
      out << "\"" << amt.commodity->symbol << "\"";
    else
      out << amt.commodity->symbol;
    if (amt.commodity->flags & COMMODITY_STYLE_SEPARATED)
      out << " ";
  }

  if (negative)
    out << "-";

  if (mpz_sgn(quotient) == 0) {
    out << '0';
  }
  else if (! (amt.commodity->flags & COMMODITY_STYLE_THOUSANDS)) {
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
	out << (amt.commodity->flags & COMMODITY_STYLE_EUROPEAN ? '.' : ',');
	out.width(3);
	out.fill('0');
      }
      out << *i;

      printed = true;
    }
  }

  if (amt.commodity->precision) {
    out << ((amt.commodity->flags & COMMODITY_STYLE_EUROPEAN) ? ',' : '.');

    out.width(amt.commodity->precision);
    out.fill('0');

    char * p = mpz_get_str(NULL, 10, rquotient);
    out << p;
    std::free(p);
  }

  if (amt.commodity->flags & COMMODITY_STYLE_SUFFIXED) {
    if (amt.commodity->flags & COMMODITY_STYLE_SEPARATED)
      out << " ";
    if (amt.commodity->quote)
      out << "\"" << amt.commodity->symbol << "\"";
    else
      out << amt.commodity->symbol;
  }

  mpz_clear(quotient);
  mpz_clear(rquotient);
  mpz_clear(remainder);

  // Things are output to a string first, so that if anyone has
  // specified a width or fill for _out, it will be applied to the
  // entire amount string, and not just the first part.

  _out << out.str();

  return _out;
}

void parse_quantity(std::istream& in, std::string& value)
{
  static char buf[256];
  char c = peek_next_nonws(in);
  READ_INTO(in, buf, 256, c,
	    std::isdigit(c) || c == '-' || c == '.' || c == ',');
  value = buf;
}

void parse_commodity(std::istream& in, std::string& symbol)
{
  static char buf[256];

  char c = peek_next_nonws(in);
  if (c == '"') {
    in.get(c);
    READ_INTO(in, buf, 256, c, c != '"');
    if (c == '"')
      in.get(c);
    else
      throw amount_error("Quoted commodity symbol lacks closing quote");
  } else {
    READ_INTO(in, buf, 256, c, ! std::isspace(c) && ! std::isdigit(c) &&
	      c != '-' && c != '.');
  }
  symbol = buf;
}

void amount_t::parse(std::istream& in)
{
  // The possible syntax for an amount is:
  //
  //   [-]NUM[ ]SYM [@ AMOUNT]
  //   SYM[ ][-]NUM [@ AMOUNT]

  std::string  symbol;
  std::string  quant;
  unsigned int flags = COMMODITY_STYLE_DEFAULTS;;

  _init();

  char c = peek_next_nonws(in);
  if (std::isdigit(c) || c == '.' || c == '-') {
    parse_quantity(in, quant);

    char n;
    if (! in.eof() && ((n = in.peek()) != '\n')) {
      if (std::isspace(n))
	flags |= COMMODITY_STYLE_SEPARATED;

      parse_commodity(in, symbol);

      flags |= COMMODITY_STYLE_SUFFIXED;
    }
  } else {
    parse_commodity(in, symbol);

    if (std::isspace(in.peek()))
      flags |= COMMODITY_STYLE_SEPARATED;

    parse_quantity(in, quant);
  }

  std::string::size_type last_comma  = quant.rfind(',');
  std::string::size_type last_period = quant.rfind('.');

  unsigned int precision = 0;

  if (last_comma != std::string::npos && last_period != std::string::npos) {
    flags |= COMMODITY_STYLE_THOUSANDS;
    if (last_comma > last_period) {
      flags |= COMMODITY_STYLE_EUROPEAN;
      precision = quant.length() - last_comma - 1;
    } else {
      precision = quant.length() - last_period - 1;
    }
  }
  else if (last_comma != std::string::npos) {
    flags |= COMMODITY_STYLE_EUROPEAN;
    precision = quant.length() - last_comma - 1;
  }
  else if (last_period != std::string::npos) {
    precision = quant.length() - last_period - 1;
  }

  quantity->prec = precision;

  // Create the commodity if has not already been seen.
  commodity = commodity_t::find_commodity(symbol, true);
  commodity->flags |= flags;
  if (precision > commodity->precision)
    commodity->precision = precision;

  // The number is specified as the user desires, with the commodity
  // flags telling how to parse it.

  int	 len	 = quant.length();
  char * buf	 = new char[len + 1];

  const char * p = quant.c_str();
  char * t       = buf;

  while (*p) {
    if (*p == ',' || *p == '.')
      p++;
    *t++ = *p++;
  }
  *t = '\0';

  mpz_set_str(MPZ(quantity), buf, 10);

  delete[] buf;
}

void amount_t::parse(const std::string& str)
{
  std::istringstream stream(str);
  parse(stream);
}


static char buf[4096];
static int  index = 0;

void amount_t::write_quantity(std::ostream& out) const
{
  char byte;

  if (! quantity) {
    byte = 0;
    out.write(&byte, sizeof(byte));
    return;
  }

  if (quantity->index == 0) {
    quantity->index = ++index;
    bigints_count++;

    byte = 1;
    out.write(&byte, sizeof(byte));

    std::size_t size;
    mpz_export(buf, &size, 1, sizeof(short), 0, 0, MPZ(quantity));
    unsigned short len = size * sizeof(short);
    out.write((char *)&len, sizeof(len));

    if (len) {
      out.write(buf, len);

      byte = mpz_sgn(MPZ(quantity)) < 0 ? 1 : 0;
      out.write(&byte, sizeof(byte));

      out.write((char *)&quantity->prec, sizeof(quantity->prec));
    }
  } else {
    assert(quantity->ref > 1);

    // Since this value has already been written, we simply write
    // out a reference to which one it was.
    byte = 2;
    out.write(&byte, sizeof(byte));
    out.write((char *)&quantity->index, sizeof(quantity->index));
  }
}

void amount_t::read_quantity(std::istream& in)
{
  assert(! quantity);

  char byte;
  in.read(&byte, sizeof(byte));

  if (byte == 0)
    return;

  if (byte == 1) {
    quantity = new(bigints_next++) bigint_t;
    quantity->flags |= BIGINT_BULK_ALLOC;

    unsigned short len;
    in.read((char *)&len, sizeof(len));
    in.read(buf, len);
    mpz_import(MPZ(quantity), len / sizeof(short), 1, sizeof(short), 0, 0, buf);

    char negative;
    in.read(&negative, sizeof(negative));
    if (negative)
      mpz_neg(MPZ(quantity), MPZ(quantity));

    in.read((char *)&quantity->prec, sizeof(quantity->prec));
  } else {
    unsigned int index;
    in.read((char *)&index, sizeof(index));
    quantity = bigints + (index - 1);
    quantity->ref++;
    DEBUG_PRINT("ledger.amount.bigint-show",
		"bigint " << quantity << " ++ref " << quantity->ref);
  }
}


commodity_t * commodity_t::find_commodity(const std::string& symbol,
					  bool auto_create)
{
  commodities_map::const_iterator i = commodities.find(symbol);
  if (i != commodities.end())
    return (*i).second;

  if (auto_create) {
    commodity_t * commodity = new commodity_t(symbol);
    add_commodity(commodity);
    return commodity;
  }

  return NULL;
}

amount_t commodity_t::value(const std::time_t moment)
{
  std::time_t age = 0;
  amount_t    price;

  for (history_map::reverse_iterator i = history.rbegin();
       i != history.rend();
       i++)
    if (moment == 0 || std::difftime(moment, (*i).first) >= 0) {
      age   = (*i).first;
      price = (*i).second;
      break;
    }

  if (updater)
    (*updater)(this, moment, age,
	       history.size() > 0 ? (*history.rbegin()).first : 0, price);

  return price;
}

} // namespace ledger
