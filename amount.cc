#include "ledger.h"

#include "gmp.h"

#define MAX_PRECISION 10

#define MPZ(x) ((MP_INT *)(x))

#define INIT() if (! quantity) _init()

namespace ledger {

static mpz_t full_divisor;
static mpz_t true_value;

static class init_amounts
{
 public:
  init_amounts() {
    mpz_init(full_divisor);
    mpz_init(true_value);
    mpz_ui_pow_ui(full_divisor, 10, MAX_PRECISION);
    mpz_mul_ui(true_value, full_divisor, 1);
  }
  ~init_amounts() {
    mpz_clear(full_divisor);
    mpz_clear(true_value);
  }
} initializer;

static void mpz_round(mpz_t out, mpz_t value, int precision)
{
  mpz_t divisor;

  mpz_t quotient;
  mpz_t remainder;

  mpz_init(divisor);
  mpz_init(quotient);
  mpz_init(remainder);

  mpz_ui_pow_ui(divisor, 10, MAX_PRECISION - precision);
  mpz_tdiv_qr(quotient, remainder, value, divisor);
  mpz_ui_pow_ui(divisor, 10, MAX_PRECISION - precision - 1);
  mpz_mul_ui(divisor, divisor, 5);

  if (mpz_sgn(remainder) < 0) {
    mpz_ui_sub(divisor, 0, divisor);

    if (mpz_cmp(remainder, divisor) < 0) {
      mpz_ui_pow_ui(divisor, 10, MAX_PRECISION - precision);
      mpz_add(remainder, divisor, remainder);
      mpz_ui_sub(remainder, 0, remainder);
      mpz_add(out, value, remainder);
    } else {
      mpz_sub(out, value, remainder);
    }
  } else {
    if (mpz_cmp(remainder, divisor) >= 0) {
      mpz_ui_pow_ui(divisor, 10, MAX_PRECISION - precision);
      mpz_sub(remainder, divisor, remainder);
      mpz_add(out, value, remainder);
    } else {
      mpz_sub(out, value, remainder);
    }
  }

  mpz_clear(divisor);
  mpz_clear(quotient);
  mpz_clear(remainder);
}

amount_t::amount_t(const bool value)
  : quantity(NULL), commodity(NULL)
{
  if (value) {
    commodity = commodity_t::null_commodity;
    quantity = new MP_INT;
    mpz_init_set(MPZ(quantity), true_value);
  }
}

amount_t::amount_t(const int value)
  : quantity(NULL), commodity(NULL)
{
  if (value != 0) {
    _init();
    commodity = commodity_t::null_commodity;
    mpz_set_si(MPZ(quantity), value);
    mpz_mul(MPZ(quantity), MPZ(quantity), full_divisor);
  }
}

amount_t::amount_t(const unsigned int value)
  : quantity(NULL), commodity(NULL)
{
  if (value != 0) {
    _init();
    commodity = commodity_t::null_commodity;
    mpz_set_ui(MPZ(quantity), value);
    mpz_mul(MPZ(quantity), MPZ(quantity), full_divisor);
  }
}

amount_t::amount_t(const double value)
  : quantity(NULL), commodity(NULL)
{
  if (value != 0.0) {
    _init();
    commodity = commodity_t::null_commodity;
    mpz_set_d(MPZ(quantity), value);
    mpz_mul(MPZ(quantity), MPZ(quantity), full_divisor);
  }
}

void amount_t::_clear()
{
  mpz_clear(MPZ(quantity));
  delete (MP_INT *) quantity;
}

void amount_t::_init()
{
  quantity = new MP_INT;
  mpz_init(MPZ(quantity));
}

void amount_t::_copy(const amount_t& amt)
{
  if (quantity) {
    mpz_set(MPZ(quantity), MPZ(amt.quantity));
  } else {
    quantity = new MP_INT;
    mpz_init_set(MPZ(quantity), MPZ(amt.quantity));
  }
  commodity = amt.commodity;
  assert(commodity);
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
  return *this;
}

amount_t& amount_t::operator=(const bool value)
{
  if (! value) {
    if (quantity) {
      _clear();
      quantity  = NULL;
      commodity = NULL;
    }
  } else {
    commodity = commodity_t::null_commodity;
    quantity = new MP_INT;
    mpz_init_set(MPZ(quantity), true_value);
  }
  return *this;
}

amount_t& amount_t::operator=(const int value)
{
  if (value == 0) {
    if (quantity) {
      _clear();
      quantity  = NULL;
      commodity = NULL;
    }
  } else {
    commodity = commodity_t::null_commodity;
    mpz_set_si(MPZ(quantity), value);
    mpz_mul(MPZ(quantity), MPZ(quantity), full_divisor);
  }
  return *this;
}

amount_t& amount_t::operator=(const unsigned int value)
{
  if (value == 0) {
    if (quantity) {
      _clear();
      quantity  = NULL;
      commodity = NULL;
    }
  } else {
    commodity = commodity_t::null_commodity;
    mpz_set_ui(MPZ(quantity), value);
    mpz_mul(MPZ(quantity), MPZ(quantity), full_divisor);
  }
  return *this;
}

amount_t& amount_t::operator=(const double value)
{
  if (value == 0.0) {
    if (quantity) {
      _clear();
      quantity  = NULL;
      commodity = NULL;
    }
  } else {
    commodity = commodity_t::null_commodity;
    mpz_set_d(MPZ(quantity), value);
    mpz_mul(MPZ(quantity), MPZ(quantity), full_divisor);
  }
  return *this;
}


amount_t& amount_t::operator+=(const amount_t& amt)
{
  if (amt.quantity) {
    assert(! commodity || commodity == amt.commodity);
    INIT();
    mpz_add(MPZ(quantity), MPZ(quantity), MPZ(amt.quantity));
  }
  return *this;
}

amount_t& amount_t::operator-=(const amount_t& amt)
{
  if (amt.quantity) {
    assert(commodity == amt.commodity);
    INIT();
    mpz_sub(MPZ(quantity), MPZ(quantity), MPZ(amt.quantity));
  }
  return *this;
}

// unary negation
amount_t& amount_t::negate()
{
  if (quantity)
    mpz_ui_sub(MPZ(quantity), 0, MPZ(quantity));
  return *this;
}

// comparisons to zero
bool amount_t::operator<(const int num) const
{
  if (num == 0) {
    return quantity ? mpz_sgn(MPZ(quantity)) < 0 : false;
  } else {
    std::string str;
    std::ostringstream strstr(str);
    strstr << num;
    amount_t amt(strstr.str());
    return *this < amt;
  }
}

bool amount_t::operator<=(const int num) const
{
  if (num == 0) {
    return quantity ? mpz_sgn(MPZ(quantity)) <= 0 : true;
  } else {
    std::string str;
    std::ostringstream strstr(str);
    strstr << num;
    amount_t amt(strstr.str());
    return *this <= amt;
  }
}

bool amount_t::operator>(const int num) const
{
  if (num == 0) {
    return quantity ? mpz_sgn(MPZ(quantity)) > 0 : false;
  } else {
    std::string str;
    std::ostringstream strstr(str);
    strstr << num;
    amount_t amt(strstr.str());
    return *this > amt;
  }
}

bool amount_t::operator>=(const int num) const
{
  if (num == 0) {
    return quantity ? mpz_sgn(MPZ(quantity)) >= 0 : true;
  } else {
    std::string str;
    std::ostringstream strstr(str);
    strstr << num;
    amount_t amt(strstr.str());
    return *this >= amt;
  }
}

// comparisons between amounts
bool amount_t::operator<(const amount_t& amt) const
{
  if (! quantity)		// equivalent to zero
    return amt > 0;
  if (! amt.quantity)		// equivalent to zero
    return *this < 0;
  assert(commodity == amt.commodity);
  return mpz_cmp(MPZ(quantity), MPZ(amt.quantity)) < 0;
}

bool amount_t::operator<=(const amount_t& amt) const
{
  if (! quantity)		// equivalent to zero
    return amt >= 0;
  if (! amt.quantity)		// equivalent to zero
    return *this <= 0;
  assert(commodity == amt.commodity);
  return mpz_cmp(MPZ(quantity), MPZ(amt.quantity)) <= 0;
}

bool amount_t::operator>(const amount_t& amt) const
{
  if (! quantity)		// equivalent to zero
    return amt < 0;
  if (! amt.quantity)		// equivalent to zero
    return *this > 0;
  assert(commodity == amt.commodity);
  return mpz_cmp(MPZ(quantity), MPZ(amt.quantity)) > 0;
}

bool amount_t::operator>=(const amount_t& amt) const
{
  if (! quantity)		// equivalent to zero
    return amt <= 0;
  if (! amt.quantity)		// equivalent to zero
    return *this >= 0;
  assert(commodity == amt.commodity);
  return mpz_cmp(MPZ(quantity), MPZ(amt.quantity)) >= 0;
}

bool amount_t::operator==(const amount_t& amt) const
{
  if (commodity != amt.commodity)
    return false;
  assert(amt.quantity);
  assert(quantity);
  return mpz_cmp(MPZ(quantity), MPZ(amt.quantity)) == 0;
}

amount_t::operator bool() const
{
  if (quantity) {
    assert(commodity);
    mpz_t temp;
    mpz_t divisor;
    mpz_init_set(temp, MPZ(quantity));
    mpz_init(divisor);
    mpz_ui_pow_ui(divisor, 10, MAX_PRECISION - commodity->precision);
    mpz_tdiv_q(temp, temp, divisor);
    bool zero = mpz_sgn(temp) == 0;
    mpz_clear(divisor);
    mpz_clear(temp);
    return ! zero;
  } else {
    return false;
  }
}

amount_t amount_t::value(const std::time_t moment) const
{
  if (quantity && ! (commodity->flags & COMMODITY_STYLE_NOMARKET))
    if (amount_t amt = commodity->value(moment))
      return (amt * *this).round();

  return *this;
}

amount_t& amount_t::operator*=(const amount_t& amt)
{
  if (! amt.quantity)
    return *this;

  INIT();

  mpz_mul(MPZ(quantity), MPZ(quantity), MPZ(amt.quantity));

  // Truncate to the recorded precision: MAX_PRECISION.
  mpz_t divisor;
  mpz_init(divisor);
  mpz_ui_pow_ui(divisor, 10, MAX_PRECISION);
  mpz_tdiv_q(MPZ(quantity), MPZ(quantity), divisor);
  mpz_clear(divisor);

  return *this;
}

amount_t& amount_t::operator/=(const amount_t& amt)
{
  if (! amt.quantity)
    return *this;

  INIT();

  mpz_t divisor;
  mpz_init(divisor);
  mpz_ui_pow_ui(divisor, 10, MAX_PRECISION);

  mpz_mul(MPZ(quantity), MPZ(quantity), divisor);
  mpz_tdiv_q(MPZ(quantity), MPZ(quantity), MPZ(amt.quantity));

  mpz_clear(divisor);

  return *this;
}

amount_t& amount_t::operator%=(const amount_t& amt)
{
  if (! amt.quantity)
    return *this;

  INIT();

  mpz_t divisor;
  mpz_init(divisor);
  mpz_ui_pow_ui(divisor, 10, MAX_PRECISION);

  mpz_mul(MPZ(quantity), MPZ(quantity), divisor);
  mpz_tdiv_r(MPZ(quantity), MPZ(quantity), MPZ(amt.quantity));

  mpz_clear(divisor);

  return *this;
}

amount_t amount_t::round(int precision) const
{
  if (! quantity) {
    return *this;
  } else {
    amount_t temp = *this;
    mpz_round(MPZ(temp.quantity), MPZ(temp.quantity),
	      precision == -1 ? commodity->precision : precision);
    return temp;
  }
}

std::ostream& operator<<(std::ostream& out, const amount_t& amt)
{
  mpz_t quotient;
  mpz_t rquotient;
  mpz_t remainder;
  mpz_t divisor;

  if (! amt.quantity)
    return out;

  mpz_init(quotient);
  mpz_init(rquotient);
  mpz_init(remainder);
  mpz_init(divisor);

  bool negative = false;

  // Ensure the value is rounded to the commodity's precision before
  // outputting it.  NOTE: `rquotient' is used here as a temp variable!

  if (amt.commodity->precision != MAX_PRECISION)
    mpz_round(rquotient, MPZ(amt.quantity), amt.commodity->precision);

  mpz_ui_pow_ui(divisor, 10, MAX_PRECISION);
  mpz_tdiv_qr(quotient, remainder, rquotient, divisor);

  if (mpz_sgn(quotient) < 0 || mpz_sgn(remainder) < 0)
    negative = true;

  mpz_abs(quotient, quotient);
  mpz_abs(remainder, remainder);

  if (amt.commodity->precision == MAX_PRECISION) {
    mpz_set(rquotient, remainder);
  } else {
    assert(MAX_PRECISION - amt.commodity->precision > 0);
    mpz_ui_pow_ui(divisor, 10, MAX_PRECISION - amt.commodity->precision);
    mpz_tdiv_qr(rquotient, remainder, remainder, divisor);
  }

  bool odd_chars_in_symbol = false;

  for (const char * p = amt.commodity->symbol.c_str();
       *p;
       p++)
    if (std::isspace(*p) || std::isdigit(*p) || *p == '-' || *p == '.') {
      odd_chars_in_symbol = true;
      break;
    }

  if (! (amt.commodity->flags & COMMODITY_STYLE_SUFFIXED)) {
    if (odd_chars_in_symbol)
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
    out << quotient;
  }
  else {
    bool printed = false;

    mpz_t temp;
    mpz_init(temp);

    // jww (2003-09-29): use a smarter starting value for `powers'
    for (int powers = 15; powers >= 0; powers -= 3) {
      mpz_ui_pow_ui(divisor, 10, powers);
      mpz_tdiv_q(temp, quotient, divisor);

      if (mpz_sgn(temp) == 0)
	continue;

      mpz_ui_pow_ui(divisor, 10, 3);
      mpz_tdiv_r(temp, temp, divisor);

      if (printed) {
	out.width(3);
	out.fill('0');
      }
      out << temp;

      if (powers > 0) {
	out << ((amt.commodity->flags & COMMODITY_STYLE_EUROPEAN) ? '.' : ',');
	printed = true;
      }
    }

    mpz_clear(temp);
  }

  out << ((amt.commodity->flags & COMMODITY_STYLE_EUROPEAN) ? ',' : '.');

  out.width(amt.commodity->precision);
  out.fill('0');
  out << rquotient;

  if (amt.commodity->flags & COMMODITY_STYLE_SUFFIXED) {
    if (amt.commodity->flags & COMMODITY_STYLE_SEPARATED)
      out << " ";
    if (odd_chars_in_symbol)
      out << "\"" << amt.commodity->symbol << "\"";
    else
      out << amt.commodity->symbol;
  }

  mpz_clear(quotient);
  mpz_clear(rquotient);
  mpz_clear(remainder);
  mpz_clear(divisor);

  return out;
}

amount_t::operator std::string() const
{
  std::ostringstream s;
  s << *this;
  return s.str();
}

static inline char peek_next_nonws(std::istream& in)
{
  char c = in.peek();
  while (! in.eof() && std::isspace(c)) {
    in.get(c);
    c = in.peek();
  }
  return c;
}

void parse_quantity(std::istream& in, std::string& value)
{
  char c = peek_next_nonws(in);
  while (std::isdigit(c) || c == '-' || c == '.' || c == ',') {
    in.get(c);
    if (in.eof())
      break;
    value += c;
    c = in.peek();
  }
}

void parse_commodity(std::istream& in, std::string& symbol)
{
  char c = peek_next_nonws(in);
  if (c == '"') {
    in.get(c);
    c = in.peek();
    while (! in.eof() && c != '"') {
      in.get(c);
      if (c == '\\')
	in.get(c);
      symbol += c;
      c = in.peek();
    }

    if (c == '"')
      in.get(c);
    else
      assert(0);
  } else {
    while (! std::isspace(c) && ! std::isdigit(c) && c != '-' && c != '.') {
      in.get(c);
      if (in.eof())
	break;
      symbol += c;
      c = in.peek();
    }
  }
}

void amount_t::parse(std::istream& in)
{
  // The possible syntax for an amount is:
  //
  //   [-]NUM[ ]SYM [@ AMOUNT]
  //   SYM[ ][-]NUM [@ AMOUNT]

  std::string   symbol;
  std::string   quant;
  unsigned int	flags	  = COMMODITY_STYLE_DEFAULTS;;
  unsigned int	precision = MAX_PRECISION;

  if (! quantity)
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
  else {
    precision = 0;
    quant = quant + ".0";
  }
  assert(precision <= MAX_PRECISION);

  // Create the commodity if has not already been seen.
  commodity = commodity_t::find_commodity(symbol, true);
  commodity->flags |= flags;
  if (precision > commodity->precision)
    commodity->precision = precision;

  // The number is specified as the user desires, with the commodity
  // flags telling how to parse it.

  int	 len	 = quant.length();
  int	 buf_len = len + MAX_PRECISION;
  char * buf	 = new char[buf_len];

  std::memset(buf, '0', buf_len - 1);
  std::strncpy(buf, quant.c_str(), len);

  if (flags & COMMODITY_STYLE_THOUSANDS)
    while (char * t =
	   std::strchr(buf, flags & COMMODITY_STYLE_EUROPEAN ? '.' : ','))
      do { *t = *(t + 1); } while (*(t++ + 1));

  char * t = std::strchr(buf, flags & COMMODITY_STYLE_EUROPEAN ? ',' : '.');
  if (! t)
    t = buf + len;

  for (int prec = 0; prec < MAX_PRECISION; prec++) {
    *t = *(t + 1);
    t++;
  }
  *t = '\0';

  mpz_set_str(MPZ(quantity), buf, 10);

  delete[] buf;
}

static char buf[4096];

void amount_t::write_quantity(std::ostream& out) const
{
  unsigned short len;
  if (quantity) {
    mpz_get_str(buf, 10, MPZ(quantity));
    len = std::strlen(buf);
    assert(len);
    out.write((char *)&len, sizeof(len));
    out.write(buf, len);
  } else {
    len = 0;
    out.write((char *)&len, sizeof(len));
  }
}

void amount_t::read_quantity(std::istream& in)
{
  unsigned short len;
  in.read((char *)&len, sizeof(len));
  if (len) {
    in.read(buf, len);
    buf[len] = '\0';
    if (! quantity)
      _init();
    mpz_set_str(MPZ(quantity), buf, 10);
  } else {
    if (quantity)
      _clear();
    quantity = NULL;
  }
}

void (*commodity_t::updater)(commodity_t *     commodity,
			     const std::time_t date,
			     const amount_t&   price,
			     const std::time_t moment) = NULL;

commodities_map commodity_t::commodities;
commodity_t *   commodity_t::null_commodity =
		     commodity_t::find_commodity("", true);

struct cleanup_commodities
{
  ~cleanup_commodities() {
    for (commodities_map::iterator i
	   = commodity_t::commodities.begin();
	 i != commodity_t::commodities.end();
	 i++)
      delete (*i).second;
  }
};

static cleanup_commodities cleanup;

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

  if (updater)
    updater(this, age, price, moment);

  for (history_map::reverse_iterator i = history.rbegin();
       i != history.rend();
       i++)
    if (moment == 0 || std::difftime(moment, (*i).first) >= 0) {
      age   = (*i).first;
      price = (*i).second;
      break;
    }

  return price;
}

} // namespace ledger
