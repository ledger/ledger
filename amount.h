#ifndef _AMOUNT_H
#define _AMOUNT_H

#include <map>
#include <string>
#include <ctime>
#include <iostream>
#include <sstream>

namespace ledger {

class commodity_t;

class amount_t
{
  void _init();
  void _copy(const amount_t& amt);
  void _release();
  void _dup();
  void _resize(int prec);

  void _clear() {
    if (quantity)
      _release();
    quantity  = NULL;
    commodity = NULL;
    precision = 0;
  }

 public:
  struct bigint_t;

  bigint_t *	 quantity;	// amount, to MAX_PRECISION
  unsigned short precision;
  commodity_t *	 commodity;

  bool valid() const {
    if (quantity)
      return commodity != NULL;
    else
      return commodity == NULL;
  }

  // constructors
  amount_t(commodity_t * _commodity = NULL)
    : quantity(NULL), precision(0), commodity(_commodity) {}

  amount_t(const amount_t& amt) : quantity(NULL) {
    if (amt.quantity) {
      _copy(amt);
    } else {
      precision = 0;
      commodity = NULL;
    }
  }
  amount_t(const std::string& value) : quantity(NULL) {
    parse(value);
  }
  amount_t(const char * value) : quantity(NULL) {
    parse(value);
  }
  amount_t(const bool value);
  amount_t(const int value);
  amount_t(const unsigned int value);
  amount_t(const double value);

  // destructor
  ~amount_t() {
    if (quantity)
      _release();
  }

  // assignment operator
  amount_t& operator=(const amount_t& amt);
  amount_t& operator=(const std::string& value);
  amount_t& operator=(const char * value);
  amount_t& operator=(const bool value);
  amount_t& operator=(const int value);
  amount_t& operator=(const unsigned int value);
  amount_t& operator=(const double value);

  // general methods
  amount_t round(int prec = -1) const;

  // in-place arithmetic
  amount_t& operator*=(const amount_t& amt);
  amount_t& operator/=(const amount_t& amt);
  amount_t& operator+=(const amount_t& amt);
  amount_t& operator-=(const amount_t& amt);

  // simple arithmetic
  amount_t operator*(const amount_t& amt) const {
    amount_t temp = *this;
    temp *= amt;
    return temp;
  }
  amount_t operator/(const amount_t& amt) const {
    amount_t temp = *this;
    temp /= amt;
    return temp;
  }
  amount_t operator+(const amount_t& amt) const {
    amount_t temp = *this;
    temp += amt;
    return temp;
  }
  amount_t operator-(const amount_t& amt) const {
    amount_t temp = *this;
    temp -= amt;
    return temp;
  }

  // unary negation
  amount_t& negate();
  amount_t negated() const {
    amount_t temp = *this;
    temp.negate();
    return temp;
  }
  amount_t operator-() const {
    return negated();
  }

  // test for non-zero (use ! for zero)
  operator bool() const;

  // integer comparisons
  bool operator<(const int num) const;
  bool operator<=(const int num) const;
  bool operator>(const int num) const;
  bool operator>=(const int num) const;

  bool operator<(const unsigned int num) const;
  bool operator<=(const unsigned int num) const;
  bool operator>(const unsigned int num) const;
  bool operator>=(const unsigned int num) const;
  bool operator==(const unsigned int num) const;
  bool operator!=(const unsigned int num) const {
    return ! (*this == num);
  }

  // comparisons between amounts
  bool operator<(const amount_t& amt) const;
  bool operator<=(const amount_t& amt) const;
  bool operator>(const amount_t& amt) const;
  bool operator>=(const amount_t& amt) const;
  bool operator==(const amount_t& amt) const;
  bool operator!=(const amount_t& amt) const {
    if (commodity != amt.commodity)
      return true;
    return ! (*this == amt);
  }

  amount_t value(const std::time_t moment) const;

  void abs() {
    if (*this < 0)
      negate();
  }

  operator std::string() const;

  void parse(std::istream& in);
  void parse(const std::string& str) {
    std::istringstream stream(str);
    parse(stream);
  }

  void write_quantity(std::ostream& out) const;
  void read_quantity(std::istream& in);

  friend std::istream& operator>>(std::istream& in, amount_t& amt);
};

void parse_quantity(std::istream& in, std::string& value);
void parse_commodity(std::istream& in, std::string& symbol);

inline amount_t abs(const amount_t& amt) {
  return amt < 0 ? amt.negated() : amt;
}

inline std::istream& operator>>(std::istream& in, amount_t& amt) {
  amt.parse(in);
  return in;
}

std::ostream& operator<<(std::ostream& out, const amount_t& amt);


#define COMMODITY_STYLE_DEFAULTS   0x0000
#define COMMODITY_STYLE_SUFFIXED   0x0001
#define COMMODITY_STYLE_SEPARATED  0x0002
#define COMMODITY_STYLE_EUROPEAN   0x0004
#define COMMODITY_STYLE_THOUSANDS  0x0008
#define COMMODITY_STYLE_CONSULTED  0x0010
#define COMMODITY_STYLE_NOMARKET   0x0020

typedef std::map<const std::time_t, amount_t>  history_map;
typedef std::pair<const std::time_t, amount_t> history_pair;

typedef std::map<const std::string, commodity_t *>  commodities_map;
typedef std::pair<const std::string, commodity_t *> commodities_pair;

class commodity_t
{
 public:
  class updater_t {
   public:
    virtual ~updater_t() {}
    virtual void operator()(commodity_t *     commodity,
			    const std::time_t moment,
			    const std::time_t date,
			    amount_t&         price) = 0;
  };

  typedef unsigned long ident_t;

  std::string	 symbol;
  bool		 quote;
  std::string	 name;
  std::string	 note;
  unsigned short precision;
  unsigned short flags;
  history_map	 history;
  amount_t	 conversion;
  ident_t	 ident;

  // If set, this global function pointer is called to determine
  // whether prices have been updated in the meanwhile.

  static updater_t * updater;

  // This map remembers all commodities that have been
  // defined thus far.

  static commodities_map commodities;
  static commodity_t *   null_commodity;

  static void add_commodity(commodity_t * commodity,
			    const std::string symbol = "") {
    commodities.insert(commodities_pair((symbol.empty() ?
					 commodity->symbol : symbol),
					commodity));
  }
  static bool remove_commodity(commodity_t * commodity) {
    commodities_map::size_type n = commodities.erase(commodity->symbol);
    return n > 0;
  }
  static commodity_t * find_commodity(const std::string& symbol,
				      bool auto_create = false);

  // Now the per-object constructor and methods

  commodity_t(const std::string& _symbol    = "",
	      unsigned int	 _precision = 0,
	      unsigned int       _flags	    = COMMODITY_STYLE_DEFAULTS)
    : symbol(_symbol), quote(false), precision(_precision), flags(_flags) {
    check_symbol();
  }

  void check_symbol() {
    for (const char * p = symbol.c_str(); *p; p++)
      if (std::isspace(*p) || std::isdigit(*p) || *p == '-' || *p == '.') {
	quote = true;
	return;
      }
  }

  void add_price(const std::time_t date, const amount_t& price) {
    history.insert(history_pair(date, price));
  }
  bool remove_price(const std::time_t date) {
    history_map::size_type n = history.erase(date);
    return n > 0;
  }

  void set_conversion(const amount_t& price) {
    conversion = price;
  }

  amount_t value(const std::time_t moment = std::time(NULL));
};

} // namespace ledger

#endif // _AMOUNT_H
