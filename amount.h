#ifndef _AMOUNT_H
#define _AMOUNT_H

#include <map>
#include <string>
#include <ctime>
#include <cctype>
#include <iostream>

#include "debug.h"

namespace ledger {

class commodity_t;

class amount_t
{
  void _init();
  void _copy(const amount_t& amt);
  void _release();
  void _dup();
  void _resize(unsigned int prec);

  void _clear() {
    if (quantity) {
      assert(commodity);
      _release();
      quantity  = NULL;
      commodity = NULL;
    } else {
      assert(! commodity);
    }
  }

 public:
  class bigint_t;

  bigint_t *	quantity;
  commodity_t *	commodity;

  // constructors
  amount_t() : quantity(NULL), commodity(NULL) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor amount_t");
  }
  amount_t(const amount_t& amt) : quantity(NULL) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor amount_t");
    if (amt.quantity)
      _copy(amt);
    else
      commodity = NULL;
  }
  amount_t(const std::string& value) : quantity(NULL) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor amount_t");
    parse(value);
  }
  amount_t(const char * value) : quantity(NULL) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor amount_t");
    parse(value);
  }
  amount_t(const bool value);
  amount_t(const int value);
  amount_t(const unsigned int value);
  amount_t(const double value);

  // destructor
  ~amount_t() {
    DEBUG_PRINT("ledger.memory.dtors", "dtor amount_t");
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
  amount_t round(unsigned int prec) const;

  // in-place arithmetic
  amount_t& operator+=(const amount_t& amt);
  amount_t& operator-=(const amount_t& amt);
  amount_t& operator*=(const amount_t& amt);
  amount_t& operator/=(const amount_t& amt);

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
  bool operator==(const int num) const;
  bool operator!=(const int num) const {
    return ! (*this == num);
  }

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

  void parse(std::istream& in);
  void parse(const std::string& str);

  void write_quantity(std::ostream& out) const;
  void read_quantity(char *& data);
  void read_quantity(std::istream& in);

  bool valid() const;

  friend std::istream& operator>>(std::istream& in, amount_t& amt);
};

unsigned int sizeof_bigint_t();

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
#define COMMODITY_STYLE_NOMARKET   0x0010

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
			    const std::time_t last,
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
  std::time_t	 last_lookup;
  amount_t	 conversion;
  ident_t	 ident;

  // If set, this global function pointer is called to determine
  // whether prices have been updated in the meanwhile.

  static updater_t *     updater;

  // This map remembers all commodities that have been defined.

  static commodities_map commodities;
  static commodity_t *   null_commodity;

  static void add_commodity(commodity_t * commodity,
			    const std::string symbol = "") {
    // The argument "symbol" is useful for creating a symbol alias to
    // an underlying commodity type; it is used by the Gnucash parser
    // to link "USD" to "$".
    std::pair<commodities_map::iterator, bool> result
      = commodities.insert(commodities_pair((symbol.empty() ?
					     commodity->symbol : symbol),
					    commodity));
    assert(result.second);
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
    : symbol(_symbol), quote(false), precision(_precision),
      flags(_flags), last_lookup(0) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor commodity_t");
    check_symbol();
  }
#ifdef DEBUG_ENABLED
  ~commodity_t() {
    DEBUG_PRINT("ledger.memory.dtors", "dtor commodity_t");
  }
#endif

  void check_symbol() {
    for (const char * p = symbol.c_str(); *p; p++)
      if (std::isspace(*p) || std::isdigit(*p) || *p == '-' || *p == '.') {
	quote = true;
	return;
      }
  }

  void add_price(const std::time_t date, const amount_t& price);
  bool remove_price(const std::time_t date) {
    history_map::size_type n = history.erase(date);
    return n > 0;
  }

  void set_conversion(const amount_t& price) {
    conversion = price;
  }

  amount_t value(const std::time_t moment = std::time(NULL));

  bool valid() const {
    if (symbol.empty() && this != null_commodity)
      return false;

    if (precision > 16)
      return false;

    if (flags & ~0x1f)
      return false;

    if (! conversion.valid())
      return false;

    return true;
  }
};

} // namespace ledger

#endif // _AMOUNT_H
