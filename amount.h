#ifndef _AMOUNT_H
#define _AMOUNT_H

#include <map>
#include <string>
#include <ctime>
#include <cctype>
#include <iostream>
#include <cassert>
#include <exception>

namespace ledger {

class commodity_t;

class amount_t
{
 public:
  class bigint_t;

 protected:
  void _init();
  void _copy(const amount_t& amt);
  void _release();
  void _dup();
  void _resize(unsigned int prec);

  void _clear() {
    if (quantity) {
      assert(commodity_);
      _release();
      quantity   = NULL;
      commodity_ = NULL;
    } else {
      assert(! commodity_);
    }
  }

  bigint_t *	quantity;
  commodity_t *	commodity_;

 public:
  // constructors
  amount_t() : quantity(NULL), commodity_(NULL) {}
  amount_t(const amount_t& amt) : quantity(NULL) {
    if (amt.quantity)
      _copy(amt);
    else
      commodity_ = NULL;
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

  commodity_t& commodity() const;
  void set_commodity(commodity_t& comm) {
    commodity_ = &comm;
  }
  void clear_commodity() {
    commodity_ = NULL;
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

  template <typename T>
  amount_t& operator+=(T value) {
    return *this += amount_t(value);
  }
  template <typename T>
  amount_t& operator-=(T value) {
    return *this -= amount_t(value);
  }
  template <typename T>
  amount_t& operator*=(T value) {
    return *this *= amount_t(value);
  }
  template <typename T>
  amount_t& operator/=(T value) {
    return *this /= amount_t(value);
  }

  // simple arithmetic
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

  template <typename T>
  amount_t operator+(T value) const {
    amount_t temp = *this;
    temp += value;
    return temp;
  }
  template <typename T>
  amount_t operator-(T value) const {
    amount_t temp = *this;
    temp -= value;
    return temp;
  }
  template <typename T>
  amount_t operator*(T value) const {
    amount_t temp = *this;
    temp *= value;
    return temp;
  }
  template <typename T>
  amount_t operator/(T value) const {
    amount_t temp = *this;
    temp /= value;
    return temp;
  }

  // unary negation
  void negate();
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

  // comparisons between amounts
  bool operator<(const amount_t& amt) const;
  bool operator<=(const amount_t& amt) const;
  bool operator>(const amount_t& amt) const;
  bool operator>=(const amount_t& amt) const;
  bool operator==(const amount_t& amt) const;
  bool operator!=(const amount_t& amt) const {
    if (commodity_ != amt.commodity_)
      return true;
    return ! (*this == amt);
  }

  template <typename T>
  void parse_num(T num) {
    std::string str;
    { std::ostringstream strstr(str);
      strstr << num;
    }
    { std::istringstream strstr(str);
      parse(strstr);
    }
  }

  int sign() const;

  // POD comparisons
#define AMOUNT_CMP_INT(OP)					\
  template <typename T>						\
  bool operator OP (T num) const {			\
    if (num == 0) {						\
      return sign() OP 0;					\
    } else {							\
      amount_t amt;						\
      amt.parse_num(num);					\
      return *this OP amt;					\
    }								\
  }

  AMOUNT_CMP_INT(<)
  AMOUNT_CMP_INT(<=)
  AMOUNT_CMP_INT(>)
  AMOUNT_CMP_INT(>=)
  AMOUNT_CMP_INT(==)

  template <typename T>
  bool operator!=(T num) const {
    return ! (*this == num);
  }

  amount_t value(const std::time_t moment) const;

  void abs() {
    if (*this < 0)
      negate();
  }

  void parse(std::istream& in);
  void parse(const std::string& str);

  void read_quantity(char *& data);
  void read_quantity(std::istream& in);
  void write_quantity(std::ostream& out) const;

  bool valid() const;

  // Classes that are friends, and help to implement this class

  friend std::ostream& operator<<(std::ostream& out, const amount_t& amt);
  friend std::istream& operator>>(std::istream& in, amount_t& amt);

  friend unsigned int sizeof_bigint_t();

  friend void read_binary_amount(char *& data, amount_t& amt);
  friend void write_binary_amount(std::ostream& out, const amount_t& amt);

  // This function is special, and exists only to support a custom
  // optimization in binary.cc (which offers a significant enough gain
  // to be worth the trouble).

  friend void clean_commodity_history(char * item_pool,
				      char * item_pool_end);
};

unsigned int sizeof_bigint_t();

void parse_quantity(std::istream& in, std::string& value);
void parse_commodity(std::istream& in, std::string& symbol);

inline amount_t abs(const amount_t& amt) {
  return amt < 0 ? amt.negated() : amt;
}

std::ostream& operator<<(std::ostream& out, const amount_t& amt);

inline std::istream& operator>>(std::istream& in, amount_t& amt) {
  amt.parse(in);
  return in;
}


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
    virtual void operator()(commodity_t&      commodity,
			    const std::time_t moment,
			    const std::time_t date,
			    const std::time_t last,
			    amount_t&         price) = 0;
  };

  typedef unsigned long ident_t;

  const std::string symbol;
  bool		    quote;
  std::string	    name;
  std::string	    note;
  unsigned short    precision;
  unsigned short    flags;
  history_map	    history;
  std::time_t	    last_lookup;
  amount_t	    conversion;
  ident_t	    ident;

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
    : precision(_precision), flags(_flags), last_lookup(0) {
    set_symbol(_symbol);
  }

  operator bool() const {
    return this != null_commodity;
  }
  bool operator==(const commodity_t& comm) const {
    return this == &comm;
  }
  bool operator!=(const commodity_t& comm) const {
    return this != &comm;
  }

  void set_symbol(const std::string& sym) {
    *(const_cast<std::string *>(&symbol)) = sym;
    quote = false;
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

inline std::ostream& operator<<(std::ostream& out, const commodity_t& comm) {
  out << comm.symbol;
  return out;
}

inline commodity_t& amount_t::commodity() const {
  if (! commodity_)
    return *commodity_t::null_commodity;
  else
    return *commodity_;
}

class amount_error : public std::exception {
  std::string reason;
 public:
  amount_error(const std::string& _reason) throw() : reason(_reason) {}
  virtual ~amount_error() throw() {}

  virtual const char* what() const throw() {
    return reason.c_str();
  }
};

} // namespace ledger

#endif // _AMOUNT_H
