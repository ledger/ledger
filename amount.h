#ifndef _AMOUNT_H
#define _AMOUNT_H

#include <map>
#include <stack>
#include <string>
#include <ctime>
#include <cctype>
#include <iostream>
#include <sstream>
#include <cassert>
#include <exception>

#include "debug.h"

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
  amount_t(const long value);
  amount_t(const unsigned long value);
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
  void annotate_commodity(const amount_t& price,
			  const std::time_t  date = 0,
			  const std::string& tag  = "");
  amount_t reduce_commodity(const bool keep_price = false,
			    const bool keep_date  = false,
			    const bool keep_tag   = false) const;
  void clear_commodity() {
    commodity_ = NULL;
  }
  amount_t price() const;
  std::time_t date() const;

  bool null() const {
    return ! quantity && ! commodity_;
  }

  std::string quantity_string() const;

  // assignment operator
  amount_t& operator=(const amount_t& amt);
  amount_t& operator=(const std::string& value);
  amount_t& operator=(const char * value);
  amount_t& operator=(const bool value);
  amount_t& operator=(const long value);
  amount_t& operator=(const unsigned long value);
  amount_t& operator=(const double value);

  // general methods
  amount_t round(unsigned int prec) const;
  amount_t round() const;
  amount_t unround() const;

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
  operator long() const;
  operator double() const;

  bool realzero() const;

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
    std::ostringstream temp;
    temp << num;
    std::istringstream in(temp.str());
    parse(in);
  }

  int sign() const;

  // POD comparisons
#define AMOUNT_CMP_INT(OP)			\
  template <typename T>				\
  bool operator OP (T num) const {		\
    if (num == 0) {				\
      return sign() OP 0;			\
    } else {					\
      amount_t amt;				\
      amt.parse_num(num);			\
      return *this OP amt;			\
    }						\
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

#define AMOUNT_PARSE_NO_MIGRATE 0x01
#define AMOUNT_PARSE_NO_REDUCE  0x02

  void parse(std::istream& in, unsigned short flags = 0);
  void parse(const std::string& str, unsigned short flags = 0);
  void reduce();

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
void parse_annotations(std::istream& in, const std::string& symbol,
		       std::string& name, std::string& price,
		       std::string& date, std::string& tag);
void parse_conversion(const std::string& larger,
		      const std::string& smaller);

inline bool is_quote_or_paren(char * p) {
  return *p == '"' || *p == '{' || *p == '[' || *p == '(';
}  

inline char * scan_past_quotes_and_parens(char * expr)
{
  std::stack<char> paren_stack;

  for (const char * p = expr; *p; p++) {
    if (*p == '"' ||
	((*p == '(' || ((*p == '{' || *p == '[') &&
			paren_stack.top() != '(')) &&
	 paren_stack.top() != '"')) {
      paren_stack.push(*p);
    }
    else if ((*p == ')' && paren_stack.top() == '(') ||
	     (*p == '}' && paren_stack.top() == '{') ||
	     (*p == ']' && paren_stack.top() == '[') ||
	     (*p == '"' && paren_stack.top() == '"')) {
      paren_stack.pop();
      if (paren_stack.size() == 0)
	break;
    }
  }
}

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
#define COMMODITY_STYLE_BUILTIN    0x0020

typedef std::map<const std::time_t, amount_t>  history_map;
typedef std::pair<const std::time_t, amount_t> history_pair;

class commodity_base_t;

typedef std::map<const std::string, commodity_base_t *>  base_commodities_map;
typedef std::pair<const std::string, commodity_base_t *> base_commodities_pair;

class commodity_base_t
{
 public:
  friend class commodity_t;
  friend class annotated_commodity_t;

  typedef unsigned long ident_t;

  ident_t	 ident;
  std::string	 name;
  std::string	 note;
  unsigned short precision;
  unsigned short flags;
  amount_t *	 smaller;
  amount_t *	 larger;

  commodity_base_t() : precision(0), flags(COMMODITY_STYLE_DEFAULTS),
		       history(NULL), smaller(NULL), larger(NULL) {}

  commodity_base_t(const std::string& _symbol,
		   unsigned int	_precision = 0,
		   unsigned int _flags	   = COMMODITY_STYLE_DEFAULTS)
    : precision(_precision), flags(_flags), history(NULL),
      smaller(NULL), larger(NULL), symbol(_symbol) {}

  ~commodity_base_t() {
    if (history) delete history;
    if (smaller) delete smaller;
    if (larger)  delete larger;
  }

  static base_commodities_map commodities;
  static commodity_base_t * create(const std::string& symbol);

  std::string symbol;

  struct history_t {
    history_map	prices;
    std::time_t	last_lookup;
  };
  history_t * history;

  void add_price(const std::time_t date, const amount_t& price);
  bool remove_price(const std::time_t date) {
    if (history) {
      history_map::size_type n = history->prices.erase(date);
      return n > 0;
    }
    return false;
  }

  amount_t value(const std::time_t moment = std::time(NULL));

  class updater_t {
   public:
    virtual ~updater_t() {}
    virtual void operator()(commodity_base_t& commodity,
			    const std::time_t moment,
			    const std::time_t date,
			    const std::time_t last,
			    amount_t&         price) = 0;
  };
  friend class updater_t;

  static updater_t * updater;
};

typedef std::map<const std::string, commodity_t *>  commodities_map;
typedef std::pair<const std::string, commodity_t *> commodities_pair;

class commodity_t
{
  friend class annotated_commodity_t;

 public:
  // This map remembers all commodities that have been defined.

  static commodities_map commodities;
  static bool            commodities_sorted;
  static commodity_t *   null_commodity;
  static commodity_t *   default_commodity;

  static commodity_t * create(const std::string& symbol);
  static commodity_t * find(const std::string& name);
  static commodity_t * find_or_create(const std::string& symbol);

  static bool needs_quotes(const std::string& symbol);

  static void make_alias(const std::string& symbol,
			 commodity_t * commodity);

  // These are specific to each commodity reference

  typedef unsigned long ident_t;

  ident_t	     ident;
  commodity_base_t * ptr;
  std::string	     qualified_symbol;
  bool		     annotated;

 public:
  explicit commodity_t() : ptr(NULL), annotated(false) {}

  operator bool() const {
    return this != null_commodity;
  }
  bool operator==(const commodity_t& comm) const {
    return ptr == comm.ptr;
  }
  bool operator!=(const commodity_t& comm) const {
    return ptr != comm.ptr;
  }

  std::string base_symbol() const {
    return ptr->symbol;
  }
  std::string symbol() const {
    return qualified_symbol;
  }

  void write(std::ostream& out) const {
    out << symbol();
  }

  std::string name() const {
    return ptr->name;
  }
  void set_name(const std::string& arg) {
    ptr->name = arg;
  }

  std::string note() const {
    return ptr->note;
  }
  void set_note(const std::string& arg) {
    ptr->note = arg;
  }

  unsigned short precision() const {
    return ptr->precision;
  }
  void set_precision(unsigned short arg) {
    ptr->precision = arg;
  }

  unsigned short flags() const {
    return ptr->flags;
  }
  void set_flags(unsigned short arg) {
    ptr->flags = arg;
  }
  void add_flags(unsigned short arg) {
    ptr->flags |= arg;
  }
  void drop_flags(unsigned short arg) {
    ptr->flags &= ~arg;
  }

  amount_t * smaller() const {
    return ptr->smaller;
  }
  void set_smaller(const amount_t& arg) {
    if (ptr->smaller)
      delete ptr->smaller;
    ptr->smaller = new amount_t(arg);
  }

  amount_t * larger() const {
    return ptr->larger;
  }
  void set_larger(const amount_t& arg) {
    if (ptr->larger)
      delete ptr->larger;
    ptr->larger = new amount_t(arg);
  }

  commodity_base_t::history_t * history() const {
    return ptr->history;
  }

  void add_price(const std::time_t date, const amount_t& price) {
    return ptr->add_price(date, price);
  }
  bool remove_price(const std::time_t date) {
    return ptr->remove_price(date);
  }
  amount_t value(const std::time_t moment = std::time(NULL)) const {
    return ptr->value(moment);
  }

  bool valid() const;
};

class annotated_commodity_t : public commodity_t
{
 public:
  const commodity_t * base;

  amount_t    price;
  std::time_t date;
  std::string tag;

  static std::string date_format;

  static void write_annotations(std::ostream&      out,
				const amount_t&    price,
				const std::time_t  date,
				const std::string& tag);
  static
  std::string make_qualified_name(const commodity_t& comm,
				  const amount_t& price,
				  const std::time_t date,
				  const std::string& tag);
  static commodity_t * create(const commodity_t& comm,
			      const amount_t&    price,
			      const std::time_t  date,
			      const std::string& tag,
			      const std::string& entry_name = "");
  static commodity_t * create(const std::string& symbol,
			      const amount_t&    price,
			      const std::time_t  date,
			      const std::string& tag);
  static commodity_t * create(const std::string& symbol,
			      const std::string& price,
			      const std::string& date,
			      const std::string& tag);
  static commodity_t * find_or_create(const commodity_t& comm,
				      const amount_t&    price,
				      const std::time_t  date,
				      const std::string& tag);

  explicit annotated_commodity_t() {
    annotated = true;
  }

  void write_annotations(std::ostream& out) const {
    annotated_commodity_t::write_annotations(out, price, date, tag);
  }
};

inline std::ostream& operator<<(std::ostream& out, const commodity_t& comm) {
  out << comm.symbol();
  return out;
}

inline amount_t amount_t::round() const {
  return round(commodity().precision());
}

inline commodity_t& amount_t::commodity() const {
  if (! commodity_)
    return *commodity_t::null_commodity;
  else
    return *commodity_;
}

inline amount_t amount_t::price() const {
  if (commodity_ && commodity_->annotated) {
    amount_t temp(((annotated_commodity_t *)commodity_)->price);
    temp *= *this;
    DEBUG_PRINT("amounts.commodities",
		"Returning price of " << *this << " = " << temp);
    return temp;
  } else {
    return 0L;
  }
}

inline std::time_t amount_t::date() const {
  if (commodity_ && commodity_->annotated) {
    DEBUG_PRINT("amounts.commodities",
		"Returning date of " << *this << " = "
		<< ((annotated_commodity_t *)commodity_)->date);
    return ((annotated_commodity_t *)commodity_)->date;
  } else {
    return 0L;
  }
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
