#ifndef _AMOUNT_H
#define _AMOUNT_H

#include <map>
#include <deque>
#include <stack>
#include <string>
#include <cctype>
#include <iostream>
#include <sstream>
#include <cassert>
#include <exception>

#include "datetime.h"
#include "debug.h"
#include "error.h"

namespace ledger {

extern bool do_cleanup;

class commodity_t;

class amount_t
{
 public:
  class bigint_t;

  static bool keep_price;
  static bool keep_date;
  static bool keep_tag;
  static bool keep_base;

 protected:
  void _init();
  void _copy(const amount_t& amt);
  void _release();
  void _dup();
  void _resize(unsigned int prec);
  void _clear();

  bigint_t *	quantity;
  commodity_t *	commodity_;

 public:
  // constructors
  amount_t() : quantity(NULL), commodity_(NULL) {
    TRACE_CTOR("amount_t()");
  }
  amount_t(const amount_t& amt) : quantity(NULL) {
    TRACE_CTOR("amount_t(copy)");
    if (amt.quantity)
      _copy(amt);
    else
      commodity_ = NULL;
  }
  amount_t(const std::string& val) : quantity(NULL) {
    TRACE_CTOR("amount_t(const std::string&)");
    parse(val);
  }
  amount_t(const char * val) : quantity(NULL) {
    TRACE_CTOR("amount_t(const char *)");
    parse(val);
  }
  amount_t(const bool val);
  amount_t(const long val);
  amount_t(const unsigned long val);
  amount_t(const double val);

  // destructor
  ~amount_t() {
    TRACE_DTOR("amount_t");
    if (quantity)
      _release();
  }

  bool has_commodity() const;
  commodity_t& commodity() const;
  void set_commodity(commodity_t& comm) {
    commodity_ = &comm;
  }
  void annotate_commodity(const amount_t&    price,
			  const datetime_t&  date = datetime_t(),
			  const std::string& tag  = "");
  amount_t strip_annotations(const bool _keep_price = keep_price,
			     const bool _keep_date  = keep_date,
			     const bool _keep_tag   = keep_tag) const;
  void clear_commodity() {
    commodity_ = NULL;
  }
  amount_t price() const;
  datetime_t date() const;

  bool null() const {
    return ! quantity && ! has_commodity();
  }

  std::string quantity_string() const;

  // assignment operator
  amount_t& operator=(const amount_t& amt);
  amount_t& operator=(const std::string& val);
  amount_t& operator=(const char * val);
  amount_t& operator=(const bool val);
  amount_t& operator=(const long val);
  amount_t& operator=(const unsigned long val);
  amount_t& operator=(const double val);

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
  amount_t& operator+=(T val) {
    return *this += amount_t(val);
  }
  template <typename T>
  amount_t& operator-=(T val) {
    return *this -= amount_t(val);
  }
  template <typename T>
  amount_t& operator*=(T val) {
    return *this *= amount_t(val);
  }
  template <typename T>
  amount_t& operator/=(T val) {
    return *this /= amount_t(val);
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
  amount_t operator+(T val) const {
    amount_t temp = *this;
    temp += val;
    return temp;
  }
  template <typename T>
  amount_t operator-(T val) const {
    amount_t temp = *this;
    temp -= val;
    return temp;
  }
  template <typename T>
  amount_t operator*(T val) const {
    amount_t temp = *this;
    temp *= val;
    return temp;
  }
  template <typename T>
  amount_t operator/(T val) const {
    amount_t temp = *this;
    temp /= val;
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
  int compare(const amount_t& amt) const;

  bool operator<(const amount_t& amt) const {
    return compare(amt) < 0;
  }
  bool operator<=(const amount_t& amt) const {
    return compare(amt) <= 0;
  }
  bool operator>(const amount_t& amt) const {
    return compare(amt) > 0;
  }
  bool operator>=(const amount_t& amt) const {
    return compare(amt) >= 0;
  }
  bool operator==(const amount_t& amt) const;
  bool operator!=(const amount_t& amt) const;

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

  amount_t value(const datetime_t& moment) const;

  void abs() {
    if (*this < 0)
      negate();
  }

  void reduce();
  amount_t reduced() const {
    amount_t temp(*this);
    temp.reduce();
    return temp;
  }

  bool valid() const;

  // This function is special, and exists only to support a custom
  // optimization in binary.cc (which offers a significant enough gain
  // to be worth the trouble).

  friend void clean_commodity_history(char * item_pool,
				      char * item_pool_end);

  friend void parse_annotations(std::istream& in, amount_t& price,
				datetime_t& date, std::string& tag);

  // Streaming interface

  void dump(std::ostream& out) const {
    out << "AMOUNT(";
    print(out);
    out << ")";
  }

#define AMOUNT_PARSE_NO_MIGRATE 0x01
#define AMOUNT_PARSE_NO_REDUCE  0x02

  void print(std::ostream& out) const;
  void parse(std::istream& in, unsigned char flags = 0);
  void parse(const std::string& str, unsigned char flags = 0) {
    std::istringstream stream(str);
    parse(stream, flags);
  }

  void print_quantity(std::ostream& out) const;

  void write(std::ostream& out) const;
  void read(std::istream& in);
  void read(char *& data);

  void write_quantity(std::ostream& out) const;
  void read_quantity(std::istream& in);
  void read_quantity(char *& data);
};

inline amount_t abs(const amount_t& amt) {
  return amt < 0 ? amt.negated() : amt;
}

template <typename T>
inline amount_t operator+(const T val, const amount_t& amt) {
  amount_t temp(val);
  temp += amt;
  return temp;
}

template <typename T>
inline amount_t operator-(const T val, const amount_t& amt) {
  amount_t temp(val);
  temp -= amt;
  return temp;
}

template <typename T>
inline amount_t operator*(const T val, const amount_t& amt) {
  amount_t temp(val);
  temp *= amt;
  return temp;
}

template <typename T>
inline amount_t operator/(const T val, const amount_t& amt) {
  amount_t temp(val);
  temp /= amt;
  return temp;
}

template <typename T>
inline bool operator<(const T val, const amount_t& amt) {
  return amount_t(val) < amt;
}

template <typename T>
inline bool operator<=(const T val, const amount_t& amt) {
  return amount_t(val) <= amt;
}

template <typename T>
inline bool operator>(const T val, const amount_t& amt) {
  return amount_t(val) > amt;
}

template <typename T>
inline bool operator>=(const T val, const amount_t& amt) {
  return amount_t(val) >= amt;
}

template <typename T>
inline bool operator==(const T val, const amount_t& amt) {
  return amount_t(val) == amt;
}

template <typename T>
inline bool operator!=(const T val, const amount_t& amt) {
  return amount_t(val) != amt;
}

inline std::ostream& operator<<(std::ostream& out, const amount_t& amt) {
  amt.print(out);
  return out;
}
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

typedef std::map<const datetime_t, amount_t>  history_map;
typedef std::pair<const datetime_t, amount_t> history_pair;

class commodity_base_t;

typedef std::map<const std::string, commodity_base_t *>  base_commodities_map;
typedef std::pair<const std::string, commodity_base_t *> base_commodities_pair;

class commodity_base_t
{
 public:
  friend class commodity_t;
  friend class annotated_commodity_t;

  typedef unsigned long ident_t;

  ident_t	ident;
  std::string	name;
  std::string	note;
  unsigned char precision;
  unsigned char flags;
  amount_t *	smaller;
  amount_t *	larger;

  commodity_base_t()
    : precision(0), flags(COMMODITY_STYLE_DEFAULTS),
      smaller(NULL), larger(NULL), history(NULL) {}

  commodity_base_t(const std::string& _symbol,
		   unsigned int	_precision = 0,
		   unsigned int _flags	   = COMMODITY_STYLE_DEFAULTS)
    : precision(_precision), flags(_flags),
      smaller(NULL), larger(NULL), symbol(_symbol), history(NULL) {}

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
    datetime_t	last_lookup;
    datetime_t	bogus_time;
    history_t() : last_lookup(0), bogus_time(0) {}
  };
  history_t * history;

  void	   add_price(const datetime_t& date, const amount_t& price);
  bool	   remove_price(const datetime_t& date);
  amount_t value(const datetime_t& moment = datetime_t::now);

  class updater_t {
   public:
    virtual ~updater_t() {}
    virtual void operator()(commodity_base_t& commodity,
			    const datetime_t& moment,
			    const datetime_t& date,
			    const datetime_t& last,
			    amount_t& price) = 0;
  };
  friend class updater_t;

  static updater_t * updater;
};

typedef std::map<const std::string, commodity_t *>  commodities_map;
typedef std::pair<const std::string, commodity_t *> commodities_pair;

typedef std::deque<commodity_t *> commodities_array;

class commodity_t
{
  friend class annotated_commodity_t;

 public:
  // This map remembers all commodities that have been defined.

  static commodities_map   commodities;
  static commodities_array commodities_by_ident;
  static bool		   commodities_sorted;
  static commodity_t *	   null_commodity;
  static commodity_t *	   default_commodity;

  static commodity_t * create(const std::string& symbol);
  static commodity_t * find(const std::string& name);
  static commodity_t * find_or_create(const std::string& symbol);

  static bool needs_quotes(const std::string& symbol);

  static void make_alias(const std::string& symbol,
			 commodity_t * commodity);

  // These are specific to each commodity reference

  typedef unsigned long ident_t;

  ident_t	     ident;
  commodity_base_t * base;
  std::string	     qualified_symbol;
  bool		     annotated;

 public:
  explicit commodity_t() : base(NULL), annotated(false) {
    TRACE_CTOR("commodity_t()");
  }
  virtual ~commodity_t() {
    TRACE_DTOR("commodity_t");
  }

  operator bool() const {
    return this != null_commodity;
  }
  virtual bool operator==(const commodity_t& comm) const {
    if (comm.annotated)
      return comm == *this;
    return base == comm.base;
  }
  bool operator!=(const commodity_t& comm) const {
    return ! (*this == comm);
  }

  std::string base_symbol() const {
    return base->symbol;
  }
  std::string symbol() const {
    return qualified_symbol;
  }

  void write(std::ostream& out) const {
    out << symbol();
  }

  std::string name() const {
    return base->name;
  }
  void set_name(const std::string& arg) {
    base->name = arg;
  }

  std::string note() const {
    return base->note;
  }
  void set_note(const std::string& arg) {
    base->note = arg;
  }

  unsigned char precision() const {
    return base->precision;
  }
  void set_precision(unsigned char arg) {
    base->precision = arg;
  }

  unsigned char flags() const {
    return base->flags;
  }
  void set_flags(unsigned char arg) {
    base->flags = arg;
  }
  void add_flags(unsigned char arg) {
    base->flags |= arg;
  }
  void drop_flags(unsigned char arg) {
    base->flags &= ~arg;
  }

  amount_t * smaller() const {
    return base->smaller;
  }
  void set_smaller(const amount_t& arg) {
    if (base->smaller)
      delete base->smaller;
    base->smaller = new amount_t(arg);
  }

  amount_t * larger() const {
    return base->larger;
  }
  void set_larger(const amount_t& arg) {
    if (base->larger)
      delete base->larger;
    base->larger = new amount_t(arg);
  }

  commodity_base_t::history_t * history() const {
    return base->history;
  }

  void add_price(const datetime_t& date, const amount_t& price) {
    return base->add_price(date, price);
  }
  bool remove_price(const datetime_t& date) {
    return base->remove_price(date);
  }
  amount_t value(const datetime_t& moment = datetime_t::now) const {
    return base->value(moment);
  }

  bool valid() const;
};

class annotated_commodity_t : public commodity_t
{
 public:
  const commodity_t * ptr;

  amount_t    price;
  datetime_t  date;
  std::string tag;

  explicit annotated_commodity_t() {
    TRACE_CTOR("annotated_commodity_t()");
    annotated = true;
  }

  virtual bool operator==(const commodity_t& comm) const;

  void write_annotations(std::ostream& out) const {
    annotated_commodity_t::write_annotations(out, price, date, tag);
  }

  static void write_annotations(std::ostream&      out,
				const amount_t&    price,
				const datetime_t&  date,
				const std::string& tag);

 private:
  static commodity_t * create(const commodity_t& comm,
			      const amount_t&    price,
			      const datetime_t&  date,
			      const std::string& tag,
			      const std::string& mapping_key);

  static commodity_t * find_or_create(const commodity_t& comm,
				      const amount_t&    price,
				      const datetime_t&  date,
				      const std::string& tag);

  friend class amount_t;
};

inline std::ostream& operator<<(std::ostream& out, const commodity_t& comm) {
  out << comm.symbol();
  return out;
}

inline amount_t amount_t::round() const {
  return round(commodity().precision());
}

inline bool amount_t::has_commodity() const {
  return commodity_ && commodity_ != commodity_t::null_commodity;
}

inline commodity_t& amount_t::commodity() const {
  if (! commodity_)
    return *commodity_t::null_commodity;
  else
    return *commodity_;
}


void parse_conversion(const std::string& larger_str,
		      const std::string& smaller_str);


class amount_error : public error {
 public:
  amount_error(const std::string& _reason) throw() : error(_reason) {}
  virtual ~amount_error() throw() {}
};

struct compare_amount_commodities {
  bool operator()(const amount_t * left, const amount_t * right) const;
};

} // namespace ledger

#endif // _AMOUNT_H
