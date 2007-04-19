/**
 * @file   amount.h
 * @author John Wiegley
 * @date   Wed Apr 18 22:05:53 2007
 * 
 * @brief  Types for handling commoditized math.
 * 
 * This file contains two of the most basic types in Ledger: amount_t
 * commodity_t, and annotated_commodity_t.  Both the commodity types
 * share a common base class, commodity_base_t.  These four class
 * together allow Ledger to handle mathematical expressions involving
 * differing commodities, or in some cases math using no commodities
 * at all (such as increasing a dollar amount by a multiplier).
 */

/*
 * Copyright (c) 2003-2007, John Wiegley.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * - Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 *
 * - Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * - Neither the name of New Artisans LLC nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

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

#include "times.h"
#include "debug.h"
#include "error.h"

namespace ledger {

extern bool do_cleanup;

class commodity_t;

/**
 * @class amount_t
 *
 * @brief Encapsulates infinite-precision commoditized amounts.
 *
 * The amount_t class can be used for commoditized infinite-precision
 * math, and also for uncommoditized math.  In the commoditized case,
 * commodities keep track of how they are used, and will always
 * display back to the user after the same fashion.  For
 * uncommoditized numbers, no display truncation is ever done.
 * Internally, precision is always kept to an excessive degree.
 */

class amount_t
{
 public:
  class bigint_t;

  static bool keep_price;
  static bool keep_date;
  static bool keep_tag;
  static bool keep_base;
  static bool full_strings;

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
  amount_t(const long val);
  amount_t(const unsigned long val);
  amount_t(const double val);

  // destructor
  ~amount_t() {
    TRACE_DTOR("amount_t");
    if (quantity)
      _release();
  }

  amount_t number() const {
    amount_t temp(*this);
    temp.clear_commodity();
    return temp;
  }

  bool has_commodity() const;
  commodity_t& commodity() const;
  void set_commodity(commodity_t& comm) {
    commodity_ = &comm;
  }
  void annotate_commodity(const amount_t&    price,
			  const ptime&	     date = ptime(),
			  const std::string& tag  = "");
  amount_t strip_annotations(const bool _keep_price = keep_price,
			     const bool _keep_date  = keep_date,
			     const bool _keep_tag   = keep_tag) const;
  void clear_commodity() {
    commodity_ = NULL;
  }
  amount_t price() const;
  ptime date() const;

  bool null() const {
    return ! quantity && ! has_commodity();
  }

  // assignment operator
  amount_t& operator=(const amount_t& amt);
  amount_t& operator=(const std::string& val);
  amount_t& operator=(const char * val);
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
  // jww (2007-04-17): change the name here
  void negate();
  amount_t negated() const {
    amount_t temp = *this;
    temp.negate();
    return temp;
  }
  amount_t operator-() const {
    return negated();
  }

  // test for zero and non-zero
  int sign() const;
  bool zero() const;
  bool realzero() const {
    return sign() == 0;
  }
  operator bool() const {
    return ! zero();
  }
  operator std::string() const {
    return to_string();
  }

  operator long() const;
  operator double() const;

  std::string to_string() const;
  std::string to_fullstring() const;
  std::string quantity_string() const;

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

  amount_t value(const ptime& moment) const;

  // jww (2007-04-17): change the name here
  void abs() {
    if (*this < 0)
      negate();
  }

  // jww (2007-04-17): change the name here
  void reduce();
  amount_t reduced() const {
    amount_t temp(*this);
    temp.reduce();
    return temp;
  }

  bool valid() const;

  static amount_t exact(const std::string& value);

  // This function is special, and exists only to support a custom
  // optimization in binary.cc (which offers a significant enough gain
  // to be worth the trouble).

  friend void clean_commodity_history(char * item_pool,
				      char * item_pool_end);

  friend bool parse_annotations(std::istream& in, amount_t& price,
				ptime& date, std::string& tag);

  // Streaming interface

  void dump(std::ostream& out) const {
    out << "AMOUNT(";
    print(out);
    out << ")";
  }

#define AMOUNT_PARSE_NO_MIGRATE 0x01
#define AMOUNT_PARSE_NO_REDUCE  0x02

  void print(std::ostream& out, bool omit_commodity = false,
	     bool full_precision = false) const;
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

inline amount_t amount_t::exact(const std::string& value) {
  amount_t temp;
  temp.parse(value, AMOUNT_PARSE_NO_MIGRATE);
  return temp;
}

inline std::string amount_t::to_string() const {
  std::ostringstream bufstream;
  print(bufstream);
  return bufstream.str();
}

inline std::string amount_t::to_fullstring() const {
  std::ostringstream bufstream;
  print(bufstream, false, true);
  return bufstream.str();
}

inline std::string amount_t::quantity_string() const {
  std::ostringstream bufstream;
  print(bufstream, true);
  return bufstream.str();
}

inline amount_t abs(const amount_t& amt) {
  return amt < 0 ? amt.negated() : amt;
}

#define DEFINE_AMOUNT_OPERATORS(T)				\
inline amount_t operator+(const T val, const amount_t& amt) {	\
  amount_t temp(val);						\
  temp += amt;							\
  return temp;							\
}								\
inline amount_t operator-(const T val, const amount_t& amt) {	\
  amount_t temp(val);						\
  temp -= amt;							\
  return temp;							\
}								\
inline amount_t operator*(const T val, const amount_t& amt) {	\
  amount_t temp(val);						\
  temp *= amt;							\
  return temp;							\
}								\
inline amount_t operator/(const T val, const amount_t& amt) {	\
  amount_t temp(val);						\
  temp /= amt;							\
  return temp;							\
}								\
								\
inline bool operator<(const T val, const amount_t& amt) {	\
  return amount_t(val) < amt;					\
}								\
inline bool operator<=(const T val, const amount_t& amt) {	\
  return amount_t(val) <= amt;					\
}								\
inline bool operator>(const T val, const amount_t& amt) {	\
  return amount_t(val) > amt;					\
}								\
inline bool operator>=(const T val, const amount_t& amt) {	\
  return amount_t(val) >= amt;					\
}								\
inline bool operator==(const T val, const amount_t& amt) {	\
  return amount_t(val) == amt;					\
}								\
inline bool operator!=(const T val, const amount_t& amt) {	\
  return amount_t(val) != amt;					\
}

DEFINE_AMOUNT_OPERATORS(long)
DEFINE_AMOUNT_OPERATORS(unsigned long)
DEFINE_AMOUNT_OPERATORS(double)

inline std::ostream& operator<<(std::ostream& out, const amount_t& amt) {
  amt.print(out, false, amount_t::full_strings);
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

typedef std::map<const ptime, amount_t>  history_map;
typedef std::pair<const ptime, amount_t> history_pair;

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
    ptime	last_lookup;
    // jww (2007-04-18): What is bogus_time?
    ptime	bogus_time;
    history_t() : last_lookup(), bogus_time() {}
  };
  history_t * history;

  void	   add_price(const ptime& date, const amount_t& price);
  bool	   remove_price(const ptime& date);
  amount_t value(const ptime& moment = now);

  class updater_t {
   public:
    virtual ~updater_t() {}
    virtual void operator()(commodity_base_t& commodity,
			    const ptime& moment,
			    const ptime& date,
			    const ptime& last,
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

  void add_price(const ptime& date, const amount_t& price) {
    return base->add_price(date, price);
  }
  bool remove_price(const ptime& date) {
    return base->remove_price(date);
  }
  amount_t value(const ptime& moment = now) const {
    return base->value(moment);
  }

  bool valid() const;
};

class annotated_commodity_t : public commodity_t
{
 public:
  const commodity_t * ptr;

  amount_t    price;
  ptime  date;
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
				const ptime&  date,
				const std::string& tag);

 private:
  static commodity_t * create(const commodity_t& comm,
			      const amount_t&    price,
			      const ptime&  date,
			      const std::string& tag,
			      const std::string& mapping_key);

  static commodity_t * find_or_create(const commodity_t& comm,
				      const amount_t&    price,
				      const ptime&  date,
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
