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

#include "utils.h"

namespace ledger {

extern bool do_cleanup;

class commodity_t;

DECLARE_EXCEPTION(amount_error);

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
    : public ordered_field_operators<amount_t,
	     ordered_field_operators<amount_t, long,
	     ordered_field_operators<amount_t, unsigned long,
	     ordered_field_operators<amount_t, double> > > >
{
public:
  class bigint_t;

  static void initialize();
  static void shutdown();

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
    TRACE_CTOR(amount_t, "");
  }
  amount_t(const amount_t& amt) : quantity(NULL) {
    TRACE_CTOR(amount_t, "copy");
    if (amt.quantity)
      _copy(amt);
    else
      commodity_ = NULL;
  }
  amount_t(const long val);
  amount_t(const unsigned long val);
  amount_t(const double val);

  amount_t(const string& val) : quantity(NULL) {
    TRACE_CTOR(amount_t, "const string&");
    parse(val);
  }
  amount_t(const char * val) : quantity(NULL) {
    TRACE_CTOR(amount_t, "const char *");
    parse(val);
  }

  ~amount_t() {
    TRACE_DTOR(amount_t);
    if (quantity)
      _release();
  }

  static amount_t exact(const string& value);

  // assignment operator
  amount_t& operator=(const amount_t& amt);

  // comparisons between amounts
  int compare(const amount_t& amt) const;
  bool operator==(const amount_t& amt) const;

  template <typename T>
  bool operator==(const T& val) const {
    return compare(val) == 0;
  }
  template <typename T>
  bool operator<(const T& amt) const {
    return compare(amt) < 0;
  }
  template <typename T>
  bool operator>(const T& amt) const {
    return compare(amt) > 0;
  }

  // in-place arithmetic
  amount_t& operator+=(const amount_t& amt);
  amount_t& operator-=(const amount_t& amt);
  amount_t& operator*=(const amount_t& amt);
  amount_t& operator/=(const amount_t& amt);

  // unary negation
  amount_t operator-() const {
    return negate();
  }
  amount_t negate() const {
    amount_t temp = *this;
    temp.in_place_negate();
    return temp;
  }
  void in_place_negate();

  // test for truth, zero and non-zero
  operator bool() const {
    return ! zero();
  }

  int  sign() const;
  bool zero() const;
  bool realzero() const {
    return sign() == 0;
  }

  // conversion methods
  string to_string() const;
  string to_fullstring() const;
  string quantity_string() const;

  // methods relating to the commodity
  bool is_null() const {
    return ! quantity && ! has_commodity();
  }

  amount_t number() const {
    if (! has_commodity())
      return *this;
    amount_t temp(*this);
    temp.clear_commodity();
    return temp;
  }

  bool has_commodity() const;
  void set_commodity(commodity_t& comm) {
    commodity_ = &comm;
  }
  void clear_commodity() {
    commodity_ = NULL;
  }

  commodity_t& commodity() const;

  void annotate_commodity(const optional<amount_t>& tprice,
			  const optional<moment_t>& tdate = optional<moment_t>(),
			  const optional<string>&   ttag  = optional<string>());

  amount_t strip_annotations(const bool _keep_price = keep_price,
			     const bool _keep_date  = keep_date,
			     const bool _keep_tag   = keep_tag) const;

  optional<amount_t> price() const;
  optional<moment_t> date() const;
  optional<string>   tag() const;

  // general methods
  amount_t round(unsigned int prec) const;
  amount_t round() const;
  amount_t unround() const;
  amount_t value(const moment_t& moment) const;

  amount_t abs() const {
    if (sign() < 0)
      return negate();
    return *this;
  }

  amount_t reduce() const {
    amount_t temp(*this);
    temp.in_place_reduce();
    return temp;
  }
  void in_place_reduce();

  bool valid() const;

  // This function is special, and exists only to support a custom
  // optimization in binary.cc (which offers a significant enough gain
  // to be worth the trouble).

  friend void clean_commodity_history(char * item_pool,
				      char * item_pool_end);

  friend void parse_annotations(std::istream&	    in,
				optional<amount_t>& price,
				optional<moment_t>& date,
				optional<string>&   tag);

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
  void parse(const string& str, unsigned char flags = 0) {
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

inline amount_t amount_t::exact(const string& value) {
  amount_t temp;
  temp.parse(value, AMOUNT_PARSE_NO_MIGRATE);
  return temp;
}

inline string amount_t::to_string() const {
  std::ostringstream bufstream;
  print(bufstream);
  return bufstream.str();
}

inline string amount_t::to_fullstring() const {
  std::ostringstream bufstream;
  print(bufstream, false, true);
  return bufstream.str();
}

inline string amount_t::quantity_string() const {
  std::ostringstream bufstream;
  print(bufstream, true);
  return bufstream.str();
}

inline std::ostream& operator<<(std::ostream& out, const amount_t& amt) {
  amt.print(out, false, amount_t::full_strings);
  return out;
}
inline std::istream& operator>>(std::istream& in, amount_t& amt) {
  amt.parse(in);
  return in;
}

} // namespace ledger

#include "commodity.h"

namespace ledger {

inline bool amount_t::operator==(const amount_t& amt) const {
  if (commodity() != amt.commodity())
    return false;
  return compare(amt) == 0;
}

inline amount_t amount_t::round() const {
  if (! has_commodity())
    return *this;
  return round(commodity().precision());
}

inline bool amount_t::has_commodity() const {
  return commodity_ && commodity_ != commodity_t::null_commodity;
}

inline commodity_t& amount_t::commodity() const {
  return has_commodity() ? *commodity_ : *commodity_t::null_commodity;
}

void parse_conversion(const string& larger_str,
		      const string& smaller_str);

} // namespace ledger

#endif // _AMOUNT_H
