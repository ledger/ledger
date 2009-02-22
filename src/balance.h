/*
 * Copyright (c) 2003-2009, John Wiegley.  All rights reserved.
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

/**
 * @addtogroup math
 */

/**
 * @file   balance.h
 * @author John Wiegley
 *
 * @ingroup math
 *
 * @brief  Basic type for adding multiple commodities together
 *
 * Unlike the amount_t class, which throws an exception if amounts of
 * differing commodities are added or subtracted, the balance_t class
 * is designed to allow this, tracking the amounts of each component
 * commodity separately.
 */
#ifndef _BALANCE_H
#define _BALANCE_H

#include "commodity.h"

namespace ledger {

DECLARE_EXCEPTION(balance_error, std::runtime_error);

/**
 * @class balance_t
 *
 * @brief A wrapper around amount_t allowing addition of multiple commodities.
 *
 * The balance_t class is appopriate for keeping a running balance
 * where amounts of multiple commodities may be involved.
 */
class balance_t
  : public equality_comparable<balance_t,
	   equality_comparable<balance_t, amount_t,
	   equality_comparable<balance_t, double,
	   equality_comparable<balance_t, unsigned long,
	   equality_comparable<balance_t, long,
	   additive<balance_t,
	   additive<balance_t, amount_t,
	   additive<balance_t, double,
	   additive<balance_t, unsigned long,
	   additive<balance_t, long,
	   multiplicative<balance_t, amount_t,
	   multiplicative<balance_t, double,
	   multiplicative<balance_t, unsigned long,
	   multiplicative<balance_t, long> > > > > > > > > > > > > >
{
public:
  typedef std::map<const commodity_t *, amount_t> amounts_map;

  amounts_map amounts;

  /**
   * Constructors.  balance_t supports similar forms of construction
   * to amount_t.
   *
   * balance_t() creates an empty balance to which amounts or other
   * balances may be added or subtracted.
   *
   * balance_t(amount_t) constructs a balance whose starting value is
   * equal to the given amount.
   *
   * balance_t(double), balance_t(unsigned long) and balance_t(long)
   * will construct an amount from their arguments and then construct
   * a balance whose starting value is equal to that amount.  This
   * initial balance will have no commodity.
   *
   * balance_t(string) and balance_t(const char *) both convert from a
   * string representation of an amount to a balance whose initial
   * value is that amount.  This is the proper way to initialize a
   * balance like '$100.00'.
   */
  balance_t() {
    TRACE_CTOR(balance_t, "");
  }
  balance_t(const amount_t& amt) {
    TRACE_CTOR(balance_t, "const amount_t&");
    if (amt.is_null())
      throw_(balance_error,
	     "Cannot initialize a balance from an uninitialized amount");
    if (! amt.is_realzero())
      amounts.insert(amounts_map::value_type(&amt.commodity(), amt));
  }
  balance_t(const double val) {
    TRACE_CTOR(balance_t, "const double");
    amounts.insert
      (amounts_map::value_type(amount_t::current_pool->null_commodity, val));
  }
  balance_t(const unsigned long val) {
    TRACE_CTOR(balance_t, "const unsigned long");
    amounts.insert
      (amounts_map::value_type(amount_t::current_pool->null_commodity, val));
  }
  balance_t(const long val) {
    TRACE_CTOR(balance_t, "const long");
    amounts.insert
      (amounts_map::value_type(amount_t::current_pool->null_commodity, val));
  }

  explicit balance_t(const string& val) {
    TRACE_CTOR(balance_t, "const string&");
    amount_t temp(val);
    amounts.insert(amounts_map::value_type(&temp.commodity(), temp));
  }
  explicit balance_t(const char * val) {
    TRACE_CTOR(balance_t, "const char *");
    amount_t temp(val);
    amounts.insert(amounts_map::value_type(&temp.commodity(), temp));
  }

  /**
   * Destructor.  Destroys all of the accumulated amounts in the
   * balance.
   */
  virtual ~balance_t() {
    TRACE_DTOR(balance_t);
  }

  /**
   * Assignment and copy operators.  An balance may be assigned or copied.
   */
  balance_t(const balance_t& bal) : amounts(bal.amounts) {
    TRACE_CTOR(balance_t, "copy");
  }

  balance_t& operator=(const balance_t& bal) {
    if (this != &bal)
      amounts = bal.amounts;
    return *this;
  }
  balance_t& operator=(const amount_t& amt) {
    if (amt.is_null())
      throw_(balance_error,
	     "Cannot assign an uninitialized amount to a balance");

    amounts.clear();
    if (! amt.is_realzero())
      amounts.insert(amounts_map::value_type(&amt.commodity(), amt));

    return *this;
  }

  balance_t& operator=(const string& str) {
    return *this = balance_t(str);
  }
  balance_t& operator=(const char * str) {
    return *this = balance_t(str);
  }

  /**
   * Comparison operators.  Balances are fairly restrictive in terms
   * of how they may be compared.  They may be compared for equality
   * or inequality, but this is all, since the concept of "less than"
   * or "greater than" makes no sense when amounts of multiple
   * commodities are involved.
   *
   * Balances may also be compared to amounts, in which case the sum
   * of the balance must equal the amount exactly.
   *
   * If a comparison between balances is desired, the balances must
   * first be rendered to value equivalent amounts using the `value'
   * method, to determine a market valuation at some specific moment
   * in time.
   */
  bool operator==(const balance_t& bal) const {
    amounts_map::const_iterator i, j;
    for (i = amounts.begin(), j = bal.amounts.begin();
	 i != amounts.end() && j != bal.amounts.end();
	 i++, j++) {
      if (! (i->first == j->first && i->second == j->second))
	return false;
    }
    return i == amounts.end() && j == bal.amounts.end();
  }
  bool operator==(const amount_t& amt) const {
    if (amt.is_null())
      throw_(balance_error,
	     "Cannot compare a balance to an uninitialized amount");

    if (amt.is_realzero())
      return amounts.empty();
    else
      return amounts.size() == 1 && amounts.begin()->second == amt;
  }

  template <typename T>
  bool operator==(const T& val) const {
    return *this == balance_t(val);
  }

  /**
   * Binary arithmetic operators.  Balances support addition and
   * subtraction of other balances or amounts, but multiplication and
   * division are restricted to uncommoditized amounts only.
   */
  balance_t& operator+=(const balance_t& bal);
  balance_t& operator+=(const amount_t& amt);
  balance_t& operator-=(const balance_t& bal);
  balance_t& operator-=(const amount_t& amt);

  virtual balance_t& operator*=(const amount_t& amt);

  balance_t& operator*=(const double val) {
    return *this *= amount_t(val);
  }
  balance_t& operator*=(const unsigned long val) {
    return *this *= amount_t(val);
  }
  balance_t& operator*=(const long val) {
    return *this *= amount_t(val);
  }

  virtual balance_t& operator/=(const amount_t& amt);

  balance_t& operator/=(const double val) {
    return *this /= amount_t(val);
  }
  balance_t& operator/=(const unsigned long val) {
    return *this /= amount_t(val);
  }
  balance_t& operator/=(const long val) {
    return *this /= amount_t(val);
  }

  /**
   * Unary arithmetic operators.  There are only a few unary methods
   * support on balance:
   *
   * negate(), also unary minus (- x), returns a balance all of whose
   * component amounts have been negated.  In order words, it inverts
   * the sign of all member amounts.
   *
   * abs() returns a balance where no component amount is negative.
   *
   * reduce() reduces the values in a balance to their most basic
   * commodity forms, for amounts that utilize "scaling commodities".
   * For example, a balance of 1h and 1m after reduction will be
   * 3660s.
   *
   * unreduce(), if used with amounts that use "scaling commodities",
   * yields the most compact form greater than 1.0 for each component
   * amount.  That is, a balance of 10m and 1799s will unreduce to
   * 39.98m.
   *
   * value(optional<datetime_t>) returns the total historical value for
   * a balance -- the default moment returns a value based on the most
   * recently known price -- based on the price history of its
   * component commodities.  See amount_t::value for an example.
   *
   * Further, for the sake of efficiency and avoiding temporary
   * objects, the following methods support "in-place" variants act on
   * the balance itself and return a reference to the result
   * (`*this'):
   *
   * in_place_negate()
   * in_place_reduce()
   * in_place_unreduce()
   */
  balance_t negate() const {
    balance_t temp(*this);
    temp.in_place_negate();
    return temp;
  }
  virtual balance_t& in_place_negate() {
    foreach (amounts_map::value_type& pair, amounts)
      pair.second.in_place_negate();
    return *this;
  }
  balance_t operator-() const {
    return negate();
  }

  balance_t abs() const {
    balance_t temp;
    foreach (const amounts_map::value_type& pair, amounts)
      temp += pair.second.abs();
    return temp;
  }

  balance_t rounded() const {
    balance_t temp;
    foreach (const amounts_map::value_type& pair, amounts)
      temp += pair.second.rounded();
    return temp;
  }

  balance_t unrounded() const {
    balance_t temp;
    foreach (const amounts_map::value_type& pair, amounts)
      temp += pair.second.unrounded();
    return temp;
  }

  balance_t reduced() const {
    balance_t temp(*this);
    temp.in_place_reduce();
    return temp;
  }
  virtual balance_t& in_place_reduce() {
    // A temporary must be used here because reduction may cause
    // multiple component amounts to collapse to the same commodity.
    balance_t temp;
    foreach (const amounts_map::value_type& pair, amounts)
      temp += pair.second.reduced();
    return *this = temp;
  }

  balance_t unreduced() const {
    balance_t temp(*this);
    temp.in_place_unreduce();
    return temp;
  }
  virtual balance_t& in_place_unreduce() {
    // A temporary must be used here because unreduction may cause
    // multiple component amounts to collapse to the same commodity.
    balance_t temp;
    foreach (const amounts_map::value_type& pair, amounts)
      temp += pair.second.unreduced();
    return *this = temp;
  }

  optional<balance_t>
  value(const bool		      primary_only = false,
	const optional<datetime_t>&   moment	   = none,
	const optional<commodity_t&>& in_terms_of  = none) const;

  /**
   * Truth tests.  An balance may be truth test in two ways:
   *
   * is_nonzero(), or operator bool, returns true if a balance's
   * display value is not zero.
   *
   * is_zero() returns true if an balance's display value is zero.
   * Thus, a balance containing $0.0001 is considered zero if the
   * current display precision for dollars is two decimal places.
   *
   * is_realzero() returns true if an balance's actual value is zero.
   * Thus, a balance containing $0.0001 is never considered realzero.
   *
   * is_empty() returns true if a balance has no amounts within it.
   * This can occur after a balance has been default initialized, or
   * if the exact amount it contains is subsequently subtracted from
   * it.
   */
  operator bool() const {
    foreach (const amounts_map::value_type& pair, amounts)
      if (pair.second.is_nonzero())
	return true;
    return false;
  }

  bool is_zero() const {
    if (is_empty())
      return true;

    foreach (const amounts_map::value_type& pair, amounts)
      if (! pair.second.is_zero())
	return false;
    return true;
  }

  bool is_realzero() const {
    if (is_empty())
      return true;

    foreach (const amounts_map::value_type& pair, amounts)
      if (! pair.second.is_realzero())
	return false;
    return true;
  }

  bool is_empty() const {
    return amounts.size() == 0;
  }

  /**
   * Conversion methods.  A balance can be converted to an amount, but
   * only if contains a single component amount.
   */
  amount_t to_amount() const {
    if (is_empty())
      throw_(balance_error, "Cannot convert an empty balance to an amount");
    else if (amounts.size() == 1)
      return amounts.begin()->second;
    else
      throw_(balance_error,
	     "Cannot convert a balance with multiple commodities to an amount");
  }

  /**
   * Commodity-related methods.  Balances support two
   * commodity-related methods:
   *
   * commodity_count() returns the number of different commodities
   * stored in the balance.
   *
   * commodity_amount(optional<commodity_t>) returns an (optional)
   * amount for the given commodity within the balance; if no
   * commodity is specified, it returns the (optional) uncommoditized
   * component of the balance.  If no matching element can be found,
   * boost::none is returned.
   */
  std::size_t commodity_count() const {
    return amounts.size();
  }

  optional<amount_t>
  commodity_amount(const optional<const commodity_t&>& commodity = none) const;

  /**
   * Annotated commodity methods.  The amounts contained by a balance
   * may use annotated commodities.  The `strip_annotations' method
   * will return a balance all of whose component amount have had
   * their commodity annotations likewise stripped.  See
   * amount_t::strip_annotations for more details.
   */
  balance_t strip_annotations(const keep_details_t& what_to_keep) const;

  /**
   * Printing methods.  A balance may be output to a stream using the
   * `print' method.  There is also a global operator<< defined which
   * simply calls print for a balance on the given stream.  There is
   * one form of the print method, which takes two required arguments
   * and one arguments with a default value:
   *
   * print(ostream, int first_width, int latter_width) prints a
   * balance to the given output stream, using each commodity's
   * default display characteristics.  The first_width parameter
   * specifies the width that should be used for printing amounts
   * (since they are likely to vary in width).  The latter_width, if
   * specified, gives the width to be used for each line after the
   * first.  This is useful when printing in a column which falls at
   * the right-hand side of the screen.
   *
   * In addition to the width constraints, balances will also print
   * with commodities in alphabetized order, regardless of the
   * relative amounts of those commodities.  There is no option to
   * change this behavior.
   */
  void print(std::ostream& out,
	     const int	   first_width	 = -1,
	     const int	   latter_width	 = -1,
	     const bool	   right_justify = true) const;

  /**
   * Debugging methods.  There are two methods defined to help with
   * debugging:
   *
   * dump(ostream) dumps a balance to an output stream.  There is
   * little different from print(), it simply surrounds the display
   * value with a marker, for example "BALANCE($1.00, DM 12.00)".
   * This code is used by other dumping code elsewhere in Ledger.
   *
   * valid() returns true if the amounts within the balance are valid.
   */
  void dump(std::ostream& out) const {
    out << "BALANCE(";
    bool first = true;
    foreach (const amounts_map::value_type& pair, amounts) {
      if (first)
	first = false;
      else
	out << ", ";
      pair.second.print(out);
    }
    out << ")";
  }

  virtual bool valid() const {
    foreach (const amounts_map::value_type& pair, amounts)
      if (! pair.second.valid())
	return false;
    return true;
  }
};

inline std::ostream& operator<<(std::ostream& out, const balance_t& bal) {
  bal.print(out, 12);
  return out;
}

} // namespace ledger

#endif // _BALANCE_H
