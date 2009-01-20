/*
 * Copyright (c) 2003-2008, John Wiegley.  All rights reserved.
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
 * @defgroup numerics Core numerics
 */

/**
 * @file   balpair.h
 * @author John Wiegley
 * @date   Sun May 20 19:11:58 2007
 *
 * @brief  Provides an abstraction around balance_t for tracking costs.
 *
 * @ingroup numerics
 *
 * When a transaction's amount is added to a balance, only the "value"
 * of the amount is added -- not the associated cost of the
 * transaction.  To provide for this, the balance_pair_t type allows
 * for adding amounts and costs simultaneously to a single balance.
 * Both are tracked, and any time either the total amount balance or
 * the total cost balance may be extracted.
 *
 * Note: By default, all balance-like operations operate on the amount
 * balance, and not the cost.  Also, the cost is entirely optional, in
 * which case a balance_pair_t may be used as if it were a balance_t,
 * from which is it derived.
 */
#ifndef _BALPAIR_H
#define _BARPAIR_H

#include "balance.h"

namespace ledger {

class balance_pair_t
  : public balance_t,
    public equality_comparable<balance_pair_t,
	   equality_comparable<balance_pair_t, balance_t,
	   equality_comparable<balance_pair_t, amount_t,
	   equality_comparable<balance_pair_t, double,
	   equality_comparable<balance_pair_t, unsigned long,
	   equality_comparable<balance_pair_t, long,
	   additive<balance_pair_t,
	   additive<balance_pair_t, balance_t,
	   additive<balance_pair_t, amount_t,
	   additive<balance_pair_t, double,
	   additive<balance_pair_t, unsigned long,
	   additive<balance_pair_t, long,
	   multiplicative<balance_pair_t, amount_t,
	   multiplicative<balance_pair_t, balance_t,
	   multiplicative<balance_pair_t, double,
	   multiplicative<balance_pair_t, unsigned long,
	   multiplicative<balance_pair_t, long> > > > > > > > > > > > > > > > >
{
  /**
   * The `cost' member of a balance pair tracks the cost associated
   * with each transaction amount that is added.  This member is
   * optional, and if not cost-bearing transactions are added, it will
   * remain uninitialized.
   */
  optional<balance_t> cost;

  friend class value_t;
  friend class entry_base_t;

public:
  /**
   * Constructors.  balance_pair_t supports identical forms of construction
   * to balance_t.  See balance_t for more information.
   */
  balance_pair_t() {
    TRACE_CTOR(balance_pair_t, "");
  }
  balance_pair_t(const balance_t& bal) : balance_t(bal) {
    TRACE_CTOR(balance_pair_t, "const balance_t&");
  }
  balance_pair_t(const balance_t& bal, const balance_t& cost_bal)
    : balance_t(bal), cost(cost_bal) {
    TRACE_CTOR(balance_pair_t, "const balance_t&, const balance_t&");
  }
  balance_pair_t(const amount_t& amt) : balance_t(amt) {
    TRACE_CTOR(balance_pair_t, "const amount_t&");
  }
  balance_pair_t(const amount_t& amt, const amount_t& cost_amt)
    : balance_t(amt), cost(cost_amt) {
    TRACE_CTOR(balance_pair_t, "const amount_t&, const amount_t&");
  }
#ifdef HAVE_GDTOA
  balance_pair_t(const double val) : balance_t(val) {
    TRACE_CTOR(balance_pair_t, "const double");
  }
#endif
  balance_pair_t(const unsigned long val) : balance_t(val) {
    TRACE_CTOR(balance_pair_t, "const unsigned long");
  }
  balance_pair_t(const long val) : balance_t(val) {
    TRACE_CTOR(balance_pair_t, "const long");
  }

  explicit balance_pair_t(const string& val) : balance_t(val) {
    TRACE_CTOR(balance_pair_t, "const string&");
  }
  explicit balance_pair_t(const char * val) : balance_t(val) {
    TRACE_CTOR(balance_pair_t, "const char *");
  }

  /**
   * Destructor.
   */
  virtual ~balance_pair_t() {
    TRACE_DTOR(balance_pair_t);
  }

  /**
   * Assignment and copy operators.  A balance pair may be assigned or
   * copied, and assigned or copied from a balance.
   */
  balance_pair_t(const balance_pair_t& bal_pair)
    : balance_t(bal_pair), cost(bal_pair.cost) {
    TRACE_CTOR(balance_pair_t, "copy");
  }

  balance_pair_t& operator=(const balance_pair_t& bal_pair) {
    if (this != &bal_pair) {
      balance_t::operator=(bal_pair.quantity());
      cost = bal_pair.cost;
    }
    return *this;
  }
  balance_pair_t& operator=(const balance_t& bal) {
    balance_t::operator=(bal);
    return *this;
  }
  balance_pair_t& operator=(const amount_t& amt) {
    balance_t::operator=(amt);
    return *this;
  }

  balance_t& operator=(const string& str) {
    return *this = balance_t(str);
  }
  balance_t& operator=(const char * str) {
    return *this = balance_t(str);
  }

  /**
   * Binary arithmetic operators.  Balances support addition and
   * subtraction of other balance pairs, balances or amounts, but
   * multiplication and division are restricted to uncommoditized
   * amounts only.
   *
   * There is also an additional additive method called `add' which
   * allows for adding an amount and an associated cost
   * simultaneously.  The signature is:
   *   add(amount_t amount, optional<amount_t> cost)
   */
  balance_pair_t& operator+=(const balance_pair_t& bal_pair) {
    balance_t::operator+=(bal_pair);
    if (bal_pair.cost) {
      if (! cost)
	cost = quantity();
      *cost += *bal_pair.cost;
    }
    return *this;
  }
  balance_pair_t& operator-=(const balance_pair_t& bal_pair) {
    balance_t::operator+=(bal_pair);
    if (bal_pair.cost) {
      if (! cost)
	cost = quantity();
      *cost += *bal_pair.cost;
    }
    return *this;
  }

  virtual balance_pair_t& operator*=(const amount_t& amt) {
    balance_t::operator*=(amt);
    if (cost)
      *cost *= amt;
    return *this;
  }

  virtual balance_pair_t& operator/=(const amount_t& amt) {
    balance_t::operator/=(amt);
    if (cost)
      *cost /= amt;
    return *this;
  }

  balance_pair_t& add(const amount_t&  amt,
		      const optional<amount_t>& a_cost = none) {
    if (a_cost && ! cost)
      cost = quantity();

    *this += amt;

    if (cost)
      *cost += a_cost ? *a_cost : amt;

    return *this;
  }

  /**
   * Unary arithmetic operators.  There are only a few unary methods
   * supported for balance pairs (otherwise, the operators inherited
   * from balance_t are used):
   *
   * abs() returns the absolute value of both the quantity and the
   * cost of a balance pair.
   *
   * in_place_negate() negates all the amounts in both the quantity
   * and the cost.
   *
   * in_place_reduce() reduces all the amounts in both the quantity
   * and the cost.
   *
   * in_place_unreduce() unreduces all the amounts in both the
   * quantity and the cost.
   *
   * quantity() returns the balance part of a balance.  It is the same
   * as doing a downcast<balance_t>(balance_pair).
   */
  balance_pair_t abs() const {
    balance_t temp;
    foreach (const amounts_map::value_type& pair, amounts)
      temp += pair.second.abs();

    if (cost) {
      balance_t cost_temp;
      foreach (const amounts_map::value_type& pair, amounts)
	cost_temp += pair.second.abs();
      return balance_pair_t(temp, cost_temp);
    }
    return temp;
  }

  virtual balance_t& in_place_negate() {
    balance_t::in_place_negate();
    if (cost)
      cost->in_place_negate();
    return *this;
  }

  virtual balance_t& in_place_reduce() {
    // A temporary must be used here because reduction may cause
    // multiple component amounts to collapse to the same commodity.
    balance_t temp;
    foreach (const amounts_map::value_type& pair, amounts)
      temp += pair.second.reduce();

    if (cost) {
      balance_t cost_temp;
      foreach (const amounts_map::value_type& pair, amounts)
	cost_temp += pair.second.reduce();
      return *this = balance_pair_t(temp, cost_temp);
    }
    return *this = temp;
  }

  virtual balance_t& in_place_unreduce() {
    // A temporary must be used here because unreduction may cause
    // multiple component amounts to collapse to the same commodity.
    balance_t temp;
    foreach (const amounts_map::value_type& pair, amounts)
      temp += pair.second.unreduce();

    if (cost) {
      balance_t cost_temp;
      foreach (const amounts_map::value_type& pair, amounts)
	cost_temp += pair.second.unreduce();
      return *this = balance_pair_t(temp, cost_temp);
    }
    return *this = temp;
  }

  balance_t& quantity() {
    return *this;
  }
  const balance_t& quantity() const {
    return *this;
  }

  optional<balance_pair_t>
  value(const optional<datetime_t>&   moment      = none,
	const optional<commodity_t&>& in_terms_of = none) const;

  /**
   * Truth tests.  An balance pair may be truth tested by comparison
   * to another balance pair, or by using one of the inherited
   * operators from balance_t.
   */
  bool operator==(const balance_pair_t& bal_pair) const {
    if (quantity() != bal_pair.quantity())
      return false;

    if ((cost && ! bal_pair.cost) ||
	(! cost && bal_pair.cost))
      return false;

    if (*cost != *bal_pair.cost)
      return false;

    return true;
  }

  bool operator==(const balance_t& bal) const {
    return balance_t::operator==(bal);
  }
  bool operator==(const amount_t& amt) const {
    return balance_t::operator==(amt);
  }
  template <typename T>
  bool operator==(const T& val) const {
    return balance_t::operator==(val);
  }

  /**
   * Debugging methods.  There is only one method specifically for
   * balance pairs to help with debugging:
   *
   * valid() returns true if the balances within the balance pair are
   * valid.
   */
  bool valid() const {
    if (! balance_t::valid())
      return false;

    if (cost && ! cost->valid())
      return false;

    return true;
  }
};

} // namespace ledger

#endif // _BALPAIR_H
