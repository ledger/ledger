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

/**
 * @file   balpair.h
 * @author John Wiegley
 * @date   Sun May 20 19:11:58 2007
 * 
 * @brief  Provides an abstraction around balance_t for tracking costs.
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
  : public equality_comparable<balance_pair_t,
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
           multiplicative2<balance_pair_t, long, balance_t
			   > > > > > > > > > > > > > > > > >
{
  /**
   * The `cost' member of a balance pair tracks the cost associated
   * with each transaction amount that is added.  This member is
   * optional, and if not cost-bearing transactions are added, it will
   * remain uninitialized.
   */
  optional<balance_t> cost;

  /**
   * The `quantity' method provides direct access to the balance_t
   * base-class part of the balance pair.
   */
  balance_t& quantity() {
    return *this;
  }
  const balance_t& quantity() const {
    return *this;
  }

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
  balance_pair_t(const amount_t& amt) : balance_t(amt) {
    TRACE_CTOR(balance_pair_t, "const amount_t&");
  }
  balance_pair_t(const double val) : balance_t(val) {
    TRACE_CTOR(balance_pair_t, "const double");
  }
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

  // in-place arithmetic
  balance_pair_t& operator+=(const balance_pair_t& bal_pair) {
    if (bal_pair.cost && ! cost)
      cost = quantity;
    quantity += bal_pair.quantity;
    if (cost)
      *cost += bal_pair.cost ? *bal_pair.cost : bal_pair.quantity;
    return *this;
  }
  balance_pair_t& operator-=(const balance_pair_t& bal_pair) {
    if (bal_pair.cost && ! cost)
      cost = quantity;
    quantity -= bal_pair.quantity;
    if (cost)
      *cost -= bal_pair.cost ? *bal_pair.cost : bal_pair.quantity;
    return *this;
  }

  // comparison
  bool operator==(const balance_pair_t& bal_pair) const {
    return quantity == bal_pair.quantity;
  }
  bool operator==(const balance_t& bal) const {
    return quantity == bal;
  }
  bool operator==(const amount_t& amt) const {
    return quantity == amt;
  }

  balance_pair_t& operator*=(const amount_t& amt) {
    quantity *= amt;
    if (cost)
      *cost *= amt;
    return *this;
  }
  balance_pair_t& operator/=(const amount_t& amt) {
    quantity /= amt;
    if (cost)
      *cost /= amt;
    return *this;
  }

  // unary negation
  void in_place_negate() {
    quantity.in_place_negate();
    if (cost)
      cost->in_place_negate();
  }
  balance_pair_t negate() const {
    balance_pair_t temp = *this;
    temp.in_place_negate();
    return temp;
  }
  balance_pair_t operator-() const {
    return negate();
  }

  // test for non-zero (use ! for zero)
  operator bool() const {
    return quantity;
  }

  bool is_realzero() const {
    return ((! cost || cost->is_realzero()) && quantity.is_realzero());
  }

  balance_pair_t abs() const {
    balance_pair_t temp = *this;
    temp.quantity = temp.quantity.abs();
    if (temp.cost)
      temp.cost = temp.cost->abs();
    return temp;
  }

  optional<amount_t>
  commodity_amount(const optional<const commodity_t&>& commodity = none) const {
    return quantity.commodity_amount(commodity);
  }
  optional<balance_t> value(const optional<moment_t>& moment = none) const {
    return quantity.value(moment);
  }

  balance_t
  strip_annotations(const bool keep_price = amount_t::keep_price,
		    const bool keep_date  = amount_t::keep_date,
		    const bool keep_tag   = amount_t::keep_tag) const {
    return quantity.strip_annotations(keep_price, keep_date, keep_tag);
  }

  void print(std::ostream& out, const int first_width,
	     const int latter_width = -1) const {
    quantity.print(out, first_width, latter_width);
  }

  balance_pair_t& add(const amount_t&  amt,
		      const optional<amount_t>& a_cost = none) {
    if (a_cost && ! cost)
      cost = quantity;
    quantity += amt;
    if (cost)
      *cost += a_cost ? *a_cost : amt;
    return *this;
  }

  bool valid() {
    return quantity.valid() && (! cost || cost->valid());
  }

  void in_place_reduce() {
    quantity.in_place_reduce();
    if (cost) cost->in_place_reduce();
  }
  balance_pair_t reduce() const {
    balance_pair_t temp(*this);
    temp.in_place_reduce();
    return temp;
  }

  friend std::ostream& operator<<(std::ostream& out,
				  const balance_pair_t& bal_pair);
};

inline std::ostream& operator<<(std::ostream& out,
				const balance_pair_t& bal_pair) {
  bal_pair.quantity.print(out, 12);
  return out;
}

} // namespace ledger

#endif // _BALPAIR_H
