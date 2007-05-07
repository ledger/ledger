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

#ifndef _BALANCE_H
#define _BALANCE_H

#include "amount.h"

namespace ledger {

class balance_t
  : public equality_comparable<balance_t,
           equality_comparable<balance_t, amount_t,
           additive<balance_t,
           additive<balance_t, amount_t,
           multiplicative<balance_t, amount_t,
           multiplicative<balance_t, unsigned long,
           multiplicative<balance_t, long> > > > > > >
{
public:
  typedef std::map<const commodity_t *, amount_t> amounts_map;

protected:
  amounts_map amounts;

  friend class value_t;
  friend class entry_base_t;

public:
  // constructors
  balance_t() {
    TRACE_CTOR(balance_t, "");
  }
  balance_t(const balance_t& bal) {
    TRACE_CTOR(balance_t, "copy");
    for (amounts_map::const_iterator i = bal.amounts.begin();
	 i != bal.amounts.end();
	 i++)
      *this += (*i).second;
  }
  balance_t(const amount_t& amt) {
    TRACE_CTOR(balance_t, "const amount_t&");
    amounts.insert(amounts_map::value_type(&amt.commodity(), amt));
  }
  ~balance_t() {
    TRACE_DTOR(balance_t);
  }

  // assignment operator
  balance_t& operator=(const balance_t& bal) {
    if (this != &bal) {
      amounts.clear();
      for (amounts_map::const_iterator i = bal.amounts.begin();
	   i != bal.amounts.end();
	   i++)
	*this += (*i).second;
    }
    return *this;
  }

  int compare(const balance_t& bal) const;

  bool operator==(const balance_t& bal) const {
    amounts_map::const_iterator i, j;
    for (i = amounts.begin(), j = bal.amounts.begin();
	 i != amounts.end() && j != bal.amounts.end();
	 i++, j++) {
      if (! ((*i).first  == (*j).first &&
	     (*i).second == (*j).second))
	return false;
    }
    return i == amounts.end() && j == bal.amounts.end();
  }
  bool operator==(const amount_t& amt) const {
    return amounts.size() == 1 && amounts.begin()->second == amt;
  }

  // in-place arithmetic
  balance_t& operator+=(const balance_t& bal) {
    for (amounts_map::const_iterator i = bal.amounts.begin();
	 i != bal.amounts.end();
	 i++)
      *this += (*i).second;
    return *this;
  }
  balance_t& operator-=(const balance_t& bal) {
    for (amounts_map::const_iterator i = bal.amounts.begin();
	 i != bal.amounts.end();
	 i++)
      *this -= (*i).second;
    return *this;
  }

  balance_t& operator*=(const amount_t& amt);
  balance_t& operator/=(const amount_t& amt);

  // unary negation
  void in_place_negate() {
    for (amounts_map::iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      (*i).second = (*i).second.negate();
  }
  balance_t negate() const {
    balance_t temp = *this;
    temp.in_place_negate();
    return temp;
  }
  balance_t operator-() const {
    return negate();
  }

  // conversion operators
  operator bool() const {
    for (amounts_map::const_iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      if ((*i).second)
	return true;
    return false;
  }

  bool is_realzero() const {
    if (amounts.size() == 0)
      return true;
    for (amounts_map::const_iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      if (! (*i).second.is_realzero())
	return false;
    return true;
  }

  optional<amount_t>  amount(const optional<const commodity_t&>& commodity =
			     optional<const commodity_t&>()) const;
  optional<balance_t> value(const optional<moment_t>& moment =
			    optional<moment_t>()) const;

  balance_t
  strip_annotations(const bool keep_price = amount_t::keep_price,
		    const bool keep_date  = amount_t::keep_date,
		    const bool keep_tag   = amount_t::keep_tag) const;

  void print(std::ostream& out, const int first_width,
	     const int latter_width = -1) const;

  balance_t abs() const {
    balance_t temp = *this;
    for (amounts_map::iterator i = temp.amounts.begin();
	 i != temp.amounts.end();
	 i++)
      (*i).second = (*i).second.abs();
    return temp;
  }

  void in_place_reduce() {
    for (amounts_map::iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      (*i).second.in_place_reduce();
  }
  balance_t reduce() const {
    balance_t temp(*this);
    temp.in_place_reduce();
    return temp;
  }

  void in_place_round() {
    for (amounts_map::iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      (*i).second = (*i).second.round();
  }
  balance_t round() const {
    balance_t temp(*this);
    temp.in_place_round();
    return temp;
  }

  balance_t unround() const {
    balance_t temp;
    for (amounts_map::const_iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      if ((*i).second.commodity())
	temp += (*i).second.unround();
    return temp;
  }

  bool valid() const {
    for (amounts_map::const_iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      if (! (*i).second.valid())
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
