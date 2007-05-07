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

#include "balance.h"

namespace ledger {

balance_t& balance_t::operator*=(const balance_t& bal)
{
  if (is_realzero()) {
    return *this;
  }
  else if (bal.is_realzero()) {
    return *this = bal;
  }
  else if (bal.amounts.size() == 1) {
    return *this *= (*bal.amounts.begin()).second;
  }
  else if (amounts.size() == 1) {
    return *this = bal * *this;
  }
  else {
    // Since we would fail with an error at this point otherwise, try
    // stripping annotations to see if we can come up with a
    // reasonable result.  The user will not notice any annotations
    // missing (since they are viewing a stripped report anyway), only
    // that some of their value expression may not see any pricing or
    // date data because of this operation.

    balance_t temp(bal.strip_annotations());
    if (temp.amounts.size() == 1)
      return *this *= temp;
    temp = strip_annotations();
    if (temp.amounts.size() == 1)
      return *this = bal * temp;

    throw_(amount_error, "Cannot multiply two balances: " << temp << " * " << bal);
  }
}

balance_t& balance_t::operator*=(const amount_t& amt)
{
  if (is_realzero()) {
    return *this;
  }
  else if (amt.is_realzero()) {
    return *this = amt;
  }
  else if (! amt.commodity()) {
    // Multiplying by the null commodity causes all amounts to be
    // increased by the same factor.
    for (amounts_map::iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      (*i).second *= amt;
  }
  else if (amounts.size() == 1) {
    *this = (*amounts.begin()).second * amt;
  }
  else {
    amounts_map::iterator i = amounts.find(&amt.commodity());
    if (i != amounts.end()) {
      (*i).second *= amt;
    } else {
      // Try stripping annotations before giving an error.
      balance_t temp(strip_annotations());
      if (temp.amounts.size() == 1) {
	return *this = (*temp.amounts.begin()).second * amt;
      } else {
	i = temp.amounts.find(&amt.commodity());
	if (i != temp.amounts.end())
	  return *this = temp * amt;
      }

      throw_(amount_error, "Attempt to multiply balance by a commodity" <<
	     " not found in that balance: " << temp << " * " << amt);
    }
  }
  return *this;
}

balance_t& balance_t::operator/=(const balance_t& bal)
{
  if (bal.is_realzero()) {
    throw_(amount_error, "Divide by zero: " << *this << " / " << bal);
  }
  else if (is_realzero()) {
    return *this;
  }
  else if (bal.amounts.size() == 1) {
    return *this /= (*bal.amounts.begin()).second;
  }
  else if (*this == bal) {
    return *this = amount_t(1L);
  }
  else {
    // Try stripping annotations before giving an error.
    balance_t temp(bal.strip_annotations());
    if (temp.amounts.size() == 1)
      return *this /= temp;

    throw_(amount_error,
	   "Cannot divide two balances: " << temp << " / " << bal);
  }
}

balance_t& balance_t::operator/=(const amount_t& amt)
{
  if (amt.is_realzero()) {
    throw_(amount_error, "Divide by zero: " << *this << " / " << amt);
  }
  else if (is_realzero()) {
    return *this;
  }
  else if (! amt.commodity()) {
    // Dividing by the null commodity causes all amounts to be
    // decreased by the same factor.
    for (amounts_map::iterator i = amounts.begin();
	 i != amounts.end();
	 i++)
      (*i).second /= amt;
  }
  else if (amounts.size() == 1 &&
	   (*amounts.begin()).first == &amt.commodity()) {
    (*amounts.begin()).second /= amt;
  }
  else {
    amounts_map::iterator i = amounts.find(&amt.commodity());
    if (i != amounts.end()) {
      (*i).second /= amt;
    } else {
      // Try stripping annotations before giving an error.
      balance_t temp(strip_annotations());
      if (temp.amounts.size() == 1 &&
	  (*temp.amounts.begin()).first == &amt.commodity())
	return *this = temp / amt;

      throw_(amount_error, "Attempt to divide balance by a commodity" <<
	     " not found in that balance: " << temp << " * " << amt);
    }
  }
  return *this;
}

optional<amount_t>
balance_t::amount(const optional<const commodity_t&>& commodity) const
{
  if (! commodity) {
    if (amounts.size() == 1) {
      amounts_map::const_iterator i = amounts.begin();
      return (*i).second;
    }
    else if (amounts.size() > 1) {
      // Try stripping annotations before giving an error.
      balance_t temp(strip_annotations());
      if (temp.amounts.size() == 1)
	return temp.amount(commodity);

      throw_(amount_error,
	     "Requested amount of a balance with multiple commodities: " << temp);
    }
  }
  else if (amounts.size() > 0) {
    amounts_map::const_iterator i = amounts.find(&*commodity);
    if (i != amounts.end())
      return (*i).second;
  }
  return optional<amount_t>();
}

optional<balance_t>
balance_t::value(const optional<moment_t>& moment) const
{
  optional<balance_t> temp;

  for (amounts_map::const_iterator i = amounts.begin();
       i != amounts.end();
       i++)
    if (optional<amount_t> val = (*i).second.value(moment)) {
      if (! temp)
	temp = balance_t();
      *temp += *val;
    }

  return temp;
}

balance_t balance_t::strip_annotations(const bool keep_price,
				       const bool keep_date,
				       const bool keep_tag) const
{
  balance_t temp;

  for (amounts_map::const_iterator i = amounts.begin();
       i != amounts.end();
       i++)
    temp += (*i).second.strip_annotations(keep_price, keep_date, keep_tag);

  return temp;
}

void balance_t::print(std::ostream& out,
		      const int     first_width,
		      const int     latter_width) const
{
  bool first  = true;
  int  lwidth = latter_width;

  if (lwidth == -1)
    lwidth = first_width;

  typedef std::vector<const amount_t *> amounts_array;
  amounts_array sorted;

  for (amounts_map::const_iterator i = amounts.begin();
       i != amounts.end();
       i++)
    if ((*i).second)
      sorted.push_back(&(*i).second);

  std::stable_sort(sorted.begin(), sorted.end(),
		   compare_amount_commodities());

  for (amounts_array::const_iterator i = sorted.begin();
       i != sorted.end();
       i++) {
    int width;
    if (! first) {
      out << std::endl;
      width = lwidth;
    } else {
      first = false;
      width = first_width;
    }

    out.width(width);
    out.fill(' ');
    out << std::right << **i;
  }

  if (first) {
    out.width(first_width);
    out.fill(' ');
    out << std::right << "0";
  }
}

#if 0
balance_t::operator amount_t() const
{
  if (amounts.size() == 1) {
    return (*amounts.begin()).second;
  }
  else if (amounts.size() == 0) {
    return amount_t();
  }
  else {
    // Try stripping annotations before giving an error.
    balance_t temp(strip_annotations());
    if (temp.amounts.size() == 1)
      return (*temp.amounts.begin()).second;

    throw_(amount_error,
	   "Cannot convert a balance with " <<
	   "multiple commodities to an amount: " << temp);
  }
}
#endif

} // namespace ledger
