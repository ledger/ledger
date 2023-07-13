/*
 * Copyright (c) 2003-2023, John Wiegley.  All rights reserved.
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

#include <system.hh>

#include "balance.h"
#include "commodity.h"
#include "annotate.h"
#include "pool.h"
#include "unistring.h"          // for justify()

namespace ledger {

balance_t::balance_t(const double val)
{
  amounts.insert
    (amounts_map::value_type(commodity_pool_t::current_pool->null_commodity, val));
  TRACE_CTOR(balance_t, "const double");
}

balance_t::balance_t(const unsigned long val)
{
  amounts.insert
    (amounts_map::value_type(commodity_pool_t::current_pool->null_commodity, val));
  TRACE_CTOR(balance_t, "const unsigned long");
}

balance_t::balance_t(const long val)
{
  amounts.insert
    (amounts_map::value_type(commodity_pool_t::current_pool->null_commodity, val));
  TRACE_CTOR(balance_t, "const long");
}

balance_t& balance_t::operator+=(const balance_t& bal)
{
  foreach (const amounts_map::value_type& pair, bal.amounts)
    *this += pair.second;
  return *this;
}

balance_t& balance_t::operator+=(const amount_t& amt)
{
  if (amt.is_null())
    throw_(balance_error,
           _("Cannot add an uninitialized amount to a balance"));

  if (amt.is_realzero())
    return *this;

  amounts_map::iterator i =
    amt.commodity().has_annotation() ?
    find_by_name(amt.commodity()) : amounts.find(&amt.commodity());
  if (i != amounts.end())
    i->second += amt;
  else
    amounts.insert(amounts_map::value_type(&amt.commodity(), amt));

  return *this;
}

balance_t& balance_t::operator-=(const balance_t& bal)
{
  foreach (const amounts_map::value_type& pair, bal.amounts)
    *this -= pair.second;
  return *this;
}

balance_t& balance_t::operator-=(const amount_t& amt)
{
  if (amt.is_null())
    throw_(balance_error,
           _("Cannot subtract an uninitialized amount from a balance"));

  if (amt.is_realzero())
    return *this;

  amounts_map::iterator i =
    amt.commodity().has_annotation() ?
    find_by_name(amt.commodity()) : amounts.find(&amt.commodity());
  if (i != amounts.end()) {
    i->second -= amt;
    if (i->second.is_realzero())
      amounts.erase(i);
  } else {
    amounts.insert(amounts_map::value_type(&amt.commodity(), amt.negated()));
  }
  return *this;
}

balance_t& balance_t::operator*=(const amount_t& amt)
{
  if (amt.is_null())
    throw_(balance_error,
           _("Cannot multiply a balance by an uninitialized amount"));

  if (is_realzero()) {
    ;
  }
  else if (amt.is_realzero()) {
    *this = amt;
  }
  else if (! amt.commodity()) {
    // Multiplying by an amount with no commodity causes all the
    // component amounts to be increased by the same factor.
    foreach (amounts_map::value_type& pair, amounts)
      pair.second *= amt;
  }
  else if (amounts.size() == 1) {
    // Multiplying by a commoditized amount is only valid if the sole
    // commodity in the balance is of the same kind as the amount's
    // commodity.
    if (*amounts.begin()->first == amt.commodity())
      amounts.begin()->second *= amt;
    else
      throw_(balance_error,
             _("Cannot multiply a balance with annotated commodities by a commoditized amount"));
  }
  else {
    assert(amounts.size() > 1);
    throw_(balance_error,
           _("Cannot multiply a multi-commodity balance by a commoditized amount"));
  }
  return *this;
}

balance_t& balance_t::operator/=(const amount_t& amt)
{
  if (amt.is_null())
    throw_(balance_error,
           _("Cannot divide a balance by an uninitialized amount"));

  if (is_realzero()) {
    ;
  }
  else if (amt.is_realzero()) {
    throw_(balance_error, _("Divide by zero"));
  }
  else if (! amt.commodity()) {
    // Dividing by an amount with no commodity causes all the
    // component amounts to be divided by the same factor.
    foreach (amounts_map::value_type& pair, amounts)
      pair.second /= amt;
  }
  else if (amounts.size() == 1) {
    // Dividing by a commoditized amount is only valid if the sole
    // commodity in the balance is of the same kind as the amount's
    // commodity.
    if (*amounts.begin()->first == amt.commodity())
      amounts.begin()->second /= amt;
    else
      throw_(balance_error,
             _("Cannot divide a balance with annotated commodities by a commoditized amount"));
  }
  else {
    assert(amounts.size() > 1);
    throw_(balance_error,
           _("Cannot divide a multi-commodity balance by a commoditized amount"));
  }
  return *this;
}

optional<balance_t>
balance_t::value(const datetime_t&   moment,
                 const commodity_t * in_terms_of) const
{
  balance_t temp;
  bool      resolved = false;

  foreach (const amounts_map::value_type& pair, amounts) {
    if (optional<amount_t> val = pair.second.value(moment, in_terms_of)) {
      temp += *val;
      resolved = true;
    } else {
      temp += pair.second;
    }
  }
  return resolved ? temp : optional<balance_t>();
}

balance_t::amounts_map::iterator
balance_t::find_by_name(const commodity_t& comm)
{
  for (amounts_map::iterator i = amounts.begin();
       i != amounts.end();
       i++) {
    if (*(*i).first == comm)
      return i;
  }
  return amounts.end();
}

balance_t::amounts_map::const_iterator
balance_t::find_by_name(const commodity_t& comm) const
{
  for (amounts_map::const_iterator i = amounts.begin();
       i != amounts.end();
       i++) {
    if (*(*i).first == comm)
      return i;
  }
  return amounts.end();
}

optional<amount_t>
balance_t::commodity_amount(const optional<const commodity_t&>& commodity) const
{
  if (! commodity) {
    if (amounts.size() == 1) {
      return amounts.begin()->second;
    }
    else if (amounts.size() > 1) {
      // Try stripping annotations before giving an error.
      balance_t temp(strip_annotations(keep_details_t()));
      if (temp.amounts.size() == 1)
        return temp.commodity_amount(commodity);

      throw_(amount_error,
             _f("Requested amount of a balance with multiple commodities: %1%")
             % temp);
    }
  }
  else if (amounts.size() > 0) {
    amounts_map::const_iterator i =
      commodity->has_annotation() ?
      find_by_name(*commodity) :
      amounts.find(const_cast<commodity_t *>(&*commodity));
    if (i != amounts.end())
      return i->second;
  }
  return none;
}

balance_t
balance_t::strip_annotations(const keep_details_t& what_to_keep) const
{
  balance_t temp;

  foreach (const amounts_map::value_type& pair, amounts)
    temp += pair.second.strip_annotations(what_to_keep);

  return temp;
}

void balance_t::sorted_amounts(amounts_array& sorted) const
{
  foreach (const amounts_map::value_type& pair, amounts)
    if (! pair.second.is_null())
      sorted.push_back(&pair.second);
  std::stable_sort(
    sorted.begin(), sorted.end(),
    [](const amount_t * left, const amount_t * right) {
      return commodity_t::compare_by_commodity()(left, right) < 0;
    });
}

void balance_t::map_sorted_amounts(function<void(const amount_t&)> fn) const
{
  if (! amounts.empty()) {
    if (amounts.size() == 1) {
      const amount_t& amount((*amounts.begin()).second);
      if (! amount.is_null())
        fn(amount);
    }
    else {
      amounts_array sorted;
      sorted_amounts(sorted);
      foreach (const amount_t * amount, sorted)
        fn(*amount);
    }
  }
}

namespace {
  struct print_amount_from_balance
  {
    std::ostream& out;
    bool&         first;
    int           fwidth;
    int           lwidth;
    uint_least8_t flags;

    explicit print_amount_from_balance(std::ostream& _out,
                                       bool& _first,
                                       int _fwidth, int _lwidth,
                                       uint_least8_t _flags)
      : out(_out), first(_first), fwidth(_fwidth), lwidth(_lwidth),
        flags(_flags) {
      TRACE_CTOR(print_amount_from_balance,
                 "ostream&, int, int, uint_least8_t");
    }
    print_amount_from_balance(const print_amount_from_balance& other)
      : out(other.out), first(other.first), fwidth(other.fwidth),
        lwidth(other.lwidth), flags(other.flags) {
      TRACE_CTOR(print_amount_from_balance, "copy");
    }
    ~print_amount_from_balance() throw() {
      TRACE_DTOR(print_amount_from_balance);
    }

    void operator()(const amount_t& amount) {
      if(amount.is_zero()) return;

      int width;
      if (! first) {
        out << std::endl;
        width = lwidth;
      } else {
        first = false;
        width = fwidth;
      }

      std::ostringstream buf;
      amount.print(buf, flags);

      justify(out, buf.str(), width,
              flags & AMOUNT_PRINT_RIGHT_JUSTIFY,
              flags & AMOUNT_PRINT_COLORIZE && amount.sign() < 0);
    }

    void close() {
      out.width(fwidth);
      if (flags & AMOUNT_PRINT_RIGHT_JUSTIFY)
        out << std::right;
      else
        out << std::left;
      out << 0;
    }
  };
}

void balance_t::print(std::ostream&       out,
                      const int           first_width,
                      const int           latter_width,
                      const uint_least8_t flags) const
{
  bool first = true;
  print_amount_from_balance
    amount_printer(out, first, first_width,
                   latter_width == 1 ? first_width : latter_width, flags);
  map_sorted_amounts(amount_printer);

  if (first)
    amount_printer.close();
}

void put_balance(property_tree::ptree& st, const balance_t& bal)
{
  foreach (const balance_t::amounts_map::value_type& pair, bal.amounts)
    put_amount(st.add("amount", ""), pair.second);
}

balance_t average_lot_prices(const balance_t& bal)
{
  // First, we split the balance into multiple balances by underlying
  // commodity.
  typedef std::map<optional<std::string>,
                   std::pair<amount_t, annotation_t> > balance_map;
  balance_map bycomm;

  foreach (const balance_t::amounts_map::value_type& pair, bal.amounts) {
    optional<std::string> sym(pair.first->symbol());
    amount_t quant(pair.second.strip_annotations(keep_details_t()));

    balance_map::iterator i = bycomm.find(sym);
    if (i == bycomm.end()) {
      bycomm.insert(
        balance_map::value_type(sym, std::make_pair(quant, annotation_t())));
      i = bycomm.find(sym);    // must succeed now
    } else {
      (*i).second.first += quant;
    }

    if (pair.first->has_annotation()) {
      annotated_commodity_t& acomm(static_cast<annotated_commodity_t&>(*pair.first));
      annotation_t& ann((*i).second.second);

      if (acomm.details.price) {
        if (ann.price)
          ann.price = *ann.price + (*acomm.details.price * quant);
        else
          ann.price = *acomm.details.price * quant;
      }

      if (acomm.details.date) {
        if (! ann.date || *acomm.details.date < *ann.date)
          ann.date = *acomm.details.date;
      }
    }
  }

  balance_t result;

  foreach (balance_map::value_type& pair, bycomm) {
    amount_t amt(pair.second.first);
    if (! amt.is_realzero()) {
      if (pair.second.second.price)
        pair.second.second.price = *pair.second.second.price / amt;

      commodity_t * acomm =
        commodity_pool_t::current_pool->find_or_create(
          amt.commodity(), pair.second.second);
      amt.set_commodity(*acomm);

      result += amt;
    }
  }

  return result;
}

} // namespace ledger
