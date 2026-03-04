/*
 * Copyright (c) 2003-2025, John Wiegley.  All rights reserved.
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
 * @file   balance.cc
 * @author John Wiegley
 *
 * @ingroup math
 *
 * @brief Implementation of balance_t, the multi-commodity balance type.
 *
 * A balance_t holds amounts across multiple commodities simultaneously,
 * something that amount_t cannot do (it throws if you add two amounts with
 * different commodities).  When a user runs @c ledger @c bal, each account
 * may hold a mix of dollars, euros, shares of stock, and so on; balance_t
 * is the container that aggregates them.
 *
 * Internally the amounts are stored in an unordered map keyed by commodity
 * pointer.  Arithmetic operators delegate to the per-commodity amount_t
 * operations, so all precision and rounding rules of amount_t are
 * preserved.  Multiplication and division are intentionally restricted:
 * you can scale every component by a plain number, but multiplying two
 * multi-commodity balances is undefined.
 *
 * balance_t sits between amount_t (single commodity) and value_t (the
 * polymorphic variant that also carries strings, dates, and sequences).
 * The reporting layer converts value_t to balance_t when it needs to
 * format multi-line commodity output for commands like @c balance and
 * @c register.
 */

#include <system.hh>

#include "balance.h"
#include "commodity.h"
#include "annotate.h"
#include "pool.h"
#include "unistring.h" // for justify()

namespace ledger {

/*----------------------------------------------------------------------*/
/*  Constructors from numeric types                                     */
/*----------------------------------------------------------------------*/

/// @brief Construct a balance from a double, stored under the null commodity.
balance_t::balance_t(const double val) {
  amounts.insert(amounts_map::value_type(commodity_pool_t::current_pool->null_commodity, val));
  TRACE_CTOR(balance_t, "const double");
}

/// @brief Construct a balance from an unsigned long, stored under the null commodity.
balance_t::balance_t(const unsigned long val) {
  amounts.insert(amounts_map::value_type(commodity_pool_t::current_pool->null_commodity, val));
  TRACE_CTOR(balance_t, "const unsigned long");
}

/// @brief Construct a balance from a signed long, stored under the null commodity.
balance_t::balance_t(const long val) {
  amounts.insert(amounts_map::value_type(commodity_pool_t::current_pool->null_commodity, val));
  TRACE_CTOR(balance_t, "const long");
}

/*----------------------------------------------------------------------*/
/*  Addition operators                                                  */
/*----------------------------------------------------------------------*/

/// @brief Add every component of another balance into this one.
///
/// Each commodity in @p bal is added independently.  If both balances
/// contain the same commodity, their amounts are summed; otherwise a new
/// commodity slot is created.  This is the core operation behind running
/// totals in @c ledger @c register when multiple commodities are involved.
balance_t& balance_t::operator+=(const balance_t& bal) {
  for (const amounts_map::value_type& pair : bal.amounts)
    *this += pair.second;
  return *this;
}

/// @brief Add a single amount into the balance.
///
/// If the amount's commodity already exists in the balance, its value is
/// accumulated.  For annotated commodities (e.g., shares purchased at a
/// specific price or date), lookup is done by commodity name rather than
/// pointer identity, so that @c AAPL {$120} and @c AAPL {$130} are
/// recognized as the same underlying commodity.  Amounts that are
/// precisely zero are silently ignored to keep the map sparse.
///
/// @throws balance_error if @p amt is uninitialized (null).
balance_t& balance_t::operator+=(const amount_t& amt) {
  if (amt.is_null())
    throw_(balance_error, _("Cannot add an uninitialized amount to a balance"));

  if (amt.is_realzero())
    return *this;

  amounts_map::iterator i = amt.commodity().has_annotation() ? find_by_name(amt.commodity())
                                                             : amounts.find(&amt.commodity());
  if (i != amounts.end())
    i->second += amt;
  else
    amounts.insert(amounts_map::value_type(&amt.commodity(), amt));

  return *this;
}

/*----------------------------------------------------------------------*/
/*  Subtraction operators                                               */
/*----------------------------------------------------------------------*/

/// @brief Subtract every component of another balance from this one.
///
/// Mirrors operator+=(const balance_t&), but subtracts each component.
/// If a commodity's amount reaches exactly zero after subtraction, the
/// entry is removed from the map to keep the balance sparse.
balance_t& balance_t::operator-=(const balance_t& bal) {
  for (const amounts_map::value_type& pair : bal.amounts)
    *this -= pair.second;
  return *this;
}

/// @brief Subtract a single amount from the balance.
///
/// If the commodity is already present, the amount is subtracted in place.
/// When the result is exactly zero (is_realzero), the entry is erased so
/// that the balance does not accumulate phantom zero-valued commodities.
/// If the commodity is not present, its negation is inserted instead.
///
/// @throws balance_error if @p amt is uninitialized (null).
balance_t& balance_t::operator-=(const amount_t& amt) {
  if (amt.is_null())
    throw_(balance_error, _("Cannot subtract an uninitialized amount from a balance"));

  if (amt.is_realzero())
    return *this;

  amounts_map::iterator i = amt.commodity().has_annotation() ? find_by_name(amt.commodity())
                                                             : amounts.find(&amt.commodity());
  if (i != amounts.end()) {
    i->second -= amt;
    if (i->second.is_realzero())
      amounts.erase(i);
  } else {
    amounts.insert(amounts_map::value_type(&amt.commodity(), amt.negated()));
  }
  return *this;
}

/*----------------------------------------------------------------------*/
/*  Multiplication and division operators                               */
/*----------------------------------------------------------------------*/

/// @brief Scale every component amount by a factor.
///
/// Multiplication is only well-defined in two cases:
///
/// 1. The multiplier has no commodity (a plain number).  Every component
///    in the balance is scaled by that factor.  This is used by
///    expressions like @c amount*2 in format strings.
///
/// 2. The balance contains exactly one commodity that matches the
///    multiplier's commodity.  This handles annotated-commodity balances
///    where all entries refer to the same underlying commodity.
///
/// Multiplying a multi-commodity balance by a commoditized amount is
/// mathematically undefined and throws balance_error.
///
/// @throws balance_error if @p amt is uninitialized, or if the operation
///         is not well-defined for the given commodity combination.
balance_t& balance_t::operator*=(const amount_t& amt) {
  if (amt.is_null())
    throw_(balance_error, _("Cannot multiply a balance by an uninitialized amount"));

  if (is_realzero()) {
    ;
  } else if (amt.is_realzero()) {
    *this = amt;
  } else if (!amt.commodity()) {
    // Multiplying by an amount with no commodity causes all the
    // component amounts to be increased by the same factor.
    for (amounts_map::value_type& pair : amounts)
      pair.second *= amt;
  } else if (amounts.size() == 1) {
    // Multiplying by a commoditized amount is only valid if the sole
    // commodity in the balance is of the same kind as the amount's
    // commodity.
    if (*amounts.begin()->first == amt.commodity())
      amounts.begin()->second *= amt;
    else
      throw_(balance_error,
             _("Cannot multiply a balance with annotated commodities by a commoditized amount"));
  } else {
    assert(amounts.size() > 1);
    throw_(balance_error, _("Cannot multiply a multi-commodity balance by a commoditized amount"));
  }
  return *this;
}

/// @brief Divide every component amount by a divisor.
///
/// Division follows the same rules as multiplication: the divisor must
/// either be uncommoditized (plain number) or match the single commodity
/// already in the balance.  Division by zero throws balance_error.
///
/// @throws balance_error if @p amt is uninitialized, zero, or if the
///         operation is not well-defined for the given commodity combination.
balance_t& balance_t::operator/=(const amount_t& amt) {
  if (amt.is_null())
    throw_(balance_error, _("Cannot divide a balance by an uninitialized amount"));

  // NOLINTBEGIN(bugprone-branch-clone)
  if (is_realzero()) {
    ;
  } else if (amt.is_realzero()) {
    throw_(balance_error, _("Divide by zero"));
  } else if (!amt.commodity()) {
    // Dividing by an amount with no commodity causes all the
    // component amounts to be divided by the same factor.
    for (amounts_map::value_type& pair : amounts)
      pair.second /= amt;
  } else if (amounts.size() == 1) {
    // Dividing by a commoditized amount is only valid if the sole
    // commodity in the balance is of the same kind as the amount's
    // commodity.
    if (*amounts.begin()->first == amt.commodity())
      amounts.begin()->second /= amt;
    else
      throw_(balance_error,
             _("Cannot divide a balance with annotated commodities by a commoditized amount"));
  } else {
    assert(amounts.size() > 1);
    throw_(balance_error, _("Cannot divide a multi-commodity balance by a commoditized amount"));
  }
  // NOLINTEND(bugprone-branch-clone)
  return *this;
}

/*----------------------------------------------------------------------*/
/*  Market valuation                                                    */
/*----------------------------------------------------------------------*/

/// @brief Reprice every component amount at its market value.
///
/// Each commodity in the balance is individually repriced via
/// amount_t::value(), which consults the commodity price history.
/// If @p in_terms_of is provided, all amounts are converted to that
/// target commodity; otherwise each amount uses its own most recent
/// price.
///
/// This is the engine behind @c --market and @c --exchange on the
/// command line.  For example, @c ledger @c bal @c --market converts
/// stock holdings into their current dollar value by calling this method
/// with the current datetime.
///
/// @param moment     The point in time at which to look up prices.
///                   A default-constructed datetime uses the most recent
///                   known price.
/// @param in_terms_of  Optional target commodity for conversion (e.g.,
///                     the user's preferred reporting currency).
///
/// @return A new balance with repriced amounts, or boost::none if no
///         commodity could be repriced (i.e., no price data exists).
optional<balance_t> balance_t::value(const datetime_t& moment,
                                     const commodity_t* in_terms_of) const {
  balance_t temp;
  bool resolved = false;

  for (const amounts_map::value_type& pair : amounts) {
    if (auto val = pair.second.value(moment, in_terms_of)) {
      temp += *val;
      resolved = true;
    } else {
      temp += pair.second;
    }
  }
  return resolved ? temp : optional<balance_t>();
}

/*----------------------------------------------------------------------*/
/*  Commodity lookup by name                                            */
/*----------------------------------------------------------------------*/

/// @brief Find a commodity entry by name equality rather than pointer identity.
///
/// Annotated commodities (e.g., @c AAPL {$120} vs. @c AAPL {$130})
/// have distinct commodity_t pointers even though they share the same
/// base symbol.  This helper performs a linear scan comparing via
/// operator== on the commodity, which checks the underlying symbol.
/// It is used by operator+= and operator-= when the incoming amount
/// has an annotated commodity.
///
/// @return An iterator to the matching entry, or amounts.end() if not found.
balance_t::amounts_map::iterator balance_t::find_by_name(const commodity_t& comm) {
  for (amounts_map::iterator i = amounts.begin(); i != amounts.end(); i++) {
    if (*(*i).first == comm)
      return i;
  }
  return amounts.end();
}

/// @brief Const overload of find_by_name.
/// @see find_by_name(const commodity_t&)
balance_t::amounts_map::const_iterator balance_t::find_by_name(const commodity_t& comm) const {
  for (amounts_map::const_iterator i = amounts.begin(); i != amounts.end(); i++) {
    if (*(*i).first == comm)
      return i;
  }
  return amounts.end();
}

/*----------------------------------------------------------------------*/
/*  Single-commodity extraction                                         */
/*----------------------------------------------------------------------*/

/// @brief Extract the amount for a specific commodity from the balance.
///
/// If no commodity is specified and the balance contains exactly one
/// commodity, that amount is returned.  If the balance is multi-commodity
/// and no commodity is specified, the method first tries stripping
/// annotations (collapsing @c AAPL {$120} and @c AAPL {$130} into plain
/// @c AAPL); if the result is still multi-commodity, it throws.
///
/// This is commonly used when a value expression needs a single amount_t
/// from what might be a multi-commodity balance.
///
/// @param commodity  The commodity to look up.  If boost::none, returns
///                   the sole commodity or throws on ambiguity.
///
/// @return The matching amount, or boost::none if the commodity is not
///         present in the balance.
///
/// @throws amount_error if no commodity is specified and the balance
///         contains multiple distinct commodities after annotation stripping.
optional<amount_t>
balance_t::commodity_amount(const optional<const commodity_t&>& commodity) const {
  if (!commodity) {
    if (amounts.size() == 1) {
      return amounts.begin()->second;
    } else if (amounts.size() > 1) {
      // Try stripping annotations before giving an error.
      balance_t temp(strip_annotations(keep_details_t()));
      if (temp.amounts.size() == 1)
        return temp.commodity_amount(commodity);

      throw_(amount_error,
             _f("Requested amount of a balance with multiple commodities: %1%") % temp);
    }
  } else if (amounts.size() > 0) {
    amounts_map::const_iterator i = commodity->has_annotation()
                                        ? find_by_name(*commodity)
                                        : amounts.find(const_cast<commodity_t*>(&*commodity));
    if (i != amounts.end())
      return i->second;
  }
  return none;
}

/*----------------------------------------------------------------------*/
/*  Annotation stripping                                                */
/*----------------------------------------------------------------------*/

/// @brief Return a copy of this balance with commodity annotations removed.
///
/// Annotations are the lot-price, lot-date, and lot-tag metadata
/// attached to commodities (e.g., the @c {$120} and @c [2024-01-15]
/// parts of @c 10 @c AAPL @c {$120} @c [2024-01-15]).  Stripping them
/// collapses distinct lots of the same commodity into a single entry.
///
/// The @p what_to_keep parameter controls which annotation details are
/// preserved.  Passing a default-constructed keep_details_t() strips
/// everything; the @c --lots flag on the command line causes prices,
/// dates, and tags to be kept.
///
/// A fast path avoids the expensive GMP arithmetic of rebuilding the
/// balance when no amounts actually need stripping.
///
/// @param what_to_keep  Specifies which annotation fields to preserve.
/// @return A new balance with annotations stripped as requested.
balance_t balance_t::strip_annotations(const keep_details_t& what_to_keep) const {
  // Fast path: if no amounts have annotations that need stripping,
  // return *this directly to avoid the expensive GMP arithmetic of
  // rebuilding the balance via operator+=.
  for (const amounts_map::value_type& pair : amounts) {
    if (!what_to_keep.keep_all(pair.second.commodity())) {
      // At least one amount needs stripping; fall through to rebuild.
      // Preserve sort_order so FIFO/LIFO display preference is maintained.
      balance_t temp;
      temp.sort_order = sort_order;
      for (const amounts_map::value_type& pair2 : amounts)
        temp += pair2.second.strip_annotations(what_to_keep);
      return temp;
    }
  }

  return *this;
}

/*----------------------------------------------------------------------*/
/*  Sorting and iteration                                               */
/*----------------------------------------------------------------------*/

/// @brief Populate an array of amount pointers sorted for deterministic display.
///
/// The amounts_map is an unordered_map, so its iteration order is
/// non-deterministic.  This method collects all non-null amounts into
/// @p sorted and then sorts them so that @c ledger @c bal always prints
/// commodities in a stable, predictable order.
///
/// The sort order depends on the balance's lot_sort_order setting:
///
/// - @c by_commodity (default): alphabetical by commodity symbol, which
///   is what users see with a plain @c ledger @c bal.
///
/// - @c fifo: lots of the same commodity are ordered oldest-first by
///   their annotation date.  Used by @c --lot-dates @c --lots to show
///   positions in acquisition order.
///
/// - @c lifo: lots ordered newest-first.  Used for last-in-first-out
///   cost-basis reporting.
///
/// Within each mode, unannotated amounts sort before annotated ones,
/// and a secondary sort by commodity_t::compare_by_commodity provides
/// a stable tiebreaker.
///
/// @param[out] sorted  The vector to fill with sorted amount pointers.
void balance_t::sorted_amounts(amounts_array& sorted) const {
  for (const amounts_map::value_type& pair : amounts)
    if (!pair.second.is_null())
      sorted.push_back(&pair.second);

  if (sort_order == lot_sort_order::fifo || sort_order == lot_sort_order::lifo) {
    const bool ascending = (sort_order == lot_sort_order::fifo);
    std::stable_sort(
        sorted.begin(), sorted.end(), [ascending](const amount_t* left, const amount_t* right) {
          commodity_t& lc = left->commodity();
          commodity_t& rc = right->commodity();

          // Primary sort: base symbol (same as compare_by_commodity)
          int cmp = lc.base_symbol().compare(rc.base_symbol());
          if (cmp != 0)
            return cmp < 0;

          // Unannotated lots sort before annotated ones
          if (!lc.has_annotation() && rc.has_annotation())
            return true;
          if (lc.has_annotation() && !rc.has_annotation())
            return false;
          if (!lc.has_annotation() && !rc.has_annotation())
            return false;

          // Both annotated: compare by date first (FIFO=ascending, LIFO=descending)
          const annotated_commodity_t& alc = static_cast<const annotated_commodity_t&>(lc);
          const annotated_commodity_t& arc = static_cast<const annotated_commodity_t&>(rc);

          if (!alc.details.date && arc.details.date)
            return ascending; // no date sorts first for FIFO, last for LIFO
          if (alc.details.date && !arc.details.date)
            return !ascending;
          if (alc.details.date && arc.details.date && *alc.details.date != *arc.details.date)
            return ascending ? *alc.details.date < *arc.details.date
                             : *alc.details.date > *arc.details.date;

          // Dates equal (or both absent): fall back to standard commodity ordering
          return commodity_t::compare_by_commodity()(left, right) < 0;
        });
  } else {
    std::stable_sort(sorted.begin(), sorted.end(), [](const amount_t* left, const amount_t* right) {
      return commodity_t::compare_by_commodity()(left, right) < 0;
    });
  }
}

/// @brief Visit each amount in sorted commodity order via a callback.
///
/// This is a convenience wrapper around sorted_amounts() that handles
/// the common single-amount fast path (no sorting needed) and then
/// invokes @p fn for each amount.  It is the primary iteration
/// primitive used by print() and the report formatters.
///
/// @param fn  A callable invoked once per non-null amount, in sorted order.
void balance_t::map_sorted_amounts(const function<void(const amount_t&)>& fn) const {
  if (!amounts.empty()) {
    if (amounts.size() == 1) {
      const amount_t& amount((*amounts.begin()).second);
      if (!amount.is_null())
        fn(amount);
    } else {
      amounts_array sorted;
      sorted_amounts(sorted);
      for (const amount_t* amount : sorted)
        fn(*amount);
    }
  }
}

/*----------------------------------------------------------------------*/
/*  Printing                                                            */
/*----------------------------------------------------------------------*/

namespace {

/// @brief Functor that formats individual amounts within a balance for output.
///
/// Used internally by balance_t::print() to render each commodity on its
/// own line, right- or left-justified to the requested column widths.
/// The first amount uses @c fwidth; subsequent amounts use @c lwidth
/// (which may differ to accommodate column headers in register reports).
///
/// Zero-valued amounts are suppressed so that a balance containing only
/// a zero remainder does not produce blank output lines.  When all
/// amounts are zero, the close() method emits a single "0".
struct print_amount_from_balance {
  std::ostream& out;
  bool& first;
  int fwidth;
  int lwidth;
  uint_least8_t flags;

  explicit print_amount_from_balance(std::ostream& _out, bool& _first, int _fwidth, int _lwidth,
                                     uint_least8_t _flags)
      : out(_out), first(_first), fwidth(_fwidth), lwidth(_lwidth), flags(_flags) {
    TRACE_CTOR(print_amount_from_balance, "ostream&, int, int, uint_least8_t");
  }
  print_amount_from_balance(const print_amount_from_balance& other)
      : out(other.out), first(other.first), fwidth(other.fwidth), lwidth(other.lwidth),
        flags(other.flags) {
    TRACE_CTOR(print_amount_from_balance, "copy");
  }
  ~print_amount_from_balance() noexcept { TRACE_DTOR(print_amount_from_balance); }

  /// @brief Format and output a single amount, justified to the column width.
  void operator()(const amount_t& amount) {
    if (amount.is_zero())
      return;

    int width;
    if (!first) {
      out << '\n';
      width = lwidth;
    } else {
      first = false;
      width = fwidth;
    }

    std::ostringstream buf;
    amount.print(buf, flags);

    justify(out, buf.str(), width, flags & AMOUNT_PRINT_RIGHT_JUSTIFY,
            flags & AMOUNT_PRINT_COLORIZE && amount.sign() < 0);
  }

  /// @brief Emit a zero placeholder when the balance is entirely zero.
  void close() {
    out.width(fwidth);
    if (flags & AMOUNT_PRINT_RIGHT_JUSTIFY)
      out << std::right;
    else
      out << std::left;
    out << 0;
  }
};
} // namespace

/// @brief Print the balance to an output stream.
///
/// Each commodity in the balance is printed on its own line, sorted by
/// commodity name (or by lot date if FIFO/LIFO ordering is active).
/// The @p first_width parameter controls the column width for the first
/// line; @p latter_width controls subsequent lines, which is useful when
/// the balance appears in a right-aligned column of a register report.
///
/// If the balance is empty or all amounts are zero, a single "0" is
/// printed, justified to @p first_width.
///
/// @param out           The output stream.
/// @param first_width   Column width for the first commodity line.
/// @param latter_width  Column width for subsequent lines (defaults to
///                      first_width if set to 1).
/// @param flags         Formatting flags (right-justify, colorize, etc.).
void balance_t::print(std::ostream& out, const int first_width, const int latter_width,
                      const uint_least8_t flags) const {
  bool first = true;
  print_amount_from_balance amount_printer(out, first, first_width,
                                           latter_width == 1 ? first_width : latter_width, flags);
  map_sorted_amounts(amount_printer);

  if (first)
    amount_printer.close();
}

/*----------------------------------------------------------------------*/
/*  Serialization                                                       */
/*----------------------------------------------------------------------*/

/// @brief Serialize a balance into a Boost property tree for XML/JSON output.
///
/// Each component amount is added as a child "amount" node, enabling
/// structured output via @c ledger @c xml or @c ledger @c json.
///
/// @param st   The property tree node to populate.
/// @param bal  The balance to serialize.
void put_balance(property_tree::ptree& st, const balance_t& bal) {
  for (const balance_t::amounts_map::value_type& pair : bal.amounts)
    put_amount(st.add("amount", ""), pair.second);
}

/*----------------------------------------------------------------------*/
/*  Lot pricing strategies                                              */
/*----------------------------------------------------------------------*/

/// @brief Collapse annotated lots into a single average-priced entry per commodity.
///
/// Given a balance that may contain multiple lots of the same commodity
/// at different prices (e.g., 10 AAPL {$120} and 5 AAPL {$130}), this
/// function computes the weighted average price per share and returns a
/// new balance with a single annotated entry per commodity.
///
/// The algorithm:
/// 1. Groups all lots by their base commodity symbol.
/// 2. Sums the quantities (stripped of annotations) within each group.
/// 3. Accumulates the total cost (price * quantity) for each group.
/// 4. Divides total cost by total quantity to get the average price.
/// 5. Uses the earliest lot date as the annotation date.
///
/// This is invoked by the @c --average-lot-prices option to simplify
/// multi-lot displays into a single line per commodity.
///
/// @param bal  The input balance, potentially containing multiple annotated lots.
/// @return A new balance with at most one entry per base commodity, annotated
///         with the weighted average price and the earliest acquisition date.
balance_t average_lot_prices(const balance_t& bal) {
  // First, we split the balance into multiple balances by underlying
  // commodity.
  typedef std::map<optional<std::string>, std::pair<amount_t, annotation_t>> balance_map;
  balance_map bycomm;

  for (const balance_t::amounts_map::value_type& pair : bal.amounts) {
    optional<std::string> sym(pair.first->symbol());
    amount_t quant(pair.second.strip_annotations(keep_details_t()));

    auto [i, inserted] =
        bycomm.insert(balance_map::value_type(sym, std::make_pair(quant, annotation_t())));
    if (!inserted)
      i->second.first += quant;

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
        if (!ann.date || *acomm.details.date < *ann.date)
          ann.date = *acomm.details.date;
      }
    }
  }

  balance_t result;

  for (balance_map::value_type& pair : bycomm) {
    amount_t amt(pair.second.first);
    if (!amt.is_realzero()) {
      if (pair.second.second.price)
        pair.second.second.price = *pair.second.second.price / amt;

      commodity_t* acomm =
          commodity_pool_t::current_pool->find_or_create(amt.commodity(), pair.second.second);
      amt.set_commodity(*acomm);

      result += amt;
    }
  }

  return result;
}

/// @brief Return a copy of the balance with FIFO (first-in, first-out) lot ordering.
///
/// Sets the sort_order to @c fifo so that sorted_amounts() will arrange
/// lots from earliest to latest acquisition date.  Used by the
/// @c --lot-dates display option to show the oldest positions first.
///
/// @param bal  The input balance.
/// @return A copy with lot_sort_order::fifo.
balance_t fifo_lot_prices(const balance_t& bal) {
  balance_t result(bal);
  result.sort_order = balance_t::lot_sort_order::fifo;
  return result;
}

/// @brief Return a copy of the balance with LIFO (last-in, first-out) lot ordering.
///
/// Sets the sort_order to @c lifo so that sorted_amounts() will arrange
/// lots from latest to earliest acquisition date.
///
/// @param bal  The input balance.
/// @return A copy with lot_sort_order::lifo.
balance_t lifo_lot_prices(const balance_t& bal) {
  balance_t result(bal);
  result.sort_order = balance_t::lot_sort_order::lifo;
  return result;
}

} // namespace ledger
