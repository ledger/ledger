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
  amounts = std::move(amounts).set(commodity_pool_t::current_pool->null_commodity, val);
  TRACE_CTOR(balance_t, "const double");
}

/// @brief Construct a balance from an unsigned long, stored under the null commodity.
balance_t::balance_t(const unsigned long val) {
  amounts = std::move(amounts).set(commodity_pool_t::current_pool->null_commodity, val);
  TRACE_CTOR(balance_t, "const unsigned long");
}

/// @brief Construct a balance from a signed long, stored under the null commodity.
balance_t::balance_t(const long val) {
  amounts = std::move(amounts).set(commodity_pool_t::current_pool->null_commodity, val);
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
  // Walk a snapshot of the other map (O(1): the nodes are shared), so that
  // when @p bal is *this the additions copy the nodes they change instead
  // of rewriting the ones being walked.
  const amounts_map other(bal.amounts);
  for (const amounts_map::value_type& pair : other)
    *this += pair.second;
  return *this;
}

/// @brief Add a single amount into the balance.
///
/// If the amount's commodity already exists in the balance, its value is
/// accumulated.  Commodities are compared by identity: the commodity pool
/// holds exactly one commodity_t per distinct symbol and per distinct
/// (symbol, annotation) pair (commodity_pool_t::annotated_commodities), so
/// two amounts of the same lot -- say @c AAPL {$120} [2024/01/02] -- share
/// a pointer, and a hash lookup replaces the former linear scan by name
/// (which made balances holding K lots cost O(K) per addition).  Lots the
/// pool has had to give equal annotations are the exception, handled by
/// component_key(); a lot whose annotation the Python bindings edit in
/// place stays a component of its own.  Amounts that are precisely zero
/// are silently ignored to keep the map sparse.
///
/// @throws balance_error if @p amt is uninitialized (null).
balance_t& balance_t::operator+=(const amount_t& amt) {
  if (amt.is_null())
    throw_(balance_error, _("Cannot add an uninitialized amount to a balance"));

  if (amt.is_realzero())
    return *this;

  // When this balance owns its map outright, immer hands the lambda the
  // stored component itself (as an rvalue) and the addition happens in
  // place; a component of a shared map arrives const and is copied first.
  // amount_t has no move constructor, so a by-value parameter would copy
  // either way and force operator+= to duplicate the bigint every time.
  commodity_t* comm = component_key(amt.commodity());
  bool found = false;
  amounts = std::move(amounts).update_if_exists(comm, [&amt, &found](auto&& current) -> amount_t {
    found = true;
    if constexpr (std::is_const_v<std::remove_reference_t<decltype(current)>>) {
      amount_t sum(current);
      sum += amt;
      return sum;
    } else {
      current += amt;
      return current;
    }
  });
  if (!found)
    amounts = std::move(amounts).set(comm, amt);

  return *this;
}

/// @brief Add @p amt onto any existing component of its commodity, keeping
/// full precision if either part has it (see the declaration).
///
/// amount_t::operator+= keeps the left operand's precision flags, so the
/// part that keeps its full precision is made the left operand; precision
/// itself is a maximum and the cost-precision flag is cleared by any
/// addition, so nothing else about the sum depends on the order.
balance_t& balance_t::merge(const amount_t& amt) {
  if (amt.is_null())
    throw_(balance_error, _("Cannot add an uninitialized amount to a balance"));

  if (amt.is_realzero())
    return *this;

  commodity_t* comm = component_key(amt.commodity());
  bool found = false;
  amounts = std::move(amounts).update_if_exists(comm, [&amt, &found](auto&& current) -> amount_t {
    found = true;
    if (amt.keep_precision() && !current.keep_precision()) {
      amount_t sum(amt);
      sum += current;
      return sum;
    }
    if constexpr (std::is_const_v<std::remove_reference_t<decltype(current)>>) {
      amount_t sum(current);
      sum += amt;
      return sum;
    } else {
      current += amt;
      return current;
    }
  });
  if (!found)
    amounts = std::move(amounts).set(comm, amt);

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
  const amounts_map other(bal.amounts); // see operator+=
  for (const amounts_map::value_type& pair : other)
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

  // In place when the map is uniquely owned, as in operator+=.
  commodity_t* comm = component_key(amt.commodity());
  bool found = false;
  bool zero = false;
  amounts =
      std::move(amounts).update_if_exists(comm, [&amt, &found, &zero](auto&& current) -> amount_t {
        found = true;
        if constexpr (std::is_const_v<std::remove_reference_t<decltype(current)>>) {
          amount_t difference(current);
          difference -= amt;
          zero = difference.is_realzero();
          return difference;
        } else {
          current -= amt;
          zero = current.is_realzero();
          return current;
        }
      });
  if (!found)
    amounts = std::move(amounts).set(comm, amt.negated());
  else if (zero)
    amounts = std::move(amounts).erase(comm);
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
    transform_amounts([&amt](amount_t& component) { component *= amt; });
  } else if (amounts.size() == 1) {
    // Multiplying by a commoditized amount is only valid if the sole
    // commodity in the balance is of the same kind as the amount's
    // commodity.
    if (*amounts.begin()->first == amt.commodity())
      transform_amounts([&amt](amount_t& component) { component *= amt; });
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
    transform_amounts([&amt](amount_t& component) { component /= amt; });
  } else if (amounts.size() == 1) {
    // Dividing by a commoditized amount is only valid if the sole
    // commodity in the balance is of the same kind as the amount's
    // commodity.
    if (*amounts.begin()->first == amt.commodity())
      transform_amounts([&amt](amount_t& component) { component /= amt; });
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

namespace {
/// @brief Rebuild a balance from transformed components without depending
/// on the component map's iteration order.
///
/// value() and strip_annotations() may map several components onto one
/// commodity (lots stripped to their base, or valued in one currency);
/// balance_t::merge() makes the result of such a collapse independent of
/// the order the components are visited in (a cost kept at full precision
/// and a lot of the same commodity in a register's --gain total used to
/// print with either one's precision from one run to the next), and
/// balance_t::visit_components() makes the error of a failed valuation
/// name the same component on every run.
template <typename Fn>
void merge_components(balance_t& into, const balance_t& from, Fn&& transform) {
  from.visit_components(
      [&into, &transform](const amount_t& amount) { into.merge(transform(amount)); });
}
} // namespace

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

  merge_components(temp, *this, [&](const amount_t& amount) {
    if (auto val = amount.value(moment, in_terms_of)) {
      resolved = true;
      return *val;
    }
    return amount;
  });
  return resolved ? temp : optional<balance_t>();
}

/*----------------------------------------------------------------------*/
/*  Component lookup                                                    */
/*----------------------------------------------------------------------*/

/// @brief The key under which amounts of @p comm are kept (see the
/// declaration).
///
/// Identity, unless @p comm is one of the lots the pool has had to give
/// equal annotations (commodity_pool_t::duplicate_lots): then the member of
/// that class this balance already holds, else the lot that carried the
/// annotation first, so that the class is one component, found by
/// annotation equality as every lot used to be.  The lookup costs one hash
/// probe for any other lot, and one per member of the class for these.
commodity_t* balance_t::component_key(const commodity_t& comm) const {
  commodity_t* key = const_cast<commodity_t*>(&comm);
  if (!comm.has_annotation() || !commodity_pool_t::current_pool)
    return key;
  const auto& duplicates(commodity_pool_t::current_pool->duplicate_lots);
  if (duplicates.empty())
    return key;
  auto found = duplicates.find(key);
  if (found == duplicates.end() || amounts.find(key))
    return key;
  for (const commodity_t* member : *found->second)
    if (member != key && amounts.find(const_cast<commodity_t*>(member)))
      return const_cast<commodity_t*>(member);
  return const_cast<commodity_t*>(found->second->front());
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
    if (const amount_t* found = amounts.find(component_key(*commodity)))
      return *found;
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
      merge_components(temp, *this, [&what_to_keep](const amount_t& amount) {
        return amount.strip_annotations(what_to_keep);
      });
      return temp;
    }
  }

  return *this;
}

/*----------------------------------------------------------------------*/
/*  Sorting and iteration                                               */
/*----------------------------------------------------------------------*/

namespace {
/// The strict weak ordering behind balance_t::sorted_amounts(): lots of one
/// commodity by annotation date when a FIFO or LIFO order is requested, and
/// otherwise (and to break ties) commodity_t::compare_by_commodity.
bool sorts_before(const amount_t* left, const amount_t* right, balance_t::lot_sort_order order) {
  if (order == balance_t::lot_sort_order::fifo || order == balance_t::lot_sort_order::lifo) {
    const bool ascending = (order == balance_t::lot_sort_order::fifo);
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
  }
  return commodity_t::compare_by_commodity()(left, right) < 0;
}
} // namespace

/// @brief Populate an array of amount pointers sorted for deterministic display.
///
/// The amounts_map is a hash map, so its iteration order is
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

  std::stable_sort(sorted.begin(), sorted.end(),
                   [order = sort_order](const amount_t* left, const amount_t* right) {
                     return sorts_before(left, right, order);
                   });
}

/// @brief The amount sorted_amounts() would list first, without sorting.
///
/// A strict scan keeps the first minimal element in iteration order, which
/// is the element stable_sort would put first.
const amount_t* balance_t::first_sorted_amount() const {
  const amount_t* top = nullptr;
  for (const amounts_map::value_type& pair : amounts)
    if (!pair.second.is_null() && (!top || sorts_before(&pair.second, top, sort_order)))
      top = &pair.second;
  return top;
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
  const string& negative_color;
  std::ostringstream buf; ///< Reused for every amount; constructing a stream is costly.

  explicit print_amount_from_balance(std::ostream& _out, bool& _first, int _fwidth, int _lwidth,
                                     uint_least8_t _flags, const string& _negative_color)
      : out(_out), first(_first), fwidth(_fwidth), lwidth(_lwidth), flags(_flags),
        negative_color(_negative_color) {
    TRACE_CTOR(print_amount_from_balance, "ostream&, int, int, uint_least8_t, const string&");
  }
  print_amount_from_balance(const print_amount_from_balance& other)
      : out(other.out), first(other.first), fwidth(other.fwidth), lwidth(other.lwidth),
        flags(other.flags), negative_color(other.negative_color) {
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

    buf.str(string());
    buf.clear();
    amount.print(buf, flags);

    justify(out, buf.str(), width, flags & AMOUNT_PRINT_RIGHT_JUSTIFY,
            (flags & AMOUNT_PRINT_COLORIZE && amount.sign() < 0) ? negative_color : empty_string);
  }

  /// @brief Emit a zero placeholder when the balance is entirely zero.
  ///
  /// Padded by justify(), like every amount, so that the caller's stream
  /// keeps its adjustment flags (value_t::print() hands over its own
  /// caller's stream, and report code after it relies on the default).
  void close() { justify(out, "0", fwidth, flags & AMOUNT_PRINT_RIGHT_JUSTIFY); }
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
/// If any commodity's rendered form is wider than the requested column,
/// the column is widened uniformly to fit the widest amount so that all
/// lines of the balance remain right-aligned with each other (issue #1795).
///
/// If the balance is empty or all amounts are zero, a single "0" is
/// printed, justified to @p first_width.
///
/// @param out           The output stream.
/// @param first_width   Column width for the first commodity line.
/// @param latter_width  Column width for subsequent lines (defaults to
///                      first_width if set to 1).
/// @param flags         Formatting flags (right-justify, colorize, etc.).
/// @param negative_color  The ANSI escape sequence with which
///                      AMOUNT_PRINT_COLORIZE renders negative amounts;
///                      empty renders them without color.
void balance_t::print(std::ostream& out, const int first_width, const int latter_width,
                      const uint_least8_t flags, const string& negative_color) const {
  int effective_first = first_width;
  int effective_latter = latter_width < 0 ? first_width : latter_width;

  // When AMOUNT_PRINT_FIT_TO_WIDEST is requested (balance report), widen
  // the column uniformly if any single commodity's rendered form would
  // overflow first_width.  Without this, a balance whose widest amount
  // exceeds the nominal column (e.g., ETH with 18-place precision) prints
  // without leading padding while narrower siblings (BTC, USD) are padded
  // to the nominal column, causing the suffix appended after the balance
  // (the account name) to land at an inconsistent column (issue #1795).
  // Opt-in because the register format embeds first_width/latter_width in
  // a fixed column grid; widening there would shift downstream columns.
  if (first_width > 0 && (flags & AMOUNT_PRINT_RIGHT_JUSTIFY) &&
      (flags & AMOUNT_PRINT_FIT_TO_WIDEST)) {
    int widest = 0;
    map_sorted_amounts([&widest, flags](const amount_t& amount) {
      if (amount.is_zero())
        return;
      std::ostringstream buf;
      amount.print(buf, flags);
      const int w = static_cast<int>(unistring(strip_ansi_escapes(buf.str())).width());
      if (w > widest)
        widest = w;
    });

    // Expand both widths by the same amount so that any prefix padding
    // encoded in latter_width (e.g., --prepend-format output) is preserved.
    const int extra = std::max(0, widest - first_width);
    effective_first += extra;
    effective_latter += extra;
  }

  bool first = true;
  print_amount_from_balance amount_printer(out, first, effective_first, effective_latter, flags,
                                           negative_color);
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
/// structured output via @c ledger @c xml or @c ledger @c json.  Amounts
/// are emitted in the same order balance_t::print() uses (see
/// sorted_amounts()), so the output does not depend on the hash order of
/// the component map.
///
/// @param st   The property tree node to populate.
/// @param bal  The balance to serialize.
void put_balance(property_tree::ptree& st, const balance_t& bal) {
  bal.map_sorted_amounts(
      [&st](const amount_t& amount) { put_amount(st.add("amount", ""), amount); });
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
/// 6. Marks the average fixated when every non-zero holding in the group
///    is a lot with a fixated price (plain units keep it floating).
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
  struct group_t {
    amount_t quantity;
    annotation_t ann;
    bool all_fixated = true; ///< Every holding so far is a lot with a fixated price.
  };
  typedef std::map<optional<std::string>, group_t> balance_map;
  balance_map bycomm;

  // Add @p part into @p sum, keeping full precision if either has it (as
  // balance_t::merge()): amount_t's addition keeps the left operand's flags,
  // so a part that keeps precision is added the other way round.
  auto accumulate = [](amount_t& sum, const amount_t& part) {
    if (part.keep_precision() && !sum.keep_precision()) {
      amount_t total(part);
      total += sum;
      sum = total;
    } else {
      sum += part;
    }
  };

  auto add = [&bycomm, &accumulate](const amount_t& amount) {
    commodity_t& comm(amount.commodity());
    optional<std::string> sym(comm.symbol());
    amount_t quant(amount.strip_annotations(keep_details_t()));

    auto [i, inserted] =
        bycomm.insert(balance_map::value_type(sym, group_t{quant, annotation_t()}));
    group_t& group(i->second);
    if (!inserted)
      accumulate(group.quantity, quant);

    const annotated_commodity_t* acomm =
        comm.has_annotation() ? &static_cast<const annotated_commodity_t&>(comm) : nullptr;
    if (acomm && acomm->details.price) {
      amount_t cost(*acomm->details.price * quant);
      if (group.ann.price)
        accumulate(*group.ann.price, cost);
      else
        group.ann.price = cost;
      if (!acomm->details.has_flags(ANNOTATION_PRICE_FIXATED) && !quant.is_realzero())
        group.all_fixated = false;
    } else if (!quant.is_realzero()) {
      // Units without a price dilute the average; a fixated average would
      // value them at nothing.  (A component that netted to zero is no
      // holding: it still counts for the date, the precision and the price's
      // commodity, as it always has, but not against the fixation.)
      group.all_fixated = false;
    }

    if (acomm && acomm->details.date) {
      if (!group.ann.date || *acomm->details.date < *group.ann.date)
        group.ann.date = *acomm->details.date;
    }
  };

  // Each group's sums, earliest date and fixation come out the same in any
  // order, so the lots are visited in the component map's order, a hash
  // order.  Only the error for lots priced in different commodities would
  // name a different pair from one run to the next: on an error, visit the
  // lots in display order and let the first failure through.
  try {
    for (const balance_t::amounts_map::value_type& pair : bal.amounts)
      add(pair.second);
  } catch (...) {
    bycomm.clear();
    balance_t::amounts_array sorted;
    bal.sorted_amounts(sorted);
    for (const amount_t* amount : sorted)
      add(*amount);
    throw;
  }

  balance_t result;

  for (balance_map::value_type& pair : bycomm) {
    group_t& group(pair.second);
    amount_t amt(group.quantity);
    if (!amt.is_realzero()) {
      if (group.ann.price) {
        group.ann.price = *group.ann.price / amt;
        // Lots that were all fixated average to a fixated lot: their price
        // is not a market price to be revalued.
        if (group.all_fixated)
          group.ann.add_flags(ANNOTATION_PRICE_FIXATED);
      }

      commodity_t* acomm =
          commodity_pool_t::current_pool->find_or_create(amt.commodity(), group.ann);
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
