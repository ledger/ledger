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
#pragma once

#include "amount.h"

namespace ledger {

class balance_error : public std::runtime_error {
public:
  explicit balance_error(const string& why) noexcept : std::runtime_error(why) {}
  ~balance_error() noexcept override {}
};

/**
 * @class balance_t
 *
 * @brief A wrapper around amount_t allowing addition of multiple commodities.
 *
 * The balance_t class is appropriate for keeping a running balance
 * where amounts of multiple commodities may be involved.
 */
class balance_t
    : public equality_comparable<
          balance_t,
          equality_comparable<
              balance_t, amount_t,
              equality_comparable<
                  balance_t, double,
                  equality_comparable<
                      balance_t, unsigned long,
                      equality_comparable<
                          balance_t, long,
                          additive<
                              balance_t,
                              additive<
                                  balance_t, amount_t,
                                  additive<balance_t, double,
                                           additive<balance_t, unsigned long,
                                                    additive<balance_t, long,
                                                             multiplicative<
                                                                 balance_t, amount_t,
                                                                 multiplicative<
                                                                     balance_t,
                                                                     double,
                                                                     multiplicative<
                                                                         balance_t,
                                                                         unsigned long,
                                                                         multiplicative<
                                                                             balance_t,
                                                                             long>>>>>>>>>>>>>> {
public:
  /**
   * Memory policy for the component map: plain new/delete and non-atomic
   * reference counts.  Balances are never shared between threads, and the
   * default free-list heap would keep global state alive across sessions.
   * (immer's headers are included by system.hh, ahead of utils.h, so that
   * their use of the C assert() does not displace Ledger's own.)
   */
  using amounts_memory_policy =
      immer::memory_policy<immer::heap_policy<immer::cpp_heap>, immer::unsafe_refcount_policy,
                           immer::no_lock_policy>;

  /**
   * The component amounts, keyed by commodity identity (the pool holds
   * exactly one commodity_t per distinct symbol or lot, see
   * commodity_pool_t::annotated_commodities; component_key() covers the
   * lots the pool has had to give equal annotations, and a lot whose
   * annotation the Python bindings edit in place is not followed).
   *
   * This is a persistent, structurally shared hash map: copying a balance
   * is O(1), and adding an amount yields a new version sharing all but
   * O(log K) of its nodes with the previous one.  Every posting of a
   * register keeps its own complete running total (calc_posts stores one
   * in each post's xdata), so with K components each posting costs
   * O(log K) time and memory.  Iteration order is a hash order of commodity
   * addresses and changes from run to run, so nothing visible may depend
   * on it: display goes through sorted_amounts(), and rebuilding a balance
   * from transformed components through merge().  The map is immutable:
   * use set()/update_if_exists()/erase(), which return the new version, or
   * transform_amounts() to rewrite every component; update() is avoided
   * because it instantiates a static default amount_t that outlives
   * amount_t::shutdown().
   */
  using amounts_map = immer::map<commodity_t*, amount_t, std::hash<commodity_t*>,
                                 std::equal_to<commodity_t*>, amounts_memory_policy>;
  using amounts_array = std::vector<const amount_t*>;

  enum class lot_sort_order : std::uint8_t { by_commodity, fifo, lifo };

  amounts_map amounts;
  /**
   * The order in which the display paths list lots: sorted_amounts() and
   * first_sorted_amount() are its only readers.  It is a display
   * preference, not part of the balance's value.  Only fifo_lot_prices()
   * and lifo_lot_prices() set it (through the fifo_lots()/lifo_lots()
   * value-expression functions, which --lots-fifo/--lots-lifo install),
   * and operator== ignores it.  Every operation that copies the balance
   * keeps it (copy or move construction and assignment from a balance take
   * that balance's; negated(), rounded() and the other rounding copies,
   * and assignment from an amount, keep their own), as do abs(), reduce(),
   * unreduce() and strip_annotations(); operator+= and operator-= keep
   * the left operand's; value(), number(), average_lot_prices() and any
   * balance built afresh start at by_commodity.
   */
  lot_sort_order sort_order = lot_sort_order::by_commodity;

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
  balance_t() { TRACE_CTOR(balance_t, ""); }
  balance_t(const amount_t& amt) {
    if (amt.is_null())
      throw_(balance_error, _("Cannot initialize a balance from an uninitialized amount"));
    if (!amt.is_realzero())
      amounts = std::move(amounts).set(&amt.commodity(), amt);
    TRACE_CTOR(balance_t, "const amount_t&");
  }
  balance_t(const double val);
  balance_t(const unsigned long val);
  balance_t(const long val);

  explicit balance_t(const string& val) {
    amount_t temp(val);
    amounts = std::move(amounts).set(&temp.commodity(), temp);
    TRACE_CTOR(balance_t, "const string&");
  }
  explicit balance_t(const char* val) {
    amount_t temp(val);
    amounts = std::move(amounts).set(&temp.commodity(), temp);
    TRACE_CTOR(balance_t, "const char *");
  }

  /**
   * Destructor.  Destroys all of the accumulated amounts in the
   * balance.
   */
  ~balance_t() { TRACE_DTOR(balance_t); }

  /**
   * Assignment and copy operators.  An balance may be assigned or copied.
   */
  balance_t(const balance_t& bal) : amounts(bal.amounts), sort_order(bal.sort_order) {
    TRACE_CTOR(balance_t, "copy");
  }
  balance_t(balance_t&& bal) noexcept
      : amounts(std::move(bal.amounts)), sort_order(bal.sort_order) {
    TRACE_CTOR(balance_t, "move");
  }

  balance_t& operator=(const balance_t& bal) {
    if (this != &bal) {
      amounts = bal.amounts;
      sort_order = bal.sort_order;
    }
    return *this;
  }
  balance_t& operator=(balance_t&& bal) noexcept {
    amounts = std::move(bal.amounts);
    sort_order = bal.sort_order;
    return *this;
  }
  balance_t& operator=(const amount_t& amt) {
    if (amt.is_null())
      throw_(balance_error, _("Cannot assign an uninitialized amount to a balance"));

    amounts = amounts_map();
    if (!amt.is_realzero())
      amounts = std::move(amounts).set(&amt.commodity(), amt);

    return *this;
  }

  balance_t& operator=(const string& str) { return *this = balance_t(str); }
  balance_t& operator=(const char* str) { return *this = balance_t(str); }

  /**
   * Comparison operators.  Balances are fairly restrictive in terms
   * of how they may be compared.  They may be compared for equality
   * or inequality, but this is all, since the concept of "less than"
   * or "greater than" makes no sense when amounts of multiple
   * commodities are involved.
   *
   * Balances may also be compared to amounts: a balance equals an
   * amount when its only component equals the amount exactly, or, for a
   * zero amount, when the balance is empty.
   *
   * If a comparison between balances is desired, the balances must
   * first be rendered to value equivalent amounts using the `value'
   * method, to determine a market valuation at some specific moment
   * in time.
   */
  bool operator==(const balance_t& bal) const { return amounts == bal.amounts; }
  bool operator==(const amount_t& amt) const {
    if (amt.is_null())
      throw_(balance_error, _("Cannot compare a balance to an uninitialized amount"));

    if (amt.is_realzero())
      return amounts.empty();
    else
      return amounts.size() == 1 && amounts.begin()->second == amt;
  }

  template <typename T>
  bool operator==(const T& val) const {
    return *this == amount_t(val);
  }

  /**
   * Binary arithmetic operators.  Balances support addition and
   * subtraction of other balances or amounts.  Multiplication and
   * division take an uncommoditized amount, which scales every
   * component, or an amount in the commodity of the balance's single
   * component; see operator*=() in balance.cc for the other cases.
   */
  balance_t& operator+=(const balance_t& bal);
  balance_t& operator+=(const amount_t& amt);
  balance_t& operator+=(const double val) { return *this += amount_t(val); }
  balance_t& operator+=(const unsigned long val) { return *this += amount_t(val); }
  balance_t& operator+=(const long val) { return *this += amount_t(val); }

  balance_t& operator-=(const balance_t& bal);
  balance_t& operator-=(const amount_t& amt);
  balance_t& operator-=(const double val) { return *this -= amount_t(val); }
  balance_t& operator-=(const unsigned long val) { return *this -= amount_t(val); }
  balance_t& operator-=(const long val) { return *this -= amount_t(val); }

  /**
   * Add @p amt as a component that may collapse onto an existing one of its
   * commodity, keeping full precision if either part has it.  operator+=
   * keeps the left operand's precision flags, so rebuilding a balance from
   * transformed components (lots stripped to their base, components valued
   * in one currency, units reduced) in the component map's order -- a hash
   * order -- would print such a merged amount with either part's precision
   * from one run to the next.  A sequence of merges gives the same result
   * in any order.
   */
  balance_t& merge(const amount_t& amt);

  /**
   * The key under which amounts of @p comm are kept: @p comm itself, unless
   * @p comm is one of the lots the pool has had to give equal annotations
   * (see commodity_pool_t::reannotate): then the one of them this balance
   * already holds, else the one that carried the annotation first.
   */
  commodity_t* component_key(const commodity_t& comm) const;

  balance_t& operator*=(const amount_t& amt);
  balance_t& operator*=(const double val) { return *this *= amount_t(val); }
  balance_t& operator*=(const unsigned long val) { return *this *= amount_t(val); }
  balance_t& operator*=(const long val) { return *this *= amount_t(val); }

  balance_t& operator/=(const amount_t& amt);
  balance_t& operator/=(const double val) { return *this /= amount_t(val); }
  balance_t& operator/=(const unsigned long val) { return *this /= amount_t(val); }
  balance_t& operator/=(const long val) { return *this /= amount_t(val); }

  /**
   * Rewrite every component amount with @p fn: the keys are gathered, then
   * a transient of the map rewrites each node, in place when this balance
   * owns the map outright (a component of a shared map is copied first).
   * Used by the in_place_* operations and the scalar operators; @p fn must
   * not change the amount's commodity (reduce and unreduce, which may,
   * re-accumulate through a temporary instead).
   */
  template <typename Fn>
  void transform_amounts(Fn&& fn) {
    // The keys are gathered first, since the rewrite must not change the
    // map being walked; a transient of a map this balance owns outright
    // then rewrites the nodes in place.
    std::vector<commodity_t*> keys;
    keys.reserve(amounts.size());
    for (const amounts_map::value_type& pair : amounts)
      keys.push_back(pair.first);
    amounts_map::transient_type result = std::move(amounts).transient();
    for (commodity_t* key : keys)
      result.update_if_exists(key, component_updater(fn));
    amounts = std::move(result).persistent();
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
   * value(moment, in_terms_of) returns the market value of a balance
   * as an optional<balance_t> -- the default moment uses the most
   * recently known price -- based on the price history of its
   * component commodities, or an empty optional when no component
   * has a price.  See amount_t::value for an example.
   *
   * negate(), reduce() and unreduce() also have in-place forms
   * (in_place_negate(), in_place_reduce(), in_place_unreduce()) that
   * modify the balance itself and return void, avoiding a temporary;
   * abs() and value() have none.
   */
  [[nodiscard]] balance_t negated() const {
    balance_t temp(*this);
    temp.in_place_negate();
    return temp;
  }
  void in_place_negate() {
    transform_amounts([&](amount_t& amt) { amt.in_place_negate(); });
  }
  balance_t operator-() const { return negated(); }

  [[nodiscard]] balance_t abs() const {
    balance_t temp;
    temp.sort_order = sort_order;
    for (const amounts_map::value_type& pair : amounts)
      temp += pair.second.abs();
    return temp;
  }

  [[nodiscard]] balance_t rounded() const {
    balance_t temp(*this);
    temp.in_place_round();
    return temp;
  }
  void in_place_round() {
    transform_amounts([&](amount_t& amt) { amt.in_place_round(); });
  }

  balance_t roundto(int places) const {
    balance_t temp(*this);
    temp.in_place_roundto(places);
    return temp;
  }

  void in_place_roundto(int places) {
    transform_amounts([&](amount_t& amt) { amt.in_place_roundto(places); });
  }

  [[nodiscard]] balance_t truncated() const {
    balance_t temp(*this);
    temp.in_place_truncate();
    return temp;
  }
  void in_place_truncate() {
    transform_amounts([&](amount_t& amt) { amt.in_place_truncate(); });
  }

  void in_place_display_round() {
    transform_amounts([&](amount_t& amt) { amt.in_place_display_round(); });
  }

  void in_place_round_to_commodity_precision() {
    transform_amounts([&](amount_t& amt) { amt.in_place_round_to_commodity_precision(); });
  }

  [[nodiscard]] balance_t floored() const {
    balance_t temp(*this);
    temp.in_place_floor();
    return temp;
  }
  void in_place_floor() {
    transform_amounts([&](amount_t& amt) { amt.in_place_floor(); });
  }

  [[nodiscard]] balance_t ceilinged() const {
    balance_t temp(*this);
    temp.in_place_ceiling();
    return temp;
  }
  void in_place_ceiling() {
    transform_amounts([&](amount_t& amt) { amt.in_place_ceiling(); });
  }

  [[nodiscard]] balance_t unrounded() const {
    balance_t temp(*this);
    temp.in_place_unround();
    return temp;
  }
  void in_place_unround() {
    transform_amounts([&](amount_t& amt) { amt.in_place_unround(); });
  }

  balance_t reduced() const {
    balance_t temp(*this);
    temp.in_place_reduce();
    return temp;
  }
  void in_place_reduce() {
    // A temporary must be used here because reduction may cause
    // multiple component amounts to collapse to the same commodity.
    balance_t temp;
    temp.sort_order = sort_order;
    for (const amounts_map::value_type& pair : amounts)
      temp.merge(pair.second.reduced());
    *this = std::move(temp);
  }

  balance_t unreduced() const {
    balance_t temp(*this);
    temp.in_place_unreduce();
    return temp;
  }
  void in_place_unreduce() {
    // A temporary must be used here because unreduction may cause
    // multiple component amounts to collapse to the same commodity.
    balance_t temp;
    temp.sort_order = sort_order;
    for (const amounts_map::value_type& pair : amounts)
      temp.merge(pair.second.unreduced());
    *this = std::move(temp);
  }

  optional<balance_t> value(const datetime_t& moment = datetime_t(),
                            const commodity_t* in_terms_of = nullptr) const;

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
  operator bool() const { return is_nonzero(); }

  [[nodiscard]] bool is_nonzero() const {
    if (is_empty())
      return false;

    for (const amounts_map::value_type& pair : amounts)
      if (pair.second.is_nonzero())
        return true;
    return false;
  }

  [[nodiscard]] bool is_zero() const {
    if (is_empty())
      return true;

    for (const amounts_map::value_type& pair : amounts)
      if (!pair.second.is_zero())
        return false;
    return true;
  }

  [[nodiscard]] bool is_realzero() const {
    if (is_empty())
      return true;

    for (const amounts_map::value_type& pair : amounts)
      if (!pair.second.is_realzero())
        return false;
    return true;
  }

  [[nodiscard]] bool is_empty() const { return amounts.size() == 0; }
  [[nodiscard]] bool single_amount() const { return amounts.size() == 1; }

  /**
   * sole_amount() returns the one amount the balance comes to: its only
   * component that is not exactly zero or, if every component is exactly
   * zero, its lone component (which keeps its commodity).  It returns
   * nullptr for several non-zero components, several zero ones, or none.
   * Unlike single_amount(), it skips a component that operator+= kept
   * after cancelling it, so {$10, 0 EUR} comes to $10.  The entry stays
   * in the map: xact.cc and fn_nail_down read it.
   */
  [[nodiscard]] const amount_t* sole_amount() const;

  /**
   * Conversion methods.  A balance can be converted to an amount, but
   * only if contains a single component amount.
   */
  operator string() const { return to_string(); }
  [[nodiscard]] string to_string() const {
    std::ostringstream buf;
    print(buf);
    return buf.str();
  }

  amount_t to_amount() const {
    // NOLINTBEGIN(bugprone-branch-clone)
    if (is_empty())
      throw_(balance_error, _("Cannot convert an empty balance to an amount"));
    else if (amounts.size() == 1)
      return amounts.begin()->second;
    else
      throw_(balance_error, _("Cannot convert a balance with multiple commodities to an amount"));
    // NOLINTEND(bugprone-branch-clone)
    return amount_t();
  }

  /**
   * Commodity-related methods.  Balances support two
   * commodity-related methods:
   *
   * commodity_count() returns the number of different commodities
   * stored in the balance.
   *
   * commodity_amount(commodity) returns the component in the given
   * commodity, or an empty optional if the balance has none.  With no
   * commodity it returns the sole component, or the sole component
   * left after stripping annotations; it returns an empty optional for
   * an empty balance and throws amount_error if several commodities
   * remain.
   */
  std::size_t commodity_count() const { return amounts.size(); }

  optional<amount_t> commodity_amount(const commodity_t* commodity = nullptr) const;

  balance_t number() const {
    balance_t temp;
    for (const amounts_map::value_type& pair : amounts)
      temp.merge(pair.second.number());
    return temp;
  }

  /**
   * Annotated commodity methods.  The amounts contained by a balance
   * may use annotated commodities.  The `strip_annotations' method
   * will return a balance all of whose component amount have had
   * their commodity annotations likewise stripped.  See
   * amount_t::strip_annotations for more details.
   */
  balance_t strip_annotations(const keep_details_t& what_to_keep) const;

  /**
   * Given a balance, insert a commodity-wise sort of the amounts into the
   * given amounts_array.
   */
  void sorted_amounts(amounts_array& sorted) const;

  /**
   * The amount sorted_amounts() would place first, or nullptr for an empty
   * balance, found in linear time (top_amount() asks for this on every
   * posting of a register).
   */
  const amount_t* first_sorted_amount() const;

  /**
   * Apply @p fn to every component amount, in the map's order, and should
   * @p fn fail on one, let through the failure a visit in display order
   * would have met first, so that an error names the same component on
   * every run (the map's order is a hash order): the components not yet
   * visited that precede the failing one in display order are tried, and
   * the first of them to fail, else the first failure itself, is thrown
   * with its own error context, as the only failure would have been.  No
   * component is visited twice, since a valuation may run Python code or
   * fetch a quote.
   */
  template <typename Fn>
  void visit_components(Fn&& fn) const {
    std::optional<std::ostringstream> earlier;
    if (long(_ctxt_buffer.tellp()) != 0)
      take_error_context(earlier.emplace());
    const amount_t* failing = nullptr;
    std::exception_ptr failure;
    try {
      for (const amounts_map::value_type& pair : amounts) {
        failing = &pair.second;
        fn(pair.second);
      }
    } catch (...) {
      failure = std::current_exception();
    }
    if (!failure) {
      if (earlier)
        restore_error_context(*earlier);
      return;
    }
    std::ostringstream failed;
    take_error_context(failed);
    if (earlier)
      restore_error_context(*earlier);
    // The first loop finished exactly the components that precede the
    // failing one in the map's order, which is fixed for one map object.
    std::unordered_set<const amount_t*> visited;
    for (const amounts_map::value_type& pair : amounts) {
      if (&pair.second == failing)
        break;
      visited.insert(&pair.second);
    }
    amounts_array sorted;
    sorted_amounts(sorted);
    for (const amount_t* amount : sorted) {
      if (amount == failing)
        break;
      if (visited.count(amount) == 0)
        fn(*amount);
    }
    restore_error_context(failed);
    std::rethrow_exception(failure);
  }

  /**
   * Iteration primitives.  `map_sorted_amounts' allows one to visit
   * each amount in balance in the proper order for displaying to the
   * user.  Mostly used by `print' and other routines where the sort
   * order of the amounts' commodities is significant.  A single
   * component is visited without sorting; @p fn is called once per
   * non-null amount, in sorted order.  @p fn is called by reference and
   * never copied.
   */
  template <typename Fn>
  void map_sorted_amounts(Fn&& fn) const {
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

  /**
   * Printing methods.  print() writes the balance to a stream, one
   * component per line; operator<< calls it with a first_width of 12.
   * Every parameter after the stream has a default, and the definition
   * in balance.cc documents them.  Components print in sorted_amounts()
   * order: by commodity, unless sort_order is fifo or lifo (--lots-fifo or
   * --lots-lifo), which orders the lots of each commodity by date.
   */
  void print(std::ostream& out, const int first_width = -1, const int latter_width = -1,
             const uint_least8_t flags = AMOUNT_PRINT_NO_FLAGS,
             const string& negative_color = empty_string) const;

  /**
   * valid() returns true if the amounts within the balance are valid.
   */
  [[nodiscard]] bool valid() const {
    for (const amounts_map::value_type& pair : amounts)
      if (!pair.second.valid()) {
        DEBUG("ledger.validate", "balance_t: ! pair.second.valid()");
        return false;
      }
    return true;
  }

private:
  /**
   * The function immer's update_if_exists() takes, applying @p op to one
   * component: when the map is owned outright immer hands over the stored
   * amount itself (an rvalue) and @p op updates it in place; a component
   * of a shared map arrives const and @p op updates a copy.  amount_t has
   * no move constructor, so a by-value parameter would copy either way.
   * The result refers to @p op and must be used within the full-expression.
   */
  template <typename Fn>
  static auto component_updater(Fn&& op) {
    return [&op](auto&& current) -> amount_t {
      if constexpr (std::is_const_v<std::remove_reference_t<decltype(current)>>) {
        amount_t updated(current);
        op(updated);
        return updated;
      } else {
        op(current);
        return current;
      }
    };
  }

  /// Apply @p op to the component kept under @p key, if there is one, in
  /// place when this balance owns its map; returns whether there was one.
  template <typename Fn>
  bool update_component(commodity_t* key, Fn&& op) {
    bool found = false;
    amounts = std::move(amounts).update_if_exists(
        key, component_updater([&found, &op](amount_t& current) {
          found = true;
          op(current);
        }));
    return found;
  }
};

inline std::ostream& operator<<(std::ostream& out, const balance_t& bal) {
  bal.print(out, 12);
  return out;
}

void put_balance(property_tree::ptree& pt, const balance_t& bal);

balance_t average_lot_prices(const balance_t& bal);
balance_t fifo_lot_prices(const balance_t& bal);
balance_t lifo_lot_prices(const balance_t& bal);

} // namespace ledger
