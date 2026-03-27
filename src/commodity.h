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
 * @file   commodity.h
 * @author John Wiegley
 *
 * @ingroup math
 *
 * @brief  Types for handling commodities
 *
 * Commodities represent currencies, stocks, mutual funds, and any other
 * unit of value that can appear in a journal.  Every amount_t carries a
 * pointer to a commodity_t that determines its display symbol and
 * formatting rules.
 *
 * A key design principle is that Ledger *learns* display formatting from
 * usage: the first time a commodity like "$" is seen with two decimal
 * places and a thousands separator, those style flags are recorded in the
 * commodity's shared base_t and applied to all future output of that
 * commodity.  Users can also set formatting explicitly via commodity
 * directives.
 *
 * commodity_t also serves as the node type in the commodity price graph
 * (see history.h).  Price entries establish exchange rates between
 * commodities, and find_price() performs shortest-path lookups through
 * that graph with memoization for performance.
 */
#pragma once

#include "expr.h"

namespace ledger {

struct keep_details_t;
class commodity_pool_t;

class commodity_error : public std::runtime_error {
public:
  explicit commodity_error(const string& why) noexcept : std::runtime_error(why) {}
  ~commodity_error() noexcept override {}
};

/**
 * @brief A price observation: a specific exchange rate at a point in time.
 *
 * Produced by price lookups and price directives, and returned from
 * find_price() and the commodity price history graph.  For example,
 * a "P 2024/01/15 AAPL $185.00" directive creates a price_point_t
 * with when = 2024-01-15 and price = $185.00.
 */
struct price_point_t {
  datetime_t when; ///< When this price was observed or effective.
  amount_t price;  ///< The exchange rate, expressed in a target commodity.

  price_point_t() {}
  price_point_t(datetime_t _when, const amount_t& _price) : when(_when), price(_price) {}

  bool operator==(const price_point_t& other) const {
    return when == other.when && price == other.price;
  }
};

class commodity_t : public flags::delegates_flags<uint_least16_t>,
                    public equality_comparable1<commodity_t, noncopyable> {
protected:
  friend class commodity_pool_t;
  friend class annotated_commodity_t;

  /**
   * @brief Shared state for all amounts of the same commodity.
   *
   * Multiple commodity_t instances (including annotated variants) can share
   * a single base_t through a shared_ptr.  The base holds the canonical
   * symbol, display-style flags learned from parsed amounts, the precision
   * to use when printing, and the memoized price lookup cache.
   *
   * This sharing ensures that when Ledger learns display formatting from
   * one occurrence of a commodity (e.g., "$1,000.00" teaches THOUSANDS
   * and precision 2), all amounts of that commodity benefit immediately.
   */
  class base_t : public noncopyable, public flags::supports_flags<uint_least16_t> {
  public:
#define COMMODITY_STYLE_DEFAULTS 0x000        ///< No special formatting; symbol is prefixed.
#define COMMODITY_STYLE_SUFFIXED 0x001        ///< Symbol follows the amount (e.g., "100 EUR").
#define COMMODITY_STYLE_SEPARATED 0x002       ///< A space separates symbol from quantity.
#define COMMODITY_STYLE_DECIMAL_COMMA 0x004   ///< Use comma as decimal point (European style).
#define COMMODITY_STYLE_THOUSANDS 0x008       ///< Insert grouping separators (e.g., "1,000").
#define COMMODITY_NOMARKET 0x010              ///< Exclude from market-price valuations (-V/-X).
#define COMMODITY_BUILTIN 0x020               ///< Internally created (e.g., the null commodity).
#define COMMODITY_WALKED 0x040                ///< Traversal flag for graph algorithms.
#define COMMODITY_KNOWN 0x080                 ///< Explicitly declared via a commodity directive.
#define COMMODITY_PRIMARY 0x100               ///< This commodity has appeared as a price target.
#define COMMODITY_SAW_ANNOTATED 0x200         ///< An annotated variant has been created.
#define COMMODITY_SAW_ANN_PRICE_FLOAT 0x400   ///< Seen with a floating lot price.
#define COMMODITY_SAW_ANN_PRICE_FIXATED 0x800 ///< Seen with a fixated lot price ({=...}).
#define COMMODITY_STYLE_TIME_COLON 0x1000     ///< Use colon notation for time amounts.
#define COMMODITY_STYLE_NO_MIGRATE 0x2000     ///< Do not migrate formatting from parsed amounts.
#define COMMODITY_STYLE_THOUSANDS_APOSTROPHE 0x4000 ///< Use apostrophe as thousands separator.
#define COMMODITY_PRECISION_FROM_PRICE 0x8000 ///< Precision was inferred from a price directive.

    string symbol;                     ///< The canonical commodity name (e.g., "$", "AAPL").
    optional<std::size_t> graph_index; ///< Index into the price history graph adjacency list.
    amount_t::precision_t precision;   ///< Number of decimal places for display.
    optional<string> name;             ///< Long descriptive name (from commodity directive).
    optional<string> note;             ///< User-supplied note (from commodity directive).
    optional<amount_t> smaller;        ///< Subdivision unit (e.g., cents for dollars).
    optional<amount_t> larger;         ///< Grouping unit (e.g., thousands).
    std::optional<expr_t> value_expr;  ///< Custom valuation expression overriding find_price.

    /// Key type for the memoized price cache: (moment, oldest, target commodity).
    using memoized_price_entry = tuple<datetime_t, datetime_t, const commodity_t*>;
    /// Map from lookup parameters to cached price results.
    using memoized_price_map = std::map<memoized_price_entry, std::optional<price_point_t>>;

    /// Maximum entries before the cache is halved to prevent unbounded growth.
    inline static constexpr std::size_t max_price_map_size = 8;
    mutable memoized_price_map price_map;
    mutable std::optional<datetime_t> last_quote; ///< Time of last download attempt (issue #996).

  public:
    explicit base_t(const string& _symbol)
        : supports_flags<uint_least16_t>(
              commodity_t::decimal_comma_by_default
                  ? static_cast<uint_least16_t>(COMMODITY_STYLE_DECIMAL_COMMA)
                  : static_cast<uint_least16_t>(COMMODITY_STYLE_DEFAULTS)),
          symbol(_symbol), precision(0) {
      TRACE_CTOR(commodity_t::base_t, "const string&");
    }
    virtual ~base_t() { TRACE_DTOR(commodity_t::base_t); }
  };

  std::shared_ptr<base_t> base;

  commodity_pool_t* parent_;
  optional<string> qualified_symbol;
  bool annotated;

  explicit commodity_t(commodity_pool_t* _parent, const std::shared_ptr<base_t>& _base)
      : delegates_flags<uint_least16_t>(*_base.get()), base(_base), parent_(_parent),
        annotated(false) {
    TRACE_CTOR(commodity_t, "commodity_pool_t *, std::shared_ptr<base_t>");
  }

public:
  static bool decimal_comma_by_default;
  static bool time_colon_by_default;

  virtual ~commodity_t() { TRACE_DTOR(commodity_t); }

  operator bool() const;

  virtual bool operator==(const commodity_t& comm) const {
    if (comm.annotated)
      return comm == *this;
    return base.get() == comm.base.get();
  }
  bool operator==(const string& name) const { return base_symbol() == name; }

  static bool symbol_needs_quotes(const string& symbol);

  virtual commodity_t& referent() { return *this; }
  virtual const commodity_t& referent() const { return *this; }

  bool has_annotation() const { return annotated; }

  virtual commodity_t& strip_annotations(const keep_details_t&) { return *this; }
  virtual void write_annotations(std::ostream&, bool, const amount_t* = nullptr,
                                 uint_least8_t = 0) const {}

  commodity_pool_t& pool() const { return *parent_; }

  string base_symbol() const { return base->symbol; }
  string symbol() const { return qualified_symbol ? *qualified_symbol : base_symbol(); }

  optional<std::size_t> graph_index() const {
    ;
    return base->graph_index;
  }
  void set_graph_index(const optional<std::size_t>& arg = none) { base->graph_index = arg; }

  optional<string> name() const { return base->name; }
  void set_name(const optional<string>& arg = none) { base->name = arg; }

  optional<string> note() const { return base->note; }
  void set_note(const optional<string>& arg = none) { base->note = arg; }

  amount_t::precision_t precision() const { return base->precision; }
  void set_precision(amount_t::precision_t arg) { base->precision = arg; }

  optional<amount_t> smaller() const { return base->smaller; }
  void set_smaller(const optional<amount_t>& arg = none) { base->smaller = arg; }

  optional<amount_t> larger() const { return base->larger; }
  void set_larger(const optional<amount_t>& arg = none) { base->larger = arg; }

  virtual std::optional<expr_t> value_expr() const { return base->value_expr; }
  void set_value_expr(const std::optional<expr_t>& expr = {}) { base->value_expr = expr; }

  /** @brief Record a price for this commodity in the price history graph.
   *  @param date   When the price is effective.
   *  @param price  The exchange rate (its commodity is the target).
   *  @param reflexive  If true, mark the price's commodity as COMMODITY_PRIMARY
   *                    (the normal case for parsed P directives). */
  void add_price(const datetime_t& date, const amount_t& price, const bool reflexive = true);
  /** @brief Remove a previously recorded price from the history graph. */
  void remove_price(const datetime_t& date, commodity_t& commodity);

  void map_prices(const function<void(datetime_t, const amount_t&)>& fn,
                  const datetime_t& moment = datetime_t(), const datetime_t& _oldest = datetime_t(),
                  bool bidirectionally = false);

  std::optional<price_point_t> find_price_from_expr(expr_t& expr, const commodity_t* commodity,
                                                    const datetime_t& moment) const;

  /**
   * @brief Look up this commodity's price in terms of another commodity.
   *
   * Performs a memoized price lookup through the commodity price history
   * graph.  If no target commodity is specified, the pool's default
   * commodity is used.  Results are cached in base->price_map; when the
   * cache exceeds max_price_map_size, the oldest half is evicted.
   *
   * If the commodity has a value_expr, that expression is evaluated
   * instead of consulting the price graph.
   *
   * @param commodity  Target commodity (nullptr = pool default).
   * @param moment     Point in time for the lookup (default = now/epoch).
   * @param oldest     Oldest acceptable price date.
   * @return The price point if found, or nullopt.
   */
  std::optional<price_point_t> virtual find_price(const commodity_t* commodity = nullptr,
                                                  const datetime_t& moment = datetime_t(),
                                                  const datetime_t& oldest = datetime_t()) const;

  /**
   * @brief Conditionally download a more current price quote (--download).
   *
   * When get_quotes is enabled and the commodity is not marked NOMARKET,
   * this checks whether the existing price point is stale (older than
   * quote_leeway seconds).  If so, it invokes the external getquote
   * script to fetch a current price.  A per-session throttle
   * (last_quote) prevents redundant downloads for the same commodity
   * within a single run (issue #996).
   *
   * @param point        The best price found so far (may be nullopt).
   * @param moment       The reference time for staleness comparison.
   * @param in_terms_of  Desired target commodity for the quote.
   * @return The updated price point, or the original if no update was needed.
   */
  std::optional<price_point_t> check_for_updated_price(const std::optional<price_point_t>& point,
                                                       const datetime_t& moment,
                                                       const commodity_t* in_terms_of);

  commodity_t& nail_down(const expr_t& expr);

  // Methods related to parsing, reading, writing, etc., the commodity
  // itself.

  /**
   * @brief Parse a commodity symbol from an input stream.
   *
   * Handles both quoted symbols (delimited by double quotes, allowing any
   * characters including digits and operators) and bare symbols (terminated
   * by any character in the invalid_chars table).  Backslash escapes are
   * supported in bare symbols.  If the parsed token is a reserved word
   * (and, or, not, if, else, true, false, div), it is rejected as a
   * commodity name and the stream is rewound.
   */
  static void parse_symbol(std::istream& in, string& symbol);
  /** @brief Parse a commodity symbol from a C string pointer (advances p). */
  static void parse_symbol(char*& p, string& symbol);
  /** @brief Convenience overload that returns the parsed symbol by value. */
  static string parse_symbol(std::istream& in) {
    string temp; // NOLINT(bugprone-unused-local-non-trivial-variable)
    parse_symbol(in, temp);
    return temp;
  }

  virtual void print(std::ostream& out, bool elide_quotes = false,
                     bool print_annotations = false) const;
  bool valid() const;

  /**
   * @brief Comparator for consistent display ordering of amounts.
   *
   * Used by balance_t and reporting to sort amounts deterministically.
   * Comparison proceeds through multiple levels: base symbol (lexicographic),
   * then annotation presence, then lot price, lot date, lot tag, valuation
   * expression, and finally semantic flags (ANNOTATION_PRICE_FIXATED).
   * Returns negative, zero, or positive like strcmp.
   */
  struct compare_by_commodity {
    int operator()(const amount_t* left, const amount_t* right) const;
  };
};

inline std::ostream& operator<<(std::ostream& out, const commodity_t& comm) {
  comm.print(out, false, true);
  return out;
}

void put_commodity(property_tree::ptree& pt, const commodity_t& comm,
                   bool commodity_details = false);

// simple struct to allow std::map to compare commodities names
struct commodity_compare {
  bool operator()(const commodity_t* lhs, const commodity_t* rhs) const {
    return (lhs->symbol().compare(rhs->symbol()) < 0);
  }
};

} // namespace ledger
