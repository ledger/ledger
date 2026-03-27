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
 * @file   pool.h
 * @author John Wiegley
 *
 * @ingroup math
 *
 * @brief  Types for managing commodity pools
 *
 * The commodity pool is a singleton registry of all commodities and their
 * annotated variants, managing creation, lookup, aliasing, and price
 * exchange.  Every commodity_t and annotated_commodity_t in the system is
 * owned by a commodity_pool_t through shared_ptr, ensuring stable lifetimes
 * and identity-based equality (two amounts share a commodity if and only if
 * they point to the same pool entry).
 *
 * The pool also owns the commodity_history_t price graph, coordinates
 * price downloads (--download), and provides the exchange() methods that
 * record lot-based commodity conversions with proper annotation.
 */
#pragma once

#include "history.h"
#include "annotate.h"

namespace ledger {

/**
 * @brief Result of a commodity exchange recording the annotated amount
 *        and its cost components.
 *
 * Returned by the full exchange() overload to give callers everything
 * needed to record a transaction posting with lot information.
 */
struct cost_breakdown_t {
  amount_t amount;     ///< The original amount re-annotated with lot price/date/tag.
  amount_t final_cost; ///< The total cost of this exchange (what was paid).
  amount_t basis_cost; ///< The cost basis (uses existing lot price if available).
};

class commodity_pool_t : public noncopyable {
public:
  /**
   * The commodities collection in commodity_pool_t maintains pointers to all
   * the commodities which have ever been created by the user, whether
   * explicitly by calling the create methods of commodity_pool_t, or
   * implicitly by parsing a commoditized amount.
   */
  using commodities_map = std::map<string, std::shared_ptr<commodity_t>>;
  using annotated_commodities_map =
      std::map<std::pair<string, annotation_t>, std::shared_ptr<annotated_commodity_t>>;

  /// All known base (unannotated) commodities, keyed by symbol string.
  /// Commodity aliases also live here, mapping the alias name to the same
  /// shared_ptr as the original symbol.
  commodities_map commodities;

  /// Annotated variants of commodities, keyed by (base symbol, annotation)
  /// pairs.  Each unique combination of lot price, date, and tag produces
  /// a distinct entry, so that identity comparison is sufficient to
  /// determine whether two amounts share the same annotated commodity.
  annotated_commodities_map annotated_commodities;

  /// Commodities that were once registered under a symbol but have been
  /// replaced (e.g., when an `alias` directive remaps a symbol that was
  /// created earlier by price-db parsing).  Keeping them alive here
  /// prevents dangling raw pointers in amount_t objects that still
  /// reference the old commodity.
  std::vector<std::shared_ptr<commodity_t>> retired_commodities;

  /// The directed price graph used for commodity conversions (--market,
  /// -V, -X).  Prices from journal `P` directives, lot costs, and
  /// downloaded quotes all feed into this history.
  commodity_history_t commodity_price_history;

  /// A sentinel commodity with an empty symbol, used as the default when
  /// an amount has no explicit commodity.
  commodity_t* null_commodity;

  /// The commodity implied by the `D` directive.  When set, amounts
  /// without an explicit commodity are displayed using this commodity's
  /// formatting (decimal places, thousands separator, etc.).
  commodity_t* default_commodity;

  bool keep_base;          ///< When true (--base), display amounts in their base commodity.
  optional<path> price_db; ///< Path to the price database file (--price-db).
  long quote_leeway; ///< Seconds a cached quote remains valid before re-downloading (--leeway).
  bool get_quotes;   ///< Whether to download live quotes (--download).
  optional<path> getquote; ///< Path to the external quote-fetching script (--getquote).

  /// Callback used to obtain a live price quote for a commodity.
  /// By default this invokes the external getquote script via
  /// commodity_quote_from_script().
  function<std::optional<price_point_t>(commodity_t& commodity, const commodity_t* in_terms_of)>
      get_commodity_quote;

  /// Callback used to obtain batch price quotes for multiple commodities
  /// in a single script invocation.  By default this invokes the external
  /// getquote script via commodity_batch_quote_from_script().
  function<std::vector<std::pair<commodity_t*, price_point_t>>(
      std::vector<std::reference_wrapper<commodity_t>>& commodities,
      const commodity_t* in_terms_of)>
      get_commodity_batch_quote;

  /// The process-wide singleton pool.  Most code accesses commodities
  /// through this static pointer.
  static std::shared_ptr<commodity_pool_t> current_pool;

  explicit commodity_pool_t();
  virtual ~commodity_pool_t() { TRACE_DTOR(commodity_pool_t); }

  /// @name Base commodity operations
  /// @{

  /// @brief Create a new base commodity with the given symbol.
  ///
  /// Inserts the commodity into the pool and registers it with the price
  /// history graph.  If the symbol contains special characters, a
  /// quoted version is also stored.
  ///
  /// @param symbol  The commodity symbol (e.g., "$", "AAPL", "EUR").
  /// @return Raw pointer to the newly created commodity (owned by the pool).
  commodity_t* create(const string& symbol);

  /// @brief Look up an existing commodity by its symbol.
  /// @return The commodity if found, or nullptr.
  commodity_t* find(const string& name);

  /// @brief Look up a commodity by symbol, creating it if it does not exist.
  commodity_t* find_or_create(const string& symbol);

  /// @brief Register @p name as an alias for @p referent.
  ///
  /// After this call, find("name") returns the same commodity as
  /// find(referent.base_symbol()).  If the alias name was previously
  /// used for a different commodity (e.g., created implicitly by
  /// price-db parsing before an `alias` directive was processed),
  /// the old commodity is moved to retired_commodities to keep any
  /// existing raw pointers valid, and its price history is transferred
  /// to the referent.
  ///
  /// @param name     The alias symbol.
  /// @param referent The commodity that @p name should resolve to.
  /// @return Raw pointer to the aliased commodity.
  commodity_t* alias(const string& name, commodity_t& referent);

  /// @}

  /// @name Annotated commodity operations
  /// These methods work with annotated commodities -- base commodities
  /// augmented with lot price, date, and/or tag metadata.  In journal
  /// syntax these appear as `10 AAPL {$150} [2024-01-15] (lot1)`.
  /// @{

  /// @brief Create an annotated commodity from a symbol string and annotation.
  ///
  /// If the annotation is empty, falls through to the base create().
  commodity_t* create(const string& symbol, const annotation_t& details);

  /// @brief Look up an annotated commodity by symbol and annotation.
  /// @return The annotated commodity if found, or nullptr.
  commodity_t* find(const string& symbol, const annotation_t& details);

  /// @brief Look up an annotated commodity by symbol, creating it if needed.
  commodity_t* find_or_create(const string& symbol, const annotation_t& details);

  /// @brief Look up an annotated commodity by base commodity reference,
  ///        creating it if needed.
  ///
  /// This overload is used when the caller already holds a commodity_t
  /// pointer and wants to avoid a redundant symbol lookup.
  commodity_t* find_or_create(commodity_t& comm, const annotation_t& details);

  /// @brief Create an annotated_commodity_t wrapping @p comm with @p details.
  ///
  /// The base commodity must not already be annotated.  The new annotated
  /// commodity is registered in annotated_commodities for future lookups.
  /// Flags on the base commodity are updated to track which kinds of
  /// annotations have been seen (fixated vs. floating prices, etc.).
  annotated_commodity_t* create(commodity_t& comm, const annotation_t& details);

  /// @}

  /**
   * @brief Record a simple commodity exchange as a price in the history graph.
   *
   * This is the low-level overload: it adds a price entry for @p commodity
   * at the given @p per_unit_cost and @p moment.  If the commodity is
   * annotated, the price is recorded on the underlying (unannotated) referent.
   *
   * @param commodity      The commodity being priced.
   * @param per_unit_cost  The price per unit, in some other commodity.
   * @param moment         When this exchange rate was observed.
   */
  void exchange(commodity_t& commodity, const amount_t& per_unit_cost, const datetime_t& moment);

  /**
   * @brief Record a commodity exchange with full lot annotation.
   *
   * This is the high-level overload used when processing transaction postings
   * with cost expressions (@ or @@).  It computes the per-unit cost,
   * optionally records a price in the history graph (unless the lot price is
   * fixated), and returns a cost_breakdown_t containing the amount re-annotated
   * with the lot price, acquisition date, and optional tag.
   *
   * @param amount      The quantity being exchanged.
   * @param cost        The total or per-unit cost of the exchange.
   * @param is_per_unit True if @p cost is already per-unit (from @), false if
   *                    total (from @@).
   * @param add_price   Whether to record a market price in the history graph.
   * @param moment      Transaction datetime (used for price and default lot date).
   * @param tag         Optional lot tag annotation (from `(tag)` syntax).
   * @param lot_date    Explicit lot date (overrides moment-derived date).
   * @return A cost_breakdown_t with the annotated amount and cost components.
   */
  cost_breakdown_t exchange(const amount_t& amount, const amount_t& cost,
                            const bool is_per_unit = false, const bool add_price = true,
                            const std::optional<datetime_t>& moment = {},
                            const std::optional<string>& tag = {},
                            const std::optional<date_t>& lot_date = {});

  /**
   * @brief Parse a "P" price directive line from a journal or price database.
   *
   * Accepts lines in the format "DATE [TIME] SYMBOL AMOUNT" (e.g.,
   * "2024/01/15 AAPL $185.00").  Parses the date/time, commodity symbol,
   * and price amount, then optionally records the price in the history graph.
   *
   * @param line              Mutable C string to parse (modified in place).
   * @param do_not_add_price  If true, parse only -- do not add to history.
   * @param no_date           If true, treat the line as "SYMBOL AMOUNT" without
   *                          a leading date (used for getquote script output).
   * @return A pair of (commodity, price_point) on success, or nullopt.
   */
  std::optional<std::pair<commodity_t*, price_point_t>>
  parse_price_directive(char* line, bool do_not_add_price = false, bool no_date = false);

  /**
   * @brief Parse a price expression of the form "COMMODITY=PRICE[;PRICE...]".
   *
   * Used by the --exchange/-X option to establish conversion rates.  The
   * string is split at '=' and each semicolon-separated price on the right
   * is added as a price entry for the commodity on the left.
   *
   * @param str        The price expression string.
   * @param add_prices Whether to record prices in the history graph.
   * @param moment     Timestamp for the recorded prices (default = now).
   * @return The commodity on the left-hand side, or nullptr on failure.
   */
  commodity_t* parse_price_expression(const std::string& str, const bool add_prices = true,
                                      const std::optional<datetime_t>& moment = {});

  /**
   * @brief Pre-fetch price quotes for all eligible commodities in batch.
   *
   * Collects commodities that are candidates for downloading (not builtin,
   * not flagged NOMARKET, and without a recent last_quote), groups them
   * by their exchange commodity (inferred from the most recent price in the
   * history graph), and invokes the batch quote callback once per group.
   *
   * This is called before report generation to avoid per-commodity script
   * invocations during the report (issue #588).
   */
  void prefetch_quotes();
};

} // namespace ledger
