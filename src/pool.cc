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
 * @file   pool.cc
 * @author John Wiegley
 *
 * @ingroup math
 *
 * @brief Implements the commodity pool: commodity creation, lookup, aliasing,
 *        and exchange recording.
 *
 * The commodity pool is the central registry for all commodity_t and
 * annotated_commodity_t instances.  This file implements the create/find/
 * find_or_create family for both plain and annotated commodities, the alias
 * mechanism (with its retired_commodities safety net), and the exchange()
 * methods that record lot-based commodity conversions with proper cost
 * breakdown and annotation.  It also contains the parsers for P directives
 * and price expressions.
 */

#include <system.hh>

#include "amount.h"
#include "commodity.h"
#include "annotate.h"
#include "pool.h"
#include "history.h"
#include "quotes.h"

namespace ledger {

std::shared_ptr<commodity_pool_t> commodity_pool_t::current_pool;

/**
 * @brief Construct a new commodity pool with a null commodity.
 *
 * The null commodity (empty symbol) is created immediately and marked as
 * BUILTIN | NOMARKET.  It serves as a sentinel for amounts that have no
 * commodity (pure numeric values).  The default quote_leeway is 86400
 * seconds (24 hours), meaning downloaded prices are considered fresh
 * for one day.
 */
commodity_pool_t::commodity_pool_t()
    : default_commodity(nullptr), keep_base(false), quote_leeway(86400), get_quotes(false),
      get_commodity_quote(commodity_quote_from_script) {
  null_commodity = create("");
  null_commodity->add_flags(COMMODITY_BUILTIN | COMMODITY_NOMARKET);
  TRACE_CTOR(commodity_pool_t, "");
}

/**
 * @brief Create a new commodity with the given symbol.
 *
 * Allocates a new base_t and commodity_t, wraps the symbol in quotes if it
 * contains characters from invalid_chars (e.g., spaces or digits), inserts
 * the commodity into the commodities map, and registers it as a node in the
 * price history graph.  The caller must ensure the symbol does not already
 * exist in the pool.
 */
commodity_t* commodity_pool_t::create(const string& symbol) {
  std::shared_ptr<commodity_t::base_t> base_commodity(new commodity_t::base_t(symbol));
  std::shared_ptr<commodity_t> commodity(new commodity_t(this, base_commodity));

  DEBUG("pool.commodities", "Creating base commodity " << symbol);

  // Create the "qualified symbol" version of this commodity's symbol
  if (commodity_t::symbol_needs_quotes(symbol)) {
    commodity->qualified_symbol = "\"";
    *commodity->qualified_symbol += symbol;
    *commodity->qualified_symbol += "\"";
  }

  DEBUG("pool.commodities", "Creating commodity '" << symbol << "'");

#if DEBUG_ON
  std::pair<commodities_map::iterator, bool> result =
#endif
      commodities.insert(commodities_map::value_type(symbol, commodity));
#if DEBUG_ON
  assert(result.second);
#endif

  commodity_price_history.add_commodity(*commodity.get());

  return commodity.get();
}

/** @brief Look up a commodity by its symbol.  Returns nullptr if not found. */
commodity_t* commodity_pool_t::find(const string& symbol) {
  DEBUG("pool.commodities", "Find commodity " << symbol);

  if (auto i = commodities.find(symbol); i != commodities.end())
    return (*i).second.get();
  return nullptr;
}

/** @brief Look up a commodity by symbol, creating it if it does not exist. */
commodity_t* commodity_pool_t::find_or_create(const string& symbol) {
  DEBUG("pool.commodities", "Find-or-create commodity " << symbol);
  if (commodity_t* commodity = find(symbol))
    return commodity;
  return create(symbol);
}

/**
 * @brief Create an alias mapping @p name to @p referent's commodity entry.
 *
 * After aliasing, looking up @p name in the commodities map returns the same
 * shared_ptr as the referent's canonical entry.  If @p name was previously
 * mapped to a different commodity (e.g., created implicitly by a pricedb
 * parse before the alias directive was seen), the old commodity is moved to
 * retired_commodities to keep it alive -- existing amount_t objects may still
 * hold raw pointers to it.  Any price history on the old commodity is
 * transferred to the referent via commodity_history::alias_commodity.
 *
 * Aliasing the same name to the same referent is idempotent.
 */
commodity_t* commodity_pool_t::alias(const string& name, commodity_t& referent) {
  commodities_map::const_iterator i = commodities.find(referent.base_symbol());
  assert(i != commodities.end());

  auto [iter, inserted] = commodities.insert(commodities_map::value_type(name, (*i).second));
  if (!inserted) {
    if (iter->second.get() == (*i).second.get()) {
      // The alias is already registered with the same referent; this is idempotent.
      return iter->second.get();
    }
    // The name was previously used for a different commodity (e.g., created implicitly
    // by pricedb parsing before the alias directive was processed, or an include file
    // with commodity aliases was included multiple times).
    //
    // Keep the old commodity alive (to prevent dangling raw pointers held by existing
    // amount_t objects in the price history), transfer price history to the referent,
    // then remap the commodities map entry.
    retired_commodities.push_back(iter->second);
    commodity_price_history.alias_commodity(*iter->second.get(), referent);
    iter->second = (*i).second;
  }

  return (*iter).second.get();
}

/// @name Annotated commodity operations
/// @{

/**
 * @brief Create an annotated commodity from a symbol and annotation.
 *
 * If the annotation is non-empty, the base commodity is looked up (or
 * created) and then wrapped in an annotated_commodity_t.  If the
 * annotation is empty, falls through to the plain create().
 */
commodity_t* commodity_pool_t::create(const string& symbol, const annotation_t& details) {
  DEBUG("pool.commodities", "commodity_pool_t::create[ann] " << "symbol " << symbol << '\n'
                                                             << details);

  if (details)
    return create(*find_or_create(symbol), details);
  else
    return create(symbol);
}

/** @brief Look up an annotated commodity by symbol and annotation details. */
commodity_t* commodity_pool_t::find(const string& symbol, const annotation_t& details) {
  DEBUG("pool.commodities", "commodity_pool_t::find[ann] " << "symbol " << symbol << '\n'
                                                           << details);

  if (auto i = annotated_commodities.find(annotated_commodities_map::key_type(symbol, details));
      i != annotated_commodities.end()) {
    DEBUG("pool.commodities", "commodity_pool_t::find[ann] found "
                                  << "symbol " << (*i).second->base_symbol() << '\n'
                                  << as_annotated_commodity(*(*i).second.get()).details);
    return (*i).second.get();
  } else {
    return nullptr;
  }
}

/** @brief Look up an annotated commodity by symbol, creating it if needed. */
commodity_t* commodity_pool_t::find_or_create(const string& symbol, const annotation_t& details) {
  DEBUG("pool.commodities", "commodity_pool_t::find_or_create[ann] " << "symbol " << symbol << '\n'
                                                                     << details);

  if (details) {
    if (commodity_t* ann_comm = find(symbol, details)) {
      assert(ann_comm->annotated && as_annotated_commodity(*ann_comm).details);
      return ann_comm;
    } else {
      return create(symbol, details);
    }
  } else {
    return find_or_create(symbol);
  }
}

/** @brief Look up an annotated commodity by base commodity reference, creating if needed. */
commodity_t* commodity_pool_t::find_or_create(commodity_t& comm, const annotation_t& details) {
  DEBUG("pool.commodities", "commodity_pool_t::find_or_create[ann:comm] "
                                << "symbol " << comm.base_symbol() << '\n'
                                << details);

  if (details) {
    if (commodity_t* ann_comm = find(comm.base_symbol(), details)) {
      assert(ann_comm->annotated && as_annotated_commodity(*ann_comm).details);
      return ann_comm;
    } else {
      return create(comm, details);
    }
  } else {
    return &comm;
  }
}

/**
 * @brief Create an annotated_commodity_t wrapping a base commodity.
 *
 * The base commodity must not already be annotated, and the annotation
 * must be non-empty.  The new annotated commodity is inserted into the
 * annotated_commodities map keyed by (base_symbol, details).  Flags on
 * the base commodity are updated to record whether fixated or floating
 * price annotations have been seen (COMMODITY_SAW_ANN_PRICE_FIXATED,
 * COMMODITY_SAW_ANN_PRICE_FLOAT), which influences find_price() behavior.
 */
annotated_commodity_t* commodity_pool_t::create(commodity_t& comm, const annotation_t& details) {
  DEBUG("pool.commodities", "commodity_pool_t::create[ann:comm] " << "symbol " << comm.base_symbol()
                                                                  << '\n'
                                                                  << details);

  assert(comm);
  assert(!comm.has_annotation());
  assert(details);

  std::shared_ptr<annotated_commodity_t> commodity(new annotated_commodity_t(&comm, details));

  comm.add_flags(COMMODITY_SAW_ANNOTATED);
  if (details.price) {
    if (details.has_flags(ANNOTATION_PRICE_FIXATED))
      comm.add_flags(COMMODITY_SAW_ANN_PRICE_FIXATED);
    else
      comm.add_flags(COMMODITY_SAW_ANN_PRICE_FLOAT);
  }

  DEBUG("pool.commodities", "Creating annotated commodity " << "symbol " << commodity->base_symbol()
                                                            << '\n'
                                                            << details);

#if DEBUG_ON
  std::pair<annotated_commodities_map::iterator, bool> result =
#endif
      annotated_commodities.insert(annotated_commodities_map::value_type(
          annotated_commodities_map::key_type(comm.base_symbol(), details), commodity));
#if DEBUG_ON
  assert(result.second);
#endif

  return commodity.get();
}

/// @}

/// @name Exchange and price operations
/// @{

/**
 * @brief Record a simple per-unit exchange rate in the price history.
 *
 * Resolves through any annotation to the base commodity, then delegates
 * to commodity_t::add_price.
 */
void commodity_pool_t::exchange(commodity_t& commodity, const amount_t& per_unit_cost,
                                const datetime_t& moment) {
  DEBUG("commodity.prices.add", "exchanging commodity " << commodity << " at per unit cost "
                                                        << per_unit_cost << " on " << moment);

  commodity_t& base_commodity(commodity.annotated ? as_annotated_commodity(commodity).referent()
                                                  : commodity);

  base_commodity.add_price(moment, per_unit_cost);
}

/**
 * @brief Record a full commodity exchange with lot annotation.
 *
 * This is the workhorse for "@" and "@@" cost expressions in postings.
 * It performs these steps:
 *   1. Compute the per-unit cost (dividing total cost by quantity for @@).
 *   2. Normalize the per-unit cost to display precision so that lot prices
 *      computed from total costs can be matched by users (issue #1032).
 *   3. Optionally record a market price in the history graph (skipped for
 *      fixated prices and lot-basis consumption -- issue #1217).
 *   4. Build an annotation_t with price, date, and tag; mark computed
 *      fields with ANNOTATION_*_CALCULATED flags.
 *   5. Re-annotate the original amount with the new lot information.
 *   6. Return the cost_breakdown_t with the annotated amount, the final
 *      cost (what was paid), and the basis cost (using any prior lot price).
 */
cost_breakdown_t commodity_pool_t::exchange(const amount_t& amount, const amount_t& cost,
                                            const bool is_per_unit, const bool add_price,
                                            const std::optional<datetime_t>& moment,
                                            const std::optional<string>& tag,
                                            const std::optional<date_t>& lot_date) {
  DEBUG("commodity.prices.add", "exchange: " << amount << " for " << cost);
  DEBUG("commodity.prices.add", "exchange: is-per-unit   = " << is_per_unit);
#if DEBUG_ON
  if (moment)
    DEBUG("commodity.prices.add", "exchange: moment        = " << *moment);
  if (tag)
    DEBUG("commodity.prices.add", "exchange: tag           = " << *tag);
#endif

  commodity_t& commodity(amount.commodity());

  annotation_t* current_annotation = nullptr;
  if (commodity.annotated)
    current_annotation = &as_annotated_commodity(commodity).details;

  amount_t per_unit_cost = (is_per_unit || amount.is_zero()) ? cost.abs() : (cost / amount).abs();

  if (!cost.has_commodity())
    per_unit_cost.clear_commodity();

  if (cost.has_annotation())
    per_unit_cost = per_unit_cost.strip_annotations(keep_details_t());

  // Normalize per-unit cost to a stable precision so that lot prices are
  // deterministic regardless of how the total cost was written.  Without
  // this, `@@ $250` and `@@ $250.00` would produce different per-unit
  // prices because the internal precision metadata of the cost amount
  // differs (0 vs 2), leading to different division precisions.
  //
  // For costs computed by division (total cost / quantity), the rounding
  // precision is derived from the divisor (quantity) precision and the
  // cost commodity's display precision, deliberately excluding the
  // dividend (total cost) precision.  This ensures that $250 and $250.00
  // -- which are the same GMP rational -- produce identical per-unit
  // prices after rounding (fixes #2975).
  //
  // For per-unit costs (no division), the existing display_precision()
  // is used, preserving the user-typed precision (fixes #1032).
  if (per_unit_cost.has_commodity() && per_unit_cost.keep_precision()) {
    int round_prec;
    if (!is_per_unit && !amount.is_zero()) {
      round_prec = static_cast<int>(amount.precision() + per_unit_cost.commodity().precision() +
                                    amount_t::extend_by_digits);
    } else {
      round_prec = static_cast<int>(per_unit_cost.display_precision());
    }
    per_unit_cost.in_place_roundto(round_prec);
  }

  DEBUG("commodity.prices.add", "exchange: per-unit-cost = " << per_unit_cost);

  // Do not record commodity exchanges where amount's commodity has a
  // fixated price, since this does not establish a market value for the
  // base commodity.
  //
  // Also skip recording a price when the cost has a lot price annotation
  // whose currency matches the amount's commodity.  In that case the cost
  // is a lot-basis consumption rather than a true market exchange: the
  // implied rate (amount / cost) is an artifact of the lot, not a new
  // market price.  Recording it would incorrectly mark the cost commodity
  // as COMMODITY_PRIMARY and cause -V to stop converting it (issue #1217).
  bool cost_has_lot_price_in_amount_currency =
      cost.has_annotation() && cost.annotation().price &&
      cost.annotation().price->has_commodity() &&
      cost.annotation().price->commodity().referent() == commodity.referent();

  if (add_price && !per_unit_cost.is_realzero() &&
      (current_annotation == nullptr ||
       !(current_annotation->price && current_annotation->has_flags(ANNOTATION_PRICE_FIXATED))) &&
      !cost_has_lot_price_in_amount_currency &&
      commodity.referent() != per_unit_cost.commodity().referent()) {
    exchange(commodity, per_unit_cost, moment ? *moment : CURRENT_TIME());
  }

  cost_breakdown_t breakdown;
  breakdown.final_cost = !is_per_unit ? cost : cost * amount.abs();

  DEBUG("commodity.prices.add", "exchange: final-cost    = " << breakdown.final_cost);

  if (current_annotation && current_annotation->price)
    breakdown.basis_cost = (*current_annotation->price * amount).unrounded();
  else
    breakdown.basis_cost = breakdown.final_cost;

  DEBUG("commodity.prices.add", "exchange: basis-cost    = " << breakdown.basis_cost);

  annotation_t annotation(
      per_unit_cost,
      lot_date ? lot_date
               : (moment ? std::optional<date_t>(moment->date()) : std::optional<date_t>{}),
      tag);

  annotation.add_flags(ANNOTATION_PRICE_CALCULATED);
  if (current_annotation && current_annotation->has_flags(ANNOTATION_PRICE_FIXATED))
    annotation.add_flags(ANNOTATION_PRICE_FIXATED);
  if (!lot_date && moment)
    annotation.add_flags(ANNOTATION_DATE_CALCULATED);
  if (tag)
    annotation.add_flags(ANNOTATION_TAG_CALCULATED);

  breakdown.amount = amount_t(amount, annotation);

  DEBUG("commodity.prices.add", "exchange: amount        = " << breakdown.amount);

  return breakdown;
}

/// @}

/// @name Price directive parsing
/// @{

/**
 * @brief Parse a price directive line ("P DATE [TIME] SYMBOL AMOUNT").
 *
 * The line is split into whitespace-separated fields.  If a time component
 * is present, it is combined with the date into a full datetime; otherwise,
 * midnight is assumed.  The commodity symbol is parsed (handling quoted
 * symbols), and the remaining text is parsed as the price amount.
 *
 * On success, the commodity is looked up (or created) in the pool, the
 * price is optionally added to the history graph, and the commodity is
 * marked COMMODITY_KNOWN.  The price commodity's precision may be updated
 * from the parsed amount if it was previously at the default.
 */
std::optional<std::pair<commodity_t*, price_point_t>>
commodity_pool_t::parse_price_directive(char* line, bool do_not_add_price, bool no_date) {
  char* date_field_ptr = line;
  char* time_field_ptr = next_element(date_field_ptr);
  if (!time_field_ptr)
    return std::nullopt;
  string date_field = date_field_ptr; // NOLINT(bugprone-unused-local-non-trivial-variable)

  char* symbol_and_price;
  datetime_t datetime;
  string symbol;

  if (!no_date && std::isdigit(static_cast<unsigned char>(time_field_ptr[0]))) {
    symbol_and_price = next_element(time_field_ptr);
    if (!symbol_and_price)
      return std::nullopt;

    datetime = parse_datetime(date_field + " " + time_field_ptr);
  } else if (!no_date && std::isdigit(static_cast<unsigned char>(date_field_ptr[0]))) {
    symbol_and_price = time_field_ptr;
    datetime = datetime_t(parse_date(date_field));
  } else {
    symbol = date_field_ptr;
    symbol_and_price = time_field_ptr;
    datetime = CURRENT_TIME();
  }

  if (symbol.empty())
    commodity_t::parse_symbol(symbol_and_price, symbol);

  price_point_t point;
  point.when = datetime;
  (void)point.price.parse(symbol_and_price, PARSE_NO_MIGRATE);
  VERIFY(point.price.valid());

  // Update the price commodity's precision based on the parsed amount.
  // When PARSE_NO_MIGRATE is used, the commodity precision isn't automatically
  // updated during parsing, so we need to do it manually here. However, only
  // update if the commodity's precision is at the default value (0) AND the
  // commodity hasn't been explicitly declared (COMMODITY_KNOWN) or had its
  // format set (COMMODITY_STYLE_NO_MIGRATE). This ensures we don't override
  // precision set by commodity directives, D directives, or N directives.
  if (point.price.has_commodity()) {
    commodity_t& price_commodity = point.price.commodity();
    if (price_commodity.precision() == 0 && !price_commodity.has_flags(COMMODITY_KNOWN) &&
        !price_commodity.has_flags(COMMODITY_STYLE_NO_MIGRATE)) {
      price_commodity.set_precision(point.price.precision());
      price_commodity.add_flags(COMMODITY_PRECISION_FROM_PRICE);
    }
  }

  DEBUG("commodity.download", "Looking up symbol: " << symbol);
  if (commodity_t* commodity = find_or_create(symbol)) {
    if (point.price.has_commodity() && commodity->referent() == point.price.commodity().referent())
      throw_(parse_error,
             _f("A price directive may not use the same commodity for both source and price: %1%") %
                 symbol);
    DEBUG("commodity.download",
          "Adding price for " << symbol << ": " << point.when << " " << point.price);
    if (!do_not_add_price)
      commodity->add_price(point.when, point.price, true);
    commodity->add_flags(COMMODITY_KNOWN);
    return std::pair<commodity_t*, price_point_t>(commodity, point);
  }

  return std::nullopt;
}

/**
 * @brief Parse a "COMMODITY=PRICE[;PRICE;...]" expression.
 *
 * Splits the string at '=' to obtain the commodity name on the left and
 * one or more semicolon-separated price amounts on the right.  Each price
 * is added to the commodity's history at the given moment (or now).  Used
 * by the --exchange/-X command-line option.
 */
commodity_t* commodity_pool_t::parse_price_expression(const std::string& str, const bool add_prices,
                                                      const std::optional<datetime_t>& moment) {
  scoped_array<char> buf(new char[str.length() + 1]);

  std::strcpy(buf.get(), str.c_str());

  char* price = std::strchr(buf.get(), '=');
  if (price)
    *price++ = '\0';

  if (commodity_t* commodity = find_or_create(trim_ws(buf.get()))) {
    if (price && add_prices) {
      for (char* p = std::strtok(price, ";"); p; p = std::strtok(nullptr, ";")) {
        commodity->add_price(moment ? *moment : CURRENT_TIME(), amount_t(p));
      }
    }
    return commodity;
  }
  return nullptr;
}

/// @}

} // namespace ledger
