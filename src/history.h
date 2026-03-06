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
 * @file   history.h
 * @author John Wiegley
 *
 * @ingroup math
 *
 * @brief Commodity price history: graph-based lookups for `--market` and `--exchange`.
 *
 * When the user writes `P 2024/01/15 EUR $1.10`, Ledger records a price
 * relationship between two commodities at a point in time.  Over the life
 * of a journal, many such prices accumulate, forming a graph where:
 *
 * - **Vertices** are commodities (EUR, $, BTC, AAPL, ...)
 * - **Edges** carry time-stamped exchange rates between two commodities
 *
 * `commodity_history_t` manages this graph and answers two kinds of queries:
 *
 * 1. **Direct lookup** (`find_price(source, moment)`): Find the most recent
 *    price for a commodity relative to any other commodity, used when the
 *    target is unspecified (e.g., `--market`).
 *
 * 2. **Targeted lookup** (`find_price(source, target, moment)`): Find the
 *    exchange rate between two specific commodities, possibly through
 *    intermediate commodities.  This uses Dijkstra's shortest-path
 *    algorithm on the price graph, where edge weights represent how
 *    recently a price was recorded (preferring more recent prices).
 *
 * The implementation is hidden behind the PIMPL pattern
 * (`commodity_history_impl_t`) to isolate Boost.Graph headers from the
 * rest of the codebase, since they are expensive to compile.
 */
#pragma once

#include "amount.h"
#include "commodity.h"

namespace ledger {

/// Maps points in time to exchange rate amounts.  Each edge in the price
/// graph carries one of these, recording the full history of price
/// observations between two commodities.
using price_map_t = std::map<datetime_t, amount_t>;

class commodity_history_impl_t;

/**
 * @brief The commodity price graph: records and queries exchange rates.
 *
 * This is the public interface to the price history subsystem.  It uses
 * the PIMPL pattern to hide the Boost.Graph implementation details
 * (which pull in heavy template headers) from the rest of the codebase.
 *
 * The price graph is populated during journal parsing whenever a `P`
 * price directive or an exchange-price posting is encountered.  It is
 * queried during report generation when `--market`, `--exchange`, or
 * value expressions need to convert between commodities.
 */
class commodity_history_t : public noncopyable {
  unique_ptr<commodity_history_impl_t> p_impl; ///< PIMPL hiding Boost.Graph types

public:
  commodity_history_t();

  /// Register a commodity as a vertex in the price graph.
  /// Must be called before any prices involving this commodity are added.
  void add_commodity(commodity_t& comm);

  /// Transfer the graph vertex from @p old_comm to @p new_comm, so that
  /// all existing price edges now reference @p new_comm.  Used when a
  /// commodity is redefined or merged.
  void alias_commodity(commodity_t& old_comm, commodity_t& new_comm);

  /// Record a price: at time @p when, one unit of @p source costs @p price
  /// (whose commodity is the target).
  void add_price(const commodity_t& source, const datetime_t& when, const amount_t& price);

  /// Remove a previously recorded price between @p source and @p target
  /// at the given @p date.
  void remove_price(const commodity_t& source, const commodity_t& target, const datetime_t& date);

  /// Iterate over all prices for @p source that fall within the time range
  /// [@p _oldest, @p moment], calling @p fn for each.  When
  /// @p bidirectionally is true, also yields inverted prices (i.e., if
  /// A->B is recorded, also yield B->A).
  void map_prices(const function<void(datetime_t, const amount_t&)>& fn, const commodity_t& source,
                  const datetime_t& moment, const datetime_t& _oldest = datetime_t(),
                  bool bidirectionally = false);

  /// Find the most recent price for @p source relative to any other
  /// commodity, at or before @p moment (and no earlier than @p oldest).
  /// Returns nullopt if no price is available in the given time range.
  std::optional<price_point_t> find_price(const commodity_t& source, const datetime_t& moment,
                                          const datetime_t& oldest = datetime_t());

  /// Find the exchange rate from @p source to @p target at @p moment,
  /// possibly through intermediate commodities using shortest-path search.
  /// Returns nullopt if no path exists or all prices are out of range.
  std::optional<price_point_t> find_price(const commodity_t& source, const commodity_t& target,
                                          const datetime_t& moment,
                                          const datetime_t& oldest = datetime_t());

  /// Write the price graph in Graphviz DOT format for visualization.
  /// If @p moment is specified, only edges with prices at or before
  /// that time are included.
  void print_map(std::ostream& out, const datetime_t& moment = datetime_t());
  ~commodity_history_t();
};

} // namespace ledger
