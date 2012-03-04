/*
 * Copyright (c) 2003-2012, John Wiegley.  All rights reserved.
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
 * @brief  Types for managing commodity historys
 *
 * Long.
 */
#ifndef _HISTORY_H
#define _HISTORY_H

#include "amount.h"
#include "commodity.h"

namespace boost {
  enum edge_price_point_t { edge_price_point };
  enum edge_price_ratio_t { edge_price_ratio };
  BOOST_INSTALL_PROPERTY(edge, price_point);
  BOOST_INSTALL_PROPERTY(edge, price_ratio);
}

namespace ledger {

typedef std::map<datetime_t, amount_t> price_map_t;

template <typename EdgeWeightMap,
          typename PricePointMap,
          typename PriceRatioMap>
class recent_edge_weight
{
public:
  EdgeWeightMap weight;
  PricePointMap price_point;
  PriceRatioMap ratios;

  datetime_t           reftime;
  optional<datetime_t> oldest;

  recent_edge_weight() { }
  recent_edge_weight(EdgeWeightMap _weight,
                     PricePointMap _price_point,
                     PriceRatioMap _ratios,
                     datetime_t    _reftime,
                     const optional<datetime_t>& _oldest = none)
    : weight(_weight), price_point(_price_point), ratios(_ratios),
      reftime(_reftime), oldest(_oldest) { }

  template <typename Edge>
  bool operator()(const Edge& e) const {
    const price_map_t& prices(get(ratios, e));
    price_map_t::const_iterator low = prices.upper_bound(reftime);
    if (prices.empty() ||
        (low != prices.end() && low == prices.begin())) {
      return false;
    } else {
      if (low == prices.end())
        --low;
      assert(((*low).first <= reftime));

      if (oldest && (*low).first <= *oldest)
        return false;

      long secs = (reftime - (*low).first).total_seconds();
      assert(secs >= 0);

      put(weight, e, secs);
      put(price_point, e, price_point_t((*low).first, (*low).second));

      return true;
    }
  }
};

class commodity_history_t : public noncopyable
{
public:
  typedef adjacency_list
    <setS,                      // Store all edges as a set
     setS,                      // Store all vertices in a set
     undirectedS,               // Relations are both ways

    // All vertices are commodities
    property<vertex_name_t, const commodity_t *,
             property<vertex_index_t, std::size_t> >,

    // All edges are weights computed as the absolute difference between
    // the reference time of a search and a known price point.  A
    // filtered_graph is used to select the recent price point to the
    // reference time before performing the search.
    property<edge_weight_t, long,
             property<edge_price_ratio_t, price_map_t,
                      property<edge_price_point_t, price_point_t> > >,

    // Graph itself has a std::string name
    property<graph_name_t, std::string>
    > Graph;

  Graph price_graph;

  typedef graph_traits<Graph>::vertex_descriptor vertex_descriptor;
  typedef graph_traits<Graph>::edge_descriptor   edge_descriptor;

  typedef property_map<Graph, vertex_index_t>::type IndexMap;
  typedef property_map<Graph, vertex_name_t>::type  NameMap;

  typedef iterator_property_map<vertex_descriptor*, IndexMap,
                                vertex_descriptor,
                                vertex_descriptor&> PredecessorMap;
  typedef iterator_property_map<long*, IndexMap, long, long&> DistanceMap;

  typedef property_map<Graph, edge_weight_t>::type      EdgeWeightMap;
  typedef property_map<Graph, edge_price_point_t>::type PricePointMap;
  typedef property_map<Graph, edge_price_ratio_t>::type PriceRatioMap;

  IndexMap      indexmap;
  PricePointMap pricemap;
  PriceRatioMap ratiomap;

  typedef filtered_graph<Graph, recent_edge_weight<EdgeWeightMap,
                                                   PricePointMap,
                                                   PriceRatioMap> > FGraph;
  typedef property_map<FGraph, vertex_name_t>::type FNameMap;

  commodity_history_t()
    : indexmap(get(vertex_index, price_graph)),
      pricemap(get(edge_price_point, price_graph)),
      ratiomap(get(edge_price_ratio, price_graph)) {}

  void add_commodity(const commodity_t& comm);

  void add_price(const commodity_t& source,
                 const datetime_t&  when,
                 const amount_t&    price);
  void remove_price(const commodity_t& source,
                    const commodity_t& target,
                    const datetime_t&  date);

  optional<price_point_t>
  find_price(const commodity_t&            source,
             const datetime_t&             moment,
             const optional<datetime_t>&   oldest    = none,
             const optional<commodity_t&>& commodity = none);
};

} // namespace ledger

#endif // _HISTORY_H
