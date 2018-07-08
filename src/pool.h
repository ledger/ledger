/*
 * Copyright (c) 2003-2018, John Wiegley.  All rights reserved.
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
 * Long.
 */
#ifndef _POOL_H
#define _POOL_H

#include "history.h"
#include "annotate.h"

namespace ledger {

struct cost_breakdown_t
{
  amount_t amount;
  amount_t final_cost;
  amount_t basis_cost;
};

class commodity_pool_t : public noncopyable
{
public:
  /**
   * The commodities collection in commodity_pool_t maintains pointers to all
   * the commodities which have ever been created by the user, whether
   * explicitly by calling the create methods of commodity_pool_t, or
   * implicitly by parsing a commoditized amount.
   */
  typedef std::map<string, shared_ptr<commodity_t> > commodities_map;
  typedef std::map<std::pair<string, annotation_t>,
                   shared_ptr<annotated_commodity_t> > annotated_commodities_map;

  commodities_map           commodities;
  annotated_commodities_map annotated_commodities;
  commodity_history_t       commodity_price_history;
  commodity_t *             null_commodity;
  commodity_t *             default_commodity;

  bool           keep_base;     // --base
  optional<path> price_db;      // --price-db=
  long           quote_leeway;  // --leeway=
  bool           get_quotes;    // --download

  function<optional<price_point_t>
           (commodity_t& commodity, const commodity_t * in_terms_of)>
      get_commodity_quote;

  static shared_ptr<commodity_pool_t> current_pool;

  explicit commodity_pool_t();
  virtual ~commodity_pool_t() {
    TRACE_DTOR(commodity_pool_t);
  }

  commodity_t * create(const string& symbol);
  commodity_t * find(const string& name);
  commodity_t * find_or_create(const string& symbol);
  commodity_t * alias(const string& name, commodity_t& referent);

  commodity_t * create(const string& symbol,
                       const annotation_t& details);
  commodity_t * find(const string& symbol,
                     const annotation_t& details);
  commodity_t * find_or_create(const string& symbol,
                               const annotation_t& details);
  commodity_t * find_or_create(commodity_t& comm, const annotation_t& details);

  annotated_commodity_t * create(commodity_t& comm,
                                 const annotation_t& details);

  // Exchange one commodity for another, while recording the factored price.

  void exchange(commodity_t&      commodity,
                const amount_t&   per_unit_cost,
                const datetime_t& moment);

  cost_breakdown_t exchange(const amount_t&             amount,
                            const amount_t&             cost,
                            const bool                  is_per_unit = false,
                            const bool                  add_price   = true,
                            const optional<datetime_t>& moment     = none,
                            const optional<string>&     tag        = none);

  // Parse commodity prices from a textual representation

  optional<std::pair<commodity_t *, price_point_t> >
  parse_price_directive(char * line, bool do_not_add_price = false,
                        bool no_date = false);

  commodity_t *
  parse_price_expression(const std::string&          str,
                         const bool                  add_prices = true,
                         const optional<datetime_t>& moment     = none);
};

} // namespace ledger

#endif // _POOL_H
