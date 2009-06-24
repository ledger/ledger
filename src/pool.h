/*
 * Copyright (c) 2003-2009, John Wiegley.  All rights reserved.
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

namespace ledger {

/**
 * @brief Brief
 *
 * Long.
 */
struct cost_breakdown_t
{
  amount_t amount;
  amount_t final_cost;
  amount_t basis_cost;
};

/**
 * @brief Brief
 *
 * Long.
 */
class commodity_pool_t : public noncopyable
{
  /**
   * The commodities collection in commodity_pool_t maintains pointers to all
   * the commodities which have ever been created by the user, whether
   * explicitly by calling the create methods of commodity_pool_t, or
   * implicitly by parsing a commoditized amount.
   */
  typedef std::map<string, commodity_t *> commodities_map;

public:
  commodities_map commodities;
  commodity_t *	  null_commodity;
  commodity_t *	  default_commodity;

  bool		  keep_base;	    // --base

  optional<path>  price_db;         // --price-db=
  long		  download_leeway;  // --leeway=
  bool		  download_quotes;  // --download

public:
  function<optional<price_point_t>
	   (const optional<commodity_t&>& commodity)> get_commodity_quote;

  explicit commodity_pool_t();

  ~commodity_pool_t() {
    TRACE_DTOR(commodity_pool_t);
    foreach (commodities_map::value_type pair, commodities)
      checked_delete(pair.second);
  }

  commodity_t * create(const string& symbol);
  commodity_t * find(const string& name);
  commodity_t * find_or_create(const string& symbol);

  commodity_t * create(const string& symbol, const annotation_t& details);
  commodity_t * find(const string& symbol, const annotation_t& details);
  commodity_t * find_or_create(const string& symbol,
			       const annotation_t& details);

  commodity_t * create(commodity_t&	   comm,
		       const annotation_t& details,
		       const string&	   mapping_key);

  commodity_t * find_or_create(commodity_t&	   comm,
			       const annotation_t& details);

  // Exchange one commodity for another, while recording the factored price.

  void exchange(commodity_t&	  commodity,
		const amount_t&   per_unit_cost,
		const datetime_t& moment);

  cost_breakdown_t exchange(const amount_t&	        amount,
			    const amount_t&	        cost,
			    const bool		        is_per_unit = false,
			    const optional<datetime_t>& moment	   = none,
			    const optional<string>&     tag	   = none);

  // Parse commodity prices from a textual representation

  optional<price_point_t> parse_price_directive(char * line);

  commodity_t *
  parse_price_expression(const std::string&          str,
			 const bool                  add_prices = true,
			 const optional<datetime_t>& moment     = none);
};

} // namespace ledger

#endif // _POOL_H
