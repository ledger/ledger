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

namespace ledger {

typedef std::map<datetime_t, amount_t> price_map_t;

class commodity_history_impl_t;
class commodity_history_t : public noncopyable
{
  unique_ptr<commodity_history_impl_t> p_impl;

public:
  commodity_history_t();

  void add_commodity(commodity_t& comm);

  void add_price(const commodity_t& source,
                 const datetime_t&  when,
                 const amount_t&    price);
  void remove_price(const commodity_t& source,
                    const commodity_t& target,
                    const datetime_t&  date);

  void map_prices(function<void(datetime_t, const amount_t&)> fn,
                  const commodity_t& source,
                  const datetime_t&  moment,
                  const datetime_t&  _oldest = datetime_t(),
                  bool bidirectionally = false);

  boost::optional<price_point_t>
  find_price(const commodity_t& source,
             const datetime_t&  moment,
             const datetime_t&  oldest = datetime_t());

  boost::optional<price_point_t>
  find_price(const commodity_t& source,
             const commodity_t& target,
             const datetime_t&  moment,
             const datetime_t&  oldest = datetime_t());

  void print_map(std::ostream& out, const datetime_t& moment = datetime_t());
  ~commodity_history_t();
};

} // namespace ledger

#endif // _HISTORY_H
