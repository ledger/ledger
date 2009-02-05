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
 * @addtogroup extra
 */

/**
 * @file   quotes.h
 * @author John Wiegley
 *
 * @ingroup extra
 *
 * @brief Brief
 *
 * Long.
 */
#ifndef _QUOTES_H
#define _QUOTES_H

#include "amount.h"

namespace ledger {

#if 0
/**
 * @brief Brief
 *
 * Long.
 */
class quotes_by_script : public noncopyable, public commodity_t::base_t::updater_t
{
  string      price_db;
  std::size_t pricing_leeway;

  quotes_by_script();

public:
  quotes_by_script(path	       _price_db,
		   std::size_t _pricing_leeway)
    : price_db(_price_db), pricing_leeway(_pricing_leeway) {
    TRACE_CTOR(quotes_by_script, "path, std::size_t, bool&");
  }
  ~quotes_by_script() throw() {
    TRACE_DTOR(quotes_by_script);
  }

  virtual void operator()(commodity_base_t& commodity,
			  const datetime_t& moment,
			  const datetime_t& date,
			  const datetime_t& last,
			  amount_t&	    price);
};
#endif

} // namespace ledger

#endif // _QUOTES_H
