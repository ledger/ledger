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
 * @file   reconcile.h
 * @author John Wiegley
 *
 * @ingroup extra
 *
 * @brief Brief
 *
 * Long.
 */
#ifndef _RECONCILE_H
#define _RECONCILE_H

#include "value.h"
#include "iterators.h"
#include "filters.h"

namespace ledger {

/**
 * @brief Brief
 *
 * Long.
 */
class reconcile_xacts : public item_handler<xact_t>
{
  value_t    balance;
  date_t     cutoff;
  xacts_list xacts;

  reconcile_xacts();

public:
  reconcile_xacts(xact_handler_ptr handler,
		  const value_t&   _balance,
		  const date_t&	   _cutoff)
    : item_handler<xact_t>(handler),
      balance(_balance), cutoff(_cutoff) {
    TRACE_CTOR(reconcile_xacts,
	       "xact_handler_ptr, const value_t&, const date_t&");
  }
  virtual ~reconcile_xacts() throw() {
    TRACE_DTOR(reconcile_xacts);
  }

  void push_to_handler(xact_t * first);

  virtual void flush();
  virtual void operator()(xact_t& xact) {
    xacts.push_back(&xact);
  }
};

} // namespace ledger

#endif // _RECONCILE_H
