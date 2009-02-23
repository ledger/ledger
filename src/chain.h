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
 * @addtogroup report
 */

/**
 * @file   chain.h
 * @author John Wiegley
 *
 * @ingroup report
 *
 * @brief Brief
 *
 * Long.
 */
#ifndef _CHAIN_H
#define _CHAIN_H

#include "post.h"
#include "account.h"

namespace ledger {

/**
 * @brief Brief
 *
 * Long.
 */
template <typename T>
struct item_handler : public noncopyable
{
  shared_ptr<item_handler> handler;

public:
  item_handler() {
    TRACE_CTOR(item_handler, "");
  }
  item_handler(shared_ptr<item_handler> _handler) : handler(_handler) {
    TRACE_CTOR(item_handler, "shared_ptr<item_handler>");
  }
  virtual ~item_handler() {
    TRACE_DTOR(item_handler);
  }

  virtual void flush() {
    if (handler.get())
      handler->flush();
  }
  virtual void operator()(T& item) {
    if (handler.get()) {
      check_for_signal();
      (*handler.get())(item);
    }
  }
};

typedef shared_ptr<item_handler<post_t> > post_handler_ptr;
typedef shared_ptr<item_handler<account_t> > acct_handler_ptr;

class report_t;
post_handler_ptr
chain_post_handlers(report_t&	     report,
		    post_handler_ptr base_handler,
		    bool             only_preliminaries = false);

} // namespace ledger

#endif // _CHAIN_H
