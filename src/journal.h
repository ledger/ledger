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
 * @addtogroup data
 */

/**
 * @file   journal.h
 * @author John Wiegley
 *
 * @ingroup data
 *
 * @brief Brief
 *
 * Long.
 */
#ifndef _JOURNAL_H
#define _JOURNAL_H

#include "utils.h"
#include "hooks.h"
#include "xact.h"

namespace ledger {

typedef std::list<path>	paths_list;

class account_t;

/**
 * @brief Brief
 *
 * Long.
 */
class journal_t : public noncopyable
{
public:
  account_t *  master;
  account_t *  basket;
  xacts_list xacts;

  auto_xacts_list   auto_xacts;
  period_xacts_list period_xacts;

  hooks_t<xact_finalizer_t, xact_t> xact_finalize_hooks;

  journal_t(account_t * _master = NULL) : master(_master) {
    TRACE_CTOR(journal_t, "");
  }
  ~journal_t();

  // These four methods are delegated to the current session, since all
  // accounts processed are gathered together at the session level.
  void	      add_account(account_t * acct);
  bool	      remove_account(account_t * acct);
  account_t * find_account(const string& name, bool auto_create = true);
  account_t * find_account_re(const string& regexp);

  bool add_xact(xact_t * xact);
  bool remove_xact(xact_t * xact);

  void add_xact_finalizer(xact_finalizer_t * finalizer) {
    xact_finalize_hooks.add_hook(finalizer);
  }
  void remove_xact_finalizer(xact_finalizer_t * finalizer) {
    xact_finalize_hooks.remove_hook(finalizer);
  }

  std::size_t parse(std::istream& in,
		    scope_t&      session_scope,
		    account_t *   master	= NULL,
		    const path *  original_file = NULL,
		    bool          strict	= false);

  bool valid() const;
};

extern const string version;

} // namespace ledger

#endif // _JOURNAL_H
