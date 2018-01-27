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
 * @addtogroup report
 */

/**
 * @file   temps.h
 * @author John Wiegley
 *
 * @ingroup report
 */
#ifndef _TEMPS_H
#define _TEMPS_H

namespace ledger {

class temporaries_t
{
  optional<std::list<xact_t> >    xact_temps;
  optional<std::list<post_t> >    post_temps;
  optional<std::list<account_t> > acct_temps;

public:
  temporaries_t() {
    TRACE_CTOR(temporaries_t, "");
  }
  ~temporaries_t() {
    TRACE_DTOR(temporaries_t);
    clear();
  }

  xact_t&    copy_xact(xact_t& origin);
  xact_t&    create_xact();
  xact_t&    last_xact() {
    return xact_temps->back();
  }
  post_t&    copy_post(post_t& origin, xact_t& xact,
                       account_t * account = NULL);
  post_t&    create_post(xact_t& xact, account_t * account,
                         bool bidir_link = true);
  post_t&    last_post() {
    return post_temps->back();
  }
  account_t& create_account(const string& name   = "",
                            account_t *   parent = NULL);
  account_t& last_account() {
    return acct_temps->back();
  }

  void clear();
};

} // namespace ledger

#endif // _TEMPS_H
