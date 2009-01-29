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

#ifndef _TIMELOG_H
#define _TIMELOG_H

#include "journal.h"

namespace ledger {

class time_entry_t
{
public:
  datetime_t  checkin;
  account_t * account;
  string      desc;

  time_entry_t() : account(NULL) {
    TRACE_CTOR(time_entry_t, "");
  }
  time_entry_t(const datetime_t& _checkin,
	       account_t *	 _account = NULL,
	       const string&     _desc	  = "")
    : checkin(_checkin), account(_account), desc(_desc) {
    TRACE_CTOR(time_entry_t, "const datetime_t&, account_t *, const string&");
  }
  time_entry_t(const time_entry_t& entry)
    : checkin(entry.checkin), account(entry.account),
      desc(entry.desc) {
    TRACE_CTOR(time_entry_t, "copy");
  }
  ~time_entry_t() throw() {
    TRACE_DTOR(time_entry_t);
  }
};

class time_log_t
{
  std::list<time_entry_t> time_entries;
  journal_t&		  journal;

public:
  time_log_t(journal_t& _journal) : journal(_journal) {
    TRACE_CTOR(time_log_t, "journal_t&");
  }
  ~time_log_t();

  void clock_in(const datetime_t& checkin,
		account_t *	  account = NULL,
		const string&     desc	  = "");

  void clock_out(const datetime_t& checkin,
		 account_t *	   account = NULL,
		 const string&     desc	  = "");
};

} // namespace ledger

#endif // _TIMELOG_H
