/*
 * Copyright (c) 2003-2010, John Wiegley.  All rights reserved.
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
 * @file   timelog.h
 * @author John Wiegley
 *
 * @ingroup data
 */
#ifndef _TIMELOG_H
#define _TIMELOG_H

#include "utils.h"
#include "times.h"
#include "item.h"

namespace ledger {

class account_t;
class journal_t;

class time_xact_t
{
public:
  datetime_t  checkin;
  account_t * account;
  string      desc;
  string      note;
  position_t  position;

  time_xact_t() : account(NULL) {
    TRACE_CTOR(time_xact_t, "");
  }
  time_xact_t(const optional<position_t>& _position,
              const datetime_t&           _checkin,
              account_t *                 _account = NULL,
              const string&               _desc    = "",
              const string&               _note    = "")
    : checkin(_checkin), account(_account), desc(_desc), note(_note),
      position(_position ? *_position : position_t()) {
    TRACE_CTOR(time_xact_t,
               "position_t, datetime_t, account_t *, string, string");
  }
  time_xact_t(const time_xact_t& xact)
    : checkin(xact.checkin), account(xact.account),
      desc(xact.desc), note(xact.note), position(xact.position) {
    TRACE_CTOR(time_xact_t, "copy");
  }
  ~time_xact_t() throw() {
    TRACE_DTOR(time_xact_t);
  }
};

class time_log_t
{
  std::list<time_xact_t> time_xacts;
  journal_t&             journal;
  scope_t&               scope;

public:
  time_log_t(journal_t& _journal, scope_t& _scope)
    : journal(_journal), scope(_scope) {
    TRACE_CTOR(time_log_t, "journal_t&, scope_t&");
  }
  ~time_log_t();

  void clock_in(time_xact_t event);
  void clock_out(time_xact_t event);
};

} // namespace ledger

#endif // _TIMELOG_H
