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

#include <system.hh>

#include "timelog.h"
#include "xact.h"
#include "post.h"
#include "account.h"
#include "journal.h"
#include "context.h"

namespace ledger {

namespace {
  void create_timelog_xact(const time_xact_t& in_event,
                           const time_xact_t& out_event,
                           parse_context_t&   context)
  {
    unique_ptr<xact_t> curr(new xact_t);
    curr->_date = in_event.checkin.date();
    curr->code  = out_event.desc; // if it wasn't used above
    curr->payee = in_event.desc;
    curr->pos   = in_event.position;

    if (! in_event.note.empty())
      curr->append_note(in_event.note.c_str(), *context.scope);

    char buf[32];
    std::sprintf(buf, "%lds", long((out_event.checkin - in_event.checkin)
                                   .total_seconds()));
    amount_t amt;
    amt.parse(buf);
    VERIFY(amt.valid());

    post_t * post = new post_t(in_event.account, amt, POST_VIRTUAL);
    post->set_state(out_event.completed ? item_t::CLEARED : item_t::UNCLEARED);
    post->pos = in_event.position;
    post->checkin = in_event.checkin;
    post->checkout = out_event.checkin;
    curr->add_post(post);
    in_event.account->add_post(post);

    if (! context.journal->add_xact(curr.get()))
      throw parse_error(_("Failed to record 'out' timelog transaction"));
    else
      curr.release();
  }

  std::size_t clock_out_from_timelog(std::list<time_xact_t>& time_xacts,
                                     time_xact_t             out_event,
                                     parse_context_t&        context)
  {
    time_xact_t event;

    if (time_xacts.size() == 1) {
      event = time_xacts.back();
      time_xacts.clear();
    }
    else if (time_xacts.empty()) {
      throw parse_error(_("Timelog check-out event without a check-in"));
    }
    else if (! out_event.account) {
      throw parse_error
        (_("When multiple check-ins are active, checking out requires an account"));
    }
    else {
      bool found = false;

      for (std::list<time_xact_t>::iterator i = time_xacts.begin();
           i != time_xacts.end();
           i++)
        if (out_event.account == (*i).account) {
          event = *i;
          found = true;
          time_xacts.erase(i);
          break;
        }

      if (! found)
        throw parse_error
          (_("Timelog check-out event does not match any current check-ins"));
    }

    if (event.checkin.is_not_a_date_time())
        throw parse_error(_("Timelog check-in has no corresponding check-out"));
    if (out_event.checkin.is_not_a_date_time())
        throw parse_error(_("Timelog check-out has no corresponding check-in"));

    if (out_event.checkin < event.checkin)
      throw parse_error
        (_("Timelog check-out date less than corresponding check-in"));

    if (! out_event.desc.empty() && event.desc.empty()) {
      event.desc = out_event.desc;
      out_event.desc = empty_string;
    }

    if (! out_event.note.empty() && event.note.empty())
      event.note = out_event.note;

    if (! context.journal->day_break) {
      create_timelog_xact(event, out_event, context);
      return 1;
    } else {
      time_xact_t begin(event);
      std::size_t xact_count = 0;

      while (begin.checkin < out_event.checkin) {
        DEBUG("timelog", "begin.checkin: " << begin.checkin);
        datetime_t days_end(begin.checkin.date(), time_duration_t(23, 59, 59));
        days_end += seconds(1);
        DEBUG("timelog", "days_end: " << days_end);

        if (out_event.checkin <= days_end) {
          create_timelog_xact(begin, out_event, context);
          ++xact_count;
          break;
        } else {
          time_xact_t end(out_event);
          end.checkin = days_end;
          DEBUG("timelog", "end.checkin: " << end.checkin);
          create_timelog_xact(begin, end, context);
          ++xact_count;

          begin.checkin = end.checkin;
        }
      }
      return xact_count;
    }
  }
} // unnamed namespace

void time_log_t::close()
{
  if (! time_xacts.empty()) {
    std::list<account_t *> accounts;

    foreach (time_xact_t& time_xact, time_xacts)
      accounts.push_back(time_xact.account);

    foreach (account_t * account, accounts) {
      DEBUG("timelog", "Clocking out from account " << account->fullname());
      context.count += clock_out_from_timelog
        (time_xacts, time_xact_t(none, CURRENT_TIME(), account), context);
    }
    assert(time_xacts.empty());
  }
}

void time_log_t::clock_in(time_xact_t event)
{
  if (! time_xacts.empty()) {
    foreach (time_xact_t& time_xact, time_xacts) {
      if (event.account == time_xact.account)
        throw parse_error(_("Cannot double check-in to the same account"));
    }
  }

  time_xacts.push_back(event);
}

std::size_t time_log_t::clock_out(time_xact_t event)
{
  if (time_xacts.empty())
    throw std::logic_error(_("Timelog check-out event without a check-in"));

  return clock_out_from_timelog(time_xacts, event, context);
}

} // namespace ledger
