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

#include "journal.h"
#include "account.h"

namespace ledger {

const string version = PACKAGE_VERSION;

journal_t::~journal_t()
{
  TRACE_DTOR(journal_t);

  // Don't bother unhooking each xact's posts from the
  // accounts they refer to, because all accounts are about to
  // be deleted.
  foreach (xact_t * xact, xacts)
    checked_delete(xact);

  foreach (auto_xact_t * xact, auto_xacts)
    checked_delete(xact);

  foreach (period_xact_t * xact, period_xacts)
    checked_delete(xact);
}

void journal_t::add_account(account_t * acct)
{
  master->add_account(acct);
}

bool journal_t::remove_account(account_t * acct)
{
  return master->remove_account(acct);
}

account_t * journal_t::find_account(const string& name, bool auto_create)
{
  return master->find_account(name, auto_create);
}

account_t * journal_t::find_account_re(const string& regexp)
{
  return master->find_account_re(regexp);
}

bool journal_t::add_xact(xact_t * xact)
{
  xact->journal = this;

  if (! xact_finalize_hooks.run_hooks(*xact, false) ||
      ! xact->finalize() ||
      ! xact_finalize_hooks.run_hooks(*xact, true)) {
    xact->journal = NULL;
    return false;
  }

  xacts.push_back(xact);

  return true;
}

bool journal_t::remove_xact(xact_t * xact)
{
  bool found = false;
  xacts_list::iterator i;
  for (i = xacts.begin(); i != xacts.end(); i++)
    if (*i == xact) {
      found = true;
      break;
    }
  if (! found)
    return false;

  xacts.erase(i);
  xact->journal = NULL;

  return true;
}

bool journal_t::valid() const
{
  if (! master->valid()) {
    DEBUG("ledger.validate", "journal_t: master not valid");
    return false;
  }

  foreach (const xact_t * xact, xacts)
    if (! xact->valid()) {
      DEBUG("ledger.validate", "journal_t: xact not valid");
      return false;
    }

  return true;
}

} // namespace ledger
