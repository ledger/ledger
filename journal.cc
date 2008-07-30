/*
 * Copyright (c) 2003-2008, John Wiegley.  All rights reserved.
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
#include "session.h"

namespace ledger {

const string version = PACKAGE_VERSION;

journal_t::journal_t(session_t * _owner) :
  owner(_owner), basket(NULL), item_pool(NULL), item_pool_end(NULL)
{
  TRACE_CTOR(journal_t, "");
  master = owner->master;
}

journal_t::~journal_t()
{
  TRACE_DTOR(journal_t);

  // Don't bother unhooking each entry's xacts from the
  // accounts they refer to, because all accounts are about to
  // be deleted.
  for (entries_list::iterator i = entries.begin();
       i != entries.end();
       i++) {
    if (! item_pool ||
	reinterpret_cast<char *>(*i) <  item_pool ||
	reinterpret_cast<char *>(*i) >= item_pool_end) {
      checked_delete(*i);
    } else {
      (*i)->~entry_t();
    }
  }

  for (auto_entries_list::iterator i = auto_entries.begin();
       i != auto_entries.end();
       i++)
    if (! item_pool ||
	reinterpret_cast<char *>(*i) < item_pool ||
	reinterpret_cast<char *>(*i) >= item_pool_end)
      checked_delete(*i);
    else
      (*i)->~auto_entry_t();

  for (period_entries_list::iterator i = period_entries.begin();
       i != period_entries.end();
       i++)
    if (! item_pool ||
	reinterpret_cast<char *>(*i) < item_pool ||
	reinterpret_cast<char *>(*i) >= item_pool_end)
      checked_delete(*i);
    else
      (*i)->~period_entry_t();

  if (item_pool)
    checked_array_delete(item_pool);
}

void journal_t::add_account(account_t * acct)
{
  owner->add_account(acct);
}

bool journal_t::remove_account(account_t * acct)
{
  return owner->remove_account(acct);
}

account_t * journal_t::find_account(const string& name, bool auto_create)
{
  return owner->find_account(name, auto_create);
}

account_t * journal_t::find_account_re(const string& regexp)
{
  return owner->find_account_re(regexp);
}

bool journal_t::add_entry(entry_t * entry)
{
  entry->journal = this;

  if (! entry_finalize_hooks.run_hooks(*entry, false) ||
      ! entry->finalize() ||
      ! entry_finalize_hooks.run_hooks(*entry, true)) {
    entry->journal = NULL;
    return false;
  }

  entries.push_back(entry);

  for (xacts_list::const_iterator i = entry->xacts.begin();
       i != entry->xacts.end();
       i++)
    if ((*i)->cost) {
      assert((*i)->amount);
      (*i)->amount.commodity().add_price(entry->date(),
					 *(*i)->cost / (*i)->amount.number());
    }

  return true;
}

bool journal_t::remove_entry(entry_t * entry)
{
  bool found = false;
  entries_list::iterator i;
  for (i = entries.begin(); i != entries.end(); i++)
    if (*i == entry) {
      found = true;
      break;
    }
  if (! found)
    return false;

  entries.erase(i);
  entry->journal = NULL;

  return true;
}

bool journal_t::valid() const
{
  if (! master->valid()) {
    DEBUG("ledger.validate", "journal_t: master not valid");
    return false;
  }

  for (entries_list::const_iterator i = entries.begin();
       i != entries.end();
       i++)
    if (! (*i)->valid()) {
      DEBUG("ledger.validate", "journal_t: entry not valid");
      return false;
    }

  return true;
}

} // namespace ledger
