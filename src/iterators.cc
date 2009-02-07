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

#include "iterators.h"
#include "session.h"
#include "compare.h"

namespace ledger {

void entries_iterator::reset(session_t& session)
{
  entries_i   = session.journal->entries.begin();
  entries_end = session.journal->entries.end();
  entries_uninitialized = false;
}

entry_t * entries_iterator::operator()()
{
  if (entries_i != entries_end)
    return *entries_i++;
  else
    return NULL;
}

void session_xacts_iterator::reset(session_t& session)
{
  entries.reset(session);

  entry_t * entry = entries();
  if (entry != NULL)
    xacts.reset(*entry);
}

xact_t * session_xacts_iterator::operator()()
{
  xact_t * xact = xacts();
  if (xact == NULL) {
    entry_t * entry = entries();
    if (entry != NULL) {
      xacts.reset(*entry);
      xact = xacts();
    }
  }
  return xact;
}

account_t * basic_accounts_iterator::operator()()
{
  while (! accounts_i.empty() &&
	 accounts_i.back() == accounts_end.back()) {
    accounts_i.pop_back();
    accounts_end.pop_back();
  }
  if (accounts_i.empty())
    return NULL;

  account_t * account = (*(accounts_i.back()++)).second;
  assert(account);

  // If this account has children, queue them up to be iterated next.
  if (! account->accounts.empty())
    push_back(*account);

  return account;
}

void sorted_accounts_iterator::sort_accounts(account_t& account,
					     accounts_deque_t& deque)
{
  foreach (accounts_map::value_type& pair, account.accounts)
    deque.push_back(pair.second);

  std::stable_sort(deque.begin(), deque.end(),
		   compare_items<account_t>(sort_cmp));
}

account_t * sorted_accounts_iterator::operator()()
{
  while (! sorted_accounts_i.empty() &&
	 sorted_accounts_i.back() == sorted_accounts_end.back()) {
    sorted_accounts_i.pop_back();
    sorted_accounts_end.pop_back();
    assert(! accounts_list.empty());
    accounts_list.pop_back();
  }
  if (sorted_accounts_i.empty())
    return NULL;

  account_t * account = *sorted_accounts_i.back()++;
  assert(account);

  // If this account has children, queue them up to be iterated next.
  if (! account->accounts.empty())
    push_back(*account);

  account->xdata().drop_flags(ACCOUNT_EXT_SORT_CALC);
  return account;
}

#if 0
// jww (2008-08-03): This needs to be changed into a commodities->xacts
// iterator.

// jww (2008-08-03): We then could also use a payees->xacts iterator

void walk_commodities(commodity_pool_t::commodities_by_ident& commodities,
		      item_handler<xact_t>& handler)
{
  std::list<xact_t>    xact_temps;
  std::list<entry_t>   entry_temps;
  std::list<account_t> acct_temps;

  for (commodity_pool_t::commodities_by_ident::iterator
	 i = commodities.begin();
       i != commodities.end();
       i++) {
    if ((*i)->has_flags(COMMODITY_NOMARKET))
      continue;

    entry_temps.push_back(entry_t());
    acct_temps.push_back(account_t(NULL, (*i)->symbol()));

    if ((*i)->history())
      foreach (const commodity_t::history_map::value_type& pair,
	       (*i)->history()->prices) {
	entry_temps.back()._date = pair.first.date();

	xact_temps.push_back(xact_t(&acct_temps.back()));
	xact_t& temp = xact_temps.back();
	temp.entry  = &entry_temps.back();
	temp.amount = pair.second;
	temp.add_flags(XACT_TEMP);
	entry_temps.back().add_xact(&temp);

	handler(xact_temps.back());
      }
  }

  handler.flush();

  clear_entries_xacts(entry_temps);
}
#endif

} // namespace ledger
