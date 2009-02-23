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
#include "journal.h"
#include "compare.h"

namespace ledger {

void xacts_iterator::reset(journal_t& journal)
{
  xacts_i   = journal.xacts.begin();
  xacts_end = journal.xacts.end();
  xacts_uninitialized = false;
}

xact_t * xacts_iterator::operator()()
{
  if (xacts_i != xacts_end)
    return *xacts_i++;
  else
    return NULL;
}

void journal_posts_iterator::reset(journal_t& journal)
{
  xacts.reset(journal);

  xact_t * xact = xacts();
  if (xact != NULL)
    posts.reset(*xact);
}

post_t * journal_posts_iterator::operator()()
{
  post_t * post = posts();
  if (post == NULL) {
    xact_t * xact = xacts();
    if (xact != NULL) {
      posts.reset(*xact);
      post = posts();
    }
  }
  return post;
}

void posts_commodities_iterator::reset(journal_t& journal)
{
  journal_posts.reset(journal);

  std::set<commodity_t *> commodities;

  for (post_t * post = journal_posts(); post; post = journal_posts()) {
    commodity_t& comm(post->amount.commodity());
    if (comm.flags() & COMMODITY_NOMARKET)
      continue;
    commodities.insert(&comm);
  }

  std::map<string, xact_t *> xacts_by_commodity;

  foreach (commodity_t * comm, commodities) {
    optional<commodity_t::varied_history_t&> history = comm->varied_history();
    if (! history)
      continue;

    account_t * account = journal.master->find_account(comm->symbol());

    foreach (commodity_t::base_t::history_by_commodity_map::value_type pair,
	     history->histories) {
      foreach (commodity_t::base_t::history_map::value_type hpair,
	       pair.second.prices) {
	xact_t * xact;
	string    symbol = hpair.second.commodity().symbol();

	std::map<string, xact_t *>::iterator i =
	  xacts_by_commodity.find(symbol);
	if (i != xacts_by_commodity.end()) {
	  xact = (*i).second;
	} else {
	  xact_temps.push_back(new xact_t);
	  xact = xact_temps.back();
	  xact->payee = symbol;
	  xact->_date = hpair.first.date();
	  xacts_by_commodity.insert
	    (std::pair<string, xact_t *>(symbol, xact));
	}

	post_temps.push_back(post_t(account));
	post_t& temp = post_temps.back();
	temp._date  = hpair.first.date();
	temp.xact  = xact;
	temp.amount = hpair.second;
	temp.set_flags(ITEM_GENERATED | ITEM_TEMP);

	xact->add_post(&temp);
      }
    }
  }

  xacts.xacts_i   = xact_temps.begin();
  xacts.xacts_end = xact_temps.end();

  xacts.xacts_uninitialized = false;

  xact_t * xact = xacts();
  if (xact != NULL)
    posts.reset(*xact);
}

post_t * posts_commodities_iterator::operator()()
{
  post_t * post = posts();
  if (post == NULL) {
    xact_t * xact = xacts();
    if (xact != NULL) {
      posts.reset(*xact);
      post = posts();
    }
  }
  return post;
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

#if defined(DEBUG_ON)
  if (SHOW_DEBUG("accounts.sorted")) {
    foreach (account_t * account, deque)
      DEBUG("accounts.sorted", "Account: " << account->fullname());
  }
#endif
}

void sorted_accounts_iterator::push_all(account_t& account)
{
  accounts_deque_t& deque(accounts_list.back());

  foreach (accounts_map::value_type& pair, account.accounts) {
    deque.push_back(pair.second);
    push_all(*pair.second);
  }
}

void sorted_accounts_iterator::push_back(account_t& account)
{
  accounts_list.push_back(accounts_deque_t());

  if (flatten_all) {
    push_all(account);
    std::stable_sort(accounts_list.back().begin(),
		     accounts_list.back().end(),
		     compare_items<account_t>(sort_cmp));
#if defined(DEBUG_ON)
    if (SHOW_DEBUG("accounts.sorted")) {
      foreach (account_t * account, accounts_list.back())
	DEBUG("accounts.sorted", "Account: " << account->fullname());
    }
#endif
  } else {
    sort_accounts(account, accounts_list.back());
  }

  sorted_accounts_i.push_back(accounts_list.back().begin());
  sorted_accounts_end.push_back(accounts_list.back().end());
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
  if (! flatten_all && ! account->accounts.empty())
    push_back(*account);

  // Make sure the sorting value gets recalculated for this account
  account->xdata().drop_flags(ACCOUNT_EXT_SORT_CALC);
  return account;
}

} // namespace ledger
