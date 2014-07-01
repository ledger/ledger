/*
 * Copyright (c) 2003-2014, John Wiegley.  All rights reserved.
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

#include "iterators.h"
#include "journal.h"
#include "compare.h"

namespace ledger {

void xacts_iterator::reset(journal_t& journal)
{
  xacts_i   = journal.xacts.begin();
  xacts_end = journal.xacts.end();

  xacts_uninitialized = false;

  increment();
}

void xacts_iterator::increment()
{
  if (xacts_i != xacts_end)
    m_node = *xacts_i++;
  else
    m_node = NULL;
}

void journal_posts_iterator::reset(journal_t& journal)
{
  xacts.reset(journal);
  increment();
}

void journal_posts_iterator::increment()
{
  if (post_t * post = *posts++) {
    m_node = post;
  }
  else if (xact_t * xact = *xacts++) {
    posts.reset(*xact);
    m_node = *posts++;
  }
  else {
    m_node = NULL;
  }
}

namespace {
  struct create_price_xact
  {
    journal_t&     journal;
    account_t *    account;
    temporaries_t& temps;
    xacts_list&    xact_temps;

    std::map<string, xact_t *> xacts_by_commodity;

    create_price_xact(journal_t& _journal, account_t * _account,
                      temporaries_t& _temps, xacts_list& _xact_temps)
      : journal(_journal), account(_account), temps(_temps),
        xact_temps(_xact_temps) {
      TRACE_CTOR(create_price_xact,
                 "journal_t&, account_t *, temporaries_t&, xacts_list&");
    }
    ~create_price_xact() throw() {
      TRACE_DTOR(create_price_xact);
    }

    void operator()(datetime_t& date, const amount_t& price) {
      xact_t * xact;
      string   symbol = price.commodity().symbol();

      std::map<string, xact_t *>::iterator i =
        xacts_by_commodity.find(symbol);
      if (i != xacts_by_commodity.end()) {
        xact = (*i).second;
      } else {
        xact = &temps.create_xact();
        xact_temps.push_back(xact);
        xact->payee = symbol;
        xact->_date = date.date();
        xacts_by_commodity.insert
          (std::pair<string, xact_t *>(symbol, xact));
        xact->journal = &journal;
      }

      bool post_already_exists = false;

      foreach (post_t * post, xact->posts) {
        if (post->date() == date.date() && post->amount == price) {
          post_already_exists = true;
          break;
        }
      }

      if (! post_already_exists) {
        post_t& temp = temps.create_post(*xact, account);
        temp._date  = date.date();
        temp.amount = price;

        temp.xdata().datetime = date;
      }
    }
  };
}

void posts_commodities_iterator::reset(journal_t& journal)
{
  journal_posts.reset(journal);

  std::set<commodity_t *> commodities;

  while (const post_t * post = *journal_posts++) {
    commodity_t& comm(post->amount.commodity());
    if (comm.flags() & COMMODITY_NOMARKET)
      continue;
    commodities.insert(&comm.referent());
  }

  foreach (commodity_t * comm, commodities)
    comm->map_prices
      (create_price_xact(journal, journal.master->find_account(comm->symbol()),
                         temps, xact_temps));

  xacts.reset(xact_temps.begin(), xact_temps.end());

  increment();
}

void posts_commodities_iterator::increment()
{
  if (post_t * post = *posts++) {
    m_node = post;
  }
  else if (xact_t * xact = *xacts++) {
    posts.reset(*xact);
    m_node = *posts++;
  }
  else {
    m_node = NULL;
  }
}

void basic_accounts_iterator::increment()
{
  while (! accounts_i.empty() && accounts_i.back() == accounts_end.back()) {
    accounts_i.pop_back();
    accounts_end.pop_back();
  }

  if (accounts_i.empty()) {
    m_node = NULL;
  } else {
    account_t * account = (*(accounts_i.back()++)).second;
    assert(account);

    // If this account has children, queue them up to be iterated next.
    if (! account->accounts.empty())
      push_back(*account);

    m_node = account;
  }
}

void sorted_accounts_iterator::push_back(account_t& account)
{
  accounts_list.push_back(accounts_deque_t());

  if (flatten_all) {
    push_all(account, accounts_list.back());

    std::stable_sort(accounts_list.back().begin(),
                     accounts_list.back().end(),
                     compare_items<account_t>(sort_cmp));

#if DEBUG_ON
    if (SHOW_DEBUG("account.sorted")) {
      foreach (account_t * acct, accounts_list.back())
        DEBUG("account.sorted",
              "Account (flat): " << acct->fullname());
    }
#endif
  } else {
    sort_accounts(account, accounts_list.back());
  }

  sorted_accounts_i.push_back(accounts_list.back().begin());
  sorted_accounts_end.push_back(accounts_list.back().end());
}

void sorted_accounts_iterator::push_all(account_t& account,
                                        accounts_deque_t& deque)
{
  foreach (accounts_map::value_type& pair, account.accounts) {
    deque.push_back(pair.second);
    push_all(*pair.second, deque);
  }
}

void sorted_accounts_iterator::sort_accounts(account_t& account,
                                             accounts_deque_t& deque)
{
  foreach (accounts_map::value_type& pair, account.accounts)
    deque.push_back(pair.second);

  std::stable_sort(deque.begin(), deque.end(),
                   compare_items<account_t>(sort_cmp));

#if DEBUG_ON
  if (SHOW_DEBUG("account.sorted")) {
    foreach (account_t * acct, deque)
      DEBUG("account.sorted", "Account: " << acct->fullname());
  }
#endif
}

void sorted_accounts_iterator::increment()
{
  while (! sorted_accounts_i.empty() &&
         sorted_accounts_i.back() == sorted_accounts_end.back()) {
    sorted_accounts_i.pop_back();
    sorted_accounts_end.pop_back();
    assert(! accounts_list.empty());
    accounts_list.pop_back();
  }

  if (sorted_accounts_i.empty()) {
    m_node = NULL;
  } else {
    account_t * account = *sorted_accounts_i.back()++;
    assert(account);

    // If this account has children, queue them up to be iterated next.
    if (! flatten_all && ! account->accounts.empty())
      push_back(*account);

    // Make sure the sorting value gets recalculated for this account
    account->xdata().drop_flags(ACCOUNT_EXT_SORT_CALC);

    m_node = account;
  }
}

} // namespace ledger
