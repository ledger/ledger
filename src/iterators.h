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
 * @file   iterators.h
 * @author John Wiegley
 *
 * @ingroup data
 *
 * @brief Brief
 *
 * Long.
 */
#ifndef _ITERATORS_H
#define _ITERATORS_H

#include "journal.h"
#include "xact.h"
#include "account.h"

namespace ledger {

/**
 * @brief Brief
 *
 * Long.
 */
class posts_iterator : public noncopyable
{
public:
  virtual ~posts_iterator() throw() {}
  virtual post_t * operator()() = 0;
};

/**
 * @brief Brief
 *
 * Long.
 */
class xact_posts_iterator : public posts_iterator
{
  posts_list::iterator posts_i;
  posts_list::iterator posts_end;

  bool posts_uninitialized;

public:
  xact_posts_iterator() : posts_uninitialized(true) {
    TRACE_CTOR(xact_posts_iterator, "");
  }
  xact_posts_iterator(xact_t& xact)
    : posts_uninitialized(true) {
    TRACE_CTOR(xact_posts_iterator, "xact_t&");
    reset(xact);
  }
  virtual ~xact_posts_iterator() throw() {
    TRACE_DTOR(xact_posts_iterator);
  }

  void reset(xact_t& xact) {
    posts_i   = xact.posts.begin();
    posts_end = xact.posts.end();

    posts_uninitialized = false;
  }

  virtual post_t * operator()() {
    if (posts_i == posts_end || posts_uninitialized)
      return NULL;
    return *posts_i++;
  }
};

/**
 * @brief Brief
 *
 * Long.
 */
class xacts_iterator : public noncopyable
{
public:
  xacts_list::iterator xacts_i;
  xacts_list::iterator xacts_end;

  bool xacts_uninitialized;

  xacts_iterator() : xacts_uninitialized(true) {
    TRACE_CTOR(xacts_iterator, "");
  }
  xacts_iterator(journal_t& journal) : xacts_uninitialized(true) {
    TRACE_CTOR(xacts_iterator, "journal_t&");
    reset(journal);
  }
  virtual ~xacts_iterator() throw() {
    TRACE_DTOR(xacts_iterator);
  }

  void reset(journal_t& journal);

  xact_t * operator()();
};

/**
 * @brief Brief
 *
 * Long.
 */
class journal_posts_iterator : public posts_iterator
{
  xacts_iterator     xacts;
  xact_posts_iterator posts;

public:
  journal_posts_iterator() {
    TRACE_CTOR(journal_posts_iterator, "");
  }
  journal_posts_iterator(journal_t& journal) {
    TRACE_CTOR(journal_posts_iterator, "journal_t&");
    reset(journal);
  }
  virtual ~journal_posts_iterator() throw() {
    TRACE_DTOR(journal_posts_iterator);
  }

  void reset(journal_t& journal);

  virtual post_t * operator()();
};

/**
 * @brief Brief
 *
 * Long.
 */
class posts_commodities_iterator : public posts_iterator
{
protected:
  journal_posts_iterator  journal_posts;
  xacts_iterator	  xacts;
  xact_posts_iterator	  posts;

  std::list<post_t>	  post_temps;
  std::list<account_t>    acct_temps;
  xacts_list            xact_temps;

public:
  posts_commodities_iterator() {
    TRACE_CTOR(posts_commodities_iterator, "");
  }
  posts_commodities_iterator(journal_t& journal) {
    TRACE_CTOR(posts_commodities_iterator, "journal_t&");
    reset(journal);
  }
  virtual ~posts_commodities_iterator() throw() {
    TRACE_DTOR(posts_commodities_iterator);
    foreach (xact_t * xact, xact_temps)
      checked_delete(xact);
  }

  void reset(journal_t& journal);

  virtual post_t * operator()();
};

/**
 * @brief Brief
 *
 * Long.
 */
class accounts_iterator : public noncopyable
{
public:
  virtual ~accounts_iterator() throw() {}
  virtual account_t * operator()() = 0;
};

/**
 * @brief Brief
 *
 * Long.
 */
class basic_accounts_iterator : public accounts_iterator
{
  std::list<accounts_map::const_iterator> accounts_i;
  std::list<accounts_map::const_iterator> accounts_end;

public:
  basic_accounts_iterator() {
    TRACE_CTOR(basic_accounts_iterator, "");
  }
  basic_accounts_iterator(account_t& account) {
    TRACE_CTOR(basic_accounts_iterator, "account_t&");
    push_back(account);
  }
  virtual ~basic_accounts_iterator() throw() {
    TRACE_DTOR(basic_accounts_iterator);
  }

  void push_back(account_t& account) {
    accounts_i.push_back(account.accounts.begin());
    accounts_end.push_back(account.accounts.end());
  }

  virtual account_t * operator()();
};

/**
 * @brief Brief
 *
 * Long.
 */
class sorted_accounts_iterator : public accounts_iterator
{
  expr_t sort_cmp;
  bool   flatten_all;

  typedef std::deque<account_t *> accounts_deque_t;

  std::list<accounts_deque_t>		      accounts_list;
  std::list<accounts_deque_t::const_iterator> sorted_accounts_i;
  std::list<accounts_deque_t::const_iterator> sorted_accounts_end;

public:
  sorted_accounts_iterator(const expr_t& _sort_cmp, bool _flatten_all)
    : sort_cmp(_sort_cmp), flatten_all(_flatten_all) {
    TRACE_CTOR(sorted_accounts_iterator, "const expr_t&, bool");
  }
  sorted_accounts_iterator(const expr_t& _sort_cmp, bool _flatten_all,
			   account_t& account)
    : sort_cmp(_sort_cmp), flatten_all(_flatten_all) {
    TRACE_CTOR(sorted_accounts_iterator, "const expr_t&, bool, account_t&");
    push_back(account);
  }
  virtual ~sorted_accounts_iterator() throw() {
    TRACE_DTOR(sorted_accounts_iterator);
  }

  void sort_accounts(account_t& account, accounts_deque_t& deque);
  void push_all(account_t& account);
  void push_back(account_t& account);

  virtual account_t * operator()();
};

} // namespace ledger

#endif // _ITERATORS_H
