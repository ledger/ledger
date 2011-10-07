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
 * @file   iterators.h
 * @author John Wiegley
 *
 * @ingroup data
 */
#ifndef _ITERATORS_H
#define _ITERATORS_H

#include "xact.h"
#include "post.h"
#include "account.h"
#include "temps.h"

namespace ledger {

class journal_t;

template <typename Derived, typename Value, typename CategoryOrTraversal>
class iterator_facade_base
  : public boost::iterator_facade<Derived, Value, CategoryOrTraversal>
{
  typedef Value node_base;

public:
  iterator_facade_base() : m_node(NULL) {}

  explicit iterator_facade_base(node_base p) : m_node(p) {}

  void increment();

private:
  friend class boost::iterator_core_access;

  bool equal(iterator_facade_base const& other) const {
    return this->m_node == other.m_node;
  }

  node_base& dereference() const { return const_cast<node_base&>(m_node); }

protected:
  node_base m_node;
};

class xact_posts_iterator
  : public iterator_facade_base<xact_posts_iterator, post_t *,
                                boost::forward_traversal_tag>
{
  posts_list::iterator posts_i;
  posts_list::iterator posts_end;

  bool posts_uninitialized;

public:
  xact_posts_iterator() : posts_uninitialized(true) {}
  xact_posts_iterator(xact_t& xact)
    : posts_uninitialized(true) {
    reset(xact);
  }
  ~xact_posts_iterator() throw() {}

  void reset(xact_t& xact) {
    posts_i   = xact.posts.begin();
    posts_end = xact.posts.end();

    posts_uninitialized = false;

    increment();
  }

  void increment() {
    if (posts_uninitialized || posts_i == posts_end)
      m_node = NULL;
    else
      m_node = *posts_i++;
  }
};

class xacts_iterator
  : public iterator_facade_base<xacts_iterator, xact_t *,
                                boost::forward_traversal_tag>
{
public:
  xacts_list::iterator xacts_i;
  xacts_list::iterator xacts_end;

  bool xacts_uninitialized;

  xacts_iterator() : xacts_uninitialized(true) {}
  xacts_iterator(journal_t& journal) : xacts_uninitialized(false) {
    reset(journal);
  }
  xacts_iterator(xacts_list::iterator beg,
                 xacts_list::iterator end) : xacts_uninitialized(false) {
    reset(beg, end);
  }
  ~xacts_iterator() throw() {}

  void reset(journal_t& journal);

  void reset(xacts_list::iterator beg, xacts_list::iterator end) {
    xacts_i   = beg;
    xacts_end = end;
    increment();
  }

  void increment();
};

class journal_posts_iterator
  : public iterator_facade_base<journal_posts_iterator, post_t *,
                                boost::forward_traversal_tag>
{
  xacts_iterator      xacts;
  xact_posts_iterator posts;

public:
  journal_posts_iterator() {}
  journal_posts_iterator(journal_t& journal) {
    reset(journal);
  }
  ~journal_posts_iterator() throw() {}

  void reset(journal_t& journal);

  void increment();
};

class posts_commodities_iterator
  : public iterator_facade_base<posts_commodities_iterator, post_t *,
                                boost::forward_traversal_tag>
{
protected:
  journal_posts_iterator  journal_posts;
  xacts_iterator          xacts;
  xact_posts_iterator     posts;
  temporaries_t           temps;
  xacts_list              xact_temps;

public:
  posts_commodities_iterator() {}
  posts_commodities_iterator(journal_t& journal) {
    reset(journal);
  }
  ~posts_commodities_iterator() throw() {}

  void reset(journal_t& journal);

  void increment();
};

class basic_accounts_iterator
  : public iterator_facade_base<basic_accounts_iterator, account_t *,
                                boost::forward_traversal_tag>
{
  std::list<accounts_map::const_iterator> accounts_i;
  std::list<accounts_map::const_iterator> accounts_end;

public:
  basic_accounts_iterator() {}
  basic_accounts_iterator(account_t& account) {
    push_back(account);
    increment();
  }
  ~basic_accounts_iterator() throw() {}

  void increment();

private:
  void push_back(account_t& account) {
    accounts_i.push_back(account.accounts.begin());
    accounts_end.push_back(account.accounts.end());
  }
};

class sorted_accounts_iterator
  : public iterator_facade_base<sorted_accounts_iterator, account_t *,
                                boost::forward_traversal_tag>
{
  expr_t sort_cmp;
  bool   flatten_all;

  typedef std::deque<account_t *> accounts_deque_t;

  std::list<accounts_deque_t>                 accounts_list;
  std::list<accounts_deque_t::const_iterator> sorted_accounts_i;
  std::list<accounts_deque_t::const_iterator> sorted_accounts_end;

public:
  sorted_accounts_iterator(account_t& account,
                           const expr_t& _sort_cmp, bool _flatten_all)
    : sort_cmp(_sort_cmp), flatten_all(_flatten_all) {
    push_back(account);
    increment();
  }
  ~sorted_accounts_iterator() throw() {}

  void increment();

private:
  void push_back(account_t& account);
  void push_all(account_t& account, accounts_deque_t& deque);
  void sort_accounts(account_t& account, accounts_deque_t& deque);
};

} // namespace ledger

#endif // _ITERATORS_H
