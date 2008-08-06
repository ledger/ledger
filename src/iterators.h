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

#ifndef _ITERATORS_H
#define _ITERATORS_H

#include "journal.h"
#include "entry.h"
#include "account.h"

namespace ledger {

class xacts_iterator : public noncopyable
{
public:
  virtual ~xacts_iterator() throw() {}
  virtual xact_t * operator()() = 0;
};

class entry_xacts_iterator : public xacts_iterator
{
  xacts_list::iterator xacts_i;
  xacts_list::iterator xacts_end;

  bool xacts_uninitialized;

public:
  entry_xacts_iterator() : xacts_uninitialized(true) {
    TRACE_CTOR(entry_xacts_iterator, "");
  }
  entry_xacts_iterator(entry_t& entry)
    : xacts_uninitialized(true) {
    TRACE_CTOR(entry_xacts_iterator, "entry_t&");
    reset(entry);
  }
  virtual ~entry_xacts_iterator() throw() {
    TRACE_DTOR(entry_xacts_iterator);
  }

  void reset(entry_t& entry) {
    xacts_i   = entry.xacts.begin();
    xacts_end = entry.xacts.end();

    xacts_uninitialized = false;
  }

  virtual xact_t * operator()() {
    if (xacts_i == xacts_end || xacts_uninitialized)
      return NULL;
    return *xacts_i++;
  }
};

class entries_iterator : public noncopyable
{
  ptr_list<journal_t>::iterator journals_i;
  ptr_list<journal_t>::iterator journals_end;

  bool journals_uninitialized;

  entries_list::iterator	entries_i;
  entries_list::iterator	entries_end;

  bool entries_uninitialized;

public:
  entries_iterator()
    : journals_uninitialized(true), entries_uninitialized(true) {
    TRACE_CTOR(entries_iterator, "");
  }
  entries_iterator(session_t& session)
    : journals_uninitialized(true), entries_uninitialized(true) {
    TRACE_CTOR(entries_iterator, "session_t&");
    reset(session);
  }
  virtual ~entries_iterator() throw() {
    TRACE_DTOR(entries_iterator);
  }

  void reset(session_t& session);

  entry_t * operator()();
};

class session_xacts_iterator : public xacts_iterator
{
  entries_iterator     entries;
  entry_xacts_iterator xacts;

public:
  session_xacts_iterator() {
    TRACE_CTOR(session_xacts_iterator, "");
  }
  session_xacts_iterator(session_t& session) {
    TRACE_CTOR(session_xacts_iterator, "session_t&");
    reset(session);
  }
  virtual ~session_xacts_iterator() throw() {
    TRACE_DTOR(session_xacts_iterator);
  }

  void reset(session_t& session);

  virtual xact_t * operator()();
};

class accounts_iterator : public noncopyable
{
public:
  virtual ~accounts_iterator() throw() {}
  virtual account_t * operator()() = 0;
};

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

class sorted_accounts_iterator : public accounts_iterator
{
  expr_t sort_cmp;

  typedef std::deque<account_t *> accounts_deque_t;

  std::list<accounts_deque_t>		      accounts_list;
  std::list<accounts_deque_t::const_iterator> sorted_accounts_i;
  std::list<accounts_deque_t::const_iterator> sorted_accounts_end;

public:
  sorted_accounts_iterator(const string& sort_order) {
    TRACE_CTOR(sorted_accounts_iterator, "const string&");
    sort_cmp = expr_t(sort_order);
  }
  sorted_accounts_iterator(account_t& account, const string& sort_order) {
    TRACE_CTOR(sorted_accounts_iterator, "account_t&, const string&");
    sort_cmp = expr_t(sort_order);
    push_back(account);
  }
  virtual ~sorted_accounts_iterator() throw() {
    TRACE_DTOR(sorted_accounts_iterator);
  }

  void sort_accounts(account_t& account, accounts_deque_t& deque);

  void push_back(account_t& account) {
    accounts_list.push_back(accounts_deque_t());
    sort_accounts(account, accounts_list.back());

    sorted_accounts_i.push_back(accounts_list.back().begin());
    sorted_accounts_end.push_back(accounts_list.back().end());
  }

  virtual account_t * operator()();
};

class journals_iterator : public noncopyable
{
  ptr_list<journal_t>::iterator journals_i;
  ptr_list<journal_t>::iterator journals_end;

public:
  journals_iterator() {
    TRACE_CTOR(journals_iterator, "");
  }
  journals_iterator(session_t& session) {
    TRACE_CTOR(journals_iterator, "session_t&");
    reset(session);
  }
  virtual ~journals_iterator() throw() {
    TRACE_DTOR(journals_iterator);
  }

  void reset(session_t& session);

  virtual journal_t * operator()();
};

} // namespace ledger

#endif // _ITERATORS_H
