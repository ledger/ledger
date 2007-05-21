/*
 * Copyright (c) 2003-2007, John Wiegley.  All rights reserved.
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

#ifndef _JOURNAL_H
#define _JOURNAL_H

#include "amount.h"
#include "xpath.h"

namespace ledger {

// These flags persist with the object
#define TRANSACTION_NORMAL     0x0000
#define TRANSACTION_VIRTUAL    0x0001
#define TRANSACTION_BALANCE    0x0002
#define TRANSACTION_AUTO       0x0004
#define TRANSACTION_BULK_ALLOC 0x0008
#define TRANSACTION_CALCULATED 0x0010

class entry_t;
class account_t;

class transaction_t : public supports_flags<>
{
 public:
  enum state_t { UNCLEARED, CLEARED, PENDING };

  entry_t *	     entry;
  state_t	     state;
  account_t *	     account;
  optional<moment_t> _date;
  optional<moment_t> _date_eff;
  amount_t           amount;
  optional<amount_t> cost;
  optional<string>   note;

  static bool use_effective_date;

  explicit transaction_t(account_t * _account = NULL)
    : supports_flags<>(TRANSACTION_NORMAL), entry(NULL),
      state(UNCLEARED), account(_account) {
    TRACE_CTOR(transaction_t, "account_t *");
  }
  explicit transaction_t(account_t *	        _account,
			 const amount_t&        _amount,
			 unsigned int           _flags = TRANSACTION_NORMAL,
			 const optional<string> _note  = none)
    : supports_flags<>(_flags), entry(NULL), state(UNCLEARED),
      account(_account), amount(_amount), note(_note) {
    TRACE_CTOR(transaction_t,
	       "account_t *, const amount_t&, unsigned int, const string&");
  }
  explicit transaction_t(const transaction_t& xact)
    : supports_flags<>(xact),
      entry(xact.entry),
      state(xact.state),
      account(xact.account),
      _date(xact._date),
      _date_eff(xact._date_eff),
      amount(xact.amount),
      cost(xact.cost),
      note(xact.note) {
    TRACE_CTOR(transaction_t, "copy");
  }
  ~transaction_t();

  moment_t actual_date() const;
  moment_t effective_date() const;
  moment_t date() const {
    if (use_effective_date)
      return effective_date();
    else
      return actual_date();
  }

  bool valid() const;
};

#if 0
class xact_context : public file_context {
 public:
  const transaction_t& xact;

  xact_context(const transaction_t& _xact,
	       const string& desc = "") throw();
  virtual ~xact_context() throw() {}
};
#endif

class journal_t;

typedef std::list<transaction_t *> transactions_list;

class entry_base_t
{
 public:
  journal_t *       journal;
  transactions_list transactions;

  entry_base_t() : journal(NULL) {
    TRACE_CTOR(entry_base_t, "");
  }
  entry_base_t(const entry_base_t& e) : journal(NULL)
  {
    TRACE_CTOR(entry_base_t, "copy");
    for (transactions_list::const_iterator i = e.transactions.begin();
	 i != e.transactions.end();
	 i++)
      transactions.push_back(new transaction_t(**i));
  }
  virtual ~entry_base_t() {
    TRACE_DTOR(entry_base_t);
    for (transactions_list::iterator i = transactions.begin();
	 i != transactions.end();
	 i++)
      if (! (*i)->has_flags(TRANSACTION_BULK_ALLOC))
	checked_delete(*i);
      else
	(*i)->~transaction_t();
  }

  bool operator==(const entry_base_t& entry) {
    return this == &entry;
  }
  bool operator!=(const entry_base_t& entry) {
    return ! (*this == entry);
  }

  virtual void add_transaction(transaction_t * xact);
  virtual bool remove_transaction(transaction_t * xact);

  virtual bool finalize();
  virtual bool valid() const = 0;
};

class entry_t : public entry_base_t
{
public:
  moment_t	     _date;
  optional<moment_t> _date_eff;
  optional<string>   code;
  string	     payee;

  entry_t() {
    TRACE_CTOR(entry_t, "");
  }
  entry_t(const entry_t& e);

  virtual ~entry_t() {
    TRACE_DTOR(entry_t);
  }

  moment_t actual_date() const {
    return _date;
  }
  moment_t effective_date() const {
    return _date_eff ? *_date_eff : _date;
  }
  moment_t date() const {
    if (transaction_t::use_effective_date)
      return effective_date();
    else
      return actual_date();
  }

  virtual void add_transaction(transaction_t * xact);
  virtual bool valid() const;

  bool get_state(transaction_t::state_t * state) const;
};

struct entry_finalizer_t {
  virtual ~entry_finalizer_t() {}
  virtual bool operator()(entry_t& entry, bool post) = 0;
};

void print_entry(std::ostream& out, const entry_base_t& entry,
		 const string& prefix = "");

#if 0
class entry_context : public error_context {
 public:
  const entry_base_t& entry;

  entry_context(const entry_base_t& _entry,
		const string& _desc = "") throw()
    : error_context(_desc), entry(_entry) {}
  virtual ~entry_context() throw() {}

  virtual void describe(std::ostream& out) const throw();
};
#endif

class auto_entry_t : public entry_base_t
{
public:
  scoped_ptr<xml::xpath_t> predicate;

  auto_entry_t();
  auto_entry_t(const string& _predicate);
  virtual ~auto_entry_t();

  virtual void extend_entry(entry_base_t& entry, bool post);
  virtual bool valid() const {
    return true;
  }
};

struct auto_entry_finalizer_t : public entry_finalizer_t {
  journal_t * journal;
  auto_entry_finalizer_t(journal_t * _journal) : journal(_journal) {}
  virtual bool operator()(entry_t& entry, bool post);
};


class period_entry_t : public entry_base_t
{
 public:
  interval_t period;
  string     period_string;

  period_entry_t() {
    TRACE_CTOR(period_entry_t, "");
  }
  period_entry_t(const string& _period)
    : period(_period), period_string(_period) {
    TRACE_CTOR(period_entry_t, "const string&");
  }
  period_entry_t(const period_entry_t& e)
    : entry_base_t(e), period(e.period), period_string(e.period_string) {
    TRACE_CTOR(period_entry_t, "copy");
  }

  virtual ~period_entry_t() {
    TRACE_DTOR(period_entry_t);
  }

  virtual bool valid() const {
    return period;
  }
};


typedef std::map<const string, account_t *> accounts_map;

class account_t
{
 public:
  typedef unsigned long ident_t;

  journal_t *	   journal;
  account_t *	   parent;
  string	   name;
  optional<string> note;
  unsigned short   depth;
  accounts_map	   accounts;

  mutable ident_t ident;
  mutable string  _fullname;

  account_t(account_t *   _parent = NULL,
	    const string& _name   = "",
	    const optional<string> _note = none)
    : parent(_parent), name(_name), note(_note),
      depth(parent ? parent->depth + 1 : 0), ident(0) {
    TRACE_CTOR(account_t, "account_t *, const string&, const string&");
  }
  ~account_t();

  operator string() const {
    return fullname();
  }
  string fullname() const;

  void add_account(account_t * acct) {
    accounts.insert(accounts_map::value_type(acct->name, acct));
    acct->journal = journal;
  }
  bool remove_account(account_t * acct) {
    accounts_map::size_type n = accounts.erase(acct->name);
    acct->journal = NULL;
    return n > 0;
  }

  account_t * find_account(const string& name, bool auto_create = true);

  bool valid() const;

  friend class journal_t;
};

std::ostream& operator<<(std::ostream& out, const account_t& account);


struct func_finalizer_t : public entry_finalizer_t {
  typedef bool (*func_t)(entry_t& entry, bool post);
  func_t func;
  func_finalizer_t(func_t _func) : func(_func) {}
  func_finalizer_t(const func_finalizer_t& other) :
    entry_finalizer_t(), func(other.func) {}
  virtual bool operator()(entry_t& entry, bool post) {
    return func(entry, post);
  }
};

template <typename T>
void add_hook(std::list<T>& list, T obj, const bool prepend = false) {
  if (prepend)
    list.push_front(obj);
  else
    list.push_back(obj);
}

template <typename T>
void remove_hook(std::list<T>& list, T obj) {
  list.remove(obj);
}

template <typename T, typename Data>
bool run_hooks(std::list<T>& list, Data& item, bool post) {
  for (typename std::list<T>::const_iterator i = list.begin();
       i != list.end();
       i++)
    if (! (*(*i))(item, post))
      return false;
  return true;
}


typedef std::list<entry_t *>	    entries_list;
typedef std::list<auto_entry_t *>   auto_entries_list;
typedef std::list<period_entry_t *> period_entries_list;
typedef std::list<path>		    path_list;
typedef std::list<string>	    strings_list;

class journal_t
{
 public:
  account_t *	 master;
  account_t *	 basket;
  entries_list	 entries;
  path_list	 sources;
  optional<path> price_db;
  char *	 item_pool;
  char *	 item_pool_end;

  auto_entries_list    auto_entries;
  period_entries_list  period_entries;
  mutable accounts_map accounts_cache;

  std::list<entry_finalizer_t *> entry_finalize_hooks;

  journal_t() : basket(NULL), item_pool(NULL), item_pool_end(NULL) {
    TRACE_CTOR(journal_t, "");
    master = new account_t(NULL, "");
    master->journal = this;
  }
  ~journal_t();

  void add_account(account_t * acct) {
    master->add_account(acct);
    acct->journal = this;
  }
  bool remove_account(account_t * acct) {
    return master->remove_account(acct);
    acct->journal = NULL;
  }

  account_t * find_account(const string& name, bool auto_create = true) {
    accounts_map::iterator c = accounts_cache.find(name);
    if (c != accounts_cache.end())
      return (*c).second;

    account_t * account = master->find_account(name, auto_create);
    accounts_cache.insert(accounts_map::value_type(name, account));
    account->journal = this;
    return account;
  }
  account_t * find_account_re(const string& regexp);

  bool add_entry(entry_t * entry);
  bool remove_entry(entry_t * entry);

  void add_entry_finalizer(entry_finalizer_t * finalizer) {
    add_hook<entry_finalizer_t *>(entry_finalize_hooks, finalizer);
  }
  void remove_entry_finalizer(entry_finalizer_t * finalizer) {
    remove_hook<entry_finalizer_t *>(entry_finalize_hooks, finalizer);
  }

  bool valid() const;
};

inline void extend_entry_base(journal_t * journal, entry_base_t& entry,
			      bool post) {
  for (auto_entries_list::iterator i = journal->auto_entries.begin();
       i != journal->auto_entries.end();
       i++)
    (*i)->extend_entry(entry, post);
}

inline bool auto_entry_finalizer_t::operator()(entry_t& entry, bool post) {
  extend_entry_base(journal, entry, post);
  return true;
}

extern const string version;

} // namespace ledger

#endif // _JOURNAL_H
