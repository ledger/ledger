#ifndef _JOURNAL_H
#define _JOURNAL_H

#include <map>
#include <list>
#include <string>
#include <ctime>
#include <iostream>

#include "amount.h"
#include "datetime.h"
#include "value.h"
#include "error.h"
#include "debug.h"

namespace ledger {

// These flags persist with the object
#define TRANSACTION_NORMAL     0x0000
#define TRANSACTION_VIRTUAL    0x0001
#define TRANSACTION_BALANCE    0x0002
#define TRANSACTION_AUTO       0x0004
#define TRANSACTION_BULK_ALLOC 0x0008

class entry_t;
class account_t;

class transaction_t
{
 public:
  entry_t *	 entry;
  account_t *	 account;
  amount_t	 amount;
  amount_t *	 cost;
  unsigned short flags;
  std::string	 note;
  mutable void * data;

  transaction_t(account_t * _account = NULL)
    : entry(NULL), account(_account), cost(NULL),
      flags(TRANSACTION_NORMAL), data(NULL) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor transaction_t");
  }

  transaction_t(account_t *	   _account,
		const amount_t&    _amount,
		unsigned int	   _flags = TRANSACTION_NORMAL,
		const std::string& _note  = "")
    : entry(NULL), account(_account), amount(_amount),
      cost(NULL), flags(_flags), note(_note), data(NULL) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor transaction_t");
  }

  transaction_t(const transaction_t& xact)
    : entry(xact.entry), account(xact.account), amount(xact.amount),
      cost(xact.cost ? new amount_t(*xact.cost) : NULL),
      flags(xact.flags), note(xact.note), data(NULL) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor transaction_t");
  }

  ~transaction_t() {
    DEBUG_PRINT("ledger.memory.dtors", "dtor transaction_t");
    if (cost)
      delete cost;
  }

  bool operator==(const transaction_t& xact) {
    return this == &xact;
  }
  bool operator!=(const transaction_t& xact) {
    return ! (*this == xact);
  }

  bool valid() const;
};

typedef std::list<transaction_t *> transactions_list;

class entry_base_t
{
 public:
  transactions_list transactions;

  entry_base_t() {
    DEBUG_PRINT("ledger.memory.ctors", "ctor entry_base_t");
  }
  entry_base_t(const entry_base_t& e) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor entry_base_t");
    for (transactions_list::const_iterator i = e.transactions.begin();
	 i != e.transactions.end();
	 i++)
      transactions.push_back(new transaction_t(**i));
  }
  virtual ~entry_base_t() {
    DEBUG_PRINT("ledger.memory.dtors", "dtor entry_base_t");
    for (transactions_list::iterator i = transactions.begin();
	 i != transactions.end();
	 i++)
      if (! ((*i)->flags & TRANSACTION_BULK_ALLOC))
	delete *i;
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
  enum state_t { UNCLEARED, CLEARED, PENDING };

  std::time_t date;
  state_t     state;
  std::string code;
  std::string payee;

  entry_t() : date(0), state(UNCLEARED) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor entry_t");
  }
  entry_t(const entry_t& e);

#ifdef DEBUG_ENABLED
  virtual ~entry_t() {
    DEBUG_PRINT("ledger.memory.dtors", "dtor entry_t");
  }
#endif

  virtual void add_transaction(transaction_t * xact);

  virtual bool valid() const;
};

struct entry_finalizer_t {
  virtual ~entry_finalizer_t() {}
  virtual bool operator()(entry_t& entry) = 0;
};


template <typename T>
class item_predicate;

class auto_entry_t : public entry_base_t
{
public:
  item_predicate<transaction_t> * predicate;
  std::string predicate_string;

  auto_entry_t() : predicate(NULL) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor auto_entry_t");
  }
  auto_entry_t(const std::string& _predicate);

  virtual ~auto_entry_t();

  virtual void extend_entry(entry_base_t& entry);
  virtual bool valid() const {
    return true;
  }
};

class journal_t;
struct auto_entry_finalizer_t : public entry_finalizer_t {
  journal_t * journal;
  auto_entry_finalizer_t(journal_t * _journal) : journal(_journal) {}
  virtual bool operator()(entry_t& entry);
};


class period_entry_t : public entry_base_t
{
 public:
  interval_t  period;
  std::string period_string;

  period_entry_t() {
    DEBUG_PRINT("ledger.memory.ctors", "ctor period_entry_t");
  }
  period_entry_t(const std::string& _period)
    : period(_period), period_string(_period) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor period_entry_t");
  }
  period_entry_t(const period_entry_t& e)
    : entry_base_t(e), period(e.period), period_string(e.period_string) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor period_entry_t");
  }

#ifdef DEBUG_ENABLED
  virtual ~period_entry_t() {
    DEBUG_PRINT("ledger.memory.dtors", "dtor period_entry_t");
  }
#endif

  virtual bool valid() const {
    return period;
  }
};


typedef std::map<const std::string, account_t *> accounts_map;
typedef std::pair<const std::string, account_t *> accounts_pair;

class account_t
{
 public:
  typedef unsigned long ident_t;

  account_t *	      parent;
  std::string	      name;
  std::string	      note;
  unsigned short      depth;
  accounts_map	      accounts;

  mutable void *      data;
  mutable ident_t     ident;
  mutable std::string _fullname;

  account_t(account_t *        _parent = NULL,
	    const std::string& _name   = "",
	    const std::string& _note   = "")
    : parent(_parent), name(_name), note(_note),
      depth(parent ? parent->depth + 1 : 0), data(NULL), ident(0) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor account_t");
  }

  ~account_t();

  bool operator==(const account_t& account) {
    return this == &account;
  }
  bool operator!=(const account_t& account) {
    return ! (*this == account);
  }

  std::string fullname() const;

  void add_account(account_t * acct) {
    accounts.insert(accounts_pair(acct->name, acct));
  }
  bool remove_account(account_t * acct) {
    accounts_map::size_type n = accounts.erase(acct->name);
    return n > 0;
  }

  account_t * find_account(const std::string& name, bool auto_create = true);

  operator std::string() const {
    return fullname();
  }

  bool valid() const;

  friend class journal_t;
};

std::ostream& operator<<(std::ostream& out, const account_t& account);


struct func_finalizer_t : public entry_finalizer_t {
  typedef bool (*func_t)(entry_t& entry);
  func_t func;
  func_finalizer_t(func_t _func) : func(_func) {}
  func_finalizer_t(const func_finalizer_t& other) : func(other.func) {}
  virtual bool operator()(entry_t& entry) {
    return func(entry);
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
bool run_hooks(std::list<T>& list, Data& item) {
  for (typename std::list<T>::const_iterator i = list.begin();
       i != list.end();
       i++)
    if (! (*(*i))(item))
      return false;
  return true;
}


typedef std::list<entry_t *>	    entries_list;
typedef std::list<auto_entry_t *>   auto_entries_list;
typedef std::list<period_entry_t *> period_entries_list;
typedef std::list<std::string>	    strings_list;

class journal_t
{
 public:
  account_t *  master;
  entries_list entries;
  strings_list sources;
  std::string  price_db;
  char *       item_pool;
  char *       item_pool_end;

  auto_entries_list    auto_entries;
  period_entries_list  period_entries;
  mutable accounts_map accounts_cache;

  std::list<entry_finalizer_t *> entry_finalize_hooks;

  journal_t() {
    DEBUG_PRINT("ledger.memory.ctors", "ctor journal_t");
    master = new account_t(NULL, "");
    item_pool = item_pool_end = NULL;
  }
  ~journal_t();

  bool operator==(const journal_t& journal) {
    return this == &journal;
  }
  bool operator!=(const journal_t& journal) {
    return ! (*this == journal);
  }

  void add_account(account_t * acct) {
    master->add_account(acct);
  }
  bool remove_account(account_t * acct) {
    return master->remove_account(acct);
  }

  account_t * find_account(const std::string& name, bool auto_create = true) {
    accounts_map::iterator c = accounts_cache.find(name);
    if (c != accounts_cache.end())
      return (*c).second;

    account_t * account = master->find_account(name, auto_create);
    accounts_cache.insert(accounts_pair(name, account));
    return account;
  }
  account_t * find_account_re(const std::string& regexp);

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

inline void extend_entry_base(journal_t * journal, entry_base_t& entry) {
  for (auto_entries_list::iterator i = journal->auto_entries.begin();
       i != journal->auto_entries.end();
       i++)
    (*i)->extend_entry(entry);
}

inline bool auto_entry_finalizer_t::operator()(entry_t& entry) {
  extend_entry_base(journal, entry);
  return true;
}

extern const std::string version;

} // namespace ledger

#endif // _JOURNAL_H
