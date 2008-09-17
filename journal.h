#ifndef _JOURNAL_H
#define _JOURNAL_H

#include <map>
#include <list>
#include <string>
#include <iostream>

#include "amount.h"
#include "datetime.h"
#include "value.h"
#include "valexpr.h"
#include "error.h"
#include "debug.h"
#include "util.h"

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

class transaction_t
{
 public:
  enum state_t { UNCLEARED, CLEARED, PENDING };

  entry_t *	   entry;
  datetime_t	   _date;
  datetime_t	   _date_eff;
  account_t *	   account;
  amount_t	   amount;
  value_expr       amount_expr;
  amount_t *	   cost;
  std::string      cost_expr;
  state_t	   state;
  unsigned short   flags;
  std::string	   note;
  istream_pos_type beg_pos;
  unsigned long    beg_line;
  istream_pos_type end_pos;
  unsigned long    end_line;
  mutable void *   data;

  static bool    use_effective_date;

  transaction_t(account_t * _account = NULL)
    : entry(NULL), account(_account), cost(NULL),
      state(UNCLEARED), flags(TRANSACTION_NORMAL),
      beg_pos(0), beg_line(0), end_pos(0), end_line(0), data(NULL) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor transaction_t");
  }
  transaction_t(account_t *	   _account,
		const amount_t&    _amount,
		unsigned int	   _flags = TRANSACTION_NORMAL,
		const std::string& _note  = "")
    : entry(NULL), account(_account), amount(_amount), cost(NULL),
      state(UNCLEARED), flags(_flags),
      note(_note), beg_pos(0), beg_line(0), end_pos(0), end_line(0),
      data(NULL) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor transaction_t");
  }
  transaction_t(const transaction_t& xact)
    : entry(xact.entry), account(xact.account), amount(xact.amount),
      cost(xact.cost ? new amount_t(*xact.cost) : NULL),
      state(xact.state), flags(xact.flags), note(xact.note),
      beg_pos(0), beg_line(0), end_pos(0), end_line(0), data(NULL) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor transaction_t");
  }
  ~transaction_t();

  datetime_t actual_date() const;
  datetime_t effective_date() const;
  datetime_t date() const {
    if (use_effective_date)
      return effective_date();
    else
      return actual_date();
  }

  bool operator==(const transaction_t& xact) {
    return this == &xact;
  }
  bool operator!=(const transaction_t& xact) {
    return ! (*this == xact);
  }

  bool valid() const;
};

class xact_context : public file_context {
 public:
  const transaction_t& xact;

  xact_context(const transaction_t& _xact,
	       const std::string& desc = "") throw();
  virtual ~xact_context() throw() {}
};

class journal_t;

typedef std::list<transaction_t *> transactions_list;

class entry_base_t
{
 public:
  journal_t *       journal;
  unsigned long     src_idx;
  istream_pos_type  beg_pos;
  unsigned long     beg_line;
  istream_pos_type  end_pos;
  unsigned long     end_line;
  transactions_list transactions;

  entry_base_t() : journal(NULL),
    beg_pos(0), beg_line(0), end_pos(0), end_line(0)
  {
    DEBUG_PRINT("ledger.memory.ctors", "ctor entry_base_t");
  }
  entry_base_t(const entry_base_t& e) : journal(NULL),
    beg_pos(0), beg_line(0), end_pos(0), end_line(0)
  {
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
  datetime_t  _date;
  datetime_t  _date_eff;
  std::string code;
  std::string payee;

  entry_t() {
    DEBUG_PRINT("ledger.memory.ctors", "ctor entry_t");
  }
  entry_t(const entry_t& e);

  virtual ~entry_t() {
    DEBUG_PRINT("ledger.memory.dtors", "dtor entry_t");
  }

  datetime_t actual_date() const {
    return _date;
  }
  datetime_t effective_date() const {
    if (! _date_eff)
      return _date;
    return _date_eff;
  }
  datetime_t date() const {
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

class entry_context : public error_context {
 public:
  const entry_base_t& entry;

  entry_context(const entry_base_t& _entry,
		const std::string& desc = "") throw()
    : error_context(desc), entry(_entry) {}
  virtual ~entry_context() throw() {}

  virtual void describe(std::ostream& out) const throw();
};

class balance_error : public error {
 public:
  balance_error(const std::string& reason, error_context * ctxt = NULL) throw()
    : error(reason, ctxt) {}
  virtual ~balance_error() throw() {}
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

  virtual void extend_entry(entry_base_t& entry, bool post);
  virtual bool valid() const {
    return true;
  }
};

class journal_t;
struct auto_entry_finalizer_t : public entry_finalizer_t {
  journal_t * journal;
  auto_entry_finalizer_t(journal_t * _journal) : journal(_journal) {}
  virtual bool operator()(entry_t& entry, bool post);
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

  virtual ~period_entry_t() {
    DEBUG_PRINT("ledger.memory.dtors", "dtor period_entry_t");
  }

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

  journal_t *    journal;
  account_t *	 parent;
  std::string	 name;
  std::string	 note;
  unsigned short depth;
  accounts_map	 accounts;

  mutable void *  data;
  mutable ident_t ident;
  mutable std::string _fullname;

  account_t(account_t *        _parent = NULL,
	    const std::string& _name   = "",
	    const std::string& _note   = "")
    : parent(_parent), name(_name), note(_note),
      depth(parent ? parent->depth + 1 : 0), data(NULL), ident(0) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor account_t " << this);
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
    acct->journal = journal;
  }
  bool remove_account(account_t * acct) {
    accounts_map::size_type n = accounts.erase(acct->name);
    acct->journal = NULL;
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
  typedef bool (*func_t)(entry_t& entry, bool post);
  func_t func;
  func_finalizer_t(func_t _func) : func(_func) {}
  func_finalizer_t(const func_finalizer_t& other) : func(other.func) {}
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
typedef std::list<std::string>	    strings_list;

class journal_t
{
 public:
  account_t *  master;
  account_t *  basket;
  entries_list entries;
  strings_list sources;
  std::string  price_db;
  char *       item_pool;
  char *       item_pool_end;

  auto_entries_list    auto_entries;
  period_entries_list  period_entries;
  mutable accounts_map accounts_cache;

  std::list<entry_finalizer_t *> entry_finalize_hooks;

  journal_t() : basket(NULL) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor journal_t");
    master = new account_t(NULL, "");
    master->journal = this;
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
    acct->journal = this;
  }
  bool remove_account(account_t * acct) {
    return master->remove_account(acct);
    acct->journal = NULL;
  }

  account_t * find_account(const std::string& name, bool auto_create = true) {
    accounts_map::iterator c = accounts_cache.find(name);
    if (c != accounts_cache.end())
      return (*c).second;

    account_t * account = master->find_account(name, auto_create);
    accounts_cache.insert(accounts_pair(name, account));
    account->journal = this;
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

extern const std::string version;

} // namespace ledger

#endif // _JOURNAL_H
