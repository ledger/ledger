#ifndef _LEDGER_H
#define _LEDGER_H

//////////////////////////////////////////////////////////////////////
//
// Ledger Accounting Tool
//
//   A command-line tool for general double-entry accounting.
//
// Copyright (c) 2003,2004 John Wiegley <johnw@newartisans.com>
//

#include <map>
#include <list>
#include <string>
#include <ctime>
#include <iostream>

#include "amount.h"
#include "value.h"
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
  }

  transaction_t(account_t *	   _account,
		const amount_t&    _amount,
		unsigned int	   _flags = TRANSACTION_NORMAL,
		const std::string& _note  = "")
    : entry(NULL), account(_account), amount(_amount),
      cost(NULL), flags(_flags), note(_note), data(NULL) {
  }

  ~transaction_t() {
    //assert(! data);
    if (cost)
      delete cost;
  }

#ifdef USE_BOOST_PYTHON
  entry_t& get_entry() const {
    assert(entry);
    return *entry;
  }
#endif

  bool valid() const;
};

inline
balance_pair_t& add_transaction_to(const transaction_t& xact,
				   balance_pair_t&	bal_pair)
{
  if (xact.cost && ! bal_pair.cost)
    bal_pair.cost = new balance_t(bal_pair.quantity);

  bal_pair.quantity += xact.amount;

  if (bal_pair.cost)
    *bal_pair.cost += xact.cost ? *xact.cost : xact.amount;

  return bal_pair;
}

inline
value_t& add_transaction_to(const transaction_t& xact, value_t& value)
{
  switch (value.type) {
  case value_t::BOOLEAN:
  case value_t::INTEGER:
    value.cast(value_t::AMOUNT);

  case value_t::AMOUNT:
    if (xact.cost) {
      value.cast(value_t::BALANCE_PAIR);
      return add_transaction_to(xact, value);
    }
    else if (((amount_t *) value.data)->commodity() !=
	     xact.amount.commodity()) {
      value.cast(value_t::BALANCE);
      return add_transaction_to(xact, value);
    }
    *((amount_t *) value.data) += xact.amount;
    break;

  case value_t::BALANCE:
    if (xact.cost) {
      value.cast(value_t::BALANCE_PAIR);
      return add_transaction_to(xact, value);
    }
    *((balance_t *) value.data) += xact.amount;
    break;

  case value_t::BALANCE_PAIR:
    add_transaction_to(xact, *((balance_pair_t *) value.data));
    break;

  default:
    assert(0);
    break;
  }

  return value;
}

typedef std::list<transaction_t *> transactions_list;

class entry_t
{
 public:
  enum entry_state_t {
    UNCLEARED, CLEARED, PENDING
  };

  std::time_t	    date;
  entry_state_t     state;
  std::string	    code;
  std::string	    payee;
  transactions_list transactions;

  entry_t() : date(-1), state(UNCLEARED) {}
  ~entry_t() {
    for (transactions_list::iterator i = transactions.begin();
	 i != transactions.end();
	 i++)
      if (! ((*i)->flags & TRANSACTION_BULK_ALLOC))
	delete *i;
      else
	(*i)->~transaction_t();
  }

  void add_transaction(transaction_t * xact);
  bool remove_transaction(transaction_t * xact);

  bool valid() const;
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
  transactions_list   transactions;
  mutable void *      data;
  mutable ident_t     ident;
  mutable std::string _fullname;

  account_t(account_t *        _parent = NULL,
	    const std::string& _name   = "",
	    const std::string& _note   = "")
    : parent(_parent), name(_name), note(_note),
      depth(parent ? parent->depth + 1 : 0), data(NULL), ident(0) {}

  ~account_t();

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

  // These functions should only be called from journal_t::add_entry
  // and journal_t::remove_entry; or from the various parsers.
  void add_transaction(transaction_t * xact) {
    transactions.push_back(xact);
  }
  bool remove_transaction(transaction_t * xact);

  friend class journal_t;

  bool valid() const;
};

inline std::ostream& operator<<(std::ostream& out, const account_t& acct) {
  out << acct.fullname();
  return out;
}


typedef std::list<entry_t *>   entries_list;
typedef std::list<std::string> strings_list;

class journal_t
{
 public:
  account_t *  master;
  entries_list entries;
  strings_list sources;
  char *       item_pool;
  char *       item_pool_end;

  mutable accounts_map accounts_cache;

  journal_t() {
    master = new account_t(NULL, "");
    item_pool = item_pool_end = NULL;
  }
  ~journal_t();

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
  account_t * find_account(const std::string& name) const {
    // With auto_create false, the other `find_account' will not
    // change the object.
    return const_cast<journal_t *>(this)->find_account(name, false);
  }

  bool add_entry(entry_t * entry);
  bool remove_entry(entry_t * entry);

  entry_t * derive_entry(strings_list::iterator begin,
			 strings_list::iterator end) const;

  bool valid() const;
};

extern const std::string version;

} // namespace ledger

#endif // _LEDGER_H
