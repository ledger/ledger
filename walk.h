#ifndef _WALK_H
#define _WALK_H

#include "ledger.h"
#include "balance.h"
#include "valexpr.h"
#include "datetime.h"

#include <iostream>
#include <deque>

namespace ledger {

template <typename T>
struct item_handler {
  item_handler * handler;

 public:
  item_handler() : handler(NULL) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor item_handler<T>");
  }
  item_handler(item_handler * _handler) : handler(_handler) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor item_handler<T>");
  }

  virtual ~item_handler() {
    DEBUG_PRINT("ledger.memory.dtors", "dtor item_handler<T>");
    if (handler)
      delete handler;
  }
  virtual void flush() {
    if (handler)
      handler->flush();
  }
  virtual void operator()(T& item) {
    if (handler)
      (*handler)(item);
  }
};

template <typename T>
class compare_items {
  value_t left_result;
  value_t right_result;

  const value_expr_t * sort_order;

 public:
  compare_items(const value_expr_t * _sort_order)
    : sort_order(_sort_order) {
    assert(sort_order);
  }

  bool operator()(const T * left, const T * right) {
    assert(left);
    assert(right);

    sort_order->compute(left_result, details_t(*left));
    sort_order->compute(right_result, details_t(*right));

    return left_result < right_result;
  }
};

//////////////////////////////////////////////////////////////////////
//
// Transaction handlers
//

typedef std::deque<transaction_t *> transactions_deque;
typedef std::deque<entry_t *>       entries_deque;

inline void walk_transactions(transactions_list::iterator begin,
			      transactions_list::iterator end,
			      item_handler<transaction_t>& handler) {
  for (transactions_list::iterator i = begin; i != end; i++)
    handler(**i);
}

inline void walk_transactions(transactions_list& list,
			      item_handler<transaction_t>& handler) {
  walk_transactions(list.begin(), list.end(), handler);
}

inline void walk_transactions(transactions_deque::iterator begin,
			      transactions_deque::iterator end,
			      item_handler<transaction_t>& handler) {
  for (transactions_deque::iterator i = begin; i != end; i++)
    handler(**i);
}

inline void walk_transactions(transactions_deque& deque,
			      item_handler<transaction_t>& handler) {
  walk_transactions(deque.begin(), deque.end(), handler);
}

inline void walk_entries(entries_list::iterator       begin,
			 entries_list::iterator       end,
			 item_handler<transaction_t>& handler) {
  for (entries_list::iterator i = begin; i != end; i++)
    walk_transactions((*i)->transactions, handler);
}

inline void walk_entries(entries_list& list,
			 item_handler<transaction_t>& handler) {
  walk_entries(list.begin(), list.end(), handler);
}

//////////////////////////////////////////////////////////////////////

#define TRANSACTION_HANDLED   0x0001
#define TRANSACTION_DISPLAYED 0x0002
#define TRANSACTION_NO_TOTAL  0x0004

struct transaction_data_t
{
  value_t	 total;
  unsigned int   index;
  unsigned short dflags;

  transaction_data_t() : index(0), dflags(0) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor transaction_data_t");
  }
#ifdef DEBUG_ENABLED
  ~transaction_data_t() {
    DEBUG_PRINT("ledger.memory.dtors", "dtor transaction_data_t");
  }
#endif
};

#define XACT_DATA(xact) ((transaction_data_t *) ((xact)->data))
#define XACT_DATA_(xact) ((transaction_data_t *) ((xact).data))

#define ACCOUNT_DISPLAYED  0x1
#define ACCOUNT_TO_DISPLAY 0x2

struct account_data_t
{
  value_t	 value;
  value_t	 total;
  unsigned int   count;	// transactions counted toward total
  unsigned int   subcount;
  unsigned short dflags;

  account_data_t() : count(0), subcount(0), dflags(0) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor account_data_t");
  }
#ifdef DEBUG_ENABLED
  ~account_data_t() {
    DEBUG_PRINT("ledger.memory.dtors", "dtor account_data_t");
  }
#endif
};

#define ACCT_DATA(acct) ((account_data_t *) ((acct)->data))
#define ACCT_DATA_(acct) ((account_data_t *) ((acct).data))

//////////////////////////////////////////////////////////////////////

class ignore_transactions : public item_handler<transaction_t>
{
 public:
  virtual void operator()(transaction_t& xact) {}
};

class clear_transaction_data : public item_handler<transaction_t>
{
 public:
  virtual void operator()(transaction_t& xact) {
    if (xact.data) {
      delete (transaction_data_t *) xact.data;
      xact.data = NULL;
    }
  }
};

class set_account_value : public item_handler<transaction_t>
{
 public:
  set_account_value(item_handler<transaction_t> * handler = NULL)
    : item_handler<transaction_t>(handler) {}

  virtual void operator()(transaction_t& xact) {
    if (! ACCT_DATA(xact.account))
      xact.account->data = new account_data_t;

    add_transaction_to(xact, ACCT_DATA(xact.account)->value);
    ACCT_DATA(xact.account)->subcount++;

    if (handler)
      (*handler)(xact);
  }
};

class sort_transactions : public item_handler<transaction_t>
{
  transactions_deque   transactions;
  const value_expr_t * sort_order;

 public:
  sort_transactions(item_handler<transaction_t> * handler,
		    const value_expr_t * _sort_order)
    : item_handler<transaction_t>(handler),
      sort_order(_sort_order) {}

  virtual void flush();
  virtual void operator()(transaction_t& xact) {
    transactions.push_back(&xact);
  }
};

class filter_transactions : public item_handler<transaction_t>
{
  item_predicate<transaction_t> pred;

 public:
  filter_transactions(item_handler<transaction_t> * handler,
		      const std::string& predicate)
    : item_handler<transaction_t>(handler), pred(predicate) {}

  virtual void operator()(transaction_t& xact) {
    if (pred(xact))
      (*handler)(xact);
  }
};

class calc_transactions : public item_handler<transaction_t>
{
  transaction_t * last_xact;
  const bool      inverted;

 public:
  calc_transactions(item_handler<transaction_t> * handler,
		    const bool _inverted = false)
    : item_handler<transaction_t>(handler),
      last_xact(NULL), inverted(_inverted) {}

  virtual void operator()(transaction_t& xact);
};

class collapse_transactions : public item_handler<transaction_t>
{
  balance_pair_t     subtotal;
  unsigned int       count;
  entry_t *          last_entry;
  transaction_t *    last_xact;
  account_t *        totals_account;
  transactions_list xact_temps;

 public:
  collapse_transactions(item_handler<transaction_t> * handler)
    : item_handler<transaction_t>(handler), count(0),
      last_entry(NULL), last_xact(NULL) {
    totals_account = new account_t(NULL, "<Total>");
  }

  virtual ~collapse_transactions() {
    if (totals_account->data) {
      delete (account_data_t *) totals_account->data;
      totals_account->data = NULL;
    }
    delete totals_account;
    for (transactions_list::iterator i = xact_temps.begin();
	 i != xact_temps.end();
	 i++) {
      if ((*i)->data) {
	delete (transaction_data_t *) (*i)->data;
	(*i)->data = NULL;
      }
      delete *i;
    }
  }

  virtual void flush() {
    if (subtotal)
      report_cumulative_subtotal();
    item_handler<transaction_t>::flush();
  }

  void report_cumulative_subtotal();

  virtual void operator()(transaction_t& xact) {
    // If we've reached a new entry, report on the subtotal
    // accumulated thus far.

    if (last_entry && last_entry != xact.entry)
      report_cumulative_subtotal();

    add_transaction_to(xact, subtotal);
    count++;

    last_entry = xact.entry;
    last_xact  = &xact;
  }
};

class changed_value_transactions : public item_handler<transaction_t>
{
  // This filter requires that calc_transactions be used at some point
  // later in the chain.

  bool		    changed_values_only;
  transaction_t *   last_xact;
  entries_list      entry_temps;
  transactions_list xact_temps;

 public:
  changed_value_transactions(item_handler<transaction_t> * handler,
			     bool _changed_values_only)
    : item_handler<transaction_t>(handler),
      changed_values_only(_changed_values_only), last_xact(NULL) {}

  virtual ~changed_value_transactions() {
    for (entries_list::iterator i = entry_temps.begin();
	 i != entry_temps.end();
	 i++)
      delete *i;

    for (transactions_list::iterator i = xact_temps.begin();
	 i != xact_temps.end();
	 i++) {
      if ((*i)->data) {
	delete (transaction_data_t *) (*i)->data;
	(*i)->data = NULL;
      }
      delete *i;
    }
  }

  virtual void flush() {
    output_diff(std::time(NULL));
    last_xact = NULL;
    item_handler<transaction_t>::flush();
  }

  void output_diff(const std::time_t current);

  virtual void operator()(transaction_t& xact);
};

class subtotal_transactions : public item_handler<transaction_t>
{
  typedef std::map<account_t *, balance_pair_t>  balances_map;
  typedef std::pair<account_t *, balance_pair_t> balances_pair;

 protected:
  std::time_t	    start;
  std::time_t	    finish;
  balances_map	    balances;
  entries_list      entry_temps;
  transactions_list xact_temps;

 public:
  subtotal_transactions(item_handler<transaction_t> * handler)
    : item_handler<transaction_t>(handler) {}

  virtual ~subtotal_transactions() {
    for (entries_list::iterator i = entry_temps.begin();
	 i != entry_temps.end();
	 i++)
      delete *i;

    for (transactions_list::iterator i = xact_temps.begin();
	 i != xact_temps.end();
	 i++) {
      if ((*i)->data) {
	delete (transaction_data_t *) (*i)->data;
	(*i)->data = NULL;
      }
      delete *i;
    }
  }

  void flush(const char * spec_fmt);

  virtual void flush() {
    flush(NULL);
  }
  virtual void operator()(transaction_t& xact);
};

class interval_transactions : public subtotal_transactions
{
  std::time_t     begin;
  interval_t      interval;
  transaction_t * last_xact;

 public:
  interval_transactions(item_handler<transaction_t> * handler,
			const interval_t& _interval,
			const std::time_t _begin = 0)
    : subtotal_transactions(handler), begin(_begin),
      interval(_interval), last_xact(NULL) {}

  virtual ~interval_transactions() {
    start  = begin;
    finish = interval.increment(begin);
  }

  virtual void operator()(transaction_t& xact);
};

class dow_transactions : public subtotal_transactions
{
  transactions_list days_of_the_week[7];

 public:
  dow_transactions(item_handler<transaction_t> * handler)
    : subtotal_transactions(handler) {}

  virtual void flush();
  virtual void operator()(transaction_t& xact) {
    struct std::tm * desc = std::localtime(&xact.entry->date);
    days_of_the_week[desc->tm_wday].push_back(&xact);
  }
};

class related_transactions : public item_handler<transaction_t>
{
  bool also_matching;

 public:
  related_transactions(item_handler<transaction_t> * handler,
		       const bool _also_matching = false)
    : item_handler<transaction_t>(handler),
      also_matching(_also_matching) {}

  virtual void operator()(transaction_t& xact) {
    for (transactions_list::iterator i = xact.entry->transactions.begin();
	 i != xact.entry->transactions.end();
	 i++)
      if ((! (*i)->data ||
	   ! (XACT_DATA(*i)->dflags & TRANSACTION_HANDLED)) &&
	  (*i == &xact ? also_matching :
	   ! ((*i)->flags & TRANSACTION_AUTO))) {
	if (! (*i)->data)
	  (*i)->data = new transaction_data_t;
	XACT_DATA(*i)->dflags |= TRANSACTION_HANDLED;
	(*handler)(**i);
      }
  }
};


//////////////////////////////////////////////////////////////////////
//
// Account walking functions
//

class clear_account_data : public item_handler<account_t>
{
 public:
  virtual void operator()(account_t * account) {
    if (account->data) {
      delete (account_data_t *) account->data;
      account->data = NULL;
    }
  }
};

inline void sum_accounts(account_t& account) {
  if (! account.data)
    account.data = new account_data_t;

  for (accounts_map::iterator i = account.accounts.begin();
       i != account.accounts.end();
       i++) {
    sum_accounts(*(*i).second);
    ACCT_DATA_(account)->total += ACCT_DATA((*i).second)->total;
    ACCT_DATA_(account)->count += (ACCT_DATA((*i).second)->count +
				  ACCT_DATA((*i).second)->subcount);
  }
  ACCT_DATA_(account)->total += ACCT_DATA_(account)->value;
  ACCT_DATA_(account)->count += ACCT_DATA_(account)->subcount;
}

typedef std::deque<account_t *> accounts_deque;

inline void sort_accounts(account_t&	       account,
			  const value_expr_t * sort_order,
			  accounts_deque&      accounts) {
  for (accounts_map::iterator i = account.accounts.begin();
       i != account.accounts.end();
       i++)
    accounts.push_back((*i).second);

  std::stable_sort(accounts.begin(), accounts.end(),
		   compare_items<account_t>(sort_order));
}

inline void walk_accounts(account_t&		   account,
			  item_handler<account_t>& handler,
			  const value_expr_t *     sort_order = NULL) {
  handler(account);

  if (sort_order) {
    accounts_deque accounts;
    sort_accounts(account, sort_order, accounts);
    for (accounts_deque::const_iterator i = accounts.begin();
	 i != accounts.end();
	 i++)
      walk_accounts(**i, handler, sort_order);
  } else {
    for (accounts_map::const_iterator i = account.accounts.begin();
	 i != account.accounts.end();
	 i++)
      walk_accounts(*(*i).second, handler);
  }
}

} // namespace ledger

#endif // _WALK_H
