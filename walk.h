#ifndef _WALK_H
#define _WALK_H

#include "journal.h"
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

#define TRANSACTION_HANDLED    0x0001
#define TRANSACTION_TO_DISPLAY 0x0002
#define TRANSACTION_DISPLAYED  0x0004
#define TRANSACTION_NO_TOTAL   0x0008

struct transaction_xdata_t
{
  value_t	 total;
  unsigned int   index;
  unsigned short dflags;

  transaction_xdata_t() : index(0), dflags(0) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor transaction_xdata_t");
  }
#ifdef DEBUG_ENABLED
  ~transaction_xdata_t() {
    DEBUG_PRINT("ledger.memory.dtors", "dtor transaction_xdata_t");
  }
#endif
};

inline bool transaction_has_xdata(const transaction_t& xact) {
  return xact.data != NULL;
}

extern std::list<transaction_xdata_t> transactions_xdata;
extern std::list<void **>	      transactions_xdata_ptrs;

inline transaction_xdata_t& transaction_xdata(const transaction_t& xact) {
  if (! xact.data) {
    transactions_xdata.push_back(transaction_xdata_t());
    xact.data = &transactions_xdata.back();
    transactions_xdata_ptrs.push_back(&xact.data);
  }
  return *((transaction_xdata_t *) xact.data);
}

//////////////////////////////////////////////////////////////////////

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

inline void clear_transactions_xdata() {
  transactions_xdata.clear();

  for (std::list<void **>::iterator i = transactions_xdata_ptrs.begin();
       i != transactions_xdata_ptrs.end();
       i++)
    **i = NULL;
}

//////////////////////////////////////////////////////////////////////

class ignore_transactions : public item_handler<transaction_t>
{
 public:
  virtual void operator()(transaction_t& xact) {}
};

class set_account_value : public item_handler<transaction_t>
{
 public:
  set_account_value(item_handler<transaction_t> * handler = NULL)
    : item_handler<transaction_t>(handler) {}

  virtual void operator()(transaction_t& xact);
};

class sort_transactions : public item_handler<transaction_t>
{
  transactions_deque   transactions;
  const value_expr_t * sort_order;
  bool                 allocated;

 public:
  sort_transactions(item_handler<transaction_t> * handler,
		    const value_expr_t * _sort_order)
    : item_handler<transaction_t>(handler),
      sort_order(_sort_order), allocated(false) {}

  sort_transactions(item_handler<transaction_t> * handler,
		    const std::string& _sort_order)
    : item_handler<transaction_t>(handler), allocated(true) {
    try {
      sort_order = parse_value_expr(_sort_order);
    }
    catch (value_expr_error& err) {
      throw value_expr_error(std::string("In sort string '") + _sort_order +
			     "': " + err.what());
    }
  }

  virtual ~sort_transactions() {
    assert(sort_order);
    if (allocated)
      delete sort_order;
  }

  virtual void flush();
  virtual void operator()(transaction_t& xact) {
    transactions.push_back(&xact);
  }
};

class filter_transactions : public item_handler<transaction_t>
{
  const item_predicate<transaction_t> * pred;
  bool allocated;

 public:
  filter_transactions(item_handler<transaction_t> * handler,
		      const item_predicate<transaction_t> * predicate)
    : item_handler<transaction_t>(handler),
      pred(predicate), allocated(false) {}

  filter_transactions(item_handler<transaction_t> * handler,
		      const std::string& predicate)
    : item_handler<transaction_t>(handler),
      pred(new item_predicate<transaction_t>(predicate)),
      allocated(true) {}

  virtual ~filter_transactions() {
    if (allocated)
      delete pred;
  }

  virtual void operator()(transaction_t& xact) {
    if ((*pred)(xact))
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
  balance_pair_t    subtotal;
  unsigned int      count;
  entry_t *         last_entry;
  transaction_t *   last_xact;
  account_t         totals_account;

  std::list<transaction_t> xact_temps;

 public:
  collapse_transactions(item_handler<transaction_t> * handler)
    : item_handler<transaction_t>(handler), count(0),
      last_entry(NULL), last_xact(NULL), totals_account(NULL, "<Total>") {}

  virtual void flush() {
    if (subtotal)
      report_cumulative_subtotal();
    item_handler<transaction_t>::flush();
  }

  void report_cumulative_subtotal();

  virtual void operator()(transaction_t& xact) {
    // If we've reached a new entry, report on the subtotal
    // accumulated thus far.

    if (last_entry && last_entry != xact.entry && count > 0)
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

  bool			   changed_values_only;
  transaction_t *	   last_xact;
  std::list<entry_t>       entry_temps;
  std::list<transaction_t> xact_temps;

 public:
  changed_value_transactions(item_handler<transaction_t> * handler,
			     bool _changed_values_only)
    : item_handler<transaction_t>(handler),
      changed_values_only(_changed_values_only), last_xact(NULL) {}

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
  std::time_t		   start;
  std::time_t		   finish;
  balances_map		   balances;
  std::list<entry_t>       entry_temps;
  std::list<transaction_t> xact_temps;

 public:
  subtotal_transactions(item_handler<transaction_t> * handler)
    : item_handler<transaction_t>(handler) {}

  void flush(const char * spec_fmt);

  virtual void flush() {
    flush(NULL);
  }
  virtual void operator()(transaction_t& xact);
};

class interval_transactions : public subtotal_transactions
{
  interval_t      interval;
  transaction_t * last_xact;

 public:
  interval_transactions(item_handler<transaction_t> * handler,
			const interval_t& _interval)
    : subtotal_transactions(handler), interval(_interval),
      last_xact(NULL) {}

  interval_transactions(item_handler<transaction_t> * handler,
			const std::string& _interval)
    : subtotal_transactions(handler), interval(_interval),
      last_xact(NULL) {}

  virtual ~interval_transactions() {
    start  = interval.begin;
    finish = interval.increment(interval.begin);
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
      if ((! transaction_has_xdata(**i) ||
	   ! (transaction_xdata(**i).dflags & TRANSACTION_HANDLED)) &&
	  (*i == &xact ? also_matching :
	   ! ((*i)->flags & TRANSACTION_AUTO))) {
	transaction_xdata(**i).dflags |= TRANSACTION_HANDLED;
	(*handler)(**i);
      }
  }
};


//////////////////////////////////////////////////////////////////////
//
// Account walking functions
//

#define ACCOUNT_TO_DISPLAY 0x1
#define ACCOUNT_DISPLAYED  0x2

struct account_xdata_t
{
  value_t	 value;
  value_t	 total;
  unsigned int   count;	// transactions counted toward total
  unsigned int   subcount;
  unsigned short dflags;

  account_xdata_t() : count(0), subcount(0), dflags(0) {
    DEBUG_PRINT("ledger.memory.ctors", "ctor account_xdata_t");
  }
#ifdef DEBUG_ENABLED
  ~account_xdata_t() {
    DEBUG_PRINT("ledger.memory.dtors", "dtor account_xdata_t");
  }
#endif
};

inline bool account_has_xdata(const account_t& account) {
  return account.data != NULL;
}

extern std::list<account_xdata_t> accounts_xdata;
extern std::list<void **>	  accounts_xdata_ptrs;

inline account_xdata_t& account_xdata(const account_t& account) {
  if (! account.data) {
    accounts_xdata.push_back(account_xdata_t());
    account.data = &accounts_xdata.back();
    accounts_xdata_ptrs.push_back(&account.data);
  }
  return *((account_xdata_t *) account.data);
}

inline void set_account_value::operator()(transaction_t& xact) {
  add_transaction_to(xact, account_xdata(*xact.account).value);
  account_xdata(*xact.account).subcount++;

  if (handler)
    (*handler)(xact);
}

//////////////////////////////////////////////////////////////////////

void sum_accounts(account_t& account);

typedef std::deque<account_t *> accounts_deque;

void sort_accounts(account_t&	        account,
		   const value_expr_t * sort_order,
		   accounts_deque&      accounts);

void walk_accounts(account_t&		    account,
		   item_handler<account_t>& handler,
		   const value_expr_t *     sort_order = NULL);

void walk_accounts(account_t&		    account,
		   item_handler<account_t>& handler,
		   const std::string&       sort_string);

void clear_accounts_xdata();

} // namespace ledger

#endif // _WALK_H
