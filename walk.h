#ifndef _WALK_H
#define _WALK_H

#include "journal.h"

namespace ledger {

template <typename T>
struct item_handler : public noncopyable
{
  shared_ptr<item_handler> handler;

public:
  item_handler() {
    TRACE_CTOR(item_handler, "");
  }
  item_handler(shared_ptr<item_handler> _handler) : handler(_handler) {
    TRACE_CTOR(item_handler, "shared_ptr<item_handler>");
  }
  virtual ~item_handler() {
    TRACE_DTOR(item_handler);
  }

  virtual void flush() {
    if (handler.get())
      handler->flush();
  }
  virtual void operator()(T& item) {
    if (handler.get())
      (*handler.get())(item);
  }
};

typedef shared_ptr<item_handler<transaction_t> > xact_handler_ptr;

template <typename T>
class compare_items
{
  value_expr sort_order;

  compare_items();
  
public:
  compare_items(const compare_items& other) : sort_order(other.sort_order) {
    TRACE_CTOR(compare_items, "copy");
  }
  compare_items(const value_expr& _sort_order) : sort_order(_sort_order) {
    TRACE_CTOR(compare_items, "const value_expr&");
  }
  ~compare_items() throw() {
    TRACE_DTOR(compare_items);
  }
  bool operator()(const T * left, const T * right);
};

template <typename T>
bool compare_items<T>::operator()(const T * left, const T * right)
{
  assert(left);
  assert(right);

  value_t left_result;
  value_t right_result;
  sort_order.compute(left_result, details_t(*left));
  sort_order.compute(right_result, details_t(*right));

  return left_result < right_result;
}

template <>
bool compare_items<transaction_t>::operator()(const transaction_t * left,
					      const transaction_t * right);
template <>
bool compare_items<account_t>::operator()(const account_t * left,
					  const account_t * right);

//////////////////////////////////////////////////////////////////////
//
// Transaction handlers
//

#define TRANSACTION_RECEIVED   0x0001
#define TRANSACTION_HANDLED    0x0002
#define TRANSACTION_TO_DISPLAY 0x0004
#define TRANSACTION_DISPLAYED  0x0008
#define TRANSACTION_NO_TOTAL   0x0010
#define TRANSACTION_SORT_CALC  0x0020
#define TRANSACTION_COMPOUND   0x0040
#define TRANSACTION_MATCHES    0x0080

struct transaction_xdata_t : public noncopyable
{
  value_t	 total;
  value_t	 sort_value;
  value_t	 value;
  unsigned int   index;
  unsigned short dflags;
  datetime_t     date;
  account_t *    account;
  void *         ptr;

  transactions_list * component_xacts;

  transaction_xdata_t()
    : index(0), dflags(0),
      account(NULL), ptr(NULL), component_xacts(NULL) {
    TRACE_CTOR(transaction_xdata_t, "");
  }
  ~transaction_xdata_t() {
    TRACE_DTOR(transaction_xdata_t);
    if (component_xacts)
      checked_delete(component_xacts);
  }

  void remember_xact(transaction_t& xact) {
    if (! component_xacts)
      component_xacts = new transactions_list;
    component_xacts->push_back(&xact);
  }

  bool have_component_xacts() const {
    return component_xacts != NULL && ! component_xacts->empty();
  }

  void copy_component_xacts(transactions_list& xacts) {
    for (transactions_list::const_iterator i = xacts.begin();
	 i != xacts.end();
	 i++)
      remember_xact(**i);
  }

  void walk_component_xacts(item_handler<transaction_t>& handler) const {
    for (transactions_list::const_iterator i = component_xacts->begin();
	 i != component_xacts->end();
	 i++)
      handler(**i);
  }
};

inline bool transaction_has_xdata(const transaction_t& xact) {
  return xact.data != NULL;
}

inline transaction_xdata_t& transaction_xdata_(const transaction_t& xact) {
  return *((transaction_xdata_t *) xact.data);
}

transaction_xdata_t& transaction_xdata(const transaction_t& xact);
void add_transaction_to(const transaction_t& xact, value_t& value);

inline account_t * xact_account(transaction_t& xact) {
  if (xact.data) {
    account_t * account = transaction_xdata(xact).account;
    if (account)
      return account;
  }
  return xact.account;
}

inline const account_t * xact_account(const transaction_t& xact) {
  return xact_account(const_cast<transaction_t&>(xact));
}

//////////////////////////////////////////////////////////////////////

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
  ~entries_iterator() throw() {
    TRACE_DTOR(entries_iterator);
  }

  void reset(session_t& session);

  entry_t * operator()();
};

class transactions_iterator : public noncopyable
{
public:
  virtual transaction_t * operator()() = 0;
};

class entry_transactions_iterator : public transactions_iterator
{
  transactions_list::iterator xacts_i;
  transactions_list::iterator xacts_end;

  bool xacts_uninitialized;

public:
  entry_transactions_iterator() : xacts_uninitialized(true) {
    TRACE_CTOR(entry_transactions_iterator, "");
  }
  entry_transactions_iterator(entry_t& entry)
    : xacts_uninitialized(true) {
    TRACE_CTOR(entry_transactions_iterator, "entry_t&");
    reset(entry);
  }
  virtual ~entry_transactions_iterator() throw() {
    TRACE_DTOR(entry_transactions_iterator);
  }

  void reset(entry_t& entry) {
    xacts_i   = entry.transactions.begin();
    xacts_end = entry.transactions.end();

    xacts_uninitialized = false;
  }

  virtual transaction_t * operator()() {
    if (xacts_i == xacts_end || xacts_uninitialized)
      return NULL;
    return *xacts_i++;
  }
};

class session_transactions_iterator : public transactions_iterator
{
  entries_iterator	      entries;
  entry_transactions_iterator xacts;

public:
  session_transactions_iterator() {
    TRACE_CTOR(session_transactions_iterator, "");
  }
  session_transactions_iterator(session_t& session) {
    TRACE_CTOR(session_transactions_iterator, "session_t&");
    reset(session);
  }
  virtual ~session_transactions_iterator() throw() {
    TRACE_DTOR(session_transactions_iterator);
  }

  void reset(session_t& session);

  virtual transaction_t * operator()();
};

//////////////////////////////////////////////////////////////////////

class ignore_transactions : public item_handler<transaction_t>
{
public:
  virtual void operator()(transaction_t& xact) {}
};

class clear_transaction_xdata : public item_handler<transaction_t>
{
public:
  virtual void operator()(transaction_t& xact) {
    if (xact.data) {
      delete (transaction_xdata_t *) xact.data;
      xact.data = NULL;
    }
  }
};

class pass_down_transactions : public item_handler<transaction_t>
{
  pass_down_transactions();

public:
  pass_down_transactions(xact_handler_ptr handler,
			 transactions_iterator& iter)
    : item_handler<transaction_t>(handler) {
    TRACE_CTOR(pass_down_transactions,
	       "xact_handler_ptr, transactions_iterator");
    for (transaction_t * xact = iter(); xact; xact = iter())
      item_handler<transaction_t>::operator()(*xact);
  }

  virtual ~pass_down_transactions() {
    TRACE_DTOR(pass_down_transactions);
  }
};

class truncate_entries : public item_handler<transaction_t>
{
  int head_count;
  int tail_count;

  transactions_list xacts;

  truncate_entries();

public:
  truncate_entries(xact_handler_ptr handler,
		   int _head_count, int _tail_count)
    : item_handler<transaction_t>(handler),
      head_count(_head_count), tail_count(_tail_count) {
    TRACE_CTOR(truncate_entries, "xact_handler_ptr, int, int");
  }
  virtual ~truncate_entries() {
    TRACE_DTOR(truncate_entries);
  }

  virtual void flush();
  virtual void operator()(transaction_t& xact) {
    xacts.push_back(&xact);
  }
};

class set_account_value : public item_handler<transaction_t>
{
public:
  set_account_value(xact_handler_ptr handler = xact_handler_ptr())
    : item_handler<transaction_t>(handler) {}

  virtual void operator()(transaction_t& xact);
};

class push_to_transactions_list : public item_handler<transaction_t>
{
  push_to_transactions_list();

public:
  transactions_list& xact_list;

  push_to_transactions_list(transactions_list& _xact_list)
    : xact_list(_xact_list) {
    TRACE_CTOR(push_to_transactions_list, "transactions_list&");
  }
  virtual ~push_to_transactions_list() {
    TRACE_DTOR(push_to_transactions_list);
  }

  virtual void operator()(transaction_t& xact) {
    xact_list.push_back(&xact);
  }
};

class sort_transactions : public item_handler<transaction_t>
{
  typedef std::deque<transaction_t *> transactions_deque;

  transactions_deque transactions;
  const value_expr   sort_order;

  sort_transactions();

public:
  sort_transactions(xact_handler_ptr handler,
		    const value_expr& _sort_order)
    : item_handler<transaction_t>(handler),
      sort_order(_sort_order) {
    TRACE_CTOR(sort_transactions,
	       "xact_handler_ptr, const value_expr&");
  }
  sort_transactions(xact_handler_ptr handler,
		    const string& _sort_order)
    : item_handler<transaction_t>(handler),
      sort_order(_sort_order) {
    TRACE_CTOR(sort_transactions,
	       "xact_handler_ptr, const string&");
  }
  virtual ~sort_transactions() {
    TRACE_DTOR(sort_transactions);
  }

  virtual void post_accumulated_xacts();

  virtual void flush() {
    post_accumulated_xacts();
    item_handler<transaction_t>::flush();
  }

  virtual void operator()(transaction_t& xact) {
    transactions.push_back(&xact);
  }
};

class sort_entries : public item_handler<transaction_t>
{
  sort_transactions sorter;
  entry_t *	    last_entry;

  sort_entries();

public:
  sort_entries(xact_handler_ptr handler,
	       const value_expr& _sort_order)
    : sorter(handler, _sort_order) {
    TRACE_CTOR(sort_entries,
	       "xact_handler_ptr, const value_expr&");
  }
  sort_entries(xact_handler_ptr handler,
	       const string& _sort_order)
    : sorter(handler, _sort_order) {
    TRACE_CTOR(sort_entries,
	       "xact_handler_ptr, const string&");
  }
  virtual ~sort_entries() {
    TRACE_DTOR(sort_entries);
  }

  virtual void flush() {
    sorter.flush();
    item_handler<transaction_t>::flush();
  }

  virtual void operator()(transaction_t& xact) {
    if (last_entry && xact.entry != last_entry)
      sorter.post_accumulated_xacts();

    sorter(xact);

    last_entry = xact.entry;
  }
};

class filter_transactions : public item_handler<transaction_t>
{
  item_predicate<transaction_t> pred;

  filter_transactions();

public:
  filter_transactions(xact_handler_ptr handler,
		      const value_expr& predicate)
    : item_handler<transaction_t>(handler), pred(predicate) {
    TRACE_CTOR(filter_transactions,
	       "xact_handler_ptr, const value_expr&");
  }

  filter_transactions(xact_handler_ptr handler,
		      const string& predicate)
    : item_handler<transaction_t>(handler), pred(predicate) {
    TRACE_CTOR(filter_transactions,
	       "xact_handler_ptr, const string&");
  }
  virtual ~filter_transactions() {
    TRACE_DTOR(filter_transactions);
  }

  virtual void operator()(transaction_t& xact) {
    if (pred(xact)) {
      transaction_xdata(xact).dflags |= TRANSACTION_MATCHES;
      (*handler)(xact);
    }
  }
};

class calc_transactions : public item_handler<transaction_t>
{
  transaction_t * last_xact;

  calc_transactions();

public:
  calc_transactions(xact_handler_ptr handler)
    : item_handler<transaction_t>(handler), last_xact(NULL) {
    TRACE_CTOR(calc_transactions, "xact_handler_ptr");
  }
  virtual ~calc_transactions() {
    TRACE_DTOR(calc_transactions);
  }

  virtual void operator()(transaction_t& xact);
};

class invert_transactions : public item_handler<transaction_t>
{
  invert_transactions();

public:
  invert_transactions(xact_handler_ptr handler)
    : item_handler<transaction_t>(handler) {}

  virtual void operator()(transaction_t& xact);
};

inline void clear_entries_transactions(std::list<entry_t>& entries_list) {
  for (std::list<entry_t>::iterator i = entries_list.begin();
       i != entries_list.end();
       i++)
    (*i).transactions.clear();
}

class collapse_transactions : public item_handler<transaction_t>
{
  value_t	  subtotal;
  unsigned int    count;
  entry_t *       last_entry;
  transaction_t * last_xact;
  account_t       totals_account;

  std::list<entry_t>       entry_temps;
  std::list<transaction_t> xact_temps;

  collapse_transactions();

public:
  collapse_transactions(xact_handler_ptr handler)
    : item_handler<transaction_t>(handler), count(0),
      last_entry(NULL), last_xact(NULL),
      totals_account(NULL, "<Total>") {
    TRACE_CTOR(collapse_transactions, "xact_handler_ptr");
  }
  virtual ~collapse_transactions() {
    TRACE_DTOR(collapse_transactions);
    clear_entries_transactions(entry_temps);
  }

  virtual void flush() {
    if (subtotal)
      report_subtotal();
    item_handler<transaction_t>::flush();
  }

  void report_subtotal();

  virtual void operator()(transaction_t& xact);
};

class component_transactions : public item_handler<transaction_t>
{
  item_predicate<transaction_t> pred;

  component_transactions();

public:
  component_transactions(xact_handler_ptr handler,
			 const value_expr& predicate)
    : item_handler<transaction_t>(handler), pred(predicate) {
    TRACE_CTOR(component_transactions,
	       "xact_handler_ptr, const value_expr&");
  }
  component_transactions(xact_handler_ptr handler,
			 const string& predicate)
    : item_handler<transaction_t>(handler), pred(predicate) {
    TRACE_CTOR(component_transactions,
	       "xact_handler_ptr, const string&");
  }
  virtual ~component_transactions() throw() {
    TRACE_DTOR(component_transactions);
  }

  virtual void operator()(transaction_t& xact);
};

class related_transactions : public item_handler<transaction_t>
{
  transactions_list transactions;
  bool		    also_matching;

  related_transactions();

public:
  related_transactions(xact_handler_ptr handler,
		       const bool _also_matching = false)
    : item_handler<transaction_t>(handler),
      also_matching(_also_matching) {
    TRACE_CTOR(related_transactions,
	       "xact_handler_ptr, const bool");
  }
  virtual ~related_transactions() throw() {
    TRACE_DTOR(related_transactions);
  }

  virtual void flush();
  virtual void operator()(transaction_t& xact) {
    transaction_xdata(xact).dflags |= TRANSACTION_RECEIVED;
    transactions.push_back(&xact);
  }
};

class changed_value_transactions : public item_handler<transaction_t>
{
  // This filter requires that calc_transactions be used at some point
  // later in the chain.

  bool		  changed_values_only;
  transaction_t * last_xact;
  value_t         last_balance;

  std::list<entry_t>       entry_temps;
  std::list<transaction_t> xact_temps;

  changed_value_transactions();

public:
  changed_value_transactions(xact_handler_ptr handler,
			     bool _changed_values_only)
    : item_handler<transaction_t>(handler),
      changed_values_only(_changed_values_only), last_xact(NULL) {
    TRACE_CTOR(changed_value_transactions,
	       "xact_handler_ptr, bool");
  }
  virtual ~changed_value_transactions() {
    TRACE_DTOR(changed_value_transactions);
    clear_entries_transactions(entry_temps);
  }

  virtual void flush() {
    if (last_xact) {
      output_diff(current_moment);
      last_xact = NULL;
    }
    item_handler<transaction_t>::flush();
  }

  void output_diff(const datetime_t& current);

  virtual void operator()(transaction_t& xact);
};

class subtotal_transactions : public item_handler<transaction_t>
{
  class acct_value_t
  {
    acct_value_t();

  public:
    account_t *	account;
    value_t	value;

    transactions_list components;

    acct_value_t(account_t * a) : account(a) {
      TRACE_CTOR(acct_value_t, "acount_t *");
    }
    acct_value_t(account_t * a, value_t& v) : account(a), value(v) {
      TRACE_CTOR(acct_value_t, "acount_t *, value_t&");
    }
    acct_value_t(const acct_value_t& av)
      : account(av.account), value(av.value),
	components(av.components) {
      TRACE_CTOR(acct_value_t, "copy");
    }
    ~acct_value_t() throw() {
      TRACE_DTOR(acct_value_t);
    }
  };

  typedef std::map<string, acct_value_t>  values_map;
  typedef std::pair<string, acct_value_t> values_pair;

  subtotal_transactions();

protected:
  values_map values;
  bool       remember_components;

  std::list<entry_t>       entry_temps;
  std::list<transaction_t> xact_temps;

public:
  datetime_t start;
  datetime_t finish;

  subtotal_transactions(xact_handler_ptr handler,
			bool _remember_components = false)
    : item_handler<transaction_t>(handler),
      remember_components(_remember_components) {
    TRACE_CTOR(subtotal_transactions,
	       "xact_handler_ptr, bool");
  }
  virtual ~subtotal_transactions() {
    TRACE_DTOR(subtotal_transactions);
    clear_entries_transactions(entry_temps);
  }

  void report_subtotal(const char * spec_fmt = NULL);

  virtual void flush() {
    if (values.size() > 0)
      report_subtotal();
    item_handler<transaction_t>::flush();
  }
  virtual void operator()(transaction_t& xact);
};

class interval_expr_error : public error {
 public:
  interval_expr_error(const string& reason,
		      error_context * ctxt = NULL) throw()
    : error(reason, ctxt) {}
  virtual ~interval_expr_error() throw() {}
};

class interval_transactions : public subtotal_transactions
{
  interval_t      interval;
  transaction_t * last_xact;
  bool            started;

  interval_transactions();

public:
  interval_transactions(xact_handler_ptr _handler,
			const interval_t& _interval,
			bool remember_components = false)
    : subtotal_transactions(_handler, remember_components),
      interval(_interval), last_xact(NULL), started(false) {
    TRACE_CTOR(interval_transactions,
	       "xact_handler_ptr, const interval_t&, bool");
  }
  interval_transactions(xact_handler_ptr _handler,
			const string& _interval,
			bool remember_components = false)
    : subtotal_transactions(_handler, remember_components),
      interval(_interval), last_xact(NULL), started(false) {
    TRACE_CTOR(interval_transactions,
	       "xact_handler_ptr, const string&, bool");
  }
  virtual ~interval_transactions() throw() {
    TRACE_DTOR(interval_transactions);
  }

  void report_subtotal(const datetime_t& moment = datetime_t());

  virtual void flush() {
    if (last_xact)
      report_subtotal();
    subtotal_transactions::flush();
  }
  virtual void operator()(transaction_t& xact);
};

class by_payee_transactions : public item_handler<transaction_t>
{
  typedef std::map<string, subtotal_transactions *>  payee_subtotals_map;
  typedef std::pair<string, subtotal_transactions *> payee_subtotals_pair;

  payee_subtotals_map payee_subtotals;
  bool		      remember_components;

  by_payee_transactions();

 public:
  by_payee_transactions(xact_handler_ptr handler,
			bool _remember_components = false)
    : item_handler<transaction_t>(handler),
      remember_components(_remember_components) {
    TRACE_CTOR(by_payee_transactions,
	       "xact_handler_ptr, bool");
  }
  virtual ~by_payee_transactions();

  virtual void flush();
  virtual void operator()(transaction_t& xact);
};

class set_comm_as_payee : public item_handler<transaction_t>
{
  std::list<entry_t>       entry_temps;
  std::list<transaction_t> xact_temps;

  set_comm_as_payee();

public:
  set_comm_as_payee(xact_handler_ptr handler)
    : item_handler<transaction_t>(handler) {
    TRACE_CTOR(set_comm_as_payee, "xact_handler_ptr");
  }
  virtual ~set_comm_as_payee() {
    TRACE_DTOR(set_comm_as_payee);
    clear_entries_transactions(entry_temps);
  }

  virtual void operator()(transaction_t& xact);
};

class set_code_as_payee : public item_handler<transaction_t>
{
  std::list<entry_t>       entry_temps;
  std::list<transaction_t> xact_temps;

  set_code_as_payee();

public:
  set_code_as_payee(xact_handler_ptr handler)
    : item_handler<transaction_t>(handler) {
    TRACE_CTOR(set_code_as_payee, "xact_handler_ptr");
  }
  virtual ~set_code_as_payee() {
    TRACE_DTOR(set_code_as_payee);
    clear_entries_transactions(entry_temps);
  }

  virtual void operator()(transaction_t& xact);
};

class dow_transactions : public subtotal_transactions
{
  transactions_list days_of_the_week[7];

  dow_transactions();

public:
  dow_transactions(xact_handler_ptr handler,
		   bool remember_components = false)
    : subtotal_transactions(handler, remember_components) {
    TRACE_CTOR(dow_transactions, "xact_handler_ptr, bool");
  }
  virtual ~dow_transactions() throw() {
    TRACE_DTOR(dow_transactions);
  }

  virtual void flush();
  virtual void operator()(transaction_t& xact) {
    days_of_the_week[xact.date().date().day_of_week()].push_back(&xact);
  }
};

class generate_transactions : public item_handler<transaction_t>
{
  generate_transactions();

protected:
  typedef std::pair<interval_t, transaction_t *> pending_xacts_pair;
  typedef std::list<pending_xacts_pair>          pending_xacts_list;

  pending_xacts_list	   pending_xacts;
  std::list<entry_t>	   entry_temps;
  std::list<transaction_t> xact_temps;

public:
  generate_transactions(xact_handler_ptr handler)
    : item_handler<transaction_t>(handler) {
    TRACE_CTOR(dow_transactions, "xact_handler_ptr");
  }

  virtual ~generate_transactions() {
    TRACE_DTOR(generate_transactions);
    clear_entries_transactions(entry_temps);
  }

  void add_period_entries(period_entries_list& period_entries);

  virtual void add_transaction(const interval_t& period, transaction_t& xact);
};

#define BUDGET_NO_BUDGET  0x00
#define BUDGET_BUDGETED   0x01
#define BUDGET_UNBUDGETED 0x02

class budget_transactions : public generate_transactions
{
  unsigned short flags;

  budget_transactions();

public:
  budget_transactions(xact_handler_ptr handler,
		      unsigned long _flags = BUDGET_BUDGETED)
    : generate_transactions(handler), flags(_flags) {
    TRACE_CTOR(budget_transactions,
	       "xact_handler_ptr, unsigned long");
  }
  virtual ~budget_transactions() throw() {
    TRACE_DTOR(budget_transactions);
  }

  void report_budget_items(const datetime_t& moment);

  virtual void operator()(transaction_t& xact);
};

class forecast_transactions : public generate_transactions
{
  item_predicate<transaction_t> pred;

 public:
  forecast_transactions(xact_handler_ptr handler,
			const value_expr& predicate)
    : generate_transactions(handler), pred(predicate) {
    TRACE_CTOR(forecast_transactions, "xact_handler_ptr, const value_expr&");
  }
  forecast_transactions(xact_handler_ptr handler,
			const string& predicate)
    : generate_transactions(handler), pred(predicate) {
    TRACE_CTOR(forecast_transactions, "xact_handler_ptr, const string&");
  }
  virtual ~forecast_transactions() throw() {
    TRACE_DTOR(forecast_transactions);
  }

  virtual void add_transaction(const interval_t& period,
			       transaction_t&	 xact);
  virtual void flush();
};


//////////////////////////////////////////////////////////////////////
//
// Account walking functions
//

#define ACCOUNT_TO_DISPLAY	 0x0001
#define ACCOUNT_DISPLAYED	 0x0002
#define ACCOUNT_SORT_CALC	 0x0004
#define ACCOUNT_HAS_NON_VIRTUALS 0x0008
#define ACCOUNT_HAS_UNB_VIRTUALS 0x0010

struct account_xdata_t : public noncopyable
{
  value_t	 value;
  value_t	 total;
  value_t	 sort_value;
  unsigned int   count;		// transactions counted toward amount
  unsigned int   total_count;	// transactions counted toward total
  unsigned int   virtuals;
  unsigned short dflags;

  account_xdata_t() : count(0), total_count(0), virtuals(0), dflags(0) {
    TRACE_CTOR(account_xdata_t, "");
  }
  ~account_xdata_t() throw() {
    TRACE_DTOR(account_xdata_t);
  }
};

inline bool account_has_xdata(const account_t& account) {
  return account.data != NULL;
}

inline account_xdata_t& account_xdata_(const account_t& account) {
  return *((account_xdata_t *) account.data);
}

account_xdata_t& account_xdata(const account_t& account);

void sum_accounts(account_t& account);

//////////////////////////////////////////////////////////////////////

class accounts_iterator : public noncopyable
{
  std::list<accounts_map::const_iterator> accounts_i;
  std::list<accounts_map::const_iterator> accounts_end;

public:
  accounts_iterator() {
    TRACE_CTOR(accounts_iterator, "");
  }
  accounts_iterator(account_t& account) {
    TRACE_CTOR(accounts_iterator, "account_t&");
    push_back(account);
  }
  virtual ~accounts_iterator() throw() {
    TRACE_DTOR(accounts_iterator);
  }

  void push_back(account_t& account) {
    accounts_i.push_back(account.accounts.begin());
    accounts_end.push_back(account.accounts.end());
  }

  virtual account_t * operator()();
};

class sorted_accounts_iterator : public noncopyable
{
  value_expr sort_cmp;

  typedef std::deque<account_t *> accounts_deque_t;

  std::list<accounts_deque_t>		      accounts_list;
  std::list<accounts_deque_t::const_iterator> sorted_accounts_i;
  std::list<accounts_deque_t::const_iterator> sorted_accounts_end;

public:
  sorted_accounts_iterator(const string& sort_order) {
    TRACE_CTOR(sorted_accounts_iterator, "const string&");
    sort_cmp = value_expr(sort_order);
  }
  sorted_accounts_iterator(account_t& account, const string& sort_order) {
    TRACE_CTOR(sorted_accounts_iterator, "account_t&, const string&");
    sort_cmp = value_expr(sort_order);
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

//////////////////////////////////////////////////////////////////////

typedef shared_ptr<item_handler<account_t> > acct_handler_ptr;

class clear_account_xdata : public item_handler<account_t>
{
public:
  virtual void operator()(account_t& acct) {
    if (acct.data) {
      delete (account_xdata_t *) acct.data;
      acct.data = NULL;
    }
  }
};

template <typename Iterator>
class pass_down_accounts : public item_handler<account_t>
{
  pass_down_accounts();

public:
  pass_down_accounts(acct_handler_ptr handler, Iterator& iter)
    : item_handler<account_t>(handler) {
    TRACE_CTOR(pass_down_accounts,
	       "acct_handler_ptr, accounts_iterator");
    for (account_t * account = iter(); account; account = iter())
      item_handler<account_t>::operator()(*account);
  }

  virtual ~pass_down_accounts() {
    TRACE_DTOR(pass_down_accounts);
  }
};

//////////////////////////////////////////////////////////////////////

#if 0
inline void clear_journal_xdata(journal_t& journal) {
  clear_transaction_xdata xact_cleaner;
  walk_entries(journal.entries, xact_cleaner);

  clear_account_xdata acct_cleaner;
  walk_accounts(*journal.master, acct_cleaner);
}
#endif

//////////////////////////////////////////////////////////////////////

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

#endif // _WALK_H
