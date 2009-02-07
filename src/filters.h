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
 * @addtogroup report
 */

/**
 * @file   filters.h
 * @author John Wiegley
 *
 * @ingroup report
 *
 * @brief Brief
 *
 * Long.
 */
#ifndef _FILTERS_H
#define _FILTERS_H

#include "chain.h"
#include "predicate.h"
#include "entry.h"
#include "xact.h"
#include "account.h"

namespace ledger {

//////////////////////////////////////////////////////////////////////
//
// Transaction filters
//

/**
 * @brief Brief
 *
 * Long.
 */
class ignore_xacts : public item_handler<xact_t>
{
public:
  virtual void operator()(xact_t&) {}
};

/**
 * @brief Brief
 *
 * Long.
 */
class clear_xact_xdata : public item_handler<xact_t>
{
public:
  virtual void operator()(xact_t& xact) {
    xact.clear_xdata();
  }
};

class xacts_iterator;

/**
 * @brief Brief
 *
 * Long.
 */
class pass_down_xacts : public item_handler<xact_t>
{
  pass_down_xacts();

public:
  pass_down_xacts(xact_handler_ptr handler, xacts_iterator& iter);

  virtual ~pass_down_xacts() {
    TRACE_DTOR(pass_down_xacts);
  }
};

/**
 * @brief Brief
 *
 * Long.
 */
class push_to_xacts_list : public item_handler<xact_t>
{
  push_to_xacts_list();

public:
  xacts_list& xacts;

  push_to_xacts_list(xacts_list& _xacts) : xacts(_xacts) {
    TRACE_CTOR(push_to_xacts_list, "xacts_list&");
  }
  virtual ~push_to_xacts_list() {
    TRACE_DTOR(push_to_xacts_list);
  }

  virtual void operator()(xact_t& xact) {
    xacts.push_back(&xact);
  }
};

/**
 * @brief Brief
 *
 * Long.
 */
class truncate_entries : public item_handler<xact_t>
{
  int head_count;
  int tail_count;

  xacts_list xacts;

  truncate_entries();

public:
  truncate_entries(xact_handler_ptr handler,
		   int _head_count, int _tail_count)
    : item_handler<xact_t>(handler),
      head_count(_head_count), tail_count(_tail_count) {
    TRACE_CTOR(truncate_entries, "xact_handler_ptr, int, int");
  }
  virtual ~truncate_entries() {
    TRACE_DTOR(truncate_entries);
  }

  virtual void flush();

  virtual void operator()(xact_t& xact) {
    if (! (tail_count == 0 && head_count > 0 &&
	   static_cast<int>(xacts.size()) >= head_count))
      xacts.push_back(&xact);
  }
};

/**
 * @brief Brief
 *
 * Long.
 */
class set_account_value : public item_handler<xact_t>
{
public:
  set_account_value(xact_handler_ptr handler = xact_handler_ptr())
    : item_handler<xact_t>(handler) {}

  virtual void operator()(xact_t& xact);
};

/**
 * @brief Brief
 *
 * Long.
 */
class sort_xacts : public item_handler<xact_t>
{
  typedef std::deque<xact_t *> xacts_deque;

  xacts_deque  xacts;
  const expr_t sort_order;

  sort_xacts();

public:
  sort_xacts(xact_handler_ptr handler,
		    const expr_t&    _sort_order)
    : item_handler<xact_t>(handler),
      sort_order(_sort_order) {
    TRACE_CTOR(sort_xacts,
	       "xact_handler_ptr, const value_expr&");
  }
  sort_xacts(xact_handler_ptr handler,
		    const string& _sort_order)
    : item_handler<xact_t>(handler),
      sort_order(_sort_order) {
    TRACE_CTOR(sort_xacts,
	       "xact_handler_ptr, const string&");
  }
  virtual ~sort_xacts() {
    TRACE_DTOR(sort_xacts);
  }

  virtual void post_accumulated_xacts();

  virtual void flush() {
    post_accumulated_xacts();
    item_handler<xact_t>::flush();
  }

  virtual void operator()(xact_t& xact) {
    xacts.push_back(&xact);
  }
};

/**
 * @brief Brief
 *
 * Long.
 */
class sort_entries : public item_handler<xact_t>
{
  sort_xacts sorter;
  entry_t *  last_entry;

  sort_entries();

public:
  sort_entries(xact_handler_ptr handler,
	       const expr_t&    _sort_order)
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
    item_handler<xact_t>::flush();
  }

  virtual void operator()(xact_t& xact) {
    if (last_entry && xact.entry != last_entry)
      sorter.post_accumulated_xacts();

    sorter(xact);

    last_entry = xact.entry;
  }
};

/**
 * @brief Brief
 *
 * Long.
 */
class filter_xacts : public item_handler<xact_t>
{
  item_predicate<xact_t> pred;

  filter_xacts();

public:
  filter_xacts(xact_handler_ptr		     handler,
	       const item_predicate<xact_t>& predicate)
    : item_handler<xact_t>(handler), pred(predicate) {
    TRACE_CTOR(filter_xacts,
	       "xact_handler_ptr, const item_predicate<xact_t>&");
  }
  virtual ~filter_xacts() {
    TRACE_DTOR(filter_xacts);
  }

  virtual void operator()(xact_t& xact) {
    if (pred(xact)) {
      xact.xdata().add_flags(XACT_EXT_MATCHES);
      (*handler)(xact);
    }
  }
};

/**
 * @brief Brief
 *
 * Long.
 */
class anonymize_xacts : public item_handler<xact_t>
{
  std::list<entry_t> entry_temps;
  std::list<xact_t>  xact_temps;

  entry_t * last_entry;

  anonymize_xacts();

public:
  anonymize_xacts(xact_handler_ptr handler)
    : item_handler<xact_t>(handler), last_entry(NULL) {
    TRACE_CTOR(anonymize_xacts, "xact_handler_ptr");
  }
  virtual ~anonymize_xacts() {
    TRACE_DTOR(anonymize_xacts);
  }

  virtual void operator()(xact_t& xact);
};

/**
 * @brief Brief
 *
 * Long.
 */
class calc_xacts : public item_handler<xact_t>
{
  xact_t * last_xact;

  calc_xacts();

public:
  calc_xacts(xact_handler_ptr handler)
    : item_handler<xact_t>(handler), last_xact(NULL) {
    TRACE_CTOR(calc_xacts, "xact_handler_ptr");
  }
  virtual ~calc_xacts() {
    TRACE_DTOR(calc_xacts);
  }

  virtual void operator()(xact_t& xact);
};

/**
 * @brief Brief
 *
 * Long.
 */
class invert_xacts : public item_handler<xact_t>
{
  invert_xacts();

public:
  invert_xacts(xact_handler_ptr handler)
    : item_handler<xact_t>(handler) {}

  virtual void operator()(xact_t& xact);
};

inline void clear_entries_xacts(std::list<entry_t>& entries_list) {
  foreach (entry_t& entry, entries_list)
    entry.xacts.clear();
}

/**
 * @brief Brief
 *
 * Long.
 */
class collapse_xacts : public item_handler<xact_t>
{
  value_t     subtotal;
  std::size_t count;
  entry_t *   last_entry;
  xact_t *    last_xact;
  account_t   totals_account;

  std::list<entry_t> entry_temps;
  std::list<xact_t>  xact_temps;

  collapse_xacts();

public:
  collapse_xacts(xact_handler_ptr handler, session_t& session)
    : item_handler<xact_t>(handler), count(0),
      last_entry(NULL), last_xact(NULL),
      totals_account(NULL, "<Total>") {
    TRACE_CTOR(collapse_xacts, "xact_handler_ptr");
  }
  virtual ~collapse_xacts() {
    TRACE_DTOR(collapse_xacts);
    clear_entries_xacts(entry_temps);
  }

  virtual void flush() {
    report_subtotal();
    item_handler<xact_t>::flush();
  }

  void report_subtotal();

  virtual void operator()(xact_t& xact);
};

/**
 * @brief Brief
 *
 * Long.
 */
class related_xacts : public item_handler<xact_t>
{
  xacts_list xacts;
  bool	     also_matching;

  related_xacts();

public:
  related_xacts(xact_handler_ptr handler,
		       const bool _also_matching = false)
    : item_handler<xact_t>(handler),
      also_matching(_also_matching) {
    TRACE_CTOR(related_xacts,
	       "xact_handler_ptr, const bool");
  }
  virtual ~related_xacts() throw() {
    TRACE_DTOR(related_xacts);
  }

  virtual void flush();
  virtual void operator()(xact_t& xact) {
    xact.xdata().add_flags(XACT_EXT_RECEIVED);
    xacts.push_back(&xact);
  }
};

/**
 * @brief Brief
 *
 * Long.
 */
class changed_value_xacts : public item_handler<xact_t>
{
  // This filter requires that calc_xacts be used at some point
  // later in the chain.

  expr_t   total_expr;
  bool	   changed_values_only;
  xact_t * last_xact;
  value_t  last_balance;

  std::list<entry_t> entry_temps;
  std::list<xact_t>  xact_temps;

  changed_value_xacts();

public:
  changed_value_xacts(xact_handler_ptr handler,
		      const expr_t&    _total_expr,
		      bool	       _changed_values_only)
    : item_handler<xact_t>(handler), total_expr(_total_expr),
      changed_values_only(_changed_values_only), last_xact(NULL) {
    TRACE_CTOR(changed_value_xacts,
	       "xact_handler_ptr, bool");
  }
  virtual ~changed_value_xacts() {
    TRACE_DTOR(changed_value_xacts);
    clear_entries_xacts(entry_temps);
  }

  virtual void flush() {
    if (last_xact) {
      output_diff(CURRENT_DATE());
      last_xact = NULL;
    }
    item_handler<xact_t>::flush();
  }

  void output_diff(const date_t& current);

  virtual void operator()(xact_t& xact);
};

/**
 * @brief Brief
 *
 * Long.
 */
class subtotal_xacts : public item_handler<xact_t>
{
  class acct_value_t
  {
    acct_value_t();

  public:
    account_t *	account;
    value_t	value;

    acct_value_t(account_t * a) : account(a) {
      TRACE_CTOR(acct_value_t, "acount_t *");
    }
    acct_value_t(account_t * a, value_t& v) : account(a), value(v) {
      TRACE_CTOR(acct_value_t, "acount_t *, value_t&");
    }
    acct_value_t(const acct_value_t& av)
      : account(av.account), value(av.value) {
      TRACE_CTOR(acct_value_t, "copy");
    }
    ~acct_value_t() throw() {
      TRACE_DTOR(acct_value_t);
    }
  };

  typedef std::map<string, acct_value_t>  values_map;
  typedef std::pair<string, acct_value_t> values_pair;

  subtotal_xacts();

protected:
  values_map	     values;
  optional<string>   date_format;
  std::list<entry_t> entry_temps;
  std::list<xact_t>  xact_temps;

public:
  date_t start;
  date_t finish;

  subtotal_xacts(xact_handler_ptr handler,
		 optional<string> _date_format = none)
    : item_handler<xact_t>(handler), date_format(_date_format) {
    TRACE_CTOR(subtotal_xacts,
	       "xact_handler_ptr, bool");
  }
  virtual ~subtotal_xacts() {
    TRACE_DTOR(subtotal_xacts);
    clear_entries_xacts(entry_temps);
  }

  void report_subtotal(const char * spec_fmt = NULL);

  virtual void flush() {
    if (values.size() > 0)
      report_subtotal();
    item_handler<xact_t>::flush();
  }
  virtual void operator()(xact_t& xact);
};

/**
 * @brief Brief
 *
 * Long.
 */
class interval_xacts : public subtotal_xacts
{
  interval_t      interval;
  xact_t * last_xact;
  bool            started;

  interval_xacts();

public:
  interval_xacts(xact_handler_ptr  _handler,
		 const interval_t& _interval)
    : subtotal_xacts(_handler), interval(_interval),
      last_xact(NULL), started(false) {
    TRACE_CTOR(interval_xacts,
	       "xact_handler_ptr, const interval_t&, bool");
  }
  interval_xacts(xact_handler_ptr _handler,
		 const string&	  _interval)
    : subtotal_xacts(_handler), interval(_interval),
      last_xact(NULL), started(false) {
    TRACE_CTOR(interval_xacts,
	       "xact_handler_ptr, const string&, bool");
  }
  virtual ~interval_xacts() throw() {
    TRACE_DTOR(interval_xacts);
  }

  void report_subtotal(const date_t& moment = date_t());

  virtual void flush() {
    if (last_xact)
      report_subtotal();
    subtotal_xacts::flush();
  }
  virtual void operator()(xact_t& xact);
};

/**
 * @brief Brief
 *
 * Long.
 */
class by_payee_xacts : public item_handler<xact_t>
{
  typedef std::map<string, subtotal_xacts *>  payee_subtotals_map;
  typedef std::pair<string, subtotal_xacts *> payee_subtotals_pair;

  payee_subtotals_map payee_subtotals;

  by_payee_xacts();

 public:
  by_payee_xacts(xact_handler_ptr handler)
    : item_handler<xact_t>(handler) {
    TRACE_CTOR(by_payee_xacts,
	       "xact_handler_ptr, bool");
  }
  virtual ~by_payee_xacts();

  virtual void flush();
  virtual void operator()(xact_t& xact);
};

/**
 * @brief Brief
 *
 * Long.
 */
class set_comm_as_payee : public item_handler<xact_t>
{
  std::list<entry_t> entry_temps;
  std::list<xact_t>  xact_temps;

  set_comm_as_payee();

public:
  set_comm_as_payee(xact_handler_ptr handler)
    : item_handler<xact_t>(handler) {
    TRACE_CTOR(set_comm_as_payee, "xact_handler_ptr");
  }
  virtual ~set_comm_as_payee() {
    TRACE_DTOR(set_comm_as_payee);
    clear_entries_xacts(entry_temps);
  }

  virtual void operator()(xact_t& xact);
};

/**
 * @brief Brief
 *
 * Long.
 */
class set_code_as_payee : public item_handler<xact_t>
{
  std::list<entry_t> entry_temps;
  std::list<xact_t>  xact_temps;

  set_code_as_payee();

public:
  set_code_as_payee(xact_handler_ptr handler)
    : item_handler<xact_t>(handler) {
    TRACE_CTOR(set_code_as_payee, "xact_handler_ptr");
  }
  virtual ~set_code_as_payee() {
    TRACE_DTOR(set_code_as_payee);
    clear_entries_xacts(entry_temps);
  }

  virtual void operator()(xact_t& xact);
};

/**
 * @brief Brief
 *
 * Long.
 */
class dow_xacts : public subtotal_xacts
{
  xacts_list days_of_the_week[7];

  dow_xacts();

public:
  dow_xacts(xact_handler_ptr handler)
    : subtotal_xacts(handler) {
    TRACE_CTOR(dow_xacts, "xact_handler_ptr, bool");
  }
  virtual ~dow_xacts() throw() {
    TRACE_DTOR(dow_xacts);
  }

  virtual void flush();
  virtual void operator()(xact_t& xact) {
    days_of_the_week[xact.date().day_of_week()].push_back(&xact);
  }
};

/**
 * @brief Brief
 *
 * Long.
 */
class generate_xacts : public item_handler<xact_t>
{
  generate_xacts();

protected:
  typedef std::pair<interval_t, xact_t *> pending_xacts_pair;
  typedef std::list<pending_xacts_pair>          pending_xacts_list;

  pending_xacts_list pending_xacts;
  std::list<entry_t> entry_temps;
  std::list<xact_t>  xact_temps;

public:
  generate_xacts(xact_handler_ptr handler)
    : item_handler<xact_t>(handler) {
    TRACE_CTOR(dow_xacts, "xact_handler_ptr");
  }

  virtual ~generate_xacts() {
    TRACE_DTOR(generate_xacts);
    clear_entries_xacts(entry_temps);
  }

  void add_period_entries(period_entries_list& period_entries);

  virtual void add_xact(const interval_t& period, xact_t& xact);
};

/**
 * @brief Brief
 *
 * Long.
 */
class budget_xacts : public generate_xacts
{
#define BUDGET_NO_BUDGET  0x00
#define BUDGET_BUDGETED   0x01
#define BUDGET_UNBUDGETED 0x02

  uint_least8_t flags;

  budget_xacts();

public:
  budget_xacts(xact_handler_ptr handler,
	       uint_least8_t _flags = BUDGET_BUDGETED)
    : generate_xacts(handler), flags(_flags) {
    TRACE_CTOR(budget_xacts, "xact_handler_ptr, uint_least8_t");
  }
  virtual ~budget_xacts() throw() {
    TRACE_DTOR(budget_xacts);
  }

  void report_budget_items(const date_t& date);

  virtual void operator()(xact_t& xact);
};

/**
 * @brief Brief
 *
 * Long.
 */
class forecast_xacts : public generate_xacts
{
  item_predicate<xact_t> pred;

 public:
  forecast_xacts(xact_handler_ptr	       handler,
		 const item_predicate<xact_t>& predicate)
    : generate_xacts(handler), pred(predicate) {
    TRACE_CTOR(forecast_xacts,
	       "xact_handler_ptr, const item_predicate<xact_t>&");
  }
  virtual ~forecast_xacts() throw() {
    TRACE_DTOR(forecast_xacts);
  }

  virtual void add_xact(const interval_t& period,
			       xact_t&	 xact);
  virtual void flush();
};

//////////////////////////////////////////////////////////////////////
//
// Account filters
//

/**
 * @brief Brief
 *
 * Long.
 */
class clear_account_xdata : public item_handler<account_t>
{
public:
  virtual void operator()(account_t& acct) {
    acct.clear_xdata();
  }
};

class accounts_iterator;

/**
 * @brief Brief
 *
 * Long.
 */
class pass_down_accounts : public item_handler<account_t>
{
  pass_down_accounts();

  optional<item_predicate<account_t> > pred;

public:
  pass_down_accounts(acct_handler_ptr	handler,
		     accounts_iterator&	iter,
		     const optional<item_predicate<account_t> >& predicate = none);

  virtual ~pass_down_accounts() {
    TRACE_DTOR(pass_down_accounts);
  }
};

} // namespace ledger

#endif // _FILTERS_H
