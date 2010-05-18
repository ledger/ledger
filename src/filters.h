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
 * @addtogroup report
 */

/**
 * @file   filters.h
 * @author John Wiegley
 *
 * @ingroup report
 */
#ifndef _FILTERS_H
#define _FILTERS_H

#include "chain.h"
#include "xact.h"
#include "post.h"
#include "account.h"
#include "temps.h"

namespace ledger {

//////////////////////////////////////////////////////////////////////
//
// Posting filters
//

class ignore_posts : public item_handler<post_t>
{
public:
  virtual void operator()(post_t&) {}
};

class collect_posts : public item_handler<post_t>
{
public:
  std::vector<post_t *> posts;

  collect_posts() : item_handler<post_t>() {
    TRACE_CTOR(collect_posts, "");
  }
  virtual ~collect_posts() {
    TRACE_DTOR(collect_posts);
  }

  std::size_t length() const {
    return posts.size();
  }

  std::vector<post_t *>::iterator begin() {
    return posts.begin();
  }
  std::vector<post_t *>::iterator end() {
    return posts.end();
  }

  virtual void flush() {}
  virtual void operator()(post_t& post) {
    posts.push_back(&post);
  }
};

class posts_iterator;

class pass_down_posts : public item_handler<post_t>
{
  pass_down_posts();

public:
  pass_down_posts(post_handler_ptr handler, posts_iterator& iter);

  virtual ~pass_down_posts() {
    TRACE_DTOR(pass_down_posts);
  }
};

class push_to_posts_list : public item_handler<post_t>
{
  push_to_posts_list();

public:
  posts_list& posts;

  push_to_posts_list(posts_list& _posts) : posts(_posts) {
    TRACE_CTOR(push_to_posts_list, "posts_list&");
  }
  virtual ~push_to_posts_list() {
    TRACE_DTOR(push_to_posts_list);
  }

  virtual void operator()(post_t& post) {
    posts.push_back(&post);
  }
};

class truncate_xacts : public item_handler<post_t>
{
  int  head_count;
  int  tail_count;
  bool completed;

  posts_list  posts;
  std::size_t xacts_seen;
  xact_t *    last_xact;

  truncate_xacts();

public:
  truncate_xacts(post_handler_ptr handler,
		 int _head_count, int _tail_count)
    : item_handler<post_t>(handler),
      head_count(_head_count), tail_count(_tail_count),
      completed(false), xacts_seen(0), last_xact(NULL) {
    TRACE_CTOR(truncate_xacts, "post_handler_ptr, int, int");
  }
  virtual ~truncate_xacts() {
    TRACE_DTOR(truncate_xacts);
  }

  virtual void flush();
  virtual void operator()(post_t& post);
};

class sort_posts : public item_handler<post_t>
{
  typedef std::deque<post_t *> posts_deque;

  posts_deque  posts;
  const expr_t sort_order;

  sort_posts();

public:
  sort_posts(post_handler_ptr handler,
		    const expr_t&    _sort_order)
    : item_handler<post_t>(handler),
      sort_order(_sort_order) {
    TRACE_CTOR(sort_posts,
	       "post_handler_ptr, const value_expr&");
  }
  sort_posts(post_handler_ptr handler,
		    const string& _sort_order)
    : item_handler<post_t>(handler),
      sort_order(_sort_order) {
    TRACE_CTOR(sort_posts,
	       "post_handler_ptr, const string&");
  }
  virtual ~sort_posts() {
    TRACE_DTOR(sort_posts);
  }

  virtual void post_accumulated_posts();

  virtual void flush() {
    post_accumulated_posts();
    item_handler<post_t>::flush();
  }

  virtual void operator()(post_t& post) {
    posts.push_back(&post);
  }
};

class sort_xacts : public item_handler<post_t>
{
  sort_posts sorter;
  xact_t *  last_xact;

  sort_xacts();

public:
  sort_xacts(post_handler_ptr handler,
	       const expr_t&    _sort_order)
    : sorter(handler, _sort_order) {
    TRACE_CTOR(sort_xacts,
	       "post_handler_ptr, const value_expr&");
  }
  sort_xacts(post_handler_ptr handler,
	       const string& _sort_order)
    : sorter(handler, _sort_order) {
    TRACE_CTOR(sort_xacts,
	       "post_handler_ptr, const string&");
  }
  virtual ~sort_xacts() {
    TRACE_DTOR(sort_xacts);
  }

  virtual void flush() {
    sorter.flush();
    item_handler<post_t>::flush();
  }

  virtual void operator()(post_t& post) {
    if (last_xact && post.xact != last_xact)
      sorter.post_accumulated_posts();

    sorter(post);

    last_xact = post.xact;
  }
};

class filter_posts : public item_handler<post_t>
{
  predicate_t pred;
  scope_t&    context;

  filter_posts();

public:
  filter_posts(post_handler_ptr	  handler,
	       const predicate_t& predicate,
	       scope_t&           _context)
    : item_handler<post_t>(handler), pred(predicate), context(_context) {
    TRACE_CTOR(filter_posts, "post_handler_ptr, predicate_t, scope_t&");
  }
  virtual ~filter_posts() {
    TRACE_DTOR(filter_posts);
  }

  virtual void operator()(post_t& post) {
    bind_scope_t bound_scope(context, post);
    if (pred(bound_scope)) {
      post.xdata().add_flags(POST_EXT_MATCHES);
      (*handler)(post);
    }
  }
};

class anonymize_posts : public item_handler<post_t>
{
  temporaries_t temps;
  xact_t *	last_xact;

  anonymize_posts();

public:
  anonymize_posts(post_handler_ptr handler)
    : item_handler<post_t>(handler), last_xact(NULL) {
    TRACE_CTOR(anonymize_posts, "post_handler_ptr");
  }
  virtual ~anonymize_posts() {
    TRACE_DTOR(anonymize_posts);
  }

  virtual void operator()(post_t& post);
};

class calc_posts : public item_handler<post_t>
{
  post_t * last_post;
  expr_t&  amount_expr;
  bool     calc_running_total;

  calc_posts();

public:
  calc_posts(post_handler_ptr handler,
	     expr_t&          _amount_expr,
	     bool             _calc_running_total = false)
    : item_handler<post_t>(handler), last_post(NULL),
      amount_expr(_amount_expr), calc_running_total(_calc_running_total) {
    TRACE_CTOR(calc_posts, "post_handler_ptr, expr_t&, bool");
  }
  virtual ~calc_posts() {
    TRACE_DTOR(calc_posts);
  }

  virtual void operator()(post_t& post);
};

class collapse_posts : public item_handler<post_t>
{
  expr_t&	      amount_expr;
  predicate_t	      display_predicate;
  predicate_t	      only_predicate;
  value_t	      subtotal;
  std::size_t	      count;
  xact_t *	      last_xact;
  post_t *	      last_post;
  temporaries_t       temps;
  account_t&	      totals_account;
  bool		      only_collapse_if_zero;
  std::list<post_t *> component_posts;

  collapse_posts();

public:
  collapse_posts(post_handler_ptr handler,
		 expr_t&	  _amount_expr,
		 predicate_t	  _display_predicate,
		 predicate_t	  _only_predicate,
		 bool             _only_collapse_if_zero = false)
    : item_handler<post_t>(handler), amount_expr(_amount_expr),
      display_predicate(_display_predicate),
      only_predicate(_only_predicate), count(0),
      last_xact(NULL), last_post(NULL),
      totals_account(temps.create_account(_("<Total>"))),
      only_collapse_if_zero(_only_collapse_if_zero) {
    TRACE_CTOR(collapse_posts, "post_handler_ptr");
  }
  virtual ~collapse_posts() {
    TRACE_DTOR(collapse_posts);
  }

  virtual void flush() {
    report_subtotal();
    item_handler<post_t>::flush();
  }

  void report_subtotal();

  virtual void operator()(post_t& post);
};

class related_posts : public item_handler<post_t>
{
  posts_list posts;
  bool	     also_matching;

  related_posts();

public:
  related_posts(post_handler_ptr handler,
		       const bool _also_matching = false)
    : item_handler<post_t>(handler),
      also_matching(_also_matching) {
    TRACE_CTOR(related_posts,
	       "post_handler_ptr, const bool");
  }
  virtual ~related_posts() throw() {
    TRACE_DTOR(related_posts);
  }

  virtual void flush();
  virtual void operator()(post_t& post) {
    post.xdata().add_flags(POST_EXT_RECEIVED);
    posts.push_back(&post);
  }
};

class changed_value_posts : public item_handler<post_t>
{
  // This filter requires that calc_posts be used at some point
  // later in the chain.

  expr_t	display_amount_expr;
  expr_t	total_expr;
  expr_t	display_total_expr;
  report_t&	report;
  bool		changed_values_only;
  bool		for_accounts_report;
  bool		show_unrealized;
  post_t *	last_post;
  value_t	last_total;
  value_t	last_display_total;
  temporaries_t	temps;
  account_t&	revalued_account;
  account_t&	rounding_account;
  account_t *	gains_equity_account;
  account_t *	losses_equity_account;

  changed_value_posts();

public:
  changed_value_posts(post_handler_ptr handler,
		      report_t&	       _report,
		      bool	       _for_accounts_report,
		      bool	       _show_unrealized);

  virtual ~changed_value_posts() {
    TRACE_DTOR(changed_value_posts);
  }

  virtual void flush();

  void output_revaluation(post_t& post, const date_t& current);
  void output_rounding(post_t& post);

  virtual void operator()(post_t& post);
};

class subtotal_posts : public item_handler<post_t>
{
  subtotal_posts();

protected:
  class acct_value_t
  {
    acct_value_t();

  public:
    account_t *	account;
    value_t	value;

    acct_value_t(account_t * a) : account(a) {
      TRACE_CTOR(acct_value_t, "account_t *");
    }
    acct_value_t(account_t * a, value_t& v) : account(a), value(v) {
      TRACE_CTOR(acct_value_t, "account_t *, value_t&");
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

protected:
  expr_t&	      amount_expr;
  values_map	      values;
  optional<string>    date_format;
  temporaries_t       temps;
  std::list<post_t *> component_posts;

public:
  subtotal_posts(post_handler_ptr handler, expr_t& _amount_expr,
		 const optional<string>& _date_format = none)
    : item_handler<post_t>(handler), amount_expr(_amount_expr),
      date_format(_date_format) {
    TRACE_CTOR(subtotal_posts,
	       "post_handler_ptr, expr_t&, const optional<string>&");
  }
  virtual ~subtotal_posts() {
    TRACE_DTOR(subtotal_posts);
  }

  void report_subtotal(const char * spec_fmt = NULL,
		       const optional<date_interval_t>& interval = none);

  virtual void flush() {
    if (values.size() > 0)
      report_subtotal();
    item_handler<post_t>::flush();
  }
  virtual void operator()(post_t& post);
};

class interval_posts : public subtotal_posts
{
  date_interval_t interval;
  date_interval_t last_interval;
  post_t *	  last_post;
  account_t&	  empty_account;
  bool		  exact_periods;
  bool		  generate_empty_posts;

  interval_posts();

public:

  interval_posts(post_handler_ptr	_handler,
		 expr_t&		amount_expr,
		 const date_interval_t& _interval,
		 bool			_exact_periods	      = false,
		 bool                   _generate_empty_posts = false)
    : subtotal_posts(_handler, amount_expr), interval(_interval),
      last_post(NULL), empty_account(temps.create_account(_("<None>"))),
      exact_periods(_exact_periods),
      generate_empty_posts(_generate_empty_posts) {
    TRACE_CTOR(interval_posts,
	       "post_handler_ptr, expr_t&, date_interval_t, bool, bool");
  }
  virtual ~interval_posts() throw() {
    TRACE_DTOR(interval_posts);
  }

  void report_subtotal(const date_interval_t& interval);

  virtual void flush() {
    if (last_post && interval.duration) {
      if (interval.is_valid())
	report_subtotal(interval);
      subtotal_posts::flush();
    }
  }
  virtual void operator()(post_t& post);
};

class posts_as_equity : public subtotal_posts
{
  post_t *    last_post;
  account_t&  equity_account;
  account_t * balance_account;

  posts_as_equity();

public:
  posts_as_equity(post_handler_ptr _handler, expr_t& amount_expr)
    : subtotal_posts(_handler, amount_expr),
      equity_account(temps.create_account(_("Equity"))) {
    TRACE_CTOR(posts_as_equity, "post_handler_ptr, expr_t&");
    balance_account = equity_account.find_account(_("Opening Balances"));
  }
  virtual ~posts_as_equity() throw() {
    TRACE_DTOR(posts_as_equity);
  }

  void report_subtotal();

  virtual void flush() {
    report_subtotal();
    subtotal_posts::flush();
  }
};

class by_payee_posts : public item_handler<post_t>
{
  typedef std::map<string, shared_ptr<subtotal_posts> >  payee_subtotals_map;
  typedef std::pair<string, shared_ptr<subtotal_posts> > payee_subtotals_pair;

  expr_t&	      amount_expr;
  payee_subtotals_map payee_subtotals;

  by_payee_posts();

 public:
  by_payee_posts(post_handler_ptr handler, expr_t& _amount_expr)
    : item_handler<post_t>(handler), amount_expr(_amount_expr) {
    TRACE_CTOR(by_payee_posts, "post_handler_ptr, expr_t&");
  }
  virtual ~by_payee_posts() {
    TRACE_DTOR(by_payee_posts);
  }

  virtual void flush();
  virtual void operator()(post_t& post);
};

class transfer_details : public item_handler<post_t>
{
  account_t *	master;
  expr_t	expr;
  scope_t&	scope;
  temporaries_t temps;

  transfer_details();

public:
  enum element_t {
    SET_DATE,
    SET_ACCOUNT,
    SET_PAYEE
  } which_element;

  transfer_details(post_handler_ptr handler,
		   element_t	    _which_element,
		   account_t *	    _master,
		   const expr_t&    _expr,
		   scope_t&         _scope)
    : item_handler<post_t>(handler), master(_master),
      expr(_expr), scope(_scope), which_element(_which_element) {
    TRACE_CTOR(transfer_details,
	       "post_handler_ptr, element_t, account_t *, expr_t, scope_t&");
  }
  virtual ~transfer_details() {
    TRACE_DTOR(transfer_details);
  }

  virtual void operator()(post_t& post);
};

class dow_posts : public subtotal_posts
{
  posts_list days_of_the_week[7];

  dow_posts();

public:
  dow_posts(post_handler_ptr handler, expr_t& amount_expr)
    : subtotal_posts(handler, amount_expr) {
    TRACE_CTOR(dow_posts, "post_handler_ptr, bool");
  }
  virtual ~dow_posts() throw() {
    TRACE_DTOR(dow_posts);
  }

  virtual void flush();
  virtual void operator()(post_t& post) {
    days_of_the_week[post.date().day_of_week()].push_back(&post);
  }
};

class generate_posts : public item_handler<post_t>
{
  generate_posts();

protected:
  typedef std::pair<date_interval_t, post_t *> pending_posts_pair;
  typedef std::list<pending_posts_pair>	       pending_posts_list;

  pending_posts_list pending_posts;
  temporaries_t      temps;

public:
  generate_posts(post_handler_ptr handler)
    : item_handler<post_t>(handler) {
    TRACE_CTOR(generate_posts, "post_handler_ptr");
  }

  virtual ~generate_posts() {
    TRACE_DTOR(generate_posts);
  }

  void add_period_xacts(period_xacts_list& period_xacts);

  virtual void add_post(const date_interval_t& period, post_t& post);
};

class budget_posts : public generate_posts
{
#define BUDGET_NO_BUDGET   0x00
#define BUDGET_BUDGETED	   0x01
#define BUDGET_UNBUDGETED  0x02
#define BUDGET_WRAP_VALUES 0x04

  uint_least8_t flags;

  budget_posts();

public:
  budget_posts(post_handler_ptr handler,
	       uint_least8_t _flags = BUDGET_BUDGETED)
    : generate_posts(handler), flags(_flags) {
    TRACE_CTOR(budget_posts, "post_handler_ptr, uint_least8_t");
  }
  virtual ~budget_posts() throw() {
    TRACE_DTOR(budget_posts);
  }

  void report_budget_items(const date_t& date);

  virtual void operator()(post_t& post);
};

class forecast_posts : public generate_posts
{
  predicate_t	    pred;
  scope_t&	    context;
  const std::size_t forecast_years;

 public:
  forecast_posts(post_handler_ptr   handler,
		 const predicate_t& predicate,
		 scope_t&           _context,
		 const std::size_t  _forecast_years)
    : generate_posts(handler), pred(predicate), context(_context),
      forecast_years(_forecast_years) {
    TRACE_CTOR(forecast_posts,
	       "post_handler_ptr, predicate_t, scope_t&, std::size_t");
  }
  virtual ~forecast_posts() throw() {
    TRACE_DTOR(forecast_posts);
  }

  virtual void add_post(const date_interval_t& period, post_t& post);
  virtual void flush();
};

//////////////////////////////////////////////////////////////////////
//
// Account filters
//

class accounts_iterator;

class pass_down_accounts : public item_handler<account_t>
{
  pass_down_accounts();

  optional<predicate_t> pred;
  optional<scope_t&>    context;

public:
  pass_down_accounts(acct_handler_ptr		  handler,
		     accounts_iterator&		  iter,
		     const optional<predicate_t>& _pred    = none,
		     const optional<scope_t&>&	  _context = none);

  virtual ~pass_down_accounts() {
    TRACE_DTOR(pass_down_accounts);
  }
};

} // namespace ledger

#endif // _FILTERS_H
