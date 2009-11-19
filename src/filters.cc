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

#include <system.hh>

#include "filters.h"
#include "iterators.h"
#include "journal.h"
#include "report.h"
#include "compare.h"

namespace ledger {

pass_down_posts::pass_down_posts(post_handler_ptr handler,
				 posts_iterator& iter)
  : item_handler<post_t>(handler)
{
  TRACE_CTOR(pass_down_posts, "post_handler_ptr, posts_iterator");

  for (post_t * post = iter(); post; post = iter()) {
    try {
      item_handler<post_t>::operator()(*post);
    }
    catch (const std::exception& err) {
      add_error_context(item_context(*post, _("While handling posting")));
      throw;
    }
  }

  item_handler<post_t>::flush();
}

void truncate_xacts::flush()
{
  if (! posts.size())
    return;

  xact_t * xact = (*posts.begin())->xact;

  int l = 0;
  foreach (post_t * post, posts)
    if (xact != post->xact) {
      l++;
      xact = post->xact;
    }
  l++;

  xact = (*posts.begin())->xact;

  int i = 0;
  foreach (post_t * post, posts) {
    if (xact != post->xact) {
      xact = post->xact;
      i++;
    }

    bool print = false;
    if (head_count) {
      if (head_count > 0 && i < head_count)
	print = true;
      else if (head_count < 0 && i >= - head_count)
	print = true;
    }

    if (! print && tail_count) {
      if (tail_count > 0 && l - i <= tail_count)
	print = true;
      else if (tail_count < 0 && l - i > - tail_count)
	print = true;
    }

    if (print)
      item_handler<post_t>::operator()(*post);
  }
  posts.clear();

  item_handler<post_t>::flush();
}

void truncate_xacts::operator()(post_t& post)
{
  if (last_xact != post.xact) {
    if (last_xact)
      xacts_seen++;
    last_xact = post.xact;
  }

  if (tail_count == 0 && head_count > 0 &&
      static_cast<int>(xacts_seen) >= head_count)
    return;

  posts.push_back(&post);
}

void sort_posts::post_accumulated_posts()
{
  std::stable_sort(posts.begin(), posts.end(),
		   compare_items<post_t>(sort_order));

  foreach (post_t * post, posts) {
    post->xdata().drop_flags(POST_EXT_SORT_CALC);
    item_handler<post_t>::operator()(*post);
  }

  posts.clear();
}

namespace {
  void split_string(const string& str, const char ch,
		    std::list<string>& strings)
  {
    const char * b = str.c_str();
    for (const char * p = b; *p; p++) {
      if (*p == ch) {
	strings.push_back(string(b, p - b));
	b = p + 1;
      }
    }
    strings.push_back(string(b));
  }

  account_t * create_temp_account_from_path(std::list<string>& account_names,
					    temporaries_t&     temps,
					    account_t *        master)
  {
    account_t * new_account = NULL;
    foreach (const string& name, account_names) {
      if (new_account) {
	new_account = new_account->find_account(name);
      } else {
	new_account = master->find_account(name, false);
	if (! new_account)
	  new_account = &temps.create_account(name, master);
      }
    }

    assert(new_account != NULL);
    return new_account;
  }
}

void anonymize_posts::operator()(post_t& post)
{
  SHA1		 sha;
  uint_least32_t message_digest[5];
  bool		 copy_xact_details = false;

  if (last_xact != post.xact) {
    temps.copy_xact(*post.xact);
    last_xact = post.xact;
    copy_xact_details = true;
  }
  xact_t& xact = temps.last_xact();

  if (copy_xact_details) {
    xact.copy_details(*post.xact);

    sha.Reset();
    sha << post.xact->payee.c_str();
    sha.Result(message_digest);

    xact.payee = to_hex(message_digest);
    xact.note  = none;
  }

  std::list<string> account_names;

  for (account_t * acct = post.account;
       acct;
       acct = acct->parent) {
    sha.Reset();
    sha << acct->name.c_str();
    sha.Result(message_digest);

    account_names.push_front(to_hex(message_digest));
  }

  account_t * new_account =
    create_temp_account_from_path(account_names, temps, xact.journal->master);
  post_t& temp = temps.copy_post(post, xact, new_account);
  temp.note = none;

  (*handler)(temp);
}

void calc_posts::operator()(post_t& post)
{
  post_t::xdata_t& xdata(post.xdata());

  if (last_post) {
    assert(last_post->has_xdata());
    if (calc_running_total)
      xdata.total = last_post->xdata().total;
    xdata.count = last_post->xdata().count + 1;
  } else {
    xdata.count = 1;
  }

  post.add_to_value(xdata.visited_value, amount_expr);
  xdata.add_flags(POST_EXT_VISITED);

  account_t * acct = post.reported_account();
  acct->xdata().add_flags(ACCOUNT_EXT_VISITED);

  if (calc_running_total)
    add_or_set_value(xdata.total, xdata.visited_value);

  item_handler<post_t>::operator()(post);

  last_post = &post;
}

namespace {
  typedef function<void (post_t&)> post_functor_t;

  void handle_value(const value_t&	            value,
		    account_t *	                    account,
		    xact_t *		            xact,
		    temporaries_t&                  temps,
		    post_handler_ptr                handler,
		    const date_t&                   date	  = date_t(),
		    const value_t&                  total	  = value_t(),
		    const bool                      direct_amount = false,
		    const bool                      mark_visited  = false,
		    const optional<post_functor_t>& functor       = none)
  {
    post_t& post = temps.create_post(*xact, account);
    post.add_flags(ITEM_GENERATED);

    // If the account for this post is all virtual, then report the post as
    // such.  This allows subtotal reports to show "(Account)" for accounts
    // that contain only virtual posts.
    if (account && account->has_xdata() &&
	account->xdata().has_flags(ACCOUNT_EXT_AUTO_VIRTUALIZE)) {
      if (! account->xdata().has_flags(ACCOUNT_EXT_HAS_NON_VIRTUALS)) {
	post.add_flags(POST_VIRTUAL);
	if (! account->xdata().has_flags(ACCOUNT_EXT_HAS_UNB_VIRTUALS))
	  post.add_flags(POST_MUST_BALANCE);
      }
    }

    post_t::xdata_t& xdata(post.xdata());

    if (is_valid(date))
      xdata.date = date;

    value_t temp(value);

    switch (value.type()) {
    case value_t::BOOLEAN:
    case value_t::INTEGER:
      temp.in_place_cast(value_t::AMOUNT);
      // fall through...

    case value_t::AMOUNT:
      post.amount = temp.as_amount();
      break;

    case value_t::BALANCE:
    case value_t::SEQUENCE:
      xdata.compound_value = temp;
      xdata.add_flags(POST_EXT_COMPOUND);
      break;

    case value_t::DATETIME:
    case value_t::DATE:
    default:
      assert(false);
      break;
    }

    if (! total.is_null())
      xdata.total = total;

    if (direct_amount)
      xdata.add_flags(POST_EXT_DIRECT_AMT);

    if (functor)
      (*functor)(post);

    DEBUG("filter.changed_value.rounding", "post.amount = " << post.amount);

    (*handler)(post);

    if (mark_visited) {
      post.xdata().add_flags(POST_EXT_VISITED);
      post.account->xdata().add_flags(ACCOUNT_EXT_VISITED);
    }
  }
}

void collapse_posts::report_subtotal()
{
  if (! count)
    return;

  std::size_t displayed_count = 0;
  foreach (post_t * post, component_posts) {
    if (only_predicate(*post) && display_predicate(*post))
      displayed_count++;
  }  

  if (displayed_count == 1) {
    item_handler<post_t>::operator()(*last_post);
  }
  else if (only_collapse_if_zero && ! subtotal.is_zero()) {
    foreach (post_t * post, component_posts)
      item_handler<post_t>::operator()(*post);
  }
  else {
    date_t earliest_date;

    foreach (post_t * post, component_posts) {
      date_t reported = post->date();
      if (! is_valid(earliest_date) ||
	  reported < earliest_date)
	earliest_date = reported;
    }

    xact_t& xact = temps.create_xact();
    xact.payee	   = last_xact->payee;
    xact._date	   = (is_valid(earliest_date) ?
		      earliest_date : last_xact->_date);
    DEBUG("filter.collapse", "Pseudo-xact date = " << *xact._date);

    handle_value(subtotal, &totals_account, &xact, temps, handler);
  }

  component_posts.clear();

  last_xact = NULL;
  last_post = NULL;
  subtotal  = 0L;
  count     = 0;
}

void collapse_posts::operator()(post_t& post)
{
  // If we've reached a new xact, report on the subtotal
  // accumulated thus far.

  if (last_xact != post.xact && count > 0)
    report_subtotal();

  post.add_to_value(subtotal, amount_expr);

  component_posts.push_back(&post);

  last_xact = post.xact;
  last_post = &post;
  count++;
}

void related_posts::flush()
{
  if (posts.size() > 0) {
    foreach (post_t * post, posts) {
      if (post->xact) {
	foreach (post_t * r_post, post->xact->posts) {
	  post_t::xdata_t& xdata(r_post->xdata());
	  if (! xdata.has_flags(POST_EXT_HANDLED) &&
	      (! xdata.has_flags(POST_EXT_RECEIVED) ?
	       ! r_post->has_flags(ITEM_GENERATED | POST_VIRTUAL) :
	       also_matching)) {
	    xdata.add_flags(POST_EXT_HANDLED);
	    item_handler<post_t>::operator()(*r_post);
	  }
	}
      } else {
	// This code should only be reachable from the "output"
	// command, since that is the only command which attempts to
	// output auto or period xacts.
	post_t::xdata_t& xdata(post->xdata());
	if (! xdata.has_flags(POST_EXT_HANDLED) &&
	    ! post->has_flags(ITEM_GENERATED)) {
	  xdata.add_flags(POST_EXT_HANDLED);
	  item_handler<post_t>::operator()(*post);
	}
      }
    }
  }

  item_handler<post_t>::flush();
}

changed_value_posts::changed_value_posts(post_handler_ptr handler,
					 report_t&	  _report,
					 bool		  _for_accounts_report,
					 bool		  _show_unrealized)
  : item_handler<post_t>(handler), report(_report),
    for_accounts_report(_for_accounts_report),
    show_unrealized(_show_unrealized), last_post(NULL),
    revalued_account(temps.create_account(_("<Revalued>"))),
    rounding_account(temps.create_account(_("<Rounding>")))
{
  TRACE_CTOR(changed_value_posts, "post_handler_ptr, report_t&, bool");

  display_amount_expr = report.HANDLER(display_amount_).expr;
  total_expr	      = (report.HANDLED(revalued_total_) ?
			 report.HANDLER(revalued_total_).expr :
			 report.HANDLER(display_total_).expr);
  display_total_expr  = report.HANDLER(display_total_).expr;
  changed_values_only = report.HANDLED(revalued_only);

  gains_equity_account =
    report.session.journal->master->find_account(_("Equity:Unrealized Gains"));
  gains_equity_account->add_flags(ACCOUNT_GENERATED);

  losses_equity_account =
    report.session.journal->master->find_account(_("Equity:Unrealized Losses"));
  losses_equity_account->add_flags(ACCOUNT_GENERATED);
}

void changed_value_posts::flush()
{
  if (last_post && last_post->date() <= report.terminus.date()) {
    output_revaluation(*last_post, report.terminus.date());
    last_post = NULL;
  }
  item_handler<post_t>::flush();
}

void changed_value_posts::output_revaluation(post_t& post, const date_t& date)
{
  if (is_valid(date))
    post.xdata().date = date;

  value_t repriced_total;
  try {
    bind_scope_t bound_scope(report, post);
    repriced_total = total_expr.calc(bound_scope);
  }
  catch (...) {
    post.xdata().date = date_t();
    throw;
  }
  post.xdata().date = date_t();

  DEBUG("filter.changed_value",
	"output_revaluation(last_balance) = " << last_total);
  DEBUG("filter.changed_value",
	"output_revaluation(repriced_total) = " << repriced_total);

  if (! last_total.is_null()) {
    if (value_t diff = repriced_total - last_total) {
      DEBUG("filter.changed_value", "output_revaluation(strip(diff)) = "
	    << diff.strip_annotations(report.what_to_keep()));

      xact_t& xact = temps.create_xact();
      xact.payee = _("Commodities revalued");
      xact._date = is_valid(date) ? date : post.date();

      if (! for_accounts_report) {
	handle_value
	  (/* value=         */ diff,
	   /* account=       */ &revalued_account,
	   /* xact=          */ &xact,
	   /* temps=         */ temps,
	   /* handler=       */ handler,
	   /* date=          */ *xact._date,
	   /* total=         */ repriced_total,
	   /* direct_amount= */ false,
	   /* mark_visited=  */ false,
	   /* functor=       */ (optional<post_functor_t>
				 (bind(&changed_value_posts::output_rounding,
				       this, _1))));
      }
      else if (show_unrealized) {
	handle_value
	  (/* value=         */ - diff,
	   /* account=       */ (diff < 0L ?
				 losses_equity_account :
				 gains_equity_account),
	   /* xact=          */ &xact,
	   /* temps=         */ temps,
	   /* handler=       */ handler,
	   /* date=          */ *xact._date,
	   /* total=         */ value_t(),
	   /* direct_amount= */ false,
	   /* mark_visited=  */ true);
      }
    }
  }
}

void changed_value_posts::output_rounding(post_t& post)
{
  bind_scope_t bound_scope(report, post);
  value_t      new_display_total(display_total_expr.calc(bound_scope));

  DEBUG("filter.changed_value.rounding",
	"rounding.new_display_total     = " << new_display_total);

  if (! last_display_total.is_null()) {
    if (value_t repriced_amount = display_amount_expr.calc(bound_scope)) {
      DEBUG("filter.changed_value.rounding",
	    "rounding.repriced_amount       = " << repriced_amount);

      value_t precise_display_total(new_display_total.truncated() -
				    repriced_amount.truncated());

      DEBUG("filter.changed_value.rounding",
	    "rounding.precise_display_total = " << precise_display_total);
      DEBUG("filter.changed_value.rounding",
	    "rounding.last_display_total    = " << last_display_total);

      if (value_t diff = precise_display_total - last_display_total) {
	DEBUG("filter.changed_value.rounding",
	      "rounding.diff                  = " << diff);

	xact_t& xact = temps.create_xact();
	xact.payee = _("Commodity rounding");
	xact._date = post.date();

	handle_value(diff, &rounding_account, &xact, temps, handler,
		     *xact._date, precise_display_total, true);
      }
    }    
  }
  last_display_total = new_display_total;
}

void changed_value_posts::operator()(post_t& post)
{
  if (last_post)
    output_revaluation(*last_post, post.date());

  if (changed_values_only)
    post.xdata().add_flags(POST_EXT_DISPLAYED);

  if (! for_accounts_report)
    output_rounding(post);

  item_handler<post_t>::operator()(post);

  bind_scope_t bound_scope(report, post);
  last_total = total_expr.calc(bound_scope);
  last_post  = &post;
}

void subtotal_posts::report_subtotal(const char *		      spec_fmt,
				     const optional<date_interval_t>& interval)
{
  if (component_posts.empty())
    return;

  optional<date_t> range_start  = interval ? interval->start : none;
  optional<date_t> range_finish = interval ? interval->inclusive_end() : none;

  if (! range_start || ! range_finish) {
    foreach (post_t * post, component_posts) {
      date_t date = post->date();
      if (! range_start || date < *range_start)
	range_start = date;
      if (! range_finish || date > *range_finish)
	range_finish = date;
    }
  }
  component_posts.clear();

  std::ostringstream out_date;
  if (spec_fmt) {
    out_date << format_date(*range_finish, FMT_CUSTOM, spec_fmt);
  }
  else if (date_format) {
    out_date << "- " << format_date(*range_finish, FMT_CUSTOM,
				    date_format->c_str());
  }
  else {
    out_date << "- " << format_date(*range_finish);
  }

  xact_t& xact = temps.create_xact();
  xact.payee = out_date.str();
  xact._date = *range_start;

  foreach (values_map::value_type& pair, values)
    handle_value(pair.second.value, pair.second.account, &xact, temps,
		 handler);

  values.clear();
}

void subtotal_posts::operator()(post_t& post)
{
  component_posts.push_back(&post);

  account_t * acct = post.reported_account();
  assert(acct);

  values_map::iterator i = values.find(acct->fullname());
  if (i == values.end()) {
    value_t temp;
    post.add_to_value(temp, amount_expr);
    std::pair<values_map::iterator, bool> result
      = values.insert(values_pair(acct->fullname(), acct_value_t(acct, temp)));
    assert(result.second);
  } else {
    post.add_to_value((*i).second.value, amount_expr);
  }

  // If the account for this post is all virtual, mark it as
  // such, so that `handle_value' can show "(Account)" for accounts
  // that contain only virtual posts.

  post.reported_account()->xdata().add_flags(ACCOUNT_EXT_AUTO_VIRTUALIZE);

  if (! post.has_flags(POST_VIRTUAL))
    post.reported_account()->xdata().add_flags(ACCOUNT_EXT_HAS_NON_VIRTUALS);
  else if (! post.has_flags(POST_MUST_BALANCE))
    post.reported_account()->xdata().add_flags(ACCOUNT_EXT_HAS_UNB_VIRTUALS);
}

void interval_posts::report_subtotal(const date_interval_t& interval)
{
  if (last_post && interval) {
    if (exact_periods)
      subtotal_posts::report_subtotal();
    else
      subtotal_posts::report_subtotal(NULL, interval);
  }

  last_post = NULL;
}

void interval_posts::operator()(post_t& post)
{
  date_t date = post.date();

  if (! interval.find_period(post.date()))
    return;

  if (interval.duration) {
    if (last_interval && interval != last_interval) {
      report_subtotal(last_interval);

      if (generate_empty_posts) {
	for (++last_interval; interval != last_interval; ++last_interval) {
	  // Generate a null posting, so the intervening periods can be
	  // seen when -E is used, or if the calculated amount ends up being
	  // non-zero
	  xact_t& null_xact = temps.create_xact();
	  null_xact._date = last_interval.inclusive_end();

	  post_t& null_post = temps.create_post(null_xact, &empty_account);
	  null_post.add_flags(POST_CALCULATED);
	  null_post.amount = 0L;

	  last_post = &null_post;
	  subtotal_posts::operator()(null_post);

	  report_subtotal(last_interval);
	}
	assert(interval == last_interval);
      } else {
	last_interval = interval;
      }
    } else {
      last_interval = interval;
    }
    subtotal_posts::operator()(post);
  } else {
    item_handler<post_t>::operator()(post);
  }

  last_post = &post;
}

void posts_as_equity::report_subtotal()
{
  date_t finish;
  foreach (post_t * post, component_posts) {
    date_t date = post->date();
    if (! is_valid(finish) || date > finish)
      finish = date;
  }
  component_posts.clear();

  xact_t& xact = temps.create_xact();
  xact.payee = _("Opening Balances");
  xact._date = finish;

  value_t total = 0L;
  foreach (values_map::value_type& pair, values) {
    if (pair.second.value.is_balance()) {
      foreach (balance_t::amounts_map::value_type amount_pair,
	       pair.second.value.as_balance().amounts)
	handle_value(amount_pair.second, pair.second.account, &xact, temps,
		     handler);
    } else {
      handle_value(pair.second.value, pair.second.account, &xact, temps,
		   handler);
    }
    total += pair.second.value;
  }
  values.clear();

  if (total.is_balance()) {
    foreach (balance_t::amounts_map::value_type pair,
	     total.as_balance().amounts) {
      post_t& balance_post = temps.create_post(xact, balance_account);
      balance_post.amount = - pair.second;
      (*handler)(balance_post);
    }
  } else {
    post_t& balance_post = temps.create_post(xact, balance_account);
    balance_post.amount = - total.to_amount();
    (*handler)(balance_post);
  }
}

void by_payee_posts::flush()
{
  foreach (payee_subtotals_map::value_type& pair, payee_subtotals)
    pair.second->report_subtotal(pair.first.c_str());

  item_handler<post_t>::flush();

  payee_subtotals.clear();
}

void by_payee_posts::operator()(post_t& post)
{
  payee_subtotals_map::iterator i = payee_subtotals.find(post.xact->payee);
  if (i == payee_subtotals.end()) {
    payee_subtotals_pair
      temp(post.xact->payee,
	   shared_ptr<subtotal_posts>(new subtotal_posts(handler, amount_expr)));
    std::pair<payee_subtotals_map::iterator, bool> result
      = payee_subtotals.insert(temp);

    assert(result.second);
    if (! result.second)
      return;
    i = result.first;
  }

  (*(*i).second)(post);
}

void transfer_details::operator()(post_t& post)
{
  xact_t& xact = temps.copy_xact(*post.xact);
  xact._date = post.date();

  post_t& temp = temps.copy_post(post, xact);
  temp.set_state(post.state());

  bind_scope_t bound_scope(scope, temp);
  value_t      substitute(expr.calc(bound_scope));

  switch (which_element) {
  case SET_DATE:
    temp.xdata().date = substitute.to_date();
    break;

  case SET_ACCOUNT: {
    std::list<string> account_names;
    temp.account->remove_post(&temp);
    split_string(substitute.to_string(), ':', account_names);
    temp.account = create_temp_account_from_path(account_names, temps,
						 xact.journal->master);
    temp.account->add_post(&temp);
    break;
  }

  case SET_PAYEE:
    xact.payee = substitute.to_string();
    break;

  default:
    assert(false);
    break;
  }

  item_handler<post_t>::operator()(temp);
}

void dow_posts::flush()
{
  for (int i = 0; i < 7; i++) {
    foreach (post_t * post, days_of_the_week[i])
      subtotal_posts::operator()(*post);
    subtotal_posts::report_subtotal("%As");
    days_of_the_week[i].clear();
  }

  subtotal_posts::flush();
}

void generate_posts::add_period_xacts(period_xacts_list& period_xacts)
{
  foreach (period_xact_t * xact, period_xacts)
    foreach (post_t * post, xact->posts)
      add_post(xact->period, *post);
}

void generate_posts::add_post(const date_interval_t& period, post_t& post)
{
  pending_posts.push_back(pending_posts_pair(period, &post));
}

void budget_posts::report_budget_items(const date_t& date)
{
  if (pending_posts.size() == 0)
    return;

  bool reported;
  do {
    reported = false;
    foreach (pending_posts_list::value_type& pair, pending_posts) {
      optional<date_t> begin = pair.first.start;
      if (! begin) {
	if (! pair.first.find_period(date))
	  throw_(std::runtime_error, _("Something odd has happened"));
	begin = pair.first.start;
      }
      assert(begin);

      if (*begin <= date &&
	  (! pair.first.finish || *begin < *pair.first.finish)) {
	post_t& post = *pair.second;

	DEBUG("budget.generate", "Reporting budget for "
	      << post.reported_account()->fullname());

	xact_t& xact = temps.create_xact();
	xact.payee = _("Budget transaction");
	xact._date = begin;

	post_t& temp = temps.copy_post(post, xact);
	temp.amount.in_place_negate();

	if (flags & BUDGET_WRAP_VALUES) {
	  value_t seq;
	  seq.push_back(0L);
	  seq.push_back(temp.amount);

	  temp.xdata().compound_value = seq;
	  temp.xdata().add_flags(POST_EXT_COMPOUND);
	}

	++pair.first;
	begin = *pair.first.start;

	item_handler<post_t>::operator()(temp);

	reported = true;
      }
    }
  } while (reported);
}

void budget_posts::operator()(post_t& post)
{
  bool post_in_budget = false;

  foreach (pending_posts_list::value_type& pair, pending_posts) {
    for (account_t * acct = post.reported_account();
	 acct;
	 acct = acct->parent) {
      if (acct == (*pair.second).reported_account()) {
	post_in_budget = true;
	// Report the post as if it had occurred in the parent account.
	if (post.reported_account() != acct)
	  post.set_reported_account(acct);
	goto handle;
      }
    }
  }

 handle:
  if (post_in_budget && flags & BUDGET_BUDGETED) {
    report_budget_items(post.date());
    item_handler<post_t>::operator()(post);
  }
  else if (! post_in_budget && flags & BUDGET_UNBUDGETED) {
    item_handler<post_t>::operator()(post);
  }
}

void forecast_posts::add_post(const date_interval_t& period, post_t& post)
{
  generate_posts::add_post(period, post);

  // Advance the period's interval until it is at or beyond the current date.
  date_interval_t& i = pending_posts.back().first;
  if (! i.start) {
    if (! i.find_period(CURRENT_DATE()))
      throw_(std::runtime_error, _("Something odd has happened"));
    ++i;
  } else {
    while (*i.start < CURRENT_DATE())
      ++i;
  }
}

void forecast_posts::flush()
{
  posts_list passed;
  date_t     last = CURRENT_DATE();

  // If there are period transactions to apply in a continuing series until
  // the forecast condition is met, generate those transactions now.  Note
  // that no matter what, we abandon forecasting beyond the next 5 years.
  //
  // It works like this:
  //
  // Earlier, in forecast_posts::add_period_xacts, we cut up all the periodic
  // transactions into their components postings, so that we have N "periodic
  // postings".  For example, if the user had this:
  //
  // ~ daily
  //   Expenses:Food       $10
  //   Expenses:Auto:Gas   $20
  // ~ monthly
  //   Expenses:Food       $100
  //   Expenses:Auto:Gas   $200
  //
  // We now have 4 periodic postings in `pending_posts'.
  //
  // Each periodic postings gets its own copy of its parent transaction's
  // period, which is modified as we go.  This is found in the second member
  // of the pending_posts_list for each posting.
  //
  // The algorithm below works by iterating through the N periodic postings
  // over and over, until each of them mets the termination critera for the
  // forecast and is removed from the set.

  while (pending_posts.size() > 0) {
    // At each step through the loop, we find the first periodic posting whose
    // period contains the earliest starting date.
    pending_posts_list::iterator least = pending_posts.begin();
    for (pending_posts_list::iterator i = ++pending_posts.begin();
	 i != pending_posts.end();
	 i++) {
      if (*(*i).first.start < *(*least).first.start)
	least = i;
    }

    date_t& begin = *(*least).first.start;
    if ((*least).first.finish)
      assert(begin < *(*least).first.finish);

    // If the next date in the series for this periodic posting is more than 5
    // years beyond the last valid post we generated, drop it from further
    // consideration.
    date_t next = *(*least).first.next;
    assert(next > begin);

    if (static_cast<std::size_t>((next - last).days()) >
	static_cast<std::size_t>(365U) * forecast_years) {
      DEBUG("filters.forecast",
	    "Forecast transaction exceeds " << forecast_years
	    << " years beyond today");
      pending_posts.erase(least);
      continue;
    }

    begin = next;
    ++(*least).first;

    // `post' refers to the posting defined in the period transaction.  We
    // make a copy of it within a temporary transaction with the payee
    // "Forecast transaction".
    post_t& post = *(*least).second;
    xact_t& xact = temps.create_xact();
    xact.payee	 = _("Forecast transaction");
    xact._date	 = begin;
    post_t& temp = temps.copy_post(post, xact);

    // Submit the generated posting
    DEBUG("filters.forecast",
	  "Forecast transaction: " << temp.date()
	  << " " << temp.account->fullname()
	  << " " << temp.amount);
    item_handler<post_t>::operator()(temp);

    // If the generated posting matches the user's report query, check whether
    // it also fails to match the continuation condition for the forecast.  If
    // it does, drop this periodic posting from consideration.
    if (temp.has_xdata() && temp.xdata().has_flags(POST_EXT_MATCHES)) {
      DEBUG("filters.forecast", "  matches report query");
      bind_scope_t bound_scope(context, temp);
      if (! pred(bound_scope)) {
	DEBUG("filters.forecast", "  fails to match continuation criteria");
	pending_posts.erase(least);
	continue;
      }
    }
  }

  item_handler<post_t>::flush();
}

pass_down_accounts::pass_down_accounts(acct_handler_ptr		    handler,
				       accounts_iterator&	    iter,
				       const optional<predicate_t>& _pred,
				       const optional<scope_t&>&    _context)
  : item_handler<account_t>(handler), pred(_pred), context(_context)
{
  TRACE_CTOR(pass_down_accounts, "acct_handler_ptr, accounts_iterator, ...");

  for (account_t * account = iter(); account; account = iter()) {
    if (! pred) {
      item_handler<account_t>::operator()(*account);
    } else {
      bind_scope_t bound_scope(*context, *account);
      if ((*pred)(bound_scope))
	item_handler<account_t>::operator()(*account);
    }
  }

  item_handler<account_t>::flush();
}

} // namespace ledger
