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

#include "xact.h"
#include "post.h"
#include "account.h"
#include "journal.h"
#include "pool.h"

namespace ledger {

xact_base_t::xact_base_t(const xact_base_t& xact_base)
  : item_t(xact_base), journal(xact_base.journal)
{
  TRACE_CTOR(xact_base_t, "copy");
}

xact_base_t::~xact_base_t()
{
  TRACE_DTOR(xact_base_t);

  if (! has_flags(ITEM_TEMP)) {
    foreach (post_t * post, posts) {
      // If the posting is a temporary, it will be destructed when the
      // temporary is.
      assert(! post->has_flags(ITEM_TEMP));
      checked_delete(post);
    }
  }
}

void xact_base_t::add_post(post_t * post)
{
  // You can add temporary postings to transactions, but not real postings to
  // temporary transactions.
  if (! post->has_flags(ITEM_TEMP))
    assert(! has_flags(ITEM_TEMP));

  posts.push_back(post);
}

bool xact_base_t::remove_post(post_t * post)
{
  posts.remove(post);
  post->xact = NULL;
  return true;
}

void xact_base_t::clear_xdata()
{
  foreach (post_t * post, posts)
    if (! post->has_flags(ITEM_TEMP))
      post->clear_xdata();
}

value_t xact_base_t::magnitude() const
{
  value_t halfbal = 0L;
  foreach (const post_t * post, posts) {
    if (post->amount.sign() > 0) {
      if (post->cost)
	halfbal += *post->cost;
      else
	halfbal += post->amount;
    }
  }
  return halfbal;
}

bool xact_base_t::finalize()
{
  // Scan through and compute the total balance for the xact.  This is used
  // for auto-calculating the value of xacts with no cost, and the per-unit
  // price of unpriced commodities.

  value_t  balance;
  post_t * null_post = NULL;

  foreach (post_t * post, posts) {
    if (! post->must_balance())
      continue;

    amount_t& p(post->cost ? *post->cost : post->amount);
    if (! p.is_null()) {
      DEBUG("xact.finalize", "post must balance = " << p.reduced());
      if (! post->cost && post->amount.is_annotated() &&
	  post->amount.annotation().price) {
	// If the amount has no cost, but is annotated with a per-unit
	// price, use the price times the amount as the cost
	post->cost = (*post->amount.annotation().price *
		      post->amount).unrounded();
	DEBUG("xact.finalize",
	      "annotation price = " << *post->amount.annotation().price);
	DEBUG("xact.finalize", "amount = " << post->amount);
	DEBUG("xact.finalize", "priced cost = " << *post->cost);
	post->add_flags(POST_COST_CALCULATED);
	add_or_set_value(balance, post->cost->rounded().reduced());
      } else {
	// If the amount was a cost, it very likely has the "keep_precision"
	// flag set, meaning commodity display precision is ignored when
	// displaying the amount.  We never want this set for the balance,
	// so we must clear the flag in a temporary to avoid it propagating
	// into the balance.
	add_or_set_value(balance, p.keep_precision() ?
			 p.rounded().reduced() : p.reduced());
      }
    }
    else if (null_post) {
      throw_(std::logic_error,
	     _("Only one posting with null amount allowed per transaction"));
    }
    else {
      null_post = post;
    }
  }
  VERIFY(balance.valid());

#if defined(DEBUG_ON)
  DEBUG("xact.finalize", "initial balance = " << balance);
  DEBUG("xact.finalize", "balance is " << balance.label());
  if (balance.is_balance())
    DEBUG("xact.finalize", "balance commodity count = "
	  << balance.as_balance().amounts.size());
#endif

  // If there is only one post, balance against the default account if one has
  // been set.

  if (journal && journal->basket && posts.size() == 1 && ! balance.is_null()) {
    null_post = new post_t(journal->basket, ITEM_GENERATED);
    null_post->_state = (*posts.begin())->_state;
    add_post(null_post);
  }

  if (null_post != NULL) {
    // If one post has no value at all, its value will become the inverse of
    // the rest.  If multiple commodities are involved, multiple posts are
    // generated to balance them all.

    DEBUG("xact.finalize", "there was a null posting");

    if (balance.is_balance()) {
      bool first = true;
      const balance_t& bal(balance.as_balance());
      foreach (const balance_t::amounts_map::value_type& pair, bal.amounts) {
	if (first) {
	  null_post->amount = pair.second.negated();
	  null_post->add_flags(POST_CALCULATED);
	  first = false;
	} else {
	  post_t * p = new post_t(null_post->account, pair.second.negated(),
				  ITEM_GENERATED | POST_CALCULATED);
	  p->set_state(null_post->state());
	  add_post(p);
	}
      }
    }
    else if (balance.is_amount()) {
      null_post->amount = balance.as_amount().negated();
      null_post->add_flags(POST_CALCULATED);
    }
    else if (balance.is_long()) {
      null_post->amount = amount_t(- balance.as_long());
      null_post->add_flags(POST_CALCULATED);
    }
    else if (! balance.is_null() && ! balance.is_realzero()) {
      throw_(balance_error, _("Transaction does not balance"));
    }
    balance = NULL_VALUE;
  }
  else if (balance.is_balance() &&
	   balance.as_balance().amounts.size() == 2) {
    // When an xact involves two different commodities (regardless of how
    // many posts there are) determine the conversion ratio by dividing the
    // total value of one commodity by the total value of the other.  This
    // establishes the per-unit cost for this post for both commodities.

    DEBUG("xact.finalize", "there were exactly two commodities");

    bool     saw_cost = false;
    post_t * top_post = NULL;

    foreach (post_t * post, posts) {
      if (! post->amount.is_null()) {
	if (post->amount.is_annotated())
	  top_post = post;
	else if (! top_post)
	  top_post = post;
      }

      if (post->cost && ! post->has_flags(POST_COST_CALCULATED)) {
	saw_cost = true;
	break;
      }
    }

    if (! saw_cost && top_post) {
      const balance_t& bal(balance.as_balance());

      DEBUG("xact.finalize", "there were no costs, and a valid top_post");

      balance_t::amounts_map::const_iterator a = bal.amounts.begin();
    
      const amount_t * x = &(*a++).second;
      const amount_t * y = &(*a++).second;

      if (x->commodity() != top_post->amount.commodity()) {
	const amount_t * t = x;
	x = y;
	y = t;
      }

      if (*x && *y) {
	DEBUG("xact.finalize", "primary   amount = " << *x);
	DEBUG("xact.finalize", "secondary amount = " << *y);

	commodity_t& comm(x->commodity());
	amount_t	   per_unit_cost;
	amount_t	   total_cost;

	foreach (post_t * post, posts) {
	  if (post != top_post && post->must_balance() &&
	      ! post->amount.is_null() &&
	      post->amount.is_annotated() &&
	      post->amount.annotation().price) {
	    amount_t temp = *post->amount.annotation().price * post->amount;
	    if (total_cost.is_null()) {
	      total_cost = temp;
	      y = &total_cost;
	    } else {
	      total_cost += temp;
	    }
	    DEBUG("xact.finalize", "total_cost = " << total_cost);
	  }
	}
	per_unit_cost = (*y / *x).abs().unrounded();

	DEBUG("xact.finalize", "per_unit_cost = " << per_unit_cost);

	foreach (post_t * post, posts) {
	  const amount_t& amt(post->amount);

	  if (post->must_balance() && amt.commodity() == comm) {
	    balance -= amt;
	    post->cost = per_unit_cost * amt;
	    post->add_flags(POST_COST_CALCULATED);
	    balance += *post->cost;

	    DEBUG("xact.finalize", "set post->cost to = " << *post->cost);
	  }
	}
      }
    }
  }

  // Now that the post list has its final form, calculate the balance once
  // more in terms of total cost, accounting for any possible gain/loss
  // amounts.

  DEBUG("xact.finalize", "resolved balance = " << balance);

  posts_list copy(posts);

  foreach (post_t * post, copy) {
    if (! post->cost)
      continue;

    if (post->amount.commodity() == post->cost->commodity())
      throw_(balance_error,
	     _("A posting's cost must be of a different commodity than its amount"));

    cost_breakdown_t breakdown =
      amount_t::current_pool->exchange(post->amount, *post->cost, false,
				       datetime_t(date(), time_duration(0, 0, 0, 0)));

    if (post->amount.is_annotated() &&
	breakdown.basis_cost.commodity() ==
	breakdown.final_cost.commodity()) {
      if (amount_t gain_loss = (breakdown.basis_cost -
				breakdown.final_cost).rounded()) {
	DEBUG("xact.finalize", "gain_loss = " << gain_loss);

	add_or_set_value(balance, gain_loss.reduced());

	account_t * account;
	if (gain_loss.sign() > 0)
	  account = journal->find_account(_("Equity:Capital Gains"));
	else
	  account = journal->find_account(_("Equity:Capital Losses"));

	post_t * p = new post_t(account, gain_loss, ITEM_GENERATED);
	p->set_state(post->state());
	add_post(p);
	DEBUG("xact.finalize", "added gain_loss, balance = " << balance);
      }
    } else {
      post->amount = breakdown.amount;
      DEBUG("xact.finalize", "added breakdown, balance = " << balance);
    }
  }

  DEBUG("xact.finalize", "final balance = " << balance);

  if (! balance.is_null() && ! balance.is_zero()) {
    add_error_context(item_context(*this, _("While balancing transaction")));
    add_error_context(_("Unbalanced remainder is:"));
    add_error_context(value_context(balance));
    add_error_context(_("Amount to balance against:"));
    add_error_context(value_context(magnitude()));
    throw_(balance_error, _("Transaction does not balance"));
  }

  // Add a pointer to each posting to their related accounts

  if (dynamic_cast<xact_t *>(this)) {
    bool all_null  = true;
    bool some_null = false;

    foreach (post_t * post, posts) {
      if (! post->amount.is_null()) {
	all_null = false;
	post->amount.in_place_reduce();
      } else {
	some_null = true;
      }

      post->account->add_post(post);

      post->xdata().add_flags(POST_EXT_VISITED);
      post->account->xdata().add_flags(ACCOUNT_EXT_VISITED);
    }

    if (all_null)
      return false;		// ignore this xact completely
    else if (some_null)
      throw_(balance_error,
	     _("There cannot be null amounts after balancing a transaction"));
  }

  VERIFY(valid());

  return true;
}

xact_t::xact_t(const xact_t& e)
  : xact_base_t(e), code(e.code), payee(e.payee)
{
  TRACE_CTOR(xact_t, "copy");
}

void xact_t::add_post(post_t * post)
{
  post->xact = this;
  xact_base_t::add_post(post);
}

string xact_t::idstring() const
{
  std::ostringstream buf;
  buf << format_date(*_date, FMT_WRITTEN);
  buf << payee;
  magnitude().number().print(buf);
  return buf.str();
}

string xact_t::id() const
{
  SHA1 sha;
  sha.Reset();
  sha << idstring().c_str();
  uint_least32_t message_digest[5];
  sha.Result(message_digest);
  return to_hex(message_digest, 5);
}

namespace {
  value_t get_magnitude(xact_t& xact) {
    return xact.magnitude();
  }
  value_t get_idstring(xact_t& xact) {
    return string_value(xact.idstring());
  }
  value_t get_id(xact_t& xact) {
    return string_value(xact.id());
  }

  value_t get_code(xact_t& xact) {
    if (xact.code)
      return string_value(*xact.code);
    else
      return string_value(empty_string);
  }

  value_t get_payee(xact_t& xact) {
    return string_value(xact.payee);
  }

  template <value_t (*Func)(xact_t&)>
  value_t get_wrapper(call_scope_t& scope) {
    return (*Func)(find_scope<xact_t>(scope));
  }
}

expr_t::ptr_op_t xact_t::lookup(const symbol_t::kind_t kind,
				const string& name)
{
  if (kind != symbol_t::FUNCTION)
    return item_t::lookup(kind, name);

  switch (name[0]) {
  case 'c':
    if (name == "code")
      return WRAP_FUNCTOR(get_wrapper<&get_code>);
    break;

  case 'i':
    if (name == "id")
      return WRAP_FUNCTOR(get_wrapper<&get_id>);
    else if (name == "idstring")
      return WRAP_FUNCTOR(get_wrapper<&get_idstring>);
    break;

  case 'm':
    if (name == "magnitude")
      return WRAP_FUNCTOR(get_wrapper<&get_magnitude>);
    break;

  case 'p':
    if (name[1] == '\0' || name == "payee")
      return WRAP_FUNCTOR(get_wrapper<&get_payee>);
    break;
  }

  return item_t::lookup(kind, name);
}

bool xact_t::valid() const
{
  if (! _date) {
    DEBUG("ledger.validate", "xact_t: ! _date");
    return false;
  }

  foreach (post_t * post, posts)
    if (post->xact != this || ! post->valid()) {
      DEBUG("ledger.validate", "xact_t: post not valid");
      return false;
    }

  return true;
}

void auto_xact_t::extend_xact(xact_base_t& xact, bool post_handler)
{
  posts_list initial_posts(xact.posts.begin(), xact.posts.end());

  try {

  foreach (post_t * initial_post, initial_posts) {
    if (! initial_post->has_flags(ITEM_GENERATED) &&
	predicate(*initial_post)) {
      foreach (post_t * post, posts) {
	amount_t amt;
	assert(post->amount);
	if (! post->amount.commodity()) {
	  if (post_handler || initial_post->amount.is_null())
	    continue;
	  amt = initial_post->amount * post->amount;
	} else {
	  if (post_handler)
	    continue;
	  amt = post->amount;
	}

	IF_DEBUG("xact.extend") {
	  DEBUG("xact.extend",
		"Initial post on line " << initial_post->pos->beg_line << ": "
		<< "amount " << initial_post->amount << " (precision "
		<< initial_post->amount.precision() << ")");

#if defined(DEBUG_ON)
	  if (initial_post->amount.keep_precision())
	    DEBUG("xact.extend", "  precision is kept");
#endif

	  DEBUG("xact.extend",
		"Posting on line " << post->pos->beg_line << ": "
		<< "amount " << post->amount << ", amt " << amt
		<< " (precision " << post->amount.precision()
		<< " != " << amt.precision() << ")");

#if defined(DEBUG_ON)
	  if (post->amount.keep_precision())
	    DEBUG("xact.extend", "  precision is kept");
	  if (amt.keep_precision())
	    DEBUG("xact.extend", "  amt precision is kept");
#endif
	}

	account_t * account  = post->account;
	string fullname = account->fullname();
	assert(! fullname.empty());
	if (fullname == "$account" || fullname == "@account")
	  account = initial_post->account;

	// Copy over details so that the resulting post is a mirror of
	// the automated xact's one.
	post_t * new_post = new post_t(account, amt);
	new_post->copy_details(*post);
	new_post->add_flags(ITEM_GENERATED);

	xact.add_post(new_post);
      }
    }
  }

  }
  catch (const std::exception& err) {
    add_error_context(item_context(*this, _("While applying automated transaction")));
    add_error_context(item_context(xact, _("While extending transaction")));
    throw;
  }
}

void extend_xact_base(journal_t *  journal,
		      xact_base_t& base,
		      bool	   post_handler)
{
  foreach (auto_xact_t * xact, journal->auto_xacts)
    xact->extend_xact(base, post_handler);
}

} // namespace ledger
