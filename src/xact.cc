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

bool xact_base_t::has_xdata()
{
  foreach (post_t * post, posts)
    if (post->has_xdata())
      return true;

  return false;
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
      if (! post->cost && post->amount.has_annotation() &&
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

  if (journal && journal->bucket && posts.size() == 1 && ! balance.is_null()) {
    null_post = new post_t(journal->bucket, ITEM_GENERATED);
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
	if (post->amount.has_annotation())
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
	      post->amount.has_annotation() &&
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
      commodity_pool_t::current_pool->exchange
        (post->amount, *post->cost, false,
	 datetime_t(date(), time_duration(0, 0, 0, 0)));

    if (post->amount.has_annotation() &&
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

bool xact_base_t::verify()
{
  // Scan through and compute the total balance for the xact.

  value_t  balance;

  foreach (post_t * post, posts) {
    if (! post->must_balance())
      continue;

    amount_t& p(post->cost ? *post->cost : post->amount);
    assert(! p.is_null());
      
    // If the amount was a cost, it very likely has the "keep_precision" flag
    // set, meaning commodity display precision is ignored when displaying the
    // amount.  We never want this set for the balance, so we must clear the
    // flag in a temporary to avoid it propagating into the balance.
    add_or_set_value(balance, p.keep_precision() ?
		     p.rounded().reduced() : p.reduced());
  }
  VERIFY(balance.valid());

  // Now that the post list has its final form, calculate the balance once
  // more in terms of total cost, accounting for any possible gain/loss
  // amounts.

  foreach (post_t * post, posts) {
    if (! post->cost)
      continue;

    if (post->amount.commodity() == post->cost->commodity())
      throw_(amount_error,
	     _("A posting's cost must be of a different commodity than its amount"));
  }

  if (! balance.is_null() && ! balance.is_zero()) {
    add_error_context(item_context(*this, _("While balancing transaction")));
    add_error_context(_("Unbalanced remainder is:"));
    add_error_context(value_context(balance));
    add_error_context(_("Amount to balance against:"));
    add_error_context(value_context(magnitude()));
    throw_(balance_error, _("Transaction does not balance"));
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

namespace {

  bool post_pred(expr_t::ptr_op_t op, post_t& post)
  {
    switch (op->kind) {
    case expr_t::op_t::VALUE:
      return op->as_value().to_boolean();
      break;

    case expr_t::op_t::O_MATCH:
      if (op->left()->kind == expr_t::op_t::IDENT &&
	  op->left()->as_ident() == "account" &&
	  op->right()->kind == expr_t::op_t::VALUE &&
	  op->right()->as_value().is_mask())
	return op->right()->as_value().as_mask()
	  .match(post.reported_account()->fullname());
      else
	break;
      
    case expr_t::op_t::O_NOT:
      return ! post_pred(op->left(), post);

    case expr_t::op_t::O_AND:
      return post_pred(op->left(), post) && post_pred(op->right(), post);

    case expr_t::op_t::O_OR:
      return post_pred(op->left(), post) || post_pred(op->right(), post);

    case expr_t::op_t::O_QUERY:
      if (post_pred(op->left(), post))
	return post_pred(op->right()->left(), post);
      else
	return post_pred(op->right()->right(), post);

    default:
      break;
    }

    throw_(calc_error, _("Unhandled operator"));
    return false;
  }

} // unnamed namespace

void auto_xact_t::extend_xact(xact_base_t& xact)
{
  posts_list initial_posts(xact.posts.begin(), xact.posts.end());

  try {

  bool needs_further_verification = false;

  foreach (post_t * initial_post, initial_posts) {
    if (initial_post->has_flags(ITEM_GENERATED))
      continue;

    bool matches_predicate = false;
    if (try_quick_match) {
      try {
	bool found_memoized_result = false;
	if (! memoized_results.empty()) {
	  std::map<string, bool>::iterator i =
	    memoized_results.find(initial_post->account->fullname());
	  if (i != memoized_results.end()) {
	    found_memoized_result = true;
	    matches_predicate = (*i).second;
	  }
	}

	// Since the majority of people who use automated transactions simply
	// match against account names, try using a *much* faster version of
	// the predicate evaluator.
	if (! found_memoized_result) {
	  matches_predicate = post_pred(predicate.get_op(), *initial_post);
	  memoized_results.insert
	    (std::pair<string, bool>(initial_post->account->fullname(),
				     matches_predicate));
	}
      }
      catch (...) {
	DEBUG("xact.extend.fail",
	      "The quick matcher failed, going back to regular eval");
	try_quick_match   = false;
	matches_predicate = predicate(*initial_post);
      }
    } else {
      matches_predicate = predicate(*initial_post);
    }
    if (matches_predicate) {
      foreach (post_t * post, posts) {
	amount_t post_amount;
	if (post->amount.is_null()) {
	  if (! post->amount_expr)
	    throw_(amount_error,
		   _("Automated transaction's posting has no amount"));

	  bind_scope_t bound_scope(*scope_t::default_scope, *initial_post);
	  value_t result(post->amount_expr->calc(bound_scope));
	  if (result.is_long()) {
	    post_amount = result.to_amount();
	  } else {
	    if (! result.is_amount())
	      throw_(amount_error,
		     _("Amount expressions must result in a simple amount"));
	    post_amount = result.as_amount();
	  }
	} else {
	  post_amount = post->amount;
	}

	amount_t amt;
	if (! post_amount.commodity())
	  amt = initial_post->amount * post_amount;
	else
	  amt = post_amount;

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
		<< "amount " << post_amount << ", amt " << amt
		<< " (precision " << post_amount.precision()
		<< " != " << amt.precision() << ")");

#if defined(DEBUG_ON)
	  if (post_amount.keep_precision())
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
	new_post->account->add_post(new_post);

	if (new_post->must_balance())
	  needs_further_verification = true;
      }
    }
  }

  if (needs_further_verification)
    xact.verify();

  }
  catch (const std::exception& err) {
    add_error_context(item_context(*this, _("While applying automated transaction")));
    add_error_context(item_context(xact, _("While extending transaction")));
    throw;
  }
}

void extend_xact_base(journal_t *  journal,
		      xact_base_t& base)
{
  foreach (auto_xact_t * xact, journal->auto_xacts)
    xact->extend_xact(base);
}

void to_xml(std::ostream& out, const xact_t& xact)
{
  push_xml x(out, "transaction", true, true);

  if (xact.state() == item_t::CLEARED)
    out << " state=\"cleared\"";
  else if (xact.state() == item_t::PENDING)
    out << " state=\"pending\"";

  if (xact.has_flags(ITEM_GENERATED))
    out << " generated=\"true\"";

  x.close_attrs();

  if (xact._date) {
    push_xml y(out, "date");
    to_xml(out, *xact._date, false);
  }
  if (xact._date_eff) {
    push_xml y(out, "effective-date");
    to_xml(out, *xact._date_eff, false);
  }

  if (xact.code) {
    push_xml y(out, "code");
    out << y.guard(*xact.code);
  }

  {
    push_xml y(out, "payee");
    out << y.guard(xact.payee);
  }

  if (xact.note) {
    push_xml y(out, "note");
    out << y.guard(*xact.note);
  }

  if (xact.metadata) {
    push_xml y(out, "metadata");
    foreach (const item_t::string_map::value_type& pair, *xact.metadata) {
      if (pair.second) {
	push_xml z(out, "variable");
	{
	  push_xml w(out, "key");
	  out << y.guard(pair.first);
	}
	{
	  push_xml w(out, "value");
	  out << y.guard(*pair.second);
	}
      } else {
	push_xml z(out, "tag");
	out << y.guard(pair.first);
      }
    }
  }
}

} // namespace ledger
