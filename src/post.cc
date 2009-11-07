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

#include "post.h"
#include "xact.h"
#include "account.h"
#include "interactive.h"
#include "unistring.h"
#include "format.h"

namespace ledger {

bool post_t::has_tag(const string& tag) const
{
  if (item_t::has_tag(tag))
    return true;
  if (xact)
    return xact->has_tag(tag);
  return false;
}

bool post_t::has_tag(const mask_t& tag_mask,
		     const optional<mask_t>& value_mask) const
{
  if (item_t::has_tag(tag_mask, value_mask))
    return true;
  if (xact)
    return xact->has_tag(tag_mask, value_mask);
  return false;
}

optional<string> post_t::get_tag(const string& tag) const
{
  if (optional<string> value = item_t::get_tag(tag))
    return value;
  if (xact)
    return xact->get_tag(tag);
  return none;
}

optional<string> post_t::get_tag(const mask_t& tag_mask,
				 const optional<mask_t>& value_mask) const
{
  if (optional<string> value = item_t::get_tag(tag_mask, value_mask))
    return value;
  if (xact)
    return xact->get_tag(tag_mask, value_mask);
  return none;
}

date_t post_t::date() const
{
  if (xdata_ && is_valid(xdata_->date))
    return xdata_->date;

  if (item_t::use_effective_date) {
    if (_date_eff)
      return *_date_eff;
    else if (xact && xact->_date_eff)
      return *xact->_date_eff;
  }

  if (! _date) {
    assert(xact);
    return xact->date();
  }
  return *_date;
}

optional<date_t> post_t::effective_date() const
{
  optional<date_t> date = item_t::effective_date();
  if (! date && xact)
    return xact->effective_date();
  return date;
}

namespace {
  value_t get_this(post_t& post) {
    return value_t(static_cast<scope_t *>(&post));
  }

  value_t get_is_calculated(post_t& post) {
    return post.has_flags(POST_CALCULATED);
  }

  value_t get_is_cost_calculated(post_t& post) {
    return post.has_flags(POST_COST_CALCULATED);
  }

  value_t get_virtual(post_t& post) {
    return post.has_flags(POST_VIRTUAL);
  }

  value_t get_real(post_t& post) {
    return ! post.has_flags(POST_VIRTUAL);
  }

  value_t get_xact(post_t& post) {
    return value_t(static_cast<scope_t *>(post.xact));
  }

  value_t get_code(post_t& post) {
    if (post.xact->code)
      return string_value(*post.xact->code);
    else
      return string_value(empty_string);
  }

  value_t get_payee(post_t& post) {
    return string_value(post.xact->payee);
  }

  value_t get_note(post_t& post) {
    string note = post.note ? *post.note : empty_string;
    note += post.xact->note ? *post.xact->note : empty_string;
    return string_value(note);
  }

  value_t get_magnitude(post_t& post) {
    return post.xact->magnitude();
  }
  value_t get_idstring(post_t& post) {
    return string_value(post.xact->idstring());
  }
  value_t get_id(post_t& post) {
    return string_value(post.xact->id());
  }

  value_t get_amount(post_t& post) {
    if (post.has_xdata() && post.xdata().has_flags(POST_EXT_COMPOUND))
      return post.xdata().compound_value;
    else if (post.amount.is_null())
      return 0L;
    else
      return post.amount;
  }

  value_t get_use_direct_amount(post_t& post) {
    return post.has_xdata() && post.xdata().has_flags(POST_EXT_DIRECT_AMT);
  }

  value_t get_commodity(post_t& post) {
    return string_value(post.amount.commodity().symbol());
  }

  value_t get_commodity_is_primary(post_t& post) {
    return post.amount.commodity().has_flags(COMMODITY_PRIMARY);
  }

  value_t get_has_cost(post_t& post) {
    return post.cost ? true : false;
  }

  value_t get_cost(post_t& post) {
    if (post.cost)
      return *post.cost;
    else if (post.has_xdata() &&
	     post.xdata().has_flags(POST_EXT_COMPOUND))
      return post.xdata().compound_value;
    else if (post.amount.is_null())
      return 0L;
    else
      return post.amount;
  }

  value_t get_total(post_t& post) {
    if (post.xdata_ && ! post.xdata_->total.is_null())
      return post.xdata_->total;
    else if (post.amount.is_null())
      return 0L;
    else
      return post.amount;
  }

  value_t get_count(post_t& post) {
    if (post.xdata_)
      return long(post.xdata_->count);
    else
      return 1L;
  }

  value_t get_account(call_scope_t& scope)
  {
    in_context_t<post_t> env(scope, "&l");

    string name = env->reported_account()->fullname();

    if (env.has(0) && env.get<long>(0) > 2)
      name = format_t::truncate(name, env.get<long>(0) - 2,
				2 /* account_abbrev_length */);

    if (env->has_flags(POST_VIRTUAL)) {
      if (env->must_balance())
	name = string("[") + name + "]";
      else
	name = string("(") + name + ")";
    }
    return string_value(name);
  }

  value_t get_account_base(post_t& post) {
    return string_value(post.reported_account()->name);
  }

  value_t get_account_amount(call_scope_t& scope)
  {
    in_context_t<post_t> env(scope, "&v");

    account_t * account = NULL;
    if (env.has(0)) {
      account_t * master = env->account;
      while (master->parent)
	master = master->parent;

      if (env.value_at(0).is_string())
	account = master->find_account(env.get<string>(0), false);
      else if (env.value_at(0).is_mask())
	account = master->find_account_re(env.get<mask_t>(0).expr.str());
    } else {
      account = env->reported_account();
    }

    if (! account)
      throw_(std::runtime_error, _("Cannot locate referenced account"));

    DEBUG("post.account_amount", "Found account: " << account->fullname());

    value_t total = account->amount();
    if (total.is_null())
      return 0L;
    else
      return total.simplified();
  }

  value_t get_account_depth(post_t& post) {
    return long(post.reported_account()->depth);
  }

  value_t get_datetime(post_t& post) {
    return post.xdata().datetime;
  }

  template <value_t (*Func)(post_t&)>
  value_t get_wrapper(call_scope_t& scope) {
    return (*Func)(find_scope<post_t>(scope));
  }
}

expr_t::ptr_op_t post_t::lookup(const symbol_t::kind_t kind,
				const string& name)
{
  if (kind != symbol_t::FUNCTION)
    return item_t::lookup(kind, name);

  switch (name[0]) {
  case 'a':
    if (name[1] == '\0' || name == "amount")
      return WRAP_FUNCTOR(get_wrapper<&get_amount>);
    else if (name == "account")
      return WRAP_FUNCTOR(get_account);
    else if (name == "account_amount")
      return WRAP_FUNCTOR(get_account_amount);
    else if (name == "account_base")
      return WRAP_FUNCTOR(get_wrapper<&get_account_base>);
    break;

  case 'c':
    if (name == "code")
      return WRAP_FUNCTOR(get_wrapper<&get_code>);
    else if (name == "cost")
      return WRAP_FUNCTOR(get_wrapper<&get_cost>);
    else if (name == "cost_calculated")
      return WRAP_FUNCTOR(get_wrapper<&get_is_cost_calculated>);
    else if (name == "count")
      return WRAP_FUNCTOR(get_wrapper<&get_count>);
    else if (name == "calculated")
      return WRAP_FUNCTOR(get_wrapper<&get_is_calculated>);
    else if (name == "commodity")
      return WRAP_FUNCTOR(get_wrapper<&get_commodity>);
    break;

  case 'd':
    if (name == "depth")
      return WRAP_FUNCTOR(get_wrapper<&get_account_depth>);
    else if (name == "datetime")
      return WRAP_FUNCTOR(get_wrapper<&get_datetime>);
    break;

  case 'h':
    if (name == "has_cost")
      return WRAP_FUNCTOR(get_wrapper<&get_has_cost>);
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

  case 'n':
    if (name == "note")
      return WRAP_FUNCTOR(get_wrapper<&get_note>);
    break;

  case 'p':
    if (name == "post")
      return WRAP_FUNCTOR(get_wrapper<&get_this>);
    else if (name == "payee")
      return WRAP_FUNCTOR(get_wrapper<&get_payee>);
    else if (name == "primary")
      return WRAP_FUNCTOR(get_wrapper<&get_commodity_is_primary>);
    else if (name == "parent")
      return WRAP_FUNCTOR(get_wrapper<&get_xact>);
    break;

  case 'r':
    if (name == "real")
      return WRAP_FUNCTOR(get_wrapper<&get_real>);
    break;

  case 't':
    if (name[1] == '\0' || name == "total")
      return WRAP_FUNCTOR(get_wrapper<&get_total>);
    break;

  case 'u':
    if (name == "use_direct_amount")
      return WRAP_FUNCTOR(get_wrapper<&get_use_direct_amount>);
    break;

  case 'v':
    if (name == "virtual")
      return WRAP_FUNCTOR(get_wrapper<&get_virtual>);
    break;

  case 'x':
    if (name == "xact")
      return WRAP_FUNCTOR(get_wrapper<&get_xact>);
    break;
  }

  return item_t::lookup(kind, name);
}

bool post_t::valid() const
{
  if (! xact) {
    DEBUG("ledger.validate", "post_t: ! xact");
    return false;
  }

  posts_list::const_iterator i =
    std::find(xact->posts.begin(),
	      xact->posts.end(), this);
  if (i == xact->posts.end()) {
    DEBUG("ledger.validate", "post_t: ! found");
    return false;
  }

  if (! account) {
    DEBUG("ledger.validate", "post_t: ! account");
    return false;
  }

  if (! amount.valid()) {
    DEBUG("ledger.validate", "post_t: ! amount.valid()");
    return false;
  }

  if (cost) {
    if (! cost->valid()) {
      DEBUG("ledger.validate", "post_t: cost && ! cost->valid()");
      return false;
    }
    if (! cost->keep_precision()) {
      DEBUG("ledger.validate", "post_t: ! cost->keep_precision()");
      return false;
    }
  }

  return true;
}

void post_t::add_to_value(value_t& value, const optional<expr_t&>& expr) const
{
  if (xdata_ && xdata_->has_flags(POST_EXT_COMPOUND)) {
    add_or_set_value(value, xdata_->compound_value);
  }
  else if (expr) {
    bind_scope_t bound_scope(*expr->get_context(),
			     const_cast<post_t&>(*this));
#if 1
    value_t temp(expr->calc(bound_scope));
    add_or_set_value(value, temp);
#else
    if (! xdata_) xdata_ = xdata_t();
    xdata_->value = expr->calc(bound_scope);
    xdata_->add_flags(POST_EXT_COMPOUND);

    add_or_set_value(value, xdata_->value);
#endif
  }
  else if (xdata_ && xdata_->has_flags(POST_EXT_VISITED) &&
	   ! xdata_->visited_value.is_null()) {
    add_or_set_value(value, xdata_->visited_value);
  }
  else {
    add_or_set_value(value, amount);
  }
}

void post_t::set_reported_account(account_t * account)
{
  xdata().account = account;
  account->xdata().reported_posts.push_back(this);
}

} // namespace ledger
