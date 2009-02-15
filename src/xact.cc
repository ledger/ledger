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

#include "xact.h"
#include "journal.h"
#include "account.h"
#include "format.h"

namespace ledger {

bool xact_t::has_tag(const string& tag) const
{
  if (item_t::has_tag(tag))
    return true;
  if (entry)
    return entry->has_tag(tag);
  return false;
}

bool xact_t::has_tag(const mask_t& tag_mask,
		     const optional<mask_t>& value_mask) const
{
  if (item_t::has_tag(tag_mask, value_mask))
    return true;
  if (entry)
    return entry->has_tag(tag_mask, value_mask);
  return false;
}

optional<string> xact_t::get_tag(const string& tag) const
{
  if (optional<string> value = item_t::get_tag(tag))
    return value;
  if (entry)
    return entry->get_tag(tag);
  return none;
}

optional<string> xact_t::get_tag(const mask_t& tag_mask,
				 const optional<mask_t>& value_mask) const
{
  if (optional<string> value = item_t::get_tag(tag_mask, value_mask))
    return value;
  if (entry)
    return entry->get_tag(tag_mask, value_mask);
  return none;
}

date_t xact_t::date() const
{
  if (item_t::use_effective_date) {
    if (_date_eff)
      return *_date_eff;
    else if (entry && entry->_date_eff)
      return *entry->_date_eff;
  }

  if (! _date) {
    assert(entry);
    return *entry->_date;
  }
  return *_date;
}

optional<date_t> xact_t::effective_date() const
{
  optional<date_t> date = item_t::effective_date();
  if (! date && entry)
    return entry->effective_date();
  return date;
}

item_t::state_t xact_t::state() const
{
  if (entry) {
    state_t entry_state = entry->state();
    if ((_state == UNCLEARED && entry_state != UNCLEARED) ||
	(_state == PENDING && entry_state == CLEARED))
      return entry_state;
  }
  return _state;
}

namespace {
  value_t get_this(xact_t& xact) {
    return value_t(static_cast<scope_t *>(&xact));
  }

  value_t get_is_calculated(xact_t& xact) {
    return xact.has_flags(XACT_CALCULATED);
  }

  value_t get_virtual(xact_t& xact) {
    return xact.has_flags(XACT_VIRTUAL);
  }

  value_t get_real(xact_t& xact) {
    return ! xact.has_flags(XACT_VIRTUAL);
  }

  value_t get_actual(xact_t& xact) {
    return ! xact.has_flags(XACT_AUTO);
  }

  value_t get_entry(xact_t& xact) {
    return value_t(static_cast<scope_t *>(xact.entry));
  }

  value_t get_code(xact_t& xact) {
    if (xact.entry->code)
      return string_value(*xact.entry->code);
    else
      return string_value(empty_string);
  }

  value_t get_payee(xact_t& xact) {
    return string_value(xact.entry->payee);
  }

  value_t get_amount(xact_t& xact) {
    if (xact.has_xdata() &&
	xact.xdata().has_flags(XACT_EXT_COMPOUND)) {
      return xact.xdata().value;
    } else {
      return xact.amount;
    }
  }

  value_t get_cost(xact_t& xact) {
    if (xact.has_xdata() &&
	xact.xdata().has_flags(XACT_EXT_COMPOUND)) {
      return xact.xdata().value.cost();
    } else {
      if (xact.cost)
	return *xact.cost;
      else
	return xact.amount;
    }
  }

  value_t get_total(xact_t& xact) {
    if (xact.xdata_)
      return xact.xdata_->total;
    else
      return xact.amount;
  }

  value_t get_total_cost(xact_t& xact) {
    if (xact.xdata_)
      return xact.xdata_->total.cost();
    else if (xact.cost)
      return *xact.cost;
    else
      return xact.amount;
  }

  value_t get_count(xact_t& xact) {
    assert(xact.xdata_);
    return xact.xdata_->count;
  }

  value_t get_account(call_scope_t& scope)
  {
    xact_t& xact(find_scope<xact_t>(scope));

    var_t<long> max_width(scope, 0);

    string name = xact.reported_account()->fullname();

    if (max_width && *max_width > 2)
      name = format_t::truncate(name, *max_width - 2, true);

    if (xact.has_flags(XACT_VIRTUAL)) {
      if (xact.must_balance())
	name = string("[") + name + "]";
      else
	name = string("(") + name + ")";
    }
    return string_value(name);
  }

  value_t get_account_base(xact_t& xact) {
    return string_value(xact.reported_account()->name);
  }

  template <value_t (*Func)(xact_t&)>
  value_t get_wrapper(call_scope_t& scope) {
    return (*Func)(find_scope<xact_t>(scope));
  }
}

expr_t::ptr_op_t xact_t::lookup(const string& name)
{
  switch (name[0]) {
  case 'a':
    if (name[1] == '\0' || name == "amount")
      return WRAP_FUNCTOR(get_wrapper<&get_amount>);
    else if (name == "account")
      return WRAP_FUNCTOR(get_account);
    else if (name == "account_base")
      return WRAP_FUNCTOR(get_wrapper<&get_account_base>);
    else if (name == "actual")
      return WRAP_FUNCTOR(get_wrapper<&get_actual>);
    break;

  case 'c':
    if (name == "code")
      return WRAP_FUNCTOR(get_wrapper<&get_code>);
    else if (name == "cost")
      return WRAP_FUNCTOR(get_wrapper<&get_cost>);
    else if (name == "count")
      return WRAP_FUNCTOR(get_wrapper<&get_count>);
    else if (name == "calculated")
      return WRAP_FUNCTOR(get_wrapper<&get_is_calculated>);
    break;

  case 'e':
    if (name == "entry")
      return WRAP_FUNCTOR(get_wrapper<&get_entry>);
    break;

  case 'r':
    if (name == "real")
      return WRAP_FUNCTOR(get_wrapper<&get_real>);
    break;

  case 'p':
    if (name == "payee")
      return WRAP_FUNCTOR(get_wrapper<&get_payee>);
    break;

  case 't':
    if (name[1] == '\0' || name == "total")
      return WRAP_FUNCTOR(get_wrapper<&get_total>);
    else if (name == "total_cost")
      return WRAP_FUNCTOR(get_wrapper<&get_total_cost>);
    break;

  case 'v':
    if (name == "virtual")
      return WRAP_FUNCTOR(get_wrapper<&get_virtual>);
    break;

  case 'x':
    if (name == "xact")
      return WRAP_FUNCTOR(get_wrapper<&get_this>);
    break;
  }

  return item_t::lookup(name);
}

bool xact_t::valid() const
{
  if (! entry) {
    DEBUG("ledger.validate", "xact_t: ! entry");
    return false;
  }

  xacts_list::const_iterator i =
    std::find(entry->xacts.begin(),
	      entry->xacts.end(), this);
  if (i == entry->xacts.end()) {
    DEBUG("ledger.validate", "xact_t: ! found");
    return false;
  }

  if (! account) {
    DEBUG("ledger.validate", "xact_t: ! account");
    return false;
  }

  if (! amount.valid()) {
    DEBUG("ledger.validate", "xact_t: ! amount.valid()");
    return false;
  }

  if (cost && ! cost->valid()) {
    DEBUG("ledger.validate", "xact_t: cost && ! cost->valid()");
    return false;
  }

  return true;
}

void xact_t::add_to_value(value_t& value, expr_t& expr)
{
  if (xdata_ && xdata_->has_flags(XACT_EXT_COMPOUND)) {
    add_or_set_value(value, xdata_->value);
  }

  if (! xdata_ || ! xdata_->has_flags(XACT_EXT_NO_TOTAL)) {
    bind_scope_t bound_scope(*expr.get_context(), *this);
    if (value.is_null())
      value = amount_t(0L);

    value += expr.calc(bound_scope);
  }
}

} // namespace ledger
