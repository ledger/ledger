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

optional<date_t> xact_t::actual_date() const
{
  optional<date_t> date = item_t::actual_date();
  if (! date && entry)
    return entry->actual_date();
  return date;
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

#if 0
  value_t get_price(xact_t& xact) {
    if (xact.has_xdata() &&
	xact.xdata().has_flags(XACT_EXT_COMPOUND)) {
      return xact.xdata().value;
    } else {
      return xact.amount;
    }
  }
#endif

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

  value_t get_account(call_scope_t& scope)
  {
    xact_t& xact(downcast<xact_t>(*scope.parent));

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
    break;

  case 'c':
    if (name == "code")
      return WRAP_FUNCTOR(get_wrapper<&get_code>);
    else if (name == "cost")
      return WRAP_FUNCTOR(get_wrapper<&get_cost>);
    break;

  case 'e':
    if (name == "entry")
      return WRAP_FUNCTOR(get_wrapper<&get_entry>);
    break;

  case 'p':
    if (name == "payee")
      return WRAP_FUNCTOR(get_wrapper<&get_payee>);
#if 0
    else if (name == "price")
      return WRAP_FUNCTOR(get_wrapper<&get_price>);
#endif
    break;

  case 't':
    if (name[1] == '\0' || name == "total")
      return WRAP_FUNCTOR(get_wrapper<&get_total>);
    else if (name == "total_cost")
      return WRAP_FUNCTOR(get_wrapper<&get_total_cost>);
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

void xact_t::add_to_value(value_t& value)
{
  if (xdata_ && xdata_->has_flags(XACT_EXT_COMPOUND)) {
    add_or_set_value(value, xdata_->value);
  }
  else if (cost || (! value.is_null() && ! value.is_realzero())) {
    if (value.is_null())
      value = amount_t();
    value.add(amount, cost);
  }
  else {
    value = amount;
  }
}

} // namespace ledger
