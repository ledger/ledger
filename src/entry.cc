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

#include "entry.h"
#include "journal.h"
#include "account.h"
#include "format.h"

namespace ledger {

entry_base_t::entry_base_t(const entry_base_t&)
  : item_t(), journal(NULL)
{
  TRACE_CTOR(entry_base_t, "copy");
}

entry_base_t::~entry_base_t()
{
  TRACE_DTOR(entry_base_t);

  foreach (xact_t * xact, xacts) {
    // If the transaction is a temporary, it will be destructed when the
    // temporary is.
    if (! xact->has_flags(ITEM_TEMP))
      checked_delete(xact);
  }
}

item_t::state_t entry_base_t::state() const
{
  state_t result = CLEARED;

  foreach (xact_t * xact, xacts) {
    if (xact->_state == UNCLEARED)
      return UNCLEARED;
    else if (xact->_state == PENDING)
      result = PENDING;
  }
  return result;
}

void entry_base_t::add_xact(xact_t * xact)
{
  xacts.push_back(xact);
}

bool entry_base_t::remove_xact(xact_t * xact)
{
  xacts.remove(xact);
  xact->entry   = NULL;
  return true;
}

bool entry_base_t::finalize()
{
  // Scan through and compute the total balance for the entry.  This is used
  // for auto-calculating the value of entries with no cost, and the per-unit
  // price of unpriced commodities.

  value_t  balance;
  xact_t * null_xact = NULL;

  foreach (xact_t * xact, xacts) {
    if (xact->must_balance()) {
      amount_t& p(xact->cost ? *xact->cost : xact->amount);
      DEBUG("entry.finalize", "xact must balance = " << p);
      if (! p.is_null()) {
	if (p.keep_precision()) {
	  // If the amount was a cost, it very likely has the "keep_precision"
	  // flag set, meaning commodity display precision is ignored when
	  // displaying the amount.  We never want this set for the balance,
	  // so we must clear the flag in a temporary to avoid it propagating
	  // into the balance.
	  add_or_set_value(balance, p.rounded());
	} else {
	  add_or_set_value(balance, p);
	}
      } else {
	if (null_xact)
	  throw_(std::logic_error,
		 "Only one xact with null amount allowed per entry");
	else
	  null_xact = xact;
      }
    }
  }
  assert(balance.valid());

  DEBUG("entry.finalize", "initial balance = " << balance);

  // If there is only one xact, balance against the default account if one has
  // been set.

  if (journal && journal->basket && xacts.size() == 1 && ! balance.is_null()) {
    // jww (2008-07-24): Need to make the rest of the code aware of what to do
    // when it sees a generated xact.
    null_xact = new xact_t(journal->basket, ITEM_GENERATED);
    null_xact->_state = (*xacts.begin())->_state;
    add_xact(null_xact);
  }

  if (null_xact != NULL) {
    // If one xact has no value at all, its value will become the inverse of
    // the rest.  If multiple commodities are involved, multiple xacts are
    // generated to balance them all.

    if (balance.is_balance()) {
      bool first = true;
      const balance_t& bal(balance.as_balance());
      foreach (const balance_t::amounts_map::value_type& pair, bal.amounts) {
	if (first) {
	  null_xact->amount = pair.second.negated();
	  first = false;
	} else {
	  add_xact(new xact_t(null_xact->account, pair.second.negated(),
			      ITEM_GENERATED));
	}
      }
    }
    else if (balance.is_amount()) {
      null_xact->amount = balance.as_amount().negated();
      null_xact->add_flags(XACT_CALCULATED);
    }
    else if (! balance.is_null() && ! balance.is_realzero()) {
      throw_(balance_error, "Entry does not balance");
    }
    balance = NULL_VALUE;

  }
  else if (balance.is_balance() &&
	   balance.as_balance().amounts.size() == 2) {
    // When an entry involves two different commodities (regardless of how
    // many xacts there are) determine the conversion ratio by dividing the
    // total value of one commodity by the total value of the other.  This
    // establishes the per-unit cost for this xact for both commodities.

    DEBUG("entry.finalize", "there were exactly two commodities");

    bool     saw_cost = false;
    xact_t * top_xact = NULL;

    foreach (xact_t * xact, xacts) {
      if (! xact->amount.is_null())
	if (xact->amount.is_annotated())
	  top_xact = xact;
	else if (! top_xact)
	  top_xact = xact;

      if (xact->cost) {
	saw_cost = true;
	break;
      }
    }

    if (! saw_cost && top_xact) {
      const balance_t& bal(balance.as_balance());

      DEBUG("entry.finalize", "there were no costs, and a valid top_xact");

      balance_t::amounts_map::const_iterator a = bal.amounts.begin();
    
      const amount_t * x = &(*a++).second;
      const amount_t * y = &(*a++).second;

      if (x->commodity() != top_xact->amount.commodity()) {
	const amount_t * t = x;
	x = y;
	y = t;
      }

      DEBUG("entry.finalize", "primary   amount = " << *y);
      DEBUG("entry.finalize", "secondary amount = " << *x);

      commodity_t& comm(x->commodity());
      amount_t	   per_unit_cost;
      amount_t	   total_cost;

      foreach (xact_t * xact, xacts) {
	if (xact != top_xact && xact->must_balance() &&
	    ! xact->amount.is_null() &&
	    xact->amount.is_annotated() &&
	    xact->amount.annotation().price) {
	  amount_t temp = *xact->amount.annotation().price * xact->amount;
	  if (total_cost.is_null()) {
	    total_cost = temp;
	    y = &total_cost;
	  } else {
	    total_cost += temp;
	  }
	  DEBUG("entry.finalize", "total_cost = " << total_cost);
	}
      }
      per_unit_cost = (*y / *x).abs();

      DEBUG("entry.finalize", "per_unit_cost = " << per_unit_cost);

      foreach (xact_t * xact, xacts) {
	const amount_t& amt(xact->amount);

	if (xact->must_balance() && amt.commodity() == comm) {
	  balance -= amt;
	  xact->cost = per_unit_cost * amt;
	  balance += *xact->cost;

	  DEBUG("entry.finalize", "set xact->cost to = " << *xact->cost);
	}
      }
    }

    DEBUG("entry.finalize", "resolved balance = " << balance);
  }

  // Now that the xact list has its final form, calculate the balance once
  // more in terms of total cost, accounting for any possible gain/loss
  // amounts.

  foreach (xact_t * xact, xacts) {
    if (xact->cost) {
      if (xact->amount.commodity() == xact->cost->commodity())
	throw_(balance_error, "Transaction's cost must be of a different commodity");

      commodity_t::cost_breakdown_t breakdown =
	commodity_t::exchange(xact->amount, *xact->cost, false,
			      datetime_t(date(), time_duration(0, 0, 0, 0)));

      if (xact->amount.is_annotated() &&
	  breakdown.basis_cost.commodity() ==
	  breakdown.final_cost.commodity())
	add_or_set_value(balance, (breakdown.basis_cost -
				   breakdown.final_cost).rounded());
      else
	xact->amount = breakdown.amount;
    }
  }

  DEBUG("entry.finalize", "final balance = " << balance);

  if (! balance.is_null() && ! balance.is_zero()) {
    add_error_context(item_context(*this, "While balancing entry"));
    add_error_context("Unbalanced remainder is:");
    add_error_context(value_context(balance));
    throw_(balance_error, "Entry does not balance");
  }

  // Add the final calculated totals each to their related account

  if (dynamic_cast<entry_t *>(this)) {
    bool all_null = true;
    foreach (xact_t * xact, xacts) {
      if (! xact->amount.is_null()) {
	all_null = false;

	// jww (2008-08-09): For now, this feature only works for non-specific
	// commodities.
	add_or_set_value(xact->account->xdata().value, xact->amount);

	DEBUG("entry.finalize.totals",
	      "Total for " << xact->account->fullname() << " + "
	      << xact->amount << ": " << xact->account->xdata().value);
      }
    }
    if (all_null)
      return false;		// ignore this entry completely
  }

  return true;
}

entry_t::entry_t(const entry_t& e)
  : entry_base_t(e), code(e.code), payee(e.payee)
{
  TRACE_CTOR(entry_t, "copy");
}

void entry_t::add_xact(xact_t * xact)
{
  xact->entry = this;
  entry_base_t::add_xact(xact);
}

namespace {
  value_t get_code(entry_t& entry) {
    if (entry.code)
      return string_value(*entry.code);
    else
      return string_value(empty_string);
  }

  value_t get_payee(entry_t& entry) {
    return string_value(entry.payee);
  }

  template <value_t (*Func)(entry_t&)>
  value_t get_wrapper(call_scope_t& scope) {
    return (*Func)(find_scope<entry_t>(scope));
  }
}

expr_t::ptr_op_t entry_t::lookup(const string& name)
{
  switch (name[0]) {
  case 'c':
    if (name == "code")
      return WRAP_FUNCTOR(get_wrapper<&get_code>);
    break;

  case 'p':
    if (name[1] == '\0' || name == "payee")
      return WRAP_FUNCTOR(get_wrapper<&get_payee>);
    break;
  }

  return item_t::lookup(name);
}

bool entry_t::valid() const
{
  if (! _date || ! journal) {
    DEBUG("ledger.validate", "entry_t: ! _date || ! journal");
    return false;
  }

  foreach (xact_t * xact, xacts)
    if (xact->entry != this || ! xact->valid()) {
      DEBUG("ledger.validate", "entry_t: xact not valid");
      return false;
    }

  return true;
}

void auto_entry_t::extend_entry(entry_base_t& entry, bool post)
{
  xacts_list initial_xacts(entry.xacts.begin(), entry.xacts.end());

  foreach (xact_t * initial_xact, initial_xacts) {
    if (! initial_xact->has_flags(XACT_AUTO) && predicate(*initial_xact)) {
      foreach (xact_t * xact, xacts) {
	amount_t amt;
	assert(xact->amount);
	if (! xact->amount.commodity()) {
	  if (! post)
	    continue;
	  assert(initial_xact->amount);
	  amt = initial_xact->amount * xact->amount;
	} else {
	  if (post)
	    continue;
	  amt = xact->amount;
	}

	IF_DEBUG("entry.extend") {
	  DEBUG("entry.extend",
		"Initial xact on line " << initial_xact->beg_line << ": "
		<< "amount " << initial_xact->amount << " (precision "
		<< initial_xact->amount.precision() << ")");

	  if (initial_xact->amount.keep_precision())
	    DEBUG("entry.extend", "  precision is kept");

	  DEBUG("entry.extend",
		"Transaction on line " << xact->beg_line << ": "
		<< "amount " << xact->amount << ", amt " << amt
		<< " (precision " << xact->amount.precision()
		<< " != " << amt.precision() << ")");

	  if (xact->amount.keep_precision())
	    DEBUG("entry.extend", "  precision is kept");
	  if (amt.keep_precision())
	    DEBUG("entry.extend", "  amt precision is kept");
	}

	account_t * account  = xact->account;
	string fullname = account->fullname();
	assert(! fullname.empty());
	if (fullname == "$account" || fullname == "@account")
	  account = initial_xact->account;

	// Copy over details so that the resulting xact is a mirror of
	// the automated entry's one.
	xact_t * new_xact = new xact_t(account, amt);
	new_xact->copy_details(*xact);
	new_xact->add_flags(XACT_AUTO);

	entry.add_xact(new_xact);
      }
    }
  }
}

void extend_entry_base(journal_t * journal, entry_base_t& base, bool post)
{
  foreach (auto_entry_t * entry, journal->auto_entries)
    entry->extend_entry(base, post);
}

} // namespace ledger
