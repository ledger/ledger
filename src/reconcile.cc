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

#include "reconcile.h"

namespace ledger {

#define xact_next(x)     reinterpret_cast<xact_t *>(x->xdata().ptr)
#define xact_next_ptr(x) reinterpret_cast<xact_t **>(&x->xdata().ptr)

static bool search_for_balance(amount_t& amount,
			       xact_t ** prev, xact_t * next)
{
  for (; next; next = xact_next(next)) {
    xact_t * temp = *prev;
    *prev = next;

    amount -= next->amount;

    if (! amount) {
      *xact_next_ptr(next) = NULL;
      return true;
    }
    else if (search_for_balance(amount, xact_next_ptr(next), xact_next(next)))
    {
      return true;
    }

    amount += next->amount;

    *prev = temp;
  }
  return false;
}

void reconcile_xacts::push_to_handler(xact_t * first)
{
  for (; first; first = xact_next(first))
    item_handler<xact_t>::operator()(*first);

  item_handler<xact_t>::flush();
}

void reconcile_xacts::flush()
{
  value_t cleared_balance;
  value_t pending_balance;

  xact_t *  first    = NULL;
  xact_t ** last_ptr = &first;

  foreach (xact_t * xact, xacts) {
    if (! is_valid(cutoff) || xact->date() < cutoff) {
      switch (xact->state()) {
      case item_t::CLEARED:
	cleared_balance += xact->amount;
	break;
      case item_t::UNCLEARED:
      case item_t::PENDING:
	pending_balance += xact->amount;
	*last_ptr = xact;
	last_ptr = xact_next_ptr(xact);
	break;
      }
    }
  }

  if (cleared_balance.type() >= value_t::BALANCE)
    throw std::runtime_error("Cannot reconcile accounts with multiple commodities");

  cleared_balance.cast(value_t::AMOUNT);
  balance.cast(value_t::AMOUNT);

  commodity_t& cb_comm = cleared_balance.as_amount().commodity();
  commodity_t& b_comm  = balance.as_amount().commodity();

  balance -= cleared_balance;
  if (balance.type() >= value_t::BALANCE)
    throw_(std::runtime_error,
	   "Reconcile balance is not of the same commodity ('"
	   << b_comm.symbol() << "' != '" << cb_comm.symbol() << "')");

  // If the amount to reconcile is the same as the pending balance,
  // then assume an exact match and return the results right away.
  amount_t& to_reconcile(balance.as_amount_lval());
  pending_balance.cast(value_t::AMOUNT);
  if (to_reconcile == pending_balance.as_amount() ||
      search_for_balance(to_reconcile, &first, first)) {
    push_to_handler(first);
  } else {
    throw std::runtime_error("Could not reconcile account!");
  }
}

} // namespace ledger
