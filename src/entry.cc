/*
 * Copyright (c) 2003-2008, John Wiegley.  All rights reserved.
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
#include "format.h"
#include "session.h"
#include "report.h"

namespace ledger {

entry_base_t::entry_base_t(const entry_base_t& e)
  : supports_flags<>(), journal(NULL),
    beg_pos(0), beg_line(0), end_pos(0), end_line(0)
{
  TRACE_CTOR(entry_base_t, "copy");
  xacts.insert(xacts.end(), e.xacts.begin(), e.xacts.end());
}

entry_base_t::~entry_base_t()
{
  TRACE_DTOR(entry_base_t);

  foreach (xact_t * xact, xacts) {
    // If the transaction is a temporary, it will be destructed when the
    // temporary is.  If it's from a binary cache, we can safely destruct it
    // but its memory will be deallocated with the cache.
    if (! xact->has_flags(XACT_TEMP)) {
      if (! xact->has_flags(XACT_IN_CACHE))
	checked_delete(xact);
      else
	xact->~xact_t();
    }
  }
}

void entry_base_t::add_xact(xact_t * xact)
{
  xacts.push_back(xact);
}

bool entry_base_t::remove_xact(xact_t * xact)
{
  xacts.remove(xact);
  return true;
}

bool entry_base_t::finalize()
{
  // Scan through and compute the total balance for the entry.  This is used
  // for auto-calculating the value of entries with no cost, and the per-unit
  // price of unpriced commodities.

  // (let ((balance 0)
  //       null-xact)

  value_t  balance;
  xact_t * null_xact = NULL;

  //   (do-xacts (xact entry)
  //     (when (xact-must-balance-p xact)
  //       (let ((amt (xact-amount* xact)))
  //         (if amt
  //             (setf balance (add balance (or (xact-cost xact) amt)))
  //             (if null-xact
  //                 (error "Only one xact with null amount allowed ~
  //                         per entry (beg ~S end ~S)"
  //                        (item-position-begin-line (entry-position entry))
  //                        (item-position-end-line (entry-position entry)))
  //                 (setf null-xact xact))))))
  //

  foreach (xact_t * xact, xacts) {
    if (xact->must_balance()) {
      amount_t& p(xact->cost ? *xact->cost : xact->amount);
      if (! p.is_null()) {
	add_or_set_value(balance, p);
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

  DEBUG("ledger.journal.finalize", "initial balance = " << balance);

  // If there is only one xact, balance against the default account if
  // one has been set.

  //   (when (= 1 (length (entry-xacts entry)))
  //     (if-let ((default-account
  //                  (journal-default-account (entry-journal entry))))
  //       (setf null-xact
  //             (make-xact :entry entry
  //                               :status (xact-status
  //                                        (first (entry-xacts entry)))
  //                               :account default-account
  //                               :generatedp t))
  //       (add-xact entry null-xact)))

  if (journal && journal->basket && xacts.size() == 1) {
    // jww (2008-07-24): Need to make the rest of the code aware of what to do
    // when it sees a generated xact.
    null_xact = new xact_t(journal->basket, XACT_GENERATED);
    null_xact->state = (*xacts.begin())->state;
    add_xact(null_xact);
  }

  if (null_xact != NULL) {
    // If one xact has no value at all, its value will become the
    // inverse of the rest.  If multiple commodities are involved, multiple
    // xacts are generated to balance them all.

    // (progn
    //   (if (balance-p balance)
    //       (let ((first t))
    //         (dolist (amount (balance-amounts balance))
    //           (if first
    //               (setf (xact-amount* null-xact) (negate amount)
    //                     first nil)
    //               (add-xact
    //                entry
    //                (make-xact :entry entry
    //                                  :account (xact-account null-xact)
    //                                  :amount (negate amount)
    //                                  :generatedp t)))))
    //       (setf (xact-amount* null-xact) (negate balance)
    //             (xact-calculatedp null-xact) t))
    //
    //   (setf balance 0))

    if (balance.is_balance()) {
      bool first = true;
      const balance_t& bal(balance.as_balance());
      foreach (const balance_t::amounts_map::value_type& pair, bal.amounts) {
	if (first) {
	  null_xact->amount = pair.second.negate();
	  first = false;
	} else {
	  add_xact(new xact_t(null_xact->account, pair.second.negate(),
			      XACT_GENERATED));
	}
      }
    } else {
      null_xact->amount = balance.as_amount().negate();
      null_xact->add_flags(XACT_CALCULATED);
    }
    balance = NULL_VALUE;

  }
  else if (balance.is_balance() &&
	   balance.as_balance().amounts.size() == 2) {
    // When an entry involves two different commodities (regardless of how
    // many xacts there are) determine the conversion ratio by dividing
    // the total value of one commodity by the total value of the other.  This
    // establishes the per-unit cost for this xact for both
    // commodities.

    // (when (and (balance-p balance)
    //            (= 2 (balance-commodity-count balance)))
    //   (destructuring-bind (x y) (balance-amounts balance)
    //     (let ((a-commodity (amount-commodity x))
    //           (per-unit-cost (value-abs (divide x y))))
    //       (do-xacts (xact entry)
    //         (let ((amount (xact-amount* xact)))
    //           (unless (or (xact-cost xact)
    //                       (not (xact-must-balance-p xact))
    //                       (commodity-equal (amount-commodity amount)
    //                                        a-commodity))
    //             (setf balance (subtract balance amount)
    //                   (xact-cost xact) (multiply per-unit-cost amount)
    //                   balance (add balance (xact-cost xact))))))))))

    const balance_t& bal(balance.as_balance());

    balance_t::amounts_map::const_iterator a = bal.amounts.begin();
    
    const amount_t& x((*a++).second);
    const amount_t& y((*a++).second);

    if (! y.is_realzero()) {
      amount_t per_unit_cost = (x / y).abs();

      commodity_t& comm(x.commodity());

      foreach (xact_t * xact, xacts) {
	const amount_t& x_amt(xact->amount);

	if (! (xact->cost ||
	       ! xact->must_balance() ||
	       x_amt.commodity() == comm)) {
	  DEBUG("ledger.journal.finalize", "before operation 1 = " << balance);
	  balance -= x_amt;
	  DEBUG("ledger.journal.finalize", "after operation 1 = " << balance);
	  DEBUG("ledger.journal.finalize", "x_amt = " << x_amt);
	  DEBUG("ledger.journal.finalize", "per_unit_cost = " << per_unit_cost);

	  xact->cost = per_unit_cost * x_amt;
	  DEBUG("ledger.journal.finalize", "*xact->cost = " << *xact->cost);

	  balance += *xact->cost;
	  DEBUG("ledger.journal.finalize", "after operation 2 = " << balance);
	}

      }
    }

    DEBUG("ledger.journal.finalize", "resolved balance = " << balance);
  }

  // Now that the xact list has its final form, calculate the balance
  // once more in terms of total cost, accounting for any possible gain/loss
  // amounts.

  // (do-xacts (xact entry)
  //   (when (xact-cost xact)
  //     (let ((amount (xact-amount* xact)))
  //       (assert (not (commodity-equal (amount-commodity amount)
  //                                     (amount-commodity (xact-cost xact)))))
  //       (multiple-value-bind (annotated-amount total-cost basis-cost)
  //           (exchange-commodity amount :total-cost (xact-cost xact)
  //                               :moment (entry-date entry)
  //                               :tag (entry-code entry))
  //         (if (annotated-commodity-p (amount-commodity amount))
  //             (if-let ((price (annotation-price
  //                              (commodity-annotation
  //                               (amount-commodity amount)))))
  //               (setf balance
  //                     (add balance (subtract basis-cost total-cost))))
  //             (setf (xact-amount* xact) annotated-amount))))))

  foreach (xact_t * xact, xacts) {
    if (xact->cost) {
      const amount_t& x_amt(xact->amount);

      assert(x_amt.commodity() != xact->cost->commodity());

      entry_t * entry = dynamic_cast<entry_t *>(this);

      // jww (2008-07-24): Pass the entry's code here if we can, as the
      // auto-tag
      amount_t final_cost;
      amount_t basis_cost;
      amount_t ann_amount =
	commodity_t::exchange(x_amt, final_cost, basis_cost, xact->cost, none,
			      datetime_t(xact->actual_date(),
					 time_duration_t(0, 0, 0)),
			      entry ? entry->code : optional<string>());

      if (xact->amount.is_annotated()) {
	if (ann_amount.annotation().price)
	  add_or_set_value(balance, basis_cost - final_cost);
      } else {
	xact->amount = ann_amount;
      }
    }
  }

  DEBUG("ledger.journal.finalize", "final balance = " << balance);

  // (if (value-zerop balance)
  //     (prog1
  //         entry
  //       (setf (entry-normalizedp entry) t))
  //     (error "Entry does not balance (beg ~S end ~S); remaining balance is:~%~A"
  //            (item-position-begin-line (entry-position entry))
  //            (item-position-end-line (entry-position entry))
  //            (format-value balance :width 20)))

  if (! balance.is_null()) {
    balance.round();
    if (! balance.is_zero()) {
#if 0
      error * err =
	new balance_error("Entry does not balance",
			  new entry_context(*this, "While balancing entry:"));
      err->context.push_front
	(new value_context(balance, "Unbalanced remainder is:"));
      throw err;
#endif
    }
  }

  // Add the final calculated totals each to their related account

  if (dynamic_cast<entry_t *>(this)) {
    foreach (xact_t * xact, xacts) {
      // jww (2008-08-09): For now, this feature only works for
      // non-specific commodities.
      add_or_set_value(xact->account->xdata().value,
		       xact->amount.strip_annotations());
    }
  }

  return true;
}

entry_t::entry_t(const entry_t& e)
  : entry_base_t(e), scope_t(), _date(e._date), _date_eff(e._date_eff),
    code(e.code), payee(e.payee)
{
  TRACE_CTOR(entry_t, "copy");

  foreach (xact_t * xact, xacts)
    xact->entry = this;
}

bool entry_t::get_state(xact_t::state_t * state) const
{
  bool first  = true;
  bool hetero = false;

  foreach (xact_t * xact, xacts) {
    if (first) {
      *state = xact->state;
      first = false;
    }
    else if (*state != xact->state) {
      hetero = true;
      break;
    }
  }

  return ! hetero;
}

void entry_t::add_xact(xact_t * xact)
{
  xact->entry = this;
  entry_base_t::add_xact(xact);
}

namespace {
  value_t get_date(entry_t& entry) {
    return entry.date();
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
  case 'd':
    if (name[1] == '\0' || name == "date")
      return WRAP_FUNCTOR(get_wrapper<&get_date>);
    break;

  case 'f':
    if (name.find("fmt_") == 0) {
      switch (name[4]) {
      case 'D':
	return WRAP_FUNCTOR(get_wrapper<&get_date>);
      case 'P':
	return WRAP_FUNCTOR(get_wrapper<&get_payee>);
      }
    }
    break;

  case 'p':
    if (name[1] == '\0' || name == "payee")
      return WRAP_FUNCTOR(get_wrapper<&get_payee>);
    break;
  }
  return journal->owner->current_report->lookup(name);
}

bool entry_t::valid() const
{
  if (! is_valid(_date) || ! journal) {
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

#if 0
void entry_context::describe(std::ostream& out) const throw()
{
  if (! desc.empty())
    out << desc << std::endl;

  print_entry(out, entry, "  ");
}
#endif

void auto_entry_t::extend_entry(entry_base_t& entry, bool post)
{
  xacts_list initial_xacts(entry.xacts.begin(),
				  entry.xacts.end());

  foreach (xact_t * initial_xact, initial_xacts) {
    if (predicate(*initial_xact)) {
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

	account_t * account  = xact->account;
	string fullname = account->fullname();
	assert(! fullname.empty());
	if (fullname == "$account" || fullname == "@account")
	  account = initial_xact->account;

	xact_t * new_xact
	  = new xact_t(account, amt, xact->flags() | XACT_AUTO);

	// Copy over details so that the resulting xact is a mirror of
	// the automated entry's one.
	new_xact->state	    = xact->state;
	new_xact->_date	    = xact->_date;
	new_xact->_date_eff = xact->_date_eff;
	new_xact->note	    = xact->note;
	new_xact->beg_pos   = xact->beg_pos;
	new_xact->beg_line  = xact->beg_line;
	new_xact->end_pos   = xact->end_pos;
	new_xact->end_line  = xact->end_line;

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
