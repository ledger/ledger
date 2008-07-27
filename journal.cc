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

#include "journal.h"
#include "format.h"
#include "session.h"

namespace ledger {

const string version = PACKAGE_VERSION;

bool transaction_t::use_effective_date = false;

transaction_t::~transaction_t()
{
  TRACE_DTOR(transaction_t);
}

datetime_t transaction_t::actual_date() const
{
  if (! _date && entry)
    return entry->actual_date();
  return *_date;
}

datetime_t transaction_t::effective_date() const
{
  if (! _date_eff && entry)
    return entry->effective_date();
  return *_date_eff;
}

bool transaction_t::valid() const
{
  if (! entry) {
    DEBUG("ledger.validate", "transaction_t: ! entry");
    return false;
  }

  if (state != UNCLEARED && state != CLEARED && state != PENDING) {
    DEBUG("ledger.validate", "transaction_t: state is bad");
    return false;
  }

  transactions_list::const_iterator i =
    std::find(entry->transactions.begin(),
	      entry->transactions.end(), this);
  if (i == entry->transactions.end()) {
    DEBUG("ledger.validate", "transaction_t: ! found");
    return false;
  }

  if (! account) {
    DEBUG("ledger.validate", "transaction_t: ! account");
    return false;
  }

  if (! amount.valid()) {
    DEBUG("ledger.validate", "transaction_t: ! amount.valid()");
    return false;
  }

  if (cost && ! cost->valid()) {
    DEBUG("ledger.validate", "transaction_t: cost && ! cost->valid()");
    return false;
  }

  if (flags() & ~0x003f) {
    DEBUG("ledger.validate", "transaction_t: flags are bad");
    return false;
  }

  return true;
}

void entry_base_t::add_transaction(transaction_t * xact)
{
  transactions.push_back(xact);
}

bool entry_base_t::remove_transaction(transaction_t * xact)
{
  transactions.remove(xact);
  return true;
}

// jww (2008-04-20): Migrate the Common Lisp version here!

bool entry_base_t::finalize()
{
  // Scan through and compute the total balance for the entry.  This is used
  // for auto-calculating the value of entries with no cost, and the per-unit
  // price of unpriced commodities.

  // (let ((balance 0)
  //       null-xact)

  value_t	  balance;
  transaction_t * null_xact = NULL;

  //   (do-transactions (xact entry)
  //     (when (xact-must-balance-p xact)
  //       (let ((amt (xact-amount* xact)))
  //         (if amt
  //             (setf balance (add balance (or (xact-cost xact) amt)))
  //             (if null-xact
  //                 (error "Only one transaction with null amount allowed ~
  //                         per entry (beg ~S end ~S)"
  //                        (item-position-begin-line (entry-position entry))
  //                        (item-position-end-line (entry-position entry)))
  //                 (setf null-xact xact))))))
  //

  for (transactions_list::const_iterator x = transactions.begin();
       x != transactions.end();
       x++) {
    if ((*x)->must_balance()) {
      amount_t& p((*x)->cost ? *(*x)->cost : (*x)->amount);
      if (! p.is_null()) {
	if (balance.is_null())
	  balance = p;
	else
	  balance += p;
      } else {
	if (null_xact)
	  throw_(std::logic_error,
		 "Only one transaction with null amount allowed per entry");
	else
	  null_xact = *x;
      }
    }
  }
  assert(balance.valid());

  DEBUG("ledger.journal.finalize", "initial balance = " << balance);

  // If there is only one transaction, balance against the default account if
  // one has been set.

  //   (when (= 1 (length (entry-transactions entry)))
  //     (if-let ((default-account
  //                  (journal-default-account (entry-journal entry))))
  //       (setf null-xact
  //             (make-transaction :entry entry
  //                               :status (xact-status
  //                                        (first (entry-transactions entry)))
  //                               :account default-account
  //                               :generatedp t))
  //       (add-transaction entry null-xact)))

  if (journal && journal->basket && transactions.size() == 1) {
    // jww (2008-07-24): Need to make the rest of the code aware of what to do
    // when it sees a generated transaction.
    null_xact = new transaction_t(journal->basket, TRANSACTION_GENERATED);
    null_xact->state = (*transactions.begin())->state;
    add_transaction(null_xact);
  }

  if (null_xact != NULL) {
    // If one transaction has no value at all, its value will become the
    // inverse of the rest.  If multiple commodities are involved, multiple
    // transactions are generated to balance them all.

    // (progn
    //   (if (balance-p balance)
    //       (let ((first t))
    //         (dolist (amount (balance-amounts balance))
    //           (if first
    //               (setf (xact-amount* null-xact) (negate amount)
    //                     first nil)
    //               (add-transaction
    //                entry
    //                (make-transaction :entry entry
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
      for (balance_t::amounts_map::const_iterator i = bal.amounts.begin();
	   i != bal.amounts.end();
	   i++) {
	if (first) {
	  null_xact->amount = (*i).second.negate();
	  first = false;
	} else {
	  add_transaction(new transaction_t(null_xact->account,
					    (*i).second.negate(),
					    TRANSACTION_GENERATED));
	}
      }
    } else {
      null_xact->amount = balance.as_amount().negate();
      null_xact->add_flags(TRANSACTION_CALCULATED);
    }
    balance = NULL_VALUE;

  }
  else if (balance.is_balance() &&
	   balance.as_balance().amounts.size() == 2) {
    // When an entry involves two different commodities (regardless of how
    // many transactions there are) determine the conversion ratio by dividing
    // the total value of one commodity by the total value of the other.  This
    // establishes the per-unit cost for this transaction for both
    // commodities.

    // (when (and (balance-p balance)
    //            (= 2 (balance-commodity-count balance)))
    //   (destructuring-bind (x y) (balance-amounts balance)
    //     (let ((a-commodity (amount-commodity x))
    //           (per-unit-cost (value-abs (divide x y))))
    //       (do-transactions (xact entry)
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

      for (transactions_list::const_iterator x = transactions.begin();
	   x != transactions.end();
	   x++) {
	const amount_t& x_amt((*x)->amount);

	if (! ((*x)->cost ||
	       ! (*x)->must_balance() ||
	       x_amt.commodity() == comm)) {
	  DEBUG("ledger.journal.finalize", "before operation 1 = " << balance);
	  balance -= x_amt;
	  DEBUG("ledger.journal.finalize", "after operation 1 = " << balance);
	  DEBUG("ledger.journal.finalize", "x_amt = " << x_amt);
	  DEBUG("ledger.journal.finalize", "per_unit_cost = " << per_unit_cost);

	  (*x)->cost = per_unit_cost * x_amt;
	  DEBUG("ledger.journal.finalize", "*(*x)->cost = " << *(*x)->cost);

	  balance += *(*x)->cost;
	  DEBUG("ledger.journal.finalize", "after operation 2 = " << balance);
	}

      }
    }

    DEBUG("ledger.journal.finalize", "resolved balance = " << balance);
  }

  // Now that the transaction list has its final form, calculate the balance
  // once more in terms of total cost, accounting for any possible gain/loss
  // amounts.

  // (do-transactions (xact entry)
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

  for (transactions_list::const_iterator x = transactions.begin();
       x != transactions.end();
       x++) {
    if ((*x)->cost) {
      const amount_t& x_amt((*x)->amount);

      assert(x_amt.commodity() != (*x)->cost->commodity());

      entry_t * entry = dynamic_cast<entry_t *>(this);

      // jww (2008-07-24): Pass the entry's code here if we can, as the
      // auto-tag
      amount_t final_cost;
      amount_t basis_cost;
      amount_t ann_amount =
	commodity_t::exchange(x_amt, final_cost, basis_cost,
			      (*x)->cost, none, (*x)->actual_date(),
			      entry ? entry->code : optional<string>());

      if ((*x)->amount.commodity_annotated()) {
	if (ann_amount.annotation_details().price) {
	  if (balance.is_null())
	    balance = basis_cost - final_cost;
	  else
	    balance += basis_cost - final_cost;
	}
      } else {
	(*x)->amount = ann_amount;
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

  if (! balance.is_null() && ! balance.is_zero()) {
    error * err =
      new balance_error("Entry does not balance",
			new entry_context(*this, "While balancing entry:"));
    balance.round();
    err->context.push_front
      (new value_context(balance, "Unbalanced remainder is:"));
    throw err;
  }

  return true;
}

entry_t::entry_t(const entry_t& e)
  : entry_base_t(e), _date(e._date), _date_eff(e._date_eff),
    code(e.code), payee(e.payee)
{
  TRACE_CTOR(entry_t, "copy");

  for (transactions_list::const_iterator i = transactions.begin();
       i != transactions.end();
       i++)
    (*i)->entry = this;
}

bool entry_t::get_state(transaction_t::state_t * state) const
{
  bool first  = true;
  bool hetero = false;

  for (transactions_list::const_iterator i = transactions.begin();
       i != transactions.end();
       i++) {
    if (first) {
      *state = (*i)->state;
      first = false;
    }
    else if (*state != (*i)->state) {
      hetero = true;
      break;
    }
  }

  return ! hetero;
}

void entry_t::add_transaction(transaction_t * xact)
{
  xact->entry = this;
  entry_base_t::add_transaction(xact);
}

bool entry_t::valid() const
{
  if (! is_valid(_date) || ! journal) {
    DEBUG("ledger.validate", "entry_t: ! _date || ! journal");
    return false;
  }

  for (transactions_list::const_iterator i = transactions.begin();
       i != transactions.end();
       i++)
    if ((*i)->entry != this || ! (*i)->valid()) {
      DEBUG("ledger.validate", "entry_t: transaction not valid");
      return false;
    }

  return true;
}

void auto_entry_t::extend_entry(entry_base_t& entry, bool post)
{
  transactions_list initial_xacts(entry.transactions.begin(),
				  entry.transactions.end());

  for (transactions_list::iterator i = initial_xacts.begin();
       i != initial_xacts.end();
       i++) {
    if (predicate(**i)) {
      for (transactions_list::iterator t = transactions.begin();
	   t != transactions.end();
	   t++) {
	amount_t amt;
	assert((*t)->amount);
	if (! (*t)->amount.commodity()) {
	  if (! post)
	    continue;
	  assert((*i)->amount);
	  amt = (*i)->amount * (*t)->amount;
	} else {
	  if (post)
	    continue;
	  amt = (*t)->amount;
	}

	account_t * account  = (*t)->account;
	string fullname = account->fullname();
	assert(! fullname.empty());
	if (fullname == "$account" || fullname == "@account")
	  account = (*i)->account;

	transaction_t * xact
	  = new transaction_t(account, amt, (*t)->flags() | TRANSACTION_AUTO);

	// Copy over details so that the resulting transaction is a mirror of
	// the automated entry's one.
	xact->state	= (*t)->state;
	xact->_date	= (*t)->_date;
	xact->_date_eff = (*t)->_date_eff;
	xact->note	= (*t)->note;
	xact->beg_pos	= (*t)->beg_pos;
	xact->beg_line	= (*t)->beg_line;
	xact->end_pos	= (*t)->end_pos;
	xact->end_line	= (*t)->end_line;

	entry.add_transaction(xact);
      }
    }
  }
}

account_t::~account_t()
{
  TRACE_DTOR(account_t);

  for (accounts_map::iterator i = accounts.begin();
       i != accounts.end();
       i++)
    checked_delete((*i).second);
}

account_t * account_t::find_account(const string& name,
				    const bool	       auto_create)
{
  accounts_map::const_iterator i = accounts.find(name);
  if (i != accounts.end())
    return (*i).second;

  char buf[256];

  string::size_type sep = name.find(':');
  assert(sep < 256|| sep == string::npos);

  const char * first, * rest;
  if (sep == string::npos) {
    first = name.c_str();
    rest  = NULL;
  } else {
    std::strncpy(buf, name.c_str(), sep);
    buf[sep] = '\0';

    first = buf;
    rest  = name.c_str() + sep + 1;
  }

  account_t * account;

  i = accounts.find(first);
  if (i == accounts.end()) {
    if (! auto_create)
      return NULL;

    account = new account_t(this, first);
    std::pair<accounts_map::iterator, bool> result
      = accounts.insert(accounts_map::value_type(first, account));
    assert(result.second);
  } else {
    account = (*i).second;
  }

  if (rest)
    account = account->find_account(rest, auto_create);

  return account;
}

string account_t::fullname() const
{
  if (! _fullname.empty()) {
    return _fullname;
  } else {
    const account_t *	first	 = this;
    string		fullname = name;

    while (first->parent) {
      first = first->parent;
      if (! first->name.empty())
	fullname = first->name + ":" + fullname;
    }

    _fullname = fullname;

    return fullname;
  }
}

std::ostream& operator<<(std::ostream& out, const account_t& account)
{
  out << account.fullname();
  return out;
}

bool account_t::valid() const
{
  if (depth > 256) {
    DEBUG("ledger.validate", "account_t: depth > 256");
    return false;
  }

  for (accounts_map::const_iterator i = accounts.begin();
       i != accounts.end();
       i++) {
    if (this == (*i).second) {
      DEBUG("ledger.validate", "account_t: parent refers to itself!");
      return false;
    }

    if (! (*i).second->valid()) {
      DEBUG("ledger.validate", "account_t: child not valid");
      return false;
    }
  }

  return true;
}

journal_t::journal_t(session_t * _owner) :
  owner(_owner), basket(NULL), item_pool(NULL), item_pool_end(NULL)
{
  TRACE_CTOR(journal_t, "");
  master = owner->master;
}

journal_t::~journal_t()
{
  TRACE_DTOR(journal_t);

  // Don't bother unhooking each entry's transactions from the
  // accounts they refer to, because all accounts are about to
  // be deleted.
  for (entries_list::iterator i = entries.begin();
       i != entries.end();
       i++) {
    if (! item_pool ||
	reinterpret_cast<char *>(*i) <  item_pool ||
	reinterpret_cast<char *>(*i) >= item_pool_end) {
      checked_delete(*i);
    } else {
      (*i)->~entry_t();
    }
  }

  for (auto_entries_list::iterator i = auto_entries.begin();
       i != auto_entries.end();
       i++)
    if (! item_pool ||
	reinterpret_cast<char *>(*i) < item_pool ||
	reinterpret_cast<char *>(*i) >= item_pool_end)
      checked_delete(*i);
    else
      (*i)->~auto_entry_t();

  for (period_entries_list::iterator i = period_entries.begin();
       i != period_entries.end();
       i++)
    if (! item_pool ||
	reinterpret_cast<char *>(*i) < item_pool ||
	reinterpret_cast<char *>(*i) >= item_pool_end)
      checked_delete(*i);
    else
      (*i)->~period_entry_t();

  if (item_pool)
    checked_array_delete(item_pool);
}

void journal_t::add_account(account_t * acct)
{
  owner->add_account(acct);
}

bool journal_t::remove_account(account_t * acct)
{
  return owner->remove_account(acct);
}

account_t * journal_t::find_account(const string& name, bool auto_create)
{
  return owner->find_account(name, auto_create);
}

account_t * journal_t::find_account_re(const string& regexp)
{
  return owner->find_account_re(regexp);
}

bool journal_t::add_entry(entry_t * entry)
{
  entry->journal = this;

  if (! run_hooks(entry_finalize_hooks, *entry, false) ||
      ! entry->finalize() ||
      ! run_hooks(entry_finalize_hooks, *entry, true)) {
    entry->journal = NULL;
    return false;
  }

  entries.push_back(entry);

  for (transactions_list::const_iterator i = entry->transactions.begin();
       i != entry->transactions.end();
       i++)
    if ((*i)->cost) {
      assert((*i)->amount);
      (*i)->amount.commodity().add_price(entry->date(),
					 *(*i)->cost / (*i)->amount.number());
    }

  return true;
}

bool journal_t::remove_entry(entry_t * entry)
{
  bool found = false;
  entries_list::iterator i;
  for (i = entries.begin(); i != entries.end(); i++)
    if (*i == entry) {
      found = true;
      break;
    }
  if (! found)
    return false;

  entries.erase(i);
  entry->journal = NULL;

  return true;
}

bool journal_t::valid() const
{
  if (! master->valid()) {
    DEBUG("ledger.validate", "journal_t: master not valid");
    return false;
  }

  for (entries_list::const_iterator i = entries.begin();
       i != entries.end();
       i++)
    if (! (*i)->valid()) {
      DEBUG("ledger.validate", "journal_t: entry not valid");
      return false;
    }

  return true;
}

void entry_context::describe(std::ostream& out) const throw()
{
  if (! desc.empty())
    out << desc << std::endl;

  print_entry(out, entry, "  ");
}

xact_context::xact_context(const ledger::transaction_t& _xact,
			   const string& desc) throw()
  : file_context("", 0, desc), xact(_xact)
{
  const ledger::paths_list& sources(xact.entry->journal->sources);
  unsigned int x = 0;
  for (ledger::paths_list::const_iterator i = sources.begin();
       i != sources.end();
       i++, x++)
    if (x == xact.entry->src_idx) {
      file = *i;
      break;
    }
  line = xact.beg_line;
}

} // namespace ledger
