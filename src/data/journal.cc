/*
 * Copyright (c) 2003-2007, John Wiegley.  All rights reserved.
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
#include "xpath.h"
#include "mask.h"

namespace ledger {

const string version = PACKAGE_VERSION;

bool transaction_t::use_effective_date = false;

transaction_t::~transaction_t()
{
  TRACE_DTOR(transaction_t);
}

moment_t transaction_t::actual_date() const
{
  if (! _date && entry)
    return entry->actual_date();
  return *_date;
}

moment_t transaction_t::effective_date() const
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

bool entry_base_t::finalize()
{
  // Scan through and compute the total balance for the entry.  This
  // is used for auto-calculating the value of entries with no cost,
  // and the per-unit price of unpriced commodities.

  value_t balance;
  bool	  no_amounts = true;
  bool	  saw_null   = false;

  for (transactions_list::const_iterator x = transactions.begin();
       x != transactions.end();
       x++)
    if (! (*x)->has_flags(TRANSACTION_VIRTUAL) ||
	(*x)->has_flags(TRANSACTION_BALANCE)) {
      amount_t& p((*x)->cost ? *(*x)->cost : (*x)->amount);
      if (p) {
	if (no_amounts) {
	  balance = p;
	  no_amounts = false;
	} else {
	  balance += p;
	}

	assert((*x)->amount);
	if ((*x)->cost && (*x)->amount.commodity().annotated) {
	  annotated_commodity_t&
	    ann_comm(static_cast<annotated_commodity_t&>
		     ((*x)->amount.commodity()));
	  if (ann_comm.details.price)
	    balance += (*ann_comm.details.price * (*x)->amount.number() -
			*((*x)->cost));
	}
      } else {
	saw_null = true;
      }
    }

  // If it's a null entry, then let the user have their fun
  if (no_amounts)
    return true;

  // If there is only one transaction, balance against the basket
  // account if one has been set.

  if (journal && journal->basket && transactions.size() == 1) {
    assert(balance.is_amount());
    transaction_t * nxact = new transaction_t(journal->basket);
    // The amount doesn't need to be set because the code below will
    // balance this transaction against the other.
    add_transaction(nxact);
    nxact->add_flags(TRANSACTION_CALCULATED);
  }

  // If the first transaction of a two-transaction entry is of a
  // different commodity than the other, and it has no per-unit price,
  // determine its price by dividing the unit count into the value of
  // the balance.  This is done for the last eligible commodity.

  if (! saw_null && balance && balance.is_balance() &&
      balance.as_balance().amounts.size() == 2) {
    transactions_list::const_iterator x = transactions.begin();
    assert((*x)->amount);
    commodity_t& this_comm = (*x)->amount.commodity();

    balance_t::amounts_map::const_iterator this_bal =
      balance.as_balance().amounts.find(&this_comm);
    balance_t::amounts_map::const_iterator other_bal =
      balance.as_balance().amounts.begin();
    if (this_bal == other_bal)
      other_bal++;

    amount_t per_unit_cost =
      amount_t((*other_bal).second / (*this_bal).second.number()).unround();

    for (; x != transactions.end(); x++) {
      if ((*x)->cost || (*x)->has_flags(TRANSACTION_VIRTUAL) ||
	  (*x)->amount.commodity() != this_comm)
	continue;

      balance -= (*x)->amount;

      entry_t * entry = dynamic_cast<entry_t *>(this);

      if ((*x)->amount.commodity() &&
	  ! (*x)->amount.commodity().annotated)
	(*x)->amount.annotate_commodity
	  (annotation_t(per_unit_cost.abs(),
			entry ? entry->actual_date() : optional<moment_t>(),
			entry ? entry->code          : optional<string>()));

      (*x)->cost = - (per_unit_cost * (*x)->amount.number());
      balance += *(*x)->cost;
    }
  }

  // Walk through each of the transactions, fixing up any that we
  // can, and performing any on-the-fly calculations.

  bool empty_allowed = true;

  for (transactions_list::const_iterator x = transactions.begin();
       x != transactions.end();
       x++) {
    if ((*x)->amount ||
	((*x)->has_flags(TRANSACTION_VIRTUAL) &&
	 ! (*x)->has_flags(TRANSACTION_BALANCE)))
      continue;

    if (! empty_allowed)
      throw_(std::logic_error,
	     "Only one transaction with null amount allowed per entry");
    empty_allowed = false;

    // If one transaction gives no value at all, its value will become
    // the inverse of the value of the others.  If multiple
    // commodities are involved, multiple transactions will be
    // generated to balance them all.

    const balance_t * bal = NULL;
    switch (balance.type()) {
    case value_t::BALANCE_PAIR:
      bal = &balance.as_balance_pair().quantity();
      // fall through...

    case value_t::BALANCE:
      if (! bal)
	bal = &balance.as_balance();

      if (bal->amounts.size() < 2) {
	balance.cast(value_t::AMOUNT);
      } else {
	bool first = true;
	for (balance_t::amounts_map::const_iterator
	       i = bal->amounts.begin();
	     i != bal->amounts.end();
	     i++) {
	  amount_t amt = (*i).second.negate();

	  if (first) {
	    (*x)->amount = amt;
	    first = false;
	  } else {
	    transaction_t * nxact = new transaction_t((*x)->account);
	    add_transaction(nxact);
	    nxact->add_flags(TRANSACTION_CALCULATED);
	    nxact->amount = amt;
	  }

	  balance += amt;
	}
	break;
      }
      // fall through...

    case value_t::AMOUNT:
      (*x)->amount = balance.as_amount().negate();
      (*x)->add_flags(TRANSACTION_CALCULATED);

      balance += (*x)->amount;
      break;

    default:
      break;
    }
  }

  if (balance) {
#if 1
    throw_(balance_error, "Entry does not balance");
#else
    error * err =
      new balance_error("Entry does not balance",
			new entry_context(*this, "While balancing entry:"));
    err->context.push_front
      (new value_context(balance, "Unbalanced remainder is:"));
    throw err;
#endif
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
  if (! is_valid_moment(_date) || ! journal) {
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

auto_entry_t::auto_entry_t()
{
  TRACE_CTOR(auto_entry_t, "");
}

auto_entry_t::auto_entry_t(const string& _predicate)
  : predicate(new xml::xpath_t(_predicate))
{
  TRACE_CTOR(auto_entry_t, "const string&");
}

auto_entry_t::~auto_entry_t() {
  TRACE_DTOR(auto_entry_t);
}

void auto_entry_t::extend_entry(entry_base_t& entry, bool post)
{
#if 0
  transactions_list initial_xacts(entry.transactions.begin(),
				  entry.transactions.end());

  for (transactions_list::iterator i = initial_xacts.begin();
       i != initial_xacts.end();
       i++) {
    // jww (2006-09-10): Create a scope here based on entry
    if (predicate->calc((xml::node_t *) NULL)) {
      for (transactions_list::iterator t = transactions.begin();
	   t != transactions.end();
	   t++) {
	amount_t amt;
	assert((*t)->amount);
	if (! (*t)->amount.commodity()) {
	  if (! post)
	    continue;
	  assert((*i)->amount);
	  amt = *(*i)->amount * *(*t)->amount;
	} else {
	  if (post)
	    continue;
	  amt = *(*t)->amount;
	}

	account_t * account  = (*t)->account;
	string fullname = account->fullname();
	assert(! fullname.empty());
	if (fullname == "$account" || fullname == "@account")
	  account = (*i)->account;

	transaction_t * xact
	  = new transaction_t(account, amt, (*t)->flags() | TRANSACTION_AUTO);
	entry.add_transaction(xact);
      }
    }
  }
#endif
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
    account->journal = journal;

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

static inline
account_t * find_account_re_(account_t * account, const mask_t& regexp)
{
  if (regexp.match(account->fullname()))
    return account;

  for (accounts_map::iterator i = account->accounts.begin();
       i != account->accounts.end();
       i++)
    if (account_t * a = find_account_re_((*i).second, regexp))
      return a;

  return NULL;
}

account_t * journal_t::find_account_re(const string& regexp)
{
  return find_account_re_(master, mask_t(regexp));
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
  if (depth > 256 || ! journal) {
    DEBUG("ledger.validate", "account_t: depth > 256 || ! journal");
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

journal_t::~journal_t()
{
  TRACE_DTOR(journal_t);

  assert(master);
  checked_delete(master);

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

void print_entry(std::ostream& out, const entry_base_t& entry_base,
		 const string& prefix)
{
  string print_format;

  if (dynamic_cast<const entry_t *>(&entry_base)) {
    print_format = (prefix + "%D %X%C%P\n" +
		    prefix + "    %-34A  %12o\n%/" +
		    prefix + "    %-34A  %12o\n");
  }
  else if (const auto_entry_t * entry =
	   dynamic_cast<const auto_entry_t *>(&entry_base)) {
    out << "= " << entry->predicate->expr << '\n';
    print_format = prefix + "    %-34A  %12o\n";
  }
  else if (const period_entry_t * entry =
	   dynamic_cast<const period_entry_t *>(&entry_base)) {
    out << "~ " << entry->period_string << '\n';
    print_format = prefix + "    %-34A  %12o\n";
  }
  else {
    assert(false);
  }

#if 0
  format_entries formatter(out, print_format);
  walk_transactions(const_cast<transactions_list&>(entry_base.transactions),
		    formatter);
  formatter.flush();

  clear_transaction_xdata cleaner;
  walk_transactions(const_cast<transactions_list&>(entry_base.transactions),
		    cleaner);
#endif
}

#if 0
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
  const ledger::strings_list& sources(xact.entry->journal->sources);
  unsigned int x = 0;
  for (ledger::strings_list::const_iterator i = sources.begin();
       i != sources.end();
       i++, x++)
    if (x == xact.entry->src_idx) {
      file = *i;
      break;
    }
  line = xact.beg_line;
}
#endif

} // namespace ledger
