#include "journal.h"
#include "datetime.h"
#include "valexpr.h"
#include "mask.h"
#include "error.h"
#include "acconf.h"

#include <fstream>

namespace ledger {

const std::string version = PACKAGE_VERSION;

bool transaction_t::use_effective_date = false;

transaction_t::~transaction_t()
{
  DEBUG_PRINT("ledger.memory.dtors", "dtor transaction_t");
  if (cost) delete cost;
  if (amount_expr) amount_expr->release();
}

std::time_t transaction_t::actual_date() const
{
  if (_date == 0 && entry)
    return entry->actual_date();
  return _date;
}

std::time_t transaction_t::effective_date() const
{
  if (_date_eff == 0 && entry)
    return entry->effective_date();
  return _date_eff;
}

bool transaction_t::valid() const
{
  if (! entry)
    return false;

  if (state != UNCLEARED && state != CLEARED && state != PENDING)
    return false;

  bool found = false;
  for (transactions_list::const_iterator i = entry->transactions.begin();
       i != entry->transactions.end();
       i++)
    if (*i == this) {
      found = true;
      break;
    }
  if (! found)
    return false;

  if (! account)
    return false;

  if (! amount.valid())
    return false;

  if (cost && ! cost->valid())
    return false;

  if (flags & ~0x001f)
    return false;

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

  bool no_amounts = true;
  for (transactions_list::const_iterator x = transactions.begin();
       x != transactions.end();
       x++)
    if (! ((*x)->flags & TRANSACTION_VIRTUAL) ||
	((*x)->flags & TRANSACTION_BALANCE)) {
      amount_t * p = (*x)->cost ? (*x)->cost : &(*x)->amount;
      if (*p) {
	if (no_amounts) {
	  balance = *p;
	  no_amounts = false;
	} else {
	  balance += *p;
	}

	if ((*x)->cost && (*x)->amount.commodity().annotated) {
	  annotated_commodity_t&
	    ann_comm(static_cast<annotated_commodity_t&>
		     ((*x)->amount.commodity()));
	  if (ann_comm.price)
	    balance += ann_comm.price * (*x)->amount - *((*x)->cost);
	}
      }
    }

  // If it's a null entry, then let the user have their fun
  if (no_amounts)
    return true;

  // If there is only one transaction, balance against the basket
  // account if one has been set.

  if (journal && journal->basket && transactions.size() == 1) {
    assert(balance.type < value_t::BALANCE);
    transaction_t * nxact = new transaction_t(journal->basket);
    // The amount doesn't need to be set because the code below will
    // balance this transaction against the other.
    add_transaction(nxact);
    nxact->flags |= TRANSACTION_CALCULATED;
  }

  // If the first transaction of a two-transaction entry is of a
  // different commodity than the other, and it has no per-unit price,
  // determine its price by dividing the unit count into the value of
  // the balance.  This is done for the last eligible commodity.

  if (balance && balance.type == value_t::BALANCE &&
      ((balance_t *) balance.data)->amounts.size() == 2) {
    transactions_list::const_iterator x = transactions.begin();
    commodity_t& this_comm = (*x)->amount.commodity();

    amounts_map::const_iterator this_bal =
      ((balance_t *) balance.data)->amounts.find(&this_comm);
    amounts_map::const_iterator other_bal =
      ((balance_t *) balance.data)->amounts.begin();
    if (this_bal == other_bal)
      other_bal++;

    amount_t per_unit_cost =
      amount_t((*other_bal).second / (*this_bal).second).unround();

    for (; x != transactions.end(); x++) {
      if ((*x)->cost || ((*x)->flags & TRANSACTION_VIRTUAL) ||
	  (*x)->amount.commodity() != this_comm)
	continue;

      assert((*x)->amount);
      balance -= (*x)->amount;

      entry_t * entry = dynamic_cast<entry_t *>(this);

      if (! (*x)->amount.commodity().annotated)
	(*x)->amount.annotate_commodity(abs(per_unit_cost),
					entry ? entry->actual_date() : 0,
					entry ? entry->code : "");

      (*x)->cost = new amount_t(- (per_unit_cost * (*x)->amount));
      balance += *(*x)->cost;
    }
  }

  // Walk through each of the transactions, fixing up any that we
  // can, and performing any on-the-fly calculations.

  bool empty_allowed = true;

  for (transactions_list::const_iterator x = transactions.begin();
       x != transactions.end();
       x++) {
    if (! (*x)->amount.null() ||
	(((*x)->flags & TRANSACTION_VIRTUAL) &&
	 ! ((*x)->flags & TRANSACTION_BALANCE)))
      continue;

    if (! empty_allowed)
      break;
    empty_allowed = false;

    // If one transaction gives no value at all, its value will become
    // the inverse of the value of the others.  If multiple
    // commodities are involved, multiple transactions will be
    // generated to balance them all.

    balance_t * bal = NULL;
    switch (balance.type) {
    case value_t::BALANCE_PAIR:
      bal = &((balance_pair_t *) balance.data)->quantity;
      // fall through...

    case value_t::BALANCE:
      if (! bal)
	bal = (balance_t *) balance.data;

      if (bal->amounts.size() < 2) {
	balance.cast(value_t::AMOUNT);
      } else {
	bool first = true;
	for (amounts_map::const_iterator i = bal->amounts.begin();
	     i != bal->amounts.end();
	     i++) {
	  amount_t amt = (*i).second;
	  amt.negate();

	  if (first) {
	    (*x)->amount = amt;
	    first = false;
	  } else {
	    transaction_t * nxact = new transaction_t((*x)->account);
	    add_transaction(nxact);
	    nxact->flags |= TRANSACTION_CALCULATED;
	    nxact->amount = amt;
	  }

	  balance += amt;
	}
	break;
      }
      // fall through...

    case value_t::AMOUNT:
      (*x)->amount = *((amount_t *) balance.data);
      (*x)->amount.negate();
      (*x)->flags |= TRANSACTION_CALCULATED;

      balance += (*x)->amount;
      break;

    default:
      break;
    }
  }

  if (balance) {
    error * err =
      new balance_error("Entry does not balance",
			new entry_context(*this, "While balancing entry:"));
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
  DEBUG_PRINT("ledger.memory.ctors", "ctor entry_t");

  for (transactions_list::const_iterator i = transactions.begin();
       i != transactions.end();
       i++)
    (*i)->entry = this;
}

void entry_t::add_transaction(transaction_t * xact)
{
  xact->entry = this;
  entry_base_t::add_transaction(xact);
}

bool entry_t::valid() const
{
  if (! _date || ! journal)
    return false;

  for (transactions_list::const_iterator i = transactions.begin();
       i != transactions.end();
       i++)
    if ((*i)->entry != this || ! (*i)->valid())
      return false;

  return true;
}

auto_entry_t::auto_entry_t(const std::string& _predicate)
  : predicate_string(_predicate)
{
  DEBUG_PRINT("ledger.memory.ctors", "ctor auto_entry_t");
  predicate = new item_predicate<transaction_t>(predicate_string);
}

auto_entry_t::~auto_entry_t()
{
  DEBUG_PRINT("ledger.memory.dtors", "dtor auto_entry_t");
  if (predicate)
    delete predicate;
}

void auto_entry_t::extend_entry(entry_base_t& entry)
{
  transactions_list initial_xacts(entry.transactions.begin(),
				  entry.transactions.end());

  for (transactions_list::iterator i = initial_xacts.begin();
       i != initial_xacts.end();
       i++)
    if ((*predicate)(**i))
      for (transactions_list::iterator t = transactions.begin();
	   t != transactions.end();
	   t++) {
	amount_t amt;
	if (! (*t)->amount.commodity())
	  amt = (*i)->amount * (*t)->amount;
	else
	  amt = (*t)->amount;

	account_t * account  = (*t)->account;
	std::string fullname = account->fullname();
	assert(! fullname.empty());

	if (fullname == "$account")
	  account = (*i)->account;

	transaction_t * xact
	  = new transaction_t(account, amt, (*t)->flags | TRANSACTION_AUTO);
	entry.add_transaction(xact);
      }
}

account_t::~account_t()
{
  DEBUG_PRINT("ledger.memory.dtors", "dtor account_t " << this);
  //assert(! data);

  for (accounts_map::iterator i = accounts.begin();
       i != accounts.end();
       i++)
    delete (*i).second;
}

account_t * account_t::find_account(const std::string& name,
				    const bool	       auto_create)
{
  accounts_map::const_iterator i = accounts.find(name);
  if (i != accounts.end())
    return (*i).second;

  char buf[256];

  std::string::size_type sep = name.find(':');
  assert(sep < 256|| sep == std::string::npos);

  const char * first, * rest;
  if (sep == std::string::npos) {
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
      = accounts.insert(accounts_pair(first, account));
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

account_t * journal_t::find_account_re(const std::string& regexp)
{
  return find_account_re_(master, mask_t(regexp));
}

std::string account_t::fullname() const
{
  if (! _fullname.empty()) {
    return _fullname;
  } else {
    const account_t *	first	 = this;
    std::string		fullname = name;

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
  if (depth > 256 || ! journal)
    return false;

  for (accounts_map::const_iterator i = accounts.begin();
       i != accounts.end();
       i++)
    if (! (*i).second->valid())
      return false;

  return true;
}

journal_t::~journal_t()
{
  DEBUG_PRINT("ledger.memory.dtors", "dtor journal_t");

  assert(master);
  delete master;

  // Don't bother unhooking each entry's transactions from the
  // accounts they refer to, because all accounts are about to
  // be deleted.
  for (entries_list::iterator i = entries.begin();
       i != entries.end();
       i++)
    if (! item_pool ||
	((char *) *i) < item_pool || ((char *) *i) >= item_pool_end)
      delete *i;
    else
      (*i)->~entry_t();

  for (auto_entries_list::iterator i = auto_entries.begin();
       i != auto_entries.end();
       i++)
    if (! item_pool ||
	((char *) *i) < item_pool || ((char *) *i) >= item_pool_end)
      delete *i;
    else
      (*i)->~auto_entry_t();

  for (period_entries_list::iterator i = period_entries.begin();
       i != period_entries.end();
       i++)
    if (! item_pool ||
	((char *) *i) < item_pool || ((char *) *i) >= item_pool_end)
      delete *i;
    else
      (*i)->~period_entry_t();

  if (item_pool)
    delete[] item_pool;
}

bool journal_t::add_entry(entry_t * entry)
{
  entry->journal = this;

  if (! run_hooks(entry_finalize_hooks, *entry) || ! entry->finalize()) {
    entry->journal = NULL;
    return false;
  }

  entries.push_back(entry);

  for (transactions_list::const_iterator i = entry->transactions.begin();
       i != entry->transactions.end();
       i++)
    if ((*i)->cost && (*i)->amount)
      (*i)->amount.commodity().add_price(entry->date(),
					 *(*i)->cost / (*i)->amount);

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
  if (! master->valid())
    return false;

  for (entries_list::const_iterator i = entries.begin();
       i != entries.end();
       i++)
    if (! (*i)->valid())
      return false;

  for (commodities_map::const_iterator i = commodity_t::commodities.begin();
       i != commodity_t::commodities.end();
       i++)
    if (! (*i).second->valid())
      return false;

  return true;
}

} // namespace ledger
