#include "ledger.h"
#include "valexpr.h"
#include "datetime.h"

#include <fstream>

namespace ledger {

const std::string version = "2.0b";

bool transaction_t::valid() const
{
  if (! entry)
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

  found = false;
  for (transactions_list::const_iterator i = account->transactions.begin();
       i != account->transactions.end();
       i++)
    if (*i == this) {
      found = true;
      break;
    }
  if (! found)
    return false;

  if (! amount.valid())
    return false;

  if (cost && ! cost->valid())
    return false;

  if (flags & ~0x000f)
    return false;

  return true;
}

bool entry_t::valid() const
{
  if (! date || date == -1)
    return false;

  if (state != UNCLEARED && state != CLEARED && state != PENDING)
    return false;

  for (transactions_list::const_iterator i = transactions.begin();
       i != transactions.end();
       i++)
    if ((*i)->entry != this || ! (*i)->valid())
      return false;

  return true;
}

journal_t::~journal_t()
{
  DEBUG_PRINT("ledger.memory.dtors", "dtor journal_t");

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

  if (item_pool)
    delete[] item_pool;
}

bool journal_t::add_entry(entry_t * entry)
{
  entries.push_back(entry);

  for (transactions_list::const_iterator i = entry->transactions.begin();
       i != entry->transactions.end();
       i++) {
    (*i)->account->add_transaction(*i);

    if ((*i)->cost) {
      assert((*i)->cost->commodity);
      (*i)->amount.commodity->add_price(entry->date,
					*(*i)->cost / (*i)->amount);
    }
  }

  return true;
}

bool journal_t::remove_entry(entry_t * entry)
{
  entries.remove(entry);

  for (transactions_list::const_iterator i
	 = entry->transactions.begin();
       i != entry->transactions.end();
       i++)
    (*i)->account->remove_transaction(*i);

  return true;
}

entry_t * journal_t::derive_entry(strings_list::iterator i,
				  strings_list::iterator end) const
{
  std::auto_ptr<entry_t> added(new entry_t);

  entry_t * matching = NULL;

  if (! parse_date((*i).c_str(), &added->date)) {
    std::cerr << "Error: Bad entry date: " << *i << std::endl;
    return false;
  }
  ++i;

  if (i == end) {
    std::cerr << "Error: Too few arguments to 'entry'." << std::endl;
    return false;
  }

  mask_t regexp(*i++);

  for (entries_list::const_reverse_iterator j = entries.rbegin();
       j != entries.rend();
       j++)
    if (regexp.match((*j)->payee)) {
      matching = *j;
      break;
    }

  added->payee = matching ? matching->payee : regexp.pattern;

  if (i == end) {
    std::cerr << "Error: Too few arguments to 'entry'." << std::endl;
    return false;
  }

  if ((*i)[0] == '-' || std::isdigit((*i)[0])) {
    if (! matching) {
      std::cerr << "Error: Missing account name for non-matching entry."
		<< std::endl;
      return false;
    }

    transaction_t * m_xact, * xact, * first;
    m_xact = matching->transactions.front();

    amount_t amt(*i++);
    first = xact = new transaction_t(m_xact->account, amt);
    added->add_transaction(xact);

    if (xact->amount.commodity->symbol.empty())
      xact->amount.commodity = m_xact->amount.commodity;

    m_xact = matching->transactions.back();

    xact = new transaction_t(m_xact->account, - first->amount);
    added->add_transaction(xact);

    if (i != end && std::string(*i++) == "-from" && i != end)
      if (account_t * acct = find_account(*i))
	added->transactions.back()->account = acct;
  } else {
    while (std::string(*i) != "-from") {
      mask_t acct_regex(*i++);

      account_t *   acct  = NULL;
      commodity_t * cmdty = NULL;

      if (matching) {
	for (transactions_list::iterator x
	       = matching->transactions.begin();
	     x != matching->transactions.end();
	     x++) {
	  if (acct_regex.match((*x)->account->fullname())) {
	    acct  = (*x)->account;
	    cmdty = (*x)->amount.commodity;
	    break;
	  }
	}
      }

      if (! acct)
	acct = find_account(acct_regex.pattern);

      if (! acct) {
	std::cerr << "Error: Could not find account name '"
		  << acct_regex.pattern << "'." << std::endl;
	return false;
      }

      if (i == end) {
	std::cerr << "Error: Too few arguments to 'entry'." << std::endl;
	return false;
      }

      amount_t amt(*i++);
      transaction_t * xact = new transaction_t(acct, amt);
      added->add_transaction(xact);

      if (! xact->amount.commodity)
	xact->amount.commodity = cmdty;
    }

    if (i != end && std::string(*i++) == "-from" && i != end) {
      if (account_t * acct = find_account(*i++))
	added->add_transaction(new transaction_t(acct));
    } else {
      if (! matching) {
	std::cerr << "Error: Could not figure out the account to draw from."
		  << std::endl;
	std::exit(1);
      }
      transaction_t * xact
	= new transaction_t(matching->transactions.back()->account);
      added->add_transaction(xact);
    }
  }

  return added.release();
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

  return true;
}

void initialize_amounts();
void shutdown_amounts();

void initialize_formats();
void shutdown_formats();

void initialize()
{
  initialize_amounts();
  initialize_formats();
}

void shutdown()
{
  shutdown_amounts();
  shutdown_formats();
}

} // namespace ledger
