#include "ledger.h"
#include "valexpr.h"
#include "datetime.h"

#include <fstream>

namespace ledger {

const std::string version = "2.0b";

journal_t::~journal_t()
{
  delete master;

  // Don't bother unhooking each entry's transactions from the
  // accounts they refer to, because all accounts are about to
  // be deleted.
  for (entries_list::iterator i = entries.begin();
       i != entries.end();
       i++)
    delete *i;
}

bool journal_t::add_entry(entry_t * entry)
{
  entries.push_back(entry);

  for (transactions_list::const_iterator i = entry->transactions.begin();
       i != entry->transactions.end();
       i++) {
    (*i)->account->add_transaction(*i);

    if ((*i)->amount != (*i)->cost) {
      assert((*i)->amount.commodity);
      (*i)->amount.commodity->add_price(entry->date, (*i)->cost / (*i)->amount);
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
  entry_t * added    = new entry_t;
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
    first = xact = new transaction_t(added, m_xact->account, amt, amt);

    if (xact->amount.commodity->symbol.empty()) {
      xact->amount.commodity = m_xact->amount.commodity;
      xact->cost.commodity   = m_xact->amount.commodity;
    }
    added->add_transaction(xact);

    m_xact = matching->transactions.back();

    xact = new transaction_t(added, m_xact->account,
			     - first->amount, - first->amount);
    added->add_transaction(xact);

    if (std::string(*i++) == "-from" && i != end)
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
      transaction_t * xact = new transaction_t(added, acct, amt, amt);

      if (! xact->amount.commodity)
	xact->amount.commodity = cmdty;

      added->add_transaction(xact);
    }

    if (std::string(*i++) == "-from" && i != end) {
      if (account_t * acct = find_account(*i++)) {
	transaction_t * xact = new transaction_t(NULL, acct);
	added->add_transaction(xact);
      }
    } else {
      if (! matching) {
	std::cerr << "Error: Could not figure out the account to draw from."
		  << std::endl;
	std::exit(1);
      }
      transaction_t * xact
	= new transaction_t(added, matching->transactions.back()->account);
      added->add_transaction(xact);
    }
  }

  return added;
}

int parse_journal_file(char * p, journal_t * journal)
{
  char * sep = std::strrchr(p, '=');
  if (sep) *sep++ = '\0';

  std::ifstream stream(p);

  account_t * master;
  if (sep)
    master = journal->find_account(sep);
  else
    master = journal->master;

  journal->sources.push_back(p);

  unsigned long magic;
  std::istream::pos_type start = stream.tellg();
  stream.read((char *)&magic, sizeof(magic));
  stream.seekg(start);

  if (magic == binary_magic_number)
    return read_binary_journal(stream, "", journal, master);
  else
    return parse_textual_journal(stream, journal, master);
}

} // namespace ledger
