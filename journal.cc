#include "ledger.h"
#include "valexpr.h"
#include "datetime.h"
#include "acconf.h"

#include <fstream>

namespace ledger {

const std::string version = PACKAGE_VERSION;

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

void entry_t::add_transaction(transaction_t * xact)
{
  xact->entry = this;
  transactions.push_back(xact);
  xact->account->add_transaction(xact);
}

bool entry_t::remove_transaction(transaction_t * xact)
{
  bool found = false;
  transactions_list::iterator i;
  for (i = transactions.begin(); i != transactions.end(); i++)
    if (*i == xact) {
      found = true;
      break;
    }
  if (! found)
    return false;

  transactions.erase(i);

  for (i = xact->account->transactions.begin();
       i != xact->account->transactions.end();
       i++)
    if (*i == xact) {
      xact->account->transactions.erase(i);
      return true;
    }

  return false;
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

account_t::~account_t()
{
  DEBUG_PRINT("ledger.memory.ctors", "dtor account_t");
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

  static char buf[256];

  std::string::size_type sep = name.find(':');
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
    accounts.insert(accounts_pair(first, account));
  } else {
    account = (*i).second;
  }

  if (rest)
    account = account->find_account(rest, auto_create);

  return account;
}

bool account_t::remove_transaction(transaction_t * xact)
{
  for (transactions_list::iterator i = transactions.begin();
       i != transactions.end();
       i++)
    if (*i == xact) {
      transactions.erase(i);
      return true;
    }

  return false;
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

bool account_t::valid() const
{
  if (name.find('-') != std::string::npos)
    return false;

  if (depth > 16)
    return false;

  for (transactions_list::const_iterator i = transactions.begin();
       i != transactions.end();
       i++)
    if ((*i)->account != this)
      return false;

  for (accounts_map::const_iterator i = accounts.begin();
       i != accounts.end();
       i++)
    if (! (*i).second->valid())
      return false;

  return true;
}

void clean_commodity_history(char * item_pool, char * item_pool_end);

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
    else
      (*i)->~entry_t();

  // Remove historical prices which were allocated in the item_pool.
  clean_commodity_history(item_pool, item_pool_end);

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

  for (transactions_list::const_iterator i
	 = entry->transactions.begin();
       i != entry->transactions.end();
       i++) {
    (*i)->account->remove_transaction(*i);
  }

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

  for (commodities_map::const_iterator i = commodity_t::commodities.begin();
       i != commodity_t::commodities.end();
       i++)
    if (! (*i).second->valid())
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

#ifdef USE_BOOST_PYTHON

#include <boost/python.hpp>
#include <boost/python/suite/indexing/vector_indexing_suite.hpp>

using namespace boost::python;
using namespace ledger;

BOOST_PYTHON_MEMBER_FUNCTION_OVERLOADS(journal_find_account_overloads,
				       find_account, 1, 2)

unsigned int transactions_len(entry_t& entry)
{
  return entry.transactions.size();
}

transaction_t& transactions_getitem(entry_t& entry, int i)
{
  std::size_t len = entry.transactions.size();

  if (abs(i) >= len) {
    PyErr_SetString(PyExc_IndexError, "Index out of range");
    throw_error_already_set();
  }

  return *(entry.transactions[i < 0 ? len + i : i]);
}

unsigned int entries_len(journal_t& journal)
{
  return journal.entries.size();
}

entry_t& entries_getitem(journal_t& journal, int i)
{
  std::size_t len = journal.entries.size();

  if (abs(i) >= len) {
    PyErr_SetString(PyExc_IndexError, "Index out of range");
    throw_error_already_set();
  }

  return *(journal.entries[i < 0 ? len + i : i]);
}

unsigned int accounts_len(account_t& account)
{
  return account.accounts.size();
}

account_t& accounts_getitem(account_t& account, int index)
{
  std::size_t len = account.accounts.size();

  if (abs(index) >= len) {
    PyErr_SetString(PyExc_IndexError, "Index out of range");
    throw_error_already_set();
  }

  int x = 0;
  for (accounts_map::iterator i = account.accounts.begin();
       i != account.accounts.end();
       i++)
    if (x++ == index)
      return *((*i).second);
}

void export_journal()
{
  class_< transaction_t > ("Transaction")
    .def(init<account_t *, amount_t, optional<unsigned int, std::string> >())

    .def_readwrite("entry", &transaction_t::entry)
    .def_readwrite("account", &transaction_t::account)
    .def_readwrite("amount", &transaction_t::amount)
    .def_readwrite("cost", &transaction_t::cost)
    .def_readwrite("flags", &transaction_t::flags)
    .def_readwrite("note", &transaction_t::note)
    .def_readwrite("data", &transaction_t::data)

    .def("valid", &transaction_t::valid)
    ;

  class_< transactions_list > ("TransactionsList")
    .def(vector_indexing_suite< transactions_list >())
    ;

  class_< entry_t > ("Entry")
    .def_readwrite("date", &entry_t::date)
    .def_readwrite("state", &entry_t::state)
    .def_readwrite("code", &entry_t::code)
    .def_readwrite("payee", &entry_t::payee)

    .def("__len__", transactions_len)
    .def("__getitem__", transactions_getitem,
	 return_value_policy<reference_existing_object>())

    .def("add_transaction", &entry_t::add_transaction)
    .def("remove_transaction", &entry_t::remove_transaction)

    .def("valid", &entry_t::valid)
    ;

  class_< account_t >
    ("Account", init<optional<account_t *, std::string, std::string> >())
    .def_readwrite("parent", &account_t::parent)
    .def_readwrite("name", &account_t::name)
    .def_readwrite("note", &account_t::note)
    .def_readonly("depth", &account_t::depth)
    .def_readonly("transactions", &account_t::transactions)
    .def_readwrite("data", &account_t::data)
    .def_readonly("ident", &account_t::ident)

    .def(self_ns::str(self))

    .def("fullname", &account_t::fullname)
    .def("add_account", &account_t::add_account)
    .def("remove_account", &account_t::remove_account)
    .def("find_account", &account_t::find_account,
	 return_value_policy<reference_existing_object>())
    .def("add_transaction", &account_t::add_transaction)
    .def("remove_transaction", &account_t::remove_transaction)

    .def("valid", &account_t::valid)
    ;

  class_< entries_list > ("EntriesList")
    .def(vector_indexing_suite< entries_list >())
    ;

  class_< strings_list > ("StringsList")
    .def(vector_indexing_suite< strings_list >())
    ;

  class_< journal_t > ("Journal")
    .def_readonly("sources", &journal_t::sources)

    .def("__len__", entries_len)
    .def("__getitem__", entries_getitem,
	 return_value_policy<reference_existing_object>())

    .def("add_account", &journal_t::add_account)
    .def("remove_account", &journal_t::remove_account)
#if 0
    .def("find_account", &journal_t::find_account,
	 journal_find_account_overloads(args("name", "auto_create"))
	 [return_value_policy<reference_existing_object>()])
#endif

    .def("add_entry", &journal_t::add_entry)
    .def("remove_entry", &journal_t::remove_entry)
    .def("derive_entry", &journal_t::derive_entry,
	 return_value_policy<manage_new_object>())

    .def("valid", &journal_t::valid)
    ;
}

#endif // USE_BOOST_PYTHON
