#include "ledger.h"
#include "valexpr.h"
#include "datetime.h"
#include "error.h"
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

balance_pair_t& add_transaction_to(const transaction_t& xact,
				   balance_pair_t&	bal_pair)
{
  if (xact.cost && ! bal_pair.cost)
    bal_pair.cost = new balance_t(bal_pair.quantity);

  bal_pair.quantity += xact.amount;

  if (bal_pair.cost)
    *bal_pair.cost += xact.cost ? *xact.cost : xact.amount;

  return bal_pair;
}

value_t& add_transaction_to(const transaction_t& xact, value_t& value)
{
  switch (value.type) {
  case value_t::BOOLEAN:
  case value_t::INTEGER:
    value.cast(value_t::AMOUNT);

  case value_t::AMOUNT:
    if (xact.cost) {
      value.cast(value_t::BALANCE_PAIR);
      return add_transaction_to(xact, value);
    }
    else if (((amount_t *) value.data)->commodity() !=
	     xact.amount.commodity()) {
      value.cast(value_t::BALANCE);
      return add_transaction_to(xact, value);
    }
    *((amount_t *) value.data) += xact.amount;
    break;

  case value_t::BALANCE:
    if (xact.cost) {
      value.cast(value_t::BALANCE_PAIR);
      return add_transaction_to(xact, value);
    }
    *((balance_t *) value.data) += xact.amount;
    break;

  case value_t::BALANCE_PAIR:
    add_transaction_to(xact, *((balance_pair_t *) value.data));
    break;

  default:
    assert(0);
    break;
  }

  return value;
}

void entry_t::add_transaction(transaction_t * xact)
{
  xact->entry = this;
  transactions.push_back(xact);
  xact->account->add_transaction(xact);
}

bool entry_t::remove_transaction(transaction_t * xact)
{
  transactions.remove(xact);
  xact->account->transactions.remove(xact);
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

bool account_t::remove_transaction(transaction_t * xact)
{
  transactions.remove(xact);
  return true;
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
    if ((*i)->cost)
      (*i)->amount.commodity().add_price(entry->date,
					 *(*i)->cost / (*i)->amount);
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
				  strings_list::iterator end)
{
  std::auto_ptr<entry_t> added(new entry_t);

  entry_t * matching = NULL;

  if (! parse_date((*i).c_str(), &added->date))
    throw error("Bad date passed to 'entry'");

  if (++i == end)
    throw error("Too few arguments to 'entry'");

  mask_t regexp(*i++);

  for (entries_list::reverse_iterator j = entries.rbegin();
       j != entries.rend();
       j++)
    if (regexp.match((*j)->payee)) {
      matching = *j;
      break;
    }

  added->payee = matching ? matching->payee : regexp.pattern;

  if (i == end)
    throw error("Too few arguments to 'entry'");

  if ((*i)[0] == '-' || std::isdigit((*i)[0])) {
    if (! matching)
      throw error("Could not determine the account to draw from");

    transaction_t * m_xact, * xact, * first;
    m_xact = matching->transactions.front();

    first = xact = new transaction_t(m_xact->account, amount_t(*i++));
    added->add_transaction(xact);

    if (xact->amount.commodity().symbol.empty())
      xact->amount.set_commodity(m_xact->amount.commodity());

    m_xact = matching->transactions.back();

    xact = new transaction_t(m_xact->account, - first->amount);
    added->add_transaction(xact);

    if (i != end) {
      account_t * acct = find_account_re(*i);
      if (! acct)
	acct = find_account(*i);
      if (acct)
	added->transactions.back()->account = acct;
    }
  } else {
    while (i != end) {
      std::string&  re_pat(*i++);
      account_t *   acct  = NULL;
      commodity_t * cmdty = NULL;

      if (matching) {
	mask_t acct_regex(re_pat);

	for (transactions_list::const_iterator x
	       = matching->transactions.begin();
	     x != matching->transactions.end();
	     x++) {
	  if (acct_regex.match((*x)->account->fullname())) {
	    acct  = (*x)->account;
	    cmdty = &(*x)->amount.commodity();
	    break;
	  }
	}
      }

      if (! acct) {
	acct = find_account_re(re_pat);
	if (acct && acct->transactions.size() > 0)
	  cmdty = &acct->transactions.back()->amount.commodity();
      }

      if (! acct)
	acct = find_account(re_pat);

      if (i == end) {
	added->add_transaction(new transaction_t(acct));
	goto done;
      }

      transaction_t * xact = new transaction_t(acct, amount_t(*i++));
      if (! xact->amount.commodity())
	xact->amount.set_commodity(*cmdty);

      added->add_transaction(xact);
    }

    if (! matching) {
      throw error("Could not determine the account to draw from");
    } else {
      transaction_t * xact
	= new transaction_t(matching->transactions.back()->account);
      added->add_transaction(xact);
    }
  }

 done:
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

} // namespace ledger

#ifdef USE_BOOST_PYTHON

#include <boost/python.hpp>
#include <boost/python/exception_translator.hpp>

using namespace boost::python;
using namespace ledger;

entry_t& transaction_entry(const transaction_t& xact)
{
  return *xact.entry;
}

unsigned int transactions_len(entry_t& entry)
{
  return entry.transactions.size();
}

transaction_t& transactions_getitem(entry_t& entry, int i)
{
  static int last_index = 0;
  static entry_t * last_entry = NULL;
  static transactions_list::iterator elem;

  std::size_t len = entry.transactions.size();

  if (abs(i) >= len) {
    PyErr_SetString(PyExc_IndexError, "Index out of range");
    throw_error_already_set();
  }

  if (&entry == last_entry && i == last_index + 1) {
    last_index = i;
    return **++elem;
  }

  int x = i < 0 ? len + i : i;
  elem = entry.transactions.begin();
  while (--x >= 0)
    elem++;

  last_entry = &entry;
  last_index = i;

  return **elem;
}

unsigned int entries_len(journal_t& journal)
{
  return journal.entries.size();
}

entry_t& entries_getitem(journal_t& journal, int i)
{
  static int last_index = 0;
  static journal_t * last_journal = NULL;
  static entries_list::iterator elem;

  std::size_t len = journal.entries.size();

  if (abs(i) >= len) {
    PyErr_SetString(PyExc_IndexError, "Index out of range");
    throw_error_already_set();
  }

  if (&journal == last_journal && i == last_index + 1) {
    last_index = i;
    return **++elem;
  }

  int x = i < 0 ? len + i : i;
  elem = journal.entries.begin();
  while (--x >= 0)
    elem++;

  last_journal = &journal;
  last_index   = i;

  return **elem;
}

unsigned int accounts_len(account_t& account)
{
  return account.accounts.size();
}

account_t& accounts_getitem(account_t& account, int i)
{
  static int last_index = 0;
  static account_t * last_account = NULL;
  static accounts_map::iterator elem;

  std::size_t len = account.accounts.size();

  if (abs(i) >= len) {
    PyErr_SetString(PyExc_IndexError, "Index out of range");
    throw_error_already_set();
  }

  if (&account == last_account && i == last_index + 1) {
    last_index = i;
    return *(*++elem).second;
  }

  int x = i < 0 ? len + i : i;
  elem = account.accounts.begin();
  while (--x >= 0)
    elem++;

  last_account = &account;
  last_index   = i;

  return *(*elem).second;
}

account_t * py_find_account_1(journal_t& journal, const std::string& name)
{
  return journal.find_account(name);
}

account_t * py_find_account_2(journal_t& journal, const std::string& name,
			      const bool auto_create)
{
  return journal.find_account(name, auto_create);
}

#define EXC_TRANSLATOR(type)				\
  void exc_translate_ ## type(const type& err) {	\
    PyErr_SetString(PyExc_RuntimeError, err.what());	\
  }

EXC_TRANSLATOR(error)
EXC_TRANSLATOR(compute_error)
EXC_TRANSLATOR(value_expr_error)
EXC_TRANSLATOR(interval_expr_error)
EXC_TRANSLATOR(format_error)
EXC_TRANSLATOR(parse_error)

void export_journal()
{
  scope().attr("TRANSACTION_NORMAL")	   = TRANSACTION_NORMAL;
  scope().attr("TRANSACTION_VIRTUAL")	   = TRANSACTION_VIRTUAL;
  scope().attr("TRANSACTION_BALANCE")	   = TRANSACTION_BALANCE;
  scope().attr("TRANSACTION_AUTO")	   = TRANSACTION_AUTO;
  scope().attr("TRANSACTION_BULK_ALLOC")   = TRANSACTION_BULK_ALLOC;

  class_< transaction_t > ("Transaction")
    .def(init<account_t *, amount_t, optional<unsigned int, std::string> >())

    .def(self == self)
    .def(self != self)

    .add_property("entry",
		  make_getter(&transaction_t::entry,
			      return_value_policy<reference_existing_object>()))
    .def_readwrite("account", &transaction_t::account)
    .def_readwrite("amount", &transaction_t::amount)
    .def_readwrite("cost", &transaction_t::cost)
    .def_readwrite("flags", &transaction_t::flags)
    .def_readwrite("note", &transaction_t::note)
    .def_readwrite("data", &transaction_t::data)

    .def("valid", &transaction_t::valid)
    ;

  class_< account_t >
    ("Account", init<optional<account_t *, std::string, std::string> >())
    .def(self == self)
    .def(self != self)

    .add_property("parent",
		  make_getter(&account_t::parent,
			      return_internal_reference<1>()))
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

  class_< journal_t > ("Journal")
    .def(self == self)
    .def(self != self)

    .add_property("master",
		  make_getter(&journal_t::master,
			      return_internal_reference<1>()))
    .def_readonly("sources", &journal_t::sources)

    .def("__len__", entries_len)
    .def("__getitem__", entries_getitem, return_internal_reference<1>())
    .def("add_account", &journal_t::add_account)
    .def("remove_account", &journal_t::remove_account)
    .def("find_account", py_find_account_1, return_internal_reference<1>())
    .def("find_account", py_find_account_2, return_internal_reference<1>())
    .def("find_account_re", &journal_t::find_account_re,
	 return_internal_reference<1>())

    .def("add_entry", &journal_t::add_entry)
    .def("remove_entry", &journal_t::remove_entry)
    .def("derive_entry", &journal_t::derive_entry,
	 return_value_policy<manage_new_object>())

    .def("valid", &journal_t::valid)
    ;

  scope in_entry = class_< entry_t > ("Entry")
    .def(self == self)
    .def(self != self)

    .def_readwrite("date", &entry_t::date)
    .def_readwrite("state", &entry_t::state)
    .def_readwrite("code", &entry_t::code)
    .def_readwrite("payee", &entry_t::payee)

    .def("__len__", transactions_len)
    .def("__getitem__", transactions_getitem,
	 return_internal_reference<1>())

    .def("add_transaction", &entry_t::add_transaction)
    .def("remove_transaction", &entry_t::remove_transaction)

    .def("valid", &entry_t::valid)
    ;

  enum_< entry_t::state_t > ("State")
    .value("UNCLEARED", entry_t::UNCLEARED)
    .value("CLEARED",   entry_t::CLEARED)
    .value("PENDING",   entry_t::PENDING)
    ;

#define EXC_TRANSLATE(type)					\
  register_exception_translator<type>(&exc_translate_ ## type);

  EXC_TRANSLATE(error);
  EXC_TRANSLATE(compute_error);
  EXC_TRANSLATE(value_expr_error);
  EXC_TRANSLATE(interval_expr_error);
  EXC_TRANSLATE(format_error);
  EXC_TRANSLATE(parse_error);
}

#endif // USE_BOOST_PYTHON
