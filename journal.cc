#include "journal.h"
#include "datetime.h"
#include "valexpr.h"
#include "mask.h"
#include "error.h"
#ifdef USE_BOOST_PYTHON
#include "py_eval.h"
#endif
#include "acconf.h"

#include <fstream>

namespace ledger {

const std::string version = PACKAGE_VERSION;

bool transaction_t::use_effective_date = false;

std::time_t transaction_t::actual_date() const
{
  if (_date == 0) {
    assert(entry);
    return entry->actual_date();
  }
  return _date;
}

std::time_t transaction_t::effective_date() const
{
  if (_date_eff == 0) {
    assert(entry);
    return entry->effective_date();
  }
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

  if (flags & ~0x000f)
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
      }
    }

  // If it's a null entry, then let the user have their fun
  if (no_amounts)
    return true;

  if (journal && journal->basket && transactions.size() == 1) {
    assert(balance.type < value_t::BALANCE);
    transaction_t * nxact = new transaction_t(journal->basket);
    // The amount doesn't need to be set because the code below will
    // balance this transaction against the other.
    add_transaction(nxact);
  }

  // If one transaction of a two-line transaction is of a different
  // commodity than the others, and it has no per-unit price,
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

    amount_t per_unit_cost = (*other_bal).second / (*this_bal).second;

    for (; x != transactions.end(); x++) {
      if ((*x)->cost || ((*x)->flags & TRANSACTION_VIRTUAL) ||
	  (*x)->amount.commodity() != this_comm)
	continue;

      assert((*x)->amount);

      balance -= (*x)->amount;
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

      balance += (*x)->amount;
      break;

    default:
      break;
    }
  }

  return ! balance;
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
	if ((*t)->amount.commodity().symbol.empty())
	  amt = (*i)->amount * (*t)->amount;
	else
	  amt = (*t)->amount;

	account_t * account  = (*t)->account;
	std::string fullname = account->fullname();
	assert(! fullname.empty());

	if (fullname == "$account")
	  account = (*i)->account;
#ifdef USE_BOOST_PYTHON
	else if (fullname[0] == '@') {
	  value_expr_t * expr = parse_value_expr(fullname);
	  if (expr->kind == value_expr_t::F_INTERP_FUNC)
	    python_call(expr->constant_s, expr->right, details_t(**i),
			*(*t)->account);
	}
#endif

	transaction_t * xact
	  = new transaction_t(account, amt, (*t)->flags | TRANSACTION_AUTO);
	entry.add_transaction(xact);
      }
}

account_t::~account_t()
{
  DEBUG_PRINT("ledger.memory.dtors", "dtor account_t");
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

  if (! entry->finalize() ||
      ! run_hooks(entry_finalize_hooks, *entry)) {
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

#ifdef USE_BOOST_PYTHON

#include <boost/python.hpp>
#include <boost/python/exception_translator.hpp>

using namespace boost::python;
using namespace ledger;

entry_t& transaction_entry(const transaction_t& xact)
{
  return *xact.entry;
}

unsigned int transactions_len(entry_base_t& entry)
{
  return entry.transactions.size();
}

transaction_t& transactions_getitem(entry_base_t& entry, int i)
{
  static int last_index = 0;
  static entry_base_t * last_entry = NULL;
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

PyObject * py_account_get_data(account_t& account)
{
  return (PyObject *) account.data;
}

void py_account_set_data(account_t& account, PyObject * obj)
{
  account.data = obj;
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

bool py_add_entry(journal_t& journal, entry_t * entry) {
  return journal.add_entry(new entry_t(*entry));
}

void py_add_transaction(entry_base_t& entry, transaction_t * xact) {
  return entry.add_transaction(new transaction_t(*xact));
}

struct entry_base_wrap : public entry_base_t
{
  PyObject * self;
  entry_base_wrap(PyObject * self_) : self(self_) {}

  virtual bool valid() const {
    return call_method<bool>(self, "valid");
  }
};

struct py_entry_finalizer_t : public entry_finalizer_t {
  object pyobj;
  py_entry_finalizer_t() {}
  py_entry_finalizer_t(object obj) : pyobj(obj) {}
  py_entry_finalizer_t(const py_entry_finalizer_t& other)
    : pyobj(other.pyobj) {}
  virtual bool operator()(entry_t& entry) {
    return call<bool>(pyobj.ptr(), entry);
  }
};

std::list<py_entry_finalizer_t> py_finalizers;

void py_add_entry_finalizer(journal_t& journal, object x)
{
  py_finalizers.push_back(py_entry_finalizer_t(x));
  journal.add_entry_finalizer(&py_finalizers.back());
}

void py_remove_entry_finalizer(journal_t& journal, object x)
{
  for (std::list<py_entry_finalizer_t>::iterator i = py_finalizers.begin();
       i != py_finalizers.end();
       i++)
    if ((*i).pyobj == x) {
      journal.remove_entry_finalizer(&(*i));
      py_finalizers.erase(i);
      return;
    }
}

void py_run_entry_finalizers(journal_t& journal, entry_t& entry)
{
  run_hooks(journal.entry_finalize_hooks, entry);
}

#define EXC_TRANSLATOR(type)				\
  void exc_translate_ ## type(const type& err) {	\
    PyErr_SetString(PyExc_RuntimeError, err.what());	\
  }

EXC_TRANSLATOR(error)
EXC_TRANSLATOR(interval_expr_error)
EXC_TRANSLATOR(format_error)
EXC_TRANSLATOR(parse_error)

void export_journal()
{
  scope().attr("TRANSACTION_NORMAL")	 = TRANSACTION_NORMAL;
  scope().attr("TRANSACTION_VIRTUAL")	 = TRANSACTION_VIRTUAL;
  scope().attr("TRANSACTION_BALANCE")	 = TRANSACTION_BALANCE;
  scope().attr("TRANSACTION_AUTO")	 = TRANSACTION_AUTO;
  scope().attr("TRANSACTION_BULK_ALLOC") = TRANSACTION_BULK_ALLOC;

  class_< transaction_t > ("Transaction")
    .def(init<account_t *, amount_t, optional<unsigned int, std::string> >())

    .def(self == self)
    .def(self != self)

    .add_property("entry",
		  make_getter(&transaction_t::entry,
			      return_value_policy<reference_existing_object>()))
    .add_property("account",
		  make_getter(&transaction_t::account,
			      return_value_policy<reference_existing_object>()))
    .def_readwrite("amount", &transaction_t::amount)
    .add_property("cost",
		  make_getter(&transaction_t::cost,
			      return_internal_reference<1>()))
    .def_readwrite("state", &transaction_t::state)
    .def_readwrite("flags", &transaction_t::flags)
    .def_readwrite("note", &transaction_t::note)
#if 0
    .def_readwrite("data", &transaction_t::data)
#endif

    .def("valid", &transaction_t::valid)
    ;

  enum_< transaction_t::state_t > ("State")
    .value("UNCLEARED", transaction_t::UNCLEARED)
    .value("CLEARED",   transaction_t::CLEARED)
    .value("PENDING",   transaction_t::PENDING)
    ;

  class_< account_t >
    ("Account",
     init<optional<account_t *, std::string, std::string> >()
     [with_custodian_and_ward<1, 2>()])
    .def(self == self)
    .def(self != self)

    .def_readonly("journal", &account_t::journal)
    .add_property("parent",
		  make_getter(&account_t::parent,
			      return_internal_reference<1>()))
    .def_readwrite("name", &account_t::name)
    .def_readwrite("note", &account_t::note)
    .def_readonly("depth", &account_t::depth)
    .add_property("data", py_account_get_data, py_account_set_data)
    .def_readonly("ident", &account_t::ident)

    .def(self_ns::str(self))

    .def("fullname", &account_t::fullname)
    .def("add_account", &account_t::add_account)
    .def("remove_account", &account_t::remove_account)
    .def("find_account", &account_t::find_account,
	 return_value_policy<reference_existing_object>())

    .def("valid", &account_t::valid)
    ;

  class_< journal_t > ("Journal")
    .def(self == self)
    .def(self != self)

    .add_property("master",
		  make_getter(&journal_t::master,
			      return_internal_reference<1>()))
    .add_property("basket",
		  make_getter(&journal_t::basket,
			      return_internal_reference<1>()))
    .def_readwrite("price_db", &journal_t::price_db)
    .def_readonly("sources", &journal_t::sources)

    .def("__len__", entries_len)
    .def("__getitem__", entries_getitem, return_internal_reference<1>())
    .def("add_account", &journal_t::add_account)
    .def("remove_account", &journal_t::remove_account)
    .def("find_account", py_find_account_1, return_internal_reference<1>())
    .def("find_account", py_find_account_2, return_internal_reference<1>())
    .def("find_account_re", &journal_t::find_account_re,
	 return_internal_reference<1>())

    .def("add_entry", py_add_entry)
    .def("remove_entry", &journal_t::remove_entry)
    .def("add_entry_finalizer", py_add_entry_finalizer)
    .def("remove_entry_finalizer", py_remove_entry_finalizer)
    .def("run_entry_finalizers", py_run_entry_finalizers)

    .def("valid", &journal_t::valid)
    ;

  class_< entry_base_t, entry_base_wrap, boost::noncopyable > ("EntryBase")
    .def("__len__", transactions_len)
    .def("__getitem__", transactions_getitem,
	 return_internal_reference<1>())

    .def_readonly("journal", &entry_base_t::journal)
    .def_readonly("src_idx", &entry_base_t::src_idx)
    .def_readonly("beg_pos", &entry_base_t::beg_pos)
    .def_readonly("beg_line", &entry_base_t::beg_line)
    .def_readonly("end_pos", &entry_base_t::end_pos)
    .def_readonly("end_line", &entry_base_t::end_line)

    .def("add_transaction", py_add_transaction)
    .def("remove_transaction", &entry_base_t::remove_transaction)

    .def(self == self)
    .def(self != self)

    .def("valid", &entry_base_t::valid)
    ;

  scope in_entry = class_< entry_t, bases<entry_base_t> > ("Entry")
    .def_readwrite("date", &entry_t::date)
    .def_readwrite("code", &entry_t::code)
    .def_readwrite("payee", &entry_t::payee)

    .def("valid", &entry_t::valid)
    ;

  class_< auto_entry_t, bases<entry_base_t> > ("AutoEntry")
    .add_property("predicate",
		  make_getter(&auto_entry_t::predicate,
			      return_internal_reference<1>()))
    .def_readonly("predicate_string", &auto_entry_t::predicate_string)

    .def("valid", &auto_entry_t::valid)
    ;

  class_< period_entry_t, bases<entry_base_t> > ("PeriodEntry")
    .def_readonly("period", &period_entry_t::period)
    .def_readonly("period_string", &period_entry_t::period_string)

    .def("valid", &period_entry_t::valid)
    ;

#define EXC_TRANSLATE(type)					\
  register_exception_translator<type>(&exc_translate_ ## type);

  EXC_TRANSLATE(error);
  EXC_TRANSLATE(interval_expr_error);
  EXC_TRANSLATE(format_error);
  EXC_TRANSLATE(parse_error);
}

#endif // USE_BOOST_PYTHON
