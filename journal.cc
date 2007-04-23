#include "journal.h"
#include "mask.h"
#if 0
#ifdef USE_BOOST_PYTHON
#include "py_eval.h"
#endif
#endif

namespace ledger {

const string version = PACKAGE_VERSION;

bool transaction_t::use_effective_date = false;

transaction_t::~transaction_t()
{
  TRACE_DTOR(transaction_t);
  if (cost) delete cost;
}

moment_t transaction_t::actual_date() const
{
  if (! is_valid_moment(_date) && entry)
    return entry->actual_date();
  return _date;
}

moment_t transaction_t::effective_date() const
{
  if (! is_valid_moment(_date_eff) && entry)
    return entry->effective_date();
  return _date_eff;
}

bool transaction_t::valid() const
{
  if (! entry) {
    DEBUG_PRINT("ledger.validate", "transaction_t: ! entry");
    return false;
  }

  if (state != UNCLEARED && state != CLEARED && state != PENDING) {
    DEBUG_PRINT("ledger.validate", "transaction_t: state is bad");
    return false;
  }

  bool found = false;
  for (transactions_list::const_iterator i = entry->transactions.begin();
       i != entry->transactions.end();
       i++)
    if (*i == this) {
      found = true;
      break;
    }
  if (! found) {
    DEBUG_PRINT("ledger.validate", "transaction_t: ! found");
    return false;
  }

  if (! account) {
    DEBUG_PRINT("ledger.validate", "transaction_t: ! account");
    return false;
  }

  if (! amount.valid()) {
    DEBUG_PRINT("ledger.validate", "transaction_t: ! amount.valid()");
    return false;
  }

  if (cost && ! cost->valid()) {
    DEBUG_PRINT("ledger.validate", "transaction_t: cost && ! cost->valid()");
    return false;
  }

  if (flags & ~0x003f) {
    DEBUG_PRINT("ledger.validate", "transaction_t: flags are bad");
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

  bool no_amounts = true;
  bool saw_null   = false;
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
	    balance += ann_comm.price * (*x)->amount.number() - *((*x)->cost);
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

  if (! saw_null && balance && balance.type == value_t::BALANCE &&
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
      amount_t((*other_bal).second / (*this_bal).second.number()).unround();

    for (; x != transactions.end(); x++) {
      if ((*x)->cost || ((*x)->flags & TRANSACTION_VIRTUAL) ||
	  ! (*x)->amount || (*x)->amount.commodity() != this_comm)
	continue;

      assert((*x)->amount);
      balance -= (*x)->amount;

      entry_t * entry = dynamic_cast<entry_t *>(this);

      if ((*x)->amount.commodity() &&
	  ! (*x)->amount.commodity().annotated)
	(*x)->amount.annotate_commodity
	  (per_unit_cost.abs(),
	   entry ? entry->actual_date() : moment_t(),
	   entry ? entry->code : "");

      (*x)->cost = new amount_t(- (per_unit_cost * (*x)->amount.number()));
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
      throw new error("Only one transaction with null amount allowed per entry");
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
	  amount_t amt = (*i).second.negate();

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
      (*x)->amount = ((amount_t *) balance.data)->negate();
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
    code(e.code), payee(e.payee), data(NULL)
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
    DEBUG_PRINT("ledger.validate", "entry_t: ! _date || ! journal");
    return false;
  }

  for (transactions_list::const_iterator i = transactions.begin();
       i != transactions.end();
       i++)
    if ((*i)->entry != this || ! (*i)->valid()) {
      DEBUG_PRINT("ledger.validate", "entry_t: transaction not valid");
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
    // jww (2006-09-10): Create a scope here based on entry
    if (predicate.calc((xml::node_t *) NULL)) {
      for (transactions_list::iterator t = transactions.begin();
	   t != transactions.end();
	   t++) {
	amount_t amt;
	if (! (*t)->amount.commodity()) {
	  if (! post)
	    continue;
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
	  = new transaction_t(account, amt, (*t)->flags | TRANSACTION_AUTO);
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
    delete (*i).second;
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
    DEBUG_PRINT("ledger.validate", "account_t: depth > 256 || ! journal");
    return false;
  }

  for (accounts_map::const_iterator i = accounts.begin();
       i != accounts.end();
       i++) {
    if (this == (*i).second) {
      DEBUG_PRINT("ledger.validate", "account_t: parent refers to itself!");
      return false;
    }

    if (! (*i).second->valid()) {
      DEBUG_PRINT("ledger.validate", "account_t: child not valid");
      return false;
    }
  }

  return true;
}

journal_t::~journal_t()
{
  TRACE_DTOR(journal_t);

  assert(master);
  delete master;

  if (document)
    delete document;

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
    if ((*i)->cost && (*i)->amount)
      (*i)->amount.commodity().add_price(entry->date(),
					 *(*i)->cost / (*i)->amount.number());

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
    DEBUG_PRINT("ledger.validate", "journal_t: master not valid");
    return false;
  }

  for (entries_list::const_iterator i = entries.begin();
       i != entries.end();
       i++)
    if (! (*i)->valid()) {
      DEBUG_PRINT("ledger.validate", "journal_t: entry not valid");
      return false;
    }

  for (commodities_map::const_iterator i = commodity_t::commodities.begin();
       i != commodity_t::commodities.end();
       i++)
    if (! (*i).second->valid()) {
      DEBUG_PRINT("ledger.validate", "journal_t: commodity not valid");
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
    out << "= " << entry->predicate.expr << '\n';
    print_format = prefix + "    %-34A  %12o\n";
  }
  else if (const period_entry_t * entry =
	   dynamic_cast<const period_entry_t *>(&entry_base)) {
    out << "~ " << entry->period_string << '\n';
    print_format = prefix + "    %-34A  %12o\n";
  }
  else {
    assert(0);
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

} // namespace ledger

#if 0
#ifdef USE_BOOST_PYTHON

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

account_t * py_find_account_1(journal_t& journal, const string& name)
{
  return journal.find_account(name);
}

account_t * py_find_account_2(journal_t& journal, const string& name,
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
  virtual bool operator()(entry_t& entry, bool post) {
    return call<bool>(pyobj.ptr(), entry, post);
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

void py_run_entry_finalizers(journal_t& journal, entry_t& entry, bool post)
{
  run_hooks(journal.entry_finalize_hooks, entry, post);
}

#define EXC_TRANSLATOR(type)				\
  void exc_translate_ ## type(const type& err) {	\
    PyErr_SetString(PyExc_RuntimeError, err.what());	\
  }

EXC_TRANSLATOR(balance_error)
EXC_TRANSLATOR(interval_expr_error)
EXC_TRANSLATOR(format_error)
EXC_TRANSLATOR(parse_error)

value_t py_transaction_amount(transaction_t * xact) {
  return value_t(xact->amount);
}

transaction_t::state_t py_entry_state(entry_t * entry) {
  transaction_t::state_t state;
  if (entry->get_state(&state))
    return state;
  else
    return transaction_t::UNCLEARED;
}

void export_journal()
{
  scope().attr("TRANSACTION_NORMAL")	 = TRANSACTION_NORMAL;
  scope().attr("TRANSACTION_VIRTUAL")	 = TRANSACTION_VIRTUAL;
  scope().attr("TRANSACTION_BALANCE")	 = TRANSACTION_BALANCE;
  scope().attr("TRANSACTION_AUTO")	 = TRANSACTION_AUTO;
  scope().attr("TRANSACTION_BULK_ALLOC") = TRANSACTION_BULK_ALLOC;
  scope().attr("TRANSACTION_CALCULATED") = TRANSACTION_CALCULATED;

  enum_< transaction_t::state_t > ("State")
    .value("Uncleared", transaction_t::UNCLEARED)
    .value("Cleared",   transaction_t::CLEARED)
    .value("Pending",   transaction_t::PENDING)
    ;

  class_< transaction_t > ("Transaction")
    .def(init<optional<account_t *> >())
    .def(init<account_t *, amount_t, optional<unsigned int, const string&> >())

    .def(self == self)
    .def(self != self)

    .add_property("entry",
		  make_getter(&transaction_t::entry,
			      return_value_policy<reference_existing_object>()))
    .add_property("account",
		  make_getter(&transaction_t::account,
			      return_value_policy<reference_existing_object>()))

    .add_property("amount", &py_transaction_amount)
    .def_readonly("amount_expr", &transaction_t::amount_expr)
    .add_property("cost",
		  make_getter(&transaction_t::cost,
			      return_internal_reference<1>()))
    .def_readonly("cost_expr", &transaction_t::cost_expr)

    .def_readwrite("state", &transaction_t::state)
    .def_readwrite("flags", &transaction_t::flags)
    .def_readwrite("note", &transaction_t::note)

    .def_readonly("beg_pos", &transaction_t::beg_pos)
    .def_readonly("beg_line", &transaction_t::beg_line)
    .def_readonly("end_pos", &transaction_t::end_pos)
    .def_readonly("end_line", &transaction_t::end_line)

    .def("actual_date", &transaction_t::actual_date)
    .def("effective_date", &transaction_t::effective_date)
    .def("date", &transaction_t::date)

    .def("use_effective_date", &transaction_t::use_effective_date)

    .def("valid", &transaction_t::valid)
    ;

  class_< account_t >
    ("Account",
     init<optional<account_t *, string, string> >()
     [with_custodian_and_ward<1, 2>()])
    .def(self == self)
    .def(self != self)

    .def(self_ns::str(self))

    .def("__len__", accounts_len)
    .def("__getitem__", accounts_getitem, return_internal_reference<1>())

    .add_property("journal",
		  make_getter(&account_t::journal,
			      return_value_policy<reference_existing_object>()))
    .add_property("parent",
		  make_getter(&account_t::parent,
			      return_value_policy<reference_existing_object>()))
    .def_readwrite("name", &account_t::name)
    .def_readwrite("note", &account_t::note)
    .def_readonly("depth", &account_t::depth)
    .add_property("data", py_account_get_data, py_account_set_data)
    .def_readonly("ident", &account_t::ident)

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

    .def("__len__", entries_len)
    .def("__getitem__", entries_getitem, return_internal_reference<1>())

    .add_property("master", make_getter(&journal_t::master,
					return_internal_reference<1>()))
    .add_property("basket", make_getter(&journal_t::basket,
					return_internal_reference<1>()))

    .def_readonly("sources", &journal_t::sources)

    .def_readwrite("price_db", &journal_t::price_db)

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

    .def("finalize", &entry_base_t::finalize)
    .def("valid", &entry_base_t::valid)
    ;

  class_< entry_t, bases<entry_base_t> > ("Entry")
    .add_property("date", &entry_t::date)
    .add_property("effective_date", &entry_t::effective_date)
    .add_property("actual_date", &entry_t::actual_date)

    .def_readwrite("code", &entry_t::code)
    .def_readwrite("payee", &entry_t::payee)

    .add_property("state", &py_entry_state)

    .def("valid", &entry_t::valid)
    ;

#define EXC_TRANSLATE(type)					\
  register_exception_translator<type>(&exc_translate_ ## type);

  EXC_TRANSLATE(balance_error);
  EXC_TRANSLATE(interval_expr_error);
  EXC_TRANSLATE(format_error);
  EXC_TRANSLATE(parse_error);
}

#endif // USE_BOOST_PYTHON
#endif
