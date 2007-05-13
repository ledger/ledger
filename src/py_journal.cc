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
    ("Account", init<optional<account_t *, string, string> >()
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
  register_error_translator<type>(&exc_translate_ ## type);

  EXC_TRANSLATE(balance_error);
  EXC_TRANSLATE(interval_expr_error);
  EXC_TRANSLATE(format_error);
  EXC_TRANSLATE(parse_error);
}
