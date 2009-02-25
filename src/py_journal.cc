/*
 * Copyright (c) 2003-2009, John Wiegley.  All rights reserved.
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

#include "pyinterp.h"
#include "pyutils.h"
#include "journal.h"

namespace ledger {

using namespace boost::python;

#define EXC_TRANSLATOR(type)				\
  void exc_translate_ ## type(const type& err) {	\
    PyErr_SetString(PyExc_ArithmeticError, err.what());	\
  }

//EXC_TRANSLATOR(journal_error)

void export_journal()
{
#if 0
  class_< journal_t > ("Journal")
    ;
#endif

  //register_optional_to_python<amount_t>();

  //implicitly_convertible<string, amount_t>();

#define EXC_TRANSLATE(type) \
  register_exception_translator<type>(&exc_translate_ ## type);

  //EXC_TRANSLATE(journal_error);
}

} // namespace ledger

#if 0
xact_t& post_xact(const post_t& post)
{
  return *post.xact;
}

unsigned int posts_len(xact_base_t& xact)
{
  return xact.posts.size();
}

post_t& posts_getitem(xact_base_t& xact, int i)
{
  static int last_index = 0;
  static xact_base_t * last_xact = NULL;
  static posts_list::iterator elem;

  std::size_t len = xact.posts.size();

  if (abs(i) >= len) {
    PyErr_SetString(PyExc_IndexError, _("Index out of range"));
    throw_error_already_set();
  }

  if (&xact == last_xact && i == last_index + 1) {
    last_index = i;
    return **++elem;
  }

  int x = i < 0 ? len + i : i;
  elem = xact.posts.begin();
  while (--x >= 0)
    elem++;

  last_xact = &xact;
  last_index = i;

  return **elem;
}

unsigned int xacts_len(journal_t& journal)
{
  return journal.xacts.size();
}

xact_t& xacts_getitem(journal_t& journal, int i)
{
  static int last_index = 0;
  static journal_t * last_journal = NULL;
  static xacts_list::iterator elem;

  std::size_t len = journal.xacts.size();

  if (abs(i) >= len) {
    PyErr_SetString(PyExc_IndexError, _("Index out of range"));
    throw_error_already_set();
  }

  if (&journal == last_journal && i == last_index + 1) {
    last_index = i;
    return **++elem;
  }

  int x = i < 0 ? len + i : i;
  elem = journal.xacts.begin();
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
    PyErr_SetString(PyExc_IndexError, _("Index out of range"));
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

bool py_add_xact(journal_t& journal, xact_t * xact) {
  return journal.add_xact(new xact_t(*xact));
}

void py_add_post(xact_base_t& xact, post_t * post) {
  return xact.add_post(new post_t(*post));
}

struct xact_base_wrap : public xact_base_t
{
  PyObject * self;
  xact_base_wrap(PyObject * self_) : self(self_) {}

  virtual bool valid() const {
    return call_method<bool>(self, "valid");
  }
};

struct py_xact_finalizer_t : public xact_finalizer_t {
  object pyobj;
  py_xact_finalizer_t() {}
  py_xact_finalizer_t(object obj) : pyobj(obj) {}
  py_xact_finalizer_t(const py_xact_finalizer_t& other)
    : pyobj(other.pyobj) {}
  virtual bool operator()(xact_t& xact, bool post) {
    return call<bool>(pyobj.ptr(), xact, post);
  }
};

std::list<py_xact_finalizer_t> py_finalizers;

void py_add_xact_finalizer(journal_t& journal, object x)
{
  py_finalizers.push_back(py_xact_finalizer_t(x));
  journal.add_xact_finalizer(&py_finalizers.back());
}

void py_remove_xact_finalizer(journal_t& journal, object x)
{
  for (std::list<py_xact_finalizer_t>::iterator i = py_finalizers.begin();
       i != py_finalizers.end();
       i++)
    if ((*i).pyobj == x) {
      journal.remove_xact_finalizer(&(*i));
      py_finalizers.erase(i);
      return;
    }
}

void py_run_xact_finalizers(journal_t& journal, xact_t& xact, bool post)
{
  run_hooks(journal.xact_finalize_hooks, xact, post);
}

#define EXC_TRANSLATOR(type)				\
  void exc_translate_ ## type(const type& err) {	\
    PyErr_SetString(PyExc_RuntimeError, err.what());	\
  }

EXC_TRANSLATOR(balance_error)
EXC_TRANSLATOR(interval_expr_error)
EXC_TRANSLATOR(format_error)
EXC_TRANSLATOR(parse_error)

value_t py_post_amount(post_t * post) {
  return value_t(post->amount);
}

post_t::state_t py_xact_state(xact_t * xact) {
  post_t::state_t state;
  if (xact->get_state(&state))
    return state;
  else
    return post_t::UNCLEARED;
}

void export_journal()
{
  scope().attr("POST_NORMAL")     = POST_NORMAL;
  scope().attr("POST_VIRTUAL")    = POST_VIRTUAL;
  scope().attr("POST_BALANCE")    = POST_BALANCE;
  scope().attr("POST_AUTO")	  = POST_AUTO;
  scope().attr("POST_BULK_ALLOC") = POST_BULK_ALLOC;
  scope().attr("POST_CALCULATED") = POST_CALCULATED;

  enum_< post_t::state_t > ("State")
    .value("Uncleared", post_t::UNCLEARED)
    .value("Cleared",   post_t::CLEARED)
    .value("Pending",   post_t::PENDING)
    ;

  class_< post_t > ("Post")
    .def(init<optional<account_t *> >())
    .def(init<account_t *, amount_t, optional<unsigned int, const string&> >())

    .def(self == self)
    .def(self != self)

    .add_property("xact",
		  make_getter(&post_t::xact,
			      return_value_policy<reference_existing_object>()))
    .add_property("account",
		  make_getter(&post_t::account,
			      return_value_policy<reference_existing_object>()))

    .add_property("amount", &py_post_amount)
    .def_readonly("amount_expr", &post_t::amount_expr)
    .add_property("cost",
		  make_getter(&post_t::cost,
			      return_internal_reference<1>()))
    .def_readonly("cost_expr", &post_t::cost_expr)

    .def_readwrite("state", &post_t::state)
    .def_readwrite("flags", &post_t::flags)
    .def_readwrite("note", &post_t::note)

    .def_readonly("beg_pos", &post_t::beg_pos)
    .def_readonly("beg_line", &post_t::beg_line)
    .def_readonly("end_pos", &post_t::end_pos)
    .def_readonly("end_line", &post_t::end_line)

    .def("actual_date", &post_t::actual_date)
    .def("effective_date", &post_t::effective_date)
    .def("date", &post_t::date)

    .def("use_effective_date", &post_t::use_effective_date)

    .def("valid", &post_t::valid)
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

    .def("__len__", xacts_len)
    .def("__getitem__", xacts_getitem, return_internal_reference<1>())

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

    .def("add_xact", py_add_xact)
    .def("remove_xact", &journal_t::remove_xact)

    .def("add_xact_finalizer", py_add_xact_finalizer)
    .def("remove_xact_finalizer", py_remove_xact_finalizer)
    .def("run_xact_finalizers", py_run_xact_finalizers)

    .def("valid", &journal_t::valid)
    ;

  class_< xact_base_t, xact_base_wrap, boost::noncopyable > ("XactBase")
    .def("__len__", posts_len)
    .def("__getitem__", posts_getitem,
	 return_internal_reference<1>())

    .def_readonly("journal", &xact_base_t::journal)

    .def_readonly("src_idx", &xact_base_t::src_idx)
    .def_readonly("beg_pos", &xact_base_t::beg_pos)
    .def_readonly("beg_line", &xact_base_t::beg_line)
    .def_readonly("end_pos", &xact_base_t::end_pos)
    .def_readonly("end_line", &xact_base_t::end_line)

    .def("add_post", py_add_post)
    .def("remove_post", &xact_base_t::remove_post)

    .def(self == self)
    .def(self != self)

    .def("finalize", &xact_base_t::finalize)
    .def("valid", &xact_base_t::valid)
    ;

  class_< xact_t, bases<xact_base_t> > ("Xact")
    .add_property("date", &xact_t::date)
    .add_property("effective_date", &xact_t::effective_date)
    .add_property("actual_date", &xact_t::actual_date)

    .def_readwrite("code", &xact_t::code)
    .def_readwrite("payee", &xact_t::payee)

    .add_property("state", &py_xact_state)

    .def("valid", &xact_t::valid)
    ;

#define EXC_TRANSLATE(type)					\
  register_error_translator<type>(&exc_translate_ ## type);

  EXC_TRANSLATE(balance_error);
  EXC_TRANSLATE(interval_expr_error);
  EXC_TRANSLATE(format_error);
  EXC_TRANSLATE(parse_error);
}
#endif
