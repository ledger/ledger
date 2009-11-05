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

#include <system.hh>

#include "pyinterp.h"
#include "pyutils.h"
#include "hooks.h"
#include "journal.h"
#include "xact.h"

namespace ledger {

using namespace boost::python;

namespace {

  account_t& py_account_master(journal_t& journal) {
    return *journal.master;
  }

  commodity_pool_t& py_commodity_pool(journal_t& journal) {
    return *journal.commodity_pool;
  }

  long xacts_len(journal_t& journal)
  {
    return journal.xacts.size();
  }

  xact_t& xacts_getitem(journal_t& journal, long i)
  {
    static long last_index = 0;
    static journal_t * last_journal = NULL;
    static xacts_list::iterator elem;

    long len = journal.xacts.size();

    if (labs(i) >= len) {
      PyErr_SetString(PyExc_IndexError, _("Index out of range"));
      throw_error_already_set();
    }

    if (&journal == last_journal && i == last_index + 1) {
      last_index = i;
      return **++elem;
    }

    long x = i < 0 ? len + i : i;
    elem = journal.xacts.begin();
    while (--x >= 0)
      elem++;

    last_journal = &journal;
    last_index   = i;

    return **elem;
  }

  long accounts_len(account_t& account)
  {
    return account.accounts.size();
  }

  account_t& accounts_getitem(account_t& account, long i)
  {
    static long last_index = 0;
    static account_t * last_account = NULL;
    static accounts_map::iterator elem;

    long len = account.accounts.size();

    if (labs(i) >= len) {
      PyErr_SetString(PyExc_IndexError, _("Index out of range"));
      throw_error_already_set();
    }

    if (&account == last_account && i == last_index + 1) {
      last_index = i;
      return *(*++elem).second;
    }

    long x = i < 0 ? len + i : i;
    elem = account.accounts.begin();
    while (--x >= 0)
      elem++;

    last_account = &account;
    last_index   = i;

    return *(*elem).second;
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
    journal.xact_finalize_hooks.run_hooks(xact, post);
  }

} // unnamed namespace

void export_journal()
{
  class_< journal_t::fileinfo_t > ("FileInfo")
    .def(init<path>())

    .add_property("filename",
		  make_getter(&journal_t::fileinfo_t::filename),
		  make_setter(&journal_t::fileinfo_t::filename))
    .add_property("size",
		  make_getter(&journal_t::fileinfo_t::size),
		  make_setter(&journal_t::fileinfo_t::size))
    .add_property("modtime",
		  make_getter(&journal_t::fileinfo_t::modtime),
		  make_setter(&journal_t::fileinfo_t::modtime))
    .add_property("from_stream",
		  make_getter(&journal_t::fileinfo_t::from_stream),
		  make_setter(&journal_t::fileinfo_t::from_stream))
    ;

  class_< journal_t, boost::noncopyable > ("Journal")
    .def(init<path>())
    .def(init<string>())

    .add_property("master", make_getter(&journal_t::master,
					return_internal_reference<1>()))
    .add_property("basket",
		  make_getter(&journal_t::basket,
			      return_internal_reference<1>()),
		  make_setter(&journal_t::basket))
    .add_property("was_loaded", make_getter(&journal_t::was_loaded))
    .add_property("commodity_pool",
		  make_getter(&journal_t::commodity_pool,
			      return_internal_reference<1>()))
#if 0
    .add_property("xact_finalize_hooks",
		  make_getter(&journal_t::xact_finalize_hooks),
		  make_setter(&journal_t::xact_finalize_hooks))
#endif

    .def("add_account", &journal_t::add_account)
    .def("remove_account", &journal_t::remove_account)

    .def("find_account", py_find_account_1, return_internal_reference<1>())
    .def("find_account", py_find_account_2, return_internal_reference<1>())
    .def("find_account_re", &journal_t::find_account_re,
	 return_internal_reference<1>())

    .def("add_xact", &journal_t::add_xact)
    .def("remove_xact", &journal_t::remove_xact)

    .def("add_xact_finalizer", py_add_xact_finalizer)
    .def("remove_xact_finalizer", py_remove_xact_finalizer)
    .def("run_xact_finalizers", py_run_xact_finalizers)

    .def("__len__", xacts_len)
    .def("__getitem__", xacts_getitem, return_internal_reference<1>())

    .def("__iter__", range<return_internal_reference<> >
	 (&journal_t::xacts_begin, &journal_t::xacts_end))
    .def("xacts", range<return_internal_reference<> >
	 (&journal_t::xacts_begin, &journal_t::xacts_end))
    .def("auto_xacts", range<return_internal_reference<> >
	 (&journal_t::auto_xacts_begin, &journal_t::auto_xacts_end))
    .def("period_xacts", range<return_internal_reference<> >
	 (&journal_t::period_xacts_begin, &journal_t::period_xacts_end))
    .def("sources", range<return_internal_reference<> >
	 (&journal_t::sources_begin, &journal_t::sources_end))

    .def("clear_xdata", &journal_t::clear_xdata)

    .def("valid", &journal_t::valid)
    ;
}

} // namespace ledger
