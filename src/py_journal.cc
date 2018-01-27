/*
 * Copyright (c) 2003-2018, John Wiegley.  All rights reserved.
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
#include "journal.h"
#include "xact.h"
#include "post.h"
#include "chain.h"
#include "filters.h"
#include "iterators.h"
#include "scope.h"
#include "report.h"

namespace ledger {

using namespace boost::python;

namespace {

#if 0
  account_t& py_account_master(journal_t& journal) {
    return *journal.master;
  }
#endif

  long xacts_len(journal_t& journal)
  {
    return static_cast<long>(journal.xacts.size());
  }

#if 0
  xact_t& xacts_getitem(journal_t& journal, long i)
  {
    static long last_index = 0;
    static journal_t * last_journal = NULL;
    static xacts_list::iterator elem;

    long len = static_cast<long>(journal.xacts.size());

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
    return static_cast<long>(account.accounts.size());
  }

  account_t& accounts_getitem(account_t& account, long i)
  {
    static long last_index = 0;
    static account_t * last_account = NULL;
    static accounts_map::iterator elem;

    long len = static_cast<long>(account.accounts.size());

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
#endif

  account_t * py_find_account_1(journal_t& journal, const string& name)
  {
    return journal.find_account(name);
  }

  account_t * py_find_account_2(journal_t& journal, const string& name,
                                const bool auto_create)
  {
    return journal.find_account(name, auto_create);
  }

  account_t * py_register_account(journal_t& journal, const string& name, post_t* post)
  {
    return journal.register_account(name, post, journal.master);
  }

#if 0
  std::size_t py_read(journal_t& journal, const string& pathname)
  {
    return journal.read(context_stack);
  }
#endif

  struct collector_wrapper
  {
    journal_t& journal;
    report_t   report;

    post_handler_ptr posts_collector;

    collector_wrapper(journal_t& _journal, report_t& base)
      : journal(_journal), report(base),
        posts_collector(new collect_posts) {
      TRACE_CTOR(collector_wrapper, "journal_t&, report_t&");
    }
    ~collector_wrapper() {
      TRACE_DTOR(collector_wrapper);
      journal.clear_xdata();
    }

    std::size_t length() const {
      return dynamic_cast<collect_posts *>(posts_collector.get())->length();
    }

    std::vector<post_t *>::iterator begin() {
      return dynamic_cast<collect_posts *>(posts_collector.get())->begin();
    }
    std::vector<post_t *>::iterator end() {
      return dynamic_cast<collect_posts *>(posts_collector.get())->end();
    }
  };

  shared_ptr<collector_wrapper> py_query(journal_t& journal,
                                         const string& query)
  {
    if (journal.has_xdata()) {
      PyErr_SetString(PyExc_RuntimeError,
                      _("Cannot have more than one active journal query"));
      throw_error_already_set();
    }

    report_t& current_report(downcast<report_t>(*scope_t::default_scope));
    shared_ptr<collector_wrapper>
      coll(new collector_wrapper(journal, current_report));

    unique_ptr<journal_t> save_journal(coll->report.session.journal.release());
    coll->report.session.journal.reset(&coll->journal);

    try {
      strings_list remaining =
        process_arguments(split_arguments(query.c_str()), coll->report);
      coll->report.normalize_options("register");

      value_t args;
      foreach (const string& arg, remaining)
        args.push_back(string_value(arg));
      coll->report.parse_query_args(args, "@Journal.query");

      coll->report.posts_report(coll->posts_collector);
    }
    catch (...) {
      coll->report.session.journal.release();
      coll->report.session.journal.reset(save_journal.release());
      throw;
    }
    coll->report.session.journal.release();
    coll->report.session.journal.reset(save_journal.release());

    return coll;
  }

  post_t * posts_getitem(collector_wrapper& collector, long i)
  {
    return dynamic_cast<collect_posts *>(collector.posts_collector.get())
      ->posts[static_cast<std::size_t>(i)];
  }

} // unnamed namespace

#define EXC_TRANSLATOR(type)                            \
  void exc_translate_ ## type(const type& err) {        \
    PyErr_SetString(PyExc_RuntimeError, err.what()); \
  }

EXC_TRANSLATOR(parse_error)
EXC_TRANSLATOR(error_count)

void export_journal()
{
  class_< item_handler<post_t>, shared_ptr<item_handler<post_t> >,
          boost::noncopyable >("PostHandler")
    ;

  class_< collector_wrapper, shared_ptr<collector_wrapper>,
          boost::noncopyable >("PostCollectorWrapper", no_init)
    .def("__len__", &collector_wrapper::length)
    .def("__getitem__", posts_getitem, return_internal_reference<>())
    .def("__iter__", python::range<return_internal_reference<> >
         (&collector_wrapper::begin, &collector_wrapper::end))
    ;

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
#if 0
    .def(init<path>())
    .def(init<string>())
#endif
    .add_property("master",
                  make_getter(&journal_t::master,
                              return_internal_reference<1,
                                  with_custodian_and_ward_postcall<1, 0> >()))
    .add_property("bucket",
                  make_getter(&journal_t::bucket,
                              return_internal_reference<1,
                                  with_custodian_and_ward_postcall<1, 0> >()),
                  make_setter(&journal_t::bucket))
    .add_property("was_loaded", make_getter(&journal_t::was_loaded))

    .def("add_account", &journal_t::add_account)
    .def("remove_account", &journal_t::remove_account)

    .def("find_account", py_find_account_1,
         return_internal_reference<1,
             with_custodian_and_ward_postcall<1, 0> >())
    .def("find_account", py_find_account_2,
         return_internal_reference<1,
             with_custodian_and_ward_postcall<1, 0> >())
    .def("find_account_re", &journal_t::find_account_re,
         return_internal_reference<1,
             with_custodian_and_ward_postcall<1, 0> >())

    .def("register_account", py_register_account,
         return_internal_reference<1,
             with_custodian_and_ward_postcall<1, 0> >())

    .def("expand_aliases", &journal_t::expand_aliases,
         return_internal_reference<1,
             with_custodian_and_ward_postcall<1, 0> >())

    .def("add_xact", &journal_t::add_xact)
    .def("remove_xact", &journal_t::remove_xact)

    .def("__len__", xacts_len)
#if 0
    .def("__getitem__", xacts_getitem,
         return_internal_reference<1,
             with_custodian_and_ward_postcall<1, 0> >())
#endif

    .def("__iter__", python::range<return_internal_reference<> >
         (&journal_t::xacts_begin, &journal_t::xacts_end))
    .def("xacts", python::range<return_internal_reference<> >
         (&journal_t::xacts_begin, &journal_t::xacts_end))
    .def("auto_xacts", python::range<return_internal_reference<> >
         (&journal_t::auto_xacts_begin, &journal_t::auto_xacts_end))
    .def("period_xacts", python::range<return_internal_reference<> >
         (&journal_t::period_xacts_begin, &journal_t::period_xacts_end))
    .def("sources", python::range<return_internal_reference<> >
         (&journal_t::sources_begin, &journal_t::sources_end))
#if 0
    .def("read", py_read)
#endif
    .def("has_xdata", &journal_t::has_xdata)
    .def("clear_xdata", &journal_t::clear_xdata)

    .def("query", py_query)

    .def("valid", &journal_t::valid)
    ;

#define EXC_TRANSLATE(type) \
  register_exception_translator<type>(&exc_translate_ ## type);

  EXC_TRANSLATE(parse_error);
  EXC_TRANSLATE(error_count);
}

} // namespace ledger
