/*
 * Copyright (c) 2003-2025, John Wiegley.  All rights reserved.
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

/**
 * @file   py_journal.cc
 * @brief  Python bindings for journal_t -- the central accounting ledger.
 * @ingroup python
 *
 * Exposes journal_t to Python as the `ledger.Journal` class, along with
 * supporting types FileInfo (source file metadata) and PostCollectorWrapper
 * (query result iteration).  The journal is the top-level container holding
 * all transactions, accounts, and automated/periodic entries.  This binding
 * provides account lookup, transaction management, iterable access to
 * transactions and postings, and a query() method that runs Ledger's full
 * report pipeline and returns collected postings.
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

long xacts_len(journal_t& journal) {
  return static_cast<long>(journal.xacts.size());
}

/*--- Account Lookup Wrappers ---*/

account_t* py_find_account_1(journal_t& journal, const string& name) {
  return journal.find_account(name);
}

account_t* py_find_account_2(journal_t& journal, const string& name, const bool auto_create) {
  return journal.find_account(name, auto_create);
}

account_t* py_register_account(journal_t& journal, const string& name, post_t* post) {
  return journal.register_account(name, post, journal.master);
}

/*--- Query Result Collection ---*/

/// The chain a grouped Journal.query() flushes each --group-by group
/// through.  post_splitter clears its chain after every group; here that
/// retires the group's chain, still alive with the postings it collected
/// and the synthetic postings its filters own, and starts the next group
/// on a fresh chain.  Chains are released newest first.
class group_chains : public item_handler<post_t> {
  std::function<post_handler_ptr()> make_chain;
  std::vector<post_handler_ptr> retired;

public:
  explicit group_chains(std::function<post_handler_ptr()> _make_chain)
      : item_handler<post_t>(_make_chain()), make_chain(std::move(_make_chain)) {}

  /// A later group's chain adopts the temporary accounts (--account,
  /// --inject) that an earlier chain registered under journal->master and
  /// owns, and each chain's ~temporaries_t reads its postings' accounts.
  /// Release the newest chain first so that no chain outlives the chain
  /// whose accounts it points into; destroying a std::vector's elements
  /// is left to the library's own order (front to back with libstdc++).
  ~group_chains() override {
    handler.reset();
    while (!retired.empty())
      retired.pop_back();
  }

  void clear() override {
    retired.push_back(std::move(handler));
    handler = make_chain();
  }
};

/// A collect_posts that also records each posting's display amount: the
/// report's --display-amount expression, which carries -X/-V exchange and
/// other display transforms (#2158).  It is evaluated as the posting
/// arrives, while the chain's xdata for it is intact; an error raises, as
/// it does in every other report.
class collect_display_posts : public collect_posts {
  report_t& report;

public:
  std::vector<value_t> display_amounts; ///< Display amount of each entry of posts.

  explicit collect_display_posts(report_t& _report) : report(_report) {
    TRACE_CTOR(collect_display_posts, "report_t&");
  }
  ~collect_display_posts() override { TRACE_DTOR(collect_display_posts); }

  void operator()(post_t& post) override {
    bind_scope_t bound_scope(report, post);
    display_amounts.push_back(report.HANDLER(display_amount_).expr.calc(bound_scope));
    collect_posts::operator()(post);
  }

  void clear() override {
    display_amounts.clear();
    collect_posts::clear();
  }
};

/// Wraps a report_t and a collect_display_posts filter to hold query
/// results: the collected postings and their display amounts.  Owns the
/// full filter chain (handler_chain) so that any synthetic posts created by
/// interval or period filters remain valid while Python iterates.
struct collector_wrapper {
  boost::shared_ptr<journal_t> journal_sp;
  report_t report;
  post_handler_ptr handler_chain; // Keeps the filter chain alive (owns synthetic temp posts)
  std::shared_ptr<collect_display_posts> posts_collector;

  collector_wrapper(boost::shared_ptr<journal_t> _journal_sp, report_t& base)
      : journal_sp(std::move(_journal_sp)), report(base),
        posts_collector(std::make_shared<collect_display_posts>(report)) {
    TRACE_CTOR(collector_wrapper, "boost::shared_ptr<journal_t>, report_t&");
  }
  ~collector_wrapper() {
    TRACE_DTOR(collector_wrapper);
    journal_sp->clear_xdata();
  }

  std::size_t length() const { return posts_collector->length(); }

  std::vector<post_t*>::iterator begin() { return posts_collector->begin(); }
  std::vector<post_t*>::iterator end() { return posts_collector->end(); }
};

/// Runs a Ledger query string against the journal, building the full
/// report filter chain and collecting matching posts into a
/// PostCollectorWrapper that Python can iterate over.
shared_ptr<collector_wrapper> py_query(boost::shared_ptr<journal_t> journal_sp,
                                       const string& query) {
  journal_t& journal = *journal_sp;
  if (journal.has_xdata()) {
    PyErr_SetString(PyExc_RuntimeError, _("Cannot have more than one active journal query"));
    throw_error_already_set();
  }

  report_t& current_report(downcast<report_t>(*scope_t::default_scope));
  shared_ptr<collector_wrapper> coll(new collector_wrapper(journal_sp, current_report));
  epoch_scope_t clock; // A --now given to this Journal.query ends with it.

  boost::shared_ptr<journal_t> save_journal = coll->report.session.journal;
  coll->report.session.journal = journal_sp;

  try {
    // Parse this query's options under the globals of the report it copies,
    // not those the previous query's options left.
    coll->report.apply_option_globals();
    strings_list remaining = process_arguments(split_arguments(query.c_str()), coll->report);
    coll->report.normalize_options("register");

    value_t args;
    for (const string& arg : remaining)
      args.push_back(string_value(arg));
    coll->report.parse_query_args(args, "@Journal.query");

    // handler_chain keeps every filter alive, and with them every
    // synthetic posting (interval_posts, changed_value_posts, ...) the
    // collector points at, while Python iterates.  group_chains gives
    // each --group-by group a chain of its own and keeps the finished
    // ones; without --group-by it holds the one chain.  walk_posts
    // clears the journal's xdata between groups (unless cumulative) and
    // when the walk ends.
    collector_wrapper* c = coll.get();
    coll->handler_chain = coll->report.walk_posts(std::make_shared<group_chains>(
        [c] { return chain_post_handlers(c->posts_collector, c->report); }));
  } catch (...) {
    coll->report.session.journal = save_journal;
    throw;
  }
  coll->report.session.journal = save_journal;

  return coll;
}

post_t* posts_getitem(collector_wrapper& collector, long i) {
  std::vector<post_t*>& posts = collector.posts_collector->posts;
  return posts[python::py_sequence_index(i, posts.size())];
}

/// Convert the display_amounts vector to a Python list of Value objects.
boost::python::object py_collector_display_amounts(collector_wrapper& coll) {
  return python::to_py_list(coll.posts_collector->display_amounts.begin(),
                            coll.posts_collector->display_amounts.end(),
                            [](const value_t& v) -> const value_t& { return v; });
}

/*--- FileInfo Property Wrappers ---*/

/// Adapters for journal_t::fileinfo_t fields that convert between
/// C++ optional/path types and Python None/str.
object py_fileinfo_filename(const journal_t::fileinfo_t& fi) {
  if (fi.filename)
    return object(fi.filename->native());
  return object();
}

void py_fileinfo_set_filename(journal_t::fileinfo_t& fi, const object& obj) {
  if (obj.is_none())
    fi.filename = none;
  else
    fi.filename = path(extract<std::string>(obj)());
}

object py_fileinfo_modtime(const journal_t::fileinfo_t& fi) {
  if (fi.modtime.is_not_a_date_time())
    return object();
  return object(fi.modtime);
}

void py_fileinfo_set_modtime(journal_t::fileinfo_t& fi, const object& obj) {
  if (obj.is_none())
    fi.modtime = datetime_t();
  else
    fi.modtime = extract<datetime_t>(obj)();
}

boost::python::list py_find_accounts_re(journal_t& journal, const string& regexp) {
  auto accounts = journal.find_accounts_re(regexp);
  return python::to_py_list(accounts.begin(), accounts.end(),
                            [](account_t* a) { return boost::python::ptr(a); });
}

} // unnamed namespace

void export_journal() {
  class_<item_handler<post_t>, shared_ptr<item_handler<post_t>>, boost::noncopyable>("PostHandler");

  class_<collector_wrapper, shared_ptr<collector_wrapper>, boost::noncopyable>(
      "PostCollectorWrapper", no_init)
      .def("__len__", &collector_wrapper::length)
      .def("__getitem__", posts_getitem, return_internal_reference<>())
      .def("__iter__", boost::python::range<return_internal_reference<>>(&collector_wrapper::begin,
                                                                         &collector_wrapper::end))
      .add_property("display_amounts", py_collector_display_amounts);

  class_<journal_t::fileinfo_t>("FileInfo")
      .def(init<path>())

      .add_property("filename", make_function(py_fileinfo_filename),
                    make_function(py_fileinfo_set_filename))
      .add_property("modtime", make_function(py_fileinfo_modtime),
                    make_function(py_fileinfo_set_modtime))
      .add_property("from_stream", make_getter(&journal_t::fileinfo_t::from_stream),
                    make_setter(&journal_t::fileinfo_t::from_stream));

  class_<journal_t, boost::shared_ptr<journal_t>, boost::noncopyable>("Journal")
      .add_property(
          "master",
          make_getter(&journal_t::master,
                      return_internal_reference<1, with_custodian_and_ward_postcall<1, 0>>()))
      .add_property(
          "bucket",
          make_getter(&journal_t::bucket,
                      return_internal_reference<1, with_custodian_and_ward_postcall<1, 0>>()),
          make_setter(&journal_t::bucket))
      .add_property("was_loaded", make_getter(&journal_t::was_loaded))

      .def("add_account", &journal_t::add_account)
      .def("remove_account", &journal_t::remove_account)

      .def("find_account", py_find_account_1,
           return_internal_reference<1, with_custodian_and_ward_postcall<1, 0>>())
      .def("find_account", py_find_account_2,
           return_internal_reference<1, with_custodian_and_ward_postcall<1, 0>>())
      .def("find_account_re", &journal_t::find_account_re,
           return_internal_reference<1, with_custodian_and_ward_postcall<1, 0>>())
      .def("find_accounts_re", py_find_accounts_re)

      .def("register_account", py_register_account,
           return_internal_reference<1, with_custodian_and_ward_postcall<1, 0>>())

      .def("expand_aliases", &journal_t::expand_aliases,
           return_internal_reference<1, with_custodian_and_ward_postcall<1, 0>>())

      .def("add_xact", &journal_t::add_xact)
      .def("remove_xact", &journal_t::remove_xact)

      .def("__len__", xacts_len)

      .def("__iter__", boost::python::range<return_internal_reference<>>(&journal_t::xacts_begin,
                                                                         &journal_t::xacts_end))
      .add_property("xacts", boost::python::range<return_internal_reference<>>(
                                 &journal_t::xacts_begin, &journal_t::xacts_end))
      .add_property("auto_xacts", boost::python::range<return_internal_reference<>>(
                                      &journal_t::auto_xacts_begin, &journal_t::auto_xacts_end))
      .add_property("period_xacts",
                    boost::python::range<return_internal_reference<>>(
                        &journal_t::period_xacts_begin, &journal_t::period_xacts_end))
      .add_property("sources", boost::python::range<return_internal_reference<>>(
                                   &journal_t::sources_begin, &journal_t::sources_end))
      .def("has_xdata", &journal_t::has_xdata)
      .def("clear_xdata", &journal_t::clear_xdata)

      .def("query", py_query)

      .def("valid", &journal_t::valid);

  python::register_error_translator<parse_error>(PyExc_RuntimeError);
  python::register_error_translator<error_count>(PyExc_RuntimeError);
}

} // namespace ledger
