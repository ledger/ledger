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

#include <system.hh>

#include "pyinterp.h"
#include "pyutils.h"
#include "error.h"
#include "session.h"
#include "pool.h"

namespace ledger {

using namespace python;
using namespace boost::python;

namespace {
boost::shared_ptr<journal_t> py_read_journal(const string& pathname) {
  python_session->read_journal(path(pathname));
  return python_session->journal;
}

boost::shared_ptr<journal_t> py_read_journal_from_string(const string& data) {
  python_session->read_journal_from_string(data);
  return python_session->journal;
}

PyObject* py_error_context([[maybe_unused]] const session_t& session) {
  return str_to_py_unicode(error_context());
}
// After close_journal_files() replaces the commodity pool, rebind the
// module-level `ledger.commodities` attribute to the new pool.  Without
// this, any commodity_t* obtained from the old pool carries a graph_index
// into the old pool's Boost.Graph price-history; using it as `target` in
// find_price() on the new pool causes an out-of-bounds vertex() call and
// the this=0x0 segfault described in issue #976.
void py_update_commodities() {
  object main_module = import("ledger");
  main_module.attr("commodities") = commodity_pool_t::current_pool;
}

void py_close_journal_files() {
  python_session->close_journal_files();
  py_update_commodities();
}

void py_session_close_journal_files(session_t& session) {
  session.close_journal_files();
  py_update_commodities();
}

boost::shared_ptr<journal_t> py_session_read_journal(session_t& session, const path& pathname) {
  session.read_journal(pathname);
  return session.journal;
}

boost::shared_ptr<journal_t> py_session_read_journal_from_string(session_t& session,
                                                                 const string& data) {
  session.read_journal_from_string(data);
  return session.journal;
}

boost::shared_ptr<journal_t> py_session_read_journal_files(session_t& session) {
  session.read_journal_files();
  return session.journal;
}

boost::shared_ptr<journal_t> py_session_get_journal(session_t& session) {
  return session.journal;
}
} // namespace

void export_session() {
  class_<session_t, boost::noncopyable>("Session")
      .def("read_journal", py_session_read_journal)
      .def("read_journal_from_string", py_session_read_journal_from_string)
      .def("read_journal_files", py_session_read_journal_files)
      .def("close_journal_files", &py_session_close_journal_files)
      .def("journal", py_session_get_journal)
      .def("error_context", &py_error_context);

  scope().attr("session") = object(ptr(static_cast<session_t*>(python_session.get())));
  scope().attr("close_journal_files") = boost::python::make_function(&py_close_journal_files);
  scope().attr("read_journal") = boost::python::make_function(&py_read_journal);
  scope().attr("read_journal_from_string") =
      boost::python::make_function(&py_read_journal_from_string);
}

} // namespace ledger
