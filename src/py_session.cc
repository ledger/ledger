/*
 * Copyright (c) 2003-2023, John Wiegley.  All rights reserved.
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

namespace ledger {

using namespace python;
using namespace boost::python;

namespace {
  journal_t * py_read_journal(const string& pathname)
  {
    return python_session->read_journal(path(pathname));
  }

  journal_t * py_read_journal_from_string(const string& data)
  {
    return python_session->read_journal_from_string(data);
  }

  PyObject* py_error_context(const session_t& session)
  {
      return str_to_py_unicode(error_context());
  }
  void py_close_journal_files() {
    python_session->close_journal_files();
  }
}

void export_session()
{
  class_< session_t, boost::noncopyable > ("Session")
    .def("read_journal", &session_t::read_journal,
         return_internal_reference<>())
    .def("read_journal_from_string", &session_t::read_journal_from_string,
         return_internal_reference<>())
    .def("read_journal_files", &session_t::read_journal_files,
         return_internal_reference<>())
    .def("close_journal_files", &session_t::close_journal_files)
    .def("journal", &session_t::get_journal,
         return_internal_reference<>())
    .def("error_context", &py_error_context)
    ;

  scope().attr("session") =
    object(ptr(static_cast<session_t *>(python_session.get())));
  scope().attr("close_journal_files") =
    boost::python::make_function(&py_close_journal_files);
  scope().attr("read_journal") =
    boost::python::make_function(&py_read_journal,
                          return_internal_reference<>());
  scope().attr("read_journal_from_string") =
    boost::python::make_function(&py_read_journal_from_string,
                          return_internal_reference<>());
}

} // namespace ledger
