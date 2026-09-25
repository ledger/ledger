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

#include <ledger.hh>

#include "pyinterp.h"
#include "report.h"

using namespace boost::python;

namespace ledger {
extern void initialize_for_python();
}

namespace {

/// Callback registered with Python's `atexit` module to drop the global
/// python_session while the interpreter is still fully alive.  When ledger
/// is loaded as a Python extension module, the shared_ptr would otherwise
/// be destroyed by C++ static destructors after Py_Finalize has already
/// released the GIL, causing boost::python::object destructors to crash
/// while decrementing refcounts on torn-down Python objects.
///
/// This function is exposed to Python as `ledger._release_session`.
void release_python_session() {
  ledger::python_session.reset();
}

} // namespace

BOOST_PYTHON_MODULE(_core) {
  using namespace ledger;

  scope().attr("__author__") = "John Wiegley <jwiegley@gmail.com>";
  scope().attr("__version__") = Ledger_VERSION;
  scope().attr("__date__") = Ledger_VERSION_DATE;
  scope().attr("__doc__") =
      "Python API Documentation\n\n"
      "Documentation of the Ledger Python API is an ongoing process and you are invited\n"
      "to help out and contribute. In case you find this documentation incorrect,\n"
      "incomplete, unclear, or lacking please open a pull request at\n"
      "https://git.ledger-cli.org/ledger/pulls.";

#if !DEBUG_MODE
  docstring_options doc_options;
  doc_options.disable_cpp_signatures();
#endif

  if (!python_session.get())
    python_session.reset(new python_interpreter_t);

  set_session_context(python_session.get());

  // When imported as a standalone Python module (not via the ledger CLI),
  // global_scope_t is never created, so default_scope and empty_scope are
  // still null.  Initialize them here, before initialize_for_python() calls
  // export_session(), so that export_session() captures the correct and
  // stable python_session pointer.  Without this, a second python_session
  // reset inside initialize_for_python() would replace the object whose raw
  // pointer was already registered as the Python `session` attribute, leaving
  // it dangling (GitHub issue #2163).
  if (!scope_t::default_scope) {
    std::shared_ptr<session_t> session_ptr = python_session;
    scope_t::default_scope = new report_t(*session_ptr);
    scope_t::empty_scope   = new empty_scope_t;
  }

  initialize_for_python();

  // Register release_python_session with Python's `atexit` module so the
  // shared_ptr is dropped before Py_Finalize runs.  Py_AtExit callbacks
  // fire during finalization when the GIL has already been released, so
  // destroying boost::python objects there is unsafe; atexit callbacks run
  // earlier, while the interpreter is fully alive.  Without this, static
  // C++ destructors tear down python_session after Python is gone,
  // crashing in boost::python object destructors (GitHub issue #513).
  object release_fn = make_function(&release_python_session);
  scope().attr("_release_session") = release_fn;
  try {
    import("atexit").attr("register")(release_fn);
  } catch (const error_already_set&) {
    PyErr_Clear();
  }
}
