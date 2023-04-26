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

#include <ledger.hh>

#include "pyinterp.h"

using namespace boost::python;

namespace ledger {
  extern void initialize_for_python();
}

BOOST_PYTHON_MODULE(ledger)
{
  using namespace ledger;

  scope().attr("__author__") = "John Wiegley <jwiegley@gmail.com>";
  scope().attr("__version__") = Ledger_VERSION;
  scope().attr("__date__") = Ledger_VERSION_DATE;
  scope().attr("__doc__") =
    "Python API Documentation\n\n"
    "Documentation of the Ledger Python API is an ongoing process and you are invited\n"
    "to help out and contribute. In case you find this documentation incorrect,\n"
    "incomplete, unclear, or lacking please open a pull request at\n"
    "https://git.ledger-cli.org/ledger/pulls."
    ;

#if !DEBUG_MODE
  docstring_options doc_options;
  doc_options.disable_cpp_signatures();
#endif


  if (! python_session.get())
    python_session.reset(new python_interpreter_t);

  set_session_context(python_session.get());

  initialize_for_python();
}
