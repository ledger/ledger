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

#include "parser.h"

#if 0
#ifdef USE_BOOST_PYTHON

using namespace boost::python;
using namespace ledger;

struct py_parser_t : public parser_t
{
  PyObject * self;
  py_parser_t(PyObject * self_) : self(self_) {}

  virtual bool test(std::istream& in) const {
    return call_method<bool>(self, "test", in);
  }

  virtual repitem_t * parse(std::istream&       in,
			    journal_t *	        journal,
			    account_t *	        master	      = NULL,
			    const string * original_file = NULL) {
    return call_method<unsigned int>(self, "parse", in, journal, master,
				     original_file);
  }
};

BOOST_PYTHON_MEMBER_FUNCTION_OVERLOADS(parser_parse_overloads,
				       py_parser_t::parse, 2, 4)

BOOST_PYTHON_FUNCTION_OVERLOADS(parse_journal_overloads, parse_journal, 2, 4)
BOOST_PYTHON_FUNCTION_OVERLOADS(parse_journal_file_overloads,
				parse_journal_file, 2, 4)

void export_parser() {
  class_< parser_t, py_parser_t, boost::noncopyable > ("Parser")
    .def("test", &py_parser_t::test)
    .def("parse", &py_parser_t::parse, parser_parse_overloads())
    ;

  def("register_parser", register_parser);
  def("unregister_parser", unregister_parser);

  def("parse_journal", parse_journal, parse_journal_overloads());
  def("parse_journal_file", parse_journal_file, parse_journal_file_overloads());
}

#endif // USE_BOOST_PYTHON
#endif
