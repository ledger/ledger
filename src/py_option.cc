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

struct py_option_t : public option_t
{
  PyObject * self;

  py_option_t(PyObject * self_,
		      const string& long_opt,
		      const bool wants_arg)
    : self(self_), option_t(long_opt, wants_arg) {}

  virtual ~py_option_t() {}

  virtual bool check(option_source_t source) {
    return call_method<bool>(self, "check", source);
  }

  virtual void select(report_t * report, const char * optarg = NULL) {
    if (optarg)
      return call_method<void>(self, "select", report, optarg);
    else
      return call_method<void>(self, "select", report);
  }
};

BOOST_PYTHON_MEMBER_FUNCTION_OVERLOADS(option_select_overloads,
				       py_option_t::select, 1, 2)

typedef std::map<const string, object> options_map;

options_map options;

static option_t * find_option(const string& name)
{
  options_map::const_iterator i = options.find(name);
  if (i != options.end())
    return extract<py_option_t *>((*i).second.ptr());

  return NULL;
}

void shutdown_option()
{
  options.clear();
}

void export_option()
{
  class_< option_t, py_option_t, boost::noncopyable >
    ("Option", init<const string&, bool>())
    .def_readonly("long_opt", &py_option_t::long_opt)
    .def_readonly("short_opt", &py_option_t::short_opt)
    .def_readonly("wants_arg", &py_option_t::wants_arg)
    .def_readwrite("handled", &py_option_t::handled)
    .def("check", &py_option_t::check)
    .def("select", &py_option_t::select, option_select_overloads())
    ;

  enum_< option_t::option_source_t > ("OptionSource")
    .value("InitFile",    option_t::INIT_FILE)
    .value("Environment", option_t::ENVIRONMENT)
    .value("DataFile",    option_t::DATA_FILE)
    .value("CommandLine", option_t::COMMAND_LINE)
    ;

  class_< options_map > ("OptionsMap")
    .def(map_indexing_suite<options_map>())
    ;

  scope().attr("options") = ptr(&options);
}
