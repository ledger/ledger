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

#include "pyinterp.h"

#include <boost/python/module_init.hpp>

namespace ledger {

using namespace boost::python;

void export_utils();
void export_times();
void export_amount();
void export_commodity();
#if 0
void export_balance();
void export_value();
void export_journal();
void export_parser();
void export_option();
void export_walk();
void export_report();
void export_format();
void export_valexpr();
#endif

void initialize_for_python()
{
  export_utils();
  export_times();
  export_amount();
  export_commodity();
#if 0
  export_balance();
  export_value();
  export_journal();
  export_parser();
  export_option();
  export_walk();
  export_format();
  export_report();
  export_valexpr();
#endif
}

struct python_run
{
  object result;

  python_run(python_interpreter_t * intepreter,
	     const string& str, int input_mode)
    : result(handle<>(borrowed(PyRun_String(str.c_str(), input_mode,
					    intepreter->nspace.ptr(),
					    intepreter->nspace.ptr())))) {}
  operator object() {
    return result;
  }
};

python_interpreter_t::python_interpreter_t(xml::xpath_t::scope_t * parent)
  : xml::xpath_t::scope_t(parent),
    mmodule(borrowed(PyImport_AddModule("__main__"))),
    nspace(handle<>(borrowed(PyModule_GetDict(mmodule.get()))))
{
  Py_Initialize();
  boost::python::detail::init_module("ledger", &initialize_for_python);
}

object python_interpreter_t::import(const string& str)
{
  assert(Py_IsInitialized());

  try {
    PyObject * mod = PyImport_Import(PyString_FromString(str.c_str()));
    if (! mod)
      throw_(std::logic_error, "Failed to import Python module " << str);

    object newmod(handle<>(borrowed(mod)));

#if 1
    // Import all top-level entries directly into the main namespace
    dict m_nspace(handle<>(borrowed(PyModule_GetDict(mod))));
    nspace.update(m_nspace);
#else
    nspace[string(PyModule_GetName(mod))] = newmod;
#endif
    return newmod;
  }
  catch (const error_already_set&) {
    PyErr_Print();
    throw_(std::logic_error, "Importing Python module " << str);
  }
}

object python_interpreter_t::eval(std::istream& in, py_eval_mode_t mode)
{
  bool	      first = true;
  string buffer;
  buffer.reserve(4096);

  while (! in.eof()) {
    char buf[256];
    in.getline(buf, 255);
    if (buf[0] == '!')
      break;
    if (first)
      first = false;
    else
      buffer += "\n";
    buffer += buf;
  }

  try {
    int input_mode;
    switch (mode) {
    case PY_EVAL_EXPR:  input_mode = Py_eval_input;   break;
    case PY_EVAL_STMT:  input_mode = Py_single_input; break;
    case PY_EVAL_MULTI: input_mode = Py_file_input;   break;
    }
    assert(Py_IsInitialized());
    return python_run(this, buffer, input_mode);
  }
  catch (const error_already_set&) {
    PyErr_Print();
    throw_(std::logic_error, "Evaluating Python code");
  }
}

object python_interpreter_t::eval(const string& str, py_eval_mode_t mode)
{
  try {
    int input_mode;
    switch (mode) {
    case PY_EVAL_EXPR:  input_mode = Py_eval_input;   break;
    case PY_EVAL_STMT:  input_mode = Py_single_input; break;
    case PY_EVAL_MULTI: input_mode = Py_file_input;   break;
    }
    assert(Py_IsInitialized());
    return python_run(this, str, input_mode);
  }
  catch (const error_already_set&) {
    PyErr_Print();
    throw_(std::logic_error, "Evaluating Python code");
  }
}

value_t python_interpreter_t::functor_t::operator()(xml::xpath_t::scope_t * locals)
{
  try {
    if (! PyCallable_Check(func.ptr())) {
      return extract<value_t>(func.ptr());
    } else {
      if (locals->args.size() > 0) {
	list arglist;
	foreach (const value_t& value, locals->args)
	  arglist.append(value);

	if (PyObject * val =
	    PyObject_CallObject(func.ptr(),
				boost::python::tuple(arglist).ptr())) {
	  value_t result = extract<value_t>(val)();
	  Py_DECREF(val);
	  return result;
	}
	else if (PyObject * err = PyErr_Occurred()) {
	  PyErr_Print();
	  throw_(xml::xpath_t::calc_error,
		 "While calling Python function '" /*<< name() <<*/ "'");
	} else {
	  assert(false);
	}
      } else {
	return call<value_t>(func.ptr());
      }
    }
  }
  catch (const error_already_set&) {
    PyErr_Print();
    throw_(xml::xpath_t::calc_error,
	   "While calling Python function '" /*<< name() <<*/ "'");
  }
}

value_t python_interpreter_t::lambda_t::operator()
  (xml::xpath_t::scope_t * locals)
{
  try {
    assert(locals->args.size() == 1);
    value_t item = locals->args[0];
    assert(item.is_type(value_t::XML_NODE));
    return call<value_t>(func.ptr(), item.as_xml_node());
  }
  catch (const error_already_set&) {
    PyErr_Print();
    throw_(xml::xpath_t::calc_error,
	   "While evaluating Python lambda expression");
  }
}

} // namespace ledger
