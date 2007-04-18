#include "py_eval.h"
#include "error.h"
#include "acconf.h"

#include <istream>

void export_amount();
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

void shutdown_option();
#endif

namespace ledger {

void initialize_ledger_for_python()
{
  export_amount();
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

void shutdown_ledger_for_python()
{
#if 0
  shutdown_option();
#endif
}

struct python_run
{
  object result;
  python_run(python_interpreter_t * intepreter,
	     const std::string& str, int input_mode)
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
  detail::init_module("ledger", &initialize_ledger_for_python);
}

object python_interpreter_t::import(const std::string& str)
{
  assert(Py_IsInitialized());

  try {
    PyObject * mod = PyImport_Import(PyString_FromString(str.c_str()));
    if (! mod)
      throw error(std::string("Failed to import Python module ") + str);

    object newmod(handle<>(borrowed(mod)));

#if 1
    // Import all top-level entries directly into the main namespace
    dict m_nspace(handle<>(borrowed(PyModule_GetDict(mod))));
    nspace.update(m_nspace);
#else
    nspace[std::string(PyModule_GetName(mod))] = newmod;
#endif
    return newmod;
  }
  catch (const error_already_set&) {
    PyErr_Print();
    throw error(std::string("Importing Python module ") + str);
  }
}

object python_interpreter_t::eval(std::istream& in, py_eval_mode_t mode)
{
  bool	      first = true;
  std::string buffer;
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
    throw error("Evaluating Python code");
  }
}

object python_interpreter_t::eval(const std::string& str, py_eval_mode_t mode)
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
    throw error("Evaluating Python code");
  }
}

void python_interpreter_t::functor_t::operator()(value_t& result,
						 xml::xpath_t::scope_t * locals)
{
  try {
    if (! PyCallable_Check(func.ptr())) {
      result = static_cast<const value_t&>(extract<value_t>(func.ptr()));
    } else {
      assert(locals->args.type == value_t::SEQUENCE);
      if (locals->args.to_sequence()->size() > 0) {
	list arglist;
	for (value_t::sequence_t::iterator
	       i = locals->args.to_sequence()->begin();
	     i != locals->args.to_sequence()->end();
	     i++)
	  arglist.append(*i);

	if (PyObject * val =
	    PyObject_CallObject(func.ptr(), tuple(arglist).ptr())) {
	  result = extract<value_t>(val)();
	  Py_DECREF(val);
	}
	else if (PyObject * err = PyErr_Occurred()) {
	  PyErr_Print();
	  throw new xml::xpath_t::calc_error
	    (std::string("While calling Python function '") + name() + "'");
	} else {
	  assert(0);
	}
      } else {
	result = call<value_t>(func.ptr());
      }
    }
  }
  catch (const error_already_set&) {
    PyErr_Print();
    throw new xml::xpath_t::calc_error
      (std::string("While calling Python function '") + name() + "'");
  }
}

void python_interpreter_t::lambda_t::operator()(value_t& result,
						xml::xpath_t::scope_t * locals)
{
  try {
    assert(locals->args.type == value_t::SEQUENCE);
    assert(locals->args.to_sequence()->size() == 1);
    value_t item = locals->args[0];
    assert(item.type == value_t::POINTER);
    result = call<value_t>(func.ptr(), (xml::node_t *)*(void **)item.data);
  }
  catch (const error_already_set&) {
    PyErr_Print();
    throw new xml::xpath_t::calc_error("While evaluating Python lambda expression");
  }
}

} // namespace ledger
