#include "py_eval.h"
#include "error.h"
#include "acconf.h"

#include <istream>

void export_amount();
void export_balance();
void export_value();
void export_datetime();

void export_journal();
void export_parser();
void export_option();
void export_walk();
void export_report();
void export_format();
void export_valexpr();

void shutdown_option();

namespace {
  bool python_initialized = false;
  bool module_initialized = false;

  void initialize_ledger_for_python()
  {
    export_amount();
    export_balance();
    export_value();
#if 0
    export_datetime();
#endif

    export_journal();
    export_parser();
    export_option();
#if 0
    export_walk();
    export_format();
#endif
    export_report();
    export_valexpr();

    module_initialized = true;
  }
}

namespace ledger {

void shutdown_ledger_for_python()
{
  shutdown_option();
}

static struct python_main_t
{
  handle<> mmodule;
  dict     nspace;
  python_main_t()
    : mmodule(borrowed(PyImport_AddModule("__main__"))),
      nspace(handle<>(borrowed(PyModule_GetDict(mmodule.get())))) {}
}
  * python_main = NULL;

struct python_run
{
  object result;
  python_run(const std::string& str, int input_mode)
    : result(handle<>(borrowed(PyRun_String(str.c_str(), input_mode,
					    python_main->nspace.ptr(),
					    python_main->nspace.ptr())))) {}
  operator object() {
    return result;
  }
};

static struct cleanup_python {
  ~cleanup_python() {
    if (python_main) {
      delete python_main;
      python_main = NULL;
    }
    if (python_initialized)
      Py_Finalize();
  }
} _cleanup;

void init_python()
{
  if (! module_initialized) {
    Py_Initialize();
    python_initialized = true;
    detail::init_module("ledger", &initialize_ledger_for_python);
  }
  python_main = new python_main_t;
}

object python_import(const std::string& str)
{
  if (! python_initialized)
    init_python();

  assert(Py_IsInitialized());

  try {
    PyObject * mod = PyImport_Import(PyString_FromString(str.c_str()));
    if (! mod)
      throw error(std::string("Failed to import Python module ") + str);

    object newmod(handle<>(borrowed(mod)));
    python_main->nspace[std::string(PyModule_GetName(mod))] = newmod;
    return newmod;
  }
  catch(const boost::python::error_already_set&) {
    PyErr_Print();
    throw error(std::string("Importing Python module ") + str);
  }
}

object python_eval(std::istream& in, py_eval_mode_t mode)
{
  if (! python_initialized)
    init_python();

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
    return python_run(buffer, input_mode);
  }
  catch(const boost::python::error_already_set&) {
    PyErr_Print();
    throw error("Evaluating Python code");
  }
}

object python_eval(const std::string& str, py_eval_mode_t mode)
{
  if (! python_initialized)
    init_python();

  try {
    int input_mode;
    switch (mode) {
    case PY_EVAL_EXPR:  input_mode = Py_eval_input;   break;
    case PY_EVAL_STMT:  input_mode = Py_single_input; break;
    case PY_EVAL_MULTI: input_mode = Py_file_input;   break;
    }
    assert(Py_IsInitialized());
    return python_run(str, input_mode);
  }
  catch(const boost::python::error_already_set&) {
    PyErr_Print();
    throw error("Evaluating Python code");
  }
}

object python_eval(const char * c_str, py_eval_mode_t mode)
{
  std::string str(c_str);
  return python_eval(str, mode);
}

void python_functor_t::operator()(value_t& result, valexpr_t::scope_t * args)
{
  std::string func_name = "<Unknown>";

  try {
    if (args->arg_scope && args->args.size() > 0) {
      list arglist;
      for (std::vector<valexpr_t::node_t *>::iterator i = args->args.begin();
	   i != args->args.end();
	   i++)
	arglist.append((*i)->value());

      if (PyObject * val =
	  PyObject_CallObject(func.ptr(), tuple(arglist).ptr())) {
	result = extract<value_t>(val)();
	Py_DECREF(val);
      }
      else if (PyObject * err = PyErr_Occurred()) {
	PyErr_Print();
	throw new valexpr_t::calc_error
	  (std::string("While calling Python function '") + func_name + "'");
      } else {
	assert(0);
      }
    } else {
      result = call<value_t>(func.ptr());
    }
  }
  catch(const boost::python::error_already_set&) {
    PyErr_Print();
    throw new valexpr_t::calc_error
      (std::string("While calling Python function '") + func_name + "'");
  }
}

value_t python_lambda_t::operator()(valexpr_t::scope_t * args)
{
  try {
    assert(args->arg_scope && args->args.size() == 1);
    value_t item = args->args[0]->value();
    assert(item.type == value_t::POINTER);
#if 0
    return call<value_t>(func.ptr(), (repitem_t *)*(void **)item.data);
#else
    return value_t();
#endif
  }
  catch(const boost::python::error_already_set&) {
    PyErr_Print();
    throw new valexpr_t::calc_error
      ("While evaluating Python lambda expression");
  }
}

} // namespace ledger
