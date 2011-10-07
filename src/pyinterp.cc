/*
 * Copyright (c) 2003-2010, John Wiegley.  All rights reserved.
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
#include "account.h"
#include "xact.h"
#include "post.h"

namespace ledger {

using namespace python;

shared_ptr<python_interpreter_t> python_session;

char * argv0;

void export_account();
void export_amount();
void export_balance();
void export_commodity();
void export_expr();
void export_format();
void export_item();
void export_journal();
void export_post();
void export_times();
void export_utils();
void export_value();
void export_xact();

void initialize_for_python()
{
  export_times();
  export_utils();
  export_commodity();
  export_amount();
  export_value();
  export_account();
  export_balance();
  export_expr();
  export_format();
  export_item();
  export_post();
  export_xact();
  export_journal();
}

struct python_run
{
  object result;

  python_run(python_interpreter_t * intepreter,
             const string& str, int input_mode)
    : result(handle<>(borrowed(PyRun_String(str.c_str(), input_mode,
                                            intepreter->main_nspace.ptr(),
                                            intepreter->main_nspace.ptr())))) {}
  operator object() {
    return result;
  }
};

void python_interpreter_t::initialize()
{
  TRACE_START(python_init, 1, "Initialized Python");

  try {
    DEBUG("python.interp", "Initializing Python");

    Py_Initialize();
    assert(Py_IsInitialized());

    hack_system_paths();

    object main_module = python::import("__main__");
    if (! main_module)
      throw_(std::runtime_error,
             _("Python failed to initialize (couldn't find __main__)"));

    main_nspace = extract<dict>(main_module.attr("__dict__"));
    if (! main_nspace)
      throw_(std::runtime_error,
             _("Python failed to initialize (couldn't find __dict__)"));

    python::detail::init_module("ledger", &initialize_for_python);

    is_initialized = true;
  }
  catch (const error_already_set&) {
    PyErr_Print();
    throw_(std::runtime_error, _("Python failed to initialize"));
  }

  TRACE_FINISH(python_init, 1);
}

void python_interpreter_t::hack_system_paths()
{
  // Hack ledger.__path__ so it points to a real location
  python::object sys_module = python::import("sys");
  python::object sys_dict   = sys_module.attr("__dict__");

  python::list paths(sys_dict["path"]);

#if defined(DEBUG_ON)
  bool path_initialized = false;
#endif
  int n = python::extract<int>(paths.attr("__len__")());
  for (int i = 0; i < n; i++) {
    python::extract<std::string> str(paths[i]);
    path pathname(str());
    DEBUG("python.interp", "sys.path = " << pathname);

    if (exists(pathname / "ledger" / "__init__.py")) {
      if (python::object module_ledger = python::import("ledger")) {
        DEBUG("python.interp",
              "Setting ledger.__path__ = " << (pathname / "ledger"));

        python::object ledger_dict = module_ledger.attr("__dict__");
        python::list temp_list;
        temp_list.append((pathname / "ledger").string());

        ledger_dict["__path__"] = temp_list;
      } else {
        throw_(std::runtime_error,
               _("Python failed to initialize (couldn't find ledger)"));
      }
#if defined(DEBUG_ON)
      path_initialized = true;
#endif
      break;
    }
  }
#if defined(DEBUG_ON)
  if (! path_initialized)
    DEBUG("python.init",
          "Ledger failed to find 'ledger/__init__.py' on the PYTHONPATH");
#endif
}

object python_interpreter_t::import_into_main(const string& str)
{
  if (! is_initialized)
    initialize();

  try {
    object mod = python::import(str.c_str());
    if (! mod)
      throw_(std::runtime_error,
             _("Failed to import Python module %1") << str);

    // Import all top-level entries directly into the main namespace
    main_nspace.update(mod.attr("__dict__"));

    return mod;
  }
  catch (const error_already_set&) {
    PyErr_Print();
  }
  return object();
}

object python_interpreter_t::import_option(const string& str)
{
  path file(str);

  python::object sys_module = python::import("sys");
  python::object sys_dict   = sys_module.attr("__dict__");

  python::list paths(sys_dict["path"]);

#if BOOST_VERSION >= 103700
  paths.insert(0, file.parent_path().string());
  sys_dict["path"] = paths;

#if BOOST_VERSION >= 104600
  string name = file.filename().string();
  if (contains(name, ".py"))
    name = file.stem().string();
#else
  string name = file.filename();
  if (contains(name, ".py"))
    name = file.stem();
#endif
#else // BOOST_VERSION >= 103700
  paths.insert(0, file.branch_path().string());
  sys_dict["path"] = paths;

  string name = file.leaf();
#endif // BOOST_VERSION >= 103700

  return python::import(python::str(name.c_str()));
}

object python_interpreter_t::eval(std::istream& in, py_eval_mode_t mode)
{
  bool   first = true;
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

  if (! is_initialized)
    initialize();

  try {
    int input_mode = -1;
    switch (mode) {
    case PY_EVAL_EXPR:  input_mode = Py_eval_input;   break;
    case PY_EVAL_STMT:  input_mode = Py_single_input; break;
    case PY_EVAL_MULTI: input_mode = Py_file_input;   break;
    }

    return python_run(this, buffer, input_mode);
  }
  catch (const error_already_set&) {
    PyErr_Print();
    throw_(std::runtime_error, _("Failed to evaluate Python code"));
  }
  return object();
}

object python_interpreter_t::eval(const string& str, py_eval_mode_t mode)
{
  if (! is_initialized)
    initialize();

  try {
    int input_mode = -1;
    switch (mode) {
    case PY_EVAL_EXPR:  input_mode = Py_eval_input;   break;
    case PY_EVAL_STMT:  input_mode = Py_single_input; break;
    case PY_EVAL_MULTI: input_mode = Py_file_input;   break;
    }

    return python_run(this, str, input_mode);
  }
  catch (const error_already_set&) {
    PyErr_Print();
    throw_(std::runtime_error, _("Failed to evaluate Python code"));
  }
  return object();
}

value_t python_interpreter_t::python_command(call_scope_t& args)
{
  if (! is_initialized)
    initialize();

  char ** argv(new char *[args.size() + 1]);

  argv[0] = new char[std::strlen(argv0) + 1];
  std::strcpy(argv[0], argv0);

  for (std::size_t i = 0; i < args.size(); i++) {
    string arg = args.get<string>(i);
    argv[i + 1] = new char[arg.length() + 1];
    std::strcpy(argv[i + 1], arg.c_str());
  }

  int status = 1;

  try {
    status = Py_Main(static_cast<int>(args.size()) + 1, argv);
  }
  catch (const error_already_set&) {
    PyErr_Print();
    throw_(std::runtime_error, _("Failed to execute Python module"));
  }
  catch (...) {
    for (std::size_t i = 0; i < args.size() + 1; i++)
      delete[] argv[i];
    delete[] argv;
    throw;
  }

  for (std::size_t i = 0; i < args.size() + 1; i++)
    delete[] argv[i];
  delete[] argv;

  if (status != 0)
    throw status;

  return NULL_VALUE;
}

value_t python_interpreter_t::server_command(call_scope_t& args)
{
  if (! is_initialized)
    initialize();

  python::object server_module;

  try {
    server_module = python::import("ledger.server");
    if (! server_module)
      throw_(std::runtime_error,
             _("Could not import ledger.server; please check your PYTHONPATH"));
  }
  catch (const error_already_set&) {
    PyErr_Print();
    throw_(std::runtime_error,
           _("Could not import ledger.server; please check your PYTHONPATH"));
  }

  if (python::object main_function = server_module.attr("main")) {
    functor_t func(main_function, "main");
    try {
      func(args);
    }
    catch (const error_already_set&) {
      PyErr_Print();
      throw_(std::runtime_error,
             _("Error while invoking ledger.server's main() function"));
    }
    return true;
  } else {
      throw_(std::runtime_error,
             _("The ledger.server module is missing its main() function!"));
  }

  return false;
}

option_t<python_interpreter_t> *
python_interpreter_t::lookup_option(const char * p)
{
  switch (*p) {
  case 'i':
    OPT(import_);
    break;
  }
  return NULL;
}

expr_t::ptr_op_t python_interpreter_t::lookup(const symbol_t::kind_t kind,
                                              const string& name)
{
  // Give our superclass first dibs on symbol definitions
  if (expr_t::ptr_op_t op = session_t::lookup(kind, name))
    return op;

  switch (kind) {
  case symbol_t::FUNCTION:
    if (option_t<python_interpreter_t> * handler = lookup_option(name.c_str()))
      return MAKE_OPT_FUNCTOR(python_interpreter_t, handler);

    if (is_initialized && main_nspace.has_key(name.c_str())) {
      DEBUG("python.interp", "Python lookup: " << name);

      if (python::object obj = main_nspace.get(name.c_str()))
        return WRAP_FUNCTOR(functor_t(obj, name));
    }
    break;

  case symbol_t::OPTION:
    if (option_t<python_interpreter_t> * handler = lookup_option(name.c_str()))
      return MAKE_OPT_HANDLER(python_interpreter_t, handler);
    break;

  case symbol_t::PRECOMMAND: {
    const char * p = name.c_str();
    switch (*p) {
    case 'p':
      if (is_eq(p, "python"))
        return MAKE_FUNCTOR(python_interpreter_t::python_command);
      break;

    case 's':
      if (is_eq(p, "server"))
        return MAKE_FUNCTOR(python_interpreter_t::server_command);
      break;
    }
  }

  default:
    break;
  }

  return NULL;
}

namespace {
  void append_value(list& lst, const value_t& value)
  {
    if (value.is_scope()) {
      const scope_t * scope = value.as_scope();
      if (const post_t * post = dynamic_cast<const post_t *>(scope))
        lst.append(ptr(post));
      else if (const xact_t * xact = dynamic_cast<const xact_t *>(scope))
        lst.append(ptr(xact));
      else if (const account_t * account =
               dynamic_cast<const account_t *>(scope))
        lst.append(ptr(account));
      else if (const period_xact_t * period_xact =
               dynamic_cast<const period_xact_t *>(scope))
        lst.append(ptr(period_xact));
      else if (const auto_xact_t * auto_xact =
               dynamic_cast<const auto_xact_t *>(scope))
        lst.append(ptr(auto_xact));
      else
        throw_(std::logic_error,
               _("Cannot downcast scoped object to specific type"));
    } else {
      lst.append(value);
    }
  }
}

value_t python_interpreter_t::functor_t::operator()(call_scope_t& args)
{
  try {
    std::signal(SIGINT, SIG_DFL);

    if (! PyCallable_Check(func.ptr())) {
      extract<value_t> val(func);
      std::signal(SIGINT, sigint_handler);
      if (val.check())
        return val();
      return NULL_VALUE;
    }
    else if (args.size() > 0) {
      list arglist;
      // jww (2009-11-05): What about a single argument which is a sequence,
      // rather than a sequence of arguments?
      if (args.value().is_sequence())
        foreach (const value_t& value, args.value().as_sequence())
          append_value(arglist, value);
      else
        append_value(arglist, args.value());

      if (PyObject * val =
          PyObject_CallObject(func.ptr(), python::tuple(arglist).ptr())) {
        extract<value_t> xval(val);
        value_t result;
        if (xval.check()) {
          result = xval();
          Py_DECREF(val);
        } else {
          Py_DECREF(val);
          throw_(calc_error,
                 _("Could not evaluate Python variable '%1'") << name);
        }
        std::signal(SIGINT, sigint_handler);
        return result;
      }
      else if (PyErr_Occurred()) {
        PyErr_Print();
        throw_(calc_error, _("Failed call to Python function '%1'") << name);
      } else {
        assert(false);
      }
    }
    else {
      std::signal(SIGINT, sigint_handler);
      return call<value_t>(func.ptr());
    }
  }
  catch (const error_already_set&) {
    std::signal(SIGINT, sigint_handler);
    PyErr_Print();
    throw_(calc_error, _("Failed call to Python function '%1'") << name);
  }
  catch (...) {
    std::signal(SIGINT, sigint_handler);
  }
  std::signal(SIGINT, sigint_handler);

  return NULL_VALUE;
}

} // namespace ledger
