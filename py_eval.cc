#include "py_eval.h"
#include "journal.h"
#include "error.h"
#include "acconf.h"

#include <sstream>
#include <map>

namespace {
  bool python_initialized = false;
  bool module_initialized = false;
}

void export_amount();
void export_balance();
void export_value();
void export_journal();
void export_parser();
void export_textual();
void export_binary();
void export_qif();
#ifdef HAVE_XMLPARSE
void export_xml();
void export_gnucash();
#endif
#ifdef HAVE_LIBOFX
void export_ofx();
#endif
void export_option();
void export_config();
void export_walk();
void export_format();
void export_valexpr();
void export_datetime();
void export_derive();
void export_emacs();

void initialize_ledger_for_python()
{
  export_amount();
  export_balance();
  export_value();
  export_journal();
  export_parser();
  export_textual();
  export_binary();
  export_qif();
#ifdef HAVE_XMLPARSE
  export_xml();
  export_gnucash();
#endif
#ifdef HAVE_LIBOFX
  export_ofx();
#endif
  export_option();
  export_config();
  export_walk();
  export_format();
  export_valexpr();
  export_datetime();
  export_derive();
  export_emacs();

  module_initialized = true;
}

namespace ledger {

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
  std::istringstream stream(str);
  return python_eval(stream, mode);
}

object python_eval(const char * c_str, py_eval_mode_t mode)
{
  std::string str(c_str);
  return python_eval(str, mode);
}

} // namespace ledger
