#ifndef _PYTHON_H
#define _PYTHON_H

#include "error.h"

#include <boost/python.hpp>

using namespace boost::python;

namespace ledger {

struct python_support
{
  handle<> main_module;
  dict     main_namespace;

  python_support()
    : main_module(borrowed(PyImport_AddModule("__main__"))),
      main_namespace(handle<>(borrowed(PyModule_GetDict(main_module.get()))))
  {}
  ~python_support() {
  }
};

extern python_support * python_interpretor;

void init_python();

inline void python_eval(std::istream& in)
{
  if (! python_interpretor)
    init_python();

  std::string buffer;
  buffer.reserve(4096);
  while (! in.eof()) {
    char buf[256];
    in.getline(buf, 255);
    if (buf[0] == '!')
      break;
    buffer += buf;
    buffer += "\n";
  }

  try {
    handle<>(borrowed(PyRun_String(buffer.c_str(), Py_file_input,
				   python_interpretor->main_namespace.ptr(),
				   python_interpretor->main_namespace.ptr())));
  }
  catch(const boost::python::error_already_set&) {
    PyErr_Print();
    throw error("Evaluating Python code");
  }
}

} // namespace ledger

#endif // _PYTHON_H
