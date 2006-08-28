#ifndef _PYTHON_H
#define _PYTHON_H

#include <string>
#include <iostream>

#include <boost/python.hpp>

using namespace boost::python;

namespace ledger {

void shutdown_ledger_for_python();

object python_import(const std::string& name);

enum py_eval_mode_t {
  PY_EVAL_EXPR,
  PY_EVAL_STMT,
  PY_EVAL_MULTI
};

object python_eval(std::istream& in, py_eval_mode_t mode = PY_EVAL_EXPR);
object python_eval(const std::string& str, py_eval_mode_t mode = PY_EVAL_EXPR);
object python_eval(const char * c_str, py_eval_mode_t mode = PY_EVAL_EXPR);

} // namespace ledger

#endif // _PYTHON_H
